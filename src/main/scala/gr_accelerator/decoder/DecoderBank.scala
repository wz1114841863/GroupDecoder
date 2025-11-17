package gr_accelerator.decoder

import chisel3._
import chisel3.util._
import gr_accelerator.common._

class MetaReqPayload(val p: GRDecoderCoreParams) extends Bundle {
    val addr = UInt(p.metaGroupIndexWidth.W)
}
class StreamReqPayload(val p: GRDecoderCoreParams) extends Bundle {
    val addr = UInt(p.streamAddrWidth.W)
}

/** DecoderBank (顶层分发模块) 的 IO 端口
  */
class DecoderBankIO(val p: DecoderBankParams) extends Bundle {

    /** 控制端口 (来自 TopAccelerator 或测试 平台)
      *
      * 使用 Decoupled (valid/ready) 握手协议
      */
    val start_req = Flipped(Decoupled(new Bundle {
        val base_group_index = UInt(p.coreParams.metaGroupIndexWidth.W)
        val num_groups_to_decode = UInt(p.coreParams.metaGroupIndexWidth.W)
    }))

    /** 完成信号
      */
    val bank_finished = Output(Bool())

    /** (简化接口) 用于在测试中预加载内部存储
      */
    val load_meta = Flipped(Decoupled(new Bundle {
        val addr = UInt(log2Ceil(p.metaSramDepth).W)
        val data = Input(p.metaDataType)
    }))

    val load_stream = Flipped(Decoupled(new Bundle {
        val addr = UInt(p.coreParams.streamAddrWidth.W) // 字节地址
        val data = UInt(8.W) // 一次写 1 字节
    }))

    /** 数据输出 (去往 SystolicArray)
      *
      * 暴露 P=8 个并行的 SRAM 写入端口
      */
    val sram_write_outputs = Output(
      Vec(
        p.P,
        new Bundle {
            val valid = Bool()
            val addr = UInt(p.coreParams.outputSramAddrWidth.W)
            val data = UInt(p.coreParams.weightWidth.W)
            val wave_index = UInt(8.W) // 假设 8-bit 足够
        }
      )
    )

    val meta_write_outputs = Output(
      Vec(
        p.P,
        new Bundle {
            val valid = Bool()
            val zp = UInt(p.coreParams.zpWidth.W)
            // 我们需要输出 wave_index 以便外部知道写到哪里
            // 这里的宽度应与 FSM 中的计数器宽度匹配 (目前设为 8.W)
            val wave_index = UInt(8.W) // 假设 8-bit 足够 (支持 256 波)
            // (可选) 如果以后需要输出 group_index 或 bank_addr,也可以加在这里
            // val addr = ...
        }
      )
    )
}

/** DecoderBank (顶层分发模块)
  */
class DecoderBank(val p: DecoderBankParams) extends Module {
    val io = IO(new DecoderBankIO(p))

    // 调试计数器
    val bank_cycle_count = RegInit(0.U(32.W))

    // --- 1. 实例化 (P为并行的解码器) ---
    val decoders = VecInit(
      Seq.tabulate(p.P)(i => Module(new GRDecoderCore(p.coreParams, i.U)).io)
    )

    // --- 2. 实例化内部内存 (用于简化测试) ---

    /** MetaSRAM (内部) Chisel 'Mem' 模拟 SRAM
      */
    val meta_sram = Mem(p.metaSramDepth, p.metaDataType)

    /** SharedFrontend (内部) 2KB 共享缓存 (字节寻址)
      */
    val shared_cache = Mem(p.sharedCacheKBytes * 1024, UInt(8.W))

    // --- 3. 内存加载 逻辑 (用于测试) ---

    // MetaSRAM 加载
    io.load_meta.ready := true.B // 总是准备好接收
    when(io.load_meta.valid) {
        meta_sram.write(io.load_meta.bits.addr, io.load_meta.bits.data)
    }

    // 共享缓存 加载
    io.load_stream.ready := true.B // 总是准备好接收
    when(io.load_stream.valid) {
        shared_cache.write(io.load_stream.bits.addr, io.load_stream.bits.data)
    }

    // --- 4. 连接 (仲裁器) ---
    // P个解码器
    val p_grp = 0 until p.P

    val meta_arb = Module(new RRArbiter(new MetaReqPayload(p.coreParams), p.P))
    val stream_arb = Module(
      new RRArbiter(new StreamReqPayload(p.coreParams), p.P)
    )

    for (i <- 0 until p.P) {
        meta_arb.io.in(i).valid := decoders(i).meta_req.valid
        meta_arb.io.in(i).bits.addr := decoders(i).meta_req.addr
        stream_arb.io.in(i).valid := decoders(i).stream_req.valid
        stream_arb.io.in(i).bits.addr := decoders(i).stream_req.addr
    }

    // --- MetaSRAM 响应 (组合逻辑) ---
    val meta_req_valid = meta_arb.io.out.valid
    val meta_req_addr = meta_arb.io.out.bits.addr
    val meta_req_chosen = meta_arb.io.chosen

    val meta_read_data = meta_sram(meta_req_addr)
    meta_arb.io.out.ready := true.B

    // --- SharedCache 响应 (组合逻辑) ---
    val stream_req_valid = stream_arb.io.out.valid
    val stream_req_addr = stream_arb.io.out.bits.addr
    val stream_req_chosen = stream_arb.io.chosen

    val stream_read_data = Cat(
      (0 until p.coreParams.streamFetchWidthBytes).map { i =>
          shared_cache(stream_req_addr + i.U)
      }
    )
    stream_arb.io.out.ready := true.B

    // --- FSM 定义 ---
    object State extends ChiselEnum {
        val sIdle, sRun, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // 每个解码器 的任务计数器
    val groups_decoded_per_core = RegInit(VecInit(Seq.fill(p.P)(0.U(8.W))))
    val groups_total_per_core = RegInit(VecInit(Seq.fill(p.P)(0.U(8.W))))

    // 跟踪哪个解码器 正在忙
    val core_is_busy = RegInit(VecInit(Seq.fill(p.P)(false.B)))
    val base_idx_reg = Reg(UInt(p.coreParams.metaGroupIndexWidth.W))

    // 暴露 P=8 个写入 端口
    // io.sram_write_outputs := decoders.map(_.sram_write)

    // --- SRAM 写入端口连接逻辑 ---
    // 我们需要将 Core 的输出和 FSM 的 wave_index 结合起来
    for (i <- 0 until p.P) {
        io.sram_write_outputs(i).valid := decoders(i).sram_write.valid
        io.sram_write_outputs(i).addr := decoders(i).sram_write.addr
        io.sram_write_outputs(i).data := decoders(i).sram_write.data
        io.sram_write_outputs(i).wave_index := groups_decoded_per_core(i)
    }

    // --- 响应分发 (不再广播数据) ---
    for (i <- p_grp) {
        val core_won_meta = (meta_req_chosen === i.U)
        val core_won_stream = (stream_req_chosen === i.U)

        // Meta 响应
        decoders(i).meta_resp.valid := meta_req_valid && core_won_meta
        // 只有在获胜时才连接数据, 否则连接 0
        decoders(i).meta_resp.start_byte_addr := Mux(
          core_won_meta,
          meta_read_data.start_byte_addr,
          0.U
        )
        decoders(i).meta_resp.zero_point := Mux(
          core_won_meta,
          meta_read_data.zero_point,
          0.U
        )

        // Stream 响应
        decoders(i).stream_resp.valid := stream_req_valid && core_won_stream
        // 只有在获胜时才连接数据, 否则连接 0
        decoders(i).stream_resp.data := Mux(
          core_won_stream,
          stream_read_data,
          0.U
        )

        // *** [DEBUG PRINTF] ***
        // 当这个 Core (i) 赢得了 stream 仲裁时, 打印它请求的地址和它收到的数据
        // when(stream_req_valid && core_won_stream) {
        //     printf(
        //       p"[Bank] Stream RESP: Core ${i} WON. " +
        //           p"req_addr=0x${Hexadecimal(stream_req_addr)}, " +
        //           p"sent_data=0x${Hexadecimal(stream_read_data(63, 32))}...\n"
        //     )
        // }

        // 当这个 Core (i) 赢得了 meta 仲裁时, 打印
        // when(meta_req_valid && core_won_meta) {
        //     printf(
        //       p"[Bank] Meta RESP: Core ${i} WON. " +
        //           p"req_addr=${meta_req_addr}, " +
        //           p"sent_addr=0x${Hexadecimal(meta_read_data.start_byte_addr)}\n"
        //     )
        // }
    }

    // --- 5. FSM(状态机, "Group_ID % P" 方案) ---

    // 默认输出
    io.bank_finished := false.B
    io.start_req.ready := false.B // 默认不接收新任务
    decoders.foreach(_.start := false.B)
    // decoders.foreach(_.group_index := 0.U)

    for (i <- p_grp) {
        decoders(i).group_index := base_idx_reg + (groups_decoded_per_core(
          i
        ) * p.P.U) + i.U
    }

    // --- 连接元数据输出端口 (ZP 和 Wave Index) ---
    // 因为 groups_decoded_per_core 已经定义了,现在可以连接了
    for (i <- p_grp) {
        // Valid 信号复用发给 Core 的 valid 信号
        // (当 Core 收到 Meta 响应时,我们也向外输出 ZP)
        io.meta_write_outputs(i).valid := decoders(i).meta_resp.valid

        // ZP 直接来自 decoder 接收到的数据 (已经过 Mux 处理)
        io.meta_write_outputs(i).zp := decoders(i).meta_resp.zero_point

        // Wave Index 直接来自 FSM 计数器
        io.meta_write_outputs(i).wave_index := groups_decoded_per_core(i)
    }

    switch(state) {
        is(State.sIdle) {
            io.start_req.ready := true.B // 准备好接收任务

            when(io.start_req.valid) {
                state := State.sRun

                // 锁存任务
                base_idx_reg := io.start_req.bits.base_group_index
                val num_groups = io.start_req.bits.num_groups_to_decode

                // 计算每个解码器 的任务量
                val base_per_core = num_groups / p.P.U
                val remainder = num_groups % p.P.U

                for (i <- p_grp) {
                    groups_total_per_core(i) := base_per_core + Mux(
                      i.U < remainder,
                      1.U,
                      0.U
                    )
                    groups_decoded_per_core(i) := 0.U
                    core_is_busy(i) := false.B
                }
            }
        }

        is(State.sRun) {
            // 独立调度 P=8 个解码器
            for (i <- p_grp) {
                // 检查这个解码器 是否已完成 *所有* 工作
                val core_has_work_left =
                    (groups_decoded_per_core(i) < groups_total_per_core(i))

                when(core_has_work_left) {
                    // 检查它当前是否空闲
                    when(!core_is_busy(i)) {
                        // 启动新任务
                        core_is_busy(i) := true.B
                        decoders(i).start := true.B
                    }
                }

                // 检查已启动的任务是否完成
                when(core_is_busy(i) && decoders(i).finished) {
                    core_is_busy(i) := false.B
                    decoders(i).start := false.B // 必须拉低 start
                    groups_decoded_per_core(i) := groups_decoded_per_core(
                      i
                    ) + 1.U
                }
            }

            // 检查是否所有解码器都完成了所有任务
            val all_cores_finished = (
              groups_decoded_per_core
                  .zip(groups_total_per_core)
                  .map { case (decoded, total) => decoded === total }
                  .reduce(_ && _)
            )

            // --- 调试输出 ---
            bank_cycle_count := bank_cycle_count + 1.U

            // printf(p"[Bank FSM] [C=${bank_cycle_count}] State: ${state} \n")
            // for (i <- 0 until p.P) {
            //     printf(
            //       p"  [Core ${i}] Busy: ${core_is_busy(i)} | " +
            //           p"Core_Finished_In: ${decoders(i).finished} | " +
            //           p"Decoded: ${groups_decoded_per_core(i)} (of ${groups_total_per_core(i)}) | " +
            //           p"Core_Start_Out: ${decoders(i).start}\n"
            //     )
            // }

            when(all_cores_finished) {
                state := State.sDone
            }
        }

        is(State.sDone) {
            io.bank_finished := true.B
            when(io.start_req.ready) { // (等待 Top 确认)
                state := State.sIdle
            }
        }
    }
}
