package gr_accelerator

import chisel3._
import chisel3.util._

import gr_accelerator.common._
import gr_accelerator.decoder._
import gr_accelerator.systolic._

class TopAcceleratorIO(val p: TopAcceleratorParams) extends Bundle {
    // --- 1. 全局控制 ---
    val start = Input(Bool()) // 启动信号
    val done = Output(Bool()) // 完成中断

    // 本次任务需要计算多少个 Tile (Total Tiles)
    val total_tiles = Input(UInt(32.W))

    // --- 2. 数据加载接口 (DMA 模拟 - 仅仅是连线) ---
    // 连接到 DecoderBank.shared_cache
    val load_stream_in = Flipped(Decoupled(new Bundle {
        val addr = UInt(p.decoderParams.coreParams.streamAddrWidth.W)
        val data = UInt(8.W)
    }))

    // 连接到 DecoderBank.meta_sram (ZP)
    val load_meta_in = Flipped(Decoupled(new Bundle {
        val addr = UInt(log2Ceil(p.decoderParams.metaSramDepth).W)
        val data = Input(p.decoderParams.metaDataType)
    }))

    // 连接到 MetaSRAMBuffer.scale_mem (Scale)
    val load_scale_in = Flipped(Decoupled(new Bundle {
        val addr = UInt(p.metaSramParams.addrWidth.W)
        val data = UInt(p.peParams.scaleWidth.W)
    }))

    // --- 3. 激活输入与结果输出 (流式接口) ---
    // 实际上这里连接到 SystolicArray 的边缘
    val act_in = Input(Vec(p.N, SInt(p.peParams.actWidth.W)))
    val sum_out = Output(Vec(p.N, SInt(p.peParams.accWidth.W)))

    // 调试状态
    val debug_state = Output(UInt(4.W))
}

class TopAccelerator(val p: TopAcceleratorParams) extends Module {
    val io = IO(new TopAcceleratorIO(p))

    // ==============================================================================
    // 1. 模块实例化
    // ==============================================================================

    // 生产者: DecoderBank (包含 shared_cache 和 meta_sram)
    val decoderBank = Module(new DecoderBank(p.decoderParams))

    // 缓冲: WeightSRAM (权重双缓冲)
    val weightSRAM = Module(new WeightSRAM(p.weightSramParams))

    // 缓冲: MetaSRAMBuffer (ZP/Scale 双缓冲)
    val metaSRAM = Module(new MetaSRAMBuffer(p.metaSramParams))

    // 接口: WeightLoader (P -> N 转换)
    val weightLoader = Module(new WeightLoader(p.weightSramParams))

    // 接口: MetadataLoader (串行 -> 并行广播)
    val metaLoader = Module(new MetadataLoader(p.metaSramParams))

    // 消费者: SystolicArray (计算核心)
    val systolicArray = Module(new SystolicArray(p.saParams, p.peParams))

    // ==============================================================================
    // 2. 外部加载接口连线 (透传 - 无额外存储)
    // ==============================================================================

    // Stream Load -> DecoderBank
    decoderBank.io.load_stream <> io.load_stream_in

    // Meta Load -> DecoderBank (ZP 部分)
    decoderBank.io.load_meta <> io.load_meta_in

    // Scale Load -> MetaSRAMBuffer (Scale 部分)
    io.load_scale_in.ready := true.B // 总是准备好接收
    metaSRAM.io.scale_write_port.valid := io.load_scale_in.valid
    metaSRAM.io.scale_write_port.addr := io.load_scale_in.bits.addr
    metaSRAM.io.scale_write_port.data := io.load_scale_in.bits.data

    // *** [DEBUG] 监控 DecoderBank 输出的 ZP ***
    for (i <- 0 until p.P) {
        when(decoderBank.io.meta_write_outputs(i).valid) {
            printf(p"[Top] Decoder[$i] Writing ZP: zp=${decoderBank.io
                    .meta_write_outputs(i)
                    .zp}, wave=${decoderBank.io.meta_write_outputs(i).wave_index}\n")
        }
    }

    // ==============================================================================
    // 3. 内部模块互联 (数据通路)
    // ==============================================================================

    // --- Path A: DecoderBank -> Buffers (生产) ---
    // 1. 权重写入 WeightSRAM
    weightSRAM.io.write_ports := decoderBank.io.sram_write_outputs

    // 2. ZP 写入 MetaSRAM
    metaSRAM.io.zp_write_ports := decoderBank.io.meta_write_outputs

    // --- Path B: Buffers -> Loaders (搬运) ---
    // 1. WeightSRAM -> WeightLoader
    weightSRAM.io.read_addrs := weightLoader.io.sram_read_addrs
    weightLoader.io.sram_read_data := weightSRAM.io.read_data

    // 2. MetaSRAM -> MetadataLoader
    metaSRAM.io.read_addr := metaLoader.io.sram_read_addr
    metaLoader.io.sram_read_zp := metaSRAM.io.read_zp
    metaLoader.io.sram_read_scale := metaSRAM.io.read_scale

    // --- Path C: Loaders -> SystolicArray (加载阵列) ---
    // 1. 权重推入
    systolicArray.io.in_weight := weightLoader.io.array_w_in
    systolicArray.io.ctrl_load_en := weightLoader.io.array_load_en // 这里的使能信号很重要

    // 2. 元数据推入 (ZP/Scale)
    systolicArray.io.in_zp := metaLoader.io.array_zp_out
    systolicArray.io.in_scale := metaLoader.io.array_scale_out

    // *** [DEBUG] 监控最终推入 Array 的 ZP ***
    // 在 Loader 完成时,打印前两个和最后一个 ZP
    val meta_loader_done_pulse =
        metaLoader.io.done && !RegNext(metaLoader.io.done)
    when(meta_loader_done_pulse) {
        printf(p"[Top] MetaLoader DONE. Broadcasting ZPs to Array:\n")
        printf(p"  ZP[0]=${metaLoader.io.array_zp_out(0)}\n")
        printf(p"  ZP[1]=${metaLoader.io.array_zp_out(1)}\n")
        printf(p"  ZP[2]=${metaLoader.io.array_zp_out(2)}\n")
        printf(p"  ZP[N-1]=${metaLoader.io.array_zp_out(p.N - 1)}\n")
    }

    // 3. 部分和初始化 (设为 0)
    systolicArray.io.in_sum := VecInit(Seq.fill(p.N)(0.S))

    // --- Path D: Compute IO (外部激活 -> 阵列 -> 外部结果) ---
    systolicArray.io.in_act := io.act_in
    io.sum_out := systolicArray.io.out_sum

    // ==============================================================================
    // 4. 顶层主控 FSM (流水线调度)
    // ==============================================================================

    object State extends ChiselEnum {
        val sIdle, sPrefill, sFlip, sRun, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // 计数器: 当前处理了多少个 Tile
    val tile_cnt = RegInit(0.U(32.W))

    // 双缓冲翻转信号寄存器
    val flip_reg = RegInit(false.B)

    // 完成状态记分板
    val wl_done_latched = RegInit(false.B)
    val ml_done_latched = RegInit(false.B)
    val db_done_latched = RegInit(false.B)

    // --- 默认控制信号 ---
    decoderBank.io.start_req.valid := false.B
    decoderBank.io.start_req.bits.base_group_index := 0.U // 需动态计算
    decoderBank.io.start_req.bits.num_groups_to_decode := (p.N).U // 每次解码一个 Tile (N行)

    weightLoader.io.start := false.B
    // WeightLoader 需要知道当前 Tile 在 WeightSRAM 中的起始 Wave Index
    // 简单起见,假设 WeightSRAM 总是复用地址 0 开始的空间 (Ping-Pong)
    weightLoader.io.base_group_idx := 0.U

    metaLoader.io.start := false.B
    metaLoader.io.base_group_idx := 0.U // 同上

    // 连接 Flip 信号到缓冲
    weightSRAM.io.flip := flip_reg
    metaSRAM.io.flip := flip_reg

    io.done := false.B
    io.debug_state := state.asUInt

    // --- 状态机逻辑 ---
    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                // 复位计数器
                tile_cnt := 0.U
                flip_reg := false.B // 初始状态: 产A, 耗B

                // 启动第一个任务: 解码 Tile 0 到 Buffer A
                decoderBank.io.start_req.valid := true.B
                // Tile 0 对应的 Group ID: 0
                decoderBank.io.start_req.bits.base_group_index := 0.U

                state := State.sPrefill
                printf("[Top] State -> sPrefill (Decoding Tile 0)\n")
            }
        }

        is(State.sPrefill) {
            // 等待 DecoderBank 完成第一块 Tile 的解码
            when(decoderBank.io.bank_finished) {
                state := State.sFlip
                printf("[Top] State -> sFlip\n")
            }
        }

        is(State.sFlip) {
            // 翻转缓冲: A 变为消费 (Ready), B 变为生产 (Empty)
            flip_reg := !flip_reg
            tile_cnt := tile_cnt + 1.U

            // 检查是否全部完成
            when(tile_cnt === io.total_tiles) {
                state := State.sDone
            }.otherwise {
                // 并行启动:
                // 1. 消费者: Loaders 将刚刚生产好的缓冲数据搬入阵列
                weightLoader.io.start := true.B
                metaLoader.io.start := true.B

                // 2. 生产者: DecoderBank 开始解码下一块 Tile 到另一个缓冲
                // 下一块 Tile 的 Group ID = tile_cnt * N (假设 Tile 是 N 行 Group)
                decoderBank.io.start_req.valid := true.B
                decoderBank.io.start_req.bits.base_group_index := tile_cnt * p.N.U

                state := State.sRun
                printf(p"[Top] State -> sRun (Tile ${tile_cnt})\n")
            }
        }

        is(State.sRun) {
            // 在这个状态, 生产者(解码) 和 消费者(加载+计算) 并行运行

            // 我们需要等待 Loaders 完成搬运,阵列才能开始计算
            // (注意: WeightLoader 完成意味着阵列已填满)
            val loaders_done = weightLoader.io.done && metaLoader.io.done
            val decoder_done = decoderBank.io.bank_finished

            // 这里简化模型: 假设 Loaders 完成后,脉动阵列自动准备好计算
            // 实际上还需要等待计算完成的信号 (T_compute).
            // 但在 WeightStationary 架构中,加载完通常就可以 flip (如果计算和解码重叠)
            // 为了严谨,我们这里等待 "两者都完成"

            when(loaders_done && decoder_done) {
                // 生产者和消费者都完成了,可以进行下一轮翻转
                state := State.sFlip
                printf("[Top] sRun Finished. Flipping.\n")
            }
        }

        // is(State.sRun) {
        //     // 在这个状态, 生产者(解码) 和 消费者(加载+计算) 并行运行

        //     // 使用 "记分板" (Latch) 来捕获稍纵即逝的脉冲信号
        //     // 只要收到一次 done, 就将对应的 latched 寄存器置为 true
        //     when(weightLoader.io.done) { wl_done_latched := true.B }
        //     when(metaLoader.io.done) { ml_done_latched := true.B }
        //     when(decoderBank.io.bank_finished) { db_done_latched := true.B }

        //     // 检查锁存的状态, 而不是瞬时信号
        //     // 只有当三个子模块都已经在历史的某个时刻完成了任务, 才继续
        //     when(wl_done_latched && ml_done_latched && db_done_latched) {

        //         // 生产者和消费者都完成了,可以进行下一轮翻转
        //         state := State.sFlip
        //         printf(
        //           p"[Top] sRun Finished (WL=${wl_done_latched}, ML=${ml_done_latched}, DB=${db_done_latched}). Flipping.\n"
        //         )

        //         // [CRITICAL] 清除记分板, 为下一轮 (Tile 1) 做准备
        //         wl_done_latched := false.B
        //         ml_done_latched := false.B
        //         db_done_latched := false.B
        //     }
        // }

        is(State.sDone) {
            io.done := true.B
            when(!io.start) {
                state := State.sIdle
            }
        }
    }
}
