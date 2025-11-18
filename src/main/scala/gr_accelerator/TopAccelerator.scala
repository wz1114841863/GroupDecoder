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
    // 例如: 32 表示需要处理 32 个 Tile 的数据
    val total_tiles = Input(UInt(32.W))

    // --- 2. 数据加载接口 (DMA 模拟 - 仅仅是连线) ---
    // 连接到 DecoderBank.shared_cache (Little-Endian Byte Stream)
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

    // 调试状态输出
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
    // 2. 外部加载接口连线 (透传)
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
    // 1. 权重推入 (控制加载使能)
    systolicArray.io.in_weight := weightLoader.io.array_w_in
    systolicArray.io.ctrl_load_en := weightLoader.io.array_load_en

    // 2. 元数据推入 (ZP/Scale)
    // 注意: 必须在 ctrl_load_en 有效期间保持稳定
    systolicArray.io.in_zp := metaLoader.io.array_zp_out
    systolicArray.io.in_scale := metaLoader.io.array_scale_out

    // 3. 部分和初始化 (设为 0, 支持 Bias 可在此修改)
    systolicArray.io.in_sum := VecInit(Seq.fill(p.N)(0.S))

    // --- Path D: Compute IO (外部激活 -> 阵列 -> 外部结果) ---
    systolicArray.io.in_act := io.act_in
    io.sum_out := systolicArray.io.out_sum

    // ==============================================================================
    // 4. 顶层主控 FSM (Ping-Pong 流水线调度)
    // ==============================================================================

    object State extends ChiselEnum {
        val sIdle, sPrefill, sFlip, sLoadMeta, sRun, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // 计数器: 当前处理了多少个 Tile (生产计数)
    val tile_cnt = RegInit(0.U(32.W))

    // 双缓冲翻转信号寄存器 (0: A产B耗, 1: B产A耗)
    val flip_reg = RegInit(false.B)

    // 排空计数器,用于最后一个 Tile
    val drain_cnt = RegInit(0.U(8.W))

    // 完成状态记分板 (Scoreboard)
    val wl_done_latched = RegInit(false.B)
    val db_done_latched = RegInit(false.B)

    // --- 默认控制信号 ---
    // DecoderBank
    decoderBank.io.start_req.valid := false.B
    decoderBank.io.start_req.bits.base_group_index := 0.U
    // 每次解码一个 Tile (N行)
    decoderBank.io.start_req.bits.num_groups_to_decode := (p.N).U

    // WeightLoader
    weightLoader.io.start := false.B
    // 关键: WeightSRAM 每次 Flip 后, Decoder 都会从 Wave 0 开始写
    // 所以 Loader 始终从逻辑 Group 0 (对应 Wave 0) 开始读
    weightLoader.io.base_group_idx := 0.U

    // MetaLoader
    metaLoader.io.start := false.B
    metaLoader.io.base_group_idx := 0.U

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
                flip_reg := false.B // 初始状态: 产A, 耗B (虽然B是空的)

                // 启动流水线第一步: 预填充 (Prefill)
                // 解码 Tile 0 到 Buffer A
                decoderBank.io.start_req.valid := true.B
                decoderBank.io.start_req.bits.base_group_index := 0.U

                state := State.sPrefill
                // printf("[Top] Start -> sPrefill (Decoding Tile 0)\n")
            }
        }

        is(State.sPrefill) {
            // 等待 DecoderBank 完成第一块 Tile 的解码
            when(decoderBank.io.bank_finished) {
                state := State.sFlip
                // printf("[Top] Prefill Done -> sFlip\n")
            }
        }

        // is(State.sFlip) {
        //     // 1. 翻转 Buffer
        //     flip_reg := !flip_reg

        //     // 2. 更新计数器 (表示第 N 个 Tile 已进入计算就绪状态)
        //     tile_cnt := tile_cnt + 1.U

        //     // 3. 重置记分板
        //     wl_done_latched := false.B
        //     db_done_latched := false.B

        //     // 4. 启动 MetaLoader (无论是否是最后一块, 都要加载当前块的 Meta)
        //     metaLoader.io.start := true.B

        //     // 5. 检查是否是最后一块 (Pipeline Draining)
        //     when(tile_cnt === io.total_tiles) {
        //         // [Fix] 已经是最后一块 Tile 了,不需要再启动 Decoder
        //         // 我们只需要计算当前 Buffer 里的数据
        //         // 伪造 Decoder 完成信号, 防止 sRun 卡死
        //         db_done_latched := true.B
        //         state := State.sLoadMeta
        //         // printf("[Top] Last Tile Reached. Draining pipeline.\n")
        //     }.otherwise {
        //         // [Normal] 还有 Tile 需要解码
        //         // 并行启动 DecoderBank (解码下一块 Tile)
        //         // 下一块 Tile 的全局 Group ID = (tile_cnt) * N
        //         decoderBank.io.start_req.valid := true.B
        //         decoderBank.io.start_req.bits.base_group_index := tile_cnt * p.N.U

        //         state := State.sLoadMeta
        //         // printf(p"[Top] Flip -> sLoadMeta. Decoding Next Tile (ID: $tile_cnt)\n")
        //     }
        // }

        is(State.sFlip) {
            flip_reg := !flip_reg
            val next_tile_cnt = tile_cnt + 1.U
            tile_cnt := next_tile_cnt

            wl_done_latched := false.B
            db_done_latched := false.B

            // 重置排空计数器
            drain_cnt := 0.U

            metaLoader.io.start := true.B

            when(next_tile_cnt === io.total_tiles) {
                // 最后一个 Tile: 不启动 Decoder
                // 注意: 我们不再直接设置 db_done_latched = true
                // 而是让它在 sRun 里通过计数器来变高
                state := State.sLoadMeta
            }.otherwise {
                decoderBank.io.start_req.valid := true.B
                decoderBank.io.start_req.bits.base_group_index := next_tile_cnt * p.N.U
                state := State.sLoadMeta
            }
        }

        is(State.sLoadMeta) {
            // 持续捕获 Decoder 完成信号 (可能比 MetaLoader 早完成)
            // 注意: 如果是最后一块, db_done_latched 已经在 sFlip 置为 true
            when(decoderBank.io.bank_finished) { db_done_latched := true.B }

            // 等待元数据加载完成
            when(metaLoader.io.done) {
                // Meta 好了, 现在启动 WeightLoader 开始推入权重到阵列
                weightLoader.io.start := true.B
                state := State.sRun
                // printf("[Top] Meta Loaded -> sRun (Start WeightLoader)\n")
            }
        }

        is(State.sRun) {
            when(weightLoader.io.done) { wl_done_latched := true.B }

            // Decoder 完成信号逻辑
            // 如果不是最后一个 Tile,等待 Decoder 硬件信号
            // 如果是最后一个 Tile,我们使用 drain_cnt 模拟延迟
            when(tile_cnt === io.total_tiles) {
                // 计数直到 64 (足够 Flush + Compute)
                when(drain_cnt < 100.U) { // 给足 100 个周期
                    drain_cnt := drain_cnt + 1.U
                }.otherwise {
                    db_done_latched := true.B
                }
            }.otherwise {
                // 正常模式: 等待 Decoder
                when(decoderBank.io.bank_finished) { db_done_latched := true.B }
            }

            when(wl_done_latched && db_done_latched) {
                when(tile_cnt === io.total_tiles) {
                    state := State.sDone
                }.otherwise {
                    state := State.sFlip
                }
            }

            // ==============================================================================
            // [DEBUG] 关键信号探针
            // ==============================================================================

            // 1. 监控进入阵列的接口信号 (Gatekeeper)
            // 只在权重真正加载进阵列的那几个周期打印 (ctrl_load_en = true)
            // 且只看第 0 列 (因为所有列的 ZP 行为通常是一致的)
            // when(weightLoader.io.array_load_en) {
            //     printf(
            //       p"[Top Monitor] Loading Array @ Tile $tile_cnt: " +
            //           p"Weight=${weightLoader.io.array_w_in(0)} | " +
            //           p"ZP_In=${metaLoader.io.array_zp_out(0)} | " +
            //           p"Scale_In=${metaLoader.io.array_scale_out(0)}\n"
            //     )
            // }

            // // 2. 监控 MetaLoader 的完成状态 (确认它是否过早复位)
            // // 当 WeightLoader 正在加载时,MetaLoader 应该保持 Done 状态或者 Idle 状态,且输出必须稳定
            // when(weightLoader.io.array_load_en) {
            //     // 打印 MetaLoader 的输出是否还在变?
            //     val zp_current = metaLoader.io.array_zp_out(0)
            //     val zp_prev = RegNext(zp_current)
            //     when(zp_current =/= zp_prev) {
            //         printf(
            //           p"[Top WARNING] ZP Changed during loading! $zp_prev -> $zp_current\n"
            //         )
            //     }
            // }
        }

        is(State.sDone) {
            io.done := true.B
            when(!io.start) {
                state := State.sIdle
            }
        }
    }
}
