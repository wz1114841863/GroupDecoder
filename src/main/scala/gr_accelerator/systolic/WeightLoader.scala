package gr_accelerator.systolic

import chisel3._
import chisel3.util._

import gr_accelerator.common._

class WeightLoaderIO(val p: WeightSRAMParams) extends Bundle {
    // --- 控制 ---
    val start = Input(Bool())
    val done = Output(Bool())

    // 使用 saReadAddrWidth (例如 14-bit), 因为 sramAddrWidth (9-bit) 不足以存储 Group ID
    // 瓦片在 SRAM 中的起始逻辑 Group ID
    val base_group_idx = Input(UInt(p.saReadAddrWidth.W))

    // --- 连接 WeightSRAM (读) ---
    val sram_read_addrs = Output(Vec(p.P, UInt(p.saReadAddrWidth.W)))
    val sram_read_data = Input(Vec(p.P, UInt(p.weightWidth.W)))

    // --- 连接 SystolicArray (写) ---
    val array_w_in = Output(Vec(p.N, UInt(p.weightWidth.W)))
    val array_load_en = Output(Bool())
}

class WeightLoader(val p: WeightSRAMParams) extends Module {
    val io = IO(new WeightLoaderIO(p))

    val N = p.N
    val P = p.P
    val SubBlocks = N / P

    object State extends ChiselEnum {
        val sIdle, sRead, sWait, sLatchData, sPush, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // 计数器
    val row_idx = Reg(UInt(log2Ceil(N + 1).W))
    val chunk_idx = Reg(UInt(log2Ceil(SubBlocks + 1).W))

    // 行缓冲
    val row_buffer = Reg(Vec(N, UInt(p.weightWidth.W)))

    // 默认输出
    io.done := false.B
    io.array_load_en := false.B
    io.array_w_in := row_buffer
    for (i <- 0 until P) io.sram_read_addrs(i) := 0.U

    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                state := State.sRead
                row_idx := 0.U
                chunk_idx := 0.U
            }
        }

        is(State.sRead) {
            // 发起 P 个并行读取
            // 目标: 读取当前行 (row_idx) 在 P 个不同列 (Groups) 中的值
            // 当前块覆盖的列范围: [chunk_idx*P, (chunk_idx+1)*P - 1]
            for (i <- 0 until P) {
                // 1. 计算当前的逻辑 Group ID
                //    = 基础 Group + (当前块 * 每块P列) + 当前端口偏移
                val current_group_id =
                    io.base_group_idx + (chunk_idx * P.U) + i.U

                // 2. 计算发送给 WeightSRAM 的逻辑地址
                //    WeightSRAM 会自动将其映射到物理 Bank
                //    Addr = (Group_ID * GroupSize) + Row_Index
                io.sram_read_addrs(
                  i
                ) := (current_group_id * p.groupSize.U) + row_idx
            }

            // WeightSRAM 延迟 = 2 (SyncReadMem + RegNext)
            // sRead (Req) -> sWait (Wait) -> sLatchData (Valid)
            state := State.sWait

            // printf(
            //   p"[WeightLoader] Chunk $chunk_idx: Requesting Group ${io.base_group_idx + (chunk_idx * P.U)} (Row $row_idx)\n"
            // )
        }

        is(State.sWait) {
            // 等待 1 周期
            state := State.sLatchData
        }

        is(State.sLatchData) {
            // 锁存数据
            val base_pos = chunk_idx * P.U
            for (i <- 0 until P) {
                val full_idx = base_pos +& i.U
                val truncated_idx = full_idx(log2Ceil(N) - 1, 0)
                row_buffer(truncated_idx) := io.sram_read_data(i)
            }

            // 检查是否读完一行的所有块
            if (SubBlocks == 1) {
                state := State.sPush
                chunk_idx := 0.U
            } else {
                when(chunk_idx === (SubBlocks - 1).U) {
                    state := State.sPush
                    chunk_idx := 0.U
                }.otherwise {
                    state := State.sRead // 继续读下一块
                    chunk_idx := chunk_idx + 1.U
                }
            }
        }

        is(State.sPush) {
            // 推送一行到阵列
            io.array_load_en := true.B

            // [DEBUG]
            // printf(p"[WeightLoader] Pushing Row $row_idx to Array:\n")
            // printf(p"  W[0]=${row_buffer(0)} W[1]=${row_buffer(1)} ...\n")
            // printf(p"  W[2]=${row_buffer(2)} W[3]=${row_buffer(3)} ...\n")

            // 检查是否完成所有行
            when(row_idx === (N - 1).U) {
                state := State.sDone
            }.otherwise {
                state := State.sRead
                row_idx := row_idx + 1.U
            }
        }

        is(State.sDone) {
            io.done := true.B
            when(!io.start) {
                state := State.sIdle
            }
        }
    }
}
