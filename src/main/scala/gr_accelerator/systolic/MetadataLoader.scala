package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

class MetadataLoaderIO(val p: MetaSRAMParams) extends Bundle {
    // --- 控制 ---
    val start = Input(Bool())
    val done = Output(Bool())
    val base_group_idx = Input(UInt(p.addrWidth.W)) // 起始 Group ID

    // --- 连接 MetaSRAM (读) ---
    val sram_read_addr = Output(UInt(p.addrWidth.W))
    val sram_read_zp = Input(UInt(p.zpWidth.W))
    val sram_read_scale = Input(UInt(p.scaleWidth.W))

    // --- 连接 SystolicArray (写) ---
    // 并行输出 N 个 ZP 和 Scale
    val array_zp_out = Output(Vec(p.N, UInt(p.zpWidth.W)))
    val array_scale_out = Output(Vec(p.N, UInt(p.scaleWidth.W)))
}

class MetadataLoader(val p: MetaSRAMParams) extends Module {
    val io = IO(new MetadataLoaderIO(p))

    // --- 内部缓冲 ---
    // 存储 N 列的元数据
    val zp_buffer = Reg(Vec(p.N, UInt(p.zpWidth.W)))
    val scale_buffer = Reg(Vec(p.N, UInt(p.scaleWidth.W)))

    // --- 状态机 ---
    object State extends ChiselEnum {
        val sIdle, sRead, sWait, sCapture, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // 计数器: 当前加载到第几列 (0..N-1)
    val col_cnt = Reg(UInt(log2Ceil(p.N + 1).W))
    val wait_cnt = Reg(UInt(2.W))

    // --- 默认输出 ---
    io.done := false.B
    io.sram_read_addr := 0.U
    io.array_zp_out := zp_buffer
    io.array_scale_out := scale_buffer

    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                state := State.sRead
                col_cnt := 0.U
            }
        }

        is(State.sRead) {
            // 读取第 col_cnt 列的元数据
            // 这里的映射假设 MetaSRAM 的地址直接对应 Group ID
            // 实际应用中可能需要映射逻辑 (wave * P + core ...)
            // 假设外部已经把 MetaSRAM 整理好了,或者地址就是线性的 Group ID
            io.sram_read_addr := io.base_group_idx + col_cnt

            wait_cnt := 1.U // SyncReadMem 延迟
            state := State.sWait
        }

        is(State.sWait) {
            when(wait_cnt === 0.U) {
                state := State.sCapture
            }.otherwise {
                wait_cnt := wait_cnt - 1.U
            }
        }

        is(State.sCapture) {
            // 锁存数据
            zp_buffer(col_cnt) := io.sram_read_zp
            scale_buffer(col_cnt) := io.sram_read_scale

            when(col_cnt === (p.N - 1).U) {
                state := State.sDone
            }.otherwise {
                col_cnt := col_cnt + 1.U
                state := State.sRead
            }
        }

        is(State.sDone) {
            // 加载完成,保持输出有效
            io.done := true.B
            // 可以在这里添加握手逻辑,等待下一次 start 信号重置
            when(!io.start) {
                state := State.sIdle
            }
        }
    }
}
