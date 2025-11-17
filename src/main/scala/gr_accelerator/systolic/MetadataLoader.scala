package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/*
 * MetadataLoader 从 MetaSRAMBuffer 中加载 ZP 和 Scale 元数据
 * 并将它们提供给 SystolicArray
 */
class MetadataLoaderIO(val p: MetaSRAMParams) extends Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val base_group_idx = Input(UInt(p.addrWidth.W)) // 起始 Group ID

    val sram_read_addr = Output(UInt(p.addrWidth.W))
    val sram_read_zp = Input(UInt(p.zpWidth.W))
    val sram_read_scale = Input(UInt(p.scaleWidth.W))

    val array_zp_out = Output(Vec(p.N, UInt(p.zpWidth.W)))
    val array_scale_out = Output(Vec(p.N, UInt(p.scaleWidth.W)))
}

class MetadataLoader(val p: MetaSRAMParams) extends Module {
    val io = IO(new MetadataLoaderIO(p))

    val zp_buffer = Reg(Vec(p.N, UInt(p.zpWidth.W)))
    val scale_buffer = Reg(Vec(p.N, UInt(p.scaleWidth.W)))

    object State extends ChiselEnum {
        val sIdle, sRead, sCapture, sDone = Value
    }
    val state = RegInit(State.sIdle)

    val col_cnt = Reg(UInt(log2Ceil(p.N + 1).W))

    io.done := false.B
    // 默认地址设为当前 col_cnt 对应的地址,保持稳定
    io.sram_read_addr := io.base_group_idx + col_cnt

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
            // T: 给出地址 (默认逻辑已处理)
            // io.sram_read_addr := io.base_group_idx + col_cnt

            // MetaSRAMBuffer (SyncReadMem) Latency = 1 cycle.
            // T (Addr) -> T+1 (Data Valid)
            // 所以直接跳转到 sCapture
            state := State.sCapture
        }

        is(State.sCapture) {
            // T+1: 锁存数据
            val idx = col_cnt(log2Ceil(p.N) - 1, 0)
            printf(
              p"[MetaLoader] Capture: col_cnt=$col_cnt idx=$idx ZP=${io.sram_read_zp}\n"
            )
            zp_buffer(idx) := io.sram_read_zp
            scale_buffer(idx) := io.sram_read_scale

            when(col_cnt === (p.N - 1).U) {
                state := State.sDone
            }.otherwise {
                col_cnt := col_cnt + 1.U
                state := State.sRead // 读取下一个
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
