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

    // SRAM 延迟: SyncReadMem(1) = 1 Cycle
    // 注意: MetaSRAMBuffer 直接输出了 SyncReadMem 的结果,没有加 RegNext
    // 所以延迟是 1,而不是 WeightSRAM 的 2
    val SRAM_LATENCY = 1

    object State extends ChiselEnum {
        val sIdle, sBurst, sDrain, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // --- 计数器 ---
    val req_idx = RegInit(0.U(log2Ceil(p.N).W)) // 请求地址计数
    val resp_idx = RegInit(0.U(log2Ceil(p.N).W)) // 接收地址计数

    // 飞行计数器 (In-Flight Requests)
    val inflight_cnt = RegInit(0.U(log2Ceil(p.N + SRAM_LATENCY + 1).W))

    // 输出缓冲
    val zp_buffer = Reg(Vec(p.N, UInt(p.zpWidth.W)))
    val scale_buffer = Reg(Vec(p.N, UInt(p.scaleWidth.W)))

    // --- 默认输出 ---
    io.done := false.B
    io.sram_read_addr := io.base_group_idx // 默认输出基地址

    io.array_zp_out := zp_buffer
    io.array_scale_out := scale_buffer

    // --- 状态机 ---
    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                state := State.sBurst
                req_idx := 0.U
                resp_idx := 0.U
                inflight_cnt := 0.U
            }
        }

        is(State.sBurst) {
            // --- 1. 发射逻辑 ---
            // 每周期发射一个地址
            io.sram_read_addr := io.base_group_idx + req_idx

            // 更新请求计数
            when(req_idx === (p.N - 1).U) {
                state := State.sDrain // 发完了
            }.otherwise {
                req_idx := req_idx + 1.U
            }
        }

        is(State.sDrain) {
            // 等待数据全部收回
            when(inflight_cnt === 0.U) {
                state := State.sDone
            }
        }

        is(State.sDone) {
            io.done := true.B
            when(!io.start) {
                state := State.sIdle
            }
        }
    }

    // --- 接收逻辑 (Response Pipeline) ---
    val req_valid = (state === State.sBurst)
    val data_valid = ShiftRegister(req_valid, SRAM_LATENCY, false.B, true.B)

    // 更新 inflight_cnt
    when(req_valid && !data_valid) {
        inflight_cnt := inflight_cnt + 1.U
    }.elsewhen(!req_valid && data_valid) {
        inflight_cnt := inflight_cnt - 1.U
    }.otherwise {
        inflight_cnt := inflight_cnt
    }

    // 数据接收与写入
    when(data_valid) {
        zp_buffer(resp_idx) := io.sram_read_zp
        scale_buffer(resp_idx) := io.sram_read_scale

        // 响应计数递增 (不需要 wrap,因为 N 个收完 FSM 就走了)
        resp_idx := resp_idx + 1.U
    }
}
