package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

class WeightLoaderIO(val p: WeightSRAMParams) extends Bundle {
    // --- 控制 ---
    val start = Input(Bool())
    val done = Output(Bool())

    // 使用 saReadAddrWidth
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

    val SRAM_LATENCY = 2

    if (P > 1) {
        assert(
          io.base_group_idx(log2Ceil(P) - 1, 0) === 0.U,
          "WeightLoader base_group_idx must be aligned to P!"
        )
    }

    object State extends ChiselEnum {
        val sIdle, sBurst, sDrain, sDone = Value
    }
    val state = RegInit(State.sIdle)

    val req_row_idx = RegInit(0.U(log2Ceil(N).W))
    val req_chunk_idx = RegInit(0.U(log2Ceil(SubBlocks).W))
    val resp_row_idx = RegInit(0.U(log2Ceil(N).W))
    val resp_chunk_idx = RegInit(0.U(log2Ceil(SubBlocks).W))
    val inflight_cnt = RegInit(
      0.U(log2Ceil(N * SubBlocks + SRAM_LATENCY + 1).W)
    )

    val row_buffer = Reg(Vec(N, UInt(p.weightWidth.W)))
    val load_en_reg = RegInit(false.B)

    io.done := false.B

    io.array_load_en := load_en_reg
    io.array_w_in := row_buffer

    val req_valid = (state === State.sBurst)
    val data_valid = ShiftRegister(req_valid, SRAM_LATENCY, false.B, true.B)

    // --- 统一处理 inflight_cnt 更新 ---
    val req_fired = req_valid
    when(req_fired && !data_valid) {
        inflight_cnt := inflight_cnt + 1.U
    }.elsewhen(!req_fired && data_valid) {
        inflight_cnt := inflight_cnt - 1.U
    }.otherwise {
        inflight_cnt := inflight_cnt
    }

    // 默认: 每一拍自动拉低 (产生脉冲)
    load_en_reg := false.B

    for (i <- 0 until P) io.sram_read_addrs(i) := (i * p.groupSize).U

    // --- 调试打印 ---
    // when(load_en_reg) {
    //     printf(p"[WeightLoader] Push Row: ${row_buffer}\n")
    // }

    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                state := State.sBurst
                req_row_idx := 0.U
                req_chunk_idx := 0.U
                resp_row_idx := 0.U
                resp_chunk_idx := 0.U
            }
        }

        is(State.sBurst) {
            for (i <- 0 until P) {
                val current_group_id =
                    io.base_group_idx + (req_chunk_idx * P.U) + i.U
                io.sram_read_addrs(
                  i
                ) := (current_group_id * p.groupSize.U) + req_row_idx
            }

            val req_chunk_wrap = req_chunk_idx === (SubBlocks - 1).U
            val req_row_wrap = req_row_idx === (N - 1).U

            req_chunk_idx := Mux(req_chunk_wrap, 0.U, req_chunk_idx + 1.U)

            when(req_chunk_wrap) {
                req_row_idx := req_row_idx + 1.U
            }

            when(req_chunk_wrap && req_row_wrap) {
                state := State.sDrain
            }
        }

        is(State.sDrain) {
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

    when(data_valid) {
        val base_pos = resp_chunk_idx * P.U

        for (i <- 0 until P) {
            val full_idx = base_pos +& i.U
            val truncated_idx = full_idx(log2Ceil(N) - 1, 0)

            if (i + (SubBlocks - 1) * P < N) {
                row_buffer(truncated_idx) := io.sram_read_data(i)
            } else {
                when(full_idx < N.U) {
                    row_buffer(truncated_idx) := io.sram_read_data(i)
                }
            }
        }

        val resp_chunk_wrap = resp_chunk_idx === (SubBlocks - 1).U
        resp_chunk_idx := Mux(resp_chunk_wrap, 0.U, resp_chunk_idx + 1.U)

        when(resp_chunk_wrap) {
            resp_row_idx := resp_row_idx + 1.U
            load_en_reg := true.B
        }
    }
}
