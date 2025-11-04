package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

// 流水线模块的输入Bundle
class PipelinedOABundle_in(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Bundle {
    // 注意: 现在所有输入被打包在一个Bundle里
    // val segment_results = Vec(pudp.segmentCount, new SegmentDecodeBundle(pudp))
    val peek_window = UInt(pudp.peekWindowWidth.W)
    val k = UInt(2.W)
}

// 流水线模块的输出Bundle
class PipelinedOABundle_out(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Bundle {
    // 所有输出也被打包在一个Bundle里
    val final_q_vec = Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    val length_vec =
        Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 3 + 1).W))
    val offset_vec = Vec(p.unrollFactor, UInt(log2Ceil(pudp.peekWindowWidth).W))
    val total_consumed_bits = Output(
      UInt(log2Ceil(p.unrollFactor * (p.maxQuotient + 3) + 1).W)
    )
    val error = Output(Bool())
}

// PipelinedOffsetAccumulator 的IO接口
class PipelinedOffsetAccumulatorIO(
    p: PDUParams,
    pudp: ParallelUnaryDecoderParams
) extends Bundle {
    val in = Flipped(Decoupled(new PipelinedOABundle_in(p, pudp)))
    val out = Decoupled(new PipelinedOABundle_out(p, pudp))
}

class PipelinedOffsetAccumulator(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Module {
    val io = IO(new PipelinedOffsetAccumulatorIO(p, pudp))

    // --- 内部状态定义 ---
    object State extends ChiselEnum { val sIDLE, sBUSY, sDONE = Value }
    val state = RegInit(State.sIDLE)
    val weight_idx = Reg(UInt(log2Ceil(p.unrollFactor).W))

    // --- 输入与状态寄存器 ---
    val peek_window_reg = Reg(UInt(pudp.peekWindowWidth.W))
    val k_reg = Reg(UInt(2.W))
    val current_bit_ptr_reg = Reg(UInt(log2Ceil(pudp.peekWindowWidth + 1).W))

    // --- 结果累加寄存器 ---
    val final_q_vec_reg = Reg(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    )
    val length_vec_reg = Reg(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 3 + 1).W))
    )
    val error_reg = Reg(Bool())

    // =================================================================
    //  核心计算单元 (只计算一个权重, 纯组合逻辑)
    // =================================================================
    val shifted_window = peek_window_reg << current_bit_ptr_reg

    // (这部分逻辑与之前组合逻辑版本for循环内的逻辑完全相同)
    val reanalyzed_results = Wire(
      Vec(pudp.segmentCount, new SegmentDecodeBundle(pudp))
    )
    for (j <- 0 until pudp.segmentCount) {
        val high = pudp.peekWindowWidth - 1 - (j * pudp.segmentWidth)
        val low = pudp.peekWindowWidth - ((j + 1) * pudp.segmentWidth)

        val current_segment = shifted_window(high, low)
        val inverted_segment = ~current_segment
        val is_all_ones = current_segment.andR
        reanalyzed_results(j).is_all_ones := is_all_ones
        val first_zero_index = PriorityEncoder(
          Reverse(inverted_segment).asBools
        )
        reanalyzed_results(j).local_q := Mux(
          is_all_ones,
          pudp.segmentWidth.U,
          first_zero_index
        )
    }

    val q_chain = Wire(Vec(pudp.segmentCount + 1, UInt()))
    val still_searching_chain = Wire(Vec(pudp.segmentCount + 1, Bool()))
    q_chain(0) := 0.U
    still_searching_chain(0) := true.B
    for (j <- 0 until pudp.segmentCount) {
        when(still_searching_chain(j)) {
            q_chain(j + 1) := q_chain(j) + reanalyzed_results(j).local_q
            still_searching_chain(j + 1) := reanalyzed_results(j).is_all_ones
        }.otherwise {
            q_chain(j + 1) := q_chain(j)
            still_searching_chain(j + 1) := false.B
        }
    }
    val current_q = q_chain(pudp.segmentCount)
    val current_len = current_q + 1.U + (k_reg + 1.U)

    // =================================================================
    //  FSM 与 状态更新
    // =================================================================
    io.in.ready := false.B
    io.out.valid := false.B

    switch(state) {
        is(State.sIDLE) {
            io.in.ready := true.B
            when(io.in.fire) {
                state := State.sBUSY
                peek_window_reg := io.in.bits.peek_window
                k_reg := io.in.bits.k
                weight_idx := 0.U
                current_bit_ptr_reg := 0.U
                error_reg := false.B
                // **关键**: 清空结果寄存器, 防止数据污染
                final_q_vec_reg.foreach(_ := 0.U)
                length_vec_reg.foreach(_ := 0.U)
            }
        }
        is(State.sBUSY) {
            // 在每个周期, 计算并更新一个权重的结果
            final_q_vec_reg(weight_idx) := current_q
            length_vec_reg(weight_idx) := current_len
            current_bit_ptr_reg := current_bit_ptr_reg + current_len
            when(current_q > p.maxQuotient.U) { error_reg := true.B }

            weight_idx := weight_idx + 1.U

            when(weight_idx === (p.unrollFactor - 1).U) {
                // 计算完最后一个权重, 进入sDONE状态以稳定和输出结果
                state := State.sDONE
            }
        }
        is(State.sDONE) {
            // 在这个状态, 所有内部寄存器都已稳定为最终结果
            // 这是对外输出的唯一/安全时刻
            io.out.valid := true.B
            when(io.out.ready) {
                // 握手成功, 返回空闲
                state := State.sIDLE
            }
        }
    }

    // =================================================================
    //  输出连接
    // =================================================================
    val offsets = length_vec_reg.scanLeft(0.U)(_ + _).slice(0, p.unrollFactor)

    io.out.bits.final_q_vec := final_q_vec_reg
    io.out.bits.length_vec := length_vec_reg
    io.out.bits.offset_vec := VecInit(offsets)
    io.out.bits.total_consumed_bits := current_bit_ptr_reg
    io.out.bits.error := error_reg
}
