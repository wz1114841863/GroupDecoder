package group_decoder.pdu

import chisel3._
import chisel3.util._

import group_decoder.common._

class PDU(p: PDUParams) extends Module {
    val io = IO(new PDU_IO(p))

    // 我们将stageWidth硬编码为 unrollFactor/2
    val sp = DecoderStageParams(stageWidth = p.unrollFactor / 2)
    require(p.unrollFactor % 2 == 0, "unrollFactor必须为偶数")

    // --- 1. 实例化子模块 ---
    val stage_0 = Module(new DecoderStage(p, sp))
    val stage_1 = Module(new DecoderStage(p, sp))
    val reconstructor = Module(new ParallelReconstructorArray(p))

    // --- 2. 核心状态与数据通路 ---
    object State extends ChiselEnum { val sIDLE, sS1, sS2 = Value }
    val state = RegInit(State.sIDLE)

    val shifter_reg = Reg(UInt(p.streamChunkWidth.W))
    val decoded_count = Reg(UInt(log2Ceil(p.groupSize + 1).W))
    val k_reg = Reg(UInt(p.kInWidth.W))
    val zp_reg = Reg(UInt(4.W))
    val tag_reg = Reg(UInt(p.tagWidth.W))

    // --- 3. 流水线寄存器 ---
    // S1 (stage_0) -> S2 (stage_1)
    val s1_offset_4 = Reg(UInt(log2Ceil(p.peekWindowWidth).W))
    val s1_q_vec_0_3 = Reg(Vec(sp.stageWidth, UInt(p.qValWidth.W)))
    val s1_r_vec_0_3 = Reg(Vec(sp.stageWidth, UInt(p.rValWidth.W)))
    val s1_error = Reg(Bool())

    // S2 (stage_1) -> S3 (Reconstructor)
    val s2_q_vec_4_7 = Reg(Vec(sp.stageWidth, UInt(p.qValWidth.W)))
    val s2_r_vec_4_7 = Reg(Vec(sp.stageWidth, UInt(p.rValWidth.W)))
    val s2_total_consumed = Reg(UInt(p.lengthWidth.W)) // FIXME: 位宽
    val s2_error = Reg(Bool())

    // S3 (Output) -> io.result_out
    val s3_weights = Reg(Vec(p.unrollFactor, UInt(4.W)))
    val s3_tag = Reg(UInt(p.tagWidth.W))
    val s3_last = Reg(Bool())
    val s3_error = Reg(Bool())
    val s3_valid = RegInit(false.B)

    // =================================================================
    //  组合逻辑数据通路 (在FSM的when块中被锁存)
    // =================================================================

    // --- Stage 0 逻辑 ---
    val peek_window = shifter_reg(
      p.streamChunkWidth - 1,
      p.streamChunkWidth - p.peekWindowWidth
    )
    stage_0.io.peek_window := peek_window
    stage_0.io.start_offset_in := 0.U
    stage_0.io.k_in := k_reg

    // --- Stage 1 逻辑 ---
    stage_1.io.peek_window := peek_window
    stage_1.io.start_offset_in := s1_offset_4
    stage_1.io.k_in := k_reg

    // --- Stage 2 逻辑 ---
    reconstructor.io.final_q_vec := s1_q_vec_0_3 ## s2_q_vec_4_7
    reconstructor.io.r_vec := s1_r_vec_0_3 ## s2_r_vec_4_7
    reconstructor.io.k_in := k_reg
    reconstructor.io.zp := zp_reg

    // =================================================================
    //  FSM (控制数据流)
    // =================================================================

    // 默认连接
    io.task_in.ready := false.B
    io.result_out.valid := s3_valid
    io.result_out.bits.weights := s3_weights
    io.result_out.bits.tag := s3_tag
    io.result_out.bits.last := s3_last
    io.error := s3_error

    when(s3_valid && io.result_out.ready) {
        // 当输出被接收时, S3无效
        s3_valid := false.B
    }

    switch(state) {
        is(State.sIDLE) {
            io.task_in.ready := true.B
            when(io.task_in.fire) {
                // 锁存任务, 进入S1
                shifter_reg := io.task_in.bits.stream_chunk
                k_reg := io.task_in.bits.k
                zp_reg := io.task_in.bits.zp
                tag_reg := io.task_in.bits.tag
                decoded_count := 0.U
                state := State.sS1
            }
        }

        is(State.sS1) {
            // S1 正在计算. (stage_0 组合逻辑正在执行)
            // 在时钟沿, 锁存S1的结果, 进入S2
            when(!s2_valid || s3_ready) { // S2 空闲
                s1_offset_4 := stage_0.io.next_stage_offset_out
                s1_q_vec_0_3 := stage_0.io.final_q_vec
                s1_r_vec_0_3 := stage_0.io.final_r_vec
                s1_error := stage_0.io.error
                state := State.sS2
            }
        }

        is(State.sS2) {
            // S2 正在计算. (stage_1 组合逻辑正在执行)
            // 在时钟沿, 锁存S2的结果, 准备输出
            when(!s3_valid || io.result_out.ready) { // S3 空闲
                s2_q_vec_4_7 := stage_1.io.final_q_vec
                s2_r_vec_4_7 := stage_1.io.final_r_vec
                s2_total_consumed := s1_offset_4 + stage_1.io.total_consumed_bits_out
                s2_error := s1_error || stage_1.io.error

                // 锁存最终输出
                s3_weights := reconstructor.io.final_weights_vec
                s3_tag := tag_reg
                s3_error := s2_error
                s3_valid := true.B // 数据已准备好

                val is_last =
                    (decoded_count + p.unrollFactor.U) >= p.groupSize.U
                s3_last := is_last

                // 更新下一批数据的上下文
                shifter_reg := shifter_reg << s2_total_consumed
                decoded_count := decoded_count + p.unrollFactor.U

                when(is_last) {
                    state := State.sIDLE
                }.otherwise {
                    state := State.sS1 // 返回S1, 处理下一批
                }
            }
        }
    }
}
