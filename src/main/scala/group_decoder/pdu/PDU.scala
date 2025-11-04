package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

// 定义输入的"任务包"结构
class PDUTaskBundle(p: PDUParams) extends Bundle {
    val tag = UInt(16.W) // 假设最多有2^16个组
    val stream_chunk = UInt(p.streamChunkWidth.W)
    val zp = UInt(4.W)
    val k = UInt(2.W)
}

// 定义输出的"结果包"结构
class PDUResultBundle(p: PDUParams) extends Bundle {
    val tag = UInt(16.W)
    val weights = Vec(p.unrollFactor, UInt(4.W))
    val last = Bool()
}

// 最终的PDU模块IO定义
class PDU_IO(p: PDUParams) extends Bundle {
    val task_in = Flipped(Decoupled(new PDUTaskBundle(p)))
    val result_out = Decoupled(new PDUResultBundle(p))
    val error = Output(Bool())
}

// PDU模块的骨架
// PDU模块的骨架
class PDU(p: PDUParams) extends Module {
    val io = IO(new PDU_IO(p))

    // --- 1. 实例化所有流水线子模块 ---
    val unaryDecoder = Module(
      new ParallelUnaryDecoder(ParallelUnaryDecoderParams(p.peekWindowWidth))
    )
    val offsetAccumulator = Module(
      new PipelinedOffsetAccumulator(
        p,
        ParallelUnaryDecoderParams(p.peekWindowWidth)
      )
    )
    val remainderExtractor = Module(
      new PipelinedParallelRemainderExtractor(
        p,
        ParallelUnaryDecoderParams(p.peekWindowWidth)
      )
    )
    val reconstructorArray = Module(new ParallelReconstructorArray(p))

    unaryDecoder.io.peek_window := 0.U

    offsetAccumulator.io.in.valid := false.B
    offsetAccumulator.io.in.bits.peek_window := 0.U
    offsetAccumulator.io.in.bits.k := 0.U

    remainderExtractor.io.in.valid := false.B
    remainderExtractor.io.in.bits.final_q_vec.foreach(_ := 0.U)
    remainderExtractor.io.in.bits.offset_vec.foreach(_ := 0.U)
    remainderExtractor.io.in.bits.k := 0.U
    remainderExtractor.io.in.bits.peek_window := 0.U

    reconstructorArray.io.final_q_vec.foreach(_ := 0.U)
    reconstructorArray.io.r_vec.foreach(_ := 0.U)
    reconstructorArray.io.k := 0.U
    reconstructorArray.io.zp := 0.U

    // --- 2. 内部状态与核心数据通路寄存器 ---
    object State extends ChiselEnum { val sIDLE, sDECODING = Value }
    val state = RegInit(State.sIDLE)
    val input_stream_shifter = Reg(UInt(p.streamChunkWidth.W))
    val decoded_count = Reg(UInt(log2Ceil(p.groupSize + 1).W))

    // --- 3. 上下文信息流水线寄存器 ---
    val task_reg_p1 = Reg(new PDUTaskBundle(p)) // OA 输入级
    val task_reg_p2 = Reg(new PDUTaskBundle(p)) // PRE 输入级
    // q_vec 也需要同步传递, 为 P4 提供正确的输入
    val q_vec_p3 = Reg(Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W)))
    val task_reg_p3 = Reg(new PDUTaskBundle(p)) // PRA & 输出级

    // 添加一个寄存器来流水化 P1 (OA) 使用的 peek_window
    val peek_window_p2_reg = Reg(UInt(p.peekWindowWidth.W))
    // =========================================================

    // --- 4. 主FSM与任务接收 ---
    io.task_in.ready := state === State.sIDLE
    when(io.task_in.fire) {
        state := State.sDECODING
        input_stream_shifter := io.task_in.bits.stream_chunk
        task_reg_p1 := io.task_in.bits
        decoded_count := 0.U
    }

    val peek_window = input_stream_shifter(
      p.streamChunkWidth - 1,
      p.streamChunkWidth - p.peekWindowWidth
    )
    unaryDecoder.io.peek_window := peek_window

    // P1 -> P2 (OA)
    offsetAccumulator.io.in.valid := (state === State.sDECODING) && (decoded_count < p.groupSize.U)
    offsetAccumulator.io.in.bits.peek_window := peek_window
    offsetAccumulator.io.in.bits.k := task_reg_p1.k

    // P2 -> P3 (PRE)
    remainderExtractor.io.in.valid := offsetAccumulator.io.out.valid
    offsetAccumulator.io.out.ready := remainderExtractor.io.in.ready
    remainderExtractor.io.in.bits.final_q_vec := offsetAccumulator.io.out.bits.final_q_vec
    remainderExtractor.io.in.bits.offset_vec := offsetAccumulator.io.out.bits.offset_vec
    remainderExtractor.io.in.bits.k := task_reg_p2.k

    // 使用流水化传递过来的 peek_window_p2_reg
    remainderExtractor.io.in.bits.peek_window := peek_window_p2_reg

    // P3 -> P4 (PRA)
    reconstructorArray.io.final_q_vec := q_vec_p3
    reconstructorArray.io.r_vec := remainderExtractor.io.out.bits.r_vec
    reconstructorArray.io.k := task_reg_p3.k
    reconstructorArray.io.zp := task_reg_p3.zp

    // --- 上下文流水线传递 ---
    when(offsetAccumulator.io.in.fire) {
        task_reg_p2 := task_reg_p1
        peek_window_p2_reg := peek_window
    }
    when(remainderExtractor.io.in.fire) {
        task_reg_p3 := task_reg_p2
        q_vec_p3 := remainderExtractor.io.in.bits.final_q_vec // 将q_vec也同步锁存
    }

    // --- 移位器反馈控制 ---
    when(offsetAccumulator.io.out.fire) {
        input_stream_shifter := input_stream_shifter << offsetAccumulator.io.out.bits.total_consumed_bits
    }

    // --- 计数器与状态更新 ---
    val is_last_dispatch =
        (decoded_count + (p.unrollFactor * 2).U) >= p.groupSize.U
    when(io.result_out.fire) {
        decoded_count := decoded_count + p.unrollFactor.U
        when(io.result_out.bits.last) {
            state := State.sIDLE
        }
    }

    // --- 6. 连接最终输出 ---
    io.result_out.valid := remainderExtractor.io.out.valid
    // **核心修正**: 将外部的ready信号连接到流水线末端
    remainderExtractor.io.out.ready := io.result_out.ready

    io.result_out.bits.weights := reconstructorArray.io.final_weights_vec
    io.result_out.bits.tag := task_reg_p3.tag

    // last信号的同步传递
    val last_signal_p2 =
        RegEnable(is_last_dispatch, offsetAccumulator.io.in.fire)
    val last_signal_p3 =
        RegEnable(last_signal_p2, remainderExtractor.io.in.fire)
    io.result_out.bits.last := last_signal_p3

    io.error := RegNext(
      offsetAccumulator.io.out.fire && offsetAccumulator.io.out.bits.error,
      init = false.B
    )
}
