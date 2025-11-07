package group_decoder.pdu

import chisel3._
import chisel3.util._
// 导入我们共享的公共参数
import group_decoder.common.PDUParams

/** DecoderStage 的特有参数
  * @param stageWidth
  *   此流水线阶段内部串联的MicroDecoder的数量
  */
case class DecoderStageParams(
    stageWidth: Int = 4 // 默认为4, 对应架构中 8/2 = 4
)

class DecoderStageIO(p: PDUParams, sp: DecoderStageParams) extends Bundle {
    // --- 输入 ---
    /** 从PDU顶层(或上一级流水线)传来的共享数据窗口 */
    val peek_window = Input(UInt(p.peekWindowWidth.W))

    /** 本阶段的起始比特偏移量 (对于S1, 这是0; 对于S2, 这是S1的输出) */
    val start_offset_in = Input(UInt(log2Ceil(p.peekWindowWidth).W))

    /** 任务的 k 值 (k_in=0 -> k=1, k_in=1 -> k=2) */
    val k_in = Input(UInt(p.kInWidth.W))

    // --- 输出 ---
    /** 本阶段解码出的 q 值向量 */
    val final_q_vec = Output(Vec(sp.stageWidth, UInt(p.qValWidth.W)))

    /** 本阶段解码出的 r 值向量 */
    val final_r_vec = Output(Vec(sp.stageWidth, UInt(p.rValWidth.W)))

    /** 本阶段所有权重消耗的总比特数 */
    val total_consumed_bits_out = Output(
      UInt(log2Ceil(p.peekWindowWidth).W)
    ) // FIXME: 位宽可能需要调整

    /** 为下一个流水线阶段准备的起始偏移量 */
    val next_stage_offset_out = Output(UInt(log2Ceil(p.peekWindowWidth).W))

    /** 异常信号 */
    val error = Output(Bool())
}

class DecoderStage(p: PDUParams, sp: DecoderStageParams) extends Module {
    val io = IO(new DecoderStageIO(p, sp))

    // --- 1. 实例化 N (stageWidth) 个 MicroDecoder ---
    val microDecoders = Seq.fill(sp.stageWidth)(Module(new MicroDecoder(p)))

    // --- 2. 内部连线 ---
    val current_offsets = Wire(
      Vec(sp.stageWidth + 1, UInt(log2Ceil(p.peekWindowWidth).W))
    )
    val current_lengths = Wire(Vec(sp.stageWidth, UInt(p.lengthWidth.W)))
    val current_qs = Wire(Vec(sp.stageWidth, UInt(p.qValWidth.W)))
    val current_rs = Wire(Vec(sp.stageWidth, UInt(p.rValWidth.W)))
    val current_errors = Wire(Vec(sp.stageWidth, Bool()))

    // --- 3. 构建组合逻辑依赖链 (The "Domino Chain") ---

    // a. 链条的起点
    current_offsets(0) := io.start_offset_in

    // b. 生成链条的主体 (并行生成 N 块串联的电路)
    for (i <- 0 until sp.stageWidth) {
        // 连接 MicroDecoder 的输入
        microDecoders(i).io.peek_window := io.peek_window
        microDecoders(i).io.start_offset := current_offsets(i)
        microDecoders(i).io.k_in := io.k_in

        // 收集 MicroDecoder 的输出
        current_lengths(i) := microDecoders(i).io.length
        current_qs(i) := microDecoders(i).io.final_q
        current_rs(i) := microDecoders(i).io.final_r
        current_errors(i) := microDecoders(i).io.error

        // **关键依赖**: 计算下一个 MicroDecoder 的起始偏移量
        current_offsets(i + 1) := current_offsets(i) + current_lengths(i)
    }

    // --- 4. 连接到模块的输出端口 ---
    io.final_q_vec := current_qs
    io.final_r_vec := current_rs
    io.error := current_errors.reduce(_ || _) // 任何一个出错, 整体就出错

    // 本阶段的总消耗比特数
    // 注意: 这不是一个简单的 'sum', 而是最后一个偏移量减去第一个
    io.total_consumed_bits_out := current_offsets(
      sp.stageWidth
    ) - io.start_offset_in

    // 为下一个阶段准备的起始偏移量
    io.next_stage_offset_out := current_offsets(sp.stageWidth)
}
