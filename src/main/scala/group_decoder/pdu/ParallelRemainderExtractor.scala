package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

class ParallelRemainderExtractorIO(
    p: PDUParams,
    pudp: ParallelUnaryDecoderParams
) extends Bundle {
    // 从PDU主数据通路接收的原始数据窥视窗口
    val peek_window = Input(UInt(pudp.peekWindowWidth.W))
    // 从OffsetAccumulator接收的N个权重的最终q值
    val final_q_vec = Input(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    )
    // 从OffsetAccumulator接收的N个权重在本周期窥视窗口内的相对起始偏移
    val offset_vec = Input(
      Vec(p.unrollFactor, UInt(log2Ceil(pudp.peekWindowWidth).W))
    )
    // 从PDU主控制器接收的当前组的k值
    val k = Input(UInt(2.W))

    // --- 输出 ---
    // N个并行提取出的余数r值
    // k_val最大为3 (k_in=2 -> k_val=3), 所以r最多需要3比特
    val r_vec = Output(Vec(p.unrollFactor, UInt(3.W)))
}

class ParallelRemainderExtractor(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Module {
    val io = IO(new ParallelRemainderExtractorIO(p, pudp))

    val k_val = io.k + 1.U

    for (i <- 0 until p.unrollFactor) {
        // 1. r_start_bit 的计算仍然正确, 它代表从MSB(最左边)开始的偏移
        val r_start_bit = io.offset_vec(i) + io.final_q_vec(i) + 1.U

        // a. 将整个peek_window向左动态移位, 使r_i的第一个比特对齐到窗口的最高位(MSB)
        val shifted_window = io.peek_window << r_start_bit

        // b. 从移位后的窗口的最高位开始, 提取k_val个比特
        //    例如, k_val=3, 我们需要提取 [width-1, width-3] 这3个比特
        val extracted_r = Wire(UInt(3.W))
        val width = pudp.peekWindowWidth

        when(k_val === 1.U) {
            extracted_r := shifted_window(width - 1)
        }.elsewhen(k_val === 2.U) {
            extracted_r := shifted_window(width - 1, width - 2)
        }.otherwise { // k_val === 3.U
            extracted_r := shifted_window(width - 1, width - 3)
        }

        io.r_vec(i) := extracted_r
    }
}
