package group_decoder

import chisel3._
import chisel3.util._

class ParallelReconstructorArrayIO(p: PDUParams) extends Bundle {
    // --- 输入 ---
    // 从OffsetAccumulator接收的N个权重的最终q值
    val final_q_vec = Input(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    )
    // 从ParallelRemainderExtractor接收的N个并行提取出的余数r值
    val r_vec = Input(Vec(p.unrollFactor, UInt(3.W)))
    // 从PDU主控制器接收的当前组的k值
    val k = Input(UInt(2.W))
    // 从PDU主控制器接收的当前组的零点zp值
    val zp = Input(UInt(4.W))

    // --- 输出 ---
    // N个并行重构出的最终4-bit权重
    val final_weights_vec = Output(Vec(p.unrollFactor, UInt(4.W)))
}

class ParallelReconstructorArray(p: PDUParams) extends Module {
    val io = IO(new ParallelReconstructorArrayIO(p))

    // 实际的k值 (1, 2, or 3)
    val k_val = io.k + 1.U

    // 并行处理N个权重
    for (i <- 0 until p.unrollFactor) {
        // --- 步骤 A: 合并q和r, 得到unsigned_delta ---
        val q_i = io.final_q_vec(i)
        val r_i = io.r_vec(i)
        val unsigned_delta = (q_i << k_val) | r_i

        // --- 步骤 B: 逆映射, 得到signed_delta ---
        val is_even = (unsigned_delta(0) === 0.U)
        // 使用足够大的位宽以避免有符号数溢出
        val signed_delta = Wire(SInt((p.maxQuotient + 5).W))

        when(is_even) {
            // 正数或0: signed = unsigned / 2 (右移1位)
            signed_delta := (unsigned_delta >> 1).asSInt
        }.otherwise {
            // 负数: signed = -(unsigned + 1) / 2
            signed_delta := -(((unsigned_delta + 1.U) >> 1).asSInt)
        }

        // --- 步骤 C: 添加零点, 得到最终权重 ---
        // 将zp也转换为SInt进行有符号加法, 然后截断结果为4-bit UInt
        io.final_weights_vec(i) := (signed_delta + io.zp.asSInt).asUInt
    }
}
