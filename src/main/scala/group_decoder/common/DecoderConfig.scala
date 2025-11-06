package group_decoder.common

import chisel3._
import chisel3.util.log2Ceil

/** 包含解码器所有模块的通用配置参数.
  *
  * @param P_WIDTH
  *   并行处理的总宽度 (例如 8 或 16)
  * @param N_VLD_WIDTH
  *   VLD 级联的深度 (例如 4 或 8)
  * @param Q_WIDTH_MAX
  *   "Slow Path" 优先编码器需要查看的最大 q 宽度. 基于 analy.py, q_max=15,因此需要 16 bits.
  * @param K_WIDTH_MAX
  *   'k' 的最大值 (k=1, 2, 3).
  * @param DELTA_WIDTH
  *   "mapped_delta" 的位宽. 基于 analy.py, max=30,需要 5 bits.
  * @param FAST_PATH_MAX_BITS
  *   "Fast Path" LUT 需要检查的最大比特数 (<= 4 bits).
  */
case class DecoderConfig(
    val P_WIDTH: Int = 8,
    val N_VLD_WIDTH: Int = 4,
    val Q_WIDTH_MAX: Int = 16,
    val K_WIDTH_MAX: Int = 3,
    val DELTA_WIDTH: Int = 5,
    val FAST_PATH_MAX_BITS: Int = 4
) {
    // --- 派生参数 (Derived Parameters) ---

    /** 'k' 输入端口的位宽 (k=0,1,2,3 -> 2 bits) */
    val kValueWidth: Int = log2Ceil(K_WIDTH_MAX + 1)

    /** "mapped_delta" 输出端口的位宽 */
    val mappedDeltaWidth: Int = DELTA_WIDTH

    /** "bits_consumed" 输出端口的位宽 最坏情况: q(15) + 1 + k(3) = 19 bits.log2(20) = 5
      * bits.
      */
    val bitsConsumedWidth: Int = log2Ceil(Q_WIDTH_MAX + 1 + K_WIDTH_MAX + 1)

    /** GRDecodeSlice 需要 "查看" 的最小位流宽度. 由 "Slow Path" 的最坏情况决定: q(16 bits) + r(3
      * bits) = 19 bits.
      */
    val streamWidthIn: Int = Q_WIDTH_MAX + K_WIDTH_MAX
}
