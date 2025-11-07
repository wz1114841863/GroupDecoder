package group_decoder.common

import chisel3._
import chisel3.util._

/** PDU及其所有子模块共享的参数化配置. 融合了DecoderConfig和analy.py的洞察.
  *
  * @param unrollFactor
  *   "超展开"系数 (P_WIDTH), 每个周期并行解码的权重数, e.g., 8.
  * @param groupSize
  *   解码组的权重数量, e.g., 512.
  * @param tagWidth
  *   任务标签(Group ID)的位宽, e.g., 16.
  *
  * --- 基于 analy.py 和 DecoderConfig 的数据驱动参数 ---
  * @param kMax
  *   硬件支持的最大k值 (k=1, 2). 我们从数据得知k=3永不使用.
  * @param qMax
  *   硬件支持的最大q值 (q=15).
  * @param deltaMax
  *   硬件支持的最大mapped_delta (delta=30).
  * @param fastPathMaxBits
  *   LUT快车道处理的最大编码长度 (<= 4 bits).
  */
case class PDUParams(
    unrollFactor: Int = 8,
    groupSize: Int = 512,
    tagWidth: Int = 16,
    kMax: Int = 2,
    qMax: Int = 15,
    deltaMax: Int = 30,
    fastPathMaxBits: Int = 4,
    peekWindowWidth: Int = 256
) {
    // --- 派生参数 (Derived Parameters) ---

    /** k 输入端口的位宽 (k_in=0 -> k=1, k_in=1 -> k=2) */
    val kInWidth: Int = log2Ceil(kMax) // 1 bit

    /** k 值的真实位宽 (k=1, 2) */
    val kValWidth: Int = log2Ceil(kMax + 1) // 2 bits

    /** q 输出端口的位宽 (0-15) */
    val qValWidth: Int = log2Ceil(qMax + 1) // 4 bits

    /** r 输出端口的位宽 (k=2时, r=0,1,2,3 -> 2 bits) */
    // **修正**: k=2时, r=0,1,2,3 (2 bits). k=1时, r=0,1 (1 bit).
    // 我们取最大值, 故r位宽为2 bits. 您的k=3(3-bit)已根据analy.py移除.
    val rValWidth: Int = kMax

    /** 'length' (bits_consumed) 输出端口的位宽 最坏情况: q(15) + 1 + k(2) = 18 bits.
      * log2(19) = 5 bits.
      */
    val lengthWidth: Int = log2Ceil(qMax + 1 + kMax + 1) // 5 bits

    /** MicroDecoder 需要 "查看" 的最小位流宽度. 由 "Slow Path" 的最坏情况决定: q(15+1 bits) + r(2
      * bits) = 18 bits.
      */
    val microDecoderStreamWidth: Int = (qMax + 1) + kMax // 18 bits

    /** PDU顶层 'peek_window' 的总位宽. 必须足以容纳 N 个权重在最坏情况下的级联. e.g., 8 * 18 bits = 144
      * bits. 我们取一个对齐的更大值.
      */
    // val peekWindowWidth: Int = 256

    /** PDU顶层 'stream_chunk' 的总位宽. 必须大于 peekWindow. */
    val streamChunkWidth: Int = 1024
}
