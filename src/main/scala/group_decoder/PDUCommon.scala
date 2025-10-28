package group_decoder

import chisel3._
import chisel3.util._

// MSB-first

/** 存放PDU及其所有子模块共享的参数化配置, 确保整个设计体系的参数一致性.
  *
  * @param groupSize
  *   解码组的权重数量, e.g., 512.
  * @param unrollFactor
  *   "超展开"系数, 每个周期并行解码的权重数, e.g., 8.
  * @param streamChunkWidth
  *   MDU提供的压缩比特流切片的位宽, e.g., 1024.
  * @param tagWidth
  *   任务标签(Group ID)的位宽, e.g., 16.
  * @param maxQuotient
  *   单个权重允许的最大商q值, 用于异常检测, e.g., 64.
  */
case class PDUParams(
    groupSize: Int = 512,
    unrollFactor: Int = 8,
    streamChunkWidth: Int = 1024,
    tagWidth: Int = 16,
    maxQuotient: Int = 64,
    peekWindowWidth: Int = 256
) {
    require(isPow2(unrollFactor), "unrollFactor必须是2的幂次以简化树形逻辑")
    require(
      streamChunkWidth > maxQuotient * unrollFactor,
      "streamChunkWidth可能不足以应对最坏情况"
    )
}

/** ParallelUnaryDecoder的模块参数
  * @param peekWindowWidth
  *   检测窗口的总位宽, e.g., 256.
  * @param segmentWidth
  *   每个处理段的位宽, e.g., 32.
  */
case class ParallelUnaryDecoderParams(
    peekWindowWidth: Int = 256,
    segmentWidth: Int = 32
) {
    require(
      peekWindowWidth % segmentWidth == 0,
      "peekWindowWidth必须是segmentWidth的整数倍"
    )
    val segmentCount = peekWindowWidth / segmentWidth
}

/** 单个处理段的输出结果
  * @param p
  *   ParallelUnaryDecoder的配置参数
  */
class SegmentDecodeBundle(p: ParallelUnaryDecoderParams) extends Bundle {
    // 在本段内找到的连续'1'的数量
    val local_q = UInt(log2Ceil(p.segmentWidth + 1).W)
    // 整个段是否全为'1'
    val is_all_ones = Bool()
}
