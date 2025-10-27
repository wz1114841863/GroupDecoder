package group_decoder

import chisel3._
import chisel3.util._

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
    maxQuotient: Int = 64
) {
    require(isPow2(unrollFactor), "unrollFactor必须是2的幂次以简化树形逻辑")
    require(
      streamChunkWidth > maxQuotient * unrollFactor,
      "streamChunkWidth可能不足以应对最坏情况"
    )
}
