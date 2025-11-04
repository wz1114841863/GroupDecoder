package group_decoder.common

import chisel3._
import chisel3.util._

/** MDU (内存管理与分发单元) 的特有参数
  *
  * @param metadataSRAMDepth
  *   索引表/ZP表SRAM的最大条目数 (e.g., 4096, 对应4096个组)
  * @param stagingBufferDepth
  *   原始权重流暂存缓冲的深度 (e.g., 8192, 对应8KB)
  * @param grTaskQueueDepth
  *   Golomb-Rice任务队列的深度 (e.g., 16)
  * @param fbHoldBufferSlots
  *   Fallback保持缓冲的槽位数 (e.g., 16)
  * @param addrWidth
  *   DRAM地址位宽 (e.g., 64)
  * @param dataWidth
  *   AXI数据总线位宽 (e.g., 512 bits / 64 bytes)
  */
case class MDUParams(
    metadataSRAMDepth: Int = 4096,
    stagingBufferDepth: Int = 8192,
    grTaskQueueDepth: Int = 16,
    fbHoldBufferSlots: Int = 16,
    addrWidth: Int = 64,
    dataWidth: Int = 512
) {
    require(
      isPow2(metadataSRAMDepth),
      "metadataSRAMDepth必须是2的幂次以简化地址计算"
    )
    require(
      isPow2(stagingBufferDepth),
      "stagingBufferDepth必须是2的幂次以简化地址计算"
    )
}
