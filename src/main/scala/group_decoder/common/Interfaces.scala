package group_decoder.common

import chisel3._
import chisel3.util._

/** MDU -> PDU 的 Golomb-Rice 任务包
  */
class PDUTaskBundle(p: PDUParams) extends Bundle {
    val tag = UInt(p.tagWidth.W)
    val stream_chunk = UInt(p.streamChunkWidth.W)
    val zp = UInt(4.W)
    val k = UInt(2.W)
}

/** MDU -> ROB 的 Fallback "快车道" 数据包
  */
class FallbackDataBundle(p: PDUParams) extends Bundle {
    val tag = UInt(p.tagWidth.W)
    val raw_data = UInt((p.groupSize * 4).W)
}

/** PDU -> ROB 的结果包
  */
class PDUResultBundle(p: PDUParams) extends Bundle {
    val tag = UInt(p.tagWidth.W)
    val weights = Vec(p.unrollFactor, UInt(4.W))
    val last = Bool()
}

/** 简化的AXI4 Master 读接口 Bundle
  */
class AXIMasterRead(p: MDUParams) extends Bundle {
    // 读地址通道
    val ar = Decoupled(new Bundle {
        val addr = UInt(p.addrWidth.W)
        val len = UInt(8.W) // 突发长度 (0-255)
    })
    // 读数据通道
    val r = Flipped(Decoupled(new Bundle {
        val data = UInt(p.dataWidth.W)
        val last = Bool()
    }))
}
