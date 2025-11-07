class PDUTaskBundle(p: PDUParams) extends Bundle {
    val tag = UInt(p.tagWidth.W)
    val stream_chunk = UInt(p.streamChunkWidth.W)
    val zp = UInt(4.W)
    val k = UInt(p.kInWidth.W) // 注意: 这里是k_in (0或1)
}

class PDUResultBundle(p: PDUParams) extends Bundle {
    val tag = UInt(p.tagWidth.W)
    val weights = Vec(p.unrollFactor, UInt(4.W))
    val last = Bool()
}

class PDU_IO(p: PDUParams) extends Bundle {
    val task_in = Flipped(Decoupled(new PDUTaskBundle(p)))
    val result_out = Decoupled(new PDUResultBundle(p))
    val error = Output(Bool())
}
