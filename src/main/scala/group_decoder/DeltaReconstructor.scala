package group_decoder

import chisel3._
import chisel3.util._

class DeltaReconstructorIO extends Bundle {
    val unsigned_delta = Flipped(Decoupled(UInt(16.W)))
    val zp = Input(UInt(4.W)) // zero-point
    val quantized_weight = Decoupled(UInt(4.W))
}

class DeltaReconstructor extends Module {
    val io = IO(new DeltaReconstructorIO)

    val is_even = (io.unsigned_delta.bits(0) === 0.U)
    val signed_delta = Wire(SInt(17.W))
    when(is_even) {
        signed_delta := (io.unsigned_delta.bits >> 1).asSInt
    }.otherwise {
        signed_delta := -(((io.unsigned_delta.bits + 1.U) >> 1).asSInt)
    }

    io.quantized_weight.bits := (signed_delta + io.zp.asSInt).asUInt
    io.quantized_weight.valid := io.unsigned_delta.valid
    io.unsigned_delta.ready := io.quantized_weight.ready
}
