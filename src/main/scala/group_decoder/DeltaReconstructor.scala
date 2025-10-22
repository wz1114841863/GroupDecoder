package group_decoder

import chisel3._
import chisel3.util._

class DeltaReconstructorIO extends Bundle {
    val unsigned_delta = Input(UInt(16.W))
    val zp = Input(UInt(4.W))
    val quantized_weight = Output(UInt(4.W))
}

class DeltaReconstructor extends Module {
    val io = IO(new DeltaReconstructorIO)

    val is_even = (io.unsigned_delta(0) === 0.U)

    val signed_delta = Wire(SInt(17.W)) // 使用足够大的有符号位宽
    when(is_even) {
        // 正数或0: signed = unsigned / 2
        signed_delta := (io.unsigned_delta >> 1).asSInt
    } .otherwise {
        // 负数: signed = -(unsigned + 1) / 2
        signed_delta := -(((io.unsigned_delta + 1.U) >> 1).asSInt)
    }

    // 加上零点,然后截断为4-bit UInt
    io.quantized_weight := (signed_delta + io.zp.asSInt).asUInt
}
