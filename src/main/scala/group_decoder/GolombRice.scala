package group_decoder

import chisel3._
import chisel3.util._

case class GolombRiceCoreParams(
    maxK: Int = 3, // k 的最大值为 3
    maxQuotient: Int = 31, // 商 q 的最大值设为 31 (5 bits)
    outputWidth: Int = 16 // unsigned_delta 的位宽为 16
)

class GolombRiceCoreIO(params: GolombRiceCoreParams) extends Bundle {
    val start = Input(Bool())
    // k_reg 的位宽现在是 log2Ceil(3+1) = 2 bits
    val k = Input(UInt(log2Ceil(params.maxK + 1).W))

    // bits_in 的位宽是 k 的最大可能值
    val bits_in = Flipped(Decoupled(UInt(params.maxK.W)))
    val request_bits = Valid(UInt(log2Ceil(params.maxK + 1).W))

    // 输出端口现在是 5 bits
    val unsigned_delta = Decoupled(UInt(params.outputWidth.W))
    val busy = Output(Bool())
}

class GolombRiceCore(params: GolombRiceCoreParams) extends Module {
    val io = IO(new GolombRiceCoreIO(params))

    val sIdle :: sDecodeQuotient :: sDecodeRemainder :: sCalculate :: Nil =
        Enum(4)
    val state = RegInit(sIdle)

    // 内部寄存器的位宽也随之优化
    val k_reg = Reg(UInt(log2Ceil(params.maxK + 1).W))
    val q_reg = Reg(UInt(log2Ceil(params.maxQuotient + 1).W))
    val r_reg = Reg(UInt(params.maxK.W))

    // Default outputs
    io.request_bits.valid := false.B
    io.request_bits.bits := 0.U
    io.unsigned_delta.valid := false.B
    io.unsigned_delta.bits := 0.U
    io.busy := (state =/= sIdle)

    // 我什么时候准备好接收数据?当且仅当我处于需要数据的状态时.
    io.bits_in.ready := (state === sDecodeQuotient || state === sDecodeRemainder)

    // FSM logic
    switch(state) {
        is(sIdle) {
            when(io.start) {
                state := sDecodeQuotient
                k_reg := io.k
                q_reg := 0.U // Reset quotient for the new value
            }
        }

        is(sDecodeQuotient) {
            // Always request 1 bit to read the unary-coded quotient
            io.request_bits.valid := true.B
            io.request_bits.bits := 1.U

            when(io.bits_in.fire) {
                when(io.bits_in.bits === 1.U) {
                    // Got a '1', increment quotient and stay in this state
                    q_reg := q_reg + 1.U
                    state := sDecodeQuotient // Stay
                }.otherwise {
                    // Got a '0', quotient is done, move to read remainder
                    state := sDecodeRemainder
                }
            }
        }

        is(sDecodeRemainder) {
            // Request k bits for the remainder
            io.request_bits.valid := true.B
            io.request_bits.bits := k_reg

            when(io.bits_in.fire) {
                r_reg := io.bits_in.bits
                state := sCalculate
            }
        }

        is(sCalculate) {
            // Calculate final value and present it on the output
            val result = (q_reg << k_reg) | r_reg
            io.unsigned_delta.valid := true.B
            io.unsigned_delta.bits := result

            // Handshake with the downstream module
            when(io.unsigned_delta.ready) {
                state := sIdle // Data accepted, return to idle
            }
        }
    }
}
