package group_decoder

import chisel3._
import chisel3.util._

case class GroupDecoderParams(
    groupSize: Int = 32,
    nBits: Int = 4,
    kOptions: Seq[Int] = Seq(1, 2, 3),
    zpWidth: Int = 4,
    brp: BitstreamReaderParams =
        BitstreamReaderParams(streamWidth = 32, bufferWidth = 64),
    gcp: GolombRiceCoreParams =
        GolombRiceCoreParams(maxK = 3, maxQuotient = 31, outputWidth = 5)
) {
    val numKChoices: Int = kOptions.size
    val flagBits: Int = log2Ceil(numKChoices + 1)
    val flagToKMap: Seq[(UInt, UInt)] = kOptions.zipWithIndex.map {
        case (k, i) => i.U -> k.U
    }
    val fallbackFlagVal: Int = numKChoices
}

class GroupDecoderIO(params: GroupDecoderParams) extends Bundle {
    val start = Input(Bool())
    val zero_point = Input(UInt(params.zpWidth.W))
    val inputStream = Flipped(Decoupled(UInt(params.brp.streamWidth.W)))
    val quantized_weight = Decoupled(UInt(params.nBits.W))
    val busy = Output(Bool())
    val done = Output(Bool())
}

class GroupDecoder(params: GroupDecoderParams) extends Module {
    val io = IO(new GroupDecoderIO(params))

    val bitReader = Module(new BitstreamReader(params.brp))
    val golombCore = Module(new GolombRiceCore(params.gcp))
    val deltaRecon = Module(new DeltaReconstructor())

    val sIdle :: sReadFlag :: sDecode :: sDone :: Nil = Enum(4)
    val state = RegInit(sIdle)

    val weights_decoded_reg = RegInit(0.U(log2Ceil(params.groupSize + 1).W))
    val is_raw_path_reg = RegInit(false.B)
    val k_reg = Reg(UInt(log2Ceil(params.gcp.maxK + 1).W))

    // Default assignments
    io.busy := (state =/= sIdle)
    io.done := false.B
    bitReader.io.request.valid := false.B
    bitReader.io.request.bits := 0.U
    golombCore.io.start := false.B
    golombCore.io.k := 0.U

    // --- Static Connections ---
    bitReader.io.inputStream <> io.inputStream
    deltaRecon.io.zp := io.zero_point

    // --- Dynamic Connections based on Path ---

    when(is_raw_path_reg) {
        // --- RAW PATH WIRING ---
        // Final output is connected to BitReader
        io.quantized_weight.valid := bitReader.io.bits_out.valid && (state === sDecode)
        io.quantized_weight.bits := bitReader.io.bits_out.bits
        bitReader.io.bits_out.ready := io.quantized_weight.ready && (state === sDecode)

        // The Golomb pipeline is idle. Its inputs must be given default values.
        golombCore.io.bits_in.ready := false.B // We are not consuming from Golomb path
        deltaRecon.io.unsigned_delta.valid := false.B
        deltaRecon.io.unsigned_delta.bits := 0.U
        // ** THE FIX IS HERE **
        // Give the unused module's input a defined value.
        deltaRecon.io.quantized_weight.ready := false.B

    }.otherwise {
        // --- GOLOMB PATH WIRING ---
        // Connect the full pipeline: BitReader -> GolombCore -> DeltaRecon -> Final Output
        golombCore.io.bits_in <> bitReader.io.bits_out
        deltaRecon.io.unsigned_delta <> golombCore.io.unsigned_delta
        io.quantized_weight <> deltaRecon.io.quantized_weight
    }

    // --- FSM LOGIC ---
    switch(state) {
        is(sIdle) {
            weights_decoded_reg := 0.U
            when(io.start) {
                state := sReadFlag
            }
        }

        is(sReadFlag) {
            bitReader.io.request.valid := true.B
            bitReader.io.request.bits := params.flagBits.U

            // FSM itself is the consumer of the flag, so it's always ready.
            bitReader.io.bits_out.ready := true.B

            when(bitReader.io.bits_out.fire) {
                val flag = bitReader.io.bits_out.bits
                when(flag === params.fallbackFlagVal.U) {
                    is_raw_path_reg := true.B
                }.otherwise {
                    is_raw_path_reg := false.B
                    k_reg := MuxLookup(flag, 0.U)(params.flagToKMap)
                }
                state := sDecode
            }
        }

        is(sDecode) {
            when(is_raw_path_reg) {
                bitReader.io.request.valid := true.B
                bitReader.io.request.bits := params.nBits.U
            }.otherwise {
                when(!golombCore.io.busy) {
                    golombCore.io.start := true.B
                    golombCore.io.k := k_reg
                }
            }

            when(io.quantized_weight.fire) {
                weights_decoded_reg := weights_decoded_reg + 1.U

                when(weights_decoded_reg + 1.U === params.groupSize.U) {
                    state := sDone
                }.otherwise {
                    state := sDecode
                }
            }
        }

        is(sDone) {
            io.done := true.B
            state := sIdle
        }
    }

    when(RegNext(io.start, false.B) || io.busy) {
        printf(p"====================================================\n")
        printf(
          p"State: ${state} | Decoded: ${weights_decoded_reg} | Is_Raw: ${is_raw_path_reg}\n"
        )
        printf(
          p"  [BitReader Out] valid: ${bitReader.io.bits_out.valid}, ready: ${bitReader.io.bits_out.ready}, bits: ${bitReader.io.bits_out.bits}\n"
        )
        printf(
          p"  [GolombCore In] valid: ${golombCore.io.bits_in.valid}, ready: ${golombCore.io.bits_in.ready}\n"
        )
        printf(
          p"  [Final Out]     valid: ${io.quantized_weight.valid}, ready: ${io.quantized_weight.ready}, bits: ${io.quantized_weight.bits}\n"
        )
    }
}
