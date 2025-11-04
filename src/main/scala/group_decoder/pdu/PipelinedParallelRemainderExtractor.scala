package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

// 输入Bundle
class PipelinedPREBundle_in(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Bundle {
    val peek_window = UInt(pudp.peekWindowWidth.W)
    val final_q_vec = Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    val offset_vec = Vec(p.unrollFactor, UInt(log2Ceil(pudp.peekWindowWidth).W))
    val k = UInt(2.W)
}

// 输出Bundle
class PipelinedPREBundle_out(p: PDUParams) extends Bundle {
    val r_vec = Vec(p.unrollFactor, UInt(3.W))
}

// IO接口
class PipelinedParallelRemainderExtractorIO(
    p: PDUParams,
    pudp: ParallelUnaryDecoderParams
) extends Bundle {
    val in = Flipped(Decoupled(new PipelinedPREBundle_in(p, pudp)))
    val out = Decoupled(new PipelinedPREBundle_out(p))
}

class PipelinedParallelRemainderExtractor(
    p: PDUParams,
    pudp: ParallelUnaryDecoderParams
) extends Module {
    val io = IO(new PipelinedParallelRemainderExtractorIO(p, pudp))

    // --- 流水线寄存器 ---
    // 阶段1 (S1) 的 valid 寄存器
    val valid_s1 = RegInit(false.B)
    // 阶段2 (S2) 的 valid 寄存器
    val valid_s2 = RegInit(false.B)

    // 阶段1 (S1) 的数据寄存器
    val r_start_bit_vec_s1 = Reg(
      Vec(p.unrollFactor, UInt(log2Ceil(pudp.peekWindowWidth).W))
    )
    val peek_window_s1 = Reg(UInt(pudp.peekWindowWidth.W))
    val k_val_s1 = Reg(UInt(3.W)) // k_val 最大为3

    // **关键**: 阶段2 (S2) 的数据寄存器, 它将直接驱动输出
    val r_vec_s2 = Reg(Vec(p.unrollFactor, UInt(3.W)))

    // --- 流水线控制逻辑 ---
    val ready_s2 = io.out.ready || !valid_s2
    val ready_s1 = ready_s2 || !valid_s1

    io.in.ready := ready_s1

    // --- 阶段1 (S1): 地址计算 ---
    when(ready_s1) {
        valid_s1 := io.in.valid
        when(io.in.valid) {
            // 当输入有效时, 计算并锁存S1的数据
            for (i <- 0 until p.unrollFactor) {
                r_start_bit_vec_s1(i) := io.in.bits.offset_vec(i) + io.in.bits
                    .final_q_vec(i) + 1.U
            }
            peek_window_s1 := io.in.bits.peek_window
            k_val_s1 := io.in.bits.k + 1.U
        }
    }

    // --- 阶段2 (S2): 数据提取 ---
    when(ready_s2) {
        // 将valid位从S1传递到S2
        valid_s2 := valid_s1

        // **关键**: 计算逻辑在S2级完成, 结果存入S2的寄存器
        when(valid_s1) { // 只有当从S1传来的数据有效时, 才进行计算
            for (i <- 0 until p.unrollFactor) {
                val shifted_window = peek_window_s1 << r_start_bit_vec_s1(i)
                val width = pudp.peekWindowWidth

                when(k_val_s1 === 1.U) {
                    r_vec_s2(i) := shifted_window(width - 1)
                }.elsewhen(k_val_s1 === 2.U) {
                    r_vec_s2(i) := shifted_window(width - 1, width - 2)
                }.otherwise {
                    r_vec_s2(i) := shifted_window(width - 1, width - 3)
                }
            }
        }
    }

    io.out.valid := valid_s2
    io.out.bits.r_vec := r_vec_s2
}
