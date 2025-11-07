package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

class ParallelReconstructorArrayIO(p: PDUParams) extends Bundle {
    // --- 输入 ---
    /** 从 MicroDecoder 阵列接收的 N 个 q 值 */
    val final_q_vec = Input(Vec(p.unrollFactor, UInt(p.qValWidth.W)))

    /** 从 MicroDecoder 阵列接收的 N 个 r 值 */
    val r_vec = Input(Vec(p.unrollFactor, UInt(p.rValWidth.W)))

    /** 从 PDU 顶层锁存器广播来的 k_in 值 (0或1) */
    val k_in = Input(UInt(p.kInWidth.W))

    /** 从 PDU 顶层锁存器广播来的 zp 值 */
    val zp = Input(UInt(4.W))

    // --- 输出 ---
    /** N 个并行重构出的最终 4-bit 权重 */
    val final_weights_vec = Output(Vec(p.unrollFactor, UInt(4.W)))
}

class ParallelReconstructorArray(p: PDUParams) extends Module {
    val io = IO(new ParallelReconstructorArrayIO(p))

    // 1. 广播信号
    val k_val = io.k_in + 1.U(p.kValWidth.W)
    val zp_sint = io.zp.asSInt // 将zp转换为有符号整数一次

    // 2. 并行实例化 N 个重构单元 (纯组合逻辑)
    for (i <- 0 until p.unrollFactor) {

        // --- 步骤 A: 合并q和r, 得到 unsigned_delta ---
        val q_i = io.final_q_vec(i)
        val r_i = io.r_vec(i)

        // 我们知道 deltaMax <= 30, 所以 5-bit 足够
        // val unsigned_delta =
        //     (q_i << k_val) | r_i(p.kValWidth - 2, 0) // 确保r_i被正确截断

        val r_mask = (1.U << k_val) - 1.U
        val r_masked = r_i & r_mask

        // 步骤 A: 合并q和r, 得到unsigned_delta
        val unsigned_delta = (q_i << k_val) | r_masked

        // --- 步骤 B: 逆映射 (Unsigned-to-Signed), 逻辑来自 verify_decoder.py ---
        val is_even = (unsigned_delta(0) === 0.U)
        // 最终的 signed_delta 范围为 [-15, 15], 5-bit有符号数足够
        val signed_delta = Wire(SInt(5.W))

        when(is_even) {
            // 正数或0: signed = unsigned / 2 (右移1位)
            signed_delta := (unsigned_delta >> 1).asSInt
        }.otherwise {
            // 负数: signed = -(unsigned + 1) / 2
            signed_delta := -(((unsigned_delta + 1.U) >> 1).asSInt)
        }

        // --- 步骤 C: 添加零点, 得到最终权重 ---
        // (signed_delta) + (zp) -> 结果范围约 [-15+0, 15+15] -> [-15, 30]
        // .asUInt(4.W) 会自动处理截断和环绕 (e.g., -1 -> 15)
        val tmp = (signed_delta + zp_sint).asUInt
        io.final_weights_vec(i) := tmp(3, 0)
    }
}
