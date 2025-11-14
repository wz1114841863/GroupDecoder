package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/** PE (Processing Element) 的 IO 端口
  */
class PEIO(val p: PEParams) extends Bundle {
    // --- 数据流动: 激活 (西 -> 东) ---
    val A_in = Input(SInt(p.actWidth.W)) // 激活输入 (INT8)
    val A_out = Output(SInt(p.actWidth.W)) // 激活输出 (INT8)

    // --- 数据流动: 部分和 (北 -> 南) ---
    val O_in = Input(SInt(p.accWidth.W)) // 部分和输入
    val O_out = Output(SInt(p.accWidth.W)) // 部分和输出

    // --- 静态加载端口 (来自 WeightLoader 和 MetadataLoader) ---
    // (这些端口用于在 "sLoadWeights" 阶段 预加载)
    val W_in = Input(UInt(p.weightWidth.W)) // 权重 (INT4)
    val ZP_in = Input(UInt(p.zpWidth.W)) // 零点 (UInt8)
    val Scale_in = Input(UInt(p.scaleWidth.W)) // 缩放因子 (BF16)

    // --- 控制信号 (来自 SA_Controller) ---
    val load_w_en = Input(Bool()) // 权重加载使能
}

/** 脉动阵列处理单元 (PE)
  *
  * 架构: 权重固定 (Weight Stationary)
  *
  * 计算: 简化的 INT4 * INT8 MAC
  */
class PE(val p: PEParams) extends Module {
    val io = IO(new PEIO(p))

    // --- 1. 内部状态寄存器 (权重固定) ---
    // 使用 RegInit 初始化
    val W_reg = RegInit(0.U(p.weightWidth.W))
    val ZP_reg = RegInit(0.U(p.zpWidth.W))
    val Scale_reg = RegInit(0.U(p.scaleWidth.W))

    // --- 2. 流水线寄存器 (数据流动) ---
    val A_reg = RegInit(0.S(p.actWidth.W))
    val O_reg = RegInit(0.S(p.accWidth.W))

    // --- 3. 权重加载逻辑 ---
    when(io.load_w_en) {
        W_reg := io.W_in
        ZP_reg := io.ZP_in
        Scale_reg := io.Scale_in
    }

    // --- 4. 激活流动逻辑 (西 -> 东) ---
    A_reg := io.A_in
    io.A_out := A_reg // 输出 T-1 周期的激活

    // --- 5. 部分和流动逻辑 (北 -> 南) ---
    // O_reg (部分和) 与 A_reg (激活) 同步延迟
    O_reg := io.O_in

    // MAC (使用 T-1 锁存的 A_reg 和 W_reg)
    val mac_result = (W_reg.asSInt * A_reg)

    // O_out 是组合逻辑
    // (使用 T-1 锁存的 O_reg 和 T-1 的 mac_result)
    io.O_out := O_reg + mac_result
}
