package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/** PE (Processing Element) 的 IO 端口
  */
class PEIO(val p: PEParams) extends Bundle {
    // --- 水平流动: 激活 (Activations) ---
    val in_act = Input(SInt(p.actWidth.W)) // 来自左侧
    val out_act = Output(SInt(p.actWidth.W)) // 传给右侧

    // --- 垂直流动: 部分和 (Partial Sums) ---
    val in_sum = Input(SInt(p.accWidth.W)) // 来自上方
    val out_sum = Output(SInt(p.accWidth.W)) // 传给下方

    // --- 垂直流动: 权重 & 元数据 (Weight Path) ---
    // 在加载阶段,这些数据从上方流入,传给下方
    val in_weight = Input(UInt(p.weightWidth.W))
    val in_zp = Input(UInt(p.zpWidth.W))
    val in_scale = Input(UInt(p.scaleWidth.W))

    val out_weight = Output(UInt(p.weightWidth.W))
    val out_zp = Output(UInt(p.zpWidth.W))
    val out_scale = Output(UInt(p.scaleWidth.W))

    // --- 控制信号 ---
    // true: 加载模式 (移位); false: 计算模式 (固定)
    val ctrl_load_en = Input(Bool())
}

/** 脉动阵列处理单元 (PE)
  *
  * 架构: 权重固定 (Weight Stationary)
  *
  * 计算: 简化的 INT4 * INT8 MAC
  */
class PE(val p: PEParams = PEParams.default) extends Module {
    val io = IO(new PEIO(p))

    // --- 1. 状态寄存器 (Stationary Storage) ---
    val reg_weight = RegInit(0.U(p.weightWidth.W))
    val reg_zp = RegInit(0.U(p.zpWidth.W))
    val reg_scale = RegInit(0.U(p.scaleWidth.W))

    // --- 2. 数据流寄存器 (Pipelining) ---
    val reg_act = RegInit(0.S(p.actWidth.W))
    val reg_sum = RegInit(0.S(p.accWidth.W))

    // --- 3. 权重加载逻辑 (Daisy Chain) ---
    // 这是一个移位逻辑.
    // 当 load_en 有效时,接收上方数据.
    // 无论何时,都将当前寄存器值输出给下方,形成链条.
    when(io.ctrl_load_en) {
        reg_weight := io.in_weight
        reg_zp := io.in_zp
        reg_scale := io.in_scale
    }

    // 始终输出当前存储的权重给下一级,支持链式加载
    io.out_weight := reg_weight
    io.out_zp := reg_zp
    io.out_scale := reg_scale

    // --- 4. 激活与计算逻辑 ---
    // 锁存输入激活
    reg_act := io.in_act
    io.out_act := reg_act // 传给右侧

    // MAC 计算 (简化版: INT4 * INT8 + Sum)
    // 实际硬件这里会应用 (W - ZP) * Scale,我们暂时简化验证通路
    // 注意:这里的计算逻辑在 load_en=true 时结果无效,但不影响功能
    val mac_res = (reg_weight.asSInt * reg_act)
    val sum_next = io.in_sum + mac_res

    reg_sum := sum_next
    io.out_sum := reg_sum // 传给下方
}
