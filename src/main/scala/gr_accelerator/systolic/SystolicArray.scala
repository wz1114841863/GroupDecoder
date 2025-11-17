package gr_accelerator.systolic

import chisel3._
import chisel3.util._

import gr_accelerator.common._

/* 这个模块实例化了N*N个 PE, 并按照权重固定 (WS) 架构连接它们:
 * 权重加载:
 *      从顶部(Top)输入,像波浪一样向下移位(Shift),直到填满阵列.
 * 激活流动:
 *      从左侧(Left)输入,向右脉动.
 * 部分和流动:
 *      从顶部(Top)输入(通常为 0),向下脉动累加
 * */
class SystolicArrayIO(val saParams: SystolicArrayParams, val peParams: PEParams)
    extends Bundle {
    val N = saParams.N

    // --- 顶部输入 (权重 & 部分和) ---
    // 权重按列加载 (N 列)
    val in_weight = Input(Vec(N, UInt(peParams.weightWidth.W)))
    val in_zp = Input(Vec(N, UInt(peParams.zpWidth.W)))
    val in_scale = Input(Vec(N, UInt(peParams.scaleWidth.W)))

    // 部分和初始值通常为 0
    val in_sum = Input(Vec(N, SInt(peParams.accWidth.W)))

    // --- 左侧输入 (激活) ---
    // 激活按行进入 (N 行)
    // 注意:外部需要负责 Skew (歪斜) 输入数据以匹配脉动时序
    val in_act = Input(Vec(N, SInt(peParams.actWidth.W)))

    // --- 底部输出 (最终结果) ---
    val out_sum = Output(Vec(N, SInt(peParams.accWidth.W)))

    // --- 控制 ---
    // 全局加载使能.为 true 时,权重沿列向下移位.
    // 为 false 时,权重固定,进行计算.
    val ctrl_load_en = Input(Bool())
}

class SystolicArray(
    val saParams: SystolicArrayParams = SystolicArrayParams.default,
    val peParams: PEParams = PEParams.default
) extends Module {
    val io = IO(new SystolicArrayIO(saParams, peParams))
    val N = saParams.N

    // 1. 实例化 PE 网格 (二维数组)
    // pe_grid(row)(col)
    val pe_grid = Seq.fill(N)(Seq.fill(N)(Module(new PE(peParams))))

    // 2. 互连逻辑
    for (r <- 0 until N) {
        for (c <- 0 until N) {
            val pe = pe_grid(r)(c)

            // --- 全局控制 ---
            pe.io.ctrl_load_en := io.ctrl_load_en

            // --- 垂直连接 (权重, ZP, Scale, Sum) ---
            if (r == 0) {
                // 第一行:连接到模块顶部输入
                pe.io.in_weight := io.in_weight(c)
                pe.io.in_zp := io.in_zp(c)
                pe.io.in_scale := io.in_scale(c)
                pe.io.in_sum := io.in_sum(c)
            } else {
                // 其他行:连接到上一行的输出
                pe.io.in_weight := pe_grid(r - 1)(c).io.out_weight
                pe.io.in_zp := pe_grid(r - 1)(c).io.out_zp
                pe.io.in_scale := pe_grid(r - 1)(c).io.out_scale
                pe.io.in_sum := pe_grid(r - 1)(c).io.out_sum
            }

            // --- 水平连接 (激活) ---
            if (c == 0) {
                // 第一列:连接到模块左侧输入
                pe.io.in_act := io.in_act(r)
            } else {
                // 其他列:连接到左侧 PE 的输出
                pe.io.in_act := pe_grid(r)(c - 1).io.out_act
            }
        }
    }

    // 3. 输出连接 (底部边缘)
    for (c <- 0 until N) {
        io.out_sum(c) := pe_grid(N - 1)(c).io.out_sum
    }
}
