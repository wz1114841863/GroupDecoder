package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import gr_accelerator.common._

class SystolicArraySpec extends AnyFreeSpec with Matchers with ChiselSim {

    // 定义一个小规模的 4x4 阵列用于测试
    val testSAParams = SystolicArrayParams(N = 4, weightWidth = 4)
    implicit val peParams: PEParams = PEParams.default

    "SystolicArray (4x4)" - {

        "should correct load weights and compute matmul" in {
            simulate(new SystolicArray(testSAParams, peParams)) { dut =>
                val N = 4
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                println("--- [SystolicArray] Start Test (4x4) ---")

                // --- 1. 准备数据 ---
                // 权重矩阵 W (4x4) - 全部设为 1,方便调试
                // W = [[1, 1, 1, 1],
                //      [1, 1, 1, 1],
                //      [1, 1, 1, 1],
                //      [1, 1, 1, 1]]
                val weights = Array.fill(N, N)(1)

                // 激活矩阵 A (输入序列长度 L=4, 维度 D=4)
                // A = [[1, 2, 3, 4],  (Time 0 vector)
                //      [1, 2, 3, 4],  (Time 1 vector)
                //      ...
                // 为了简单,我们只输入一个向量 A_vec = [1, 2, 3, 4]
                // 预期结果 O = A_vec * W
                // O[0] = 1*1 + 2*1 + 3*1 + 4*1 = 10
                // O[1] = 10, O[2] = 10, O[3] = 10
                val act_vec = Array(1, 2, 3, 4)

                // --- 2. 加载权重 (Load Phase) ---
                println("[Test] Loading Weights...")
                dut.io.ctrl_load_en.poke(true.B)

                // 我们需要 N 个周期来填满阵列.
                // 移位方向是 Top -> Bottom.
                // 第 1 个周期推入的数据最终会到达 Row 3 (最底部).
                // 第 4 个周期推入的数据会留在 Row 0 (最顶部).
                // 所以我们需要倒序推入行:Row 3, Row 2, Row 1, Row 0.

                for (r <- (0 until N).reverse) {
                    for (c <- 0 until N) {
                        dut.io.in_weight(c).poke(weights(r)(c).U)
                        dut.io.in_zp(c).poke(0.U)
                        dut.io.in_scale(c).poke(1.U)
                    }
                    dut.clock.step(1)
                }

                // 加载完成,关闭加载使能
                dut.io.ctrl_load_en.poke(false.B)
                println("[Test] Weights Loaded.")

                // --- 3. 执行计算 (Compute Phase) ---
                println("[Test] Streaming Activations...")

                // 初始化部分和输入为 0
                for (c <- 0 until N) dut.io.in_sum(c).poke(0.S)

                // 我们需要按 "对角线" (Skew) 方式输入激活
                // T=0: Row 0 输入 act[0]
                // T=1: Row 1 输入 act[1]
                // T=2: Row 2 输入 act[2]
                // T=3: Row 3 输入 act[3]

                // 我们运行 N + N + 5 个周期,确保结果流出
                val totalCycles = 15
                val capturedOutput = Array.fill(N)(0)

                for (cycle <- 0 until totalCycles) {
                    // 设置当前周期的激活输入
                    for (r <- 0 until N) {
                        // 检查当前行 r 是否应该接收数据
                        // 数据 act_vec[r] 应该在 cycle == r 时进入 Row r
                        if (cycle == r) {
                            dut.io.in_act(r).poke(act_vec(r).S)
                        } else {
                            dut.io.in_act(r).poke(0.S)
                        }
                    }

                    // 检查输出
                    // 理论上,结果应该在 cycle = N + N + Latency 附近出现
                    // Row 0 计算开始于 T=0
                    // Row 3 计算开始于 T=3
                    // 结果从底部流出.
                    // 第 0 列的结果涉及:
                    //   PE(0,0) @ T0 -> PE(1,0) @ T1 -> ... -> PE(3,0) @ T3 -> Out @ T4 (假设无额外延迟)
                    // 实际上 PE 内部 O_reg 有 1 周期延迟.
                    // PE(0,0) 计算 A0*W0, 结果在 T=1 出现在 O_out.
                    // PE(1,0) 在 T=1 接收 O_in, 并接收 A1 (因为我们 Skew 了 A1 在 T=1 到达 Row 1).
                    // PE(1,0) 结果在 T=2 出现在 O_out.
                    // ...
                    // PE(3,0) 结果在 T=4 出现在 O_out.
                    // 所以,如果我们在 T=r 输入 A[r],结果应该在 T = 2*N 左右有效?

                    // 让我们简单地打印非零输出
                    if (cycle > 0) {
                        for (c <- 0 until N) {
                            val out = dut.io.out_sum(c).peek().litValue.toInt
                            if (out != 0) {
                                println(
                                  s"   [Cycle $cycle] Col $c Output: $out"
                                )
                                capturedOutput(c) = out
                            }
                        }
                    }

                    dut.clock.step(1)
                }

                // --- 4. 验证结果 ---
                val expectedSum = 10 // 1+2+3+4
                println(s"[Test] Captured: ${capturedOutput.mkString(",")}")

                for (c <- 0 until N) {
                    assert(
                      capturedOutput(c) == expectedSum,
                      s"Col $c expected $expectedSum but got ${capturedOutput(c)}"
                    )
                }
                println("[Test] All checks passed!")
            }
        }
    }
}
