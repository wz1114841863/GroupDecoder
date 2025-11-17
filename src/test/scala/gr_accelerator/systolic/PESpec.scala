package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import gr_accelerator.common._

/** PESpec (PE 单元) 测试模块 针对新的权重固定 (Weight Stationary) 架构进行验证
  */
class PESpec extends AnyFreeSpec with Matchers with ChiselSim {
    implicit val p: PEParams = PEParams.default

    "PE (Weight Stationary, Streaming)" - {
        "should correctly LOAD weights (Shift Mode)" in {
            simulate(new PE(p)) { dut =>
                println("--- [PESpec] Testing Load/Shift Mode ---")

                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                // 1. 开启加载模式
                dut.io.ctrl_load_en.poke(true.B)

                // 2. 输入数据 (模拟从上方 PE 传入)
                val w_val = 5
                val zp_val = 2
                val scale_val = 10

                dut.io.in_weight.poke(w_val.U)
                dut.io.in_zp.poke(zp_val.U)
                dut.io.in_scale.poke(scale_val.U)

                // 3. Step 1 (寄存器锁存)
                dut.clock.step(1)

                // 4. 验证输出 (模拟传给下方 PE)
                // 由于 io.out_weight := reg_weight,且 reg_weight 在时钟沿更新
                // 所以 T+1 时刻输出应该等于 T 时刻的输入
                dut.io.out_weight.expect(w_val.U)
                dut.io.out_zp.expect(zp_val.U)
                dut.io.out_scale.expect(scale_val.U)

                println("   Load verified: Data latched and output correctly.")
            }
        }

        "should correctly COMPUTE and FLOW data" in {
            simulate(new PE(p)) { dut =>
                println("--- [PESpec] Testing Compute/Flow Mode ---")

                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                // --- Phase 1: 加载权重 ---
                dut.io.ctrl_load_en.poke(true.B)
                dut.io.in_weight.poke(4.U) // Weight = 4
                dut.clock.step(1)
                dut.io.ctrl_load_en.poke(false.B) // 锁定权重

                // 验证权重已锁定
                dut.io.out_weight.expect(4.U)

                // --- Phase 2: 执行计算 (Pipeline Check) ---
                // 我们的 PE 逻辑:
                // 1. reg_act := io.in_act
                // 2. mac_res = reg_weight * reg_act
                // 3. reg_sum := io.in_sum + mac_res
                // 4. io.out_sum := reg_sum

                // T=0: 输入激活
                val act_val = 3
                dut.io.in_act.poke(act_val.S) // A = 3
                dut.io.in_sum.poke(0.S)

                dut.clock.step(1)

                // T=1:
                // reg_act 现在是 3 (来自 T=0 的输入)
                // 验证激活传递 (水平流)
                dut.io.out_act.expect(act_val.S)

                // 此时 MAC 正在计算: 4 (Weight) * 3 (reg_act) = 12
                // 我们需要在 T=1 提供 in_sum 来与这个 MAC 结果相加
                val sum_in_val = 10
                dut.io.in_sum.poke(sum_in_val.S)

                dut.clock.step(1)

                // T=2:
                // reg_sum 现在应该锁存了: in_sum(T=1) + (Weight * reg_act(T=1))
                // Result = 10 + (4 * 3) = 22
                dut.io.out_sum.expect(22.S)

                println(
                  "   Compute verified: MAC calculation and pipeline delay are correct."
                )
            }
        }
    }
}
