package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import gr_accelerator.common._

/** PESpec (PE 单元) 测试模块
  */
class PESpec extends AnyFreeSpec with Matchers with ChiselSim {
    "should correctly load and flow data with correct timing" in {
        implicit val p: PEParams = PEParams.default
        simulate(new PE(p)) { dut =>
            println("--- [PESpec] 开始测试 ---")

            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.clock.step(1) // T=0 (复位完成)

            // --- 1. 周期 T=0: 加载权重 ---
            println("[T=0] 加载 W_reg = 5")
            dut.io.load_w_en.poke(true.B)
            dut.io.W_in.poke(5.U) // W_reg = 5
            dut.io.A_in.poke(0.S)
            dut.io.O_in.poke(0.S)

            // 检查 T=0 时的输出 (来自复位)
            dut.io.A_out.expect(0.S)
            dut.io.O_out.expect(0.S) // O_reg(0) + (W_reg(0) * A_reg(0)) = 0

            dut.clock.step(1) // T=0 结束, T=1 开始

            // --- 2. 周期 T=1: 泵入第一组数据 ---
            // 内部 (T=1 时钟沿锁存):
            // W_reg = 5 (来自 T=0 加载)
            // A_reg = 0 (来自 T=0 A_in)
            // O_reg = 0 (来自 T=0 O_in)
            println("[T=1] 泵入 A=2, O=10")
            dut.io.load_w_en.poke(false.B)
            dut.io.A_in.poke(2.S) // 泵入 A=2
            dut.io.O_in.poke(10.S) // 泵入 O=10

            // 检查 T=1 时的输出 (来自 T=0 的锁存值)
            dut.io.A_out.expect(0.S) // = A_reg (0)
            // O_out = O_reg(0) + (W_reg(5) * A_reg(0)) = 0 + (5 * 0) = 0
            dut.io.O_out.expect(0.S)

            dut.clock.step(1) // T=1 结束, T=2 开始

            // --- 3. 周期 T=2: 泵入第二组数据, 检查第一组结果 ---
            // 内部 (T=2 时钟沿锁存):
            // A_reg = 2 (来自 T=1 A_in)
            // O_reg = 10 (来自 T=1 O_in)
            println("[T=2] 泵入 A=-3, O=100")
            dut.io.A_in.poke(-3.S)
            dut.io.O_in.poke(100.S)

            // 检查 T=2 时的输出 (来自 T=1 的锁存值)
            dut.io.A_out.expect(2.S) // = A_reg (2)
            // [FIXED] O_out = O_reg(10) + (W_reg(5) * A_reg(2)) = 10 + 10 = 20
            dut.io.O_out.expect(20.S)

            dut.clock.step(1) // T=2 结束, T=3 开始

            // --- 4. 周期 T=3: 检查第二组结果 ---
            // 内部 (T=3 时钟沿锁存):
            // A_reg = -3 (来自 T=2 A_in)
            // O_reg = 100 (来自 T=2 O_in)
            println("[T=3] 泵入 A=0, O=0")
            dut.io.A_in.poke(0.S)
            dut.io.O_in.poke(0.S)

            // 检查 T=3 时的输出 (来自 T=2 的锁存值)
            dut.io.A_out.expect(-3.S) // = A_reg (-3)
            // [FIXED] O_out = O_reg(100) + (W_reg(5) * A_reg(-3)) = 100 - 15 = 85
            dut.io.O_out.expect(85.S)

            dut.clock.step(1) // T=3 结束, T=4 开始

            // --- 5. 周期 T=4: 检查第三组结果 ---
            // 内部 (T=4 时钟沿锁存):
            // A_reg = 0 (来自 T=3 A_in)
            // O_reg = 0 (来自 T=3 O_in)
            println("[T=4] 检查最后结果")

            // 检查 T=4 时的输出 (来自 T=3 的锁存值)
            dut.io.A_out.expect(0.S) // = A_reg (0)
            // [FIXED] O_out = O_reg(0) + (W_reg(5) * A_reg(0)) = 0 + 0 = 0
            dut.io.O_out.expect(0.S)

            println("--- [PESpec] 测试通过 ---")
        }
    }

}
