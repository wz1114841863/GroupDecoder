package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class DeltaReconstructorSpec extends AnyFreeSpec with Matchers with ChiselSim {

    /** 软件 "黄金模型" (保持不变)
      */
    def goldenModel(unsignedDelta: Int, zp: Int): Int = {
        val signedDelta = if (unsignedDelta % 2 == 0) {
            unsignedDelta / 2
        } else {
            -(unsignedDelta + 1) / 2
        }
        val result = signedDelta + zp
        result & 0xf
    }

    "DeltaReconstructor with Decoupled IO should correctly reconstruct weights" in {
        simulate(new DeltaReconstructor) { dut =>
            /** [修改点]新的辅助函数,用于驱动 Decoupled 接口
              */
            def testCombination(unsignedDelta: Int, zp: Int): Unit = {
                val expected = goldenModel(unsignedDelta, zp)

                // 1. 准备输入数据和 valid 信号
                dut.io.unsigned_delta.valid.poke(true.B)
                dut.io.unsigned_delta.bits.poke(unsignedDelta.U)
                dut.io.zp.poke(zp.U)

                // 2. 模拟下游模块总是准备好接收数据
                dut.io.quantized_weight.ready.poke(true.B)

                // 3. 验证输出的 valid 和 bits 是否都正确
                //    由于是纯组合逻辑,在一个 valid 的输入周期,输出也应该是 valid 的
                dut.io.quantized_weight.valid.expect(
                  true.B,
                  "Output should be valid when input is valid"
                )
                dut.io.quantized_weight.bits.expect(
                  expected.U,
                  s"Failed for unsigned_delta=$unsignedDelta, zp=$zp. Expected $expected, got ${dut.io.quantized_weight.bits.peek().litValue}"
                )

                // 4. 驱动时钟并撤销输入信号,为下一次测试做准备
                dut.clock.step(1)
                dut.io.unsigned_delta.valid.poke(false.B)
            }

            println("--- Testing Decoupled DeltaReconstructor ---")

            // --- 核心测试用例保持不变,因为它们测试的是计算逻辑 ---

            // Test Case 1: Zero delta
            println("Testing zero delta...")
            testCombination(unsignedDelta = 0, zp = 5)
            testCombination(unsignedDelta = 0, zp = 10)

            // Test Case 2: Positive deltas
            println("Testing positive deltas...")
            testCombination(unsignedDelta = 2, zp = 5)
            testCombination(unsignedDelta = 10, zp = 1)

            // Test Case 3: Negative deltas
            println("Testing negative deltas...")
            testCombination(unsignedDelta = 1, zp = 5)
            testCombination(unsignedDelta = 9, zp = 15)

            // Test Case 4: Testing wrap-around
            println("Testing wrap-around (modulo) arithmetic...")
            testCombination(unsignedDelta = 6, zp = 14) // 14 + 3 = 17 -> 1
            testCombination(unsignedDelta = 7, zp = 2) // 2 - 4 = -2 -> 14

            // Test Case 5: Exhaustive test
            println("Testing exhaustively against all zero-points...")
            for (zp_val <- 0 to 15) {
                testCombination(unsignedDelta = 6, zp = zp_val)
                testCombination(unsignedDelta = 7, zp = zp_val)
            }

            // [新增测试]验证反压信号的传递
            println("Testing backpressure (ready signal propagation)...")
            dut.io.unsigned_delta.valid.poke(true.B)
            // 模拟下游模块繁忙,不准备接收数据
            dut.io.quantized_weight.ready.poke(false.B)
            // 此时,模块应该将反压信号传递给上游
            dut.io.unsigned_delta.ready.expect(
              false.B,
              "Ready signal should propagate backwards when downstream is not ready"
            )
            dut.clock.step(1)
            // 恢复正常
            dut.io.quantized_weight.ready.poke(true.B)
            dut.io.unsigned_delta.ready.expect(
              true.B,
              "Ready signal should be high when downstream is ready"
            )

            println("\nDecoupled DeltaReconstructor tests passed successfully!")
        }
    }
}
