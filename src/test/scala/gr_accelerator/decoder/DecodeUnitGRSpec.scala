package gr_accelerator.decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import gr_accelerator.common.GRDecoderConfig

/** DecodeUnit_GR_Spec测试模块
  */
class DecodeUnitGRSpec extends AnyFreeSpec with Matchers with ChiselSim {
    "DecodeUnit_GR should correctly decode GR streams" in {
        // 1. 实例化我们的共享配置
        implicit val p = GRDecoderConfig.default

        // 2. 启动模拟
        simulate(new DecodeUnitGR(p)) { dut =>
            // 3. 标准复位序列
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            println("--- 开始测试 DecodeUnit_GR (V10.5, 职责分离) ---")

            // 辅助函数: 创建一个BigInt, 从MSB(左)侧填充字符串
            def s(bitString: String): BigInt = {
                val padded = bitString.padTo(p.grChunkWidth, '0')
                BigInt(padded, 2)
            }

            // 辅助函数: 运行单个测试 (参考 MicroDecoderSpec)
            def runTest(
                testName: String,
                k_in: Int,
                bits: String, // 比特流必须在 MSB
                exp_q: Int,
                exp_r: Int,
                exp_len: Int,
                exp_err: Boolean = false
            ): Unit = {
                println(
                  s"Test ($testName): k=${k_in + 1}, bits=$bits"
                )
                dut.io.k_in.poke(k_in.U)
                dut.io.aligned_chunk.poke(s(bits))
                dut.clock.step(1)
                dut.io.final_q.expect(exp_q.U)
                dut.io.final_r.expect(exp_r.U)
                dut.io.consumed_bits_gr.expect(exp_len.U)
                dut.io.error.expect(exp_err.B)
            }

            runTest(
              "1: Fast k=1",
              k_in = 0,
              bits = "1101", // q=2, r=1
              exp_q = 2,
              exp_r = 1,
              exp_len = 4 // (q=2 + 1 + k=1) = 4
            )

            runTest(
              "2: Fast k=2",
              k_in = 1,
              bits = "1011", // q=1, r=3
              exp_q = 1,
              exp_r = 3,
              exp_len = 4 // (q=1 + 1 + k=2) = 4
            )

            runTest(
              "3: Fast k=1",
              k_in = 0,
              bits = "1111100", // q=5, r=0
              exp_q = 5,
              exp_r = 0,
              exp_len = 7 // (q=5 + 1 + k=1) = 7
            )

            runTest(
              "4: Fast k=2",
              k_in = 1,
              bits = "111001", // q=3, r=1
              exp_q = 3,
              exp_r = 1,
              exp_len = 6 // (q=3 + 1 + k=2) = 6
            )

            // Test 5: 快车道 (k=1, q=3 编码 "1110")
            runTest(
              "5: Fast Path Miss",
              k_in = 0,
              bits = "11101", // q=3, r=1
              exp_q = 3,
              exp_r = 1,
              exp_len = 5 // (q=3 + 1 + k=1) = 5
            )

            // Test 6: 慢车道边界 (q=qMax=15)
            val q15_k1_r0 =
                "1" * 15 + "0" + "0" // 15个'1', '0' (q结束), '0' (r)
            runTest(
              "6: Slow qMax k=1",
              k_in = 0,
              bits = q15_k1_r0,
              exp_q = 15,
              exp_r = 0,
              exp_len = 17 // (q=15 + 1 + k=1) = 17
            )

            // Test 7: 慢车道最坏情况 (k=kMax, q=qMax)
            val q15_k2_r3 =
                "1" * 15 + "0" + "11" // 15个'1', '0' (q结束), '11' (r=3)
            runTest(
              "7: Slow qMax k=2",
              k_in = 1,
              bits = q15_k2_r3,
              exp_q = 15,
              exp_r = 3,
              exp_len = 18 // (q=15 + 1 + k=2) = 18
            )

            // Test 8: 错误 (q > qMax)
            val q16_k1_r0 =
                "1" * 16 + "0" + "0" // 16个'1'
            runTest(
              "8: Error (q > qMax)",
              k_in = 0,
              bits = q16_k1_r0,
              exp_q = 0, // (q_is_valid=false)
              exp_r = 0, // (q_is_valid=false)
              exp_len = 0, // (q_is_valid=false)
              exp_err = true
            )

            // Test 9: 快路径 k=1, q=0
            runTest(
              "9: Fast k=1, q=0 (r=1)",
              k_in = 0,
              bits = "01", // q=0, r=1
              exp_q = 0,
              exp_r = 1,
              exp_len = 2 // (q=0 + 1 + k=1) = 2
            )

            // Test 10: 快路径 k=2, q=0
            runTest(
              "10: Fast k=2, q=0 (r=2)",
              k_in = 1,
              bits = "010", // q=0, r=2
              exp_q = 0,
              exp_r = 2,
              exp_len = 3 // (q=0 + 1 + k=2) = 3
            )

            // Test 11: 验证 6-bit 快路径 (k=1)
            runTest(
              "11: Fast k=1, q=4 (6-bit)",
              k_in = 0,
              bits = "111101", // q=4, r=1
              exp_q = 4,
              exp_r = 1,
              exp_len = 6 // (q=4 + 1 + k=1) = 6
            )

            // Test 12: 验证 6-bit 快路径 (k=2)
            runTest(
              "12: Fast k=2, q=3 (6-bit)",
              k_in = 1,
              bits = "111011", // q=3, r=3
              exp_q = 3,
              exp_r = 3,
              exp_len = 6 // (q=3 + 1 + k=2) = 6
            )
            println("--- DecodeUnit_GR所有测试用例通过 ---")
        }
    }
}
