package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import group_decoder.common.PDUParams

/** MicroDecoderSpec 测试模块
  */
class MicroDecoderSpec extends AnyFreeSpec with Matchers with ChiselSim {

    "MicroDecoder (v5.0 Dual Path) should correctly decode from various offsets" in {

        // 1. 实例化我们的共享配置
        implicit val p = PDUParams(
          peekWindowWidth = 64, // 测试窗口
          qMax = 15,
          kMax = 2
        )

        // 2. 启动模拟
        simulate(new MicroDecoder(p)) { dut =>
            // 3. 标准复位序列
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            println("--- 开始测试 MicroDecoder (带 start_offset) ---")

            // 辅助函数: 创建一个BigInt, 从MSB(左)侧填充字符串, 模拟 peek_window
            def s(bitString: String): BigInt = {
                val padded = bitString.padTo(p.peekWindowWidth, '0')
                BigInt(padded, 2)
            }

            // 辅助函数: 运行单个测试
            def runTest(
                testName: String,
                k_in: Int,
                offset: Int,
                bits: String,
                exp_q: Int,
                exp_r: Int,
                exp_len: Int,
                exp_err: Boolean = false
            ): Unit = {
                println(
                  s"Test ($testName): k=${k_in + 1}, offset=$offset, bits=$bits"
                )
                dut.io.k_in.poke(k_in.U)
                dut.io.start_offset.poke(offset.U)
                dut.io.peek_window.poke(s(bits))
                dut.clock.step(1)
                dut.io.final_q.expect(exp_q.U)
                dut.io.final_r.expect(exp_r.U)
                dut.io.length.expect(exp_len.U)
                dut.io.error.expect(exp_err.B)
            }

            runTest(
              "1: Fast k=1",
              k_in = 0,
              offset = 0,
              bits = "1101",
              exp_q = 2,
              exp_r = 1,
              exp_len = 4
            )

            runTest(
              "2: Fast k=2 Offset",
              k_in = 1,
              offset = 5,
              bits = "00000" + "1011",
              exp_q = 1,
              exp_r = 3,
              exp_len = 4
            )

            runTest(
              "3: Slow k=1",
              k_in = 0,
              offset = 0,
              bits = "1111100",
              exp_q = 5,
              exp_r = 0,
              exp_len = 7
            )

            runTest(
              "4: Slow k=2 Offset",
              k_in = 1,
              offset = 10,
              bits = "0" * 10 + "111001",
              exp_q = 3,
              exp_r = 1,
              exp_len = 6
            )

            // Test 5: 快车道未命中 (k=1, q=3 编码 "1110"), 必须由慢车道处理
            runTest(
              "5: Fast Path Miss",
              k_in = 0,
              offset = 0,
              bits = "11101", // q=3, r=1
              exp_q = 3,
              exp_r = 1,
              exp_len = 5
            ) // len=3+1+1=5

            // Test 6: 慢车道边界 (q=qMax=15)
            val q15_k1_r0 =
                "1" * 15 + "0" + "0" // 15个'1', '0' (q结束), '0' (r)
            runTest(
              "6: Slow qMax k=1",
              k_in = 0,
              offset = 0,
              bits = q15_k1_r0,
              exp_q = 15,
              exp_r = 0,
              exp_len = 17
            ) // len=15+1+1=17

            // Test 7: 慢车道最坏情况 (k=kMax, q=qMax)
            val q15_k2_r3 =
                "1" * 15 + "0" + "11" // 15个'1', '0' (q结束), '11' (r=3)
            runTest(
              "7: Slow qMax k=2",
              k_in = 1,
              offset = 3,
              bits = "000" + q15_k2_r3,
              exp_q = 15,
              exp_r = 3,
              exp_len = 18
            ) // len=15+1+2=18

            println("--- MicroDecoder所有(包括补充)测试用例通过 ---")
        }
    }
}
