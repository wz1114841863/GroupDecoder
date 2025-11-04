package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

class ParallelRemainderExtractorSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 使用与OffsetAccumulatorSpec相同的测试参数以保持一致
    val p = PDUParams(unrollFactor = 4, maxQuotient = 64)
    val pudp =
        ParallelUnaryDecoderParams(peekWindowWidth = 128, segmentWidth = 32)

    /** 软件参考模型 (Golden Model) 接收所有输入, 计算期望的r向量
      */
    def calculateExpected_r(
        peekWindowStr: String,
        k_in: Int,
        qs: Seq[Int],
        offsets: Seq[Int]
    ): Seq[Int] = {
        val k_val = k_in + 1
        val r_values = new ArrayBuffer[Int]()

        for (i <- 0 until p.unrollFactor) {
            val q = qs(i)
            val offset = offsets(i)

            // 计算r在字符串中的起始索引
            val r_start_idx = offset + q + 1

            if (r_start_idx + k_val > peekWindowStr.length) {
                // 如果超出范围, r为0
                r_values += 0
            } else {
                val r_str =
                    peekWindowStr.substring(r_start_idx, r_start_idx + k_val)
                r_values += BigInt(r_str, 2).toInt
            }
        }
        r_values.toSeq
    }

    "ParallelRemainderExtractor should correctly extract r_vec based on input vectors" in {
        simulate(new ParallelRemainderExtractor(p, pudp)) { dut =>
            /** 测试向量: (名称, k_in, 输入比特流, q向量, offset向量)
              * q向量和offset向量是我们根据比特流手动预计算好的, 模拟了OffsetAccumulator的正确输出
              */
            val testVectors = Seq(
              (
                "k=2, simple offsets",
                1, // k=2
                // q=3, len=6, off=0. r_start=4. r="10" (2)
                // q=4, len=7, off=6. r_start=11. r="11" (3)
                // q=2, len=5, off=13. r_start=16. r="10" (2)
                // q=5, len=8, off=18. r_start=24. r="11" (3)
                "111010" + "1111011" + "11010" + "11111011",
                Seq(3, 4, 2, 5),
                Seq(0, 6, 13, 18)
              ),
              (
                "k=1, with cross-segment q",
                0, // k=1
                // q=35, len=37, off=0. r_start=36. r="0" (0)
                // q=2, len=4, off=37. r_start=40. r="1" (1)
                // q=0, len=2, off=41. r_start=42. r="0" (0)
                // q=10, len=12, off=43. r_start=54. r="1" (1)
                "1" * 35 + "0" + "0" +
                    "110" + "1" +
                    "0" + "0" +
                    "1" * 10 + "0" + "1",
                Seq(35, 2, 0, 10),
                Seq(0, 37, 41, 43)
              ),
              (
                "k=3, mixed bag",
                2, // k=3
                // q=4, len=8, off=0. r_start=5. r="101" (5)
                // q=0, len=4, off=8. r_start=9. r="010" (2)
                // q=1, len=5, off=12. r_start=14. r="111" (7)
                // q=2, len=6, off=17. r_start=20. r="000" (0)
                "11110101" + "0010" + "10111" + "110000",
                Seq(4, 0, 1, 2),
                Seq(0, 8, 12, 17)
              )
            )

            // --- Standard Reset ---
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B);
            dut.clock.step(1)

            var testNum = 0
            for ((name, k_in, bitstream, qs, offsets) <- testVectors) {
                testNum += 1
                println(s"\n--- Running Test $testNum: $name ---")

                // 1. 使用软件参考模型计算期望的r向量
                val expected_r_vec =
                    calculateExpected_r(bitstream, k_in, qs, offsets)

                // 2. 将所有输入送入DUT
                val padded_bitstream =
                    bitstream.padTo(pudp.peekWindowWidth, '0')
                dut.io.peek_window.poke(BigInt(padded_bitstream, 2).U)
                dut.io.k.poke(k_in.U)
                dut.io.final_q_vec.zip(qs).foreach { case (port, value) =>
                    port.poke(value.U)
                }
                dut.io.offset_vec.zip(offsets).foreach { case (port, value) =>
                    port.poke(value.U)
                }

                // 3. 等待组合逻辑传播
                dut.clock.step(1)

                // 4. 验证输出r_vec
                println(
                  s"  Verifying r_vec (expected: ${expected_r_vec.mkString("[", ", ", "]")})"
                )
                dut.io.r_vec.zip(expected_r_vec).foreach {
                    case (dut_r, exp_r) => dut_r.expect(exp_r.U)
                }

                println(s"--- Test $testNum Passed ---")
            }
        }
    }
}
