package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer
import group_decoder.common._

class OffsetAccumulatorSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // 为了便于测试和手动创建向量, 我们使用较小的参数
    val p = PDUParams(unrollFactor = 4, maxQuotient = 64)
    val pudp =
        ParallelUnaryDecoderParams(peekWindowWidth = 128, segmentWidth = 32)

    /** 软件参考模型 (Golden Model) 完美模拟OffsetAccumulator的预期行为
      */
    def calculateExpected(
        peekWindowStr: String,
        k_in: Int
    ): (Seq[Int], Seq[Int], Seq[Int], Int, Boolean) = {
        val k_val = k_in + 1

        val final_qs = new ArrayBuffer[Int]()
        val lengths = new ArrayBuffer[Int]()
        val offsets = new ArrayBuffer[Int]()
        var error = false

        var current_bit_ptr = 0

        for (i <- 0 until p.unrollFactor) {
            offsets += current_bit_ptr

            if (current_bit_ptr >= peekWindowStr.length) {
                // 如果指针超出范围, 后续结果都为0
                final_qs += 0
                lengths += 0
            } else {
                // 寻找第一个'0'
                val first_zero_idx = peekWindowStr.indexOf('0', current_bit_ptr)

                val q = if (first_zero_idx != -1) {
                    first_zero_idx - current_bit_ptr
                } else {
                    // 没找到'0', q就是剩余长度
                    peekWindowStr.length - current_bit_ptr
                }

                if (q > p.maxQuotient) error = true

                final_qs += q
                val len = q + 1 + k_val
                lengths += len
                current_bit_ptr += len
            }
        }

        val total_consumed = lengths.sum
        (final_qs.toSeq, lengths.toSeq, offsets.toSeq, total_consumed, error)
    }

    "OffsetAccumulator (Combinational Model) should correctly calculate all vectors" in {
        simulate(new OffsetAccumulator(p, pudp)) { dut =>
            // --- 测试向量: (名称, k_in, 输入比特流字符串) ---
            val testVectors = Seq(
              (
                "Simple Case: All weights are short",
                1, // k=2
                // q=3 (len=6), q=4 (len=7), q=2 (len=5), q=5 (len=8)
                "111010" + "1111011" + "11010" + "11111011"
              ),
              (
                "Cross-Segment Case: First q > 32",
                0, // k=1
                // q=35 (len=37), q=2 (len=4), q=0 (len=2), q=10 (len=12)
                "1" * 35 + "0" + "10" +
                    "110" + "1" +
                    "0" + "0" +
                    "1" * 10 + "0" + "1"
              ),
              (
                "Mixed Bag Case: Mix of long, short, and zero q's",
                2, // k=3
                // q=40 (len=44), q=0 (len=4), q=1 (len=5), q=20 (len=24)
                "1" * 40 + "0" + "101" +
                    "0" + "010" +
                    "10" + "111" +
                    "1" * 20 + "0" + "000"
              )
            )

            // --- Standard Reset ---
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.clock.step(1)

            var testNum = 0
            for ((name, k_in, bitstream) <- testVectors) {
                testNum += 1
                println(s"\n--- Running Test $testNum: $name ---")

                // 1. 使用软件参考模型计算期望结果
                val (exp_qs, exp_lens, exp_offsets, exp_total, exp_error) =
                    calculateExpected(bitstream, k_in)

                // 2. 将输入送入DUT
                // 确保比特流字符串长度与peekWindowWidth一致, 不足则在右侧补'0'
                val padded_bitstream =
                    bitstream.padTo(pudp.peekWindowWidth, '0')
                dut.io.peek_window.poke(BigInt(padded_bitstream, 2).U)
                dut.io.k.poke(k_in.U)
                // (segment_results 输入在此组合逻辑版本中被内部重新计算, 故无需驱动)

                // 3. 等待组合逻辑传播
                dut.clock.step(1)

                // 4. 逐一对比所有输出
                println("  Verifying final_q_vec...")
                dut.io.final_q_vec.zip(exp_qs).foreach { case (dut_q, exp_q) =>
                    dut_q.expect(exp_q.U)
                }

                println("  Verifying length_vec...")
                dut.io.length_vec.zip(exp_lens).foreach {
                    case (dut_len, exp_len) => dut_len.expect(exp_len.U)
                }

                println("  Verifying offset_vec...")
                dut.io.offset_vec.zip(exp_offsets).foreach {
                    case (dut_off, exp_off) => dut_off.expect(exp_off.U)
                }

                println("  Verifying total_consumed_bits...")
                dut.io.total_consumed_bits.expect(exp_total.U)

                println("  Verifying error signal...")
                dut.io.error.expect(exp_error.B)

                println(s"--- Test $testNum Passed ---")
            }
        }
    }
}
