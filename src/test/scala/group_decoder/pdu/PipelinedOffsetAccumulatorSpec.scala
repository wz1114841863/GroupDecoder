package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.{ArrayBuffer, Queue}
import group_decoder.common._

class PipelinedOffsetAccumulatorSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 使用与组合逻辑版本完全相同的参数, 以便进行功能等价性验证
    val p = PDUParams(unrollFactor = 4, maxQuotient = 64)
    val pudp =
        ParallelUnaryDecoderParams(peekWindowWidth = 128, segmentWidth = 32)

    // -----------------------------------------------------------------
    //  黄金模型 (Golden Model) - 与之前的测试完全相同
    // -----------------------------------------------------------------
    type ExpectedResult = (Seq[Int], Seq[Int], Seq[Int], Int, Boolean)
    def calculateExpected(peekWindowStr: String, k_in: Int): ExpectedResult = {
        // ... (此处省略与OffsetAccumulatorSpec中完全相同的代码)
        val k_val = k_in + 1; val final_qs = new ArrayBuffer[Int]();
        val lengths = new ArrayBuffer[Int]();
        val offsets = new ArrayBuffer[Int](); var error = false;
        var current_bit_ptr = 0;
        for (i <- 0 until p.unrollFactor) {
            offsets += current_bit_ptr;
            if (current_bit_ptr >= peekWindowStr.length) {
                final_qs += 0; lengths += 0
            } else {
                val first_zero_idx =
                    peekWindowStr.indexOf('0', current_bit_ptr);
                val q =
                    if (first_zero_idx != -1) first_zero_idx - current_bit_ptr
                    else peekWindowStr.length - current_bit_ptr;
                if (q > p.maxQuotient) error = true; final_qs += q;
                val len = q + 1 + k_val; lengths += len; current_bit_ptr += len
            }
        }; (final_qs.toSeq, lengths.toSeq, offsets.toSeq, lengths.sum, error)
    }

    "PipelinedOffsetAccumulator should be functionally equivalent to the combinational model" in {
        simulate(new PipelinedOffsetAccumulator(p, pudp)) { dut =>
            // --- 使用与之前完全相同的测试向量 ---
            val testVectors = Seq(
              ("Simple Case", 1, "111010" + "1111011" + "11010" + "11111011"),
              (
                "Cross-Segment Case",
                0,
                "1" * 35 + "0" + "0" + "110" + "1" + "0" + "0" + "1" * 10 + "0" + "1"
              ),
              (
                "Mixed Bag Case",
                2,
                "1" * 40 + "0" + "101" + "0" + "010" + "10" + "111" + "1" * 20 + "0" + "000"
              )
            )

            // --- 延迟队列 (Latency Queue) ---
            // 用于存储黄金模型的计算结果, 以便与有延迟的DUT输出进行对比
            val expected_queue = new Queue[ExpectedResult]()

            // --- 标准复位 ---
            dut.reset.poke(true.B);
            dut.clock.step(1);
            dut.reset.poke(false.B)

            // 初始化IO
            dut.io.in.valid.poke(false.B)
            dut.io.out.ready.poke(false.B)
            dut.clock.step(1)

            var sent_count = 0
            var received_count = 0
            var cycles = 0

            // 循环直到所有向量都已发送, 并且所有对应的结果都已接收
            while (received_count < testVectors.length) {
                assert(
                  cycles < 500,
                  "Timeout: Test is running for too long, potential deadlock in DUT"
                )

                // ------------------ 输入驱动逻辑 ------------------
                if (
                  sent_count < testVectors.length && dut.io.in.ready
                      .peek()
                      .litToBoolean
                ) {
                    val (name, k_in, bitstream) = testVectors(sent_count)
                    println(s"Cycle ${cycles}: Sending test '$name'...")

                    // 驱动DUT输入
                    dut.io.in.valid.poke(true.B)
                    val padded_bitstream =
                        bitstream.padTo(pudp.peekWindowWidth, '0')
                    dut.io.in.bits.peek_window
                        .poke(BigInt(padded_bitstream, 2).U)
                    dut.io.in.bits.k.poke(k_in.U)

                    // 计算期望结果并存入延迟队列
                    val expected = calculateExpected(bitstream, k_in)
                    expected_queue.enqueue(expected)

                    sent_count += 1
                } else {
                    dut.io.in.valid.poke(false.B)
                }

                // ------------------ 输出验证逻辑 ------------------
                // 我们总是准备好接收DUT的输出
                dut.io.out.ready.poke(true.B)
                if (dut.io.out.valid.peek().litToBoolean) {
                    println(s"Cycle ${cycles}: Receiving a result from DUT...")

                    // 从延迟队列头部取出对应的期望结果
                    val (exp_qs, exp_lens, exp_offsets, exp_total, exp_error) =
                        expected_queue.dequeue()

                    // 逐一对比
                    dut.io.out.bits.final_q_vec.zip(exp_qs).foreach {
                        case (d, e) => d.expect(e.U)
                    }
                    dut.io.out.bits.length_vec.zip(exp_lens).foreach {
                        case (d, e) => d.expect(e.U)
                    }
                    dut.io.out.bits.offset_vec.zip(exp_offsets).foreach {
                        case (d, e) => d.expect(e.U)
                    }
                    dut.io.out.bits.total_consumed_bits.expect(exp_total.U)
                    dut.io.out.bits.error.expect(exp_error.B)

                    println(
                      "  Result matches expected value from Golden Model. Verification Passed for this vector."
                    )
                    received_count += 1
                }

                // --- 时钟步进 ---
                dut.clock.step(1)
                cycles += 1
            }
        }
    }
}
