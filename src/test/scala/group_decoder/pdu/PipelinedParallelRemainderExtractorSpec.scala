package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.{ArrayBuffer, Queue}

class PipelinedParallelRemainderExtractorSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 使用与之前完全相同的参数
    val p = PDUParams(unrollFactor = 4, maxQuotient = 64)
    val pudp =
        ParallelUnaryDecoderParams(peekWindowWidth = 128, segmentWidth = 32)

    // -----------------------------------------------------------------
    //  黄金模型 (Golden Model) - 与之前的测试完全相同
    // -----------------------------------------------------------------
    type ExpectedResult = Seq[Int]
    def calculateExpected_r(
        peekWindowStr: String,
        k_in: Int,
        qs: Seq[Int],
        offsets: Seq[Int]
    ): ExpectedResult = {
        val k_val = k_in + 1; val r_values = new ArrayBuffer[Int]();
        for (i <- 0 until p.unrollFactor) {
            val q = qs(i); val offset = offsets(i);
            val r_start_idx = offset + q + 1;
            if (r_start_idx + k_val > peekWindowStr.length) { r_values += 0 }
            else {
                val r_str =
                    peekWindowStr.substring(r_start_idx, r_start_idx + k_val);
                r_values += BigInt(r_str, 2).toInt
            }
        }; r_values.toSeq
    }

    "PipelinedParallelRemainderExtractor should be functionally equivalent to the combinational model" in {
        simulate(new PipelinedParallelRemainderExtractor(p, pudp)) { dut =>
            val testVectors = Seq(
              (
                "k=2, simple",
                1,
                "111010" + "1111011" + "11010" + "11111011",
                Seq(3, 4, 2, 5),
                Seq(0, 6, 13, 18)
              ),
              (
                "k=1, cross-segment",
                0,
                "1" * 35 + "0" + "0" + "110" + "1" + "0" + "0" + "1" * 10 + "0" + "1",
                Seq(35, 2, 0, 10),
                Seq(0, 37, 41, 43)
              ),
              (
                "k=3, mixed",
                2,
                "11110101" + "0010" + "10111" + "110000",
                Seq(4, 0, 1, 2),
                Seq(0, 8, 12, 17)
              )
            )

            // 延迟队列, 存储黄金模型的结果
            val expected_queue = new Queue[ExpectedResult]()

            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            dut.io.in.valid.poke(false.B)
            dut.io.out.ready.poke(false.B)
            dut.clock.step(1)

            var sent_count = 0
            var received_count = 0
            var cycles = 0

            while (received_count < testVectors.length) {
                assert(
                  cycles < 200,
                  "Timeout: Functional test running too long"
                )

                // 随机化下游的ready信号, 模拟真实环境
                dut.io.out.ready
                    .poke((cycles % 3) != 0) // 在每3个周期中, 有1个周期下游不ready

                // --- 输入驱动 ---
                if (
                  sent_count < testVectors.length && dut.io.in.ready
                      .peek()
                      .litToBoolean
                ) {
                    val (name, k_in, bitstream, qs, offsets) =
                        testVectors(sent_count)

                    dut.io.in.valid.poke(true.B)
                    val padded_bitstream =
                        bitstream.padTo(pudp.peekWindowWidth, '0')
                    dut.io.in.bits.peek_window
                        .poke(BigInt(padded_bitstream, 2).U)
                    dut.io.in.bits.k.poke(k_in.U)
                    dut.io.in.bits.final_q_vec.zip(qs).foreach { case (p, v) =>
                        p.poke(v.U)
                    }
                    dut.io.in.bits.offset_vec.zip(offsets).foreach {
                        case (p, v) => p.poke(v.U)
                    }

                    expected_queue.enqueue(
                      calculateExpected_r(bitstream, k_in, qs, offsets)
                    )
                    sent_count += 1
                } else {
                    dut.io.in.valid.poke(false.B)
                }

                // --- 输出验证 ---
                if (
                  dut.io.out.valid.peek().litToBoolean && dut.io.out.ready
                      .peek()
                      .litToBoolean
                ) {
                    val expected_r_vec = expected_queue.dequeue()
                    dut.io.out.bits.r_vec.zip(expected_r_vec).foreach {
                        case (d, e) => d.expect(e.U)
                    }
                    received_count += 1
                    println(s"Cycle ${cycles}: Received and verified a result.")
                }

                dut.clock.step(1)
                cycles += 1
            }
        }
    }

    "PipelinedParallelRemainderExtractor should handle backpressure correctly" in {
        simulate(new PipelinedParallelRemainderExtractor(p, pudp)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            dut.io.in.valid.poke(false.B)
            dut.io.out.ready.poke(true.B)
            dut.clock.step(1)

            println("--- Backpressure Test ---")
            // 1. 发送第一个输入 (Input A)
            println("Cycle 0: Sending first input (A)...")
            dut.io.in.valid.poke(true.B)
            // (使用虚拟数据)
            dut.io.in.bits.peek_window.poke(0.U)
            dut.io.in.bits.k.poke(1.U)
            dut.io.in.ready
                .expect(true.B, "DUT should be ready at the beginning")
            dut.clock.step(1) // Input A 被锁存到 S1
            dut.io.in.valid.poke(false.B)

            // 2. 流水线填充
            // 经过1个周期, Input A 到达 S2, S1 变空
            println("Cycle 1: Input A moves to S2. S1 is now empty.")
            dut.io.out.valid.expect(false.B)
            dut.clock.step(1) // Input A 的结果到达输出

            // 3. 结果到达输出, 但下游不ready, 制造反压
            println(
              "Cycle 2: Result A is at output, but stalling with out.ready=false"
            )
            dut.io.out.valid
                .expect(true.B, "Result A should be valid at output")
            dut.io.out.ready.poke(false.B) // << 施加反压

            // 4. 验证解耦缓冲行为
            // 因为 S1 是空的, 即使 S2 阻塞了, 模块的输入应该仍然 ready
            dut.io.in.ready.expect(
              true.B,
              "DUT should still be ready as its S1 stage is empty (skid buffer)"
            )
            println("  Verified: in.ready is HIGH because S1 can accept data.")

            // 5. 发送第二个输入 (Input B), 填满 S1
            println(
              "Cycle 2: Sending second input (B) to fill the empty S1 slot."
            )
            dut.io.in.valid.poke(true.B)
            dut.clock.step(1) // 在这个周期, Input B 被锁存到 S1

            // 6. 验证反压信号
            // 现在 S2 被 A 阻塞, S1 被 B 填满.流水线完全满了.
            // 因此, 在这个周期的开始, in.ready 必须被拉低.
            println(
              "Cycle 3: Pipeline is now full (A in S2, B in S1). Verifying backpressure."
            )
            dut.io.in.ready.expect(
              false.B,
              "DUT must signal not ready (backpressure) when both stages are full and stalled"
            )
            println("  Verified: in.ready is now LOW due to backpressure.")
            dut.io.in.valid.poke(false.B) // 停止发送

            // 7. 释放反压
            println("Cycle 3: Releasing backpressure by setting out.ready=true")
            dut.io.out.ready.poke(true.B)
            dut.io.out.valid
                .expect(true.B, "Result A should still be valid") // 结果A应该仍然有效

            // 8. 验证流水线恢复
            // 在 Cycle 3 的末尾, A 会被接收 (out.fire), B 会从S1移动到S2
            // 所以在下一个周期的开始 (Cycle 4), S1 变空, in.ready 应该恢复为 true
            dut.clock.step(1)
            println("Cycle 4: Verifying pipeline has recovered")
            dut.io.in.ready.expect(
              true.B,
              "DUT should be ready again after stall is released"
            )
            println("--- Backpressure Test Passed ---")
        }
    }
}
