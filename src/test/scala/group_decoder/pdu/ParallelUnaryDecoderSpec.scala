package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import group_decoder.common._

class ParallelUnaryDecoderSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    val testParams =
        ParallelUnaryDecoderParams(peekWindowWidth = 64, segmentWidth = 32)

    "ParallelUnaryDecoder should correctly analyze segments for local_q and is_all_ones" in {
        simulate(new ParallelUnaryDecoder(testParams)) { dut =>
            /** Test Vectors: Each tuple contains:
              *   1. A descriptive name for the test case. 2. The 64-bit input
              *      BigInt for the peek_window. 3. A sequence of expected
              *      results for each segment. Each result is a (local_q,
              *      is_all_ones) tuple.
              */
            val testVectors = Seq(
              (
                "MSB-half has 3 leading ones, LSB-half is all ones",
                BigInt(
                  "E0000000FFFFFFFF",
                  16
                ), // Segment 0: 1...1, Segment 1: 1110...
                Seq((32, true), (3, false))
              ),
              (
                "Segment 1 starts with zero, Segment 0 is almost all ones",
                BigInt(
                  "FFFFFFFE7FFFFFFF",
                  16
                ), // Segment 0: 01...1, Segment 1: 1...10
                Seq((0, false), (31, false))
              ),
              (
                "Both segments have some leading ones",
                BigInt(
                  "A0000000F0000000",
                  16
                ), // Segment 0: 11110..., Segment 1: 1010...
                Seq((4, false), (1, false))
              ),
              (
                "Both segments are all zeros",
                BigInt("0000000000000000", 16),
                Seq((0, false), (0, false))
              )
            )

            // --- Standard Reset Sequence ---
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.clock.step(1)

            var testCaseNum = 0
            // --- Iterate through all test vectors ---
            for ((name, input, expectedOutputs) <- testVectors) {
                testCaseNum += 1
                println(s"--- Running Test Case $testCaseNum: $name ---")

                // Poke the input value to the DUT
                dut.io.peek_window.poke(input.U)

                // The module is purely combinational, so we only need to step the clock once
                // for the simulation to propagate the signals.
                dut.clock.step(1)

                // Check the outputs for each segment
                for (i <- 0 until testParams.segmentCount) {
                    val expected_q = expectedOutputs(i)._1
                    val expected_all_ones = expectedOutputs(i)._2

                    dut.io
                        .result_vec(i)
                        .is_all_ones
                        .expect(
                          expected_all_ones.B,
                          s"\n[FAIL] Test: '$name', Segment $i: is_all_ones mismatch."
                        )

                    dut.io
                        .result_vec(i)
                        .local_q
                        .expect(
                          expected_q.U,
                          s"\n[FAIL] Test: '$name', Segment $i: local_q mismatch."
                        )

                    println(
                      s"  Segment $i -> local_q: ${dut.io.result_vec(i).local_q.peek().litValue} (expected $expected_q), is_all_ones: ${dut.io.result_vec(i).is_all_ones.peek().litValue} (expected $expected_all_ones)"
                    )
                }
                println(s"--- Test Case $testCaseNum Passed ---")
            }
        }
    }
}
