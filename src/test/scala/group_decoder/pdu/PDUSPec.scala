package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.{ArrayBuffer, Queue}

case class ExpectedPDUResult(weights: Seq[Int], last: Boolean)

class PDUSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // 为了让仿真快速完成, 我们使用一个较小的/可被unrollFactor整除的groupSize
    val p = PDUParams(
      groupSize = 32,
      unrollFactor = 4,
      maxQuotient = 64,
      peekWindowWidth = 128
    )

    /** 最终的/端到端的软件参考模型 (Golden Model) 它模拟PDU的完整行为, 包括分批输出
      */
    def calculateExpectedPDUOutput(
        bitstream: String,
        k_in: Int,
        zp: Int
    ): Queue[ExpectedPDUResult] = {
        val k_val = k_in + 1
        val all_weights = new ArrayBuffer[Int]()
        var current_bit_ptr = 0

        while (
          all_weights.length < p.groupSize && current_bit_ptr < bitstream.length
        ) {
            val first_zero_idx = bitstream.indexOf('0', current_bit_ptr)
            if (first_zero_idx == -1) { // Handle case where no '0' is found
                // This case is unlikely in valid streams but good for robustness
                current_bit_ptr = bitstream.length
            } else {
                val q = first_zero_idx - current_bit_ptr
                val r_start_idx = first_zero_idx + 1
                val r = BigInt(
                  bitstream.substring(r_start_idx, r_start_idx + k_val),
                  2
                ).toInt

                val unsigned_delta = (q << k_val) | r
                val signed_delta =
                    if (unsigned_delta % 2 == 0) unsigned_delta / 2
                    else -(unsigned_delta + 1) / 2
                val final_weight = (signed_delta + zp) & 0xf
                all_weights += final_weight

                current_bit_ptr = r_start_idx + k_val
            }
        }

        val output_queue = new Queue[ExpectedPDUResult]()
        all_weights.grouped(p.unrollFactor).zipWithIndex.foreach {
            case (weights_chunk, index) =>
                val is_last =
                    (index == (all_weights.grouped(p.unrollFactor).size - 1))
                output_queue.enqueue(
                  ExpectedPDUResult(
                    weights = weights_chunk.toSeq,
                    last = is_last
                  )
                )
        }

        output_queue
    }

    "PDU top-level module should correctly decode a full group with pipeline and backpressure" in {
        simulate(new PDU(p)) { dut =>
            // --- 创建一个足够长的测试向量 ---
            val k_in = 1 // k=2
            val zp = 8
            var bitstream = ""
            // 生成32个权重, q值从0到31
            (0 until p.groupSize).foreach { i =>
                val q = i % 10 // 循环的q值
                val r = i % 4 // 循环的r值
                bitstream += "1" * q + "0" + r.toBinaryString.reverse
                    .padTo(2, '0')
                    .reverse
            }

            // --- 计算完整的期望输出序列 ---
            val expected_sequence =
                calculateExpectedPDUOutput(bitstream, k_in, zp)

            // --- 标准复位与初始化 ---
            dut.reset.poke(true.B);
            dut.clock.step(1);
            dut.reset.poke(false.B)
            dut.io.task_in.valid.poke(false.B)
            dut.io.result_out.ready.poke(false.B)
            dut.clock.step(2)

            // --- 发送单个任务 ---
            println("--- Sending Task to PDU ---")
            dut.io.task_in.valid.poke(true.B)
            dut.io.task_in.bits.tag.poke(123.U) // 任意tag
            dut.io.task_in.bits.zp.poke(zp.U)
            dut.io.task_in.bits.k.poke(k_in.U)
            // dut.io.task_in.bits.stream_chunk.poke(BigInt(bitstream, 2).U)
            // Pad the bitstream to the full chunk width before converting to BigInt
            val padded_bitstream = bitstream.padTo(p.streamChunkWidth, '0')
            dut.io.task_in.bits.stream_chunk.poke(BigInt(padded_bitstream, 2).U)

            dut.io.task_in.ready.expect(true.B)
            dut.clock.step(1)
            dut.io.task_in.valid.poke(false.B)
            println("--- Task Sent, PDU is now in DECODING state ---")

            var cycles = 0
            while (!expected_sequence.isEmpty) {
                assert(
                  cycles < 1000,
                  "Timeout: PDU integration test is running too long"
                )
                dut.io.result_out.ready.poke((cycles % 4) != 0)
                if (
                  dut.io.result_out.valid
                      .peek()
                      .litToBoolean && dut.io.result_out.ready
                      .peek()
                      .litToBoolean
                ) {
                    val expected_bundle = expected_sequence.dequeue()

                    println(
                      s"Cycle ${cycles}: Received a result packet from PDU."
                    )

                    dut.io.result_out.bits.tag.expect(123.U)
                    dut.io.result_out.bits.weights
                        .zip(expected_bundle.weights)
                        .foreach { case (dut_w, exp_w) =>
                            dut_w.expect(exp_w.U)
                        }
                    dut.io.result_out.bits.last.expect(expected_bundle.last.B)

                    if (expected_bundle.last) {
                        println(
                          "--- Last packet received and verified. Test Passed! ---"
                        )
                    }
                }

                cycles += 1
                dut.clock.step(1)
            }
        }
    }
}
