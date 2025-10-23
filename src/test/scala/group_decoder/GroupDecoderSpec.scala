package group_decoder

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.scalatest.ChiselSim
import scala.collection.mutable.ListBuffer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class GroupDecoderSpec extends AnyFreeSpec with Matchers with ChiselSim {
    // 使用一个小的groupSize进行测试
    val params = GroupDecoderParams(groupSize = 8, streamWidth = 32)

    "GroupDecoder should correctly decode a Golomb-Rice compressed group" in {
        simulate(new GroupDecoder(params)) { dut =>
            // --- 测试数据准备 ---
            // 黄金模型 (Python端逻辑)
            // original_weights = [7, 8, 6, 9, 5, 10, 4, 11]
            // zp = 7
            // deltas = [0, 1, -1, 2, -2, 3, -3, 4]
            // unsigned_deltas = [0, 2, 1, 4, 3, 6, 5, 8]
            // best_k = 1 (实际k=2)
            // GR(k=2) 编码:
            // 0->000, 2->010, 1->001, 4->1000, 3->011, 6->1010, 5->1001, 8->11000
            // 拼接: 000 010 001 1000 011 1010 1001 11000 (总计 29 bits)
            val compressed_stream_str = "00001000110000111010100111000"
            val compressed_data = BigInt(compressed_stream_str, 2)
            val golden_weights = Seq(7, 8, 6, 9, 5, 10, 4, 11)

            val decoded_weights = new ListBuffer[Int]()

            // --- 初始化 ---
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.io.start.poke(false.B)
            dut.io.compressedStream.valid.poke(false.B)
            dut.io.decodedWeights.ready.poke(false.B)

            // --- 发送压缩数据 ---
            dut.io.compressedStream.valid.poke(true.B)
            dut.io.compressedStream.bits.poke(compressed_data.U)

            // 等待DUT接收数据
            while (!dut.io.compressedStream.ready.peek().litToBoolean) {
                dut.clock.step(1)
            }
            dut.clock.step(1)
            dut.io.compressedStream.valid.poke(false.B)

            // --- 启动解码 ---
            dut.io.start.poke(true.B)
            dut.io.zp.poke(7.U)
            dut.io.best_k.poke(1.U) // k=2
            dut.io.isFallback.poke(false.B)
            dut.clock.step(1)
            dut.io.start.poke(false.B)

            var cycles = 0
            // --- 循环接收解码数据, 直到done信号为高 ---
            while (!dut.io.done.peek().litToBoolean) {
                assert(cycles < 1000, "Timeout: GroupDecoder took too long")

                // 我们总是准备好接收数据
                dut.io.decodedWeights.ready.poke(true.B)

                if (dut.io.decodedWeights.valid.peek().litToBoolean) {
                    val decoded_val =
                        dut.io.decodedWeights.bits.peek().litValue.toInt
                    decoded_weights += decoded_val
                    println(s"Cycle ${cycles}: Decoded value -> ${decoded_val}")
                }

                dut.clock.step(1)
                cycles += 1
            }

            // --- 验证结果 ---
            println("Decoding complete.")
            println(s"Decoded weights: ${decoded_weights.toList}")
            println(s"Golden weights:  ${golden_weights}")
            decoded_weights.toList must be(golden_weights)
        }
    }

    "GroupDecoder should correctly decode a Fallback group" in {
        simulate(new GroupDecoder(params)) { dut =>
            // --- 测试数据准备 ---
            // 黄金模型: original_weights = [10, 5, 12, 3, 15, 0, 8, 1]
            // Fallback模式直接拼接4-bit值: 1010 0101 1100 0011 1111 0000 1000 0001
            val compressed_stream_str = "10100101110000111111000010000001"
            val compressed_data = BigInt(compressed_stream_str, 2)
            val golden_weights = Seq(10, 5, 12, 3, 15, 0, 8, 1)

            // (后续测试逻辑与上一个测试用例几乎完全相同, 只是输入不同)
            val decoded_weights = new ListBuffer[Int]()
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            dut.io.start.poke(false.B);
            dut.io.compressedStream.valid.poke(false.B);
            dut.io.decodedWeights.ready.poke(false.B)
            dut.io.compressedStream.valid.poke(true.B);
            dut.io.compressedStream.bits.poke(compressed_data.U)
            while (!dut.io.compressedStream.ready.peek().litToBoolean) {
                dut.clock.step(1)
            }
            dut.clock.step(1)
            dut.io.compressedStream.valid.poke(false.B)

            // --- 启动解码 (Fallback模式) ---
            dut.io.start.poke(true.B)
            dut.io.zp.poke(8.U) // zp在fallback模式下不被使用, 但仍需提供
            dut.io.best_k.poke(0.U)
            dut.io.isFallback.poke(true.B) // 关键输入!
            dut.clock.step(1)
            dut.io.start.poke(false.B)

            var cycles = 0
            while (!dut.io.done.peek().litToBoolean) {
                assert(cycles < 100, "Timeout on Fallback test")
                dut.io.decodedWeights.ready.poke(true.B)
                if (dut.io.decodedWeights.valid.peek().litToBoolean) {
                    decoded_weights += dut.io.decodedWeights.bits
                        .peek()
                        .litValue
                        .toInt
                }
                dut.clock.step(1)
                cycles += 1
            }

            decoded_weights.toList must be(golden_weights)
            println("Fallback test passed.")
        }
    }
}
