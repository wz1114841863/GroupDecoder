package group_decoder

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class GRCoreSpec extends AnyFreeSpec with Matchers with ChiselSim {
    val params = GroupDecoderParams(groupSize = 512, streamWidth = 32)
    "GR_Core should correctly decode various Golomb-Rice values" in {
        simulate(new GRCore(params)) { dut =>
            // 模拟BitstreamReader的行为, 根据GR_Core的请求,从一个给定的比特串中提供数据
            def runTestVector(
                k_in: Int,
                bitstream: String,
                expectedValue: BigInt
            ): Unit = {
                println(
                  s"--- Testing k=${k_in + 1}, stream='${bitstream}', expected=${expectedValue} ---"
                )

                // --- 启动GR_Core ---
                dut.io.start.poke(true.B)
                dut.io.k.poke(k_in.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var bitstream_idx = 0
                var cycles = 0

                // 模拟循环, 直到GR_Core完成解码, 发出done信号
                while (!dut.io.done.peek().litToBoolean) {
                    assert(cycles < 100, "Test timed out")

                    // 检查GR_Core是否发出读取请求
                    if (dut.io.reader_req.valid.peek().litToBoolean) {
                        val n_bits_requested =
                            dut.io.reader_req.bits.peek().litValue.toInt
                        assert(
                          bitstream_idx + n_bits_requested <= bitstream.length,
                          "Test vector bitstream is too short!"
                        )

                        // 从测试比特串中提取数据
                        val data_to_send = BigInt(
                          bitstream.substring(
                            bitstream_idx,
                            bitstream_idx + n_bits_requested
                          ),
                          2
                        )
                        bitstream_idx = bitstream_idx + n_bits_requested

                        // 将数据提供给GR_Core
                        dut.io.reader_resp.valid.poke(true.B)
                        dut.io.reader_resp.bits.poke(data_to_send.U)
                    } else {
                        dut.io.reader_resp.valid.poke(false.B)
                    }

                    dut.clock.step(1)
                    cycles = cycles + 1
                }

                // --- 验证结果 ---
                dut.io.decoded_val.expect(
                  expectedValue.U,
                  s"Failed for k=${k_in + 1}, stream='${bitstream}'"
                )
                println(s"Passed in ${cycles} cycles.")

                // 清理状态, 为下一个测试向量做准备
                dut.io.reader_resp.valid.poke(false.B)
                dut.clock.step(2) // 等待状态机回到Idle
            }

            // --- 初始化和复位 ---
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.io.start.poke(false.B)
            dut.io.reader_resp.valid.poke(false.B)
            dut.clock.step(2)

            // --- 执行一系列测试向量 ---
            // 算法: value = (q << k) | r
            // k=1 (k_in=0), q=3, r=1 -> (3 << 1) | 1 = 7. 编码: 1110 1
            runTestVector(k_in = 0, bitstream = "11101", expectedValue = 7)

            // k=2 (k_in=1), q=0, r=2 -> (0 << 2) | 2 = 2. 编码: 0 10
            runTestVector(k_in = 1, bitstream = "010", expectedValue = 2)

            // k=3 (k_in=2), q=2, r=5 -> (2 << 3) | 5 = 21. 编码: 110 101
            runTestVector(k_in = 2, bitstream = "110101", expectedValue = 21)

            // 边界情况: q=0, r=0
            runTestVector(k_in = 0, bitstream = "00", expectedValue = 0)
        }
    }
}
