package group_decoder

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class BitstreamReaderSpec extends AnyFreeSpec with Matchers with ChiselSim {
    // 定义模块参数
    val params = BitstreamReaderParams(streamWidth = 32, bufferWidth = 64)

    "BitstreamReader should handle reads within and across stream boundaries" in {
        simulate(new BitstreamReader(params)) { dut =>
            def sendData(data: BigInt): Unit = {
                // 等待DUT准备好接收数据
                while (!dut.io.inputStream.ready.peek().litToBoolean) {
                    dut.clock.step(1)
                }
                dut.io.inputStream.bits.poke(data.U)
                dut.io.inputStream.valid.poke(true.B)
                dut.clock.step(1)
                dut.io.inputStream.valid.poke(false.B)
            }

            // 从 BitstreamReader 请求 N 个比特并验证结果
            def requestBits(n: Int, expected: BigInt): Unit = {
                dut.io.request.bits.poke(n.U)
                dut.io.request.valid.poke(true.B)

                // 等待 DUT 返回有效数据
                while (!dut.io.bits_out.valid.peek().litToBoolean) {
                    dut.clock.step(1)
                }
                dut.io.bits_out.bits
                    .expect(expected.U, s"Failed requesting $n bits")
                println(
                  s"Passed: requested $n bits, got 0x${expected.toString(16)}"
                )

                dut.clock.step(1)
                dut.io.request.valid.poke(false.B)
            }

            println("--- Testing BitstreamReader ---")

            // 1. 初始化和复位
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            dut.io.inputStream.valid.poke(false.B)
            dut.io.request.valid.poke(false.B)
            dut.clock.step(2)

            // 2. 发送第一个数据块
            val data1 = BigInt("DEADBEEF", 16)
            println(s"Sending data1: 0x${data1.toString(16)}")
            sendData(data1)

            // 3. 在第一个数据块内部进行读取
            requestBits(16, BigInt("DEAD", 16)) // 读取前16位
            requestBits(8, BigInt("BE", 16)) // 读取接下来8位
            // 此时 shifter 中还剩下 8 位: 0xEF

            // 4. 发送第二个数据块
            val data2 = BigInt("12345678", 16)
            println(s"Sending data2: 0x${data2.toString(16)}")
            sendData(data2) // 此时 shifter 中应该有 8+32=40 位: 0xEF12345678

            // 5. 进行关键的[跨边界读取]
            // 我们需要 16 比特, 其中 8 比特来自 data1 的剩余部分 (EF), 8 比特来自 data2 的开头 (12)
            requestBits(16, BigInt("EF12", 16))

            // 6. 继续读取
            requestBits(24, BigInt("345678", 16))

        }
    }
}
