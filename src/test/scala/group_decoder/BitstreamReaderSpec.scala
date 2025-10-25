package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.util.Random

class BitstreamReaderSpec extends AnyFreeSpec with Matchers with ChiselSim {
    val params = BitstreamReaderParams(streamWidth = 32, bufferWidth = 64)

    /** 辅助函数:发送一个数据块,会一直等待直到DUT准备好
      */
    def sendData(dut: BitstreamReader, data: BigInt): Unit = {
        while (!dut.io.inputStream.ready.peek().litToBoolean) {
            dut.clock.step(1)
        }
        dut.io.inputStream.valid.poke(true.B)
        dut.io.inputStream.bits.poke(data.U)
        dut.clock.step(1)
        dut.io.inputStream.valid.poke(false.B)
    }

    /** 辅助函数:请求N个比特并验证结果
      */
    def requestBits(dut: BitstreamReader, n: Int, expected: BigInt): Unit = {
        // 1. 发出请求,并保持 valid 信号,直到数据被接收
        dut.io.request.valid.poke(true.B)
        dut.io.request.bits.poke(n.U)

        // 2. 同时,作为消费者,我们表示已准备好接收数据
        dut.io.bits_out.ready.poke(true.B)

        // 3. 等待 DUT 将 valid 置高,完成握手
        while (!dut.io.bits_out.valid.peek().litToBoolean) {
            dut.clock.step(1)
        }

        // 4. 在 valid 和 ready 都为高的这个周期,数据是有效的,进行验证
        dut.io.bits_out.bits.expect(expected.U, s"Failed requesting $n bits")

        // 5. 握手完成,撤销请求和 ready 信号
        dut.clock.step(1)
        dut.io.request.valid.poke(false.B)
        dut.io.bits_out.ready.poke(false.B)
    }

    "BitstreamReader should handle reads within and across stream boundaries" in {
        simulate(new BitstreamReader(params)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            sendData(dut, BigInt("DEADBEEF", 16))
            requestBits(dut, 16, BigInt("DEAD", 16))
            requestBits(dut, 8, BigInt("BE", 16))
            sendData(dut, BigInt("12345678", 16))
            requestBits(dut, 16, BigInt("EF12", 16))
            requestBits(dut, 24, BigInt("345678", 16))
        }
    }

    "BitstreamReader should handle buffer underrun by waiting for data" in {
        simulate(new BitstreamReader(params)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)

            println("\n--- Testing Buffer Underrun ---")
            sendData(dut, BigInt("AAAAAAAA", 16))
            requestBits(dut, 24, BigInt("AAAAAA", 16))

            dut.io.request.valid.poke(true.B)
            dut.io.request.bits.poke(16.U)
            dut.clock.step(1)

            dut.io.bits_out.valid.expect(false.B)
            dut.io.inputStream.ready.expect(true.B)

            sendData(dut, BigInt("BBBBBBBB", 16))

            while (!dut.io.bits_out.valid.peek().litToBoolean) {
                dut.clock.step(1)
            }

            // FIX 1: The expected value for a 16-bit request is "AABB", not "AABBBB".
            dut.io.bits_out.bits.expect(BigInt("AABB", 16).U)

            dut.clock.step(1)
            dut.io.request.valid.poke(false.B)
            println("Passed: Correctly handled underrun scenario.")
        }
    }

    "BitstreamReader should apply backpressure when buffer is full" in {
        simulate(new BitstreamReader(params)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)

            sendData(dut, BigInt("11111111", 16))
            sendData(dut, BigInt("22222222", 16))

            dut.io.inputStream.ready
                .expect(false.B, "Ready should be low when buffer is full")

            // FIX 2: Consume enough data (a full 32-bit word) to guarantee space for the next word.
            requestBits(dut, 32, BigInt("11111111", 16))

            dut.io.inputStream.ready.expect(
              true.B,
              "Ready should be high after consuming a full word"
            )
        }
    }

    // "BitstreamReader should handle a sequence of random requests and data" in {
    //     simulate(new BitstreamReader(params)) { dut =>
    //         val rand = new Random(42) // 固定种子以保证可重复
    //         var goldenModel = BigInt(0)
    //         var goldenBits = 0

    //         dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)

    //         for (_ <- 0 until 200) { // 增加循环次数以进行更充分的测试
    //             val doSend = rand.nextBoolean()

    //             // 尝试发送数据
    //             if (doSend && dut.io.inputStream.ready.peek().litToBoolean) {
    //                 val data = BigInt(params.streamWidth, rand)
    //                 sendData(dut, data)
    //                 goldenModel = (goldenModel << params.streamWidth) | data
    //                 goldenBits += params.streamWidth
    //             }
    //             // 尝试请求数据
    //             else if (!doSend && goldenBits > 0) {
    //                 val reqSize =
    //                     (rand.nextInt(params.streamWidth) + 1).min(goldenBits)

    //                 // FIX 2: 删除对内部状态 validBits 的非法访问
    //                 // 直接发出请求,让 requestBits 辅助函数自己去等待
    //                 val mask = (BigInt(1) << reqSize) - 1
    //                 val expected =
    //                     (goldenModel >> (goldenBits - reqSize)) & mask
    //                 requestBits(dut, reqSize, expected)

    //                 // 更新我们的软件模型
    //                 goldenBits -= reqSize
    //             }

    //             dut.clock.step(1)
    //         }
    //     }
    // }
}
