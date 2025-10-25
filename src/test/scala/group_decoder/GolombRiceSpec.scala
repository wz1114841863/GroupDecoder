package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.Queue
import scala.util.Random

class GolombRiceCoreSpec extends AnyFreeSpec with Matchers with ChiselSim {

    val params =
        GolombRiceCoreParams(maxK = 3, maxQuotient = 31, outputWidth = 5)

    def golombRiceEncoder(value: Int, k: Int): String = {
        val q = value >> k
        val r = value & ((1 << k) - 1)
        "1" * q + "0" + String
            .format(s"%${k}s", r.toBinaryString)
            .replace(' ', '0')
    }

    def runDecodeTest(
        dut: GolombRiceCore,
        k: Int,
        expectedValue: Int,
        stallCycles: Int = 0,
        backpressureCycles: Int = 0
    ): Unit = {
        val bitstream = golombRiceEncoder(expectedValue, k)
        val bitQueue = Queue.from(bitstream.map(_.toString.toInt))

        println(
          s"\n--- Testing k=$k, value=$expectedValue. Bitstream: '$bitstream' ---"
        )

        dut.io.start.poke(true.B)
        dut.io.k.poke(k.U)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        var timeout = 0
        val max_cycles =
            bitstream.length * (stallCycles + 5) + backpressureCycles + 20

        while (
          !dut.io.unsigned_delta.valid
              .peek()
              .litToBoolean && timeout < max_cycles
        ) {
            dut.io.busy.expect(true.B)

            if (dut.io.request_bits.valid.peek().litToBoolean) {
                dut.clock.step(stallCycles)
                timeout += stallCycles

                val n_bits = dut.io.request_bits.bits.peek().litValue.toInt
                var bits_to_send = BigInt(0)
                for (_ <- 0 until n_bits) {
                    if (bitQueue.nonEmpty)
                        bits_to_send = (bits_to_send << 1) | bitQueue.dequeue()
                }

                // --- 握手逻辑开始 ---
                dut.io.bits_in.valid.poke(true.B)
                dut.io.bits_in.bits.poke(bits_to_send.U)

                // 持续提供数据,直到 DUT 表示 "ready"
                while (
                  !dut.io.bits_in.ready
                      .peek()
                      .litToBoolean && timeout < max_cycles
                ) {
                    dut.clock.step(1)
                    timeout += 1
                }
                // --- 握手逻辑结束 ---
            }

            dut.clock.step(1)
            dut.io.bits_in.valid.poke(false.B) // valid 信号只在需要时拉高
            timeout += 1
        }

        require(
          timeout < max_cycles,
          s"Test timed out for k=$k, value=$expectedValue"
        )

        // 输出验证和反压测试 (这部分逻辑不变)
        dut.io.unsigned_delta.valid.expect(true.B)
        dut.io.unsigned_delta.bits.expect(expectedValue.U)

        if (backpressureCycles > 0) {
            println(s"Applying backpressure for $backpressureCycles cycles...")
            dut.io.unsigned_delta.ready.poke(false.B)
            dut.clock.step(backpressureCycles)
            dut.io.busy.expect(true.B)
            dut.io.unsigned_delta.valid.expect(true.B)
        }

        dut.io.unsigned_delta.ready.poke(true.B)
        dut.clock.step(1)
        dut.io.busy.expect(false.B)
    }

    "GolombRiceCore FSM should work correctly" in {
        simulate(new GolombRiceCore(params)) { dut =>
            // 初始化
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            dut.io.start.poke(false.B)
            dut.io.bits_in.valid.poke(false.B)
            dut.io.unsigned_delta.ready.poke(true.B)

            // --- 基础功能验证 ---
            runDecodeTest(dut, k = 1, expectedValue = 5)
            runDecodeTest(dut, k = 2, expectedValue = 10)

            // --- 边界条件 ---
            runDecodeTest(dut, k = 2, expectedValue = 3)
            runDecodeTest(dut, k = 3, expectedValue = 0)

            // --- 最大值验证 ---
            runDecodeTest(dut, k = 1, expectedValue = 30)

            // --- 时序兼容性测试 ---
            println("\n=== Testing with BitstreamReader Stalls ===")
            runDecodeTest(dut, k = 2, expectedValue = 7, stallCycles = 2)

            println("\n=== Testing with Downstream Backpressure ===")
            runDecodeTest(
              dut,
              k = 3,
              expectedValue = 13,
              backpressureCycles = 5
            )

            // --- 随机压力测试 ---
            println("\n=== Randomized Stress Test ===")
            val rand = new Random(42)
            for (_ <- 0 until 50) {
                val k_rand = rand.nextInt(params.maxK) + 1
                val val_rand = rand.nextInt(31)
                val stall_rand = rand.nextInt(3)
                val backpressure_rand = rand.nextInt(4)
                runDecodeTest(
                  dut,
                  k_rand,
                  val_rand,
                  stall_rand,
                  backpressure_rand
                )
            }
        }
    }
}
