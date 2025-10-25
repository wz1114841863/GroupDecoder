package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.Queue
import scala.util.Random

class GroupDecoderSpec extends AnyFreeSpec with Matchers with ChiselSim {

    val params = GroupDecoderParams(
      groupSize = 32,
      nBits = 4,
      kOptions = Seq(1, 2, 3),
      brp = BitstreamReaderParams(streamWidth = 32, bufferWidth = 64)
    )

    object GoldenCompressor {
        def mapSignedToUnsigned(delta: Int): Int = {
            if (delta >= 0) 2 * delta else -2 * delta - 1
        }
        def golombRiceEncoder(value: Int, k: Int): String = {
            val q = value >> k
            val r = value & ((1 << k) - 1)
            "1" * q + "0" + String
                .format(s"%${k}s", r.toBinaryString)
                .replace(' ', '0')
        }
        def compressGroup(weights: Seq[Int], zp: Int): String = {
            val deltas = weights.map(_ - zp)
            val mappedDeltas = deltas.map(mapSignedToUnsigned)
            var bestK = -1; var bestBitstream = ""; var minBits = Int.MaxValue
            for (k <- params.kOptions) {
                val currentBitstream =
                    mappedDeltas.map(golombRiceEncoder(_, k)).mkString
                if (currentBitstream.length < minBits) {
                    minBits = currentBitstream.length; bestK = k;
                    bestBitstream = currentBitstream
                }
            }
            val originalBits = weights.size * params.nBits
            val compressedBits = bestBitstream.length + params.flagBits
            if (compressedBits >= originalBits) {
                val fallbackFlag = String
                    .format(
                      s"%${params.flagBits}s",
                      params.fallbackFlagVal.toBinaryString
                    )
                    .replace(' ', '0')
                val rawBits = weights
                    .map(w =>
                        String
                            .format(s"%${params.nBits}s", w.toBinaryString)
                            .replace(' ', '0')
                    )
                    .mkString
                fallbackFlag + rawBits
            } else {
                // ==================== 核心修复点 1 ====================
                // 从比较 Chisel UInt 改为查找 Scala Int 的索引,更健壮
                val flagValue = params.kOptions.indexOf(bestK)
                val kFlag = String
                    .format(s"%${params.flagBits}s", flagValue.toBinaryString)
                    .replace(' ', '0')
                // ======================================================
                kFlag + bestBitstream
            }
        }
    }

    def runGroupTest(
        dut: GroupDecoder,
        originalWeights: Seq[Int],
        zp: Int,
        backpressureOdds: Double = 0.0
    ): Unit = {
        val bitstream = GoldenCompressor.compressGroup(originalWeights, zp)
        val streamWidth = params.brp.streamWidth
        val bitstreamQueue = Queue.from(
          bitstream
              .grouped(streamWidth)
              .map(chunk => BigInt(chunk.padTo(streamWidth, '0'), 2))
        )
        val receivedQueue = Queue[Int]()
        val rand = new Random(42)
        var timeout = 0
        val maxCycles = (bitstream.length + originalWeights.size) * 5

        dut.io.start.poke(true.B)
        dut.io.zero_point.poke(zp.U)
        dut.clock.step(1)
        dut.io.start.poke(false.B)

        while (
          receivedQueue.size < originalWeights.size && timeout < maxCycles
        ) {
            if (
              bitstreamQueue.nonEmpty && dut.io.inputStream.ready
                  .peek()
                  .litToBoolean
            ) {
                dut.io.inputStream.valid.poke(true.B)
                dut.io.inputStream.bits.poke(bitstreamQueue.dequeue().U)
            } else {
                dut.io.inputStream.valid.poke(false.B)
            }

            if (rand.nextDouble() > backpressureOdds) {
                dut.io.quantized_weight.ready.poke(true.B)
            } else {
                dut.io.quantized_weight.ready.poke(false.B)
            }

            // ==================== 核心修复点 2 ====================
            // 手动在 Scala 中实现 .fire 的逻辑
            val isValid = dut.io.quantized_weight.valid.peek().litToBoolean
            val isReady = dut.io.quantized_weight.ready.peek().litToBoolean
            if (isValid && isReady) {
                val received =
                    dut.io.quantized_weight.bits.peek().litValue.toInt
                receivedQueue.enqueue(received)
            }
            // ======================================================

            dut.clock.step(1)
            timeout += 1
        }

        require(
          timeout < maxCycles,
          s"Test timed out! Received ${receivedQueue.size}/${originalWeights.size} weights."
        )

        while (!dut.io.done.peek().litToBoolean) {
            dut.io.quantized_weight.ready.poke(true.B)
            dut.clock.step(1)
        }
        dut.io.done.expect(
          true.B,
          "Done signal should be high after group is finished"
        )
        receivedQueue.toSeq must equal(originalWeights)
    }

    // 测试用例部分保持不变
    "GroupDecoder should correctly decode a group compressed with Golomb-Rice" in {
        simulate(new GroupDecoder(params)) { dut =>
            val zp = 8
            val originalWeights =
                Seq.fill(params.groupSize)(zp).zipWithIndex.map { case (z, i) =>
                    (z + (i % 5) - 2).max(0).min(15)
                }
            println(
              s"\n--- Testing Golomb-Rice Path (Group Size: ${params.groupSize}) ---"
            )
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            runGroupTest(dut, originalWeights, zp)
        }
    }

    "it should correctly decode a group stored in fallback (Raw) format" in {
        simulate(new GroupDecoder(params)) { dut =>
            val rand = new Random(1337)
            val zp = 8
            val originalWeights = Seq.fill(params.groupSize)(rand.nextInt(16))
            println(
              s"\n--- Testing Fallback (Raw) Path (Group Size: ${params.groupSize}) ---"
            )
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            runGroupTest(dut, originalWeights, zp)
        }
    }

    "it should handle backpressure from the downstream consumer" in {
        simulate(new GroupDecoder(params)) { dut =>
            val zp = 8
            val originalWeights =
                Seq.fill(params.groupSize)(zp).zipWithIndex.map { case (z, i) =>
                    (z + (i % 5) - 2).max(0).min(15)
                }
            println(s"\n--- Testing with 30% Downstream Backpressure ---")
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B)
            runGroupTest(dut, originalWeights, zp, backpressureOdds = 0.3)
        }
    }
}
