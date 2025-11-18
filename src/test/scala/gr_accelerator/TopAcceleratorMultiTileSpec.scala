package gr_accelerator

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

import gr_accelerator.common._
import gr_accelerator.decoder._
import gr_accelerator.systolic._

/** TopAccelerator Multi-Tile Continuous Processing Test Target: Verify
  * Ping-Pong buffer mechanism and state transitions Config: Total Tiles = 4
  * Verification Logic:
  *   - Tile 0, 2: ZP=8 -> Weight=7 -> Sum=56
  *   - Tile 1, 3: ZP=9 -> Weight=8 -> Sum=64
  */
class TopAcceleratorMultiTileSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 1. Test Configuration
    val P_TEST = 4
    val N_TEST = 8
    val GS_TEST = 64

    val miniParams = TopAcceleratorParams(
      P = P_TEST,
      N = N_TEST,
      groupSize = GS_TEST
    )

    implicit val p: TopAcceleratorParams = miniParams

    // 2. Construct Data (Hardcoded Bytes)
    // Stream: "00"(Flag k=1) + "01"*64 (Data)
    // Byte 0: 0x15 (00010101) -> Flag(00) + Data(01)...
    val hardcoded_bytes = Array(
      0x15, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
      0x55, 0x55, 0x55, 0x55, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    )

    val TOTAL_TILES = 4
    val TOTAL_GROUPS = TOTAL_TILES * N_TEST

    var streamDB = Map[Int, Int]()
    var metaDB_ZP = Map[Int, Int]()
    var metaDB_Scale = Map[Int, Int]()

    for (g <- 0 until TOTAL_GROUPS) {
        val baseAddr = g * 24
        for ((bVal, i) <- hardcoded_bytes.zipWithIndex) {
            streamDB += (baseAddr + i) -> bVal
        }

        // Meta ZP: Even Tiles -> 8, Odd Tiles -> 9
        val tileIdx = g / N_TEST
        val zp_val = if (tileIdx % 2 == 0) 8 else 9

        metaDB_ZP += g -> zp_val
        metaDB_Scale += g -> 1
    }

    // 3. Load DRAM Helper
    def loadDRAM(dut: TopAccelerator): Unit = {
        for ((addr, byteVal) <- streamDB) {
            dut.io.load_stream_in.valid.poke(true.B)
            dut.io.load_stream_in.bits.addr.poke(addr.U)
            dut.io.load_stream_in.bits.data.poke(byteVal.U)
            dut.clock.step(1)
        }
        dut.io.load_stream_in.valid.poke(false.B)

        for ((g, zp) <- metaDB_ZP) {
            dut.io.load_meta_in.valid.poke(true.B)
            dut.io.load_meta_in.bits.addr.poke(g.U)
            dut.io.load_meta_in.bits.data.zero_point.poke(zp.U)
            dut.io.load_meta_in.bits.data.start_byte_addr.poke((g * 24).U)
            dut.clock.step(1)
        }
        dut.io.load_meta_in.valid.poke(false.B)

        for (i <- 0 until N_TEST) {
            dut.io.load_scale_in.valid.poke(true.B)
            dut.io.load_scale_in.bits.addr.poke(i.U)
            dut.io.load_scale_in.bits.data.poke(1.U)
            dut.clock.step(1)
        }
        dut.io.load_scale_in.valid.poke(false.B)
    }

    // 4. Main Test
    "TopAccelerator Multi-Tile Test" - {
        "should process 4 Tiles consecutively with correct Ping-Pong behavior" in {
            simulate(new TopAccelerator(miniParams)) { dut =>
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)

                loadDRAM(dut)
                println("--- [MultiTileSpec] DRAM Loaded ---")

                println(
                  s"--- [MultiTileSpec] Starting Task: Total Tiles = $TOTAL_TILES ---"
                )
                dut.io.start.poke(true.B)
                dut.io.total_tiles.poke(TOTAL_TILES.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                // State Mapping (Must match Hardware)
                val sRun = 4

                var cycles = 0
                val timeout = 5000
                var current_state = 0

                val all_tile_results = ArrayBuffer[Array[Int]]()
                var current_tile_results = Array.fill(N_TEST)(-1)

                // Local counter to track cycles within a state
                var run_cycles = 0

                while (
                  cycles < timeout && all_tile_results.length < TOTAL_TILES
                ) {
                    val state_val = dut.io.debug_state.peek().litValue.toInt

                    if (state_val != current_state) {
                        println(
                          s"[Cycle $cycles] State: $current_state -> $state_val"
                        )

                        // Capture results when leaving sRun
                        if (current_state == sRun) {
                            println(
                              s"--- [MultiTileSpec] Tile Finished. Capturing Results ---"
                            )
                            println(
                              s"    Data: ${current_tile_results.mkString(",")}"
                            )
                            all_tile_results += current_tile_results.clone()
                            current_tile_results = Array.fill(N_TEST)(-1)
                        }

                        current_state = state_val
                        run_cycles = 0 // Reset local counter on state change
                    }

                    if (current_state == sRun) {
                        // [FIXED] 统一使用 40 周期 Flush
                        // 硬件已修改,保证最后一个 Tile 至少有 100 周期
                        // WeightLoader 加载约需 25-30 周期,40 周期足够避开干扰
                        if (run_cycles < 80) {
                            // FLUSH PHASE: 输入 0 清洗管道,并等待新权重加载完毕
                            for (r <- 0 until N_TEST) dut.io.act_in(r).poke(0.S)
                        } else {
                            // COMPUTE PHASE: 输入 1 进行计算
                            for (r <- 0 until N_TEST) dut.io.act_in(r).poke(1.S)

                            for (c <- 0 until N_TEST) {
                                val out =
                                    dut.io.sum_out(c).peek().litValue.toInt
                                if (out > current_tile_results(c)) {
                                    current_tile_results(c) = out
                                }
                            }
                        }
                        run_cycles += 1
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                println(
                  s"--- [MultiTileSpec] Simulation Done in $cycles cycles ---"
                )

                assert(
                  all_tile_results.length == TOTAL_TILES,
                  s"Expected $TOTAL_TILES tiles, got ${all_tile_results.length}"
                )

                for ((tileRes, idx) <- all_tile_results.zipWithIndex) {
                    val expected = if (idx % 2 == 0) 56 else 64
                    println(s"Verifying Tile $idx (Expect $expected)...")

                    for (c <- 0 until N_TEST) {
                        assert(
                          tileRes(c) == expected,
                          s"Tile $idx Col $c Failed: Got ${tileRes(c)}, Expected $expected"
                        )
                    }
                }

                println(
                  "--- [MultiTileSpec] ALL TILES PASSED! Ping-Pong works! ---"
                )
            }
        }
    }
}
