package gr_accelerator

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import gr_accelerator.common._
import gr_accelerator.systolic._

class TopAcceleratorIntegrationSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // --- 1. 测试配置 (P=4, N=8, GS=64) ---
    val P_TEST = 4
    val N_TEST = 8
    val GS_TEST = 64

    val testParams = TopAcceleratorParams(
      P = P_TEST,
      N = N_TEST,
      groupSize = GS_TEST
    )
    implicit val p: TopAcceleratorParams = testParams

    "TopAccelerator Integration (Debug Mode)" - {
        "should compute Matrix Multiplication (Raw W * A) correctly" in {
            simulate(new TopAccelerator(testParams)) { dut =>
                val rand = new Random(42)
                val numTiles = 2

                // ============================================================
                // 2. 数据准备 (Golden Model)
                // ============================================================
                println(
                  s"--- [Integration] Preparing Data (N=$N_TEST, Tiles=$numTiles) ---"
                )

                // A. 权重流: "00" + "01"*64 -> Raw Weight = 1
                val stream_pattern = ("00" + "01" * 64).padTo(3 * 64, '0')
                val raw_stream_chunks = (0 until 3).map { i =>
                    val chunkStr = stream_pattern.drop(i * 64).take(64)
                    var chunkBigInt = BigInt(0)
                    for (b <- 0 until 8) { // 8 bytes in 64 bits
                        val byteStr = chunkStr.drop(b * 8).take(8)
                        // 如果不足8位(padding), 补0
                        val safeByteStr = byteStr.padTo(8, '0')
                        val byteVal = Integer.parseInt(safeByteStr, 2)
                        // 放入 BigInt 的对应位置
                        chunkBigInt = chunkBigInt | (BigInt(byteVal) << (b * 8))
                    }
                    chunkBigInt
                }
                // B. 激活: 随机生成
                val input_activations = Array.fill(numTiles, N_TEST)(0)
                val expected_output = Array.fill(numTiles, N_TEST)(0)
                // 保存每一列的实际权重用于验证
                val column_weights = Array.fill(numTiles, N_TEST)(0)

                for (t <- 0 until numTiles) {
                    println(s"  Tile $t Input Activations:")
                    var sum_act = 0
                    for (r <- 0 until N_TEST) {
                        val act = rand.nextInt(11) - 5
                        input_activations(t)(r) = act
                        sum_act += act
                        print(f"$act%3d ")
                    }
                    println(s" (Sum=$sum_act)")

                    // 正确计算 Golden
                    for (c <- 0 until N_TEST) {
                        // 重新生成相同的随机 ZP (Random(42) 是确定性的)
                        val zp = 5 + rand.nextInt(8)
                        val w_val = -1 + zp
                        column_weights(t)(c) = w_val // 存下来给后面加载用

                        // Golden = Sum(Acts) * Weight[c]
                        expected_output(t)(c) = sum_act * w_val
                    }
                    println(
                      s"  Tile $t Expected Output: ${expected_output(t).mkString(", ")}"
                    )
                }

                // ============================================================
                // 3. 加载 DRAM
                // ============================================================
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)

                // Load Stream (保持不变)
                for (g <- 0 until numTiles * N_TEST) {
                    val baseAddr = g * 24
                    for (i <- 0 until 3) {
                        val chunk = raw_stream_chunks(i)
                        for (b <- 0 until 8) {
                            val byteVal = (chunk >> (b * 8)) & 0xff
                            dut.io.load_stream_in.valid.poke(true.B)
                            dut.io.load_stream_in.bits.addr
                                .poke((baseAddr + i * 8 + b).U)
                            dut.io.load_stream_in.bits.data.poke(byteVal.U)
                            dut.clock.step(1)
                        }
                    }
                }
                dut.io.load_stream_in.valid.poke(false.B)

                // Load Meta (使用之前生成的 column_weights 反推 ZP)
                for (t <- 0 until numTiles) {
                    for (c <- 0 until N_TEST) {
                        val g = t * N_TEST + c
                        val w_val = column_weights(t)(c)
                        val zp = w_val + 1 // 因为 w = -1 + zp

                        dut.io.load_meta_in.valid.poke(true.B)
                        dut.io.load_meta_in.bits.addr.poke(g.U)
                        dut.io.load_meta_in.bits.data.zero_point.poke(zp.U)
                        dut.io.load_meta_in.bits.data.start_byte_addr
                            .poke((g * 24).U)
                        dut.clock.step(1)
                    }
                }
                dut.io.load_meta_in.valid.poke(false.B)

                // Load Scale
                for (addr <- 0 until N_TEST) {
                    dut.io.load_scale_in.valid.poke(true.B)
                    dut.io.load_scale_in.bits.addr.poke(addr.U)
                    dut.io.load_scale_in.bits.data.poke(1.U)
                    dut.clock.step(1)
                }
                dut.io.load_scale_in.valid.poke(false.B)

                // ============================================================
                // 4. 执行推理
                // ============================================================
                println(s"--- [Integration] Starting Simulation ---")

                dut.io.start.poke(true.B)
                dut.io.total_tiles.poke(numTiles.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var cycles = 0
                val timeout = 4000
                val sRun = 3

                var current_tile = 0
                var tiles_passed = 0

                var cycles_in_run = 0
                var checked_this_tile = false

                while (cycles < timeout && tiles_passed < numTiles) {
                    val state = dut.io.debug_state.peek().litValue.toInt

                    if (state == sRun) {
                        cycles_in_run += 1

                        // 1. 注入激活
                        if (current_tile < numTiles) {
                            for (r <- 0 until N_TEST) {
                                dut.io
                                    .act_in(r)
                                    .poke(input_activations(current_tile)(r).S)
                            }
                        }

                        // 2. 检查输出
                        if (cycles_in_run == 1000 && !checked_this_tile) {
                            println(
                              s"[Check] Checking output for Tile $current_tile at cycle $cycles"
                            )

                            val hardware_output = (0 until N_TEST).map { c =>
                                dut.io.sum_out(c).peek().litValue.toInt
                            }

                            val expected = expected_output(current_tile)

                            println(
                              s"  HW Out: ${hardware_output.mkString(", ")}"
                            )
                            println(s"  Golden: ${expected.mkString(", ")}")

                            var match_cnt = 0
                            for (c <- 0 until N_TEST) {
                                if (hardware_output(c) == expected(c))
                                    match_cnt += 1
                            }

                            if (match_cnt == N_TEST) {
                                println(s"  -> Tile $current_tile PASSED!")
                                tiles_passed += 1
                                checked_this_tile = true
                            } else {
                                println(
                                  s"  -> Tile $current_tile FAILED mismatch!"
                                )
                            }
                        }

                    } else {
                        if (state == 2) { // sFlip
                            if (checked_this_tile) {
                                current_tile += 1
                                cycles_in_run = 0
                                checked_this_tile = false
                                println(
                                  s"[State] Flipped to Tile $current_tile"
                                )
                            }
                        }
                        for (r <- 0 until N_TEST) dut.io.act_in(r).poke(0.S)
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                if (tiles_passed == numTiles) {
                    println("--- [Integration] ALL TESTS PASSED ---")
                } else {
                    fail(
                      s"Failed: Only $tiles_passed / $numTiles tiles passed."
                    )
                }
            }
        }
    }
}
