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

/** TopAccelerator 正确性验证测试 * 验证目标:
  *   1. Weight = 7 (由 ZP=8, k=1, Data="01" 产生) 2. Act = 1 3. Tile Size = 8x8 4.
  *      预期结果 = 8 * (7 * 1) = 56
  */
class TopAcceleratorCorrectSpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 1. 测试配置
    val P_TEST = 4
    val N_TEST = 8
    val GS_TEST = 64

    val miniParams = TopAcceleratorParams(
      P = P_TEST,
      N = N_TEST,
      groupSize = GS_TEST
    )

    implicit val p: TopAcceleratorParams = miniParams

    // ==============================================================================
    // 2. 构造数据 (所见即所得的硬编码)
    // ==============================================================================
    // 我们需要构造 64 个权重.
    // Flag: "00" (k=1)
    // Weight Data: "01" (Value=1 -> Signed=-1 -> Weight=7)
    // Stream: "00" + "01" * 64

    // Byte 0: "00" (Flag) + "01" (W0) + "01" (W1) + "01" (W2) = "00010101" = 0x15
    // Byte 1-15: "01" (W3) + "01" (W4) + "01" (W5) + "01" (W6) = "01010101" = 0x55
    // Byte 16: "01" (W63) + "000000" (Padding) = "01000000" = 0x40

    val hardcoded_bytes = Array(
      0x15, // Byte 0 (含 Flag)
      0x55, 0x55, 0x55, 0x55, 0x55, // Bytes 1-5
      0x55, 0x55, 0x55, 0x55, 0x55, // Bytes 6-10
      0x55, 0x55, 0x55, 0x55, 0x55, // Bytes 11-15
      0x40, // Byte 16 (最后一个权重 + Padding)
      // Padding to 24 bytes (3 * 64-bit words)
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    )

    // 验证长度: 17 字节数据 + 7 字节填充 = 24 字节
    assert(hardcoded_bytes.length == 24, "Hardcoded bytes length error!")

    var streamDB = Map[Int, Int]() // Addr -> Byte
    var metaDB_ZP = Map[Int, Int]()
    var metaDB_Scale = Map[Int, Int]()

    // 填充 8 个 Group (对应 8 列)
    for (g <- 0 until N_TEST) {
        val baseAddr = g * 24
        for ((bVal, i) <- hardcoded_bytes.zipWithIndex) {
            streamDB += (baseAddr + i) -> bVal
        }
        // 设置 ZP = 8. (Weight = -1 + 8 = 7)
        metaDB_ZP += g -> 8
        metaDB_Scale += g -> 1
    }

    // ==============================================================================
    // 3. 辅助函数: 加载 DRAM
    // ==============================================================================
    def loadDRAM(dut: TopAccelerator): Unit = {
        // 1. Load Stream
        for ((addr, byteVal) <- streamDB) {
            dut.io.load_stream_in.valid.poke(true.B)
            dut.io.load_stream_in.bits.addr.poke(addr.U)
            dut.io.load_stream_in.bits.data.poke(byteVal.U)
            dut.clock.step(1)
        }
        dut.io.load_stream_in.valid.poke(false.B)

        // 2. Load Meta (ZP)
        for ((g, zp) <- metaDB_ZP) {
            dut.io.load_meta_in.valid.poke(true.B)
            dut.io.load_meta_in.bits.addr.poke(g.U)
            dut.io.load_meta_in.bits.data.zero_point.poke(zp.U)
            dut.io.load_meta_in.bits.data.start_byte_addr.poke((g * 24).U)
            dut.clock.step(1)
        }
        dut.io.load_meta_in.valid.poke(false.B)

        // 3. Load Scale
        for ((addr, scale) <- metaDB_Scale) {
            dut.io.load_scale_in.valid.poke(true.B)
            dut.io.load_scale_in.bits.addr.poke(addr.U)
            dut.io.load_scale_in.bits.data.poke(scale.U)
            dut.clock.step(1)
        }
        dut.io.load_scale_in.valid.poke(false.B)
    }

    // ==============================================================================
    // 4. 测试主体
    // ==============================================================================
    "TopAccelerator Correctness Test" - {
        "should compute MatMul correctly (Expect 56)" in {
            simulate(new TopAccelerator(miniParams)) { dut =>
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)

                // 加载数据
                loadDRAM(dut)

                println("--- [TopSpec] Starting Computation ---")
                dut.io.start.poke(true.B)
                dut.io.total_tiles.poke(1.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                // 状态机映射 (与 TopAccelerator.scala 一致)
                val sRun = 4

                var cycles = 0
                val timeout = 2000
                var current_state = 0 // sIdle

                val captured_results = Array.fill(N_TEST)(-1)
                var results_valid_count = 0

                while (cycles < timeout && results_valid_count < N_TEST) {
                    val state_val = dut.io.debug_state.peek().litValue.toInt

                    // 打印状态转换
                    if (state_val != current_state) {
                        println(
                          s"[Cycle $cycles] State Transition: $current_state -> $state_val"
                        )
                        current_state = state_val
                    }

                    // 在计算阶段 (sRun) 注入激活数据
                    if (current_state == sRun) {
                        for (r <- 0 until N_TEST) dut.io.act_in(r).poke(1.S)

                        // 监控输出 (持续更新最大值)
                        for (c <- 0 until N_TEST) {
                            val out = dut.io.sum_out(c).peek().litValue.toInt
                            if (out > captured_results(c)) {
                                captured_results(c) = out
                            }
                        }
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                println(
                  s"--- [TopSpec] Done. Captured: ${captured_results.mkString(",")}"
                )

                // 验证结果
                val expected = 56
                for (c <- 0 until N_TEST) {
                    assert(
                      captured_results(c) == expected,
                      s"Col $c Failed! Expected $expected, Got ${captured_results(c)}"
                    )
                }
                println("--- [TopSpec] ALL CORRECT! Result is 56. ---")
            }
        }
    }
}
