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

/** TopAccelerator 冒烟测试
  *
  * 目标: 验证从 DRAM 加载 -> 解码 -> 缓冲 -> 加载阵列 -> 计算 -> 输出 的完整链路
  *
  * 策略: 使用最小化参数 (P=4, N=8) 和简单数据模式进行 "冒烟测试" (Smoke Test)
  */
class TopAcceleratorSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // ==============================================================================
    // 1. 定义测试参数 (Mini Config)
    // ==============================================================================
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
    // 2. 准备测试数据 (Golden Model)
    // ==============================================================================

    // A. 权重流 (模拟 DRAM)
    // 目标逻辑权重: 7
    // 编码: ZP=8, k=1, stream="01" -> mapped=1 -> signed=-1 -> (-1+8)=7
    // 一个 Tile (8x8) 需要 8 个 Group (按列分块, 每列 1 个 Group)
    // 每个 Group (64 权重) 需要的比特流: "00" (Flag) + "01"*64 (Data) = 130 bits
    // 130 bits 填充到 64-bit 边界 -> 3 个 64-bit 块 (192 bits)

    val stream_pattern = ("00" + "01" * 64).padTo(3 * 64, '0')

    // 我们需要 8 个 Group 的数据 (对应 8 列)
    // Group 0..7
    var streamDB = Map[Int, BigInt]()
    var metaDB_ZP = Map[Int, Int]()
    var metaDB_Scale = Map[Int, Int]()

    for (g <- 0 until N_TEST) {
        // Stream: 每个 Group 占用 3 个块 (24 字节)
        // 地址简单的线性映射: Group * 24
        val baseAddr = g * 24
        val chunks = stream_pattern.grouped(64).toList
        for ((chunkStr, i) <- chunks.zipWithIndex) {
            streamDB += (baseAddr + i * 8) -> BigInt(chunkStr.reverse, 2)
        }

        // Meta: ZP=8
        metaDB_ZP += g -> 8

        // Scale: 1 (简单起见)
        metaDB_Scale += g -> 1
    }

    // B. 激活数据 (Input Activations)
    // 8x8 矩阵, 全部填 1
    val act_matrix = Array.fill(N_TEST)(Array.fill(N_TEST)(1))

    // ==============================================================================
    // 3. 辅助函数
    // ==============================================================================

    def loadDRAM(dut: TopAccelerator): Unit = {
        println(s"--- [TopSpec] Starting DRAM Pre-load ---")

        // 1. Load Stream
        // 注意: DecoderBank 期望的地址是字节地址
        for ((addr, data) <- streamDB) {
            // data 是 64-bit, 需要拆分成 8 个字节写入
            for (b <- 0 until 8) {
                val byteVal = (data >> (b * 8)) & 0xff
                dut.io.load_stream_in.valid.poke(true.B)
                dut.io.load_stream_in.bits.addr.poke((addr + b).U)
                dut.io.load_stream_in.bits.data.poke(byteVal.U)
                dut.clock.step(1)
            }
        }
        dut.io.load_stream_in.valid.poke(false.B)
        println(s"   -> Stream Loaded")

        // 2. Load Meta (ZP)
        // DecoderBank 期望的地址是 Group ID (对于简单的线性映射)
        // 同时需要提供 Start Byte Addr.
        // Group g 的 Start Addr = g * 24
        for ((g, zp) <- metaDB_ZP) {
            dut.io.load_meta_in.valid.poke(true.B)
            dut.io.load_meta_in.bits.addr.poke(g.U)
            dut.io.load_meta_in.bits.data.zero_point.poke(zp.U)
            dut.io.load_meta_in.bits.data.start_byte_addr.poke((g * 24).U)
            dut.clock.step(1)
        }
        dut.io.load_meta_in.valid.poke(false.B)
        println(s"   -> ZP Metadata Loaded")

        // 3. Load Scale
        // MetaSRAMBuffer 期望地址 0..N-1
        for ((addr, scale) <- metaDB_Scale) {
            dut.io.load_scale_in.valid.poke(true.B)
            dut.io.load_scale_in.bits.addr.poke(addr.U)
            dut.io.load_scale_in.bits.data.poke(scale.U)
            dut.clock.step(1)
        }
        dut.io.load_scale_in.valid.poke(false.B)
        println(s"   -> Scale Metadata Loaded")
    }

    // ==============================================================================
    // 4. 测试主体
    // ==============================================================================

    "TopAccelerator Integration Smoke Test" - {
        "should flow data from DRAM to Output without hanging" in {
            simulate(new TopAccelerator(miniParams)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                // 1. 初始化内存
                loadDRAM(dut)

                // 2. 启动任务
                println(s"--- [TopSpec] Starting Inference Task ---")
                dut.io.start.poke(true.B)
                dut.io.total_tiles.poke(1.U) // 只计算 1 个 Tile

                // 默认激活输入为 0
                for (i <- 0 until N_TEST) dut.io.act_in(i).poke(0.S)

                dut.clock.step(1)
                dut.io.start.poke(false.B)

                // 3. 运行循环与监控
                var cycles = 0
                val timeout = 3000

                // FSM 状态映射 (复制自 TopAccelerator.scala)
                val sIdle = 0; val sPrefill = 1; val sFlip = 2; val sRun = 3;
                val sDone = 5

                var current_state = sIdle
                var output_captured = false

                println(s"--- [TopSpec] Entering Simulation Loop ---")

                while (cycles < timeout && !output_captured) {
                    val state_val = dut.io.debug_state.peek().litValue.toInt

                    // 状态变化打印
                    if (state_val != current_state) {
                        val state_name = state_val match {
                            case 0 => "sIdle"
                            case 1 => "sPrefill (Decoding Tile 0)"
                            case 2 => "sFlip (Buffer Swap)"
                            case 3 => "sRun (Compute & Decode Next)"
                            case 5 => "sDone"
                            case _ => "Unknown"
                        }
                        println(
                          s"[Cycle $cycles] State Transition -> $state_name"
                        )
                        current_state = state_val
                    }

                    // 如果进入 sRun, 开始泵入激活数据
                    // (Systolic Array 需要 Skewed Input, 这里简单起见,
                    // 我们在 sRun 开始后的第 50 个周期 (等待 Loader 完成) 开始泵入)
                    // *更好的做法*: 监控 Loader 的 done 信号, 但 Top 没有暴露.
                    // 我们假设 Loader 很快.

                    // 只要在 sRun, 我们就尝试泵入激活
                    // 简单的 Skew 逻辑: T=0 feed Row 0, T=1 feed Row 1...
                    // 假设 sRun 开始于 T_start
                    if (current_state == sRun) {
                        // 简单的全 1 输入
                        for (r <- 0 until N_TEST) dut.io.act_in(r).poke(1.S)

                        // 检查输出
                        // 如果全是 7 的权重 * 全是 1 的激活
                        // 一个输出应该是 7 * 1 + 7 * 1 ... (Accumulation)
                        // 实际上这是一个 Tile 乘法.
                        // 我们主要看有没有非零输出,证明链路通了.

                        var nonzero = false
                        for (c <- 0 until N_TEST) {
                            val out = dut.io.sum_out(c).peek().litValue.toInt
                            if (out != 0) {
                                print(s"$out ")
                                nonzero = true
                            }
                        }
                        if (nonzero) {
                            println(
                              s"\n[Cycle $cycles] !!! OUTPUT DETECTED !!!"
                            )
                            output_captured = true // 测试成功
                        }
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                if (cycles >= timeout) {
                    println(s"--- [TopSpec] TIMEOUT reached! ---")
                    fail("Simulation timed out without producing output.")
                } else {
                    println(
                      s"--- [TopSpec] SUCCESS: Data flowed through the entire system! ---"
                    )
                }
            }
        }
    }
}
