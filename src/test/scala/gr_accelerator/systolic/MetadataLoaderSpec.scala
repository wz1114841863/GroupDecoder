package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.Queue

import gr_accelerator.common._

class MetadataLoaderSpec extends AnyFreeSpec with Matchers with ChiselSim {

    implicit val p: MetaSRAMParams = MetaSRAMParams.default

    "MetadataLoader (Pipelined)" - {
        "should burst-read metadata and output parallel vectors" in {
            simulate(new MetadataLoader(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                val N = p.N
                val baseAddr = 0
                val SRAM_LATENCY = 1 // MetaSRAMBuffer 的延迟

                println(s"--- [MetadataLoaderSpec] Start Burst Loading (N=$N) ---")

                dut.io.start.poke(true.B)
                dut.io.base_group_idx.poke(baseAddr.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var cycles = 0
                val timeout = N + 20 // 应该非常快 (N + Latency)

                // 模拟 SRAM 流水线: Queue[(LatencyLeft, Data)]
                case class PendingMeta(var latency: Int, zp: Int, scale: Int)
                val sramPipe = Queue[PendingMeta]()

                // 追踪发出的请求数,避免测试脚本在 sDone 后继续 enqueue
                var req_count = 0

                while (dut.io.done.peek().litValue == 0 && cycles < timeout) {

                    // --- 1. 处理 SRAM 响应 (Output) ---
                    sramPipe.foreach(_.latency -= 1)

                    if (sramPipe.nonEmpty && sramPipe.front.latency == 0) {
                        val resp = sramPipe.dequeue()
                        dut.io.sram_read_zp.poke(resp.zp.U)
                        dut.io.sram_read_scale.poke(resp.scale.U)
                    } else {
                        // 保持或设为0
                    }

                    // --- 2. 处理 SRAM 请求 (Input) ---
                    // 只有在 sBurst 期间 (cycles >= 1 且 req_count < N) 才会有有效请求
                    // Start 在 Cycle 0 拉高,硬件在 Cycle 1 进入 sBurst
                    if (cycles >= 1 && req_count < N) {
                        val addr = dut.io.sram_read_addr.peek().litValue.toInt

                        // 模拟数据: ZP = Addr & 0xFF, Scale = Addr * 2
                        val zp = addr & 0xff
                        val scale = (addr * 2) & 0xffff

                        // 推入管道
                        sramPipe.enqueue(PendingMeta(SRAM_LATENCY, zp, scale))
                        req_count += 1
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                println(s"--- [MetadataLoaderSpec] Done in $cycles cycles ---")
                dut.io.done.expect(true.B)

                // --- 3. 验证 ---
                // 验证第 0 个和第 N-1 个
                val exp_zp_0 = (baseAddr + 0) & 0xff
                dut.io.array_zp_out(0).expect(exp_zp_0.U)

                val exp_zp_last = (baseAddr + N - 1) & 0xff
                dut.io.array_zp_out(N-1).expect(exp_zp_last.U)

                println(s"[MetadataLoaderSpec] Verification Passed. Latency efficient!")
            }
        }
    }
}
