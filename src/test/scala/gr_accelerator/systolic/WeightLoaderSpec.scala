package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.Queue

import gr_accelerator.common._

class WeightLoaderSpec extends AnyFreeSpec with Matchers with ChiselSim {
    implicit val p: WeightSRAMParams = WeightSRAMParams.default

    "WeightLoader (Pipelined)" - {
        "should correctly burst-read P-wide data and push N-wide rows" in {
            simulate(new WeightLoader(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                val N = p.N
                val P = p.P

                // 模拟 SRAM Pipeline:
                // Request (T) -> ... -> Data (T + Latency)
                // 使用 Queue 来存储 (剩余延迟, 数据内容)
                // WeightSRAM Latency = 2
                case class PendingReq(var latencyLeft: Int, data: Seq[Int])
                val sramPipe = Queue[PendingReq]()

                // 准备测试数据: Base Group Index 对齐 P
                val baseGrp = 0 // 0 % 8 == 0

                println(
                  s"--- [WeightLoaderSpec] Start Pipelined Loading (N=$N, P=$P) ---"
                )

                dut.io.start.poke(true.B)
                dut.io.base_group_idx.poke(baseGrp.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var rows_pushed = 0
                var cycles = 0
                val timeout = 200 // 应该非常快,几十个周期就完成了

                var req_count = 0
                val total_reqs =
                    (N / P) * N // SubBlocks * Rows = 2 * 16 = 32 个请求

                while (dut.io.done.peek().litValue == 0 && cycles < timeout) {

                    // --- 1. 处理 SRAM 响应 (Pipeline Output) ---
                    // 检查管道里是否有延迟归零的请求
                    var data_poked = false

                    // 遍历队列,减少所有请求的剩余延迟
                    // 注意: 这是一个简单的软件模拟模型
                    sramPipe.foreach { req => req.latencyLeft -= 1 }

                    // 检查队首是否就绪 (Latency=0)
                    if (sramPipe.nonEmpty && sramPipe.front.latencyLeft == 0) {
                        val req = sramPipe.dequeue()
                        for (i <- 0 until P) {
                            dut.io.sram_read_data(i).poke(req.data(i).U)
                        }
                        data_poked = true
                    } else {
                        // 如果没有有效数据,保持旧值或设为0 (SRAM行为)
                        // 这里不 poke,保持上一次的值,或者 poke 0
                    }

                    // --- 2. 处理 SRAM 请求 (Pipeline Input) ---
                    // 模拟数据生成: Data = Addr & 0xF
                    if (cycles >= 1 && req_count < total_reqs) {
                        val current_req_data = (0 until P).map { i =>
                            val addr =
                                dut.io.sram_read_addrs(i).peek().litValue.toInt
                            addr & 0xf
                        }
                        sramPipe.enqueue(PendingReq(2, current_req_data))
                        req_count += 1
                    }

                    // --- 3. 验证 Output Push ---
                    if (dut.io.array_load_en.peek().litValue == 1) {
                        rows_pushed += 1
                        val currentRowIdx = rows_pushed - 1

                        // 验证数据正确性 (Col 0 and Col N-1)
                        // Data = Addr & 0xF
                        // Addr = (Base + Col/P * P + Col%P) * GS + Row
                        // Addr = (Base + Col) * GS + Row

                        val expected_val_0 =
                            ((baseGrp + 0) * p.groupSize + currentRowIdx) & 0xf
                        dut.io.array_w_in(0).expect(expected_val_0.U)

                        val expected_val_last =
                            ((baseGrp + (N - 1)) * p.groupSize + currentRowIdx) & 0xf
                        dut.io.array_w_in(N - 1).expect(expected_val_last.U)

                        // println(s"[Time $cycles] Pushed Row $currentRowIdx")
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                println(
                  s"--- [WeightLoaderSpec] Done in $cycles cycles. Rows pushed: $rows_pushed ---"
                )
                dut.io.done.expect(true.B)
                assert(rows_pushed == N, s"Expected $N rows, got $rows_pushed")

                // 性能检查: 全流水线应该很快
                // 理论时间: N/P * N + Latency + Setup
                // N=16, P=8 -> SubBlocks=2. Rows=16. Total reqs = 32.
                // Burst time = 32 cycles. Latency = 2.
                // 应该在 35-40 周期左右完成.
                if (cycles < N * N) {
                    println("Performance Check: PASS (Burst mode active)")
                } else {
                    println(
                      s"Performance Check: WARNING (Took $cycles cycles, might not be fully pipelined)"
                    )
                }
            }
        }
    }
}
