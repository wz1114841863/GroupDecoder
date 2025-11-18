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

    "WeightLoader" - {
        "should correctly accumulate P-wide data and push N-wide rows" in {
            simulate(new WeightLoader(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                val N = p.N // 16
                val P = p.P // 8

                // --- 1. 模拟 SRAM 的延迟队列 ---
                // WeightSRAM 有 2 周期读取延迟 (SyncReadMem + RegNext)
                // 所以我们需要预填充 2 个空数据
                val sramLatencyQueue = Queue[Seq[Int]]()
                sramLatencyQueue.enqueue(Seq.fill(P)(0)) // Cycle 1 delay
                sramLatencyQueue.enqueue(Seq.fill(P)(0)) // Cycle 2 delay

                // --- 2. 启动加载 ---
                println(
                  s"--- [WeightLoaderSpec] Start Loading (N=$N, P=$P) ---"
                )

                // base_group_idx 必须对齐到 P (8)
                val validBaseGroupIdx = 96

                dut.io.start.poke(true.B)
                dut.io.base_group_idx.poke(validBaseGroupIdx.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var rows_pushed = 0
                var cycles = 0
                val timeout = 10000

                while (dut.io.done.peek().litValue == 0 && cycles < timeout) {

                    // --- A. SRAM 行为模拟 (读取请求) ---
                    // 模拟数据: Data = Addr & 0xF
                    val current_req_data = (0 until P).map { i =>
                        val addr =
                            dut.io.sram_read_addrs(i).peek().litValue.toInt
                        addr & 0xf
                    }

                    // --- B. 推入延迟队列 ---
                    sramLatencyQueue.enqueue(current_req_data)

                    // --- C. 返回数据 (延迟 2 周期后) ---
                    if (sramLatencyQueue.nonEmpty) {
                        val data_to_poke = sramLatencyQueue.dequeue()
                        for (i <- 0 until P) {
                            dut.io.sram_read_data(i).poke(data_to_poke(i).U)
                        }
                    }

                    // --- D. 验证 ---
                    if (dut.io.array_load_en.peek().litValue == 1) {
                        rows_pushed += 1
                        val currentRowIdx = rows_pushed - 1

                        // 动态验证第 0 个元素 (Col 0)
                        // Col 0 的数据来自 Group (Base + 0)
                        // Addr = GroupID * GroupSize + Row
                        val expected_val_0 =
                            ((validBaseGroupIdx + 0) * 512 + currentRowIdx) & 0xf
                        dut.io.array_w_in(0).expect(expected_val_0.U)

                        // 动态验证最后一个元素 (Col N-1)
                        // Col N-1 的数据来自 Group (Base + N - 1)
                        val lastIdx = N - 1
                        val expected_val_last =
                            ((validBaseGroupIdx + lastIdx) * 512 + currentRowIdx) & 0xf

                        dut.io.array_w_in(lastIdx).expect(expected_val_last.U)
                    }

                    dut.clock.step(1)
                    cycles += 1
                }

                println(
                  s"--- [WeightLoaderSpec] Done in $cycles cycles. Rows pushed: $rows_pushed ---"
                )
                dut.io.done.expect(true.B)
                assert(rows_pushed == N, s"Expected $N rows, got $rows_pushed")
            }
        }
    }
}
