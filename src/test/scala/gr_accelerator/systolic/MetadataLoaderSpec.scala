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

    "MetadataLoader" - {
        "should serially read metadata and output parallel vectors" in {
            simulate(new MetadataLoader(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                val N = p.N
                val baseAddr = 0

                println(
                  s"--- [MetadataLoaderSpec] Start Loading (N=$N, Base=$baseAddr) ---"
                )
                dut.io.start.poke(true.B)
                dut.io.base_group_idx.poke(baseAddr.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var cycles = 0
                val timeout = N * 3 + 100

                // SRAM 模型变量
                // 我们需要模拟 SyncReadMem:
                // T: Addr -> T+1: Data
                var last_cycle_addr = 0

                while (dut.io.done.peek().litValue == 0 && cycles < timeout) {

                    // 1. 获取当前周期的地址请求
                    val current_addr =
                        dut.io.sram_read_addr.peek().litValue.toInt

                    // 2. 计算数据 (基于 *上一个* 周期的地址,模拟 1 周期延迟)
                    // ZP = Addr & 0xFF, Scale = (Addr * 2) & 0xFFFF
                    val zp_data = last_cycle_addr & 0xff
                    val scale_data = (last_cycle_addr * 2) & 0xffff

                    // 3. 提供数据给 DUT
                    dut.io.sram_read_zp.poke(zp_data.U)
                    dut.io.sram_read_scale.poke(scale_data.U)

                    // 4. 更新状态
                    last_cycle_addr = current_addr

                    dut.clock.step(1)
                    cycles += 1
                }

                println(s"--- [MetadataLoaderSpec] Done in $cycles cycles ---")
                dut.io.done.expect(true.B)

                // --- 3. 验证最终并行输出 ---
                val exp_zp_0 = (baseAddr + 0) & 0xff
                val exp_scale_0 = ((baseAddr + 0) * 2) & 0xffff
                dut.io.array_zp_out(0).expect(exp_zp_0.U)
                dut.io.array_scale_out(0).expect(exp_scale_0.U)

                val lastIdx = N - 1
                val exp_zp_last = (baseAddr + lastIdx) & 0xff
                val exp_scale_last = ((baseAddr + lastIdx) * 2) & 0xffff

                dut.io.array_zp_out(lastIdx).expect(exp_zp_last.U)
                dut.io.array_scale_out(lastIdx).expect(exp_scale_last.U)

                println(
                  s"[MetadataLoaderSpec] Parallel output verified for N=$N."
                )
            }
        }
    }
}
