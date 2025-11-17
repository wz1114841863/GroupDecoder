package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import gr_accelerator.common._

/** MetaSRAMBufferSpec 测试模块
  */
class MetaSRAMBufferSpec extends AnyFreeSpec with Matchers with ChiselSim {

    implicit val p: MetaSRAMParams = MetaSRAMParams.default

    "MetaSRAMBuffer" - {

        "should correctly write ZP/Scale to Bank A and read back" in {
            simulate(new MetaSRAMBuffer(p)) { dut =>
                println(
                  "--- [MetaSRAMSpec] Start Test: Basic Write A / Read A ---"
                )
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)

                // 初始化: flip=false (写 A)
                dut.io.flip.poke(false.B)
                dut.io.scale_write_port.valid.poke(false.B)
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // --- 1. 写入 Bank A ---

                // 1a. 写 ZP
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(10.U)
                dut.io.zp_write_ports(0).wave_index.poke(0.U) // Addr 0

                dut.io.zp_write_ports(1).valid.poke(true.B)
                dut.io.zp_write_ports(1).zp.poke(20.U)
                dut.io.zp_write_ports(1).wave_index.poke(1.U) // Addr 9 (1*8+1)

                // 1b. 写 Scale
                dut.io.scale_write_port.valid.poke(true.B)
                dut.io.scale_write_port.addr.poke(0.U)
                dut.io.scale_write_port.data.poke(100.U)

                dut.clock.step(1) // 数据写入 Bank A

                // 写第二个 Scale
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)
                dut.io.scale_write_port.addr.poke(9.U)
                dut.io.scale_write_port.data.poke(200.U)

                dut.clock.step(1)
                dut.io.scale_write_port.valid.poke(false.B)

                println(
                  "--- [MetaSRAMSpec] Write A Complete, Switching to Read A ---"
                )

                // --- 2. 切换状态以读取 Bank A ---
                // [FIXED] flip 必须保持为 true 才能持续从 Bank A 读取
                dut.io.flip.poke(true.B)

                // --- 3. 读取 Bank A ---

                // 读 Row 0
                dut.io.read_addr.poke(0.U)

                dut.clock.step(1) // SyncReadMem 延迟

                // 验证 Row 0
                dut.io.read_zp.expect(10.U)
                dut.io.read_scale.expect(100.U)

                // 读 Row 9
                dut.io.read_addr.poke(9.U)

                dut.clock.step(1)

                // 验证 Row 9
                dut.io.read_zp.expect(20.U)
                dut.io.read_scale.expect(200.U)

                println("--- [MetaSRAMSpec] Read A Verified ---")
            }
        }

        "should handle simultaneous Write B / Read A" in {
            simulate(new MetaSRAMBuffer(p)) { dut =>
                println("--- [MetaSRAMSpec] Start Test: Simultaneous Ops ---")
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)

                // 1. 预填充 Bank A (flip=false: 写 A)
                dut.io.flip.poke(false.B)
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(0xaa.U) // ZP A
                dut.io.zp_write_ports(0).wave_index.poke(0.U) // Addr 0
                dut.clock.step(1)

                // 清除写信号
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // 2. 切换状态 (flip=true: 读 A, 写 B)
                // [FIXED] 保持 flip 为 true
                dut.io.flip.poke(true.B)

                // 3. 同时操作

                // 3a. 从 A 读取 Addr 0
                dut.io.read_addr.poke(0.U)

                // 3b. 向 B 写入 Addr 0
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(0xbb.U) // ZP B
                dut.io.zp_write_ports(0).wave_index.poke(0.U)

                dut.clock.step(1) // 等待读延迟 / 写提交

                // 4. 验证读取结果 (应为 A 的数据)
                dut.io.read_zp.expect(0xaa.U)

                // 清除写信号
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // 5. 再次切换 (flip=false: 读 B, 写 A)
                dut.io.flip.poke(false.B)

                // 6. 从 B 读取 (State: Read B)
                dut.io.read_addr.poke(0.U)
                dut.clock.step(1)

                // 验证读取结果 (应为 B 的数据)
                dut.io.read_zp.expect(0xbb.U)

                println("--- [MetaSRAMSpec] Simultaneous Ops Verified ---")
            }
        }
    }
}
