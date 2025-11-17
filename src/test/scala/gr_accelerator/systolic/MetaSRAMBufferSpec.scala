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
    // Use default parameters (P=8, N=128)
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

                // Init
                dut.io.flip.poke(false.B) // State: False (Write A, Read B)
                dut.io.scale_write_port.valid.poke(false.B)
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // --- 1. Write to Bank A ---

                // 1a. Write ZP (Simulating DecoderBank)
                // Row 0 -> Wave 0, Core 0
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(10.U)
                dut.io.zp_write_ports(0).wave_index.poke(0.U)

                // Row 9 -> Wave 1, Core 1 (1 * 8 + 1 = 9)
                dut.io.zp_write_ports(1).valid.poke(true.B)
                dut.io.zp_write_ports(1).zp.poke(20.U)
                dut.io.zp_write_ports(1).wave_index.poke(1.U)

                // 1b. Write Scale (Simulating Top Controller)
                dut.io.scale_write_port.valid.poke(true.B)
                dut.io.scale_write_port.addr.poke(0.U)
                dut.io.scale_write_port.data.poke(100.U)

                dut.clock.step(1) // Data written to Bank A

                // Write another scale
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)
                dut.io.scale_write_port.addr.poke(9.U)
                dut.io.scale_write_port.data.poke(200.U)

                dut.clock.step(1) // Scale(9) written
                dut.io.scale_write_port.valid.poke(false.B)

                println("--- [MetaSRAMSpec] Write A Complete, Flipping ---")

                // --- 2. Flip Buffers ---
                // [FIX] Perform flip in a dedicated cycle
                dut.io.flip.poke(true.B)
                dut.clock.step(1) // State transitions False -> True
                dut.io.flip.poke(false.B)

                // --- 3. Read Bank A (State: True => Read A) ---

                // Read Row 0
                dut.io.read_addr.poke(0.U)

                dut.clock.step(1) // SyncReadMem Latency

                // Verify Row 0
                dut.io.read_zp.expect(10.U)
                dut.io.read_scale.expect(100.U)

                // Read Row 9
                dut.io.read_addr.poke(9.U)

                dut.clock.step(1)

                // Verify Row 9
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

                // 1. Pre-fill Bank A (State: False/Write A)
                dut.io.flip.poke(false.B)
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(0xaa.U) // ZP A
                dut.io.zp_write_ports(0).wave_index.poke(0.U) // Addr 0
                dut.clock.step(1)

                // Clear write
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // 2. Flip State (False -> True)
                // [FIX] Dedicated flip cycle
                dut.io.flip.poke(true.B)
                dut.clock.step(1)
                dut.io.flip.poke(false.B)

                // 3. Simultaneous Ops (State: True/Read A, Write B)

                // 3a. Read from A (Addr 0)
                dut.io.read_addr.poke(0.U)

                // 3b. Write to B (Addr 0)
                dut.io.zp_write_ports(0).valid.poke(true.B)
                dut.io.zp_write_ports(0).zp.poke(0xbb.U) // ZP B
                dut.io.zp_write_ports(0).wave_index.poke(0.U)

                dut.clock.step(1) // Wait for Read Latency / Write Commit

                // 4. Verify Read Result (Should be A's data)
                // If logic was wrong, we might read 0xBB or garbage.
                dut.io.read_zp.expect(0xaa.U)

                // Clear write
                for (i <- 0 until p.P)
                    dut.io.zp_write_ports(i).valid.poke(false.B)

                // 5. Flip Back (True -> False)
                dut.io.flip.poke(true.B)
                dut.clock.step(1)
                dut.io.flip.poke(false.B)

                // 6. Read Bank B (State: False/Read B)
                dut.io.read_addr.poke(0.U)
                dut.clock.step(1)

                // Verify Read Result (Should be B's data)
                dut.io.read_zp.expect(0xbb.U)

                println("--- [MetaSRAMSpec] Simultaneous Ops Verified ---")
            }
        }
    }
}
