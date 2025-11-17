package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import gr_accelerator.common._

/** WeightSRAMSpec 测试模块
  */
class WeightSRAMSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // --- 1. 辅助函数 ---
    val stressParams = WeightSRAMParams(
      decoderParams = DecoderBankParams.default,
      saParams = SystolicArrayParams(N = 128)
    )
    implicit val p: WeightSRAMParams = stressParams

    def resetPorts(dut: WeightSRAM): Unit = {
        dut.io.flip.poke(false.B)
        for (i <- 0 until p.P) {
            dut.io.write_ports(i).valid.poke(false.B)
            dut.io.write_ports(i).addr.poke(0.U)
            dut.io.write_ports(i).data.poke(0.U)
            dut.io.write_ports(i).wave_index.poke(0.U)
            dut.io.read_addrs(i).poke(0.U)
        }
    }

    /** 检查 P=8 个并行读取端口
      *   1. 显式指定位宽 2. (在调用时) 只检查已写入的端口
      */
    def checkReadData(
        dut: WeightSRAM,
        expected: Seq[Int],
        portsToCheck: Seq[Int]
    ): Unit = {
        for (i <- portsToCheck) {
            dut.io.read_data(i).expect(expected(i).U(p.weightWidth.W))
        }
    }

    def calcLogicalAddr(
        bankIndex: Int,
        waveIndex: Int,
        localOffset: Int
    ): Int = {
        val groupId = (waveIndex * p.P) + bankIndex
        val logicalAddr = (groupId * p.groupSize) + localOffset
        logicalAddr
    }

    // --- 2. 测试场景 ---

    "WeightSRAM (P=8, N=128) " - {

        "should correctly write to Bank A using wave_index and read back" in {
            simulate(new WeightSRAM(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)
                resetPorts(dut)

                // --- 1. 写入 Bank A (flip=0) ---
                dut.io.flip.poke(false.B)
                dut.io.write_ports(0).valid.poke(true.B)
                dut.io.write_ports(0).addr.poke(5.U)
                dut.io.write_ports(0).data.poke(0xa.U) // 10
                dut.io.write_ports(0).wave_index.poke(0.U)
                dut.io.write_ports(1).valid.poke(true.B)
                dut.io.write_ports(1).addr.poke(10.U)
                dut.io.write_ports(1).data.poke(0xb.U) // 11
                dut.io.write_ports(1).wave_index.poke(1.U)

                dut.clock.step(1) // 写入
                resetPorts(dut)

                // --- 2. 读取 Bank A (flip=1) ---
                dut.io.flip.poke(true.B)

                val logicalAddr0 = calcLogicalAddr(
                  bankIndex = 0,
                  waveIndex = 0,
                  localOffset = 5
                ) // 5
                val logicalAddr1 = calcLogicalAddr(
                  bankIndex = 1,
                  waveIndex = 1,
                  localOffset = 10
                ) // 4618

                dut.io.read_addrs(0).poke(logicalAddr0.U)
                dut.io.read_addrs(1).poke(logicalAddr1.U)

                dut.clock.step(1)
                // 移除对 T+1 的检查, 因为我们知道 T+1 是读延迟的中间态

                dut.clock.step(1)
                // 延迟 2 周期: 数据在 T+2 稳定
                // 创建一个完整的 P=8 期望数组 (尽管我们只关心 0 和 1)
                val expectedData = Array.fill(p.P)(0)
                expectedData(0) = 0xa // 10
                expectedData(1) = 0xb // 11

                // 只检查我们写入过的端口
                checkReadData(dut, expectedData.toSeq, portsToCheck = Seq(0, 1))
            }
        }

        "should correctly write to Bank B in parallel and read back" in {
            simulate(new WeightSRAM(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)
                resetPorts(dut)

                // --- 1. 写入 Bank B (flip=1) ---
                dut.io.flip.poke(true.B)
                val waveIdx = 2
                val goldenData = (1 to 8).toSeq // [1, 2, ... 8]

                for (i <- 0 until p.P) {
                    dut.io.write_ports(i).valid.poke(true.B)
                    dut.io.write_ports(i).addr.poke(i.U)
                    dut.io.write_ports(i).data.poke(goldenData(i).U)
                    dut.io.write_ports(i).wave_index.poke(waveIdx.U)
                }

                dut.clock.step(1)
                resetPorts(dut)

                // --- 2. 读取 Bank B (flip=0) ---
                dut.io.flip.poke(false.B)

                val logicalAddrs = (0 until p.P).map { i =>
                    calcLogicalAddr(
                      bankIndex = i,
                      waveIndex = waveIdx,
                      localOffset = i
                    )
                }

                for (i <- 0 until p.P) {
                    dut.io.read_addrs(i).poke(logicalAddrs(i).U)
                }

                dut.clock.step(2) // 等待 2 周期延迟

                // 检查所有 P=8 个端口
                checkReadData(dut, goldenData, portsToCheck = (0 until p.P))
            }
        }

        "should handle simultaneous read (Bank A) and write (Bank B)" in {
            simulate(new WeightSRAM(p)) { dut =>
                dut.reset.poke(true.B)
                dut.clock.step(1)
                dut.reset.poke(false.B)
                resetPorts(dut)

                // --- 1. 预加载 Bank A ---
                dut.io.flip.poke(false.B) // 写 A
                dut.io.write_ports(0).valid.poke(true.B)
                dut.io.write_ports(0).addr.poke(100.U)
                dut.io.write_ports(0).data.poke(0x7.U)
                dut.io.write_ports(0).wave_index.poke(0.U)
                dut.clock.step(1)
                resetPorts(dut)

                // --- 2. 同时 读 A / 写 B (flip=1) ---
                dut.io.flip.poke(true.B)

                val logicalAddrA = calcLogicalAddr(
                  bankIndex = 0,
                  waveIndex = 0,
                  localOffset = 100
                )
                dut.io.read_addrs(0).poke(logicalAddrA.U)

                dut.io.write_ports(1).valid.poke(true.B)
                dut.io.write_ports(1).addr.poke(200.U)
                dut.io.write_ports(1).data.poke(0x9.U)
                dut.io.write_ports(1).wave_index.poke(0.U)

                dut.clock.step(2) // 等待读延迟

                // 验证: 必须读到 A 的数据 0x7
                // 使用 checkReadData 辅助函数
                checkReadData(
                  dut,
                  Seq(0x7, 0, 0, 0, 0, 0, 0, 0),
                  portsToCheck = Seq(0)
                )

                // --- 3. 翻转, 验证写入 B 的数据 ---
                resetPorts(dut)
                dut.io.flip.poke(false.B) // 读 B

                val logicalAddrB = calcLogicalAddr(
                  bankIndex = 1,
                  waveIndex = 0,
                  localOffset = 200
                )
                dut.io.read_addrs(1).poke(logicalAddrB.U)

                dut.clock.step(2) // 等待读延迟

                // 验证: 必须读到 B 的数据 0x9
                // 使用 checkReadData 辅助函数
                checkReadData(
                  dut,
                  Seq(0, 0x9, 0, 0, 0, 0, 0, 0),
                  portsToCheck = Seq(1)
                )
            }
        }
    }

    "[STRESS TEST] should handle a full, parallel, multi-wave load" in {

        simulate(new WeightSRAM(p)) { dut => //  移除了 withAnnotations
            dut.reset.poke(true.B)
            dut.clock.step(1)
            dut.reset.poke(false.B)
            resetPorts(dut)

            val numWaves = p.bankDepth / p.groupSize // 2048 / 512 = 4
            val timeout = p.bankDepth + 100 // 2048 + 100 = 2148 周期

            // --- 1. 预加载 Bank B ---
            println("--- [Stress Test] Pre-loading Bank B ---")
            dut.io.flip.poke(true.B) // 写入 Bank B

            dut.io.write_ports(0).valid.poke(true.B)
            dut.io.write_ports(0).addr.poke(0.U)
            dut.io.write_ports(0).data.poke(0xa.U) // 标记 A
            dut.io.write_ports(0).wave_index.poke(0.U)

            // 修复: 必须在不同的周期或不同的端口写入
            // (我们将 wave=3 的写入移到端口 1, 避免冲突)
            dut.io.write_ports(1).valid.poke(true.B)
            dut.io.write_ports(1).addr.poke(511.U)
            dut.io.write_ports(1).data.poke(0xf.U) // 标记 F
            dut.io.write_ports(1).wave_index.poke(3.U)

            dut.clock.step(1)
            resetPorts(dut)

            // --- 2. 同时 读 B / 写 A (flip=0) ---
            println(
              s"--- [Stress Test] Starting simultaneous Read(B)/Write(A) for ${p.bankDepth} cycles ---"
            )
            dut.io.flip.poke(false.B) // 读 B, 写 A

            var readCycleCounter = 0

            for (cycle <- 0 until p.bankDepth) {

                // --- 2a. 生产者: 写入 Bank A ---
                val current_wave = cycle / p.groupSize
                val current_addr = cycle % p.groupSize

                for (i <- 0 until p.P) {
                    //  所有的计算都必须是 Scala Int
                    // Chisel .U 仅在 poke() 时使用
                    val write_data: Int =
                        ((current_wave & 0x3) << 2) | (i & 0x3)

                    dut.io.write_ports(i).valid.poke(true.B)
                    dut.io.write_ports(i).addr.poke(current_addr.U)
                    dut.io.write_ports(i).data.poke(write_data.U) // 在这里转换为 .U
                    dut.io.write_ports(i).wave_index.poke(current_wave.U)
                }

                // --- 2b. 消费者: 读取 Bank B ---
                if (readCycleCounter == 0) {
                    val logicalAddrA = calcLogicalAddr(0, 0, 0)
                    dut.io.read_addrs(0).poke(logicalAddrA.U)
                } else if (readCycleCounter == 5) {
                    // [FIX] 读取 B(1)
                    val logicalAddrF = calcLogicalAddr(1, 3, 511)
                    dut.io.read_addrs(1).poke(logicalAddrF.U)
                }

                // --- 2c. 消费者: 验证 Bank B (2 周期延迟后) ---
                if (readCycleCounter == 2) {
                    dut.io.read_data(0).expect(0xa.U(p.weightWidth.W)) // 验证标记 A
                } else if (readCycleCounter == 7) {
                    // [FIX] 验证 B(1)
                    dut.io.read_data(1).expect(0xf.U(p.weightWidth.W)) // 验证标记 F
                }

                dut.clock.step(1)
                readCycleCounter = (readCycleCounter + 1) % 10
            }

            println("--- [Stress Test] Write(A) complete ---")
            resetPorts(dut)
            dut.clock.step(2)

            // --- 3. 翻转, 验证 Bank A ---
            println("--- [Stress Test] Flipping buffers, verifying Bank A ---")
            dut.io.flip.poke(true.B) // 读 A

            // 检查 A(0), wave=0, addr=0
            val logicalAddr_A0 = calcLogicalAddr(0, 0, 0)
            dut.io.read_addrs(0).poke(logicalAddr_A0.U)
            dut.clock.step(2)
            //  使用 Scala Int 计算
            val expected_A0: Int = ((0 & 0x3) << 2) | (0 & 0x3)
            dut.io.read_data(0).expect(expected_A0.U(p.weightWidth.W))

            // 检查 A(7), wave=3, addr=511
            val logicalAddr_A_last = calcLogicalAddr(7, 3, 511)
            dut.io.read_addrs(7).poke(logicalAddr_A_last.U)
            dut.clock.step(2)
            // 使用 Scala Int 计算
            val expected_A_last: Int =
                ((3 & 0x3) << 2) | (7 & 0x3) // 0xC | 0x3 = 15 (0xF)
            dut.io.read_data(7).expect(expected_A_last.U(p.weightWidth.W))

            // 检查 A(3), wave=1, addr=100
            val logicalAddr_A_mid = calcLogicalAddr(3, 1, 100)
            dut.io.read_addrs(3).poke(logicalAddr_A_mid.U)
            dut.clock.step(2)
            // 使用 Scala Int 计算
            val expected_A_mid: Int =
                ((1 & 0x3) << 2) | (3 & 0x3) // 0x4 | 0x3 = 7
            dut.io.read_data(3).expect(expected_A_mid.U(p.weightWidth.W))

            println("--- [Stress Test] Bank A verification PASSED ---")
        }
    }
}
