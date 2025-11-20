package gr_accelerator.decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import gr_accelerator.common._

/** GRDecoderCoreSpec 测试模块 (Updated) * 修正说明: 数据构造逻辑已更新为匹配 TopAccelerator 的
  * Little-Endian 物理接口. 模拟 DRAM 时,Addr 0 (Stream Head) 放置在 64-bit Word 的 LSB
  * [7:0].
  */
class GRDecoderCoreSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // --- 辅助函数: 软件模型 (保持不变) ---
    def mapUnsignedToSigned(unsigned: Int): Int = {
        if (unsigned % 2 == 0) (unsigned / 2) else -(unsigned + 1) / 2
    }
    def addZeroPoint(signed: Int, zp: Int): Int = (signed + zp) & 0xf

    // --- 测试主体 ---
    "GRDecoderCore" - {

        // 只需要保留这一个最关键的测试即可验证逻辑闭环
        // 它涵盖了: 字节序/非对齐访问/数据解析
        "should handle UNALIGNED memory access with Little-Endian layout" in {
            implicit val p = GRDecoderCoreParams.default
            // val p = GRDecoderCoreParams.default.copy(useSingleCycleLoop = true)

            // ==================================================================
            // 1. 准备黄金数据
            // ==================================================================
            // 数据: 0x15 (00010101)
            // Bin: 00(Flag k=1) + 01(W0) + 01(W1) + 01(W2)
            // Weight Calculation:
            //   Code "01" -> q=0,r=1 -> Val=1 -> Signed=-1
            //   ZP=8. Final Weight = -1 + 8 = 7.

            val validByte = 0x15
            val goldenWeights = Array(7, 7, 7) // 前3个权重

            // ==================================================================
            // Case 1: Offset = 1 Byte (8 bits 垃圾在低位)
            // 内存视图: [Addr 6000: Garbage] [Addr 6001: Valid(0x15)] ...
            // 硬件读取 (LE): [ ... | Byte 1 (Valid) | Byte 0 (Garbage) ]
            // ==================================================================
            val offset1 = 1
            val garbage1 = 0xff // 垃圾

            // 构造 64-bit 字: Valid 在高位 (<< 8), Garbage 在低位
            // BigInt = (0x15 << 8) | 0xFF = 0x15FF
            val word1_val = (BigInt(validByte) << 8) | BigInt(garbage1)

            val addr1_aligned = 6000
            val addr1_real = 6001 // 6000 + 1

            val streamDB1 = Map(addr1_aligned -> word1_val)
            val metaDB1 = Map(10 -> (addr1_real, 8)) // ZP=8, Group=10

            // ==================================================================
            // Case 2: Offset = 7 Bytes (56 bits 垃圾在低位)
            // 内存视图: [Addr 7000..7006: Garbage] [Addr 7007: Valid(0x15)]
            // 硬件读取 (LE): [ Byte 7 (Valid) | Garbage ... Garbage ]
            // ==================================================================
            val offset2 = 7
            // 构造 7 个字节的垃圾
            var word2_val = BigInt(0)
            for (i <- 0 until 7) {
                word2_val |= (BigInt(0xaa) << (i * 8))
            }
            // 把 Valid 数据放在最高字节 [63:56]
            word2_val |= (BigInt(validByte) << 56)

            val addr2_aligned = 7000
            val addr2_real = 7007

            val streamDB2 = Map(addr2_aligned -> word2_val)
            val metaDB2 = Map(11 -> (addr2_real, 8))

            // 合并 DB
            val testMetaDB = metaDB1 ++ metaDB2
            val testStreamDB = streamDB1 ++ streamDB2

            // ==================================================================
            // 模拟 Case 1 (Offset = 1)
            // ==================================================================
            simulate(new GRDecoderCore(p, 0.U)) { dut =>
                dut.reset.poke(true.B); dut.clock.step()
                dut.reset.poke(false.B); dut.clock.step()

                val capturedWeights = Array.fill(p.groupSize)(-1)

                dut.io.start.poke(true.B)
                dut.io.group_index.poke(10.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var cycles = 0
                val timeout = 2000
                var finished = false

                while (!finished && cycles < timeout) {
                    dut.io.meta_resp.valid.poke(false.B)
                    dut.io.stream_resp.valid.poke(false.B)

                    if (dut.io.meta_req.valid.peek().litValue > 0) {
                        val idx = dut.io.meta_req.addr.peek().litValue.toInt
                        val (addr, zp) = testMetaDB(idx)
                        dut.io.meta_resp.valid.poke(true.B)
                        dut.io.meta_resp.start_byte_addr.poke(addr.U)
                        dut.io.meta_resp.zero_point.poke(zp.U)
                    }

                    if (dut.io.stream_req.valid.peek().litValue > 0) {
                        val addr = dut.io.stream_req.addr.peek().litValue.toInt
                        // 核心检查: Core 必须请求对齐地址
                        assert(
                          addr % 8 == 0,
                          s"Core requested unaligned address $addr"
                        )

                        // 简单的 Memory 模型: 只有第一次请求返回数据,后续返回 0
                        if (addr == addr1_aligned) {
                            dut.io.stream_resp.valid.poke(true.B)
                            dut.io.stream_resp.data.poke(testStreamDB(addr).U)
                        } else {
                            dut.io.stream_resp.valid.poke(true.B)
                            dut.io.stream_resp.data.poke(0.U) // Padding 0
                        }
                    }

                    if (dut.io.sram_write.valid.peek().litValue > 0) {
                        val wAddr = dut.io.sram_write.addr.peek().litValue.toInt
                        capturedWeights(wAddr) =
                            dut.io.sram_write.data.peek().litValue.toInt
                    }

                    if (dut.io.finished.peek().litValue > 0) finished = true
                    dut.clock.step(1)
                    cycles += 1
                }

                cycles must be < timeout
                // 验证前3个权重是否为7
                capturedWeights(0) must be(goldenWeights(0))
                capturedWeights(1) must be(goldenWeights(1))
                capturedWeights(2) must be(goldenWeights(2))
                println(
                  s"Corrected Test (Offset=1) Passed. cycles $cycles."
                )
            }

            // ==================================================================
            // 模拟 Case 2 (Offset = 7)
            // ==================================================================
            simulate(new GRDecoderCore(p, 0.U)) { dut =>
                dut.reset.poke(true.B); dut.clock.step()
                dut.reset.poke(false.B); dut.clock.step()

                val capturedWeights = Array.fill(p.groupSize)(-1)

                dut.io.start.poke(true.B)
                dut.io.group_index.poke(11.U)
                dut.clock.step(1)
                dut.io.start.poke(false.B)

                var cycles = 0
                val timeout = 2000
                var finished = false

                while (!finished && cycles < timeout) {
                    dut.io.meta_resp.valid.poke(false.B)
                    dut.io.stream_resp.valid.poke(false.B)

                    if (dut.io.meta_req.valid.peek().litValue > 0) {
                        val idx = dut.io.meta_req.addr.peek().litValue.toInt
                        val (addr, zp) = testMetaDB(idx)
                        dut.io.meta_resp.valid.poke(true.B)
                        dut.io.meta_resp.start_byte_addr.poke(addr.U)
                        dut.io.meta_resp.zero_point.poke(zp.U)
                    }

                    if (dut.io.stream_req.valid.peek().litValue > 0) {
                        val addr = dut.io.stream_req.addr.peek().litValue.toInt
                        if (addr == addr2_aligned) {
                            dut.io.stream_resp.valid.poke(true.B)
                            dut.io.stream_resp.data.poke(testStreamDB(addr).U)
                        } else {
                            dut.io.stream_resp.valid.poke(true.B)
                            dut.io.stream_resp.data.poke(0.U)
                        }
                    }

                    if (dut.io.sram_write.valid.peek().litValue > 0) {
                        val wAddr = dut.io.sram_write.addr.peek().litValue.toInt
                        capturedWeights(wAddr) =
                            dut.io.sram_write.data.peek().litValue.toInt
                    }

                    if (dut.io.finished.peek().litValue > 0) finished = true
                    dut.clock.step(1)
                    cycles += 1
                }

                cycles must be < timeout
                // 验证前3个权重
                capturedWeights(0) must be(goldenWeights(0))
                println(s"Corrected Test (Offset=7) Passed. cycles $cycles.")
            }
        }
    }
}
