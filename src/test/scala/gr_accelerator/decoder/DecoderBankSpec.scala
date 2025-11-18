package gr_accelerator.decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

import gr_accelerator.common._

/** DecoderBankSpec 测试模块 (Redesigned) * 适配 Little-Endian 物理内存布局:
  *   - Stream String: "Head...Tail"
  *   - DRAM/SRAM Word: [Tail Byte ... Head Byte(LSB)]
  */
class DecoderBankSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // ==============================================================================
    // 1. 软件黄金模型 (Software Golden Model)
    // ==============================================================================

    def mapUnsignedToSigned(unsigned: Int): Int = {
        if (unsigned % 2 == 0) (unsigned / 2)
        else -(unsigned + 1) / 2
    }

    def addZeroPoint(signed: Int, zp: Int): Int = {
        (signed + zp) & 0xf
    }

    // 软件 GR 解码器 (处理字符串流)
    def decodeGR(bitStream: String, k: Int): (Int, Int, String) = {
        var q = 0
        var bits = bitStream
        // 1. 解码 q (Unary)
        while (bits.nonEmpty && bits.startsWith("1")) {
            q += 1; bits = bits.drop(1)
        }
        if (bits.isEmpty) return (-1, q + 1, "")
        bits = bits.drop(1) // 消耗 '0' 分隔符

        // 2. 解码 r (Binary)
        if (bits.length < k) return (-1, q + 1 + k, "")
        val r = if (k > 0) Integer.parseInt(bits.take(k), 2) else 0
        bits = bits.drop(k)

        ((q << k) | r, q + 1 + k, bits)
    }

    // 软件 Fallback 解码器
    def decodeFB(bitStream: String): (Int, Int, String) = {
        if (bitStream.length < 4) return (-1, 4, "")
        val weight = Integer.parseInt(bitStream.take(4), 2)
        (weight, 4, bitStream.drop(4))
    }

    /** 从 LE 存储的 DB 中重建流并生成黄金权重 */
    def genGoldenWeights(
        p: GRDecoderCoreParams,
        groupIndex: Int,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt]
    ): Array[Int] = {
        val (startAddr, zp) = metaDB(groupIndex)
        var fullBitStream = ""
        var currentAddr = startAddr

        // 读取足够多的 Chunk
        val chunksToRead = (p.groupSize * 4 / 64) + 5 // 估算: 最差情况全fallback + 余量

        for (_ <- 0 until chunksToRead) {
            val word = streamDB.getOrElse(currentAddr, BigInt(0))

            // [关键修复] 从 Little-Endian Word 重建 Stream String
            // Word: [Byte7 ... Byte0(LSB)]
            // Stream: Byte0 + Byte1 ...
            for (b <- 0 until 8) {
                val byteVal = (word >> (b * 8)) & 0xff
                // 格式化为 8位 01 字符串
                val byteStr =
                    String.format("%8s", byteVal.toString(2)).replace(' ', '0')
                fullBitStream += byteStr
            }
            currentAddr += 8 // 字节地址 +8
        }

        // 开始解码
        // 解析 Flag (2 bits)
        val flagStr = fullBitStream.take(2)
        val flag = Integer.parseInt(flagStr, 2)
        var bits = fullBitStream.drop(2)

        val golden = Array.fill(p.groupSize)(0)
        val flag_fallback = p.grDecoderConfig.grKInWidth + 1 // 2

        if (flag == flag_fallback) { // Fallback
            for (i <- 0 until p.groupSize) {
                val (weight, _, nextBits) = decodeFB(bits)
                golden(i) = weight
                bits = nextBits
            }
        } else { // GR
            val k = flag + 1
            for (i <- 0 until p.groupSize) {
                val (mapped, _, nextBits) = decodeGR(bits, k)
                val signed = mapUnsignedToSigned(mapped)
                golden(i) = addZeroPoint(signed, zp)
                bits = nextBits
            }
        }
        golden
    }

    // ==============================================================================
    // 2. 数据构造辅助函数 (Data Construction Helper)
    // ==============================================================================

    /** * 将比特流字符串切片并打包进 Map (模拟 DRAM) 策略: Little-Endian Packing String: "Head
      * ... Tail" Word: [... | Byte1 | Byte0(Head)]
      */
    def sliceStreamToDB(
        startAddr: Int,
        stream: String,
        chunkBytes: Int // 8 for 64-bit
    ): Map[Int, BigInt] = {
        // 1. 补齐流长度到 64-bit 边界
        val paddedLen = ((stream.length + 63) / 64) * 64
        val paddedStream = stream.padTo(paddedLen, '0')

        paddedStream
            .grouped(chunkBytes * 8) // 每 64 chars 一组
            .zipWithIndex
            .map { case (chunkStr, i) =>
                // chunkStr 是 64 个 '0'/'1'
                // 我们需要按字节拆分,第一个字节放 LSB
                val bytesStr = chunkStr.grouped(8).toList
                var word = BigInt(0)

                for ((byteStr, byteIdx) <- bytesStr.zipWithIndex) {
                    // byteIdx 0 是字符串最左边的字节 (Head) -> 放入 Word 的 LSB (shift 0)
                    val byteVal = BigInt(byteStr, 2)
                    word |= (byteVal << (byteIdx * 8))
                }
                (startAddr + i * chunkBytes) -> word
            }
            .toMap
    }

    // ==============================================================================
    // 3. 测试配置与数据准备
    // ==============================================================================

    // --- 参数配置 ---
    val pressureTestConfig = DecoderBankParams(
      coreParams = GRDecoderCoreParams.default.copy(groupSize = 128),
      P = 2
    )

    // --- 构造测试流 (Bitstream Strings) ---
    // G0: k=1, "01" -> -1 -> +ZP
    val str_G0 = "00" + "01" * 128
    // G1: Fallback, "0101" -> 5
    val str_G1 = "10" + "0101" * 128
    val str_G2 = str_G0
    val str_G3 = str_G0

    // MetaDB: (Addr, ZP)
    // ZP 保持不同,用于验证数据是否去往了正确的 Core
    val metaDB_shared = Map(
      0 -> (1000, 8), // G0 -> Core 0 (Result: -1+8=7)
      1 -> (2000, 5), // G1 -> Core 1 (Result: 5)
      2 -> (3000, 7), // G2 -> Core 0 (Result: -1+7=6) <-- 只要结果是 6,就说明 Core 0 读到了 G2 的 ZP
      3 -> (4000, 10) // G3 -> Core 1 (Result: -1+10=9)
    )

    // StreamDB: 通过 sliceStreamToDB 生成
    val streamDB_shared =
        sliceStreamToDB(1000, str_G0, 8) ++
            sliceStreamToDB(2000, str_G1, 8) ++
            sliceStreamToDB(3000, str_G2, 8) ++
            sliceStreamToDB(4000, str_G3, 8)

    // ==============================================================================
    // 4. 内存预加载辅助函数
    // ==============================================================================
    def preloadMemory(
        dut: DecoderBank,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt],
        p: DecoderBankParams
    ): Unit = {
        // Load Meta
        for ((grpId, (byteAddr, zp)) <- metaDB) {
            dut.io.load_meta.valid.poke(true.B)
            dut.io.load_meta.bits.addr.poke(grpId.U)
            dut.io.load_meta.bits.data.start_byte_addr.poke(byteAddr.U)
            dut.io.load_meta.bits.data.zero_point.poke(zp.U)
            dut.clock.step(1)
        }
        dut.io.load_meta.valid.poke(false.B)

        // Load Stream (按字节写入 SharedCache)
        for ((wordAddr, wordData) <- streamDB) {
            for (b <- 0 until 8) {
                val byteVal = (wordData >> (b * 8)) & 0xff
                dut.io.load_stream.valid.poke(true.B)
                dut.io.load_stream.bits.addr.poke((wordAddr + b).U)
                dut.io.load_stream.bits.data.poke(byteVal.U)
                dut.clock.step(1)
            }
        }
        dut.io.load_stream.valid.poke(false.B)
    }

    // ==============================================================================
    // 5. 测试用例
    // ==============================================================================

    "DecoderBank" - {
        "[PRESSURE TEST] should verify LE data loading and dispatching (P=2, N=4, gs=128)" in {
            implicit val p: DecoderBankParams = pressureTestConfig

            // 1. 生成黄金结果
            val golden_G0 = genGoldenWeights(
              p.coreParams,
              0,
              metaDB_shared,
              streamDB_shared
            )
            val golden_G1 = genGoldenWeights(
              p.coreParams,
              1,
              metaDB_shared,
              streamDB_shared
            )
            val golden_G2 = genGoldenWeights(
              p.coreParams,
              2,
              metaDB_shared,
              streamDB_shared
            )
            val golden_G3 = genGoldenWeights(
              p.coreParams,
              3,
              metaDB_shared,
              streamDB_shared
            )

            // Core 0 处理 G0, G2 (偶数 ID)
            val golden_Core0 = golden_G0 ++ golden_G2
            // Core 1 处理 G1, G3 (奇数 ID)
            val golden_Core1 = golden_G1 ++ golden_G3

            simulate(new DecoderBank(p)) { dut =>
                dut.reset.poke(true.B); dut.clock.step();
                dut.reset.poke(false.B);

                // 2. 预加载
                println("--- Preloading Memory ---")
                preloadMemory(dut, metaDB_shared, streamDB_shared, p)
                println("--- Preload Done ---")
                dut.clock.step(5)

                // 3. 启动
                dut.io.start_req.valid.poke(true.B)
                dut.io.start_req.bits.base_group_index.poke(0.U)
                dut.io.start_req.bits.num_groups_to_decode.poke(4.U)
                dut.clock.step(1)
                dut.io.start_req.valid.poke(false.B)

                // 4. 捕获与监控
                val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
                var cycles = 0
                val timeout = 2000

                while (
                  dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
                ) {
                    for (i <- 0 until p.P) {
                        if (
                          dut.io.sram_write_outputs(i).valid.peek().litValue > 0
                        ) {
                            val data = dut.io
                                .sram_write_outputs(i)
                                .data
                                .peek()
                                .litValue
                                .toInt
                            capturedWrites(i) += data
                        }
                    }
                    dut.clock.step(1)
                    cycles += 1
                }

                println(s"Finished in $cycles cycles.")
                cycles must be < timeout

                // 5. 验证
                // Core 0
                println(
                  s"Core 0: Captured ${capturedWrites(0).length}, Expected ${golden_Core0.length}"
                )
                capturedWrites(0).toSeq must be(golden_Core0.toSeq)

                // Core 1
                println(
                  s"Core 1: Captured ${capturedWrites(1).length}, Expected ${golden_Core1.length}"
                )
                capturedWrites(1).toSeq must be(golden_Core1.toSeq)

                println("--- DecoderBank Pressure Test Passed ---")
            }
        }

        "[MINIMAL TEST] Simple LE alignment check" in {
            // 简单验证 Flag 在 LSB 时的解析
            val minimalConfig = DecoderBankParams(
              coreParams = GRDecoderCoreParams.default.copy(groupSize = 4),
              P = 1 // 单 Core 测试
            )
            implicit val p: DecoderBankParams = minimalConfig

            // 数据: Flag="00" (k=1), Data="01"*4.
            // String: "0001010101" (10 bits) -> Padded "0001010101000..."
            // Word 0: "00010101" (0x15) at LSB.
            val mini_str = "00" + "01" * 4
            val mini_meta = Map(0 -> (100, 8)) // Addr=100, ZP=8
            val mini_stream = sliceStreamToDB(100, mini_str, 8)

            val golden = Array(7, 7, 7, 7) // -1 + 8 = 7

            simulate(new DecoderBank(p)) { dut =>
                dut.reset.poke(true.B); dut.clock.step();
                dut.reset.poke(false.B)
                preloadMemory(dut, mini_meta, mini_stream, p)

                dut.io.start_req.valid.poke(true.B)
                dut.io.start_req.bits.base_group_index.poke(0.U)
                dut.io.start_req.bits.num_groups_to_decode.poke(1.U)
                dut.clock.step(1)
                dut.io.start_req.valid.poke(false.B)

                val captured = new ArrayBuffer[Int]()
                var cycles = 0
                while (
                  dut.io.bank_finished.peek().litValue == 0 && cycles < 200
                ) {
                    if (
                      dut.io.sram_write_outputs(0).valid.peek().litValue > 0
                    ) {
                        captured += dut.io
                            .sram_write_outputs(0)
                            .data
                            .peek()
                            .litValue
                            .toInt
                    }
                    dut.clock.step(1); cycles += 1
                }
                captured.toSeq must be(golden.toSeq)
                println("--- DecoderBank Minimal Test Passed ---")
            }
        }
    }
}
