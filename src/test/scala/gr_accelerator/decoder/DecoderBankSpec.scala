package gr_accelerator.decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import chisel3.experimental.BundleLiterals._
import scala.collection.mutable.ArrayBuffer

import gr_accelerator.common._

/** DecoderBankSpec测试模块
  */
class DecoderBankSpec extends AnyFreeSpec with Matchers with ChiselSim {

    // --- 辅助函数:软件"黄金"模型 ---
    def mapUnsignedToSigned(unsigned: Int): Int = {
        if (unsigned % 2 == 0) (unsigned / 2)
        else -(unsigned + 1) / 2
    }
    def addZeroPoint(signed: Int, zp: Int): Int = {
        (signed + zp) & 0xf
    }
    def decodeGR(bitStream: String, k: Int): (Int, Int, String) = {
        var q = 0
        var bits = bitStream
        while (bits.startsWith("1")) { q += 1; bits = bits.drop(1) }
        if (bits.length == 0) return (-1, q + 1, "")
        bits = bits.drop(1) // 丢弃 '0'
        if (bits.length < k) return (-1, q + 1 + k, "")
        val r = Integer.parseInt(bits.take(k), 2)
        bits = bits.drop(k)
        ((q << k) | r, q + 1 + k, bits)
    }
    def decodeFB(bitStream: String): (Int, Int, String) = {
        if (bitStream.length < 4) return (-1, 4, "")
        val weight = Integer.parseInt(bitStream.take(4), 2)
        (weight, 4, bitStream.drop(4))
    }

    /** "黄金"模型:为一个组生成4个权重
      */
    def genGoldenWeights(
        p: GRDecoderCoreParams,
        groupIndex: Int,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt]
    ): Array[Int] = {

        val (startAddr, zp) = metaDB(groupIndex)

        // 从 DB 构建完整的比特流
        var fullBitStream = ""
        var currentAddr = startAddr
        // 假设一个组 + 头部不会超过2个64-bit 块
        for (i <- 0 until 2) {
            fullBitStream += streamDB
                .getOrElse(currentAddr, BigInt(0))
                .toString(2)
                .reverse
                .padTo(p.streamFetchWidth, '0')
                .reverse
            currentAddr += p.streamFetchWidthBytes
        }

        val flag = Integer.parseInt(fullBitStream.take(2), 2)
        var bits = fullBitStream.drop(2)
        val golden = Array.fill(p.groupSize)(0)

        val flag_fallback = p.grDecoderConfig.grKInWidth + 1 // 2

        if (flag == flag_fallback) { // Fallback
            for (i <- 0 until p.groupSize) {
                val (weight, len, nextBits) = decodeFB(bits)
                golden(i) = weight; bits = nextBits
            }
        } else { // GR
            val k = flag + 1 // k=1 或 k=2
            for (i <- 0 until p.groupSize) {
                val (mapped, len, nextBits) = decodeGR(bits, k)
                val signed = mapUnsignedToSigned(mapped)
                golden(i) = addZeroPoint(signed, zp)
                bits = nextBits
            }
        }
        golden
    }

    // --- 共享的测试参数 ---
    val testConfig = DecoderBankParams(
      coreParams = GRDecoderCoreParams.default.copy(groupSize = 4),
      P = 2
    )
    implicit val p: DecoderBankParams = testConfig
    // --- 共享的数据库 (包含 G0, G1, G2, G3) ---

    // (MetaSRAM 数据库)
    val metaDB_shared = Map(
      0 -> (1000, 8), // G0 (k=1, zp=8)  -> Core 0
      1 -> (2000, 5), // G1 (FB, zp=5)  -> Core 1
      2 -> (3000, 7), // G2 (k=2, zp=7)  -> Core 0
      3 -> (4000, 10) // G3 (k=1, zp=10) -> Core 1
    )

    // (SharedCache 数据库)
    // G0 (k=1, q=0,r=1, "01") * 4 = "00" + "01010101" = 10 bits
    val stream_G0 = ("00" + "01" * 4).padTo(64, '0')
    // G1 (FB, w=5, "0101") * 4 = "10" + "0101"*4 = 18 bits
    val stream_G1 = ("10" + "0101" * 4).padTo(64, '0')
    // G2 (k=2, q=1,r=3, "1011") * 4 = "01" + "1011"*4 = 18 bits
    val stream_G2 = ("01" + "1011" * 4).padTo(64, '0')
    // G3 (k=1, q=3,r=1, "11101") * 4 = "00" + "11101"*4 = 22 bits
    val stream_G3 = ("00" + "11101" * 4).padTo(64, '0')

    val streamDB_shared = Map(
      1000 -> BigInt(stream_G0, 2),
      2000 -> BigInt(stream_G1, 2),
      3000 -> BigInt(stream_G2, 2),
      4000 -> BigInt(stream_G3, 2)
    )

    /** 辅助加载函数
      */
    def preloadMemory(
        dut: DecoderBank,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt]
    ): Unit = {
        // 加载 MetaSRAM
        for ((addr, (byte_addr, zp)) <- metaDB) {
            dut.io.load_meta.valid.poke(true.B)
            dut.io.load_meta.bits.addr.poke(addr.U)
            dut.io.load_meta.bits.data.start_byte_addr.poke(byte_addr.U)
            dut.io.load_meta.bits.data.zero_point.poke(zp.U)
            dut.clock.step(1)
        }
        dut.io.load_meta.valid.poke(false.B)

        // 加载 SharedCache (Big-Endian)
        for ((byte_addr, data64) <- streamDB) {
            val num_bytes = p.coreParams.streamFetchWidthBytes
            for (i <- 0 until num_bytes) {
                dut.io.load_stream.valid.poke(true.B)
                dut.io.load_stream.bits.addr.poke((byte_addr + i).U)
                // [修复] Big-Endian 写入
                val shift_amount = (num_bytes - 1 - i) * 8
                val byte_data = (data64 >> shift_amount) & 0xff
                dut.io.load_stream.bits.data.poke(byte_data.U(8.W))
                dut.clock.step(1)
            }
        }
        dut.io.load_stream.valid.poke(false.B)
    }

    "DecoderBank (V13) should correctly dispatch (P=2, N=3) and handle arbitration" in {
        // (P=2, N=3) -> Core 0: G0, G2; Core 1: G1

        // --- 1. 生成 "黄金" 答案 ---
        val golden_G0 =
            genGoldenWeights(p.coreParams, 0, metaDB_shared, streamDB_shared)
        val golden_G1 =
            genGoldenWeights(p.coreParams, 1, metaDB_shared, streamDB_shared)
        val golden_G2 =
            genGoldenWeights(p.coreParams, 2, metaDB_shared, streamDB_shared)

        // 最终 Bank 的内容 (N=3)
        // Core 0 接收 G0 和 G2
        val golden_Bank0_N3 = golden_G0 ++ golden_G2
        // Core 1 接收 G1
        val golden_Bank1_N3 = golden_G1

        // 2. 启动模拟
        simulate(new DecoderBank(p)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            // --- 3. 模拟 加载 (预加载内存) ---
            println("--- [N=3 Test] 开始预加载内存 ---")
            preloadMemory(dut, metaDB_shared, streamDB_shared)
            println("--- [N=3 Test] 预加载完成 ---")
            dut.clock.step(5) // 等待

            // --- 4. 监视输出 ---
            val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
            var cycles = 0
            val timeout = 100

            // --- 5. 启动 FSM ---
            dut.io.start_req.valid.poke(true.B)
            dut.io.start_req.bits.base_group_index.poke(0.U)
            dut.io.start_req.bits.num_groups_to_decode.poke(3.U) // 解码 3 组

            dut.clock.step(1)
            dut.io.start_req.valid.poke(false.B)
            cycles += 1

            while (
              dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
            ) {
                // 监视 P=2 个输出端口
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

            // --- 6. 验证 ---
            println(
              s"DecoderBank (P=2, N=3) FSM finished in $cycles cycles."
            )
            cycles must be < timeout // 检查超时

            // 检查 Core 0 (G0 + G2)
            println(s"Core 0 captured: ${capturedWrites(0).mkString(",")}")
            println(s"Core 0 golden:   ${golden_Bank0_N3.mkString(",")}")
            capturedWrites(
              0
            ).toSeq must contain theSameElementsAs (golden_Bank0_N3.toSeq)

            // 检查 Core 1 (G1)
            println(s"Core 1 captured: ${capturedWrites(1).mkString(",")}")
            println(s"Core 1 golden:   ${golden_Bank1_N3.mkString(",")}")
            capturedWrites(
              1
            ).toSeq must contain theSameElementsAs (golden_Bank1_N3.toSeq)
        }
    }

    "[NEW TEST] DecoderBank (V13) should handle a larger, even batch (P=2, N=4)" in {
        // (P=2, N=4) -> Core 0: G0, G2; Core 1: G1, G3

        // --- 1. 生成 "黄金" 答案 ---
        // G0, G1, G2 已在上面定义
        val golden_G0 =
            genGoldenWeights(p.coreParams, 0, metaDB_shared, streamDB_shared)
        val golden_G1 =
            genGoldenWeights(p.coreParams, 1, metaDB_shared, streamDB_shared)
        val golden_G2 =
            genGoldenWeights(p.coreParams, 2, metaDB_shared, streamDB_shared)
        // G3 (k=1, zp=10, q=3,r=1 -> mapped=7 -> signed=-4 -> final=6)
        val golden_G3 =
            genGoldenWeights(p.coreParams, 3, metaDB_shared, streamDB_shared)

        // 最终 Bank 的内容 (N=4)
        val golden_Bank0_N4 = golden_G0 ++ golden_G2
        val golden_Bank1_N4 = golden_G1 ++ golden_G3

        // 2. 启动模拟
        simulate(new DecoderBank(p)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            // --- 3. 模拟 加载 (预加载内存) ---
            println("--- [N=4 Test] 开始预加载内存 ---")
            preloadMemory(dut, metaDB_shared, streamDB_shared)
            println("--- [N=4 Test] 预加载完成 ---")
            dut.clock.step(5) // 等待

            // --- 4. 监视输出 ---
            val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
            var cycles = 0
            val timeout = 150 // (N=4, 周期稍长)

            // --- 5. 启动 FSM ---
            dut.io.start_req.valid.poke(true.B)
            dut.io.start_req.bits.base_group_index.poke(0.U)
            dut.io.start_req.bits.num_groups_to_decode.poke(4.U) // 解码 4 组

            dut.clock.step(1)
            dut.io.start_req.valid.poke(false.B)
            cycles += 1

            while (
              dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
            ) {
                // 监视 P=2 个输出端口
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

            // --- 6. 验证 ---
            println(
              s"DecoderBank (P=2, N=4) FSM finished in $cycles cycles."
            )
            cycles must be < timeout // 检查超时

            // 检查 Core 0 (G0 + G2)
            println(s"Core 0 captured: ${capturedWrites(0).mkString(",")}")
            println(s"Core 0 golden:   ${golden_Bank0_N4.mkString(",")}")
            // 期望: 7,7,7,7 (G0) + 3,3,3,3 (G2)
            golden_Bank0_N4.toSeq must be(
              Array(7, 7, 7, 7, 3, 3, 3, 3).toSeq
            )
            capturedWrites(
              0
            ).toSeq must contain theSameElementsAs (golden_Bank0_N4.toSeq)

            // 检查 Core 1 (G1 + G3)
            println(s"Core 1 captured: ${capturedWrites(1).mkString(",")}")
            println(s"Core 1 golden:   ${golden_Bank1_N4.mkString(",")}")
            // 期望: 5,5,5,5 (G1) + 6,6,6,6 (G3)
            golden_Bank1_N4.toSeq must be(
              Array(5, 5, 5, 5, 6, 6, 6, 6).toSeq
            )
            capturedWrites(
              1
            ).toSeq must contain theSameElementsAs (golden_Bank1_N4.toSeq)

            println("--- DecoderBank N=4 测试用例通过 ---")
        }
    }
}
