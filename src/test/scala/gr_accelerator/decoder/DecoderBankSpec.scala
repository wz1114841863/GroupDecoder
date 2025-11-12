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
        bits = bits.drop(1)
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

    /** "黄金"模型:为一个组生成N个权重
      */
    def genGoldenWeights(
        p: GRDecoderCoreParams,
        groupIndex: Int,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt]
    ): Array[Int] = {

        val (startAddr, zp) = metaDB(groupIndex)
        var fullBitStream = ""
        var currentAddr = startAddr

        val chunksToRead = 64

        for (i <- 0 until chunksToRead) {
            fullBitStream += streamDB
                .getOrElse(currentAddr, BigInt(0))
                .toString(2)
                .reverse
                .padTo(p.streamFetchWidth, '0') // 零扩展到 64 bit
                .reverse
            currentAddr += p.streamFetchWidthBytes
        }

        // 确保流足够长,防止软件模型过早失败
        fullBitStream = fullBitStream.padTo(fullBitStream.length + 4096, '0')

        val flag = Integer.parseInt(fullBitStream.take(2), 2)
        var bits = fullBitStream.drop(2)
        val golden = Array.fill(p.groupSize)(0)

        val flag_fallback = p.grDecoderConfig.grKInWidth + 1

        if (flag == flag_fallback) { // Fallback
            for (i <- 0 until p.groupSize) {
                val (weight, len, nextBits) = decodeFB(bits)
                if (weight == -1) { /* 流结束 */ }
                else { golden(i) = weight; bits = nextBits }
            }
        } else { // GR
            val k = flag + 1
            for (i <- 0 until p.groupSize) {
                val (mapped, len, nextBits) = decodeGR(bits, k)
                if (mapped == -1) { /* 流结束 */ }
                else {
                    val signed = mapUnsignedToSigned(mapped)
                    golden(i) = addZeroPoint(signed, zp)
                    bits = nextBits
                }
            }
        }
        golden
    }

    // --- 共享的测试参数 (gs=4, 用于快速测试) ---
    val testConfig_gs4 = DecoderBankParams(
      coreParams = GRDecoderCoreParams.default.copy(groupSize = 4),
      P = 2
    )

    // --- 压力测试参数 (gs=64) ---
    val pressureTestConfig = DecoderBankParams(
      coreParams = GRDecoderCoreParams.default.copy(groupSize = 64),
      P = 2 // 保持 P=2 以便调试
    )

    // --- 共享的数据库 (包含 G0, G1, G2, G3) ---
    val metaDB_shared = Map(
      0 -> (1000, 8), // G0 (k=1, zp=8)  -> Core 0
      1 -> (2000, 5), // G1 (FB, zp=5)  -> Core 1
      2 -> (3000, 7), // G2 (k=2, zp=7)  -> Core 0
      3 -> (4000, 10) // G3 (k=1, zp=10) -> Core 1
    )

    // 扩展 streamDB 以支持 gs=64
    // G0 (k=1, q=0,r=1, "01") * 64 = "00" + "01"*64 = 130 bits
    val stream_G0 = ("00" + "01" * 64).padTo(64 * 3, '0') // 扩展到 3 个块
    // G1 (FB, w=5, "0101") * 64 = "10" + "0101"*64 = 258 bits
    val stream_G1 = ("10" + "0101" * 64).padTo(64 * 5, '0') // 扩展到 5 个块
    // G2 (k=2, q=1,r=3, "1011") * 64 = "01" + "1011"*64 = 258 bits
    val stream_G2 = ("01" + "1011" * 64).padTo(64 * 5, '0')
    // G3 (k=1, q=3,r=1, "11101") * 64 = "00" + "11101"*64 = 322 bits
    val stream_G3 = ("00" + "11101" * 64).padTo(64 * 6, '0')

    // 辅助函数, 将长字符串切片为 Map
    def sliceStreamToDB(
        startAddr: Int,
        stream: String,
        chunkBytes: Int
    ): Map[Int, BigInt] = {
        stream
            .grouped(chunkBytes * 8) // e.g., 64 bits
            .zipWithIndex
            .map { case (chunk, i) =>
                (startAddr + i * chunkBytes) -> BigInt(chunk, 2)
            }
            .toMap
    }

    val streamDB_shared =
        sliceStreamToDB(1000, stream_G0, 8) ++
            sliceStreamToDB(2000, stream_G1, 8) ++
            sliceStreamToDB(3000, stream_G2, 8) ++
            sliceStreamToDB(4000, stream_G3, 8)

    /** 辅助加载函数 存在一定的问题, 对于压力测试可能会产生错误的输入.
      */
    def preloadMemory(
        dut: DecoderBank,
        metaDB: Map[Int, (Int, Int)],
        streamDB: Map[Int, BigInt],
        p: DecoderBankParams // 需要 p 来获取 streamFetchWidthBytes
    ): Unit = {
        for ((addr, (byte_addr, zp)) <- metaDB) {
            dut.io.load_meta.valid.poke(true.B)
            dut.io.load_meta.bits.addr.poke(addr.U)
            dut.io.load_meta.bits.data.start_byte_addr.poke(byte_addr.U)
            dut.io.load_meta.bits.data.zero_point.poke(zp.U)
            dut.clock.step(1)
        }
        dut.io.load_meta.valid.poke(false.B)

        for ((byte_addr, data64) <- streamDB) {
            val num_bytes = p.coreParams.streamFetchWidthBytes
            for (i <- 0 until num_bytes) {
                dut.io.load_stream.valid.poke(true.B)
                dut.io.load_stream.bits.addr.poke((byte_addr + i).U)
                val shift_amount = (num_bytes - 1 - i) * 8
                val byte_data = (data64 >> shift_amount) & 0xff
                dut.io.load_stream.bits.data.poke(byte_data.U(8.W))
                dut.clock.step(1)
            }
        }
        dut.io.load_stream.valid.poke(false.B)
    }

    // "DecoderBank (gs=4) should correctly dispatch (P=2, N=3)" in {
    //     implicit val p: DecoderBankParams = testConfig_gs4
    //     val golden_G0 =
    //         genGoldenWeights(p.coreParams, 0, metaDB_shared, streamDB_shared)
    //     val golden_G1 =
    //         genGoldenWeights(p.coreParams, 1, metaDB_shared, streamDB_shared)
    //     val golden_G2 =
    //         genGoldenWeights(p.coreParams, 2, metaDB_shared, streamDB_shared)

    //     val golden_Bank0_N3 = golden_G0 ++ golden_G2
    //     val golden_Bank1_N3 = golden_G1

    //     simulate(new DecoderBank(p)) { dut =>
    //         dut.reset.poke(true.B); dut.clock.step(); dut.reset.poke(false.B);
    //         dut.clock.step()
    //         preloadMemory(dut, metaDB_shared, streamDB_shared, p)
    //         dut.clock.step(5)

    //         val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
    //         var cycles = 0
    //         val timeout = 100

    //         dut.io.start_req.valid.poke(true.B)
    //         dut.io.start_req.bits.base_group_index.poke(0.U)
    //         dut.io.start_req.bits.num_groups_to_decode.poke(3.U)

    //         dut.clock.step(1)
    //         dut.io.start_req.valid.poke(false.B)
    //         cycles += 1

    //         while (
    //           dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
    //         ) {
    //             for (i <- 0 until p.P) {
    //                 if (
    //                   dut.io.sram_write_outputs(i).valid.peek().litValue > 0
    //                 ) {
    //                     val data = dut.io
    //                         .sram_write_outputs(i)
    //                         .data
    //                         .peek()
    //                         .litValue
    //                         .toInt
    //                     capturedWrites(i) += data
    //                 }
    //             }
    //             dut.clock.step(1)
    //             cycles += 1
    //         }

    //         println(
    //           s"DecoderBank (gs=4, P=2, N=3) FSM finished in $cycles cycles."
    //         )
    //         cycles must be < timeout

    //         // [FIXED] 验证逻辑: 必须检查 *严格的顺序*
    //         capturedWrites(0).toSeq must be(golden_Bank0_N3.toSeq)
    //         capturedWrites(1).toSeq must be(golden_Bank1_N3.toSeq)
    //     }
    // }

    // // =============================================
    // // --- 压力测试 (P=2, N=4, gs=64) ---
    // // =============================================
    // "[PRESSURE TEST] DecoderBank (gs=64) should handle (P=2, N=4)" in {
    //     // (P=2, N=4, gs=64) -> Core 0: G0, G2; Core 1: G1, G3
    //     implicit val p: DecoderBankParams = pressureTestConfig

    //     // --- 1. 生成 "黄金" 答案 ---
    //     println("--- [Pressure Test] 正在生成黄金模型 (gs=64) ---")
    //     // 使用genGoldenWeights
    //     val golden_G0 =
    //         genGoldenWeights(p.coreParams, 0, metaDB_shared, streamDB_shared)
    //     val golden_G1 =
    //         genGoldenWeights(p.coreParams, 1, metaDB_shared, streamDB_shared)
    //     val golden_G2 =
    //         genGoldenWeights(p.coreParams, 2, metaDB_shared, streamDB_shared)
    //     val golden_G3 =
    //         genGoldenWeights(p.coreParams, 3, metaDB_shared, streamDB_shared)
    //     println("--- [Pressure Test] 黄金模型生成完毕 ---")

    //     // 最终 Bank 的内容 (N=4, gs=64)
    //     val golden_Bank0_N4 = golden_G0 ++ golden_G2 // 64+64=128 个权重
    //     val golden_Bank1_N4 = golden_G1 ++ golden_G3 // 64+64=128 个权重

    //     // 2. 启动模拟
    //     simulate(new DecoderBank(p)) { dut =>
    //         dut.reset.poke(true.B); dut.clock.step(); dut.reset.poke(false.B);
    //         dut.clock.step()

    //         // --- 3. 模拟 加载 (预加载内存) ---
    //         println("--- [Pressure Test] 开始预加载内存 (gs=64) ---")
    //         preloadMemory(dut, metaDB_shared, streamDB_shared, p)
    //         println("--- [Pressure Test] 预加载完成 ---")
    //         dut.clock.step(5)

    //         // --- 4. 监视输出 ---
    //         val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
    //         var cycles = 0
    //         val timeout =
    //             1000 // (gs=64 * 4 组) / P=2, 2周期/权重, 需要 ~1500 周期, 设 2000

    //         // 增加超时
    //         val total_weights_to_decode = p.coreParams.groupSize * 4
    //         // 2 周期/权重 + 3 周期/64bit加载 + 2 周期/meta
    //         val estimated_cycles =
    //             (total_weights_to_decode / p.P) * 2 + (total_weights_to_decode * 5 / 64) * 3 + 4 * 2
    //         val test_timeout =
    //             estimated_cycles + 500 // 256*2 + 20*3 + 8 = 512 + 60 + 8 = 580.
    //         // 我们的 `GRDecoderCore` 是 2 周期/权重, 4 组 * 64 权重 = 256
    //         // (256 / 2 Cores) * 2 周期/权重 = ~256 周期 (解码)
    //         // 4 组 * 2 周期 (meta) = 8 周期
    //         // G0(130b), G1(258b), G2(258b), G3(322b) = 968 bits
    //         // 968b / 64b/chunk = 16 次加载 * 3 周期/加载 = 48 周期
    //         // 总计: ~256 + 8 + 48 = 312 周期. 设 1000 周期

    //         println(
    //           s"--- [Pressure Test] 启动 FSM (P=2, N=4, gs=64). Timeout=${timeout} cyc ---"
    //         )

    //         // --- 5. 启动 FSM ---
    //         dut.io.start_req.valid.poke(true.B)
    //         dut.io.start_req.bits.base_group_index.poke(0.U)
    //         dut.io.start_req.bits.num_groups_to_decode.poke(4.U) // 解码 4 组

    //         dut.clock.step(1)
    //         dut.io.start_req.valid.poke(false.B)
    //         cycles += 1

    //         while (
    //           dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
    //         ) {
    //             // 监视 P=2 个输出端口
    //             for (i <- 0 until p.P) {
    //                 if (
    //                   dut.io.sram_write_outputs(i).valid.peek().litValue > 0
    //                 ) {
    //                     val data = dut.io
    //                         .sram_write_outputs(i)
    //                         .data
    //                         .peek()
    //                         .litValue
    //                         .toInt
    //                     capturedWrites(i) += data
    //                 }
    //             }
    //             dut.clock.step(1)
    //             cycles += 1
    //         }

    //         // --- 6. 验证 ---
    //         println(
    //           s"DecoderBank (gs=64, P=2, N=4) FSM finished in $cycles cycles."
    //         )
    //         cycles must be < timeout // 检查超时

    //         // 检查 Core 0 (G0 + G2)
    //         println(s"Core 0 captured: ${capturedWrites(0).length} weights")
    //         println(s"Core 0 golden:   ${golden_Bank0_N4.length} weights")
    //         // [FIXED] 验证逻辑: 必须检查 *严格的顺序*
    //         capturedWrites(0).toSeq must be(golden_Bank0_N4.toSeq)

    //         // 检查 Core 1 (G1 + G3)
    //         println(s"Core 1 captured: ${capturedWrites(1).length} weights")
    //         println(s"Core 1 golden:   ${golden_Bank1_N4.length} weights")
    //         // [FIXED] 验证逻辑: 必须检查 *严格的顺序*
    //         capturedWrites(1).toSeq must be(golden_Bank1_N4.toSeq)

    //         println("--- DecoderBank (gs=64) 压力测试通过 ---")
    //     }
    // }

    // =============================================
    // --- 最小化, 独立的压力测试 (P=2, N=2, gs=4) ---
    // =============================================
    "[MINIMAL TEST] DecoderBank (P=2, N=2) should isolate the broadcast bug" in {
        // 1. 定义一个独立的/最小化的配置
        val minimalConfig = DecoderBankParams(
          coreParams = GRDecoderCoreParams.default.copy(groupSize = 4),
          P = 2
        )
        implicit val p: DecoderBankParams = minimalConfig

        // 2. 定义独立的/最小化的数据库
        // Core 0 (G0): k=1, zp=8, stream="01"
        // Core 1 (G1): FB,  zp=5, stream="0101"
        val minimal_metaDB = Map(
          0 -> (1000, 8), // G0 -> Core 0
          1 -> (2000, 5) // G1 -> Core 1
        )

        val minimal_streamDB = Map(
          // G0 (k=1, "01") * 4 = "00" + "01010101" = 10 bits
          1000 -> BigInt(("00" + "01" * 4).padTo(64, '0'), 2),
          // G1 (FB, "0101") * 4 = "10" + "0101010101010101" = 18 bits
          2000 -> BigInt(("10" + "0101" * 4).padTo(64, '0'), 2)
        )

        // 3. 定义独立的/硬编码的黄金结果
        // G0 (zp=8) + stream("01", k=1) -> mapped=1, signed=-1 -> final=7
        val golden_Core0 = Array(7, 7, 7, 7)
        // G1 (zp=5) + stream("0101", FB) -> final=5
        val golden_Core1 = Array(5, 5, 5, 5)

        // 4. 启动模拟
        simulate(new DecoderBank(p)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(); dut.reset.poke(false.B);
            dut.clock.step()

            // 5. 预加载 *最小化的* 内存
            println("--- [Minimal Test] 开始预加载最小化内存 ---")
            preloadMemory(dut, minimal_metaDB, minimal_streamDB, p)
            println("--- [Minimal Test] 预加载完成 ---")
            dut.clock.step(5)

            // 6. 监视输出
            val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
            var cycles = 0
            val timeout = 100 // 这个测试应该非常快

            // 7. 启动 FSM (P=2, N=2)
            dut.io.start_req.valid.poke(true.B)
            dut.io.start_req.bits.base_group_index.poke(0.U)
            dut.io.start_req.bits.num_groups_to_decode.poke(2.U) // 解码 2 组

            dut.clock.step(1)
            dut.io.start_req.valid.poke(false.B)
            cycles += 1

            while (
              dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
            ) {
                for (i <- 0 until p.P) {
                    if (
                      dut.io.sram_write_outputs(i).valid.peek().litValue > 0
                    ) {
                        capturedWrites(i) += dut.io
                            .sram_write_outputs(i)
                            .data
                            .peek()
                            .litValue
                            .toInt
                    }
                }
                dut.clock.step(1)
                cycles += 1
            }

            // 8. 验证
            println(
              s"DecoderBank [Minimal Test] FSM finished in $cycles cycles."
            )
            cycles must be < timeout

            // 验证 Core 0 (必须是 G0, 顺序严格)
            println(s"Core 0 captured: ${capturedWrites(0).mkString(",")}")
            println(s"Core 0 golden:   ${golden_Core0.mkString(",")}")
            capturedWrites(0).toSeq must be(golden_Core0.toSeq)

            // 验证 Core 1 (必须是 G1, 顺序严格)
            println(s"Core 1 captured: ${capturedWrites(1).mkString(",")}")
            println(s"Core 1 golden:   ${golden_Core1.mkString(",")}")
            capturedWrites(1).toSeq must be(golden_Core1.toSeq)

            println("--- DecoderBank [Minimal Test] 通过 ---")
        }
    }

    // =============================================
    // --- 独立压力测试 (P=4, N=8, gs=128) ---
    // =============================================
    "[P=4, N=8, GS=128 TEST] DecoderBank should handle medium-scale stress" in {

        // 1. 定义独立的配置 (P=4, gs=128)
        val mediumConfig = DecoderBankParams(
          coreParams = GRDecoderCoreParams.default.copy(groupSize = 128),
          P = 4
        )
        implicit val p: DecoderBankParams = mediumConfig

        // 2. 定义独立的数据库 (N=8)
        // 我们将使用 G0, G1, G2, G3 模式, 并重复两次
        // Core 0: G100, G104 (G0-type)
        // Core 1: G101, G105 (G1-type)
        // Core 2: G102, G106 (G2-type)
        // Core 3: G103, G107 (G3-type)
        val medium_metaDB = Map(
          100 -> (10000, 8), // G100 (G0-type, k=1, zp=8)
          101 -> (20000, 5), // G101 (G1-type, FB, zp=5)
          102 -> (30000, 7), // G102 (G2-type, k=2, zp=7)
          103 -> (40000, 10), // G103 (G3-type, k=1, zp=10)

          104 -> (11000, 8), // G104 (G0-type)
          105 -> (21000, 5), // G105 (G1-type)
          106 -> (31000, 7), // G106 (G2-type)
          107 -> (41000, 10) // G107 (G3-type)
        )

        // 3. 定义独立的流数据 (gs=128)
        // [FIXED] 确保数据流足够长 (258, 514, 514, 642 bits)
        val stream_G0_128 =
            ("00" + "01" * 128).padTo(5 * 64, '0') // k=1 (258b) -> 5 块
        val stream_G1_128 =
            ("10" + "0101" * 128).padTo(9 * 64, '0') // FB  (514b) -> 9 块
        val stream_G2_128 =
            ("01" + "1011" * 128).padTo(9 * 64, '0') // k=2 (514b) -> 9 块
        val stream_G3_128 =
            ("00" + "11101" * 128).padTo(11 * 64, '0') // k=1 (642b) -> 11 块

        val medium_streamDB =
            sliceStreamToDB(10000, stream_G0_128, 8) ++ // G100
                sliceStreamToDB(20000, stream_G1_128, 8) ++ // G101
                sliceStreamToDB(30000, stream_G2_128, 8) ++ // G102
                sliceStreamToDB(40000, stream_G3_128, 8) ++ // G103
                sliceStreamToDB(11000, stream_G0_128, 8) ++ // G104
                sliceStreamToDB(21000, stream_G1_128, 8) ++ // G105
                sliceStreamToDB(31000, stream_G2_128, 8) ++ // G106
                sliceStreamToDB(41000, stream_G3_128, 8) // G107

        // 4. 定义独立的/硬编码的黄金结果 (gs=128)
        val golden_G0 = Array.fill(128)(7) // k=1, zp=8, "01" -> 7
        val golden_G1 = Array.fill(128)(5) // FB, "0101" -> 5
        val golden_G2 = Array.fill(128)(3) // k=2, zp=7, "1011" -> 3
        val golden_G3 = Array.fill(128)(6) // k=1, zp=10, "11101" -> 6

        // Core 0: G100, G104
        val golden_Core0 = golden_G0 ++ golden_G0
        // Core 1: G101, G105
        val golden_Core1 = golden_G1 ++ golden_G1
        // Core 2: G102, G106
        val golden_Core2 = golden_G2 ++ golden_G2
        // Core 3: G103, G107
        val golden_Core3 = golden_G3 ++ golden_G3

        val all_golden_banks =
            Seq(golden_Core0, golden_Core1, golden_Core2, golden_Core3)

        // 5. 启动模拟
        simulate(new DecoderBank(p)) { dut =>
            dut.reset.poke(true.B); dut.clock.step(); dut.reset.poke(false.B);
            dut.clock.step()

            // 6. 预加载内存
            println(s"--- [P=4, gs=128 Test] 开始预加载 (N=8) ---")
            preloadMemory(dut, medium_metaDB, medium_streamDB, p)
            println(s"--- [P=4, gs=128 Test] 预加载完成 ---")
            dut.clock.step(5)

            // 7. 监视输出
            val capturedWrites = Seq.fill(p.P)(new ArrayBuffer[Int]())
            var cycles = 0
            // gs=128, N=8, P=4 -> 2 组/Core -> 256 权重/Core
            // 256 * 2 (周期/权重) = 512 周期. + 加载/meta. Timeout 1000
            val timeout = 1000

            // 8. 启动 FSM (P=4, N=8)
            dut.io.start_req.valid.poke(true.B)
            dut.io.start_req.bits.base_group_index.poke(100.U)
            dut.io.start_req.bits.num_groups_to_decode.poke(8.U) // 解码 8 组

            dut.clock.step(1)
            dut.io.start_req.valid.poke(false.B)
            cycles += 1

            while (
              dut.io.bank_finished.peek().litValue == 0 && cycles < timeout
            ) {
                for (i <- 0 until p.P) {
                    if (
                      dut.io.sram_write_outputs(i).valid.peek().litValue > 0
                    ) {
                        capturedWrites(i) += dut.io
                            .sram_write_outputs(i)
                            .data
                            .peek()
                            .litValue
                            .toInt
                    }
                }
                dut.clock.step(1)
                cycles += 1
            }

            // 9. 验证
            println(
              s"DecoderBank [P=4, gs=128 Test] FSM finished in $cycles cycles."
            )
            cycles must be < timeout

            var all_passed = true
            for (i <- 0 until p.P) {
                println(s"--- 验证 Core ${i} ---")
                println(
                  s"Core ${i} captured: ${capturedWrites(i).length} weights"
                )
                println(
                  s"Core ${i} golden:   ${all_golden_banks(i).length} weights"
                )

                val core_passed =
                    (capturedWrites(i).toSeq == all_golden_banks(i).toSeq)
                if (!core_passed) {
                    println(s"*** FAILED: Core ${i} 内容不匹配! ***")
                    val firstMismatch =
                        capturedWrites(i).zip(all_golden_banks(i)).indexWhere {
                            case (c, g) => c != g
                        }
                    if (firstMismatch != -1) {
                        println(
                          s"  第一个不匹配在索引 [${firstMismatch}] (预期值: ${all_golden_banks(i)(firstMismatch)})"
                        )
                    }
                }
                all_passed = all_passed && core_passed
            }

            all_passed must be(true)
            println("--- DecoderBank [P=4, gs=128 Test] 通过 ---")
        }
    }
}
