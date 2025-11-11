package gr_accelerator.decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer
import gr_accelerator.common._

/** GRDecoderCoreSpec 测试模块.
  *
  * 采用单线程/逐周期 (cycle-by-cycle) 的方式模拟 FSM IO
  */
class GRDecoderCoreSpec extends AnyFreeSpec with Matchers with ChiselSim {
    // --- 辅助函数:软件"黄金"模型 ---
    // 软件实现反映射
    def mapUnsignedToSigned(unsigned: Int): Int = {
        if (unsigned % 2 == 0) (unsigned / 2)
        else -(unsigned + 1) / 2
    }

    // 软件实现加零点
    def addZeroPoint(signed: Int, zp: Int): Int = {
        (signed + zp) & 0xf // 截断为 4-bit
    }

    // 软件实现 GR 解码
    def decodeGR(bitStream: String, k: Int): (Int, Int, String) = {
        var q = 0
        var bits = bitStream
        while (bits.startsWith("1")) {
            q += 1
            bits = bits.drop(1)
        }
        // 检查比特流 是否在 q 之后意外结束
        if (bits.length == 0) {
            return (-1, q + 1, "") // 错误
        }
        bits = bits.drop(1) // 丢弃 '0'

        // 检查比特流 是否在 r 之前意外结束
        if (bits.length < k) {
            return (-1, q + 1 + k, "") // 错误
        }
        val r = Integer.parseInt(bits.take(k), 2)
        bits = bits.drop(k)

        val len = q + 1 + k
        val mapped_delta = (q << k) | r
        (mapped_delta, len, bits)
    }

    // 软件实现 Fallback 解码
    def decodeFB(bitStream: String): (Int, Int, String) = {
        if (bitStream.length < 4) {
            return (-1, 4, "") // 错误
        }
        val weight = Integer.parseInt(bitStream.take(4), 2)
        val len = 4
        val bits = bitStream.drop(4)
        (weight, len, bits)
    }

    // 测试 数据库
    // 模拟 "元数据 SRAM"
    val metaDB = Map(
      0 -> (1000, 8), // 组 0: (addr=1000, zp=8)
      1 -> (2000, 5), // 组 1: (addr=2000, zp=5)
      2 -> (3000, 8) // 组 2 (用于错误测试)
    )

    // 模拟 "DRAM" (64-bit 块)
    val streamDB = Map(
      // Test 1: GR (k=1), q=0, r=1 (len=2 bits) 512 次
      1000 -> BigInt(("00" + "01" * 31).padTo(64, '0'), 2), // Flag + 31 个权重
      1008 -> BigInt(("01" * 32).padTo(64, '0'), 2), // 32 个权重
      1016 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1024 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1032 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1040 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1048 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1056 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1064 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1072 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1080 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1088 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1096 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1104 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1112 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1120 -> BigInt(("01" * 32).padTo(64, '0'), 2),
      1128 -> BigInt(("01" * 1).padTo(64, '0'), 2), // 最后 1 个权重 + 填充

      // Test 2: Fallback (flag="10") + "0101" (raw 5) 512 次
      2000 -> BigInt(
        ("10" + "0101" * 15 + "01").padTo(64, '0'),
        2
      ), // Flag + 15.5 权重
      2008 -> BigInt(("0101" * 16).padTo(64, '0'), 2), // 16 个权重
      // ... (后续数据省略,但 FSM 会持续请求)

      // Test 3: GR 错误 (q > qMax)
      3000 -> BigInt(
        ("00" + "1" * 16 + "0").padTo(64, '0'),
        2
      ) // Flag=k1, q="1"*16
    )

    // "黄金"模型:生成 512 个预期权重
    def genGoldenWeights(
        p: GRDecoderCoreParams,
        groupIndex: Int
    ): Array[Int] = {
        val (startAddr, zp) = metaDB(groupIndex)

        var fullBitStream = ""
        var currentAddr = startAddr
        // 确保我们构建了足够的比特流 (40 个块 > 2050 bits)
        for (i <- 0 until 40) {
            fullBitStream += streamDB
                .getOrElse(currentAddr, BigInt(0))
                .toString(2)
                .reverse
                .padTo(64, '0')
                .reverse
            currentAddr += p.streamFetchWidthBytes
        }

        val flag = Integer.parseInt(fullBitStream.take(2), 2)
        var bits = fullBitStream.drop(2)
        val golden = Array.fill(p.groupSize)(0)

        val flag_fallback = p.grDecoderConfig.grKInWidth + 1 // 2

        if (flag == flag_fallback) { // 场景 2: Fallback
            for (i <- 0 until p.groupSize) {
                val (weight, len, nextBits) = decodeFB(bits)
                if (weight == -1) { /* 碰到流结尾 */ }
                else {
                    golden(i) = weight
                    bits = nextBits
                }
            }
        } else { // 场景 1: GR
            val k = flag + 1 // k=1
            for (i <- 0 until p.groupSize) {
                // 修正:签名已改为 (Int, Int, String)
                val (mapped, len, nextBits) = decodeGR(bits, k)
                if (mapped == -1) { /* 碰到流结尾 */ }
                else {
                    val signed = mapUnsignedToSigned(mapped)
                    golden(i) = addZeroPoint(signed, zp)
                    bits = nextBits
                }
            }
        }
        golden
    }

    // =============================================
    // --- 测试 场景
    // =============================================

    "GRDecoderCore should handle a full GR Group (k=1) (Scene 1 & 3)" in {

        implicit val p = GRDecoderCoreParams.default
        val goldenWeights = genGoldenWeights(p, 0) // 测试 组 0

        simulate(new GRDecoderCore(p, 0.U)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            // 捕获 SRAM 写入
            val capturedWeights = Array.fill(p.groupSize)(-1)

            // 模拟 IO 的状态
            var meta_latency_counter = 0
            var stream_latency_counter = 0
            var meta_resp_data: (Int, Int) = (0, 0)
            var stream_resp_data: BigInt = 0

            // 启动 FSM
            dut.io.start.poke(true.B)
            dut.io.group_index.poke(0.U) // 组 0
            dut.clock.step(1)
            dut.io.start.poke(false.B)

            var cycles = 0
            val timeout = 3000

            // 核心:单线程/逐周期 模拟 循环
            while (dut.io.finished.peek().litValue == 0 && cycles < timeout) {
                // 在每个周期开始时,默认将所有响应 Valid 拉低
                dut.io.meta_resp.valid.poke(false.B)
                dut.io.stream_resp.valid.poke(false.B)

                // --- 1. 模拟 IO *响应* (处理延迟) ---
                if (meta_latency_counter > 0) {
                    meta_latency_counter -= 1
                    if (meta_latency_counter == 0) { // 延迟结束
                        dut.io.meta_resp.valid.poke(true.B)
                        dut.io.meta_resp.start_byte_addr
                            .poke(meta_resp_data._1.U)
                        dut.io.meta_resp.zero_point.poke(meta_resp_data._2.U)
                    }
                }

                if (stream_latency_counter > 0) {
                    stream_latency_counter -= 1
                    if (stream_latency_counter == 0) { // 延迟结束
                        dut.io.stream_resp.valid.poke(true.B)
                        dut.io.stream_resp.data.poke(stream_resp_data.U)
                    }
                }

                // --- 2. 模拟 IO *请求* (来自 DUT) ---
                if (
                  dut.io.meta_req.valid
                      .peek()
                      .litValue > 0 && meta_latency_counter == 0
                ) {
                    val addr = dut.io.meta_req.addr.peek().litValue.toInt
                    meta_resp_data = metaDB.getOrElse(addr, (0, 0))
                    meta_latency_counter = 2 // 模拟 2 周期延迟
                }

                if (
                  dut.io.stream_req.valid
                      .peek()
                      .litValue > 0 && stream_latency_counter == 0
                ) {
                    val addr = dut.io.stream_req.addr.peek().litValue.toInt
                    stream_resp_data = streamDB.getOrElse(addr, BigInt(0))
                    stream_latency_counter = 3 // 模拟 3 周期延迟
                }

                // --- 3. 监视 SRAM 写入 ---
                if (dut.io.sram_write.valid.peek().litValue > 0) {
                    val addr = dut.io.sram_write.addr.peek().litValue.toInt
                    val data = dut.io.sram_write.data.peek().litValue.toInt
                    if (addr < p.groupSize) {
                        capturedWeights(addr) = data
                    }
                }

                // --- 4. 步进时钟 ---
                dut.clock.step(1)
                cycles += 1
            }

            // 7. 验证
            println(s"GR (k=1) Test: FSM finished in $cycles cycles.")
            cycles must be < timeout
            capturedWeights must contain theSameElementsAs (goldenWeights)
        }
    }

    "GRDecoderCore should handle a full Fallback Group (Scene 2 & 3)" in {

        implicit val p = GRDecoderCoreParams.default
        val goldenWeights = genGoldenWeights(p, 1) // 测试 组 1

        simulate(new GRDecoderCore(p, 0.U)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            val capturedWeights = Array.fill(p.groupSize)(-1)

            // 模拟 IO 的状态
            var meta_latency_counter = 0
            var stream_latency_counter = 0
            var meta_resp_data: (Int, Int) = (0, 0)
            var stream_resp_data: BigInt = 0

            // 启动 FSM (组 1)
            dut.io.start.poke(true.B)
            dut.io.group_index.poke(1.U)
            dut.clock.step(1)
            dut.io.start.poke(false.B)

            var cycles = 0
            val timeout = 3000

            // 核心:单线程/逐周期 模拟 循环
            while (dut.io.finished.peek().litValue == 0 && cycles < timeout) {
                // 在每个周期开始时,默认将所有响应 Valid 拉低
                dut.io.meta_resp.valid.poke(false.B)
                dut.io.stream_resp.valid.poke(false.B)

                // --- 1. 模拟 IO *响应* ---
                if (meta_latency_counter > 0) {
                    meta_latency_counter -= 1
                    if (meta_latency_counter == 0) {
                        dut.io.meta_resp.valid.poke(true.B)
                        dut.io.meta_resp.start_byte_addr
                            .poke(meta_resp_data._1.U)
                        dut.io.meta_resp.zero_point.poke(meta_resp_data._2.U)
                    }
                }

                if (stream_latency_counter > 0) {
                    stream_latency_counter -= 1
                    if (stream_latency_counter == 0) {
                        dut.io.stream_resp.valid.poke(true.B)
                        dut.io.stream_resp.data.poke(stream_resp_data.U)
                    }
                }

                // --- 2. 模拟 IO *请求* ---
                if (
                  dut.io.meta_req.valid
                      .peek()
                      .litValue > 0 && meta_latency_counter == 0
                ) {
                    val addr = dut.io.meta_req.addr.peek().litValue.toInt
                    meta_resp_data = metaDB.getOrElse(addr, (0, 0))
                    meta_latency_counter = 2
                }
                if (
                  dut.io.stream_req.valid
                      .peek()
                      .litValue > 0 && stream_latency_counter == 0
                ) {
                    val addr = dut.io.stream_req.addr.peek().litValue.toInt
                    stream_resp_data = streamDB.getOrElse(addr, BigInt(0))
                    stream_latency_counter = 3
                }

                // --- 3. 监视 SRAM 写入 ---
                if (dut.io.sram_write.valid.peek().litValue > 0) {
                    val addr = dut.io.sram_write.addr.peek().litValue.toInt
                    val data = dut.io.sram_write.data.peek().litValue.toInt
                    if (addr < p.groupSize) {
                        capturedWeights(addr) = data
                    }
                }

                dut.clock.step(1)
                cycles += 1
            }

            println(s"Fallback Test: FSM finished in $cycles cycles.")
            cycles must be < timeout
            capturedWeights must contain theSameElementsAs (goldenWeights)
        }
    }

    // // 场景 4: GR 错误
    "GRDecoderCore should stop on GR Error (Scene 4)" in {

        implicit val p = GRDecoderCoreParams.default

        simulate(new GRDecoderCore(p, 0.U)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            val capturedWeights = Array.fill(p.groupSize)(-1)

            // 模拟 IO 的状态
            var meta_latency_counter = 0
            var stream_latency_counter = 0
            var meta_resp_data: (Int, Int) = (0, 0)
            var stream_resp_data: BigInt = 0

            // 启动 FSM (组 2 - Error)
            dut.io.start.poke(true.B)
            dut.io.group_index.poke(2.U)
            dut.clock.step(1)
            dut.io.start.poke(false.B)

            var cycles = 0
            val timeout = 50 // FSM 必须在 50 周期内停止

            while (dut.io.finished.peek().litValue == 0 && cycles < timeout) {
                // 在每个周期开始时,默认将所有响应 Valid 拉低
                dut.io.meta_resp.valid.poke(false.B)
                dut.io.stream_resp.valid.poke(false.B)

                // (模拟 IO 的完整逻辑)
                if (meta_latency_counter > 0) {
                    meta_latency_counter -= 1
                    if (meta_latency_counter == 0) {
                        dut.io.meta_resp.valid.poke(true.B)
                        dut.io.meta_resp.start_byte_addr
                            .poke(meta_resp_data._1.U)
                        dut.io.meta_resp.zero_point.poke(meta_resp_data._2.U)
                    }
                }

                if (stream_latency_counter > 0) {
                    stream_latency_counter -= 1
                    if (stream_latency_counter == 0) {
                        dut.io.stream_resp.valid.poke(true.B)
                        dut.io.stream_resp.data.poke(stream_resp_data.U)
                    }
                }

                if (
                  dut.io.meta_req.valid
                      .peek()
                      .litValue > 0 && meta_latency_counter == 0
                ) {
                    val addr = dut.io.meta_req.addr.peek().litValue.toInt
                    meta_resp_data = metaDB.getOrElse(addr, (0, 0))
                    meta_latency_counter = 2
                }
                if (
                  dut.io.stream_req.valid
                      .peek()
                      .litValue > 0 && stream_latency_counter == 0
                ) {
                    val addr = dut.io.stream_req.addr.peek().litValue.toInt
                    stream_resp_data = streamDB.getOrElse(addr, BigInt(0))
                    stream_latency_counter = 3
                }

                dut.clock.step(1)
                cycles += 1
            }

            println(s"GR Error Test: FSM finished in $cycles cycles.")

            // FSM 必须在 512 周期 *之前* 停止
            cycles must be < timeout

            // FSM 必须在出错时停止,不应填满 SRAM
            capturedWeights(0) must be(-1)
            capturedWeights(511) must be(-1)
        }
    }
}
