package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

import group_decoder.common.PDUParams

/** DecoderStageSpec 测试模块
  */
class DecoderStageSpec extends AnyFreeSpec with Matchers with ChiselSim {
    // 1. 实例化我们的共享配置
    implicit val p: PDUParams = PDUParams(unrollFactor = 4)
    val sp = DecoderStageParams(stageWidth = 4) // 我们的S1/S2阶段宽度为4

    /** 软件参考模型: 串行解码 N (stageWidth) 个权重.
      */
    def calculateExpected_Stage(
        peekWindowStr: String,
        k_in: Int,
        start_offset_in: Int
    ): (Seq[Int], Seq[Int], Int, Int, Boolean) = {

        val k_val = k_in + 1
        val final_qs = new ArrayBuffer[Int]()
        val final_rs = new ArrayBuffer[Int]()
        val lengths = new ArrayBuffer[Int]()
        var error = false

        var current_bit_ptr = start_offset_in

        for (i <- 0 until sp.stageWidth) {
            if (current_bit_ptr >= peekWindowStr.length) {
                // 如果指针超出范围, 后续结果都为0
                final_qs += 0
                final_rs += 0
                lengths += 0
            } else {
                // --- 模拟 MicroDecoder 的 Slow Path ---
                // 1. 寻找第一个'0' (q)
                val first_zero_idx = peekWindowStr.indexOf('0', current_bit_ptr)
                var q = 0
                if (first_zero_idx == -1) { // 没找到'0' (异常)
                    q = p.peekWindowWidth - current_bit_ptr
                } else {
                    q = first_zero_idx - current_bit_ptr
                }

                if (q > p.qMax) {
                    error = true
                    q = p.qMax // 限制q值
                }
                final_qs += q

                // 2. 寻找余数r
                val q_consumed = q + 1
                val r_start_idx = current_bit_ptr + q_consumed
                var r = 0
                if (r_start_idx + k_val > peekWindowStr.length) {
                    error = true // 数据不足
                } else {
                    val r_str = peekWindowStr.substring(
                      r_start_idx,
                      r_start_idx + k_val
                    )
                    r = BigInt(r_str, 2).toInt
                }
                final_rs += r

                // 3. 计算长度
                val len = q_consumed + k_val
                lengths += len
                current_bit_ptr += len
            }
        }

        val total_consumed = current_bit_ptr - start_offset_in
        val next_offset = current_bit_ptr
        (final_qs.toSeq, final_rs.toSeq, total_consumed, next_offset, error)
    }

    "DecoderStage (v5.0) should correctly cascade decode 4 weights" in {

        // 2. 启动模拟
        simulate(new DecoderStage(p, sp)) { dut =>
            // 3. 标准复位序列
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            // 辅助函数: 创建一个BigInt, 从MSB(左)侧填充字符串
            def s(bitString: String): BigInt = {
                val padded = bitString.padTo(p.peekWindowWidth, '0')
                BigInt(padded, 2)
            }

            /** 测试向量: (名称, k_in, start_offset_in, 输入比特流)
              */
            val testVectors = Seq(
              (
                "Test 1: Simple Case (k=1, offset=0)",
                0,
                0, // k_in=0 -> k=1
                // 硬件应能处理您在GRDecodeSliceSpec中 的Fast Path情况
                // w0: q=2,r=1,len=4 ("1101")
                // w1: q=1,r=0,len=3 ("100")
                // w2: q=0,r=0,len=2 ("00")
                // w3: q=3,r=1,len=5 ("11101")
                "1101" + "100" + "00" + "11101" + "0" * (p.peekWindowWidth - 14)
              ),
              (
                "Test 2: Mixed Path (k=2, offset=10)",
                1,
                10, // k_in=1 -> k=2
                // 窗口前10位是垃圾数据
                // w0: q=1,r=3,len=4 (Fast: "1011")
                // w1: q=3,r=1,len=6 (Slow: "111001")
                // w2: q=0,r=2,len=3 (Fast: "010")
                // w3: q=1,r=0,len=4 (Fast: "1000")
                "0" * 10 + "1011" + "111001" + "010" + "1000" + "0" * (p.peekWindowWidth - 27)
              ),
              (
                "Test 3: Cascade Cross-Segment (k=1, offset=0)",
                0,
                0, // k_in=0 -> k=1
                // (这是一个更真实的/模拟analy.py 数据的测试)
                // w0: q=15,r=0,len=17 (Slow: "1"*15 + "0" + "0")
                // w1: q=7,r=1,len=9  (Slow: "1"*7 + "0" + "1")
                // w2: q=2,r=1,len=4  (Fast: "1101")
                // w3: q=0,r=0,len=2  (Fast: "00")
                "1" * 15 + "0" + "0" +
                    "1" * 7 + "0" + "1" +
                    "1101" +
                    "00" + "0" * (p.peekWindowWidth - 32)
              )
            )

            var testNum = 0
            for ((name, k_in, start_offset, bitstream) <- testVectors) {
                testNum += 1
                println(s"\n--- 运行测试 $testNum: $name ---")

                // 3. 使用软件参考模型计算期望的权重向量
                val (exp_qs, exp_rs, exp_total, exp_next_off, exp_err) =
                    calculateExpected_Stage(bitstream, k_in, start_offset)

                // 4. 将所有输入 Poke 到 DUT
                dut.io.peek_window.poke(s(bitstream))
                dut.io.start_offset_in.poke(start_offset.U)
                dut.io.k_in.poke(k_in.U)

                // 5. 等待组合逻辑传播
                dut.clock.step(1)

                // 6. 验证所有输出
                println(
                  s"  验证 final_q_vec (期望: ${exp_qs.mkString("[", ", ", "]")})"
                )
                dut.io.final_q_vec.zip(exp_qs).foreach { case (d, e) =>
                    d.expect(e.U)
                }

                println(
                  s"  验证 final_r_vec (期望: ${exp_rs.mkString("[", ", ", "]")})"
                )
                dut.io.final_r_vec.zip(exp_rs).foreach { case (d, e) =>
                    d.expect(e.U)
                }

                println(s"  验证 total_consumed_bits_out (期望: $exp_total)")
                dut.io.total_consumed_bits_out.expect(exp_total.U)

                println(s"  验证 next_stage_offset_out (期望: $exp_next_off)")
                dut.io.next_stage_offset_out.expect(exp_next_off.U)

                println(s"  验证 error (期望: $exp_err)")
                dut.io.error.expect(exp_err.B)

                println(s"--- 测试 $testNum 通过 ---")
            }

            println("\n--- DecoderStage 所有测试用例通过 ---")
        }
    }
}
