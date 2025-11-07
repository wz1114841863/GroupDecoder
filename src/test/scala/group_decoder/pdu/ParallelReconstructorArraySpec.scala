package group_decoder.pdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

import group_decoder.common.PDUParams

class ParallelReconstructorArraySpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 使用较小的unrollFactor以方便手动创建测试向量
    implicit val p: PDUParams = PDUParams(unrollFactor = 4)

    /** 软件参考模型 (Golden Model)模拟硬件的预期行为
      */
    def calculateExpected_weights(
        k_in: Int,
        zp: Int,
        qs: Seq[Int],
        rs: Seq[Int]
    ): Seq[Int] = {
        // 我们的PDUParams.kMax=2, 所以k_in只可能是0或1
        val k_val = k_in + 1
        val final_weights = new ArrayBuffer[Int]()

        for (i <- 0 until p.unrollFactor) {
            // 步骤 A: 合并q和r, 得到unsigned_delta
            val unsigned_delta = (qs(i) << k_val) | rs(i)

            // 步骤 B: 逆映射, 得到signed_delta
            val signed_delta = if (unsigned_delta % 2 == 0) {
                unsigned_delta / 2
            } else {
                -(unsigned_delta + 1) / 2
            }

            // 步骤 C: 添加零点, 得到最终权重 (并处理4-bit无符号数的环绕)
            val sum = signed_delta + zp // [-15,30]
            val sum5 = sum & 0x1f // 取 5-bit 无符号模式
            final_weights += sum5 & 0xf // 再取低 4 位
        }
        final_weights.toSeq
    }

    "ParallelReconstructorArray should correctly reconstruct final weights" in {

        // 1. 启动模拟
        simulate(new ParallelReconstructorArray(p)) { dut =>
            /** 测试向量: (名称, k_in, zp, q向量, r向量) (基于 analy.py 的数据, q_max=15,
              * r_max=3)
              */
            val testVectors = Seq(
              (
                "k=1, zp=8, mixed deltas",
                0,
                8, // k_in=0 -> k=1
                // q, r -> unsigned -> signed -> final_weight
                // 0, 0 -> 0 -> 0 -> 8
                // 1, 1 -> 3 -> -2 -> 6
                // 2, 0 -> 4 -> 2 -> 10
                // 7, 1 -> 15 -> -8 -> 0
                Seq(0, 1, 2, 7),
                Seq(0, 1, 0, 1)
              ),
              (
                "k=2, zp=2, boundary & wrap-around",
                1,
                2, // k_in=1 -> k=2
                // q, r -> unsigned -> signed -> final_weight
                // 0, 1 -> 1 -> -1 -> 1
                // 1, 3 -> 7 -> -4 -> -2 (wrap) -> 14
                // 2, 0 -> 8 -> 4 -> 6
                // 3, 2 -> 14 -> 7 -> 9
                Seq(0, 1, 2, 3),
                Seq(1, 3, 0, 2)
              ),
              (
                "k=1, zp=1, max value check",
                0,
                1, // k_in=0 -> k=1
                // q, r -> unsigned -> signed -> final_weight
                // 0, 0 -> 0 -> 0 -> 1
                // 7, 0 -> 14 -> 7 -> 8
                // 15, 0 -> 30 -> 15 -> 16 (wrap) -> 0
                // 1, 1 -> 3 -> -2 -> -1 (wrap) -> 15
                Seq(0, 7, 15, 1),
                Seq(0, 0, 0, 1)
              )
            )

            // 2. 标准复位序列 (来自 GRDecodeSliceSpec)
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B);
            dut.clock.step(1)

            var testNum = 0
            for ((name, k_in, zp, qs, rs) <- testVectors) {
                testNum += 1
                println(s"\n--- 运行测试 $testNum: $name ---")

                // 3. 使用软件参考模型计算期望的权重向量
                val expected_weights =
                    calculateExpected_weights(k_in, zp, qs, rs)

                // 4. 将所有输入 Poke 到 DUT
                dut.io.k_in.poke(k_in.U)
                dut.io.zp.poke(zp.U)
                dut.io.final_q_vec.zip(qs).foreach { case (port, value) =>
                    port.poke(value.U)
                }
                dut.io.r_vec.zip(rs).foreach { case (port, value) =>
                    port.poke(value.U)
                }

                // 5. 等待组合逻辑传播
                dut.clock.step(1)

                // 6. 验证输出 final_weights_vec
                println(
                  s"  验证 final_weights_vec (期望: ${expected_weights.mkString("[", ", ", "]")})"
                )
                dut.io.final_weights_vec.zip(expected_weights).foreach {
                    case (dut_w, exp_w) => dut_w.expect(exp_w.U)
                }

                println(s"--- 测试 $testNum 通过 ---")
            }

            println("\n--- ParallelReconstructorArray 所有测试用例通过 ---")
        }
    }
}
