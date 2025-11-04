package group_decoder

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable.ArrayBuffer

class ParallelReconstructorArraySpec
    extends AnyFreeSpec
    with Matchers
    with ChiselSim {

    // 使用较小的unrollFactor以方便手动创建测试向量
    val p = PDUParams(unrollFactor = 4)

    /** 软件参考模型 (Golden Model) 完美模拟ParallelReconstructorArray的预期行为
      */
    def calculateExpected_weights(
        k_in: Int,
        zp: Int,
        qs: Seq[Int],
        rs: Seq[Int]
    ): Seq[Int] = {
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
            val final_weight_signed = signed_delta + zp
            // 使用位与操作 `& 0xF` 来确保结果是正确的4-bit无符号值
            final_weights += (final_weight_signed & 0xf)
        }
        final_weights.toSeq
    }

    "ParallelReconstructorArray should correctly reconstruct final weights" in {
        simulate(new ParallelReconstructorArray(p)) { dut =>
            /** 测试向量: (名称, k_in, zp, q向量, r向量)
              */
            val testVectors = Seq(
              (
                "k=2, zp=8, mixed deltas",
                1,
                8, // k=2, zp=8
                // q, r -> unsigned -> signed -> final_weight
                // 0, 0 -> 0 -> 0 -> 8
                // 1, 1 -> 5 -> -3 -> 5
                // 2, 2 -> 10 -> 5 -> 13
                // 3, 3 -> 15 -> -8 -> 0
                Seq(0, 1, 2, 3),
                Seq(0, 1, 2, 3)
              ),
              (
                "k=1, zp=7, more mixed deltas",
                0,
                7, // k=1, zp=7
                // q, r -> unsigned -> signed -> final_weight
                // 0, 1 -> 1 -> -1 -> 6
                // 2, 0 -> 4 -> 2 -> 9
                // 3, 1 -> 7 -> -4 -> 3
                // 4, 0 -> 8 -> 4 -> 11
                Seq(0, 2, 3, 4),
                Seq(1, 0, 1, 0)
              ),
              (
                "k=3, zp=1, boundary check",
                2,
                1, // k=3, zp=1
                // q, r -> unsigned -> signed -> final_weight
                // 0, 0 -> 0 -> 0 -> 1
                // 1, 6 -> 14 -> 7 -> 8
                // 1, 7 -> 15 -> -8 -> -7 (wrap around) -> 9
                // 0, 1 -> 1 -> -1 -> 0
                Seq(0, 1, 1, 0),
                Seq(0, 6, 7, 1)
              )
            )

            // --- Standard Reset ---
            dut.reset.poke(true.B); dut.clock.step(1); dut.reset.poke(false.B);
            dut.clock.step(1)

            var testNum = 0
            for ((name, k_in, zp, qs, rs) <- testVectors) {
                testNum += 1
                println(s"\n--- Running Test $testNum: $name ---")

                // 1. 使用软件参考模型计算期望的权重向量
                val expected_weights =
                    calculateExpected_weights(k_in, zp, qs, rs)

                // 2. 将所有输入送入DUT
                dut.io.k.poke(k_in.U)
                dut.io.zp.poke(zp.U)
                dut.io.final_q_vec.zip(qs).foreach { case (port, value) =>
                    port.poke(value.U)
                }
                dut.io.r_vec.zip(rs).foreach { case (port, value) =>
                    port.poke(value.U)
                }

                // 3. 等待组合逻辑传播
                dut.clock.step(1)

                // 4. 验证输出 final_weights_vec
                println(
                  s"  Verifying final_weights_vec (expected: ${expected_weights
                          .mkString("[", ", ", "]")})"
                )
                dut.io.final_weights_vec.zip(expected_weights).foreach {
                    case (dut_w, exp_w) => dut_w.expect(exp_w.U)
                }

                println(s"--- Test $testNum Passed ---")
            }
        }
    }
}
