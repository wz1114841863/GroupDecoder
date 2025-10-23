package group_decoder

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class DeltaReconstructorSpec extends AnyFreeSpec with Matchers with ChiselSim {

    "DeltaReconstructor should reconstruct original values from deltas" in {
        simulate(new DeltaReconstructor()) { dut =>
            // 测试向量: (输入unsigned_delta, 输入zp, 期望输出quantized_weight)
            // 算法:
            // if (unsigned % 2 == 0) signed = unsigned / 2
            // else signed = -(unsigned + 1) / 2
            // weight = signed + zp
            val testVectors = Seq(
              // zp = 8
              (0, 8, 8), // signed = 0/2=0;       weight = 0  + 8 = 8
              (1, 8, 7), // signed = -(2)/2=-1;   weight = -1 + 8 = 7
              (2, 8, 9), // signed = 2/2=1;       weight = 1  + 8 = 9
              (3, 8, 6), // signed = -(4)/2=-2;   weight = -2 + 8 = 6
              (15, 8, 0), // signed = -(16)/2=-8; weight = -8 + 8 = 0
              // zp = 0
              (0, 0, 0),
              // signed = -1; weight = -1+0=-1, as 4-bit unsigned is 15
              (1, 0, 15)
            )

            println("--- Testing DeltaReconstructor ---")
            for ((unsigned, zp, expected) <- testVectors) {
                dut.io.unsigned_delta.poke(unsigned.U)
                dut.io.zp.poke(zp.U)
                dut.clock.step(1) // 组合逻辑,等待一个周期让信号传播
                dut.io.quantized_weight.expect(
                  expected.U,
                  s"Failed for unsigned=$unsigned, zp=$zp"
                )
                println(
                  s"Passed: unsigned=$unsigned, zp=$zp -> expected=$expected"
                )
            }
        }
    }
}
