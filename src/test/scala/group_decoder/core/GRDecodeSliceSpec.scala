package group_decoder.core

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import group_decoder.common.DecoderConfig

/** GRDecodeSliceSpec 测试模块
  */
class GRDecodeSliceSpec extends AnyFreeSpec with Matchers with ChiselSim {

    "GRDecodeSlice (v3 Dual Path) should correctly decode all test vectors" in {

        // 1. 实例化我们的共享配置
        implicit val config = DecoderConfig()

        // 2. 启动模拟
        simulate(new GRDecodeSlice) { dut =>
            // 3. 模板中的标准复位序列
            // 尽管这是一个组合逻辑模块, 但我们仍然执行复位以确保初始状态良好.
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)
            dut.clock.step()

            // 测试用例 1: (k=1) Fast Path 2-bit  q=0,r=1 → delta=1, bits=2
            dut.io.k_value.poke(1.U)
            dut.io.stream_in.poke("b0100000000000000000".U) // 19'b01...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(1.U)
            dut.io.bits_consumed.expect(2.U)

            // 测试用例 2: (k=1) Fast Path 4-bit  q=2,r=1 → delta=5, bits=4
            dut.io.k_value.poke(1.U)
            dut.io.stream_in.poke("b1101000000000000000".U) // 19'b1101...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(5.U)
            dut.io.bits_consumed.expect(4.U)

            // 测试用例 3: (k=2) Fast Path 3-bit  q=0,r=3 → delta=3, bits=3
            dut.io.k_value.poke(2.U)
            dut.io.stream_in.poke("b0110000000000000000".U) // 19'b011...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(3.U)
            dut.io.bits_consumed.expect(3.U)

            // 测试用例 4: (k=2) Fast Path 4-bit  q=1,r=2 → delta=6, bits=4
            dut.io.k_value.poke(2.U)
            dut.io.stream_in.poke("b1010000000000000000".U) // 19'b1010...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(6.U)
            dut.io.bits_consumed.expect(4.U)

            // 测试用例 5: (k=1) Slow Path  q=3,r=1 → delta=7, bits=5
            dut.io.k_value.poke(1.U)
            dut.io.stream_in.poke("b1110100000000000000".U) // 19'b11101...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(7.U)
            dut.io.bits_consumed.expect(5.U)

            // 测试用例 6: (k=2) Slow Path  q=2,r=0 → delta=8, bits=5
            dut.io.k_value.poke(2.U)
            dut.io.stream_in.poke("b1100000000000000000".U) // 19'b11000...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(8.U)
            dut.io.bits_consumed.expect(5.U)

            // 测试用例 7: (k=3) Slow Path  q=3,r=6 → delta=30, bits=7
            dut.io.k_value.poke(3.U)
            dut.io.stream_in.poke("b1110110000000000000".U) // 19'b1110110...
            dut.clock.step(1)
            dut.io.mapped_delta.expect(30.U)
            dut.io.bits_consumed.expect(7.U)

            // 测试用例 8: (k=1) Worst-case  q=15,r=0 → delta=30, bits=17
            dut.io.k_value.poke(1.U)
            dut.io.stream_in.poke("b1111111111111110000".U) // 19'b1^15 0 0
            dut.clock.step(1)
            dut.io.mapped_delta.expect(30.U)
            dut.io.bits_consumed.expect(17.U)
        }
    }
}
