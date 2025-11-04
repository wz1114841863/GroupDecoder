package group_decoder.mdu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import group_decoder.common._

class FallbackHoldBufferSpec extends AnyFreeSpec with Matchers with ChiselSim {

    val p = PDUParams(groupSize = 32, unrollFactor = 4)
    val m = MDUParams(fbHoldBufferSlots = 4)

    /** 辅助函数:向 DUT 发送一个数据
      */
    def pokeIn(dut: FallbackHoldBuffer, tag: Int, data: BigInt): Unit = {
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.tag.poke(tag.U)
        dut.io.in.bits.raw_data.poke(data.U)

        var cycles = 0
        while (!dut.io.in.ready.peek().litToBoolean) {
            cycles += 1
            assert(cycles < 50, "Timeout waiting for in.ready")
            dut.clock.step(1)
        }
        dut.clock.step(1)
        dut.io.in.valid.poke(false.B)
    }

    /** 辅助函数:查询一个tag, 并期望它是 'ready' 或 'not ready'
      */
    def expectQuery(
        dut: FallbackHoldBuffer,
        tag: Int,
        shouldBeReady: Boolean
    ): Unit = {
        dut.io.query_valid.poke(true.B)
        dut.io.query_tag.poke(tag.U)
        dut.io.is_ready.expect(shouldBeReady.B)

        // **关键**: 在查询后, 保持query_valid=false, 除非下一个操作是读取
        dut.clock.step(1)
        dut.io.query_valid.poke(false.B)
    }

    /** 辅助函数:在一个周期内完成 查询-握手-读取-验证
      */
    def queryAndRead(dut: FallbackHoldBuffer, tag: Int, data: BigInt): Unit = {
        // 1. 发起查询并同时表示准备好接收
        dut.io.query_valid.poke(true.B)
        dut.io.query_tag.poke(tag.U)
        dut.io.out.ready.poke(true.B)

        // 2. 验证硬件的响应
        dut.io.is_ready.expect(true.B, "DUT should be ready for this query")
        dut.io.out.valid
            .expect(true.B, "DUT should provide valid data this cycle")

        // 3. 验证数据内容
        dut.io.out.bits.tag.expect(tag.U)
        dut.io.out.bits.raw_data.expect(data.U)

        // 4. 步进时钟, 完成握手
        dut.clock.step(1)

        // 5. 清理输入信号
        dut.io.query_valid.poke(false.B)
        dut.io.out.ready.poke(false.B)
    }

    // --- 测试场景 ---

    "FallbackHoldBuffer" - {

        "should handle basic write, query, and read operations" in {
            simulate(new FallbackHoldBuffer(p, m)) { dut =>
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)
                dut.io.in.valid.poke(false.B)
                dut.io.query_valid.poke(false.B)
                dut.io.out.ready.poke(false.B)

                println("--- Test 1: Basic Write-Read ---")

                println("Writing tag=10...")
                pokeIn(dut, 10, 0xabcd)

                println("Querying non-existent tag=99...")
                expectQuery(dut, 99, shouldBeReady = false) // (使用旧的expectQuery)

                println("Querying and Reading tag=10...")
                queryAndRead(dut, 10, 0xabcd) // (使用新的queryAndRead)

                println("Verifying slot is empty...")
                expectQuery(dut, 10, shouldBeReady = false) // (使用旧的expectQuery)
                println("--- Test 1 Passed ---")
            }
        }

        "should handle buffer full and apply backpressure" in {
            simulate(new FallbackHoldBuffer(p, m)) { dut =>
                // ... (Test 2的逻辑与之前相同, 因为它不涉及复杂的乱序读取) ...
                println("--- Test 2: Backpressure ---")
                pokeIn(dut, 1, 11); pokeIn(dut, 2, 22); pokeIn(dut, 3, 33);
                pokeIn(dut, 4, 44)
                println("Verifying buffer is full...")
                dut.io.in.ready.expect(false.B)
                println("Reading tag=3 to free a slot...")
                queryAndRead(dut, 3, 33) // (使用新的queryAndRead)
                println("Verifying backpressure is released...")
                dut.io.in.ready.expect(true.B)
                println("--- Test 2 Passed ---")
            }
        }

        "should correctly handle out-of-order reads (NOT be a FIFO)" in {
            simulate(new FallbackHoldBuffer(p, m)) { dut =>
                dut.reset.poke(true.B); dut.clock.step(1);
                dut.reset.poke(false.B)
                dut.io.in.valid.poke(false.B)
                dut.io.query_valid.poke(false.B)
                dut.io.out.ready.poke(false.B)

                println("--- Test 3: Head-of-Line Blocking Test ---")

                println("Writing tag=87 (to slot 0)...")
                pokeIn(dut, 87, 0xaaaa)
                println("Writing tag=88 (to slot 1)...")
                pokeIn(dut, 88, 0xbbbb)

                // 2. **关键**: 乱序读取, 先读取 88
                println(
                  "Querying and Reading tag=88 (which is NOT at the head)..."
                )
                queryAndRead(dut, 88, 0xbbbb)

                // 3. 验证 87 仍然存在
                println("Verifying tag=87 (the original head) still exists...")
                expectQuery(dut, 87, shouldBeReady = true)
                // 验证 88 已被清空
                expectQuery(dut, 88, shouldBeReady = false)

                // 4. 读取 87
                println("Querying and Reading tag=87...")
                queryAndRead(dut, 87, 0xaaaa)

                // 5. 验证两者均已清空
                println("Verifying buffer is now empty...")
                expectQuery(dut, 87, shouldBeReady = false)
                expectQuery(dut, 88, shouldBeReady = false)

                println("--- Test 3 (Anti-FIFO Test) Passed ---")
            }
        }
    }
}
