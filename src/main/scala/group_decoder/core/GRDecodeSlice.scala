package group_decoder.core

import chisel3._
import chisel3.util._
import group_decoder.common.DecoderConfig

// format: off
/** GRDecodeSlice 的 I/O 接口定义.
  * 这是一个纯组合逻辑模块.
  */
// format: on
class GRDecodeSliceIO(implicit val config: DecoderConfig) extends Bundle {

    /** 输入: k 值 (例如 1.U, 2.U) */
    val k_value = Input(UInt(config.kValueWidth.W))

    /** 输入: MSB-first 的位流 (宽度 19 bits) */
    val stream_in = Input(UInt(config.streamWidthIn.W))

    /** 输出: 解码后的无符号差值 (mapped_delta) */
    val mapped_delta = Output(UInt(config.mappedDeltaWidth.W))

    /** 输出: 此 delta 消耗的总比特数 (q + 1 + k) */
    val bits_consumed = Output(UInt(config.bitsConsumedWidth.W))
}

// format: off
/** GRDecodeSlice 模块实现. * 包含两个并行的内部解码路径:
  *   1. Fast Path (LUT):
  *        处理 80%+ 的 <= 4-bit 的情况.
  *   2. Slow Path(PriorityEncoder):
  *        处理所有 Fast Path 未命中的情况.
  */
// format: on
class GRDecodeSlice(implicit val config: DecoderConfig) extends Module {
    val io = IO(new GRDecodeSliceIO)

    // --- 1. Fast Path (LUT) ---
    // 目标: 检查 k=1 和 k=2 时所有 <= 4-bit 的编码.
    val fast_path_valid = WireDefault(false.B)
    val fast_path_delta = WireDefault(0.U(config.mappedDeltaWidth.W))
    val fast_path_bits = WireDefault(0.U(config.bitsConsumedWidth.W))

    // 仅查看流的最高 4 位
    val stream_top4 = io.stream_in(
      config.streamWidthIn - 1,
      config.streamWidthIn - config.FAST_PATH_MAX_BITS
    )

    when(io.k_value === 1.U) { // k=1
        // 2-bit codes (q=0, r=0/1)
        // 3-bit codes (q=1, r=0/1)
        // 4-bit codes (q=2, r=0/1)
        when(stream_top4(3, 2) === "b00".U) { // "00xx"
            fast_path_valid := true.B
            fast_path_delta := 0.U
            fast_path_bits := 2.U
        }.elsewhen(stream_top4(3, 2) === "b01".U) { // "01xx"
            fast_path_valid := true.B
            fast_path_delta := 1.U
            fast_path_bits := 2.U
        }.elsewhen(stream_top4(3, 1) === "b100".U) { // "100x"
            fast_path_valid := true.B
            fast_path_delta := 2.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 1) === "b101".U) { // "101x"
            fast_path_valid := true.B
            fast_path_delta := 3.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 0) === "b1100".U) { // "1100"

            fast_path_valid := true.B
            fast_path_delta := 4.U
            fast_path_bits := 4.U
        }.elsewhen(stream_top4(3, 0) === "b1101".U) { // "1101"
            fast_path_valid := true.B
            fast_path_delta := 5.U
            fast_path_bits := 4.U
        }
    }.elsewhen(io.k_value === 2.U) { // k=2
        // 3-bit codes (q=0, r=0/1/2/3)
        // 4-bit codes (q=1, r=0/1/2/3)
        when(stream_top4(3, 1) === "b000".U) { // "000x"
            fast_path_valid := true.B
            fast_path_delta := 0.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 1) === "b001".U) { // "001x"
            fast_path_valid := true.B
            fast_path_delta := 1.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 1) === "b010".U) { // "010x"
            fast_path_valid := true.B
            fast_path_delta := 2.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 1) === "b011".U) { // "011x"
            fast_path_valid := true.B
            fast_path_delta := 3.U
            fast_path_bits := 3.U
        }.elsewhen(stream_top4(3, 0) === "b1000".U) { // "1000"
            fast_path_valid := true.B
            fast_path_delta := 4.U
            fast_path_bits := 4.U
        }.elsewhen(stream_top4(3, 0) === "b1001".U) { // "1001"
            fast_path_valid := true.B
            fast_path_delta := 5.U
            fast_path_bits := 4.U
        }.elsewhen(stream_top4(3, 0) === "b1010".U) { // "1010"
            fast_path_valid := true.B
            fast_path_delta := 6.U
            fast_path_bits := 4.U
        }.elsewhen(stream_top4(3, 0) === "b1011".U) { // "1011"
            fast_path_valid := true.B
            fast_path_delta := 7.U
            fast_path_bits := 4.U
        }
    }
    // 注意: k=3 在 analy.py 中未使用, 因此 Fast Path 不处理它.
    // 它将自动回退到 Slow Path.

    // --- 2. Slow Path (Priority Encoder) ---
    // 只在Fast Path未命中时执行.
    val slow_path_delta = WireDefault(0.U(config.mappedDeltaWidth.W))
    val slow_path_bits = WireDefault(0.U(config.bitsConsumedWidth.W))

    // 只有当 fast_path_valid 为 false 时,才启动 Slow Path 硬件
    when(!fast_path_valid) {
        // 1. 解码 q (商)
        val q_stream = io.stream_in(
          config.streamWidthIn - 1,
          config.streamWidthIn - config.Q_WIDTH_MAX
        )
        // q_stream.asBools 将 LSB 放在 Vec 索引 0 处
        val q_stream_vec = q_stream.asBools

        // 步骤 1: 反转码流 (寻找 '0' 变为 寻找 '1')
        val inverted_stream_vec = q_stream_vec.map(!_)

        // 步骤 2: 寻找 MSB (最高位) 的 '1'.
        // PriorityEncoderOH 默认寻找 LSB (最低位) 的 '1'.
        // 为了寻找 MSB 的 '1', 我们反转(reverse)输入, 寻找 LSB '1', 然后再反转(reverse)输出.
        val q_oh_reversed = PriorityEncoderOH(inverted_stream_vec.reverse)
        val q_oh = q_oh_reversed.reverse

        // 步骤 3: 将 one-hot 索引转换回 q 值.
        // q_stream 是 16-bit (Q_WIDTH_MAX). MSB 索引是 15.
        //   如果 '0' 在索引 15 (MSB), q=0. (15 - 15 = 0)
        //   如果 '0' 在索引 14, q=1. (15 - 14 = 1)
        //   如果 '0' 在索引 12 (例如 "1110..."), q=3. (15 - 12 = 3)
        val q_index = OHToUInt(q_oh)
        val q_width = log2Ceil(config.Q_WIDTH_MAX + 1) // 5 bits
        val q_base = (config.Q_WIDTH_MAX - 1).U - q_index
        val q = q_base.pad(q_width) // 显式转换为 5-bit
        val q_plus_1 = q + 1.U // 15 + 1 = 16 (5-bit)
        // --- q 解码结束 ---

        // 2. 解码 r (余数)
        val shifted_stream = (io.stream_in << q_plus_1)

        val r = MuxCase(
          0.U(config.K_WIDTH_MAX.W),
          Seq(
            (io.k_value === 1.U) -> (0.U(2.W) ## shifted_stream(
              config.streamWidthIn - 1
            )),
            (io.k_value === 2.U) -> (0.U(1.W) ## shifted_stream(
              config.streamWidthIn - 1,
              config.streamWidthIn - 2
            )),
            (io.k_value === 3.U) -> shifted_stream(
              config.streamWidthIn - 1,
              config.streamWidthIn - 3
            )
          )
        )
        // 3. 重构
        // 将计算结果赋值给 slow_path wires
        slow_path_delta := (q << io.k_value) + r
        slow_path_bits := q_plus_1 + io.k_value
    }

    // --- 3. 最终输出 Mux ---
    // 如果 Fast Path 有效,则使用它;否则,回退到 Slow Path 的结果.
    io.mapped_delta := Mux(fast_path_valid, fast_path_delta, slow_path_delta)
    io.bits_consumed := Mux(fast_path_valid, fast_path_bits, slow_path_bits)
}
