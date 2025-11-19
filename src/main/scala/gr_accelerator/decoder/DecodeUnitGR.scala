package gr_accelerator.decoder

import chisel3._
import chisel3.util._
import gr_accelerator.common.GRDecoderConfig

/** GR 解码器组合逻辑核心的 IO 端口.
  */
class DecodeUnitGRIO(val p: GRDecoderConfig) extends Bundle {
    // --- 输入 ---
    // 来自 FSM 的已对齐的块
    val aligned_chunk = Input(UInt(p.grChunkWidth.W))

    // k_in=0 (k=1), k_in=1 (k=2)
    val k_in = Input(UInt(p.grKInWidth.W))

    // --- 输出 ---
    // 最终的 q 和 r 值
    val final_q = Output(UInt(p.grQValWidth.W))
    val final_r = Output(UInt(p.grRValWidth.W))

    // 报告 GR 消耗的比特 (q+1+k)
    val consumed_bits_gr = Output(UInt(p.grLengthWidth.W))

    // error 信号
    val error = Output(Bool())
}

/** GR 解码器组合逻辑核心, 实现GR解码的快慢路径逻辑.
  */
class DecodeUnitGR(val p: GRDecoderConfig) extends Module {
    val io = IO(new DecodeUnitGRIO(p))

    // 模块的"k"值 (1.U 或 2.U)
    val k_val = io.k_in + 1.U(p.grKValWidth.W)

    // --- 1. 快路径 (Fast Path) ---
    val fast_path_valid = WireDefault(false.B)
    val fast_path_q = WireDefault(0.U(p.grQValWidth.W))
    val fast_path_r = WireDefault(0.U(p.grRValWidth.W))
    val fast_path_len = WireDefault(0.U(p.grLengthWidth.W))

    // 前提: FSM 保证了数据已对齐
    val stream_top_bits =
        io.aligned_chunk(
          p.grChunkWidth - 1,
          p.grChunkWidth - p.grFastPathMaxBits
        )

    when(io.k_in === 0.U) { // k=1
        when(stream_top_bits(5, 4) === "b00".U) { // "00xxxx" -> q=0, r=0 (len=2)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 0.U;
            fast_path_len := (0 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 4) === "b01".U) { // "01xxxx" -> q=0, r=1 (len=2)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 1.U;
            fast_path_len := (0 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 3) === "b100".U) { // "100xxx" -> q=1, r=0 (len=3)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 0.U;
            fast_path_len := (1 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 3) === "b101".U) { // "101xxx" -> q=1, r=1 (len=3)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 1.U;
            fast_path_len := (1 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 2) === "b1100".U) { // "1100xx" -> q=2, r=0 (len=4)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 0.U;
            fast_path_len := (2 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 2) === "b1101".U) { // "1101xx" -> q=2, r=1 (len=4)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 1.U;
            fast_path_len := (2 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 1) === "b11100".U) { // "11100x" -> q=3, r=0 (len=5)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 0.U;
            fast_path_len := (3 + 1 + 1).U
        }.elsewhen(stream_top_bits(5, 1) === "b11101".U) { // "11101x" -> q=3, r=1 (len=5)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 1.U;
            fast_path_len := (3 + 1 + 1).U
        }.elsewhen(stream_top_bits === "b111100".U) { // "111100" -> q=4, r=0 (len=6)
            fast_path_valid := true.B; fast_path_q := 4.U; fast_path_r := 0.U;
            fast_path_len := (4 + 1 + 1).U
        }.elsewhen(stream_top_bits === "b111101".U) { // "111101" -> q=4, r=1 (len=6)
            fast_path_valid := true.B; fast_path_q := 4.U; fast_path_r := 1.U;
            fast_path_len := (4 + 1 + 1).U
        }
        // (q=5, len=7 将未命中,由慢路径 处理)
    }.otherwise { // k=2
        when(stream_top_bits(5, 3) === "b000".U) { // "000xxx" -> q=0, r=0 (len=3)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 0.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 3) === "b001".U) { // "001xxx" -> q=0, r=1 (len=3)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 1.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 3) === "b010".U) { // "010xxx" -> q=0, r=2 (len=3)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 2.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 3) === "b011".U) { // "011xxx" -> q=0, r=3 (len=3)
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 3.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 2) === "b1000".U) { // "1000xx" -> q=1, r=0 (len=4)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 0.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 2) === "b1001".U) { // "1001xx" -> q=1, r=1 (len=4)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 1.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 2) === "b1010".U) { // "1010xx" -> q=1, r=2 (len=4)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 2.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 2) === "b1011".U) { // "1011xx" -> q=1, r=3 (len=4)
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 3.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 1) === "b11000".U) { // "11000x" -> q=2, r=0 (len=5)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 0.U;
            fast_path_len := (2 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 1) === "b11001".U) { // "11001x" -> q=2, r=1 (len=5)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 1.U;
            fast_path_len := (2 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 1) === "b11010".U) { // "11010x" -> q=2, r=2 (len=5)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 2.U;
            fast_path_len := (2 + 1 + 2).U
        }.elsewhen(stream_top_bits(5, 1) === "b11011".U) { // "11011x" -> q=2, r=3 (len=5)
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 3.U;
            fast_path_len := (2 + 1 + 2).U
        }.elsewhen(stream_top_bits === "b111000".U) { // "111000" -> q=3, r=0 (len=6)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 0.U;
            fast_path_len := (3 + 1 + 2).U
        }.elsewhen(stream_top_bits === "b111001".U) { // "111001" -> q=3, r=1 (len=6)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 1.U;
            fast_path_len := (3 + 1 + 2).U
        }.elsewhen(stream_top_bits === "b111010".U) { // "111010" -> q=3, r=2 (len=6)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 2.U;
            fast_path_len := (3 + 1 + 2).U
        }.elsewhen(stream_top_bits === "b111011".U) { // "111011" -> q=3, r=3 (len=6)
            fast_path_valid := true.B; fast_path_q := 3.U; fast_path_r := 3.U;
            fast_path_len := (3 + 1 + 2).U
        }
        // (q=4, len=7 将未命中,由慢路径 处理)
    }

    // --- 2. 慢路径 (Slow Path) ---
    val slow_path_q = WireDefault(0.U(p.grQValWidth.W))
    val slow_path_r = WireDefault(0.U(p.grRValWidth.W))
    val slow_path_len = WireDefault(0.U(p.grLengthWidth.W))
    val slow_path_error = WireDefault(false.B)

    when(!fast_path_valid) {
        // 解码 q (商)
        // 根据数据, q_max=15, 所以我们只需要看16比特 (q=15个'1' + 1个'0')
        val q_stream =
            io.aligned_chunk(
              p.grChunkWidth - 1,
              p.grChunkWidth - p.grQStreamWidth
            )

        // 对流取反: "1110..." 变为 "0001..."
        // (q_stream_width - 1, 0) 用于确保我们只操作这16位
        val inverted_q_stream = (~q_stream)(p.grQStreamWidth - 1, 0)
        // 检查错误: 如果取反后全是'0', 说明q > qMax
        val q_is_valid = (inverted_q_stream =/= 0.U)
        // 计算前导零的数量, 这正好等于 q
        val inverted_vec = inverted_q_stream.asBools
        // 使用 PriorityEncoder 找到第一个 'T' 的索引
        val q = PriorityEncoder(inverted_vec.reverse)

        when(q_is_valid) {
            slow_path_q := q
            val q_consumed_bits = q +& 1.U
            val len = q_consumed_bits + k_val
            slow_path_len := len

            // 解码 r (余数)
            val r_shifted_stream = (io.aligned_chunk << q_consumed_bits)

            when(k_val === 1.U) {
                slow_path_r := r_shifted_stream(p.grChunkWidth - 1)
            }.otherwise {
                slow_path_r := r_shifted_stream(
                  p.grChunkWidth - 1,
                  p.grChunkWidth - 2
                )
            }
        }.otherwise {
            slow_path_error := true.B // q > qMax
        }
    }.otherwise {
        slow_path_q := 0.U
        slow_path_r := 0.U
        slow_path_len := 0.U
        slow_path_error := false.B
    }

    // --- 3. 最终输出 Mux ---
    io.final_q := Mux(fast_path_valid, fast_path_q, slow_path_q)
    io.final_r := Mux(fast_path_valid, fast_path_r, slow_path_r)

    io.consumed_bits_gr := Mux(fast_path_valid, fast_path_len, slow_path_len)
    io.error := Mux(fast_path_valid, false.B, slow_path_error)
}
