package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

class MicroDecoderIO(p: PDUParams) extends Bundle {
    // --- 输入 ---
    /** 从PDU顶层传来的共享数据窗口 (e.g., 256-bit) */
    val peek_window = Input(UInt(p.peekWindowWidth.W))

    /** 本权重在此窗口内的起始偏移量 (来自前缀和树) */
    val start_offset = Input(UInt(log2Ceil(p.peekWindowWidth).W))

    /** 任务的 k 值 (k_in=0 -> k=1, k_in=1 -> k=2) */
    val k_in = Input(UInt(p.kInWidth.W))

    // --- 输出 ---
    val final_q = Output(UInt(p.qValWidth.W))
    val final_r = Output(UInt(p.rValWidth.W))
    val length = Output(UInt(p.lengthWidth.W))
    val error = Output(Bool())
}

class MicroDecoder(p: PDUParams) extends Module {
    val io = IO(new MicroDecoderIO(p))

    //  1. 核心移位操作:对齐数据流
    // 首先将共享窗口左移, 使本权重的第一个比特对齐到MSB
    val shifted_window = (io.peek_window << io.start_offset)

    // k_val 是 k=1 或 k=2
    val k_val = io.k_in + 1.U(p.kValWidth.W)

    //  2. Fast Path (LUT)
    val fast_path_valid = WireDefault(false.B)
    val fast_path_q = WireDefault(0.U(p.qValWidth.W))
    val fast_path_r = WireDefault(0.U(p.rValWidth.W))
    val fast_path_len = WireDefault(0.U(p.lengthWidth.W))

    // 只查看流的最高 4 位
    val stream_top4 = shifted_window(
      p.peekWindowWidth - 1,
      p.peekWindowWidth - p.fastPathMaxBits
    )

    when(io.k_in === 0.U) { // k=1
        // (k_val=1)
        when(stream_top4(3, 2) === "b00".U) { // "00xx" -> q=0, r=0
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 0.U;
            fast_path_len := (0 + 1 + 1).U
        }.elsewhen(stream_top4(3, 2) === "b01".U) { // "01xx" -> q=0, r=1
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 1.U;
            fast_path_len := (0 + 1 + 1).U
        }.elsewhen(stream_top4(3, 1) === "b100".U) { // "100x" -> q=1, r=0
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 0.U;
            fast_path_len := (1 + 1 + 1).U
        }.elsewhen(stream_top4(3, 1) === "b101".U) { // "101x" -> q=1, r=1
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 1.U;
            fast_path_len := (1 + 1 + 1).U
        }.elsewhen(stream_top4 === "b1100".U) { // q=2, r=0
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 0.U;
            fast_path_len := (2 + 1 + 1).U
        }.elsewhen(stream_top4 === "b1101".U) { // q=2, r=1
            fast_path_valid := true.B; fast_path_q := 2.U; fast_path_r := 1.U;
            fast_path_len := (2 + 1 + 1).U
        }
    }.elsewhen(io.k_in === 1.U) { // k=2
        // (k_val=2)
        when(stream_top4(3, 1) === "b000".U) { // "000x" -> q=0, r=0
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 0.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top4(3, 1) === "b001".U) { // "001x" -> q=0, r=1
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 1.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top4(3, 1) === "b010".U) { // "010x" -> q=0, r=2
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 2.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top4(3, 1) === "b011".U) { // "011x" -> q=0, r=3
            fast_path_valid := true.B; fast_path_q := 0.U; fast_path_r := 3.U;
            fast_path_len := (0 + 1 + 2).U
        }.elsewhen(stream_top4 === "b1000".U) { // q=1, r=0
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 0.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top4 === "b1001".U) { // q=1, r=1
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 1.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top4 === "b1010".U) { // q=1, r=2
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 2.U;
            fast_path_len := (1 + 1 + 2).U
        }.elsewhen(stream_top4 === "b1011".U) { // q=1, r=3
            fast_path_valid := true.B; fast_path_q := 1.U; fast_path_r := 3.U;
            fast_path_len := (1 + 1 + 2).U
        }
    }

    //  3. Slow Path (Counter)
    val slow_path_q = WireDefault(0.U(p.qValWidth.W))
    val slow_path_r = WireDefault(0.U(p.rValWidth.W))
    val slow_path_len = WireDefault(0.U(p.lengthWidth.W))
    val slow_path_error = WireDefault(false.B)

    when(!fast_path_valid) {
        // 1. 解码 q (商)
        // 根据数据, q_max=15, 所以我们只需要看16比特 (q=15个'1' + 1个'0')
        val q_stream_width = p.qMax + 1 // 16 bits
        val q_stream = shifted_window(
          p.peekWindowWidth - 1,
          p.peekWindowWidth - q_stream_width
        )
        // 对流取反: "1110..." 变为 "0001..."
        // // (q_stream_width - 1, 0) 用于确保我们只操作这16位
        val inverted_q_stream = (~q_stream)(q_stream_width - 1, 0)
        // // 检查错误: 如果取反后全是'0', 说明q > qMax
        val q_is_valid = (inverted_q_stream =/= 0.U)
        // // 计算前导零的数量, 这正好等于 q
        val inverted_vec = inverted_q_stream.asBools
        // // 使用 PriorityEncoder 找到第一个 'T' 的索引
        val q = PriorityEncoder(inverted_vec.reverse)

        // *** [DEBUG PRINTF] ***
        // 我们在这里添加打印语句
        // p"...": 这是一个 Chisel printf, 它可以打印硬件信号
        // printf(p"--- [MicroDecoder DEBUG] ---\n")
        // printf(p"  Slow Path Activated (k_in = ${io.k_in})\n")
        // printf(p"  q_stream (raw 16b):     0b${Binary(q_stream)}\n")
        // printf(p"  inverted_q_stream (16b): 0b${Binary(inverted_q_stream)}\n")
        // printf(p"  q_is_valid (must be T): ${q_is_valid}\n")
        // printf(p"  q (from PriorityEncoder): ${q}\n") // <-- 这是关键!
        // printf(p"------------------------------\n")

        when(q_is_valid) {
            slow_path_q := q
            val q_consumed_bits = q +& 1.U
            val len = q_consumed_bits + k_val
            slow_path_len := len

            // 2. 解码 r (余数)
            val r_shifted_stream = (shifted_window << q_consumed_bits)
            when(k_val === 1.U) {
                slow_path_r := r_shifted_stream(p.peekWindowWidth - 1)
                // val r_bit = r_shifted_stream(p.peekWindowWidth - 1)
                // slow_path_r := r_bit
                // printf(
                //   p"[DEBUG]  k=1. q_consumed=${q_consumed_bits}. r_bit_to_assign=${r_bit}\n slow_path_r := ${slow_path_r}\n"
                // )
            }.otherwise { // k_val === 2.U
                slow_path_r := r_shifted_stream(
                  p.peekWindowWidth - 1,
                  p.peekWindowWidth - 2
                )
                // val r_bits = r_shifted_stream(
                //   p.peekWindowWidth - 1,
                //   p.peekWindowWidth - 2
                // )
                // slow_path_r := r_bits
                // printf(
                //   p"[DEBUG]  k=2. q_consumed=${q_consumed_bits}. r_bit_to_assign=${r_bits}\n"
                // )
            }
            // printf(p" [+] r_shifted_stream: 0b${Binary(r_shifted_stream)}\n")
            // printf(
            //   p"  [+] q_is_valid = TRUE. slow_path_r ASSIGNED: ${slow_path_r}\n"
            // )
        }.otherwise {
            slow_path_error := true.B
            printf(p" [!] q_is_valid = FALSE. ERROR SET.\n")
        }

    }.otherwise {
        slow_path_q := 0.U
        slow_path_r := 0.U
        slow_path_len := 0.U
    }

    //  4. 最终输出 Mux
    io.final_q := Mux(fast_path_valid, fast_path_q, slow_path_q)
    io.final_r := Mux(fast_path_valid, fast_path_r, slow_path_r)
    io.length := Mux(fast_path_valid, fast_path_len, slow_path_len)
    io.error := Mux(fast_path_valid, false.B, slow_path_error)
}
