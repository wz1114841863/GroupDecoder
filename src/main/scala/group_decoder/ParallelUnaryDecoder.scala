package group_decoder

import chisel3._
import chisel3.util._

/** ParallelUnaryDecoder的特有参数
  * @param peekWindowWidth
  *   "窥视窗口"的总位宽, e.g., 256.
  * @param segmentWidth
  *   每个处理段的位宽, e.g., 32.
  */
case class ParallelUnaryDecoderParams(
    peekWindowWidth: Int = 256,
    segmentWidth: Int = 32
) {
    require(
      peekWindowWidth % segmentWidth == 0,
      "peekWindowWidth必须是segmentWidth的整数倍"
    )
    val segmentCount = peekWindowWidth / segmentWidth
}

/** 单个处理段的输出结果
  */
class SegmentDecodeBundle(p: ParallelUnaryDecoderParams) extends Bundle {
    // 在本段内找到的连续'1'的数量
    val local_q = UInt(log2Ceil(p.segmentWidth + 1).W)
    // 整个段是否全为'1'
    val is_all_ones = Bool()
}

/** ParallelUnaryDecoder的IO接口
  */
class ParallelUnaryDecoderIO(p: ParallelUnaryDecoderParams) extends Bundle {
    // 从输入流移位器接收的数据窥视窗口
    val peek_window = Input(UInt(p.peekWindowWidth.W))
    // 输出每个段的本地解码结果向量
    val result_vec = Output(Vec(p.segmentCount, new SegmentDecodeBundle(p)))
}

class ParallelUnaryDecoder(p: ParallelUnaryDecoderParams) extends Module {
    val io = IO(new ParallelUnaryDecoderIO(p))

    // 将输入的宽位宽窗口按segmentWidth分割成一个Chisel的Vec[UInt]
    val segments =
        io.peek_window.asTypeOf(Vec(p.segmentCount, UInt(p.segmentWidth.W)))

    // 并行处理每一个段
    for (i <- 0 until p.segmentCount) {
        val current_segment = segments(i)
        val inverted_segment = ~current_segment
        // 检查整个段是否全为'1'
        val is_all_ones =
            current_segment.andR // .andR 是一个Chisel操作, 当所有位都为1时返回true

        // 使用优先级编码器思想来计算local_q.
        // Chisel的OHToUInt和Reverse组合是实现此功能的标准技巧.
        // 1. 对段按位取反 (~), '1110...' 变为 '0001...'
        // 2. 找到第一个'1'的位置, 这是一个独热码(One-Hot)
        // 3. 将独热码转换为二进制数, 这个数就是第一个'0'的索引, 即local_q
        // val first_zero_index = OHToUInt(Reverse(Cat(~current_segment, 1.U)))
        val first_zero_index = PriorityEncoder(
          Reverse(inverted_segment).asBools
        )

        // 如果整个段都是'1', 那么local_q就是段的宽度; 否则, 就是第一个'0'的索引
        io.result_vec(i).local_q := Mux(
          is_all_ones,
          p.segmentWidth.U,
          first_zero_index
        )
        io.result_vec(i).is_all_ones := is_all_ones
    }
}
