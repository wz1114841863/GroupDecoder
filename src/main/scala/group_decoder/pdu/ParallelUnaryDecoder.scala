package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._
import group_decoder.common._
/** ParallelUnaryDecoder的IO接口
  * @param p
  *   ParallelUnaryDecoder的配置参数
  */
class ParallelUnaryDecoderIO(p: ParallelUnaryDecoderParams) extends Bundle {
    // 从输入流移位器接收的数据窥视窗口
    val peek_window = Input(UInt(p.peekWindowWidth.W))
    // 输出每个段的本地解码结果向量
    val result_vec = Output(Vec(p.segmentCount, new SegmentDecodeBundle(p)))
}

/** ParallelUnaryDecoder模块
  * @param p
  *   ParallelUnaryDecoder的配置参数
  *
  * @functionality
  *   并行解码输入的宽位宽窥视窗口, 将其划分为多个段, 并对每个段进行一元编码解码. 每个段独立处理, 检测连续的'1'的数量, 并输出本地解码结果.
  */

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
