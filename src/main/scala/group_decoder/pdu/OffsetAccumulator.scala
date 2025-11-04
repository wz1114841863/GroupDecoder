package group_decoder.pdu

import chisel3._
import chisel3.util._
import group_decoder.common._

/** OffsetAccumulator的IO接口
  * @param p
  *   PDU的全局配置参数
  * @param pudp
  *   ParallelUnaryDecoder的配置参数
  */
class OffsetAccumulatorIO(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Bundle {
    // 从ParallelUnaryDecoder接收的向量
    // 弃用, ParallelUnaryDecoder模块目前充当流水线延迟的角色, 不使用其输出
    // val segment_results = Input(
    //   Vec(pudp.segmentCount, new SegmentDecodeBundle(pudp))
    // )
    // 从PDU主控制器接收的原始数据窥视窗口
    val peek_window = Input(UInt(pudp.peekWindowWidth.W))
    // 从PDU主控制器接收的当前组的k值
    val k = Input(UInt(2.W))

    // --- 输出 ---
    // N个并行计算出的最终q值
    val final_q_vec = Output(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    )
    // N个权重各自的比特长度
    val length_vec = Output(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 3 + 1).W))
    ) // q+1+k_max
    // N个权重在本周期窥视窗口内的相对起始偏移
    val offset_vec = Output(
      Vec(p.unrollFactor, UInt(log2Ceil(pudp.peekWindowWidth).W))
    )
    // N个权重在本周期消耗的总比特数 (反馈给输入流移位器)
    val total_consumed_bits = Output(
      UInt(log2Ceil(p.unrollFactor * (p.maxQuotient + 3) + 1).W)
    )
    // 异常/错误信号
    val error = Output(Bool())
}

class OffsetAccumulator(p: PDUParams, pudp: ParallelUnaryDecoderParams)
    extends Module {
    val io = IO(new OffsetAccumulatorIO(p, pudp))

    // current_bit_ptr(i) 代表第i个权重在peek_window内的起始比特位置
    // 多出来的一个是为了方便计算总消耗比特数
    val current_bit_ptr = Wire(
      Vec(p.unrollFactor + 1, UInt(log2Ceil(pudp.peekWindowWidth + 1).W))
    )
    val final_q_wires = Wire(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 1).W))
    )
    val length_wires = Wire(
      Vec(p.unrollFactor, UInt(log2Ceil(p.maxQuotient + 3 + 1).W))
    )
    val error_wires = Wire(Vec(p.unrollFactor, Bool()))

    // 初始化:第一个权重总是从窥视窗口的第0个比特开始
    current_bit_ptr(0) := 0.U
    // 并行生成N个权重的计算逻辑
    for (i <- 0 until p.unrollFactor) {
        // -----------------------------------------------------------------
        // 阶段A: 为第 i 个权重计算 final_q
        // -----------------------------------------------------------------

        // 1. "调整望远镜": 逻辑上创建一个新的数据窗口, 其开头对齐第i个权重的起始比特
        val shifted_window = io.peek_window << current_bit_ptr(i)
        val segments_after_shift = shifted_window.asTypeOf(
          Vec(pudp.segmentCount, UInt(pudp.segmentWidth.W))
        )

        // 2. "重新侦察": 对这个移位后的新窗口进行本地分析
        val reanalyzed_results = Wire(
          Vec(pudp.segmentCount, new SegmentDecodeBundle(pudp))
        )
        for (j <- 0 until pudp.segmentCount) {
            // 计算第j个段(从左边数)的比特范围
            val high = pudp.peekWindowWidth - 1 - (j * pudp.segmentWidth)
            val low = pudp.peekWindowWidth - ((j + 1) * pudp.segmentWidth)

            val current_segment = shifted_window(high, low)
            val inverted_segment = ~current_segment
            val is_all_ones = current_segment.andR
            reanalyzed_results(j).is_all_ones := is_all_ones
            val first_zero_index = PriorityEncoder(
              Reverse(inverted_segment).asBools
            )
            reanalyzed_results(j).local_q := Mux(
              is_all_ones,
              pudp.segmentWidth.U,
              first_zero_index
            )
        }
        // val reanalyzed_results = io.segment_results
        // 3. "逻辑链条": 从新的侦察报告中, 链接计算出完整的q值
        val q_chain = Wire(Vec(pudp.segmentCount + 1, UInt()))
        val still_searching_chain = Wire(Vec(pudp.segmentCount + 1, Bool()))

        q_chain(0) := 0.U
        still_searching_chain(0) := true.B

        for (j <- 0 until pudp.segmentCount) {
            when(still_searching_chain(j)) {
                q_chain(j + 1) := q_chain(j) + reanalyzed_results(j).local_q
                still_searching_chain(j + 1) := reanalyzed_results(
                  j
                ).is_all_ones
            }.otherwise {
                q_chain(j + 1) := q_chain(j)
                still_searching_chain(j + 1) := false.B
            }
        }

        // 第i个权重的最终q值是链条的最后一个结果
        val q_i = q_chain(pudp.segmentCount)
        final_q_wires(i) := q_i

        // -----------------------------------------------------------------
        // 阶段B: 计算 length_i, error_i, 和下一个指针
        // -----------------------------------------------------------------

        // 检查异常
        error_wires(i) := q_i > p.maxQuotient.U

        // 计算权重总长度
        val k_val = io.k + 1.U
        length_wires(i) := q_i + 1.U + k_val

        // 更新下一个权重的起始指针, 形成数据依赖链
        current_bit_ptr(i + 1) := current_bit_ptr(i) + length_wires(i)
    }

    // --- 前缀和计算与最终输出连接 ---

    // 1. 使用scanLeft优雅地生成前缀和树, 计算offset向量
    val offsets = length_wires.scanLeft(0.U)(_ + _).slice(0, p.unrollFactor)

    // 2. 连接到IO端口
    io.final_q_vec := final_q_wires
    io.length_vec := length_wires
    io.error := error_wires.reduce(_ || _)
    io.offset_vec := VecInit(offsets)

    // total_consumed_bits是最后一个指针的值
    io.total_consumed_bits := current_bit_ptr(p.unrollFactor)
}
