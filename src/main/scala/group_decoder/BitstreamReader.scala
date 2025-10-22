package group_decoder

import chisel3._
import chisel3.util._

/** BitstreamReader Parameters 模块的参数化配置
  * @param streamWidth
  *   输入比特流的宽度
  * @param bufferWidth
  *   内部缓冲区的宽度
  */
case class BitstreamReaderParams(
    streamWidth: Int = 32,
    bufferWidth: Int = 64
) {
    require(
      bufferWidth >= streamWidth,
      "bufferWidth must be greater than or equal to streamWidth"
    )
    require(
      bufferWidth % streamWidth == 0,
      "bufferWidth must be a multiple of streamWidth"
    )
}

// BitstreamReader IO 接口定义
class BitstreamReaderIO(params: BitstreamReaderParams) extends Bundle {
    // 输入数据流
    val inputStream = Flipped(Decoupled(UInt(params.streamWidth.W)))
    // 来自FSM的读取请求
    val request = Flipped(Valid(UInt(log2Ceil(params.streamWidth + 1).W)))
    // 输出至FSM的数据
    val bits_out = Valid(UInt(params.streamWidth.W))
}

// BitstreamReader 模块定义
class BitstreamReader(params: BitstreamReaderParams) extends Module {
    val io = IO(new BitstreamReaderIO(params))

    // 核心缓冲寄存器, 用于拼接和移位
    val shifter = RegInit(0.U(params.bufferWidth.W))
    // 记录缓存寄存器中有多少有效的比特
    val validBits = RegInit(0.U(log2Ceil(params.bufferWidth + 1).W))

    // 默认输出
    io.bits_out.valid := false.B
    io.bits_out.bits := 0.U
    io.inputStream.ready := false.B

    // FSM请求读取的比特数
    val requestedBits = io.request.bits
    // 判断当前是否有足够的比特来满足FSM的请求
    val hasEnoughBits = validBits >= requestedBits

    // 状态机1: 数据填充逻辑, 当缓冲区中的有效比特数不足时, 从输入流中读取数据
    val needsData = validBits < params.streamWidth.U
    io.inputStream.ready := needsData

    when(io.inputStream.fire) {
        // 将新数据拼接到缓冲寄存器的低位
        shifter := (shifter << params.streamWidth) | io.inputStream.bits
        validBits := validBits + params.streamWidth.U
    }

    // 状态机2: 数据供给逻辑
    // 当FSM发出请求, 并且我们有足够的比特时, 执行读取操作
    when(io.request.valid && hasEnoughBits) {
        io.bits_out.valid := true.B

        // 从shifter的最高位部分取出所需的比特
        val result = shifter >> (validBits - requestedBits)
        io.bits_out.bits := result(params.streamWidth - 1, 0)

        // 更新shifter和validBits
        // 注意, 更新时要考虑有数据输入的情况
        val nextValidBits = validBits - requestedBits
        val nextShifter = shifter << requestedBits

        when(io.inputStream.fire) { // 如果在消耗比特的同时,又填充了新数据
            validBits := nextValidBits + params.streamWidth.U
            shifter := (nextShifter << params.streamWidth) | io.inputStream.bits
        }.otherwise { // 仅消耗比特
            validBits := nextValidBits
            shifter := nextShifter
        }
    }.elsewhen(io.request.valid && !hasEnoughBits) {
        // 如果请求有效但没有足够的比特, 则不输出数据
        io.bits_out.valid := false.B
    }
}
