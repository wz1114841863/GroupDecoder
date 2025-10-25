package group_decoder

import chisel3._
import chisel3.util._

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

class BitstreamReaderIO(params: BitstreamReaderParams) extends Bundle {
    val inputStream = Flipped(Decoupled(UInt(params.streamWidth.W)))
    val request = Flipped(Valid(UInt(log2Ceil(params.streamWidth + 1).W)))
    val bits_out = Decoupled(UInt(params.streamWidth.W))
}

class BitstreamReader(params: BitstreamReaderParams) extends Module {
    val io = IO(new BitstreamReaderIO(params))

    val shifter = RegInit(0.U(params.bufferWidth.W))
    val validBits = RegInit(0.U(log2Ceil(params.bufferWidth + 1).W))

    // --- 1. 数据填充逻辑 ---
    // 只有当缓冲区能容纳下一个完整的数据块时,才准备好接收
    io.inputStream.ready := validBits <= (params.bufferWidth.U - params.streamWidth.U)
    val willAcceptData = io.inputStream.fire // .fire 是 valid && ready 的简写

    // --- 2. 请求服务逻辑 ---
    val requestedBits = io.request.bits
    val hasEnoughBits = validBits >= requestedBits

    // 我什么时候能提供有效输出?当我收到了一个有效的请求,并且我内部有足够的数据时.
    val canServiceRequest = io.request.valid && hasEnoughBits
    io.bits_out.valid := canServiceRequest
    io.bits_out.bits := (shifter >> (validBits - requestedBits))

    // --- 3. 状态更新逻辑 (最关键的修复) ---
    // 我什么时候消耗内部数据?当我和下游成功握手时 (io.bits_out.fire)
    val willConsumeData = io.bits_out.fire

    // 根据本周期是否接收/消耗数据来计算下一个周期的状态
    val bitsConsumed = Mux(willConsumeData, requestedBits, 0.U)
    val bitsAdded = Mux(willAcceptData, params.streamWidth.U, 0.U)

    // 下一个周期的 validBits 值
    validBits := validBits - bitsConsumed + bitsAdded

    // 下一个周期的 shifter 值
    // 逻辑:先通过掩码移除消耗掉的高位比特,再为新来的比特腾出空间并拼接
    val shifterAfterConsumption =
        shifter & ((1.U << (validBits - bitsConsumed)) - 1.U)
    val shifterAfterFill = (shifterAfterConsumption << bitsAdded) | Mux(
      willAcceptData,
      io.inputStream.bits,
      0.U
    )

    // 只有在数据发生变化时才更新寄存器,可以节省功耗
    when(willConsumeData || willAcceptData) {
        shifter := shifterAfterFill
    }
}
