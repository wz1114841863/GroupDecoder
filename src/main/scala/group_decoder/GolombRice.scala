package group_decoder

import chisel3._
import chisel3.util._
import group_decoder.GroupDecoderParams

// Golomb-Rice解码核心模块
class GRCoreIO(param: GroupDecoderParams) extends Bundle {
    // 控制信号
    val start = Input(Bool())
    val k = Input(UInt(2.W)) // 0 -> k=1, 1 -> k=2, 2 -> k=3
    val done = Output(Bool())

    // 与BitstreamReader交互的接口
    val reader_req = Valid(UInt(log2Ceil(param.streamWidth + 1).W))
    val reader_resp = Flipped(Valid(UInt(param.streamWidth.W)))

    // 输出
    val decoded_val = Output(UInt(16.W)) // TODO: 输出解码后的无符号增量, 宽度可能需要改一下
}

class GRCore(param: GroupDecoderParams) extends Module {
    val io = IO(new GRCoreIO(param))
    // 内部状态定义
    object State extends ChiselEnum {
        val sIDLE, sDECODE_Q, sDECODE_R, sDONE = Value
    }
    val state = RegInit(State.sIDLE)

    // 内部寄存器
    val qCounter = RegInit(0.U(8.W)) // 商计数器
    val rValue = RegInit(0.U(16.W)) // 余数
    val k_reg = RegInit(0.U(2.W)) // k值寄存器

    // 默认输出
    io.done := false.B
    io.reader_req.valid := false.B
    io.reader_req.bits := 0.U
    io.decoded_val := 0.U

    // 状态机逻辑
    switch(state) {
        is(State.sIDLE) {
            when(io.start) {
                state := State.sDECODE_Q
                qCounter := 0.U // 重置商计数器
                k_reg := io.k // 锁存k值
            }
        }

        is(State.sDECODE_Q) {
            // 请求1个比特来解码商
            io.reader_req.valid := true.B
            io.reader_req.bits := 1.U

            when(io.reader_resp.valid) { // BitstreamReader返回了1个比特
                when(io.reader_resp.bits === 1.U) { // 读到'1'
                    qCounter := qCounter + 1.U
                    // 保持在 sDECODE_Q 状态继续读下一位
                }.otherwise { // 读到'0',商解码结束
                    state := State.sDECODE_R
                }
            }
        }

        is(State.sDECODE_R) {
            val k_val = k_reg + 1.U // 实际的k值是输入(0/1/2) + 1
            // 请求k个比特来解码余数
            io.reader_req.valid := true.B
            io.reader_req.bits := k_val

            when(io.reader_resp.valid) { // BitstreamReader返回了k个比特
                rValue := io.reader_resp.bits
                state := State.sDONE
            }
        }

        is(State.sDONE) {
            io.done := true.B
            state := State.sIDLE
        }
    }

    // 组合逻辑计算最终输出值
    io.decoded_val := (qCounter << (k_reg + 1.U)) | rValue
}
