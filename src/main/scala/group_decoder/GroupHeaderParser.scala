package group_decoder

import chisel3._
import chisel3.util._

case class GroupHeaderParserParams(
    kOptions: Seq[Int] = Seq(1, 2, 3) // 必须与压缩端保持一致
) {
    val nK: Int = kOptions.size
    val flagBits: Int = log2Ceil(nK + 1) // fallback 占最后一个编码
    val maxK: Int = kOptions.max
}

class GroupHeaderParserIO(p: GroupHeaderParserParams) extends Bundle {
    // 上游原始比特流(接 BitstreamReader)
    val rawBits = Flipped(Decoupled(UInt(p.flagBits.W))) // 请求式读取

    // 解码结果
    val headerValid = Output(Bool())
    val isFallback = Output(Bool())
    val k = Output(UInt(log2Ceil(p.maxK + 1).W))

    // 剩余比特流(透明转发给 GroupDecoderTop)
    val bitsOut = Decoupled(UInt(8.W)) // 8-bit 窄接口即可,可参数化
}

class GroupHeaderParser(p: GroupHeaderParserParams) extends Module {
    val io = IO(new GroupHeaderParserIO(p))

    // 请求读取 flag
    io.rawBits.ready := !io.headerValid
    io.bitsOut.valid := false.B
    io.bitsOut.bits := 0.U

    // 锁存头部
    val regValid = RegInit(false.B)
    val regIsFallback = Reg(Bool())
    val regK = Reg(UInt(log2Ceil(p.maxK + 1).W))

    io.headerValid := regValid
    io.isFallback := regIsFallback
    io.k := regK

    when(io.rawBits.valid && !regValid) {
        val flag = io.rawBits.bits
        regValid := true.B
        regIsFallback := flag === p.nK.U
        regK := Mux(flag === p.nK.U, 0.U, flag(log2Ceil(p.maxK) - 1, 0))
    }

    // 透明转发:上游 rawBits 直接接到下游 bitsOut
    // 注意:这里只是"占位",实际剩余比特流由顶层把 BitstreamReader 输出
    // 直接连到 GroupDecoderTop 的输入即可;本模块只负责解析 header,
    // 不缓存任何权重比特,因此无需真正转发数据,只需给出 header 信号.
}
