package group_decoder

import chisel3._
import chisel3.util._

/** GroupDecoder Parameters 模块的参数化配置
  * @param groupSize
  *   解码组的权重数量, 例如512
  * @param streamWidth
  *   输入比特流的宽度, 必须是streamWidth的整数倍, 例如32
  */
case class GroupDecoderParams(groupSize: Int, streamWidth: Int)

// GroupDecoder IO 接口定义
class GroupDecoderIO(params: GroupDecoderParams) extends Bundle {
    // 控制信号
    val start = Input(Bool())
    val done = Output(Bool())

    // 输入比特流
    val compressedStream = Flipped(Decoupled(UInt(params.streamWidth.W)))
    val zp = Input(UInt(4.W))
    val best_k = Input(UInt(2.W)) // 0 -> k=1, 1 -> k=2, 2 -> k=3
    val isFallback = Input(Bool())

    // 输出解码结果
    val decodedWeights = Decoupled(UInt(4.W))
}

// GroupDecoder 模块定义
class GroupDecoder(params: GroupDecoderParams) extends Module {
    val io = IO(new GroupDecoderIO(params))

    val bitstreamReader = Module(
      new BitstreamReader(
        BitstreamReaderParams(params.streamWidth, params.streamWidth * 2)
      )
    )

    val grCore = Module(
      new GRCore(GroupDecoderParams(params.groupSize, params.streamWidth))
    )

    val deltaReconstructor = Module(new DeltaReconstructor)

    // FSM 状态定义
    object State extends ChiselEnum {
        val sIDLE, sDECODE, sFALLBACK, sDONE = Value
    }
    val state = RegInit(State.sIDLE)

    // 输出权重计数器
    val outputCounter = RegInit(0.U(log2Ceil(params.groupSize + 1).W))
    val outputCounterIsFull = outputCounter === params.groupSize.U

    // 默认输出
    io.done := false.B
    io.decodedWeights.bits := 0.U
    io.decodedWeights.valid := false.B
    io.compressedStream.ready := false.B

    // 状态机逻辑
    switch(state) {
        is(State.sIDLE) {
            when(io.start) {
                outputCounter := 0.U
                when(io.isFallback) {
                    state := State.sFALLBACK
                }.otherwise {
                    state := State.sDECODE
                    grCore.io.start := true.B // 启动 Golomb-Rice 解码
                }
            }
        }

        is(State.sDECODE) {
            // 核心解码循环: 当GRCore完成一个值的解码, 并且下游可以接受我们的输出时
            when(grCore.io.done && io.decodedWeights.ready) {
                outputCounter := outputCounter + 1.U
                when(outputCounterIsFull) {
                    state := State.sDONE
                }.otherwise {
                    grCore.io.start := true.B // 启动下一个解码
                }
            }
        }

        is(State.sFALLBACK) {
            // 在回退模式下,我们直接向BitstreamReader请求4个比特
            bitstreamReader.io.request.valid := true.B
            bitstreamReader.io.request.bits := 4.U

            // 当BitstreamReader准备好数据并且下游可以接受时
            when(bitstreamReader.io.request.valid && io.decodedWeights.ready) {
                outputCounter := outputCounter + 1.U // 输出计数器+1

                when(outputCounterIsFull) {
                    state := State.sDONE
                }
            }
        }

        is(State.sDONE) {
            io.done := true.B
            when(!io.start) {
                state := State.sIDLE
            }
        }
    }

    // 数据通路连接
    bitstreamReader.io.inputStream <> io.compressedStream
    grCore.io.k := io.best_k

    // 连接 BitstreamReader <-> GR_Core
    // GR_Core的请求直接传给BitstreamReader
    when(state === State.sDECODE) {
        bitstreamReader.io.request <> grCore.io.reader_req
    }
    grCore.io.reader_resp <> bitstreamReader.io.bits_out

    // 连接 GR_Core -> DeltaReconstructor
    deltaReconstructor.io.unsigned_delta := grCore.io.decoded_val
    deltaReconstructor.io.zp := io.zp

    // 连接输出通路
    io.decodedWeights.valid := false.B
    io.decodedWeights.bits := 0.U

    when(state === State.sDECODE) {
        // 解码模式下,输出有效性由grCore.done决定
        io.decodedWeights.valid := grCore.io.done
        io.decodedWeights.bits := deltaReconstructor.io.quantized_weight
    }.elsewhen(state === State.sFALLBACK) {
        // 回退模式下,输出有效性由bitstreamReader.bits_out.valid决定
        io.decodedWeights.valid := bitstreamReader.io.bits_out.valid
        io.decodedWeights.bits := bitstreamReader.io.bits_out.bits
    }
}
