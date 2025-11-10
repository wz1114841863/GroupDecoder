package gr_accelerator.decoder

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/** GRDecoderCore 的 IO
  */
class GRDecoderCoreIO(val p: GRDecoderCoreParams) extends Bundle {
    // --- 控制端口 (来自 DecoderBank) ---
    val start = Input(Bool())
    val group_index = Input(UInt(p.metaGroupIndexWidth.W))
    val finished = Output(Bool())

    // --- 元数据 SRAM 接口 ---
    val meta_req = Output(new Bundle {
        val valid = Bool()
        val addr = UInt(p.metaGroupIndexWidth.W)
    })
    val meta_resp = Input(new Bundle {
        val valid = Bool()
        val start_byte_addr = UInt(p.streamAddrWidth.W)
        val zero_point = UInt(p.zpWidth.W)
    })

    // --- 共享内存前端 (SharedFrontend) 接口 ---
    val stream_req = Output(new Bundle {
        val valid = Bool()
        val addr = UInt(p.streamAddrWidth.W) // 字节地址
    })
    val stream_resp = Input(new Bundle {
        val valid = Bool()
        val data = UInt(p.streamFetchWidth.W) // 64-bit
    })

    // --- SRAM Bank 写入接口 ---
    val sram_write = Output(new Bundle {
        val valid = Bool()
        val addr = UInt(p.outputSramAddrWidth.W) // 0..511
        val data = UInt(p.weightWidth.W) // 4-bit
    })
}

/** GR 解码器核心模块
  */
class GRDecoderCore(val p: GRDecoderCoreParams) extends Module {
    val io = IO(new GRDecoderCoreIO(p))

    // 实例化子模块
    val gr_decoder = Module(new DecodeUnit_GR(p.grDecoderConfig))

    // FSM 状态定义
    object State extends ChiselEnum {
        val sIdle, sFetchMeta, sFetchStream, sParseFlag, sDecode_Exec,
            sDecode_Post, sDone = Value
    }
    // --- 状态机 ---
    val state = RegInit(State.sIdle)

    // --- FSM 内部寄存器 ---
    // 元数据寄存器
    val zero_point_reg = RegInit(0.U(p.zpWidth.W))
    val next_fetch_addr_reg = RegInit(0.U(p.streamAddrWidth.W))

    // 流式缓冲 寄存器
    val raw_buffer_reg = RegInit(0.U(p.internalBufferWidth.W)) // 128-bit
    val bits_valid_in_buffer = RegInit(
      0.U(p.bufferValidBitsWidth.W)
    ) // (0..128)

    // 解码循环控制
    val decoded_count = RegInit(0.U(p.groupCountWidth.W))
    val sram_write_addr = RegInit(0.U(p.outputSramAddrWidth.W))

    // 组内模式 (由 sParseFlag 设置)
    val is_fallback_reg = RegInit(false.B)
    val k_in_reg = RegInit(0.U(p.grDecoderConfig.grKInWidth.W))

    // 流水线 寄存器
    val weight_pipe_reg = RegInit(0.U(p.weightWidth.W))
    val q_pipe_reg = RegInit(0.U(p.grDecoderConfig.grQValWidth.W))
    val r_pipe_reg = RegInit(0.U(p.grDecoderConfig.grRValWidth.W))
    val consumed_bits_reg = RegInit(0.U(p.grDecoderConfig.grLengthWidth.W))
    val is_fallback_pipe_reg = RegInit(false.B)

    // --- 默认输出 ---
    io.finished := false.B
    io.meta_req.valid := false.B
    io.meta_req.addr := 0.U
    io.stream_req.valid := false.B
    io.stream_req.addr := 0.U
    io.sram_write.valid := false.B
    io.sram_write.addr := 0.U
    io.sram_write.data := 0.U

    // 从128-bit缓冲的顶端提取 20-bit 块
    val aligned_chunk_20bit = raw_buffer_reg(
      p.internalBufferWidth - 1,
      p.internalBufferWidth - p.grDecoderConfig.grChunkWidth
    )

    // 连接解码器
    gr_decoder.io.aligned_chunk := aligned_chunk_20bit
    gr_decoder.io.k_in := k_in_reg

    // Fallback 路径逻辑
    val fb_final_weight = aligned_chunk_20bit(
      p.grDecoderConfig.grChunkWidth - 1,
      p.grDecoderConfig.grChunkWidth - p.weightWidth
    )
    val fb_consumed_bits = p.weightWidth.U

    // 获取GR组合逻辑输出
    val gr_q = gr_decoder.io.final_q
    val gr_r = gr_decoder.io.final_r
    val gr_consumed_bits = gr_decoder.io.consumed_bits_gr
    val gr_error = gr_decoder.io.error

    // --- 状态机逻辑 ---
    switch(state) {
        is(State.sIdle) {
            when(io.start) {
                state := State.sFetchMeta
                // 发出元数据请求
                // io.meta_req.valid := true.B
                // io.meta_req.addr := io.group_index
            }
        }

        is(State.sFetchMeta) {
            io.meta_req.valid := true.B
            io.meta_req.addr := io.group_index

            when(io.meta_resp.valid) {
                io.meta_req.valid := false.B

                // 锁存元数据
                next_fetch_addr_reg := io.meta_resp.start_byte_addr
                zero_point_reg := io.meta_resp.zero_point

                // 复位计数器
                decoded_count := 0.U
                sram_write_addr := 0.U
                bits_valid_in_buffer := 0.U // 强制进入 sFetchStream

                state := State.sFetchStream
            }
        }

        is(State.sFetchStream) {
            // 请求下一个 64-bit 块
            io.stream_req.valid := true.B
            io.stream_req.addr := next_fetch_addr_reg

            when(io.stream_resp.valid) {
                // 收到响应,停止请求
                io.stream_req.valid := false.B

                // ================== [ 修复 2: 核心缓冲区 BUG ] ==================

                // 1. 将 64-bit 的新数据强制转换为 128-bit UInt
                //    使用 .pad() 来确保 Chisel 知道我们想要 128 位
                val new_data_128bit =
                    io.stream_resp.data.pad(p.internalBufferWidth)

                // 2. 将这个 128-bit 值左移,使其 *左对齐*
                //    (即,[NewData(64) | 0(64)])
                val new_data_padded_to_msb =
                    new_data_128bit << (p.internalBufferWidth - p.streamFetchWidth).U

                // 3. 将这个对齐的值右移,使其紧跟在已有的 "bits_valid_in_buffer" 之后
                //    raw_buffer_reg 已经是 [V(18) | 0(110)]
                //    new_data_shifted 变成 [0(18) | N(64) | 0(46)]
                val new_data_shifted =
                    new_data_padded_to_msb >> bits_valid_in_buffer

                // 4. 将其与已有的 'raw_buffer_reg' 合并
                //    结果: [V(18) | N(64) | 0(46)]
                raw_buffer_reg := raw_buffer_reg | new_data_shifted

                // ================== [ 修复结束 ] ==================

                // 更新有效比特计数器
                bits_valid_in_buffer := bits_valid_in_buffer + p.streamFetchWidth.U

                // 更新下一个 Fetch 地址
                next_fetch_addr_reg := next_fetch_addr_reg + p.streamFetchWidthBytes.U

                // printf(
                //   p"  [sFetchStream] RESP Valid. Received_data: 0x${Hexadecimal(io.stream_resp.data)}. " +
                //       p"BitsValid_Before: ${bits_valid_in_buffer}. " +
                //       p"Merged_raw_buffer: 0x${Hexadecimal(raw_buffer_reg | new_data_shifted)}\n"
                // )

                // 判断下一步
                when(decoded_count === 0.U) {
                    state := State.sParseFlag
                }.otherwise {
                    state := State.sDecode_Exec
                }
            }
        }

        is(State.sParseFlag) {
            // FSM 负责解析 flag
            val flag_bits = p.grDecoderConfig.grKInWidth + 1 // 2 bits
            // 从128-bit缓冲顶部读取
            val flag = raw_buffer_reg(
              p.internalBufferWidth - 1,
              p.internalBufferWidth - flag_bits
            )

            val flag_k1 = 0.U
            val flag_k2 = 1.U
            val flag_fallback = 2.U

            when(flag === flag_fallback) {
                is_fallback_reg := true.B
            }.otherwise {
                is_fallback_reg := false.B
                k_in_reg := flag // 0->k1, 1->k2
            }

            // 更新 FSM 状态 (时序)
            // (通过移位 消耗 flag bits)
            raw_buffer_reg := raw_buffer_reg << flag_bits.U
            bits_valid_in_buffer := bits_valid_in_buffer - flag_bits.U

            state := State.sDecode_Exec
        }

        is(State.sDecode_Exec) {
            when(decoded_count === p.groupSize.U) {
                // 全部解码完成
                state := State.sDone
            }.elsewhen(
              // 需要新数据
              bits_valid_in_buffer < p.grDecoderConfig.grChunkWidth.U
            ) {
                state := State.sFetchStream
            }.elsewhen(!is_fallback_reg && gr_error) {
                // 错误处理
                state := State.sDone // 错误,停止
            }.otherwise {
                // printf(
                //   p"  [sDecode_Exec] CHUNK_IN: 0x${Hexadecimal(aligned_chunk_20bit)} (k_in: ${k_in_reg}). " +
                //       p"GR_OUT -> q: ${gr_q}, r: ${gr_r}, consumed: ${gr_consumed_bits}. " +
                //       p"Fallback: ${is_fallback_reg}\n"
                // )
                // 执行解码
                state := State.sDecode_Post
                // 锁存流水线 寄存器
                is_fallback_pipe_reg := is_fallback_reg
                when(is_fallback_reg) {
                    // 锁存 Fallback 路径
                    weight_pipe_reg := fb_final_weight
                    consumed_bits_reg := fb_consumed_bits
                    q_pipe_reg := 0.U
                    r_pipe_reg := 0.U
                }.otherwise {
                    // 锁存 q 和 r
                    q_pipe_reg := gr_q
                    r_pipe_reg := gr_r
                    consumed_bits_reg := gr_consumed_bits
                }
            }
        }

        is(State.sDecode_Post) {
            state := State.sDecode_Exec
            // val final_weight = Wire(UInt(p.weightWidth.W))
            when(is_fallback_pipe_reg) {
                io.sram_write.data := weight_pipe_reg
            }.otherwise {
                //  拼接
                val k_val = k_in_reg + 1.U
                val mapped_delta = (q_pipe_reg << k_val) | r_pipe_reg

                //  反映射
                val is_even = (mapped_delta(0) === 0.U)
                val signed_delta = Mux(
                  is_even,
                  (mapped_delta >> 1.U).asSInt,
                  -((mapped_delta + 1.U) >> 1.U).asSInt
                )
                //  加零点
                val final_weight_calc =
                    (signed_delta + zero_point_reg.zext.asSInt).asUInt

                // 直接赋值
                io.sram_write.data := final_weight_calc

                // printf(
                //   p"  [sDecode_Post] PIPE -> q: ${q_pipe_reg}, r: ${r_pipe_reg}, k: ${k_in_reg + 1.U}. " +
                //       p"Fallback_Pipe: ${is_fallback_pipe_reg}. " +
                //       p"FINAL_WEIGHT: ${final_weight_calc}\n" +
                //       p"signed_delta: ${signed_delta}, zero_point: ${zero_point_reg}\n"
                // )
            }

            // 写入SRAM
            io.sram_write.valid := true.B
            io.sram_write.addr := sram_write_addr
            // io.sram_write.data := final_weight

            // 更新 FSM
            decoded_count := decoded_count + 1.U
            sram_write_addr := sram_write_addr + 1.U

            // 消耗 128-bit 缓冲
            raw_buffer_reg := raw_buffer_reg << consumed_bits_reg
            bits_valid_in_buffer := bits_valid_in_buffer - consumed_bits_reg
        }

        is(State.sDone) {
            // printf(
            //   p"  [sDone] Decoding finished. Total decoded: ${decoded_count} weights.\n"
            // )
            io.finished := true.B
            when(!io.start) {
                state := State.sIdle
            }
        }
    }

    // 添加一个周期计数器
    val cycle_count_reg = RegInit(0.U(32.W))
    cycle_count_reg := cycle_count_reg + 1.U

    // 在每个周期打印 FSM 状态 和 关键 IO
    // printf(
    //   p"[DUT C=${cycle_count_reg}] State: ${state} | " +
    //       p"Decoded: ${decoded_count} (of ${p.groupSize.U}) | " +
    //       p"BufValidBits: ${bits_valid_in_buffer} | " +
    //       // --- 关键 IO 握手 ---
    //       p"meta_REQ: ${io.meta_req.valid} | " +
    //       p"meta_RESP: ${io.meta_resp.valid} | " +
    //       p"stream_REQ: ${io.stream_req.valid} | " +
    //       p"stream_RESP: ${io.stream_resp.valid} | " +
    //       p"sram_WRITE: ${io.sram_write.valid} | " +
    //       p"FINISHED: ${io.finished}\n"
    // )
}
