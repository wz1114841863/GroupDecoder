package gr_accelerator.decoder

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/** GRDecoderCore模块IO端口实现
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
class GRDecoderCore(val p: GRDecoderCoreParams, val coreId: UInt)
    extends Module {
    val io = IO(new GRDecoderCoreIO(p))

    // 实例化子模块
    val gr_decoder = Module(new DecodeUnitGR(p.grDecoderConfig))

    // FSM 状态定义
    // 在 1-Cycle模式下, sDecode_Post不会被用到
    object State extends ChiselEnum {
        val sIdle, sFetchMeta, sFetchStream, sParseFlag, sDecode_Exec,
            sDecode_Post, sDone = Value
    }
    val state = RegInit(State.sIdle)

    // --- 寄存器定义 ---
    val zero_point_reg = RegInit(0.U(p.zpWidth.W))
    val next_fetch_addr_reg = RegInit(0.U(p.streamAddrWidth.W))
    val initial_byte_offset = RegInit(0.U(3.W))
    val is_first_fetch = RegInit(false.B)
    val flag_parsed = RegInit(false.B)

    val raw_buffer_reg = RegInit(0.U(p.internalBufferWidth.W))
    val bits_valid_in_buffer = RegInit(0.U(p.bufferValidBitsWidth.W))

    val decoded_count = RegInit(0.U(p.groupCountWidth.W))
    val sram_write_addr = RegInit(0.U(p.outputSramAddrWidth.W))

    val is_fallback_reg = RegInit(false.B)
    val k_in_reg = RegInit(0.U(p.grDecoderConfig.grKInWidth.W))

    // --- 流水线与输出控制寄存器 ---
    // pipe_valid 用于 1-Cycle 模式下指示是否有有效数据要写 SRAM
    val pipe_valid = RegInit(false.B)

    // 数据管道 (无论是 1-Cycle 还是 2-Cycle 模式都用这组寄存器传递给写逻辑)
    val weight_pipe_reg = RegInit(0.U(p.weightWidth.W))
    val q_pipe_reg = RegInit(0.U(p.grDecoderConfig.grQValWidth.W))
    val r_pipe_reg = RegInit(0.U(p.grDecoderConfig.grRValWidth.W))
    val consumed_bits_reg = RegInit(0.U(p.grDecoderConfig.grLengthWidth.W))
    val is_fallback_pipe_reg = RegInit(false.B)
    val k_in_pipe_reg = RegInit(0.U(p.grDecoderConfig.grKInWidth.W))
    val zero_point_pipe_reg = RegInit(0.U(p.zpWidth.W))

    // --- 默认输出 ---
    io.finished := false.B
    io.meta_req.valid := false.B
    io.meta_req.addr := 0.U
    io.stream_req.valid := false.B
    io.stream_req.addr := 0.U
    io.sram_write.valid := false.B
    io.sram_write.addr := 0.U
    io.sram_write.data := 0.U

    // --- 解码器连接 ---
    val aligned_chunk_top = raw_buffer_reg(
      p.internalBufferWidth - 1,
      p.internalBufferWidth - p.grDecoderConfig.grChunkWidth
    )
    gr_decoder.io.aligned_chunk := aligned_chunk_top
    gr_decoder.io.k_in := k_in_reg

    val fb_final_weight = aligned_chunk_top(
      p.grDecoderConfig.grChunkWidth - 1,
      p.grDecoderConfig.grChunkWidth - p.weightWidth
    )
    val fb_consumed_bits = p.weightWidth.U

    val gr_q = gr_decoder.io.final_q
    val gr_r = gr_decoder.io.final_r
    val gr_consumed_bits = gr_decoder.io.consumed_bits_gr
    val gr_error = gr_decoder.io.error

    // ==============================================================================
    // 共享 SRAM 写逻辑 (Writeback Stage)
    // ==============================================================================
    // 无论是 1-Cycle (由 pipe_valid 触发) 还是 2-Cycle (由 sDecode_Post 触发)
    // 最终都会通过这里将数据写入 SRAM

    // 定义一个写使能信号
    val do_sram_write = WireDefault(false.B)

    when(do_sram_write) {
        io.sram_write.valid := true.B
        io.sram_write.addr := sram_write_addr

        when(is_fallback_pipe_reg) {
            io.sram_write.data := weight_pipe_reg
        }.otherwise {
            // 拼接
            val k_val = k_in_pipe_reg + 1.U(2.W)
            val mapped_delta = (q_pipe_reg << k_val) | r_pipe_reg

            // 反映射
            val is_even = (mapped_delta(0) === 0.U)
            val signed_delta = Mux(
              is_even,
              (mapped_delta >> 1.U).asSInt,
              -((mapped_delta + 1.U) >> 1.U).asSInt
            )
            // 加零点
            val final_weight_calc =
                (signed_delta + zero_point_pipe_reg.zext.asSInt).asUInt
            io.sram_write.data := final_weight_calc
        }

        // 写入发生后,写地址自增
        sram_write_addr := sram_write_addr + 1.U

        // 如果是 1-Cycle 模式,pipe_valid 是一次性的脉冲,写完即复位
        if (p.useSingleCycleLoop) {
            pipe_valid := false.B
        }
    }

    // ==============================================================================
    // 状态机逻辑
    // ==============================================================================

    // ==============================================================================
    // 状态机逻辑 (修复版)
    // ==============================================================================

    switch(state) {
        is(State.sIdle) {
            when(io.start) { state := State.sFetchMeta }
        }

        is(State.sFetchMeta) {
            io.meta_req.valid := true.B
            io.meta_req.addr := io.group_index

            when(io.meta_resp.valid) {
                val raw_addr = io.meta_resp.start_byte_addr
                initial_byte_offset := raw_addr(2, 0)
                next_fetch_addr_reg := raw_addr & (~7.U(p.streamAddrWidth.W))
                zero_point_reg := io.meta_resp.zero_point

                decoded_count := 0.U
                sram_write_addr := 0.U
                raw_buffer_reg := 0.U
                bits_valid_in_buffer := 0.U
                is_first_fetch := true.B
                flag_parsed := false.B

                state := State.sFetchStream
            }
        }

        is(State.sFetchStream) {
            io.stream_req.valid := true.B
            io.stream_req.addr := next_fetch_addr_reg

            when(io.stream_resp.valid) {
                val raw_data_64 = io.stream_resp.data
                // Byte Swap (LE -> MSB Stream)
                val bytes = VecInit(
                  Seq.tabulate(8)(i => raw_data_64((i + 1) * 8 - 1, i * 8))
                )
                val reversed_data = Cat(bytes)

                val shift_bits =
                    Mux(is_first_fetch, initial_byte_offset << 3, 0.U)
                val adjusted_data = reversed_data << shift_bits

                val valid_bits_in_chunk = 64.U - shift_bits

                val data_at_buffer_msb = adjusted_data.asUInt.pad(
                  p.internalBufferWidth
                ) << (p.internalBufferWidth - 64)
                val data_positioned = data_at_buffer_msb >> bits_valid_in_buffer

                raw_buffer_reg := raw_buffer_reg | data_positioned
                bits_valid_in_buffer := bits_valid_in_buffer + valid_bits_in_chunk

                next_fetch_addr_reg := next_fetch_addr_reg + p.streamFetchWidthBytes.U
                is_first_fetch := false.B

                when(!flag_parsed) {
                    state := State.sParseFlag
                }.otherwise {
                    state := State.sDecode_Exec
                }
            }
        }

        is(State.sParseFlag) {
            val flag_bits = p.grDecoderConfig.grKInWidth + 1
            val flag = raw_buffer_reg(
              p.internalBufferWidth - 1,
              p.internalBufferWidth - flag_bits
            )

            val flag_fallback = 2.U

            when(flag === flag_fallback) {
                is_fallback_reg := true.B
            }.otherwise {
                is_fallback_reg := false.B
                k_in_reg := flag
            }

            raw_buffer_reg := raw_buffer_reg << flag_bits.U
            bits_valid_in_buffer := bits_valid_in_buffer - flag_bits.U

            flag_parsed := true.B
            state := State.sDecode_Exec
        }

        // ==========================================================================
        // [修复] 将 Scala if 移入 is() 块内部
        // ==========================================================================

        is(State.sDecode_Exec) {
            if (p.useSingleCycleLoop) {
                // === 方案 A: 1-Cycle Loop (高性能) ===

                // 1. 连通写使能
                do_sram_write := pipe_valid

                // 2. 预判数据是否足够
                val has_enough_bits =
                    bits_valid_in_buffer >= p.grDecoderConfig.grChunkWidth.U

                // 3. 计算消耗长度 (组合逻辑)
                val current_len =
                    Mux(is_fallback_reg, fb_consumed_bits, gr_consumed_bits)

                when(decoded_count === p.groupSize.U) {
                    state := State.sDone
                }.elsewhen(has_enough_bits) {
                    // --- 立即移位更新 Buffer ---
                    raw_buffer_reg := raw_buffer_reg << current_len
                    bits_valid_in_buffer := bits_valid_in_buffer - current_len

                    // 更新计数
                    decoded_count := decoded_count + 1.U

                    // --- 推送数据到写流水线 ---
                    pipe_valid := true.B

                    // 锁存计算参数给下一级
                    is_fallback_pipe_reg := is_fallback_reg
                    k_in_pipe_reg := k_in_reg
                    zero_point_pipe_reg := zero_point_reg

                    when(is_fallback_reg) {
                        weight_pipe_reg := fb_final_weight
                    }.otherwise {
                        q_pipe_reg := gr_q
                        r_pipe_reg := gr_r
                    }

                }.otherwise {
                    // 数据不足
                    pipe_valid := false.B
                    state := State.sFetchStream
                }

            } else {
                // === 方案 B: 2-Cycle Loop (稳健/高频) ===
                // Exec 阶段只负责计算和决策
                when(decoded_count === p.groupSize.U) {
                    state := State.sDone
                }.elsewhen(
                  bits_valid_in_buffer < p.grDecoderConfig.grChunkWidth.U
                ) {
                    state := State.sFetchStream
                }.elsewhen(!is_fallback_reg && gr_error) {
                    state := State.sDone
                }.otherwise {
                    // 跳转到 Post 去移位和写
                    state := State.sDecode_Post

                    // 锁存参数
                    is_fallback_pipe_reg := is_fallback_reg
                    k_in_pipe_reg := k_in_reg
                    zero_point_pipe_reg := zero_point_reg

                    when(is_fallback_reg) {
                        weight_pipe_reg := fb_final_weight
                        consumed_bits_reg := fb_consumed_bits
                    }.otherwise {
                        q_pipe_reg := gr_q
                        r_pipe_reg := gr_r
                        consumed_bits_reg := gr_consumed_bits
                    }
                }
            }
        }

        is(State.sDecode_Post) {
            // 只有在 2-Cycle 模式下,这个状态里的逻辑才有意义
            if (!p.useSingleCycleLoop) {
                // 1. 触发写
                do_sram_write := true.B

                // 2. 执行移位 (消耗数据)
                raw_buffer_reg := raw_buffer_reg << consumed_bits_reg
                bits_valid_in_buffer := bits_valid_in_buffer - consumed_bits_reg

                // 3. 计数更新
                decoded_count := decoded_count + 1.U

                // 4. 跳回 Exec
                state := State.sDecode_Exec
            }
        }

        is(State.sDone) {
            // 等待最后一拍写入完成
            when(!do_sram_write) {
                io.finished := true.B
                when(!io.start) {
                    state := State.sIdle
                }
            }
        }
    }
}
