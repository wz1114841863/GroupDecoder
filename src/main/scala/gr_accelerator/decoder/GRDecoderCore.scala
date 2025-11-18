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
class GRDecoderCore(val p: GRDecoderCoreParams, val coreId: UInt)
    extends Module {
    val io = IO(new GRDecoderCoreIO(p))

    // 调试计数器
    // val cycle_count_reg = RegInit(0.U(32.W))
    // cycle_count_reg := cycle_count_reg + 1.U

    // 实例化子模块
    val gr_decoder = Module(new DecodeUnitGR(p.grDecoderConfig))

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

    // 记录初始字节偏移 (0~7)
    val initial_byte_offset = RegInit(0.U(3.W))
    // 标记是否是当前组的第一次取数
    val is_first_fetch = RegInit(false.B)
    // 标记 Flag 是否已解析
    val flag_parsed = RegInit(false.B)

    // 流式缓冲 寄存器
    val raw_buffer_reg = RegInit(0.U(p.internalBufferWidth.W))
    val bits_valid_in_buffer = RegInit(0.U(p.bufferValidBitsWidth.W))

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

    // 添加 k_in 和 zp 的流水线寄存器
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

    // 从128-bit缓冲的顶端提取 20-bit 块
    val aligned_chunk_top = raw_buffer_reg(
      p.internalBufferWidth - 1,
      p.internalBufferWidth - p.grDecoderConfig.grChunkWidth
    )

    // 连接解码器
    gr_decoder.io.aligned_chunk := aligned_chunk_top
    gr_decoder.io.k_in := k_in_reg

    // Fallback 路径逻辑
    val fb_final_weight = aligned_chunk_top(
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
                // *** [DEBUG PRINTF] ***
                // printf(
                //   p"[Core ${coreId}] sFetchMeta: LATCHING Meta! " +
                //       p"group_index=${io.group_index}, " +
                //       p"recv_addr=0x${Hexadecimal(io.meta_resp.start_byte_addr)}, " +
                //       p"recv_zp=${io.meta_resp.zero_point}\n"
                // )

                // io.meta_req.valid := false.B
                // 地址对齐处理
                val raw_addr = io.meta_resp.start_byte_addr
                // 提取低3位作为 offset (0..7)
                initial_byte_offset := raw_addr(2, 0)

                // 将请求地址向下对齐到 64-bit 边界 (Mask掉低3位)
                // 例如: 33 (0x21) -> 32 (0x20)
                next_fetch_addr_reg := raw_addr & (~7.U(p.streamAddrWidth.W))
                zero_point_reg := io.meta_resp.zero_point

                // printf(
                //   p"[DUT Core ${coreId}] Fetched Meta: StartAddr=0x${Hexadecimal(
                //         io.meta_resp.start_byte_addr
                //       )}, ZP=${io.meta_resp.zero_point}\n"
                // )

                // 复位计数器
                decoded_count := 0.U
                sram_write_addr := 0.U
                raw_buffer_reg := 0.U
                bits_valid_in_buffer := 0.U // 强制进入 sFetchStream

                // 标记这是第一次取数,需要处理 offset
                is_first_fetch := true.B

                // 复位 flag_parsed
                flag_parsed := false.B

                state := State.sFetchStream
            }
        }

        // is(State.sFetchStream) {
        //     // 请求下一个 64-bit 块
        //     io.stream_req.valid := true.B
        //     io.stream_req.addr := next_fetch_addr_reg

        //     when(io.stream_resp.valid) {
        //         // *** [DEBUG PRINTF] ***
        //         // printf(
        //         //   p"[Core ${coreId}] sFetchStream: LATCHING Stream! " +
        //         //       p"req_addr=0x${Hexadecimal(next_fetch_addr_reg)}, " +
        //         //       p"recv_data=0x${Hexadecimal(io.stream_resp.data(63, 32))}...\n"
        //         // )
        //         // 收到响应,停止请求
        //         // io.stream_req.valid := false.B

        //         val raw_data_64 = io.stream_resp.data
        //         // 处理非对齐数据移位
        //         // 如果是第一次取数,我们需要右移剔除无效的低字节
        //         // 假设 Little-Endian: 地址 32 在 bits[7:0], 地址 33 在 bits[15:8]
        //         // 如果 offset=1 (Start=33), 我们要丢弃 bits[7:0], 保留 [63:8]
        //         val shift_bits =
        //             Mux(is_first_fetch, initial_byte_offset << 3, 0.U)

        //         // 1. 将 64-bit 的新数据强制转换为 128-bit UInt
        //         val new_data_128bit =
        //             io.stream_resp.data.pad(p.internalBufferWidth)

        //         // 2. 将这个 128-bit 值左移,使其 *左对齐*
        //         val new_data_padded_to_msb =
        //             new_data_128bit << (p.internalBufferWidth - p.streamFetchWidth).U

        //         // 移位后的数据 (高位补0)
        //         val adjusted_data = raw_data_64 >> shift_bits

        //         // 计算这次有效的比特数
        //         // 正常是 64,第一次可能是 64 - 8*offset
        //         val valid_bits_in_chunk = 64.U - shift_bits

        //         /// 1. 扩展到 Buffer 宽度
        //         val new_data_extended = adjusted_data.pad(p.internalBufferWidth)

        //         // 2. 左移对齐到 MSB (模拟 Queue Push)
        //         // 注意:这里计算 shift 量时,需要确保 buffer 剩下的空间足够
        //         // 假设 buffer 是从 MSB 填充的
        //         val push_shift =
        //             (p.internalBufferWidth.U - p.streamFetchWidth.U) // 这是一个常数吗?

        //         // 先把数据放到最左边 (MSB)
        //         val data_at_msb =
        //             new_data_extended << (p.internalBufferWidth.U - valid_bits_in_chunk)

        //         // 再根据当前 buffer 已有的数据量向右移动
        //         val data_positioned = data_at_msb >> bits_valid_in_buffer

        //         // 更新 Buffer
        //         raw_buffer_reg := raw_buffer_reg | data_positioned

        //         // 更新计数器
        //         bits_valid_in_buffer := bits_valid_in_buffer + valid_bits_in_chunk

        //         // 更新地址 (始终 +8, 因为我们是对齐读取)
        //         next_fetch_addr_reg := next_fetch_addr_reg + p.streamFetchWidthBytes.U

        //         // 清除标志位,后续取数不再移位
        //         is_first_fetch := false.B

        //         // printf(
        //         //   p"  [sFetchStream] RESP Valid. Received_data: 0x${Hexadecimal(io.stream_resp.data)}. " +
        //         //       p"BitsValid_Before: ${bits_valid_in_buffer}. " +
        //         //       p"Merged_raw_buffer: 0x${Hexadecimal(raw_buffer_reg | new_data_shifted)}\n"
        //         // )

        //         // 判断下一步
        //         when(!flag_parsed) {
        //             state := State.sParseFlag
        //         }.otherwise {
        //             state := State.sDecode_Exec
        //         }
        //     }
        // }

        is(State.sFetchStream) {
            io.stream_req.valid := true.B
            io.stream_req.addr := next_fetch_addr_reg

            when(io.stream_resp.valid) {
                val raw_data_64 = io.stream_resp.data

                // [FINAL FIX] 字节序翻转 (Byte Swap)
                // 现状: Little Endian Load. Addr 0 (Head) 在 LSB [7:0].
                // 目标: Stream Processing. Head 必须在 MSB [63:56].
                // 操作: 把 raw_data_64 的字节顺序完全颠倒过来.

                // 1. 拆分字节
                val bytes = VecInit(
                  Seq.tabulate(8)(i => raw_data_64((i + 1) * 8 - 1, i * 8))
                )

                // 2. 逆序拼接: Cat(b0, b1, ... b7)
                // Cat 把第一个参数放在最高位.所以 b0 (Addr 0, Head) 去到了 MSB.
                val reversed_data = Cat(bytes)

                // --- 后续逻辑使用 reversed_data ---

                // Offset 处理: 现在垃圾(如果有)位于高位(MSB), 使用左移丢弃
                val shift_bits =
                    Mux(is_first_fetch, initial_byte_offset << 3, 0.U)
                val adjusted_data = reversed_data << shift_bits

                val valid_bits_in_chunk = 64.U - shift_bits

                // 归位: 将 64-bit 数据对齐到 96-bit Buffer 的顶端
                // adjusted_data 已经是 MSB 对齐的了
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

            // 标记 Flag 已解析
            flag_parsed := true.B
            state := State.sDecode_Exec

            // [DEBUG]
            // printf(
            //   p"[Core $coreId] Parse Flag: RawBufferTop=${Binary(raw_buffer_reg >> (p.internalBufferWidth - 8))} Flag=$flag (0=k1, 1=k2, 2=FB)\n"
            // )
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
                //   p"  [sDecode_Exec] CHUNK_IN: 0x${Hexadecimal( aligned_chunk_top)} (k_in: ${k_in_reg}). " +
                //       p"GR_OUT -> q: ${gr_q}, r: ${gr_r}, consumed: ${gr_consumed_bits}. " +
                //       p"Fallback: ${is_fallback_reg}\n"
                // )

                // printf(
                //   p"[DUT Core ${coreId}] [C=${cycle_count_reg}] sDecode_Exec: LATCHING -> " +
                //       p"k_in_reg: ${k_in_reg}, zp_reg: ${zero_point_reg}, q: ${gr_q}, r: ${gr_r}\n"
                // )

                // 执行解码
                state := State.sDecode_Post
                // 锁存流水线 寄存器
                is_fallback_pipe_reg := is_fallback_reg

                k_in_pipe_reg := k_in_reg
                zero_point_pipe_reg := zero_point_reg

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
                val k_val = k_in_pipe_reg + 1.U(2.W)
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
                    (signed_delta + zero_point_pipe_reg.zext.asSInt).asUInt

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

            // [DEBUG ADDED] 打印写入 SRAM 的权重值
            // 这可以验证: 1. 解码是否正确 2. ZP 是否加上了
            // printf(
            //   p"[Core $coreId] Write SRAM: Addr=$sram_write_addr Data=${io.sram_write.data} (ZP used=${zero_point_pipe_reg})\n"
            // )
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

    // --- 调试输出 ---
    // val coreId = p.P match {
    //     case 2 => Mux(this.hashCode.U % 2.U === 0.U, 0.U, 1.U)
    //     case _ => 0.U // 默认为 0
    // }

    // printf(
    //   p"[DUT Core ${coreId}] [C=${cycle_count_reg}] State: ${state} | " +
    //       p"Start: ${io.start} | Fin: ${io.finished} | " +
    //       p"Meta(V/R): ${io.meta_req.valid}/${io.meta_resp.valid} | " +
    //       p"Stream(V/R): ${io.stream_req.valid}/${io.stream_resp.valid}\n"
    // )

}
