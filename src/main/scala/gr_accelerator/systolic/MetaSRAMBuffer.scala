package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

class MetaSRAMIO(val p: MetaSRAMParams) extends Bundle {
    // --- 控制 ---
    val flip = Input(Bool()) // 0: 写A读B, 1: 写B读A

    // --- 1. ZP 写入端口 (来自 DecoderBank) ---
    // P 个并行端口, 匹配 DecoderBank.io.meta_write_outputs
    val zp_write_ports = Input(
      Vec(
        p.P,
        new Bundle {
            val valid = Bool()
            val zp = UInt(p.zpWidth.W)
            // wave_index 用于计算行地址: Row = (Wave * P) + CoreID
            val wave_index = UInt(8.W)
        }
      )
    )

    // --- 2. Scale 写入端口 (来自 Top Controller / DMA) ---
    // 简单的单端口写入 (假设 Scale 是预加载或流式加载的)
    val scale_write_port = Input(new Bundle {
        val valid = Bool()
        val addr = UInt(p.addrWidth.W) // 0..N-1
        val data = UInt(p.scaleWidth.W)
    })

    // --- 3. 读取端口 (去往 MetadataLoader) ---
    // 提供地址, 返回对应的 ZP 和 Scale
    val read_addr = Input(UInt(p.addrWidth.W))
    val read_zp = Output(UInt(p.zpWidth.W))
    val read_scale = Output(UInt(p.scaleWidth.W))
}

/*
 * MetaSRAMBuffer (双缓冲) 用于存储每行的 ZP 和 Scale
 */
class MetaSRAMBuffer(val p: MetaSRAMParams = MetaSRAMParams.default)
    extends Module {
    val io = IO(new MetaSRAMIO(p))

    // --- 存储实例化 ---
    // 我们需要两组 SRAM (A 和 B), 每组包含 ZP 和 Scale
    // 深度为 N (对应 N 行)
    val zp_mem_A = SyncReadMem(p.N, UInt(p.zpWidth.W))
    val scale_mem_A = SyncReadMem(p.N, UInt(p.scaleWidth.W))

    val zp_mem_B = SyncReadMem(p.N, UInt(p.zpWidth.W))
    val scale_mem_B = SyncReadMem(p.N, UInt(p.scaleWidth.W))

    // --- 状态 ---
    // active_bank_is_B:
    // false => 生产者写 A, 消费者读 B
    // true  => 生产者写 B, 消费者读 A
    val active_bank_is_B = RegInit(false.B)

    when(io.flip) {
        active_bank_is_B := !active_bank_is_B
    }

    // --- 1. 写入逻辑 (ZP) ---
    // 写入 "非活动" 的 Bank
    // ZP 来自 P 个 Decoder, 地址 = (Wave * P) + CoreID
    for (i <- 0 until p.P) {
        val port = io.zp_write_ports(i)

        // 地址计算: 第几波 * 并行度 + 当前核ID
        // 例如: N=16, P=8. Core 0 在 Wave 0 写 Row 0, 在 Wave 1 写 Row 8.
        val write_addr = (port.wave_index * p.P.U) + i.U

        when(port.valid) {
            when(!active_bank_is_B) {
                // 状态0: 写 A
                zp_mem_A.write(write_addr, port.zp)
            }.otherwise {
                // 状态1: 写 B
                zp_mem_B.write(write_addr, port.zp)
            }
        }
    }

    // --- 2. 写入逻辑 (Scale) ---
    // Scale 同样写入 "非活动" 的 Bank (通常 Scale 会先于计算加载)
    // 假设 Scale 是线性加载的, 直接使用输入的地址
    when(io.scale_write_port.valid) {
        val addr = io.scale_write_port.addr
        val data = io.scale_write_port.data

        when(!active_bank_is_B) {
            scale_mem_A.write(addr, data)
        }.otherwise {
            scale_mem_B.write(addr, data)
        }
    }

    // --- 3. 读取逻辑 ---
    // 从 "活动" 的 Bank 读取
    val read_addr_reg = RegNext(io.read_addr) // SyncReadMem 有 1 周期延迟

    val zp_out_A = zp_mem_A.read(io.read_addr)
    val scale_out_A = scale_mem_A.read(io.read_addr)

    val zp_out_B = zp_mem_B.read(io.read_addr)
    val scale_out_B = scale_mem_B.read(io.read_addr)

    // 选择输出
    // 如果 active_bank_is_B 为 true (消费者读 A), 则输出 A
    // 纠正: 定义是 active_bank_is_B 为 true => 消费者读 A?
    // 让我们保持一致:
    // flip=0 (init) -> active_bank_is_B=false -> 生产者写A, 消费者读B
    // flip=1        -> active_bank_is_B=true  -> 生产者写B, 消费者读A

    io.read_zp := Mux(active_bank_is_B, zp_out_A, zp_out_B)
    io.read_scale := Mux(active_bank_is_B, scale_out_A, scale_out_B)
}
