package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

class MetaSRAMIO(val p: MetaSRAMParams) extends Bundle {
    // --- 控制 ---
    val flip = Input(Bool()) // 0: 写A读B, 1: 写B读A (电平信号)

    // --- 1. ZP 写入端口 (来自 DecoderBank) ---
    val zp_write_ports = Input(
      Vec(
        p.P,
        new Bundle {
            val valid = Bool()
            val zp = UInt(p.zpWidth.W)
            val wave_index = UInt(8.W)
        }
      )
    )

    // --- 2. Scale 写入端口 (来自 Top Controller) ---
    val scale_write_port = Input(new Bundle {
        val valid = Bool()
        val addr = UInt(p.addrWidth.W)
        val data = UInt(p.scaleWidth.W)
    })

    // --- 3. 读取端口 (去往 MetadataLoader) ---
    val read_addr = Input(UInt(p.addrWidth.W))
    val read_zp = Output(UInt(p.zpWidth.W))
    val read_scale = Output(UInt(p.scaleWidth.W))
}

class MetaSRAMBuffer(val p: MetaSRAMParams = MetaSRAMParams.default)
    extends Module {
    val io = IO(new MetaSRAMIO(p))

    // --- 存储实例化 ---
    val zp_mem_A = SyncReadMem(p.N, UInt(p.zpWidth.W))
    val scale_mem_A = SyncReadMem(p.N, UInt(p.scaleWidth.W))

    val zp_mem_B = SyncReadMem(p.N, UInt(p.zpWidth.W))
    val scale_mem_B = SyncReadMem(p.N, UInt(p.scaleWidth.W))

    // --- [FIXED] 状态控制 ---
    // 直接使用输入的电平信号,不再使用内部翻转逻辑
    // io.flip = false => active_bank_is_B = false (生产者写 A, 消费者读 B)
    // io.flip = true  => active_bank_is_B = true  (生产者写 B, 消费者读 A)
    val active_bank_is_B = io.flip

    // --- 1. 写入逻辑 (ZP) ---
    // 写入 "非活动" 的 Bank
    for (i <- 0 until p.P) {
        val port = io.zp_write_ports(i)
        val write_addr = (port.wave_index * p.P.U) + i.U

        when(port.valid) {
            // 注意: 如果 active_bank_is_B 为 true (消费者读 A), 则生产者写 B
            // 这里的逻辑要反过来匹配:
            // !active_bank_is_B (即 flip=0) => 生产者写 A
            when(!active_bank_is_B) {
                zp_mem_A.write(write_addr, port.zp)
            }.otherwise {
                zp_mem_B.write(write_addr, port.zp)
            }
        }
    }

    // --- 2. 写入逻辑 (Scale) ---
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
    val read_addr_reg = RegNext(io.read_addr)

    // 始终从两边读取
    val zp_out_A = zp_mem_A.read(io.read_addr)
    val scale_out_A = scale_mem_A.read(io.read_addr)

    val zp_out_B = zp_mem_B.read(io.read_addr)
    val scale_out_B = scale_mem_B.read(io.read_addr)

    // 选择输出
    // active_bank_is_B (flip=1) => 消费者读 A
    // !active_bank_is_B (flip=0) => 消费者读 B
    // [Check WeightSRAM logic]:
    // WeightSRAM: flip=1 -> Read A. (Correct)
    io.read_zp := Mux(active_bank_is_B, zp_out_A, zp_out_B)
    io.read_scale := Mux(active_bank_is_B, scale_out_A, scale_out_B)
}
