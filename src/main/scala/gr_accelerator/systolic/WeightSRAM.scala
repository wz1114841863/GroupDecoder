package gr_accelerator.systolic

import chisel3._
import chisel3.util._
import gr_accelerator.common._

/** WeightSRAM (双缓冲) 的 IO 端口
  */
class WeightSRAMIO(val p: WeightSRAMParams) extends Bundle {

    // --- 控制端口 (来自 TopAccelerator) ---
    val flip = Input(Bool()) // 0 = A 写, B 读; 1 = B 写, A 读

    // --- 写入 侧 (来自 DecoderBank, P=8) ---
    val write_ports = Input(
      Vec(
        p.P,
        new Bundle {
            val valid = Bool()
            val addr = UInt(p.sramAddrWidth.W) // 0..511
            val data = UInt(p.weightWidth.W) // 4-bit
            val wave_index =
                UInt(log2Ceil((p.totalWeights / p.groupSize) / p.P + 1).W)
        }
      )
    )

    // --- 读取侧 (来自 SystolicArray, P=8) ---
    // 脉动阵列现在必须提供 P=8 个逻辑地址
    val read_addrs = Input(
      Vec(p.P, UInt(p.saReadAddrWidth.W))
    ) // P=8 宽, 0..16383
    // WeightSRAM 并行返回 P=8 个数据
    val read_data = Output(Vec(p.P, UInt(p.weightWidth.W)))
}

/** WeightSRAM (双缓冲)读/写带宽已匹配
  */
class WeightSRAM(val p: WeightSRAMParams) extends Module {
    val io = IO(new WeightSRAMIO(p))

    // 1. 实例化 16KB 双缓冲
    // (2 Banks * P=8 Banks * 2048 Depth)
    val bank_a =
        Seq.fill(p.numBanks)(SyncReadMem(p.bankDepth, UInt(p.weightWidth.W)))
    val bank_b =
        Seq.fill(p.numBanks)(SyncReadMem(p.bankDepth, UInt(p.weightWidth.W)))

    // --- 2. 写入 逻辑 (P=8 并行) ---
    for (i <- 0 until p.P) {
        when(io.write_ports(i).valid) {
            // bank_addr (0..2047)
            val bank_addr = (io.write_ports(i).wave_index * p.groupSize.U) +
                io.write_ports(i).addr

            when(io.flip) {
                // flip=1: 写入 Bank B
                bank_b(i).write(bank_addr, io.write_ports(i).data)
            }.otherwise {
                // flip=0: 写入 Bank A
                bank_a(i).write(bank_addr, io.write_ports(i).data)
            }
        }
    }

    // --- 3. 读取逻辑 (P=8 并行) ---
    val read_data_wires = Wire(Vec(p.P, UInt(p.weightWidth.W)))

    for (i <- 0 until p.P) {
        // 每个 Bank (i) 只处理来自 read_addrs(i) 的请求
        val logical_addr = io.read_addrs(i)

        // --- 地址解码 ---
        // 需要确定这个 *逻辑地址* 应该去哪个 *物理 Bank*
        val group_id = logical_addr / p.groupSize.U
        val local_offset = logical_addr % p.groupSize.U

        // 这个逻辑地址 (logical_addr) 对应的 bank_index
        val physical_bank_index = (group_id % p.numBanks.U)(
          log2Ceil(p.numBanks) - 1,
          0
        )

        // 关键: 这个逻辑地址 (logical_addr) 对应的 bank 内地址
        val wave_index = group_id / p.numBanks.U
        val physical_bank_addr = (wave_index * p.groupSize.U) + local_offset

        // 设计假设:
        // read_addrs(i) 上的逻辑地址总是属于物理 Bank(i)

        // 1. 从 read_addrs(i) 计算物理 bank_addr
        val logical_addr_for_bank_i = io.read_addrs(i)
        val group_id_for_bank_i = logical_addr_for_bank_i / p.groupSize.U
        val local_offset_for_bank_i = logical_addr_for_bank_i % p.groupSize.U

        // 确认这个地址确实属于 Bank i
        // assert((group_id_for_bank_i % p.numBanks.U) === i.U, "Read address bank mismatch!")

        val wave_index_for_bank_i = group_id_for_bank_i / p.numBanks.U
        val physical_addr_for_bank_i =
            (wave_index_for_bank_i * p.groupSize.U) + local_offset_for_bank_i

        // 2. (组合地) 从 Bank A(i) 和 Bank B(i) 读取
        val data_from_a = bank_a(i).read(physical_addr_for_bank_i, true.B)
        val data_from_b = bank_b(i).read(physical_addr_for_bank_i, true.B)

        // 3. 使用 'flip' 选择 A 或 B
        read_data_wires(i) := Mux(
          io.flip,
          data_from_a, // flip=1: 读取 Bank A
          data_from_b // flip=0: 读取 Bank B
        )
    }

    // SyncReadMem 有 1 个周期的读取 延迟.
    // 我们必须将组合 读取 的结果用寄存器延迟一拍.
    io.read_data := RegNext(read_data_wires)
}
