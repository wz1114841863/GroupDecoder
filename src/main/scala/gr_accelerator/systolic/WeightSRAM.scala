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

    // [DEBUG] 打印关键参数,确认容量计算是否生效
    // --- 仅在复位后的第一拍打印 Init 信息 ---
    // val hasPrintedInit = RegInit(false.B)
    // when(!hasPrintedInit) {
    //     printf(
    //       p"[WeightSRAM Init] TotalWeights=${p.totalWeights}, BankDepth=${p.bankDepth}, AddrWidth=${p.bankAddrWidth}\n"
    //     )
    //     printf(p"[WeightSRAM Init] P=${p.P}, N=${p.N}, GS=${p.groupSize}\n")
    //     hasPrintedInit := true.B
    // }

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
        val logical_addr = io.read_addrs(i)

        // 1. 简单的地址转换
        val wave_index = logical_addr / (p.groupSize * p.P).U // 属于哪一波
        val local_offset = logical_addr % p.groupSize.U // 组内偏移

        val physical_addr = (wave_index * p.groupSize.U) + local_offset

        // 2. [关键] 验证 1:1 映射假设
        // 我们假设 logical_addr 对应的 Group ID 必须 模 P 等于 i
        val group_id = logical_addr / p.groupSize.U
        assert(
          group_id % p.P.U === i.U,
          "WeightSRAM Access Violation: Port mismatch!"
        )

        // 3. 直接读取 Bank i
        val data_from_a = bank_a(i).read(physical_addr, true.B)
        val data_from_b = bank_b(i).read(physical_addr, true.B)

        read_data_wires(i) := Mux(io.flip, data_from_a, data_from_b)

        // [DEBUG]
        // --- [FIX] 2. 优化读取打印 ---
        // 只有在地址发生变化时才打印,或者只打印前几个周期
        // 这里我们使用 RegNext 检测地址变化

        // val prev_addr = RegNext(physical_bank_addr)
        // val addr_changed = physical_bank_addr =/= prev_addr

        // 只打印 Port 0 以减少刷屏,且只在地址变化且处于读模式时打印
        // if (i == 0) {
        //     when(io.flip && addr_changed) {
        //         printf(
        //           p"[WeightSRAM] Port $i Read A: Addr=$physical_bank_addr Data=$data_from_a (Logical: $logical_addr)\n"
        //         )
        //     }
        // }

        // 为了调试当前问题,我们可以先彻底注释掉读取打印,
        // 因为 "Addr=0 Data=9" 的重复已经告诉我们数据是对的 (G0 data),
        // 只是 WeightLoader 可能一直在读 0 或者读完了停在 0.
    }

    // SyncReadMem 有 1 个周期的读取 延迟.
    // 我们必须将组合 读取 的结果用寄存器延迟一拍.
    io.read_data := RegNext(read_data_wires)
}
