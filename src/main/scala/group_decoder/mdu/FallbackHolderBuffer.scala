package group_decoder.mdu

import chisel3._
import chisel3.util._
import group_decoder.common._

class FallbackHoldBuffer(p: PDUParams, m: MDUParams) extends Module {
    val io = IO(new Bundle {
        // 来自解析器 (Parser)
        val in = Flipped(Decoupled(new FallbackDataBundle(p)))
        // 来自ROB的查询
        val query_valid = Input(Bool())
        val query_tag = Input(UInt(p.tagWidth.W))
        val is_ready = Output(Bool()) // 回应查询: "我已准备好这个tag"
        // 向ROB输出
        val out = Decoupled(new FallbackDataBundle(p))
    })

    val num_slots = m.fbHoldBufferSlots
    val slots_data = Reg(Vec(num_slots, new FallbackDataBundle(p)))
    val slots_valid = RegInit(VecInit(Seq.fill(num_slots)(false.B)))

    // --- 输入逻辑 (找到一个空槽写入) ---
    val empty_slot_idx = PriorityEncoder(slots_valid.map(!_))
    val has_empty_slot = slots_valid.contains(false.B)

    io.in.ready := has_empty_slot
    when(io.in.fire) {
        slots_data(empty_slot_idx) := io.in.bits
        slots_valid(empty_slot_idx) := true.B
    }

    // --- 查询逻辑 (并行查找匹配的tag) ---
    val query_hits = slots_valid.zip(slots_data).map { case (valid, data) =>
        valid && (data.tag === io.query_tag)
    }
    val query_hit_somewhere = query_hits.reduce(_ || _)
    val hit_slot_idx = OHToUInt(query_hits)

    io.is_ready := query_hit_somewhere

    // --- 输出逻辑 ---
    io.out.valid := false.B
    io.out.bits := DontCare // 默认值

    when(io.query_valid && query_hit_somewhere && io.out.ready) {
        // ROB发出了请求, 我们有匹配的, 且ROB准备好接收
        io.out.valid := true.B
        io.out.bits := slots_data(hit_slot_idx)
        // 清空该槽位
        slots_valid(hit_slot_idx) := false.B
    }
}
