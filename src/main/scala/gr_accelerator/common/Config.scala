package gr_accelerator.common

import chisel3._
import chisel3.util._

/** 定义 GR 解码器核心 (DecodeUnit_GR) 的所有可配置参数
  */
case class GRDecoderConfig(
    // 通过分析: 映射后最大 delta=30, q=15, k=1
    // 最大 GR 长度 = 15(q) + 1(0) + 1(r) = 17 bits.
    // 设置一个 20-bit 的块宽度
    grChunkWidth: Int,

    // k 只有 1 和 2, 1-bit 就足够表示
    grKInWidth: Int,

    // k_val (1 或 2) 所需的宽度
    grKValWidth: Int,

    // 快路径
    // 查看比特流的前 4 位
    grFastPathMaxBits: Int,

    // 慢路径
    // q_max=15, 因此 q_stream 宽度为 16
    grQMax: Int,
    grQStreamWidth: Int,

    // 基于 qMax 定义输出宽度
    // q_max=15,需要 4 bits
    grQValWidth: Int,

    // k_max=2, r_max=3, 需要 2 bits
    grRValWidth: Int,

    // 最终 mapped_delta (q << k) | r
    // 映射后权重的最大值为 30, 需要 5 bits 来表示 (0-30)
    grMappedDeltaWidth: Int,

    // 最大长度 17 bits.需要 5 bits 来表示 (0-17)
    grLengthWidth: Int
)

object GRDecoderConfig {
    def create(
        grChunkWidth: Int = 20,
        grKInWidth: Int = 1,
        grFastPathMaxBits: Int = 6,
        grQMax: Int = 15
    ): GRDecoderConfig = {
        val grKValWidth = 2
        val grQStreamWidth = grQMax + 1
        val grQValWidth = log2Ceil(grQMax + 1)
        val grRValWidth = 2
        val grMappedDeltaWidth = 5
        val grLengthWidth = log2Ceil(grChunkWidth + 1)

        GRDecoderConfig(
          grChunkWidth,
          grKInWidth,
          grKValWidth,
          grFastPathMaxBits,
          grQMax,
          grQStreamWidth,
          grQValWidth,
          grRValWidth,
          grMappedDeltaWidth,
          grLengthWidth
        )
    }

    // 提供默认配置
    val default: GRDecoderConfig = create()
}

/** GRDecoderCore 的参数
  * @param grDecoderConfig
  *   嵌套的 GRDecoderConfig
  * @param groupSize
  *   算法组 大小 (e.g., 512)
  * @param weightWidth
  *   最终输出权重宽度 (4-bit)
  * @param zpWidth
  *   零点 宽度 (8-bit)
  * @param streamFetchWidth
  *   每次从"共享前端" 获取的固定比特数
  * @param internalBufferWidth
  *   FSM 内部缓冲 区总宽度
  * @param metaGroupIndexWidth
  *   组索引 输入端口宽度
  * @param streamAddrWidth
  *   字节流 地址 端口宽度
  * @param outputSramAddrWidth
  *   解码器 输出 SRAM 地址 宽度 (0..511)
  * @param groupCountWidth
  *   组 内权重计数器 宽度 (0..512)
  * @param bufferValidBitsWidth
  *   内部缓冲 有效比特计数器 宽度 (0..128)
  * @param streamFetchWidthBytes
  *   每次获取的字节数 (用于地址递增)
  * @param groupBitOffsetWidth
  *   组 内总比特偏移 计数器宽度
  */
case class GRDecoderCoreParams(
    grDecoderConfig: GRDecoderConfig,

    // 算法参数
    groupSize: Int,
    weightWidth: Int,
    zpWidth: Int,

    // 流水线 缓冲 参数
    streamFetchWidth: Int,
    internalBufferWidth: Int,

    // 地址和计数器宽度
    metaGroupIndexWidth: Int,
    streamAddrWidth: Int,
    outputSramAddrWidth: Int,
    groupCountWidth: Int,
    bufferValidBitsWidth: Int,
    streamFetchWidthBytes: Int,
    groupBitOffsetWidth: Int
)

object GRDecoderCoreParams {
    def apply(
        grDecoderConfig: GRDecoderConfig = GRDecoderConfig.default,
        groupSize: Int = 512,
        weightWidth: Int = 4,
        zpWidth: Int = 8,
        streamFetchWidth: Int = 64,
        internalBufferWidth: Int = 96, // 64 + 20 < 96
        metaGroupIndexWidth: Int = 16, // 可支持 65536 组
        streamAddrWidth: Int = 32, // 32-bit 字节地址
        groupBitOffsetWidth: Int = 24 // 组 内最大比特偏移

    ): GRDecoderCoreParams = {

        val outputSramAddrWidth = log2Ceil(groupSize) // e.g., 9
        val groupCountWidth = log2Ceil(groupSize + 1) // e.g., 10
        val bufferValidBitsWidth = log2Ceil(internalBufferWidth + 1) // e.g., 8
        val streamFetchWidthBytes = streamFetchWidth / 8 // e.g., 8

        GRDecoderCoreParams(
          grDecoderConfig,
          groupSize,
          weightWidth,
          zpWidth,
          streamFetchWidth,
          internalBufferWidth,
          metaGroupIndexWidth,
          streamAddrWidth,
          outputSramAddrWidth,
          groupCountWidth,
          bufferValidBitsWidth,
          streamFetchWidthBytes,
          groupBitOffsetWidth
        )
    }

    val default: GRDecoderCoreParams = apply()
}

/** DecoderBank (顶层分发模块) 的参数
  *
  * @param coreParams
  *   嵌套的 GRDecoderCoreParams
  * @param P
  *   并行解码器 的数量
  * @param metaSramDepth
  *   "元数据 SRAM" 的深度
  * @param sharedCacheKBytes
  *   "共享缓存" 的大小 (2KB)
  * @param metaDataType
  *   合并的元数据
  */

class MetaData(val p: GRDecoderCoreParams) extends Bundle {
    val start_byte_addr = UInt(p.streamAddrWidth.W)
    val zero_point = UInt(p.zpWidth.W)
}

case class DecoderBankParams(
    coreParams: GRDecoderCoreParams,
    P: Int,
    metaSramDepth: Int,
    sharedCacheKBytes: Int,
    metaDataType: MetaData
)

object DecoderBankParams {
    def apply(
        coreParams: GRDecoderCoreParams = GRDecoderCoreParams.default,
        P: Int = 8,
        metaSramDepth: Int = 250000,
        sharedCacheKBytes: Int = 2
    ): DecoderBankParams = {
        DecoderBankParams(
          coreParams,
          P,
          metaSramDepth,
          sharedCacheKBytes,
          new MetaData(coreParams) // 实例化 Bundle
        )
    }

    // 提供默认配置
    val default: DecoderBankParams = apply()
}
