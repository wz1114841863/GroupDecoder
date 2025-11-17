package gr_accelerator.common

import chisel3._
import chisel3.util._

/** 顶层加速器的总配置参数
  * @param P
  *   解码器并行度 (例如 8)
  * @param N
  *   脉动阵列维度 (例如 16)
  * @param groupSize
  *   压缩组大小 (固定 512)
  * @param maxTiles
  *   支持的最大 Tile 数量 (用于 sizing 计数器)
  */
case class TopAcceleratorParams(
    P: Int,
    N: Int,
    groupSize: Int,
    maxTiles: Int, // 支持最大的层规模

    // 子模块参数 (自动派生)
    decoderParams: DecoderBankParams,
    saParams: SystolicArrayParams,
    peParams: PEParams,
    weightSramParams: WeightSRAMParams,
    metaSramParams: MetaSRAMParams
)

object TopAcceleratorParams {
    def apply(
        P: Int = 8,
        N: Int = 16,
        groupSize: Int = 512
    ): TopAcceleratorParams = {

        val maxTiles = 1024 // 假设一个合理的默认值

        // 1. 核心约束检查
        require(
          N % P == 0,
          s"脉动阵列维度 N($N) 必须是解码器并行度 P($P) 的整数倍,否则 WeightLoader 无法整除"
        )

        // 2. 构建子模块参数

        // PE 参数 (标准配置)
        val peParams = PEParams.default

        // Systolic Array 参数
        val saParams =
            SystolicArrayParams(N = N, weightWidth = peParams.weightWidth)

        // Decoder Core 参数 (groupSize 必须匹配)
        val coreParams = GRDecoderCoreParams.default.copy(groupSize = groupSize)

        // Decoder Bank 参数
        val decoderParams = DecoderBankParams(
          coreParams = coreParams,
          P = P
        )

        // SRAM 参数 (自动计算容量和地址宽度)
        val weightSramParams = WeightSRAMParams(decoderParams, saParams)
        val metaSramParams = MetaSRAMParams(decoderParams, saParams, peParams)

        new TopAcceleratorParams(
          P,
          N,
          groupSize,
          maxTiles,
          decoderParams,
          saParams,
          peParams,
          weightSramParams,
          metaSramParams
        )
    }

    // 提供一个默认的 "黄金配置" (P=8, N=16)
    val default: TopAcceleratorParams = apply(P = 8, N = 16, groupSize = 512)
}
