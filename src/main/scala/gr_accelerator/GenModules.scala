package gr_accelerator

import chisel3._
import java.io.{File, PrintWriter}

import gr_accelerator.common._
import gr_accelerator.decoder._
import gr_accelerator.systolic._

object GenModules extends App {

    // ==============================================================================
    // 1. 配置生成辅助函数
    // ==============================================================================
    // 为了公平对比 P 的影响,我们固定 N=16 和 GroupSize=512
    def getParams(pVal: Int): TopAcceleratorParams = {
        // 约束检查: N 必须是 P 的倍数
        // 如果 P > 16 (虽然不常见), 我们需要把 N 撑大
        val nVal = if (pVal > 16) pVal else 16

        TopAcceleratorParams(
          P = pVal,
          N = nVal,
          groupSize = 512
        )
    }

    // ==============================================================================
    // 2. 参数解析
    // ==============================================================================
    // 用法示例:
    //   sbt "runMain gr_accelerator.GenModules Top_All_P"      (批量生成 P=1,2,4,8)
    //   sbt "runMain gr_accelerator.GenModules Top 4"          (生成 P=4 的 Top)
    //   sbt "runMain gr_accelerator.GenModules Core_Compare"   (生成 Core 对比)

    val moduleName = args.headOption.getOrElse("Top")
    // 如果提供了第二个参数,则作为 P 值;否则默认为 8
    val pArg = if (args.length > 1) args(1).toInt else 8

    // 当前命令行指定的 P 对应的配置
    val currentParams = getParams(pArg)

    // ==============================================================================
    // 3. 生成与保存工具
    // ==============================================================================
    def generateAndSave(
        dut: => RawModule,
        folderName: String,
        fileName: String
    ): Unit = {
        val dir = new File(s"generated/$folderName")
        if (!dir.exists()) dir.mkdirs()

        println(
          s"Generating Verilog for [$fileName] in directory [$folderName] ..."
        )
        try {
            val verilog = getVerilogString(dut)
            val file = new File(dir, s"$fileName.sv")
            val writer = new PrintWriter(file)
            writer.write(verilog)
            writer.close()
            println(s"[Success] Saved to: ${file.getAbsolutePath}")
        } catch {
            case e: Exception =>
                println(s"[Error] Failed: $e")
                e.printStackTrace()
        }
    }

    println(s"=========================================================")
    println(s"  Target Module:   $moduleName")
    if (!moduleName.contains("Compare") && !moduleName.contains("All")) {
        println(s"  Parameter P:     $pArg")
    }
    println(s"=========================================================")

    // ==============================================================================
    // 4. 模块匹配与生成
    // ==============================================================================
    moduleName match {

        // --- 批量生成不同 P 的 Top (用于论文画图) ---
        case "Top_All_P" =>
            println(
              ">>> Batch Generating TopAccelerator for P = 1, 2, 4, 8 <<<"
            )
            Seq(1, 2, 4, 8).foreach { p =>
                val pParams = getParams(p)
                generateAndSave(
                  new TopAccelerator(pParams),
                  s"Top_P$p",
                  s"TopAccelerator_P$p"
                )
            }
            println(">>> Batch Generation Complete! <<<")

        // --- Core 架构对比 (1-Cycle vs 2-Cycle) ---
        case "Core_Compare" =>
            println(
              ">>> Generating Comparison Versions for GRDecoderCore (P is irrelevant here) <<<"
            )
            val baseCoreParams = currentParams.decoderParams.coreParams

            // Version A: 1-Cycle High Performance
            val paramsFast = baseCoreParams.copy(useSingleCycleLoop = true)
            generateAndSave(
              new GRDecoderCore(paramsFast, coreId = 0.U),
              "Core_1Cycle",
              "GRDecoderCore_1Cycle"
            )

            // Version B: 2-Cycle Conservative
            val paramsSlow = baseCoreParams.copy(useSingleCycleLoop = false)
            generateAndSave(
              new GRDecoderCore(paramsSlow, coreId = 0.U),
              "Core_2Cycle",
              "GRDecoderCore_2Cycle"
            )
            println(">>> Core Comparison Generation Complete! <<<")

        // --- 单个模块生成 (支持动态 P) ---

        case "Top" =>
            generateAndSave(
              new TopAccelerator(currentParams),
              s"Top_P$pArg",
              "TopAccelerator"
            )

        case "DecoderBank" =>
            generateAndSave(
              new DecoderBank(currentParams.decoderParams),
              s"DecoderBank_P$pArg",
              "DecoderBank"
            )

        case "WeightSRAM" =>
            generateAndSave(
              new WeightSRAM(currentParams.weightSramParams),
              s"WeightSRAM_P$pArg",
              "WeightSRAM"
            )

        case "WeightLoader" =>
            generateAndSave(
              new WeightLoader(currentParams.weightSramParams),
              s"WeightLoader_P$pArg",
              "WeightLoader"
            )

        case "MetaSRAM" =>
            generateAndSave(
              new MetaSRAMBuffer(currentParams.metaSramParams),
              s"MetaSRAM_P$pArg",
              "MetaSRAMBuffer"
            )

        // --- 不依赖 P 的模块 (Core, PE, DecodeUnit) ---
        // 即使传入了 P 参数,这些模块的逻辑也是一样的 (Core 取决于 GroupSize)

        case "Core" =>
            generateAndSave(
              new GRDecoderCore(currentParams.decoderParams.coreParams, 0.U),
              "Core",
              "GRDecoderCore"
            )

        case "DecodeUnit" =>
            val unitConfig = GRDecoderConfig.default
            generateAndSave(
              new DecodeUnitGR(unitConfig),
              "DecodeUnit",
              "DecodeUnitGR"
            )

        case "PE" =>
            generateAndSave(new PE(currentParams.peParams), "PE", "PE")

        case "Array" =>
            generateAndSave(
              new SystolicArray(currentParams.saParams, currentParams.peParams),
              "Array",
              "SystolicArray"
            )

        case _ =>
            println(s"Error: Unknown module name '$moduleName'")
            println(
              "Options: Top_All_P, Core_Compare, Top, DecoderBank, WeightSRAM, WeightLoader, Core, PE..."
            )
            sys.exit(1)
    }
}
