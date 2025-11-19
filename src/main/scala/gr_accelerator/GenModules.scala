package gr_accelerator

import chisel3._
import java.io.{File, PrintWriter}
import java.nio.file.{Paths, Files}

import gr_accelerator.common._
import gr_accelerator.decoder._
import gr_accelerator.systolic._

object GenModules extends App {

    // 1. 获取统一的全局配置
    val topParams = TopAcceleratorParams.default

    // 提取子模块参数
    val coreParams = topParams.decoderParams.coreParams
    val bankParams = topParams.decoderParams
    val sramParams = topParams.weightSramParams
    val metaParams = topParams.metaSramParams
    val peParams = topParams.peParams
    val saParams = topParams.saParams

    // 2. 读取命令行参数
    val moduleName = args.headOption.getOrElse("Top")

    // 3. 定义输出目录 (在项目根目录下的 generated 文件夹)
    val outputDir = "generated"
    new File(outputDir).mkdirs() // 确保目录存在

    // 4. 辅助函数: 生成并保存
    def generateAndSave(dut: => RawModule, filename: String): Unit = {
        println(s"Generating Verilog for $filename ...")
        try {
            // 获取 Verilog 字符串
            val verilog = getVerilogString(dut)

            // 写入文件
            val file = new File(s"$outputDir/$filename.sv")
            val writer = new PrintWriter(file)
            writer.write(verilog)
            writer.close()

            println(s"[Success] Verilog saved to: ${file.getAbsolutePath}")
        } catch {
            case e: Exception =>
                println(s"[Error] Failed to generate $filename: $e")
                e.printStackTrace()
        }
    }

    // 5. 匹配并生成
    moduleName match {

        case "Unit" =>
            val unitConfig = GRDecoderConfig.default
            generateAndSave(new DecodeUnitGR(unitConfig), "DecodeUnitGR")

        case "Core" =>
            generateAndSave(
              new GRDecoderCore(coreParams, coreId = 0.U),
              "GRDecoderCore"
            )

        case "Bank" =>
            generateAndSave(new DecoderBank(bankParams), "DecoderBank")

        case "PE" =>
            generateAndSave(new PE(peParams), "PE")

        case "Array" =>
            generateAndSave(
              new SystolicArray(saParams, peParams),
              "SystolicArray"
            )

        case "WeightSRAM" =>
            generateAndSave(new WeightSRAM(sramParams), "WeightSRAM")

        case "WeightLoader" =>
            generateAndSave(new WeightLoader(sramParams), "WeightLoader")

        case "Top" =>
            generateAndSave(new TopAccelerator(topParams), "TopAccelerator")

        case _ =>
            println(s"Error: Unknown module name '$moduleName'")
            println(
              "Available options: DecodeUnit, Core, Bank, PE, Array, WeightSRAM, WeightLoader, Top"
            )
            sys.exit(1)
    }
}
