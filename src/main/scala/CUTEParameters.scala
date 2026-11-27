
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile._

/**
  * CUTEBase cannot depend on XSAICache, but its loader configuration needs
  * the exact wire widths of the matrix-prefetch sideband. Keep this encoding
  * in lockstep with coupledL2.prefetch.MatrixPrefetchStream/MatrixPrefetchTagCodec:
  * valid[19] | stream[18:16] | taskId[15:0].
  */
object CuteMatrixTraceTag {
  val streamWidth = 3
  val taskIdWidth = 16
  val width = 1 + streamWidth + taskIdWidth
}

object WrapInc
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === (n-1).U)
      Mux(wrap, 0.U, value + 1.U)
    }
  }
}

object WrapDec
{
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value - 1.U)(log2Ceil(n)-1,0)
    } else {
      val wrap = (value === 0.U)
      Mux(wrap, (n-1).U, value - 1.U)
    }
  }
}

object ResponseChannelHelper {
  sealed trait LoaderMode {
    def isLegacy: Boolean
    def responseChannelCount: Int
    def renderToken: String
  }

  case object LegacyLoaderMode extends LoaderMode {
    override val isLegacy: Boolean = true
    override val responseChannelCount: Int = 1
    override val renderToken: String = "L"
  }

  final case class ParameterizedLoaderMode(responseChannelCount: Int) extends LoaderMode {
    require(
      Seq(1, 2, 4, 8).contains(responseChannelCount),
      s"parameterized loader response channel count ($responseChannelCount) must be one of 1/2/4/8"
    )

    override val isLegacy: Boolean = false
    override val renderToken: String = responseChannelCount.toString
  }

  final case class LoaderBridgeChannelConfig(
    trLoader0: LoaderMode,
    trLoader1: LoaderMode,
    accLoader0: LoaderMode,
    accStorer0: LoaderMode,
    trStorer0: LoaderMode = LegacyLoaderMode
  ) {
    // Numeric TS0 limits requests only; legacy TS0 also serializes ACK delivery.
    def trStoreRequestChannelCount: Int = trStorer0.responseChannelCount
    def render: String = s"TL0${trLoader0.renderToken}TL1${trLoader1.renderToken}" +
      s"AL0${accLoader0.renderToken}TS0${trStorer0.renderToken}AS0${accStorer0.renderToken}"
  }

  final case class LoaderRouteDescriptor(
    name: String,
    mode: LoaderMode,
    bankCount: Int,
    bankIdOffset: Int
  ) {
    require(isPow2(bankCount), s"$name bank count ($bankCount) must be a power of two")
    val bankIdWidth: Int = log2Ceil(bankCount)
    val collisionProne: Boolean = !mode.isLegacy
    val responseChannelCount: Int = mode.responseChannelCount

    def extractBankId(sourceId: UInt): UInt =
      sourceId(bankIdOffset + bankIdWidth - 1, bankIdOffset)
  }

  private val LoaderBridgeChannelConfigPattern =
    "^TL0(L|1|2|4|8)TL1(L|1|2|4|8)AL0(L|1|2|4|8)TS0(L|1|2|4|8)AS0(L|1|2|4|8)$".r
  private val LegacyLoaderBridgeChannelConfigPattern =
    "^A(L|1|2|4|8)B(L|1|2|4|8)CL(L|1|2|4|8)CS(L|1|2|4|8)(?:AS(1|2|4|8))?$".r

  def parseLoaderMode(token: String): LoaderMode = token match {
    case "L" => LegacyLoaderMode
    case "1" => ParameterizedLoaderMode(1)
    case "2" => ParameterizedLoaderMode(2)
    case "4" => ParameterizedLoaderMode(4)
    case "8" => ParameterizedLoaderMode(8)
    case other =>
      require(requirement = false, s"unsupported loader mode token '$other', expected L/1/2/4/8")
      LegacyLoaderMode
  }

  def parseLoaderBridgeChannelConfig(config: String): LoaderBridgeChannelConfig = {
    config.trim.toUpperCase match {
      case LoaderBridgeChannelConfigPattern(tl0, tl1, al0, ts0, as0) =>
        LoaderBridgeChannelConfig(parseLoaderMode(tl0), parseLoaderMode(tl1),
          parseLoaderMode(al0), parseLoaderMode(as0), parseLoaderMode(ts0))
      case LegacyLoaderBridgeChannelConfigPattern(a, b, cl, cs, as) =>
        // Preserve old omitted-AS behavior: AL means numeric TS01, not TS0L.
        val storeWidth = Option(as).map(_.toInt).getOrElse(parseLoaderMode(a).responseChannelCount)
        LoaderBridgeChannelConfig(parseLoaderMode(a), parseLoaderMode(b),
          parseLoaderMode(cl), parseLoaderMode(cs), ParameterizedLoaderMode(storeWidth))
      case _ =>
        require(requirement = false,
          s"invalid LoaderBridgeChannelConfig '$config', expected TL0LTL1LAL0LTS0LAS0L, " +
            "TL08TL18AL08TS08AS08, or legacy A1B1CL1CS8[AS4]")
        LoaderBridgeChannelConfig(LegacyLoaderMode, LegacyLoaderMode, LegacyLoaderMode, LegacyLoaderMode)
    }
  }

  def banksPerGroup(respChannelCount: Int, bankCount: Int = 8): Int = {
    require(respChannelCount >= 1 && isPow2(respChannelCount), s"respChannelCount ($respChannelCount) must be a power of 2 and >= 1")
    require(bankCount >= respChannelCount, s"bankCount ($bankCount) must be >= respChannelCount ($respChannelCount)")
    require(bankCount % respChannelCount == 0, s"bankCount ($bankCount) must be divisible by respChannelCount ($respChannelCount)")
    bankCount / respChannelCount
  }

  def groupIdOfBank(bankId: Int, respChannelCount: Int, bankCount: Int = 8): Int = {
    require(bankId >= 0 && bankId < bankCount, s"bankId ($bankId) must be within [0, $bankCount)")
    bankId / banksPerGroup(respChannelCount, bankCount)
  }

  def groupIdOfBank(bankId: UInt, respChannelCount: Int, bankCount: Int): UInt = {
    val perGroup = banksPerGroup(respChannelCount, bankCount)
    if (respChannelCount == 1) {
      0.U(1.W)
    } else {
      (bankId / perGroup.U)(log2Ceil(respChannelCount) - 1, 0)
    }
  }

  def groupBankBase(groupId: Int, respChannelCount: Int, bankCount: Int = 8): Int = {
    require(groupId >= 0 && groupId < respChannelCount, s"groupId ($groupId) must be within [0, $respChannelCount)")
    groupId * banksPerGroup(respChannelCount, bankCount)
  }

  def banksInGroup(groupId: Int, respChannelCount: Int, bankCount: Int = 8): Seq[Int] = {
    val base = groupBankBase(groupId, respChannelCount, bankCount)
    val perGroup = banksPerGroup(respChannelCount, bankCount)
    base until (base + perGroup)
  }
}


class DebugInfoIO()(implicit p: Parameters) extends CuteBundle{
    val DebugTimeStampe = UInt(64.W)
}

case object CuteParamsKey extends Field[CuteParams]

class WithCuteParams(cuteParams: CuteParams) extends Config((site, here, up) => {
    case CuteParamsKey => cuteParams
})

case object MteComputeType extends Field[UInt] {
    val ComputeTypeBitWidth = 4
    val ComputeTypeUndef = 15.U(ComputeTypeBitWidth.W)

    val I8I8I32 = 0.U(ComputeTypeBitWidth.W)
    val F16F16F32 = 1.U(ComputeTypeBitWidth.W)
    val BF16BF16F32 = 2.U(ComputeTypeBitWidth.W)
    val TF32TF32F32 = 3.U(ComputeTypeBitWidth.W)
    val I8U8I32 = 4.U(ComputeTypeBitWidth.W)
    val U8I8I32 = 5.U(ComputeTypeBitWidth.W)
    val U8U8I32 = 6.U(ComputeTypeBitWidth.W)
    val Mxfp8e4m3F32 = 7.U(ComputeTypeBitWidth.W)
    val Mxfp8e5m2F32 = 8.U(ComputeTypeBitWidth.W)
    val Nvfp4F32 = 9.U(ComputeTypeBitWidth.W)
    val Mxfp4F32 = 10.U(ComputeTypeBitWidth.W)
    val Fp8e4m3F32 = 11.U(ComputeTypeBitWidth.W)
    val Fp8e5m2F32 = 12.U(ComputeTypeBitWidth.W)
}

case class MatrixIsaParams(
    enableInt4Int32: Boolean = false,
    enableInt8Int32: Boolean = false,
    enableFp8Fp32: Boolean = false,
    enableFp8Fp16: Boolean = false,
    enableFp8Bf16: Boolean = false,
    enableFp16Fp16: Boolean = false,
    enableBf16Fp32: Boolean = false,
    enableFp16Fp32: Boolean = false,
    enableTf32Fp32: Boolean = false,
    enableMxfp4Fp32: Boolean = false,
    enableMxfp8Fp32: Boolean = false,
    enableNvfp4Fp32: Boolean = false,
) {

    def enableMatrix: Boolean =
        enableInt4Int32 || enableInt8Int32 ||
        enableFp8Fp32 || enableFp8Fp16 || enableFp8Bf16 ||
        enableFp16Fp16 || enableBf16Fp32 || enableFp16Fp32 ||
        enableTf32Fp32 ||
        enableMxfp4Fp32 || enableMxfp8Fp32 || enableNvfp4Fp32
    
    def enable4BitSrc: Boolean =
        enableInt4Int32 || enableMxfp4Fp32 || enableNvfp4Fp32

    def enable8BitSrc: Boolean =
        enableInt8Int32 ||
        enableFp8Fp32 || enableFp8Fp16 || enableFp8Bf16 ||
        enableMxfp8Fp32
    
    def enable16BitSrc: Boolean =
        enableFp16Fp16 || enableBf16Fp32 || enableFp16Fp32
    
    def enable32BitSrc: Boolean =
        enableTf32Fp32
    
    def enable64BitSrc: Boolean = false

    def enable4BitDst: Boolean = false
    
    def enable8BitDst: Boolean = false

    def enable16BitDst: Boolean =
        enableFp8Fp16 || enableFp8Bf16 || enableFp16Fp16
    
    def enable32BitDst: Boolean =
        enableInt4Int32 || enableInt8Int32 ||
        enableFp8Fp32 ||
        enableBf16Fp32 || enableFp16Fp32 ||
        enableTf32Fp32
    
    def enable64BitDst: Boolean = false

    def enableScalingFactor: Boolean =
        enableNvfp4Fp32 || enableMxfp4Fp32 || enableMxfp8Fp32
    def enableFp4withsf: Boolean =
        enableNvfp4Fp32 || enableMxfp4Fp32
}

trait CuteParamsKey{
  implicit val p: Parameters
  def CuteParams: CuteParams = p(CuteParamsKey)

}


object CuteParams {

    // baseParams:
    def baseParams = CuteParams()

    // 256 bit external memory bus
    def TL256Params = baseParams.copy(
        outsideDataWidth = 256
    )

    //default simple debug
    def simpleDebugParams = baseParams.copy(
        Debug = CuteDebugParams.AccMemoryDebugEnable
    )

    //dram&L2 performance test
    def dram_L2_8Tops_PerformanceTestParams = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 512,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 32,
    )

    def CUTE_32Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 256,
        Tensor_K = 64,
        Matrix_MN = 16,
        ReduceWidthByte = 32,
    )

    def CUTE_16Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 128,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 64,
    )

    def CUTE_8Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 128,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 32,
        MatrixExtension = MatrixIsaParams(
            enableInt8Int32 = true,
            enableFp8Fp32 = true,
            enableFp8Fp16 = true,
            enableFp8Bf16 = true,
            enableFp16Fp16 = true,
            enableBf16Fp32 = true,
        ),
        // Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    def CUTE_4Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 256,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 64,
        MatrixExtension = MatrixIsaParams(
            enableInt8Int32 = true,
            enableFp8Fp32 = true,
            enableFp8Fp16 = true,
            enableFp8Bf16 = true,
            enableFp16Fp16 = true,
            enableBf16Fp32 = true,
        ),
        Debug = CuteDebugParams.AllDebugOn,
    )

    def CUTE_2Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 32,
        MatrixExtension = MatrixIsaParams(
            enableInt8Int32 = true,
            enableFp8Fp32 = true,
            enableFp8Fp16 = true,
            enableFp8Bf16 = true,
            enableFp16Fp16 = true,
            enableBf16Fp32 = true,
        ),
        // Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    def CUTE_1Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 64,
        // Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    def CUTE_05Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 32,
        // Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    def CUTE_512SCP(params: CuteParams) = params.copy(
        Tensor_MN = 512,
        Tensor_K = 64,
    )

    def CUTE_256SCP(params: CuteParams) = params.copy(
        Tensor_MN = 256,
        Tensor_K = 64,
    )

    def CUTE_128SCP(params: CuteParams) = params.copy(
        Tensor_MN = 128,
        Tensor_K = 64,
    )

    def CUTE_64SCP(params: CuteParams) = params.copy(
        Tensor_MN = 64,
        Tensor_K = 64,
    )

    def CUTE_32Tops_512SCP  = CUTE_512SCP(CUTE_32Tops)
    def CUTE_16Tops_512SCP  = CUTE_512SCP(CUTE_16Tops)
    def CUTE_8Tops_512SCP   = CUTE_512SCP(CUTE_8Tops)
    def CUTE_4Tops_512SCP   = CUTE_512SCP(CUTE_4Tops)
    def CUTE_16Tops_256SCP  = CUTE_256SCP(CUTE_16Tops)
    def CUTE_8Tops_256SCP   = CUTE_256SCP(CUTE_8Tops)
    def CUTE_4Tops_256SCP   = CUTE_256SCP(CUTE_4Tops)
    def CUTE_2Tops_256SCP   = CUTE_256SCP(CUTE_2Tops)
    def CUTE_8Tops_128SCP   = CUTE_128SCP(CUTE_8Tops)
    def CUTE_4Tops_128SCP   = CUTE_128SCP(CUTE_4Tops)
    def CUTE_2Tops_128SCP   = CUTE_128SCP(CUTE_2Tops)
    def CUTE_1Tops_128SCP   = CUTE_128SCP(CUTE_1Tops)
    def CUTE_4Tops_64SCP    =  CUTE_64SCP(CUTE_4Tops)
    def CUTE_2Tops_64SCP    =  CUTE_64SCP(CUTE_2Tops)
    def CUTE_1Tops_64SCP    =  CUTE_64SCP(CUTE_1Tops)
    def CUTE_05Tops_64SCP   =  CUTE_64SCP(CUTE_05Tops)


    def CUTE_4Tops_128SCP_debug   = CUTE_4Tops_128SCP.copy(
        Debug = CuteDebugParams.AllDebugOn
    )

    def CUTE_2Tops_debug = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 32,
        MatrixExtension = MatrixIsaParams(
            enableInt8Int32 = true,
            enableFp8Fp32 = true,
            enableFp8Fp16 = true,
            enableFp8Bf16 = true,
            enableFp16Fp16 = true,
            enableBf16Fp32 = true,
        ),
        Debug = CuteDebugParams.AllDebugOn,
    )
}

object Cutev3Params {

    // baseParams:
    def baseParams = CuteParams.baseParams

    // 256 bit external memory bus
    def TL256Params = baseParams.copy(
        outsideDataWidth = 256
    )

    //default simple debug
    def simpleDebugParams = baseParams.copy(
        Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    //dram&L2 performance test
    def dram_L2_8Tops_PerformanceTestParams = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 512,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 32,
        Debug = CuteDebugParams.TrLoader0DebugEnable
    )

    def CUTE_8Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 128,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 32,
        MatrixExtension = MatrixIsaParams(
            enableInt8Int32 = true,
            enableFp8Fp32 = true,
            enableFp8Fp16 = true,
            enableFp8Bf16 = true,
            enableFp16Fp16 = true,
            enableBf16Fp32 = true,
        ),
        // Debug = CuteDebugParams.TrLoader0DebugEnable
    )


}

object CuteDebugParams {

  // NoDebugParams:
  def NoDebug = CuteDebugParams()

  def TrLoader0DebugEnable = NoDebug.copy(
    YJPTrLoader0DebugEnable = true,
  )

  def AccMemoryDebugEnable = NoDebug.copy(
    YJPAccMemoryDebugEnable = true,
  )

  def ComputeDebugeNable = NoDebug.copy(
    // YJPMACDebugEnable = true,
    YJPAccMemoryDebugEnable = true,
    // YJPDebugEnable = true,
  )

  def AllDebugOn = NoDebug.copy(
    YJPDebugEnable = true,
    YJPADCDebugEnable = true,
    YJPBDCDebugEnable = true,
    YJPCDCDebugEnable = true,
    YJPTrLoader0DebugEnable = true,
    YJPTrLoader1DebugEnable = true,
    YJPAccMemoryDebugEnable = true,
    YJPMACDebugEnable = true,
  )
}

case class CuteDebugParams(
    val YJPDebugEnable :Boolean     = false,
    val YJPADCDebugEnable :Boolean  = false,
    val YJPBDCDebugEnable :Boolean  = false,
    val YJPCDCDebugEnable :Boolean  = false,
    val YJPTrLoader0DebugEnable :Boolean  = false,
    val YJPTrLoader1DebugEnable :Boolean  = false,
    val YJPAccMemoryDebugEnable :Boolean  = false,
    val YJPMACDebugEnable :Boolean  = false,
)

object CuteFPEParams {

    def baseparams = CuteFPEParams()

}

case class CuteFPEParams(
    val fp8cmptreelayers :Int = 4,
    // Number of adders in the first FP4 reduction stage
    val FP4P0AddNum :Int = 2,
)

case class CuteParams(
    val outsideDataWidth :Int = 512, //CUTE external memory bandwidth

    val L2NBanks :Int = 4,

    val MMUAddrWidth :Int = 64 , //CUTE MMU address width

    val LLCSourceMaxNum :Int = 64, //maximum number of sources on the LLC bus -> this parameter is tightly coupled to LLC latency; to sustain full throughput, sourceMAXnum must exceed LLC latency
    val MemorysourceMaxNum :Int = 64, //maximum number of sources on the memory bus -> this parameter is tightly coupled to memory latency; to sustain full throughput, sourceMAXnum must exceed memory latency

    //tensor shape stored in MatrixReg
    val Tensor_MN :Int = 128,   //M and N dimensions of the tensor to be stored here
    val Tensor_K :Int = 64,    //K size of the tensor to be stored here (8-bit/element)

    //MTE matrix-multiply unit shape
    val Matrix_MN :Int = 4,     //Matrix_MN: M and N dimensions of the matrix multiply executed by TE
    val ReduceWidthByte :Int = 64,   //ReduceWidthByte: data width used by ReducePE for inner products, in bytes

    val TrLoader0ReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering trLoader0 data to CCSP
    val TrLoader1ReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering trLoader1 data to CCSP
    val AccStorer0ReadFromMatrixRegFIFODepth :Int = 4, //FIFO for buffering CCSP data to accMemory
    val AccLoader0ReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering accMemory data to CMReg

    // TaskController issue window depth (compile-time only)
    val TaskCtrlIssueWindowDepth :Int = 8,

    // Number of synchronization registers implemented by the core.
    val MsyncRegs :Int = 32,

    val EnableDifftest: Boolean = false, //whether DiffTest is enabled
    val DifftestAsyncClockName: Option[String] = None,

    // Statically elaborate matrix-prefetch descriptors, control signals, and request tags.
    val EnableMatrixPrefetch: Boolean = false,

    val Debug : CuteDebugParams = CuteDebugParams.NoDebug, //debug parameters

    val FPEparams: CuteFPEParams = CuteFPEParams.baseparams, //FPE parameters

    val MatrixExtension: MatrixIsaParams = MatrixIsaParams(),

    // User-facing loader response mode selector.
    // L means legacy single-channel loader; 1/2/4/8 mean parameterized bridge width.
    // Explicit physical-FU modes; the parser also accepts the old A/B/CL/CS[AS] grammar.
    val LoaderBridgeChannelConfig: String = "TL0LTL1LAL0LTS0LAS0L",

    // Physical-design-oriented queue depths for multi-channel loaders / bridge.
    val TrLoadResponseFifoDepth: Int = 2,
    val AccLoader0FillQueueDepth: Int = 2,
    val ResponseBridgeQueueDepth: Int = 8,

    // When enabled, CStore ack path bypasses the response bridge and only counts total acks.
    val AccStorer0DirectAckCountMode: Boolean = true,

    // Elastic safety buffer on every parameterized Omega input.
    val EnableOmegaInputQueue: Boolean = true,

    // Shared trLoader0/trLoader1 request-side C-tail mitigation; transpose and legacy paths stay natural.
    val ABBatchScheduleEnable: Boolean = true,
    val ABBatchScheduleMode: String = ABBatchSchedule.Rotate,
    val ABBatchRotateOffset: Int = 1,
    val ABBatchPermutationMul: Int = 17
) {
    val parsedLoaderBridgeChannelConfig = ResponseChannelHelper.parseLoaderBridgeChannelConfig(LoaderBridgeChannelConfig)
    val TrLoader0ChannelMode = parsedLoaderBridgeChannelConfig.trLoader0
    val TrLoader1ChannelMode = parsedLoaderBridgeChannelConfig.trLoader1
    val AccLoader0ChannelMode = parsedLoaderBridgeChannelConfig.accLoader0
    val AccStorer0ChannelMode = parsedLoaderBridgeChannelConfig.accStorer0
    val TrStorer0RequestChannelCount = parsedLoaderBridgeChannelConfig.trStoreRequestChannelCount
    val TrStorer0ChannelMode = parsedLoaderBridgeChannelConfig.trStorer0
    // Old omitted-AS configurations retain their previous parameter checks.
    if (LoaderBridgeChannelConfig.trim.toUpperCase.startsWith("TL0") ||
        LoaderBridgeChannelConfig.trim.toUpperCase.matches(".*AS[1248]$")) {
      require(TrStorer0RequestChannelCount <= TrMatrixRegNBanks,
        s"TS0 request width ($TrStorer0RequestChannelCount) exceeds TR banks ($TrMatrixRegNBanks)")
    }

    val TrLoader0RouteDescriptor = ResponseChannelHelper.LoaderRouteDescriptor(
      "A", TrLoader0ChannelMode, TrMatrixRegNBanks, log2Ceil(TrMatrixRegBankNEntries))
    val TrLoader1RouteDescriptor = ResponseChannelHelper.LoaderRouteDescriptor(
      "B", TrLoader1ChannelMode, TrMatrixRegNBanks, log2Ceil(TrMatrixRegBankNEntries))
    val AccLoader0RouteDescriptor = ResponseChannelHelper.LoaderRouteDescriptor(
      "CLoad", AccLoader0ChannelMode, AccMatrixRegNBanks, log2Ceil(AccMatrixRegBankNEntries))
    val AccStorer0RouteDescriptor = ResponseChannelHelper.LoaderRouteDescriptor(
      "CStore", AccStorer0ChannelMode, AccMatrixRegNBanks, log2Ceil(AccMatrixRegBankNEntries))

    require(
      AccLoader0ChannelMode.isLegacy == AccStorer0ChannelMode.isLegacy,
      s"mixed legacy/parameterized C loader modes are not supported now: ${parsedLoaderBridgeChannelConfig.render}"
    )
    require(TrMatrixRegNBanks >= 1 && isPow2(TrMatrixRegNBanks),
      s"TrMatrixRegNBanks ($TrMatrixRegNBanks) must be a positive power of two")
    require(AccMatrixRegNBanks >= 1 && isPow2(AccMatrixRegNBanks),
      s"AccMatrixRegNBanks ($AccMatrixRegNBanks) must be a positive power of two")
    require(L2NBanks >= 1 && isPow2(L2NBanks),
      s"L2NBanks ($L2NBanks) must be a positive power of two")
    Seq(
      "A" -> TrLoader0ChannelMode,
      "B" -> TrLoader1ChannelMode,
      "CLoad" -> AccLoader0ChannelMode,
      "CStore" -> AccStorer0ChannelMode
    ).foreach { case (name, mode) =>
      require(Seq(1, 2, 4, 8).contains(mode.responseChannelCount),
        s"$name response channel count (${mode.responseChannelCount}) must be one of 1/2/4/8")
    }
    require(ABBatchSchedule.modes.contains(ABBatchScheduleMode), "unsupported AB batch schedule mode")
    require(ABBatchRotateOffset >= 0 && ABBatchPermutationMul > 0,
        "AB batch offset must be non-negative and multiplier must be positive")
    if (L2NBanks == 8 && parsedLoaderBridgeChannelConfig.copy(trStorer0 = ResponseChannelHelper.LegacyLoaderMode) ==
      ResponseChannelHelper.LoaderBridgeChannelConfig(
        ResponseChannelHelper.ParameterizedLoaderMode(8),
        ResponseChannelHelper.ParameterizedLoaderMode(8),
        ResponseChannelHelper.ParameterizedLoaderMode(8),
        ResponseChannelHelper.ParameterizedLoaderMode(8))) {
      require(TrMatrixRegNBanks == L2NBanks && AccMatrixRegNBanks == L2NBanks,
        s"eight-bank perfect batching requires L2 and MatrixReg bank identity: " +
          s"L2=$L2NBanks AB=$TrMatrixRegNBanks C=$AccMatrixRegNBanks")
    }

    // require(ReduceWidthByte == 64, "FP8/4 now only support 512 bit reduce width")
    require(outsideDataWidth == 512 || outsideDataWidth == 256, "currently only support 512/256 bit outsideDataWidth")
    require(ReduceWidthByte == 64 || ReduceWidthByte == 32, "currently only support 512/256 bit ReduceWidth")
    require(outsideDataWidth >= ReduceWidthByte * 8, "outsideDataWidth must be larger than or equal to ReduceWidthByte")
    require((outsideDataWidth & (outsideDataWidth - 1)) == 0, "outsideDataWidth must be power of 2")
    // require((MMUAddrWidth & (MMUAddrWidth - 1)) == 0, "MMUAddrWidth must be power of 2" )
    require((LLCSourceMaxNum & (LLCSourceMaxNum - 1)) == 0, "LLCSourceMaxNum must be power of 2")
    require((MemorysourceMaxNum & (MemorysourceMaxNum - 1)) == 0, "MemorysourceMaxNum must be power of 2")
    require((Tensor_MN & (Tensor_MN - 1)) == 0, "Tensor_MN must be power of 2")
    require((Tensor_K & (Tensor_K - 1)) == 0, "Tensor_K must be power of 2")
    require((Matrix_MN & (Matrix_MN - 1)) == 0, "Matrix_MN must be power of 2")
    require((ReduceWidthByte & (ReduceWidthByte - 1)) == 0, "ReduceWidthByte must be power of 2")
    require((TrLoader0ReadFromMemoryFIFODepth & (TrLoader0ReadFromMemoryFIFODepth - 1)) == 0, "TrLoader0ReadFromMemoryFIFODepth must be power of 2")
    require((TrLoader1ReadFromMemoryFIFODepth & (TrLoader1ReadFromMemoryFIFODepth - 1)) == 0, "TrLoader1ReadFromMemoryFIFODepth must be power of 2")
    require((AccStorer0ReadFromMatrixRegFIFODepth & (AccStorer0ReadFromMatrixRegFIFODepth - 1)) == 0, "AccStorer0ReadFromMatrixRegFIFODepth must be power of 2")
    require((AccLoader0ReadFromMemoryFIFODepth & (AccLoader0ReadFromMemoryFIFODepth - 1)) == 0, "AccLoader0ReadFromMemoryFIFODepth must be power of 2")
    require(TrLoadResponseFifoDepth >= 1, "TRLoadResponseFifoDepth must be >= 1")
    require(AccLoader0FillQueueDepth >= 1, "AccLoader0FillQueueDepth must be >= 1")
    require(ResponseBridgeQueueDepth >= 1, "ResponseBridgeQueueDepth must be >= 1")
    require(Seq(4, 8, 16).contains(TaskCtrlIssueWindowDepth), "TaskCtrlIssueWindowDepth only supports 4/8/16")
    require(Seq(8, 16, 32).contains(MsyncRegs), "MsyncRegs only supports 8/16/32")

    def cuteFpeConfig: CuteFpeConfig = CuteFpeConfig(
        enableInt8Int32 = MatrixExtension.enableInt8Int32,
        enableTf32Fp32 = MatrixExtension.enableTf32Fp32,
        enableFp16Fp32 = MatrixExtension.enableFp16Fp32,
        enableMxfp4Fp32 = MatrixExtension.enableMxfp4Fp32,
        enableMxfp8Fp32 = MatrixExtension.enableMxfp8Fp32,
        enableNvfp4Fp32 = MatrixExtension.enableNvfp4Fp32
    )
    def cuteFpeParams: CuteFpeParameters = CuteFpeParametersFromCuteParams(this)

    def outsideDataWidthByte = outsideDataWidth / 8
    def ReduceWidth = ReduceWidthByte * 8
    def TrLoadNeedsFillTable = ReduceWidthByte < outsideDataWidthByte //buffer returned memory data when it cannot be written in one cycle
    def ResultWidth = CuteFpeFormat.ResultWidth
    def ResultWidthByte = ResultWidth / 8
    def MMUDataWidth = outsideDataWidth //MMU data bus width
    def MMUMaskWidth = MMUDataWidth / 8 //MMU mask width
    def MMUDataWidthBitSize = log2Ceil(MMUDataWidth) + 1 //effective data-bit count of the MMU data bus
    def LLCSourceMaxNumBitSize = log2Ceil(LLCSourceMaxNum) + 1
    def MemorysourceMaxNumBitSize = log2Ceil(MemorysourceMaxNum) + 1
    def SoureceMaxNum = math.max(LLCSourceMaxNum, MemorysourceMaxNum)
    def SoureceMaxNumBitSize = log2Ceil(SoureceMaxNum) + 1


    def ReduceGroupSize  = Tensor_K/ReduceWidthByte    //number of ReduceVectors for K to be stored, not the K size
    def mmaMaxMNGroupsPerAxis: Int = Tensor_MN / Matrix_MN
    def mmaMNGroupCountWidth: Int = log2Ceil(mmaMaxMNGroupsPerAxis + 1)
    def mmaKGroupCountWidth: Int = log2Ceil(ReduceGroupSize + 1)
    def mmaIterationCountWidth: Int = {
        val maxIterations = mmaMaxMNGroupsPerAxis * mmaMaxMNGroupsPerAxis * ReduceGroupSize
        log2Ceil(maxIterations + 1)
    }
    def MatrixRegMaxTensorDim = Math.max(Tensor_MN, Math.max(Tensor_MN, ReduceGroupSize))
    def MatrixRegMaxTensorDimBitSize = log2Ceil(MatrixRegMaxTensorDim) + 1
    //A tensor shape stored in MatrixReg is M*K
    //A MatrixReg size is Tenser_M * ReduceGroupSize * ReduceWidthByte
    //128*(4*256/8); one read covers a 128*128 tensor
    //one compute takes (128/4)*(128/4)*4 = 4096 cycles; one read takes 128*4 = 512 cycles.
    //sequential reads must be considered; MatrixReg should be banked
    def TrMatrixRegSize = Tensor_MN * ReduceGroupSize * ReduceWidthByte //reduce
    def AccMatrixRegSize = Tensor_MN * Tensor_MN * ResultWidthByte //result

    //the current MatrixReg design is split into Tensor_T banks; each access fetches Tensor_T items from different banks and concatenates them
    def TrMatrixRegEntryByteSize = ReduceWidthByte //bandwidth suitable for feeding TE
    def AccMatrixRegEntryByteSize = Matrix_MN*ResultWidthByte //bandwidth for reads and writes
    def TrMatrixRegEntryBitSize = ReduceWidthByte * 8 //bandwidth suitable for feeding TE
    def AccMatrixRegEntryBitSize = Matrix_MN*ResultWidthByte * 8//bandwidth for reads and writes
    def TrMatrixRegNBanks = Matrix_MN //note that this is strongly tied to Matrix_MN and is generally an integer multiple of Matrix_MN
    def AccMatrixRegNBanks = Matrix_MN //convenient for reorder
    def Trans_Load_Size = outsideDataWidthByte / TrMatrixRegNBanks
    def TrMatrixReg_Total_Bandwidth = TrMatrixRegNBanks * TrMatrixRegEntryByteSize  //total bandwidth of TrMatrixReg
    def AccMatrixReg_Total_Bandwidth = AccMatrixRegNBanks * AccMatrixRegEntryByteSize  //total bandwidth of AccMatrixReg
    def TrMatrixReg_Total_Bandwidth_Bit = TrMatrixRegNBanks * TrMatrixRegEntryByteSize * 8  //total bandwidth of TrMatrixReg
    def AccMatrixReg_Total_Bandwidth_Bit = AccMatrixRegNBanks * AccMatrixRegEntryByteSize * 8  //total bandwidth of AccMatrixReg
    def TrMatrixRegBankSize = TrMatrixRegSize / TrMatrixRegNBanks
    def AccMatrixRegBankSize = AccMatrixRegSize / AccMatrixRegNBanks
    def TrMatrixRegBankNEntries = TrMatrixRegBankSize / TrMatrixRegEntryByteSize
    def AccMatrixRegBankNEntries = AccMatrixRegBankSize / AccMatrixRegEntryByteSize

    /**
     * Scale Factor Parameters
     */
    def ScaleWidth = ReduceWidthByte * 8 * CuteFpeFormat.ScaleElementWidth / CuteFpeFormat.MinDataTypeWidth / CuteFpeFormat.MinGroupSize   // maximum bit width needed for one group scale
    def ABScaleSize = Tensor_MN * ReduceGroupSize * ScaleWidth
    def ABScaleNSlices = outsideDataWidth / ScaleWidth / ReduceGroupSize  // maximum scale length for one Tensor_K per slice
    def ABScaleBankNEntries = ABScaleSize / (ABScaleNSlices * ScaleWidth * ReduceGroupSize)

    // require(ReduceGroupSize == 2, "ReduceGroupSize must be 2, Wait for update")
    require(outsideDataWidthByte <= Tensor_K, "outsideDataWidthByte must be less than or equal to Tensor_K, or a load will exceed the subtensor in micro load")
    require(outsideDataWidthByte % TrMatrixRegNBanks == 0, "outsideDataWidthByte must be divisible by TrMatrixRegNBanks for transpose load")

}

trait HasCuteParams {
    def cuteParams: CuteParams
    def cuteMatrixExtension: MatrixIsaParams = cuteParams.MatrixExtension
    def cuteFpeConfig: CuteFpeConfig = cuteParams.cuteFpeConfig

    def enableMteInt8: Boolean = cuteMatrixExtension.enableInt8Int32
    def enableMteFp8: Boolean = cuteMatrixExtension.enableFp8Fp32
    def enableMteFp16: Boolean = cuteMatrixExtension.enableFp16Fp32 || cuteMatrixExtension.enableFp16Fp16
    def enableMteBf16: Boolean = cuteMatrixExtension.enableBf16Fp32
    def enableMteTf32: Boolean = cuteMatrixExtension.enableTf32Fp32
    def enableMteNvfp4: Boolean = false
    def enableMteMxfp8: Boolean = false
    def enableMteMxfp4: Boolean = false

    def DebugParams: CuteDebugParams = cuteParams.Debug

    def TaskCtrlIssueWindowDepth = cuteParams.TaskCtrlIssueWindowDepth
    def TaskCtrlIssueWindowDepthBitSize = log2Ceil(TaskCtrlIssueWindowDepth)
    def CuteMsyncRegs = cuteParams.MsyncRegs
    def CuteMsyncRegIdxWidth = log2Ceil(CuteMsyncRegs)

    def DecodedAmuCtrlFIFODepth = TaskCtrlIssueWindowDepth  // Decoded AMU instruction FIFO depth, tied to the issue window
    def DecodedAmuCtrlFIFODepthBitSize = log2Ceil(DecodedAmuCtrlFIFODepth) // Bit width of the decoded AMU instruction FIFO depth

    def TrMatrixRegCount = 4
    def AccMatrixRegCount = 4
    def TrMatrixRegIdWidth = log2Ceil(TrMatrixRegCount)
    def AccMatrixRegIdWidth = log2Ceil(AccMatrixRegCount)
    def MatrixRegIdWidth = TrMatrixRegIdWidth max AccMatrixRegIdWidth

    def YJPDebugEnable      = DebugParams.YJPDebugEnable
    def YJPADCDebugEnable   = DebugParams.YJPADCDebugEnable
    def YJPBDCDebugEnable   = DebugParams.YJPBDCDebugEnable
    def YJPCDCDebugEnable   = DebugParams.YJPCDCDebugEnable
    def YJPTrLoader0DebugEnable   = DebugParams.YJPTrLoader0DebugEnable
    def YJPTrLoader1DebugEnable   = DebugParams.YJPTrLoader1DebugEnable
    def YJPAccMemoryDebugEnable   = DebugParams.YJPAccMemoryDebugEnable
    def YJPMACDebugEnable         = DebugParams.YJPMACDebugEnable

    def outsideDataWidth = cuteParams.outsideDataWidth
    def outsideDataWidthByte = cuteParams.outsideDataWidthByte
    def ReduceWidthByte = cuteParams.ReduceWidthByte
    def ReduceWidth = cuteParams.ReduceWidth
    def mxfp8ScaleWidth = ReduceWidth * 8 / 8 / 32 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 32]
    def nvfp4ScaleWidth = ReduceWidth * 8 / 4 / 16 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 16]
    def mxfp4ScaleWidth = ReduceWidth * 8 / 4 / 32 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 32]
    def TrLoadNeedsFillTable = cuteParams.TrLoadNeedsFillTable
    def ResultWidthByte = cuteParams.ResultWidthByte
    def ResultWidth = cuteParams.ResultWidth
    def MMUAddrWidth = cuteParams.MMUAddrWidth
    def MMUDataWidth = cuteParams.MMUDataWidth
    def MMUMaskWidth = cuteParams.MMUMaskWidth
    def MMUDataWidthBitSize = cuteParams.MMUDataWidthBitSize
    def LLCSourceMaxNum = cuteParams.LLCSourceMaxNum
    def LLCSourceMaxNumBitSize = cuteParams.LLCSourceMaxNumBitSize
    def MemorysourceMaxNum = cuteParams.MemorysourceMaxNum
    def MemorysourceMaxNumBitSize = cuteParams.MemorysourceMaxNumBitSize
    def SoureceMaxNum = cuteParams.SoureceMaxNum
    def SoureceMaxNumBitSize = cuteParams.SoureceMaxNumBitSize
    def Tensor_MN = cuteParams.Tensor_MN
    def Tensor_K = cuteParams.Tensor_K
    def MatrixRegMaxTensorDim = cuteParams.MatrixRegMaxTensorDim
    def MatrixRegMaxTensorDimBitSize = cuteParams.MatrixRegMaxTensorDimBitSize
    def TrMatrixRegSize = cuteParams.TrMatrixRegSize
    def AccMatrixRegSize = cuteParams.AccMatrixRegSize
    def Matrix_MN = cuteParams.Matrix_MN
    def TrMatrixRegEntryByteSize = cuteParams.TrMatrixRegEntryByteSize
    def AccMatrixRegEntryByteSize = cuteParams.AccMatrixRegEntryByteSize
    def TrMatrixRegEntryBitSize = cuteParams.TrMatrixRegEntryBitSize
    def AccMatrixRegEntryBitSize = cuteParams.AccMatrixRegEntryBitSize
    def TrMatrixRegNBanks = cuteParams.TrMatrixRegNBanks
    def AccMatrixRegNBanks = cuteParams.AccMatrixRegNBanks
    def Trans_Load_Size = cuteParams.Trans_Load_Size
    def TrMatrixReg_Total_Bandwidth = cuteParams.TrMatrixReg_Total_Bandwidth
    def AccMatrixReg_Total_Bandwidth = cuteParams.AccMatrixReg_Total_Bandwidth
    def TrMatrixReg_Total_Bandwidth_Bit = cuteParams.TrMatrixReg_Total_Bandwidth_Bit
    def AccMatrixReg_Total_Bandwidth_Bit = cuteParams.AccMatrixReg_Total_Bandwidth_Bit
    def TrMatrixRegBankSize = cuteParams.TrMatrixRegBankSize
    def AccMatrixRegBankSize = cuteParams.AccMatrixRegBankSize
    def TrMatrixRegBankNEntries = cuteParams.TrMatrixRegBankNEntries
    def AccMatrixRegBankNEntries = cuteParams.AccMatrixRegBankNEntries
    def ScaleWidth = cuteParams.ScaleWidth
    def ABScaleBankNEntries = cuteParams.ABScaleBankNEntries
    def ABScaleNSlices = cuteParams.ABScaleNSlices
    def TrLoader0ReadFromMemoryFIFODepth = cuteParams.TrLoader0ReadFromMemoryFIFODepth
    def TrLoader1ReadFromMemoryFIFODepth = cuteParams.TrLoader1ReadFromMemoryFIFODepth
    def AccStorer0ReadFromMatrixRegFIFODepth = cuteParams.AccStorer0ReadFromMatrixRegFIFODepth
    def AccLoader0ReadFromMemoryFIFODepth = cuteParams.AccLoader0ReadFromMemoryFIFODepth
    def TrLoadResponseFifoDepth = cuteParams.TrLoadResponseFifoDepth
    def AccLoader0FillQueueDepth = cuteParams.AccLoader0FillQueueDepth
    def ResponseBridgeQueueDepth = cuteParams.ResponseBridgeQueueDepth
    def AccStorer0DirectAckCountMode = cuteParams.AccStorer0DirectAckCountMode
    def ReduceGroupSize = cuteParams.ReduceGroupSize
    def EnableDifftest = cuteParams.EnableDifftest
    def EnableMatrixPrefetch = cuteParams.EnableMatrixPrefetch
    def DifftestAsyncClockName = cuteParams.DifftestAsyncClockName
    def L2NBanks = cuteParams.L2NBanks
    def LoaderBridgeChannelConfig = cuteParams.LoaderBridgeChannelConfig
    def EnableOmegaInputQueue = cuteParams.EnableOmegaInputQueue
    def ABBatchScheduleEnable: Boolean = cuteParams.ABBatchScheduleEnable
    def ABBatchScheduleMode: String = cuteParams.ABBatchScheduleMode
    def ABBatchRotateOffset: Int = cuteParams.ABBatchRotateOffset
    def ABBatchPermutationMul: Int = cuteParams.ABBatchPermutationMul
    def parsedLoaderBridgeChannelConfig = cuteParams.parsedLoaderBridgeChannelConfig
    def TrLoader0ChannelMode = cuteParams.TrLoader0ChannelMode
    def TrLoader1ChannelMode = cuteParams.TrLoader1ChannelMode
    def AccLoader0ChannelMode = cuteParams.AccLoader0ChannelMode
    def AccStorer0ChannelMode = cuteParams.AccStorer0ChannelMode
    def TrLoader0UseLegacyLoader = TrLoader0ChannelMode.isLegacy
    def TrLoader1UseLegacyLoader = TrLoader1ChannelMode.isLegacy
    def AccLoader0UseLegacyLoader = AccLoader0ChannelMode.isLegacy
    def AccStorer0UseLegacyLoader = AccStorer0ChannelMode.isLegacy
    def TrStorer0UseLegacyLoader = cuteParams.TrStorer0ChannelMode.isLegacy
    def TrStorer0RequestChannelCount = cuteParams.TrStorer0RequestChannelCount
    def TrLoader0ResponseChannelCount = TrLoader0ChannelMode.responseChannelCount
    def TrLoader1ResponseChannelCount = TrLoader1ChannelMode.responseChannelCount
    def AccLoader0BridgeResponseChannelCount = AccLoader0ChannelMode.responseChannelCount
    def AccStorer0BridgeResponseChannelCount = AccStorer0ChannelMode.responseChannelCount
    def TrLoader0RouteDescriptor = cuteParams.TrLoader0RouteDescriptor
    def TrLoader1RouteDescriptor = cuteParams.TrLoader1RouteDescriptor
    def AccLoader0RouteDescriptor = cuteParams.AccLoader0RouteDescriptor
    def AccStorer0RouteDescriptor = cuteParams.AccStorer0RouteDescriptor

    def ScaleVecWidth(computeType : UInt) : UInt = {
        val scaleVecWidth = Wire(UInt(4.W))
        scaleVecWidth := 0.U
        switch(computeType){
        is (MteComputeType.Mxfp8e4m3F32) { scaleVecWidth := (ReduceWidthByte * 8 / 8 / 32).U }
        is (MteComputeType.Mxfp8e5m2F32) { scaleVecWidth := (ReduceWidthByte * 8 / 8 / 32).U }
        is (MteComputeType.Nvfp4F32) { scaleVecWidth := (ReduceWidthByte * 8 / 4 / 16).U }
        is (MteComputeType.Mxfp4F32) { scaleVecWidth := (ReduceWidthByte * 8 / 4 / 32).U }
        }
        scaleVecWidth
    }

    def DEBUG_FP8 = false
    def DEBUG_FP4 = false
}

case class CuteFpeParametersFromCuteParams(cuteParams: CuteParams) extends CuteFpeParameters {
    private def fpeParams: CuteFPEParams = cuteParams.FPEparams

    def cuteFpeConfig: CuteFpeConfig = cuteParams.cuteFpeConfig

    def ReduceWidth: Int = cuteParams.ReduceWidth
    def fp8cmptreelayers: Int = fpeParams.fp8cmptreelayers

    def FP4P0AddNum: Int = fpeParams.FP4P0AddNum

    def DEBUG_FP8: Boolean = false
    def DEBUG_FP4: Boolean = false
}

trait CUTEImplParameters extends HasCuteParams {
    implicit val p: Parameters
    def cuteParams: CuteParams = p(CuteParamsKey)
    def cuteFpeParameters: Parameters = p.alterPartial({
        case CuteFpeParamsKey => cuteParams.cuteFpeParams
    })
}

class CuteModule(implicit val p: Parameters) extends Module with CUTEImplParameters
class CuteBundle(implicit val p: Parameters) extends Bundle with CUTEImplParameters

class AfterOpsInterface()(implicit p: Parameters) extends CuteBundle{

    //can accept per-cycle data from CDC matching MReg and TE width, and perform splitting, reordering, scaling, transposition, and other complex vector tasks inside this module
    val CDCDataToInterface     = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
    val InterfaceToCDCData     = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
    // val CDCStoreAddr                        = Input(UInt(log2Ceil(AccMatrixRegBankNEntries).W))

    val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input(vectorWidth: Int = 256)(implicit p: Parameters) extends CuteBundle{
    require(isPow2(vectorWidth))
    val inst_uop = Output(UInt(32.W))
    val inst_src0 = Output(UInt(vectorWidth.W))
    val inst_src1 = Output(UInt(vectorWidth.W))
    val inst_src0_type = Output(UInt(2.W))//from register or input
    val inst_src1_type = Output(UInt(2.W))//from register or input
    val inst_dest_type = Output(UInt(2.W))//write back to register or output
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data ID
}

class VPUInterface_Output(vectorWidth: Int = 256)(implicit p: Parameters) extends CuteBundle{
    require(isPow2(vectorWidth))
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data ID
    val stream_data = Output(UInt(vectorWidth.W))//stream data can carry extra information that is also returned; later it can be used to configure some VPU implicit registers or kept in VPU implicit registers visible to uops, such as the next scale or bias
}

class VPUInterfaceIO(vectorWidth: Int = 256)(implicit p: Parameters) extends CuteBundle{
    val VPU_Input = (DecoupledIO(new VPUInterface_Input(vectorWidth)))
    val VPU_Output = Flipped(DecoupledIO(new VPUInterface_Output(vectorWidth)))
}


class VectorInterfaceIO(vectorWidth: Int = 256, instBufferCount: Int = 8)(implicit p: Parameters) extends CuteBundle{

    require(isPow2(vectorWidth) && isPow2(instBufferCount))

    //can accept per-cycle data from AfterOpsInterface matching vectorWidth
    val VecTask = DecoupledIO(UInt(log2Ceil(instBufferCount).W))
    val VectorDataIn     = DecoupledIO(UInt((vectorWidth).W))
    val VectorDataOut     = Flipped(DecoupledIO(UInt((vectorWidth).W)))
}


case object StreamStateType extends Field[UInt]{
    val StreamStateTypeBitWidth = 4
    val NoReorder = 0.U(StreamStateTypeBitWidth.W)
    val Reorder_DIM_N_First = 1.U(StreamStateTypeBitWidth.W)
    val Reorder_DIM_M_First = 2.U(StreamStateTypeBitWidth.W)
}

class CUTE_uop()(implicit p: Parameters) extends CuteBundle{
    val Stream_state = UInt((StreamStateType.StreamStateTypeBitWidth).W)
    val Stream_uop = UInt(32.W)
    val Element_uop = UInt(32.W)
    val Vector_uop = UInt(32.W)


}

class AfterOpsMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_C = (new Bundle{
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val ApplicationTensor_D = (new Bundle{
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))

    //tasks that accept post-ops may be reorder, scaling, transpose, or other complex post-processing tasks
    val Is_Transpose                        = (Bool())      //whether transpose is needed
    val Is_Reorder_Only_Ops                 = (Bool())      //whether this is reorder-only, with no computation needed
    val Is_EasyScale_Only_Ops               = (Bool())      //whether this is simple scaling only, without extra post-op computation
    val Is_VecFIFO_Ops                      = (Bool())      //whether the generic VecFIFO is actually needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged

    val CUTEuop                         = (new CUTE_uop)
}

class ADCMicroTaskConfigBaseIO()(implicit p: Parameters) extends CuteBundle{
    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(TrMatrixRegIdWidth.W)

    val Is_Transpose                        = (Bool())      //whether transpose is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged
}

class ADCMicroTaskConfigIO()(implicit p: Parameters) extends ADCMicroTaskConfigBaseIO {
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class ASCMicroTaskConfigIO()(implicit p: Parameters) extends ADCMicroTaskConfigBaseIO {
    val computeType                       = UInt(MteComputeType.ComputeTypeBitWidth.W)
}

class BDCMicroTaskConfigBaseIO()(implicit p: Parameters) extends CuteBundle{
    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(TrMatrixRegIdWidth.W)

    val Is_Transpose                        = (Bool())      //whether transpose is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                        = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged
}

class BDCMicroTaskConfigIO()(implicit p: Parameters) extends BDCMicroTaskConfigBaseIO {
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class BSCMicroTaskConfigIO()(implicit p: Parameters) extends BDCMicroTaskConfigBaseIO {
    val computeType                       = UInt(MteComputeType.ComputeTypeBitWidth.W)
}

class CDCMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_C = (new Bundle{
        // val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
        // val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val ApplicationTensor_D = (new Bundle{
        // val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
        // val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(AccMatrixRegIdWidth.W)

    val Is_Transpose                        = (Bool())      //whether transpose is needed
    val Is_AfterOps_Tile                    = (Bool())      //whether this is a tile that requires post-ops, including transpose

    val Is_Reorder_Only_Ops                 = (Bool())      //whether this is reorder-only, with no computation needed
    val Is_EasyScale_Only_Ops               = (Bool())      //whether this is simple scaling only, without extra post-op computation
    val Is_VecFIFO_Ops                      = (Bool())      //whether the generic VecFIFO is actually needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged
    val MicroTask_TEComputeEndValid         = Flipped(Bool())//current TE compute task completed (post-ops still pending), but TE occupancy can be released early
    val MicroTask_TEComputeEndReady         = (Bool())       //current TE compute task completion acknowledged

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class TrLoadTensorInfo()(implicit p: Parameters) extends CuteBundle{
    val baseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))//may be gone already
    val stride    = (UInt(MMUAddrWidth.W))//address offset increment for the next M
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    val HasTail                         = Bool()
    val TailByteMask                    = UInt(log2Ceil(outsideDataWidthByte + 1).W)
    val K_Beat_Count                    = UInt(MatrixRegMaxTensorDimBitSize.W)
}

class ApplicationScale_A_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationScale_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockScale_A_BaseVaddr         = (UInt(MMUAddrWidth.W))  // main active field
    val computeType                     = (UInt(MteComputeType.ComputeTypeBitWidth.W))
}

class TrLoadTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val Tensor = new TrLoadTensorInfo
    
    val LoadTaskInfo = (new LoadTask_Info)

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(TrMatrixRegIdWidth.W)
    val PrefetchTaskId                    = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.taskIdWidth.W))
    val PrefetchStream                    = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.streamWidth.W))

    val Conherent                           = (Bool())      //whether coherence is needed
    val Is_Transpose                        = (Bool())      //whether transpose is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                        = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class ASLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationScale_A = (new ApplicationScale_A_Info)

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))

    val Conherent                           = (Bool())      //whether coherence is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged
}

class BSLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationScale_B = (new ApplicationScale_B_Info)

    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))

    val Conherent                           = (Bool())      //whether coherence is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged
}

class ApplicationScale_B_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationScale_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockScale_B_BaseVaddr         = (UInt(MMUAddrWidth.W))   // main active field
    val computeType                     = (UInt(MteComputeType.ComputeTypeBitWidth.W))
}

class ApplicationTensor_C_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_C_Stride_M    = (UInt(MMUAddrWidth.W))//address offset increment for the next M
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    val HasTail                         = Bool()
    val TailByteMask                    = UInt(log2Ceil(outsideDataWidthByte + 1).W)
    val N_Beat_Count                    = UInt(MatrixRegMaxTensorDimBitSize.W)
}

class ApplicationTensor_D_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_D_Stride_M    = (UInt(MMUAddrWidth.W))//address offset increment for the next M
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class LoadTask_Info()(implicit p: Parameters) extends CuteBundle{
    val Is_ZeroLoad = (Bool())
    val Is_RepeatRowLoad = (Bool())
    val Is_FullLoad = (Bool())
}
class AccMemoryTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    //this is Tensor C, the invariant part from the accumulator-register perspective

    val ApplicationTensor_C = (new ApplicationTensor_C_Info)

    val ApplicationTensor_D = (new ApplicationTensor_D_Info)

    val LoadTaskInfo = (new LoadTask_Info)

    val Conherent                           = (Bool())      //whether coherence is needed
    val Is_Transpose                        = (Bool())      //whether transpose is needed
    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(AccMatrixRegIdWidth.W)
    val PrefetchTaskId                    = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.taskIdWidth.W))
    val PrefetchStream                    = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.streamWidth.W))
    // C-store writebacks do not train L2 prefetching. Keep a separate
    // trace-only tag so ChiselDB can still identify their issuing task.
    val StoreTraceTag                     = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.width.W))

    val LoadMicroTaskReady                  = Flipped(Bool())//can configure the next load task
    val LoadMicroTaskValid                  = (Bool())       //current load-task configuration is valid
    val LoadMicroTaskEndValid               = Flipped(Bool())//current load task completed
    val LoadMicroTaskEndReady               = (Bool())       //current load task completion acknowledged

    val StoreMicroTaskReady                 = Flipped(Bool())//can configure the next store task
    val StoreMicroTaskValid                 = (Bool())       //current store-task configuration is valid
    val StoreMicroTaskEndValid              = Flipped(Bool())//current store task completed
    val StoreMicroTaskEndReady              = (Bool())       //current store task completion acknowledged

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class MTEMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val computeType                         = Output(UInt(MteComputeType.ComputeTypeBitWidth.W))
}

//when reading from MatrixReg, it must be clear which bank and which row are accessed, then the data is concatenated and returned
//which bank and which row to read are computed by the data-control module
//how data is arranged within banks is filled by the MemoryLoader module
//MemoryLoader and data-control modules both have a window period, which can perform extra data arrangement such as quantization, sparsification reversal, dequantization, and quantization reorder
//MemoryLoader and data-control are separated to exploit the window period so the single-read/single-write MatrixReg can run independently
//is there an SRAM that can read and write simultaneously? I can guarantee no writes hit the same location, but let us use double buffering first....
//we account for response latency, so there is also a FIFO between DataControl and MatrixReg. Since the future SRAM is expected to be a simple module, keep the FIFO in DataControl so MatrixReg stays as simple as possible.
class TrDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(TrMatrixRegBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(DecoupledIO(Vec(TrMatrixRegNBanks, (UInt(log2Ceil(TrMatrixRegBankNEntries).W)))))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Valid(Vec(TrMatrixRegNBanks, UInt(TrMatrixRegEntryBitSize.W)))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

class ABScaleControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(TrMatrixRegBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(DecoupledIO(UInt(log2Ceil(ABScaleBankNEntries).W)))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Valid(Vec(ABScaleNSlices, UInt((ScaleWidth * ReduceGroupSize).W)))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

class MatrixRegLoaderWriteLane(addrWidth: Int, dataWidth: Int, maskWidth: Int)(implicit p: Parameters)
  extends CuteBundle {
    val BankAddr = Valid(UInt(addrWidth.W))
    val Data = Valid(UInt(dataWidth.W))
    val ByteMask = Valid(UInt(maskWidth.W))
}

// unified AB MemoryLoader interface, with ZeroFill support
class TrLoadMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    // When MemLoader is working, active will be true.
    // Otherwise, active will be false.
    val active = Input(Bool())
    val WriteRequest = Flipped(Vec(TrMatrixRegNBanks, Vec(2, new MatrixRegLoaderWriteLane(
        log2Ceil(TrMatrixRegBankNEntries),
        TrMatrixRegEntryBitSize,
        TrMatrixRegEntryByteSize
    ))))
    def BankAddr: Seq[Valid[UInt]] = WriteRequest.map(_(0).BankAddr)
    def Data: Seq[Valid[UInt]] = WriteRequest.map(_(0).Data)
    def ByteMask: Seq[Valid[UInt]] = WriteRequest.map(_(0).ByteMask)
}

class ABScaleLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(AScratchpadBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(Valid(UInt(log2Ceil(ABScaleBankNEntries).W)))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Flipped(Valid(Vec(ABScaleNSlices, UInt((ScaleWidth * ReduceGroupSize).W))))
    //chosen selects the ScratchPad; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

class AccDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(AccMatrixRegBankNLines), and it is input data that requires handshaking
    val ReadBankAddr = Flipped((Vec(AccMatrixRegNBanks, Valid(UInt(log2Ceil(AccMatrixRegBankNEntries).W)))))
    val WriteBankAddr = Flipped((Vec(AccMatrixRegNBanks, Valid(UInt(log2Ceil(AccMatrixRegBankNEntries).W)))))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt
    val ReadResponseData = (Vec(AccMatrixRegNBanks, Valid(UInt(AccMatrixRegEntryBitSize.W))))
    val WriteRequestData = Flipped((Vec(AccMatrixRegNBanks, Valid(UInt(AccMatrixRegEntryBitSize.W)))))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    val ReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

class AccLoadMatrixRegIO(implicit p: Parameters) extends CuteBundle {
    val WriteRequest = Flipped(Vec(AccMatrixRegNBanks, Vec(2, new MatrixRegLoaderWriteLane(
        log2Ceil(AccMatrixRegBankNEntries),
        AccMatrixRegEntryBitSize,
        AccMatrixRegEntryByteSize
    ))))
    def BankAddr: Seq[Valid[UInt]] = WriteRequest.map(_(0).BankAddr)
    def Data: Seq[Valid[UInt]] = WriteRequest.map(_(0).Data)
    def ByteMask: Seq[Valid[UInt]] = WriteRequest.map(_(0).ByteMask)
}

class AccMemoryMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val ReadRequestToMatrixReg = (new Bundle{
        val BankAddr = Flipped(Vec(AccMatrixRegNBanks, Valid(UInt(log2Ceil(AccMatrixRegBankNEntries).W))))
        val ReadResponseData = ((Vec(AccMatrixRegNBanks, Valid(UInt(AccMatrixRegEntryBitSize.W)))))
    })
    val WriteRequestToMatrixReg = new AccLoadMatrixRegIO
    val LoadReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val StoreReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val LoadReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val StoreReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

class MMURequestIO(implicit p: Parameters) extends CuteBundle{
    val RequestAddr = UInt(MMUAddrWidth.W)
    val RequestConherent = Bool()
    val RequestData = UInt(MMUDataWidth.W)
    val RequestSourceID = UInt(64.W)
    val RequestType_isWrite = Bool()
    val UseAllocatedSourceID = Bool()
    val isA = Bool()
    val MatrixIsAcc = Bool()
    val MatrixPrefetchTag = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.width.W))
    // Debug-only sideband. CUTE2TL records this in ChiselDB but never maps it
    // to a TileLink user field.
    val MatrixTraceTag = Option.when(EnableMatrixPrefetch)(UInt(CuteMatrixTraceTag.width.W))
    // Shared transpose engine uses source-row routing, not normal AB swizzle.
    val IsTranspose = Bool()
    val RequestMask = UInt(MMUMaskWidth.W)
}

class MMUResponseIO(implicit p: Parameters) extends CuteBundle{
    val ReseponseData = UInt(MMUDataWidth.W)
    val ReseponseConherent = Bool()
    val ReseponseSourceID = UInt(64.W)
}

//LocalMMU interface
class LocalMMUIO(implicit p: Parameters) extends CuteBundle{

    //issued memory request
    val Request = Flipped(Vec(TrMatrixRegNBanks, DecoupledIO(new MMURequestIO)))
    //transaction ID of the TL link to which the read request is dispatched
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //the MemoryLoader is guaranteed to receive the response back!
    val Response = Vec(TrMatrixRegNBanks, DecoupledIO(new MMUResponseIO))
}

class MMU2TLIO(implicit p: Parameters) extends CuteBundle{

    //issued memory request
    val Request = Flipped(Vec(TrMatrixRegNBanks, DecoupledIO(new MMURequestIO)))
    //transaction ID of the TL link to which the read request is dispatched
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //the MemoryLoader is guaranteed to receive the response back!
    val Response = Vec(TrMatrixRegNBanks, DecoupledIO(new MMUResponseIO))
}

class FReducePEDataType {
//0:Int8, 1:FP16, 2:BF16, 3:TF32, 4:I8 * UI8, 5:UI8 * I8, 6:UI8 * UI8
    def AdataByteWidth(computeType : UInt) : UInt = {
        val dataByteWidth = Wire(UInt(3.W))
        dataByteWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataByteWidth := 1.U }
        is (MteComputeType.F16F16F32)  { dataByteWidth := 2.U }
        is (MteComputeType.BF16BF16F32) { dataByteWidth := 2.U }
        is (MteComputeType.TF32TF32F32) { dataByteWidth := 4.U }
        is (MteComputeType.I8U8I32) { dataByteWidth := 1.U }
        is (MteComputeType.U8I8I32) { dataByteWidth := 1.U }
        is (MteComputeType.U8U8I32) { dataByteWidth := 1.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataByteWidth := 1.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataByteWidth := 1.U }
        is (MteComputeType.Fp8e4m3F32) { dataByteWidth := 1.U }
        is (MteComputeType.Fp8e5m2F32) { dataByteWidth := 1.U }
        }
        dataByteWidth
    }

    def BdataByteWidth(computeType : UInt) : UInt = {
        val dataByteWidth = Wire(UInt(3.W))
        dataByteWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataByteWidth := 1.U }
        is (MteComputeType.F16F16F32)  { dataByteWidth := 2.U }
        is (MteComputeType.BF16BF16F32) { dataByteWidth := 2.U }
        is (MteComputeType.TF32TF32F32) { dataByteWidth := 4.U }
        is (MteComputeType.I8U8I32) { dataByteWidth := 1.U }
        is (MteComputeType.U8I8I32) { dataByteWidth := 1.U }
        is (MteComputeType.U8U8I32) { dataByteWidth := 1.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataByteWidth := 1.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataByteWidth := 1.U }
        is (MteComputeType.Fp8e4m3F32) { dataByteWidth := 1.U }
        is (MteComputeType.Fp8e5m2F32) { dataByteWidth := 1.U }
        }
        dataByteWidth
    }

    def AdataBitWidth(computeType : UInt) : UInt = {
        val dataBitWidth = Wire(UInt(6.W))
        dataBitWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataBitWidth := 8.U }
        is (MteComputeType.F16F16F32)  { dataBitWidth := 16.U }
        is (MteComputeType.BF16BF16F32) { dataBitWidth := 16.U }
        is (MteComputeType.TF32TF32F32) { dataBitWidth := 32.U }
        is (MteComputeType.I8U8I32) { dataBitWidth := 8.U }
        is (MteComputeType.U8I8I32) { dataBitWidth := 8.U }
        is (MteComputeType.U8U8I32) { dataBitWidth := 8.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataBitWidth := 8.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataBitWidth := 8.U }
        is (MteComputeType.Nvfp4F32) { dataBitWidth := 4.U }
        is (MteComputeType.Mxfp4F32) { dataBitWidth := 4.U }
        is (MteComputeType.Fp8e4m3F32) { dataBitWidth := 8.U }
        is (MteComputeType.Fp8e5m2F32) { dataBitWidth := 8.U }
        }
        dataBitWidth
    }

    def BdataBitWidth(computeType : UInt) : UInt = {
        val dataBitWidth = Wire(UInt(6.W))
        dataBitWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataBitWidth := 8.U }
        is (MteComputeType.F16F16F32) { dataBitWidth := 16.U }
        is (MteComputeType.BF16BF16F32) { dataBitWidth := 16.U }
        is (MteComputeType.TF32TF32F32) { dataBitWidth := 32.U }
        is (MteComputeType.I8U8I32) { dataBitWidth := 8.U }
        is (MteComputeType.U8I8I32) { dataBitWidth := 8.U }
        is (MteComputeType.U8U8I32) { dataBitWidth := 8.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataBitWidth := 8.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataBitWidth := 8.U }
        is (MteComputeType.Nvfp4F32) { dataBitWidth := 4.U }
        is (MteComputeType.Mxfp4F32) { dataBitWidth := 4.U }
        is (MteComputeType.Fp8e4m3F32) { dataBitWidth := 8.U }
        is (MteComputeType.Fp8e5m2F32) { dataBitWidth := 8.U }
        }
        dataBitWidth
    }

    def CdataByteWidth(computeType : UInt) : UInt = {
        val dataByteWidth = Wire(UInt(3.W))
        dataByteWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataByteWidth := 4.U }
        is (MteComputeType.F16F16F32) { dataByteWidth := 4.U }
        is (MteComputeType.BF16BF16F32) { dataByteWidth := 4.U }
        is (MteComputeType.TF32TF32F32) { dataByteWidth := 4.U }
        is (MteComputeType.I8U8I32) { dataByteWidth := 4.U }
        is (MteComputeType.U8I8I32) { dataByteWidth := 4.U }
        is (MteComputeType.U8U8I32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataByteWidth := 4.U }
        is (MteComputeType.Nvfp4F32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp4F32) { dataByteWidth := 4.U }
        is (MteComputeType.Fp8e4m3F32) { dataByteWidth := 4.U }
        is (MteComputeType.Fp8e5m2F32) { dataByteWidth := 4.U }
        }
        dataByteWidth
    }

    def DdataByteWidth(computeType : UInt) : UInt = {
        val dataByteWidth = Wire(UInt(3.W))
        dataByteWidth := 0.U
        switch(computeType){
        is (MteComputeType.I8I8I32) { dataByteWidth := 4.U }
        is (MteComputeType.F16F16F32) { dataByteWidth := 4.U }
        is (MteComputeType.BF16BF16F32) { dataByteWidth := 4.U }
        is (MteComputeType.TF32TF32F32) { dataByteWidth := 4.U }
        is (MteComputeType.I8U8I32) { dataByteWidth := 4.U }
        is (MteComputeType.U8I8I32) { dataByteWidth := 4.U }
        is (MteComputeType.U8U8I32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp8e4m3F32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp8e5m2F32) { dataByteWidth := 4.U }
        is (MteComputeType.Nvfp4F32) { dataByteWidth := 4.U }
        is (MteComputeType.Mxfp4F32) { dataByteWidth := 4.U }
        is (MteComputeType.Fp8e4m3F32) { dataByteWidth := 4.U }
        is (MteComputeType.Fp8e5m2F32) { dataByteWidth := 4.U }
        }
        dataByteWidth
    }

}

//data type template class
case object  ElementDataType extends Field[UInt]{
    val DataTypeBitWidth = 4
    val DataTypeUndef   = 0.U(DataTypeBitWidth.W)
    val DataTypeWidth32 = 4.U(DataTypeBitWidth.W)
    val DataTypeWidth16 = 2.U(DataTypeBitWidth.W)
    val DataTypeWidth8  = 1.U(DataTypeBitWidth.W)
    val DataTypeWidth4  = 7.U(DataTypeBitWidth.W)
}

case object  AccMemoryTaskType extends Field[UInt]{
    val TypeBitWidth = 4
    val TaskTypeUndef = 0.U(TypeBitWidth.W)
    val TaskTypeTensorZeroLoad = 1.U(TypeBitWidth.W) //fill the data with 0 directly; in practice this does nothing, and it can write to SRAM by default, ignoring previous SRAM contents
    val TaskTypeTensorRepeatRowLoad = 2.U(TypeBitWidth.W) //load one row repeatedly; in practice this does nothing, and it can write to SRAM by default, ignoring previous SRAM contents
    val TaskTypeTensorLoad = 3.U(TypeBitWidth.W) //load all data completely
}
case object  MemoryOrderType extends Field[UInt]{
    val MemoryOrderTypeBitWidth = 8
    val OrderTypeUndef      = 0.U(MemoryOrderTypeBitWidth.W)
    val OrderType_Mb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Mb first and Kb after
    val OrderType_Mb_Nb     = 1.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Mb first and Nb after
    val OrderType_Nb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Nb first and Kb after
    val OrderType_Nb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Nb first and Mb after
    val OrderType_Kb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Kb first and Mb after
    val OrderType_Kb_Nb     = 2.U(MemoryOrderTypeBitWidth.W) //ordering in address space, with Kb first and Nb after

}

class MatrixRegTaskDecode(MatrixRegTask: UInt) extends Field[UInt]{

    def IsRead : Bool = MatrixRegTask(MatrixRegTaskType.EnableReadFromDataController) || MatrixRegTask(MatrixRegTaskType.EnableReadFromMemoryLoader)
    def IsWrite : Bool = MatrixRegTask(MatrixRegTaskType.EnableWriteFromDataController) || MatrixRegTask(MatrixRegTaskType.EnableWriteFromMemoryLoader)

    def IsReadFromDataController: Bool = MatrixRegTask(MatrixRegTaskType.ReadFromDataControllerIndex)
    def IsWriteFromDataController: Bool = MatrixRegTask(MatrixRegTaskType.WriteFromDataControllerIndex)
    def IsWriteFromMemoryLoader: Bool = MatrixRegTask(MatrixRegTaskType.WriteFromMemoryLoaderIndex)
    def IsReadFromMemoryLoader  : Bool = MatrixRegTask(MatrixRegTaskType.ReadFromMemoryLoaderIndex)

    // def ReadSrc: UInt = MatrixRegTask
}

case object MatrixRegTaskType extends Field[UInt]{
    val TaskTypeBitWidth = 4
    // for a single MatrixReg, there are three concurrent data sources, so they are encoded with 3 bits
    // 1. DataController read requests to MatrixReg for PE input data
    // 2. DataController write requests that send PE outputs into MatrixReg
    // 3. MemoryLoader write requests to MatrixReg
    // we do not know the number of MatrixReg read/write ports, so we use enable signals to indicate accepted data sources
    val EnableReadFromDataController = 1.U(TaskTypeBitWidth.W)
    val EnableWriteFromDataController = 2.U(TaskTypeBitWidth.W)
    val EnableWriteFromMemoryLoader = 4.U(TaskTypeBitWidth.W)
    val EnableReadFromMemoryLoader = 8.U(TaskTypeBitWidth.W)
    val ReadFromDataControllerIndex = 0
    val WriteFromDataControllerIndex = 1
    val WriteFromMemoryLoaderIndex = 2
    val ReadFromMemoryLoaderIndex = 3
}

class MatrixRegTask(implicit p: Parameters) extends CuteBundle{
    val ReadFromMemoryLoader = Bool()
    val WriteFromMemoryLoader = Bool()
    val WriteFromDataController = Bool()
    val ReadFromDataController = Bool()
}

case object LocalMMUTaskType extends Field[UInt]{
    val TaskTypeBitWidth = 3
    val TaskTypeMax = 6
    val AFirst = 0.U(TaskTypeBitWidth.W)
    val BFirst = 1.U(TaskTypeBitWidth.W)
    val CLoadFirst = 2.U(TaskTypeBitWidth.W)
    val CStoreFirst = 3.U(TaskTypeBitWidth.W)
    val BScaleFirst = 4.U(TaskTypeBitWidth.W)
    val AScaleFirst = 5.U(TaskTypeBitWidth.W)
    // val DFirst = 3.U(TaskTypeBitWidth.W)
}
