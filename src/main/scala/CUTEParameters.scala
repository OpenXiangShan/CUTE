
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile._

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
    a: LoaderMode,
    b: LoaderMode,
    cLoad: LoaderMode,
    cStore: LoaderMode
  ) {
    def render: String = s"A${a.renderToken}B${b.renderToken}CL${cLoad.renderToken}CS${cStore.renderToken}"
  }

  private val LoaderBridgeChannelConfigPattern = "^A(L|1|2|4|8)B(L|1|2|4|8)CL(L|1|2|4|8)CS(L|1|2|4|8)$".r

  def parseLoaderMode(token: String): LoaderMode = token match {
    case "L" => LegacyLoaderMode
    case "1" => ParameterizedLoaderMode(1)
    case "2" => ParameterizedLoaderMode(2)
    case "4" => ParameterizedLoaderMode(4)
    case "8" => ParameterizedLoaderMode(8)
    case other =>
      require(requirement = false, s"unsupported loader mode token '$other', expected one of L/1/2/4/8")
      LegacyLoaderMode
  }

  def parseLoaderBridgeChannelConfig(config: String): LoaderBridgeChannelConfig = {
    val normalized = config.trim.toUpperCase
    normalized match {
      case LoaderBridgeChannelConfigPattern(a, b, cLoad, cStore) =>
        LoaderBridgeChannelConfig(
          a = parseLoaderMode(a),
          b = parseLoaderMode(b),
          cLoad = parseLoaderMode(cLoad),
          cStore = parseLoaderMode(cStore)
        )
      case _ =>
        require(
          requirement = false,
          s"invalid LoaderBridgeChannelConfig '$config', expected format like A1B1CL1CS8 or ALBLCLLCSL"
        )
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
    enableInt84Int32: Boolean = false,
    enableFp8Fp32: Boolean = false,
    enableFp8Fp16: Boolean = false,
    enableFp8Bf16: Boolean = false,
    enableFp16Fp16: Boolean = false,
    enableBf16Fp32: Boolean = false,
    enableFp16Fp32: Boolean = false,
    enableTf32Fp32: Boolean = false,
    enableFp32Fp32: Boolean = false,
    enableMxfp4Fp32: Boolean = false,
    enableMxfp8Fp32: Boolean = false,
    enableNvfp4Fp32: Boolean = false,
) {
    assert(enableInt84Int32 == false, "enableInt84Int32 is not supported now")
    assert(enableFp32Fp32 == false, "enableFp32Fp32 is not supported now")

    def enableMatrix: Boolean =
        enableInt4Int32 || enableInt8Int32 || enableInt84Int32 ||
        enableFp8Fp32 || enableFp8Fp16 || enableFp8Bf16 ||
        enableFp16Fp16 || enableBf16Fp32 || enableFp16Fp32 ||
        enableFp32Fp32 || enableTf32Fp32 ||
        enableMxfp4Fp32 || enableMxfp8Fp32 || enableNvfp4Fp32
    
    def enable4BitSrc: Boolean =
        enableInt4Int32 || enableMxfp4Fp32 || enableNvfp4Fp32

    def enable8BitSrc: Boolean =
        enableInt8Int32 || enableInt84Int32 ||
        enableFp8Fp32 || enableFp8Fp16 || enableFp8Bf16 ||
        enableMxfp8Fp32
    
    def enable16BitSrc: Boolean =
        enableFp16Fp16 || enableBf16Fp32 || enableFp16Fp32
    
    def enable32BitSrc: Boolean =
        enableFp32Fp32 || enableTf32Fp32
    
    def enable64BitSrc: Boolean = false

    def enable4BitDst: Boolean = false
    
    def enable8BitDst: Boolean = false

    def enable16BitDst: Boolean =
        enableFp8Fp16 || enableFp8Bf16 || enableFp16Fp16
    
    def enable32BitDst: Boolean =
        enableInt4Int32 || enableInt8Int32 || enableInt84Int32 ||
        enableFp8Fp32 ||
        enableBf16Fp32 || enableFp16Fp32 ||
        enableFp32Fp32 ||
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

    // 256 bit outside memory bus,128 memory bus
    def TL256Params = baseParams.copy(
        outsideDataWidth = 256,
        MemoryDataWidth = 128
    )

    //default simple debug
    def simpleDebugParams = baseParams.copy(
        Debug = CuteDebugParams.CMLDebugEnable
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
        // Debug = CuteDebugParams.AMLDebugEnable
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
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_1Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 64,
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_05Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 32,
        // Debug = CuteDebugParams.AMLDebugEnable
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
    def baseParams = CuteParams().copy(v3config = Cutev3extParams.baseparams)

    // 256 bit outside memory bus,128 memory bus
    def TL256Params = baseParams.copy(
        outsideDataWidth = 256,
        MemoryDataWidth = 128
    )

    //default simple debug
    def simpleDebugParams = baseParams.copy(
        Debug = CuteDebugParams.AMLDebugEnable
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
        Debug = CuteDebugParams.AMLDebugEnable
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
        // Debug = CuteDebugParams.AMLDebugEnable
    )


}

object CuteDebugParams {

  // NoDebugParams:
  def NoDebug = CuteDebugParams()

  def AMLDebugEnable = NoDebug.copy(
    YJPAMLDebugEnable = true,
  )

  def CMLDebugEnable = NoDebug.copy(
    YJPCMLDebugEnable = true,
  )

  def ComputeDebugeNable = NoDebug.copy(
    // YJPMACDebugEnable = true,
    YJPCMLDebugEnable = true,
    // YJPDebugEnable = true,
  )

  def AllDebugOn = NoDebug.copy(
    YJPDebugEnable = true,
    YJPADCDebugEnable = true,
    YJPBDCDebugEnable = true,
    YJPCDCDebugEnable = true,
    YJPAMLDebugEnable = true,
    YJPBMLDebugEnable = true,
    YJPCMLDebugEnable = true,
    YJPTASKDebugEnable = true,
    YJPVECDebugEnable = true,
    YJPMACDebugEnable = true,
    YJPPEDebugEnable = true,
    YJPAfterOpsDebugEnable = true)
}

case class CuteDebugParams(
    val YJPDebugEnable :Boolean     = false,
    val YJPADCDebugEnable :Boolean  = false,
    val YJPBDCDebugEnable :Boolean  = false,
    val YJPCDCDebugEnable :Boolean  = false,
    val YJPAMLDebugEnable :Boolean  = false,
    val YJPBMLDebugEnable :Boolean  = false,
    val YJPCMLDebugEnable :Boolean  = false,
    val YJPTASKDebugEnable :Boolean = false,
    val YJPVECDebugEnable :Boolean  = false,
    val YJPMACDebugEnable :Boolean  = false,
    val YJPPEDebugEnable :Boolean   = false,
    val YJPAfterOpsDebugEnable :Boolean   = false,
)

object CuteMMUParams {
  // baseParams:
  def baseParams = CuteMMUParams()
}

case class CuteMMUParams(
    val vpnBits :Int = 12,
    val ppnBits :Int = 12,
    val pgIdxBits :Int = 12,
    val vaddrBits :Int = 39,
    val paddrBits :Int = 39,
    val corePAddrBits :Int = 64,
)


object Cutev3extParams {
    // NoV3ExtParams:
    def NoextParams = Cutev3extParams(
    TaskCtrl_AutoClear = false, //whether the task controller auto-clears completed instructions  
    )

    // V3 Base Ext
    def baseparams = Cutev3extParams()

}

case class Cutev3extParams(
    val TaskCtrl_AutoClear :Boolean = true, //whether the task controller auto-clears completed instructions
)


object CuteFPEParams {

    def baseparams = CuteFPEParams()

}

case class CuteFPEParams(
    // currently fixed
    val MinGroupSize :Int = 16,
    val MinDataTypeWidth : Int = 4,
    val ScaleElementWidth : Int = 8,
    //

    val cmptreelayers :Int = 4,
    val fp8cmptreelayers :Int = 4,
    // currently fixed and shared with FP4; leave it unchanged
    val FP4P0AddNum :Int = 2,
)

case class CuteParams(
    val outsideDataWidth :Int = 512, //CUTE external memory bandwidth
    val MemoryDataWidth :Int = 64,   //TODO: data width of the DRAM memory channel

    val VectorWidth :Int = 256,      //vector pipeline width

    val L2NBanks :Int = 4,

    val ApplicationMaxTensorSize :Int = 65536, //maximum tensor shape a program can handle,

    val MMUAddrWidth :Int = 64 , //CUTE MMU address width

    val LLCSourceMaxNum :Int = 64, //maximum number of sources on the LLC bus -> this parameter is tightly coupled to LLC latency; to sustain full throughput, sourceMAXnum must exceed LLC latency
    val MemorysourceMaxNum :Int = 64, //maximum number of sources on the memory bus -> this parameter is tightly coupled to memory latency; to sustain full throughput, sourceMAXnum must exceed memory latency


    //tensor shape stored in MatrixReg
    val Tensor_MN :Int = 128,   //M and N dimensions of the tensor to be stored here
    val Tensor_K :Int = 64,    //K size of the tensor to be stored here (8-bit/element)

    //MTE matrix-multiply unit shape
    val Matrix_MN :Int = 4,     //Matrix_MN: M and N dimensions of the matrix multiply executed by TE
    val ReduceWidthByte :Int = 64,   //ReduceWidthByte: data width used by ReducePE for inner products, in bytes
    val ResultWidthByte :Int = 4,    //ResultWidthByte: result width of ReducePE, in bytes

    val ResultFIFODepth :Int = 8,    //MAC FIFO depth

    val AMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering AML data to CCSP
    val BMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering BML data to CCSP
    val CMemoryLoaderReadFromMatrixRegFIFODepth :Int = 4, //FIFO for buffering CCSP data to CML
    val CMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //FIFO for buffering CML data to CMReg

    val VecTaskInstBufferDepth :Int = 32, //VecTask instruction buffer depth
    val VecTaskInstBufferSize :Int = 8, //number of VecTask instruction buffers
    val VecTaskDataBufferDepth :Int = 4, //VecTask buffer depth only needs to cover transfer latency from VecInterface to VPU

    // TaskController issue window depth (compile-time only)
    val TaskCtrlIssueWindowDepth :Int = 8,

    // Number of synchronization registers implemented by the core.
    val MsyncRegs :Int = 32,

    val EnableDifftest: Boolean = false, //whether DiffTest is enabled

    val Debug : CuteDebugParams = CuteDebugParams.NoDebug, //debug parameters
    val MMUParams: CuteMMUParams = CuteMMUParams.baseParams, //MMU parameters
    
    val v3config: Cutev3extParams = Cutev3extParams.NoextParams, //v3 extension parameters

    val FPEparams: CuteFPEParams = CuteFPEParams.baseparams, //FPE parameters

    val MatrixExtension: MatrixIsaParams = MatrixIsaParams(),

    // User-facing loader response mode selector.
    // L means legacy single-channel loader; 1/2/4/8 mean parameterized bridge width.
    val LoaderBridgeChannelConfig: String = "A1B2CL8CS4"
) {
    val parsedLoaderBridgeChannelConfig = ResponseChannelHelper.parseLoaderBridgeChannelConfig(LoaderBridgeChannelConfig)
    val AMLChannelMode = parsedLoaderBridgeChannelConfig.a
    val BMLChannelMode = parsedLoaderBridgeChannelConfig.b
    val CLoadChannelMode = parsedLoaderBridgeChannelConfig.cLoad
    val CStoreChannelMode = parsedLoaderBridgeChannelConfig.cStore

    require(
      CLoadChannelMode.isLegacy == CStoreChannelMode.isLegacy,
      s"mixed legacy/parameterized C loader modes are not supported now: ${parsedLoaderBridgeChannelConfig.render}"
    )

    // require(ReduceWidthByte == 64, "FP8/4 now only support 512 bit reduce width")
    require(outsideDataWidth == 512 || outsideDataWidth == 256, "currently only support 512/256 bit outsideDataWidth")
    require(ReduceWidthByte == 64 || ReduceWidthByte == 32, "currently only support 512/256 bit ReduceWidth")
    require(outsideDataWidth >= ReduceWidthByte * 8, "outsideDataWidth must be larger than or equal to ReduceWidthByte")
    require((outsideDataWidth & (outsideDataWidth - 1)) == 0, "outsideDataWidth must be power of 2")
    require((MemoryDataWidth & (MemoryDataWidth - 1)) == 0, "MemoryDataWidth must be power of 2")
    require((VectorWidth & (VectorWidth - 1)) == 0, "VectorWidth must be power of 2")
    require((ApplicationMaxTensorSize & (ApplicationMaxTensorSize - 1)) == 0, "ApplicationMaxTensorSize must be power of 2")
    // require((MMUAddrWidth & (MMUAddrWidth - 1)) == 0, "MMUAddrWidth must be power of 2" )
    require((LLCSourceMaxNum & (LLCSourceMaxNum - 1)) == 0, "LLCSourceMaxNum must be power of 2")
    require((MemorysourceMaxNum & (MemorysourceMaxNum - 1)) == 0, "MemorysourceMaxNum must be power of 2")
    require((Tensor_MN & (Tensor_MN - 1)) == 0, "Tensor_MN must be power of 2")
    require((Tensor_K & (Tensor_K - 1)) == 0, "Tensor_K must be power of 2")
    require((Matrix_MN & (Matrix_MN - 1)) == 0, "Matrix_MN must be power of 2")
    require((ReduceWidthByte & (ReduceWidthByte - 1)) == 0, "ReduceWidthByte must be power of 2")
    require((ResultWidthByte & (ResultWidthByte - 1)) == 0, "ResultWidthByte must be power of 2")
    require((ResultFIFODepth & (ResultFIFODepth - 1)) == 0, "ResultFIFODepth must be power of 2")
    require((AMemoryLoaderReadFromMemoryFIFODepth & (AMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "AMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((BMemoryLoaderReadFromMemoryFIFODepth & (BMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "BMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((CMemoryLoaderReadFromMatrixRegFIFODepth & (CMemoryLoaderReadFromMatrixRegFIFODepth - 1)) == 0, "CMemoryLoaderReadFromMatrixRegFIFODepth must be power of 2")
    require((CMemoryLoaderReadFromMemoryFIFODepth & (CMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "CMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((VecTaskInstBufferDepth & (VecTaskInstBufferDepth - 1)) == 0, "VecTaskInstBufferDepth must be power of 2")
    require((VecTaskInstBufferSize & (VecTaskInstBufferSize - 1)) == 0, "VecTaskInstBufferSize must be power of 2")
    require((VecTaskDataBufferDepth & (VecTaskDataBufferDepth - 1)) == 0, "VecTaskDataBufferDepth must be power of 2")
    require(Seq(4, 8, 16).contains(TaskCtrlIssueWindowDepth), "TaskCtrlIssueWindowDepth only supports 4/8/16")
    require(Seq(8, 16, 32).contains(MsyncRegs), "MsyncRegs only supports 8/16/32")
    require((FPEparams.MinGroupSize == 16), "FPEparams.MinGroupSize must be 16")
    require((FPEparams.MinDataTypeWidth == 4), "FPEparams.MinDataTypeWidth must be 4")
    require((FPEparams.ScaleElementWidth == 8), "FPEparams.ScaleElementWidth must be 8")

    def cuteFpeConfig: CuteFpeConfig = CuteFpeConfig(
        enableTf32Fp32 = MatrixExtension.enableTf32Fp32,
        enableFp16Fp32 = MatrixExtension.enableFp16Fp32,
        enableMxfp4Fp32 = MatrixExtension.enableMxfp4Fp32,
        enableMxfp8Fp32 = MatrixExtension.enableMxfp8Fp32,
        enableNvfp4Fp32 = MatrixExtension.enableNvfp4Fp32
    )
    def cuteFpeParams: CuteFpeParameters = CuteFpeParametersFromCuteParams(this)

    def outsideDataWidthByte = outsideDataWidth / 8
    def ReduceWidth = ReduceWidthByte * 8
    def ABMLNeedMRegFillTable = ReduceWidthByte < outsideDataWidthByte //when returned memory data cannot be written in one cycle, ABML needs a writeback buffer
    def ResultWidth = ResultWidthByte * 8
    def ApplicationMaxTensorSizeBitSize = log2Ceil(ApplicationMaxTensorSize) + 1
    def MMUDataWidth = outsideDataWidth //MMU data bus width
    def MMUMaskWidth = MMUDataWidth / 8 //MMU mask width
    def MMUDataWidthBitSize = log2Ceil(MMUDataWidth) + 1 //effective data-bit count of the MMU data bus
    def LLCSourceMaxNumBitSize = log2Ceil(LLCSourceMaxNum) + 1
    def MemorysourceMaxNumBitSize = log2Ceil(MemorysourceMaxNum) + 1
    def SoureceMaxNum = math.max(LLCSourceMaxNum, MemorysourceMaxNum)
    def SoureceMaxNumBitSize = log2Ceil(SoureceMaxNum) + 1

    def P3AddNum = ReduceWidth / 4 / FPEparams.MinGroupSize
    def P2AddNum = ReduceWidth / (P3AddNum * 16)

    def ReduceGroupSize  = Tensor_K/ReduceWidthByte    //number of ReduceVectors for K to be stored, not the K size
    def MatrixRegMaxTensorDim = Math.max(Tensor_MN, Math.max(Tensor_MN, ReduceGroupSize))
    def MatrixRegMaxTensorDimBitSize = log2Ceil(MatrixRegMaxTensorDim) + 1
    //A tensor shape stored in MatrixReg is M*K
    //A MatrixReg size is Tenser_M * ReduceGroupSize * ReduceWidthByte
    //128*(4*256/8); one read covers a 128*128 tensor
    //one compute takes (128/4)*(128/4)*4 = 4096 cycles; one read takes 128*4 = 512 cycles.
    //sequential reads must be considered; MatrixReg should be banked
    def ABMatrixRegSize = Tensor_MN * ReduceGroupSize * ReduceWidthByte //reduce
    def CMatrixRegSize = Tensor_MN * Tensor_MN * ResultWidthByte //result

    //the current MatrixReg design is split into Tensor_T banks; each access fetches Tensor_T items from different banks and concatenates them
    def ABMatrixRegEntryByteSize = ReduceWidthByte //bandwidth suitable for feeding TE
    def CMatrixRegEntryByteSize = Matrix_MN*ResultWidthByte //bandwidth for reads and writes
    def ABMatrixRegEntryBitSize = ReduceWidthByte * 8 //bandwidth suitable for feeding TE
    def CMatrixRegEntryBitSize = Matrix_MN*ResultWidthByte * 8//bandwidth for reads and writes
    // DiffAmuFinishEvent flattens per-bank payload into UInt(64) lanes.
    // Use the maximum bank payload width so one event layout works for AB/C paths.
    def DiffAmuFinishWordsPerBank = Math.max(ABMatrixRegEntryBitSize, CMatrixRegEntryBitSize) / 64
    def ABMatrixRegNBanks = Matrix_MN //note that this is strongly tied to Matrix_MN and is generally an integer multiple of Matrix_MN
    def CMatrixRegNBanks = Matrix_MN //convenient for reorder
    def Trans_Load_Size = outsideDataWidthByte / ABMatrixRegNBanks
    def ABMatrixReg_Total_Bandwidth = ABMatrixRegNBanks * ABMatrixRegEntryByteSize  //total bandwidth of ABMatrixReg
    def CMatrixReg_Total_Bandwidth = CMatrixRegNBanks * CMatrixRegEntryByteSize  //total bandwidth of CMatrixReg
    def ABMatrixReg_Total_Bandwidth_Bit = ABMatrixRegNBanks * ABMatrixRegEntryByteSize * 8  //total bandwidth of ABMatrixReg
    def CMatrixReg_Total_Bandwidth_Bit = CMatrixRegNBanks * CMatrixRegEntryByteSize * 8  //total bandwidth of CMatrixReg
    def ABMatrixRegBankSize = ABMatrixRegSize / ABMatrixRegNBanks
    def CMatrixRegBankSize = CMatrixRegSize / CMatrixRegNBanks
    def ABMatrixRegBankNEntries = ABMatrixRegBankSize / ABMatrixRegEntryByteSize
    def CMatrixRegBankNEntries = CMatrixRegBankSize / CMatrixRegEntryByteSize

    /**
     * Scale Factor Parameters
     */
    def ScaleWidth = ReduceWidthByte * 8 * FPEparams.ScaleElementWidth / FPEparams.MinDataTypeWidth / FPEparams.MinGroupSize   // maximum bit width needed for one group scale
    def ABScaleSize = Tensor_MN * ReduceGroupSize * ScaleWidth
    def ABScaleNSlices = outsideDataWidth / ScaleWidth / ReduceGroupSize  // maximum scale length for one Tensor_K per slice
    def ABScaleBankNEntries = ABScaleSize / (ABScaleNSlices * ScaleWidth * ReduceGroupSize)

    // require(ReduceGroupSize == 2, "ReduceGroupSize must be 2, Wait for update")
    require(outsideDataWidthByte <= Tensor_K, "outsideDataWidthByte must be less than or equal to Tensor_K, or a load will exceed the subtensor in micro load")
    require(outsideDataWidthByte % ABMatrixRegNBanks == 0, "outsideDataWidthByte must be divisible by ABMatrixRegNBanks for transpose load")

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

    def MMUParams: CuteMMUParams = cuteParams.MMUParams
    def DebugParams: CuteDebugParams = cuteParams.Debug
    def v3config: Cutev3extParams = cuteParams.v3config
    def FPEparams: CuteFPEParams = cuteParams.FPEparams

    def TaskCtrlIssueWindowDepth = cuteParams.TaskCtrlIssueWindowDepth
    def TaskCtrlIssueWindowDepthBitSize = log2Ceil(TaskCtrlIssueWindowDepth)
    def CuteMsyncRegs = cuteParams.MsyncRegs
    def CuteMsyncRegIdxWidth = log2Ceil(CuteMsyncRegs)

    def DecodedAmuCtrlFIFODepth = TaskCtrlIssueWindowDepth  // Decoded AMU instruction FIFO depth, tied to the issue window
    def DecodedAmuCtrlFIFODepthBitSize = log2Ceil(DecodedAmuCtrlFIFODepth) // Bit width of the decoded AMU instruction FIFO depth

    def ABMatrixRegCount = 4
    def CMatrixRegCount = 4
    def ABMatrixRegIdWidth = log2Ceil(ABMatrixRegCount)
    def CMatrixRegIdWidth = log2Ceil(CMatrixRegCount)
    def MatrixRegIdWidth = ABMatrixRegIdWidth max CMatrixRegIdWidth

    def vpnBits = MMUParams.vpnBits
    def ppnBits = MMUParams.ppnBits
    def pgIdxBits = MMUParams.pgIdxBits
    def vaddrBits = MMUParams.vaddrBits
    def paddrBits = MMUParams.paddrBits
    def corePAddrBits = MMUParams.corePAddrBits

    def TaskCtrl_AutoClear = v3config.TaskCtrl_AutoClear

    def YJPDebugEnable      = DebugParams.YJPDebugEnable
    def YJPADCDebugEnable   = DebugParams.YJPADCDebugEnable
    def YJPBDCDebugEnable   = DebugParams.YJPBDCDebugEnable
    def YJPCDCDebugEnable   = DebugParams.YJPCDCDebugEnable
    def YJPAMLDebugEnable   = DebugParams.YJPAMLDebugEnable
    def YJPBMLDebugEnable   = DebugParams.YJPBMLDebugEnable
    def YJPCMLDebugEnable   = DebugParams.YJPCMLDebugEnable
    def YJPTASKDebugEnable        = DebugParams.YJPTASKDebugEnable
    def YJPVECDebugEnable         = DebugParams.YJPVECDebugEnable
    def YJPMACDebugEnable         = DebugParams.YJPMACDebugEnable
    def YJPPEDebugEnable          = DebugParams.YJPPEDebugEnable
    def YJPAfterOpsDebugEnable    = DebugParams.YJPAfterOpsDebugEnable

    def outsideDataWidth = cuteParams.outsideDataWidth
    def outsideDataWidthByte = cuteParams.outsideDataWidthByte
    def MemoryDataWidth = cuteParams.MemoryDataWidth
    def ReduceWidthByte = cuteParams.ReduceWidthByte
    def ReduceWidth = cuteParams.ReduceWidth
    def mxfp8ScaleWidth = ReduceWidth * 8 / 8 / 32 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 32]
    def nvfp4ScaleWidth = ReduceWidth * 8 / 4 / 16 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 16]
    def mxfp4ScaleWidth = ReduceWidth * 8 / 4 / 32 //total scale width accepted by one PE per cycle [single scale width 8-bit, single element width 4-bit, groupsize 32]
    def ABMLNeedMRegFillTable = cuteParams.ABMLNeedMRegFillTable
    def ResultWidthByte = cuteParams.ResultWidthByte
    def ResultWidth = cuteParams.ResultWidth
    def VectorWidth = cuteParams.VectorWidth
    def ApplicationMaxTensorSize = cuteParams.ApplicationMaxTensorSize
    def ApplicationMaxTensorSizeBitSize = cuteParams.ApplicationMaxTensorSizeBitSize
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
    def ABMatrixRegSize = cuteParams.ABMatrixRegSize
    def CMatrixRegSize = cuteParams.CMatrixRegSize
    def Matrix_MN = cuteParams.Matrix_MN
    def ABMatrixRegEntryByteSize = cuteParams.ABMatrixRegEntryByteSize
    def CMatrixRegEntryByteSize = cuteParams.CMatrixRegEntryByteSize
    def ABMatrixRegEntryBitSize = cuteParams.ABMatrixRegEntryBitSize
    def CMatrixRegEntryBitSize = cuteParams.CMatrixRegEntryBitSize
    def DiffAmuFinishWordsPerBank = cuteParams.DiffAmuFinishWordsPerBank
    def ABMatrixRegNBanks = cuteParams.ABMatrixRegNBanks
    def CMatrixRegNBanks = cuteParams.CMatrixRegNBanks
    def Trans_Load_Size = cuteParams.Trans_Load_Size
    def ABMatrixReg_Total_Bandwidth = cuteParams.ABMatrixReg_Total_Bandwidth
    def CMatrixReg_Total_Bandwidth = cuteParams.CMatrixReg_Total_Bandwidth
    def ABMatrixReg_Total_Bandwidth_Bit = cuteParams.ABMatrixReg_Total_Bandwidth_Bit
    def CMatrixReg_Total_Bandwidth_Bit = cuteParams.CMatrixReg_Total_Bandwidth_Bit
    def ABMatrixRegBankSize = cuteParams.ABMatrixRegBankSize
    def CMatrixRegBankSize = cuteParams.CMatrixRegBankSize
    def ABMatrixRegBankNEntries = cuteParams.ABMatrixRegBankNEntries
    def CMatrixRegBankNEntries = cuteParams.CMatrixRegBankNEntries
    def ScaleWidth = cuteParams.ScaleWidth
    def ABScaleBankNEntries = cuteParams.ABScaleBankNEntries
    def ABScaleNSlices = cuteParams.ABScaleNSlices
    def ResultFIFODepth = cuteParams.ResultFIFODepth
    def AMemoryLoaderReadFromMemoryFIFODepth = cuteParams.AMemoryLoaderReadFromMemoryFIFODepth
    def BMemoryLoaderReadFromMemoryFIFODepth = cuteParams.BMemoryLoaderReadFromMemoryFIFODepth
    def CMemoryLoaderReadFromMatrixRegFIFODepth = cuteParams.CMemoryLoaderReadFromMatrixRegFIFODepth
    def CMemoryLoaderReadFromMemoryFIFODepth = cuteParams.CMemoryLoaderReadFromMemoryFIFODepth
    def VecTaskInstBufferDepth = cuteParams.VecTaskInstBufferDepth
    def VecTaskInstBufferSize = cuteParams.VecTaskInstBufferSize
    def VecTaskDataBufferDepth = cuteParams.VecTaskDataBufferDepth
    def ReduceGroupSize = cuteParams.ReduceGroupSize
    def EnableDifftest = cuteParams.EnableDifftest
    def L2NBanks = cuteParams.L2NBanks
    def LoaderBridgeChannelConfig = cuteParams.LoaderBridgeChannelConfig
    def parsedLoaderBridgeChannelConfig = cuteParams.parsedLoaderBridgeChannelConfig
    def AMLChannelMode = cuteParams.AMLChannelMode
    def BMLChannelMode = cuteParams.BMLChannelMode
    def CLoadChannelMode = cuteParams.CLoadChannelMode
    def CStoreChannelMode = cuteParams.CStoreChannelMode
    def AMLUseLegacyLoader = AMLChannelMode.isLegacy
    def BMLUseLegacyLoader = BMLChannelMode.isLegacy
    def CLoadUseLegacyLoader = CLoadChannelMode.isLegacy
    def CStoreUseLegacyLoader = CStoreChannelMode.isLegacy
    def AMLResponseChannelCount = AMLChannelMode.responseChannelCount
    def BMLResponseChannelCount = BMLChannelMode.responseChannelCount
    def CLoadBridgeResponseChannelCount = CLoadChannelMode.responseChannelCount
    def CStoreBridgeResponseChannelCount = CStoreChannelMode.responseChannelCount
    def CStoreDiffTestLaneCount: Int = CStoreBridgeResponseChannelCount
    def CMLUseMultiChannelLoader = !CLoadUseLegacyLoader

    def MinGroupSize = FPEparams.MinGroupSize //minimum compute group size of FPE
    def MinDataTypeWidth = FPEparams.MinDataTypeWidth //minimum data type width of FPE
    def ScaleElementWidth = FPEparams.ScaleElementWidth //FPE scale element width

    def cmptreelayers = FPEparams.cmptreelayers //FPE compute tree depth
    def fp8cmptreelayers = FPEparams.fp8cmptreelayers 

    def P3AddNum :Int = cuteParams.P3AddNum //number of P3 adders in FPE
    def P2AddNum :Int = cuteParams.P2AddNum

    def FP4P0AddNum :Int = FPEparams.FP4P0AddNum
    def FP4P1AddNum :Int = 16 / FP4P0AddNum

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
    def ResultWidth: Int = cuteParams.ResultWidth
    def MinGroupSize: Int = fpeParams.MinGroupSize
    def MinDataTypeWidth: Int = fpeParams.MinDataTypeWidth
    def ScaleElementWidth: Int = fpeParams.ScaleElementWidth
    def cmptreelayers: Int = fpeParams.cmptreelayers
    def fp8cmptreelayers: Int = fpeParams.fp8cmptreelayers

    def P3AddNum: Int = cuteParams.P3AddNum
    def P2AddNum: Int = cuteParams.P2AddNum
    def FP4P0AddNum: Int = fpeParams.FP4P0AddNum
    def FP4P1AddNum: Int = 16 / FP4P0AddNum

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
    // val CDCStoreAddr                        = Input(UInt(log2Ceil(CMatrixRegBankNEntries).W))

    val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input()(implicit p: Parameters) extends CuteBundle{
    val inst_uop = Output(UInt(32.W))
    val inst_src0 = Output(UInt(VectorWidth.W))
    val inst_src1 = Output(UInt(VectorWidth.W))
    val inst_src0_type = Output(UInt(2.W))//from register or input
    val inst_src1_type = Output(UInt(2.W))//from register or input
    val inst_dest_type = Output(UInt(2.W))//write back to register or output
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data ID
}

class VPUInterface_Output()(implicit p: Parameters) extends CuteBundle{
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data ID
    val stream_data = Output(UInt(VectorWidth.W))//stream data can carry extra information that is also returned; later it can be used to configure some VPU implicit registers or kept in VPU implicit registers visible to uops, such as the next scale or bias
}

class VPUInterfaceIO()(implicit p: Parameters) extends CuteBundle{
    val VPU_Input = (DecoupledIO(new VPUInterface_Input))
    val VPU_Output = Flipped(DecoupledIO(new VPUInterface_Output))
}


class VectorInterfaceIO()(implicit p: Parameters) extends CuteBundle{

    //can accept per-cycle data from AfterOpsInterface matching VectorWidth
    val VecTask = DecoupledIO(UInt(log2Ceil(VecTaskInstBufferSize).W))
    val VectorDataIn     = DecoupledIO(UInt((VectorWidth).W))
    val VectorDataOut     = Flipped(DecoupledIO(UInt((VectorWidth).W)))
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
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

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
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

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
    val MatrixRegId                       = UInt(CMatrixRegIdWidth.W)

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

class ApplicationTensor_A_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))//may be gone already
    val ApplicationTensor_A_Stride_M    = (UInt(MMUAddrWidth.W))//address offset increment for the next M
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

class AMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_A = new ApplicationTensor_A_Info
    
    val LoadTaskInfo = (new LoadTask_Info)

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

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

class BMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_B = (new ApplicationTensor_B_Info)

    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

    val Conherent                           = (Bool())      //whether coherence is needed
    val Is_Transpose                        = (Bool())      //whether transpose is needed

    val MicroTaskReady                      = Flipped(Bool())//can configure the next task
    val MicroTaskValid                      = (Bool())       //current task configuration is valid
    val MicroTaskEndValid                   = Flipped(Bool())//current task completed
    val MicroTaskEndReady                   = (Bool())       //current task completion acknowledged

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
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

class ApplicationTensor_B_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_B_Stride_N    = (UInt(MMUAddrWidth.W))//address offset increment for the next N
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    val HasTail                         = Bool()
    val TailByteMask                    = UInt(log2Ceil(outsideDataWidthByte + 1).W)
    val K_Beat_Count                    = UInt(MatrixRegMaxTensorDimBitSize.W)
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
class CMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    //this is Tensor C, the invariant part from the accumulator-register perspective

    val ApplicationTensor_C = (new ApplicationTensor_C_Info)

    val ApplicationTensor_D = (new ApplicationTensor_D_Info)

    val LoadTaskInfo = (new LoadTask_Info)

    val Conherent                           = (Bool())      //whether coherence is needed
    val Is_Transpose                        = (Bool())      //whether transpose is needed
    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(CMatrixRegIdWidth.W)

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
class ABDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(ABMatrixRegBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(DecoupledIO(Vec(ABMatrixRegNBanks, (UInt(log2Ceil(ABMatrixRegBankNEntries).W)))))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Valid(Vec(ABMatrixRegNBanks, UInt(ABMatrixRegEntryBitSize.W)))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

class ABScaleControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(ABMatrixRegBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(DecoupledIO(UInt(log2Ceil(ABScaleBankNEntries).W)))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Valid(Vec(ABScaleNSlices, UInt((ScaleWidth * ReduceGroupSize).W)))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

// unified AB MemoryLoader interface, with ZeroFill support
class ABMemoryLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    // When MemLoader is working, active will be true.
    // Otherwise, active will be false.
    val active = Input(Bool())
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(ABMatrixRegBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(Vec(ABMatrixRegNBanks, Valid(UInt(log2Ceil(ABMatrixRegBankNEntries).W))))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Flipped(Vec(ABMatrixRegNBanks, Valid(UInt(ABMatrixRegEntryBitSize.W))))
    val ByteMask = Flipped(Vec(ABMatrixRegNBanks, Valid(UInt(ABMatrixRegEntryByteSize.W))))
}

class ABScaleLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(AScratchpadBankNLines), and it is input data that requires handshaking
    val BankAddr = Flipped(Valid(UInt(log2Ceil(ABScaleBankNEntries).W)))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is ReduceWidthByte*8
    val Data = Flipped(Valid(Vec(ABScaleNSlices, UInt((ScaleWidth * ReduceGroupSize).W))))
    //chosen selects the ScratchPad; it is a Bool. We use double buffering, selecting one for output and one for loading
    // val Chosen = Input(Bool())
}

class CDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr is the row-select signal for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt whose width is log2Ceil(CMatrixRegBankNLines), and it is input data that requires handshaking
    val ReadBankAddr = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W)))))
    val WriteBankAddr = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W)))))
    //bankdata is the row data for each of the nbanks banks; it is a Vec with nbanks elements, each a UInt
    val ReadResponseData = (Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W))))
    val WriteRequestData = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W)))))
    //chosen selects the MatrixReg; it is a Bool. We use double buffering, selecting one for output and one for loading
    val ReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

class CMemoryLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val ReadRequestToMatrixReg = (new Bundle{
        val BankAddr = Flipped(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W))))
        val ReadResponseData = ((Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W)))))
    })
    val WriteRequestToMatrixReg = (new Bundle{
        val BankAddr = Flipped(Vec(CMatrixRegNBanks, (Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W)))))
        val Data = Flipped(Vec(CMatrixRegNBanks, (Valid(UInt(CMatrixRegEntryBitSize.W)))))
        val ByteMask = Flipped(Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryByteSize.W))))
    })
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
    val Request = Flipped(Vec(ABMatrixRegNBanks, DecoupledIO(new MMURequestIO)))
    //transaction ID of the TL link to which the read request is dispatched
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //the MemoryLoader is guaranteed to receive the response back!
    val Response = Vec(ABMatrixRegNBanks, DecoupledIO(new MMUResponseIO))
}

class MMU2TLIO(implicit p: Parameters) extends CuteBundle{

    //issued memory request
    val Request = Flipped(Vec(ABMatrixRegNBanks, DecoupledIO(new MMURequestIO)))
    //transaction ID of the TL link to which the read request is dispatched
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //the MemoryLoader is guaranteed to receive the response back!
    val Response = Vec(ABMatrixRegNBanks, DecoupledIO(new MMUResponseIO))
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

case object  CMemoryLoaderTaskType extends Field[UInt]{
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
