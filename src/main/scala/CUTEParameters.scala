
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


class DebugInfoIO()(implicit p: Parameters) extends CuteBundle{
    val DebugTimeStampe = UInt(64.W)
}

case object CuteParamsKey extends Field[CuteParams]

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
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_8Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 128,
        Tensor_K = 64,
        Matrix_MN = 8,
        ReduceWidthByte = 32,
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_32Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 256,
        Tensor_K = 64,
        Matrix_MN = 16,
        ReduceWidthByte = 32,
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
    )

    def CUTE_2Tops_debug = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_MN = 64,
        Tensor_K = 64,
        Matrix_MN = 4,
        ReduceWidthByte = 32,
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
    TaskCtrl_AutoClear = false, //任务控制器是否自动清除已完成指令  
    )

    // V3 Base Ext
    def baseparams = Cutev3extParams()

}

case class Cutev3extParams(
    val TaskCtrl_AutoClear :Boolean = true, //任务控制器是否自动清除已完成指令
)


object CuteFPEParams {

    def baseparams = CuteFPEParams()

}

case class CuteFPEParams(
    val cmptreelayers :Int = 4,
    val P3AddNum :Int = 4,
)

case class CuteParams(
    val outsideDataWidth :Int = 512, //cute对外访存的带宽
    val MemoryDataWidth :Int = 64,   //TODO:DRAM的访存通道的数据位宽

    val VectorWidth :Int = 256,      //向量流水线的宽度

    val L2NBanks :Int = 4,

    val ConvolutionApplicationConfigDataWidth :Int = 32, //卷积相关的配置信息的宽度
    val ConvolutionDIM_Max :Int = 65536, //卷积相关的配置信息的宽度
    val Convolution_Input_Height_Weight_Dim_Max :Int = 16384,
    val KernelSizeMax :Int = 16, //卷积核的最大尺寸
    val StrideSizeMax :Int = 4,  //步长的最大尺寸

    val ApplicationMaxTensorSize :Int = 65536, //最大可处理的程序的张量形状，

    val MMUAddrWidth :Int = 64 , //CUTE MMU的地址宽度

    val LLCSourceMaxNum :Int = 64, //LLC总线上的source最大数量 --> 这个参数和LLC的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必须大于LLC的访存延迟
    val MemorysourceMaxNum :Int = 64, //Memory总线上的source最大数量 --> 这个参数和Memory的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必顶大于Memory的访存延迟


    //MatrixReg中保存的张量形状
    val Tensor_MN :Int = 128,   //这里指要存的张量的M与N的大小
    val Tensor_K :Int = 64,    //这里指要存的张量的K(8bit/elment)的大小

    //矩阵乘计算单元MTE的形状
    val Matrix_MN :Int = 4,     //Matrix_MN，代表TE执行的矩阵乘法的M与N的大小
    val ReduceWidthByte :Int = 32,   //ReduceWidthByte 代表ReducePE进行内积时的数据宽度，单位是字节
    val ResultWidthByte :Int = 4,    //ResultWidthByte 代表ReducePE的结果宽度，单位是字节

    val ResultFIFODepth :Int = 8,    //乘累加FIFO的深度

    val AMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存AML的数据到CCSP的FIFO
    val BMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存BML的数据到CCSP的FIFO
    val CMemoryLoaderReadFromMatrixRegFIFODepth :Int = 4, //用于暂存CCSP的数据到CML的FIFO
    val CMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存CML的数据到CMReg的FIFO

    val VecTaskInstBufferDepth :Int = 32, //VecTask的指令缓冲深度
    val VecTaskInstBufferSize :Int = 8, //VecTask的指令缓冲的数量
    val VecTaskDataBufferDepth :Int = 4, //VecTask的指令缓冲深度掩盖从VecInterface到VPU的数据传输延迟即可

    val EnableDifftest: Boolean = false, //是否启用DiffTest

    val Debug : CuteDebugParams = CuteDebugParams.NoDebug, //调试参数
    val MMUParams: CuteMMUParams = CuteMMUParams.baseParams, //MMU的参数
    
    val v3config: Cutev3extParams = Cutev3extParams.NoextParams, //v3的扩展参数

    val FPEparams: CuteFPEParams = CuteFPEParams.baseparams //FPE的参数
) {

    //所有参数都必须是2的n次方
    require((outsideDataWidth & (outsideDataWidth - 1)) == 0, "outsideDataWidth must be power of 2")
    require((MemoryDataWidth & (MemoryDataWidth - 1)) == 0, "MemoryDataWidth must be power of 2")
    require((VectorWidth & (VectorWidth - 1)) == 0, "VectorWidth must be power of 2")
    require((ConvolutionApplicationConfigDataWidth & (ConvolutionApplicationConfigDataWidth - 1)) == 0, "ConvolutionApplicationConfigDataWidth must be power of 2")
    require((ConvolutionDIM_Max & (ConvolutionDIM_Max - 1)) == 0, "ConvolutionDIM_Max must be power of 2")
    require((Convolution_Input_Height_Weight_Dim_Max & (Convolution_Input_Height_Weight_Dim_Max - 1)) == 0, "Convolution_Input_Height_Weight_Dim_Max must be power of 2")
    require((KernelSizeMax & (KernelSizeMax - 1)) == 0, "KernelSizeMax must be power of 2")
    require((StrideSizeMax & (StrideSizeMax - 1)) == 0, "StrideSizeMax must be power of 2")
    require((ApplicationMaxTensorSize & (ApplicationMaxTensorSize - 1)) == 0, "ApplicationMaxTensorSize must be power of 2")
    require((MMUAddrWidth & (MMUAddrWidth - 1)) == 0, "MMUAddrWidth must be power of 2" )
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

    def outsideDataWidthByte = outsideDataWidth / 8
    def ReduceWidth = ReduceWidthByte * 8
    def ABMLNeedMRegFillTable = ReduceWidthByte < outsideDataWidthByte //内存返回的数据一周期写不完时ABML需要写回缓冲
    def ResultWidth = ResultWidthByte * 8
    def ApplicationMaxTensorSizeBitSize = log2Ceil(ApplicationMaxTensorSize) + 1
    def MMUDataWidth = outsideDataWidth //MMU的数据线宽度
    def MMUMaskWidth = MMUDataWidth / 8 //MMU的掩码线宽度
    def MMUDataWidthBitSize = log2Ceil(MMUDataWidth) + 1 //MMU的数据线有效数据位数
    def LLCSourceMaxNumBitSize = log2Ceil(LLCSourceMaxNum) + 1
    def MemorysourceMaxNumBitSize = log2Ceil(MemorysourceMaxNum) + 1
    def SoureceMaxNum = math.max(LLCSourceMaxNum, MemorysourceMaxNum)
    def SoureceMaxNumBitSize = log2Ceil(SoureceMaxNum) + 1

    def ReduceGroupSize  = Tensor_K/ReduceWidthByte    //这里指要存的张量的K的ReduceVector的数量！不是张量的K的大小
    def MatrixRegMaxTensorDim = Math.max(Tensor_MN, Math.max(Tensor_MN, ReduceGroupSize))
    def MatrixRegMaxTensorDimBitSize = log2Ceil(MatrixRegMaxTensorDim) + 1
    //A MatrixReg中保存的张量形状为M*K
    //A MatrixReg的大小为Tenser_M * ReduceGroupSize * ReduceWidthByte
    //128*(4*256/8)，单次读的张量为128*128的张量
    //单次计算需要的时间为(128/4)*(128/4)*4 = 4096拍，单次读需要128×4=512拍。
    //需要考虑MatrixReg的顺序读，需要考虑为MatrixReg分bank
    def ABMatrixRegSize = Tensor_MN * ReduceGroupSize * ReduceWidthByte //reduce
    def CMatrixRegSize = Tensor_MN * Tensor_MN * ResultWidthByte //result

    //目前的MatrixReg设计，分Tensor_T个bank，每次取Tensor_T个数据，根据取数逻辑，在不同的bank里取不同的数据，然后拼接
    def ABMatrixRegEntryByteSize = ReduceWidthByte //适合向TE供数的带宽
    def CMatrixRegEntryByteSize = Matrix_MN*ResultWidthByte //这个取数和存数的带宽
    def ABMatrixRegEntryBitSize = ReduceWidthByte * 8 //适合向TE供数的带宽
    def CMatrixRegEntryBitSize = Matrix_MN*ResultWidthByte * 8//这个取数和存数的带宽
    def ABMatrixRegNBanks = Matrix_MN //注意这里与Matrix_MN有强相关性，一般是Matrix_MN的整数倍
    def CMatrixRegNBanks = Matrix_MN //方便进行reorder
    def ABMatrixReg_Total_Bandwidth = ABMatrixRegNBanks * ABMatrixRegEntryByteSize  //ABMatrixReg的总带宽
    def CMatrixReg_Total_Bandwidth = CMatrixRegNBanks * CMatrixRegEntryByteSize  //CMatrixReg的总带宽
    def ABMatrixReg_Total_Bandwidth_Bit = ABMatrixRegNBanks * ABMatrixRegEntryByteSize * 8  //ABMatrixReg的总带宽
    def CMatrixReg_Total_Bandwidth_Bit = CMatrixRegNBanks * CMatrixRegEntryByteSize * 8  //CMatrixReg的总带宽
    def ABMatrixRegBankSize = ABMatrixRegSize / ABMatrixRegNBanks
    def CMatrixRegBankSize = CMatrixRegSize / CMatrixRegNBanks
    def ABMatrixRegBankNEntrys = ABMatrixRegBankSize / ABMatrixRegEntryByteSize
    def CMatrixRegBankNEntrys = CMatrixRegBankSize / CMatrixRegEntryByteSize

    require(ReduceGroupSize == 2, "ReduceGroupSize must be 2, Wait for update")
    require(outsideDataWidthByte <= Tensor_K, "outsideDataWidthByte must be less than or equal to Tensor_K, or a load will exceed the subtensor in micro load")

}

trait CUTEImplParameters{
    implicit val p: Parameters
    def cuteParams: CuteParams = p(CuteParamsKey)
    def MMUParams: CuteMMUParams = cuteParams.MMUParams
    def DebugParams: CuteDebugParams = cuteParams.Debug
    def v3config: Cutev3extParams = cuteParams.v3config
    def FPEparams: CuteFPEParams = cuteParams.FPEparams

    val DecodedAmuCtrlFIFODepth = 8  //解码后的AMU指令FIFO的深度
    val DecodedAmuCtrlFIFODepthBitSize = log2Ceil(DecodedAmuCtrlFIFODepth) //解码后的AMU指令FIFO的深度

    val ABMatrixRegCount = 4
    val CMatrixRegCount = 4
    val ABMatrixRegIdWidth = log2Ceil(ABMatrixRegCount)
    val CMatrixRegIdWidth = log2Ceil(CMatrixRegCount)

    val vpnBits = MMUParams.vpnBits
    val ppnBits = MMUParams.ppnBits
    val pgIdxBits = MMUParams.pgIdxBits
    val vaddrBits = MMUParams.vaddrBits
    val paddrBits = MMUParams.paddrBits
    val corePAddrBits = MMUParams.corePAddrBits

    val TaskCtrl_AutoClear = v3config.TaskCtrl_AutoClear

    val YJPDebugEnable      = DebugParams.YJPDebugEnable
    val YJPADCDebugEnable   = DebugParams.YJPADCDebugEnable
    val YJPBDCDebugEnable   = DebugParams.YJPBDCDebugEnable
    val YJPCDCDebugEnable   = DebugParams.YJPCDCDebugEnable
    val YJPAMLDebugEnable   = DebugParams.YJPAMLDebugEnable
    val YJPBMLDebugEnable   = DebugParams.YJPBMLDebugEnable
    val YJPCMLDebugEnable   = DebugParams.YJPCMLDebugEnable
    val YJPTASKDebugEnable        = DebugParams.YJPTASKDebugEnable
    val YJPVECDebugEnable         = DebugParams.YJPVECDebugEnable
    val YJPMACDebugEnable         = DebugParams.YJPMACDebugEnable
    val YJPPEDebugEnable          = DebugParams.YJPPEDebugEnable
    val YJPAfterOpsDebugEnable    = DebugParams.YJPAfterOpsDebugEnable

    val ConvolutionApplicationConfigDataWidth = cuteParams.ConvolutionApplicationConfigDataWidth
    val ConvolutionDIM_Max = cuteParams.ConvolutionDIM_Max
    val Convolution_Input_Height_Weight_Dim_Max = cuteParams.Convolution_Input_Height_Weight_Dim_Max
    val KernelSizeMax = cuteParams.KernelSizeMax
    val StrideSizeMax = cuteParams.StrideSizeMax
    val outsideDataWidth = cuteParams.outsideDataWidth
    val outsideDataWidthByte = cuteParams.outsideDataWidthByte
    val MemoryDataWidth = cuteParams.MemoryDataWidth
    val ReduceWidthByte = cuteParams.ReduceWidthByte
    val ReduceWidth = cuteParams.ReduceWidth
    val ABMLNeedMRegFillTable = cuteParams.ABMLNeedMRegFillTable
    val ResultWidthByte = cuteParams.ResultWidthByte
    val ResultWidth = cuteParams.ResultWidth
    val VectorWidth = cuteParams.VectorWidth
    val ApplicationMaxTensorSize = cuteParams.ApplicationMaxTensorSize
    val ApplicationMaxTensorSizeBitSize = cuteParams.ApplicationMaxTensorSizeBitSize
    val MMUAddrWidth = cuteParams.MMUAddrWidth
    val MMUDataWidth = cuteParams.MMUDataWidth
    val MMUMaskWidth = cuteParams.MMUMaskWidth
    val MMUDataWidthBitSize = cuteParams.MMUDataWidthBitSize
    val LLCSourceMaxNum = cuteParams.LLCSourceMaxNum
    val LLCSourceMaxNumBitSize = cuteParams.LLCSourceMaxNumBitSize
    val MemorysourceMaxNum = cuteParams.MemorysourceMaxNum
    val MemorysourceMaxNumBitSize = cuteParams.MemorysourceMaxNumBitSize
    val SoureceMaxNum = cuteParams.SoureceMaxNum
    val SoureceMaxNumBitSize = cuteParams.SoureceMaxNumBitSize
    val Tensor_MN = cuteParams.Tensor_MN
    val Tensor_K = cuteParams.Tensor_K
    val MatrixRegMaxTensorDim = cuteParams.MatrixRegMaxTensorDim
    val MatrixRegMaxTensorDimBitSize = cuteParams.MatrixRegMaxTensorDimBitSize
    val ABMatrixRegSize = cuteParams.ABMatrixRegSize
    val CMatrixRegSize = cuteParams.CMatrixRegSize
    val Matrix_MN = cuteParams.Matrix_MN
    val ABMatrixRegEntryByteSize = cuteParams.ABMatrixRegEntryByteSize
    val CMatrixRegEntryByteSize = cuteParams.CMatrixRegEntryByteSize
    val ABMatrixRegEntryBitSize = cuteParams.ABMatrixRegEntryBitSize
    val CMatrixRegEntryBitSize = cuteParams.CMatrixRegEntryBitSize
    val ABMatrixRegNBanks = cuteParams.ABMatrixRegNBanks
    val CMatrixRegNBanks = cuteParams.CMatrixRegNBanks
    val ABMatrixReg_Total_Bandwidth = cuteParams.ABMatrixReg_Total_Bandwidth
    val CMatrixReg_Total_Bandwidth = cuteParams.CMatrixReg_Total_Bandwidth
    val ABMatrixReg_Total_Bandwidth_Bit = cuteParams.ABMatrixReg_Total_Bandwidth_Bit
    val CMatrixReg_Total_Bandwidth_Bit = cuteParams.CMatrixReg_Total_Bandwidth_Bit
    val ABMatrixRegBankSize = cuteParams.ABMatrixRegBankSize
    val CMatrixRegBankSize = cuteParams.CMatrixRegBankSize
    val ABMatrixRegBankNEntrys = cuteParams.ABMatrixRegBankNEntrys
    val CMatrixRegBankNEntrys = cuteParams.CMatrixRegBankNEntrys
    val ResultFIFODepth = cuteParams.ResultFIFODepth
    val AMemoryLoaderReadFromMemoryFIFODepth = cuteParams.AMemoryLoaderReadFromMemoryFIFODepth
    val BMemoryLoaderReadFromMemoryFIFODepth = cuteParams.BMemoryLoaderReadFromMemoryFIFODepth
    val CMemoryLoaderReadFromMatrixRegFIFODepth = cuteParams.CMemoryLoaderReadFromMatrixRegFIFODepth
    val CMemoryLoaderReadFromMemoryFIFODepth = cuteParams.CMemoryLoaderReadFromMemoryFIFODepth
    val VecTaskInstBufferDepth = cuteParams.VecTaskInstBufferDepth
    val VecTaskInstBufferSize = cuteParams.VecTaskInstBufferSize
    val VecTaskDataBufferDepth = cuteParams.VecTaskDataBufferDepth
    val ReduceGroupSize = cuteParams.ReduceGroupSize
    val EnableDifftest = cuteParams.EnableDifftest
    val L2NBanks = cuteParams.L2NBanks

    val cmptreelayers = FPEparams.cmptreelayers //FPE的计算树层数
    val P3AddNum :Int = FPEparams.P3AddNum //FPE的P3加法器的数量
    val P2AddNum :Int = ReduceWidth / (P3AddNum * 16)
}

class CuteModule(implicit val p: Parameters) extends Module with CUTEImplParameters
class CuteBundle(implicit val p: Parameters) extends Bundle with CUTEImplParameters

class AfterOpsInterface()(implicit p: Parameters) extends CuteBundle{

    //每拍可接受一个来自CDC的与MReg和TE等宽的数据，并在自己模块内完成数据的拆分、重排、缩放、转置以及其他复杂向量任务
    val CDCDataToInterface     = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
    val InterfaceToCDCData     = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
    // val CDCStoreAddr                        = Input(UInt(log2Ceil(CMatrixRegBankNEntrys).W))

    val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input()(implicit p: Parameters) extends CuteBundle{
    val inst_uop = Output(UInt(32.W))
    val inst_src0 = Output(UInt(VectorWidth.W))
    val inst_src1 = Output(UInt(VectorWidth.W))
    val inst_src0_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_src1_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_dest_type = Output(UInt(2.W))//写回寄存器还是写回输出
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data的id
}

class VPUInterface_Output()(implicit p: Parameters) extends CuteBundle{
    val stream_id = Output(UInt(log2Ceil(Matrix_MN*Matrix_MN+10).W))//stream data的id
    val stream_data = Output(UInt(VectorWidth.W))//stream data还能存一些额外的信息，这些信息也会返回，后续可以用于配置VPU的部分隐式寄存器，或者留存在VPU的的隐式寄存器中，这些寄存器是uop可见的，如下一次的scale，下一次的bias等。
}

class VPUInterfaceIO()(implicit p: Parameters) extends CuteBundle{
    val VPU_Input = (DecoupledIO(new VPUInterface_Input))
    val VPU_Output = Flipped(DecoupledIO(new VPUInterface_Output))
}


class VectorInterfaceIO()(implicit p: Parameters) extends CuteBundle{

    //每拍可接受一个来自AfterOpsInterface的与VectorWidth等宽的数据
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

//描述计算时，数据流的访问顺序，transpose的时候就是N_M，不transpose的时候就是M_N
case object CaculateStreamStateType extends Field[UInt]{
    val CaculateStreamStateTypeBitWidth = 4

    val M_N = 0.U(CaculateStreamStateTypeBitWidth.W)
    val N_M = 1.U(CaculateStreamStateTypeBitWidth.W)
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

    //接受后操作的任务，有可能是重排序，有可能是缩放，有可能是转置，有可能是其他复杂后操作任务
    val Is_Transpose                        = (Bool())      //是否需要转置
    val Is_Reorder_Only_Ops                 = (Bool())      //是否只是重排，不需要计算
    val Is_EasyScale_Only_Ops               = (Bool())      //是否只是简单的缩放，不需要额外的后操作计算
    val Is_VecFIFO_Ops                      = (Bool())      //是否真的需要通用VecFIFO的参与

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成

    val CUTEuop                         = (new CUTE_uop)
}

class ADCMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_A = (new Bundle{
        // val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
        // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

    val Is_Transpose                        = (Bool())      //是否需要转置

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class BDCMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_B = (new Bundle{
        // val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
        // val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
    })

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

    val Is_Transpose                        = (Bool())      //是否需要转置

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
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

    val Is_Transpose                        = (Bool())      //是否需要转置
    val Is_AfterOps_Tile                    = (Bool())      //是否是需要执行后操作的Tile，包括转置等

    val Is_Reorder_Only_Ops                 = (Bool())      //是否只是重排，不需要计算
    val Is_EasyScale_Only_Ops               = (Bool())      //是否只是简单的缩放，不需要额外的后操作计算
    val Is_VecFIFO_Ops                      = (Bool())      //是否真的需要通用VecFIFO的参与

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
    val MicroTask_TEComputeEndValid         = Flipped(Bool())//已完成当前的TE的计算任务(但是还没有完成后操作)，但是可以提前释放TE的占用
    val MicroTask_TEComputeEndReady         = (Bool())       //已知晓当前的TE的计算任务完成

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class ApplicationTensor_A_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))//可能没有了
    val ApplicationTensor_A_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class AMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_A = new ApplicationTensor_A_Info
    
    val LoadTaskInfo = (new LoadTask_Info)

    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

    val Conherent                           = (Bool())      //是否需要coherent

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class ApplicationTensor_B_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_B_Stride_N    = (UInt(MMUAddrWidth.W))//下一个N需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class BMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_B = (new ApplicationTensor_B_Info)

    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(ABMatrixRegIdWidth.W)

    val Conherent                           = (Bool())      //是否需要coherent

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class ApplicationTensor_C_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_C_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class ApplicationTensor_D_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_D_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class LoadTask_Info()(implicit p: Parameters) extends CuteBundle{
    val Is_ZeroLoad = (Bool())
    val Is_RepeatRowLoad = (Bool())
    val Is_FullLoad = (Bool())
}
class CMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    //就是一个TensorC，是累加寄存器视角的不动的部分

    val ApplicationTensor_C = (new ApplicationTensor_C_Info)

    val ApplicationTensor_D = (new ApplicationTensor_D_Info)

    val LoadTaskInfo = (new LoadTask_Info)

    val StoreTaskInfo = (new Bundle{
        val Is_ZeroStore = (Bool())//暂时没有传递的参数
    })

    val Conherent                           = (Bool())      //是否需要coherent
    val Is_Transpose                        = (Bool())      //是否需要转置
    val MatrixRegTensor_M                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_N                 = (UInt(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegId                       = UInt(CMatrixRegIdWidth.W)

    val IsLoadMicroTask                     = (Bool())      //是否是Load任务
    val IsStoreMicroTask                    = (Bool())      //是否是Store任务

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成

    val pc                                = Option.when(EnableDifftest) (UInt(64.W))
    val coreid                            = Option.when(EnableDifftest) (UInt(8.W))
}

class MTEMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val dataType                            = Output(UInt(ElementDataType.DataTypeBitWidth.W))
}

class MRegControlInfo()(implicit p: Parameters) extends CuteBundle{
    val ADC_MReg_ID = UInt(ABMatrixRegIdWidth.W)
    val BDC_MReg_ID = UInt(ABMatrixRegIdWidth.W)
    val CDC_MReg_ID = UInt(CMatrixRegIdWidth.W)
    val AML_MReg_ID = UInt(ABMatrixRegIdWidth.W)
    val BML_MReg_ID = UInt(ABMatrixRegIdWidth.W)
    val CML_MReg_ID = UInt(CMatrixRegIdWidth.W)
}

//从MatrixReg中取数，要明确是从哪个bank里，取第几行的数据，然后完成数据拼接返回
//从哪个bank里取数据，取第几行的数据，是由datacontrol模块算出来的
//怎么在bank里编排数据，是由MemoryLoader模块填进去的
//MemoryLoader模块和datacontrol模块都有窗口期，可以完成数据额外的一些编排如量化、反稀疏、反量化、量化重排等等
//将MemoryLoader模块和datacontrol模块分开，是为了使用窗口期，让单读写口的MatrixReg可以独立运行
//有没有能同时读写的SRAM啊？我能保证不写同一块数据,还是先doublebuffer吧....
//我们考虑到回数的延迟，所以DataControl与MatrixReg之间也是有fifo的。考虑到后续的SRAM是一个简单模块，fifo要加在DataControl里，让MatrixReg尽可能简单。
class ABDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AMatrixRegBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(DecoupledIO(Vec(ABMatrixRegNBanks, (UInt(log2Ceil(ABMatrixRegBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Valid(Vec(ABMatrixRegNBanks, UInt(ABMatrixRegEntryBitSize.W)))
    //chosen是选择该MatrixReg的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

// 统一的AB MemoryLoader接口，支持ZeroFill功能
class ABMemoryLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(ABMatrixRegBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(Vec(ABMatrixRegNBanks, Valid(UInt(log2Ceil(ABMatrixRegBankNEntrys).W))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Flipped(Vec(ABMatrixRegNBanks, Valid(UInt(ABMatrixRegEntryBitSize.W))))
    //zerofill用于指示是否填零（统一支持，B矩阵可以不使用）
    val ZeroFill = Input(Vec(ABMatrixRegNBanks, Valid(UInt(log2Ceil(ABMatrixRegBankNEntrys).W))))
    //chosen是选择该MatrixReg的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class CDataControlMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(CMatrixRegBankNLines)，是输入的需要握手的数据
    val ReadBankAddr = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntrys).W)))))
    val WriteBankAddr = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt
    val ReadResponseData = (Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W))))
    val WriteRequestData = Flipped((Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W)))))
    //chosen是选择该MatrixReg的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    val ReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

class CMemoryLoaderMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val ReadRequestToMatrixReg = (new Bundle{
        val BankAddr = Flipped(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntrys).W))))
        val ReadResponseData = ((Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W)))))
    })
    val WriteRequestToMatrixReg = (new Bundle{
        val BankAddr = Flipped(Vec(CMatrixRegNBanks, (Valid(UInt(log2Ceil(CMatrixRegBankNEntrys).W)))))
        val Data = Flipped(Vec(CMatrixRegNBanks, (Valid(UInt(CMatrixRegEntryBitSize.W)))))
    })

    val ReadWriteRequest = Input(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((MatrixRegTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

//LocalMMU的接口
class LocalMMUIO(implicit p: Parameters) extends CuteBundle{

    //发出的访存请求
    val Request = Flipped(DecoupledIO(new Bundle{
        val RequestVirtualAddr = UInt(MMUAddrWidth.W)
        val RequestConherent = Bool()
        val RequestData = UInt(MMUDataWidth.W)
        val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
        val RequestType_isWrite = Bool()
    }))
    //读请求分发到的TL Link的事务编号
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //Memoryloader一定能保证收回！
    val Response = DecoupledIO(new Bundle{
        val ReseponseData = UInt(MMUDataWidth.W)
        val ReseponseConherent = Bool()
        val ReseponseSourceID = UInt(SoureceMaxNumBitSize.W)
    })
}

class MMU2TLIO(implicit p: Parameters) extends CuteBundle{

    //发出的访存请求
    val Request = Flipped(DecoupledIO(new Bundle{
        val RequestPhysicalAddr = UInt(MMUAddrWidth.W)
        val RequestConherent = Bool()
        val RequestData = UInt(MMUDataWidth.W)
        val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
        val RequestType_isWrite = Bool()
        val RequestMask = UInt(MMUMaskWidth.W) //MMU的Mask
        val MatrixIsAcc = Bool() // false for A/B matrix (tile matrix register), true for C matrix (accumulation matrix register)
    }))
    //读请求分发到的TL Link的事务编号
    val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
    val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

    //Memoryloader一定能保证收回！
    val Response = DecoupledIO(new Bundle{
        val ReseponseData = UInt(MMUDataWidth.W)
        val ReseponseConherent = Bool()
        val ReseponseSourceID = UInt(SoureceMaxNumBitSize.W)
    })
}

class FReducePEDataType(dataType: UInt){
//0:Int8, 1:FP16, 2:BF16, 3:TF32, 4:I8 * UI8, 5:UI8 * I8, 6:UI8 * UI8
    def AdataByteWidth: Int = dataType match {
        case ElementDataType.DataTypeI8I8I32 => 1
        case ElementDataType.DataTypeF16F16F32 => 2
        case ElementDataType.DataTypeBF16BF16F32 => 2
        case ElementDataType.DataTypeTF32TF32F32 => 4
        case ElementDataType.DataTypeI8U8I32 => 1
        case ElementDataType.DataTypeU8I8I32 => 1
        case ElementDataType.DataTypeU8U8I32 => 1
        case _ => 0 //未定义的类型，返回0字节宽度
    }

    def BdataByteWidth: Int = dataType match {
        case ElementDataType.DataTypeI8I8I32 => 1
        case ElementDataType.DataTypeF16F16F32 => 2
        case ElementDataType.DataTypeBF16BF16F32 => 2
        case ElementDataType.DataTypeTF32TF32F32 => 4
        case ElementDataType.DataTypeI8U8I32 => 1
        case ElementDataType.DataTypeU8I8I32 => 1
        case ElementDataType.DataTypeU8U8I32 => 1
        case _ => 0 //未定义的类型，返回0字节宽度
    }

    def CdataByteWidth: Int = dataType match {
        case ElementDataType.DataTypeI8I8I32 => 4
        case ElementDataType.DataTypeF16F16F32 => 4
        case ElementDataType.DataTypeBF16BF16F32 => 4
        case ElementDataType.DataTypeTF32TF32F32 => 4
        case ElementDataType.DataTypeI8U8I32 => 4
        case ElementDataType.DataTypeU8I8I32 => 4
        case ElementDataType.DataTypeU8U8I32 => 4
        case _ => 0 //未定义的类型，返回0字节宽度
    }

    def DdataByteWidth: Int = dataType match {
        case ElementDataType.DataTypeI8I8I32 => 4
        case ElementDataType.DataTypeF16F16F32 => 4
        case ElementDataType.DataTypeBF16BF16F32 => 4
        case ElementDataType.DataTypeTF32TF32F32 => 4
        case ElementDataType.DataTypeI8U8I32 => 4
        case ElementDataType.DataTypeU8I8I32 => 4
        case ElementDataType.DataTypeU8U8I32 => 4
        case _ => 0 //未定义的类型，返回0字节宽度
    }
}

//数据类型的样板类
case object  ElementDataType extends Field[UInt]{
    val DataTypeBitWidth = 3
    val DataTypeUndef   = 0.U(DataTypeBitWidth.W)
    val DataTypeWidth32 = 4.U(DataTypeBitWidth.W)
    val DataTypeWidth16 = 2.U(DataTypeBitWidth.W)
    val DataTypeWidth8  = 1.U(DataTypeBitWidth.W)
    val DataTypeWidth4  = 7.U(DataTypeBitWidth.W)

    val DataTypeI8I8I32     = 0.U(DataTypeBitWidth.W)     //I8 * I8 * I32
    val DataTypeF16F16F32   = 1.U(DataTypeBitWidth.W)     //FP16 * FP16 * FP32
    val DataTypeBF16BF16F32 = 2.U(DataTypeBitWidth.W)     //BF16 * BF16 * FP32
    val DataTypeTF32TF32F32 = 3.U(DataTypeBitWidth.W)     //TF32 * TF32 * FP32
    val DataTypeI8U8I32     = 4.U(DataTypeBitWidth.W)     //I8 * UI8 * I32
    val DataTypeU8I8I32     = 5.U(DataTypeBitWidth.W)     //U8 * I8 * I32
    val DataTypeU8U8I32     = 6.U(DataTypeBitWidth.W)     //U8 * U8 * I32

}

//工作任务的样板类
case object  CUTETaskType extends Field[UInt]{
    val CUTETaskBitWidth = 8
    val TaskTypeUndef = 0.U(CUTETaskBitWidth.W)
    val TaskTypeMatrixMul = 1.U(CUTETaskBitWidth.W)
    val TaskTypeConv = 2.U(CUTETaskBitWidth.W)
}

case object  CMemoryLoaderTaskType extends Field[UInt]{
    val TypeBitWidth = 4
    val TaskTypeUndef = 0.U(TypeBitWidth.W)
    val TaskTypeTensorZeroLoad = 1.U(TypeBitWidth.W) //直接将数据填充为0，实际上是什么也没做，默认可以写入SRAM，无视以前SRAM里面的数据即可
    val TaskTypeTensorRepeatRowLoad = 2.U(TypeBitWidth.W) //重复加载一行数据，实际上是什么也没做，默认可以写入SRAM，无视以前SRAM里面的数据即可
    val TaskTypeTensorLoad = 3.U(TypeBitWidth.W) //完整的加载所有数据
}
case object  MemoryOrderType extends Field[UInt]{
    val MemoryOrderTypeBitWidth = 8
    val OrderTypeUndef      = 0.U(MemoryOrderTypeBitWidth.W)
    val OrderType_Mb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Mb在前，Kb在后
    val OrderType_Mb_Nb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Mb在前，Nb在后
    val OrderType_Nb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Nb在前，Kb在后
    val OrderType_Nb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Nb在前，Mb在后
    val OrderType_Kb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Kb在前，Mb在后
    val OrderType_Kb_Nb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Kb在前，Nb在后

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
    // 对于单个MatrixReg，其并发的数据来源一共用3个，所以用3bit来表示。
    // 1. DataController对PE的输入数据的对MatrixReg读请求
    // 2. DataController将PE的输出结果送入MatrixReg写请求
    // 3. MemoryLoader对MatrixReg的写请求
    // 我们不知道MatrixReg的读写端口数量，所以用使能信号表示接受的数据来源
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
    val TaskTypeBitWidth = 2
    val TaskTypeMax = 3
    val AFirst = 0.U(TaskTypeBitWidth.W)
    val BFirst = 1.U(TaskTypeBitWidth.W)
    val CFirst = 2.U(TaskTypeBitWidth.W)
    // val DFirst = 3.U(TaskTypeBitWidth.W)
}