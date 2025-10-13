
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
        Tensor_M = 512,
        Tensor_N = 512,
        Tensor_K = 64,
        Matrix_M = 8,
        Matrix_N = 8,
        ReduceWidthByte = 32,
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_8Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_M = 128,
        Tensor_N = 128,
        Tensor_K = 64,
        Matrix_M = 8,
        Matrix_N = 8,
        ReduceWidthByte = 32,
        // Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_32Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_M = 256,
        Tensor_N = 256,
        Tensor_K = 64,
        Matrix_M = 16,
        Matrix_N = 16,
        ReduceWidthByte = 32,
        Debug = CuteDebugParams.AllDebugOn,
    )

    def CUTE_2Tops = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_M = 64,
        Tensor_N = 64,
        Tensor_K = 64,
        Matrix_M = 4,
        Matrix_N = 4,
        ReduceWidthByte = 32,
    )

    def CUTE_2Tops_debug = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_M = 64,
        Tensor_N = 64,
        Tensor_K = 64,
        Matrix_M = 4,
        Matrix_N = 4,
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
        Tensor_M = 512,
        Tensor_N = 512,
        Tensor_K = 64,
        Matrix_M = 8,
        Matrix_N = 8,
        ReduceWidthByte = 32,
        Debug = CuteDebugParams.AMLDebugEnable
    )

    def CUTE_8Tops_128SCP = baseParams.copy(
        outsideDataWidth = 512,
        LLCSourceMaxNum = 64,
        MemorysourceMaxNum = 64,
        Tensor_M = 128,
        Tensor_N = 128,
        Tensor_K = 64,
        Matrix_M = 8,
        Matrix_N = 8,
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

    val ConvolutionApplicationConfigDataWidth :Int = 32, //卷积相关的配置信息的宽度
    val ConvolutionDIM_Max :Int = 65536, //卷积相关的配置信息的宽度
    val Convolution_Input_Height_Weight_Dim_Max :Int = 16384,
    val KernelSizeMax :Int = 16, //卷积核的最大尺寸
    val StrideSizeMax :Int = 4,  //步长的最大尺寸

    val ApplicationMaxTensorSize :Int = 65536, //最大可处理的程序的张量形状，

    val MMUAddrWidth :Int = 64 , //CUTE MMU的地址宽度

    val LLCSourceMaxNum :Int = 64, //LLC总线上的source最大数量 --> 这个参数和LLC的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必须大于LLC的访存延迟
    val MemorysourceMaxNum :Int = 64, //Memory总线上的source最大数量 --> 这个参数和Memory的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必顶大于Memory的访存延迟


    //Scaratchpad中保存的张量形状
    val Tensor_M :Int = 128,   //这里指要存的张量的M的大小
    val Tensor_N :Int = 128,   //这里指要存的张量的N的大小
    val Tensor_K :Int = 64,    //这里指要存的张量的K(8bit/elment)的大小

    //矩阵乘计算单元MTE的形状
    val Matrix_M :Int = 4,     //Matrix_M，代表TE执行的矩阵乘法的M的大小
    val Matrix_N :Int = 4,     //Matrix_N，代表TE执行的矩阵乘法的N的大小
    val ReduceWidthByte :Int = 32,   //ReduceWidthByte 代表ReducePE进行内积时的数据宽度，单位是字节
    val ResultWidthByte :Int = 4,    //ResultWidthByte 代表ReducePE的结果宽度，单位是字节

    val ResultFIFODepth :Int = 8,    //乘累加FIFO的深度

    val AMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存AML的数据到CCSP的FIFO
    val BMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存BML的数据到CCSP的FIFO
    val CMemoryLoaderReadFromScratchpadFIFODepth :Int = 4, //用于暂存CCSP的数据到CML的FIFO
    val CMemoryLoaderReadFromMemoryFIFODepth :Int = 4, //用于暂存CML的数据到CSCP的FIFO

    val VecTaskInstBufferDepth :Int = 32, //VecTask的指令缓冲深度
    val VecTaskInstBufferSize :Int = 8, //VecTask的指令缓冲的数量
    val VecTaskDataBufferDepth :Int = 4, //VecTask的指令缓冲深度掩盖从VecInterface到VPU的数据传输延迟即可

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
    require((Tensor_M & (Tensor_M - 1)) == 0, "Tensor_M must be power of 2")
    require((Tensor_N & (Tensor_N - 1)) == 0, "Tensor_N must be power of 2")
    require((Tensor_K & (Tensor_K - 1)) == 0, "Tensor_K must be power of 2")
    require((Matrix_M & (Matrix_M - 1)) == 0, "Matrix_M must be power of 2")
    require((Matrix_N & (Matrix_N - 1)) == 0, "Matrix_N must be power of 2")
    require((ReduceWidthByte & (ReduceWidthByte - 1)) == 0, "ReduceWidthByte must be power of 2")
    require((ResultWidthByte & (ResultWidthByte - 1)) == 0, "ResultWidthByte must be power of 2")
    require((ResultFIFODepth & (ResultFIFODepth - 1)) == 0, "ResultFIFODepth must be power of 2")
    require((AMemoryLoaderReadFromMemoryFIFODepth & (AMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "AMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((BMemoryLoaderReadFromMemoryFIFODepth & (BMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "BMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((CMemoryLoaderReadFromScratchpadFIFODepth & (CMemoryLoaderReadFromScratchpadFIFODepth - 1)) == 0, "CMemoryLoaderReadFromScratchpadFIFODepth must be power of 2")
    require((CMemoryLoaderReadFromMemoryFIFODepth & (CMemoryLoaderReadFromMemoryFIFODepth - 1)) == 0, "CMemoryLoaderReadFromMemoryFIFODepth must be power of 2")
    require((VecTaskInstBufferDepth & (VecTaskInstBufferDepth - 1)) == 0, "VecTaskInstBufferDepth must be power of 2")
    require((VecTaskInstBufferSize & (VecTaskInstBufferSize - 1)) == 0, "VecTaskInstBufferSize must be power of 2")
    require((VecTaskDataBufferDepth & (VecTaskDataBufferDepth - 1)) == 0, "VecTaskDataBufferDepth must be power of 2")

    def outsideDataWidthByte = outsideDataWidth / 8
    def ReduceWidth = ReduceWidthByte * 8
    def ABMLNeedSCPFillTable = ReduceWidthByte < outsideDataWidthByte //内存返回的数据一周期写不完时ABML需要写回缓冲
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
    def ScaratchpadMaxTensorDim = Math.max(Tensor_M, Math.max(Tensor_N, ReduceGroupSize))
    def ScaratchpadMaxTensorDimBitSize = log2Ceil(ScaratchpadMaxTensorDim) + 1
    //AScaratchpad中保存的张量形状为M*K
    //AScaratchpad的大小为Tenser_M * ReduceGroupSize * ReduceWidthByte
    //128*(4*256/8)，单次读的张量为128*128的张量
    //单次计算需要的时间为(128/4)*(128/4)*4 = 4096拍，单次读需要128×4=512拍。
    //需要考虑Scaratchpad的顺序读，需要考虑为Scaratchpad分bank
    def AScratchpadSize = Tensor_M * ReduceGroupSize * ReduceWidthByte //reduce
    def BScratchpadSize = Tensor_N * ReduceGroupSize * ReduceWidthByte //reduce
    def CScratchpadSize = Tensor_M * Tensor_N * ResultWidthByte //result

    //目前的Scratchpad设计，分Tensor_T个bank，每次取Tensor_T个数据，根据取数逻辑，在不同的bank里取不同的数据，然后拼接
    def AScratchpadEntryByteSize = ReduceWidthByte //适合向TE供数的带宽
    def BScratchpadEntryByteSize = ReduceWidthByte 
    def CScratchpadEntryByteSize = Matrix_M*ResultWidthByte //这个取数和存数的带宽
    def AScratchpadEntryBitSize = ReduceWidthByte * 8 //适合向TE供数的带宽
    def BScratchpadEntryBitSize = ReduceWidthByte * 8
    def CScratchpadEntryBitSize = Matrix_M*ResultWidthByte * 8//这个取数和存数的带宽
    def AScratchpadNBanks = Matrix_M //注意这里与Matrix_M有强相关性，一般是Matrix_M的整数倍
    def BScratchpadNBanks = Matrix_N //这里与Matrix_N强相关
    def CScratchpadNBanks = Matrix_N //方便进行reorder
    def AScratchpad_Total_Bandwidth = AScratchpadNBanks * AScratchpadEntryByteSize  //ACSP的总带宽
    def BScratchpad_Total_Bandwidth = BScratchpadNBanks * BScratchpadEntryByteSize  //BCSP的总带宽
    def CScratchpad_Total_Bandwidth = CScratchpadNBanks * CScratchpadEntryByteSize  //CCSP的总带宽
    def AScratchpad_Total_Bandwidth_Bit = AScratchpadNBanks * AScratchpadEntryByteSize * 8  //ACSP的总带宽
    def BScratchpad_Total_Bandwidth_Bit = BScratchpadNBanks * BScratchpadEntryByteSize * 8  //BCSP的总带宽
    def CScratchpad_Total_Bandwidth_Bit = CScratchpadNBanks * CScratchpadEntryByteSize * 8  //CCSP的总带宽
    def AScratchpadBankSize = AScratchpadSize / AScratchpadNBanks
    def BScratchpadBankSize = BScratchpadSize / BScratchpadNBanks
    def CScratchpadBankSize = CScratchpadSize / CScratchpadNBanks
    def AScratchpadBankNEntrys = AScratchpadBankSize / AScratchpadEntryByteSize
    def BScratchpadBankNEntrys = BScratchpadBankSize / BScratchpadEntryByteSize
    def CScratchpadBankNEntrys = CScratchpadBankSize / CScratchpadEntryByteSize

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

    val MarcoInstFIFODepth = 4  //宏指令FIFO的深度
    val MarcoInstFIFODepthBitSize = log2Ceil(MarcoInstFIFODepth) //宏指令FIFO的深度

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
    val ABMLNeedSCPFillTable = cuteParams.ABMLNeedSCPFillTable
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
    val Tensor_M = cuteParams.Tensor_M
    val Tensor_N = cuteParams.Tensor_N
    val Tensor_K = cuteParams.Tensor_K
    val ScaratchpadMaxTensorDim = cuteParams.ScaratchpadMaxTensorDim
    val ScaratchpadMaxTensorDimBitSize = cuteParams.ScaratchpadMaxTensorDimBitSize
    val AScratchpadSize = cuteParams.AScratchpadSize
    val BScratchpadSize = cuteParams.BScratchpadSize
    val CScratchpadSize = cuteParams.CScratchpadSize
    val Matrix_M = cuteParams.Matrix_M
    val Matrix_N = cuteParams.Matrix_N
    val AScratchpadEntryByteSize = cuteParams.AScratchpadEntryByteSize
    val BScratchpadEntryByteSize = cuteParams.BScratchpadEntryByteSize
    val CScratchpadEntryByteSize = cuteParams.CScratchpadEntryByteSize
    val AScratchpadEntryBitSize = cuteParams.AScratchpadEntryBitSize
    val BScratchpadEntryBitSize = cuteParams.BScratchpadEntryBitSize
    val CScratchpadEntryBitSize = cuteParams.CScratchpadEntryBitSize
    val AScratchpadNBanks = cuteParams.AScratchpadNBanks
    val BScratchpadNBanks = cuteParams.BScratchpadNBanks
    val CScratchpadNBanks = cuteParams.CScratchpadNBanks
    val AScratchpad_Total_Bandwidth = cuteParams.AScratchpad_Total_Bandwidth
    val BScratchpad_Total_Bandwidth = cuteParams.BScratchpad_Total_Bandwidth
    val CScratchpad_Total_Bandwidth = cuteParams.CScratchpad_Total_Bandwidth
    val AScratchpad_Total_Bandwidth_Bit = cuteParams.AScratchpad_Total_Bandwidth_Bit
    val BScratchpad_Total_Bandwidth_Bit = cuteParams.BScratchpad_Total_Bandwidth_Bit
    val CScratchpad_Total_Bandwidth_Bit = cuteParams.CScratchpad_Total_Bandwidth_Bit
    val AScratchpadBankSize = cuteParams.AScratchpadBankSize
    val BScratchpadBankSize = cuteParams.BScratchpadBankSize
    val CScratchpadBankSize = cuteParams.CScratchpadBankSize
    val AScratchpadBankNEntrys = cuteParams.AScratchpadBankNEntrys
    val BScratchpadBankNEntrys = cuteParams.BScratchpadBankNEntrys
    val CScratchpadBankNEntrys = cuteParams.CScratchpadBankNEntrys
    val ResultFIFODepth = cuteParams.ResultFIFODepth
    val AMemoryLoaderReadFromMemoryFIFODepth = cuteParams.AMemoryLoaderReadFromMemoryFIFODepth
    val BMemoryLoaderReadFromMemoryFIFODepth = cuteParams.BMemoryLoaderReadFromMemoryFIFODepth
    val CMemoryLoaderReadFromScratchpadFIFODepth = cuteParams.CMemoryLoaderReadFromScratchpadFIFODepth
    val CMemoryLoaderReadFromMemoryFIFODepth = cuteParams.CMemoryLoaderReadFromMemoryFIFODepth
    val VecTaskInstBufferDepth = cuteParams.VecTaskInstBufferDepth
    val VecTaskInstBufferSize = cuteParams.VecTaskInstBufferSize
    val VecTaskDataBufferDepth = cuteParams.VecTaskDataBufferDepth
    val ReduceGroupSize = cuteParams.ReduceGroupSize

    val cmptreelayers = FPEparams.cmptreelayers //FPE的计算树层数
    val P3AddNum :Int = FPEparams.P3AddNum //FPE的P3加法器的数量
    val P2AddNum :Int = ReduceWidth / (P3AddNum * 16)
}

class CuteModule(implicit val p: Parameters) extends Module with CUTEImplParameters
class CuteBundle(implicit val p: Parameters) extends Bundle with CUTEImplParameters

//需要配置的信息：oc -- 控制器发来的oc编号, 
//                ic, oh, ow, kh, kw, ohb -- 外层循环次数,
//                icb -- 矩阵乘计算中的中间长度
//                paddingH, paddingW, strideH, strideW -- 卷积层属性

class TaskCtrlInfo()(implicit p: Parameters) extends CuteBundle{
    val ADC = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val ComputeEnd = Flipped(DecoupledIO(Bool()))
    })
    val BDC = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val ComputeEnd = Flipped(DecoupledIO(Bool()))
    })
    val CDC = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val ComputeEnd = Flipped(DecoupledIO(Bool()))
    })

    val AML = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val LoadEnd = Flipped(DecoupledIO(Bool()))
    })

    val BML = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val LoadEnd = Flipped(DecoupledIO(Bool()))
    })

    val CML = (new Bundle {
        // val TaskWorking = Valid(Bool())
        val TaskEnd = DecoupledIO(Bool())
        val LoadEnd = Flipped(DecoupledIO(Bool()))
    })

    val ScaratchpadChosen = (new Bundle {
        val ADataControllerChosenIndex = UInt(1.W)
        val BDataControllerChosenIndex = UInt(1.W)
        val CDataControllerChosenIndex = UInt(1.W)

        val AMemoryLoaderChosenIndex = UInt(1.W)
        val BMemoryLoaderChosenIndex = UInt(1.W)
        val CMemoryLoaderChosenIndex = UInt(1.W)
    })
}

//CUTE能接收的宏指令形式
class MacroInst()(implicit p: Parameters) extends CuteBundle{
    // Application_M,Application_N,Application_K代表这条宏指令要执行的MNK的长度
    // conv_stride是卷积的stride步长
    // kernel_size是卷积核的大小
    // kernel_stride是每一个index的卷积核的大小，我们要求卷积核的数据排布是(kh,kw,oc,ic)
    // stride_A、stride_B,stride_C,stride_D代表各个矩阵Reduce_DIM的长度(多少byte)
    // transpose_result表示结果是否需要进行转置
    // conv_oh_index,conv_ow_index代表当前处理的矩阵A的起始地址，落在卷积任务input的哪个index上
    // conv_oh_max,conv_ow_max与index配合，可以完成padding、stride等操作的加速
    // void * VectorOp,int VectorInst_Length代表了要融合的向量任务的具体指令和指令块长度。
    // ABCD分别为矩阵A、矩阵B和结果矩阵C，偏置矩阵D的起始地址。要求所有矩阵都是Reduce_DIM_FIRST的
    val ApplicationTensor_A_BaseVaddr = UInt(64.W) //矩阵A的起始地址
    val ApplicationTensor_B_BaseVaddr = UInt(64.W) //矩阵B的起始地址
    val ApplicationTensor_C_BaseVaddr = UInt(64.W) //矩阵C的起始地址
    val ApplicationTensor_D_BaseVaddr = UInt(64.W) //矩阵D的起始地址

    val ApplicationTensor_A_Stride = UInt(64.W) //矩阵A的stride,代表下一组Reduce_DIM需要增加多少地址偏移量，对于矩阵A[M][N]来说就是M+1需要增加多少地址偏移量，对于卷积[hw][c]来说，就是hw+1需要增加多少地址偏移量
    val ApplicationTensor_B_Stride = UInt(64.W) //矩阵B的stride,代表下一组Reduce_DIM需要增加多少地址偏移量
    val ApplicationTensor_C_Stride = UInt(64.W) //矩阵C的stride,代表下一组Reduce_DIM需要增加多少地址偏移量
    val ApplicationTensor_D_Stride = UInt(64.W) //矩阵D的stride,代表下一组Reduce_DIM需要增加多少地址偏移量

    val Application_M = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的M的大小，对于卷积来说[ohow][oc][ic]的[ohow]的大小
    val Application_N = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的N的大小，对于卷积来说[ohow][oc][ic]的[oc]的大小
    val Application_K = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的K的大小，对于卷积来说[ohow][oc][ic]的[ic]的大小

    val element_type = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵元素的数据类型
    val bias_data_type = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵乘的bias的数据类型
    val bias_type = UInt(CMemoryLoaderTaskType.TypeBitWidth.W) //矩阵乘的bias的存储类型

    val transpose_result = Bool() //结果是否需要转置，用于attention加速
    val conv_oh_index = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W) // TODO:位宽不够
    val conv_ow_index = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W)
    val conv_stride = UInt(log2Ceil(StrideSizeMax).W) //卷积的stride步长
    val conv_oh_max = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W) //卷积的oh长度，用于和stride配合完成padding等操作
    val conv_ow_max = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W) //卷积的ow长度，用于和stride配合完成padding等操作
    val conv_oh_per_add = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W)//避免在计算过程中进行除法运算，这里可以提前计算好
    val conv_ow_per_add = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W)//避免在计算过程中进行取余运算，这里可以提前计算好
    val kernel_size = UInt(log2Ceil(KernelSizeMax).W) //卷积核的大小
    val kernel_stride = UInt((64.W)) //kernel_stride是每一个index的卷积核的大小，我们要求卷积核的数据排布是(kh,kw,oc,ic)

    // val VectorOpInstAddr = UInt(64.W)
    // val VectorInst_Length = UInt(32.W)
}

//CUTE能接受的，Load模块能处理的微指令形式
class LoadMicroInst()(implicit p: Parameters) extends CuteBundle{
    // Application_M,Application_N,Application_K代表这条宏指令要执行的MNK的长度
    // conv_stride是卷积的stride步长
    // kernel_size是卷积核的大小
    // kernel_stride是每一个index的卷积核的大小，我们要求卷积核的数据排布是(kh,kw,oc,ic)
    // stride_A、stride_B,stride_C,stride_D代表各个矩阵Reduce_DIM的长度(多少byte)
    // transpose_result表示结果是否需要进行转置
    // conv_oh_index,conv_ow_index代表当前处理的矩阵A的起始地址，落在卷积任务input的哪个index上
    // conv_oh_max,conv_ow_max与index配合，可以完成padding、stride等操作的加速
    // void * VectorOp,int VectorInst_Length代表了要融合的向量任务的具体指令和指令块长度。
    // ABCD分别为矩阵A、矩阵B和结果矩阵C，偏置矩阵D的起始地址。要求所有矩阵都是Reduce_DIM_FIRST的
    val ApplicationTensor_A = new ApplicationTensor_A_Info
    val ApplicationTensor_B = new ApplicationTensor_B_Info
    val ApplicationTensor_C = new ApplicationTensor_C_Info//大多时候是0，所以存在一个大寄存器里可能会亏？
    val CLoadTaskInfo = new LoadTask_Info

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

    //知道卷积核的位置和当前的OHOW，确认是否需要padding进行0填充
    val Convolution_Current_OH_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_OW_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_KH_Index        = (UInt(log2Ceil(KernelSizeMax).W))
    val Convolution_Current_KW_Index        = (UInt(log2Ceil(KernelSizeMax).W))

    val ConherentA                           = (Bool())      //是否需要coherent
    val ConherentB                           = (Bool())      //是否需要coherent
    val ConherentC                           = (Bool())      //是否需要coherent

    val Is_A_Work                          = (Bool())      //是否需要工作
    val Is_B_Work                          = (Bool())      //是否需要工作
    val Is_C_Work                          = (Bool())      //是否需要工作

    val A_SCPID                            = UInt(2.W)//代表Load的结果存在哪个SCP上，这个值保存在Resoure_Info里
    val B_SCPID                            = UInt(2.W)//代表Load的结果存在哪个SCP上，这个值保存在Resoure_Info里
    val C_SCPID                            = UInt(2.W)//代表Load的结果存在哪个SCP上，这个值保存在Resoure_Info里

    val IsTranspose                         = (Bool())      //是否需要转置
    

    // val VectorOpInstAddr = UInt(64.W)
    // val VectorInst_Length = UInt(32.W)
}

//用于描述微指令间依赖关系和资源依赖关系的信息，用于下一阶段的微指令(Compute)能否发射的信息
class LoadMicroInst_Resource_Info()(implicit p: Parameters) extends CuteBundle{
    // Application_M,Application_N,Application_K代表这条宏指令要执行的MNK的长度
    val A_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val B_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    
}

//CUTE能接受的，Compute模块能处理的微指令形式
class ComputeMicroInst()(implicit p: Parameters) extends CuteBundle{
    val DataType_A                          = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵A的数据类型
    val DataType_B                          = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵B的数据类型
    val DataType_C                          = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵C的数据类型
    val DataType_D                          = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵D的数据类型

    val Have_Aops                         = Bool()      //是否有AfterOps
    val Is_AfterOps_Tile = Bool()            //是否是AfterOps的Tile
    val Is_Transpose = Bool()                //是否是Transpose的Tile
    val Is_Reorder_Only_Ops = Bool()         //是否是Reorder的Tile
    val Is_EasyScale_Only_Ops = Bool()       //是否是EasyScale的Tile
    val Is_VecFIFO_Ops = Bool()              //是否是VecOps的Tile

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val Have_Store_Micro_Inst               = (Bool())      //是否有依赖于这条计算指令的Store的指令
}

//用于描述微指令间依赖关系和资源依赖关系的信息，用于下一阶段的微指令(Store)能否发射的信息
class ComputeMicroInst_Resource_Info()(implicit p: Parameters) extends CuteBundle{
    val A_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val B_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val Load_Micro_Inst_FIFO_Index = UInt(4.W)//代表Load的指令在队列中的位置
}
//CUTE能接受的，Store模块能处理的微指令形式
class StoreMicroInst()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_D = new ApplicationTensor_D_Info
    val Conherent                           = (Bool())      //是否需要coherent
    val Is_Transpose                        = (Bool())      //是否需要转置
    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val Is_Last_Store                       = (Bool())      //是否是最后一次store
}

//用于描述微指令间依赖关系和资源依赖关系的信息，用于下一阶段的微指令(Vec或者唤醒CPU)能否发射的信息
class StoreMicroInst_Resource_Info()(implicit p: Parameters) extends CuteBundle{
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val Compute_Micro_Inst_FIFO_Index = UInt(4.W)//代表Compute的指令在队列中的位置
    val Marco_Inst_FIFO_Index = UInt(4.W)//代表Marco的指令在队列中的位置
}

class AfterOpsInterface()(implicit p: Parameters) extends CuteBundle{

    //每拍可接受一个来自CDC的与SCP和TE等宽的数据，并在自己模块内完成数据的拆分、重排、缩放、转置以及其他复杂向量任务
    val CDCDataToInterface     = DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W))
    val InterfaceToCDCData     = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W)))
    // val CDCStoreAddr                        = Input(UInt(log2Ceil(CScratchpadBankNEntrys).W))

    val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input()(implicit p: Parameters) extends CuteBundle{
    val inst_uop = Output(UInt(32.W))
    val inst_src0 = Output(UInt(VectorWidth.W))
    val inst_src1 = Output(UInt(VectorWidth.W))
    val inst_src0_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_src1_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_dest_type = Output(UInt(2.W))//写回寄存器还是写回输出
    val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
}

class VPUInterface_Output()(implicit p: Parameters) extends CuteBundle{
    val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
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

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

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

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

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

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

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

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

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
}

class ApplicationTensor_A_Info()(implicit p: Parameters) extends CuteBundle{
    val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))//可能没有了
    val ApplicationTensor_A_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val Convolution_OH_DIM_Length       = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_OW_DIM_Length       = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Stride_H            = (UInt(log2Ceil(StrideSizeMax).W))
    val Convolution_Stride_W            = (UInt(log2Ceil(StrideSizeMax).W))
    val Convolution_KH_DIM_Length       = (UInt(log2Ceil(KernelSizeMax).W))
    val Convolution_KW_DIM_Length       = (UInt(log2Ceil(KernelSizeMax).W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class AMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_A = new ApplicationTensor_A_Info

    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

    //知道卷积核的位置和当前的OHOW，确认是否需要padding进行0填充
    val Convolution_Current_OH_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_OW_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_KH_Index        = (UInt(log2Ceil(KernelSizeMax).W))
    val Convolution_Current_KW_Index        = (UInt(log2Ceil(KernelSizeMax).W))

    val Conherent                           = (Bool())      //是否需要coherent

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class ApplicationTensor_B_Info()(implicit p: Parameters) extends CuteBundle{
        val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
        val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val ApplicationTensor_B_Stride_N    = (UInt(MMUAddrWidth.W))//下一个N需要增加多少的地址偏移量
        // val Convolution_OC_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val Convolution_KH_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val Convolution_KW_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class BMLMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{

    val ApplicationTensor_B = (new ApplicationTensor_B_Info)

    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

    //知道卷积核的位置，确认kernel的具体BlockTensor_B_BaseVaddr，那这个不需要传进来，TaskCtrl算完送进来就行了。
    // val Convolution_Current_KH_Index        = (UInt(ConvolutionApplicationConfigDataWidth.W))
    // val Convolution_Current_KW_Index        = (UInt(ConvolutionApplicationConfigDataWidth.W))

    val Conherent                           = (Bool())      //是否需要coherent

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
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
    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

    val IsLoadMicroTask                     = (Bool())      //是否是Load任务
    val IsStoreMicroTask                    = (Bool())      //是否是Store任务

    val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
    val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
    val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
    val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class MTEMicroTaskConfigIO()(implicit p: Parameters) extends CuteBundle{
    val dataType                            = Output(UInt(ElementDataType.DataTypeBitWidth.W))
}

class SCPControlInfo()(implicit p: Parameters) extends CuteBundle{
    val ADC_SCP_ID = UInt(1.W)
    val BDC_SCP_ID = UInt(1.W)
    val CDC_SCP_ID = UInt(1.W)
    val AML_SCP_ID = UInt(1.W)
    val BML_SCP_ID = UInt(1.W)
    val CML_SCP_ID = UInt(1.W)
}




class ConfigInfoIO()(implicit p: Parameters) extends CuteBundle{

    val MMUConfig = Flipped(new MMUConfigIO)
    val ApplicationTensor_A = (new Bundle{
        val ApplicationTensor_A_BaseVaddr = (UInt(MMUAddrWidth.W))
        val BlockTensor_A_BaseVaddr       = (UInt(MMUAddrWidth.W))
        val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
        val Conherent                     = (Bool())
    })

    val ApplicationTensor_B = (new Bundle{
        val ApplicationTensor_B_BaseVaddr = (UInt(MMUAddrWidth.W))
        val BlockTensor_B_BaseVaddr       = (UInt(MMUAddrWidth.W))
        val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
        val Conherent                     = (Bool())
    })
    
    val ApplicationTensor_C = (new Bundle{
        val ApplicationTensor_C_BaseVaddr = (UInt(MMUAddrWidth.W))
        val BlockTensor_C_BaseVaddr       = (UInt(MMUAddrWidth.W))
        val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
        val Conherent                     = (Bool())
    })
    val ApplicationTensor_D = (new Bundle{
        val ApplicationTensor_D_BaseVaddr = (UInt(MMUAddrWidth.W))
        val BlockTensor_D_BaseVaddr       = (UInt(MMUAddrWidth.W))
        val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
        val Conherent                     = (Bool())
    })
    val ApplicationTensor_M = (UInt(ApplicationMaxTensorSizeBitSize.W))
    val ApplicationTensor_N = (UInt(ApplicationMaxTensorSizeBitSize.W))
    val ApplicationTensor_K = (UInt(ApplicationMaxTensorSizeBitSize.W))

    val ScaratchpadTensor_M = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的M
    val ScaratchpadTensor_N = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的N
    val ScaratchpadTensor_K = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的K

    val ComputeGo = (Bool())


    val dataType = (UInt(ElementDataType.DataTypeBitWidth.W)) //0-矩阵乘，1-卷积
    val taskType = (UInt(CUTETaskType.CUTETaskBitWidth.W)) //1-32位，2-16位， 4-32位
    // val ExternalReduceSize = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val CMemoryLoaderConfig = (new Bundle{
        val MemoryOrder = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
        val TaskType = (UInt(CMemoryLoaderTaskType.TypeBitWidth.W))
    })

}

//从Scaratchpad中取数，要明确是从哪个bank里，取第几行的数据，然后完成数据拼接返回
//从哪个bank里取数据，取第几行的数据，是由datacontrol模块算出来的
//怎么在bank里编排数据，是由MemoryLoader模块填进去的
//MemoryLoader模块和datacontrol模块都有窗口期，可以完成数据额外的一些编排如量化、反稀疏、反量化、量化重排等等
//将MemoryLoader模块和datacontrol模块分开，是为了使用窗口期，让单读写口的ScarchPad可以独立运行
//有没有能同时读写的SRAM啊？我能保证不写同一块数据,还是先doublebuffer吧....
//我们考虑到回数的延迟，所以DataControl与Scarachpad之间也是有fifo的。考虑到后续的SRAM是一个简单模块，fifo要加在DataControl里，让Scarachpad尽可能简单。
class ADataControlScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(DecoupledIO(Vec(AScratchpadNBanks, (UInt(log2Ceil(AScratchpadBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Valid(Vec(AScratchpadNBanks, UInt(AScratchpadEntryBitSize.W)))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class AMemoryLoaderScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankId = Flipped(Valid(UInt(log2Ceil(AScratchpadNBanks).W)))
    val BankAddr = Flipped(Vec(AScratchpadNBanks, Valid(UInt(log2Ceil(AScratchpadBankNEntrys).W))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Flipped(Vec(AScratchpadNBanks, Valid(UInt(AScratchpadEntryBitSize.W))))
    //zerofill用于指示是否填零
    val ZeroFill = Input(Vec(AScratchpadNBanks, Valid(UInt(log2Ceil(AScratchpadBankNEntrys).W))))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class BDataControlScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(DecoupledIO(Vec(BScratchpadNBanks, (UInt(log2Ceil(BScratchpadBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Valid(Vec(BScratchpadNBanks, UInt(BScratchpadEntryBitSize.W)))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class BMemoryLoaderScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankId = Flipped(Valid(UInt(log2Ceil(BScratchpadNBanks).W)))
    val BankAddr = Flipped(Vec(BScratchpadNBanks, Valid(UInt(log2Ceil(BScratchpadBankNEntrys).W))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Flipped(Vec(BScratchpadNBanks, Valid(UInt(BScratchpadEntryBitSize.W))))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}


class CDataControlScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val ReadBankAddr = Flipped((Vec(CScratchpadNBanks, Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W)))))
    val WriteBankAddr = Flipped((Vec(CScratchpadNBanks, Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt
    val ReadResponseData = (Vec(CScratchpadNBanks, Valid(UInt(CScratchpadEntryBitSize.W))))
    val WriteRequestData = Flipped((Vec(CScratchpadNBanks, Valid(UInt(CScratchpadEntryBitSize.W)))))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    val ReadWriteRequest = Input(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
    // val Chosen = Input(Bool())
}

class CMemoryLoaderScaratchpadIO(implicit p: Parameters) extends CuteBundle{
    val ReadRequestToScarchPad = (new Bundle{
        val BankAddr = Flipped(Vec(CScratchpadNBanks, Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W))))
        val ReadResponseData = ((Vec(CScratchpadNBanks, Valid(UInt(CScratchpadEntryBitSize.W)))))
    })
    val WriteRequestToScarchPad = (new Bundle{
        val BankAddr = Flipped(Vec(CScratchpadNBanks, (Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W)))))
        val Data = Flipped(Vec(CScratchpadNBanks, (Valid(UInt(CScratchpadEntryBitSize.W)))))
    })

    val ReadWriteRequest = Input(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
    val ReadWriteResponse = Output(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
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

class CTRLCounter(implicit p: Parameters) extends CuteBundle{
    val ALoad = Bool()
    val BLoad = Bool() //是否是B的Load任务 
    val CLoad = Bool() //是否是C的Load任务
    val DStore = Bool() //是否是D的Store任务
    val InstQueueEmpty = Bool() //指令队列是否为空
    val getConfigured = Bool() //是否已经开始配置
    val AOPBusy = Bool() //是否有后操作任务在执行
    val computeInstQueueEmpty = Bool() //计算指令队列是否为空
    val computeInstCanIssue = Bool() //计算指令是否可以发出
    val InstCanDecode = Bool() //指令是否可以解码
}

class CUTECounter(implicit p: Parameters) extends CuteBundle{
    val computeBusy = Bool() //计算是否忙
    val ALoad = Bool() //是否是A的Load任务
    val BLoad = Bool() //是否是B的Load任务
    val CLoad = Bool() //是否是C的Load任务
    val DStore = Bool() //是否是D的Store任务
    val InstQueueEmpty = Bool() //指令队列是否为空
    val getConfigured = Bool() //是否已经开始配置
    val AOPBusy = Bool() //是否有后操作任务在执行
    val computeInstQueueEmpty = Bool() //计算指令队列是否为空
    val computeInstCanIssue = Bool() //计算指令是否可以发出
    val InstCanDecode = Bool() //指令是否可以解码
    val mmu_req_valid = Bool()
    val mmu_req_ready = Bool()
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


class ScaratchpadTaskDecode(ScaratchpadTask: UInt) extends Field[UInt]{

    def IsRead : Bool = ScaratchpadTask(ScaratchpadTaskType.EnableReadFromDataController) || ScaratchpadTask(ScaratchpadTaskType.EnableReadFromMemoryLoader)
    def IsWrite : Bool = ScaratchpadTask(ScaratchpadTaskType.EnableWriteFromDataController) || ScaratchpadTask(ScaratchpadTaskType.EnableWriteFromMemoryLoader)

    def IsReadFromDataController: Bool = ScaratchpadTask(ScaratchpadTaskType.ReadFromDataControllerIndex)
    def IsWriteFromDataController: Bool = ScaratchpadTask(ScaratchpadTaskType.WriteFromDataControllerIndex)
    def IsWriteFromMemoryLoader: Bool = ScaratchpadTask(ScaratchpadTaskType.WriteFromMemoryLoaderIndex)
    def IsReadFromMemoryLoader  : Bool = ScaratchpadTask(ScaratchpadTaskType.ReadFromMemoryLoaderIndex)

    // def ReadSrc: UInt = ScaratchpadTask
}

case object ScaratchpadTaskType extends Field[UInt]{
    val TaskTypeBitWidth = 4    //对于单个Scaratchpad，其并发的数据来源一共用3个，所以用3bit来表示。1.DataController对PE的输入数据的对ScarchPad读请求 2.DataController将PE的输出结果送入ScaratchPad写请求 3。MemoryLoader对ScarchPad的写请求
    //我们不知道Scaratchpad的读写端口数量，所以用使能信号表示接受的数据来源
    val EnableReadFromDataController = 1.U(TaskTypeBitWidth.W)
    val EnableWriteFromDataController = 2.U(TaskTypeBitWidth.W)
    val EnableWriteFromMemoryLoader = 4.U(TaskTypeBitWidth.W)
    val EnableReadFromMemoryLoader = 8.U(TaskTypeBitWidth.W)
    val ReadFromDataControllerIndex = 0
    val WriteFromDataControllerIndex = 1
    val WriteFromMemoryLoaderIndex = 2
    val ReadFromMemoryLoaderIndex = 3
}

class ScaratchpadTask(implicit p: Parameters) extends CuteBundle{
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