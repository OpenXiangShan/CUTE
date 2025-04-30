
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import boom.exu.ygjk._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile._
// import boom.common.BoomBundle
// import freechips.rocketchip.regmapper.RRTest0Map

case class MMAccConfig()
case object MMAccKey extends Field[Option[MMAccConfig]](None)
case object BuildDMAygjk extends Field[Seq[Parameters => LazyRoCC]](Nil)
// class Withacc_MMacc extends Config((site,here,up) => {  
//     case BuildYGAC =>
//         (p:Parameters) => {          
//             val myAccel = Module(new MMacc)
//             myAccel
//         }
//     case MMAccKey => true
//     case BuildDMAygjk => true
//     }
// )

class CUTECrossingParams(
  override val MemDirectMaster: TilePortParamsLike = TileMasterPortParams(where = MBUS)
) extends RocketCrossingParams

class DebugInfoIO() extends Bundle with HWParameters{
    val DebugTimeStampe = UInt(64.W)
}

trait HWParameters{

    val vpnBits = 12
    val ppnBits = 12
    val pgIdxBits = 12
    val vaddrBits = 39
    val paddrBits = 39
    val corePAddrBits = 64

    val YJPDebugEnable      = true
    val YJPADCDebugEnable   = true
    val YJPBDCDebugEnable   = true
    val YJPCDCDebugEnable   = true
    val YJPAMLDebugEnable   = true
    val YJPBMLDebugEnable   = true
    val YJPCMLDebugEnable   = true

    val YJPTASKDebugEnable  = true
    val YJPVECDebugEnable   = true
    val YJPMACDebugEnable   = true
    val YJPPEDebugEnable    = true
    val YJPAfterOpsDebugEnable    = true

    // val YJPDebugEnable      = false
    // val YJPADCDebugEnable   = false
    // val YJPBDCDebugEnable   = false
    // val YJPCDCDebugEnable   = false
    // val YJPAMLDebugEnable   = false
    // val YJPBMLDebugEnable   = false
    // val YJPCMLDebugEnable   = false

    // val YJPTASKDebugEnable        = false
    // val YJPVECDebugEnable         = false
    // val YJPMACDebugEnable         = false
    // val YJPPEDebugEnable          = false
    // val YJPAfterOpsDebugEnable    = false

    val ConvolutionApplicationConfigDataWidth = 32 //卷积相关的配置信息的宽度
    val ConvolutionDIM_Max = 65536 //卷积相关的配置信息的宽度
    val Convolution_Input_Height_Weight_Dim_Max = 1024
    val KernelSizeMax = 16 //卷积核的最大尺寸
    val StrideSizeMax = 4  //步长的最大尺寸
//LLC的数据线宽度
    val LLCDataWidth = 256      //TODO:这个值需要从chipyard的config中来
    val LLCDataWidthByte = LLCDataWidth / 8
//Memory的数据线宽度
    val MemoryDataWidth = 64    //TODO:这个值需要从chipyard的config中来
//ReduceWidthByte 代表ReducePE进行内积时的数据宽度，单位是字节
    val ReduceWidthByte = 32
    val ReduceWidth = ReduceWidthByte * 8

    val ABMLNeedSCPFillTable = ReduceWidthByte < LLCDataWidthByte //内存返回的数据一周期写不完时ABML需要写回缓冲
//ResultWidthByte 代表ReducePE的结果宽度，单位是字节
    val ResultWidthByte = 4
    val ResultWidth = ResultWidthByte * 8

    val VectorWidth = 256   //向量流水线的宽度

//最大可处理的程序的张量形状，
    val ApplicationMaxTensorSize = 65536
    val ApplicationMaxTensorSizeBitSize = log2Ceil(ApplicationMaxTensorSize) + 1
//MMU的地址宽度
    val MMUAddrWidth = 64
//MMU的数据线宽度
    val MMUDataWidth = LLCDataWidth //TODO:ReduceWidth等于LLCDataWidth，以后得改
//MMU的数据线有效数据位数
    val MMUDataWidthBitSize = log2Ceil(MMUDataWidth) + 1

//LLC总线上的source最大数量 --> 这个参数和LLC的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必须大于LLC的访存延迟
    val LLCSourceMaxNum = 64
    val LLCSourceMaxNumBitSize = log2Ceil(LLCSourceMaxNum) + 1
//Memory总线上的source最大数量 --> 这个参数和Memory的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必顶大于Memory的访存延迟
    val MemorysourceMaxNum = 64
    val MemorysourceMaxNumBitSize = log2Ceil(MemorysourceMaxNum) + 1

    val SoureceMaxNum = math.max(LLCSourceMaxNum, MemorysourceMaxNum)
    val SoureceMaxNumBitSize = log2Ceil(SoureceMaxNum) + 1


//Scaratchpad中保存的张量形状
    val Tensor_M = 64   //这里指要存的张量的M的大小
    val Tensor_N = 64   //这里指要存的张量的N的大小
    val Tensor_K = 2    //这里指要存的张量的K的ReduceVector的数量！不是张量的K的大小
    val Tensor_K_Element_Length = Tensor_K * ReduceWidthByte
    val Tensor_K_PerK_Element_Length = Tensor_K_Element_Length / Tensor_K
    val ScaratchpadMaxTensorDim = Math.max(Tensor_M, Math.max(Tensor_N, Tensor_K))
    val ScaratchpadMaxTensorDimBitSize = log2Ceil(ScaratchpadMaxTensorDim) + 1
//AScaratchpad中保存的张量形状为M*K
//AScaratchpad的大小为Tenser_M * Tensor_K * ReduceWidthByte
//128*(4*256/8)，单次读的张量为128*128的张量
//单次计算需要的时间为(128/4)*(128/4)*4 = 4096拍，单次读需要128×4=512拍。
//需要考虑Scaratchpad的顺序读，需要考虑为Scaratchpad分bank
    val AScratchpadSize = Tensor_M * Tensor_K * ReduceWidthByte //reduce
    val BScratchpadSize = Tensor_N * Tensor_K * ReduceWidthByte //reduce
    val CScratchpadSize = Tensor_M * Tensor_N * ResultWidthByte //result
//Matrix_M，代表TE执行的矩阵乘法的M的大小
    val Matrix_M = 4
//Matrix_N，代表TE执行的矩阵乘法的N的大小
    val Matrix_N = 4

//目前的Scratchpad设计，分Tensor_T个bank，每次取Tensor_T个数据，根据取数逻辑，在不同的bank里取不同的数据，然后拼接

    val AScratchpadEntryByteSize = ReduceWidthByte //适合向TE供数的带宽
    val BScratchpadEntryByteSize = ReduceWidthByte 
    val CScratchpadEntryByteSize = Matrix_M*ResultWidthByte //这个取数和存数的带宽

    val AScratchpadEntryBitSize = ReduceWidthByte * 8 //适合向TE供数的带宽
    val BScratchpadEntryBitSize = ReduceWidthByte * 8
    val CScratchpadEntryBitSize = Matrix_M*ResultWidthByte * 8//这个取数和存数的带宽

    val AScratchpadNBanks = Matrix_M //注意这里与Matrix_M有强相关性，一般是Matrix_M的整数倍
    val BScratchpadNBanks = Matrix_N //这里与Matrix_N强相关
    val CScratchpadNBanks = Matrix_N //方便进行reorder

    val AScratchpad_Total_Bandwidth = AScratchpadNBanks * AScratchpadEntryByteSize  //ACSP的总带宽
    val BScratchpad_Total_Bandwidth = BScratchpadNBanks * BScratchpadEntryByteSize  //BCSP的总带宽
    val CScratchpad_Total_Bandwidth = CScratchpadNBanks * CScratchpadEntryByteSize  //CCSP的总带宽

    val AScratchpad_Total_Bandwidth_Bit = AScratchpadNBanks * AScratchpadEntryByteSize * 8  //ACSP的总带宽
    val BScratchpad_Total_Bandwidth_Bit = BScratchpadNBanks * BScratchpadEntryByteSize * 8  //BCSP的总带宽
    val CScratchpad_Total_Bandwidth_Bit = CScratchpadNBanks * CScratchpadEntryByteSize * 8  //CCSP的总带宽


    val AScratchpadBankSize = AScratchpadSize / AScratchpadNBanks
    val BScratchpadBankSize = BScratchpadSize / BScratchpadNBanks
    val CScratchpadBankSize = CScratchpadSize / CScratchpadNBanks
    val AScratchpadBankNEntrys = AScratchpadBankSize / AScratchpadEntryByteSize
    val BScratchpadBankNEntrys = BScratchpadBankSize / BScratchpadEntryByteSize
    val CScratchpadBankNEntrys = CScratchpadBankSize / CScratchpadEntryByteSize


//MACLatency 用于ReducePE内的乘累加树的延迟描述
    val MAC32TreeLevel = log2Ceil(ReduceWidthByte * 8 / 32)
    val MAC32Latency = 3 //这是一个经验值，依据时序结果，填写的需要切分的流水段数量
    val MAC16TreeLevel = log2Ceil(ReduceWidthByte * 8 / 16)
    val MAC16Latency = 4
    val MAC8TreeLevel = log2Ceil(ReduceWidthByte * 8 / 8)
    val MAC8Latency = 5
//乘累加FIFO的深度
    val ResultFIFODepth = 8
    val InputFIFODepth = 8

    val AMemoryLoaderReadFromMemoryFIFODepth = 4 //用于暂存AML的数据到CCSP的FIFO

    val BMemoryLoaderReadFromMemoryFIFODepth = 4 //用于暂存BML的数据到CCSP的FIFO
    
    val CMemoryLoaderReadFromScratchpadFIFODepth = 4 //用于暂存CCSP的数据到CML的FIFO
    val CMemoryLoaderReadFromMemoryFIFODepth = 4 //用于暂存CML的数据到CSCP的FIFO

    val VecTaskInstBufferDepth = 32 //VecTask的指令缓冲深度
    val VecTaskInstBufferSize = 8 //VecTask的指令缓冲的数量
    val VecTaskDataBufferDepth = 4 //VecTask的指令缓冲深度掩盖从VecInterface到VPU的数据传输延迟即可
}

//需要配置的信息：oc -- 控制器发来的oc编号, 
//                ic, oh, ow, kh, kw, ohb -- 外层循环次数,
//                icb -- 矩阵乘计算中的中间长度
//                paddingH, paddingW, strideH, strideW -- 卷积层属性

class TaskCtrlInfo() extends Bundle with HWParameters with YGJKParameters{
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
class MacroInst() extends Bundle with HWParameters with YGJKParameters{
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
    val conv_oh_index = UInt(log2Ceil(Convolution_Input_Height_Weight_Dim_Max).W)
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
class LoadMicroInst() extends Bundle with HWParameters with YGJKParameters{
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
class LoadMicroInst_Resource_Info() extends Bundle with HWParameters with YGJKParameters{
    // Application_M,Application_N,Application_K代表这条宏指令要执行的MNK的长度
    val A_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val B_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    
}

//CUTE能接受的，Compute模块能处理的微指令形式
class ComputeMicroInst() extends Bundle with HWParameters with YGJKParameters{
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
class ComputeMicroInst_Resource_Info() extends Bundle with HWParameters with YGJKParameters{
    val A_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val B_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val Load_Micro_Inst_FIFO_Index = UInt(4.W)//代表Load的指令在队列中的位置
}
//CUTE能接受的，Store模块能处理的微指令形式
class StoreMicroInst() extends Bundle with HWParameters with YGJKParameters{
    val ApplicationTensor_D = new ApplicationTensor_D_Info
    val Conherent                           = (Bool())      //是否需要coherent
    val Is_Transpose                        = (Bool())      //是否需要转置
    val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
    val Is_Last_Store                       = (Bool())      //是否是最后一次store
}

//用于描述微指令间依赖关系和资源依赖关系的信息，用于下一阶段的微指令(Vec或者唤醒CPU)能否发射的信息
class StoreMicroInst_Resource_Info() extends Bundle with HWParameters with YGJKParameters{
    val C_SCPID = UInt(4.W)//代表Load的结果存在哪个SCP上
    val Compute_Micro_Inst_FIFO_Index = UInt(4.W)//代表Compute的指令在队列中的位置
    val Marco_Inst_FIFO_Index = UInt(4.W)//代表Marco的指令在队列中的位置
}

class AfterOpsInterface() extends Bundle with HWParameters with YGJKParameters{

    //每拍可接受一个来自CDC的与SCP和TE等宽的数据，并在自己模块内完成数据的拆分、重排、缩放、转置以及其他复杂向量任务
    val CDCDataToInterface     = DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W))
    val InterfaceToCDCData     = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W)))
    // val CDCStoreAddr                        = Input(UInt(log2Ceil(CScratchpadBankNEntrys).W))

    val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input() extends Bundle with HWParameters with YGJKParameters{
    val inst_uop = Output(UInt(32.W))
    val inst_src0 = Output(UInt(VectorWidth.W))
    val inst_src1 = Output(UInt(VectorWidth.W))
    val inst_src0_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_src1_type = Output(UInt(2.W))//从寄存器还是来自输入
    val inst_dest_type = Output(UInt(2.W))//写回寄存器还是写回输出
    val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
}

class VPUInterface_Output() extends Bundle with HWParameters with YGJKParameters{
    val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
    val stream_data = Output(UInt(VectorWidth.W))//stream data还能存一些额外的信息，这些信息也会返回，后续可以用于配置VPU的部分隐式寄存器，或者留存在VPU的的隐式寄存器中，这些寄存器是uop可见的，如下一次的scale，下一次的bias等。
}

class VPUInterfaceIO() extends Bundle with HWParameters with YGJKParameters{
    val VPU_Input = (DecoupledIO(new VPUInterface_Input))
    val VPU_Output = Flipped(DecoupledIO(new VPUInterface_Output))
}


class VectorInterfaceIO() extends Bundle with HWParameters with YGJKParameters{

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

class CUTE_uop() extends Bundle with HWParameters with YGJKParameters{
    val Stream_state = UInt((StreamStateType.StreamStateTypeBitWidth).W)
    val Stream_uop = UInt(32.W)
    val Element_uop = UInt(32.W)
    val Vector_uop = UInt(32.W)


}

class AfterOpsMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
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

class ADCMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
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

class BDCMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
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

class CDCMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
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

class ApplicationTensor_A_Info() extends Bundle with HWParameters with YGJKParameters{
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

class AMLMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{

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

class ApplicationTensor_B_Info() extends Bundle with HWParameters with YGJKParameters{
        val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
        val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
        val ApplicationTensor_B_Stride_N    = (UInt(MMUAddrWidth.W))//下一个N需要增加多少的地址偏移量
        // val Convolution_OC_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val Convolution_KH_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val Convolution_KW_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
        val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class BMLMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{

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

class ApplicationTensor_C_Info() extends Bundle with HWParameters with YGJKParameters{
    val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_C_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class ApplicationTensor_D_Info() extends Bundle with HWParameters with YGJKParameters{
    val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_D_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
}

class LoadTask_Info() extends Bundle with HWParameters with YGJKParameters{
    val Is_ZeroLoad = (Bool())
    val Is_RepeatRowLoad = (Bool())
    val Is_FullLoad = (Bool())
}
class CMLMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
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

class MTEMicroTaskConfigIO() extends Bundle with HWParameters with YGJKParameters{
    val dataType                            = Output(UInt(ElementDataType.DataTypeBitWidth.W))
    val valid = Output(Bool())
    val ready = Input(Bool())
}

class SCPControlInfo() extends Bundle with HWParameters with YGJKParameters{
    val ADC_SCP_ID = UInt(1.W)
    val BDC_SCP_ID = UInt(1.W)
    val CDC_SCP_ID = UInt(1.W)
    val AML_SCP_ID = UInt(1.W)
    val BML_SCP_ID = UInt(1.W)
    val CML_SCP_ID = UInt(1.W)
}




class ConfigInfoIO() extends Bundle with HWParameters with YGJKParameters{

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
class ADataControlScaratchpadIO extends Bundle with HWParameters{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(DecoupledIO(Vec(AScratchpadNBanks, (UInt(log2Ceil(AScratchpadBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Valid(Vec(AScratchpadNBanks, UInt(AScratchpadEntryBitSize.W)))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class AMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
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

class BDataControlScaratchpadIO extends Bundle with HWParameters{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankAddr = Flipped(DecoupledIO(Vec(BScratchpadNBanks, (UInt(log2Ceil(BScratchpadBankNEntrys).W)))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Valid(Vec(BScratchpadNBanks, UInt(BScratchpadEntryBitSize.W)))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}

class BMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
    //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
    val BankId = Flipped(Valid(UInt(log2Ceil(BScratchpadNBanks).W)))
    val BankAddr = Flipped(Vec(BScratchpadNBanks, Valid(UInt(log2Ceil(BScratchpadBankNEntrys).W))))
    //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
    val Data = Flipped(Vec(BScratchpadNBanks, Valid(UInt(BScratchpadEntryBitSize.W))))
    //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
    // val Chosen = Input(Bool())
}


class CDataControlScaratchpadIO extends Bundle with HWParameters{
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

class CMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
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
class LocalMMUIO extends Bundle with HWParameters{

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

class MMU2TLIO extends Bundle with HWParameters{

    //发出的访存请求
    val Request = Flipped(DecoupledIO(new Bundle{
        val RequestPhysicalAddr = UInt(MMUAddrWidth.W)
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


//数据类型的样板类
case object  ElementDataType extends Field[UInt]{
    val DataTypeBitWidth = 3
    val DataTypeUndef   = 0.U(DataTypeBitWidth.W)
    val DataTypeWidth32 = 4.U(DataTypeBitWidth.W)
    val DataTypeWidth16 = 2.U(DataTypeBitWidth.W)
    val DataTypeWidth8  = 1.U(DataTypeBitWidth.W)

    val DataTypeUInt8   = 0.U(DataTypeBitWidth.W)
    val DataTypeSInt8   = 1.U(DataTypeBitWidth.W)
    val DataTypeUInt16  = 2.U(DataTypeBitWidth.W)
    val DataTypeSInt16  = 3.U(DataTypeBitWidth.W)
    val DataTypeUInt32  = 4.U(DataTypeBitWidth.W)
    val DataTypeSInt32  = 5.U(DataTypeBitWidth.W)

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

class ScaratchpadTask extends Bundle with HWParameters{
    // * Elements defined earlier in the Bundle are higher order upon
    // * serialization. For example:
    // *   val bundle = Wire(new MyBundle)
    // *   bundle.foo := 0x1234.U
    // *   bundle.bar := 0x5678.U
    // *   val uint = bundle.asUInt
    // *   assert(uint === "h12345678".U) // This will pass
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