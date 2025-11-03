
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._
import freechips.rocketchip.rocket.Instructions.MAX
import freechips.rocketchip.util.SeqToAugmentedSeq
//AMemoryLoader，用于加载A矩阵的数据，供给Scratchpad使用
//从不同的存储介质中加载数据，供给Scratchpad使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据Scarchpad的设计，完成Tensor的切分以及将数据的填入Scaratchpad

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

class ASourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val ScratchpadBankId = UInt(log2Ceil(AScratchpadNBanks).W)
    val ScratchpadAddr = UInt(log2Ceil(AScratchpadBankNEntrys).W)
}

//本模块的核心任务是从外部接口加载数据到Scarchpad中
//所有的访存任务会通过TL-Link接口发出，每次的读请求的宽度是ReduceWidthByte

//总写回SCP的次数 = Tensor_M * ReduceGroupSize
//需要解决的核心问题为，计算出每次访存的地址，然后发出访存请求，然后接受访存的返回值，然后将返回值写入到Scarchpad中
//1.计算访存地址，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.每次Load的微任务，KH_Index、KW_Index不变，根据OH、OW、stride_H、stride_W即可得到当前Load任务的起始地址。
////b.我们的卷积数据的Input是[NHW][C]排布的,所以ApplicationTensor_A_Stride_M就是下一个[NHW]的地址偏移量
////c.每次迭代下一个Load请求，IH_Index、IW_Index会变，根据IH_Index、IW_Index，即可得到当前的Load任务是否需要发生真实的Load请求，还是直接发生0填充
//2.发出访存请求，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要记录mmu发送的source_id，用于后续的数据写回，这里的source_id是唯一的，且会记录该source_id对应的Scarchpad的地址和bank号
////b.需要0填充的情况，需要单独处理，找机会和某一次写回合并，根据此刻某个bank是否被占用来偷时点(由于SCP的总读写带宽肯定比LLC的总带宽大，所以这里的偷时点是可行的)
//3.接受访存返回值，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要根据source_id找到对应的Scarchpad的地址和bank号
////b.同时找机会和某一次写回合并！根据此刻某个bank是否被占用来偷时点写回SCP

//重要的几个逻辑部分
//1.计算IH、IW是否超界，如果超界，需要进行0填充，而不是发出访存请求(这里可能是时序不满足的点，如果不满足，可以提前就算好)
//2.0填充的逻辑，需要单独处理，需要找机会和某一次写回合并。故需要一个NACK寄存器来记录哪个bank此刻有ZeroFill任务，如果无法记录NACK任务，则停止发出访存请求，直到有空闲的bank

class AMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个ScarchPad的接口的总体设计
        val ToScarchPadIO = Flipped(new AMemoryLoaderScaratchpadIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
    })
    //TODO:init
    io.ToScarchPadIO.BankAddr := 0.U.asTypeOf(io.ToScarchPadIO.BankAddr)
    io.ToScarchPadIO.BankId.valid := false.B
    io.ToScarchPadIO.BankId.bits := 0.U
    io.ToScarchPadIO.Data := 0.U.asTypeOf(io.ToScarchPadIO.Data)
    io.ToScarchPadIO.ZeroFill := 0.U.asTypeOf(io.ToScarchPadIO.ZeroFill)
    io.LocalMMUIO.Request.valid := false.B
    io.LocalMMUIO.Request.bits := 0.U.asTypeOf(io.LocalMMUIO.Request.bits)
    io.LocalMMUIO.Response.ready := false.B
    
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B


    val ScaratchpadBankAddr = io.ToScarchPadIO.BankAddr
    val ScaratchpadData = io.ToScarchPadIO.Data

    val ConfigInfo = io.ConfigInfo

    val Tensor_A_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

    val HasScarhpadWrite = WireInit(false.B)

    val ApplicationTensor_A_Stride_M    = RegInit(0.U(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val Convolution_OH_DIM_Length       = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W)) //对于卷积来说，OH的值是多少，用于我们进行地址的计算
    val Convolution_OW_DIM_Length       = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W)) //对于卷积来说，OW的值是多少，用于我们进行地址的计算
    val Convolution_Stride_H            = RegInit(0.U(log2Ceil(StrideSizeMax).W)) //对于卷积来说，stride是多少，用于我们进行地址的计算
    val Convolution_Stride_W            = RegInit(0.U(log2Ceil(StrideSizeMax).W)) //对于卷积来说，stride是多少，用于我们进行地址的计算
    val Convolution_KH_DIM_Length       = RegInit(0.U(log2Ceil(KernelSizeMax).W)) //对于卷积来说，KH的值是多少，用于我们进行地址的计算
    val Convolution_KW_DIM_Length       = RegInit(0.U(log2Ceil(KernelSizeMax).W)) //对于卷积来说，KW的值是多少，用于我们进行地址的计算
    val dataType                        = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))  //数据类型

    val ScaratchpadTensor_M                 = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))//需要加载到Scaratchpad的Tensor的M
    val ScaratchpadTensor_K                 = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))//需要加载到Scaratchpad的Tensor的K

    //知道卷积核的位置和当前的OHOW，确认是否需要padding进行0填充
    val Convolution_Current_OH_Index        = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_OW_Index        = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
    val Init_Convolution_Current_OH_Index   = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
    val Init_Convolution_Current_OW_Index   = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Current_KH_Index        = RegInit(0.U(log2Ceil(KernelSizeMax).W))
    val Convolution_Current_KW_Index        = RegInit(0.U(log2Ceil(KernelSizeMax).W))

    //任务状态机,顺序读取所有分块矩阵
    val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
    val state = RegInit(s_idle)

    //访存状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val MemoryOrder_LoadConfig = RegInit(MemoryOrderType.OrderType_Mb_Kb)

    val IH_Stride = RegInit(0.U((MMUAddrWidth).W))
    val IW_Stride = RegInit(0.U((MMUAddrWidth).W))

    val Conherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供

    val Convolution_IW_DIM_Length = RegInit(0.U((log2Ceil(ConvolutionDIM_Max)).W))
    val Convolution_IH_DIM_Length = RegInit(0.U((log2Ceil(ConvolutionDIM_Max)).W))

    //允许每个bank最多1个nack，这样保证只要有bank空闲我们都能写入数据，同时保证了Load请求的译码不停顿。
    val NACK_ZeroFill_Hloding_Reg = RegInit((VecInit(Seq.fill(AScratchpadNBanks)(0.U((new ASourceIdSearch).getWidth.W)))))//每个bank的NACK值
    val NACK_ZeroFill_Hloding_Valid = RegInit(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))//每个bank的NACK计数器是否有效
    val Zero_Fill_TableItem = WireInit((VecInit(Seq.fill(AScratchpadNBanks)(0.U((new ASourceIdSearch).getWidth.W)))))
    val Zero_Fill_TableItem_Valid = WireInit(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))
    
    val Current_IH_Index = RegInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))
    val Current_IW_Index = RegInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))
    val Init_IH_DIM_Length = RegInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))
    val Init_IW_DIM_Length = RegInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))


    //如果configinfo有效
    //状态机
    when(state === s_idle){
        //idel状态才可以接受新的配置信息
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            //当前配置的指令有效
            state := s_mm_task
            memoryload_state := s_load_init
            ScaratchpadTensor_M := ConfigInfo.ScaratchpadTensor_M
            ScaratchpadTensor_K := ConfigInfo.ScaratchpadTensor_K

            Tensor_A_BaseVaddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
            // Tensor_Block_BaseAddr := ConfigInfo.ApplicationTensor_A.BlockTensor_A_BaseVaddr
            // Conherent := io.ConfigInfo.bits.ApplicationTensor_A.Conherent

            ApplicationTensor_A_Stride_M := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M
            Convolution_OH_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OH_DIM_Length
            Convolution_OW_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length
            Convolution_Stride_H := ConfigInfo.ApplicationTensor_A.Convolution_Stride_H
            Convolution_Stride_W := ConfigInfo.ApplicationTensor_A.Convolution_Stride_W
            Convolution_KH_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length
            Convolution_KW_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length
            dataType := ConfigInfo.ApplicationTensor_A.dataType

            Init_Convolution_Current_OH_Index := ConfigInfo.Convolution_Current_OH_Index
            Init_Convolution_Current_OW_Index := ConfigInfo.Convolution_Current_OW_Index
            Convolution_Current_OH_Index := ConfigInfo.Convolution_Current_OH_Index
            Convolution_Current_OW_Index := ConfigInfo.Convolution_Current_OW_Index
            Convolution_Current_KH_Index := ConfigInfo.Convolution_Current_KH_Index
            Convolution_Current_KW_Index := ConfigInfo.Convolution_Current_KW_Index

            IH_Stride :=  ConfigInfo.ApplicationTensor_A.Convolution_Stride_W * ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length * ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M //每移动一次IH，需要增加的地址偏移量
            IW_Stride :=  ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M //每移动一次IW，需要增加的地址偏移量

            Convolution_IW_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length * ConfigInfo.ApplicationTensor_A.Convolution_Stride_W//可以提前算，存成Reg
            Convolution_IH_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OH_DIM_Length * ConfigInfo.ApplicationTensor_A.Convolution_Stride_H//可以提前算，存成Reg

            //TODO:这里可能是性能瓶颈，如果这里不满足时序要求，我们可以在这里切流水，提前算好然后喂进AML,我们有一百种方法在这里优化时序:p
            Current_IH_Index := Cat(0.U(1.W),ConfigInfo.Convolution_Current_OH_Index).asSInt * Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_Stride_H).asSInt + Cat(0.U(1.W),ConfigInfo.Convolution_Current_KH_Index).asSInt - Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length/2.U).asSInt//这里是计算当前的IH的index的初始值,如果变瓶颈了再改
            Current_IW_Index := Cat(0.U(1.W),ConfigInfo.Convolution_Current_OW_Index).asSInt * Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_Stride_W).asSInt + Cat(0.U(1.W),ConfigInfo.Convolution_Current_KW_Index).asSInt - Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length/2.U).asSInt//这里是计算当前的IW的index的初始值,如果变瓶颈了再改

            Init_IH_DIM_Length := Cat(0.U(1.W),ConfigInfo.Convolution_Current_OH_Index).asSInt * Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_Stride_H).asSInt + Cat(0.U(1.W),ConfigInfo.Convolution_Current_KH_Index).asSInt - Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length/2.U).asSInt//这里是计算当前的IH的index的初始值,如果变瓶颈了再改
            Init_IW_DIM_Length := Cat(0.U(1.W),ConfigInfo.Convolution_Current_OW_Index).asSInt * Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_Stride_W).asSInt + Cat(0.U(1.W),ConfigInfo.Convolution_Current_KW_Index).asSInt - Cat(0.U(1.W),ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length/2.U).asSInt//这里是计算当前的IW的index的初始值,如果变瓶颈了再改
            assert(ConfigInfo.ScaratchpadTensor_K === ReduceGroupSize.U)
            //
            if(YJPAMLDebugEnable)
            {
                printf("[AML<%d>]AMemoryLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                //输出所有配置项
                printf("[AML<%d>]ScaratchpadTensor_M:%d, ScaratchpadTensor_K:%d, Tensor_A_BaseVaddr:%x, ApplicationTensor_A_Stride_M:%x, Convolution_OH_DIM_Length:%d, Convolution_OW_DIM_Length:%d, Convolution_Stride_H:%d, Convolution_Stride_W:%d, Convolution_KH_DIM_Length:%d, Convolution_KW_DIM_Length:%d, dataType:%d, Convolution_Current_OH_Index:%d, Convolution_Current_OW_Index:%d, Convolution_Current_KH_Index:%d, Convolution_Current_KW_Index:%d\n",io.DebugInfo.DebugTimeStampe,ConfigInfo.ScaratchpadTensor_M,ConfigInfo.ScaratchpadTensor_K,ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr,ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M,ConfigInfo.ApplicationTensor_A.Convolution_OH_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_Stride_H,ConfigInfo.ApplicationTensor_A.Convolution_Stride_W,ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length,ConfigInfo.ApplicationTensor_A.dataType,ConfigInfo.Convolution_Current_OH_Index,ConfigInfo.Convolution_Current_OW_Index,ConfigInfo.Convolution_Current_KH_Index,ConfigInfo.Convolution_Current_KW_Index)
            }
        }
    }

    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_M*ReduceGroupSize)+1).W)) //总共要加载的张量大小，总加载的数据量不会超过Tensor_M*ReduceGroupSize*ruduceWidthByte，这个是不会变的
    val CurrentLoaded_BlockTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    
    //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
    //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器
    
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new ASourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_M*ReduceGroupSize*ReduceWidthByte)).W))

    val SCP_Fill_Table = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val SCP_Fill_Table_SCP_Addr = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(AScratchpadBankNEntrys).W)))))//记录这个LLC回的数是在scp的哪个地址
    val SCP_Fill_Table_Time = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/AScratchpadEntryByteSize)+1).W)))))//记录这个LLC回的数需要回填的次数，完成就可以将数据释放了
    val SCP_Fill_Table_Free = SCP_Fill_Table_Time.map(_ === 0.U)//记录这个FIFO能否能填数据
    val SCP_Fill_Table_Insert_Index = PriorityEncoder(SCP_Fill_Table_Free)//返回第一个空位的index
    val SCP_Fill_Table_Not_Full = SCP_Fill_Table_Free.reduce(_ || _)//这个FIFO是否还有空位
    val MAX_Fill_Times = outsideDataWidthByte/AScratchpadEntryByteSize
    val zero_fill_k = RegInit(0.U((log2Ceil(MAX_Fill_Times)+1).W))

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(AScratchpadNBanks)(VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))))//记录fifo里的数据是哪个bank的
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(AScratchpadNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))//想要往scp里bank(x)写的最后一个scp_fill_fifo的index
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(AScratchpadNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(AScratchpadNBanks)(true.B)))
    val Bank_Fill_Valid = WireInit(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))//每个bank是否有数据需要写入scp
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//是否有数据需要写scp

    for(i <- 0 until AScratchpadNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), AMemoryLoaderReadFromMemoryFIFODepth)//fifo满了
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//这个bank不需要写scp
        Bank_Fill_Valid(i) := Bank_Fill_Search_FIFO_Head(i) =/= Bank_Fill_Search_FIFO_Tail(i)//这个bank有数据需要写scp
    }

    // 向上取到4的倍数
    val MaxBlockTensor_M_Index = ScaratchpadTensor_M / Matrix_M.U * Matrix_M.U + ((ScaratchpadTensor_M % Matrix_M.U) =/= 0.U) * 4.U
    val MaxBlockTensor_K_Index = ScaratchpadTensor_K

    val Init_Current_M_BaseAddr = RegInit(0.U((MMUAddrWidth).W))//当前M不动的情况下的基地指
    

    val Request = io.LocalMMUIO.Request

    val Current_IH_Index_U = Current_IH_Index(log2Ceil(ConvolutionDIM_Max)-1,0)
    val Current_IW_Index_U = Current_IW_Index(log2Ceil(ConvolutionDIM_Max)-1,0)
    val Next_IW = Current_IW_Index + Cat(0.U,Convolution_Stride_W).asSInt
    val Next_IH = Current_IH_Index + Cat(0.U,Convolution_Stride_H).asSInt
    val Next_IW_U = Next_IW(log2Ceil(ConvolutionDIM_Max)-1,0)
    val Next_IH_U = Next_IH(log2Ceil(ConvolutionDIM_Max)-1,0)
    val Init_IW = (Cat(0.U,Convolution_Current_KW_Index).asSInt - Cat(0.U,Convolution_KW_DIM_Length/2.U).asSInt)
    val Init_IW_U = Init_IW(log2Ceil(KernelSizeMax)-1,0)
    // 向上补充的M也算作非法来填零
    val Is_invalid_IH_IW = Current_IH_Index < 0.S || Current_IW_Index < 0.S || Current_IH_Index >= Cat(0.U(1.W),Convolution_IH_DIM_Length).asSInt || Current_IW_Index >= Cat(0.U(1.W),Convolution_IW_DIM_Length).asSInt || (CurrentLoaded_BlockTensor_M >= ScaratchpadTensor_M)
    val Have_NACK_Need_ZeroFill = NACK_ZeroFill_Hloding_Valid.asUInt.orR
    val Finish_Decode_Load_Request = !(CurrentLoaded_BlockTensor_M < MaxBlockTensor_M_Index && CurrentLoaded_BlockTensor_K < MaxBlockTensor_K_Index)
    val Infight_Load_Request_Num = RegInit(0.U((log2Ceil(SoureceMaxNum)+1).W))//当前正在进行的访存请求的数量

    val Current_M_BaseAddr = RegInit(0.U((MMUAddrWidth).W))//当前M不动的情况下的基地指,为了保证时序，我们需要提前算好
    //Current_M_BaseAddr这个值，需要根据下一次的IH和IW确定
    //IH和IW是否超界，如果超界，需要进行0填充，由Is_invalid_IH_IW根据OW、OH、stride_H、stride_W、KH、KW来判断
    //Current_M_BaseAddr只要在IH和IW不越界的情况下，值是对的即可。
    //OW达到上限变为0时，OH+1，OW=0，此时IW = kh - kh_dim/2，这个值不能写错！此时IW对application_A的地址偏移贡献为(kh - kh_dim/2)*iw_stride,此时IH对application_A的地址偏移贡献为(oh+1)*Convolution_Stride_H*ih_stride


    Request.valid := false.B
    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        CurrentLoaded_BlockTensor_M := 0.U
        CurrentLoaded_BlockTensor_K := 0.U
        MaxRequestIter := MaxBlockTensor_M_Index * MaxBlockTensor_K_Index * ReduceWidthByte.U / (outsideDataWidthByte.U) //总共要发出的访存请求的次数
        Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Current_IW_Index_U + Tensor_A_BaseVaddr
        Init_Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Current_IW_Index_U + Tensor_A_BaseVaddr
        Infight_Load_Request_Num := 0.U
        
    }.elsewhen(memoryload_state === s_load_working){
        //根据不同的MemoryOrder，执行不同的访存模式

        //Is_invalid_IH_IW 需要单独处理，找机会和某一次写回合并！根据此刻某个bank是否被占用来偷时点

        //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
        //不用担心乘法电路延迟，EDA工具会把他优化好，再不济也能提前算好，我们有一万种优化时序的方法:)
        Request.bits.RequestVirtualAddr := Current_M_BaseAddr + CurrentLoaded_BlockTensor_K * ReduceWidthByte.U

        val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
        Request.bits.RequestConherent := Conherent
        Request.bits.RequestSourceID := sourceId.bits
        Request.bits.RequestType_isWrite := false.B
        Request.valid := true.B
        when(Finish_Decode_Load_Request || Is_invalid_IH_IW)//Is_invalid_IH_IW时，不发出访存请求，尝试直接0填充
        {
            Request.valid := false.B
        }

        //输出oh,ow,ih,iw,is_invalid
        if (YJPAMLDebugEnable)
        {
            printf("[AML<%d>]AML WORKING STATE!Current_OH_Index:%d,Current_OW_Index:%d,Current_IH_Index:%d,Current_IW_Index:%d,Is_invalid_IH_IW:%d\n",io.DebugInfo.DebugTimeStampe,Convolution_Current_OH_Index,Convolution_Current_OW_Index,Current_IH_Index_U,Current_IW_Index_U,Is_invalid_IH_IW)
        }
        //数据在Scarachpad中的编排
        //数据会先排M，再排K，
        //AVector一定是不同M的数据，K不断送入，直到K迭代完成，再换新的M，
        //这里的0 1 2 3是一个K连续的ReduceWidth宽的数据
        //   K 0 1 2 3 4 5 6 7     time     AVector     ScaratchpadData也这么排布
        // M                        0       0 8 g o             {bank[0] [1] [2] [3]}
        // 0   0 1 2 3 4 5 6 7      1       1 9 h p   |addr    0 |    0   8   g   o
        // 1   8 9 a b c d e f      2       2 a i q   |        1 |    1   9   h   p
        // 2   g h i j k l m n      3       3 b j r   |        2 |    2   a   i   q
        // 3   o p g r s t u v      4       4 c k s   |        3 |    3   b   j   r
        // 4   w x y z .......      5       5 d l t   |        4 |    4   c   k   s
        // 5   !..............      6       6 e m u   |        5 |    5   d   l   t
        // 6   @..............      7       7 f n v   |        6 |    6   e   m   u
        // 7   #..............      8       w ! @ #   |        7 |    7   f   n   v
        // 8   $..............      9       .......   | ...........................
        //
        //
        // 在内存中的排布则是 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z .......

        when(Request.fire && sourceId.valid && !Is_invalid_IH_IW && !Finish_Decode_Load_Request){
            val TableItem = Wire(new ASourceIdSearch)
            TableItem.ScratchpadBankId := CurrentLoaded_BlockTensor_M % AScratchpadNBanks.U
            TableItem.ScratchpadAddr := ((CurrentLoaded_BlockTensor_M / AScratchpadNBanks.U) * ReduceGroupSize.U) + CurrentLoaded_BlockTensor_K
            SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

            if(YJPAMLDebugEnable)
            {
                //输出当前的IH和IW，M，K，当前访存任务的虚地址，五个一起输出
                printf("[AML<%d>{AML Load Trace}]Load MMU Request! Current_IH_Index:%d,Current_IW_Index:%d,CurrentLoaded_BlockTensor_M:%d,CurrentLoaded_BlockTensor_K:%d,RequestVirtualAddr:%x\n",io.DebugInfo.DebugTimeStampe,Current_IH_Index,Current_IW_Index,CurrentLoaded_BlockTensor_M,CurrentLoaded_BlockTensor_K,Request.bits.RequestVirtualAddr)
            }
            when(CurrentLoaded_BlockTensor_M < MaxBlockTensor_M_Index && CurrentLoaded_BlockTensor_K < MaxBlockTensor_K_Index){
                CurrentLoaded_BlockTensor_M := CurrentLoaded_BlockTensor_M + 1.U
                Convolution_Current_OW_Index := Convolution_Current_OW_Index + 1.U
                Current_IW_Index := Next_IW
                Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Next_IW_U + Tensor_A_BaseVaddr //下一个M的地址,IW正常增加
                when(Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U)
                {
                    Convolution_Current_OW_Index := 0.U //OW变成0了
                    Convolution_Current_OH_Index := Convolution_Current_OH_Index + 1.U //OH+1
                    Current_IW_Index := Init_IW //IW回到初始位置
                    Current_IH_Index := Next_IH //IH正常增加
                    //计算这个地址可能是性能瓶颈，如果这里不满足时序要求，我们可以切流水算好地址，然后喂进来。做成一个算地址的FIFO即可，单单多几拍算地址的延迟而已，这里求地址的吞吐不变
                    Current_M_BaseAddr := IH_Stride * Next_IH_U + Init_IW_U * IW_Stride + Tensor_A_BaseVaddr //下一个M的地址，IH正常增加，这要看kernel的值
                }
                when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U)//next_K
                {
                    CurrentLoaded_BlockTensor_M := 0.U
                    CurrentLoaded_BlockTensor_K := CurrentLoaded_BlockTensor_K + MAX_Fill_Times.U
                    Convolution_Current_OH_Index := Init_Convolution_Current_OH_Index
                    Convolution_Current_OW_Index := Init_Convolution_Current_OW_Index
                    Current_IH_Index := Init_IH_DIM_Length
                    Current_IW_Index := Init_IW_DIM_Length
                    Current_M_BaseAddr := Init_Current_M_BaseAddr
                }

            }
        }.elsewhen(Is_invalid_IH_IW && !Finish_Decode_Load_Request)
        {
            val TableItem = Wire(new ASourceIdSearch)
            TableItem.ScratchpadBankId := CurrentLoaded_BlockTensor_M % AScratchpadNBanks.U
            TableItem.ScratchpadAddr := ((CurrentLoaded_BlockTensor_M / AScratchpadNBanks.U) * ReduceGroupSize.U) + CurrentLoaded_BlockTensor_K + zero_fill_k
            TableItem.asUInt

            val Response_sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            val Response_ScratchpadBankId = SoureceIdSearchTable(Response_sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
            //有两种情况会发生NACK
            //1.如果当前的写回任务(优先级更高)与当前的0填充任务，是同一个Bank的任务，则进行NACK处理
            //2.前面的0填充任务还没有被处理，那么这个任务就需要NACK，如果连续发生NACK，就需要Stall
            val IsNACK_From_Response = WireInit(false.B)
            if (ABMLNeedSCPFillTable)
            {
                IsNACK_From_Response := Bank_Fill_Valid(TableItem.ScratchpadBankId) 
            } else
            {
                IsNACK_From_Response := (io.LocalMMUIO.Response.valid && (Response_ScratchpadBankId === TableItem .ScratchpadBankId))
            }
            val IsNACK_From_NACKReg = NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId)
            val IsNACK = IsNACK_From_Response || IsNACK_From_NACKReg

            val NACK2Stall = WireInit(false.B)//是否需要stall,如果当前的bank有NACK，那么就需要stall，暂时停止译码(基本不可能发生)

            //有SCP要写回时，是最高优先级的任务
            //次高优先级的任务是以前的零填充的NACK的任务
            //预填充好Zero_Fill_TableItem和Zero_Fill_TableItem_Valid，表示这一拍会执行的0填充任务，这些任务是上一拍冲突的任务，在这一拍优先被处理
            for (i <- 0 until AScratchpadNBanks)
            {
                //当前bank的NACK是有效的，且当前的bank不是当前Response的bank
                if (ABMLNeedSCPFillTable)
                {
                    Zero_Fill_TableItem_Valid(i) := NACK_ZeroFill_Hloding_Valid(i) && Bank_Fill_Search_FIFO_Empty(i)
                } else
                {
                    Zero_Fill_TableItem_Valid(i) := NACK_ZeroFill_Hloding_Valid(i) && ((io.LocalMMUIO.Response.valid === false.B)||(Response_ScratchpadBankId =/= i.U))
                }
                //如果Zero_Fill_TableItem_Valid为真，则NACK_ZeroFill_Hloding一定会被处理，所以这里要清空
                NACK_ZeroFill_Hloding_Valid(i) := Mux(Zero_Fill_TableItem_Valid(i),false.B,NACK_ZeroFill_Hloding_Valid(i))
                Zero_Fill_TableItem(i) := Mux(Zero_Fill_TableItem_Valid(i),NACK_ZeroFill_Hloding_Reg(i),0.U)
            }

            //接下来看当前的任务是否需要NACK
            when(IsNACK_From_Response && !IsNACK_From_NACKReg){
                //由于与Response的bank冲突，那么这个任务此刻不能被写回，且Reg中没有NACK，那么就需要将这个任务放入NACK的Reg中
                NACK_ZeroFill_Hloding_Reg(TableItem.ScratchpadBankId) := TableItem.asUInt
                NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId) := true.B
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>]NACK_From_Response && not NACK From NACKReg! Response_bank_id = %d,decode_zerofill_bank_id = %d,NACK_Reg = %b\n",io.DebugInfo.DebugTimeStampe,Response_ScratchpadBankId,TableItem.ScratchpadBankId,NACK_ZeroFill_Hloding_Valid.asUInt)
                }
            }.elsewhen(IsNACK_From_Response && IsNACK_From_NACKReg){
                //如果当前任务的bank已经有NACK了，且NACK_Reg中有NACK冲突了，那么就需要stall,因为要优先处理response的任务
                NACK2Stall := true.B
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>]NACK_From_Response && NACK From NACKReg STALL!!! Response_bank_id = %d,decode_zerofill_bank_id = %d,NACK_Reg = %b\n",io.DebugInfo.DebugTimeStampe,Response_ScratchpadBankId,TableItem.ScratchpadBankId,NACK_ZeroFill_Hloding_Valid.asUInt)
                }
            }.elsewhen(!IsNACK_From_Response && IsNACK_From_NACKReg)
            {
                //如果当前任务的Response bank没有冲突，但是Reg中有NACK冲突了，那么这个NACK这一拍就要被处理
                NACK_ZeroFill_Hloding_Reg(TableItem.ScratchpadBankId) := TableItem.asUInt
                NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId) := true.B
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>]not NACK_From_Response && NACK From NACKReg! Response_bank_id = %d,decode_zerofill_bank_id = %d,NACK_Reg = %b\n",io.DebugInfo.DebugTimeStampe,Response_ScratchpadBankId,TableItem.ScratchpadBankId,NACK_ZeroFill_Hloding_Valid.asUInt)
                }
            }.otherwise
            {
                //恭喜我们没有发生NACK,这是常见情况
                Zero_Fill_TableItem_Valid(TableItem.ScratchpadBankId) := true.B
                Zero_Fill_TableItem(TableItem.ScratchpadBankId) := TableItem.asUInt
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>]No NACK HAPPY!! Response_bank_id = %d,decode_zerofill_bank_id = %d,NACK_Reg = %b\n",io.DebugInfo.DebugTimeStampe,Response_ScratchpadBankId,TableItem.ScratchpadBankId,NACK_ZeroFill_Hloding_Valid.asUInt)
                }
            }

            
            when(!NACK2Stall)//可以继续译码
            {
                when(CurrentLoaded_BlockTensor_M < MaxBlockTensor_M_Index && CurrentLoaded_BlockTensor_K < MaxBlockTensor_K_Index){
                    zero_fill_k := zero_fill_k + 1.U
                    when(zero_fill_k === MAX_Fill_Times.U - 1.U)
                    {
                        zero_fill_k := 0.U
                        CurrentLoaded_BlockTensor_M := CurrentLoaded_BlockTensor_M + 1.U
                        Convolution_Current_OW_Index := Convolution_Current_OW_Index + 1.U
                        Current_IW_Index := Next_IW
                        Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Next_IW_U + Tensor_A_BaseVaddr //下一个M的地址,IW正常增加
                        when(Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U)
                        {
                            Convolution_Current_OW_Index := 0.U //OW变成0了
                            Convolution_Current_OH_Index := Convolution_Current_OH_Index + 1.U //OH+1
                            Current_IW_Index := Init_IW //IW回到初始位置
                            Current_IH_Index := Next_IH //IH正常增加
                            Current_M_BaseAddr := IH_Stride * Next_IH_U + Init_IW_U * IW_Stride + Tensor_A_BaseVaddr //下一个M的地址，IH正常增加，这要看kernel的值
                        }
                        when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U)
                        {
                            CurrentLoaded_BlockTensor_M := 0.U
                            CurrentLoaded_BlockTensor_K := CurrentLoaded_BlockTensor_K + MAX_Fill_Times.U
                            Convolution_Current_OH_Index := Init_Convolution_Current_OH_Index
                            Convolution_Current_OW_Index := Init_Convolution_Current_OW_Index
                            Current_IH_Index := Init_IH_DIM_Length
                            Current_IW_Index := Init_IW_DIM_Length
                            Current_M_BaseAddr := Init_Current_M_BaseAddr
                        }
                    }

                    if (YJPAMLDebugEnable)
                    {
                        //输出下一个Current_M_BaseAddr的地址，同时输出计算这个值需要的其他值的数据
                        val debug_Current_M_BaseAddr = WireInit(0.U((MMUAddrWidth).W))
                        debug_Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Next_IW_U + Tensor_A_BaseVaddr
                        when(Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U)
                        {
                            debug_Current_M_BaseAddr := IH_Stride * Next_IH_U + Init_IW_U * IW_Stride + Tensor_A_BaseVaddr
                            printf("[AML<%d>{Current_M_BaseAddr}]No Need Read Request Load! Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U\n",io.DebugInfo.DebugTimeStampe)
                        }
                        when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U)
                        {
                            debug_Current_M_BaseAddr := Init_Current_M_BaseAddr
                            printf("[AML<%d>{Current_M_BaseAddr}]No Need Read Request Load! CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U\n",io.DebugInfo.DebugTimeStampe)
                        }
                        printf("[AML<%d>{Current_M_BaseAddr}]No Need Read Request Load! Next M BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,debug_Current_M_BaseAddr)
                    }
                }
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>{AML Load Trace}]No Need Read Request Load!Current_IH_Index:%d,Current_IW_Index:%d,CurrentLoaded_BlockTensor_M:%d,CurrentLoaded_BlockTensor_K:%d\n",io.DebugInfo.DebugTimeStampe,Current_IH_Index,Current_IW_Index,CurrentLoaded_BlockTensor_M,CurrentLoaded_BlockTensor_K)
                }
            }.otherwise
            {
                //stall
                if (YJPAMLDebugEnable)
                {
                    printf("[AML<%d>]NACK2Stall!\n",io.DebugInfo.DebugTimeStampe)
                }
            }

        }.elsewhen(Finish_Decode_Load_Request && Have_NACK_Need_ZeroFill)//已经结束编码了，但是还有NACK的回填任务
        {
            //如果当前的任务已经完成，但是还有NACK的任务，那么就要处理NACK的任务
            val Response_sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            val Response_ScratchpadBankId = SoureceIdSearchTable(Response_sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
            //有SCP要写回时，是最高优先级的任务
            //次高优先级的任务是以前的零填充的NACK的任务
            //预填充好Zero_Fill_TableItem和Zero_Fill_TableItem_Valid，表示这一拍会执行的0填充任务，这些任务是上一拍冲突的任务，在这一拍优先被处理
            for (i <- 0 until AScratchpadNBanks)
            {
                //当前bank的NACK是有效的，且当前的bank不是当前Response的bank
                if (ABMLNeedSCPFillTable)
                {
                    Zero_Fill_TableItem_Valid(i) := NACK_ZeroFill_Hloding_Valid(i) && Bank_Fill_Search_FIFO_Empty(i)
                } else
                {
                    Zero_Fill_TableItem_Valid(i) := NACK_ZeroFill_Hloding_Valid(i) && ((io.LocalMMUIO.Response.valid === false.B)||(Response_ScratchpadBankId =/= i.U))
                }
            //如果Zero_Fill_TableItem_Valid为真，则NACK_ZeroFill_Hloding一定会被处理，所以这里要清空
                NACK_ZeroFill_Hloding_Valid(i) := Mux(Zero_Fill_TableItem_Valid(i),false.B,NACK_ZeroFill_Hloding_Valid(i))
                Zero_Fill_TableItem(i) := Mux(Zero_Fill_TableItem_Valid(i),NACK_ZeroFill_Hloding_Reg(i),0.U)
            }
            if (YJPAMLDebugEnable)
            {
                printf("[AML<%d>]Finish_Decode_Load_Request && Have_NACK_Need_ZeroFill\n",io.DebugInfo.DebugTimeStampe)
            }
        }.elsewhen(Finish_Decode_Load_Request && !Have_NACK_Need_ZeroFill)
        {
            //如果当前的任务已经完成，且没有NACK的任务,说明在等待Load的Response
            if (YJPAMLDebugEnable)
            {
                printf("[AML<%d>]Waiting for Load Response (infight:%d).Finish_Decode_Load_Request && not Have_NACK_Need_ZeroFill!\n",io.DebugInfo.DebugTimeStampe,Infight_Load_Request_Num)
            }
        }

        val current_fill_fifo_full = WireInit(false.B)
        when(io.LocalMMUIO.Response.valid)
        {
            val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
            current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(ScratchpadBankId)
        }

        //接受访存的返回值
        //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
        //根据response的sourceid，找到对应的Scarchpad的Fill_Table的队伍头的索引，填充到Fill_Table中
        if (ABMLNeedSCPFillTable)
        {
            io.LocalMMUIO.Response.ready := SCP_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
        } else
        {
            // 访问内存回复一次能写回时直接写入SCP
            io.LocalMMUIO.Response.ready := true.B
        }
        when(io.LocalMMUIO.Response.fire){
            //Trick注意这个设计，是doublebuffer的，AB只能是doublebuffer，回数一定是不会堵的，而且我们有时间对数据进行压缩解压缩～
            //如果要做release设计，要么数据位宽翻倍，腾出周期来使得有空泡能给写任务进行，要么就是数据位宽不变，将读写端口变成独立的读和独立的写端口
            val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
            val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).ScratchpadAddr
            val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
            val FIFOIndex = Bank_Fill_Search_FIFO_Head(ScratchpadBankId)//该bank的fill_fifo_index，标注了它当前在fillfifo的哪个位置，我们一共有bank个fill_fifo

            SCP_Fill_Table(SCP_Fill_Table_Insert_Index) := ResponseData
            SCP_Fill_Table_SCP_Addr(SCP_Fill_Table_Insert_Index) := ScratchpadAddr
            SCP_Fill_Table_Time(SCP_Fill_Table_Insert_Index) := MAX_Fill_Times.U

            Bank_Fill_Search_FIFO(ScratchpadBankId)(FIFOIndex) := SCP_Fill_Table_Insert_Index
            Bank_Fill_Search_FIFO_Head(ScratchpadBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(ScratchpadBankId), AMemoryLoaderReadFromMemoryFIFODepth)
            //Scartchpad->MemoryLoader->MMU->Memory Bus->Memory上的长组合逻辑链，可以实现一下，为后续的开发做准备
            //否则就靠软件来保证数据流和访存流，保证访存流的稳定性，一定不会堵，就可以省下这个长组合逻辑的延迟？
            //还有一点，我们的ScartchPad是写优先的呀！！！所以只要写端口数唯一，就不会堵，不需要fifo～～～
            //Trick:写优先是真的很有说法，本来外部存储就是慢的，读快速存储器的任务等一等就好了，但是所有的ScartchPad都想要读数据的，不能等，所以写优先

            if (!ABMLNeedSCPFillTable)
            {
                for (i <- 0 until AScratchpadNBanks)
                {
                    when(ScratchpadBankId === i.U)
                    {
                        io.ToScarchPadIO.BankAddr(i).bits := ScratchpadAddr
                        io.ToScarchPadIO.Data(i).bits := ResponseData
                        io.ToScarchPadIO.BankAddr(i).valid := true.B
                        io.ToScarchPadIO.Data(i).valid := true.B
                    }
                }
            }
            
            //根据response的的id
            if (YJPAMLDebugEnable)
            {
                //输出这次response的信息
                printf("[AML<%d>]ResponseData:%x,ScratchpadBankId:%d,ScratchpadAddr:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,ScratchpadBankId,ScratchpadAddr)
                //response的sourceid
                printf("[AML<%d>]ResponseSourceID:%d\n",io.DebugInfo.DebugTimeStampe,sourceId)
            }
        }

        // Fill_Table的回填优先级最高，一旦有回填任务就立即执行
        val Current_Fill_SCP_Time = WireInit(VecInit(Seq.fill(AScratchpadNBanks)(0.U(1.W))))
        if (ABMLNeedSCPFillTable)
        {
            for (i <- 0 until AScratchpadNBanks){
                when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                    val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                    Current_Fill_SCP_Time(i) := 1.U
                    val ScarchPadWriteRequest = io.ToScarchPadIO
                    val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*AScratchpadEntryByteSize).W)))))
                    FIFOData := SCP_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                    ScarchPadWriteRequest.BankAddr(i).bits := SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex) + (MAX_Fill_Times.U - SCP_Fill_Table_Time(CurrentFIFOIndex))
                    ScarchPadWriteRequest.BankAddr(i).valid := true.B
                    ScarchPadWriteRequest.Data(i).bits := FIFOData(MAX_Fill_Times.U - SCP_Fill_Table_Time(CurrentFIFOIndex))
                    ScarchPadWriteRequest.Data(i).valid := true.B

                    SCP_Fill_Table_Time(CurrentFIFOIndex) := SCP_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                    when(SCP_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                        Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), AMemoryLoaderReadFromMemoryFIFODepth)
                    }

                    if (YJPCMLDebugEnable)
                    {
                        //输出fill_time 和 fifoindex
                        printf("[AML AMemoryLoader_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, SCP_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex), SCP_Fill_Table_Time(CurrentFIFOIndex))
                        printf("[AML AMemoryLoader_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex), ScarchPadWriteRequest.BankAddr(i).bits, ScarchPadWriteRequest.Data(i).bits)
                    }
                }
            }
        }

        //完成零填充写回
        for (i <- 0 until AScratchpadNBanks)
        {
            when(Zero_Fill_TableItem_Valid(i))
            {
                io.ToScarchPadIO.ZeroFill(i).bits := Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadAddr
                io.ToScarchPadIO.ZeroFill(i).valid := true.B
                if(YJPAMLDebugEnable)
                {
                    //输出这次的零填充信息
                    printf("[AML<%d>]ZeroFillData:%x,ScratchpadBankId:%d,ScratchpadAddr:%d\n",io.DebugInfo.DebugTimeStampe,0.U,Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadBankId,Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadAddr)
                }
            }
        }

        Infight_Load_Request_Num := Mux(Request.fire === false.B && io.LocalMMUIO.Response.fire === true.B,Infight_Load_Request_Num - 1.U,
                                    Mux(Request.fire === true.B && io.LocalMMUIO.Response.fire === false.B,Infight_Load_Request_Num + 1.U,Infight_Load_Request_Num))

        val Load_Size = WireInit(0.U((log2Ceil(AScratchpadNBanks)+1).W))
        if (ABMLNeedSCPFillTable)
        {
            Load_Size := PopCount(Current_Fill_SCP_Time.asUInt) + PopCount(Zero_Fill_TableItem_Valid)
        } else
        {
            Load_Size := io.LocalMMUIO.Response.valid.asUInt + PopCount(Zero_Fill_TableItem_Valid)
        }

        TotalLoadSize := TotalLoadSize + Load_Size
        if(YJPAMLDebugEnable)
        {
            //输出总共加载的数据量

            when(Load_Size =/= 0.U)
            {
                printf("[AML<%d>]TotalLoadSize:%d, Load_Size = %d, Zero_Fill_TableItem_Valid = %b,io.LocalMMUIO.Response.valid= %b\n",io.DebugInfo.DebugTimeStampe,TotalLoadSize + Load_Size,Load_Size,Zero_Fill_TableItem_Valid.asUInt,io.LocalMMUIO.Response.valid)
            }
        }
        when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
            memoryload_state := s_load_end
        }
    }.elsewhen(memoryload_state === s_load_end){
        ConfigInfo.MicroTaskEndValid := true.B
        when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
            memoryload_state := s_load_idle
            state := s_idle
            if(YJPAMLDebugEnable)
            {
                printf("[AML<%d>]AMemoryLoader Task End\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{

    }

}