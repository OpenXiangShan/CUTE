package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._

class BScaleSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    // val ScratchpadBankId = UInt(log2Ceil(BScaleNSlices).W)
    val ScratchpadAddr = UInt(log2Ceil(BScratchpadBankNEntrys).W)
}

//对于卷积，数据摆放是[khkwoc][ic],对于矩阵乘，数据摆放是[N][K]

class BScaleLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个ScarchPad的接口的总体设计
        val ToScarchPadIO = Flipped(new BScaleLoaderScaratchpadIO)
        val ConfigInfo = Flipped(new BSLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
    })

    io.ToScarchPadIO.BankAddr := 0.U.asTypeOf(io.ToScarchPadIO.BankAddr)
    io.ToScarchPadIO.Data := 0.U.asTypeOf(io.ToScarchPadIO.Data)
    io.LocalMMUIO.Request.valid := false.B
    io.LocalMMUIO.Request.bits := 0.U.asTypeOf(io.LocalMMUIO.Request.bits)
    io.LocalMMUIO.Response.ready := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B


    // val ScaratchpadBankAddr = io.ToScarchPadIO.BankAddr
    // val ScaratchpadData = io.ToScarchPadIO.Data

    val ConfigInfo = io.ConfigInfo

    val dataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W)) //数据类型，float16、int8等，由TaskController提供

    val ScaratchpadTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val Scale_B_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


    val MemoryRequestSize = RegInit(0.U((log2Ceil(Tensor_N*ReduceGroupSize*ReduceWidthByte)+1).W))

    
    //任务状态机
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)


    //访存状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val Scale_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

    val Conherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供

    
    //如果configinfo有效

    when(state === s_idle){
        //idel状态才可以接受新的配置信息
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            //当前配置的指令有效
            state := s_mm_task
            memoryload_state := s_load_init
            // ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
            dataType := io.ConfigInfo.ApplicationScale_B.dataType
            ScaratchpadTensor_N := io.ConfigInfo.ScaratchpadTensor_N
            ScaratchpadTensor_K := io.ConfigInfo.ScaratchpadTensor_K
            Scale_B_BaseVaddr := io.ConfigInfo.ApplicationScale_B.ApplicationScale_B_BaseVaddr //这个不重要
            Scale_Block_BaseAddr := io.ConfigInfo.ApplicationScale_B.BlockScale_B_BaseVaddr //这个是关键
            Conherent := io.ConfigInfo.Conherent

            if(YJPBMLDebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BSL<%d>]ScaratchpadTensor_N:%d,ScaratchpadTensor_K:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ScaratchpadTensor_N,io.ConfigInfo.ScaratchpadTensor_K)
                printf("[BSL<%d>]Scale_B_BaseVaddr:%x,Scale_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationScale_B.ApplicationScale_B_BaseVaddr,io.ConfigInfo.ApplicationScale_B.BlockScale_B_BaseVaddr)
            }
        }
    }

    //处理取数逻辑，BScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_N*ReduceGroupSize*ReduceWidthByte)+1).W)) //总共要加载的数据量
    val CurrentLoaded_BlockTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    
    val MaxBlockTensor_N_Index = ScaratchpadTensor_N
    val MaxBlockTensor_K_Index = ScaratchpadTensor_K

    //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
    //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BScaleSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new BScaleSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_N*ReduceGroupSize*ReduceWidthByte)).W))

    
    val Request = io.LocalMMUIO.Request
    Request.valid := false.B
    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        MemoryRequestSize := 0.U
        TotalLoadSize := 0.U
        CurrentLoaded_BlockTensor_N := 0.U
        CurrentLoaded_BlockTensor_K := 0.U
        MaxRequestIter := ScaratchpadTensor_K * ScaratchpadTensor_N * ScaleVecWidth(dataType) / (outsideDataWidthByte.U) //总共要发出的访存请求的次数
        printf("[bSL] MaxRequestIter:%d. ScaratchpadTensor_K:%d,ScaratchpadTensor_N:%d,ScaleVecWidth:%d,outsideDataWidthByte:%d\n",MaxRequestIter,ScaratchpadTensor_K,ScaratchpadTensor_N,ScaleVecWidth(dataType), outsideDataWidthByte.U)
    }.elsewhen(memoryload_state === s_load_working){
        //根据不同的MemoryOrder，执行不同的访存模式

        //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
        //不用担心乘法电路延迟，再不济，可以提前几个周期将乘法结果算好，做成fifo送进来
        Request.bits.RequestVirtualAddr := Scale_Block_BaseAddr + outsideDataWidthByte.U * MemoryRequestSize
        
        val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
        Request.bits.RequestConherent := Conherent
        Request.bits.RequestSourceID := sourceId.bits
        Request.bits.RequestType_isWrite := false.B
        Request.valid := true.B
        when(MemoryRequestSize === MaxRequestIter)//Is_invalid_IH_IW时，不发出访存请求，尝试直接0填充
        {
            Request.valid := false.B
        }

        
        // 内存排布是每个子张量的因子都连续放，具体看文档
        
        when(Request.fire && sourceId.valid){//符合条件的话，这条访存请求一定会被发出
            //Request.ready表明了LocalMMU会处理这条访存请求，sourceID valid，表明这条访存请求的sourceID是被LocalMMU认可有效才发送到这个模块的
            val TableItem = Wire(new BScaleSourceIdSearch)
            TableItem.ScratchpadAddr := MemoryRequestSize 
            SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
            if (YJPBMLDebugEnable)
            {
                //输出id和request的信息
                printf("[BML<%d>]sourceId:%d,ScratchpadAddr:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.ScratchpadAddr)
                //输出这次request的信息
                printf("[BML<%d>]RequestVirtualAddr:%x,RequestConherent:%d,RequestSourceID:%d,RequestType_isWrite:%d\n",io.DebugInfo.DebugTimeStampe,Request.bits.RequestVirtualAddr,Request.bits.RequestConherent,Request.bits.RequestSourceID,Request.bits.RequestType_isWrite)
            }
            when(MemoryRequestSize < MaxRequestIter){
                MemoryRequestSize := MemoryRequestSize + 1.U
            }
        }
        
        //接受访存的返回值
        //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
        //根据response的sourceid，找到对应的Scarchpad的Fill_Table的队伍头的索引，填充到Fill_Table中
        io.LocalMMUIO.Response.ready := true.B
        
        when(io.LocalMMUIO.Response.fire){
            //Trick注意这个设计，是doublebuffer的，AB只能是doublebuffer，回数一定是不会堵的，而且我们有时间对数据进行压缩解压缩～
            //如果要做release设计，要么数据位宽翻倍，腾出周期来使得有空泡能给写任务进行，要么就是数据位宽不变，将读写端口变成独立的读和独立的写端口
            val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new BScaleSourceIdSearch).ScratchpadAddr
            val ResponseData = Wire(Vec(BScaleNSlices,UInt((ScaleWidth * ReduceGroupSize).W)))
            ResponseData := io.LocalMMUIO.Response.bits.ReseponseData.asTypeOf(ResponseData)

            TotalLoadSize := TotalLoadSize + 1.U
            io.ToScarchPadIO.BankAddr.valid := true.B
            io.ToScarchPadIO.BankAddr.bits := ScratchpadAddr
            io.ToScarchPadIO.Data.valid := true.B
            for (i <- 0 until BScaleNSlices)
            {
                io.ToScarchPadIO.Data.bits(i) := ResponseData(i)
            }

            
            if (YJPBMLDebugEnable)
            {
                //输出这次response的信息
                printf("[BSL<%d>]ResponseData:%x,ScratchpadAddr:%d\n",io.DebugInfo.DebugTimeStampe,io.LocalMMUIO.Response.bits.ReseponseData,ScratchpadAddr)
            }
        }

        //状态机切换
        when(TotalLoadSize === (MaxRequestIter)){
            memoryload_state := s_load_end
            if (YJPCMLDebugEnable)
            {
                printf("[BScaleLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
            }
        }
        
    }.elsewhen(memoryload_state === s_load_end){
        io.ConfigInfo.MicroTaskEndValid := true.B
        when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
            memoryload_state := s_load_idle
            state := s_idle
            if(YJPBMLDebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task End\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
    }

}