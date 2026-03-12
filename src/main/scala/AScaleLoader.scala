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

class AScaleSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val ScratchpadAddr = UInt(log2Ceil(AScratchpadBankNEntrys).W)
}

//对于卷积，数据摆放是[khkwoc][ic],对于矩阵乘，数据摆放是[N][K]

class AScaleLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个ScarchPad的接口的总体设计
        val ToScarchPadIO = Flipped(new AScaleLoaderScaratchpadIO)
        val ConfigInfo = Flipped(new ASLMicroTaskConfigIO)
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

    val ScaratchpadTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val Scale_A_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


    val MemoryRequestSize = RegInit(0.U((log2Ceil(Tensor_M*ReduceGroupSize*ReduceWidthByte)+1).W))

    
    //任务状态机
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)


    //访存状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

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
            dataType := io.ConfigInfo.ApplicationScale_A.dataType
            ScaratchpadTensor_M := io.ConfigInfo.ScaratchpadTensor_M
            ScaratchpadTensor_K := io.ConfigInfo.ScaratchpadTensor_K
            Scale_A_BaseVaddr := io.ConfigInfo.ApplicationScale_A.ApplicationScale_A_BaseVaddr //这个不重要
            Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationScale_A.BlockScale_A_BaseVaddr //这个是关键
            Conherent := io.ConfigInfo.Conherent

            if(YJPAMLDebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BSL<%d>]ScaratchpadTensor_N:%d,ScaratchpadTensor_K:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ScaratchpadTensor_M,io.ConfigInfo.ScaratchpadTensor_K)
                printf("[BSL<%d>]Scale_B_BaseVaddr:%x,Scale_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationScale_A.ApplicationScale_A_BaseVaddr,io.ConfigInfo.ApplicationScale_A.BlockScale_A_BaseVaddr)
                // printf("[BSL<%d>]ApplicationTensor_B_Stride_N:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationScale_A.ApplicationTensor_A_Stride_M)
            }
        }
    }

    //三个张量的虚拟地址，肯定得是连续的，这个可以交给操作系统和编译器来保证

    //A的数据已经完成了reorder
    //32×32×4B的数据    --->    一个4K页
    //32×128×1B的数据   --->    一个4K页
    //64×64×1B的数据    --->    一个4K页

    //页面内数据怎么排好像也无所谓，只要数据对齐且数据连续的就行了
    //这里的数据排布、更多的是为了memory连续读取时的性能考虑
    //那最好把单次读取的数据，都先放在一个页内不去连续的处理N个页？
    //那首先，每次连续读取的Tensor的数据是   AScartchpad = Tensor_M×Tensor_K×ReduceWidth = 64×64×256bit = 128KB
    //                                  BSctatchpad = Tensor_K×Tensor_N×ReduceWidth = 64×64×256bit = 128KB
    //                                  CScartchpad = Tensor_M×Tensor_N×ResultWidth = 64×64×32bit = 16KB


    //这里的Scaratchpad，有可以节省大小的方案，就是尽可能早的去标记某个数据是无效的，然后对下一个数据发出请求，这样对SRAM的读写端口数量要求就高了，多读写端口vsdoublebufferSRAM
    //LLC的访存带宽我们设定成和每个bank的每个entry的大小一样。

    //处理取数逻辑，BScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_M*ReduceGroupSize*ReduceWidthByte)+1).W)) //总共要加载的数据量
    val CurrentLoaded_BlockTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    
    val MaxBlockTensor_M_Index = ScaratchpadTensor_M
    val MaxBlockTensor_K_Index = ScaratchpadTensor_K

    //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
    //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BScaleSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new AScaleSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_M*ReduceGroupSize*ReduceWidthByte)).W))

    
    val Request = io.LocalMMUIO.Request
    Request.valid := false.B
    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        MemoryRequestSize := 0.U
        TotalLoadSize := 0.U
        CurrentLoaded_BlockTensor_M := 0.U
        CurrentLoaded_BlockTensor_K := 0.U
        MaxRequestIter := ScaratchpadTensor_K * ScaratchpadTensor_M * ScaleVecWidth(dataType) / (outsideDataWidthByte.U) //总共要发出的访存请求的次数
        printf("[bSL] MaxRequestIter:%d. ScaratchpadTensor_K:%d,ScaratchpadTensor_M:%d,ScaleVecWidth:%d,outsideDataWidthByte:%d\n",MaxRequestIter,ScaratchpadTensor_K,ScaratchpadTensor_M,ScaleVecWidth(dataType), outsideDataWidthByte.U)
    }.elsewhen(memoryload_state === s_load_working){
        //根据不同的MemoryOrder，执行不同的访存模式

        //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
        //不用担心乘法电路延迟，再不济，可以提前几个周期将乘法结果算好，做成fifo送进来
        Request.bits.RequestVirtualAddr := Tensor_Block_BaseAddr + outsideDataWidthByte.U * MemoryRequestSize
        
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
            val TableItem = Wire(new AScaleSourceIdSearch)
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
            val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new AScaleSourceIdSearch).ScratchpadAddr
            val ResponseData = Wire(Vec(AScaleNSlices,UInt((ScaleWidth * ReduceGroupSize).W)))
            ResponseData := io.LocalMMUIO.Response.bits.ReseponseData.asTypeOf(ResponseData)

            TotalLoadSize := TotalLoadSize + 1.U
            io.ToScarchPadIO.BankAddr.valid := true.B
            io.ToScarchPadIO.BankAddr.bits := ScratchpadAddr
            io.ToScarchPadIO.Data.valid := true.B
            for (i <- 0 until AScaleNSlices)
            {
                io.ToScarchPadIO.Data.bits(i) := ResponseData(i)
            }


            //需要一个fifo？TODO:需要fifo的设计是可能这里会堵，实际上我们满吞吐的doublebuff的设计，咱们这里是不会堵的，直接填就完事了？还是等总线上去握手？
            //Scartchpad->MemoryLoader->MMU->Memory Bus->Memory上的长组合逻辑链，可以实现一下，为后续的开发做准备
            //否则就靠软件来保证数据流和访存流，保证访存流的稳定性，一定不会堵，就可以省下这个长组合逻辑的延迟？
            //还有一点，我们的ScartchPad是写优先的呀！！！所以只要写端口数唯一，就不会堵，不需要fifo～～～
            //Trick:写优先是真的很有说法，本来外部存储就是慢的，读快速存储器的任务等一等就好了，但是所有的ScartchPad都想要读数据的，不能等，所以写优先
            
            //根据response的的id
            //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
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