
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._

//BMemoryLoader，用于加载B矩阵的数据，供给MatrixReg使用
//从不同的存储介质中加载数据，供给MatrixReg使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据MatrixReg的设计，完成Tensor的切分以及将数据的填入MatrixReg

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

class BSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(ABMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(ABMatrixRegBankNEntrys).W)
}

//对于卷积，数据摆放是[khkwoc][ic],对于矩阵乘，数据摆放是[N][K]

class BMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个 MatrixReg 的接口的总体设计
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new BMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })
    // 对外统一使用 ToMatrixRegIO

    io.ToMatrixRegIO.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.Data.map(_.valid := false.B)
    io.ToMatrixRegIO.Data.map(_.bits := DontCare)
    io.ToMatrixRegIO.ZeroFill.map(_.valid := false.B)
    io.ToMatrixRegIO.ZeroFill.map(_.bits := DontCare)
    io.LocalMMUIO.Request.valid := false.B
    io.LocalMMUIO.Request.bits := DontCare // It will be set if Request is valid
    io.LocalMMUIO.Response.ready := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    if (EnableDifftest) {
      val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent, delay = 0, dontCare = true)
        // 默认值初始化
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        difftestAmuFinish.index := 1.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        for (i <- 0 until ABMatrixRegNBanks) {
          difftestAmuFinish.bankValid(i) := io.ToMatrixRegIO.BankAddr(i).valid
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.data(i * 4 + 0) := io.ToMatrixRegIO.Data(i).bits(63,0)
          difftestAmuFinish.data(i * 4 + 1) := io.ToMatrixRegIO.Data(i).bits(127,64)
          difftestAmuFinish.data(i * 4 + 2) := io.ToMatrixRegIO.Data(i).bits(191,128)
          difftestAmuFinish.data(i * 4 + 3) := io.ToMatrixRegIO.Data(i).bits(255,192)
        }
        difftestAmuFinish.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    }

    val MatrixRegBankAddr = io.ToMatrixRegIO.BankAddr
    val MatrixRegData = io.ToMatrixRegIO.Data


    val ConfigInfo = io.ConfigInfo
    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    val MatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val Tensor_B_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))

    val ApplicationTensor_B_Stride_N = RegInit(0.U(MMUAddrWidth.W))

    
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
            MatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := io.ConfigInfo.MatrixRegId
            Tensor_B_BaseVaddr := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr //这个不重要
            Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr //这个是关键
            Conherent := io.ConfigInfo.Conherent
            ApplicationTensor_B_Stride_N := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N //下一个N，需要增加多少地址偏移量
            if(YJPBMLDebugEnable)
            {
                printf("[BML<%d>]BMemoryLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BML<%d>]MatrixRegTensor_N:%d,MatrixRegTensor_K:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.MatrixRegTensor_N,io.ConfigInfo.MatrixRegTensor_K)
                printf("[BML<%d>]Tensor_B_BaseVaddr:%x,Tensor_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr,io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr)
                printf("[BML<%d>]ApplicationTensor_B_Stride_N:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N)
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
    //那首先，每次连续读取的Tensor的数据是   AScartchpad = Tensor_MN×Tensor_K×ReduceWidth = 64×64×256bit = 128KB
    //                                  BSctatchpad = Tensor_K×Tensor_MN×ReduceWidth = 64×64×256bit = 128KB
    //                                  CScartchpad = Tensor_MN×Tensor_MN×ResultWidth = 64×64×32bit = 16KB


    //这里的MatrixReg，有可以节省大小的方案，就是尽可能早的去标记某个数据是无效的，然后对下一个数据发出请求，这样对SRAM的读写端口数量要求就高了，多读写端口vsdoublebufferSRAM
    //LLC的访存带宽我们设定成和每个bank的每个entry的大小一样。

    //处理取数逻辑，BScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W)) //总共要加载的数据量
    val CurrentLoaded_BlockTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    
    val MaxBlockTensor_N_Index = MatrixRegTensor_N
    val MaxBlockTensor_K_Index = MatrixRegTensor_K

    //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
    //用sourceid做索引，存储MatrixReg的地址和bank号，是一组寄存器
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new BSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(ABMatrixRegBankNEntrys).W)))))//记录这个LLC回的数是在scp的哪个地址
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/ABMatrixRegEntryByteSize)+1).W)))))//记录这个LLC回的数需要回填的次数，完成就可以将数据释放了
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)//记录这个FIFO能否能填数据
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)//记录这个FIFO里的数据是否有效
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)//返回第一个空位的index
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)//这个FIFO是否还有空位
    val MAX_Fill_Times = outsideDataWidthByte/ABMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))))//记录fifo里的数据是哪个bank的
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))//想要往scp里bank(x)写的最后一个scp_fill_fifo的index
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//每个bank，是否有数据需要写scp
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//是否有数据需要写scp

    for(i <- 0 until ABMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), BMemoryLoaderReadFromMemoryFIFODepth)//fifo满了
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//这个bank不需要写scp
    }

    
    val Request = io.LocalMMUIO.Request
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            CurrentLoaded_BlockTensor_N := 0.U
            CurrentLoaded_BlockTensor_K := 0.U
            MaxRequestIter := MatrixRegTensor_K * MatrixRegTensor_N * ReduceWidthByte.U / (outsideDataWidthByte.U) //总共要发出的访存请求的次数
        }
        is(s_load_working) {
            //根据不同的MemoryOrder，执行不同的访存模式

            //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
            //不用担心乘法电路延迟，再不济，可以提前几个周期将乘法结果算好，做成fifo送进来
            Request.bits.RequestVirtualAddr := Tensor_Block_BaseAddr + (CurrentLoaded_BlockTensor_N * ApplicationTensor_B_Stride_N) + (CurrentLoaded_BlockTensor_K * ReduceWidthByte.U)
            
            val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            Request.bits.RequestConherent := Conherent
            Request.bits.RequestSourceID := sourceId.bits
            Request.bits.RequestType_isWrite := false.B
            Request.valid := true.B
            when(CurrentLoaded_BlockTensor_N === MaxBlockTensor_N_Index || CurrentLoaded_BlockTensor_K === MaxBlockTensor_K_Index)//Is_invalid_IH_IW时，不发出访存请求，尝试直接0填充
            {
                Request.valid := false.B
            }

            //数据在MatrixReg中的编排
            //数据会先排K，再排M
            //AVector一定是不同M的数据，K不断送入，直到K迭代完成，再换新的M，
            //   K 0 1 2 3 4 5 6 7     time     AVector     MatrixRegData也这么排布
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

            
            when(Request.fire && sourceId.valid){//符合条件的话，这条访存请求一定会被发出
                //Request.ready表明了LocalMMU会处理这条访存请求，sourceID valid，表明这条访存请求的sourceID是被LocalMMU认可有效才发送到这个模块的
                val TableItem = Wire(new BSourceIdSearch)
                TableItem.MatrixRegBankId := CurrentLoaded_BlockTensor_N % ABMatrixRegNBanks.U
                TableItem.MatrixRegAddr := ((CurrentLoaded_BlockTensor_N / ABMatrixRegNBanks.U) * ReduceGroupSize.U) + CurrentLoaded_BlockTensor_K
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
                if (YJPBMLDebugEnable)
                {
                    //输出id和request的信息
                    printf("[BML<%d>]sourceId:%d,MatrixRegBankId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegBankId,TableItem.MatrixRegAddr)
                    //输出这次request的信息
                    printf("[BML<%d>]RequestVirtualAddr:%x,RequestConherent:%d,RequestSourceID:%d,RequestType_isWrite:%d\n",io.DebugInfo.DebugTimeStampe,Request.bits.RequestVirtualAddr,Request.bits.RequestConherent,Request.bits.RequestSourceID,Request.bits.RequestType_isWrite)
                }
                when(CurrentLoaded_BlockTensor_N < MaxBlockTensor_N_Index){
                    when(CurrentLoaded_BlockTensor_K + MAX_Fill_Times.U < MaxBlockTensor_K_Index){
                        //根据不同的内存Order，计算出访存请求的地址
                        CurrentLoaded_BlockTensor_K := CurrentLoaded_BlockTensor_K + MAX_Fill_Times.U
                    }.otherwise{
                        CurrentLoaded_BlockTensor_K := 0.U
                        CurrentLoaded_BlockTensor_N := CurrentLoaded_BlockTensor_N + 1.U
                    }
                }
            }
            val current_fill_fifo_full = WireInit(false.B)
            when(io.LocalMMUIO.Response.valid)
            {
                val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
                val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch).MatrixRegBankId
                current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
            }
            //接受访存的返回值
            //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
            //根据response的sourceid，找到对应的MatrixReg的Fill_Table的队伍头的索引，填充到Fill_Table中
            if (ABMLNeedMRegFillTable)
            {
                io.LocalMMUIO.Response.ready := MReg_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
            } else 
            {
                io.LocalMMUIO.Response.ready := true.B
            }
            when(io.LocalMMUIO.Response.fire){
                //Trick注意这个设计，是doublebuffer的，AB只能是doublebuffer，回数一定是不会堵的，而且我们有时间对数据进行压缩解压缩～
                //如果要做release设计，要么数据位宽翻倍，腾出周期来使得有空泡能给写任务进行，要么就是数据位宽不变，将读写端口变成独立的读和独立的写端口
                val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
                val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch).MatrixRegBankId
                val MatrixRegAddr = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch).MatrixRegAddr
                val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
                val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)//该bank的fill_fifo_index，标注了它当前在fillfifo的哪个位置，我们一共有bank个fill_fifo

                if (!ABMLNeedMRegFillTable)
                {
                    TotalLoadSize := TotalLoadSize + 1.U
                    for (i <- 0 until ABMatrixRegNBanks)
                    {
                        when(MatrixRegBankId === i.U)
                        {
                            io.ToMatrixRegIO.BankAddr(i).bits := MatrixRegAddr
                            io.ToMatrixRegIO.Data(i).bits := ResponseData
                            io.ToMatrixRegIO.BankAddr(i).valid := true.B
                            io.ToMatrixRegIO.Data(i).valid := true.B
                        }
                    }
                }

                MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U

                Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), BMemoryLoaderReadFromMemoryFIFODepth)
                //需要一个fifo？TODO:需要fifo的设计是可能这里会堵，实际上我们满吞吐的doublebuff的设计，咱们这里是不会堵的，直接填就完事了？还是等总线上去握手？
                //MatrixReg->MemoryLoader->MMU->Memory Bus->Memory上的长组合逻辑链，可以实现一下，为后续的开发做准备
                //否则就靠软件来保证数据流和访存流，保证访存流的稳定性，一定不会堵，就可以省下这个长组合逻辑的延迟？
                //还有一点，我们的MatrixReg是写优先的呀！！！所以只要写端口数唯一，就不会堵，不需要fifo～～～
                //Trick:写优先是真的很有说法，本来外部存储就是慢的，读快速存储器的任务等一等就好了，但是所有的MatrixReg都想要读数据的，不能等，所以写优先
                
                //根据response的的id
                //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
                if (YJPBMLDebugEnable)
                {
                    //输出这次response的信息
                    printf("[BML<%d>]ResponseData:%x,MatrixRegBankId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr)
                }
            }

            // Fill_Table的回填优先级最高，一旦有回填任务就立即执行
            val HasScarhpadWrite = Have_Bank_Fill
            val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))
            if (ABMLNeedMRegFillTable)
            {
                for (i <- 0 until ABMatrixRegNBanks){
                    when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                        val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                        Current_Fill_MReg_Time(i) := 1.U
                        val MatrixRegWriteRequest = io.ToMatrixRegIO
                        val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*ABMatrixRegEntryByteSize).W)))))
                        FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                        MatrixRegWriteRequest.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + (MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex))
                        MatrixRegWriteRequest.BankAddr(i).valid := true.B
                        MatrixRegWriteRequest.Data(i).bits := FIFOData(MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex))
                        MatrixRegWriteRequest.Data(i).valid := true.B

                        MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                        when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                            Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), BMemoryLoaderReadFromMemoryFIFODepth)
                        }

                        if (YJPCMLDebugEnable)
                        {
                            //输出fill_time 和 fifoindex
                            printf("[BML BMemoryLoader_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, MReg_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                            printf("[BML BMemoryLoader_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits)
                        }
                    }
                }
            }

            val Current_Load_Fill_Size = WireInit(0.U((log2Ceil(ABMatrixRegNBanks)+1).W))
            Current_Load_Fill_Size := PopCount(Current_Fill_MReg_Time.asUInt)

            if (ABMLNeedMRegFillTable)
            {
                TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
            }
            if (YJPCMLDebugEnable)
            {
                when(Current_Load_Fill_Size =/= 0.U)
                {
                    printf("[BMemoryLoader_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                }
            }
            //状态机切换
            when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                memoryload_state := s_load_end
                if (YJPCMLDebugEnable)
                {
                    printf("[BMemoryLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
        is(s_load_end) {
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                memoryload_state := s_load_idle
                state := s_idle
                if(YJPBMLDebugEnable)
                {
                    printf("[BML<%d>]BMemoryLoader Task End\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }
}