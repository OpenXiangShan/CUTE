package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.SeqToAugmentedSeq

//CMemoryLoader，用于加载C矩阵的数据，供给MatrixReg使用
//从不同的存储介质中加载数据，供给MatrixReg使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据MatrixReg的设计，完成Tensor的切分以及将数据的填入MatrixReg

class MultiChannelsCMemLoader(implicit p: Parameters) extends CuteModule{
    class CSourceId(implicit p: Parameters) extends CuteBundle{
        val MatrixRegBankId =UInt(log2Ceil(CMatrixRegNBanks).W)
        val MatrixRegAddr = UInt(log2Ceil(CMatrixRegBankNEntries).W)
        val MatrixRegisTail = Bool()
    }
    private val CSourceIdAddrWidth = log2Ceil(CMatrixRegBankNEntries)
    private val CSourceIdBankWidth = log2Ceil(CMatrixRegNBanks)
    private val CSourceIdTailOffset = CSourceIdAddrWidth + CSourceIdBankWidth

    private def encodeCSourceId(bank: UInt, addr: UInt, isTail: Bool): UInt =
        Cat(isTail, bank(CSourceIdBankWidth - 1, 0), addr(CSourceIdAddrWidth - 1, 0))

    private def decodeCSourceId(source: UInt): CSourceId = {
        val decoded = Wire(new CSourceId)
        decoded.MatrixRegAddr := source(CSourceIdAddrWidth - 1, 0)
        decoded.MatrixRegBankId := source(CSourceIdTailOffset - 1, CSourceIdAddrWidth)
        decoded.MatrixRegisTail := source(CSourceIdTailOffset)
        decoded
    }
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new CMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new CMLMicroTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
    })

    println(s"[CUTE] CMatrix: RegBanks ${CMatrixRegNBanks} BankEntries ${CMatrixRegBankNEntries}")

    // 对外统一使用 ToMatrixRegIO

    io.ConfigInfo.LoadMicroTaskEndValid := false.B
    io.ConfigInfo.StoreMicroTaskEndValid := false.B
    io.ConfigInfo.LoadMicroTaskReady := false.B
    io.ConfigInfo.StoreMicroTaskReady := false.B
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.Data := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequestToMatrixReg.Data)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask)
    for (i <- 0 until CMatrixRegNBanks) {
        io.LoadLocalMMUIO.Request(i).valid := false.B
        io.LoadLocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LoadLocalMMUIO.Request(i).bits)
        io.LoadLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LoadLocalMMUIO.Response(i).ready := false.B
        io.StoreLocalMMUIO.Request(i).valid := false.B
        io.StoreLocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.StoreLocalMMUIO.Request(i).bits)
        io.StoreLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.StoreLocalMMUIO.Response(i).ready := false.B
    }
    

    val ConfigInfo = io.ConfigInfo
    val CurrentLoadMatrixRegId = RegInit(0.U(CMatrixRegIdWidth.W))
    val CurrentStoreMatrixRegId = RegInit(0.U(CMatrixRegIdWidth.W))
    io.LoadMatrixRegId := CurrentLoadMatrixRegId
    io.StoreMatrixRegId := CurrentStoreMatrixRegId

    val LoadPcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None
    val StorePcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None

    if (EnableDifftest) {
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_C_WORDS_PER_BANK", CMatrixRegEntryBitSize / 64)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_C_REG_SIZE_BYTES", CMatrixRegSize)

        val loadWriteAny = io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr.map(_.valid).reduce(_||_)
        val loadFinishAny = io.ConfigInfo.LoadMicroTaskEndValid && io.ConfigInfo.LoadMicroTaskEndReady
        val storeFinishAny = io.ConfigInfo.StoreMicroTaskEndValid && io.ConfigInfo.StoreMicroTaskEndReady

        val difftestLoadFinish = DifftestModule(new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank), delay = 0, dontCare = true)
        difftestLoadFinish.coreid := io.ConfigInfo.coreid.get
        difftestLoadFinish.index := 2.U
        difftestLoadFinish.valid := loadWriteAny || loadFinishAny
        difftestLoadFinish.pc := LoadPcReg.get

        val eventWordsPerBank = difftestLoadFinish.data.length / CMatrixRegNBanks
        val cMRegWordsPerBank = CMatrixRegEntryBitSize / 64
        require(difftestLoadFinish.data.length % CMatrixRegNBanks == 0, "DiffAmuFinishEvent.data should divide by C bank count")
        require(CMatrixRegEntryBitSize % 64 == 0, s"CMatrixRegEntryBitSize must be 64-bit aligned, got $CMatrixRegEntryBitSize")
        require(cMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuFinishEvent only supports up to $eventWordsPerBank words per C bank, got $cMRegWordsPerBank")
        for (i <- 0 until CMatrixRegNBanks) {
          difftestLoadFinish.bankValid(i) := io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).valid
          difftestLoadFinish.bankAddr(i) := io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).bits
          difftestLoadFinish.bankMask(i) := io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < cMRegWordsPerBank) {
              val lo = w * 64
              val hi = lo + 63
              difftestLoadFinish.data(i * eventWordsPerBank + w) := io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(i).bits(hi, lo)
            } else {
              difftestLoadFinish.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
        }
        difftestLoadFinish.finish := loadFinishAny

        val difftestStoreFinish = DifftestModule(new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank), delay = 0, dontCare = true)
        difftestStoreFinish.coreid := io.ConfigInfo.coreid.get
        difftestStoreFinish.index := 5.U
        difftestStoreFinish.valid := storeFinishAny
        difftestStoreFinish.pc := StorePcReg.get
        difftestStoreFinish.bankValid.foreach(_ := false.B)
        difftestStoreFinish.bankAddr.foreach(_ := 0.U)
        difftestStoreFinish.bankMask.foreach(_ := 0.U)
        difftestStoreFinish.data.foreach(_ := 0.U)
        difftestStoreFinish.finish := storeFinishAny
    }

    val LoadMatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val LoadMatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val StoreMatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val StoreMatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    //访存读状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val MemoryOrder_LoadConfig = RegInit(MemoryOrderType.OrderTypeUndef)

    //访存写状态机，用来配合流水线刷新
    val s_store_idle :: s_store_init :: s_store_working :: s_store_end :: Nil = Enum(4)
    val memorystore_state = RegInit(s_store_idle)

    val LoadTensorBlockBaseAddr = Reg(UInt(MMUAddrWidth.W))
    val StoreTensorBlockBaseAddr = Reg(UInt(MMUAddrWidth.W))

    val IsLoadConherent = RegInit(true.B)
    val IsStoreConherent = RegInit(true.B)
    val IsStoreTranspose = RegInit(false.B)

    val HasScarhpadRead = WireInit(false.B)
    val HasScarhpadWrite = WireInit(false.B)
    io.ToMatrixRegIO.LoadReadWriteRequest := Cat(0.U(1.W), Cat(HasScarhpadWrite,Cat(0.U(1.W),0.U(1.W))))
    io.ToMatrixRegIO.StoreReadWriteRequest := Cat(HasScarhpadRead, Cat(0.U(1.W),Cat(0.U(1.W),0.U(1.W))))

    val ApplicationTensor_C_Stride_M = RegInit(0.U(MMUAddrWidth.W))
    val ApplicationTensor_D_Stride_M = RegInit(0.U(MMUAddrWidth.W))
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val N_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)
    val Is_RepeatRowLoad = RegInit(false.B)

    val C_DataWidth = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val D_DataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))

    io.ConfigInfo.LoadMicroTaskReady := memoryload_state === s_load_idle
    io.ConfigInfo.StoreMicroTaskReady := memorystore_state === s_store_idle

    when(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.LoadMicroTaskReady) {
        CurrentLoadMatrixRegId := io.ConfigInfo.MatrixRegId
        LoadTensorBlockBaseAddr := io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr
        ApplicationTensor_C_Stride_M := io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M
        IsLoadConherent := io.ConfigInfo.Conherent
        LoadMatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
        LoadMatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
        HasTail := io.ConfigInfo.ApplicationTensor_C.HasTail
        TailByteMask := io.ConfigInfo.ApplicationTensor_C.TailByteMask
        N_Beat_Count := io.ConfigInfo.ApplicationTensor_C.N_Beat_Count

        Is_ZeroLoad := io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad
        Is_FullLoad := io.ConfigInfo.LoadTaskInfo.Is_FullLoad
        Is_RepeatRowLoad := io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad
        val peDataType = new FReducePEDataType
        C_DataWidth := peDataType.CdataByteWidth(io.ConfigInfo.ApplicationTensor_C.dataType)
        memoryload_state := s_load_init
        if (YJPCMLDebugEnable) {
            printf("[CMemoryLoader_Load<%d>]Load C Tensor Start, Tensor_Block_BaseAddr: %x, ApplicationTensor_C_Stride_M: %x, IsConherent: %x,MatrixRegTensor_M: %x,MatrixRegTensor_N: %x,C_DataWidth(zero,full,repeatrow) :(%d,%d,%d)\n", io.DebugInfo.DebugTimeStampe, io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr, io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M, io.ConfigInfo.Conherent,io.ConfigInfo.MatrixRegTensor_M,io.ConfigInfo.MatrixRegTensor_N,io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.asUInt,io.ConfigInfo.LoadTaskInfo.Is_FullLoad.asUInt,io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.asUInt)
        }
        if (EnableDifftest) {
            LoadPcReg.get := io.ConfigInfo.pc.get
        }
    }

    when(io.ConfigInfo.StoreMicroTaskValid && io.ConfigInfo.StoreMicroTaskReady) {
        CurrentStoreMatrixRegId := io.ConfigInfo.MatrixRegId
        StoreTensorBlockBaseAddr := io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr
        IsStoreConherent := io.ConfigInfo.Conherent
        ApplicationTensor_D_Stride_M := io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M
        IsStoreTranspose := io.ConfigInfo.Is_Transpose
        StoreMatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
        StoreMatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
        D_DataType := io.ConfigInfo.ApplicationTensor_D.dataType
        memorystore_state := s_store_init
        if (YJPCMLDebugEnable) {
            printf("[CMemoryLoader_Start<%d>]Store D Tensor Start, Tensor_Block_BaseAddr: %x, ApplicationTensor_D_Stride_M: %x, IsConherent: %x, Is_Transpose: %x,MatrixRegTensor_M: %x,MatrixRegTensor_N: %x\n", io.DebugInfo.DebugTimeStampe, io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr, io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M, io.ConfigInfo.Conherent, io.ConfigInfo.Is_Transpose,io.ConfigInfo.MatrixRegTensor_M,io.ConfigInfo.MatrixRegTensor_N)
        }
        if (EnableDifftest) {
            StorePcReg.get := io.ConfigInfo.pc.get
        }
    }

    assert(!(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.StoreMicroTaskValid),
      "MultiChannelsCMemLoader: split channels share one config payload, load/store valid cannot be high together")



    //三个张量的虚拟地址，肯定得是连续的，这个可以交给操作系统和编译器来保证

    //C的数据需要在这里完成reorder，然后写入memory。
    //同时也能从memory中读取数据，然后reorder，然后写入Scartchpad


    //这里的MatrixReg，有可以节省大小的方案，就是尽可能早的去标记某个数据是无效的，然后对下一个数据发出请求，这样对SRAM的读写端口数量要求就高了，多读写端口vsdoublebufferSRAM
    //LLC的访存带宽我们设定成和每个bank的每个entry的大小一样。

    //处理取数逻辑，AScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总共要执行的MReg的写入的数据量
    val TotalRequestSize = Seq.fill(CMatrixRegNBanks){RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))} //总发出的Memory请求的数据量
    val CurrentLoaded_BlockTensor_M_Iter = Seq.fill(CMatrixRegNBanks){RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))}
    val CurrentLoaded_BlockTensor_N_Iter = Seq.fill(CMatrixRegNBanks){RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))}
    
    //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
    //用sourceid做索引，存储MatrixReg的地址和bank号，是一组寄存器

    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    class FillBundle extends Bundle {
        val addr = UInt(log2Ceil(CMatrixRegBankNEntries).W)
        val data = UInt(outsideDataWidth.W)
        val isTail = Bool()
    }
    val fillQueues = Seq.fill(CMatrixRegNBanks){Module(new Queue(new FillBundle, 8, pipe = true))}
    // init fillQueues input ports
    for (i <- 0 until CMatrixRegNBanks) {
        fillQueues(i).io.enq.valid := false.B
        fillQueues(i).io.enq.bits.addr := 0.U
        fillQueues(i).io.enq.bits.data := 0.U
        fillQueues(i).io.enq.bits.isTail := false.B
        fillQueues(i).io.deq.ready := false.B
    }

    val MAX_Fill_Times = outsideDataWidthByte/CMatrixRegEntryByteSize

    val Repeat_Fill_Is_Working = RegInit(false.B)//是否在回填数据
    val Repeat_Fill_Times = RegInit(0.U(log2Ceil(Tensor_MN).W))//记录这个数据需要回填的次数
    val Repeat_Fill_Group_Times = RegInit(0.U(log2Ceil(outsideDataWidthByte/CMatrixRegEntryByteSize).W))//记录这个数据需要回填的次数
    val Repeat_Fill_Table_Index = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个数据在FIFO里的index
    val Repeat_Fill_Request_Infight = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个有多少请求已经发出，由于我们一个发出的请求需要回填16拍，所以必须记录一下infight的数量，不能多发请求

    io.LoadLocalMMUIO.Request.foreach(_.valid := false.B)
    io.StoreLocalMMUIO.Request.foreach(_.valid := false.B)

    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        TotalRequestSize.foreach(_ := 0.U)
        CurrentLoaded_BlockTensor_M_Iter.foreach(_ := 0.U)
        CurrentLoaded_BlockTensor_N_Iter.foreach(_ := 0.U)
        MaxRequestIter := LoadMatrixRegTensor_M * N_Beat_Count //总共要发出的访存请求的次数
        Repeat_Fill_Times := 0.U
        Repeat_Fill_Group_Times := 0.U
        Repeat_Fill_Request_Infight := 0.U
        Repeat_Fill_Is_Working := false.B
    }.elsewhen(memoryload_state === s_load_working){
        //根据不同的MemoryOrder，执行不同的访存模式
        //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
        //担心乘法电路延迟，可以提前几个周期将乘法结果算好
        //TODO:注意这里的分块逻辑/地址拼接的逻辑，我们在设计MemoryOrderType分块的逻辑时，要考虑到这里的求地址的电路逻辑，是可以减少这部分的乘法电路的逻辑的
        //注意MatrixReg内的存数的状态

        //数据在C MatrixReg中的编排
        //数据会先排N，再排M,这里每个都是4byte的数据，是一个全精度的数据，是一个element，和AML、BML里的不是一个概念
        //   N 0 1 2 3 4 5 6 7     CMatrixRegData里的排布
        // M                               {bank  [0] [1]     [2] [3] }
        // 0   0 1 2 3 4 5 6 7   |addr    0 |    0123 89ab   ghij opgr 
        // 1   8 9 a b c d e f   |        1 |    4567 cdef   klmn stuv 
        // 2   g h i j k l m n   |        2 |    wxyz !...   @... #... 
        // 3   o p g r s t u v   |        3 |    .... ....   .... ....
        // 4   w x y z .......   |        4 |    .... ....   .... .... 
        // 5   !..............   |        5 |    .... ....   .... ....
        // 6   @..............   |        6 |    .... ....   .... ....
        // 7   #..............   |        7 |    .... ....   .... .... 
        // 8   $..............   | ....................................

        //向量的访存顺序
        //01,89,gh,op,23,ab,ij,gr,45,cd,kl,st,67,ef,mn,uv,打散bank去填数据
        //   N 0 1 2 3 4 5 6 7     CMatrixRegData里的排布
        // M                               {bank  [0] [1]     [2] [3] }
        // 0   0 1 2 3 4 5 6 7   |addr    0 |      0   8       g   o 
        // 1   8 9 a b c d e f   |        1 |      1   9       h   p 
        // 2   g h i j k l m n   |        2 |      2   a       i   q
        // 3   o p g r s t u v   |        3 |    ...沙莉花园. ....   .... ....
        // 4   w x y z .......   |        4 |    .... ....   .... .... 
        // 5   !..............   |        5 |    .... ....   .... ....
        // 6   @..............   |        6 |    .... ....   .... ....
        // 7   #..............   |        7 |    .... ....   .... .... 
        // 8   $..............   | ....................................
        //

        when(Is_FullLoad)
        {
            val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
            val fullTaskMask = Fill(outsideDataWidthByte, true.B)
            for (bank <- 0 until CMatrixRegNBanks) {
                val ReadRequest = io.LoadLocalMMUIO.Request(bank)
                val M_Iter = CurrentLoaded_BlockTensor_M_Iter(bank)
                val N_Iter = CurrentLoaded_BlockTensor_N_Iter(bank)
                val TotalRequestSizeBank = TotalRequestSize(bank)
                val M_cursor = M_Iter + bank.U
                val M_byte_offset = M_cursor * ApplicationTensor_C_Stride_M
                val N_byte_offset = N_Iter << log2Ceil(outsideDataWidthByte)
                ReadRequest.bits.RequestAddr := LoadTensorBlockBaseAddr + M_byte_offset + N_byte_offset

                val csourceId = Wire(new CSourceId)
                csourceId.MatrixRegBankId := bank.U
                csourceId.MatrixRegAddr := (M_Iter / CMatrixRegNBanks.U * (Tensor_MN.U / Matrix_MN.U)) + (N_Iter << log2Ceil(MAX_Fill_Times)) //该访存请求的第零号数据，落在哪个MatrixRegBank的哪个地址上
                csourceId.MatrixRegisTail := HasTail && (N_Iter === (N_Beat_Count - 1.U))

                ReadRequest.valid := (M_cursor < LoadMatrixRegTensor_M) && (N_Beat_Count =/= 0.U) && (TotalRequestSizeBank < MaxRequestIter)
                ReadRequest.bits.RequestConherent := IsLoadConherent
                ReadRequest.bits.RequestSourceID := encodeCSourceId(csourceId.MatrixRegBankId, csourceId.MatrixRegAddr, csourceId.MatrixRegisTail)
                ReadRequest.bits.RequestType_isWrite := false.B
                ReadRequest.bits.UseAllocatedSourceID := false.B
                ReadRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))

                when(ReadRequest.fire){
                    val cycle = io.DebugInfo.DebugTimeStampe
                    printf(cf"[CMLWHZ<${cycle}>][channel ${bank}][LoadRequest] " +
                    cf"Addr ${ReadRequest.bits.RequestAddr}%x, " +
                    cf"cacheBank ${ReadRequest.bits.RequestAddr(8, 6)}, " +
                    cf"bank ${csourceId.MatrixRegBankId}, " +
                    cf"setAddr ${csourceId.MatrixRegAddr}, " +
                    cf"CurrentLoaded_BlockTensor_M_Iter ${CurrentLoaded_BlockTensor_M_Iter}, " +
                    cf"CMatrixRegNBanks ${CMatrixRegNBanks}, " +
                    cf"CurrentLoaded_BlockTensor_N_Iter ${CurrentLoaded_BlockTensor_N_Iter}, " +
                    cf"MatrixRegTensor_M ${LoadMatrixRegTensor_M}, " +
                    cf"MatrixRegTensor_N ${LoadMatrixRegTensor_N}, " +
                    cf"TotalRequestSize ${TotalRequestSize}, " +
                    cf"MaxRequestIter ${MaxRequestIter}, " +
                    cf"C_DataWidth ${C_DataWidth}, " +
                    cf"\n")

                    N_Iter := N_Iter + 1.U
                    when(N_Iter === (N_Beat_Count - 1.U)){
                        N_Iter := 0.U
                        M_Iter := M_Iter + CMatrixRegNBanks.U
                    }

                    //不过我们保证了数据是256bit对齐的～剩下的就是Tensor_M和Tensor_K不满足的情况思考好就行了
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Load<%d>]RequestMatrixRegAddr: %x,RequestMatrixRegBankId: %x,CurrentLoaded_BlockTensor_N_Iter: %x,CurrentLoaded_BlockTensor_M_Iter: %x,RequestAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x, RequestTimes: %d\n", io.DebugInfo.DebugTimeStampe, csourceId.MatrixRegAddr,csourceId.MatrixRegBankId,CurrentLoaded_BlockTensor_N_Iter(bank),CurrentLoaded_BlockTensor_M_Iter(bank),ReadRequest.bits.RequestAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite, TotalRequestSize.reduce(_+_))
                    }
                    when(TotalRequestSizeBank === MaxRequestIter){
                        //assert!
                        //error!
                    }.otherwise{
                        TotalRequestSizeBank := TotalRequestSizeBank + 1.U
                    }
                }


                // OmegaRouter 已保证 channel bank 上的 response 属于 bank bank，
                // 因此直接使用 fillQueues(bank) 是正确的。
                val resp = io.LoadLocalMMUIO.Response(bank)
                val respSourceID = decodeCSourceId(resp.bits.ReseponseSourceID)
                val MatrixRegBankId = respSourceID.MatrixRegBankId
                val MatrixRegAddr = respSourceID.MatrixRegAddr
                val ResponseData = resp.bits.ReseponseData

                val fillq = fillQueues(bank).io
                resp.ready := fillq.enq.ready
                fillq.enq.valid := resp.valid
                fillq.enq.bits.addr := MatrixRegAddr
                fillq.enq.bits.data := ResponseData
                fillq.enq.bits.isTail := respSourceID.MatrixRegisTail

                when(resp.fire){
                    assert(MatrixRegBankId === bank.U,
                        cf"CMemoryLoader: response bankId mismatch on channel $bank, expected $bank but got $MatrixRegBankId. Router may be missing.")
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Load<%d>]ResponseData: %x, MatrixRegBankId: %x, MatrixRegAddr: %x\n",io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr)
                    }
                }
            }

            //检查每个bank是否有数据需要回填
            HasScarhpadWrite := fillQueues.map(_.io.deq.valid).reduce(_ || _)

            val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U(1.W))))
            for (i <- 0 until CMatrixRegNBanks){
                val fillTimes = RegInit(MAX_Fill_Times.U)
                val fillq = fillQueues(i).io
                val allowWrite = io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B
                when(allowWrite)
                {
                    fillq.deq.ready := fillTimes === 1.U
                    when(fillq.deq.valid){
                        Current_Fill_MReg_Time(i) := 1.U
                        val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg
                        val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*CMatrixRegEntryByteSize).W)))))
                        val addr = fillq.deq.bits.addr + (MAX_Fill_Times.U - fillTimes)
                        val fillSlot = MAX_Fill_Times.U - fillTimes
                        val currentIsTail = fillq.deq.bits.isTail
                        val fullByteMask = Fill(CMatrixRegEntryByteSize, true.B)
                        val tailByteMaskVec = Wire(Vec(MAX_Fill_Times, UInt(CMatrixRegEntryByteSize.W)))
                        for (j <- 0 until MAX_Fill_Times) {
                            val high = (j + 1) * CMatrixRegEntryByteSize - 1
                            val low = j * CMatrixRegEntryByteSize
                            tailByteMaskVec(j) := tailTaskMask(high, low)
                        }
                        FIFOData := fillq.deq.bits.data.asTypeOf(FIFOData)
                        MatrixRegWriteRequest.BankAddr(i).bits := addr
                        MatrixRegWriteRequest.BankAddr(i).valid := true.B
                        MatrixRegWriteRequest.Data(i).bits := FIFOData(fillSlot)
                        MatrixRegWriteRequest.Data(i).valid := true.B
                        MatrixRegWriteRequest.ByteMask(i).bits := Mux(currentIsTail, tailByteMaskVec(fillSlot), fullByteMask)
                        MatrixRegWriteRequest.ByteMask(i).valid := true.B

                        fillTimes := fillTimes - 1.U
                        when(fillTimes === 1.U){
                            fillTimes := MAX_Fill_Times.U
                        }

                        if (YJPCMLDebugEnable)
                        {
                            printf("[CMemoryLoader_Load<%d>][LoadWriteReg] bank: %d, regAddr: 0x%x, fillTimes: %d, writeData: 0x%x, byteMask: 0x%x\n", io.DebugInfo.DebugTimeStampe,i.U, addr, fillTimes, FIFOData(fillSlot), MatrixRegWriteRequest.ByteMask(i).bits)
                        }
                    }
                }
            }

            val Current_Load_Fill_Size = WireInit(0.U((log2Ceil(CMatrixRegNBanks)+1).W))
            Current_Load_Fill_Size := PopCount(Current_Fill_MReg_Time.asUInt)

            TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size

            if (YJPCMLDebugEnable)
            {
                when(Current_Load_Fill_Size =/= 0.U)
                {
                    printf("[CMemoryLoader_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                }
            }
            //状态机切换
            when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                memoryload_state := s_load_end
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }.elsewhen(Is_ZeroLoad) {
            //给所有的bank发出写0的请求
            HasScarhpadWrite := true.B
            //每次写所有bank的一个entry，总共要写CMatrixRegBankNEntries次
            val Max_ZeroLoad_Write_Times = CMatrixRegBankNEntries
            for (i <- 0 until CMatrixRegNBanks)
            {
                io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).bits := TotalLoadSize
                io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).valid := true.B
                io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(i).bits := 0.U
                io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(i).valid := true.B
                io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).bits := Fill(CMatrixRegEntryByteSize, true.B)
                io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).valid := true.B
            }

            when(io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B)
            {
                TotalLoadSize := TotalLoadSize + 1.U
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Load<%d>]ZeroLoad, TotalLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, TotalLoadSize)
                }
                when(TotalLoadSize === (Max_ZeroLoad_Write_Times-1).U)
                {
                    memoryload_state := s_load_end
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Load<%d>]ZeroLoadEnd\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
            }
        }
    }.elsewhen(memoryload_state === s_load_end){
        io.ConfigInfo.LoadMicroTaskEndValid := true.B
        when(io.ConfigInfo.LoadMicroTaskEndReady && io.ConfigInfo.LoadMicroTaskEndValid){
            memoryload_state := s_load_idle
            if (YJPCMLDebugEnable)
            {
                printf("[CMemoryLoader_Load<%d>]Load Finish\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
        //闲闲没事做
    }


    //Store时，MReg的数据肯定是Reduce_Dim主序的，顺序取数即可

    /**
     * 将Max_BlockTensor_Major_DIM向下取Matrix_M的倍数来计算正常增长的地址
     */
    val MaxIncStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * 将Max_BlockTensor_Major_DIM向下取Matrix_M的倍数来计算正常增长的地址
     */
    val MaxIncStoreScpRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * 每个 bank 访问 C 矩阵寄存器的次数
     */
    val Max_Load_MReg_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * 总共要发对LLC的访存次数（所有 channel 合计）
     */
    val Max_Store_Memory_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))
    val Max_BlockTensor_Request_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Major_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Per_LLC_Store_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Per_MReg_Load_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Max_SubReduce_DIM = RegInit(0.U((log2Ceil(Tensor_MN)).W))

    /**
     * 每组 tile 的写内存请求数量
     */
    val Per_MReg_Load_Write_Memory_Time = CMatrixReg_Total_Bandwidth_Bit/outsideDataWidth

    /**
     * ${Matrix_MN} 行一组 tile，一共有 ${M_Get_IteratorMax} 行 tile
     */
    val M_Get_IteratorMax = Mux(
        IsStoreTranspose,
        (StoreMatrixRegTensor_M / (Matrix_MN.U * 2.U) + (StoreMatrixRegTensor_M % (Matrix_MN.U * 2.U) =/= 0.U)) * 2.U,
        (StoreMatrixRegTensor_M / Matrix_MN.U) + ((StoreMatrixRegTensor_M % Matrix_MN.U) =/= 0.U))

    /**
     * ${Matrix_MN} 列一组 tile，一共有 ${N_Get_IteratorMax} 列 tile
     */
    val N_Get_IteratorMax = WireInit(0.U(log2Ceil(CMatrixRegBankNEntries).W))
    N_Get_IteratorMax := (StoreMatrixRegTensor_N / Matrix_MN.U)

    val transpose_scp_addr = WireInit(0.U(log2Ceil(CMatrixRegBankNEntries).W))

    class BankStore(bank: Int) {
        /**
         * 写数请求，总共存储的数据量，这个参数表示已经对MMU发出的存储请求次数
         */
        val TotalStoreSize = Reg(UInt(log2Ceil(Tensor_MN*Tensor_MN).W))

        /**
         * 总共读取的请求数据量，这个参数表示已经对ScartchPad发出的读请求次数
         */
        val TotalScpReadCount = Reg(UInt(log2Ceil(Tensor_MN*Tensor_MN).W))
        
        val StoreQueueDepth = 4
        val StoreQueue = Module(new Queue(UInt(outsideDataWidth.W), StoreQueueDepth, pipe = false, flow = false))
        StoreQueue.io.enq.valid := false.B
        StoreQueue.io.enq.bits := 0.U
        StoreQueue.io.deq.ready := false.B

        val CollectTimes = outsideDataWidthByte / (CMatrixRegEntryBitSize / 8)
        println(s"[CML][BankStore] CollecTimes $CollectTimes")
        val BusDataCollector = Reg(Vec(CollectTimes, UInt(CMatrixRegEntryBitSize.W)))
        val collectIter = Reg(UInt(log2Ceil(CollectTimes + 1).W))
        val BusDataValid = Reg(Bool())

        val needStallRegRead = Reg(Bool())
        val stallRegRead = Reg(Bool())

        // Dimension Iters
        val CurrentStore_BlockTensor_SubMajor_DIM_Iter = Reg(UInt(MatrixRegMaxTensorDimBitSize.W))
        val CurrentStore_BlockTensor_Major_DIM_Iter = Reg(UInt(MatrixRegMaxTensorDimBitSize.W))
        val CurrentStore_BlockTensor_Reduce_DIM_Iter = Reg(UInt(MatrixRegMaxTensorDimBitSize.W))

        val WriteResponseCounter = Reg(UInt((log2Ceil(Tensor_MN*Tensor_MN)).W))

        val end = RegInit(true.B)

        def init(): Unit = {
            end := false.B
            TotalStoreSize := 0.U
            TotalScpReadCount := 0.U
            assert(StoreQueue.io.count === 0.U)
            BusDataCollector.foreach(_ := 0.U)
            collectIter := 0.U
            needStallRegRead := false.B
            stallRegRead := false.B
            BusDataValid := false.B
            CurrentStore_BlockTensor_SubMajor_DIM_Iter := 0.U
            CurrentStore_BlockTensor_Major_DIM_Iter := 0.U
            CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
            WriteResponseCounter := 0.U
        }

        def logger(msg: Printable): Unit = {
            if (YJPCMLDebugEnable) {
                printf(
                  cf"[CMemoryLoader_Store<${io.DebugInfo.DebugTimeStampe}>][bank $bank][BankStore] " + msg + "\n"
                )
            }
        }

        def mainLogic(): Unit = {
            //根据MatrixReg的仲裁结果，我们可以读取数据了
            val AllowRead = WireInit(io.ToMatrixRegIO.StoreReadWriteResponse(MatrixRegTaskType.ReadFromMemoryLoaderIndex))
            val BankReadValid = (TotalScpReadCount < MaxIncStoreScpRequestSize) && AllowRead && (!stallRegRead || StoreQueue.io.deq.fire)
            io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).valid := BankReadValid
            io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).bits := Mux(IsStoreTranspose, transpose_scp_addr, TotalScpReadCount)

            when(BankReadValid){
                val TotalScpReadCount_Next = TotalScpReadCount + 1.U
                TotalScpReadCount := TotalScpReadCount_Next
                logger(cf"MReg Load Request times: ${TotalScpReadCount_Next}")
                when(stallRegRead && StoreQueue.io.deq.fire) {
                    logger(cf"StoreQueue deq discard stallRegRead")
                }
            }

            val RegReadResp_Valid = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid
            val RegReadResp_Data = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits

            val NextQueueCount = StoreQueue.io.count + StoreQueue.io.enq.fire.asUInt - StoreQueue.io.deq.fire.asUInt

            when (RegReadResp_Valid) {
                BusDataCollector(collectIter) := RegReadResp_Data
                logger(cf"BusDataCollector($collectIter) := 0x$RegReadResp_Data%x")
                // 设 collectIter === (CollectTimes -2).U 这个周期为 C.
                // 此时指向 BusDataCollector(CollectTimes - 1) 的 RegReadReq 是 valid, C+1 必然写入 BusDataCollector(CollectTimes - 1).
                // 需要在 C 时刻决定 C+1 是否应该继续有效预计写入 BusDataCollector(CollectTimes - 1) 的 RegReadReq:
                //   1. 若即将满队列，本轮 BusDataCollector 需要暂存，不能发送写 Iter 0 的读请求，在 C+1 拉低 read valid
                // 在 C+1 时刻，已经由 C 时刻决定了是否发送读请求，若此时存在 deq ，则 C+2 时刻一定可以入队，因此可以覆盖发送读请求
                when (collectIter === (CollectTimes - 2).U && NextQueueCount === StoreQueueDepth.U) {
                    assert(!stallRegRead)
                    stallRegRead := true.B
                    logger(cf"raise stallRegRead next cycle.")
                }
                when (collectIter === (CollectTimes - 1).U) {
                    collectIter := 0.U
                    BusDataValid := true.B
                }.otherwise{
                    collectIter := collectIter + 1.U
                }
            }

            StoreQueue.io.enq.valid := BusDataValid
            StoreQueue.io.enq.bits := BusDataCollector.asUInt

            when (StoreQueue.io.enq.fire) {
                BusDataValid := false.B
                // 应该保证在写 0 号位的这个周期 fire
                assert(collectIter === 0.U)
            }

            val WriteRequest = io.StoreLocalMMUIO.Request(bank)
            StoreQueue.io.deq.ready := WriteRequest.ready
            WriteRequest.valid := StoreQueue.io.deq.valid
            WriteRequest.bits.RequestAddr := StoreTensorBlockBaseAddr + (CurrentStore_BlockTensor_Major_DIM_Iter + bank.U) * ApplicationTensor_D_Stride_M + CurrentStore_BlockTensor_Reduce_DIM_Iter * D_DataType
            WriteRequest.bits.RequestConherent := IsStoreConherent
            WriteRequest.bits.RequestSourceID := encodeCSourceId(bank.U, TotalStoreSize, false.B)
            WriteRequest.bits.RequestType_isWrite := true.B
            WriteRequest.bits.UseAllocatedSourceID := false.B
            WriteRequest.bits.RequestData := StoreQueue.io.deq.bits
            WriteRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))

            when(WriteRequest.fire){
                logger(
                    cf"WriteRequest.fire: {addr: 0x${WriteRequest.bits.RequestAddr}%x," +
                    cf"source: ${WriteRequest.bits.RequestSourceID}, " +
                    cf"TotalStoreSize: ${TotalStoreSize}, " +
                    cf"CurrentStore_BlockTensor_Major_DIM_Iter: ${CurrentStore_BlockTensor_Major_DIM_Iter}, " +
                    cf"CurrentStore_BlockTensor_SubMajor_DIM_Iter: ${bank.U}, " +
                    cf"CurrentStore_BlockTensor_Reduce_DIM_Iter: ${CurrentStore_BlockTensor_Reduce_DIM_Iter}, " +
                    cf"RequestData: 0x${WriteRequest.bits.RequestData}%x}"
                )

                TotalStoreSize := TotalStoreSize + 1.U

                // 向右移动，跳过一次访存覆盖的列数
                CurrentStore_BlockTensor_Reduce_DIM_Iter := CurrentStore_BlockTensor_Reduce_DIM_Iter + Per_LLC_Store_ReduceDim_Iter
                when(CurrentStore_BlockTensor_Reduce_DIM_Iter === Max_BlockTensor_Request_Reduce_DIM - Per_LLC_Store_ReduceDim_Iter){
                    CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
                    CurrentStore_BlockTensor_Major_DIM_Iter := CurrentStore_BlockTensor_Major_DIM_Iter + Matrix_MN.U
                }
            
                // StoreQueue 出队，可同时写入 BusCollector 0 号位
                when(stallRegRead) {
                    stallRegRead := false.B
                }

                when(TotalStoreSize === (Max_Store_Memory_Time / Matrix_MN.U - 1.U)){
                    end := true.B
                    logger(cf"go StoreEnd.")
                }
            }
        }

        def onResponse() = {
            val resp = io.StoreLocalMMUIO.Response(bank)
            resp.ready := true.B
            when(resp.fire) {
                WriteResponseCounter := WriteResponseCounter + 1.U
                logger(cf"WriteResponse: sourceId: ${resp.bits.ReseponseSourceID}, CurrentCount(Include): ${WriteResponseCounter + 1.U}")
            }
        }
    }

    val BankStores = Seq.tabulate(Matrix_MN){ i => new BankStore(i) }

    when(memorystore_state === s_store_init){
        memorystore_state := s_store_working

        BankStores.foreach(_.init())

        Max_Load_MReg_Time := StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / CMatrixReg_Total_Bandwidth.U

        Max_Store_Memory_Time := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_M) * StoreMatrixRegTensor_N * D_DataType / outsideDataWidthByte.U

        MaxIncStoreScpRequestSize := M_Get_IteratorMax * Matrix_MN.U * StoreMatrixRegTensor_N * D_DataType / CMatrixReg_Total_Bandwidth.U

        Per_LLC_Store_ReduceDim_Iter := Mux(D_DataType === 1.U, outsideDataWidthByte.U,
                                Mux(D_DataType === 2.U, outsideDataWidthByte.U/2.U,
                                Mux(D_DataType === 4.U, outsideDataWidthByte.U/4.U, outsideDataWidthByte.U)))

        Per_MReg_Load_ReduceDim_Iter := Mux(D_DataType === 1.U, CMatrixReg_Total_Bandwidth.U,
                                    Mux(D_DataType === 2.U, CMatrixReg_Total_Bandwidth.U/2.U,
                                    Mux(D_DataType === 4.U, CMatrixReg_Total_Bandwidth.U/4.U, CMatrixReg_Total_Bandwidth.U)))

        Max_SubReduce_DIM := Mux(D_DataType === 1.U, CMatrixReg_Total_Bandwidth.U,
                                Mux(D_DataType === 2.U, CMatrixReg_Total_Bandwidth.U/2.U,
                                Mux(D_DataType === 4.U, CMatrixReg_Total_Bandwidth.U/4.U, CMatrixReg_Total_Bandwidth.U)))

        Max_BlockTensor_Reduce_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N)

        // C矩阵 列 维度
        Max_BlockTensor_Request_Reduce_DIM := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_N)

        // C矩阵 行 维度
        Max_BlockTensor_Major_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M)
    }.elsewhen(memorystore_state === s_store_working){
        HasScarhpadRead := BankStores.map(_.TotalScpReadCount).map(_ < MaxIncStoreScpRequestSize).reduce(_||_)
        for (bank <- 0 until Matrix_MN) {
            BankStores(bank).mainLogic()
        }

        val allWillEnd = BankStores.map(_.end).reduce(_ && _)
        when (allWillEnd) {
            memorystore_state := s_store_end
        }
    }.elsewhen(memorystore_state === s_store_end){
        val WriteResponseCountSum = BankStores.map(_.WriteResponseCounter).reduce(_ + _)
        when(WriteResponseCountSum === Max_Store_Memory_Time) {
            io.ConfigInfo.StoreMicroTaskEndValid := true.B
        }
        when(io.ConfigInfo.StoreMicroTaskEndReady && io.ConfigInfo.StoreMicroTaskEndValid){
            memorystore_state := s_store_idle
            if (YJPCMLDebugEnable)
            {
                printf("[CMemoryLoader_Store<%d>]Store Finish\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
        //闲闲没事做
    }

    when(memorystore_state === s_store_working || memorystore_state === s_store_end){
        for (bank <- 0 until Matrix_MN) {
            BankStores(bank).onResponse()
        }
    }

    if (YJPCMLDebugEnable) {
        when(RegNext(memorystore_state === s_store_init)) {
            printf(
                cf"[CMemoryLoader_Store<${io.DebugInfo.DebugTimeStampe}>] Store D Tensor Start\n" +
                cf"  Max_Load_MReg_Time: ${Max_Load_MReg_Time}\n" +
                cf"  Max_Store_Memory_Time: ${Max_Store_Memory_Time}\n" +
                cf"  MaxIncStoreScpRequestSize: ${MaxIncStoreScpRequestSize}\n" +
                cf"  Per_LLC_Store_ReduceDim_Iter: ${Per_LLC_Store_ReduceDim_Iter}\n" +
                cf"  Per_MReg_Load_ReduceDim_Iter: ${Per_MReg_Load_ReduceDim_Iter}\n" +
                cf"  Max_SubReduce_DIM: ${Max_SubReduce_DIM}\n" +
                cf"  Max_BlockTensor_Reduce_DIM: ${Max_BlockTensor_Reduce_DIM}\n" +
                cf"  Max_BlockTensor_Request_Reduce_DIM: ${Max_BlockTensor_Request_Reduce_DIM}\n" +
                cf"  Max_BlockTensor_Major_DIM: ${Max_BlockTensor_Major_DIM}\n"
            )
        }
    }
}
