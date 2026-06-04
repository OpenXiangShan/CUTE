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

class CSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId =UInt(log2Ceil(CMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(CMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
}

class CMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new CMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new CMLMicroTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
    })

    io.ConfigInfo.LoadMicroTaskEndValid := false.B
    io.ConfigInfo.StoreMicroTaskEndValid := false.B
    io.ConfigInfo.LoadMicroTaskReady := false.B
    io.ConfigInfo.StoreMicroTaskReady := false.B
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.Data.map(_.valid := false.B)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.Data.map(_.bits := DontCare)
    io.ToMatrixRegIO.LoadReadWriteRequest := 0.U
    io.ToMatrixRegIO.StoreReadWriteRequest := 0.U
    io.LoadLocalMMUIO.Request.valid := false.B
    io.LoadLocalMMUIO.Request.bits := DontCare
    io.LoadLocalMMUIO.Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
    io.LoadLocalMMUIO.Response.ready := false.B
    io.StoreLocalMMUIO.Request.valid := false.B
    io.StoreLocalMMUIO.Request.bits := DontCare
    io.StoreLocalMMUIO.Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
    io.StoreLocalMMUIO.Response.ready := false.B
    io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask.map(_.valid := false.B)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask.map(_.bits := Fill(CMatrixRegEntryByteSize, true.B))

    val CurrentLoadMatrixRegId = RegInit(0.U(CMatrixRegIdWidth.W))
    val CurrentStoreMatrixRegId = RegInit(0.U(CMatrixRegIdWidth.W))
    io.LoadMatrixRegId := CurrentLoadMatrixRegId
    io.StoreMatrixRegId := CurrentStoreMatrixRegId

    val LoadPcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None
    val StorePcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None

    // Difftest interface
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

        // Store path has no per-bank writeback payload, only finish handshake.
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
    io.ToMatrixRegIO.LoadReadWriteRequest := Cat(0.U(1.W), Cat(HasScarhpadWrite, Cat(0.U(1.W), 0.U(1.W))))
    io.ToMatrixRegIO.StoreReadWriteRequest := Cat(HasScarhpadRead, Cat(0.U(1.W), Cat(0.U(1.W), 0.U(1.W))))

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
    val loadTaskIdle = memoryload_state === s_load_idle
    val storeTaskIdle = memorystore_state === s_store_idle
    io.ConfigInfo.LoadMicroTaskReady := loadTaskIdle
    io.ConfigInfo.StoreMicroTaskReady := storeTaskIdle

    when(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.LoadMicroTaskReady){
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
        if (EnableDifftest) {
          LoadPcReg.get := io.ConfigInfo.pc.get
        }
    }

    when(io.ConfigInfo.StoreMicroTaskValid && io.ConfigInfo.StoreMicroTaskReady){
        CurrentStoreMatrixRegId := io.ConfigInfo.MatrixRegId
        StoreTensorBlockBaseAddr := io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr
        IsStoreConherent := io.ConfigInfo.Conherent
        ApplicationTensor_D_Stride_M := io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M
        IsStoreTranspose := io.ConfigInfo.Is_Transpose
        StoreMatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
        StoreMatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
        D_DataType := io.ConfigInfo.ApplicationTensor_D.dataType
        memorystore_state := s_store_init
        if (EnableDifftest) {
          StorePcReg.get := io.ConfigInfo.pc.get
        }
    }

    assert(!(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.StoreMicroTaskValid),
      "CMemoryLoader: split channels share one config payload, load/store valid cannot be high together")



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
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总发出的Memory请求的数据量
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    
    //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
    //用sourceid做索引，存储MatrixReg的地址和bank号，是一组寄存器
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new CSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new CSourceIdSearch).getWidth.W))))
    
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))
    
    //读数的FIFO
    //MReg_Fill_FIFO是用来记录数据的fifo，最多能暂存CMemoryLoaderReadFromMemoryFIFODepth个数据
    //只要有空位，就可以继续填数据，每次填数据需要完成如下操作
    //1.将数据填入fifo，更新Tail
    //2.将MReg_Fill_FIFO_Time置为0
    //3.将这个数据将要填入的scp的第一个地址填入MReg_Fill_FIFO_MReg_Addr
    //4.根据这个数据的对应的bank号(x)，将这个数据对应的Fill_FIFO的index填入Bank_Fill_Search_FIFO(x)(Bank_Fill_Search_FIFO_Head(x))
    //5.更新Bank_Fill_Search_FIFO_Head(x)
    //每次回填MReg需要完成如下操作：
    //1.查看每一个bank是否有数据需要回填
    //2.给每个准备回填数据的bank，找到其对应的Fill_FIFO的index，在这个fill_fifo[index]的filltime+1，如果filltime==MAX_Fill_Times，那么这个数据就用完了
    //3.更新FIFO，更新Tail，更新Table
    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(CMatrixRegBankNEntries).W)))))//记录这个LLC回的数是在scp的哪个地址
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/CMatrixRegEntryByteSize)+1).W)))))//记录这个LLC回的数需要回填的次数，完成就可以将数据释放了
    val MReg_Fill_Table_IsTail = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(false.B)))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)//记录这个FIFO能否能填数据
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)//记录这个FIFO里的数据是否有效
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)//返回第一个空位的index
    val MReg_Fill_Table_Valid_Index = PriorityEncoder(MReg_Fill_Table_Valid)//返回第一个有效的index,RepeatRowLoad需要用到
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)//这个FIFO是否还有空位
    val MReg_Fill_Table_Not_Empty = MReg_Fill_Table_Valid.reduce(_ || _)//这个FIFO是否还有数据,用于RepeatRowLoad
    val MReg_Fill_Table_Head = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个FIFO里的数据的头指针,用于RepeatRowLoad
    val MReg_Fill_Table_Tail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个FIFO里的数据的尾指针,用于RepeatRowLoad
    val MAX_Fill_Times = outsideDataWidthByte/CMatrixRegEntryByteSize

    val Repeat_Fill_Is_Working = RegInit(false.B)//是否在回填数据
    val Repeat_Fill_Times = RegInit(0.U(log2Ceil(Tensor_MN).W))//记录这个数据需要回填的次数
    val Repeat_Fill_Group_Times = RegInit(0.U(log2Ceil(outsideDataWidthByte/CMatrixRegEntryByteSize).W))//记录这个数据需要回填的次数
    val Repeat_Fill_Table_Index = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个数据在FIFO里的index
    val Repeat_Fill_Request_Infight = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个有多少请求已经发出，由于我们一个发出的请求需要回填16拍，所以必须记录一下infight的数量，不能多发请求

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(CMatrixRegNBanks)(VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))))//记录fifo里的数据是哪个bank的
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(CMatrixRegNBanks)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))//想要往scp里bank(x)写的最后一个scp_fill_fifo的index
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(CMatrixRegNBanks)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//每个bank，是否有数据需要写scp
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//是否有数据需要写scp

    for(i <- 0 until CMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), CMemoryLoaderReadFromMemoryFIFODepth)//fifo满了
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//这个bank不需要写scp
    }

    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(Matrix_MN).W))
    // val Fill_N_Iter_Time = RegInit(0.U(log2Ceil(Tensor_MN).W))
    //读数请求
    val ReadRequest = io.LoadLocalMMUIO.Request
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            CurrentLoaded_BlockTensor_M_Iter := 0.U
            CurrentLoaded_BlockTensor_N_Iter := 0.U
            MaxRequestIter := LoadMatrixRegTensor_M * N_Beat_Count //总共要发出的访存请求的次数
            Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table := 0.U.asTypeOf(MReg_Fill_Table)
            MReg_Fill_Table_MReg_Addr := 0.U.asTypeOf(MReg_Fill_Table_MReg_Addr)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
            MReg_Fill_Table_IsTail := VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(false.B))
            Request_M_Iter_Time := 0.U
            MReg_Fill_Table_Head := 0.U
            MReg_Fill_Table_Tail := 0.U
            Repeat_Fill_Times := 0.U
            Repeat_Fill_Group_Times := 0.U
            Repeat_Fill_Request_Infight := 0.U
            Repeat_Fill_Is_Working := false.B
        }
        is(s_load_working) {
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

            // 断言：保证三者中至多只有一个为 true
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad, Is_RepeatRowLoad)) === 1.U, 
                   "Error! Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad, Is_RepeatRowLoad should be true!")

            when(Is_FullLoad)
            {
                val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
                val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_N_Iter === (N_Beat_Count - 1.U))
                val RequestMatrixRegBankId = (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) % CMatrixRegNBanks.U //访存请求落在哪个MatrixRegBank上
                val RequestMatrixRegAddr = ((CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) / CMatrixRegNBanks.U) * (Tensor_MN.U / Matrix_MN.U) + (CurrentLoaded_BlockTensor_N_Iter << log2Ceil(MAX_Fill_Times)) //该访存请求的第零号数据，落在哪个MatrixRegBank的哪个地址上
                
                ReadRequest.bits.RequestVirtualAddr := LoadTensorBlockBaseAddr + (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) * ApplicationTensor_C_Stride_M + (CurrentLoaded_BlockTensor_N_Iter << log2Ceil(outsideDataWidthByte))
                
                // val CurrentBankID = RequestMatrixRegBankId
                // val CurrentFIFOIndex = FromMemoryLoaderReadFIFOHead

                val sourceId = Mux(IsLoadConherent,io.LoadLocalMMUIO.ConherentRequsetSourceID,io.LoadLocalMMUIO.nonConherentRequsetSourceID)
                

                ReadRequest.bits.RequestConherent := IsLoadConherent
                ReadRequest.bits.RequestSourceID := sourceId.bits
                ReadRequest.bits.RequestType_isWrite := false.B
                ReadRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                ReadRequest.valid := (TotalRequestSize < MaxRequestIter)

                //确定这个访存请求一定会发出
                when(ReadRequest.fire){
                    val TableItem = Wire(new CSourceIdSearch)
                    TableItem.MatrixRegBankId := RequestMatrixRegBankId
                    TableItem.MatrixRegAddr := RequestMatrixRegAddr
                    TableItem.MatrixRegisTail := RequestBeatIsTail
                    SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                    Request_M_Iter_Time := Request_M_Iter_Time + 1.U//连续的跨bank去访存
                    when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === LoadMatrixRegTensor_M - 1.U){
                        Request_M_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + 1.U
                        when(CurrentLoaded_BlockTensor_N_Iter + 1.U === N_Beat_Count){
                            CurrentLoaded_BlockTensor_N_Iter := 0.U
                            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                        }
                    }
                    //输出发出的访存请求
                    // printf("[CMemoryLoader]RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
                    //输出这次请求的TableItem
                    // printf("[CMemoryLoader]TableItem: %x, %x, %x\n", TableItem.MatrixRegBankId, TableItem.MatrixRegAddr, TableItem.FIFOIndex)

                    //只要这条取数指令可以被发出，就计算下一个访存请求的地址
                    //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
                    //不过我们保证了数据是256bit对齐的～剩下的就是Tensor_M和Tensor_K不满足的情况思考好就行了
                    //输出request的次数
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Load<%d>]RequestMatrixRegAddr: %x,RequestMatrixRegBankId: %x,CurrentLoaded_BlockTensor_N_Iter: %x,CurrentLoaded_BlockTensor_M_Iter: %x,Request_M_Iter_Time: %x,RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x, RequestTimes: %d\n", io.DebugInfo.DebugTimeStampe, RequestMatrixRegAddr,RequestMatrixRegBankId,CurrentLoaded_BlockTensor_N_Iter,CurrentLoaded_BlockTensor_M_Iter,Request_M_Iter_Time,ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite, TotalRequestSize)
                    }
                    when(TotalRequestSize === MaxRequestIter){
                        //assert!
                        //error!
                    }.otherwise{
                        TotalRequestSize := TotalRequestSize + 1.U
                    }
                }
                
                val current_fill_fifo_full = WireInit(false.B)
                when(io.LoadLocalMMUIO.Response.valid)
                {
                    val sourceId = io.LoadLocalMMUIO.Response.bits.ReseponseSourceID
                    val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).MatrixRegBankId
                    current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
                }

                io.LoadLocalMMUIO.Response.ready := MReg_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
                //接受访存的返回值
                //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
                //根据response的sourceid，找到对应的MatrixReg的地址和bank号，回填数据
                when(io.LoadLocalMMUIO.Response.fire){
                    val sourceId = io.LoadLocalMMUIO.Response.bits.ReseponseSourceID
                    val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).MatrixRegBankId
                    val MatrixRegAddr = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).MatrixRegAddr
                    val ResponseData = io.LoadLocalMMUIO.Response.bits.ReseponseData
                    val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)//该bank的fill_fifo_index，标注了它当前在fillfifo的哪个位置，我们一共有bank个fill_fifo

                    MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                    MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                    MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                    MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).MatrixRegisTail

                    Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                    Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), CMemoryLoaderReadFromMemoryFIFODepth)

                    //输出回填的数据
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Load<%d>]ResponseData: %x, MatrixRegBankId: %x, MatrixRegAddr: %x, FIFOIndex: %x\n",io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr, FIFOIndex)
                    }
                }

                //检查每个bank是否有数据需要回填
                HasScarhpadWrite := Have_Bank_Fill
                val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U(1.W))))
                for (i <- 0 until CMatrixRegNBanks){
                    when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                        val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                        when(io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B)
                        {
                            Current_Fill_MReg_Time(i) := 1.U
                            val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*CMatrixRegEntryByteSize).W)))))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                            val fullByteMask = Fill(CMatrixRegEntryByteSize, true.B)
                            val tailByteMaskVec = Wire(Vec(MAX_Fill_Times, UInt(CMatrixRegEntryByteSize.W)))
                            for (j <- 0 until MAX_Fill_Times) {
                                val high = (j + 1) * CMatrixRegEntryByteSize - 1
                                val low = j * CMatrixRegEntryByteSize
                                tailByteMaskVec(j) := tailTaskMask(high, low)
                            }
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            MatrixRegWriteRequest.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + fillSlot
                            MatrixRegWriteRequest.BankAddr(i).valid := true.B
                            MatrixRegWriteRequest.Data(i).bits := FIFOData(fillSlot)
                            MatrixRegWriteRequest.Data(i).valid := true.B
                            MatrixRegWriteRequest.ByteMask(i).bits := Mux(currentIsTail, tailByteMaskVec(fillSlot), fullByteMask)
                            MatrixRegWriteRequest.ByteMask(i).valid := true.B

                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), CMemoryLoaderReadFromMemoryFIFODepth)
                            }

                            if (YJPCMLDebugEnable)
                            {
                                //输出fill_time 和 fifoindex
                                printf("[CMemoryLoader_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, MReg_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                                printf("[CMemoryLoader_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits)
                            }
                        }.otherwise
                        {
                            if (YJPCMLDebugEnable)
                            {
                                printf("[CMemoryLoader_Load<%d>]bankid: %d no authority\n", io.DebugInfo.DebugTimeStampe,i.U)
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
            }

            when(Is_ZeroLoad)
            {
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

            // when(Is_RepeatRowLoad)
            // {
            //     //由于RepeatRowLoad的特殊性，我们一次Load需要写MReg很多次,导致我们的FIFO在被写满时，会导致长时间的TL无法握手。
            //     //故，我们针对这样的情况，我们需要为每一个发出的访存请求预留一个FIFO的空位，这样就可以保证TL握手成功，从而不浪费访存带宽，这样可能会导致整体延迟增加(但不会低到阻碍吞吐)，但我们的访存带宽利用率一定不会低
            //     //获取整个Row的数据，然后重复填充，Row的总数据量为Tensor_N*C_DataWidth
            //     val sourceId = Mux(IsConherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            //     val Max_RepeatRowLoad_Memory_Load_Times = Tensor_MN.U * C_DataWidth / outsideDataWidthByte.U //总共要发出的访存请求的次数
            //     val Max_MReg_Write_Times = Tensor_MN*Tensor_MN*ResultWidthByte/CMatrixReg_Total_Bandwidth //总共要写入MReg的次数
            //     ReadRequest.bits.RequestVirtualAddr := Tensor_Block_BaseAddr +  CurrentLoaded_BlockTensor_N_Iter * C_DataWidth
            //     ReadRequest.bits.RequestConherent := IsConherent
            //     ReadRequest.bits.RequestSourceID := sourceId.bits
            //     ReadRequest.bits.RequestType_isWrite := false.B
            //     ReadRequest.valid := (TotalRequestSize < Max_RepeatRowLoad_Memory_Load_Times) && (Repeat_Fill_Request_Infight < (CMemoryLoaderReadFromMemoryFIFODepth-1).U)


            //     val Per_Load_Fill_MReg_Times = (Tensor_MN/Matrix_MN) * (outsideDataWidthByte/CMatrixRegEntryByteSize)
            //     val Per_Data_Repeat_Times = (Tensor_MN/Matrix_MN) //每组数据要重复写MReg这么多次
            //     val Per_Memory_Load_Have_Data_Write_Group = (outsideDataWidthByte/CMatrixRegEntryByteSize)//每次Memory的load，有几组数据要写回
            //     val Per_Write_MReg_Addr_Add = (Tensor_MN / Matrix_MN).U //一组数据Per_Data_Repeat_Times迭代中，下一次写入的scp地址的增量

            //     // val Load_Time = CurrentLoaded_BlockTensor_N_Iter / (outsideDataWidthByte.U/C_DataWidth)

            //     //向量的访存顺序
            //     //01,23,45,67.....
            //     //   N 0 1 2 3 4 5 6 7     CMatrixRegData里的排布
            //     // M                               {bank  [0] [1]     [2] [3] }
            //     // 0   0 1 2 3 4 5 6 7   |addr    0 |      0   0       0   0 
            //     //                       |        1 |      1   1       1   1 
            //     //                       |        2 |      2   2       2   2
            //     //                       |        . |    .... ....   .... ....
            //     //                       |        . |    .... ....   .... .... 
            //     //                       |       15 |     15   15     15   15  
            //     //                       |       16 |      0   0       0   0  
            //     //                       |       17 |    .... ....   .... ....
            //     //                       | ....................................
            //     //
            //     //MReg的写回顺序
            //     //0,16,32,48.....每次加Per_Write_MReg_Addr_Add，一共写Per_Data_Repeat_Times次
            //     //1,17,33,49.....每次加Per_Write_MReg_Addr_Add，一共写Per_Data_Repeat_Times次
            //     //一次Memory的load，有Per_Memory_Load_Have_Data_Write_Group组数据，每组数据写回Per_Data_Repeat_Times次

            //     //确定这个访存请求一定会发出
            //     when(ReadRequest.fire){

            //         val TableItem = Wire(new CSourceIdSearch)
            //         TableItem.MatrixRegBankId := 0.U
            //         TableItem.MatrixRegAddr := TotalRequestSize * Per_Memory_Load_Have_Data_Write_Group.U//这个数据的第一个数据，落在哪个MatrixRegBank的哪个地址上
            //         SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

            //         CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + outsideDataWidthByte.U / C_DataWidth
            //         Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight + 1.U
            //         if (YJPCMLDebugEnable)
            //         {
            //             printf("[CMemoryLoader_Load<%d>]RepeatRowLoad,CurrentLoaded_BlockTensor_N_Iter: %x, RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_N_Iter, ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
            //         }
            //         when(TotalRequestSize === Max_RepeatRowLoad_Memory_Load_Times){
            //             //assert!
            //             //error!
            //         }.otherwise{
            //             TotalRequestSize := TotalRequestSize + 1.U
            //         }
            //     }

            //     io.LocalMMUIO.Response.ready := true.B
            //     when(io.LocalMMUIO.Response.fire){
            //         val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
            //         val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
            //         val FIFOIndex = MReg_Fill_Table_Insert_Index
            //         MReg_Fill_Table(FIFOIndex) := ResponseData
            //         MReg_Fill_Table_MReg_Addr(FIFOIndex) := SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).MatrixRegAddr
            //         MReg_Fill_Table_Time(FIFOIndex) := 1.U//当valid用

            //         if (YJPCMLDebugEnable)
            //         {
            //             printf("[CMemoryLoader_Load<%d>]RepeatRowLoad, ResponseData: %x, FIFOIndex: %x\n", io.DebugInfo.DebugTimeStampe, ResponseData, FIFOIndex)
            //         }
            //     }

                
            //     when(MReg_Fill_Table_Not_Empty && Repeat_Fill_Is_Working === false.B)
            //     {
            //         Repeat_Fill_Is_Working := true.B
            //         Repeat_Fill_Table_Index := MReg_Fill_Table_Valid_Index
            //         Repeat_Fill_Times := 0.U
            //         Repeat_Fill_Group_Times := 0.U
            //     }

            //     when(Repeat_Fill_Is_Working)
            //     {
            //         val CurrentFIFOData = MReg_Fill_Table(Repeat_Fill_Table_Index)
            //         val All_Group_Data = WireInit((VecInit(Seq.fill(Per_Memory_Load_Have_Data_Write_Group)(0.U(CMatrixRegEntryBitSize.W)))))
            //         All_Group_Data := CurrentFIFOData.asTypeOf(All_Group_Data)
            //         val MReg_Write_Addr = MReg_Fill_Table_MReg_Addr(Repeat_Fill_Table_Index) + Repeat_Fill_Times * Per_Write_MReg_Addr_Add + Repeat_Fill_Group_Times

            //         val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg
                    
            //         for (i <- 0 until CMatrixRegNBanks)
            //         {
            //             MatrixRegWriteRequest.BankAddr(i).bits := MReg_Write_Addr
            //             MatrixRegWriteRequest.BankAddr(i).valid := true.B
            //             MatrixRegWriteRequest.Data(i).bits := All_Group_Data(Repeat_Fill_Group_Times)
            //             MatrixRegWriteRequest.Data(i).valid := true.B
            //         }

            //         HasScarhpadWrite := true.B

            //         {
            //             Repeat_Fill_Times := Repeat_Fill_Times + 1.U
            //             TotalLoadSize := TotalLoadSize + 1.U
            //             when(Repeat_Fill_Times === (Per_Data_Repeat_Times - 1).U)
            //             {
            //                 Repeat_Fill_Times := 0.U
            //                 Repeat_Fill_Group_Times := Repeat_Fill_Group_Times + 1.U
            //                 when(Repeat_Fill_Group_Times === (Per_Memory_Load_Have_Data_Write_Group - 1).U)
            //                 {
            //                     Repeat_Fill_Group_Times := 0.U
            //                     Repeat_Fill_Is_Working := false.B
            //                     Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight - 1.U
            //                     when(ReadRequest.fire)
            //                     {
            //                         Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight
            //                     }
            //                     MReg_Fill_Table_Time(Repeat_Fill_Table_Index) := 0.U
            //                 }
            //             }
            //             if (YJPCMLDebugEnable)
            //             {
            //                 printf("[CMemoryLoader_Load<%d>]RepeatRowLoad,Repeat_Fill_Times:  %d,TotalLoadSize:  %d,Repeat_Fill_Group_Times:  %d, MReg_Write_Addr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,Repeat_Fill_Times,TotalLoadSize,Repeat_Fill_Group_Times, MReg_Write_Addr, All_Group_Data(Repeat_Fill_Group_Times))
            //             }
            //         }
            //     }

            //     when(TotalLoadSize === (Max_MReg_Write_Times).U)
            //     {
            //         memoryload_state := s_load_end
            //         if (YJPCMLDebugEnable)
            //         {
            //             printf("[CMemoryLoader_Load<%d>]RepeatRowLoadEnd\n", io.DebugInfo.DebugTimeStampe)
            //         }
            //     }
            // }
        }
        is(s_load_end) {
            io.ConfigInfo.LoadMicroTaskEndValid := true.B
            when(io.ConfigInfo.LoadMicroTaskEndReady && io.ConfigInfo.LoadMicroTaskEndValid){
                memoryload_state := s_load_idle
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Load<%d>]Load Finish\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }


    //Store时，MReg的数据肯定是Reduce_Dim主序的，顺序取数即可
    //写数请求
    val TotalStoreSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总共存储的数据量，这个参数表示已经对MMU发出的存储请求次数
    val TotalStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总共读取的请求数据量，这个参数表示已经对ScartchPad发出的读请求次数
    val MaxIncStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // 将Max_BlockTensor_Major_DIM向下取Matrix_M的倍数来计算正常增长的地址
    val MaxIncStoreScpRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // 将Max_BlockTensor_Major_DIM向下取Matrix_M的倍数来计算正常增长的地址
    val Max_Load_Scp_Tail_SubMajor_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Current_Load_Scp_Tail_subMajor_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Current_Load_Scp_addr = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))
    val Current_Load_M_iter = RegInit(0.U((log2Ceil(Tensor_MN)).W)) //当前的M迭代次数
    val Current_Load_N_iter = RegInit(0.U((log2Ceil(Tensor_MN)).W)) //当前的N迭代次数
    val Max_Load_MReg_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总共要发对SCAP的访存次数
    val Max_Store_Memory_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) //总共要发对LLC的访存次数
    val CurrentStore_BlockTensor_Major_DIM_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentStore_BlockTensor_Reduce_DIM_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Request_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Major_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Per_LLC_Store_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Per_MReg_Load_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Max_SubReduce_DIM = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val CurrentStore_BlockTensor_SubMajor_DIM_Iter= RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentStore_BlockTensor_SubReduce_DIM_Iter= RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Write_Mem_Wait_Table = RegInit(VecInit(Seq.fill(SoureceMaxNum)(false.B))) //存储每个访存请求的等待状态

    //CMemoryLoaderReadFromMatrixRegFIFODepth深度的fifo
    val FromMatrixRegReadFIFO = Reg(Vec(CMemoryLoaderReadFromMatrixRegFIFODepth, UInt(CMatrixReg_Total_Bandwidth_Bit.W)))
    val FromMatrixRegReadFIFOHead = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMatrixRegFIFODepth).W))
    val FromMatrixRegReadFIFOTail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMatrixRegFIFODepth).W))
    val FromMatrixRegReadFIFOFull = FromMatrixRegReadFIFOTail === WrapInc(FromMatrixRegReadFIFOHead, CMemoryLoaderReadFromMatrixRegFIFODepth)
    val FromMatrixRegReadFIFO_ISSUE_Full = FromMatrixRegReadFIFOTail === WrapInc(WrapInc(FromMatrixRegReadFIFOHead, CMemoryLoaderReadFromMatrixRegFIFODepth),CMemoryLoaderReadFromMatrixRegFIFODepth)
    val FromMatrixRegReadFIFOEmpty = FromMatrixRegReadFIFOHead === FromMatrixRegReadFIFOTail

    val Per_MReg_Load_Write_Memory_Time = CMatrixReg_Total_Bandwidth_Bit/outsideDataWidth
    val FireTimes = RegInit(0.U(log2Ceil(CMatrixRegNBanks).W))

    when(Write_Mem_Wait_Table.reduce(_||_)){
        io.StoreLocalMMUIO.Response.ready := true.B
    }
    when(io.StoreLocalMMUIO.Response.fire){
        val sourceId = io.StoreLocalMMUIO.Response.bits.ReseponseSourceID
        Write_Mem_Wait_Table(sourceId) := false.B
    }

    val M_Get_IteratorMax = Mux(IsStoreTranspose, (StoreMatrixRegTensor_M / (Matrix_MN.U * 2.U) + (StoreMatrixRegTensor_M % (Matrix_MN.U * 2.U) =/= 0.U)) * 2.U, (StoreMatrixRegTensor_M / Matrix_MN.U) + ((StoreMatrixRegTensor_M % Matrix_MN.U) =/= 0.U))
    val N_Get_IteratorMax = WireInit(0.U(log2Ceil(CMatrixRegBankNEntries).W))
    N_Get_IteratorMax := (StoreMatrixRegTensor_N / Matrix_MN.U)
    val transpose_scp_addr = WireInit(0.U(log2Ceil(CMatrixRegBankNEntries).W))

    // val Max_Caculate_Iter = M_Get_IteratorMax * N_Get_IteratorMax

    // val GetCount = RegInit(0.U(32.W))

    //一组reoder的寄存器，来存储重排的数据
    //存一个Matrix_M*Matrix_MN*T的数据矩阵寄存器，用于存储重排的数据，T = CMReg的总带宽/Matrix_MN*ResultWidth
    val Per_GetMatrix_NDim_Width = Matrix_MN*ResultWidth //每次Get的N连续的数据的宽度
    val Reorder_ToLLC_GroupSize = outsideDataWidth / (Per_GetMatrix_NDim_Width)//填满一个LLCDataWidth需要这么多次
    val Reorder_ToLLC_Reg = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(Matrix_MN)(VecInit(Seq.fill(Reorder_ToLLC_GroupSize)(0.U((Per_GetMatrix_NDim_Width).W))))))))
    val Reorder_ToLLC_Reg_Valid = RegInit(VecInit(Seq.fill(2)(false.B)))
    val Reorder_ToLLC_Reg_Get_Index  = RegInit(0.U(log2Ceil(2).W))//双缓冲
    val Reorder_ToLLC_Reg_Send_Index = RegInit(0.U(log2Ceil(2).W))//双缓冲

    val Fill_LLC_Iter = RegInit(0.U(log2Ceil(Reorder_ToLLC_GroupSize).W))
    val Fill_LLC_Max_Iter = Reorder_ToLLC_GroupSize

    val Send_LLC_Iter = RegInit(0.U(log2Ceil(Matrix_MN).W))
    val Send_LLC_Max_Iter = Matrix_MN

    switch(memorystore_state) {
        is(s_store_init) {
            memorystore_state := s_store_working

            Reorder_ToLLC_Reg := 0.U.asTypeOf(Reorder_ToLLC_Reg)
            Reorder_ToLLC_Reg_Valid := 0.U.asTypeOf(Reorder_ToLLC_Reg_Valid) 
            Reorder_ToLLC_Reg_Get_Index := 0.U

            Fill_LLC_Iter := 0.U

            TotalStoreSize := 0.U
            TotalStoreRequestSize := 0.U
            CurrentStore_BlockTensor_Major_DIM_Iter := 0.U
            CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
            FromMatrixRegReadFIFO := 0.U.asTypeOf(FromMatrixRegReadFIFO)
            FromMatrixRegReadFIFOHead := 0.U
            FromMatrixRegReadFIFOTail := 0.U
            Max_Load_MReg_Time := StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / CMatrixReg_Total_Bandwidth.U//总共要发对SCAP的访存次数
            Max_Store_Memory_Time := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_M) * StoreMatrixRegTensor_N * D_DataType / outsideDataWidthByte.U//总共要发对LLC的访存次数
            // MaxIncStoreScpRequestSize := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, M_Get_IteratorMax * Matrix_MN.U) * Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N) * D_DataType / CMatrixReg_Total_Bandwidth.U
            MaxIncStoreScpRequestSize := M_Get_IteratorMax * Matrix_MN.U * StoreMatrixRegTensor_N * D_DataType / CMatrixReg_Total_Bandwidth.U
            MaxIncStoreRequestSize := (Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M) / Matrix_MN.U * Matrix_MN.U) * Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N) * D_DataType / CMatrixReg_Total_Bandwidth.U
            Max_Load_Scp_Tail_SubMajor_Iter := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M) % Matrix_MN.U
            Current_Load_Scp_Tail_subMajor_Iter := 0.U
            Current_Load_Scp_addr := 0.U
            Current_Load_M_iter := 0.U
            Current_Load_N_iter := 0.U
            Per_LLC_Store_ReduceDim_Iter := Mux(D_DataType === 1.U, outsideDataWidthByte.U,
                                        Mux(D_DataType === 2.U, outsideDataWidthByte.U/2.U,
                                        Mux(D_DataType === 4.U, outsideDataWidthByte.U/4.U, outsideDataWidthByte.U)))
            Per_MReg_Load_ReduceDim_Iter := Mux(D_DataType === 1.U, CMatrixReg_Total_Bandwidth.U,
                                            Mux(D_DataType === 2.U, CMatrixReg_Total_Bandwidth.U/2.U,
                                            Mux(D_DataType === 4.U, CMatrixReg_Total_Bandwidth.U/4.U, CMatrixReg_Total_Bandwidth.U)))
            Max_SubReduce_DIM := Mux(D_DataType === 1.U, CMatrixReg_Total_Bandwidth.U,
                                    Mux(D_DataType === 2.U, CMatrixReg_Total_Bandwidth.U/2.U,
                                    Mux(D_DataType === 4.U, CMatrixReg_Total_Bandwidth.U/4.U, CMatrixReg_Total_Bandwidth.U)))
            FireTimes := 0.U
            Max_BlockTensor_Reduce_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N)
            Max_BlockTensor_Request_Reduce_DIM := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_N)
            Max_BlockTensor_Major_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M)

            if(YJPCMLDebugEnable)
            {
                printf("[CMemoryLoader_Store<%d>]Store D Tensor Start, Max_Load_MReg_Time: %x\n", io.DebugInfo.DebugTimeStampe, StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / CMatrixReg_Total_Bandwidth.U)
            }
        }
        is(s_store_working) {
            val Reorder_ToLLC_Reg_Ready_Get = !(Reorder_ToLLC_Reg_Valid.reduce(_&&_))//只要有一个不是Valid就是true,表示可以接受CDC的数据
            //如果MatrixReg的仲裁结果允许我们读取数据
            HasScarhpadRead := !FromMatrixRegReadFIFO_ISSUE_Full && !FromMatrixRegReadFIFOFull && TotalStoreRequestSize < MaxIncStoreScpRequestSize
            when(HasScarhpadRead){
                transpose_scp_addr := Current_Load_N_iter + Current_Load_M_iter * N_Get_IteratorMax
                if(YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Store<%d>]N_Get_IteratorMax: %x, Current_Load_N_iter: %x, Current_Load_M_iter: %x, transpose_scp_addr: %x\n", io.DebugInfo.DebugTimeStampe, N_Get_IteratorMax, Current_Load_N_iter, Current_Load_M_iter, transpose_scp_addr)
                }
                //根据MatrixReg的仲裁结果，我们可以读取数据了
                for (i <- 0 until CMatrixRegNBanks){
                    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(i).bits := Mux(IsStoreTranspose, transpose_scp_addr, Current_Load_Scp_addr)
                    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(i).valid := true.B
                }
                when(io.ToMatrixRegIO.StoreReadWriteResponse(MatrixRegTaskType.ReadFromMemoryLoaderIndex)){
                    TotalStoreRequestSize := TotalStoreRequestSize + 1.U
                    // logic for transpose
                    Current_Load_M_iter := Current_Load_M_iter + 1.U
                    when(Current_Load_M_iter === (M_Get_IteratorMax - 1.U)){
                        Current_Load_M_iter := 0.U
                        Current_Load_N_iter := Current_Load_N_iter + 1.U
                    }

                    Current_Load_Scp_addr := Current_Load_Scp_addr + 1.U
                }
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Store<%d>]StoreTask MReg Load Request times: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreRequestSize)
                }
            }
            
            //只要MatrixReg的数据读数有效，就可以将这个数置入fifo
            val ReadResponseData_Valid = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData.map(_.valid).reduce(_ && _)
            val ReadResponseData_Bits = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData.map(_.bits)
            when(ReadResponseData_Valid){
                FromMatrixRegReadFIFO(FromMatrixRegReadFIFOHead) := ReadResponseData_Bits.asUInt
                FromMatrixRegReadFIFOHead := WrapInc(FromMatrixRegReadFIFOHead, CMemoryLoaderReadFromMatrixRegFIFODepth)
                
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Store<%d>]FromMatrixRegReadFIFOHead: %x,FromMatrixRegReadFIFOTail: %x, data: %x\n", io.DebugInfo.DebugTimeStampe, FromMatrixRegReadFIFOHead,FromMatrixRegReadFIFOTail, ReadResponseData_Bits.asUInt)
                }
            }

            //只要fifo内的数据有效，就可以写入LLC
            val WriteRequest = io.StoreLocalMMUIO.Request
            WriteRequest.valid := false.B
            when(!FromMatrixRegReadFIFOEmpty && Reorder_ToLLC_Reg_Ready_Get){
                val Read_Data_list = WireInit(VecInit(Seq.fill(Matrix_MN)(0.U(Per_GetMatrix_NDim_Width.W))))
                Read_Data_list := FromMatrixRegReadFIFO(FromMatrixRegReadFIFOTail).asTypeOf(Read_Data_list)
                for (i <- 0 until Matrix_MN){
                    Reorder_ToLLC_Reg(Reorder_ToLLC_Reg_Get_Index)(i)(Fill_LLC_Iter) := Read_Data_list(i)
                }
                //更新相关迭代器
                Fill_LLC_Iter := WrapInc(Fill_LLC_Iter, Fill_LLC_Max_Iter)
                FromMatrixRegReadFIFOTail := WrapInc(FromMatrixRegReadFIFOTail, CMemoryLoaderReadFromMatrixRegFIFODepth)
                when(Fill_LLC_Iter === (Fill_LLC_Max_Iter - 1).U){
                    Fill_LLC_Iter := 0.U
                    Reorder_ToLLC_Reg_Valid(Reorder_ToLLC_Reg_Get_Index) := true.B
                    Reorder_ToLLC_Reg_Get_Index := WrapInc(Reorder_ToLLC_Reg_Get_Index, 2)

                    //完成一组数据，4*4数据的填充输出这一组数据
                    // if (YJPAfterOpsDebugEnable)
                    // {
                    //     val Groups_Iter = GetCount / (Matrix_MN.U)
                    //     printf("[AfterOps<%d>]AfterOps: Fill data to Reorder_ToVector_Reg, GetCount is %d(Groups %d),Fill_Reg_data is %x\n",io.DebugInfo.DebugTimeStampe, GetCount,Groups_Iter,Reorder_ToVector_Reg(Reorder_ToVector_Reg_Get_Index).asUInt)
                    // }
                }
            }
            val Reorder_ToLLC_Reg_Ready_Send= Reorder_ToLLC_Reg_Valid.reduce(_||_)//只要有一个是Valid就是ture，表示可以发往后操作执行
            when(Reorder_ToLLC_Reg_Ready_Send){

                val Request_Data = WireInit(VecInit(Seq.fill(outsideDataWidthByte / ResultWidthByte)(0.U(ResultWidth.W))))

                Request_Data := Reorder_ToLLC_Reg(Reorder_ToLLC_Reg_Send_Index)(Send_LLC_Iter).asTypeOf(Request_Data)

                val need_fill_zero = CurrentStore_BlockTensor_Reduce_DIM_Iter + Per_LLC_Store_ReduceDim_Iter > Max_BlockTensor_Reduce_DIM

                when(need_fill_zero) {
                    for (i <- 0 until outsideDataWidthByte / ResultWidthByte) {
                        when(CurrentStore_BlockTensor_Reduce_DIM_Iter + i.U >= Max_BlockTensor_Reduce_DIM) {
                            Request_Data(i) := 0.U
                        }
                    }
                }
                
                WriteRequest.bits.RequestVirtualAddr := StoreTensorBlockBaseAddr + (CurrentStore_BlockTensor_Major_DIM_Iter + CurrentStore_BlockTensor_SubMajor_DIM_Iter) * ApplicationTensor_D_Stride_M + CurrentStore_BlockTensor_Reduce_DIM_Iter * D_DataType
                WriteRequest.bits.RequestConherent := IsStoreConherent
                WriteRequest.bits.RequestSourceID := io.StoreLocalMMUIO.ConherentRequsetSourceID.bits
                WriteRequest.bits.RequestType_isWrite := true.B
                WriteRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                WriteRequest.bits.RequestData := Request_Data.asUInt
                WriteRequest.valid := true.B
                //只有fire了才能继续
                when(WriteRequest.fire && io.StoreLocalMMUIO.ConherentRequsetSourceID.valid){
                    Send_LLC_Iter := WrapInc(Send_LLC_Iter, Send_LLC_Max_Iter)
                    // if (YJPAfterOpsDebugEnable)
                    // {
                    //     printf("[AfterOps<%d>]AfterOps: Send data to Vector, Send_Vector_Iter is %d,Send_Vector_Data is %x\n",io.DebugInfo.DebugTimeStampe, Send_Vector_Iter,io.VectorInterface.VectorDataIn.bits)
                    // }
                    when(IsStoreTranspose) {
                        when(Send_LLC_Iter === (Send_LLC_Max_Iter - 1).U) {
                            Send_LLC_Iter := 0.U
                            Reorder_ToLLC_Reg_Valid(Reorder_ToLLC_Reg_Send_Index) := false.B
                            Reorder_ToLLC_Reg_Send_Index := WrapInc(Reorder_ToLLC_Reg_Send_Index, 2)
                        }

                    }.otherwise {
                        when(Send_LLC_Iter === (Send_LLC_Max_Iter - 1).U || (CurrentStore_BlockTensor_Major_DIM_Iter + CurrentStore_BlockTensor_SubMajor_DIM_Iter) === (StoreMatrixRegTensor_M - 1.U)){
                            Send_LLC_Iter := 0.U
                            Reorder_ToLLC_Reg_Valid(Reorder_ToLLC_Reg_Send_Index) := false.B
                            Reorder_ToLLC_Reg_Send_Index := WrapInc(Reorder_ToLLC_Reg_Send_Index, 2)
                            // printf("[AfterOps<%d>]AfterOps: Send Reorder Group finish, Send_Vector_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Send_Vector_Iter)
                        }
                    }

                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Store<%d>]WriteRequest: RequestVirtualAddr= %x, RequestConherent= %x,RequestSourceID= %x,RequestType_isWrite= %x,CurrentStore_BlockTensor_Major_DIM_Iter: %x, CurrentStore_BlockTensor_Reduce_DIM_Iter: %x,RequestData:%x\n", io.DebugInfo.DebugTimeStampe, WriteRequest.bits.RequestVirtualAddr, WriteRequest.bits.RequestConherent, WriteRequest.bits.RequestSourceID, WriteRequest.bits.RequestType_isWrite, CurrentStore_BlockTensor_Major_DIM_Iter, CurrentStore_BlockTensor_Reduce_DIM_Iter,WriteRequest.bits.RequestData)
                    }
                    

                    CurrentStore_BlockTensor_SubMajor_DIM_Iter := CurrentStore_BlockTensor_SubMajor_DIM_Iter + 1.U
                    // M % Matrix_M的剩余M
                    when(CurrentStore_BlockTensor_SubMajor_DIM_Iter === (Matrix_MN-1).U || (CurrentStore_BlockTensor_Major_DIM_Iter + CurrentStore_BlockTensor_SubMajor_DIM_Iter) === (Max_BlockTensor_Major_DIM - 1.U))
                    {
                        CurrentStore_BlockTensor_SubMajor_DIM_Iter := 0.U
                        CurrentStore_BlockTensor_Reduce_DIM_Iter := CurrentStore_BlockTensor_Reduce_DIM_Iter + Per_LLC_Store_ReduceDim_Iter
                        when(CurrentStore_BlockTensor_Reduce_DIM_Iter === Max_BlockTensor_Request_Reduce_DIM - Per_LLC_Store_ReduceDim_Iter){
                            CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
                            CurrentStore_BlockTensor_Major_DIM_Iter := CurrentStore_BlockTensor_Major_DIM_Iter + Matrix_MN.U
                        }
                    }

                    Write_Mem_Wait_Table(io.StoreLocalMMUIO.ConherentRequsetSourceID.bits) := true.B
                    TotalStoreSize := TotalStoreSize + 1.U
                    //输出完成的写回次数
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Store<%d>]TotalStoreSize: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreSize)
                    }
                    when(TotalStoreSize === Max_Store_Memory_Time - 1.U){
                        memorystore_state := s_store_end

                        if (YJPCMLDebugEnable)
                        {
                            printf("[CMemoryLoader_Store<%d>]StoreEnd\n",io.DebugInfo.DebugTimeStampe)
                        }
                    }
                    
                    if (YJPCMLDebugEnable)
                    {
                        printf("[CMemoryLoader_Store<%d>]CurrentStore_BlockTensor_Major_DIM_Iter: %x, CurrentStore_BlockTensor_Reduce_DIM_Iter: %x\n", io.DebugInfo.DebugTimeStampe, CurrentStore_BlockTensor_Major_DIM_Iter, CurrentStore_BlockTensor_Reduce_DIM_Iter)
                    }
                }
            }
        }
        is(s_store_end) {
            // memorystore_state := s_store_end
            when(!Write_Mem_Wait_Table.reduce(_||_)) {
                io.ConfigInfo.StoreMicroTaskEndValid := true.B
            }
            when(io.ConfigInfo.StoreMicroTaskEndReady && io.ConfigInfo.StoreMicroTaskEndValid){
                memorystore_state := s_store_idle
                if (YJPCMLDebugEnable)
                {
                    printf("[CMemoryLoader_Store<%d>]Store Finish\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }

}
