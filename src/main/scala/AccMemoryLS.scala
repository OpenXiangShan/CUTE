package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.SeqToAugmentedSeq
import xscache.coupledL2.prefetch.{MatrixPrefetchStream, MatrixPrefetchTagCodec}

// Legacy ACC memory path: load C data into the accumulator register.
// Load data from memory into register storage.

// Data primarily arrives through the external memory interface.
// The shared memory unit accepts loader address requests and returns their data.
// Future virtual-memory integration would need OS support for swapping, lazy allocation and copy-on-write.
// Address translation, when supported, belongs in LocalMMU; the current CUTE path uses physical addresses.

// Split each tensor into register-sized transfers and fill the register banks.

class AccLoadSourceId(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId =UInt(log2Ceil(AccMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(AccMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
}

class AccMemoryLegacy(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new AccMemoryMatrixRegIO)
        val ConfigInfo = Flipped(new AccMemoryTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val idle = Output(Bool())
    })

    io.ConfigInfo.LoadMicroTaskEndValid := false.B
    io.ConfigInfo.StoreMicroTaskEndValid := false.B
    io.ConfigInfo.LoadMicroTaskReady := false.B
    io.ConfigInfo.StoreMicroTaskReady := false.B
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest :=
        0.U.asTypeOf(io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest.foreach(_.foreach { write =>
        write.ByteMask.bits := Fill(AccMatrixRegEntryByteSize, true.B)
    })
    io.ToMatrixRegIO.LoadReadWriteRequest := 0.U
    io.ToMatrixRegIO.StoreReadWriteRequest := 0.U
    for (i <- 0 until TrMatrixRegNBanks) {
        io.LoadLocalMMUIO.Request(i).valid := false.B
        io.LoadLocalMMUIO.Request(i).bits := DontCare
        io.LoadLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LoadLocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.LoadLocalMMUIO.Request(i).bits.MatrixPrefetchTag.foreach(_ := 0.U)
        io.LoadLocalMMUIO.Request(i).bits.MatrixTraceTag.foreach(_ := 0.U)
        io.LoadLocalMMUIO.Response(i).ready := false.B
        io.StoreLocalMMUIO.Request(i).valid := false.B
        io.StoreLocalMMUIO.Request(i).bits := DontCare
        io.StoreLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.StoreLocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.StoreLocalMMUIO.Request(i).bits.MatrixPrefetchTag.foreach(_ := 0.U)
        io.StoreLocalMMUIO.Request(i).bits.MatrixTraceTag.foreach(_ := 0.U)
        io.StoreLocalMMUIO.Response(i).ready := false.B
    }

    // Legacy accMemory only uses channel 0 for load/store memory traffic.
    val ReadRequest = io.LoadLocalMMUIO.Request(0)
    val ReadResponse = io.LoadLocalMMUIO.Response(0)
    val WriteRequest = io.StoreLocalMMUIO.Request(0)
    val WriteResponse = io.StoreLocalMMUIO.Response(0)

    val CurrentLoadMatrixRegId = RegInit(0.U(AccMatrixRegIdWidth.W))
    val CurrentStoreMatrixRegId = RegInit(0.U(AccMatrixRegIdWidth.W))
    io.LoadMatrixRegId := CurrentLoadMatrixRegId
    io.StoreMatrixRegId := CurrentStoreMatrixRegId

    val LoadPcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None
    val StorePcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None

    val LoadCoreidReg = Option.when(EnableDifftest)(RegInit(0.U(8.W)))
    val StoreCoreidReg = Option.when(EnableDifftest)(RegInit(0.U(8.W)))

    // Difftest interface
    if (EnableDifftest) {
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_C_WORDS_PER_BANK", AccMatrixRegEntryBitSize / 64)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_C_REG_SIZE_BYTES", AccMatrixRegSize)

        val loadWriteAny = io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr.map(_.valid).reduce(_||_)
        val loadFinishAny = io.ConfigInfo.LoadMicroTaskEndValid && io.ConfigInfo.LoadMicroTaskEndReady
        val storeFinishAny = io.ConfigInfo.StoreMicroTaskEndValid && io.ConfigInfo.StoreMicroTaskEndReady

        val probeWrites = MatrixRegWriteProbes.genWriteProbes(MatrixRegWriteProbes.cLoadHash, ports = 2)
        val difftestLoadFinish = probeWrites(0)
        difftestLoadFinish.coreid := LoadCoreidReg.get
        difftestLoadFinish.index := 2.U
        difftestLoadFinish.valid := loadWriteAny || loadFinishAny
        difftestLoadFinish.pc := LoadPcReg.get

        val eventWordsPerBank = difftestLoadFinish.data.length / AccMatrixRegNBanks
        val cMRegWordsPerBank = AccMatrixRegEntryBitSize / 64
        require(difftestLoadFinish.data.length % AccMatrixRegNBanks == 0, "DiffAmuWritebackEvent.data should divide by C bank count")
        require(AccMatrixRegEntryBitSize % 64 == 0, s"AccMatrixRegEntryBitSize must be 64-bit aligned, got $AccMatrixRegEntryBitSize")
        require(cMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuWritebackEvent only supports up to $eventWordsPerBank words per C bank, got $cMRegWordsPerBank")
        difftestLoadFinish.bankValid := VecInit(io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr.map(_.valid)).asUInt
        for (i <- 0 until AccMatrixRegNBanks) {
          difftestLoadFinish.bankAddr(i) := io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).bits
          difftestLoadFinish.bankMask.get(i) := io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).bits
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

        val difftestLoadFinishLane1 = probeWrites(1)
        difftestLoadFinishLane1.coreid := difftestLoadFinish.coreid
        difftestLoadFinishLane1.index := 2.U
        difftestLoadFinishLane1.valid := io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest.map(_(1).BankAddr.valid).reduce(_ || _)
        difftestLoadFinishLane1.pc := LoadPcReg.get
        difftestLoadFinishLane1.bankValid := VecInit(
          io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest.map(_(1).BankAddr.valid)
        ).asUInt
        for (i <- 0 until AccMatrixRegNBanks) {
          val write = io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(i)(1)
          difftestLoadFinishLane1.bankAddr(i) := write.BankAddr.bits
          difftestLoadFinishLane1.bankMask.get(i) := write.ByteMask.bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < cMRegWordsPerBank) {
              difftestLoadFinishLane1.data(i * eventWordsPerBank + w) := write.Data.bits(w * 64 + 63, w * 64)
            } else {
              difftestLoadFinishLane1.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
        }
        difftestLoadFinishLane1.finish := false.B

        // Store path has no per-bank writeback payload, only finish handshake.
        val difftestStoreFinish = CuteDifftest(new DiffAmuFinishEvent, delay = 0, dontCare = true)
        difftestStoreFinish.coreid := StoreCoreidReg.get
        difftestStoreFinish.index := 0.U
        difftestStoreFinish.valid := storeFinishAny
        difftestStoreFinish.pc := StorePcReg.get
    }

    val LoadMatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val LoadMatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val StoreMatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val StoreMatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // Memory-read state machine, including pipeline draining.
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    // Memory-write state machine, including pipeline draining.
    val s_store_idle :: s_store_init :: s_store_working :: s_store_end :: Nil = Enum(4)
    val memorystore_state = RegInit(s_store_idle)

    val LoadTensorBlockBaseAddr = Reg(UInt(MMUAddrWidth.W))
    val StoreTensorBlockBaseAddr = Reg(UInt(MMUAddrWidth.W))

    val IsLoadConherent = RegInit(true.B)
    val IsStoreConherent = RegInit(true.B)
    val IsLoadTranspose = RegInit(false.B)
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
    val PrefetchTaskId = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.taskIdWidth.W)))
    val PrefetchStream = Option.when(EnableMatrixPrefetch)(RegInit(MatrixPrefetchStream.none))
    val StoreTraceTag = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.width.W)))

    val C_DataWidth = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val D_DataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val loadTaskIdle = memoryload_state === s_load_idle
    val storeTaskIdle = memorystore_state === s_store_idle
    io.ConfigInfo.LoadMicroTaskReady := loadTaskIdle
    io.ConfigInfo.StoreMicroTaskReady := storeTaskIdle
    io.idle := loadTaskIdle && storeTaskIdle

    when(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.LoadMicroTaskReady){
        CurrentLoadMatrixRegId := io.ConfigInfo.MatrixRegId
        LoadTensorBlockBaseAddr := io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr
        ApplicationTensor_C_Stride_M := io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M
        IsLoadConherent := io.ConfigInfo.Conherent
        IsLoadTranspose := io.ConfigInfo.Is_Transpose
        LoadMatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
        LoadMatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
        HasTail := io.ConfigInfo.ApplicationTensor_C.HasTail
        TailByteMask := io.ConfigInfo.ApplicationTensor_C.TailByteMask
        N_Beat_Count := io.ConfigInfo.ApplicationTensor_C.N_Beat_Count

        Is_ZeroLoad := io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad
        Is_FullLoad := io.ConfigInfo.LoadTaskInfo.Is_FullLoad
        Is_RepeatRowLoad := io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad
        PrefetchTaskId.zip(io.ConfigInfo.PrefetchTaskId).foreach { case (to, from) => to := from }
        PrefetchStream.zip(io.ConfigInfo.PrefetchStream).foreach { case (to, from) => to := from }
        val peDataType = new FReducePEDataType
        C_DataWidth := peDataType.CdataByteWidth(io.ConfigInfo.ApplicationTensor_C.dataType)
        memoryload_state := s_load_init
        if (EnableDifftest) {
          LoadPcReg.get := io.ConfigInfo.pc.get
          LoadCoreidReg.get := io.ConfigInfo.coreid.get
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
        StoreTraceTag.zip(io.ConfigInfo.StoreTraceTag).foreach { case (to, from) => to := from }
        memorystore_state := s_store_init
        if (EnableDifftest) {
          StorePcReg.get := io.ConfigInfo.pc.get
          StoreCoreidReg.get := io.ConfigInfo.coreid.get
        }
    }

    assert(!(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.StoreMicroTaskValid),
      "AccMemoryLegacy: split channels share one config payload, load/store valid cannot be high together")



    // Design note: contiguous tensor virtual ranges would be the responsibility of the OS and compiler.

    // Reorder accumulator data before writing it to memory.
    // Likewise, reorder memory data before writing it into the scratchpad.


    // Design alternative: release consumed data early to reduce storage, at the cost of more SRAM ports instead of double buffering.
    // LLC transfer bandwidth is matched to the per-bank entry size.

    // Read the accumulator data through LLC, where it is expected to be cached.
    // Initialize registers in s_load_init.
    // Issue and receive data in s_load_working.
    // Complete the transfer in s_load_end.
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Total number of register writes performed.
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Total number of memory requests issued.
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // Map each in-flight source ID to its register bank and address.
    // This is a register table indexed by source ID.

    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new AccLoadSourceId)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new AccLoadSourceId).getWidth.W))))

    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    // Memory-response fill FIFO.
    // The fill FIFO buffers up to AccLoader0ReadFromMemoryFIFODepth responses.
    // While a slot is available, fill it with the following steps.
    // 1. Insert the data and advance the tail.
    // 2. Initialize MReg_Fill_FIFO_Time to zero.
    // 3. Record the first destination scratchpad address in MReg_Fill_FIFO_MReg_Addr.
    // 4. Append the fill-entry index to the destination bank's Bank_Fill_Search_FIFO.
    // 5. Advance Bank_Fill_Search_FIFO_Head for that bank.
    // Refill the register banks with the following steps.
    // 1. Check each bank for pending fill data.
    // 2. Locate its fill entry and advance the fill count; the entry is consumed at MAX_Fill_Times.
    // 3. Update the FIFO, tail and metadata table.
    val MReg_Fill_Table = Reg(Vec(AccLoader0ReadFromMemoryFIFODepth, UInt(outsideDataWidth.W)))
    val MReg_Fill_Table_MReg_Addr = Reg(Vec(AccLoader0ReadFromMemoryFIFODepth, UInt(log2Ceil(AccMatrixRegBankNEntries).W))) // Scratchpad address associated with the LLC response.
    val MReg_Fill_Table_TransposeLane = Reg(Vec(AccLoader0ReadFromMemoryFIFODepth, UInt(log2Ceil(AccMatrixRegNBanks).W)))
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(AccLoader0ReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/AccMatrixRegEntryByteSize)+1).W)))))// Remaining fill cycles for this response; release the entry when finished.
    val MReg_Fill_Table_IsTail = Reg(Vec(AccLoader0ReadFromMemoryFIFODepth, Bool()))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)// Whether this FIFO entry can accept data.
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)// Whether this FIFO entry contains valid data.
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)// Index of the first free entry.
    val MReg_Fill_Table_Valid_Index = PriorityEncoder(MReg_Fill_Table_Valid)// First valid entry, used by RepeatRowLoad.
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)// Whether the FIFO has space.
    val MReg_Fill_Table_Not_Empty = MReg_Fill_Table_Valid.reduce(_ || _)// Whether the FIFO has data, used by RepeatRowLoad.
    val MReg_Fill_Table_Head = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// FIFO head pointer, used by RepeatRowLoad.
    val MReg_Fill_Table_Tail = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// FIFO tail pointer, used by RepeatRowLoad.
    val MAX_Fill_Times = outsideDataWidthByte/AccMatrixRegEntryByteSize

    val Repeat_Fill_Is_Working = RegInit(false.B)// Whether a repeated fill is active.
    val Repeat_Fill_Times = RegInit(0.U(log2Ceil(Tensor_MN).W))// Number of times this data must be written back.
    val Repeat_Fill_Group_Times = RegInit(0.U(log2Ceil(outsideDataWidthByte/AccMatrixRegEntryByteSize).W))// Number of times this data must be written back.
    val Repeat_Fill_Table_Index = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// Index of this response in the fill FIFO.
    val Repeat_Fill_Request_Infight = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// Track in-flight requests so their multi-cycle fills cannot overrun the available entries.

    val Bank_Fill_Search_FIFO = Reg(Vec(AccMatrixRegNBanks, Vec(AccLoader0ReadFromMemoryFIFODepth, UInt(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W)))) // Per-bank queue of fill-entry indices.
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(AccMatrixRegNBanks)(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W)))))// Tail of the fill-entry queue targeting scratchpad bank x.
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(AccMatrixRegNBanks)(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(AccMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(AccMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}// Whether each bank has a pending scratchpad write.
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)// Whether any bank has a pending scratchpad write.

    for(i <- 0 until AccMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), AccLoader0ReadFromMemoryFIFODepth)// The FIFO is full.
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)// This bank has no pending scratchpad write.
    }

    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(Matrix_MN).W))
    // val Fill_N_Iter_Time = RegInit(0.U(log2Ceil(Tensor_MN).W))
    // Memory-read requests.
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            CurrentLoaded_BlockTensor_M_Iter := 0.U
            CurrentLoaded_BlockTensor_N_Iter := 0.U
            MaxRequestIter := LoadMatrixRegTensor_M * N_Beat_Count // Total number of memory requests to issue.
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
            Request_M_Iter_Time := 0.U
            MReg_Fill_Table_Head := 0.U
            MReg_Fill_Table_Tail := 0.U
            Repeat_Fill_Times := 0.U
            Repeat_Fill_Group_Times := 0.U
            Repeat_Fill_Request_Infight := 0.U
            Repeat_Fill_Is_Working := false.B
        }
        is(s_load_working) {
            // Select the access traversal from MemoryOrder.
            // A ready request is accepted by LocalMMU, allowing the next request to proceed.
            // Address products can be computed earlier if multiplication timing becomes critical.
            // TODO: Account for address-generation cost when defining MemoryOrderType block traversal; address concatenation may avoid multipliers.
            // Track the state of data stored in the register.

            // Data layout in the accumulator register.
            // N precedes M; each item is one full-precision 4-byte element, unlike TR load slices.
            // N 0 1 2 3 4 5 6 7     AccMatrixRegData layout
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

            // Vector access order.
            // 01,89,gh,op,23,ab,ij,gr,45,cd,kl,st,67,ef,mn,uv: distribute fills across banks.
            // N 0 1 2 3 4 5 6 7     AccMatrixRegData layout
            // M                               {bank  [0] [1]     [2] [3] }
            // 0   0 1 2 3 4 5 6 7   |addr    0 |      0   8       g   o
            // 1   8 9 a b c d e f   |        1 |      1   9       h   p
            // 2   g h i j k l m n   |        2 |      2   a       i   q
            // 3   o p g r s t u v   |        3 |    .... .... .... .... ....
            // 4   w x y z .......   |        4 |    .... ....   .... ....
            // 5   !..............   |        5 |    .... ....   .... ....
            // 6   @..............   |        6 |    .... ....   .... ....
            // 7   #..............   |        7 |    .... ....   .... ....
            // 8   $..............   | ....................................
            //

            // At most one of these three conditions may be true.
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad, Is_RepeatRowLoad)) === 1.U,
                   "Error! Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad, Is_RepeatRowLoad should be true!")

            when(Is_FullLoad)
            {
                val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
                val fullTaskMask = Fill(outsideDataWidthByte, true.B)
                val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_N_Iter === (N_Beat_Count - 1.U))

                val RequestMatrixRegBankId = (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) % AccMatrixRegNBanks.U // Destination register bank for this request.
                val RequestMatrixRegAddr = ((CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) / AccMatrixRegNBanks.U) * (Tensor_MN.U / Matrix_MN.U) + (CurrentLoaded_BlockTensor_N_Iter << log2Ceil(MAX_Fill_Times)) // Destination register address for the first item in this memory request.
                val RequestMatrixRegTransposeBaseAddr =
                    CurrentLoaded_BlockTensor_N_Iter * (MAX_Fill_Times * (Tensor_MN / Matrix_MN)).U +
                    (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) / AccMatrixRegNBanks.U

                ReadRequest.bits.RequestAddr := LoadTensorBlockBaseAddr + (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) * ApplicationTensor_C_Stride_M + (CurrentLoaded_BlockTensor_N_Iter << log2Ceil(outsideDataWidthByte))

                // val CurrentBankID = RequestMatrixRegBankId
                // val CurrentFIFOIndex = FromMemoryLoaderReadFIFOHead

                val sourceId = Mux(IsLoadConherent,io.LoadLocalMMUIO.ConherentRequsetSourceID,io.LoadLocalMMUIO.nonConherentRequsetSourceID)


                ReadRequest.bits.RequestConherent := IsLoadConherent
                ReadRequest.bits.RequestSourceID := sourceId.bits
                ReadRequest.bits.RequestType_isWrite := false.B
                ReadRequest.bits.UseAllocatedSourceID := true.B
                ReadRequest.bits.MatrixPrefetchTag.foreach { tag =>
                  tag := MatrixPrefetchTagCodec.encode(
                    true.B,
                    PrefetchStream.get,
                    PrefetchTaskId.get
                  )
                }
                ReadRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                ReadRequest.bits.IsTranspose := false.B
                ReadRequest.valid := (TotalRequestSize < MaxRequestIter)

                // The memory request has been accepted.
                when(ReadRequest.fire){
                    val TableItem = Wire(new AccLoadSourceId)
                    TableItem.MatrixRegBankId := RequestMatrixRegBankId
                    TableItem.MatrixRegAddr := Mux(IsLoadTranspose, RequestMatrixRegTransposeBaseAddr, RequestMatrixRegAddr)
                    TableItem.MatrixRegisTail := RequestBeatIsTail
                    SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                    Request_M_Iter_Time := Request_M_Iter_Time + 1.U// Advance across consecutive banks.
                    when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === LoadMatrixRegTensor_M - 1.U){
                        Request_M_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + 1.U
                        when(CurrentLoaded_BlockTensor_N_Iter + 1.U === N_Beat_Count){
                            CurrentLoaded_BlockTensor_N_Iter := 0.U
                            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                        }
                    }
                    // Log the number of requests issued.
                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Load<%d>]RequestMatrixRegAddr: %x,RequestMatrixRegBankId: %x,CurrentLoaded_BlockTensor_N_Iter: %x,CurrentLoaded_BlockTensor_M_Iter: %x,Request_M_Iter_Time: %x,RequestAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x, RequestTimes: %d\n", io.DebugInfo.DebugTimeStampe, RequestMatrixRegAddr,RequestMatrixRegBankId,CurrentLoaded_BlockTensor_N_Iter,CurrentLoaded_BlockTensor_M_Iter,Request_M_Iter_Time,ReadRequest.bits.RequestAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite, TotalRequestSize)
                    }
                    when(TotalRequestSize === MaxRequestIter){
                        //assert!
                        //error!
                    }.otherwise{
                        TotalRequestSize := TotalRequestSize + 1.U
                    }
                }

                val current_fill_fifo_full = WireInit(false.B)
                when(ReadResponse.valid && !IsLoadTranspose)
                {
                    val sourceId = ReadResponse.bits.ReseponseSourceID
                    val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new AccLoadSourceId).MatrixRegBankId
                    current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
                }

                ReadResponse.ready := MReg_Fill_Table_Not_Full && (IsLoadTranspose || !current_fill_fifo_full)
                // Receive a memory response.
                // Map each in-flight source ID to its register bank and address.
                // Use the response source ID to find the destination register bank and address.
                when(ReadResponse.fire){
                    val sourceId = ReadResponse.bits.ReseponseSourceID
                    val searchEntry = SoureceIdSearchTable(sourceId).asTypeOf(new AccLoadSourceId)
                    val MatrixRegBankId = searchEntry.MatrixRegBankId
                    val MatrixRegAddr = searchEntry.MatrixRegAddr
                    val ResponseData = ReadResponse.bits.ReseponseData
                    val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)// Head fill-entry index for this bank; each bank has its own queue.

                    MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                    MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                    MReg_Fill_Table_TransposeLane(MReg_Fill_Table_Insert_Index) := MatrixRegBankId
                    MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                    MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := searchEntry.MatrixRegisTail

                    when(!IsLoadTranspose) {
                        Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                        Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), AccLoader0ReadFromMemoryFIFODepth)
                    }

                    // Log the data written back.
                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Load<%d>]ResponseData: %x, MatrixRegBankId: %x, MatrixRegAddr: %x, FIFOIndex: %x\n",io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr, FIFOIndex)
                    }
                }

                // Check each bank for pending fill data.
                val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(AccMatrixRegNBanks)(0.U(2.W))))
                when(IsLoadTranspose) {
                    HasScarhpadWrite := MReg_Fill_Table_Not_Empty
                    when(MReg_Fill_Table_Not_Empty && io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex)) {
                        val CurrentFIFOIndex = MReg_Fill_Table_Valid_Index
                        val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                        val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                        val transposeLane = MReg_Fill_Table_TransposeLane(CurrentFIFOIndex)
                        val matrixRegAddr = MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) +
                            fillSlot * (Tensor_MN / Matrix_MN).U
                        val responseElements = MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(
                            Vec(outsideDataWidthByte / ResultWidthByte, UInt((ResultWidthByte * 8).W)))
                        val fillPhase = if (MAX_Fill_Times == 1) 0.U(1.W)
                            else fillSlot(log2Ceil(MAX_Fill_Times) - 1, 0)
                        val elementByteMask = VecInit((0 until AccMatrixRegNBanks).map { lane =>
                            (((BigInt(1) << ResultWidthByte) - 1) << (lane * ResultWidthByte)).U(AccMatrixRegEntryByteSize.W)
                        })(transposeLane)

                        Current_Fill_MReg_Time(0) := 1.U
                        for (i <- 0 until AccMatrixRegNBanks) {
                            val sourceElement = if (MAX_Fill_Times == 1) responseElements(i) else {
                                VecInit((0 until MAX_Fill_Times).map { phase =>
                                    responseElements(phase * AccMatrixRegNBanks + i)
                                })(fillPhase)
                            }
                            val tailElementValid = if (MAX_Fill_Times == 1) {
                                tailTaskMask((i + 1) * ResultWidthByte - 1, i * ResultWidthByte).andR
                            } else {
                                VecInit((0 until MAX_Fill_Times).map { phase =>
                                    val element = phase * AccMatrixRegNBanks + i
                                    tailTaskMask((element + 1) * ResultWidthByte - 1, element * ResultWidthByte).andR
                                })(fillPhase)
                            }
                            val sourceElementValid = !currentIsTail || tailElementValid
                            val shiftedData = VecInit((0 until AccMatrixRegNBanks).map { lane =>
                                (sourceElement.pad(AccMatrixRegEntryBitSize) << (lane * ResultWidthByte * 8))(AccMatrixRegEntryBitSize - 1, 0)
                            })(transposeLane)
                            val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg

                            MatrixRegWriteRequest.BankAddr(i).bits := matrixRegAddr
                            MatrixRegWriteRequest.BankAddr(i).valid := sourceElementValid
                            MatrixRegWriteRequest.Data(i).bits := shiftedData
                            MatrixRegWriteRequest.Data(i).valid := sourceElementValid
                            MatrixRegWriteRequest.ByteMask(i).bits := elementByteMask
                            MatrixRegWriteRequest.ByteMask(i).valid := sourceElementValid
                        }

                        MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                    }
                }.otherwise {
                    HasScarhpadWrite := Have_Bank_Fill
                    for (i <- 0 until AccMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            when(io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B)
                            {
                            val rowsThisCycle = Mux(MReg_Fill_Table_Time(CurrentFIFOIndex) > 1.U, 2.U, 1.U)
                            Current_Fill_MReg_Time(i) := rowsThisCycle
                                val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg
                                val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*AccMatrixRegEntryByteSize).W)))))
                                val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                                val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                                val fullByteMask = Fill(AccMatrixRegEntryByteSize, true.B)
                                val tailByteMaskVec = Wire(Vec(MAX_Fill_Times, UInt(AccMatrixRegEntryByteSize.W)))
                                for (j <- 0 until MAX_Fill_Times) {
                                    val high = (j + 1) * AccMatrixRegEntryByteSize - 1
                                    val low = j * AccMatrixRegEntryByteSize
                                    tailByteMaskVec(j) := tailTaskMask(high, low)
                                }
                                FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            for (lane <- 0 until 2) {
                                val laneValid = MReg_Fill_Table_Time(CurrentFIFOIndex) > lane.U
                                val laneSlot = if (MAX_Fill_Times == 1) 0.U else
                                    Mux(laneValid, fillSlot + lane.U, 0.U)(log2Ceil(MAX_Fill_Times) - 1, 0)
                                val write = MatrixRegWriteRequest.WriteRequest(i)(lane)
                                write.BankAddr.bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + laneSlot
                                write.BankAddr.valid := laneValid
                                write.Data.bits := FIFOData(laneSlot)
                                write.Data.valid := laneValid
                                write.ByteMask.bits := Mux(currentIsTail, tailByteMaskVec(laneSlot), fullByteMask)
                                write.ByteMask.valid := laneValid

                            }

                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - rowsThisCycle
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) <= 2.U){
                                    Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), AccLoader0ReadFromMemoryFIFODepth)
                                }

                                if (YJPAccMemoryDebugEnable)
                                {
                                    // Log the fill count and FIFO index.
                                    printf("[AccMemory_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, MReg_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                                    printf("[AccMemory_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x, ByteMask: %x\n", io.DebugInfo.DebugTimeStampe,i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits, MatrixRegWriteRequest.ByteMask(i).bits)
                                }
                            }.otherwise
                            {
                                if (YJPAccMemoryDebugEnable)
                                {
                                    printf("[AccMemory_Load<%d>]bankid: %d no authority\n", io.DebugInfo.DebugTimeStampe,i.U)
                                }
                            }
                        }
                    }
                }

                val Current_Load_Fill_Size = WireInit(0.U(log2Ceil(2 * AccMatrixRegNBanks + 1).W))
                Current_Load_Fill_Size := Current_Fill_MReg_Time.reduce(_ +& _)

                TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size

                if (YJPAccMemoryDebugEnable)
                {
                    when(Current_Load_Fill_Size =/= 0.U)
                    {
                        printf("[AccMemory_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                    }
                }
                // Advance the state machine.
                when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                    memoryload_state := s_load_end
                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
            }

            when(Is_ZeroLoad)
            {
                // Issue zero writes to every bank.
                HasScarhpadWrite := true.B
                // Write one entry per bank each cycle for AccMatrixRegBankNEntries cycles.
                val Max_ZeroLoad_Write_Times = AccMatrixRegBankNEntries
                for (i <- 0 until AccMatrixRegNBanks)
                {
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).bits := TotalLoadSize
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(i).valid := true.B
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(i).bits := 0.U
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(i).valid := true.B
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).bits := Fill(AccMatrixRegEntryByteSize, true.B)
                    io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(i).valid := true.B
                }

                when(io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B)
                {
                    TotalLoadSize := TotalLoadSize + 1.U
                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Load<%d>]ZeroLoad, TotalLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, TotalLoadSize)
                    }
                    when(TotalLoadSize === (Max_ZeroLoad_Write_Times-1).U)
                    {
                        memoryload_state := s_load_end
                        if (YJPAccMemoryDebugEnable)
                        {
                            printf("[AccMemory_Load<%d>]ZeroLoadEnd\n",io.DebugInfo.DebugTimeStampe)
                        }
                    }
                }
            }

            // when(Is_RepeatRowLoad)
            // {
            //     // RepeatRowLoad writes each memory response many times; a full FIFO can stall TileLink for an extended interval.
            //     // Reserve a fill entry for each request to keep TileLink responses flowing; this may add latency without reducing memory throughput.
            //     // Load a complete row and repeat it; the row contains Tensor_N * C_DataWidth bytes.
            //     val sourceId = Mux(IsConherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            //     val Max_RepeatRowLoad_Memory_Load_Times = Tensor_MN.U * C_DataWidth / outsideDataWidthByte.U // Total number of memory requests to issue.
            //     val Max_MReg_Write_Times = Tensor_MN*Tensor_MN*ResultWidthByte/AccMatrixReg_Total_Bandwidth // Total number of register writes.
            //     ReadRequest.bits.RequestAddr := Tensor_Block_BaseAddr +  CurrentLoaded_BlockTensor_N_Iter * C_DataWidth
            //     ReadRequest.bits.RequestConherent := IsConherent
            //     ReadRequest.bits.RequestSourceID := sourceId.bits
            //     ReadRequest.bits.RequestType_isWrite := false.B
            //     ReadRequest.valid := (TotalRequestSize < Max_RepeatRowLoad_Memory_Load_Times) && (Repeat_Fill_Request_Infight < (AccLoader0ReadFromMemoryFIFODepth-1).U)


            //     val Per_Load_Fill_MReg_Times = (Tensor_MN/Matrix_MN) * (outsideDataWidthByte/AccMatrixRegEntryByteSize)
            //     val Per_Data_Repeat_Times = (Tensor_MN/Matrix_MN) // Number of repeated register writes for each data group.
            //     val Per_Memory_Load_Have_Data_Write_Group = (outsideDataWidthByte/AccMatrixRegEntryByteSize)// Number of writeback groups in each memory response.
            //     val Per_Write_MReg_Addr_Add = (Tensor_MN / Matrix_MN).U // Scratchpad address increment between repetitions of a data group.

            //     // val Load_Time = CurrentLoaded_BlockTensor_N_Iter / (outsideDataWidthByte.U/C_DataWidth)

            //     // Vector access order.
            //     //01,23,45,67.....
            //     // N 0 1 2 3 4 5 6 7     AccMatrixRegData layout
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
            //     // Register writeback order.
            //     // 0,16,32,48,...: advance by Per_Write_MReg_Addr_Add for Per_Data_Repeat_Times writes.
            //     // 1,17,33,49,...: advance by Per_Write_MReg_Addr_Add for Per_Data_Repeat_Times writes.
            //     // Each memory response has Per_Memory_Load_Have_Data_Write_Group groups, each repeated Per_Data_Repeat_Times.

            //     // The memory request has been accepted.
            //     when(ReadRequest.fire){

            //         val TableItem = Wire(new AccLoadSourceId)
            //         TableItem.MatrixRegBankId := 0.U
            //         TableItem.MatrixRegAddr := TotalRequestSize * Per_Memory_Load_Have_Data_Write_Group.U// Register address of the first item in this response.
            //         SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

            //         CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + outsideDataWidthByte.U / C_DataWidth
            //         Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight + 1.U
            //         if (YJPAccMemoryDebugEnable)
            //         {
            //             printf("[AccMemory_Load<%d>]RepeatRowLoad,CurrentLoaded_BlockTensor_N_Iter: %x, RequestAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_N_Iter, ReadRequest.bits.RequestAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
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
            //         MReg_Fill_Table_MReg_Addr(FIFOIndex) := SoureceIdSearchTable(sourceId).asTypeOf(new AccLoadSourceId).MatrixRegAddr
            //         MReg_Fill_Table_Time(FIFOIndex) := 1.U// Use this count as a valid marker.

            //         if (YJPAccMemoryDebugEnable)
            //         {
            //             printf("[AccMemory_Load<%d>]RepeatRowLoad, ResponseData: %x, FIFOIndex: %x\n", io.DebugInfo.DebugTimeStampe, ResponseData, FIFOIndex)
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
            //         val All_Group_Data = WireInit((VecInit(Seq.fill(Per_Memory_Load_Have_Data_Write_Group)(0.U(AccMatrixRegEntryBitSize.W)))))
            //         All_Group_Data := CurrentFIFOData.asTypeOf(All_Group_Data)
            //         val MReg_Write_Addr = MReg_Fill_Table_MReg_Addr(Repeat_Fill_Table_Index) + Repeat_Fill_Times * Per_Write_MReg_Addr_Add + Repeat_Fill_Group_Times

            //         val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg

            //         for (i <- 0 until AccMatrixRegNBanks)
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
            //             if (YJPAccMemoryDebugEnable)
            //             {
            //                 printf("[AccMemory_Load<%d>]RepeatRowLoad,Repeat_Fill_Times:  %d,TotalLoadSize:  %d,Repeat_Fill_Group_Times:  %d, MReg_Write_Addr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,Repeat_Fill_Times,TotalLoadSize,Repeat_Fill_Group_Times, MReg_Write_Addr, All_Group_Data(Repeat_Fill_Group_Times))
            //             }
            //         }
            //     }

            //     when(TotalLoadSize === (Max_MReg_Write_Times).U)
            //     {
            //         memoryload_state := s_load_end
            //         if (YJPAccMemoryDebugEnable)
            //         {
            //             printf("[AccMemory_Load<%d>]RepeatRowLoadEnd\n", io.DebugInfo.DebugTimeStampe)
            //         }
            //     }
            // }
        }
        is(s_load_end) {
            io.ConfigInfo.LoadMicroTaskEndValid := true.B
            when(io.ConfigInfo.LoadMicroTaskEndReady && io.ConfigInfo.LoadMicroTaskEndValid){
                memoryload_state := s_load_idle
                if (YJPAccMemoryDebugEnable)
                {
                    printf("[AccMemory_Load<%d>]Load Finish\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }


    // Store reads register data in Reduce_Dim-major order.
    // Memory-write requests.
    val TotalStoreSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Number of store requests already issued to LocalMMU.
    val TotalStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Number of read requests already issued to the scratchpad.
    val MaxIncStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Round Max_BlockTensor_Major_DIM down to a multiple of Matrix_M for the regular address range.
    val MaxIncStoreScpRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Round Max_BlockTensor_Major_DIM down to a multiple of Matrix_M for the regular address range.
    val Max_Load_Scp_Tail_SubMajor_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Current_Load_Scp_Tail_subMajor_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Current_Load_Scp_addr = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))
    val Current_Load_M_iter = RegInit(0.U((log2Ceil(Tensor_MN)).W)) // Current M iterator.
    val Current_Load_N_iter = RegInit(0.U((log2Ceil(Tensor_MN)).W)) // Current N iterator.
    val Max_Load_MReg_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Total number of scratchpad read requests.
    val Max_Store_Memory_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W)) // Total number of LLC write requests.
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
    val Write_Mem_Wait_Table = RegInit(VecInit(Seq.fill(SoureceMaxNum)(false.B))) // Waiting state for each outstanding memory request.

    // FIFO with AccStorer0ReadFromMatrixRegFIFODepth entries.
    val FromMatrixRegReadFIFO = Reg(Vec(AccStorer0ReadFromMatrixRegFIFODepth, UInt(AccMatrixReg_Total_Bandwidth_Bit.W)))
    val FromMatrixRegReadFIFOHead = RegInit(0.U(log2Ceil(AccStorer0ReadFromMatrixRegFIFODepth).W))
    val FromMatrixRegReadFIFOTail = RegInit(0.U(log2Ceil(AccStorer0ReadFromMatrixRegFIFODepth).W))
    val FromMatrixRegReadFIFOFull = FromMatrixRegReadFIFOTail === WrapInc(FromMatrixRegReadFIFOHead, AccStorer0ReadFromMatrixRegFIFODepth)
    val FromMatrixRegReadFIFO_ISSUE_Full = FromMatrixRegReadFIFOTail === WrapInc(WrapInc(FromMatrixRegReadFIFOHead, AccStorer0ReadFromMatrixRegFIFODepth),AccStorer0ReadFromMatrixRegFIFODepth)
    val FromMatrixRegReadFIFOEmpty = FromMatrixRegReadFIFOHead === FromMatrixRegReadFIFOTail

    val Per_MReg_Load_Write_Memory_Time = AccMatrixReg_Total_Bandwidth_Bit/outsideDataWidth
    val FireTimes = RegInit(0.U(log2Ceil(AccMatrixRegNBanks).W))

    when(Write_Mem_Wait_Table.reduce(_||_)){
        WriteResponse.ready := true.B
    }
    when(WriteResponse.fire){
        val sourceId = WriteResponse.bits.ReseponseSourceID
        Write_Mem_Wait_Table(sourceId) := false.B
        if (YJPAccMemoryDebugEnable) {
            printf("[accMemory.Store][%d] Resp.fire sourceId=%x\n", io.DebugInfo.DebugTimeStampe, sourceId)
        }
    }

    val M_Get_IteratorMax = Mux(IsStoreTranspose, (StoreMatrixRegTensor_M / (Matrix_MN.U * 2.U) + (StoreMatrixRegTensor_M % (Matrix_MN.U * 2.U) =/= 0.U)) * 2.U, (StoreMatrixRegTensor_M / Matrix_MN.U) + ((StoreMatrixRegTensor_M % Matrix_MN.U) =/= 0.U))
    val N_Get_IteratorMax = WireInit(0.U(log2Ceil(AccMatrixRegBankNEntries).W))
    N_Get_IteratorMax := (StoreMatrixRegTensor_N / Matrix_MN.U)
    val matrixRegScpAddr = WireInit(0.U(log2Ceil(AccMatrixRegBankNEntries).W))
    matrixRegScpAddr := Current_Load_M_iter * (Tensor_MN / Matrix_MN).U + Current_Load_N_iter

    // val Max_Caculate_Iter = M_Get_IteratorMax * N_Get_IteratorMax

    // val GetCount = RegInit(0.U(32.W))

    // Registers holding reordered output data.
    // Hold a Matrix_M x Matrix_MN x T block, where T is accumulator-register bandwidth divided by Matrix_MN * ResultWidth.
    val Per_GetMatrix_NDim_Width = Matrix_MN*ResultWidth // Width of the contiguous N data returned by each register read.
    val Reorder_ToLLC_GroupSize = outsideDataWidth / (Per_GetMatrix_NDim_Width)// Number of register reads needed to fill one LLC data beat.
    val Reorder_ToLLC_Reg = Reg(Vec(2, Vec(Matrix_MN, Vec(Reorder_ToLLC_GroupSize, UInt(Per_GetMatrix_NDim_Width.W)))))
    val Reorder_ToLLC_Reg_Valid = RegInit(VecInit(Seq.fill(2)(false.B)))
    val Reorder_ToLLC_Reg_Get_Index  = RegInit(0.U(log2Ceil(2).W))// Double buffering.
    val Reorder_ToLLC_Reg_Send_Index = RegInit(0.U(log2Ceil(2).W))// Double buffering.

    val Fill_LLC_Iter = RegInit(0.U(log2Ceil(Reorder_ToLLC_GroupSize).W))
    val Fill_LLC_Max_Iter = Reorder_ToLLC_GroupSize

    val Send_LLC_Iter = RegInit(0.U(log2Ceil(Matrix_MN).W))
    val Send_LLC_Max_Iter = Matrix_MN

    switch(memorystore_state) {
        is(s_store_init) {
            memorystore_state := s_store_working

            Reorder_ToLLC_Reg_Valid := 0.U.asTypeOf(Reorder_ToLLC_Reg_Valid)
            Reorder_ToLLC_Reg_Get_Index := 0.U
            Reorder_ToLLC_Reg_Send_Index := 0.U

            Fill_LLC_Iter := 0.U

            TotalStoreSize := 0.U
            TotalStoreRequestSize := 0.U
            CurrentStore_BlockTensor_Major_DIM_Iter := 0.U
            CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
            FromMatrixRegReadFIFOHead := 0.U
            FromMatrixRegReadFIFOTail := 0.U
            Max_Load_MReg_Time := StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / AccMatrixReg_Total_Bandwidth.U// Total number of scratchpad read requests.
            Max_Store_Memory_Time := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_M) * StoreMatrixRegTensor_N * D_DataType / outsideDataWidthByte.U// Total number of LLC write requests.
            // MaxIncStoreScpRequestSize := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, M_Get_IteratorMax * Matrix_MN.U) * Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N) * D_DataType / AccMatrixReg_Total_Bandwidth.U
            MaxIncStoreScpRequestSize := M_Get_IteratorMax * Matrix_MN.U * StoreMatrixRegTensor_N * D_DataType / AccMatrixReg_Total_Bandwidth.U
            MaxIncStoreRequestSize := (Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M) / Matrix_MN.U * Matrix_MN.U) * Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N) * D_DataType / AccMatrixReg_Total_Bandwidth.U
            Max_Load_Scp_Tail_SubMajor_Iter := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M) % Matrix_MN.U
            Current_Load_Scp_Tail_subMajor_Iter := 0.U
            Current_Load_Scp_addr := 0.U
            Current_Load_M_iter := 0.U
            Current_Load_N_iter := 0.U
            Per_LLC_Store_ReduceDim_Iter := Mux(D_DataType === 1.U, outsideDataWidthByte.U,
                                        Mux(D_DataType === 2.U, outsideDataWidthByte.U/2.U,
                                        Mux(D_DataType === 4.U, outsideDataWidthByte.U/4.U, outsideDataWidthByte.U)))
            Per_MReg_Load_ReduceDim_Iter := Mux(D_DataType === 1.U, AccMatrixReg_Total_Bandwidth.U,
                                            Mux(D_DataType === 2.U, AccMatrixReg_Total_Bandwidth.U/2.U,
                                            Mux(D_DataType === 4.U, AccMatrixReg_Total_Bandwidth.U/4.U, AccMatrixReg_Total_Bandwidth.U)))
            Max_SubReduce_DIM := Mux(D_DataType === 1.U, AccMatrixReg_Total_Bandwidth.U,
                                    Mux(D_DataType === 2.U, AccMatrixReg_Total_Bandwidth.U/2.U,
                                    Mux(D_DataType === 4.U, AccMatrixReg_Total_Bandwidth.U/4.U, AccMatrixReg_Total_Bandwidth.U)))
            FireTimes := 0.U
            Max_BlockTensor_Reduce_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N)
            Max_BlockTensor_Request_Reduce_DIM := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_N)
            Max_BlockTensor_Major_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_N, StoreMatrixRegTensor_M)

            if(YJPAccMemoryDebugEnable)
            {
                printf("[AccMemory_Store<%d>]Store D Tensor Start, Max_Load_MReg_Time: %x\n", io.DebugInfo.DebugTimeStampe, StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / AccMatrixReg_Total_Bandwidth.U)
            }
        }
        is(s_store_working) {
            val Reorder_ToLLC_Reg_Ready_Get = !(Reorder_ToLLC_Reg_Valid.reduce(_&&_))// Any invalid slot can accept data from the data controller.
            // Wait for the register arbiter to permit a read.
            HasScarhpadRead := !FromMatrixRegReadFIFO_ISSUE_Full && !FromMatrixRegReadFIFOFull && TotalStoreRequestSize < MaxIncStoreScpRequestSize
            when(HasScarhpadRead){
                if(YJPAccMemoryDebugEnable)
                {
                    printf("[AccMemory_Store<%d>]N_Get_IteratorMax: %x, Current_Load_N_iter: %x, Current_Load_M_iter: %x, matrixRegScpAddr: %x\n", io.DebugInfo.DebugTimeStampe, N_Get_IteratorMax, Current_Load_N_iter, Current_Load_M_iter, matrixRegScpAddr)
                }
                // The register arbiter permits this read.
                for (i <- 0 until AccMatrixRegNBanks){
                    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(i).bits := matrixRegScpAddr
                    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(i).valid := true.B
                }
                when(io.ToMatrixRegIO.StoreReadWriteResponse(MatrixRegTaskType.ReadFromMemoryLoaderIndex)){
                    TotalStoreRequestSize := TotalStoreRequestSize + 1.U
                    when(IsStoreTranspose) {
                        Current_Load_M_iter := Current_Load_M_iter + 1.U
                        when(Current_Load_M_iter === (M_Get_IteratorMax - 1.U)) {
                            Current_Load_M_iter := 0.U
                            Current_Load_N_iter := Current_Load_N_iter + 1.U
                        }
                    }.otherwise {
                        Current_Load_N_iter := Current_Load_N_iter + 1.U
                        when(Current_Load_N_iter === (N_Get_IteratorMax - 1.U)) {
                            Current_Load_N_iter := 0.U
                            Current_Load_M_iter := Current_Load_M_iter + 1.U
                        }
                    }

                    Current_Load_Scp_addr := Current_Load_Scp_addr + 1.U
                }
                if (YJPAccMemoryDebugEnable)
                {
                    printf("[AccMemory_Store<%d>]StoreTask MReg Load Request times: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreRequestSize)
                }
            }

            // Enqueue valid register-read data into the FIFO.
            val ReadResponseData_Valid = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData.map(_.valid).reduce(_ && _)
            val ReadResponseData_Bits = io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData.map(_.bits)
            when(ReadResponseData_Valid){
                FromMatrixRegReadFIFO(FromMatrixRegReadFIFOHead) := ReadResponseData_Bits.asUInt
                FromMatrixRegReadFIFOHead := WrapInc(FromMatrixRegReadFIFOHead, AccStorer0ReadFromMatrixRegFIFODepth)

                if (YJPAccMemoryDebugEnable)
                {
                    printf("[AccMemory_Store<%d>]FromMatrixRegReadFIFOHead: %x,FromMatrixRegReadFIFOTail: %x, data: %x\n", io.DebugInfo.DebugTimeStampe, FromMatrixRegReadFIFOHead,FromMatrixRegReadFIFOTail, ReadResponseData_Bits.asUInt)
                }
            }

            // Valid FIFO data can be written to LLC.
            WriteRequest.valid := false.B
            when(!FromMatrixRegReadFIFOEmpty && Reorder_ToLLC_Reg_Ready_Get){
                val Read_Data_matrix = Wire(Vec(Matrix_MN, Vec(Matrix_MN, UInt(ResultWidth.W))))
                Read_Data_matrix := FromMatrixRegReadFIFO(FromMatrixRegReadFIFOTail).asTypeOf(Read_Data_matrix)
                for (i <- 0 until Matrix_MN){
                    val Transposed_Block_Row = VecInit((0 until Matrix_MN).map(j => Read_Data_matrix(j)(i))).asUInt
                    Reorder_ToLLC_Reg(Reorder_ToLLC_Reg_Get_Index)(i)(Fill_LLC_Iter) :=
                        Mux(IsStoreTranspose, Transposed_Block_Row, Read_Data_matrix(i).asUInt)
                }
                // Advance the associated iterators.
                Fill_LLC_Iter := WrapInc(Fill_LLC_Iter, Fill_LLC_Max_Iter)
                FromMatrixRegReadFIFOTail := WrapInc(FromMatrixRegReadFIFOTail, AccStorer0ReadFromMatrixRegFIFODepth)
                when(Fill_LLC_Iter === (Fill_LLC_Max_Iter - 1).U){
                    Fill_LLC_Iter := 0.U
                    Reorder_ToLLC_Reg_Valid(Reorder_ToLLC_Reg_Get_Index) := true.B
                    Reorder_ToLLC_Reg_Get_Index := WrapInc(Reorder_ToLLC_Reg_Get_Index, 2)

                    // A complete 4 x 4 group is ready for output.
                    // if (YJPAfterOpsDebugEnable)
                    // {
                    //     val Groups_Iter = GetCount / (Matrix_MN.U)
                    //     printf("[AfterOps<%d>]AfterOps: Fill data to Reorder_ToVector_Reg, GetCount is %d(Groups %d),Fill_Reg_data is %x\n",io.DebugInfo.DebugTimeStampe, GetCount,Groups_Iter,Reorder_ToVector_Reg(Reorder_ToVector_Reg_Get_Index).asUInt)
                    // }
                }
            }
            val Reorder_ToLLC_Reg_Ready_Send= Reorder_ToLLC_Reg_Valid.reduce(_||_)// Any valid slot can be sent to the next stage.
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

                WriteRequest.bits.RequestAddr := StoreTensorBlockBaseAddr + (CurrentStore_BlockTensor_Major_DIM_Iter + CurrentStore_BlockTensor_SubMajor_DIM_Iter) * ApplicationTensor_D_Stride_M + CurrentStore_BlockTensor_Reduce_DIM_Iter * D_DataType
                WriteRequest.bits.RequestConherent := IsStoreConherent
                WriteRequest.bits.RequestSourceID := io.StoreLocalMMUIO.ConherentRequsetSourceID.bits
                WriteRequest.bits.RequestType_isWrite := true.B
                WriteRequest.bits.UseAllocatedSourceID := true.B
                // Keep the actual L2 tag invalid: C stores must not become
                // matrix-prefetch demand events. MatrixTraceTag is debug-only.
                WriteRequest.bits.MatrixPrefetchTag.foreach(_ := 0.U)
                WriteRequest.bits.MatrixTraceTag.zip(StoreTraceTag).foreach { case (to, from) => to := from }
                WriteRequest.bits.RequestData := Request_Data.asUInt
                WriteRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                WriteRequest.bits.IsTranspose := false.B
                WriteRequest.valid := true.B
                // Advance only after the request fires.
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

                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[accMemory.Store][%d] Req.fire addr=%x, coherent=%x, sourceId=%x, isWrite=%x, majorIter=%x, reduceIter=%x, data=%x\n", io.DebugInfo.DebugTimeStampe, WriteRequest.bits.RequestAddr, WriteRequest.bits.RequestConherent, WriteRequest.bits.RequestSourceID, WriteRequest.bits.RequestType_isWrite, CurrentStore_BlockTensor_Major_DIM_Iter, CurrentStore_BlockTensor_Reduce_DIM_Iter, WriteRequest.bits.RequestData)
                    }


                    CurrentStore_BlockTensor_SubMajor_DIM_Iter := CurrentStore_BlockTensor_SubMajor_DIM_Iter + 1.U
                    // Remaining M elements modulo Matrix_M.
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
                    // Log the number of completed writebacks.
                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Store<%d>]TotalStoreSize: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreSize)
                    }
                    when(TotalStoreSize === Max_Store_Memory_Time - 1.U){
                        memorystore_state := s_store_end

                        if (YJPAccMemoryDebugEnable)
                        {
                            printf("[AccMemory_Store<%d>]StoreEnd\n",io.DebugInfo.DebugTimeStampe)
                        }
                    }

                    if (YJPAccMemoryDebugEnable)
                    {
                        printf("[AccMemory_Store<%d>]CurrentStore_BlockTensor_Major_DIM_Iter: %x, CurrentStore_BlockTensor_Reduce_DIM_Iter: %x\n", io.DebugInfo.DebugTimeStampe, CurrentStore_BlockTensor_Major_DIM_Iter, CurrentStore_BlockTensor_Reduce_DIM_Iter)
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
                if (YJPAccMemoryDebugEnable)
                {
                    printf("[AccMemory_Store<%d>]Store Finish\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }

}
