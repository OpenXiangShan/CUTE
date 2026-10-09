
package cute

import chisel3._
import chisel3.util._
import difftest._
import difftest.util.MatrixHash
import org.chipsalliance.cde.config._

// Loads matrix B into MatrixReg from the configured memory source.

// Data is primarily loaded through the external memory interface.
// The system memory unit accepts loader requests and returns data for the requested addresses.
// The local MMU translates virtual addresses and routes requests to the selected memory source.

// The loader transfers tensor tiles into MatrixReg according to its bank layout.

// Data reordering can also be performed offline by the compiler.

class BSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(ABMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(ABMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
}

// Convolution data uses [khkwoc][ic] layout; matrix multiplication uses [N][K].

class BMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // MatrixReg interface.
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new BMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })
    // Use ToMatrixRegIO as the common external interface.

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.Data.map(_.valid := false.B)
    io.ToMatrixRegIO.Data.map(_.bits := DontCare)
    io.ToMatrixRegIO.ByteMask.map(_.valid := false.B)
    io.ToMatrixRegIO.ByteMask.map(_.bits := Fill(ABMatrixRegEntryByteSize, true.B))

    // Initialize all channels, but legacy BML only uses channel 0
    for (i <- 0 until ABMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := DontCare
        io.LocalMMUIO.Response(i).ready := false.B
    }

    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    if (EnableDifftest) {
      val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = MatrixHash(ABMatrixRegNBanks, ABMatrixRegEntryByteSize, ABMatrixRegSize, Tensor_K)
        // Initialize default values.
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        difftestAmuFinish.index := 1.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        // DiffAmuFinishEvent packing is parameterized by words-per-bank.
        val eventWordsPerBank = difftestAmuFinish.data.length / ABMatrixRegNBanks
        val abMRegWordsPerBank = ABMatrixRegEntryBitSize / 64
        require(difftestAmuFinish.data.length % ABMatrixRegNBanks == 0, "DiffAmuFinishEvent.data should divide by AB bank count")
        require(ABMatrixRegEntryBitSize % 64 == 0, s"ABMatrixRegEntryBitSize must be 64-bit aligned, got $ABMatrixRegEntryBitSize")
        require(abMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuFinishEvent only supports up to $eventWordsPerBank words per AB bank, got $abMRegWordsPerBank")
        for (i <- 0 until ABMatrixRegNBanks) {
          difftestAmuFinish.bankValid(i) := io.ToMatrixRegIO.BankAddr(i).valid
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.bankMask(i) := io.ToMatrixRegIO.ByteMask(i).bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < abMRegWordsPerBank) {
              val lo = w * 64
              val hi = lo + 63
              difftestAmuFinish.data(i * eventWordsPerBank + w) := io.ToMatrixRegIO.Data(i).bits(hi, lo)
            } else {
              difftestAmuFinish.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
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
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Tensor_B_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
    val ApplicationTensor_B_Stride_N = RegInit(0.U(MMUAddrWidth.W))

    
    // Task state machine.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)


    // Memory state machine used to coordinate pipeline draining.
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) // Base address of the current matrix block.

    val Conherent = RegInit(true.B) // Coherent-access flag supplied by TaskController.

    
    // Accept a task when ConfigInfo is valid.

    when(state === s_idle){
        // Accept new configuration only while idle.
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            // A new instruction configuration was accepted.
            state := s_mm_task
            memoryload_state := s_load_init
            // ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
            MatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := io.ConfigInfo.MatrixRegId
            Tensor_B_BaseVaddr := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr // Full tensor base address.
            Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr // Base address of the current block.
            Conherent := io.ConfigInfo.Conherent
            assert(!io.ConfigInfo.Is_Transpose, "BML transpose is disabled; use the standalone transpose engine")
            HasTail := io.ConfigInfo.ApplicationTensor_B.HasTail
            TailByteMask := io.ConfigInfo.ApplicationTensor_B.TailByteMask
            K_Beat_Count := io.ConfigInfo.ApplicationTensor_B.K_Beat_Count
            ApplicationTensor_B_Stride_N := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N // Address increment for the next N coordinate.
            if(YJPBMLDebugEnable)
            {
                printf("[BML<%d>]BMemoryLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BML<%d>]MatrixRegTensor_N:%d,MatrixRegTensor_K:%d,Is_Transpose:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.MatrixRegTensor_N,io.ConfigInfo.MatrixRegTensor_K,io.ConfigInfo.Is_Transpose)
                printf("[BML<%d>]Tensor_B_BaseVaddr:%x,Tensor_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr,io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr)
                printf("[BML<%d>]ApplicationTensor_B_Stride_N:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N)
            }
        }
    }

    // Tensor virtual-address ranges are expected to be contiguous; the OS and compiler can enforce this.

    // Matrix A has already been reordered.
    // 32x32x4B, 32x128x1B, and 64x64x1B each occupy one 4 KiB page.

    // Within a page, aligned contiguous data is sufficient; layout mainly affects sequential-read performance.
    // Keeping one transfer within a page may avoid accessing multiple pages at once.
    // Scratchpad sizes: A = 64x64x256 bits = 128 KiB;
    // B = 64x64x256 bits = 128 KiB; C = 64x64x32 bits = 16 KiB.


    // MatrixReg capacity can be reduced by marking invalid data early and issuing the next request,
    // at the cost of more SRAM read/write ports than double-buffered SRAM.
    // LLC bandwidth is configured to match one MatrixReg bank entry per cycle.

    // B scratchpad data is expected to reside in LLC, so it can be loaded directly from there.
    // s_load_init initializes the state; s_load_working transfers data; s_load_end completes the task.
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W)) // Total amount of data loaded.
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_N_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, ABMatrixRegEntryByteSize)).W))


    // A CAM maps each memory request source ID to a MatrixReg address and bank.
    // Source IDs index this register table.
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new BSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(ABMatrixRegBankNEntries).W)))))//MatrixReg address for each returned LLC response.
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/ABMatrixRegEntryByteSize)+1).W)))))//Remaining writeback slices before releasing the entry.
    val MReg_Fill_Table_IsTail = RegInit(VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(false.B)))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)//True when a fill-table entry is free.
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)//True when a fill-table entry is valid.
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)//Index of the first free entry.
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)//True when an entry is available.
    val MAX_Fill_Times = outsideDataWidthByte/ABMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))))//Bank assigned to each FIFO entry.
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))//Next fill-table index to write for each bank.
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//True when a bank has data to write to scratchpad.
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//True when any bank has a pending writeback.

    for(i <- 0 until ABMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), BMemoryLoaderReadFromMemoryFIFODepth)//FIFO is full.
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//No pending write for this bank.
    }

    // Legacy BML only uses channel 0 for requests
    val Request = io.LocalMMUIO.Request(0)
    val Response = io.LocalMMUIO.Response(0)
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            CurrentLoaded_BlockTensor_N_Iter := 0.U
            CurrentLoaded_BlockTensor_K_Iter := 0.U
            Request_N_Iter_Time := 0.U
            MaxRequestIter := MatrixRegTensor_N * K_Beat_Count //Total number of memory requests.
            Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table := 0.U.asTypeOf(MReg_Fill_Table)
            MReg_Fill_Table_MReg_Addr := 0.U.asTypeOf(MReg_Fill_Table_MReg_Addr)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
            MReg_Fill_Table_IsTail := VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(false.B))
        }
        is(s_load_working) {
            io.ToMatrixRegIO.active := true.B
            //Select the access pattern based on MemoryOrder.



            //Convert the one-hot value to a prefix mask by subtracting one.
            val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
            val fullTaskMask = Fill(outsideDataWidthByte, true.B)
            val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))

            // Match AML order: issue along N across four banks, advance K, then move to the next N block.
            val RequestMatrixRegNIndex = CurrentLoaded_BlockTensor_N_Iter + Request_N_Iter_Time
            val NormalRequestMatrixRegBankId = RequestMatrixRegNIndex % ABMatrixRegNBanks.U
            val NormalRequestMatrixRegBaseAddr = ((RequestMatrixRegNIndex / ABMatrixRegNBanks.U) * ReduceGroupSize.U)
            val NormalRequestMatrixRegAddr = NormalRequestMatrixRegBaseAddr + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(MAX_Fill_Times))
            val RequestMatrixRegBankId = NormalRequestMatrixRegBankId
            val RequestMatrixRegAddr = NormalRequestMatrixRegAddr

            Request.bits.RequestAddr := Tensor_Block_BaseAddr + RequestMatrixRegNIndex * ApplicationTensor_B_Stride_N + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
            
            val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            Request.bits.RequestConherent := Conherent
            Request.bits.RequestSourceID := sourceId.bits
            Request.bits.RequestType_isWrite := false.B
            Request.bits.UseAllocatedSourceID := true.B
            Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
            Request.valid := TotalRequestSize < MaxRequestIter

            when(Request.fire && sourceId.valid){//A valid source ID means this request can be issued.
                //Request.ready means LocalMMU accepts the request; sourceId.valid means its allocated ID is valid.
                val TableItem = Wire(new BSourceIdSearch)
                TableItem.MatrixRegBankId := RequestMatrixRegBankId
                TableItem.MatrixRegAddr := RequestMatrixRegAddr
                TableItem.MatrixRegisTail := RequestBeatIsTail
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
                if (YJPBMLDebugEnable) {
                    printf("[BML_RequestHandshake<%d>] sourceId:%d, MatrixRegBankId:%d, MatrixRegAddr:%d, RequestAddr:%x, RequestConherent:%d, RequestType_isWrite:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegBankId,TableItem.MatrixRegAddr,Request.bits.RequestAddr,Request.bits.RequestConherent,Request.bits.RequestType_isWrite,RequestBeatIsTail)
                }

                Request_N_Iter_Time := Request_N_Iter_Time + 1.U
                    when(Request_N_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_N_Iter + Request_N_Iter_Time) === MatrixRegTensor_N - 1.U){
                        Request_N_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                        when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                            CurrentLoaded_BlockTensor_K_Iter := 0.U
                            CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + Matrix_MN.U
                        }
                    }
                when(TotalRequestSize =/= MaxRequestIter){
                    TotalRequestSize := TotalRequestSize + 1.U
                }
            }
            val current_fill_fifo_full = WireInit(false.B)
            when(Response.valid)
            {
                val sourceId = Response.bits.ReseponseSourceID
                val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch).MatrixRegBankId
                current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
            }
            //Handle memory responses.
            //A CAM maps request source IDs to MatrixReg addresses and banks.
            //Use the response source ID to find the fill-table entry to update.
            val normalRespReady = if (ABMLNeedMRegFillTable) {
                MReg_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
            } else {
                true.B
            }
            Response.ready := normalRespReady
            when(Response.fire){
                //The double-buffered AB register guarantees room for responses while compression runs.
                //A release design would need either double-width data to create writeback bubbles, or separate read and write ports at the existing data width.
                val sourceId = Response.bits.ReseponseSourceID
                val searchEntry = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch)
                val MatrixRegBankId = searchEntry.MatrixRegBankId
                val MatrixRegAddr = searchEntry.MatrixRegAddr
                val ResponseData = Response.bits.ReseponseData
                val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)//Fill FIFO index for this bank.

                if (YJPBMLDebugEnable) {
                    printf("[BML_ResponseHandshake<%d>] ResponseData:%x, MatrixRegBankId:%d, MatrixRegAddr:%d, SourceId:%d, FIFOIndex:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr,sourceId,FIFOIndex,searchEntry.MatrixRegisTail)
                }

                if (!ABMLNeedMRegFillTable)
                    {
                        TotalLoadSize := TotalLoadSize + 1.U
                        for (i <- 0 until ABMatrixRegNBanks)
                        {
                            when(MatrixRegBankId === i.U)
                            {
                                io.ToMatrixRegIO.BankAddr(i).bits := MatrixRegAddr
                                io.ToMatrixRegIO.Data(i).bits := ResponseData(ABMatrixRegEntryBitSize - 1, 0)
                                io.ToMatrixRegIO.BankAddr(i).valid := true.B
                                io.ToMatrixRegIO.Data(i).valid := true.B
                                io.ToMatrixRegIO.ByteMask(i).bits := Mux(searchEntry.MatrixRegisTail, tailTaskMask(ABMatrixRegEntryByteSize - 1, 0), Fill(ABMatrixRegEntryByteSize, true.B))
                                io.ToMatrixRegIO.ByteMask(i).valid := true.B
                            }
                        }
                    }

                    MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                    MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                    MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                    MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := searchEntry.MatrixRegisTail

                    Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                    Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), BMemoryLoaderReadFromMemoryFIFODepth)
                //A FIFO may be needed if this path can stall; full-throughput double buffering is expected to avoid stalls.
                //A direct MatrixReg-to-memory path could reduce latency but creates a long combinational path.
                //Alternatively, software can schedule traffic to keep the memory path stable and avoid that path.
                //MatrixReg writes have priority, so a unique write port should not block and may not need a FIFO.
                //Slow external-memory reads can wait; MatrixReg reads cannot, which motivates write priority.
                
                //Select the destination using the response ID.
                //TODO: Generalize the fixed read quantity for boundary cases.
                if (YJPBMLDebugEnable)
                {
                    //Log response details.
                    printf("[BML<%d>]ResponseData:%x,MatrixRegBankId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr)
                }
            }

            // Fill-table writeback has highest priority and runs whenever work is pending.
                val HasScarhpadWrite = Have_Bank_Fill
                val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))
                if (ABMLNeedMRegFillTable)
                {
                    for (i <- 0 until ABMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val fillSlotOH = UIntToOH(fillSlot, MAX_Fill_Times)
                            val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                            Current_Fill_MReg_Time(i) := 1.U
                            val MatrixRegWriteRequest = io.ToMatrixRegIO
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*ABMatrixRegEntryByteSize).W)))))
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            // Generic tail mask: split the outside-data tail mask into entry-sized slots
                            val tailMaskSlots = VecInit((0 until MAX_Fill_Times).map { j =>
                                tailTaskMask((j + 1) * ABMatrixRegEntryByteSize - 1, j * ABMatrixRegEntryByteSize)
                            })
                            MatrixRegWriteRequest.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + fillSlot
                            MatrixRegWriteRequest.BankAddr(i).valid := true.B
                            MatrixRegWriteRequest.Data(i).bits := FIFOData(fillSlot)
                            MatrixRegWriteRequest.Data(i).valid := true.B
                            MatrixRegWriteRequest.ByteMask(i).bits := Mux(currentIsTail, tailMaskSlots(fillSlot), Fill(ABMatrixRegEntryByteSize, true.B))
                            MatrixRegWriteRequest.ByteMask(i).valid := true.B
                            if (YJPBMLDebugEnable) {
                                printf("[BML_MRegWriteHandshake<%d>] bankid: %d, CurrentFIFOIndex: %d, ScartchPadAddr: %x, BankAddr: %x, Data: %x, ByteMask: %x\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits, MatrixRegWriteRequest.ByteMask(i).bits)
                            }

                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), BMemoryLoaderReadFromMemoryFIFODepth)
                            }

                            if (YJPBMLDebugEnable)
                            {
                                //Log fill count and FIFO index.
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
                if (YJPBMLDebugEnable)
                {
                    when(Current_Load_Fill_Size =/= 0.U)
                    {
                        printf("[BMemoryLoader_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                    }
                }
                //Advance the state machine.
                when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                    memoryload_state := s_load_end
                    if (YJPBMLDebugEnable)
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
