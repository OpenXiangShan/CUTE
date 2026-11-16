
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import xscache.coupledL2.prefetch.{MatrixPrefetchStream, MatrixPrefetchTagCodec}

// Legacy TR load kernels and the independent TR store path.
// Loader 0 retains its row-first traversal; loader 1 retains its reduce-first traversal.

class TrLoader0SourceId(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(TrMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(TrMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
}

class TrLoader0Legacy(emitDifftest: Boolean = true)(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
    })

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.WriteRequest := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequest)
    io.ToMatrixRegIO.WriteRequest.foreach(_.foreach { write =>
        write.ByteMask.bits := Fill(TrMatrixRegEntryByteSize, true.B)
    })

    // Initialize all LocalMMUIO channels; the legacy trLoader0 only drives channel 0.
    for (i <- 0 until TrMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := DontCare
        io.LocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.LocalMMUIO.Request(i).bits.MatrixPrefetchTag.foreach(_ := 0.U)
        io.LocalMMUIO.Response(i).ready := false.B
    }
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    // Legacy trLoader0 only uses channel 0 for requests and responses.
    val Request = io.LocalMMUIO.Request(0)
    val Response = io.LocalMMUIO.Response(0)

    val CurrentMatrixRegId = RegInit(0.U(TrMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    dontTouch(io)

    if (EnableDifftest && emitDifftest) {
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_WORDS_PER_BANK", TrMatrixRegEntryBitSize / 64)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_REG_SIZE_BYTES", TrMatrixRegSize)
        val pcReg = RegInit(0.U(64.W))
        val coreidReg = RegInit(0.U(8.W))
        when (io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
          pcReg := io.ConfigInfo.pc.get
          coreidReg := io.ConfigInfo.coreid.get
        }
        val probeWrites = MatrixRegWriteProbes.genWriteProbes(MatrixRegWriteProbes.abLoadHash, ports = 2)
        val difftestAmuFinish = probeWrites(0)
        difftestAmuFinish.coreid := coreidReg
        difftestAmuFinish.index := 0.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        // DiffAmuWritebackEvent packing is parameterized by words-per-bank.
        val eventWordsPerBank = difftestAmuFinish.data.length / TrMatrixRegNBanks
        val abMRegWordsPerBank = TrMatrixRegEntryBitSize / 64
        require(difftestAmuFinish.data.length % TrMatrixRegNBanks == 0, "DiffAmuWritebackEvent.data should divide by AB bank count")
        require(TrMatrixRegEntryBitSize % 64 == 0, s"TrMatrixRegEntryBitSize must be 64-bit aligned, got $TrMatrixRegEntryBitSize")
        require(abMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuWritebackEvent only supports up to $eventWordsPerBank words per AB bank, got $abMRegWordsPerBank")
        difftestAmuFinish.bankValid := VecInit(io.ToMatrixRegIO.BankAddr.map(_.valid)).asUInt
        for (i <- 0 until TrMatrixRegNBanks) {
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.bankMask.get(i) := io.ToMatrixRegIO.ByteMask(i).bits
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

        val difftestAmuFinishLane1 = probeWrites(1)
        difftestAmuFinishLane1.coreid := difftestAmuFinish.coreid
        difftestAmuFinishLane1.index := 0.U
        difftestAmuFinishLane1.valid := io.ToMatrixRegIO.WriteRequest.map(_(1).BankAddr.valid).reduce(_ || _)
        difftestAmuFinishLane1.pc := pcReg
        difftestAmuFinishLane1.bankValid := VecInit(io.ToMatrixRegIO.WriteRequest.map(_(1).BankAddr.valid)).asUInt
        for (i <- 0 until TrMatrixRegNBanks) {
          val write = io.ToMatrixRegIO.WriteRequest(i)(1)
          difftestAmuFinishLane1.bankAddr(i) := write.BankAddr.bits
          difftestAmuFinishLane1.bankMask.get(i) := write.ByteMask.bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < abMRegWordsPerBank) {
              difftestAmuFinishLane1.data(i * eventWordsPerBank + w) := write.Data.bits(w * 64 + 63, w * 64)
            } else {
              difftestAmuFinishLane1.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
        }
        difftestAmuFinishLane1.finish := false.B
    }

    val ConfigInfo = io.ConfigInfo
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W))
    val stride = RegInit(0.U(MMUAddrWidth.W))
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Conherent = RegInit(true.B)
    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)
    val PrefetchTaskId = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.taskIdWidth.W)))
    val PrefetchStream = Option.when(EnableMatrixPrefetch)(RegInit(MatrixPrefetchStream.none))

    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, TrMatrixRegEntryByteSize)).W))

    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new TrLoader0SourceId).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = Reg(Vec(TrLoader0ReadFromMemoryFIFODepth, UInt(outsideDataWidth.W)))
    val MReg_Fill_Table_MReg_Addr = Reg(Vec(TrLoader0ReadFromMemoryFIFODepth, UInt(log2Ceil(TrMatrixRegBankNEntries).W)))
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(TrLoader0ReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/TrMatrixRegEntryByteSize)+1).W)))))
    val MReg_Fill_Table_IsTail = Reg(Vec(TrLoader0ReadFromMemoryFIFODepth, Bool()))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)
    val MAX_Fill_Times = outsideDataWidthByte/TrMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = Reg(Vec(TrMatrixRegNBanks, Vec(TrLoader0ReadFromMemoryFIFODepth, UInt(log2Ceil(TrLoader0ReadFromMemoryFIFODepth).W))))
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(log2Ceil(TrLoader0ReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(log2Ceil(TrLoader0ReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(true.B)))

    for(i <- 0 until TrMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), TrLoader0ReadFromMemoryFIFODepth)
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)
    }

    when(state === s_idle){
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            state := s_mm_task
            memoryload_state := s_load_init
            MatrixRegTensor_M := ConfigInfo.MatrixRegTensor_M
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            Tensor_Block_BaseAddr := ConfigInfo.Tensor.baseVaddr
            stride := ConfigInfo.Tensor.stride
            HasTail := ConfigInfo.Tensor.HasTail
            TailByteMask := ConfigInfo.Tensor.TailByteMask
            K_Beat_Count := ConfigInfo.Tensor.K_Beat_Count
            Is_ZeroLoad := ConfigInfo.LoadTaskInfo.Is_ZeroLoad
            Is_FullLoad := ConfigInfo.LoadTaskInfo.Is_FullLoad
            PrefetchTaskId.zip(ConfigInfo.PrefetchTaskId).foreach { case (to, from) => to := from }
            PrefetchStream.zip(ConfigInfo.PrefetchStream).foreach { case (to, from) => to := from }
            Conherent := ConfigInfo.Conherent
            assert(!ConfigInfo.Is_Transpose, "trLoader0 transpose is disabled; use the standalone transpose engine")
            if(YJPTrLoader0DebugEnable){
                printf("[trLoader0<%d>]TrLoader0Legacy Task Start, MatrixRegTensor_M:%d, MatrixRegTensor_K:%d, BaseVaddr:%x, Stride_M:%x, Is_ZeroLoad:%d, Is_FullLoad:%d\n",
                  io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_K,
                  ConfigInfo.Tensor.baseVaddr, ConfigInfo.Tensor.stride,
                  ConfigInfo.LoadTaskInfo.Is_ZeroLoad.asUInt, ConfigInfo.LoadTaskInfo.Is_FullLoad.asUInt)
            }
        }
    }

    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            CurrentLoaded_BlockTensor_M_Iter := 0.U
            CurrentLoaded_BlockTensor_K_Iter := 0.U
            Request_M_Iter_Time := 0.U
            MaxRequestIter := MatrixRegTensor_M * K_Beat_Count
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
        }
        is(s_load_working) {
            io.ToMatrixRegIO.active := true.B
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad)) === 1.U,
                  "Error! trLoader0 Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad should be true!")


            when(Is_ZeroLoad){
                val Max_ZeroLoad_Write_Times = TrMatrixRegBankNEntries
                for (i <- 0 until TrMatrixRegNBanks){
                    io.ToMatrixRegIO.BankAddr(i).bits := TotalLoadSize
                    io.ToMatrixRegIO.BankAddr(i).valid := true.B
                    io.ToMatrixRegIO.Data(i).bits := 0.U
                    io.ToMatrixRegIO.Data(i).valid := true.B
                    io.ToMatrixRegIO.ByteMask(i).bits := Fill(TrMatrixRegEntryByteSize, true.B)
                    io.ToMatrixRegIO.ByteMask(i).valid := true.B
                }
                TotalLoadSize := TotalLoadSize + 1.U
                if (YJPTrLoader0DebugEnable) printf("[trLoader0<%d>]ZeroLoad, TotalLoadSize: %d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize)
                when(TotalLoadSize === (Max_ZeroLoad_Write_Times - 1).U){
                    memoryload_state := s_load_end
                    if (YJPTrLoader0DebugEnable) printf("[trLoader0<%d>]ZeroLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                }
            }

            when(Is_FullLoad){
                // Convert the one-hot value to a prefix mask by subtracting one.
                val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
                val fullTaskMask = Fill(outsideDataWidthByte, true.B)
                val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))

                // Interleave M across banks, then traverse K. Address = BaseAddr + M*Stride_M + K*64B.
                val RequestMatrixRegMIndex = CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time
                val NormalRequestMatrixRegBankId = RequestMatrixRegMIndex % TrMatrixRegNBanks.U
                val NormalRequestMatrixRegBaseAddr = ((RequestMatrixRegMIndex / TrMatrixRegNBanks.U) * ReduceGroupSize.U)
                val NormalRequestMatrixRegAddr = NormalRequestMatrixRegBaseAddr + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(MAX_Fill_Times))
                val RequestMatrixRegBankId = NormalRequestMatrixRegBankId
                val RequestMatrixRegAddr = NormalRequestMatrixRegAddr

                Request.bits.RequestAddr := Tensor_Block_BaseAddr + RequestMatrixRegMIndex * stride + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
                val sourceId = Mux(Conherent, io.LocalMMUIO.ConherentRequsetSourceID, io.LocalMMUIO.nonConherentRequsetSourceID)
                Request.bits.RequestConherent := Conherent
                Request.bits.RequestSourceID := sourceId.bits
                Request.bits.RequestType_isWrite := false.B
                Request.bits.UseAllocatedSourceID := true.B
                Request.bits.MatrixPrefetchTag.foreach { tag =>
                  tag := MatrixPrefetchTagCodec.encode(
                    true.B,
                    PrefetchStream.get,
                    PrefetchTaskId.get
                  )
                }
                Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                Request.bits.IsTranspose := false.B
                Request.valid := TotalRequestSize < MaxRequestIter

                when(Request.fire){
                    val TableItem = Wire(new TrLoader0SourceId)
                    TableItem.MatrixRegBankId := RequestMatrixRegBankId
                    TableItem.MatrixRegAddr := RequestMatrixRegAddr
                    TableItem.MatrixRegisTail := RequestBeatIsTail
                    SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                    if (YJPTrLoader0DebugEnable) {
                        printf("[TrLoader0_RequestHandshake<%d>] M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d, SourceId:%d, Tail:%d\n",
                          io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_M_Iter_Time, Request.bits.RequestAddr, RequestMatrixRegBankId, RequestMatrixRegAddr, sourceId.bits, RequestBeatIsTail)
                    }

                    Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                        when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === MatrixRegTensor_M - 1.U){
                            Request_M_Iter_Time := 0.U
                            CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                            when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                                CurrentLoaded_BlockTensor_K_Iter := 0.U
                                CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                            }
                        }
                    when(TotalRequestSize =/= MaxRequestIter){
                        TotalRequestSize := TotalRequestSize + 1.U
                    }
                    if (YJPTrLoader0DebugEnable){
                        printf("[trLoader0<%d>]FullLoad Request, M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d\n",
                          io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_M_Iter_Time, Request.bits.RequestAddr, RequestMatrixRegBankId, RequestMatrixRegAddr)
                    }
                }

                val current_fill_fifo_full = WireInit(false.B)
                when(Response.valid){
                    val respSourceId = Response.bits.ReseponseSourceID
                    val MatrixRegBankId = SoureceIdSearchTable(respSourceId).asTypeOf(new TrLoader0SourceId).MatrixRegBankId
                    current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
                }
                Response.ready := MReg_Fill_Table_Not_Full && !current_fill_fifo_full

                when(Response.fire){
                    val sourceId = Response.bits.ReseponseSourceID
                    val MatrixRegSearch = SoureceIdSearchTable(sourceId).asTypeOf(new TrLoader0SourceId)
                    val MatrixRegBankId = MatrixRegSearch.MatrixRegBankId
                    val MatrixRegAddr = MatrixRegSearch.MatrixRegAddr
                    val ResponseData = Response.bits.ReseponseData
                    val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)

                    if (YJPTrLoader0DebugEnable) {
                        printf("[TrLoader0_ResponseHandshake<%d>] Data:%x, BankId:%d, RegAddr:%d, SourceId:%d, FIFOIndex:%d, Tail:%d\n", io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr, sourceId, FIFOIndex, MatrixRegSearch.MatrixRegisTail)
                    }

                    MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                        MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                        MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                        MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := MatrixRegSearch.MatrixRegisTail
                        Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                        Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), TrLoader0ReadFromMemoryFIFODepth)
                    if (YJPTrLoader0DebugEnable){
                        printf("[trLoader0<%d>]Response, Data:%x, BankId:%d, RegAddr:%d\n", io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr)
                    }
                }

                val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(2.W))))
                    // Generic per-slot tail mask derived from tailTaskMask.
                    val tailByteMaskPerSlot = VecInit((0 until MAX_Fill_Times).map { j =>
                        tailTaskMask((j + 1) * TrMatrixRegEntryByteSize - 1, j * TrMatrixRegEntryByteSize)
                    })
                    for (i <- 0 until TrMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                            val rowsThisCycle = Mux(MReg_Fill_Table_Time(CurrentFIFOIndex) > 1.U, 2.U, 1.U)
                            Current_Fill_MReg_Time(i) := rowsThisCycle
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*TrMatrixRegEntryByteSize).W)))))
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            for (lane <- 0 until 2) {
                                val laneValid = MReg_Fill_Table_Time(CurrentFIFOIndex) > lane.U
                                val laneSlot = if (MAX_Fill_Times == 1) 0.U else
                                    Mux(laneValid, fillSlot + lane.U, 0.U)(log2Ceil(MAX_Fill_Times) - 1, 0)
                                val write = io.ToMatrixRegIO.WriteRequest(i)(lane)
                                write.BankAddr.bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + laneSlot
                                write.BankAddr.valid := laneValid
                                write.Data.bits := FIFOData(laneSlot)
                                write.Data.valid := laneValid
                                write.ByteMask.bits := Mux(currentIsTail, tailByteMaskPerSlot(laneSlot), Fill(TrMatrixRegEntryByteSize, true.B))
                                write.ByteMask.valid := laneValid

                            }
                            if (YJPTrLoader0DebugEnable) {
                                printf("[TrLoader0_MRegWriteHandshake<%d>] bank:%d, RegAddr:%x, WriteAddr:%x, Data:%x, ByteMask:%x, Time:%d\n", io.DebugInfo.DebugTimeStampe, i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), io.ToMatrixRegIO.BankAddr(i).bits, io.ToMatrixRegIO.Data(i).bits, io.ToMatrixRegIO.ByteMask(i).bits, MReg_Fill_Table_Time(CurrentFIFOIndex))
                            }
                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - rowsThisCycle
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) <= 2.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), TrLoader0ReadFromMemoryFIFODepth)
                            }
                            if (YJPTrLoader0DebugEnable){
                                printf("[trLoader0<%d>]Fill bank:%d, RegAddr:%x, Time:%d\n", io.DebugInfo.DebugTimeStampe, i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                            }
                        }
                    }
                    val Current_Load_Fill_Size = Current_Fill_MReg_Time.reduce(_ +& _)
                    TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
                    if(YJPTrLoader0DebugEnable){
                        when(Current_Load_Fill_Size =/= 0.U) {
                            printf("[trLoader0<%d>]TotalLoadSize:%d, FillSize:%d, Max:%d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize, Current_Load_Fill_Size, MaxRequestIter * MAX_Fill_Times.U)
                        }
                    }
                    when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                        memoryload_state := s_load_end
                        if (YJPTrLoader0DebugEnable) printf("[trLoader0<%d>]FullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                    }
            }
        }
        is(s_load_end) {
            ConfigInfo.MicroTaskEndValid := true.B
            when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
                memoryload_state := s_load_idle
                state := s_idle
                if(YJPTrLoader0DebugEnable) printf("[trLoader0<%d>]TrLoader0Legacy Task End\n", io.DebugInfo.DebugTimeStampe)
            }
        }
    }
}

// Legacy TR loader 1: ordinary A/B loads share the normalized row/beat descriptor.

// Data is primarily loaded through the external memory interface.
// The system memory unit accepts loader requests and returns data for the requested addresses.
// The current path uses physical addresses and routes requests through LocalMMU.

// The loader transfers tensor tiles into MatrixReg according to its bank layout.

// Data reordering can also be performed offline by the compiler.

class TrLoader1SourceId(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(TrMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(TrMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
}

// Ordinary A/B tasks both use normalized register rows and contiguous reduce bytes.

class TrLoader1Legacy(implicit p: Parameters) extends CuteModule{

    val io = IO(new Bundle{
        // MatrixReg interface.
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
    })
    // Use ToMatrixRegIO as the common external interface.

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.WriteRequest := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequest)
    io.ToMatrixRegIO.WriteRequest.foreach(_.foreach { write =>
        write.ByteMask.bits := Fill(TrMatrixRegEntryByteSize, true.B)
    })

    // Initialize all channels, but legacy trLoader1 only uses channel 0
    for (i <- 0 until TrMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := DontCare
        io.LocalMMUIO.Request(i).bits.MatrixPrefetchTag.foreach(_ := 0.U)
        io.LocalMMUIO.Response(i).ready := false.B
    }

    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    if (EnableDifftest) {
        val pcReg = RegInit(0.U(64.W))
        val coreidReg = RegInit(0.U(8.W))
        when (io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
          pcReg := io.ConfigInfo.pc.get
          coreidReg := io.ConfigInfo.coreid.get
        }
        val probeWrites = MatrixRegWriteProbes.genWriteProbes(MatrixRegWriteProbes.abLoadHash, ports = 2)
        val difftestAmuFinish = probeWrites(0)
        // Initialize default values.
        difftestAmuFinish.coreid := coreidReg
        difftestAmuFinish.index := 1.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        // DiffAmuWritebackEvent packing is parameterized by words-per-bank.
        val eventWordsPerBank = difftestAmuFinish.data.length / TrMatrixRegNBanks
        val abMRegWordsPerBank = TrMatrixRegEntryBitSize / 64
        require(difftestAmuFinish.data.length % TrMatrixRegNBanks == 0, "DiffAmuWritebackEvent.data should divide by AB bank count")
        require(TrMatrixRegEntryBitSize % 64 == 0, s"TrMatrixRegEntryBitSize must be 64-bit aligned, got $TrMatrixRegEntryBitSize")
        require(abMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuWritebackEvent only supports up to $eventWordsPerBank words per AB bank, got $abMRegWordsPerBank")
        difftestAmuFinish.bankValid := VecInit(io.ToMatrixRegIO.BankAddr.map(_.valid)).asUInt
        for (i <- 0 until TrMatrixRegNBanks) {
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.bankMask.get(i) := io.ToMatrixRegIO.ByteMask(i).bits
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

        val difftestAmuFinishLane1 = probeWrites(1)
        difftestAmuFinishLane1.coreid := difftestAmuFinish.coreid
        difftestAmuFinishLane1.index := 1.U
        difftestAmuFinishLane1.valid := io.ToMatrixRegIO.WriteRequest.map(_(1).BankAddr.valid).reduce(_ || _)
        difftestAmuFinishLane1.pc := pcReg
        difftestAmuFinishLane1.bankValid := VecInit(io.ToMatrixRegIO.WriteRequest.map(_(1).BankAddr.valid)).asUInt
        for (i <- 0 until TrMatrixRegNBanks) {
          val write = io.ToMatrixRegIO.WriteRequest(i)(1)
          difftestAmuFinishLane1.bankAddr(i) := write.BankAddr.bits
          difftestAmuFinishLane1.bankMask.get(i) := write.ByteMask.bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < abMRegWordsPerBank) {
              difftestAmuFinishLane1.data(i * eventWordsPerBank + w) := write.Data.bits(w * 64 + 63, w * 64)
            } else {
              difftestAmuFinishLane1.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
        }
        difftestAmuFinishLane1.finish := false.B
    }

    val MatrixRegBankAddr = io.ToMatrixRegIO.BankAddr
    val MatrixRegData = io.ToMatrixRegIO.Data


    val ConfigInfo = io.ConfigInfo
    val PrefetchTaskId = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.taskIdWidth.W)))
    val PrefetchStream = Option.when(EnableMatrixPrefetch)(RegInit(MatrixPrefetchStream.none))
    val CurrentMatrixRegId = RegInit(0.U(TrMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Tensor_BaseAddr = RegInit(0.U(MMUAddrWidth.W))
    val stride = RegInit(0.U(MMUAddrWidth.W))


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
            MatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := io.ConfigInfo.MatrixRegId
            PrefetchTaskId.zip(ConfigInfo.PrefetchTaskId).foreach { case (to, from) => to := from }
            PrefetchStream.zip(ConfigInfo.PrefetchStream).foreach { case (to, from) => to := from }
            Tensor_BaseAddr := io.ConfigInfo.Tensor.baseVaddr // Full tensor base address.
            Tensor_Block_BaseAddr := io.ConfigInfo.Tensor.baseVaddr // Base address of the current block.
            Conherent := io.ConfigInfo.Conherent
            assert(!io.ConfigInfo.Is_Transpose, "trLoader1 transpose is disabled; use the standalone transpose engine")
            HasTail := io.ConfigInfo.Tensor.HasTail
            TailByteMask := io.ConfigInfo.Tensor.TailByteMask
            K_Beat_Count := io.ConfigInfo.Tensor.K_Beat_Count
            stride := io.ConfigInfo.Tensor.stride // Address increment between normalized register rows.
            if(YJPTrLoader1DebugEnable)
            {
                printf("[trLoader1<%d>]TrLoader1Legacy Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[trLoader1<%d>]MatrixRegTensor_M:%d,MatrixRegTensor_K:%d,Is_Transpose:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.MatrixRegTensor_M,io.ConfigInfo.MatrixRegTensor_K,io.ConfigInfo.Is_Transpose)
                printf("[trLoader1<%d>]Tensor_BaseAddr:%x,Tensor_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.Tensor.baseVaddr,io.ConfigInfo.Tensor.baseVaddr)
                printf("[trLoader1<%d>]stride:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.Tensor.stride)
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
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, TrMatrixRegEntryByteSize)).W))


    // A CAM maps each memory request source ID to a MatrixReg address and bank.
    // Source IDs index this register table.

    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new TrLoader1SourceId)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new TrLoader1SourceId).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = Reg(Vec(TrLoader1ReadFromMemoryFIFODepth, UInt(outsideDataWidth.W)))
    val MReg_Fill_Table_MReg_Addr = Reg(Vec(TrLoader1ReadFromMemoryFIFODepth, UInt(log2Ceil(TrMatrixRegBankNEntries).W))) //MatrixReg address for each returned LLC response.
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(TrLoader1ReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/TrMatrixRegEntryByteSize)+1).W)))))//Remaining writeback slices before releasing the entry.
    val MReg_Fill_Table_IsTail = Reg(Vec(TrLoader1ReadFromMemoryFIFODepth, Bool()))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)//True when a fill-table entry is free.
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)//True when a fill-table entry is valid.
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)//Index of the first free entry.
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)//True when an entry is available.
    val MAX_Fill_Times = outsideDataWidthByte/TrMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = Reg(Vec(TrMatrixRegNBanks, Vec(TrLoader1ReadFromMemoryFIFODepth, UInt(log2Ceil(TrLoader1ReadFromMemoryFIFODepth).W)))) //Bank assigned to each FIFO entry.
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(log2Ceil(TrLoader1ReadFromMemoryFIFODepth).W)))))//Next fill-table index to write for each bank.
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(log2Ceil(TrLoader1ReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//True when a bank has data to write to scratchpad.
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//True when any bank has a pending writeback.

    for(i <- 0 until TrMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), TrLoader1ReadFromMemoryFIFODepth)//FIFO is full.
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//No pending write for this bank.
    }

    // Legacy trLoader1 only uses channel 0 for requests
    val Request = io.LocalMMUIO.Request(0)
    val Response = io.LocalMMUIO.Response(0)
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            CurrentLoaded_BlockTensor_M_Iter := 0.U
            CurrentLoaded_BlockTensor_K_Iter := 0.U
            Request_M_Iter_Time := 0.U
            MaxRequestIter := MatrixRegTensor_M * K_Beat_Count //Total number of memory requests.
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
        }
        is(s_load_working) {
            io.ToMatrixRegIO.active := true.B
            //Select the access pattern based on MemoryOrder.



            //Convert the one-hot value to a prefix mask by subtracting one.
            val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
            val fullTaskMask = Fill(outsideDataWidthByte, true.B)
            val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))

            // Match trLoader0 order: issue along N across four banks, advance K, then move to the next N block.
            val RequestMatrixRegRowIndex = CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time
            val NormalRequestMatrixRegBankId = RequestMatrixRegRowIndex % TrMatrixRegNBanks.U
            val NormalRequestMatrixRegBaseAddr = ((RequestMatrixRegRowIndex / TrMatrixRegNBanks.U) * ReduceGroupSize.U)
            val NormalRequestMatrixRegAddr = NormalRequestMatrixRegBaseAddr + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(MAX_Fill_Times))
            val RequestMatrixRegBankId = NormalRequestMatrixRegBankId
            val RequestMatrixRegAddr = NormalRequestMatrixRegAddr

            Request.bits.RequestAddr := Tensor_Block_BaseAddr + RequestMatrixRegRowIndex * stride + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))

            val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            Request.bits.RequestConherent := Conherent
            Request.bits.RequestSourceID := sourceId.bits
            Request.bits.RequestType_isWrite := false.B
            Request.bits.UseAllocatedSourceID := true.B
            Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
            Request.bits.IsTranspose := false.B
            Request.bits.MatrixPrefetchTag.foreach { tag =>
                tag := MatrixPrefetchTagCodec.encode(true.B, PrefetchStream.get, PrefetchTaskId.get)
            }
            Request.valid := TotalRequestSize < MaxRequestIter

            when(Request.fire && sourceId.valid){//A valid source ID means this request can be issued.
                //Request.ready means LocalMMU accepts the request; sourceId.valid means its allocated ID is valid.
                val TableItem = Wire(new TrLoader1SourceId)
                TableItem.MatrixRegBankId := RequestMatrixRegBankId
                TableItem.MatrixRegAddr := RequestMatrixRegAddr
                TableItem.MatrixRegisTail := RequestBeatIsTail
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
                if (YJPTrLoader1DebugEnable) {
                    printf("[TrLoader1_RequestHandshake<%d>] sourceId:%d, MatrixRegBankId:%d, MatrixRegAddr:%d, RequestAddr:%x, RequestConherent:%d, RequestType_isWrite:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegBankId,TableItem.MatrixRegAddr,Request.bits.RequestAddr,Request.bits.RequestConherent,Request.bits.RequestType_isWrite,RequestBeatIsTail)
                }

                Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                    when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === MatrixRegTensor_M - 1.U){
                        Request_M_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                        when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                            CurrentLoaded_BlockTensor_K_Iter := 0.U
                            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
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
                val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new TrLoader1SourceId).MatrixRegBankId
                current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
            }
            //Handle memory responses.
            //A CAM maps request source IDs to MatrixReg addresses and banks.
            //Use the response source ID to find the fill-table entry to update.
            val normalRespReady = if (TrLoadNeedsFillTable) {
                MReg_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
            } else {
                true.B
            }
            Response.ready := normalRespReady
            when(Response.fire){
                //The double-buffered AB register guarantees room for responses while compression runs.
                //A release design would need either double-width data to create writeback bubbles, or separate read and write ports at the existing data width.
                val sourceId = Response.bits.ReseponseSourceID
                val searchEntry = SoureceIdSearchTable(sourceId).asTypeOf(new TrLoader1SourceId)
                val MatrixRegBankId = searchEntry.MatrixRegBankId
                val MatrixRegAddr = searchEntry.MatrixRegAddr
                val ResponseData = Response.bits.ReseponseData
                val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)//Fill FIFO index for this bank.

                if (YJPTrLoader1DebugEnable) {
                    printf("[TrLoader1_ResponseHandshake<%d>] ResponseData:%x, MatrixRegBankId:%d, MatrixRegAddr:%d, SourceId:%d, FIFOIndex:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr,sourceId,FIFOIndex,searchEntry.MatrixRegisTail)
                }

                if (!TrLoadNeedsFillTable)
                    {
                        TotalLoadSize := TotalLoadSize + 1.U
                        for (i <- 0 until TrMatrixRegNBanks)
                        {
                            when(MatrixRegBankId === i.U)
                            {
                                io.ToMatrixRegIO.BankAddr(i).bits := MatrixRegAddr
                                io.ToMatrixRegIO.Data(i).bits := ResponseData(TrMatrixRegEntryBitSize - 1, 0)
                                io.ToMatrixRegIO.BankAddr(i).valid := true.B
                                io.ToMatrixRegIO.Data(i).valid := true.B
                                io.ToMatrixRegIO.ByteMask(i).bits := Mux(searchEntry.MatrixRegisTail, tailTaskMask(TrMatrixRegEntryByteSize - 1, 0), Fill(TrMatrixRegEntryByteSize, true.B))
                                io.ToMatrixRegIO.ByteMask(i).valid := true.B
                            }
                        }
                    }

                    MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                    MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                    MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                    MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := searchEntry.MatrixRegisTail

                    Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                    Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), TrLoader1ReadFromMemoryFIFODepth)
                //A FIFO may be needed if this path can stall; full-throughput double buffering is expected to avoid stalls.
                //A direct MatrixReg-to-memory path could reduce latency but creates a long combinational path.
                //Alternatively, software can schedule traffic to keep the memory path stable and avoid that path.
                //MatrixReg writes have priority, so a unique write port should not block and may not need a FIFO.
                //Slow external-memory reads can wait; MatrixReg reads cannot, which motivates write priority.

                //Select the destination using the response ID.
                //TODO: Generalize the fixed read quantity for boundary cases.
                if (YJPTrLoader1DebugEnable)
                {
                    //Log response details.
                    printf("[trLoader1<%d>]ResponseData:%x,MatrixRegBankId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr)
                }
            }

            // Fill-table writeback has highest priority and runs whenever work is pending.
                val HasScarhpadWrite = Have_Bank_Fill
                val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(2.W))))
                if (TrLoadNeedsFillTable)
                {
                    for (i <- 0 until TrMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val fillSlotOH = UIntToOH(fillSlot, MAX_Fill_Times)
                            val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                            val rowsThisCycle = Mux(MReg_Fill_Table_Time(CurrentFIFOIndex) > 1.U, 2.U, 1.U)
                            Current_Fill_MReg_Time(i) := rowsThisCycle
                            val MatrixRegWriteRequest = io.ToMatrixRegIO
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*TrMatrixRegEntryByteSize).W)))))
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            // Generic tail mask: split the outside-data tail mask into entry-sized slots
                            val tailMaskSlots = VecInit((0 until MAX_Fill_Times).map { j =>
                                tailTaskMask((j + 1) * TrMatrixRegEntryByteSize - 1, j * TrMatrixRegEntryByteSize)
                            })
                            for (lane <- 0 until 2) {
                                val laneValid = MReg_Fill_Table_Time(CurrentFIFOIndex) > lane.U
                                val laneSlot = if (MAX_Fill_Times == 1) 0.U else
                                    Mux(laneValid, fillSlot + lane.U, 0.U)(log2Ceil(MAX_Fill_Times) - 1, 0)
                                val write = MatrixRegWriteRequest.WriteRequest(i)(lane)
                                write.BankAddr.bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + laneSlot
                                write.BankAddr.valid := laneValid
                                write.Data.bits := FIFOData(laneSlot)
                                write.Data.valid := laneValid
                                write.ByteMask.bits := Mux(currentIsTail, tailMaskSlots(laneSlot), Fill(TrMatrixRegEntryByteSize, true.B))
                                write.ByteMask.valid := laneValid

                            }
                            if (YJPTrLoader1DebugEnable) {
                                printf("[TrLoader1_MRegWriteHandshake<%d>] bankid: %d, CurrentFIFOIndex: %d, ScartchPadAddr: %x, BankAddr: %x, Data: %x, ByteMask: %x\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits, MatrixRegWriteRequest.ByteMask(i).bits)
                            }

                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - rowsThisCycle
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) <= 2.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), TrLoader1ReadFromMemoryFIFODepth)
                            }

                            if (YJPTrLoader1DebugEnable)
                            {
                                //Log fill count and FIFO index.
                                printf("[trLoader1 TrLoader1_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, MReg_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                                printf("[trLoader1 TrLoader1_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits)
                            }
                        }
                    }
                }

                val Current_Load_Fill_Size = WireInit(0.U((log2Ceil(TrMatrixRegNBanks)+1).W))
                Current_Load_Fill_Size := Current_Fill_MReg_Time.reduce(_ +& _)

                if (TrLoadNeedsFillTable)
                {
                    TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
                }
                if (YJPTrLoader1DebugEnable)
                {
                    when(Current_Load_Fill_Size =/= 0.U)
                    {
                        printf("[TrLoader1_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                    }
                }
                //Advance the state machine.
                when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                    memoryload_state := s_load_end
                    if (YJPTrLoader1DebugEnable)
                    {
                        printf("[TrLoader1_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
            }
        is(s_load_end) {
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                memoryload_state := s_load_idle
                state := s_idle
                if(YJPTrLoader1DebugEnable)
                {
                    printf("[trLoader1<%d>]TrLoader1Legacy Task End\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }
}

/**
  * Legacy TR store backend with one logical memory request/ACK channel.
  * Its state machine is independent from the loaders and retains each output
  * batch until all requests have been accepted.
  */
class TrStorerLegacy(implicit p: Parameters) extends CuteModule with CUTEImplParameters
    with TrStoreTransposeTarget with HasTransposeStoreEngine {
    val io = IO(new TrStoreIO)

    override protected def storeIo: TrStoreIO = io
    override protected def storeCommand: TrStoreCommand = StoreCommand
    override protected def storeState: UInt = memorystore_state
    override protected def storeCaptureState: UInt = s_store_capture
    override protected def storeRowIter: UInt = CurrentStore_Row_Iter
    override protected def storeByteOffset: UInt = CurrentStore_Byte_Offset
    override protected def storePhase: UInt = CurrentStore_Phase
    override protected def storePhaseCount: UInt = Store_Phase_Count
    override protected def storeElementLgBytes: UInt = Store_Element_LgBytes
    override protected def storeReadLaneCount: Int = StoreReadLaneCount
    override protected def storeTransposeDataBuffer: Vec[Vec[Vec[UInt]]] = Store_Data_Buffer
    require(outsideDataWidthByte == 64)
    require(Set(4, 8).contains(TrMatrixRegNBanks))
    require(Set(32, 64).contains(TrMatrixRegEntryByteSize))

    protected val StoreCommand = Reg(new TrStoreCommand)
    protected val s_store_idle :: s_store_read :: s_store_capture :: s_store_send :: s_store_drain :: s_store_end :: Nil = Enum(6)
    protected val memorystore_state = RegInit(s_store_idle)
    protected val CurrentStore_Row_Iter = RegInit(0.U((MatrixRegMaxTensorDimBitSize + 2).W))
    // Normal rows hold up to Tensor_K bytes; e32 transpose rows hold up to Tensor_MN * 4 bytes.
    protected val CurrentStore_Byte_Offset = RegInit(0.U(log2Ceil((Tensor_K max (Tensor_MN * 4)) + 1).W))
    protected val CurrentStore_Phase = RegInit(0.U(5.W))
    protected val StoreReadLaneCount = outsideDataWidthByte / TrMatrixRegEntryByteSize
    protected val Store_Phase_Count = WireDefault(1.U(6.W))
    // A paired read supplies two K segments for transpose; keep both until sent.
    protected val Store_Data_Buffer = Reg(Vec(StoreReadLaneCount,
        Vec(TrMatrixRegNBanks, Vec(64, UInt(8.W)))))
    protected val Store_Output_Context = RegInit(0.U(1.W))
    protected val Store_Pending_Lanes = RegInit(0.U(TrMatrixRegNBanks.W))
    protected val Store_Batch_Id = RegInit(0.U(32.W))
    protected val TotalStoreRequestSize = RegInit(0.U(32.W))
    protected val TotalStoreResponseSize = RegInit(0.U(32.W))

    protected val Store_Element_LgBytes = StoreCommand.widths(1, 0)
    private def storeRowBytes(command: TrStoreCommand): UInt =
        Mux(command.widths === Bundles.MSew.e4, command.columns >> 1,
            command.columns.pad(MatrixRegMaxTensorDimBitSize + 2) << command.widths(1, 0))
    protected val Store_Memory_Row_Count = Mux(StoreCommand.transpose, StoreCommand.columns, StoreCommand.rows)
    protected val Store_Memory_Row_Byte_Count = Mux(StoreCommand.transpose,
        StoreCommand.rows.pad(MatrixRegMaxTensorDimBitSize + 2) << Store_Element_LgBytes,
        storeRowBytes(StoreCommand))
    protected val Store_Active_Lanes = VecInit((0 until StoreReadLaneCount).map { context =>
        VecInit((0 until TrMatrixRegNBanks).map { lane =>
            CurrentStore_Row_Iter + lane.U +
                (if (StoreReadLaneCount == 1) 0.U else
                    (if (context == 0) 0.U else
                        (TrMatrixRegEntryByteSize.U >> Store_Element_LgBytes))) < Store_Memory_Row_Count
        }).asUInt
    })

    io.command.ready := memorystore_state === s_store_idle
    io.done.valid := memorystore_state === s_store_end
    io.done.bits := true.B
    io.regId := StoreCommand.regId
    when (io.done.fire) { memorystore_state := s_store_idle }

    when (io.command.fire) {
        val newCommand = io.command.bits
        val nonempty = newCommand.rows =/= 0.U && newCommand.columns =/= 0.U
        val bytes = Mux(newCommand.transpose,
            newCommand.rows.pad(MatrixRegMaxTensorDimBitSize + 2) << newCommand.widths(1, 0),
            storeRowBytes(newCommand))
        val rows = Mux(newCommand.transpose, newCommand.columns, newCommand.rows)
        assert(newCommand.widths <= Bundles.MSew.e32 || newCommand.widths === Bundles.MSew.e4,
            "TR store supports raw e4/e8/e16/e32")
        assert(!newCommand.transpose || newCommand.widths <= Bundles.MSew.e32,
            "TR transpose store supports only e8/e16/e32")
        assert(newCommand.rows <= Tensor_MN.U && storeRowBytes(newCommand) <= Tensor_K.U,
            "TR store exceeds the architectural register geometry")
        when (nonempty) {
            assert(newCommand.base(5, 0) === 0.U, "TR store requires a 64-byte aligned base")
            // TODO(tr-store-tail): add masked L2 writes or a reviewed RMW path.
            // The current CUTE/L2 Put path only accepts complete 64-byte lines;
            // never round up and overwrite bytes outside the requested tile.
            assert(bytes(5, 0) === 0.U && bytes =/= 0.U,
                "TR store requires each effective row to contain a multiple of 64 bytes; partial stores are not implemented")
            when (newCommand.widths === Bundles.MSew.e4) {
                assert(!newCommand.columns(0), "TR store does not support a partial e4 byte")
            }
            when (rows > 1.U) {
                assert(newCommand.stride(5, 0) === 0.U,
                    "TR store requires a 64-byte aligned stride")
                // TODO(tr-store-overlap): support overlapping rows only with reviewed write ordering.
                assert(newCommand.stride >= bytes,
                    "TR store does not support overlapping destination rows")
            }
        }
        StoreCommand := newCommand
        CurrentStore_Row_Iter := 0.U
        CurrentStore_Byte_Offset := 0.U
        CurrentStore_Phase := 0.U
        Store_Output_Context := 0.U
        Store_Batch_Id := 0.U
        TotalStoreRequestSize := 0.U
        TotalStoreResponseSize := 0.U
        Store_Pending_Lanes := 0.U
        memorystore_state := Mux(nonempty, s_store_read, s_store_end)
    }

    io.read.BankAddr.valid := memorystore_state === s_store_read
    for (bank <- 0 until TrMatrixRegNBanks) {
        val baseAddress =
            (CurrentStore_Row_Iter / TrMatrixRegNBanks.U) * ReduceGroupSize.U +
                (CurrentStore_Byte_Offset / TrMatrixRegEntryByteSize.U) + CurrentStore_Phase
        for (lane <- 0 until StoreReadLaneCount) {
            io.read.BankAddr.bits(bank)(lane) := baseAddress + lane.U
        }
    }

    when (io.read.BankAddr.fire) { memorystore_state := s_store_capture }
    when (memorystore_state === s_store_capture) {
        assert(io.read.Data.valid,
            "TR store requires the accepted SRAM read to return one cycle later")
        when (io.read.Data.valid) {
            when (CurrentStore_Phase +& 1.U === Store_Phase_Count) {
                CurrentStore_Phase := 0.U
                Store_Output_Context := 0.U
                Store_Pending_Lanes := Store_Active_Lanes(0)
                memorystore_state := s_store_send
            }.otherwise {
                CurrentStore_Phase := CurrentStore_Phase + 1.U
                memorystore_state := s_store_read
            }
        }
    }

    when (memorystore_state === s_store_capture && io.read.Data.valid && !StoreCommand.transpose) {
        for (bank <- 0 until TrMatrixRegNBanks;
             readLane <- 0 until StoreReadLaneCount;
             byte <- 0 until TrMatrixRegEntryByteSize) {
            Store_Data_Buffer(0)(bank)(readLane * TrMatrixRegEntryByteSize + byte) :=
                io.read.Data.bits(bank)(readLane)(8 * byte + 7, 8 * byte)
        }
    }

    private val Store_Transpose_Row_Offset =
        Store_Output_Context * (TrMatrixRegEntryByteSize.U >> Store_Element_LgBytes)
    private val Store_Output_Row_Iter = CurrentStore_Row_Iter +
        Mux(StoreCommand.transpose, Store_Transpose_Row_Offset, 0.U)

    private def nextStoreRowIter: UInt = {
        val nextRow = CurrentStore_Row_Iter + TrMatrixRegNBanks.U
        if (StoreReadLaneCount == 2) {
            val entryRows = TrMatrixRegEntryByteSize.U >> Store_Element_LgBytes
            // entryRows is a power of two for every supported element width.
            Mux(StoreCommand.transpose && (nextRow & (entryRows - 1.U)) === 0.U,
                nextRow + entryRows,
                nextRow)
        } else {
            nextRow
        }
    }

    private def finishStoreRowGroup(): Unit = {
        Store_Output_Context := 0.U
        Store_Pending_Lanes := 0.U
        when (CurrentStore_Byte_Offset +& 64.U === Store_Memory_Row_Byte_Count) {
            CurrentStore_Byte_Offset := 0.U
            CurrentStore_Row_Iter := nextStoreRowIter
            memorystore_state := Mux(
                nextStoreRowIter >= Store_Memory_Row_Count,
                s_store_drain,
                s_store_read
            )
        }.otherwise {
            CurrentStore_Byte_Offset := CurrentStore_Byte_Offset + 64.U
            memorystore_state := s_store_read
        }
    }

    private def completeStoreOutputContext(): Unit = {
        Store_Batch_Id := Store_Batch_Id + 1.U
        if (StoreReadLaneCount == 2) {
            when (StoreCommand.transpose && Store_Output_Context === 0.U) {
                Store_Output_Context := 1.U
                Store_Pending_Lanes := Store_Active_Lanes(1)
            }.otherwise {
                finishStoreRowGroup()
            }
        } else {
            finishStoreRowGroup()
        }
    }

    val BankStoreRequest = Wire(Vec(TrMatrixRegNBanks, Decoupled(new MMURequestIO)))
    for (bank <- 0 until TrMatrixRegNBanks) {
        BankStoreRequest(bank).valid := false.B
        BankStoreRequest(bank).bits := 0.U.asTypeOf(BankStoreRequest(bank).bits)
    }
    private val legacyArb = Module(new Arbiter(new MMURequestIO, TrMatrixRegNBanks))
        .suggestName("trStorer0_legacy_req_arb")
    for (bank <- 0 until TrMatrixRegNBanks) {
        legacyArb.io.in(bank) <> BankStoreRequest(bank)
    }
    io.memory.Request(0).valid := legacyArb.io.out.valid
    io.memory.Request(0).bits := legacyArb.io.out.bits
    legacyArb.io.out.ready := io.memory.Request(0).ready
    for (lane <- 1 until TrMatrixRegNBanks) {
        io.memory.Request(lane).valid := false.B
        io.memory.Request(lane).bits := 0.U.asTypeOf(io.memory.Request(lane).bits)
    }

    val StoreRequestFire = Wire(Vec(TrMatrixRegNBanks, Bool()))
    val StoreResponseFire = Wire(Vec(TrMatrixRegNBanks, Bool()))
    for (lane <- 0 until TrMatrixRegNBanks) {
        val request = BankStoreRequest(lane)
        request.valid := memorystore_state === s_store_send && Store_Pending_Lanes(lane)
        request.bits := 0.U.asTypeOf(request.bits)
        request.bits.RequestAddr := StoreCommand.base +
            (Store_Output_Row_Iter + lane.U) * StoreCommand.stride + CurrentStore_Byte_Offset
        val outputData = if (StoreReadLaneCount == 1) Store_Data_Buffer(0)(lane)
            else Store_Data_Buffer(Store_Output_Context)(lane)
        request.bits.RequestData := outputData.asUInt
        request.bits.RequestSourceID := (Store_Batch_Id << log2Ceil(TrMatrixRegNBanks)) | lane.U
        request.bits.RequestType_isWrite := true.B
        request.bits.RequestConherent := true.B
        request.bits.RequestMask := Fill(MMUMaskWidth, true.B)
        StoreRequestFire(lane) := request.fire
        io.memory.Response(lane).ready := memorystore_state =/= s_store_idle &&
            memorystore_state =/= s_store_end
        StoreResponseFire(lane) := io.memory.Response(lane).fire
    }
    when (StoreRequestFire.asUInt.orR) {
        TotalStoreRequestSize := TotalStoreRequestSize + PopCount(StoreRequestFire)
    }
    when (StoreResponseFire.asUInt.orR) {
        TotalStoreResponseSize := TotalStoreResponseSize + PopCount(StoreResponseFire)
    }
    when (memorystore_state =/= s_store_idle) {
        assert(TotalStoreResponseSize +& PopCount(StoreResponseFire) <=
            TotalStoreRequestSize +& PopCount(StoreRequestFire),
            "TR store received more acknowledgements than accepted requests")
    }

    when (memorystore_state === s_store_send) {
        Store_Pending_Lanes := Store_Pending_Lanes & ~StoreRequestFire.asUInt
        when ((Store_Pending_Lanes & ~StoreRequestFire.asUInt) === 0.U) {
            completeStoreOutputContext()
        }
    }
    when (memorystore_state === s_store_drain &&
        TotalStoreResponseSize === TotalStoreRequestSize) {
        memorystore_state := s_store_end
    }

    transposeStoreDatapath()

    if (EnableDifftest) {
        val event = CuteDifftest(new DiffAmuFinishEvent, delay = 0, dontCare = true)
        event.index := 2.U
        event.valid := io.done.fire
        event.pc := StoreCommand.pc.get
        event.coreid := StoreCommand.coreid.get
    }
}
