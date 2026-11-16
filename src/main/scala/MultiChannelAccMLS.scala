package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.SeqToAugmentedSeq
import xscache.coupledL2.prefetch.{MatrixPrefetchStream, MatrixPrefetchTagCodec}

// Configurable ACC memory path: load and store accumulator register data.
// Load data from memory into register storage.

// Data primarily arrives through the external memory interface.
// The shared memory unit accepts loader address requests and returns their data.
// Future virtual-memory integration would need OS support for swapping, lazy allocation and copy-on-write.
// Address translation, when supported, belongs in LocalMMU; the current CUTE path uses physical addresses.

// Split each tensor into register-sized transfers and fill the register banks.

object AccLoadPerfectBatchSelector {
    def select(
      valid: Seq[Bool],
      address: Seq[UInt],
      destination: Seq[UInt],
      retained: Seq[Bool],
      seedPointer: UInt,
      l2BankCount: Int
    ): UInt = {
        require(valid.nonEmpty && isPow2(valid.size))
        require(address.size == valid.size && destination.size == valid.size && retained.size == valid.size)
        require(isPow2(l2BankCount))
        val laneCount = valid.size
        val laneWidth = log2Ceil(laneCount max 2)
        val validVec = VecInit(valid)
        val validMask = validVec.asUInt
        val retainedMask = VecInit(retained).asUInt
        val rotatedValid = Wire(Vec(laneCount, Bool()))
        for (offset <- 0 until laneCount) {
            val lane = (seedPointer + offset.U)(laneWidth - 1, 0)
            rotatedValid(offset) := validVec(lane)
        }
        val seedOffset = PriorityEncoder(rotatedValid.asUInt)
        val seedLane = (seedPointer + seedOffset)(laneWidth - 1, 0)
        val seedMask = Mux(retainedMask.orR || !validMask.orR, 0.U, UIntToOH(seedLane, laneCount))
        val stages = Seq.fill(laneCount + 1)(Wire(UInt(laneCount.W)))
        stages.head := retainedMask | seedMask
        val banks = address.map(IdealL2Banking.bankOf(_, l2BankCount))

        for (lane <- 0 until laneCount) {
            val current = stages(lane)
            val candidateBit = UIntToOH(lane.U, laneCount)
            val tentative = current | candidateBit
            val bankConflict = (0 until laneCount).map { selectedLane =>
                current(selectedLane) && banks(selectedLane) === banks(lane)
            }.reduce(_ || _)
            val validByBank = Wire(Vec(laneCount, Bool()))
            val destinationByBank = Wire(Vec(laneCount, UInt(destination.head.getWidth.W)))
            for (sourceBank <- 0 until laneCount) {
                val bankMatches = (0 until laneCount).map { selectedLane =>
                    tentative(selectedLane) && banks(selectedLane) === sourceBank.U
                }
                validByBank(sourceBank) := bankMatches.reduce(_ || _)
                destinationByBank(sourceBank) := Mux1H(bankMatches, destination)
            }
            val survivor = OmegaTopology.survivorMask(validByBank, destinationByBank)
            val routeCompatible = (survivor & validByBank.asUInt) === validByBank.asUInt
            val add = valid(lane) && !current(lane) && !bankConflict && routeCompatible
            stages(lane + 1) := Mux(add, tentative, current)
        }

        Mux(stages.last.orR, stages.last, PriorityEncoderOH(validMask))
    }
}

class MultiChannelAccMemory(implicit p: Parameters) extends CuteModule{
    class AccLoadSourceId(implicit p: Parameters) extends CuteBundle{
        val MatrixRegBankId =UInt(log2Ceil(AccMatrixRegNBanks).W)
        val MatrixRegAddr = UInt(log2Ceil(AccMatrixRegBankNEntries).W)
        val MatrixRegisTail = Bool()
    }
    private val AccLoadSourceIdAddrWidth = log2Ceil(AccMatrixRegBankNEntries)
    private val AccLoadSourceIdBankWidth = log2Ceil(AccMatrixRegNBanks)
    private val AccLoadSourceIdTailOffset = AccLoadSourceIdAddrWidth + AccLoadSourceIdBankWidth

    private def encodeAccLoadSourceId(bank: UInt, addr: UInt, isTail: Bool): UInt =
        Cat(isTail, bank(AccLoadSourceIdBankWidth - 1, 0), addr(AccLoadSourceIdAddrWidth - 1, 0))

    private def decodeAccLoadSourceId(source: UInt): AccLoadSourceId = {
        val decoded = Wire(new AccLoadSourceId)
        decoded.MatrixRegAddr := source(AccLoadSourceIdAddrWidth - 1, 0)
        decoded.MatrixRegBankId := source(AccLoadSourceIdTailOffset - 1, AccLoadSourceIdAddrWidth)
        decoded.MatrixRegisTail := source(AccLoadSourceIdTailOffset)
        decoded
    }


    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new AccMemoryMatrixRegIO)
        val ConfigInfo = Flipped(new AccMemoryTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val perfProbe = Output(new AccLoadPerfProbe)
    })

    println(s"[CUTE] CMatrix: RegBanks ${AccMatrixRegNBanks} BankEntries ${AccMatrixRegBankNEntries}")

    // Expose the common ToMatrixRegIO interface.

    io.ConfigInfo.LoadMicroTaskEndValid := false.B
    io.ConfigInfo.StoreMicroTaskEndValid := false.B
    io.ConfigInfo.LoadMicroTaskReady := false.B
    io.ConfigInfo.StoreMicroTaskReady := false.B
    io.perfProbe := 0.U.asTypeOf(new AccLoadPerfProbe)
    io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest :=
        0.U.asTypeOf(io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest)
    io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest.foreach(_.foreach { write =>
        write.ByteMask.bits := Fill(AccMatrixRegEntryByteSize, true.B)
    })
    for (i <- 0 until AccMatrixRegNBanks) {
        io.LoadLocalMMUIO.Request(i).valid := false.B
        io.LoadLocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LoadLocalMMUIO.Request(i).bits)
        io.LoadLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LoadLocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.LoadLocalMMUIO.Response(i).ready := false.B
        io.StoreLocalMMUIO.Request(i).valid := false.B
        io.StoreLocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.StoreLocalMMUIO.Request(i).bits)
        io.StoreLocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.StoreLocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.StoreLocalMMUIO.Response(i).ready := false.B
    }


    val ConfigInfo = io.ConfigInfo
    val CurrentLoadMatrixRegId = RegInit(0.U(AccMatrixRegIdWidth.W))
    val CurrentStoreMatrixRegId = RegInit(0.U(AccMatrixRegIdWidth.W))
    io.LoadMatrixRegId := CurrentLoadMatrixRegId
    io.StoreMatrixRegId := CurrentStoreMatrixRegId

    val LoadPcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None
    val StorePcReg = if (EnableDifftest) Some(RegInit(0.U(64.W))) else None

    val LoadCoreidReg = Option.when(EnableDifftest)(RegInit(0.U(8.W)))
    val StoreCoreidReg = Option.when(EnableDifftest)(RegInit(0.U(8.W)))

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
    val MemoryOrder_LoadConfig = RegInit(MemoryOrderType.OrderTypeUndef)

    // Memory-write state machine, including pipeline draining.
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
    val PrefetchTaskId = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.taskIdWidth.W)))
    val PrefetchStream = Option.when(EnableMatrixPrefetch)(RegInit(MatrixPrefetchStream.none))
    val StoreTraceTag = Option.when(EnableMatrixPrefetch)(RegInit(0.U(MatrixPrefetchTagCodec.width.W)))

    val C_DataWidth = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val D_DataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val CLoadTaskStartCycle = RegInit(0.U(64.W))

    io.ConfigInfo.LoadMicroTaskReady := memoryload_state === s_load_idle
    io.ConfigInfo.StoreMicroTaskReady := memorystore_state === s_store_idle

    when(io.ConfigInfo.LoadMicroTaskValid && io.ConfigInfo.LoadMicroTaskReady) {
        CLoadTaskStartCycle := io.DebugInfo.DebugTimeStampe
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

    when(io.ConfigInfo.StoreMicroTaskValid && io.ConfigInfo.StoreMicroTaskReady) {
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
      "MultiChannelAccMemory: split channels share one config payload, load/store valid cannot be high together")



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
    val TotalRequestSize = Seq.fill(AccMatrixRegNBanks){RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))} // Total number of memory requests issued.
    val CurrentLoaded_BlockTensor_M_Iter = Seq.fill(AccMatrixRegNBanks){RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))}
    val CurrentLoaded_BlockTensor_N_Iter = Seq.fill(AccMatrixRegNBanks){RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))}
    val CLoadTotalFired = RegInit(0.U((log2Ceil(Tensor_MN * Tensor_MN * 2).max(1)).W))
    val CLoadPerfectBatches = RegInit(0.U((log2Ceil(Tensor_MN * Tensor_MN).max(1)).W))
    val CLoadPerfectRequests = RegInit(0.U((log2Ceil(Tensor_MN * Tensor_MN * 2).max(1)).W))
    val CLoadFireVec = WireInit(VecInit(Seq.fill(AccMatrixRegNBanks)(false.B)))
    val CLoadPerfectBatch = WireInit(false.B)
    val CLoadRetainedValid = RegInit(VecInit(Seq.fill(AccMatrixRegNBanks)(false.B)))
    val CLoadSeedPointer = RegInit(0.U(log2Ceil(AccMatrixRegNBanks max 2).W))
    // The C response queues are per register bank and accept out-of-order
    // responses.  Keep an issued bitmap per lane instead of deriving progress
    // from a single head iterator.  The combinational reducer only looks at a
    // bounded four-entry window; a retained entry is immutable until fire.
    val CScoreboardDepth = Tensor_MN * 8
    val CScoreboardIndexWidth = log2Ceil(CScoreboardDepth max 2)
    val CLoadIssued = Seq.fill(AccMatrixRegNBanks) {
        RegInit(VecInit(Seq.fill(CScoreboardDepth)(false.B)))
    }
    val CLoadRetainedIndex = Seq.fill(AccMatrixRegNBanks) {
        RegInit(0.U(CScoreboardIndexWidth.W))
    }
    val CLoadWindowBase = Seq.fill(AccMatrixRegNBanks) {
        RegInit(0.U(CScoreboardIndexWidth.W))
    }

    io.perfProbe.loadTotalFired := CLoadTotalFired
    io.perfProbe.loadPerfectBatches := CLoadPerfectBatches
    io.perfProbe.loadPerfectRequests := CLoadPerfectRequests
    io.perfProbe.loadPerfectBatch := CLoadPerfectBatch

    // Map each in-flight source ID to its register bank and address.
    // This is a register table indexed by source ID.

    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    private val fillQueueMetaWidth = AccLoadSourceIdAddrWidth + 1
    val fillDataQueues = Seq.tabulate(AccMatrixRegNBanks) { bank =>
        Module(new Queue(UInt(outsideDataWidth.W), AccLoader0FillQueueDepth, pipe = true))
            .suggestName(s"accLoader0_bank${bank}_fill_data_queue")
    }
    val fillMetaQueues = Seq.tabulate(AccMatrixRegNBanks) { bank =>
        Module(new Queue(UInt(fillQueueMetaWidth.W), AccLoader0FillQueueDepth, pipe = true))
            .suggestName(s"accLoader0_bank${bank}_fill_meta_queue")
    }
    // init fill queue input ports
    for (i <- 0 until AccMatrixRegNBanks) {
        fillDataQueues(i).io.enq.valid := false.B
        fillDataQueues(i).io.enq.bits := 0.U
        fillDataQueues(i).io.deq.ready := false.B
        fillMetaQueues(i).io.enq.valid := false.B
        fillMetaQueues(i).io.enq.bits := 0.U
        fillMetaQueues(i).io.deq.ready := false.B

        assert(
            fillDataQueues(i).io.enq.ready === fillMetaQueues(i).io.enq.ready,
            s"accMemory bank $i fill enqueue queues diverged"
        )
        assert(
            fillDataQueues(i).io.deq.valid === fillMetaQueues(i).io.deq.valid,
            s"accMemory bank $i fill dequeue queues diverged"
        )
    }

    val MAX_Fill_Times = outsideDataWidthByte/AccMatrixRegEntryByteSize

    val Repeat_Fill_Is_Working = RegInit(false.B)// Whether a repeated fill is active.
    val Repeat_Fill_Times = RegInit(0.U(log2Ceil(Tensor_MN).W))// Number of times this data must be written back.
    val Repeat_Fill_Group_Times = RegInit(0.U(log2Ceil(outsideDataWidthByte/AccMatrixRegEntryByteSize).W))// Number of times this data must be written back.
    val Repeat_Fill_Table_Index = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// Index of this response in the fill FIFO.
    val Repeat_Fill_Request_Infight = RegInit(0.U(log2Ceil(AccLoader0ReadFromMemoryFIFODepth).W))// Track in-flight requests so their multi-cycle fills cannot overrun the available entries.

    io.LoadLocalMMUIO.Request.foreach(_.valid := false.B)
    io.StoreLocalMMUIO.Request.foreach(_.valid := false.B)

    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        TotalRequestSize.foreach(_ := 0.U)
        CurrentLoaded_BlockTensor_M_Iter.foreach(_ := 0.U)
        CurrentLoaded_BlockTensor_N_Iter.foreach(_ := 0.U)
        CLoadTotalFired := 0.U
        CLoadPerfectBatches := 0.U
        CLoadPerfectRequests := 0.U
        CLoadRetainedValid.foreach(_ := false.B)
        CLoadIssued.foreach(_.foreach(_ := false.B))
        CLoadRetainedIndex.foreach(_ := 0.U)
        CLoadWindowBase.foreach(_ := 0.U)
        CLoadSeedPointer := 0.U
        MaxRequestIter := LoadMatrixRegTensor_M * N_Beat_Count // Total number of memory requests to issue.
        Repeat_Fill_Times := 0.U
        Repeat_Fill_Group_Times := 0.U
        Repeat_Fill_Request_Infight := 0.U
        Repeat_Fill_Is_Working := false.B
    }.elsewhen(memoryload_state === s_load_working){
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

        when(Is_FullLoad)
        {
            val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
            val fullTaskMask = Fill(outsideDataWidthByte, true.B)
            val candidateValid = Wire(Vec(AccMatrixRegNBanks, Bool()))
            val candidateAddress = Wire(Vec(AccMatrixRegNBanks, UInt(MMUAddrWidth.W)))
            val candidateDestination = Wire(Vec(AccMatrixRegNBanks, UInt(log2Ceil(AccMatrixRegNBanks max 2).W)))
            val candidateIndex = Wire(Vec(AccMatrixRegNBanks, UInt(CScoreboardIndexWidth.W)))
            val candidateBeat = Wire(Vec(AccMatrixRegNBanks, UInt(MatrixRegMaxTensorDimBitSize.W)))
            val candidateGroup = Wire(Vec(AccMatrixRegNBanks, UInt(MatrixRegMaxTensorDimBitSize.W)))
            val usePerfectBatchOrder = N_Beat_Count === 8.U
            // Eight legal Omega templates form an 8-cycle.  For the zames
            // stride, each lane visits every beat exactly once while every
            // cycle is a legal permutation.  The bitmap still guards replay
            // and makes the mechanism safe when ready is asymmetric.
            val beatLut = Seq(
              Seq(0, 1, 2, 3, 5, 6, 7, 0),
              Seq(1, 2, 3, 4, 4, 5, 6, 7),
              Seq(2, 7, 4, 1, 7, 4, 1, 6),
              Seq(3, 0, 5, 2, 6, 3, 0, 5),
              Seq(4, 5, 6, 7, 1, 2, 3, 4),
              Seq(5, 6, 7, 0, 0, 1, 2, 3),
              Seq(6, 3, 0, 5, 3, 0, 5, 2),
              Seq(7, 4, 1, 6, 2, 7, 4, 1))
            for (bank <- 0 until AccMatrixRegNBanks) {
                val cycle = CLoadWindowBase(bank)(2, 0)
                val beat = VecInit(beatLut.map(row => row(bank).U(3.W)))(cycle)
                val group = CLoadWindowBase(bank) >> 3
                val generatedIndex = (group << 3) + beat
                candidateIndex(bank) := Mux(CLoadRetainedValid(bank), CLoadRetainedIndex(bank), generatedIndex)
                val idx = candidateIndex(bank)
                val perfectBeat = idx(2, 0)
                val perfectGroup = idx >> 3
                val fallbackBeat = CurrentLoaded_BlockTensor_N_Iter(bank)
                val fallbackGroup = CurrentLoaded_BlockTensor_M_Iter(bank)
                candidateBeat(bank) := Mux(usePerfectBatchOrder, perfectBeat, fallbackBeat)
                candidateGroup(bank) := Mux(usePerfectBatchOrder, perfectGroup, fallbackGroup)
                val row = candidateGroup(bank) * AccMatrixRegNBanks.U + bank.U
                val perfectValid = (idx < (LoadMatrixRegTensor_M * N_Beat_Count / AccMatrixRegNBanks.U)) &&
                  !CLoadIssued(bank)(idx)
                val fallbackValid = row < LoadMatrixRegTensor_M && fallbackBeat < N_Beat_Count
                candidateValid(bank) := Mux(usePerfectBatchOrder, perfectValid, fallbackValid)
                candidateAddress(bank) := LoadTensorBlockBaseAddr +
                  row * ApplicationTensor_C_Stride_M +
                  (candidateBeat(bank) << log2Ceil(outsideDataWidthByte))
                candidateDestination(bank) := bank.U
                when(usePerfectBatchOrder && CLoadRetainedValid(bank)) {
                    assert(candidateValid(bank), s"accMemory retained lane $bank lost its legal candidate")
                }
            }
            val selectedMask = AccLoadPerfectBatchSelector.select(
              candidateValid,
              candidateAddress,
              candidateDestination,
              CLoadRetainedValid,
              CLoadSeedPointer,
              L2NBanks)
            for (bank <- 0 until AccMatrixRegNBanks) {
                val ReadRequest = io.LoadLocalMMUIO.Request(bank)
                val TotalRequestSizeBank = TotalRequestSize(bank)
                ReadRequest.bits.RequestAddr := candidateAddress(bank)

                val csourceId = Wire(new AccLoadSourceId)
                csourceId.MatrixRegBankId := bank.U
                csourceId.MatrixRegAddr := candidateGroup(bank) * (Tensor_MN.U / Matrix_MN.U) +
                  (candidateBeat(bank) << log2Ceil(MAX_Fill_Times))
                csourceId.MatrixRegisTail := HasTail && (candidateBeat(bank) === (N_Beat_Count - 1.U))

                ReadRequest.valid := candidateValid(bank) && selectedMask(bank)
                ReadRequest.bits.RequestConherent := IsLoadConherent
                ReadRequest.bits.RequestSourceID := encodeAccLoadSourceId(csourceId.MatrixRegBankId, csourceId.MatrixRegAddr, csourceId.MatrixRegisTail)
                ReadRequest.bits.RequestType_isWrite := false.B
                ReadRequest.bits.UseAllocatedSourceID := false.B
                ReadRequest.bits.MatrixPrefetchTag.foreach { tag =>
                  tag := MatrixPrefetchTagCodec.encode(
                    true.B,
                    PrefetchStream.get,
                    PrefetchTaskId.get
                  )
                }
                ReadRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                CLoadFireVec(bank) := ReadRequest.fire

                when(ReadRequest.fire) {
                    CLoadRetainedValid(bank) := false.B
                    when(usePerfectBatchOrder) {
                        CLoadIssued(bank)(candidateIndex(bank)) := true.B
                        CLoadWindowBase(bank) := CLoadWindowBase(bank) + 1.U
                    }.otherwise {
                        when(CurrentLoaded_BlockTensor_N_Iter(bank) === (N_Beat_Count - 1.U)) {
                            CurrentLoaded_BlockTensor_N_Iter(bank) := 0.U
                            CurrentLoaded_BlockTensor_M_Iter(bank) := CurrentLoaded_BlockTensor_M_Iter(bank) + 1.U
                        }.otherwise {
                            CurrentLoaded_BlockTensor_N_Iter(bank) := CurrentLoaded_BlockTensor_N_Iter(bank) + 1.U
                        }
                    }
                }.elsewhen(ReadRequest.valid && !ReadRequest.ready) {
                    CLoadRetainedValid(bank) := true.B
                    when(usePerfectBatchOrder) {
                        CLoadRetainedIndex(bank) := candidateIndex(bank)
                    }
                }

                when(ReadRequest.fire){
                    assert(decodeAccLoadSourceId(ReadRequest.bits.RequestSourceID).MatrixRegBankId === bank.U,
                        s"accMemory channel $bank fired a request owned by another C register bank")
                    val cycle = io.DebugInfo.DebugTimeStampe


                    // Data is 256-bit aligned; partial Tensor_M and Tensor_K boundaries require separate handling.
                    when(TotalRequestSizeBank === MaxRequestIter){
                        //assert!
                        //error!
                    }.otherwise{
                        TotalRequestSizeBank := TotalRequestSizeBank + 1.U
                    }
                }

                val requestStalled = ReadRequest.valid && !ReadRequest.ready
                when(RegNext(requestStalled, false.B)) {
                    assert(ReadRequest.valid,
                        s"accMemory channel $bank dropped a stalled request")
                    assert(ReadRequest.bits === RegEnable(ReadRequest.bits, requestStalled),
                        s"accMemory channel $bank changed a stalled request payload")
                }


                // The response router guarantees that channel bank's response targets the same register bank,
                // so it can be enqueued directly into fillQueues(bank).
                val resp = io.LoadLocalMMUIO.Response(bank)
                val respSourceID = decodeAccLoadSourceId(resp.bits.ReseponseSourceID)
                val MatrixRegBankId = respSourceID.MatrixRegBankId
                val MatrixRegAddr = respSourceID.MatrixRegAddr
                val ResponseData = resp.bits.ReseponseData

                val fillDataQ = fillDataQueues(bank).io
                val fillMetaQ = fillMetaQueues(bank).io
                resp.ready := fillDataQ.enq.ready && fillMetaQ.enq.ready
                fillDataQ.enq.valid := resp.valid
                fillDataQ.enq.bits := ResponseData
                fillMetaQ.enq.valid := resp.valid
                fillMetaQ.enq.bits := Cat(respSourceID.MatrixRegisTail, MatrixRegAddr)

                when(resp.fire){
                    assert(MatrixRegBankId === bank.U,
                        cf"AccMemoryLegacy: response bankId mismatch on channel $bank, expected $bank but got $MatrixRegBankId. Router may be missing.")

                }
            }

            val firedBanks = CLoadFireVec.zipWithIndex.map { case (fired, bank) =>
                Mux(fired, IdealL2Banking.bankOf(io.LoadLocalMMUIO.Request(bank).bits.RequestAddr, L2NBanks), 0.U)
            }
            val firedValid = CLoadFireVec
            val distinctBanks = (0 until AccMatrixRegNBanks).flatMap { i =>
                ((i + 1) until AccMatrixRegNBanks).map { j =>
                    !firedValid(i) || !firedValid(j) || firedBanks(i) =/= firedBanks(j)
                }
            }.reduceOption(_ && _).getOrElse(true.B)
            val firedByBank = Wire(Vec(AccMatrixRegNBanks, Bool()))
            val destinationByBank = Wire(Vec(AccMatrixRegNBanks, UInt(log2Ceil(AccMatrixRegNBanks max 2).W)))
            for (sourceBank <- 0 until AccMatrixRegNBanks) {
                val bankMatches = (0 until AccMatrixRegNBanks).map { lane =>
                    firedValid(lane) && firedBanks(lane) === sourceBank.U
                }
                firedByBank(sourceBank) := bankMatches.reduce(_ || _)
                destinationByBank(sourceBank) := Mux1H(bankMatches, candidateDestination)
            }
            val survivors = OmegaTopology.survivorMask(firedByBank, destinationByBank)
            CLoadPerfectBatch := usePerfectBatchOrder && CLoadFireVec.asUInt.andR && distinctBanks &&
                (survivors === firedByBank.asUInt)
            for (i <- 0 until AccMatrixRegNBanks; j <- (i + 1) until AccMatrixRegNBanks) {
                assert(!(CLoadFireVec(i) && CLoadFireVec(j) &&
                    io.LoadLocalMMUIO.Request(i).bits.RequestSourceID ===
                      io.LoadLocalMMUIO.Request(j).bits.RequestSourceID),
                  s"accMemory fired duplicate source IDs on channels $i and $j")
            }
            assert(!CLoadPerfectBatch ||
                (CLoadFireVec.asUInt.andR && distinctBanks && survivors === firedByBank.asUInt),
              "accMemory perfect-batch classification disagrees with the exact route predicates")
            when(CLoadFireVec.asUInt.orR) {
                CLoadSeedPointer := CLoadSeedPointer + 1.U
                CLoadTotalFired := CLoadTotalFired + PopCount(CLoadFireVec)
                when(CLoadPerfectBatch) {
                    CLoadPerfectBatches := CLoadPerfectBatches + 1.U
                    CLoadPerfectRequests := CLoadPerfectRequests + AccMatrixRegNBanks.U

                }
            }

            // Check each bank for pending fill data.
            HasScarhpadWrite := fillDataQueues.map(_.io.deq.valid).reduce(_ || _)

            val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(AccMatrixRegNBanks)(0.U(2.W))))
            for (i <- 0 until AccMatrixRegNBanks){
                val fillTimes = RegInit(MAX_Fill_Times.U)
                val fillDataQ = fillDataQueues(i).io
                val fillMetaQ = fillMetaQueues(i).io
                val allowWrite = io.ToMatrixRegIO.LoadReadWriteResponse(MatrixRegTaskType.WriteFromMemoryLoaderIndex) === true.B
                when(allowWrite)
                {
                    when(fillDataQ.deq.valid){
                        val rowsThisCycle = Mux(fillTimes > 1.U, 2.U, 1.U)
                        Current_Fill_MReg_Time(i) := rowsThisCycle
                        val MatrixRegWriteRequest = io.ToMatrixRegIO.WriteRequestToMatrixReg
                        val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*AccMatrixRegEntryByteSize).W)))))
                        val fillSlot = MAX_Fill_Times.U - fillTimes
                        val currentIsTail = fillMetaQ.deq.bits(fillQueueMetaWidth - 1)
                        val fullByteMask = Fill(AccMatrixRegEntryByteSize, true.B)
                        val tailByteMaskVec = Wire(Vec(MAX_Fill_Times, UInt(AccMatrixRegEntryByteSize.W)))
                        for (j <- 0 until MAX_Fill_Times) {
                            val high = (j + 1) * AccMatrixRegEntryByteSize - 1
                            val low = j * AccMatrixRegEntryByteSize
                            tailByteMaskVec(j) := tailTaskMask(high, low)
                        }
                        FIFOData := fillDataQ.deq.bits.asTypeOf(FIFOData)
                        for (lane <- 0 until 2) {
                            val laneValid = fillTimes > lane.U
                            val laneSlot = if (MAX_Fill_Times == 1) 0.U else
                                Mux(laneValid, fillSlot + lane.U, 0.U)(log2Ceil(MAX_Fill_Times) - 1, 0)
                            val write = MatrixRegWriteRequest.WriteRequest(i)(lane)
                            write.BankAddr.bits := fillMetaQ.deq.bits(AccLoadSourceIdAddrWidth - 1, 0) + laneSlot
                            write.BankAddr.valid := laneValid
                            write.Data.bits := FIFOData(laneSlot)
                            write.Data.valid := laneValid
                            write.ByteMask.bits := Mux(currentIsTail, tailByteMaskVec(laneSlot), fullByteMask)
                            write.ByteMask.valid := laneValid

                        }

                        fillTimes := fillTimes - rowsThisCycle
                        when(fillTimes <= 2.U){
                            fillTimes := MAX_Fill_Times.U
                            fillDataQ.deq.ready := true.B
                            fillMetaQ.deq.ready := true.B
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

                }
            }
            // Advance the state machine.
            val perfectRequestPermille = Mux(
              CLoadTotalFired === 0.U,
              0.U,
              (CLoadPerfectRequests * 1000.U) / CLoadTotalFired)
            when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                memoryload_state := s_load_end
                printf(
                  p"[accMemory][${io.DebugInfo.DebugTimeStampe}][accMemory-LOAD-END] matrixRegId=${CurrentLoadMatrixRegId} " +
                    p"startCycle=${CLoadTaskStartCycle} totalFired=${CLoadTotalFired} " +
                    p"perfectBatches=${CLoadPerfectBatches} perfectRequests=${CLoadPerfectRequests} " +
                    p"perfectRequestPermille=${perfectRequestPermille}\n"
                )
            }
        }.elsewhen(Is_ZeroLoad) {
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

                }
                when(TotalLoadSize === (Max_ZeroLoad_Write_Times-1).U)
                {
                    memoryload_state := s_load_end
                    if (YJPAccMemoryDebugEnable)
                    {

                    }
                }
            }
        }
    }.elsewhen(memoryload_state === s_load_end){
        io.ConfigInfo.LoadMicroTaskEndValid := true.B
        when(io.ConfigInfo.LoadMicroTaskEndReady && io.ConfigInfo.LoadMicroTaskEndValid){
            memoryload_state := s_load_idle
            if (YJPAccMemoryDebugEnable)
            {

            }
        }
    }.otherwise{
        // Idle.
    }


    // Store reads register data in Reduce_Dim-major order.

    /**
     * Round Max_BlockTensor_Major_DIM down to a multiple of Matrix_M for the regular address range.
     */
    val MaxIncStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * Round Max_BlockTensor_Major_DIM down to a multiple of Matrix_M for the regular address range.
     */
    val MaxIncStoreScpRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * Number of accumulator-register accesses per bank.
     */
    val Max_Load_MReg_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    /**
     * Total number of LLC write requests across all channels.
     */
    val Max_Store_Memory_Time = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))
    val Max_BlockTensor_Request_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Reduce_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Max_BlockTensor_Major_DIM = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Per_LLC_Store_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Per_MReg_Load_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_MN)).W))
    val Max_SubReduce_DIM = RegInit(0.U((log2Ceil(Tensor_MN)).W))

    /**
     * Memory-write requests per tile group.
     */
    val Per_MReg_Load_Write_Memory_Time = AccMatrixReg_Total_Bandwidth_Bit/outsideDataWidth

    /**
     * Each tile spans Matrix_MN rows; there are M_Get_IteratorMax row tiles.
     */
    val M_Get_IteratorMax = Mux(
        IsStoreTranspose,
        (StoreMatrixRegTensor_M / (Matrix_MN.U * 2.U) + (StoreMatrixRegTensor_M % (Matrix_MN.U * 2.U) =/= 0.U)) * 2.U,
        (StoreMatrixRegTensor_M / Matrix_MN.U) + ((StoreMatrixRegTensor_M % Matrix_MN.U) =/= 0.U))

    /**
     * Each tile spans Matrix_MN columns; there are N_Get_IteratorMax column tiles.
     */
    val N_Get_IteratorMax = WireInit(0.U(log2Ceil(AccMatrixRegBankNEntries).W))
    N_Get_IteratorMax := (StoreMatrixRegTensor_N / Matrix_MN.U)

    val transpose_scp_addr = WireInit(0.U(log2Ceil(AccMatrixRegBankNEntries).W))
    val DirectAckResponseCount = RegInit(0.U((log2Ceil(Tensor_MN*Tensor_MN)).W))

    class BankStore(bank: Int) {
        /**
         * Number of store requests already issued to LocalMMU.
         */
        val TotalStoreSize = Reg(UInt(log2Ceil(Tensor_MN*Tensor_MN).W))

        /**
         * Number of read requests already issued to the scratchpad.
         */
        val TotalScpReadCount = Reg(UInt(log2Ceil(Tensor_MN*Tensor_MN).W))

        val StoreQueueDepth = 4
        val StoreQueue = Module(new Queue(UInt(outsideDataWidth.W), StoreQueueDepth, pipe = false, flow = false))
        StoreQueue.io.enq.valid := false.B
        StoreQueue.io.enq.bits := 0.U
        StoreQueue.io.deq.ready := false.B

        val CollectTimes = outsideDataWidthByte / (AccMatrixRegEntryBitSize / 8)
        println(s"[accMemory][BankStore] CollecTimes $CollectTimes")
        val BusDataCollector = Reg(Vec(CollectTimes, UInt(AccMatrixRegEntryBitSize.W)))
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
            if (YJPAccMemoryDebugEnable) {
                printf(
                  cf"[AccMemory_Store<${io.DebugInfo.DebugTimeStampe}>][bank $bank][BankStore] " + msg + "\n"
                )
            }
        }

        def mainLogic(): Unit = {
            // The register arbiter permits this read.
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
                val collectIndex = if (CollectTimes == 1) 0.U else
                    collectIter(log2Ceil(CollectTimes) - 1, 0)
                BusDataCollector(collectIndex) := RegReadResp_Data
                logger(cf"BusDataCollector($collectIter) := 0x$RegReadResp_Data%x")
                // Let C be the cycle where collectIter === (CollectTimes - 2).U.
                // The current read request targets BusDataCollector(CollectTimes - 1), which receives its data in C+1.
                // Decide in cycle C whether the next read request, returning in C+2, may start a new collector batch.
                // 1. If the queue will be full, retain this collector batch and suppress the next read request in C+1.
                // A dequeue in C+1 guarantees space in C+2, so it can override that suppression.
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
                // The enqueue should fire in the cycle that writes collector slot 0.
                assert(collectIter === 0.U)
            }

            val WriteRequest = io.StoreLocalMMUIO.Request(bank)
            StoreQueue.io.deq.ready := WriteRequest.ready
            WriteRequest.valid := StoreQueue.io.deq.valid
            WriteRequest.bits.RequestAddr := StoreTensorBlockBaseAddr + (CurrentStore_BlockTensor_Major_DIM_Iter + bank.U) * ApplicationTensor_D_Stride_M + CurrentStore_BlockTensor_Reduce_DIM_Iter * D_DataType
            WriteRequest.bits.RequestConherent := IsStoreConherent
            WriteRequest.bits.RequestSourceID := encodeAccLoadSourceId(bank.U, TotalStoreSize, false.B)
            WriteRequest.bits.RequestType_isWrite := true.B
            WriteRequest.bits.UseAllocatedSourceID := false.B
            WriteRequest.bits.MatrixPrefetchTag.foreach(_ := 0.U)
            WriteRequest.bits.MatrixTraceTag.zip(StoreTraceTag).foreach { case (to, from) => to := from }
            WriteRequest.bits.RequestData := StoreQueue.io.deq.bits
            WriteRequest.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
            WriteRequest.bits.IsTranspose := false.B

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

                // Advance past the columns covered by one memory transfer.
                CurrentStore_BlockTensor_Reduce_DIM_Iter := CurrentStore_BlockTensor_Reduce_DIM_Iter + Per_LLC_Store_ReduceDim_Iter
                when(CurrentStore_BlockTensor_Reduce_DIM_Iter === Max_BlockTensor_Request_Reduce_DIM - Per_LLC_Store_ReduceDim_Iter){
                    CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
                    CurrentStore_BlockTensor_Major_DIM_Iter := CurrentStore_BlockTensor_Major_DIM_Iter + Matrix_MN.U
                }

                // Dequeue StoreQueue while simultaneously filling collector slot 0.
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
        DirectAckResponseCount := 0.U

        Max_Load_MReg_Time := StoreMatrixRegTensor_M * StoreMatrixRegTensor_N * D_DataType / AccMatrixReg_Total_Bandwidth.U

        Max_Store_Memory_Time := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_M) * StoreMatrixRegTensor_N * D_DataType / outsideDataWidthByte.U

        MaxIncStoreScpRequestSize := M_Get_IteratorMax * Matrix_MN.U * StoreMatrixRegTensor_N * D_DataType / AccMatrixReg_Total_Bandwidth.U

        Per_LLC_Store_ReduceDim_Iter := Mux(D_DataType === 1.U, outsideDataWidthByte.U,
                                Mux(D_DataType === 2.U, outsideDataWidthByte.U/2.U,
                                Mux(D_DataType === 4.U, outsideDataWidthByte.U/4.U, outsideDataWidthByte.U)))

        Per_MReg_Load_ReduceDim_Iter := Mux(D_DataType === 1.U, AccMatrixReg_Total_Bandwidth.U,
                                    Mux(D_DataType === 2.U, AccMatrixReg_Total_Bandwidth.U/2.U,
                                    Mux(D_DataType === 4.U, AccMatrixReg_Total_Bandwidth.U/4.U, AccMatrixReg_Total_Bandwidth.U)))

        Max_SubReduce_DIM := Mux(D_DataType === 1.U, AccMatrixReg_Total_Bandwidth.U,
                                Mux(D_DataType === 2.U, AccMatrixReg_Total_Bandwidth.U/2.U,
                                Mux(D_DataType === 4.U, AccMatrixReg_Total_Bandwidth.U/4.U, AccMatrixReg_Total_Bandwidth.U)))

        Max_BlockTensor_Reduce_DIM := Mux(IsStoreTranspose, StoreMatrixRegTensor_M, StoreMatrixRegTensor_N)

        // C-matrix column dimension.
        Max_BlockTensor_Request_Reduce_DIM := Mux(IsStoreTranspose, M_Get_IteratorMax * Matrix_MN.U, StoreMatrixRegTensor_N)

        // C-matrix row dimension.
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
        val StoreAckDone = Mux(
            AccStorer0DirectAckCountMode.B,
            DirectAckResponseCount === Max_Store_Memory_Time,
            WriteResponseCountSum === Max_Store_Memory_Time
        )
        when(StoreAckDone) {
            io.ConfigInfo.StoreMicroTaskEndValid := true.B
        }
        when(io.ConfigInfo.StoreMicroTaskEndReady && io.ConfigInfo.StoreMicroTaskEndValid){
            memorystore_state := s_store_idle
            if (YJPAccMemoryDebugEnable)
            {
                printf("[AccMemory_Store<%d>]Store Finish\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
        // Idle.
    }

    when(memorystore_state === s_store_working || memorystore_state === s_store_end){
        if (AccStorer0DirectAckCountMode) {
            val ackValids = io.StoreLocalMMUIO.Response.map(_.valid)
            val ackReadies = Wire(Vec(Matrix_MN, Bool()))
            val directAckFireVec = Wire(Vec(Matrix_MN, Bool()))
            val anyAckValid = ackValids.reduce(_ || _)
            val ackCanAccept = DirectAckResponseCount =/= Max_Store_Memory_Time
            for (bank <- 0 until Matrix_MN) {
                ackReadies(bank) := ackCanAccept
                io.StoreLocalMMUIO.Response(bank).ready := ackReadies(bank)
                directAckFireVec(bank) := io.StoreLocalMMUIO.Response(bank).fire
            }
            val firedAckCount = PopCount(directAckFireVec)
            when(firedAckCount =/= 0.U) {
                DirectAckResponseCount := DirectAckResponseCount + firedAckCount
                if (YJPAccMemoryDebugEnable) {
                    printf(
                        cf"[AccMemory_Store<${io.DebugInfo.DebugTimeStampe}>][DirectAck] " +
                        cf"ackFireVec=${directAckFireVec.asUInt} ackInc=${firedAckCount} " +
                        cf"totalAck=${DirectAckResponseCount + firedAckCount}/${Max_Store_Memory_Time}\n"
                    )
                }
            }.elsewhen(anyAckValid) {
                if (YJPAccMemoryDebugEnable) {
                    printf(
                        cf"[AccMemory_Store<${io.DebugInfo.DebugTimeStampe}>][DirectAck] " +
                        cf"ackValidNoFire validVec=${ackValids.asUInt} ready=${ackCanAccept}\n"
                    )
                }
            }
        } else {
            for (bank <- 0 until Matrix_MN) {
                BankStores(bank).onResponse()
            }
        }
    }

    if (YJPAccMemoryDebugEnable) {
        when(RegNext(memorystore_state === s_store_init)) {
            printf(
                cf"[AccMemory_Store<${io.DebugInfo.DebugTimeStampe}>] Store D Tensor Start\n" +
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
