
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._

class MultiChannelTrStorer(
    requestChannelCount: Int,
    contextName: String
)(implicit p: Parameters) extends CuteModule with CUTEImplParameters
    with TrStoreTransposeTarget with HasTransposeStoreEngine {
    require(Seq(1, 2, 4, 8).contains(requestChannelCount))
    require(TrMatrixRegNBanks % requestChannelCount == 0)
    require(outsideDataWidthByte == 64)
    require(Set(4, 8).contains(TrMatrixRegNBanks))
    require(Set(32, 64).contains(TrMatrixRegEntryByteSize))
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"MultiChannelTrStorer_${nameContext}"

    val io = IO(new TrStoreIO)

    private val StoreCommand = Reg(new TrStoreCommand)
    private val s_store_idle :: s_store_read :: s_store_capture :: s_store_send :: s_store_drain :: s_store_end :: Nil = Enum(6)
    private val memorystore_state = RegInit(s_store_idle)
    private val CurrentStore_Row_Iter = RegInit(0.U((MatrixRegMaxTensorDimBitSize + 2).W))
    // Normal rows hold up to Tensor_K bytes; e32 transpose rows hold up to Tensor_MN * 4 bytes.
    private val CurrentStore_Byte_Offset = RegInit(0.U(log2Ceil((Tensor_K max (Tensor_MN * 4)) + 1).W))
    private val CurrentStore_Phase = RegInit(0.U(5.W))
    private val StoreReadLaneCount = outsideDataWidthByte / TrMatrixRegEntryByteSize
    private val Store_Phase_Count = WireDefault(1.U(6.W))
    // A paired read supplies two K segments for transpose; keep both until sent.
    private val Store_Data_Buffer = Reg(Vec(StoreReadLaneCount,
        Vec(TrMatrixRegNBanks, Vec(64, UInt(8.W)))))
    private val Store_Output_Context = RegInit(0.U(1.W))
    private val Store_Pending_Lanes = RegInit(0.U(TrMatrixRegNBanks.W))
    // Full-line stores cannot transfer more than the architectural TR capacity.
    private val StoreMaxRequests = (Tensor_MN * Tensor_K + 63) / 64
    // Round up both dimensions: paired transpose reads may send an empty context.
    private val StoreMaxBatches = ((Tensor_MN + TrMatrixRegNBanks - 1) / TrMatrixRegNBanks) *
        ((Tensor_K + 63) / 64)
    private val Store_Batch_Id = RegInit(0.U(log2Ceil(StoreMaxBatches + 1).W))
    private val TotalStoreRequestSize = RegInit(0.U(log2Ceil(StoreMaxRequests + 1).W))
    private val TotalStoreResponseSize = RegInit(0.U(log2Ceil(StoreMaxRequests + 1).W))
    private val StoreLineAddrWidth = MMUAddrWidth - 6
    private val Store_Row_Line_Addr = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Output_Line_Addr = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Entry_Line_Stride = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Line_Stride = StoreCommand.stride(MMUAddrWidth - 1, 6)

    private val Store_Element_LgBytes = StoreCommand.widths(1, 0)

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

    private def storeRowBytes(command: TrStoreCommand): UInt =
        Mux(command.widths === Bundles.MSew.e4, command.columns >> 1,
            command.columns.pad(MatrixRegMaxTensorDimBitSize + 2) << command.widths(1, 0))
    private val Store_Memory_Row_Count = Mux(StoreCommand.transpose, StoreCommand.columns, StoreCommand.rows)
    private val Store_Memory_Row_Byte_Count = Mux(StoreCommand.transpose,
        StoreCommand.rows.pad(MatrixRegMaxTensorDimBitSize + 2) << Store_Element_LgBytes,
        storeRowBytes(StoreCommand))
    private val Store_Active_Lanes = VecInit((0 until StoreReadLaneCount).map { context =>
        VecInit((0 until TrMatrixRegNBanks).map { lane =>
            CurrentStore_Row_Iter + lane.U +
                (if (context == 0) 0.U else
                    (TrMatrixRegEntryByteSize.U >> Store_Element_LgBytes)) < Store_Memory_Row_Count
        }).asUInt
    })

    io.command.ready := memorystore_state === s_store_idle
    io.done.valid := memorystore_state === s_store_end
    io.done.bits := true.B
    io.regId := StoreCommand.regId
    when(io.done.fire) { memorystore_state := s_store_idle }

    when(io.command.fire) {
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
        when(nonempty) {
            assert(newCommand.base(5, 0) === 0.U, "TR store requires a 64-byte aligned base")
            // TODO(tr-store-tail): add masked L2 writes or a reviewed RMW path.
            assert(bytes(5, 0) === 0.U && bytes =/= 0.U,
                "TR store requires each effective row to contain a multiple of 64 bytes; partial stores are not implemented")
            when(newCommand.widths === Bundles.MSew.e4) {
                assert(!newCommand.columns(0), "TR store does not support a partial e4 byte")
            }
            when(rows > 1.U) {
                assert(newCommand.stride(5, 0) === 0.U,
                    "TR store requires a 64-byte aligned stride")
                // TODO(tr-store-overlap): support overlapping rows only with reviewed write ordering.
                assert(newCommand.stride >= bytes,
                    "TR store does not support overlapping destination rows")
            }
        }
        StoreCommand := newCommand
        Store_Row_Line_Addr := newCommand.base(MMUAddrWidth - 1, 6)
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

    // The command is already latched throughout this state. Prepare the second
    // transpose context before capture/send without extending the dispatch cone.
    when (memorystore_state === s_store_read) {
        Store_Entry_Line_Stride := MuxLookup(Store_Element_LgBytes, 0.U)(
            (0 to 2).map { lg => lg.U ->
                (Store_Line_Stride << (log2Ceil(TrMatrixRegEntryByteSize) - lg)) })
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
    when(io.read.BankAddr.fire) {
        // Use the existing SRAM read cycle to prepare the output line base.
        Store_Output_Line_Addr := Store_Row_Line_Addr + (CurrentStore_Byte_Offset >> 6)
        memorystore_state := s_store_capture
    }
    when(memorystore_state === s_store_capture) {
        assert(io.read.Data.valid, "TR store requires the accepted SRAM read to return one cycle later")
        when(io.read.Data.valid) {
            when(CurrentStore_Phase +& 1.U === Store_Phase_Count) {
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
    when(memorystore_state === s_store_capture && io.read.Data.valid && !StoreCommand.transpose) {
        for (bank <- 0 until TrMatrixRegNBanks;
             readLane <- 0 until StoreReadLaneCount;
             byte <- 0 until TrMatrixRegEntryByteSize) {
            Store_Data_Buffer(0)(bank)(readLane * TrMatrixRegEntryByteSize + byte) :=
                io.read.Data.bits(bank)(readLane)(8 * byte + 7, 8 * byte)
        }
    }

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
        when(CurrentStore_Byte_Offset +& 64.U === Store_Memory_Row_Byte_Count) {
            CurrentStore_Byte_Offset := 0.U
            CurrentStore_Row_Iter := nextStoreRowIter
            val skipEntry = StoreCommand.transpose &&
                nextStoreRowIter =/= CurrentStore_Row_Iter + TrMatrixRegNBanks.U
            Store_Row_Line_Addr := Store_Row_Line_Addr +
                (Store_Line_Stride << log2Ceil(TrMatrixRegNBanks)) +
                Mux(skipEntry, Store_Entry_Line_Stride, 0.U)
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
            when(StoreCommand.transpose && Store_Output_Context === 0.U) {
                Store_Output_Context := 1.U
                Store_Output_Line_Addr := Store_Output_Line_Addr + Store_Entry_Line_Stride
                Store_Pending_Lanes := Store_Active_Lanes(1)
            }.otherwise {
                finishStoreRowGroup()
            }
        } else {
            finishStoreRowGroup()
        }
    }

    private val BankStoreRequest = Wire(Vec(TrMatrixRegNBanks, Decoupled(new MMURequestIO)))
    for (bank <- 0 until TrMatrixRegNBanks) {
        BankStoreRequest(bank).valid := false.B
        BankStoreRequest(bank).bits := 0.U.asTypeOf(BankStoreRequest(bank).bits)
    }
    for (group <- 0 until requestChannelCount) {
        val banks = ResponseChannelHelper.banksInGroup(group, requestChannelCount, TrMatrixRegNBanks)
        val arb = Module(new Arbiter(new MMURequestIO, banks.size))
            .suggestName(s"${nameContext}_store_group${group}_arb")
        for ((bank, idx) <- banks.zipWithIndex) { arb.io.in(idx) <> BankStoreRequest(bank) }
        io.memory.Request(group).valid := arb.io.out.valid
        io.memory.Request(group).bits := arb.io.out.bits
        arb.io.out.ready := io.memory.Request(group).ready
    }
    for (lane <- requestChannelCount until TrMatrixRegNBanks) {
        io.memory.Request(lane).valid := false.B
        io.memory.Request(lane).bits := 0.U.asTypeOf(io.memory.Request(lane).bits)
    }

    private val StoreRequestFire = Wire(Vec(TrMatrixRegNBanks, Bool()))
    private val StoreResponseFire = Wire(Vec(TrMatrixRegNBanks, Bool()))
    for (lane <- 0 until TrMatrixRegNBanks) {
        val request = BankStoreRequest(lane)
        request.valid := memorystore_state === s_store_send && Store_Pending_Lanes(lane)
        request.bits := 0.U.asTypeOf(request.bits)
        // Multirow strides and every active line are aligned; a single row only
        // uses lane zero, so its unconstrained stride never contributes.
        val lineAddress = Store_Output_Line_Addr + lane.U * Store_Line_Stride
        request.bits.RequestAddr := Cat(lineAddress(StoreLineAddrWidth - 1, 0), 0.U(6.W))
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
    when(StoreRequestFire.asUInt.orR) {
        TotalStoreRequestSize := TotalStoreRequestSize + PopCount(StoreRequestFire)
    }
    when(StoreResponseFire.asUInt.orR) {
        TotalStoreResponseSize := TotalStoreResponseSize + PopCount(StoreResponseFire)
    }
    when(memorystore_state =/= s_store_idle) {
        assert(TotalStoreRequestSize +& PopCount(StoreRequestFire) <= StoreMaxRequests.U,
            "TR store request count exceeds the register capacity")
        assert(Store_Batch_Id <= StoreMaxBatches.U, "TR store batch counter overflow")
        assert(TotalStoreResponseSize +& PopCount(StoreResponseFire) <=
            TotalStoreRequestSize +& PopCount(StoreRequestFire),
            "TR store received more acknowledgements than accepted requests")
    }
    when(memorystore_state === s_store_send) {
        Store_Pending_Lanes := Store_Pending_Lanes & ~StoreRequestFire.asUInt
        when((Store_Pending_Lanes & ~StoreRequestFire.asUInt) === 0.U) {
            completeStoreOutputContext()
        }
    }
    when(memorystore_state === s_store_drain && TotalStoreResponseSize === TotalStoreRequestSize) {
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

class MultiChannelTrLoader(
    label: String = "trLoader0",
    contextName: String = "",
    emitDifftest: Boolean = true
)(implicit p: Parameters) extends CuteModule{
    private val nameContext = VerilogNameHelper.sanitize(if (contextName.nonEmpty) contextName else label)
    override def desiredName: String = s"MultiChannelTrLoader_${nameContext}"

    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
        val running = Output(Bool())
        val perfProbe = Output(new TrLoadPerfProbe)
    })

    val timer = RegInit(0.U(log2Ceil(50000).W))
    timer := timer + 1.U
    io.perfProbe := 0.U.asTypeOf(new TrLoadPerfProbe)
    private def log(s: Printable, end: String="\n"): Unit = if (YJPTrLoader0DebugEnable) printf(cf"[$timer][$label] " + s + end)
    private val loaderSource = if (label == "trLoader1") 1.U(1.W) else 0.U(1.W)

    val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    val CurrentMatrixRegId = RegInit(0.U(TrMatrixRegIdWidth.W))
    val ConfigInfo = io.ConfigInfo
    val BaseVAddr = RegInit(0.U(MMUAddrWidth.W))
    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Conherent = RegInit(true.B)
    val Stride = RegInit(0.U((MMUAddrWidth).W))

    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)

    val MAX_Fill_Times = outsideDataWidthByte / TrMatrixRegEntryByteSize
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val BankIdWidth = log2Ceil(TrMatrixRegNBanks)
    val RegAddrWidth = log2Ceil(TrMatrixRegBankNEntries)
    val TailBitOffset = BankIdWidth + RegAddrWidth
    require(TailBitOffset <= 60,
        s"[$label] normal source id tail bit exceeds safe range: $TailBitOffset")
    println(s"[$label] BankIdWidth $BankIdWidth, RegAddrWidth $RegAddrWidth, TailBitOffset $TailBitOffset")

    private val normalScheduleFullWidth =
        (if (label == "trLoader1") TrLoader1ResponseChannelCount else TrLoader0ResponseChannelCount) ==
            TrMatrixRegNBanks && TrMatrixRegNBanks == L2NBanks && L2NBanks == 8

    // A non-bijective permutation falls back to natural order for this task.
    val batchPermutationValid = RegInit(true.B)

    val currentM = Seq.tabulate(TrMatrixRegNBanks)(i => RegInit(i.U(MatrixRegMaxTensorDimBitSize.W)))
    val currentK = Seq.fill(TrMatrixRegNBanks)(RegInit(0.U(MatrixRegMaxTensorDimBitSize.W)))
    private val bankRespMetaWidth = RegAddrWidth + 1
    class LoaderReadReq extends Bundle {
        val addr = UInt(MMUAddrWidth.W)
        val coherent = Bool()
        val sourceId = UInt(64.W)
        val mask = UInt(MMUMaskWidth.W)
    }

    class BankRespFifo(bankIdx: Int) {
        val dataQ = Module(
            new Queue(UInt(outsideDataWidth.W), TrLoadResponseFifoDepth, pipe = true, flow = true)
        )
            .suggestName(s"${nameContext}_bank${bankIdx}_resp_data_fifo")
        val metaQ = Module(
            new Queue(UInt(bankRespMetaWidth.W), TrLoadResponseFifoDepth, pipe = true, flow = true)
        )
            .suggestName(s"${nameContext}_bank${bankIdx}_resp_meta_fifo")

        val curRemain = RegInit(MAX_Fill_Times.U((log2Ceil(MAX_Fill_Times) + 1).W))

        dataQ.io.enq.valid := false.B
        dataQ.io.enq.bits := 0.U
        dataQ.io.deq.ready := false.B
        metaQ.io.enq.valid := false.B
        metaQ.io.enq.bits := 0.U
        metaQ.io.deq.ready := false.B

        assert(
            dataQ.io.enq.ready === metaQ.io.enq.ready,
            s"[$label] bank $bankIdx response enqueue queues diverged"
        )
        assert(
            dataQ.io.deq.valid === metaQ.io.deq.valid,
            s"[$label] bank $bankIdx response dequeue queues diverged"
        )

        def readyForResp: Bool = dataQ.io.enq.ready && metaQ.io.enq.ready

        def enqFromResp(sourceId: UInt, respData: UInt, isTail: Bool): Unit = {
            dataQ.io.enq.valid := true.B
            dataQ.io.enq.bits := respData
            metaQ.io.enq.valid := true.B
            metaQ.io.enq.bits := Cat(isTail, sourceId(RegAddrWidth - 1, 0))
            log(cf"Response[$bankIdx] enqueue sourceId=$sourceId isTail=$isTail")
        }

        def stepWriteback(toMReg: TrLoadMatrixRegIO, tailMask: Vec[UInt]): UInt = {
            val rowsWritten = WireInit(0.U(2.W))
            val sliceIdx = MAX_Fill_Times.U - curRemain
            val slices = Wire(Vec(MAX_Fill_Times, UInt((8 * TrMatrixRegEntryByteSize).W)))
            val meta = metaQ.io.deq.bits
            val baseAddr = meta(RegAddrWidth - 1, 0)
            val isTail = meta(bankRespMetaWidth - 1)
            slices := dataQ.io.deq.bits.asTypeOf(slices)

            when (dataQ.io.deq.valid) {
                val rowsThisCycle = Mux(curRemain > 1.U, 2.U, 1.U)
                for (lane <- 0 until 2) {
                    val laneValid = curRemain > lane.U
                    val laneSlice = if (MAX_Fill_Times == 1) 0.U else
                        Mux(laneValid, sliceIdx + lane.U, 0.U)(log2Ceil(MAX_Fill_Times) - 1, 0)
                    val write = toMReg.WriteRequest(bankIdx)(lane)
                    write.BankAddr.bits := baseAddr + laneSlice
                    write.BankAddr.valid := laneValid
                    write.Data.bits := slices(laneSlice)
                    write.Data.valid := laneValid
                    write.ByteMask.valid := laneValid
                    write.ByteMask.bits := Mux(isTail, tailMask(laneSlice), Fill(TrMatrixRegEntryByteSize, true.B))

                }
                rowsWritten := rowsThisCycle
                when (curRemain > 2.U) {
                    curRemain := curRemain - rowsThisCycle
                }.otherwise {
                    curRemain := MAX_Fill_Times.U
                    dataQ.io.deq.ready := true.B
                    metaQ.io.deq.ready := true.B
                }
            }

            rowsWritten
        }
    }

    val bankFifos = Seq.tabulate(TrMatrixRegNBanks)(i => new BankRespFifo(i))
    private val normalReqQueueDepth = 2
    val normalReqQueues = Seq.tabulate(TrMatrixRegNBanks) { bankIdx =>
        Module(new Queue(new LoaderReadReq, normalReqQueueDepth, pipe = false, flow = false))
            .suggestName(s"${nameContext}_bank${bankIdx}_req_queue")
    }
    val normalReqQueueOccupancy = Seq.fill(TrMatrixRegNBanks)(
        RegInit(0.U(log2Ceil(normalReqQueueDepth + 1).W))
    )
    val normalLoadTotalFired = RegInit(0.U((log2Ceil(Tensor_MN * Tensor_K * 2).max(1)).W))
    val normalFireVec = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(false.B)))
    val normalPerfectBatch = WireInit(false.B)

    io.perfProbe.loadTotalFired := normalLoadTotalFired
    io.perfProbe.loadPerfectBatches := 0.U
    io.perfProbe.loadPerfectRequests := 0.U
    io.perfProbe.loadPerfectBatch := normalPerfectBatch
    io.perfProbe.seedStatus := ABPerfectBatchSeed.Disabled.U
    io.perfProbe.seedVector := 0.U.asTypeOf(io.perfProbe.seedVector)
    io.perfProbe.seedAttempts := 0.U
    io.perfProbe.seedSuccesses := 0.U
    io.perfProbe.seedFallbacks := 0.U
    io.perfProbe.windowRepairs := 0.U

    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    def stepLoadInit(): Unit = {
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        TotalRequestSize := 0.U
        for (i <- 0 until TrMatrixRegNBanks) {
            currentM(i) := i.U
            currentK(i) := 0.U
            normalReqQueueOccupancy(i) := 0.U
        }
        normalLoadTotalFired := 0.U
        MaxRequestIter := MatrixRegTensor_M * K_Beat_Count
    }

    def stepLoadWorking(): Unit = {
        log(
          cf"working M=$MatrixRegTensor_M K=$MatrixRegTensor_K beat=$K_Beat_Count " +
          cf"stride=$Stride tail=$HasTail totalReq=$TotalRequestSize totalLoad=$TotalLoadSize"
        )

        val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(TrMatrixRegNBanks)(0.U(2.W))))

        val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
        val tailByteMaskPerSlot = VecInit((0 until MAX_Fill_Times).map { j =>
            tailTaskMask((j + 1) * TrMatrixRegEntryByteSize - 1, j * TrMatrixRegEntryByteSize)
        })

        def stepNormalQueueRequests(): Unit = {
            for (i <- 0 until TrMatrixRegNBanks) {
                val reqQueue = normalReqQueues(i)
                val request = io.LocalMMUIO.Request(i)
                // currentM is traversal position, not the row's data identity.
                // Schedule complete groups only; a partial final group remains last.
                val traversalRow = currentM(i)
                val completeBatchCount = MatrixRegTensor_M / TrMatrixRegNBanks.U
                val traversalBatch = traversalRow / TrMatrixRegNBanks.U
                val scheduleActive = ABBatchScheduleEnable.B && normalScheduleFullWidth.B &&
                    traversalBatch < completeBatchCount && batchPermutationValid
                val physicalBatch = ABBatchSchedule.batch(traversalBatch, completeBatchCount,
                    ABBatchScheduleMode, ABBatchRotateOffset, ABBatchPermutationMul)
                val physicalRow = Mux(scheduleActive,
                    physicalBatch * TrMatrixRegNBanks.U + i.U, traversalRow)
                // Response tags name the canonical row selected for this request.
                // Keeping traversalBatch in the tag here would silently permute GEMM data.
                val logicalRow = physicalRow
                val mIter = logicalRow
                val kIter = currentK(i)
                val scheduledBeat = kIter
                val inRange = traversalRow < MatrixRegTensor_M && kIter < K_Beat_Count
                val queueHasRoom = normalReqQueueOccupancy(i) < normalReqQueueDepth.U
                val issueFire = inRange && queueHasRoom
                val requestBeatIsTail = HasTail && (scheduledBeat === (K_Beat_Count - 1.U))
                val regAddr = (mIter / TrMatrixRegNBanks.U) * ReduceGroupSize.U +
                  (scheduledBeat << log2Ceil(MAX_Fill_Times))
                val sourceId = Cat(requestBeatIsTail, i.U(BankIdWidth.W), regAddr(RegAddrWidth - 1, 0))

                reqQueue.io.enq.valid := issueFire
                reqQueue.io.enq.bits := 0.U.asTypeOf(reqQueue.io.enq.bits)
                reqQueue.io.enq.bits.addr := BaseVAddr + physicalRow * Stride +
                  (scheduledBeat << log2Ceil(outsideDataWidthByte))
                reqQueue.io.enq.bits.coherent := Conherent
                reqQueue.io.enq.bits.mask := Fill(MMUMaskWidth, 1.U(1.W))
                reqQueue.io.enq.bits.sourceId := sourceId

                request.valid := reqQueue.io.deq.valid
                request.bits := 0.U.asTypeOf(request.bits)
                request.bits.RequestAddr := reqQueue.io.deq.bits.addr
                request.bits.RequestConherent := reqQueue.io.deq.bits.coherent
                request.bits.RequestMask := reqQueue.io.deq.bits.mask
                request.bits.RequestSourceID := reqQueue.io.deq.bits.sourceId
                reqQueue.io.deq.ready := request.ready

                val requestDeqFire = request.valid && request.ready
                normalFireVec(i) := requestDeqFire
                normalReqQueueOccupancy(i) :=
                    normalReqQueueOccupancy(i) + issueFire.asUInt - requestDeqFire.asUInt

                when(issueFire) {
                    when(kIter + 1.U === K_Beat_Count) {
                        currentK(i) := 0.U
                        currentM(i) := traversalRow + TrMatrixRegNBanks.U
                    }.otherwise {
                        currentK(i) := kIter + 1.U
                    }
                    if (YJPTrLoader0DebugEnable) {
                        log(cf"NormalReq bank=$i m=$mIter k=$kIter addr=${reqQueue.io.enq.bits.addr}%x reg=$regAddr tail=$requestBeatIsTail")
                    }
                }
            }
        }

        def stepNormalResponsesAndWriteback(): Unit = {
            for (i <- 0 until TrMatrixRegNBanks) {
                io.LocalMMUIO.Response(i).ready := bankFifos(i).readyForResp

                // Keep enqueue valid independent of ready for flow-through queues.
                when(io.LocalMMUIO.Response(i).valid) {
                    val respSourceId = io.LocalMMUIO.Response(i).bits.ReseponseSourceID
                    val respIsTail = respSourceId(TailBitOffset)
                    bankFifos(i).enqFromResp(respSourceId, io.LocalMMUIO.Response(i).bits.ReseponseData,
                      respIsTail)

                    if (YJPTrLoader0DebugEnable) {
                        log(cf"NormalResp bank=$i source=$respSourceId tail=$respIsTail")
                    }
                }

                Current_Fill_MReg_Time(i) := bankFifos(i).stepWriteback(io.ToMatrixRegIO, tailByteMaskPerSlot)
            }
        }

        when(Is_ZeroLoad) {
            val Max_ZeroLoad_Write_Times = TrMatrixRegBankNEntries
            for (i <- 0 until TrMatrixRegNBanks) {
                io.ToMatrixRegIO.BankAddr(i).bits := TotalLoadSize
                io.ToMatrixRegIO.BankAddr(i).valid := true.B
                io.ToMatrixRegIO.Data(i).bits := 0.U
                io.ToMatrixRegIO.Data(i).valid := true.B
                io.ToMatrixRegIO.ByteMask(i).bits := Fill(TrMatrixRegEntryByteSize, true.B)
                io.ToMatrixRegIO.ByteMask(i).valid := true.B
            }
            TotalLoadSize := TotalLoadSize + 1.U
            if (YJPTrLoader0DebugEnable) log(cf"ZeroLoad TotalLoadSize=$TotalLoadSize")
            when(TotalLoadSize === (Max_ZeroLoad_Write_Times - 1).U) {
                memoryload_state := s_load_end
                if (YJPTrLoader0DebugEnable) log(cf"ZeroLoadEnd")
            }
        }

        when(Is_FullLoad) {
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad)) === 1.U,
                "AB loader task must select exactly one load type")
            stepNormalQueueRequests()
            stepNormalResponsesAndWriteback()
            val rowsWritten = Current_Fill_MReg_Time.reduce(_ +& _)
            TotalLoadSize := TotalLoadSize + rowsWritten
            val expectedRows = MatrixRegTensor_M * K_Beat_Count * MAX_Fill_Times.U
            when(TotalLoadSize === expectedRows) { memoryload_state := s_load_end }
            val firedCount = PopCount(normalFireVec)
            normalLoadTotalFired := normalLoadTotalFired + firedCount
            normalPerfectBatch := normalFireVec.asUInt.andR
        }
    }

    def stepLoadEnd(): Unit = {
        ConfigInfo.MicroTaskEndValid := true.B
        when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady) {

            memoryload_state := s_load_idle
            state := s_idle
            if (YJPTrLoader0DebugEnable) log(cf"TaskEnd")
        }
    }

    def acceptMicroTaskInIdle(): Unit = {
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady) {
            state := s_mm_task
            memoryload_state := s_load_init

            MatrixRegTensor_M := ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := ConfigInfo.MatrixRegId

            BaseVAddr := ConfigInfo.Tensor.baseVaddr
            Stride := ConfigInfo.Tensor.stride

            HasTail := ConfigInfo.Tensor.HasTail
            TailByteMask := ConfigInfo.Tensor.TailByteMask
            K_Beat_Count := ConfigInfo.Tensor.K_Beat_Count
            assert(!ConfigInfo.Is_Transpose, s"[$label] transpose is disabled in this baseline")
            if (ABBatchScheduleEnable && normalScheduleFullWidth && ABBatchScheduleMode == ABBatchSchedule.Permute) {
                val completeBatches = ConfigInfo.MatrixRegTensor_M / TrMatrixRegNBanks.U
                val validCounts = (1 to (Tensor_MN / TrMatrixRegNBanks)).filter(
                    count => ABBatchSchedule.coprime(ABBatchPermutationMul, count))
                batchPermutationValid := completeBatches === 0.U || validCounts.map(
                    count => completeBatches === count.U).reduceOption(_ || _).getOrElse(false.B)
            }

            Is_ZeroLoad := ConfigInfo.LoadTaskInfo.Is_ZeroLoad
            Is_FullLoad := ConfigInfo.LoadTaskInfo.Is_FullLoad
            Conherent := ConfigInfo.Conherent



            if (YJPTrLoader0DebugEnable) {
                log(cf"Config M=${ConfigInfo.MatrixRegTensor_M} K=${ConfigInfo.MatrixRegTensor_K} transpose=${ConfigInfo.Is_Transpose} tail=${ConfigInfo.Tensor.HasTail}")
            }
        }
    }

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.WriteRequest := 0.U.asTypeOf(io.ToMatrixRegIO.WriteRequest)
    io.ToMatrixRegIO.WriteRequest.foreach(_.foreach { write =>
        write.ByteMask.bits := Fill(TrMatrixRegEntryByteSize, true.B)
    })

    for (i <- 0 until TrMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LocalMMUIO.Request(i).bits)
        io.LocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LocalMMUIO.Request(i).bits.IsTranspose := false.B
        io.LocalMMUIO.Response(i).ready := false.B

        normalReqQueues(i).io.enq.valid := false.B
        normalReqQueues(i).io.enq.bits := 0.U.asTypeOf(normalReqQueues(i).io.enq.bits)
        normalReqQueues(i).io.deq.ready := false.B
    }

    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
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
        val diffIndexMap = Map(
            "trLoader0" -> 0,
            "trLoader1" -> 1
        )
        difftestAmuFinish.index := diffIndexMap.getOrElse(label, 0xdeadabab).U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
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
        difftestAmuFinishLane1.index := (if (label == "trLoader1") 1.U else 0.U)
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

    io.running := false.B

    when(state === s_idle) {
        acceptMicroTaskInIdle()
    }.otherwise {
        io.running := true.B
    }

    when(memoryload_state === s_load_init) {
        stepLoadInit()
    }.elsewhen(memoryload_state === s_load_working) {
        io.ToMatrixRegIO.active := true.B
        stepLoadWorking()
    }.elsewhen(memoryload_state === s_load_end) {
        stepLoadEnd()
    }
}
