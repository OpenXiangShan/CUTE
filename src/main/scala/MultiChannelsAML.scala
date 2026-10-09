
package cute

import chisel3._
import chisel3.util._
import difftest._
import difftest.util.MatrixHash
import org.chipsalliance.cde.config._

class MultiChannelsABMemLoader(
    label: String = "AML",
    contextName: String = "",
    emitDifftest: Boolean = true
)(implicit p: Parameters) extends CuteModule{
    private val nameContext = VerilogNameHelper.sanitize(if (contextName.nonEmpty) contextName else label)
    override def desiredName: String = s"MultiChannelsABMemLoader_${nameContext}"

    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
        val running = Output(Bool())
    })

    val timer = RegInit(0.U(log2Ceil(50000).W))
    timer := timer + 1.U
    private def log(s: Printable, end: String="\n"): Unit = if (YJPAMLDebugEnable) printf(cf"[$timer][$label] " + s + end)

    val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
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

    val MAX_Fill_Times = outsideDataWidthByte / ABMatrixRegEntryByteSize
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val BankIdWidth = log2Ceil(ABMatrixRegNBanks)
    val RegAddrWidth = log2Ceil(ABMatrixRegBankNEntries)
    val TailBitOffset = BankIdWidth + RegAddrWidth
    require(TailBitOffset <= 60,
        s"[$label] normal source id tail bit exceeds safe range: $TailBitOffset")
    println(s"[$label] BankIdWidth $BankIdWidth, RegAddrWidth $RegAddrWidth, TailBitOffset $TailBitOffset")

    val currentM = Seq.tabulate(ABMatrixRegNBanks)(i => RegInit(i.U(MatrixRegMaxTensorDimBitSize.W)))
    val currentK = Seq.fill(ABMatrixRegNBanks)(RegInit(0.U(MatrixRegMaxTensorDimBitSize.W)))
    private val bankRespMetaWidth = RegAddrWidth + 1
    class LoaderReadReq extends Bundle {
        val addr = UInt(MMUAddrWidth.W)
        val coherent = Bool()
        val sourceId = UInt(64.W)
        val mask = UInt(MMUMaskWidth.W)
    }

    class BankRespFifo(bankIdx: Int) {
        val dataQ = Module(
            new Queue(UInt(outsideDataWidth.W), ABMultiResponseFifoDepth, pipe = true, flow = true)
        )
            .suggestName(s"${nameContext}_bank${bankIdx}_resp_data_fifo")
        val metaQ = Module(
            new Queue(UInt(bankRespMetaWidth.W), ABMultiResponseFifoDepth, pipe = true, flow = true)
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

        def stepWriteback(toMReg: ABMemoryLoaderMatrixRegIO, tailMask: Vec[UInt]): Bool = {
            val haveWritten = WireInit(false.B)
            val sliceIdx = MAX_Fill_Times.U - curRemain
            val slices = Wire(Vec(MAX_Fill_Times, UInt((8 * ABMatrixRegEntryByteSize).W)))
            val meta = metaQ.io.deq.bits
            val baseAddr = meta(RegAddrWidth - 1, 0)
            val isTail = meta(bankRespMetaWidth - 1)
            slices := dataQ.io.deq.bits.asTypeOf(slices)

            when (dataQ.io.deq.valid) {
                toMReg.BankAddr(bankIdx).bits := baseAddr + sliceIdx
                toMReg.BankAddr(bankIdx).valid := true.B
                toMReg.Data(bankIdx).bits := slices(sliceIdx)
                toMReg.Data(bankIdx).valid := true.B
                toMReg.ByteMask(bankIdx).valid := true.B
                toMReg.ByteMask(bankIdx).bits := Mux(isTail, tailMask(sliceIdx), Fill(ABMatrixRegEntryByteSize, true.B))
                haveWritten := true.B
                when (curRemain > 1.U) {
                    curRemain := curRemain - 1.U
                }.elsewhen (curRemain === 1.U) {
                    curRemain := MAX_Fill_Times.U
                }
            }

            when (curRemain === 1.U) {
                dataQ.io.deq.ready := true.B
                metaQ.io.deq.ready := true.B
            }

            haveWritten
        }
    }

    val bankFifos = Seq.tabulate(ABMatrixRegNBanks)(i => new BankRespFifo(i))
    private val normalReqQueueDepth = 2
    val normalReqQueues = Seq.tabulate(ABMatrixRegNBanks) { bankIdx =>
        Module(new Queue(new LoaderReadReq, normalReqQueueDepth, pipe = false, flow = false))
            .suggestName(s"${nameContext}_bank${bankIdx}_req_queue")
    }
    val normalReqQueueOccupancy = Seq.fill(ABMatrixRegNBanks)(
        RegInit(0.U(log2Ceil(normalReqQueueDepth + 1).W))
    )

    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    def stepLoadInit(): Unit = {
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        TotalRequestSize := 0.U
        for (i <- 0 until ABMatrixRegNBanks) {
            currentM(i) := i.U
            currentK(i) := 0.U
            normalReqQueueOccupancy(i) := 0.U
        }
        MaxRequestIter := MatrixRegTensor_M * K_Beat_Count
    }

    def stepLoadWorking(): Unit = {
        log(
          cf"working M=$MatrixRegTensor_M K=$MatrixRegTensor_K beat=$K_Beat_Count " +
          cf"stride=$Stride tail=$HasTail totalReq=$TotalRequestSize totalLoad=$TotalLoadSize"
        )

        val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))

        val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
        val tailByteMaskPerSlot = VecInit((0 until MAX_Fill_Times).map { j =>
            tailTaskMask((j + 1) * ABMatrixRegEntryByteSize - 1, j * ABMatrixRegEntryByteSize)
        })

        when(Is_ZeroLoad) {
            val Max_ZeroLoad_Write_Times = ABMatrixRegBankNEntries
            for (i <- 0 until ABMatrixRegNBanks) {
                io.ToMatrixRegIO.BankAddr(i).bits := TotalLoadSize
                io.ToMatrixRegIO.BankAddr(i).valid := true.B
                io.ToMatrixRegIO.Data(i).bits := 0.U
                io.ToMatrixRegIO.Data(i).valid := true.B
                io.ToMatrixRegIO.ByteMask(i).bits := Fill(ABMatrixRegEntryByteSize, true.B)
                io.ToMatrixRegIO.ByteMask(i).valid := true.B
            }
            TotalLoadSize := TotalLoadSize + 1.U
            if (YJPAMLDebugEnable) log(cf"ZeroLoad TotalLoadSize=$TotalLoadSize")
            when(TotalLoadSize === (Max_ZeroLoad_Write_Times - 1).U) {
                memoryload_state := s_load_end
                if (YJPAMLDebugEnable) log(cf"ZeroLoadEnd")
            }
        }

        when(Is_FullLoad) {
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad)) === 1.U,
                "Error! AML Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad should be true!")

            for (i <- 0 until ABMatrixRegNBanks) {
                val reqQueue = normalReqQueues(i)
                val request = io.LocalMMUIO.Request(i)
                val mIter = currentM(i)
                val kIter = currentK(i)
                val inRange = mIter < MatrixRegTensor_M && kIter < K_Beat_Count
                val queueHasRoom = normalReqQueueOccupancy(i) < normalReqQueueDepth.U
                val issueFire = inRange && queueHasRoom
                val requestBeatIsTail = HasTail && (kIter === (K_Beat_Count - 1.U))
                val regAddr = (mIter / ABMatrixRegNBanks.U) * ReduceGroupSize.U + (kIter << log2Ceil(MAX_Fill_Times))
                val sourceId = Cat(requestBeatIsTail, i.U(BankIdWidth.W), regAddr(RegAddrWidth - 1, 0))

                reqQueue.io.enq.valid := issueFire
                reqQueue.io.enq.bits.addr := BaseVAddr + mIter * Stride + (kIter << log2Ceil(outsideDataWidthByte))
                reqQueue.io.enq.bits.coherent := Conherent
                reqQueue.io.enq.bits.mask := Fill(MMUMaskWidth, 1.U(1.W))
                reqQueue.io.enq.bits.sourceId := sourceId

                request.valid := reqQueue.io.deq.valid
                request.bits.RequestAddr := reqQueue.io.deq.bits.addr
                request.bits.RequestConherent := reqQueue.io.deq.bits.coherent
                request.bits.RequestData := 0.U
                request.bits.RequestSourceID := reqQueue.io.deq.bits.sourceId
                request.bits.RequestType_isWrite := false.B
                request.bits.UseAllocatedSourceID := false.B
                request.bits.isA := false.B
                request.bits.MatrixIsAcc := false.B
                request.bits.RequestMask := reqQueue.io.deq.bits.mask
                reqQueue.io.deq.ready := request.ready

                val requestDeqFire = request.valid && request.ready
                normalReqQueueOccupancy(i) :=
                    normalReqQueueOccupancy(i) + issueFire.asUInt - requestDeqFire.asUInt

                when(issueFire) {
                    when(kIter + 1.U === K_Beat_Count) {
                        currentK(i) := 0.U
                        currentM(i) := mIter + ABMatrixRegNBanks.U
                    }.otherwise {
                        currentK(i) := kIter + 1.U
                    }
                    if (YJPAMLDebugEnable) {
                        log(cf"NormalReq bank=$i m=$mIter k=$kIter addr=${reqQueue.io.enq.bits.addr}%x reg=$regAddr tail=$requestBeatIsTail")
                    }
                }

                io.LocalMMUIO.Response(i).ready := bankFifos(i).readyForResp
                when(io.LocalMMUIO.Response(i).fire) {
                    val respSourceId = io.LocalMMUIO.Response(i).bits.ReseponseSourceID
                    val respIsTail = respSourceId(TailBitOffset)
                    bankFifos(i).enqFromResp(respSourceId, io.LocalMMUIO.Response(i).bits.ReseponseData, respIsTail)
                    if (YJPAMLDebugEnable) {
                        log(cf"NormalResp bank=$i source=$respSourceId tail=$respIsTail")
                    }
                }

                when(bankFifos(i).stepWriteback(io.ToMatrixRegIO, tailByteMaskPerSlot)) {
                    Current_Fill_MReg_Time(i) := 1.U
                }
            }

            val Load_Size = PopCount(Current_Fill_MReg_Time.asUInt)
            TotalLoadSize := TotalLoadSize + Load_Size
            val ExpectedLoadSize = MatrixRegTensor_M * K_Beat_Count * MAX_Fill_Times.U
            when(TotalLoadSize === ExpectedLoadSize) {
                memoryload_state := s_load_end
                if (YJPAMLDebugEnable) log(cf"NormalLoadEnd TotalLoadSize=$TotalLoadSize")
            }
        }
    }

    def stepLoadEnd(): Unit = {
        ConfigInfo.MicroTaskEndValid := true.B
        when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady) {
            memoryload_state := s_load_idle
            state := s_idle
            if (YJPAMLDebugEnable) log(cf"TaskEnd")
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

            BaseVAddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
            Stride := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M

            HasTail := ConfigInfo.ApplicationTensor_A.HasTail
            TailByteMask := ConfigInfo.ApplicationTensor_A.TailByteMask
            K_Beat_Count := ConfigInfo.ApplicationTensor_A.K_Beat_Count
            assert(!ConfigInfo.Is_Transpose, s"[$label] transpose is disabled in this baseline")

            Is_ZeroLoad := ConfigInfo.LoadTaskInfo.Is_ZeroLoad
            Is_FullLoad := ConfigInfo.LoadTaskInfo.Is_FullLoad
            Conherent := ConfigInfo.Conherent

            if (YJPAMLDebugEnable) {
                log(cf"Config M=${ConfigInfo.MatrixRegTensor_M} K=${ConfigInfo.MatrixRegTensor_K} transpose=${ConfigInfo.Is_Transpose} tail=${ConfigInfo.ApplicationTensor_A.HasTail}")
            }
        }
    }

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.BankAddr)
    io.ToMatrixRegIO.Data := 0.U.asTypeOf(io.ToMatrixRegIO.Data)
    io.ToMatrixRegIO.ByteMask.map(_.valid := false.B)
    io.ToMatrixRegIO.ByteMask.map(_.bits := Fill(ABMatrixRegEntryByteSize, true.B))

    for (i <- 0 until ABMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LocalMMUIO.Request(i).bits)
        io.LocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
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
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_WORDS_PER_BANK", ABMatrixRegEntryBitSize / 64)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_REG_SIZE_BYTES", ABMatrixRegSize)
        val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = MatrixHash(ABMatrixRegNBanks, ABMatrixRegEntryByteSize, ABMatrixRegSize, Tensor_K)
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        val diffIndexMap = Map(
            "AML" -> 0,
            "BML" -> 1
        )
        difftestAmuFinish.index := diffIndexMap.getOrElse(label, 0xdeadabab).U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
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
