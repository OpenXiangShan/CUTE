
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._

class MultiChannelsABMemLoader(
    label: String = "AML",
    contextName: String = ""
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

    val s_load_idle :: s_load_init :: s_load_working :: s_load_quiesce :: s_load_end :: Nil = Enum(5)
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
    val Is_Transpose = RegInit(false.B)

    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)

    val MAX_Fill_Times = outsideDataWidthByte / ABMatrixRegEntryByteSize
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val BankIdWidth = log2Ceil(ABMatrixRegNBanks)
    val RegAddrWidth = log2Ceil(ABMatrixRegBankNEntries)
    val TailBitOffset = BankIdWidth + RegAddrWidth
    val BeatIndexWidth = log2Ceil(ABMatrixRegEntryByteSize)
    val TransposeTailBitOffset = RegAddrWidth + BankIdWidth + BeatIndexWidth
    require(TailBitOffset <= 60,
        s"[$label] normal source id tail bit exceeds safe range: $TailBitOffset")
    require(TransposeTailBitOffset <= 60,
        s"[$label] transpose source id tail bit exceeds safe range: $TransposeTailBitOffset")
    println(s"[$label] BankIdWidth $BankIdWidth, RegAddrWidth $RegAddrWidth, TailBitOffset $TailBitOffset")

    val currentM = Seq.tabulate(ABMatrixRegNBanks)(i => RegInit(i.U(MatrixRegMaxTensorDimBitSize.W)))
    val currentK = Seq.fill(ABMatrixRegNBanks)(RegInit(0.U(MatrixRegMaxTensorDimBitSize.W)))

    class BankRespFifo(bankIdx: Int) {
        class Entry extends Bundle {
            val data = UInt(outsideDataWidth.W)
            val baseAddr = UInt(log2Ceil(ABMatrixRegBankNEntries).W)
            val isTail = Bool()
        }

        val q = Module(new Queue(new Entry, 128, pipe=true, flow=true))
            .suggestName(s"${nameContext}_bank${bankIdx}_resp_fifo")

        val processing = RegInit(false.B)
        val curRemain = RegInit(MAX_Fill_Times.U((log2Ceil(MAX_Fill_Times) + 1).W))

        q.io.enq.valid := false.B
        q.io.enq.bits := 0.U.asTypeOf(q.io.enq.bits)
        q.io.deq.ready := false.B

        def readyForResp: Bool = q.io.enq.ready

        def enqFromResp(sourceId: UInt, respData: UInt, isTail: Bool): Unit = {
            q.io.enq.valid := true.B
            q.io.enq.bits.data := respData
            q.io.enq.bits.baseAddr := sourceId(RegAddrWidth - 1, 0)
            q.io.enq.bits.isTail := isTail
            log(cf"Response[$bankIdx] enqueue sourceId=$sourceId isTail=$isTail")
        }

        def stepWriteback(toMReg: ABMemoryLoaderMatrixRegIO, tailMask: Vec[UInt]): Bool = {
            val haveWritten = WireInit(false.B)
            val sliceIdx = MAX_Fill_Times.U - curRemain
            val slices = Wire(Vec(MAX_Fill_Times, UInt((8 * ABMatrixRegEntryByteSize).W)))
            slices := q.io.deq.bits.data.asTypeOf(slices)

            when (q.io.deq.valid) {
                toMReg.BankAddr(bankIdx).bits := q.io.deq.bits.baseAddr + sliceIdx
                toMReg.BankAddr(bankIdx).valid := true.B
                toMReg.Data(bankIdx).bits := slices(sliceIdx)
                toMReg.Data(bankIdx).valid := true.B
                toMReg.ByteMask(bankIdx).valid := true.B
                toMReg.ByteMask(bankIdx).bits := Mux(q.io.deq.bits.isTail, tailMask(sliceIdx), Fill(ABMatrixRegEntryByteSize, true.B))
                haveWritten := true.B
                when (curRemain > 1.U) {
                    curRemain := curRemain - 1.U
                }.elsewhen (curRemain === 1.U) {
                    curRemain := MAX_Fill_Times.U
                }
            }

            when (curRemain === 1.U) {
                q.io.deq.ready := true.B
            }

            haveWritten
        }
    }

    val bankFifos = Seq.tabulate(ABMatrixRegNBanks)(i => new BankRespFifo(i))
    private val normalReqQueueDepth = 2
    val normalReqQueues = Seq.tabulate(ABMatrixRegNBanks) { bankIdx =>
        Module(new Queue(new MMURequestIO, normalReqQueueDepth, pipe = false, flow = false))
            .suggestName(s"${nameContext}_bank${bankIdx}_req_queue")
    }
    val normalReqQueueOccupancy = Seq.fill(ABMatrixRegNBanks)(
        RegInit(0.U(log2Ceil(normalReqQueueDepth + 1).W))
    )

    val transAlignPipes = Seq.tabulate(ABMatrixRegNBanks) { i =>
        Module(new TransAlignPipe(i)).suggestName(s"${nameContext}_bank${i}_trans_align_pipe")
    }
    val transRouters = Seq.tabulate(ABMatrixRegNBanks) { i =>
        Module(new OOORouter).suggestName(s"${nameContext}_bank${i}_trans_router")
    }
    val transPipeInValid = WireInit(false.B)
    val transPipeInData = WireInit(0.U(outsideDataWidth.W))
    val transPipeInMask = WireInit(0.U(outsideDataWidthByte.W))
    val transPipeRespBeatCnt = WireInit(0.U((BeatIndexWidth + 1).W))
    val transPipeEntryOffset = WireInit(0.U(BeatIndexWidth.W))
    val transPipeDrainTrigger = WireInit(false.B)
    val transBusStall = transAlignPipes.map(_.io.bus_stall).reduce(_ || _)
    val transAlignEmpty = transAlignPipes.map(_.io.empty).reduce(_ && _)
    val transRouterEmpty = transRouters.map(_.io.empty).reduce(_ && _)
    val transPipelineEmpty = transAlignEmpty && transRouterEmpty
    val transRouterValidVec = VecInit(transRouters.map(_.io.valid)).asUInt
    val transRouterWriteValid = transRouters.map(_.io.valid).reduce(_ || _)

    private val transBaseAddrBits = RegAddrWidth
    val transWriteBaseAddr = RegInit(0.U(transBaseAddrBits.W))
    val transWriteAddrCnt = RegInit(0.U(log2Ceil(Trans_Load_Size).W))
    val transWriteAddrOffset = transWriteAddrCnt * ReduceGroupSize.U
    val transWriteAddrWide = transWriteBaseAddr + transWriteAddrOffset
    val transWriteAddr = transWriteAddrWide(transBaseAddrBits - 1, 0)

    for (i <- 0 until ABMatrixRegNBanks) {
        transAlignPipes(i).io.in_data := transPipeInData
        transAlignPipes(i).io.in_mask := transPipeInMask
        transAlignPipes(i).io.resp_beat_cnt := transPipeRespBeatCnt
        transAlignPipes(i).io.entry_offset := transPipeEntryOffset
        transAlignPipes(i).io.debug_time := io.DebugInfo.DebugTimeStampe
        transAlignPipes(i).io.is_drain_trigger := transPipeDrainTrigger
        transAlignPipes(i).io.in_valid := transPipeInValid
        transRouters(i).io.in <> transAlignPipes(i).io.out
    }

    val Request_M_Iter_Time = RegInit(0.U(BeatIndexWidth.W))
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val group_req_cnt = RegInit(0.U((BeatIndexWidth + 1).W))
    val group_resp_cnt = RegInit(0.U((BeatIndexWidth + 1).W))
    val group_size_reg = RegInit(0.U((BeatIndexWidth + 1).W))

    private val transposeEndDrainCycles = Trans_Load_Size + 2 + 3
    val transposeEndDrainCnt = RegInit(0.U(log2Ceil(transposeEndDrainCycles + 1).W))

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
        Request_M_Iter_Time := 0.U
        CurrentLoaded_BlockTensor_M_Iter := 0.U
        CurrentLoaded_BlockTensor_K_Iter := 0.U
        group_req_cnt := 0.U
        group_resp_cnt := 0.U
        group_size_reg := 0.U
        transposeEndDrainCnt := 0.U
        transWriteBaseAddr := 0.U
        transWriteAddrCnt := 0.U
        MaxRequestIter := MatrixRegTensor_M * K_Beat_Count
    }

    def stepLoadWorking(): Unit = {
        log(
          cf"working M=$MatrixRegTensor_M K=$MatrixRegTensor_K beat=$K_Beat_Count " +
          cf"stride=$Stride trans=$Is_Transpose tail=$HasTail totalReq=$TotalRequestSize totalLoad=$TotalLoadSize"
        )

        val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))

        val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
        val fullTaskMask = Fill(outsideDataWidthByte, true.B)
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

            when(Is_Transpose) {
                val Request = io.LocalMMUIO.Request(0)
                val Response = io.LocalMMUIO.Response(0)

                val transpose_large_m_base = CurrentLoaded_BlockTensor_M_Iter * ABMatrixRegEntryByteSize.U
                val transpose_current_m = transpose_large_m_base + Request_M_Iter_Time
                val transpose_group_in_range = transpose_large_m_base < MatrixRegTensor_M
                val transpose_group_remain = Mux(transpose_group_in_range, MatrixRegTensor_M - transpose_large_m_base, 0.U)
                val current_group_size = Wire(UInt((BeatIndexWidth + 1).W))
                current_group_size := Mux(
                    transpose_group_remain < ABMatrixRegEntryByteSize.U,
                    transpose_group_remain(BeatIndexWidth - 1, 0),
                    ABMatrixRegEntryByteSize.U
                )
                val group_has_no_requests = group_req_cnt === 0.U && group_resp_cnt === 0.U
                val group_is_idle = group_has_no_requests && transPipelineEmpty
                val active_group_size = Mux(group_has_no_requests, current_group_size, group_size_reg)
                val transpose_group_can_issue = Mux(
                    group_is_idle,
                    current_group_size =/= 0.U,
                    group_req_cnt < active_group_size
                )
                val transpose_req_enable = (TotalRequestSize < MaxRequestIter) && transpose_group_can_issue

                val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))
                val TransposeRequestMatrixRegAddr = CurrentLoaded_BlockTensor_K_Iter * (Trans_Load_Size * ReduceGroupSize).U + CurrentLoaded_BlockTensor_M_Iter
                val sourceId = Cat(RequestBeatIsTail, Request_M_Iter_Time, 0.U(BankIdWidth.W), TransposeRequestMatrixRegAddr(RegAddrWidth - 1, 0))

                Request.bits.RequestAddr := BaseVAddr + transpose_current_m * Stride + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
                Request.bits.RequestConherent := Conherent
                Request.bits.RequestSourceID := sourceId
                Request.bits.RequestType_isWrite := false.B
                Request.bits.UseAllocatedSourceID := false.B
                Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                Request.valid := transpose_req_enable

                when(Request.fire) {
                    when(group_is_idle) {
                        group_size_reg := current_group_size
                    }
                    group_req_cnt := group_req_cnt + 1.U

                    Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                    val small_m_reach_group_boundary = Request_M_Iter_Time === (ABMatrixRegEntryByteSize - 1).U
                    val small_m_reach_tensor_boundary = transpose_current_m === (MatrixRegTensor_M - 1.U)
                    val small_m_wrap = small_m_reach_group_boundary || small_m_reach_tensor_boundary
                    val k_wrap = CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U)
                    when(small_m_wrap) {
                        Request_M_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                        when(k_wrap) {
                            CurrentLoaded_BlockTensor_K_Iter := 0.U
                            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + 1.U
                        }
                    }
                    when(TotalRequestSize =/= MaxRequestIter) {
                        TotalRequestSize := TotalRequestSize + 1.U
                    }
                    if (YJPAMLDebugEnable) {
                        log(cf"TransposeReq m=$transpose_current_m k=$CurrentLoaded_BlockTensor_K_Iter group=$group_req_cnt source=$sourceId tail=$RequestBeatIsTail")
                    }
                }

                Response.ready := !transBusStall
                when(Response.fire) {
                    val respSourceId = Response.bits.ReseponseSourceID
                    val respRegAddr = respSourceId(RegAddrWidth - 1, 0)
                    val respBeatIndex = respSourceId(TransposeTailBitOffset - 1, RegAddrWidth + BankIdWidth)
                    val respIsTail = respSourceId(TransposeTailBitOffset)
                    val next_group_resp_cnt = group_resp_cnt + 1.U
                    val drain_trigger = next_group_resp_cnt === active_group_size

                    transPipeInValid := true.B
                    transPipeInData := Response.bits.ReseponseData
                    transPipeInMask := Mux(respIsTail, tailTaskMask, fullTaskMask)
                    transPipeRespBeatCnt := group_resp_cnt
                    transPipeEntryOffset := respBeatIndex
                    transPipeDrainTrigger := drain_trigger

                    when(group_resp_cnt === 0.U) {
                        transWriteBaseAddr := respRegAddr
                        transWriteAddrCnt := 0.U
                    }

                    when(next_group_resp_cnt === active_group_size) {
                        group_req_cnt := 0.U
                        group_resp_cnt := 0.U
                        group_size_reg := 0.U
                    }.otherwise {
                        group_resp_cnt := next_group_resp_cnt
                    }
                    if (YJPAMLDebugEnable) {
                        log(cf"TransposeResp source=$respSourceId beat=$respBeatIndex tail=$respIsTail drain=$drain_trigger")
                    }
                }

                for (i <- 0 until ABMatrixRegNBanks) {
                    val routerValid = transRouters(i).io.valid
                    io.ToMatrixRegIO.BankAddr(i).bits := transWriteAddr
                    io.ToMatrixRegIO.BankAddr(i).valid := routerValid
                    io.ToMatrixRegIO.Data(i).bits := transRouters(i).io.final_data
                    io.ToMatrixRegIO.Data(i).valid := routerValid
                    io.ToMatrixRegIO.ByteMask(i).bits := transRouters(i).io.final_mask
                    io.ToMatrixRegIO.ByteMask(i).valid := routerValid
                    if (YJPAMLDebugEnable) {
                        when(routerValid) {
                            log(cf"TransposeWrite bank=$i addr=$transWriteAddr data=${transRouters(i).io.final_data}%x mask=${transRouters(i).io.final_mask}%x")
                        }
                    }
                }
                when(transRouterWriteValid) {
                    transWriteAddrCnt := Mux(
                        transWriteAddrCnt === (Trans_Load_Size - 1).U,
                        0.U,
                        transWriteAddrCnt + 1.U
                    )
                    if (YJPAMLDebugEnable) {
                        log(cf"TransposeWriteTick validVec=$transRouterValidVec base=$transWriteBaseAddr cnt=$transWriteAddrCnt")
                    }
                }
                val Current_Load_Fill_Size = transRouterWriteValid.asUInt
                TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
                val transposeDone = TotalRequestSize === MaxRequestIter && group_req_cnt === 0.U && group_resp_cnt === 0.U && transPipelineEmpty
                when(transposeDone) {
                    memoryload_state := s_load_quiesce
                    transposeEndDrainCnt := (transposeEndDrainCycles - 1).U
                    if (YJPAMLDebugEnable) log(cf"TransposeWorkingEnd")
                }
            }.otherwise {
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
                    reqQueue.io.enq.bits.RequestAddr := BaseVAddr + mIter * Stride + (kIter << log2Ceil(outsideDataWidthByte))
                    reqQueue.io.enq.bits.RequestConherent := Conherent
                    reqQueue.io.enq.bits.RequestType_isWrite := false.B
                    reqQueue.io.enq.bits.UseAllocatedSourceID := false.B
                    reqQueue.io.enq.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                    reqQueue.io.enq.bits.RequestSourceID := sourceId

                    request.valid := reqQueue.io.deq.valid
                    request.bits := reqQueue.io.deq.bits
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
                            log(cf"NormalReq bank=$i m=$mIter k=$kIter addr=${reqQueue.io.enq.bits.RequestAddr}%x reg=$regAddr tail=$requestBeatIsTail")
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
    }

    def stepLoadQuiesce(): Unit = {
        for (i <- 0 until ABMatrixRegNBanks) {
            val routerValid = transRouters(i).io.valid
            io.ToMatrixRegIO.BankAddr(i).bits := transWriteAddr
            io.ToMatrixRegIO.BankAddr(i).valid := routerValid
            io.ToMatrixRegIO.Data(i).bits := transRouters(i).io.final_data
            io.ToMatrixRegIO.Data(i).valid := routerValid
            io.ToMatrixRegIO.ByteMask(i).bits := transRouters(i).io.final_mask
            io.ToMatrixRegIO.ByteMask(i).valid := routerValid
        }
        when(transRouterWriteValid) {
            transWriteAddrCnt := Mux(
                transWriteAddrCnt === (Trans_Load_Size - 1).U,
                0.U,
                transWriteAddrCnt + 1.U
            )
        }
        when(transposeEndDrainCnt === 0.U) {
            memoryload_state := s_load_end
            if (YJPAMLDebugEnable) log(cf"TransposeQuiesceEnd")
        }.otherwise {
            transposeEndDrainCnt := transposeEndDrainCnt - 1.U
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
            Is_Transpose := ConfigInfo.Is_Transpose

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

    if (EnableDifftest) {
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_WORDS_PER_BANK", ABMatrixRegEntryBitSize / 64)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_REG_SIZE_BYTES", ABMatrixRegSize)
        val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent(ABMatrixRegNBanks, DiffAmuFinishWordsPerBank), delay = 0, dontCare = true)
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        difftestAmuFinish.index := 0.U
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
    }.elsewhen(memoryload_state === s_load_quiesce) {
        io.ToMatrixRegIO.active := true.B
        stepLoadQuiesce()
    }.elsewhen(memoryload_state === s_load_end) {
        stepLoadEnd()
    }
}
