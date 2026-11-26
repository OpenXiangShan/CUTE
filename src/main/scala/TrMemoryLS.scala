
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import xscache.coupledL2.prefetch.{MatrixPrefetchStream, MatrixPrefetchTagCodec}

// Legacy TR load kernels and the independent TR store path.
// Loader 0 retains its row-first traversal; loader 1 retains its reduce-first traversal.

class TrMxSourceFragment(implicit p: Parameters) extends CuteBundle {
    val source = new MxfpConvertSource
    val readByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val writeByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val fragmentBytes = UInt(log2Ceil(outsideDataWidthByte + 1).W)
}

trait HasTrMxLoad { this: CuteModule =>
    protected def checkMxLoad(config: TrLoadTaskConfigIO): Unit = {
        when(config.ConvertF32ToMx) {
            val fp4 = config.ConvertComputeType === MteComputeType.Mxfp4F32
            val packedEntries = Mux(fp4, 1.U, ((64 + TrMatrixRegEntryByteSize - 1) / TrMatrixRegEntryByteSize).U)
            assert(ExperimentalMxfpConvertLoad.B && !config.Is_Transpose && !config.ConvertF32ToI8 &&
                config.LoadTaskInfo.Is_FullLoad && !config.LoadTaskInfo.Is_ZeroLoad &&
                !config.LoadTaskInfo.Is_RepeatRowLoad && !config.Tensor.HasTail &&
                config.Tensor.dataType === ElementDataType.DataTypeWidth32 &&
                config.ConvertKElements === 64.U && config.Tensor.K_Beat_Count === 4.U &&
                config.MatrixRegTensor_M =/= 0.U && config.MatrixRegTensor_M <= Tensor_MN.U &&
                config.MatrixRegTensor_K === packedEntries && packedEntries <= ReduceGroupSize.U,
                "TR MX conversion requires a full non-transposed K64 F32 source and packed destination")
            when(config.ConvertDynamic.getOrElse(false.B)) {
                assert(fp4 && config.MatrixRegTensor_M === 8.U,
                    "dynamic TR conversion requires exactly eight K64 FP4 rows")
            }
        }.otherwise {
            assert(!config.ConvertDynamic.getOrElse(false.B), "dynamic TR load must select MX conversion")
        }
    }

    protected def connectMxFill(config: TrLoadTaskConfigIO, converting: Bool,
        scale: Option[ABScaleControlMatrixRegIO], generated: Option[DecoupledIO[MxfpGeneratedScale]],
        response: DecoupledIO[MMUResponseIO], metadata: => TrMxSourceFragment): Option[MxfpConvertFill] = {
        val converter = Option.when(ExperimentalMxfpConvertLoad)(Module(new MxfpConvertFill))
        config.MicroTaskFlags := Mux(converting, converter.map(_.io.flags).getOrElse(0.U), 0.U)
        converter.foreach { fill =>
            fill.io.start := config.MicroTaskReady && config.MicroTaskValid && config.ConvertF32ToMx
            fill.io.computeType := config.ConvertComputeType
            fill.io.dynamic.foreach(_ := config.ConvertDynamic.get)
            fill.io.FromScaleRegIO <> scale.get
            generated.zip(fill.io.GeneratedScale).foreach { case (outer, inner) => outer <> inner }
            fill.io.request.valid := false.B
            fill.io.request.bits := 0.U.asTypeOf(fill.io.request.bits)
            fill.io.response.valid := false.B
            val meta = metadata
            fill.io.response.bits.source := meta.source
            fill.io.response.bits.readByteOffset := meta.readByteOffset
            fill.io.response.bits.writeByteOffset := meta.writeByteOffset
            fill.io.response.bits.fragmentBytes := meta.fragmentBytes
            fill.io.response.bits.data := response.bits.ReseponseData
        }
        converter
    }

    protected def issueMxFragment(fill: MxfpConvertFill, request: DecoupledIO[MMURequestIO],
        hasRequest: Bool, address: UInt, row: UInt, beat: UInt, second: Bool, meta: TrMxSourceFragment): Bool = {
        val offset = address(log2Ceil(outsideDataWidthByte) - 1, 0)
        val firstBytes = outsideDataWidthByte.U - offset
        val aligned = address & (~(outsideDataWidthByte - 1).U(MMUAddrWidth.W))
        request.bits.RequestAddr := aligned + Mux(second, outsideDataWidthByte.U, 0.U)
        request.valid := hasRequest && fill.io.request.ready
        meta.source.row := row
        meta.source.beat := beat(1, 0)
        meta.readByteOffset := Mux(second, 0.U, offset)
        meta.writeByteOffset := Mux(second, firstBytes, 0.U)
        meta.fragmentBytes := Mux(second, offset, firstBytes)
        fill.io.request.valid := request.fire
        fill.io.request.bits := meta.source
        when(request.fire) { second := !second && offset.orR }
        second || !offset.orR
    }

    protected def receiveMxFragment(fill: MxfpConvertFill, response: DecoupledIO[MMUResponseIO],
        target: TrLoadMatrixRegIO): Unit = {
        fill.io.response.valid := response.valid
        response.ready := fill.io.response.ready
        for (bank <- 0 until TrMatrixRegNBanks) {
            target.BankAddr(bank) := fill.io.ToMatrixRegIO.BankAddr(bank)
            target.Data(bank) := fill.io.ToMatrixRegIO.Data(bank)
            target.ByteMask(bank) := fill.io.ToMatrixRegIO.ByteMask(bank)
        }
    }
}

class TrLoader0SourceId(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(TrMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(TrMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
    val Mx = Option.when(ExperimentalMxfpConvertLoad)(new TrMxSourceFragment)
}

class TrLoader0Legacy(emitDifftest: Boolean = true)(implicit p: Parameters) extends CuteModule with HasTrMxLoad {
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
        val FromScaleRegIO = Option.when(ExperimentalMxfpConvertLoad)(Flipped(new ABScaleControlMatrixRegIO))
        val ScaleBankId = Output(UInt(ABScaleRegIdWidth.W))
        val ScaleReadActive = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(Decoupled(new MxfpGeneratedScale))
        val idle = Output(Bool())
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
    val ConvertF32ToMx = RegInit(false.B)
    val ConvertDynamic = RegInit(false.B)
    val ConvertScaleBankId = RegInit(0.U(ABScaleRegIdWidth.W))
    val ConvertSecondFragment = RegInit(false.B)
    io.idle := state === s_idle
    io.ScaleBankId := ConvertScaleBankId
    io.ScaleReadActive := ConvertF32ToMx && !ConvertDynamic && !io.idle

    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, TrMatrixRegEntryByteSize)).W))

    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new TrLoader0SourceId).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val convertFill = connectMxFill(ConfigInfo, ConvertF32ToMx, io.FromScaleRegIO, io.GeneratedScale, Response,
        SoureceIdSearchTable(Response.bits.ReseponseSourceID).asTypeOf(new TrLoader0SourceId).Mx.get)

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
            ConvertF32ToMx := ConfigInfo.ConvertF32ToMx
            ConvertDynamic := ConfigInfo.ConvertDynamic.getOrElse(false.B)
            ConvertScaleBankId := ConfigInfo.ConvertScaleBankId
            checkMxLoad(ConfigInfo)
            assert(!ConfigInfo.ConvertF32ToI8, "F32-to-I8 conversion belongs to legacy TR loader 1")
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
            ConvertSecondFragment := false.B
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

                val logicalRequestAddr = Tensor_Block_BaseAddr + RequestMatrixRegMIndex * stride +
                    (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
                Request.bits.RequestAddr := logicalRequestAddr
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

                val TableItem = Wire(new TrLoader0SourceId)
                TableItem.MatrixRegBankId := RequestMatrixRegBankId
                TableItem.MatrixRegAddr := RequestMatrixRegAddr
                TableItem.MatrixRegisTail := RequestBeatIsTail
                TableItem.Mx.foreach(meta => meta := 0.U.asTypeOf(meta))
                val advanceBeat = WireDefault(true.B)
                convertFill.foreach { fill =>
                    when(ConvertF32ToMx) {
                        advanceBeat := issueMxFragment(fill, Request, TotalRequestSize < MaxRequestIter && sourceId.valid,
                            logicalRequestAddr, RequestMatrixRegMIndex, CurrentLoaded_BlockTensor_K_Iter,
                            ConvertSecondFragment, TableItem.Mx.get)
                    }
                }
                when(Request.fire){
                    SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                    if (YJPTrLoader0DebugEnable) {
                        printf("[TrLoader0_RequestHandshake<%d>] M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d, SourceId:%d, Tail:%d\n",
                          io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_M_Iter_Time, Request.bits.RequestAddr, RequestMatrixRegBankId, RequestMatrixRegAddr, sourceId.bits, RequestBeatIsTail)
                    }

                    when(advanceBeat) {
                        when(ConvertF32ToMx) {
                            CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                            when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count) {
                                CurrentLoaded_BlockTensor_K_Iter := 0.U
                                CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + 1.U
                            }
                        }.otherwise {
                            Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                            when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === MatrixRegTensor_M - 1.U){
                                Request_M_Iter_Time := 0.U
                                CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                                when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                                    CurrentLoaded_BlockTensor_K_Iter := 0.U
                                    CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                                }
                            }
                        }
                        when(TotalRequestSize =/= MaxRequestIter){
                            TotalRequestSize := TotalRequestSize + 1.U
                        }
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

                when(Response.fire && !ConvertF32ToMx){
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
                    when(!ConvertF32ToMx && TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                        memoryload_state := s_load_end
                        if (YJPTrLoader0DebugEnable) printf("[trLoader0<%d>]FullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                    }
                convertFill.foreach { fill =>
                    when(ConvertF32ToMx) {
                        receiveMxFragment(fill, Response, io.ToMatrixRegIO)
                        when(TotalRequestSize === MaxRequestIter && fill.io.idle) {
                            memoryload_state := s_load_end
                        }
                    }
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
    val Mx = Option.when(ExperimentalMxfpConvertLoad)(new TrMxSourceFragment)
    val I8Beat = UInt(2.W)
}

// Ordinary A/B tasks both use normalized register rows and contiguous reduce bytes.

class TrLoader1Legacy(implicit p: Parameters) extends CuteModule with HasTrMxLoad {

    val io = IO(new Bundle{
        // MatrixReg interface.
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
        val FromScaleRegIO = Option.when(ExperimentalMxfpConvertLoad)(Flipped(new ABScaleControlMatrixRegIO))
        val ScaleBankId = Output(UInt(ABScaleRegIdWidth.W))
        val ScaleReadActive = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(Decoupled(new MxfpGeneratedScale))
        val idle = Output(Bool())
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
    val ConvertF32ToMx = RegInit(false.B)
    val ConvertDynamic = RegInit(false.B)
    val ConvertScaleBankId = RegInit(0.U(ABScaleRegIdWidth.W))
    val ConvertSecondFragment = RegInit(false.B)
    val ConvertF32ToI8 = RegInit(false.B)
    private val I8Layout = outsideDataWidthByte == 64 && TrMatrixRegEntryByteSize == 32 && ReduceGroupSize >= 2
    val I8WaitRow = RegInit(false.B)
    val I8Have = RegInit(0.U(4.W))
    val I8SecondValid = RegInit(false.B)
    val I8Slots = Reg(Vec(4, UInt(128.W)))
    val I8SecondBank = Reg(UInt(log2Ceil(TrMatrixRegNBanks).W))
    val I8SecondAddr = Reg(UInt(log2Ceil(TrMatrixRegBankNEntries).W))
    val I8SecondData = Reg(UInt(256.W))
    io.ScaleBankId := ConvertScaleBankId
    io.ScaleReadActive := ConvertF32ToMx && !ConvertDynamic && !io.idle

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
        val ordinaryHash = !(ConvertF32ToMx && ConvertDynamic)
        difftestAmuFinish.valid := ordinaryHash && (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
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
        difftestAmuFinishLane1.valid := ordinaryHash && io.ToMatrixRegIO.WriteRequest.map(_(1).BankAddr.valid).reduce(_ || _)
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
    io.idle := state === s_idle


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
            ConvertF32ToMx := ConfigInfo.ConvertF32ToMx
            ConvertDynamic := ConfigInfo.ConvertDynamic.getOrElse(false.B)
            ConvertScaleBankId := ConfigInfo.ConvertScaleBankId
            ConvertF32ToI8 := ConfigInfo.ConvertF32ToI8
            checkMxLoad(ConfigInfo)
            when(ConfigInfo.ConvertF32ToI8) {
                val offsetBits = log2Ceil(outsideDataWidthByte)
                assert(I8Layout.B && !ConfigInfo.ConvertF32ToMx && !ConfigInfo.Tensor.HasTail &&
                    ConfigInfo.Tensor.K_Beat_Count === 4.U &&
                    ConfigInfo.Tensor.dataType === ElementDataType.DataTypeWidth32 &&
                    ConfigInfo.LoadTaskInfo.Is_FullLoad && !ConfigInfo.LoadTaskInfo.Is_ZeroLoad &&
                    !ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad &&
                    ConfigInfo.MatrixRegTensor_M =/= 0.U && ConfigInfo.MatrixRegTensor_M <= Tensor_MN.U &&
                    !ConfigInfo.Tensor.baseVaddr(offsetBits - 1, 0).orR &&
                    (ConfigInfo.MatrixRegTensor_M === 1.U || !ConfigInfo.Tensor.stride(offsetBits - 1, 0).orR),
                    "legacy TR loader 1 F32-to-I8 expects a full aligned K64 F32 tile and 32-byte entries")
            }
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
    val returnMeta = SoureceIdSearchTable(Response.bits.ReseponseSourceID).asTypeOf(new TrLoader1SourceId)
    val convertFill = connectMxFill(ConfigInfo, ConvertF32ToMx, io.FromScaleRegIO, io.GeneratedScale, Response,
        returnMeta.Mx.get)
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            ConvertSecondFragment := false.B
            I8WaitRow := false.B
            I8Have := 0.U
            I8SecondValid := false.B
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
            val RequestMatrixRegAddr = Mux(ConvertF32ToI8, NormalRequestMatrixRegBaseAddr, NormalRequestMatrixRegAddr)

            val logicalRequestAddr = Tensor_Block_BaseAddr + RequestMatrixRegRowIndex * stride +
                (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
            Request.bits.RequestAddr := logicalRequestAddr

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
            Request.valid := TotalRequestSize < MaxRequestIter && (!ConvertF32ToI8 || (!I8WaitRow && sourceId.valid))

            val TableItem = Wire(new TrLoader1SourceId)
            TableItem.MatrixRegBankId := RequestMatrixRegBankId
            TableItem.MatrixRegAddr := RequestMatrixRegAddr
            TableItem.MatrixRegisTail := RequestBeatIsTail
            TableItem.I8Beat := Mux(ConvertF32ToI8, CurrentLoaded_BlockTensor_K_Iter(1, 0), 0.U)
            TableItem.Mx.foreach(meta => meta := 0.U.asTypeOf(meta))
            val advanceBeat = WireDefault(true.B)
            convertFill.foreach { fill =>
                when(ConvertF32ToMx) {
                    advanceBeat := issueMxFragment(fill, Request, TotalRequestSize < MaxRequestIter && sourceId.valid,
                        logicalRequestAddr, RequestMatrixRegRowIndex, CurrentLoaded_BlockTensor_K_Iter,
                        ConvertSecondFragment, TableItem.Mx.get)
                }
            }

            when(Request.fire && sourceId.valid){//A valid source ID means this request can be issued.
                //Request.ready means LocalMMU accepts the request; sourceId.valid means its allocated ID is valid.
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
                if (YJPTrLoader1DebugEnable) {
                    printf("[TrLoader1_RequestHandshake<%d>] sourceId:%d, MatrixRegBankId:%d, MatrixRegAddr:%d, RequestAddr:%x, RequestConherent:%d, RequestType_isWrite:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegBankId,TableItem.MatrixRegAddr,Request.bits.RequestAddr,Request.bits.RequestConherent,Request.bits.RequestType_isWrite,RequestBeatIsTail)
                }

                when(advanceBeat) {
                    when(ConvertF32ToMx || ConvertF32ToI8) {
                        CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                        when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count) {
                            CurrentLoaded_BlockTensor_K_Iter := 0.U
                            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + 1.U
                            when(ConvertF32ToI8) { I8WaitRow := true.B }
                        }
                    }.otherwise {
                        Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                        when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === MatrixRegTensor_M - 1.U){
                            Request_M_Iter_Time := 0.U
                            CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                            when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                                CurrentLoaded_BlockTensor_K_Iter := 0.U
                                CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                            }
                        }
                    }
                    when(TotalRequestSize =/= MaxRequestIter){
                        TotalRequestSize := TotalRequestSize + 1.U
                    }
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
            when(Response.fire && !ConvertF32ToMx && !ConvertF32ToI8){
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
                when(!ConvertF32ToMx && !ConvertF32ToI8 && TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                    memoryload_state := s_load_end
                    if (YJPTrLoader1DebugEnable)
                    {
                        printf("[TrLoader1_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
                convertFill.foreach { fill =>
                    when(ConvertF32ToMx) {
                        receiveMxFragment(fill, Response, io.ToMatrixRegIO)
                        when(TotalRequestSize === MaxRequestIter && fill.io.idle) {
                            memoryload_state := s_load_end
                        }
                    }
                }
                if (I8Layout) {
                    when(ConvertF32ToI8) {
                        Response.ready := !I8SecondValid
                        def writeI8(bank: UInt, addr: UInt, data: UInt): Unit = {
                            val lane = io.ToMatrixRegIO.WriteRequest(bank)(0)
                            lane.BankAddr.valid := true.B
                            lane.BankAddr.bits := addr
                            lane.Data.valid := true.B
                            lane.Data.bits := data
                            lane.ByteMask.valid := true.B
                            lane.ByteMask.bits := Fill(TrMatrixRegEntryByteSize, true.B)
                        }
                        when(Response.fire) {
                            val beat = returnMeta.I8Beat
                            val data = Response.bits.ReseponseData
                            val converted = Cat((0 until 16).reverse.map { lane =>
                                F32ToI8FixedScale(data(lane * 32 + 31, lane * 32))
                            })
                            val mask = UIntToOH(beat, 4)
                            val have = I8Have | mask
                            assert(!(I8Have & mask).orR, "F32-to-I8 received a duplicate beat")
                            I8Slots(beat) := converted
                            I8Have := have
                            when(have.andR) {
                                val slots = VecInit((0 until 4).map(index =>
                                    Mux(beat === index.U, converted, I8Slots(index))))
                                writeI8(returnMeta.MatrixRegBankId, returnMeta.MatrixRegAddr, Cat(slots(1), slots(0)))
                                I8Have := 0.U
                                I8SecondValid := true.B
                                I8SecondBank := returnMeta.MatrixRegBankId
                                I8SecondAddr := returnMeta.MatrixRegAddr + 1.U
                                I8SecondData := Cat(slots(3), slots(2))
                            }
                        }
                        when(I8SecondValid) {
                            writeI8(I8SecondBank, I8SecondAddr, I8SecondData)
                            I8SecondValid := false.B
                            I8WaitRow := false.B
                            when(TotalRequestSize === MaxRequestIter) { memoryload_state := s_load_end }
                        }
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
    // Full-line stores cannot transfer more than the architectural TR capacity.
    private val StoreMaxRequests = (Tensor_MN * Tensor_K + 63) / 64
    // Round up both dimensions: paired transpose reads may send an empty context.
    private val StoreMaxBatches = ((Tensor_MN + TrMatrixRegNBanks - 1) / TrMatrixRegNBanks) *
        ((Tensor_K + 63) / 64)
    protected val Store_Batch_Id = RegInit(0.U(log2Ceil(StoreMaxBatches + 1).W))
    protected val TotalStoreRequestSize = RegInit(0.U(log2Ceil(StoreMaxRequests + 1).W))
    protected val TotalStoreResponseSize = RegInit(0.U(log2Ceil(StoreMaxRequests + 1).W))
    private val StoreLineAddrWidth = MMUAddrWidth - 6
    private val Store_Row_Line_Addr = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Output_Line_Addr = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Entry_Line_Stride = Reg(UInt(StoreLineAddrWidth.W))
    private val Store_Line_Stride = StoreCommand.stride(MMUAddrWidth - 1, 6)

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

    when (io.read.BankAddr.fire) {
        // Use the existing SRAM read cycle to prepare the output line base.
        Store_Output_Line_Addr := Store_Row_Line_Addr + (CurrentStore_Byte_Offset >> 6)
        memorystore_state := s_store_capture
    }
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
            when (StoreCommand.transpose && Store_Output_Context === 0.U) {
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
    when (StoreRequestFire.asUInt.orR) {
        TotalStoreRequestSize := TotalStoreRequestSize + 1.U
    }
    when (StoreResponseFire.asUInt.orR) {
        TotalStoreResponseSize := TotalStoreResponseSize + PopCount(StoreResponseFire)
    }
    when (memorystore_state =/= s_store_idle) {
        assert(TotalStoreRequestSize +& PopCount(StoreRequestFire) <= StoreMaxRequests.U,
            "TR store request count exceeds the register capacity")
        assert(Store_Batch_Id <= StoreMaxBatches.U, "TR store batch counter overflow")
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

// Owned source fragments of the existing normal-load CAM, not a second memory
// protocol. Reserve a K32 owner before issuing its first containing-line Get.
class MxfpConvertSource(implicit p: Parameters) extends CuteBundle {
    val row = UInt(MatrixRegMaxTensorDimBitSize.W)
    val beat = UInt(2.W) // four 64-byte F32 beats in the experimental K64 row
}

class MxfpConvertFragment(implicit p: Parameters) extends CuteBundle {
    val source = new MxfpConvertSource
    val data = UInt(outsideDataWidth.W)
    val readByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val writeByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val fragmentBytes = UInt(log2Ceil(outsideDataWidthByte + 1).W)
}

// Shared AML/BML response-fill extension. The original loaders still own
// addresses, containing-line fragments and SourceID allocation. This stage
// owns K32 assembly, the existing supplied-scale encoder and packed SCP writes.
class MxfpConvertFill(implicit p: Parameters) extends CuteModule {
    require(outsideDataWidth == 512 && TrMatrixRegEntryByteSize >= 32)
    private val Owners = MxfpConvertOwners
    private val OwnerBits = log2Ceil(Owners)
    val io = IO(new Bundle {
        val start = Input(Bool())
        val computeType = Input(UInt(MteComputeType.ComputeTypeBitWidth.W))
        val dynamic = Option.when(ExperimentalMxfpDynamicQuant)(Input(Bool()))
        val request = Flipped(Decoupled(new MxfpConvertSource))
        val response = Flipped(Decoupled(new MxfpConvertFragment))
        val FromScaleRegIO = Flipped(new ABScaleControlMatrixRegIO)
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(
            Decoupled(new MxfpGeneratedScale))
        val idle = Output(Bool())
        val flags = Output(UInt(5.W))
    })
    val mode = RegEnable(io.computeType, MteComputeType.ComputeTypeUndef, io.start)
    val dynamic = io.dynamic.map(d => RegEnable(d, false.B, io.start)).getOrElse(false.B)
    val flags = RegInit(0.U(5.W))
    io.flags := flags
    val allocated = RegInit(VecInit(Seq.fill(Owners)(false.B)))
    val submitted = RegInit(VecInit(Seq.fill(Owners)(false.B)))
    val rows = Reg(Vec(Owners, UInt(MatrixRegMaxTensorDimBitSize.W)))
    val blocks = Reg(Vec(Owners, Bool()))
    val raw = Reg(Vec(Owners, UInt(1024.W)))
    val received = RegInit(VecInit(Seq.fill(Owners)(0.U(128.W))))
    val scales = Reg(Vec(Owners, UInt(8.W)))
    val scaleRequested = RegInit(VecInit(Seq.fill(Owners)(false.B)))
    val scaleValid = RegInit(VecInit(Seq.fill(Owners)(false.B)))
    val dynamicUnsupported = Option.when(ExperimentalMxfpDynamicQuant)(
        RegInit(VecInit(Seq.fill(Owners)(false.B))))

    val requestMatches = VecInit((0 until Owners).map(i => allocated(i) &&
        rows(i) === io.request.bits.row && blocks(i) === io.request.bits.beat(1)))
    val free = VecInit(allocated.map(!_))
    val requestOwner = Mux(requestMatches.asUInt.orR,
        PriorityEncoder(requestMatches), PriorityEncoder(free))
    io.request.ready := !reset.asBool && (requestMatches.asUInt.orR || free.asUInt.orR)
    when(io.request.fire && !requestMatches.asUInt.orR) {
        allocated(requestOwner) := true.B
        submitted(requestOwner) := false.B
        rows(requestOwner) := io.request.bits.row
        blocks(requestOwner) := io.request.bits.beat(1)
        raw(requestOwner) := 0.U
        received(requestOwner) := 0.U
        scaleRequested(requestOwner) := false.B
        scaleValid(requestOwner) := false.B
        dynamicUnsupported.foreach(_(requestOwner) := false.B)
    }

    val responseMatches = VecInit((0 until Owners).map(i => allocated(i) &&
        rows(i) === io.response.bits.source.row &&
        blocks(i) === io.response.bits.source.beat(1)))
    val responseOwner = PriorityEncoder(responseMatches)
    io.response.ready := !reset.asBool && responseMatches.asUInt.orR && !submitted(responseOwner)
    val fragmentOffset = Cat(io.response.bits.source.beat(0), 0.U(6.W)) +&
        io.response.bits.writeByteOffset
    val lowMask = (UIntToOH(io.response.bits.fragmentBytes, 65) - 1.U)(63, 0)
    val fragmentMask = (lowMask.pad(128) << fragmentOffset)(127, 0)
    val byteMask = Cat((0 until 128).reverse.map(i => Fill(8, fragmentMask(i))))
    val aligned = (io.response.bits.data >> (io.response.bits.readByteOffset << 3)).pad(1024)
    val positioned = (aligned << (fragmentOffset << 3))(1023, 0)
    when(io.response.valid) {
        assert(responseMatches.asUInt.orR, "MX fill response has no reserved K32 owner")
        assert(PopCount(responseMatches) === 1.U, "MX fill response has ambiguous owner")
    }
    when(io.response.fire) {
        assert(io.response.bits.fragmentBytes =/= 0.U &&
            fragmentOffset +& io.response.bits.fragmentBytes <= 128.U,
            "MX fill fragment exceeds its K32")
        assert((received(responseOwner) & fragmentMask) === 0.U,
            "MX fill received a duplicate source fragment")
        raw(responseOwner) := (raw(responseOwner) & ~byteMask) | (positioned & byteMask)
        received(responseOwner) := received(responseOwner) | fragmentMask
    }

    // Reserve a destination owner before accepting the synchronous Valid-only
    // SF response. One outstanding read, independently held scale per K32.
    val scalePending = RegInit(false.B)
    val scaleOwner = Reg(UInt(OwnerBits.W))
    val scaleByte = Reg(UInt(6.W))
    val scaleCandidates = VecInit((0 until Owners).map(i => allocated(i) && !scaleRequested(i)))
    val nextScaleOwner = PriorityEncoder(scaleCandidates)
    val scaleIndex = (rows(nextScaleOwner) << 1) + blocks(nextScaleOwner)
    io.FromScaleRegIO.BankAddr.valid := !reset.asBool && !dynamic &&
        !scalePending && scaleCandidates.asUInt.orR
    io.FromScaleRegIO.BankAddr.bits := scaleIndex >> 6
    when(io.FromScaleRegIO.BankAddr.fire) {
        scalePending := true.B
        scaleOwner := nextScaleOwner
        scaleByte := scaleIndex(5, 0)
        scaleRequested(nextScaleOwner) := true.B
    }
    when(io.FromScaleRegIO.Data.valid && !reset.asBool) {
        assert(scalePending, "MX fill received an unowned SF response")
        scales(scaleOwner) := (io.FromScaleRegIO.Data.bits.asUInt >> (scaleByte << 3))(7, 0)
        scaleValid(scaleOwner) := true.B
        scalePending := false.B
    }

    val choose = Module(new RRArbiter(UInt(OwnerBits.W), Owners))
    for (i <- 0 until Owners) {
        choose.io.in(i).valid := allocated(i) && received(i).andR &&
            (dynamic || scaleValid(i)) && !submitted(i) && !reset.asBool
        choose.io.in(i).bits := i.U
    }
    val ownerQ = Module(new Queue(UInt(OwnerBits.W), Owners))
    val fp4 = Module(new Mxfp4EncodeBlock(MxfpConvertLanes))
    val e4 = Module(new Mxfp8EncodeBlock(true, MxfpConvertLanes))
    val e5 = Module(new Mxfp8EncodeBlock(false, MxfpConvertLanes))
    val use4 = mode === MteComputeType.Mxfp4F32
    val useE4 = mode === MteComputeType.Mxfp8e4m3F32
    val useE5 = mode === MteComputeType.Mxfp8e5m2F32
    val selected = choose.io.out.bits
    // One shared K32 tree AFTER owner selection, never one tree per owner.
    val dynamicScale = Option.when(ExperimentalMxfpDynamicQuant)(Module(new Mxfp4DynamicScale))
    dynamicScale.foreach(_.io.values := raw(selected).asTypeOf(Vec(32, UInt(32.W))))
    val selectedE = Mux(dynamic, dynamicScale.map(_.io.e).getOrElse(0.U), scales(selected))
    val selectedSupported = dynamicScale.map(_.io.supported).getOrElse(true.B)
    Seq(fp4.io.in, e4.io.in, e5.io.in).zip(Seq(use4, useE4, useE5)).foreach {
        case (port, enabled) =>
            port.valid := choose.io.out.valid && ownerQ.io.enq.ready && enabled
            // Unsupported dynamic input still traverses an owned encoder
            // token, so it cannot strand an owner or suppress task End.
            port.bits.values := Mux(dynamic && !selectedSupported, 0.U, raw(selected))
                .asTypeOf(Vec(32, UInt(32.W)))
            port.bits.e := selectedE
            port.bits.active := Fill(32, true.B)
    }
    val encoderReady = Mux(use4, fp4.io.in.ready, Mux(useE4, e4.io.in.ready, e5.io.in.ready))
    choose.io.out.ready := encoderReady && ownerQ.io.enq.ready
    ownerQ.io.enq.valid := choose.io.out.valid && encoderReady
    ownerQ.io.enq.bits := selected
    when(choose.io.out.fire) {
        submitted(selected) := true.B
        dynamicUnsupported.foreach(_(selected) := dynamic && !selectedSupported)
    }

    class PackedWrite extends Bundle {
        val row = UInt(MatrixRegMaxTensorDimBitSize.W)
        val block = Bool()
        val payload = UInt(256.W)
        val e = Option.when(ExperimentalMxfpDynamicQuant)(UInt(8.W))
        val unsupported = Option.when(ExperimentalMxfpDynamicQuant)(Bool())
    }
    val writes = Module(new Queue(new PackedWrite, Owners))
    val encoderValid = Mux(use4, fp4.io.out.valid, Mux(useE4, e4.io.out.valid, e5.io.out.valid))
    val encoderEligible = Mux(use4, fp4.io.out.bits.eligible,
        Mux(useE4, e4.io.out.bits.eligible, e5.io.out.bits.eligible))
    val encoderInvalid = Mux(use4, fp4.io.out.bits.invalid,
        Mux(useE4, e4.io.out.bits.invalid, e5.io.out.bits.invalid))
    val reject = !encoderEligible || encoderInvalid ||
        (dynamic && dynamicUnsupported.map(_(ownerQ.io.deq.bits)).getOrElse(false.B))
    val payload = Mux(use4, fp4.io.out.bits.payload.pad(256),
        Mux(useE4, e4.io.out.bits.payload, e5.io.out.bits.payload))
    fp4.io.out.ready := use4 && ownerQ.io.deq.valid && writes.io.enq.ready
    e4.io.out.ready := useE4 && ownerQ.io.deq.valid && writes.io.enq.ready
    e5.io.out.ready := useE5 && ownerQ.io.deq.valid && writes.io.enq.ready
    writes.io.enq.valid := encoderValid && ownerQ.io.deq.valid
    writes.io.enq.bits.row := rows(ownerQ.io.deq.bits)
    writes.io.enq.bits.block := blocks(ownerQ.io.deq.bits)
    writes.io.enq.bits.payload := Mux(reject, 0.U, payload)
    writes.io.enq.bits.e.foreach(_ := fp4.io.out.bits.scale)
    writes.io.enq.bits.unsupported.foreach(_ := reject)
    ownerQ.io.deq.ready := encoderValid && writes.io.enq.ready
    when(encoderValid) { assert(ownerQ.io.deq.valid, "MX encoder result lost its source owner") }
    when(writes.io.enq.fire) {
        allocated(ownerQ.io.deq.bits) := false.B
        when(reject) { flags := flags | "h10".U }
    }
    // The existing SCP write port is Valid-only: each asserted write really
    // reaches the selected bank. Retain packed payload+mask together in FIFO.
    io.GeneratedScale.foreach { port =>
        port.valid := dynamic && writes.io.deq.valid && !reset.asBool
        port.bits.row := writes.io.deq.bits.row
        port.bits.block := writes.io.deq.bits.block
        port.bits.e := writes.io.deq.bits.e.get
        port.bits.unsupported := writes.io.deq.bits.unsupported.get
    }
    writes.io.deq.ready := !reset.asBool &&
        (!dynamic || io.GeneratedScale.map(_.ready).getOrElse(true.B))
    io.ToMatrixRegIO := 0.U.asTypeOf(io.ToMatrixRegIO)
    io.ToMatrixRegIO.active := allocated.asUInt.orR || writes.io.deq.valid
    val write = writes.io.deq.bits
    val packedByteOffset = Mux(use4, write.block.asUInt << 4, write.block.asUInt << 5)
    val entryOffset = packedByteOffset % TrMatrixRegEntryByteSize.U
    val entry = (write.row / TrMatrixRegNBanks.U) * ReduceGroupSize.U +
        packedByteOffset / TrMatrixRegEntryByteSize.U
    val mask = Mux(use4, "hffff".U(TrMatrixRegEntryByteSize.W),
        "hffffffff".U(TrMatrixRegEntryByteSize.W)) << entryOffset
    for (i <- 0 until TrMatrixRegNBanks) {
        val valid = writes.io.deq.fire && write.row % TrMatrixRegNBanks.U === i.U
        io.ToMatrixRegIO.BankAddr(i).valid := valid
        io.ToMatrixRegIO.BankAddr(i).bits := entry
        io.ToMatrixRegIO.Data(i).valid := valid
        io.ToMatrixRegIO.Data(i).bits := write.payload << (entryOffset << 3)
        io.ToMatrixRegIO.ByteMask(i).valid := valid
        io.ToMatrixRegIO.ByteMask(i).bits := mask
    }
    io.idle := !allocated.asUInt.orR && !ownerQ.io.deq.valid && !writes.io.deq.valid &&
        !scalePending && !io.FromScaleRegIO.Data.valid && !encoderValid
    when(io.start) {
        assert(io.idle, "MX fill task replaced before all actual writes drained")
        assert(io.computeType === MteComputeType.Mxfp4F32 ||
            io.computeType === MteComputeType.Mxfp8e4m3F32 ||
            io.computeType === MteComputeType.Mxfp8e5m2F32, "MX fill requires a supported format")
        when(io.dynamic.getOrElse(false.B)) {
            assert(io.computeType === MteComputeType.Mxfp4F32, "dynamic quantization is FP4 only")
        }
        flags := 0.U
    }
}

// The fixed power-of-two scale is folded into the F32 exponent, avoiding a
// floating multiplier in the loader fill path.
object F32ToI8FixedScale {
    def apply(bits: UInt): UInt = {
        require(bits.getWidth == 32)
        val sign = bits(31)
        val exp = bits(30, 23)
        val mantissa = bits(22, 0)
        val isNaN = exp.andR && mantissa.orR
        val isInf = exp.andR && !mantissa.orR
        val significand = Cat(1.U(1.W), mantissa).pad(32)
        val shift = 156.U(9.W) - exp.pad(9)
        val truncated = (significand >> shift)(31, 0)
        val remainderMask = (1.U(32.W) << shift) - 1.U(32.W)
        val remainder = significand & remainderMask
        val halfway = 1.U(32.W) << (shift - 1.U)
        val roundUp = remainder > halfway || (remainder === halfway && truncated(0))
        val roundedWide = truncated + roundUp.asUInt
        val rounded = roundedWide(7, 0)
        val magnitude = Mux(
            exp <= 131.U,
            0.U(8.W),
            Mux(
                exp >= 140.U,
                127.U(8.W),
                Mux(rounded > 127.U, 127.U(8.W), rounded)
            )
        )
        val signedMagnitude = Mux(sign, (0.U(8.W) - magnitude), magnitude)
        Mux(
            isNaN,
            0.U(8.W),
            Mux(isInf, Mux(sign, 129.U(8.W), 127.U(8.W)), signedMagnitude)
        )
    }
}

// Supplied-scale MX encoder primitives. These do not connect to production
// BML, scale SRAMs, dynamic scale selection or architectural flags.
// The FP4 caller still owns architectural admission, including NX-sticky.

// MXFP8 supplied-scale lane prototype. Reuse Fudian's exact normalization and
// rounding rather than introducing a second IEEE converter. Neither fflags
// output below is an architectural flag update. CPU scale selection and
// admission remain outside this primitive.
object Mxfp8EncodeLane {
    def apply(bits: UInt, e: UInt, active: Bool, e4m3: Boolean): (UInt, Bool, Bool) = {
        val magnitude = bits(30, 0)
        val special = bits(30, 23).andR
        val invalid = active && special && bits(22, 0).orR
        // max finite is 1.75*2^8 or 1.75*2^15. These envelope thresholds
        // are normal FP32 values even at e=0; larger exponents admit every
        // finite FP32 without wrapping the threshold's exponent field.
        val envelopeExponent = e.pad(9) + (if (e4m3) 8 else 15).U(9.W)
        val envelope = Cat(envelopeExponent(7, 0), 0x600000.U(23.W))
        val eligible = !active || (!special &&
            (envelopeExponent >= 255.U || magnitude <= envelope))

        val widen = Module(new fudian.FPUpConverter(8, 24, 11, 25))
        widen.io.in := Mux(active, bits, 0.U(32.W))
        widen.io.rm := 0.U // exact widening, including original subnormals
        val wideExponent = widen.io.result(34, 24)
        // MXFP8 d=2^(e-127). E4M3FN is represented temporarily as E5M3
        // y/256: identical subnormal grid and biased exponent0..15, with
        // no IEEE-special exponent in the admitted E4M3FN envelope.
        val adjustedExponent = wideExponent.zext +
            (if (e4m3) 119 else 127).S(13.W) - e.zext
        val adjustedExponentBits = adjustedExponent.asUInt
        val scaled = Cat(widen.io.result(35),
            Mux(wideExponent.orR, adjustedExponentBits(10, 0), 0.U(11.W)),
            widen.io.result(23, 0))
        val narrow = Module(new fudian.FPDownConverter(11, 25, 5, if (e4m3) 4 else 3))
        narrow.io.in := scaled
        narrow.io.rm := 0.U // fixed RNE, independent of scalar/matrix CSR
        val encoded = if (e4m3) {
            Cat(narrow.io.result(8), narrow.io.result(6, 0))
        } else {
            narrow.io.result
        }
        (Mux(active, encoded, 0.U(8.W)), invalid, eligible)
    }
}

// Stage-A scale-result primitive for one fully active K32. This is NOT an
// unrestricted replacement for CPU log2f/floorf or their accrued flags.
// The caller owns scalar-NX admission and CPU fallback outside this guard.
// No architectural rounding mode or flags are read or written here.
class Mxfp4DynamicScale extends Module {
    val io = IO(new Bundle {
        val values = Input(Vec(32, UInt(32.W)))
        val amax = Output(UInt(31.W)) // unsigned raw-F32 magnitude, including rejects
        val e = Output(UInt(8.W))
        val supported = Output(Bool())
    })

    // Pair adjacent nodes at each elaboration level: 32 -> 16 -> 8 -> 4 -> 2
    // -> 1, i.e. five comparator levels, not a left-fold's 31-level chain.
    private def balancedMax(nodes: Seq[UInt]): UInt = {
        if (nodes.size == 1) nodes.head
        else balancedMax(nodes.grouped(2).map { pair =>
            Mux(pair(0) >= pair(1), pair(0), pair(1))
        }.toSeq)
    }
    val maximum = balancedMax(io.values.map(_(30, 0)).toSeq)
    // Inspect EVERY lane: magnitude max alone is not a finite-input check.
    val hasSpecial = VecInit(io.values.map(_(30, 23).andR)).asUInt.orR
    val exponent = maximum(30, 23)
    val fraction = maximum(22, 0)
    val zero = maximum === 0.U
    val guardedFraction = fraction === 0.U ||
        (fraction >= "h000400".U(23.W) && fraction <= "h7ffc00".U(23.W))
    val guardedNonzero = exponent >= 2.U && exponent <= 253.U && guardedFraction

    io.amax := maximum
    io.supported := !hasSpecial && (zero || guardedNonzero)
    // All +/-zero has e0. Exact powers in the guarded exponent range are
    // supported; rejected near-power/tiny/special inputs return e0 as well.
    io.e := Mux(io.supported && !zero, exponent - 2.U, 0.U(8.W))
}

// Shared supplied-scale FP4 lane logic. The block encoder shares one boundary
// set across its lanes; other execution owners may supply per-lane scales.
// This is the existing strict-first dyadic search, not a generic RNE converter.
object Mxfp4EncodeLane {
    def boundaries(e: UInt): Seq[UInt] = {
        require(e.getWidth == 8)
        def boundary(exponentOffset: Int, fraction: Int, subnormals: Seq[Int] = Seq.empty): UInt = {
            val exponent = if (exponentOffset >= 0) {
                e.pad(9) + exponentOffset.U(9.W)
            } else {
                e.pad(9) - (-exponentOffset).U(9.W)
            }
            val normal = Cat(exponent(7, 0), fraction.U(23.W))
            if (subnormals.isEmpty) {
                normal
            } else {
                MuxLookup(e, normal)(subnormals.zipWithIndex.map { case (bits, scale) =>
                    scale.U -> bits.U(31.W)
                })
            }
        }
        Seq(
            boundary(-2, 0x000000, Seq(0x00100000, 0x00200000, 0x00400000)), // 0.5d
            boundary(-1, 0x400000, Seq(0x00300000, 0x00600000)),             // 1.5d
            boundary(0, 0x200000, Seq(0x00500000)),                          // 2.5d
            boundary(0, 0x600000, Seq(0x00700000)),                          // 3.5d
            boundary(1, 0x200000),                                         // 5d
            boundary(1, 0x600000),                                         // 7d
            boundary(2, 0x200000)                                          // 10d
        )
    }

    def maxMagnitude(e: UInt): UInt = {
        require(e.getWidth == 8)
        val exponent = e.pad(9) + 3.U(9.W)
        Cat(exponent(7, 0), 0.U(23.W)) // 16d; finite through e=251.
    }

    // Scale-range qualification remains the caller's responsibility, just as
    // in the original block encoder. Inactive lanes have code0/no invalid.
    def apply(bits: UInt, active: Bool, thresholds: Seq[UInt], maximum: UInt): (UInt, Bool, Bool) = {
        require(bits.getWidth == 32 && thresholds.length == 7)
        val magnitude = bits(30, 0)
        val special = bits(30, 23).andR
        val isNaN = special && bits(22, 0).orR
        val isInf = special && !bits(22, 0).orR
        val magnitudeCode = PopCount(thresholds.map(magnitude > _))
        val code = Mux(
            active && !special && magnitudeCode.orR,
            Cat(bits(31), magnitudeCode),
            0.U(4.W)
        )
        val invalid = active && isNaN
        val eligible = !active || isNaN || (!isInf && magnitude <= maximum)
        (code, invalid, eligible)
    }
}

// d=2^(e-128). Elaboration selects either the C two-slot/four-group control
// with eight real lane datapaths, or the A/E single-output-slot control with
// 32 datapaths. Payload nibble j belongs to continuous K lane j.
class Mxfp4EncodeBlock(val lanes: Int = 8) extends Module {
    require(lanes == 8 || lanes == 32, "MXFP4 encoder supports only 8 or 32 lanes")
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val values = Vec(32, UInt(32.W))
            val e = UInt(8.W)
            val active = UInt(32.W)
        }))
        val out = Decoupled(new Bundle {
            val payload = UInt(128.W)
            val scale = UInt(8.W)
            val invalid = Bool()
            val eligible = Bool()
        })
    })

    // The eight-lane branch selects the registered group BEFORE these lane
    // datapaths. The 32-lane branch instead connects the current full input.
    val laneValues = Wire(Vec(lanes, UInt(32.W)))
    val laneActive = Wire(Vec(lanes, Bool()))
    val laneScale = Wire(UInt(8.W))

    // C's seven exact dyadic boundaries, shared across the selected lanes.
    // Unsigned positive FP32 bit order also covers the e=0/1/2 subnormals.
    val boundaries = Mxfp4EncodeLane.boundaries(laneScale)
    val maxMagnitude = Mxfp4EncodeLane.maxMagnitude(laneScale)
    val laneCodes = Wire(Vec(lanes, UInt(4.W)))
    val laneInvalid = Wire(Vec(lanes, Bool()))
    val laneEligible = Wire(Vec(lanes, Bool()))
    for (lane <- 0 until lanes) {
        val (code, invalid, eligible) = Mxfp4EncodeLane(
            laneValues(lane), laneActive(lane), boundaries, maxMagnitude)
        laneCodes(lane) := code
        laneInvalid(lane) := invalid
        laneEligible(lane) := eligible
    }

    val resetting = reset.asBool
    if (lanes == 8) {
        val s_idle :: s_encode :: Nil = Enum(2)
        val state = RegInit(s_idle)
        val valuesReg = Reg(Vec(32, UInt(32.W)))
        val activeReg = Reg(UInt(32.W))
        val scaleReg = Reg(UInt(8.W))
        val group = RegInit(0.U(2.W))
        val packedGroups = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
        val invalidReg = RegInit(false.B)
        val eligibleReg = RegInit(false.B)
        for (lane <- 0 until lanes) {
            val sourceIndex = Cat(group, lane.U(3.W))
            laneValues(lane) := valuesReg(sourceIndex)
            laneActive(lane) := activeReg(sourceIndex)
        }
        laneScale := scaleReg

        val outputValidReg = RegInit(false.B)
        val outputPayloadReg = Reg(UInt(128.W))
        val outputScaleReg = Reg(UInt(8.W))
        val outputInvalidReg = Reg(Bool())
        val outputEligibleReg = Reg(Bool())
        val outputAccept = !outputValidReg || io.out.ready
        val finalGroup = !resetting && state === s_encode && group === 3.U
        val completionAccepted = finalGroup && outputAccept
        io.in.ready := !resetting && (state === s_idle || completionAccepted)
        io.out.valid := !resetting && outputValidReg
        io.out.bits.payload := outputPayloadReg
        io.out.bits.scale := outputScaleReg
        io.out.bits.invalid := outputInvalidReg
        io.out.bits.eligible := outputEligibleReg

        val finalInvalid = invalidReg || laneInvalid.asUInt.orR
        val finalEligible = eligibleReg && laneEligible.asUInt.andR
        val finalPayload = VecInit(
            packedGroups(0), packedGroups(1), packedGroups(2), laneCodes.asUInt
        ).asUInt
        val replacementFire = completionAccepted && io.in.valid
        when(completionAccepted) {
            outputPayloadReg := Mux(finalEligible, finalPayload, 0.U(128.W))
            outputScaleReg := scaleReg
            outputInvalidReg := finalInvalid
            outputEligibleReg := finalEligible
            when(replacementFire) {
                valuesReg := io.in.bits.values
                activeReg := io.in.bits.active
                scaleReg := io.in.bits.e
                group := 0.U
                packedGroups := 0.U.asTypeOf(packedGroups)
                invalidReg := false.B
                eligibleReg := io.in.bits.e <= 251.U
            }.otherwise {
                state := s_idle
            }
        }.elsewhen(state === s_idle && io.in.fire) {
            valuesReg := io.in.bits.values
            activeReg := io.in.bits.active
            scaleReg := io.in.bits.e
            group := 0.U
            packedGroups := 0.U.asTypeOf(packedGroups)
            invalidReg := false.B
            eligibleReg := io.in.bits.e <= 251.U
            state := s_encode
        }.elsewhen(state === s_encode && group =/= 3.U) {
            packedGroups(group) := laneCodes.asUInt
            invalidReg := invalidReg || laneInvalid.asUInt.orR
            eligibleReg := eligibleReg && laneEligible.asUInt.andR
            group := group + 1.U
        }

        // Completion must win over simultaneous retirement of the old output.
        when(completionAccepted) {
            outputValidReg := true.B
        }.elsewhen(io.out.fire) {
            outputValidReg := false.B
        }
    } else {
        laneValues := io.in.bits.values
        laneActive := VecInit(io.in.bits.active.asBools)
        laneScale := io.in.bits.e
        val payloadReg = Reg(UInt(128.W))
        val scaleReg = Reg(UInt(8.W))
        val invalidReg = Reg(Bool())
        val eligibleReg = Reg(Bool())
        val validReg = RegInit(false.B)
        io.in.ready := !resetting && (!validReg || io.out.ready)
        io.out.valid := !resetting && validReg
        io.out.bits.payload := payloadReg
        io.out.bits.scale := scaleReg
        io.out.bits.invalid := invalidReg
        io.out.bits.eligible := eligibleReg
        val outputFire = io.out.fire
        val inputFire = io.in.fire
        when(outputFire) {
            validReg := false.B
        }
        when(inputFire) {
            val eligible = io.in.bits.e <= 251.U && laneEligible.asUInt.andR
            payloadReg := Mux(eligible, laneCodes.asUInt, 0.U(128.W))
            scaleReg := io.in.bits.e
            invalidReg := laneInvalid.asUInt.orR
            eligibleReg := eligible
            validReg := true.B
        }
    }
}

// d=2^(e-127). The F lane helper owns exact normalization, RNE and signed
// zero. Select F's eight-lane work/output slots or E's 32-lane output slot
// at elaboration; payload byte j always belongs to continuous K lane j.
class Mxfp8EncodeBlock(val e4m3: Boolean, val lanes: Int = 8) extends Module {
    require(lanes == 8 || lanes == 32, "MXFP8 encoder supports only 8 or 32 lanes")
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val values = Vec(32, UInt(32.W))
            val e = UInt(8.W)
            val active = UInt(32.W)
        }))
        val out = Decoupled(new Bundle {
            val payload = UInt(256.W)
            val scale = UInt(8.W)
            val invalid = Bool()
            val eligible = Bool()
        })
    })

    val laneValues = Wire(Vec(lanes, UInt(32.W)))
    val laneActive = Wire(Vec(lanes, Bool()))
    val laneScale = Wire(UInt(8.W))
    val laneCodes = Wire(Vec(lanes, UInt(8.W)))
    val laneInvalid = Wire(Vec(lanes, Bool()))
    val laneEligible = Wire(Vec(lanes, Bool()))
    for (lane <- 0 until lanes) {
        val (code, invalid, eligible) = Mxfp8EncodeLane(
            laneValues(lane), laneScale, laneActive(lane), e4m3)
        laneCodes(lane) := code
        laneInvalid(lane) := invalid
        laneEligible(lane) := eligible
    }

    val resetting = reset.asBool
    if (lanes == 8) {
        val s_idle :: s_encode :: Nil = Enum(2)
        val state = RegInit(s_idle)
        val valuesReg = Reg(Vec(32, UInt(32.W)))
        val activeReg = Reg(UInt(32.W))
        val scaleReg = Reg(UInt(8.W))
        val group = RegInit(0.U(2.W))
        val packedGroups = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
        val invalidReg = RegInit(false.B)
        val eligibleReg = RegInit(false.B)
        for (lane <- 0 until lanes) {
            val sourceIndex = Cat(group, lane.U(3.W))
            laneValues(lane) := valuesReg(sourceIndex)
            laneActive(lane) := activeReg(sourceIndex)
        }
        laneScale := scaleReg

        val outputValidReg = RegInit(false.B)
        val outputPayloadReg = Reg(UInt(256.W))
        val outputScaleReg = Reg(UInt(8.W))
        val outputInvalidReg = Reg(Bool())
        val outputEligibleReg = Reg(Bool())
        val outputAccept = !outputValidReg || io.out.ready
        val finalGroup = !resetting && state === s_encode && group === 3.U
        val completionAccepted = finalGroup && outputAccept
        io.in.ready := !resetting && (state === s_idle || completionAccepted)
        io.out.valid := !resetting && outputValidReg
        io.out.bits.payload := outputPayloadReg
        io.out.bits.scale := outputScaleReg
        io.out.bits.invalid := outputInvalidReg
        io.out.bits.eligible := outputEligibleReg

        val finalInvalid = invalidReg || laneInvalid.asUInt.orR
        val finalEligible = eligibleReg && laneEligible.asUInt.andR
        val finalPayload = VecInit(
            packedGroups(0), packedGroups(1), packedGroups(2), laneCodes.asUInt
        ).asUInt
        val replacementFire = completionAccepted && io.in.valid
        when(completionAccepted) {
            outputPayloadReg := Mux(finalEligible, finalPayload, 0.U(256.W))
            outputScaleReg := scaleReg
            outputInvalidReg := finalInvalid
            outputEligibleReg := finalEligible
            when(replacementFire) {
                valuesReg := io.in.bits.values
                activeReg := io.in.bits.active
                scaleReg := io.in.bits.e
                group := 0.U
                packedGroups := 0.U.asTypeOf(packedGroups)
                invalidReg := false.B
                eligibleReg := io.in.bits.e <= 254.U
            }.otherwise {
                state := s_idle
            }
        }.elsewhen(state === s_idle && io.in.fire) {
            valuesReg := io.in.bits.values
            activeReg := io.in.bits.active
            scaleReg := io.in.bits.e
            group := 0.U
            packedGroups := 0.U.asTypeOf(packedGroups)
            invalidReg := false.B
            eligibleReg := io.in.bits.e <= 254.U
            state := s_encode
        }.elsewhen(state === s_encode && group =/= 3.U) {
            packedGroups(group) := laneCodes.asUInt
            invalidReg := invalidReg || laneInvalid.asUInt.orR
            eligibleReg := eligibleReg && laneEligible.asUInt.andR
            group := group + 1.U
        }

        when(completionAccepted) {
            outputValidReg := true.B
        }.elsewhen(io.out.fire) {
            outputValidReg := false.B
        }
    } else {
        laneValues := io.in.bits.values
        laneActive := VecInit(io.in.bits.active.asBools)
        laneScale := io.in.bits.e
        val payloadReg = Reg(UInt(256.W))
        val scaleReg = Reg(UInt(8.W))
        val invalidReg = Reg(Bool())
        val eligibleReg = Reg(Bool())
        val validReg = RegInit(false.B)
        io.in.ready := !resetting && (!validReg || io.out.ready)
        io.out.valid := !resetting && validReg
        io.out.bits.payload := payloadReg
        io.out.bits.scale := scaleReg
        io.out.bits.invalid := invalidReg
        io.out.bits.eligible := eligibleReg
        val outputFire = io.out.fire
        val inputFire = io.in.fire
        when(outputFire) {
            validReg := false.B
        }
        when(inputFire) {
            val eligible = io.in.bits.e <= 254.U && laneEligible.asUInt.andR
            payloadReg := Mux(eligible, laneCodes.asUInt, 0.U(256.W))
            scaleReg := io.in.bits.e
            invalidReg := laneInvalid.asUInt.orR
            eligibleReg := eligible
            validReg := true.B
        }
    }
}
