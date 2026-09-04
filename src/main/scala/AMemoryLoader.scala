
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
import xscache.coupledL2.prefetch.{MatrixPrefetchStream, MatrixPrefetchTagCodec}

// AMemoryLoader：加载 A 矩阵数据到 MatrixReg，仅支持矩阵加载（无卷积）
// 参考 CMemoryLoader 的 FullLoad 实现，按 M、K 维度顺序访存

class ASourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(ABMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(ABMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
    val BeatIndex = UInt(log2Ceil(ABMatrixRegEntryByteSize).W)
}

object TransposeBytePlane {
    private val BytesPerElementWidth = 3

    def bytesPerElement(dataType: UInt): UInt = {
        MuxLookup(dataType, 0.U(BytesPerElementWidth.W))(Seq(
            ElementDataType.DataTypeWidth8  -> 1.U(BytesPerElementWidth.W),
            ElementDataType.DataTypeWidth16 -> 2.U(BytesPerElementWidth.W),
            ElementDataType.DataTypeWidth32 -> 4.U(BytesPerElementWidth.W)
        ))
    }

    def isSupported(bytesPerElement: UInt): Bool = {
        bytesPerElement === 1.U || bytesPerElement === 2.U || bytesPerElement === 4.U
    }

    def groupRows(bytesPerElement: UInt, entryBytes: Int): UInt = {
        require(entryBytes % 4 == 0, "transpose entry width must support e32 byte planes")
        val width = log2Ceil(entryBytes + 1)
        MuxLookup(bytesPerElement, 0.U(width.W))(Seq(
            1.U(BytesPerElementWidth.W) -> entryBytes.U(width.W),
            2.U(BytesPerElementWidth.W) -> (entryBytes / 2).U(width.W),
            4.U(BytesPerElementWidth.W) -> (entryBytes / 4).U(width.W)
        ))
    }

    private def fixedShift(value: UInt, shift: Int, outputWidth: Int): UInt = {
        require(shift >= 0, "transpose shift must be non-negative")
        require(outputWidth >= value.getWidth + shift, "transpose shift result width is too narrow")
        (value << shift).pad(outputWidth)
    }

    // Do not turn the E-derived shift amount into a runtime barrel shifter.
    // The E=1/2/4 cases select three elaboration-time constant shifts instead.
    private def scaleByElementWidth(value: UInt, bytesPerElement: UInt, e8Shift: Int, outputWidth: Int): UInt = {
        require(e8Shift >= 2, "transpose e32 scaling requires at least a two-bit e8 shift")
        MuxLookup(bytesPerElement, 0.U(outputWidth.W))(Seq(
            1.U(BytesPerElementWidth.W) -> fixedShift(value, e8Shift, outputWidth),
            2.U(BytesPerElementWidth.W) -> fixedShift(value, e8Shift - 1, outputWidth),
            4.U(BytesPerElementWidth.W) -> fixedShift(value, e8Shift - 2, outputWidth)
        ))
    }

    def groupBase(groupIndex: UInt, bytesPerElement: UInt, entryBytes: Int): UInt = {
        require(isPow2(entryBytes), "transpose entry width must be a power of two")
        val e8Shift = log2Ceil(entryBytes)
        val outputWidth = groupIndex.getWidth + log2Ceil(entryBytes + 1)
        scaleByElementWidth(groupIndex, bytesPerElement, e8Shift, outputWidth)
    }

    def beatEntryBase(beatIndex: UInt, bytesPerElement: UInt, byteSlots: Int, reduceGroupSize: Int): UInt = {
        require(isPow2(byteSlots), "transpose response slots must be a power of two")
        require(isPow2(reduceGroupSize), "transpose reduce group size must be a power of two")
        val e8Shift = log2Ceil(byteSlots) + log2Ceil(reduceGroupSize)
        val outputWidth = beatIndex.getWidth + log2Ceil(byteSlots + 1) + log2Ceil(reduceGroupSize + 1)
        scaleByElementWidth(beatIndex, bytesPerElement, e8Shift, outputWidth)
    }

    def reduceGroupOffset(elementSlot: UInt, reduceGroupSize: Int): UInt = {
        require(isPow2(reduceGroupSize), "transpose reduce group size must be a power of two")
        elementSlot << log2Ceil(reduceGroupSize)
    }

    def sourceByteIndex(bank: Int, slot: Int, bytesPerElement: Int, bankCount: Int): Int = {
        require(Seq(1, 2, 4).contains(bytesPerElement), "unsupported transpose element width")
        bytesPerElement * (bank + bankCount * (slot / bytesPerElement)) + (slot % bytesPerElement)
    }

    def entryOffset(rowOffset: UInt, phase: UInt, bytesPerElement: UInt, entryOffsetBits: Int): UInt = {
        val e16Offset = Cat(rowOffset, phase(0))(entryOffsetBits - 1, 0)
        val e32Offset = Cat(rowOffset, phase(1, 0))(entryOffsetBits - 1, 0)
        MuxLookup(bytesPerElement, 0.U(entryOffsetBits.W))(Seq(
            1.U(BytesPerElementWidth.W) -> rowOffset,
            2.U(BytesPerElementWidth.W) -> e16Offset,
            4.U(BytesPerElementWidth.W) -> e32Offset
        ))
    }

    def elementSlot(phase: UInt, bytesPerElement: UInt): UInt = {
        MuxLookup(bytesPerElement, 0.U(phase.getWidth.W))(Seq(
            1.U(BytesPerElementWidth.W) -> phase,
            2.U(BytesPerElementWidth.W) -> (phase >> 1).pad(phase.getWidth),
            4.U(BytesPerElementWidth.W) -> (phase >> 2).pad(phase.getWidth)
        ))
    }

    def tailAligned(tailBytes: UInt, bytesPerElement: UInt): Bool = {
        MuxLookup(bytesPerElement, false.B)(Seq(
            1.U(BytesPerElementWidth.W) -> true.B,
            2.U(BytesPerElementWidth.W) -> !tailBytes(0),
            4.U(BytesPerElementWidth.W) -> !tailBytes(1, 0).orR
        ))
    }
}

class TransDataPacket(implicit p: Parameters) extends CuteBundle {
    val data = UInt(8.W)
    val mask = Bool()
    val entry_offset = UInt(log2Ceil(ABMatrixRegEntryByteSize).W)
    val phase = UInt(log2Ceil(Trans_Load_Size).W)
}

class TransAlignPipe(bankId: Int, debugEnable: Boolean = false)(implicit p: Parameters) extends CuteModule {
    private val transLoadSize = Trans_Load_Size
    private val transLoadSizeBits = log2Ceil(transLoadSize)
    private val entryOffsetBits = log2Ceil(ABMatrixRegEntryByteSize)
    private val routerLatency = 3
    private val drainCycles = transLoadSize + 2 + routerLatency // 两级 barrel shifter 和 OOORouter 三级流水都需要排空。

    val io = IO(new Bundle {
        val in_data = Input(UInt(outsideDataWidth.W))
        val in_mask = Input(UInt(outsideDataWidthByte.W))
        val resp_beat_cnt = Input(UInt(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
        val entry_offset = Input(UInt(entryOffsetBits.W))
        val bytes_per_element = Input(UInt(3.W))
        val debug_time = Input(UInt(64.W))
        val is_drain_trigger = Input(Bool())
        val in_valid = Input(Bool())
        val bus_stall = Output(Bool())
        val empty = Output(Bool())
        val out = Decoupled(Vec(transLoadSize, new TransDataPacket))
    })

    require(isPow2(transLoadSize), "Trans_Load_Size must be power of 2")
    require(transLoadSize == 8 || transLoadSize == 16, "Trans_Load_Size currently supports 8 or 16")

    val s_normal :: s_drain :: Nil = Enum(2)
    val state = RegInit(s_normal)
    val drain_cnt = RegInit(0.U(log2Ceil(drainCycles + 1).W))

    val out_valid_now = Wire(Bool())
    val outputPhase = RegInit(0.U(transLoadSizeBits.W))

    require(ABMatrixRegEntryByteSize % 4 == 0, "transpose entry width must support e32 byte planes")
    require(outsideDataWidthByte % (ABMatrixRegNBanks * 4) == 0, "transpose response width must support e32 byte planes")
    val inBytes = io.in_data.asTypeOf(Vec(outsideDataWidthByte, UInt(8.W)))
    val inMaskBits = io.in_mask.asBools
    val bankData = VecInit((0 until transLoadSize).map { slot =>
        val candidates = Seq(1, 2, 4).map { bytesPerElement =>
            val byteIdx = TransposeBytePlane.sourceByteIndex(bankId, slot, bytesPerElement, ABMatrixRegNBanks)
            bytesPerElement.U(3.W) -> Mux(inMaskBits(byteIdx), inBytes(byteIdx), 0.U(8.W))
        }
        MuxLookup(io.bytes_per_element, 0.U(8.W))(candidates)
    })
    val bankByteValid = VecInit((0 until transLoadSize).map { slot =>
        val candidates = Seq(1, 2, 4).map { bytesPerElement =>
            val byteIdx = TransposeBytePlane.sourceByteIndex(bankId, slot, bytesPerElement, ABMatrixRegNBanks)
            bytesPerElement.U(3.W) -> inMaskBits(byteIdx)
        }
        MuxLookup(io.bytes_per_element, false.B)(candidates)
    })

    def rotateLeft[T <: Data](data: Vec[T], amount: UInt): Vec[T] = {
        VecInit((0 until transLoadSize).map { i =>
            val idx = (i.U(transLoadSizeBits.W) + amount.pad(transLoadSizeBits))(transLoadSizeBits - 1, 0)
            data(idx)
        })
    }

    val shift_amt = io.resp_beat_cnt(transLoadSizeBits - 1, 0)
    val shift_low = shift_amt(1, 0)
    val shift_high_amt = if (transLoadSizeBits > 2) Cat(shift_amt(transLoadSizeBits - 1, 2), 0.U(2.W)) else 0.U(transLoadSizeBits.W)

    val coarseData = rotateLeft(bankData, shift_high_amt)
    val coarseByteValid = rotateLeft(bankByteValid, shift_high_amt)

    val stage1Data = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(8.W))))
    val stage1Valid = RegInit(false.B)
    val stage1ByteValid = RegInit(VecInit(Seq.fill(transLoadSize)(false.B)))
    val stage1ShiftLow = RegInit(0.U(2.W))
    val stage1EntryOffset = RegInit(0.U(entryOffsetBits.W))

    val stage2Data = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(8.W))))
    val stage2Valid = RegInit(false.B)
    val stage2ByteValid = RegInit(VecInit(Seq.fill(transLoadSize)(false.B)))
    val stage2EntryOffset = RegInit(0.U(entryOffsetBits.W))

    val fineData = rotateLeft(stage1Data, stage1ShiftLow)
    val fineByteValid = rotateLeft(stage1ByteValid, stage1ShiftLow)

    val laneData = Seq.tabulate(transLoadSize) { i =>
        RegInit(VecInit(Seq.fill(i + 1)(0.U(8.W))))
    }
    // laneValid marks occupancy. Byte validity follows the same path so an
    // invalid tail byte cannot turn into a masked write later in the router.
    val laneValid = Seq.tabulate(transLoadSize) { i =>
        RegInit(VecInit(Seq.fill(i + 1)(false.B)))
    }
    val laneByteValid = Seq.tabulate(transLoadSize) { i =>
        RegInit(VecInit(Seq.fill(i + 1)(false.B)))
    }
    // offset 与最后一级 lane 的物理位置绑定，而不是一拍内所有 lane 共享。
    // 新输入的 offset 进入 offset(0)，随后沿 offset(0)->offset(1)->... 移动，
    // 对齐“同一笔输入数据依次出现在最后一级第 0、1、...、N-1 个位置”的时空轨迹。
    val entryOffsetPipe = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(entryOffsetBits.W))))

    val laneValidBits = laneValid.zipWithIndex.flatMap { case (validVec, laneIdx) =>
        (0 to laneIdx).map(validVec(_))
    }
    val any_valid_in_lane_pipe = laneValidBits.reduce(_ || _)
    val any_valid_in_pipe = stage1Valid || stage2Valid || any_valid_in_lane_pipe
    val input_accept = state === s_normal && io.in_valid
    val pipe_advance = input_accept || (state === s_drain)

    if (bankId == 0 && debugEnable) {
        when(input_accept) {
            printf("[AML_TRANS_ALIGN_IN<%d>] beatCnt:%d shift:%d shiftHigh:%d shiftLow:%d entry:%d raw:%x mask:%x bankData:%x coarse:%x drainTrig:%d\n",
              io.debug_time, io.resp_beat_cnt, shift_amt, shift_high_amt, shift_low,
              io.entry_offset, io.in_data, io.in_mask, bankData.asUInt,
              coarseData.asUInt, io.is_drain_trigger.asUInt)
        }
    }

    // MatrixReg 写侧恒 ready，因此转置对齐流水线不再等待末端 valid/ready。
    // drain 期间仍反压总线响应，防止下一组数据在旧组 router 三级流水未排空前进入。
    io.bus_stall := state === s_drain
    io.empty := state === s_normal && !any_valid_in_pipe

    when(pipe_advance) {
        stage1Data := coarseData
        stage1Valid := input_accept
        stage1ByteValid := coarseByteValid
        stage1ShiftLow := shift_low
        stage1EntryOffset := io.entry_offset

        stage2Data := fineData
        stage2Valid := stage1Valid
        stage2ByteValid := fineByteValid
        stage2EntryOffset := stage1EntryOffset

        for (i <- 0 until transLoadSize) {
            laneData(i)(0) := stage2Data(i)
            laneValid(i)(0) := stage2Valid
            laneByteValid(i)(0) := stage2ByteValid(i)
            for (j <- 1 to i) {
                laneData(i)(j) := laneData(i)(j - 1)
                laneValid(i)(j) := laneValid(i)(j - 1)
                laneByteValid(i)(j) := laneByteValid(i)(j - 1)
            }
        }

        entryOffsetPipe(0) := Mux(stage2Valid, stage2EntryOffset, 0.U)
        for (i <- 1 until transLoadSize) {
            entryOffsetPipe(i) := entryOffsetPipe(i - 1)
        }
    }

    val outPackets = Wire(Vec(transLoadSize, new TransDataPacket))
    val outValids = Wire(Vec(transLoadSize, Bool()))
    for (i <- 0 until transLoadSize) {
        val depthIdx = i
        outPackets(i).data := laneData(i)(depthIdx)
        outPackets(i).mask := laneValid(i)(depthIdx) && laneByteValid(i)(depthIdx)
        outPackets(i).entry_offset := TransposeBytePlane.entryOffset(
            entryOffsetPipe(i), outputPhase, io.bytes_per_element, entryOffsetBits
        )
        outPackets(i).phase := outputPhase
        outValids(i) := laneValid(i)(depthIdx)
    }
    out_valid_now := outValids.asUInt.orR

    io.out.bits := outPackets
    io.out.valid := out_valid_now && pipe_advance

    when(io.empty && !io.in_valid) {
        outputPhase := 0.U
    }.elsewhen(io.out.valid) {
        outputPhase := Mux(
            outputPhase === (transLoadSize - 1).U,
            0.U,
            outputPhase + 1.U
        )
    }

    if (bankId == 0 && debugEnable) {
        val outDataUInt = VecInit((0 until transLoadSize).map(i => outPackets(i).data)).asUInt
        val outEntryUInt = VecInit((0 until transLoadSize).map(i => outPackets(i).entry_offset)).asUInt
        when(io.out.valid) {
            printf("[AML_TRANS_ALIGN_OUT<%d>] valids:%b data:%x entry:%x\n",
              io.debug_time, outValids.asUInt, outDataUInt, outEntryUInt)
        }
    }

    switch(state) {
        is(s_normal) {
            when(input_accept && io.is_drain_trigger) {
                state := s_drain
                drain_cnt := (drainCycles - 1).U
            }
        }
        is(s_drain) {
            when(drain_cnt === 0.U) {
                state := s_normal
            }.otherwise {
                drain_cnt := drain_cnt - 1.U
            }
        }
    }
}

class OOORouter(implicit p: Parameters) extends CuteModule {
    private val transLoadSize = Trans_Load_Size
    private val entryOffsetBits = log2Ceil(ABMatrixRegEntryByteSize)
    private val groupSize = math.max(1, transLoadSize / 4)

    val io = IO(new Bundle {
        val in = Flipped(Decoupled(Vec(transLoadSize, new TransDataPacket)))
        val final_data = Output(UInt(ABMatrixRegEntryBitSize.W))
        val final_mask = Output(UInt(ABMatrixRegEntryByteSize.W))
        val valid = Output(Bool())
        val txn_valid = Output(Bool())
        val phase = Output(UInt(log2Ceil(transLoadSize).W))
        val empty = Output(Bool())
    })

    require(ABMatrixRegEntryByteSize == 32, "OOORouter currently targets 32B AB MatrixReg entries")
    require(transLoadSize == 8 || transLoadSize == 16, "OOORouter currently supports Trans_Load_Size 8 or 16")

    io.in.ready := true.B

    val inputFire = io.in.valid && io.in.ready

    // Byte mask 与 data 使用同一套 offset 译码和规约树。
    // 不保留历史 life mask，避免在当前拍没有有效 data 的 byte 上误拉高写使能。
    val stage1Data = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(32.W))))
    val stage1Mask = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(4.W))))
    val stage1WordIdx = RegInit(VecInit(Seq.fill(transLoadSize)(0.U(3.W))))
    val stage1Valid = RegInit(VecInit(Seq.fill(transLoadSize)(false.B)))
    val stage1TxnValid = RegInit(false.B)
    val stage1Phase = RegInit(0.U(log2Ceil(transLoadSize).W))

    val stage2Data = RegInit(VecInit(Seq.fill(4)(0.U(ABMatrixRegEntryBitSize.W))))
    val stage2Mask = RegInit(VecInit(Seq.fill(4)(0.U(ABMatrixRegEntryByteSize.W))))
    val stage2TxnValid = RegInit(false.B)
    val stage2Phase = RegInit(0.U(log2Ceil(transLoadSize).W))

    val stage3Data = RegInit(0.U(ABMatrixRegEntryBitSize.W))
    val stage3Mask = RegInit(0.U(ABMatrixRegEntryByteSize.W))
    val stage3TxnValid = RegInit(false.B)
    val stage3Phase = RegInit(0.U(log2Ceil(transLoadSize).W))

    for (i <- 0 until transLoadSize) {
        val pkt = io.in.bits(i)
        val byteInWord = pkt.entry_offset(1, 0)
        val wordIdx = pkt.entry_offset(entryOffsetBits - 1, 2)
        stage1Data(i) := Mux(inputFire && pkt.mask, (pkt.data.pad(32) << (byteInWord << 3))(31, 0), 0.U)
        stage1Mask(i) := Mux(inputFire && pkt.mask, UIntToOH(byteInWord, 4).asUInt, 0.U)
        stage1WordIdx(i) := wordIdx
        stage1Valid(i) := inputFire && pkt.mask
    }
    stage1TxnValid := inputFire
    stage1Phase := Mux(inputFire, io.in.bits(0).phase, 0.U)

    val expandedData = (0 until transLoadSize).map { i =>
        Mux(stage1Valid(i), (stage1Data(i).asUInt << (stage1WordIdx(i) << 5))(ABMatrixRegEntryBitSize - 1, 0), 0.U(ABMatrixRegEntryBitSize.W))
    }
    val expandedMask = (0 until transLoadSize).map { i =>
        Mux(stage1Valid(i), (stage1Mask(i).asUInt << (stage1WordIdx(i) << 2))(ABMatrixRegEntryByteSize - 1, 0), 0.U(ABMatrixRegEntryByteSize.W))
    }

    for (g <- 0 until 4) {
        val start = g * groupSize
        val end = math.min(start + groupSize, transLoadSize)
        val dataTerms = (start until end).map(expandedData)
        val maskTerms = (start until end).map(expandedMask)
        val reducedData = if (dataTerms.nonEmpty) dataTerms.reduce(_ | _) else 0.U(ABMatrixRegEntryBitSize.W)
        val reducedMask = if (maskTerms.nonEmpty) maskTerms.reduce(_ | _) else 0.U(ABMatrixRegEntryByteSize.W)
        stage2Data(g) := reducedData
        stage2Mask(g) := reducedMask
    }
    stage2TxnValid := stage1TxnValid
    stage2Phase := stage1Phase

    stage3Data := stage2Data.reduce(_ | _)
    stage3Mask := stage2Mask.reduce(_ | _)
    stage3TxnValid := stage2TxnValid
    stage3Phase := stage2Phase

    io.final_data := stage3Data
    io.final_mask := stage3Mask
    io.valid := stage3TxnValid && stage3Mask.orR
    io.txn_valid := stage3TxnValid
    io.phase := stage3Phase
    io.empty := !stage1TxnValid && !stage2TxnValid && !stage3TxnValid
}

class AMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.Data.map(_.valid := false.B)
    io.ToMatrixRegIO.Data.map(_.bits := DontCare)
    io.ToMatrixRegIO.ByteMask.map(_.valid := false.B)
    io.ToMatrixRegIO.ByteMask.map(_.bits := Fill(ABMatrixRegEntryByteSize, true.B))

    // Initialize all LocalMMUIO channels; the legacy AML only drives channel 0.
    for (i <- 0 until ABMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := DontCare
        io.LocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LocalMMUIO.Request(i).bits.MatrixPrefetchTag := 0.U
        io.LocalMMUIO.Response(i).ready := false.B
    }
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    // Legacy AML only uses channel 0 for requests and responses.
    val Request = io.LocalMMUIO.Request(0)
    val Response = io.LocalMMUIO.Response(0)

    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
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

    val ConfigInfo = io.ConfigInfo
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W))
    val ApplicationTensor_A_Stride_M = RegInit(0.U(MMUAddrWidth.W))
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Conherent = RegInit(true.B)
    val Is_Transpose = RegInit(false.B)
    val transposeBytesPerElement = RegInit(1.U(3.W))

    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)
    val PrefetchTaskId = RegInit(0.U(MatrixPrefetchTagCodec.taskIdWidth.W))
    val PrefetchStream = RegInit(MatrixPrefetchStream.none)

    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val s_load_idle :: s_load_init :: s_load_working :: s_load_quiesce :: s_load_end :: Nil = Enum(5)
    val memoryload_state = RegInit(s_load_idle)
    private val transposeEndDrainCycles = Trans_Load_Size + 2 + 3
    val transposeEndDrainCnt = RegInit(0.U(log2Ceil(transposeEndDrainCycles + 1).W))

    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*outsideDataWidthByte)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, ABMatrixRegEntryByteSize)).W))

    val group_req_cnt = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
    val group_resp_cnt = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
    val group_size_reg = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))

    private val transBaseAddrBits = log2Ceil(ABMatrixRegBankNEntries)

    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new ASourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(ABMatrixRegBankNEntries).W)))))
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/ABMatrixRegEntryByteSize)+1).W)))))
    val MReg_Fill_Table_IsTail = RegInit(VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(false.B)))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)
    val MAX_Fill_Times = outsideDataWidthByte/ABMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))))
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(true.B)))

    for(i <- 0 until ABMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), AMemoryLoaderReadFromMemoryFIFODepth)
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)
    }

    val transAlignPipes = Seq.tabulate(ABMatrixRegNBanks)(i => Module(new TransAlignPipe(i, YJPAMLDebugEnable)))
    val transRouters = Seq.tabulate(ABMatrixRegNBanks)(_ => Module(new OOORouter))
    val transPipeInValid = WireInit(false.B)
    val transPipeInData = WireInit(0.U(outsideDataWidth.W))
    val transPipeInMask = WireInit(0.U(outsideDataWidthByte.W))
    val transPipeRespBeatCnt = WireInit(0.U(group_resp_cnt.getWidth.W))
    val transPipeEntryOffset = WireInit(0.U(log2Ceil(ABMatrixRegEntryByteSize).W))
    val transPipeDrainTrigger = WireInit(false.B)
    val transBusStall = transAlignPipes.map(_.io.bus_stall).reduce(_ || _)
    val transWriteBaseAddr = RegInit(0.U(transBaseAddrBits.W))
    val transposeGroupRows = TransposeBytePlane.groupRows(transposeBytesPerElement, ABMatrixRegEntryByteSize)

    for (i <- 0 until ABMatrixRegNBanks) {
        transAlignPipes(i).io.in_data := transPipeInData
        transAlignPipes(i).io.in_mask := transPipeInMask
        transAlignPipes(i).io.resp_beat_cnt := transPipeRespBeatCnt
        transAlignPipes(i).io.entry_offset := transPipeEntryOffset
        transAlignPipes(i).io.bytes_per_element := transposeBytesPerElement
        transAlignPipes(i).io.debug_time := io.DebugInfo.DebugTimeStampe
        transAlignPipes(i).io.is_drain_trigger := transPipeDrainTrigger
        transAlignPipes(i).io.in_valid := transPipeInValid
        transRouters(i).io.in <> transAlignPipes(i).io.out
    }
    val transAlignEmpty = transAlignPipes.map(_.io.empty).reduce(_ && _)
    val transRouterEmpty = transRouters.map(_.io.empty).reduce(_ && _)
    val transPipelineEmpty = transAlignEmpty && transRouterEmpty
    val transRouterValidVec = VecInit(transRouters.map(_.io.valid)).asUInt
    val transRouterWriteValid = transRouters.map(_.io.valid).reduce(_ || _)
    val transRouterTxnValid = transRouters.map(_.io.txn_valid).reduce(_ || _)
    val transWritePhase = transRouters.head.io.phase
    val transWriteElementSlot = TransposeBytePlane.elementSlot(transWritePhase, transposeBytesPerElement)
    val transWriteAddrOffset = TransposeBytePlane.reduceGroupOffset(transWriteElementSlot, ReduceGroupSize)
    val transWriteAddrWide = transWriteBaseAddr +& transWriteAddrOffset
    val transWriteAddr = transWriteAddrWide(transBaseAddrBits - 1, 0)

    when(Is_Transpose && transRouterTxnValid) {
        assert(transWriteAddrWide < ABMatrixRegBankNEntries.U,
            "transpose write address must stay within an AB MatrixReg bank")
    }

    when(state === s_idle){
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            state := s_mm_task
            memoryload_state := s_load_init
            MatrixRegTensor_M := ConfigInfo.MatrixRegTensor_M
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            Tensor_Block_BaseAddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
            ApplicationTensor_A_Stride_M := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M
            HasTail := ConfigInfo.ApplicationTensor_A.HasTail
            TailByteMask := ConfigInfo.ApplicationTensor_A.TailByteMask
            K_Beat_Count := ConfigInfo.ApplicationTensor_A.K_Beat_Count
            Is_ZeroLoad := ConfigInfo.LoadTaskInfo.Is_ZeroLoad
            Is_FullLoad := ConfigInfo.LoadTaskInfo.Is_FullLoad
            PrefetchTaskId := ConfigInfo.PrefetchTaskId
            PrefetchStream := ConfigInfo.PrefetchStream
            Conherent := ConfigInfo.Conherent
            Is_Transpose := ConfigInfo.Is_Transpose
            transposeBytesPerElement := TransposeBytePlane.bytesPerElement(ConfigInfo.ApplicationTensor_A.dataType)
            if(YJPAMLDebugEnable){
                printf("[AML<%d>]AMemoryLoader Task Start, MatrixRegTensor_M:%d, MatrixRegTensor_K:%d, BaseVaddr:%x, Stride_M:%x, dataType:%d, Is_Transpose:%d, Is_ZeroLoad:%d, Is_FullLoad:%d\n",
                  io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_K,
                  ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr, ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M,
                  ConfigInfo.ApplicationTensor_A.dataType, ConfigInfo.Is_Transpose.asUInt,
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
            group_req_cnt := 0.U
            group_resp_cnt := 0.U
            group_size_reg := 0.U
            transposeEndDrainCnt := 0.U
            transWriteBaseAddr := 0.U
            MaxRequestIter := MatrixRegTensor_M * K_Beat_Count
            Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table := 0.U.asTypeOf(MReg_Fill_Table)
            MReg_Fill_Table_MReg_Addr := 0.U.asTypeOf(MReg_Fill_Table_MReg_Addr)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
            MReg_Fill_Table_IsTail := VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(false.B))
        }
        is(s_load_working) {
            io.ToMatrixRegIO.active := true.B
            assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad)) === 1.U,
                  "Error! AML Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad should be true!")
            when(Is_Transpose) {
                assert(TransposeBytePlane.isSupported(transposeBytesPerElement),
                    "AML transpose supports only e8/e16/e32 element widths")
                when(HasTail) {
                    assert(TransposeBytePlane.tailAligned(TailByteMask, transposeBytesPerElement),
                        "AML transpose tail must end on a complete element")
                }
            }

            when(Is_ZeroLoad){
                val Max_ZeroLoad_Write_Times = ABMatrixRegBankNEntries
                for (i <- 0 until ABMatrixRegNBanks){
                    io.ToMatrixRegIO.BankAddr(i).bits := TotalLoadSize
                    io.ToMatrixRegIO.BankAddr(i).valid := true.B
                    io.ToMatrixRegIO.Data(i).bits := 0.U
                    io.ToMatrixRegIO.Data(i).valid := true.B
                    io.ToMatrixRegIO.ByteMask(i).bits := Fill(ABMatrixRegEntryByteSize, true.B)
                    io.ToMatrixRegIO.ByteMask(i).valid := true.B
                }
                TotalLoadSize := TotalLoadSize + 1.U
                if (YJPAMLDebugEnable) printf("[AML<%d>]ZeroLoad, TotalLoadSize: %d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize)
                when(TotalLoadSize === (Max_ZeroLoad_Write_Times - 1).U){
                    memoryload_state := s_load_end
                    if (YJPAMLDebugEnable) printf("[AML<%d>]ZeroLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                }
            }

            when(Is_FullLoad){
                //先转换成独热码然后进行减一即可计算出掩码
                val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
                val fullTaskMask = Fill(outsideDataWidthByte, true.B)
                val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))

                // Transpose request path reuses the normal iter registers:
                //   CurrentLoaded_BlockTensor_M_Iter -> large_M group index
                //   CurrentLoaded_BlockTensor_K_Iter -> K/M-beat index
                //   Request_M_Iter_Time              -> small_M inside current group
                val transpose_large_m_base = TransposeBytePlane.groupBase(
                    CurrentLoaded_BlockTensor_M_Iter, transposeBytesPerElement, ABMatrixRegEntryByteSize
                )
                val transpose_current_m = transpose_large_m_base + Request_M_Iter_Time
                val transpose_group_in_range = transpose_large_m_base < MatrixRegTensor_M
                val transpose_group_remain = Mux(transpose_group_in_range, MatrixRegTensor_M - transpose_large_m_base, 0.U)
                val current_group_size = Wire(UInt(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
                current_group_size := Mux(
                    transpose_group_remain < transposeGroupRows,
                    transpose_group_remain(current_group_size.getWidth - 1, 0),
                    transposeGroupRows
                )
                val group_has_no_requests = group_req_cnt === 0.U && group_resp_cnt === 0.U
                val group_is_idle = group_has_no_requests && transPipelineEmpty
                val active_group_size = Mux(group_has_no_requests, current_group_size, group_size_reg)
                val transpose_group_can_issue = Mux(
                    group_has_no_requests,
                    group_is_idle && (current_group_size =/= 0.U),
                    group_req_cnt < active_group_size
                )
                val transpose_req_enable = (TotalRequestSize < MaxRequestIter) && transpose_group_can_issue

                // 矩阵访存顺序：按 M 分 bank 交织，再扫 K。地址 = BaseAddr + M*Stride_M + K*64B
                val RequestMatrixRegMIndex = Mux(Is_Transpose, transpose_current_m, CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time)
                val NormalRequestMatrixRegBankId = RequestMatrixRegMIndex % ABMatrixRegNBanks.U
                val NormalRequestMatrixRegBaseAddr = ((RequestMatrixRegMIndex / ABMatrixRegNBanks.U) * ReduceGroupSize.U)
                val NormalRequestMatrixRegAddr = NormalRequestMatrixRegBaseAddr + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(MAX_Fill_Times))
                val TransposeRequestMatrixRegAddr = TransposeBytePlane.beatEntryBase(
                    CurrentLoaded_BlockTensor_K_Iter, transposeBytesPerElement, Trans_Load_Size, ReduceGroupSize
                ) + CurrentLoaded_BlockTensor_M_Iter
                val RequestMatrixRegBankId = Mux(Is_Transpose, 0.U, NormalRequestMatrixRegBankId)
                val RequestMatrixRegAddr = Mux(Is_Transpose, TransposeRequestMatrixRegAddr, NormalRequestMatrixRegAddr)

                Request.bits.RequestAddr := Tensor_Block_BaseAddr + RequestMatrixRegMIndex * ApplicationTensor_A_Stride_M + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
                val sourceId = Mux(Conherent, io.LocalMMUIO.ConherentRequsetSourceID, io.LocalMMUIO.nonConherentRequsetSourceID)
                Request.bits.RequestConherent := Conherent
                Request.bits.RequestSourceID := sourceId.bits
                Request.bits.RequestType_isWrite := false.B
                Request.bits.UseAllocatedSourceID := true.B
                Request.bits.MatrixPrefetchTag := MatrixPrefetchTagCodec.encode(
                  Is_FullLoad && !Is_ZeroLoad,
                  PrefetchStream,
                  PrefetchTaskId
                )
                Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
                Request.valid := Mux(Is_Transpose, transpose_req_enable, TotalRequestSize < MaxRequestIter)

                when(Request.fire){
                    val TableItem = Wire(new ASourceIdSearch)
                    TableItem.MatrixRegBankId := RequestMatrixRegBankId
                    TableItem.MatrixRegAddr := RequestMatrixRegAddr
                    TableItem.MatrixRegisTail := RequestBeatIsTail
                    TableItem.BeatIndex := Request_M_Iter_Time
                    SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                    if (YJPAMLDebugEnable) {
                        printf("[AML_RequestHandshake<%d>] M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d, SourceId:%d, Tail:%d\n",
                          io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_M_Iter_Time, Request.bits.RequestAddr, RequestMatrixRegBankId, RequestMatrixRegAddr, sourceId.bits, RequestBeatIsTail)
                    }

                    when(Is_Transpose) {
                        if (YJPAMLDebugEnable) {
                            printf("[AML_TRANS_REQ<%d>] totalReq:%d groupReq:%d groupResp:%d idle:%d largeM:%d kBeat:%d beat:%d groupSize:%d activeGroupSize:%d regBase:%d vaddr:%x source:%d tail:%d\n",
                              io.DebugInfo.DebugTimeStampe, TotalRequestSize, group_req_cnt, group_resp_cnt,
                              group_is_idle.asUInt, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                              Request_M_Iter_Time, current_group_size, active_group_size, RequestMatrixRegAddr,
                              Request.bits.RequestAddr, sourceId.bits, RequestBeatIsTail.asUInt)
                        }

                        val small_m_reach_group_boundary = Request_M_Iter_Time === (transposeGroupRows - 1.U)
                        val small_m_reach_tensor_boundary = transpose_current_m === (MatrixRegTensor_M - 1.U)
                        val small_m_wrap = small_m_reach_group_boundary || small_m_reach_tensor_boundary
                        val k_wrap = CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U)

                        when(group_is_idle) {
                            group_size_reg := current_group_size
                        }
                        group_req_cnt := group_req_cnt + 1.U

                        Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                        when(small_m_wrap) {
                            Request_M_Iter_Time := 0.U
                            CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                            when(k_wrap) {
                                CurrentLoaded_BlockTensor_K_Iter := 0.U
                                CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + 1.U
                            }
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
                    if (YJPAMLDebugEnable){
                        printf("[AML<%d>]FullLoad Request, M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d\n",
                          io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_M_Iter_Time, Request.bits.RequestAddr, RequestMatrixRegBankId, RequestMatrixRegAddr)
                    }
                }

                val current_fill_fifo_full = WireInit(false.B)
                when(Response.valid && !Is_Transpose){
                    val respSourceId = Response.bits.ReseponseSourceID
                    val MatrixRegBankId = SoureceIdSearchTable(respSourceId).asTypeOf(new ASourceIdSearch).MatrixRegBankId
                    current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
                }
                Response.ready := Mux(Is_Transpose, !transBusStall, MReg_Fill_Table_Not_Full && !current_fill_fifo_full)

                when(Response.fire){
                    val sourceId = Response.bits.ReseponseSourceID
                    val MatrixRegSearch = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch)
                    val MatrixRegBankId = MatrixRegSearch.MatrixRegBankId
                    val MatrixRegAddr = MatrixRegSearch.MatrixRegAddr
                    val ResponseData = Response.bits.ReseponseData
                    val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)

                    if (YJPAMLDebugEnable) {
                        printf("[AML_ResponseHandshake<%d>] Data:%x, BankId:%d, RegAddr:%d, SourceId:%d, FIFOIndex:%d, Tail:%d\n", io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr, sourceId, FIFOIndex, MatrixRegSearch.MatrixRegisTail)
                    }

                    when(Is_Transpose) {
                        val next_group_resp_cnt = group_resp_cnt + 1.U
                        val drain_trigger = next_group_resp_cnt === active_group_size

                        if (YJPAMLDebugEnable) {
                            printf("[AML_TRANS_RESP<%d>] source:%d data:%x tail:%d mask:%x base:%d beatIndex:%d respCnt:%d nextResp:%d activeGroupSize:%d drainTrig:%d writeBase:%d\n",
                              io.DebugInfo.DebugTimeStampe, sourceId, ResponseData, MatrixRegSearch.MatrixRegisTail.asUInt,
                              Mux(MatrixRegSearch.MatrixRegisTail, tailTaskMask, fullTaskMask), MatrixRegAddr,
                              MatrixRegSearch.BeatIndex, group_resp_cnt, next_group_resp_cnt, active_group_size,
                              drain_trigger.asUInt, transWriteBaseAddr)
                        }

                        transPipeInValid := true.B
                        transPipeInData := ResponseData
                        transPipeInMask := Mux(MatrixRegSearch.MatrixRegisTail, tailTaskMask, fullTaskMask)
                        transPipeRespBeatCnt := group_resp_cnt
                        transPipeEntryOffset := MatrixRegSearch.BeatIndex
                        transPipeDrainTrigger := drain_trigger

                        when(group_resp_cnt === 0.U) {
                            transWriteBaseAddr := MatrixRegAddr
                        }

                        when(next_group_resp_cnt === active_group_size) {
                            group_req_cnt := 0.U
                            group_resp_cnt := 0.U
                            group_size_reg := 0.U
                        }.otherwise {
                            group_resp_cnt := next_group_resp_cnt
                        }
                    }.otherwise {
                        MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                        MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                        MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                        MReg_Fill_Table_IsTail(MReg_Fill_Table_Insert_Index) := MatrixRegSearch.MatrixRegisTail
                        Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                        Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), AMemoryLoaderReadFromMemoryFIFODepth)
                    }
                    if (YJPAMLDebugEnable){
                        printf("[AML<%d>]Response, Data:%x, BankId:%d, RegAddr:%d\n", io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr)
                    }
                }

                when(Is_Transpose) {
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
                                printf("[AML_TransposeWrite<%d>] bank:%d, Addr:%d, Data:%x, Mask:%x\n",
                                  io.DebugInfo.DebugTimeStampe, i.U, io.ToMatrixRegIO.BankAddr(i).bits,
                                  transRouters(i).io.final_data, transRouters(i).io.final_mask)
                            }
                        }
                    }
                    when(transRouterWriteValid) {
                        if (YJPAMLDebugEnable) {
                            printf("[AML_TRANS_WRITE<%d>] validVec:%b base:%d phase:%d q:%d addr:%d bank0Data:%x bank0Mask:%x totalLoad:%d pipelineEmpty:%d\n",
                              io.DebugInfo.DebugTimeStampe, transRouterValidVec, transWriteBaseAddr, transWritePhase, transWriteElementSlot,
                              transWriteAddr, transRouters(0).io.final_data, transRouters(0).io.final_mask,
                              TotalLoadSize, transPipelineEmpty.asUInt)
                        }
                    }
                    // 转置路径以真实写侧 valid 计数；任务结束只看请求、响应和流水线是否全部静默。
                    val Current_Load_Fill_Size = transRouterWriteValid.asUInt
                    val nextTotalLoadSize = TotalLoadSize + Current_Load_Fill_Size
                    val transposeDone = TotalRequestSize === MaxRequestIter &&
                        group_req_cnt === 0.U && group_resp_cnt === 0.U &&
                        transPipelineEmpty
                    TotalLoadSize := nextTotalLoadSize
                    if(YJPAMLDebugEnable){
                        when(Current_Load_Fill_Size =/= 0.U) {
                            printf("[AML_TransposeLoad<%d>]TotalLoadSize:%d, FillSize:%d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize, Current_Load_Fill_Size)
                        }
                    }
                    when(transposeDone){
                        memoryload_state := s_load_quiesce
                        transposeEndDrainCnt := (transposeEndDrainCycles - 1).U
                        if (YJPAMLDebugEnable) printf("[AML<%d>]TransposeFullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                    }
                }.otherwise {
                    val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))
                    // Generic per-slot tail mask derived from tailTaskMask.
                    val tailByteMaskPerSlot = VecInit((0 until MAX_Fill_Times).map { j =>
                        tailTaskMask((j + 1) * ABMatrixRegEntryByteSize - 1, j * ABMatrixRegEntryByteSize)
                    })
                    for (i <- 0 until ABMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val currentIsTail = MReg_Fill_Table_IsTail(CurrentFIFOIndex)
                            Current_Fill_MReg_Time(i) := 1.U
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*ABMatrixRegEntryByteSize).W)))))
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            io.ToMatrixRegIO.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + fillSlot
                            io.ToMatrixRegIO.BankAddr(i).valid := true.B
                            io.ToMatrixRegIO.Data(i).bits := FIFOData(fillSlot)
                            io.ToMatrixRegIO.Data(i).valid := true.B
                            io.ToMatrixRegIO.ByteMask(i).bits := Mux(currentIsTail, tailByteMaskPerSlot(fillSlot), Fill(ABMatrixRegEntryByteSize, true.B))
                            io.ToMatrixRegIO.ByteMask(i).valid := true.B
                            if (YJPAMLDebugEnable) {
                                printf("[AML_MRegWriteHandshake<%d>] bank:%d, RegAddr:%x, WriteAddr:%x, Data:%x, ByteMask:%x, Time:%d\n", io.DebugInfo.DebugTimeStampe, i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), io.ToMatrixRegIO.BankAddr(i).bits, io.ToMatrixRegIO.Data(i).bits, io.ToMatrixRegIO.ByteMask(i).bits, MReg_Fill_Table_Time(CurrentFIFOIndex))
                            }
                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), AMemoryLoaderReadFromMemoryFIFODepth)
                            }
                            if (YJPAMLDebugEnable){
                                printf("[AML<%d>]Fill bank:%d, RegAddr:%x, Time:%d\n", io.DebugInfo.DebugTimeStampe, i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                            }
                        }
                    }
                    val Current_Load_Fill_Size = PopCount(Current_Fill_MReg_Time.asUInt)
                    TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
                    if(YJPAMLDebugEnable){
                        when(Current_Load_Fill_Size =/= 0.U) {
                            printf("[AML<%d>]TotalLoadSize:%d, FillSize:%d, Max:%d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize, Current_Load_Fill_Size, MaxRequestIter * MAX_Fill_Times.U)
                        }
                    }
                    when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                        memoryload_state := s_load_end
                        if (YJPAMLDebugEnable) printf("[AML<%d>]FullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                    }
                }
            }
        }
        is(s_load_quiesce) {
            io.ToMatrixRegIO.active := true.B
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
                        printf("[AML_TransposeQuiesceWrite<%d>] bank:%d, Addr:%d, Data:%x, Mask:%x\n",
                          io.DebugInfo.DebugTimeStampe, i.U, io.ToMatrixRegIO.BankAddr(i).bits,
                          transRouters(i).io.final_data, transRouters(i).io.final_mask)
                    }
                }
            }
            when(transRouterWriteValid) {
                if (YJPAMLDebugEnable) {
                    printf("[AML_TRANS_QUIESCE_WRITE<%d>] validVec:%b base:%d phase:%d q:%d addr:%d bank0Data:%x bank0Mask:%x drainCnt:%d pipelineEmpty:%d\n",
                      io.DebugInfo.DebugTimeStampe, transRouterValidVec, transWriteBaseAddr, transWritePhase, transWriteElementSlot,
                      transWriteAddr, transRouters(0).io.final_data, transRouters(0).io.final_mask,
                      transposeEndDrainCnt, transPipelineEmpty.asUInt)
                }
            }
            when(transposeEndDrainCnt === 0.U) {
                memoryload_state := s_load_end
                if (YJPAMLDebugEnable) printf("[AML<%d>]TransposeQuiesceEnd\n", io.DebugInfo.DebugTimeStampe)
            }.otherwise {
                transposeEndDrainCnt := transposeEndDrainCnt - 1.U
            }
        }
        is(s_load_end) {
            ConfigInfo.MicroTaskEndValid := true.B
            when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
                memoryload_state := s_load_idle
                state := s_idle
                if(YJPAMLDebugEnable) printf("[AML<%d>]AMemoryLoader Task End\n", io.DebugInfo.DebugTimeStampe)
            }
        }
    }
}
