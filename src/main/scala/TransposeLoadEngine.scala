package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config.Parameters

/** Coordinates are source-memory coordinates, in elements (never beat counts). */
class TransposeLoadCommand(implicit p: Parameters) extends CuteBundle {
  val base = UInt(MMUAddrWidth.W)
  val stride = UInt(MMUAddrWidth.W)
  val rows = UInt(MatrixRegMaxTensorDimBitSize.W)
  val columns = UInt(MatrixRegMaxTensorDimBitSize.W)
  val elementLgBytes = UInt(2.W) // e8/e16/e32 = 0/1/2
}

class TransposeReadRequest(implicit p: Parameters) extends CuteBundle {
  val address = UInt(MMUAddrWidth.W)
  val slot = Bool()
  val row = UInt(3.W)
}

class TransposeReadResponse(implicit p: Parameters) extends CuteBundle {
  val slot = Bool()
  val row = UInt(3.W)
  val data = UInt(512.W)
}

class TransposeWriteback(implicit p: Parameters) extends CuteBundle {
  val address = Vec(ABMatrixRegNBanks, UInt(log2Ceil(ABMatrixRegBankNEntries).W))
  val data = Vec(ABMatrixRegNBanks, UInt(ABMatrixRegEntryBitSize.W))
  val mask = Vec(ABMatrixRegNBanks, UInt(ABMatrixRegEntryByteSize.W))
  val last = Bool()
}

/** Two independent slots, each containing eight 512-bit response banks.
  *
  * A slot is reserved BEFORE requests are issued. Responses carry slot/row
  * tags; arrival order and physical memory channel are not coordinates.
  * Adapter/response-bridge lanes are normalized by row % memoryPorts, so
  * each data bank has one fixed input bus instead of an 8 x 8 crossbar.
  * A bank is written during fill OR read during drain, never both in a cycle.
  * Raw storage is unreset: expected/received masks guard every observation.
  * No memory primitive, byte barrel shifter, or one-hot-minus-one tail mask.
  */
class TransposeLoadEngine(memoryPorts: Int)(implicit p: Parameters) extends CuteModule {
  require(outsideDataWidthByte == 64, "SP-2 banks hold one 64-byte response")
  require(Set(1, 2, 4, 8).contains(memoryPorts))
  require(Set(4, 8).contains(ABMatrixRegNBanks))
  require(isPow2(ABMatrixRegEntryByteSize) && ABMatrixRegEntryByteSize >= 32)

  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new TransposeLoadCommand))
    val request = Vec(memoryPorts, Decoupled(new TransposeReadRequest))
    val response = Flipped(Vec(memoryPorts, Decoupled(new TransposeReadResponse)))
    val writeback = Decoupled(new TransposeWriteback)
    val done = Decoupled(Bool())
    val busy = Output(Bool())
  })

  private val banks = ABMatrixRegNBanks
  private val entryBytes = ABMatrixRegEntryByteSize
  private val dimBits = MatrixRegMaxTensorDimBitSize
  private val command = Reg(new TransposeLoadCommand)
  private val active = RegInit(false.B)
  private val done = RegInit(false.B)
  private val occupied = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val expected = Reg(Vec(2, Vec(8, Bool())))
  private val issued = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(8)(false.B)))))
  private val received = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(8)(false.B)))))
  private val storage = Seq.fill(2)(Seq.fill(8)(Reg(UInt(512.W))))
  private val slotBase = Reg(Vec(2, UInt(MMUAddrWidth.W)))
  private val slotRow = Reg(Vec(2, UInt(dimBits.W)))
  private val slotColumn = Reg(Vec(2, UInt(dimBits.W)))
  private val slotLast = Reg(Vec(2, Bool()))
  private val rowOffset = Reg(Vec(8, UInt(MMUAddrWidth.W)))
  private val allocateSlot = RegInit(false.B)
  private val issueSlot = RegInit(false.B)
  private val drainSlot = RegInit(false.B)
  private val nextRow = Reg(UInt(dimBits.W))
  private val nextColumn = Reg(UInt(dimBits.W))
  private val nextRowBase = Reg(UInt(MMUAddrWidth.W))
  private val allAllocated = RegInit(false.B)
  private val phase = RegInit(0.U(6.W))
  private val releasingSlot = WireDefault(false.B)

  // Stage 0 selects a segment and transposes by fixed wires; stage 1 places
  // that fragment in an entry. Both stages hold data under backpressure.
  private val stage0 = Reg(new TransposeWriteback)
  private val stage0Offset = Reg(UInt(log2Ceil(entryBytes).W))
  private val stage0Valid = RegInit(false.B)
  private val stage1 = Reg(new TransposeWriteback)
  private val stage1Valid = RegInit(false.B)
  private val stage1Ready = !stage1Valid || io.writeback.ready
  private val stage0Ready = !stage0Valid || stage1Ready
  io.writeback.valid := stage1Valid
  io.writeback.bits := stage1
  when(stage1Ready) {
    stage1Valid := stage0Valid
    when(stage0Valid) {
      stage1 := stage0
      for (b <- 0 until banks) {
        stage1.data(b) := MuxLookup(stage0Offset, 0.U)(
          (0 until entryBytes by 8).map(offset => offset.U ->
            (stage0.data(b) << (8 * offset))(ABMatrixRegEntryBitSize - 1, 0)))
        stage1.mask(b) := MuxLookup(stage0Offset, 0.U)(
          (0 until entryBytes by 8).map(offset => offset.U ->
            (stage0.mask(b) << offset)(entryBytes - 1, 0)))
      }
    }
  }
  when(stage0Ready) { stage0Valid := false.B }

  io.command.ready := !active && !done
  io.done.valid := done
  io.done.bits := true.B
  io.busy := active || done
  when(io.done.fire) { done := false.B }
  when(io.command.fire) {
    val cmd = io.command.bits
    assert(cmd.elementLgBytes <= 2.U, "transpose supports only e8/e16/e32")
    assert((cmd.rows << cmd.elementLgBytes) <= Tensor_K.U)
    assert(cmd.columns <= Tensor_MN.U)
    assert(cmd.base(5, 0) === 0.U, "transpose has the ordinary-load alignment contract")
    when(cmd.rows > 1.U) { assert(cmd.stride(5, 0) === 0.U) }
    command := cmd
    active := cmd.rows =/= 0.U && cmd.columns =/= 0.U
    done := cmd.rows === 0.U || cmd.columns === 0.U
    nextRow := 0.U
    nextColumn := 0.U
    nextRowBase := cmd.base
    allAllocated := false.B
    allocateSlot := false.B
    issueSlot := false.B
    drainSlot := false.B
    phase := 0.U
    for (r <- 0 until 8) { rowOffset(r) := cmd.stride * r.U }
  }

  private val columnsPerBeat = 64.U(7.W) >> command.elementLgBytes
  private val lastColumns = nextColumn +& columnsPerBeat >= command.columns
  private val allocating = active && !allAllocated &&
    (!occupied(allocateSlot) || (releasingSlot && allocateSlot === drainSlot))
  // Re-reserve at the last read edge: that read captures old data/metadata
  // in stage 0; the earliest new response writes on a later edge.
  when(allocating) {
    occupied(allocateSlot) := true.B
    slotBase(allocateSlot) := nextRowBase + (nextColumn << command.elementLgBytes)
    slotRow(allocateSlot) := nextRow
    slotColumn(allocateSlot) := nextColumn
    slotLast(allocateSlot) := lastColumns && nextRow +& 8.U >= command.rows
    for (r <- 0 until 8) {
      expected(allocateSlot)(r) := nextRow +& r.U < command.rows
      issued(allocateSlot)(r) := false.B
      received(allocateSlot)(r) := false.B
    }
    when(lastColumns) {
      nextColumn := 0.U
      nextRow := nextRow + 8.U
      nextRowBase := nextRowBase + (command.stride << 3)
      when(nextRow +& 8.U >= command.rows) { allAllocated := true.B }
    }.otherwise { nextColumn := nextColumn + columnsPerBeat }
    allocateSlot := !allocateSlot
  }

  // Each lane services its statically assigned rows. Requests stay stable
  // until accepted; reserving the whole slot prevents response overflow.
  private val requestsThisCycle = WireInit(VecInit(Seq.fill(8)(false.B)))
  for (lane <- 0 until memoryPorts) {
    val rows = lane until 8 by memoryPorts
    val candidates = VecInit(rows.map(r => expected(issueSlot)(r) && !issued(issueSlot)(r)))
    val chosen = PriorityEncoder(candidates.asUInt)
    val row = Mux1H(UIntToOH(chosen, rows.size), rows.map(_.U(3.W)))
    val req = io.request(lane)
    req.valid := active && occupied(issueSlot) && candidates.asUInt.orR
    req.bits.address := slotBase(issueSlot) + rowOffset(row)
    req.bits.slot := issueSlot
    req.bits.row := row
    for (r <- rows) {
      when(req.fire && row === r.U) {
        issued(issueSlot)(r) := true.B
        requestsThisCycle(r) := true.B
      }
    }
  }
  private val issuedAll = (0 until 8).map(r =>
    !expected(issueSlot)(r) || issued(issueSlot)(r) || requestsThisCycle(r)).reduce(_ && _)
  when(active && occupied(issueSlot) && requestsThisCycle.asUInt.orR && issuedAll) {
    issueSlot := !issueSlot
  }

  private val drainComplete = (0 until 8).map(r =>
    !expected(drainSlot)(r) || received(drainSlot)(r)).reduce(_ && _)
  private val readSlot = active && occupied(drainSlot) && drainComplete && stage0Ready
  for (lane <- 0 until memoryPorts) {
    val resp = io.response(lane)
    val s = resp.bits.slot
    val r = resp.bits.row
    resp.ready := active && occupied(s) && expected(s)(r) && !received(s)(r) &&
      (issued(s)(r) || (s === issueSlot && requestsThisCycle(r)))
    when(resp.fire) {
      received(s)(r) := true.B
      assert(!(readSlot && s === drainSlot), "single-port bank read/write collision")
      assert((r % memoryPorts.U) === lane.U, "response lane must be normalized by source row")
    }
    for (other <- 0 until lane) {
      when(resp.valid && io.response(other).valid) {
        assert(!(s === io.response(other).bits.slot && r === io.response(other).bits.row),
          "duplicate transpose response")
      }
    }
    for (slot <- 0 until 2; row <- lane until 8 by memoryPorts) {
      when(resp.fire && s === slot.U && r === row.U) { storage(slot)(row) := resp.bits.data }
    }
  }

  private val selectedRows = (0 until 8).map(r => Mux(drainSlot, storage(1)(r), storage(0)(r)))
  private val column = slotColumn(drainSlot) + (phase << log2Ceil(banks))
  private val lastPhase = (phase +& 1.U) * banks.U >= columnsPerBeat ||
    column +& banks.U >= command.columns
  releasingSlot := readSlot && lastPhase
  when(readSlot) {
    stage0Valid := true.B
    stage0.last := slotLast(drainSlot) && lastPhase
    stage0Offset := (slotRow(drainSlot) << command.elementLgBytes)(log2Ceil(entryBytes) - 1, 0)
    for (b <- 0 until banks) {
      stage0.address(b) := ((column + b.U) >> log2Ceil(banks)) * ReduceGroupSize.U +
        ((slotRow(drainSlot) << command.elementLgBytes) >> log2Ceil(entryBytes))
      val fragments = (0 to 2).map { lg =>
        val e = 1 << lg
        val rowSegments = selectedRows.map(_.asTypeOf(Vec(64 / (banks * e), UInt((banks * e * 8).W))))
        val rowBytes = rowSegments.map(v => v(phase(log2Ceil(v.length) - 1, 0)))
        val data = Cat(rowBytes.reverse.map(x => x((b + 1) * e * 8 - 1, b * e * 8)))
        val mask = VecInit((0 until 8 * e).map(i =>
          expected(drainSlot)(i / e) && column +& b.U < command.columns)).asUInt
        (lg.U, data.pad(ABMatrixRegEntryBitSize), mask.pad(entryBytes))
      }
      stage0.data(b) := MuxLookup(command.elementLgBytes, 0.U)(fragments.map(x => x._1 -> x._2))
      stage0.mask(b) := MuxLookup(command.elementLgBytes, 0.U)(fragments.map(x => x._1 -> x._3))
    }
    when(lastPhase) {
      when(!(allocating && allocateSlot === drainSlot)) { occupied(drainSlot) := false.B }
      drainSlot := !drainSlot
      phase := 0.U
    }.otherwise { phase := phase + 1.U }
  }
  when(io.writeback.fire && io.writeback.bits.last) {
    active := false.B
    done := true.B
    assert(!occupied.asUInt.orR, "transpose completed with a live slot")
  }
}

/** AML-side adapter for selecting normal loads or the transpose engine. */
trait HasTransposeLoadEngine { this: CuteModule =>
  protected def connectTransposeLoadEngine(
      config: AMLMicroTaskConfigIO, memory: LocalMMUIO,
      target: ABMemoryLoaderMatrixRegIO, targetId: UInt,
      normalConfig: AMLMicroTaskConfigIO, normalMemory: LocalMMUIO,
      normalTarget: ABMemoryLoaderMatrixRegIO, normalId: UInt,
      legacy: Boolean, diffIndex: Int
  )(implicit p: Parameters): Unit = {
    val ports = if (legacy) 1 else ABMatrixRegNBanks
    val engine = Module(new TransposeLoadEngine(ports))
    val busy = RegInit(false.B)
    val transpose = RegInit(false.B)
    val regId = Reg(UInt(ABMatrixRegIdWidth.W))
    val coherent = Reg(Bool())
    val accepted = config.MicroTaskValid && config.MicroTaskReady

    normalConfig <> config
    normalConfig.MicroTaskValid := config.MicroTaskValid && !busy && !config.Is_Transpose
    normalConfig.MicroTaskEndReady := config.MicroTaskEndReady && busy && !transpose
    normalMemory.ConherentRequsetSourceID := memory.ConherentRequsetSourceID
    normalMemory.nonConherentRequsetSourceID := memory.nonConherentRequsetSourceID

    val app = config.ApplicationTensor_A
    val lgBytes = MuxLookup(app.dataType, 0.U(2.W))(Seq(
      ElementDataType.DataTypeWidth8 -> 0.U,
      ElementDataType.DataTypeWidth16 -> 1.U,
      ElementDataType.DataTypeWidth32 -> 2.U))
    val fullBytes = app.K_Beat_Count << 6
    val rowBytes = Mux(app.HasTail, fullBytes - 64.U + app.TailByteMask, fullBytes)
    engine.io.command.bits.base := app.ApplicationTensor_A_BaseVaddr
    engine.io.command.bits.stride := app.ApplicationTensor_A_Stride_M
    engine.io.command.bits.rows := config.MatrixRegTensor_M
    engine.io.command.bits.columns := rowBytes >> lgBytes
    engine.io.command.bits.elementLgBytes := lgBytes
    engine.io.command.valid := config.MicroTaskValid && !busy && config.Is_Transpose
    config.MicroTaskReady := !busy && engine.io.command.ready && normalConfig.MicroTaskReady
    config.MicroTaskEndValid := busy && Mux(transpose, engine.io.done.valid, normalConfig.MicroTaskEndValid)
    engine.io.done.ready := busy && transpose && config.MicroTaskEndReady
    engine.io.writeback.ready := true.B

    when(accepted) {
      busy := true.B
      transpose := config.Is_Transpose
      regId := config.MatrixRegId
      coherent := config.Conherent
      when(config.Is_Transpose) {
        assert(app.dataType === ElementDataType.DataTypeWidth8 ||
          app.dataType === ElementDataType.DataTypeWidth16 ||
          app.dataType === ElementDataType.DataTypeWidth32, "e4 transpose is not implemented")
      }
    }
    when(config.MicroTaskEndValid && config.MicroTaskEndReady) { busy := false.B }

    val bankBits = log2Ceil(ABMatrixRegNBanks)
    val bankOffset = log2Ceil(ABMatrixRegBankNEntries)
    require(bankOffset >= 2, "transpose tag needs slot and upper source-row bits")
    for (lane <- 0 until ABMatrixRegNBanks) {
      val useTranspose = busy && transpose
      memory.Request(lane) <> normalMemory.Request(lane)
      normalMemory.Response(lane) <> memory.Response(lane)
      normalMemory.Request(lane).ready := memory.Request(lane).ready && !useTranspose
      normalMemory.Response(lane).valid := memory.Response(lane).valid && !useTranspose
      if (lane < ports) {
        val req = engine.io.request(lane)
        val resp = engine.io.response(lane)
        val transReq = WireDefault(0.U.asTypeOf(new MMURequestIO))
        transReq.RequestAddr := req.bits.address
        transReq.RequestConherent := coherent
        transReq.RequestSourceID := (req.bits.row(bankBits - 1, 0) << bankOffset) |
          ((req.bits.row >> bankBits) << 1) | req.bits.slot
        transReq.RequestMask := Fill(MMUMaskWidth, true.B)
        req.ready := memory.Request(lane).ready && useTranspose
        resp.valid := memory.Response(lane).valid && useTranspose
        resp.bits.data := memory.Response(lane).bits.ReseponseData
        val id = memory.Response(lane).bits.ReseponseSourceID
        resp.bits.slot := id(0)
        resp.bits.row := (if (bankBits == 3) id(bankOffset + 2, bankOffset)
          else Cat(id(1), id(bankOffset + bankBits - 1, bankOffset)))
        when(useTranspose) {
          memory.Request(lane).valid := req.valid
          memory.Request(lane).bits := transReq
          memory.Response(lane).ready := resp.ready
        }
      } else {
        when(useTranspose) {
          memory.Request(lane).valid := false.B
          memory.Response(lane).ready := false.B
        }
      }
    }

    target <> normalTarget
    targetId := normalId
    when(busy && transpose) {
      targetId := regId
      target.active := true.B
      for (bank <- 0 until ABMatrixRegNBanks) {
        val valid = engine.io.writeback.valid && engine.io.writeback.bits.mask(bank).orR
        target.BankAddr(bank).valid := valid
        target.BankAddr(bank).bits := engine.io.writeback.bits.address(bank)
        target.Data(bank).valid := valid
        target.Data(bank).bits := engine.io.writeback.bits.data(bank)
        target.ByteMask(bank).valid := valid
        target.ByteMask(bank).bits := engine.io.writeback.bits.mask(bank)
      }
    }

    if (EnableDifftest) {
      DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_WORDS_PER_BANK", ABMatrixRegEntryBitSize / 64)
      DifftestModule.addCppMacro("CONFIG_DIFF_AMU_AB_REG_SIZE_BYTES", ABMatrixRegSize)
      val pc = RegEnable(config.pc.get, accepted)
      val coreid = RegEnable(config.coreid.get, accepted)
      val event = DifftestModule(new DiffAmuFinishEvent(ABMatrixRegNBanks, DiffAmuFinishWordsPerBank),
        delay = 0, dontCare = true)
      event.index := diffIndex.U
      event.pc := pc
      event.coreid := coreid
      event.finish := config.MicroTaskEndValid && config.MicroTaskEndReady
      event.valid := target.BankAddr.map(_.valid).reduce(_ || _) || event.finish
      val words = event.data.length / ABMatrixRegNBanks
      for (bank <- 0 until ABMatrixRegNBanks) {
        event.bankValid(bank) := target.BankAddr(bank).valid
        event.bankAddr(bank) := target.BankAddr(bank).bits
        event.bankMask(bank) := target.ByteMask(bank).bits
        for (word <- 0 until words) {
          event.data(bank * words + word) := (if (word < ABMatrixRegEntryBitSize / 64)
            target.Data(bank).bits(word * 64 + 63, word * 64) else 0.U)
        }
      }
    }
  }
}
