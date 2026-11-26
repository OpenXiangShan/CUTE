package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util._
import cute.Bundles._
import difftest._
import utility.ChiselDB

/*
 * TaskController scheduling overview:
 * - Decode AMU instructions into a fixed-size issue window.
 * - Build dependencies at enqueue time from static read/write footprints.
 * - Scan the window oldest-first and issue at most one ready slot per cycle.
 * - Route completion back through FU ownerSlot; retire only from the window head.
 * - Release and NopLike stay local; ZeroAcc and ZeroTr map to load-like aliases.
 */
class TaskControllerIO(implicit p: Parameters) extends CuteBundle {
  val ygjkctrl = Flipped(new YGJKControl)
  val ADC_MicroTask_Config = new ADCMicroTaskConfigIO
  val BDC_MicroTask_Config = new BDCMicroTaskConfigIO
  val ASC_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new ASCMicroTaskConfigIO)
  val BSC_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new BSCMicroTaskConfigIO)
  val CDC_MicroTask_Config = new CDCMicroTaskConfigIO
  val AML_MicroTask_Config = new AMLMicroTaskConfigIO
  val BML_MicroTask_Config = new BMLMicroTaskConfigIO
  val ASL_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new ASLMicroTaskConfigIO)
  val BSL_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new BSLMicroTaskConfigIO)
  val CML_MicroTask_Config = new CMLMicroTaskConfigIO
  val MTE_MicroTask_Config = new MTEMicroTaskConfigIO
  val DebugTimeStampe = Input(UInt(32.W))
  val downstreamIdle = Input(Bool())
  val perfProbe = Output(new TaskControllerPerfProbe)
  // Observation only: source the live FU owner, not the current issue mux.
  val ownedScaleTrace = Option.when(EnableDifftest && cuteMatrixExtension.enableScalingFactor)(
    Output(Vec(2, new TaskCtrlOwnedScaleTrace)))
  val mmaTrace = Option.when(EnableDifftest && cuteMatrixExtension.enableScalingFactor)(
    Output(Vec(2, new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits))))
  val retirementTrace = Option.when(EnableDifftest && cuteMatrixExtension.enableScalingFactor)(
    Output(Vec(2, new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits))))
}

abstract class BaseTaskController(implicit p: Parameters) extends CuteModule {
  val io = IO(new TaskControllerIO)
}

object NewTaskController {
  val RegIdWidth = 4
  val MaxReadRegs = 3
  val MaxWriteRegs = 1
}

class DecodedAmuCtrlEntry(implicit p: Parameters) extends CuteBundle {
  import NewTaskController._

  val ctrl = new AmuCtrlIO
  val readRegs = Vec(MaxReadRegs, UInt(RegIdWidth.W))
  val readValid = Vec(MaxReadRegs, Bool())
  val writeRegs = Vec(MaxWriteRegs, UInt(RegIdWidth.W))
  val writeValid = Vec(MaxWriteRegs, Bool())
}

object TaskCtrlOpKind extends ChiselEnum {
  val NopLike, LoadA, LoadB, LoadC, Compute, Store, Release, ZeroAcc, ZeroTr, LoadScaleA, LoadScaleB = Value
}

class TaskController(implicit p: Parameters) extends BaseTaskController {
  import NewTaskController._

  dontTouch(io)

  // Scheduler flow: decode -> window/dependency build -> oldest-ready issue -> done/retire/enqueue.
  // Release/NopLike stay scheduler-local; ZeroAcc/ZeroTr remain mzero-like load aliases.

  private val WinDepth = TaskCtrlIssueWindowDepth
  private val SlotIdxWidth = log2Ceil(WinDepth)
  private val WinCountWidth = log2Ceil(WinDepth + 1)
  private val SeqIdWidth = 16
  private val ScaleGenerationWidth = 32

  // ===================== Default output assignments =====================
  // This is the single canonical postcommit return stream. Once enqueued, the
  // complete event belongs to the Queue, never to a retired/reused window slot.
  // No flow-through: an older flags merge precedes a replacement Release by
  // at least one cycle. Pipe permits a full entry to pop/push on the same edge.
  // Enrich the SAME queue token for observation only. No tail crosses the
  // production return interface, and no trace storage exists without Difftest.
  private class OwnedReturn extends MreleaseIO {
    val tracePc = Option.when(EnableDifftest)(UInt(64.W))
    val traceCoreid = Option.when(EnableDifftest)(UInt(8.W))
    val traceRetireId = Option.when(EnableDifftest)(UInt(64.W))
    val traceEpoch = Option.when(EnableDifftest)(UInt(64.W))
    val traceComputeType = Option.when(EnableDifftest)(UInt(4.W))
  }
  private val returnQ = Module(new Queue(new OwnedReturn, 1, pipe = true, flow = false))
  io.ygjkctrl.mrelease.valid := returnQ.io.deq.valid
  io.ygjkctrl.mrelease.bits.msyncRd := returnQ.io.deq.bits.msyncRd
  io.ygjkctrl.mrelease.bits.flagsValid := returnQ.io.deq.bits.flagsValid
  io.ygjkctrl.mrelease.bits.flags := returnQ.io.deq.bits.flags
  returnQ.io.deq.ready := io.ygjkctrl.mrelease.ready

  io.ADC_MicroTask_Config.dataType := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.ADC_MicroTask_Config.MatrixRegId := 0.U
  io.ADC_MicroTask_Config.Is_Transpose := false.B
  io.ADC_MicroTask_Config.MicroTaskValid := false.B
  io.ADC_MicroTask_Config.MicroTaskEndReady := false.B

  io.ASC_MicroTask_Config.foreach { cfg =>
    cfg.MatrixRegTensor_M := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.MatrixRegTensor_N := 0.U
    cfg.MatrixRegId := 0.U
    cfg.ScaleBankId := 0.U
    cfg.ScaleBlocksPerRow := 0.U
    cfg.KElements := 0.U
    cfg.computeType := MteComputeType.ComputeTypeUndef
    cfg.Is_Transpose := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.BDC_MicroTask_Config.dataType := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BDC_MicroTask_Config.MatrixRegId := 0.U
  io.BDC_MicroTask_Config.Is_Transpose := false.B
  io.BDC_MicroTask_Config.MicroTaskValid := false.B
  io.BDC_MicroTask_Config.MicroTaskEndReady := false.B

  io.BSC_MicroTask_Config.foreach { cfg =>
    cfg.MatrixRegTensor_M := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.MatrixRegTensor_N := 0.U
    cfg.MatrixRegId := 0.U
    cfg.ScaleBankId := 0.U
    cfg.ScaleBlocksPerRow := 0.U
    cfg.KElements := 0.U
    cfg.computeType := MteComputeType.ComputeTypeUndef
    cfg.Is_Transpose := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := 0.U
  io.CDC_MicroTask_Config.ApplicationTensor_D.dataType := 0.U
  io.CDC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.CDC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.CDC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.CDC_MicroTask_Config.MatrixRegActive_M := 0.U
  io.CDC_MicroTask_Config.MatrixRegActive_N := 0.U
  io.CDC_MicroTask_Config.MatrixRegId := 0.U
  io.CDC_MicroTask_Config.Is_Transpose := false.B
  io.CDC_MicroTask_Config.Is_AfterOps_Tile := false.B
  io.CDC_MicroTask_Config.Is_Reorder_Only_Ops := false.B
  io.CDC_MicroTask_Config.Is_EasyScale_Only_Ops := false.B
  io.CDC_MicroTask_Config.Is_VecFIFO_Ops := false.B
  io.CDC_MicroTask_Config.MicroTaskValid := false.B
  io.CDC_MicroTask_Config.MicroTaskEndReady := false.B
  io.CDC_MicroTask_Config.MicroTask_TEComputeEndReady := false.B
  if (EnableDifftest) {
    io.CDC_MicroTask_Config.pc.get := 0.U
    io.CDC_MicroTask_Config.coreid.get := 0.U
  }

  io.AML_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.AML_MicroTask_Config.ApplicationTensor_A)
  io.AML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.AML_MicroTask_Config.LoadTaskInfo)
  io.AML_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.AML_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.AML_MicroTask_Config.Conherent := false.B
  io.AML_MicroTask_Config.Is_Transpose := false.B
  io.AML_MicroTask_Config.ConvertF32ToMx := false.B
  io.AML_MicroTask_Config.ConvertComputeType := 0.U
  io.AML_MicroTask_Config.ConvertKElements := 0.U
  io.AML_MicroTask_Config.ConvertScaleBankId := 0.U
  io.AML_MicroTask_Config.ConvertDynamic.foreach(_ := false.B)
  io.AML_MicroTask_Config.MatrixRegId := 0.U
  io.AML_MicroTask_Config.MicroTaskValid := false.B
  io.AML_MicroTask_Config.MicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.AML_MicroTask_Config.pc.get := 0.U
    io.AML_MicroTask_Config.coreid.get := 0.U
  }

  io.ASL_MicroTask_Config.foreach { cfg =>
    cfg.ApplicationScale_A := 0.U.asTypeOf(cfg.ApplicationScale_A)
    cfg.MatrixRegTensor_M := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.ScaleBankId := 0.U
    cfg.Generated.foreach(_ := false.B)
    cfg.Conherent := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.BML_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BML_MicroTask_Config.ApplicationTensor_B)
  io.BML_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BML_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BML_MicroTask_Config.Conherent := false.B
  io.BML_MicroTask_Config.Is_Transpose := false.B
  io.BML_MicroTask_Config.ConvertF32ToI8 := false.B
  io.BML_MicroTask_Config.ConvertF32ToMx := false.B
  io.BML_MicroTask_Config.ConvertComputeType := 0.U
  io.BML_MicroTask_Config.ConvertKElements := 0.U
  io.BML_MicroTask_Config.ConvertScaleBankId := 0.U
  io.BML_MicroTask_Config.ConvertDynamic.foreach(_ := false.B)
  io.BML_MicroTask_Config.MatrixRegId := 0.U
  io.BML_MicroTask_Config.MicroTaskValid := false.B
  io.BML_MicroTask_Config.MicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.BML_MicroTask_Config.pc.get := 0.U
    io.BML_MicroTask_Config.coreid.get := 0.U
  }

  io.BSL_MicroTask_Config.foreach { cfg =>
    cfg.ApplicationScale_B := 0.U.asTypeOf(cfg.ApplicationScale_B)
    cfg.MatrixRegTensor_N := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.ScaleBankId := 0.U
    cfg.Generated.foreach(_ := false.B)
    cfg.Conherent := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.CML_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_C)
  io.CML_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_D)
  io.CML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.LoadTaskInfo)
  io.CML_MicroTask_Config.Conherent := false.B
  io.CML_MicroTask_Config.Is_Transpose := false.B
  io.CML_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.CML_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.CML_MicroTask_Config.MatrixRegId := 0.U
  io.CML_MicroTask_Config.LoadMicroTaskValid := false.B
  io.CML_MicroTask_Config.LoadMicroTaskEndReady := false.B
  io.CML_MicroTask_Config.StoreMicroTaskValid := false.B
  io.CML_MicroTask_Config.StoreMicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.CML_MicroTask_Config.pc.get := 0.U
    io.CML_MicroTask_Config.coreid.get := 0.U
  }

  io.MTE_MicroTask_Config.MicroTaskValid := false.B
  io.MTE_MicroTask_Config.computeType := MteComputeType.ComputeTypeUndef
  io.perfProbe := 0.U.asTypeOf(new TaskControllerPerfProbe)

  // ===================== ChiselDB event definitions =====================
  private val TileDimWidth = Bundles.Mtilex.width
  private val LoadFifoIdxWidth = 4
  private val ComputeFifoIdxWidth = 4
  private val StoreFifoIdxWidth = 4

  class LoadEventEntry extends Bundle {
    val eventType = UInt(4.W)
    val regId = UInt(4.W)
    val fifoIdx = UInt(LoadFifoIdxWidth.W)
    val needMask = UInt(3.W) // {C,B,A}
    val row = UInt(TileDimWidth.W)
    val column = UInt(TileDimWidth.W)
    val transpose = Bool()
    val isAcc = Bool()
    val slotId = UInt(SlotIdxWidth.W)
    val seqId = UInt(SeqIdWidth.W)
  }

  class ComputeEventEntry extends Bundle {
    val eventType = UInt(4.W)
    val aReg = UInt(4.W)
    val bReg = UInt(4.W)
    val cReg = UInt(4.W)
    val fifoIdx = UInt(ComputeFifoIdxWidth.W)
    val mtilem = UInt(TileDimWidth.W)
    val mtilen = UInt(TileDimWidth.W)
    val mtilek = UInt(TileDimWidth.W)
    val isMma = Bool()
    val isFp = Bool()
    val slotId = UInt(SlotIdxWidth.W)
    val seqId = UInt(SeqIdWidth.W)
  }

  class StoreEventEntry extends Bundle {
    val eventType = UInt(3.W)
    val regId = UInt(4.W)
    val fifoIdx = UInt(StoreFifoIdxWidth.W)
    val row = UInt(TileDimWidth.W)
    val column = UInt(TileDimWidth.W)
    val transpose = Bool()
    val isAcc = Bool()
    val slotId = UInt(SlotIdxWidth.W)
    val seqId = UInt(SeqIdWidth.W)
  }

  class ReleaseEventEntry extends Bundle {
    val eventType = UInt(2.W)
    val msync = UInt(CuteMsyncRegIdxWidth.W)
    val slotId = UInt(SlotIdxWidth.W)
    val seqId = UInt(SeqIdWidth.W)
  }

  private val loadEventTable = ChiselDB.createTable("CUTELoadEvent", new LoadEventEntry, basicDB = true)
  private val computeEventTable = ChiselDB.createTable("CUTEComputeEvent", new ComputeEventEntry, basicDB = true)
  private val storeEventTable = ChiselDB.createTable("CUTEStoreEvent", new StoreEventEntry, basicDB = true)
  private val releaseEventTable = ChiselDB.createTable("CUTEReleaseEvent", new ReleaseEventEntry, basicDB = true)

  private val loadAllocateEvent = WireInit(0.U.asTypeOf(new LoadEventEntry))
  private val loadAllocateEventEn = WireInit(false.B)
  private val loadIssueEvent = WireInit(0.U.asTypeOf(new LoadEventEntry))
  private val loadIssueEventEn = WireInit(false.B)
  private val loadAFinishEvent = WireInit(0.U.asTypeOf(new LoadEventEntry))
  private val loadAFinishEventEn = WireInit(false.B)
  private val loadBFinishEvent = WireInit(0.U.asTypeOf(new LoadEventEntry))
  private val loadBFinishEventEn = WireInit(false.B)
  private val loadCFinishEvent = WireInit(0.U.asTypeOf(new LoadEventEntry))
  private val loadCFinishEventEn = WireInit(false.B)

  private val computeIssueEvent = WireInit(0.U.asTypeOf(new ComputeEventEntry))
  private val computeIssueEventEn = WireInit(false.B)
  private val computeReadAFinishEvent = WireInit(0.U.asTypeOf(new ComputeEventEntry))
  private val computeReadAFinishEventEn = WireInit(false.B)
  private val computeReadBFinishEvent = WireInit(0.U.asTypeOf(new ComputeEventEntry))
  private val computeReadBFinishEventEn = WireInit(false.B)
  private val computeWriteCFinishEvent = WireInit(0.U.asTypeOf(new ComputeEventEntry))
  private val computeWriteCFinishEventEn = WireInit(false.B)

  private val storeIssueEvent = WireInit(0.U.asTypeOf(new StoreEventEntry))
  private val storeIssueEventEn = WireInit(false.B)
  private val storeFinishEvent = WireInit(0.U.asTypeOf(new StoreEventEntry))
  private val storeFinishEventEn = WireInit(false.B)

  private val releaseIssueEvent = WireInit(0.U.asTypeOf(new ReleaseEventEntry))
  private val releaseIssueEventEn = WireInit(false.B)

  // ===================== Decoded instruction FIFO =====================
  private val decodedFifo = Module(new Queue(new DecodedAmuCtrlEntry, DecodedAmuCtrlFIFODepth))

  decodedFifo.io.enq.valid := io.ygjkctrl.amuCtrl.valid
  io.ygjkctrl.amuCtrl.ready := decodedFifo.io.enq.ready

  val amuCtrlBits = io.ygjkctrl.amuCtrl.bits
  val decEntryEnq = Wire(new DecodedAmuCtrlEntry)
  decEntryEnq.ctrl := amuCtrlBits

  for (i <- 0 until MaxReadRegs) {
    decEntryEnq.readRegs(i) := 0.U
    decEntryEnq.readValid(i) := false.B
  }
  for (i <- 0 until MaxWriteRegs) {
    decEntryEnq.writeRegs(i) := 0.U
    decEntryEnq.writeValid(i) := false.B
  }


  val enqMma = decodeMma(amuCtrlBits)
  val enqLsu = decodeLsu(amuCtrlBits)
  val enqArith = decodeArith(amuCtrlBits)

  when(amuCtrlBits.isMma()) {
    val mma = enqMma
    decEntryEnq.readRegs(0) := mma.ms1
    decEntryEnq.readRegs(1) := mma.ms2
    decEntryEnq.readRegs(2) := mma.md
    decEntryEnq.readValid(0) := true.B
    decEntryEnq.readValid(1) := true.B
    decEntryEnq.readValid(2) := true.B
    decEntryEnq.writeRegs(0) := mma.md
    decEntryEnq.writeValid(0) := true.B
  }
  when(amuCtrlBits.isMls() && !AmuScaleLoad.selected(amuCtrlBits)) {
    val lsu = enqLsu
    when(lsu.ls === 0.U) {
      decEntryEnq.writeRegs(0) := lsu.ms
      decEntryEnq.writeValid(0) := true.B
    }.otherwise {
      decEntryEnq.readRegs(0) := lsu.ms
      decEntryEnq.readValid(0) := true.B
    }
  }
  when(amuCtrlBits.isArith()) {
    val arith = enqArith
    // Current CUTE flow only supports mzero-style marith.
    // Unknown marith opType is treated as NopLike (no read/write footprint).
    when(isMzeroLike(arith)) {
      decEntryEnq.writeRegs(0) := arith.md
      decEntryEnq.writeValid(0) := true.B
    }
  }

  decodedFifo.io.enq.bits := decEntryEnq

  // ===================== Issue window state =====================
  class IssueWindowSlot(implicit p: Parameters) extends CuteBundle {
    val valid = Bool()
    val issued = Bool()
    val completed = Bool()
    val readADone = Bool()
    val readBDone = Bool()
    val scaleReadADone = Bool()
    val scaleReadBDone = Bool()
    val writeCDone = Bool()
    val completionFlags = UInt(5.W)
    val scaleAGeneration = UInt(ScaleGenerationWidth.W)
    val scaleBGeneration = UInt(ScaleGenerationWidth.W)
    val scaleABank = UInt(ABScaleRegIdWidth.W)
    val scaleBBank = UInt(ABScaleRegIdWidth.W)
    val waitCompleteMask = UInt(WinDepth.W)
    val waitReadAMask = UInt(WinDepth.W)
    val waitReadBMask = UInt(WinDepth.W)
    val waitScaleReadAMask = UInt(WinDepth.W)
    val waitScaleReadBMask = UInt(WinDepth.W)
    val opKind = TaskCtrlOpKind()
    val entry = new DecodedAmuCtrlEntry
    val seqId = UInt(SeqIdWidth.W)
    val fifoIdx = UInt(4.W)
  }

  class FuTracker extends Bundle {
    val busy = Bool()
    val ownerSlot = UInt(SlotIdxWidth.W)
  }

  val slots = RegInit(VecInit(Seq.fill(WinDepth)(0.U.asTypeOf(new IssueWindowSlot))))
  val winHead = RegInit(0.U(SlotIdxWidth.W))
  val winTail = RegInit(0.U(SlotIdxWidth.W))
  val winCount = RegInit(0.U(WinCountWidth.W))
  val seqIdAlloc = RegInit(0.U(SeqIdWidth.W))

  val loadFifoIdxAlloc = RegInit(0.U(LoadFifoIdxWidth.W))
  val computeIssueIdx = RegInit(0.U(ComputeFifoIdxWidth.W))
  val storeIssueIdx = RegInit(0.U(StoreFifoIdxWidth.W))

  val fuAML = RegInit(0.U.asTypeOf(new FuTracker))
  val fuBML = RegInit(0.U.asTypeOf(new FuTracker))
  val fuCMLLoad = RegInit(0.U.asTypeOf(new FuTracker))
  val fuCompute = RegInit(0.U.asTypeOf(new FuTracker))
  val fuCMLStore = RegInit(0.U.asTypeOf(new FuTracker))
  val fuASL = RegInit(0.U.asTypeOf(new FuTracker))
  val fuBSL = RegInit(0.U.asTypeOf(new FuTracker))

  // Four resident registers per side, one active reader per side (compute or
  // converting load). Converting loads conservatively hold through loader End.
  // Reader end releases inUse, not architectural scale validity. A same-reg
  // reload may replace a resident generation only after scale WAR deps drain.
  // Per-register generations are captured by consumers at enqueue.
  // Independent loader/controller toggle order is never an ownership signal.
  class ScaleBankState extends Bundle {
    val occupied = Bool()
    val valid = Bool()
    val inUse = Bool()
    val regId = UInt(ABMatrixRegIdWidth.W)
    val generation = UInt(ScaleGenerationWidth.W)
    val computeType = UInt(MteComputeType.ComputeTypeBitWidth.W)
    val rows = UInt(Bundles.Mtilex.width.W)
    val kVectors = UInt(MatrixRegMaxTensorDimBitSize.W)
    val scaleBlocks = UInt(MatrixRegMaxTensorDimBitSize.W)
  }
  val scaleA = RegInit(VecInit(Seq.fill(ABScaleRegCount)(0.U.asTypeOf(new ScaleBankState))))
  val scaleB = RegInit(VecInit(Seq.fill(ABScaleRegCount)(0.U.asTypeOf(new ScaleBankState))))
  val latestScaleA = RegInit(VecInit(Seq.fill(ABMatrixRegCount)(0.U(ScaleGenerationWidth.W))))
  val latestScaleB = RegInit(VecInit(Seq.fill(ABMatrixRegCount)(0.U(ScaleGenerationWidth.W))))
  val freeScaleA = VecInit(scaleA.map(b => !b.occupied)).asUInt
  val freeScaleB = VecInit(scaleB.map(b => !b.occupied)).asUInt

  private def slotOH(idx: UInt): UInt = UIntToOH(idx, WinDepth)
  private def abIdx(x: UInt): UInt = x(ABMatrixRegIdWidth - 1, 0)
  private def cIdx(x: UInt): UInt = x(CMatrixRegIdWidth - 1, 0)

  private def decodeMma(ctrl: AmuCtrlIO): AmuMmaIO = ctrl.data.asTypeOf(new AmuMmaIO)
  private def decodeLsu(ctrl: AmuCtrlIO): AmuLsuIO = ctrl.data.asTypeOf(new AmuLsuIO)
  private def decodeArith(ctrl: AmuCtrlIO): AmuArithIO = ctrl.data.asTypeOf(new AmuArithIO)
  private def isMzeroLike(arith: AmuArithIO): Bool = arith.opType(8, 2) === "b1101110".U
  private def isScaledCtrl(ctrl: AmuCtrlIO): Bool = {
    val mode = AmuScaleLoad.mmaComputeType(ctrl)
    ctrl.isMma() && (mode === MteComputeType.Mxfp4F32 ||
      mode === MteComputeType.Mxfp8e4m3F32 || mode === MteComputeType.Mxfp8e5m2F32)
  }
  private def isConvertCtrl(ctrl: AmuCtrlIO): Bool =
    ExperimentalMxfpConvertLoad.B && AmuConvertLoad.selected(ctrl)
  private def isDynamicCtrl(ctrl: AmuCtrlIO): Bool =
    ExperimentalMxfpDynamicQuant.B && AmuConvertLoad.dynamic(ctrl)
  private def writesScale(op: TaskCtrlOpKind.Type, ctrl: AmuCtrlIO, a: Boolean): Bool =
    (op === (if (a) TaskCtrlOpKind.LoadScaleA else TaskCtrlOpKind.LoadScaleB)) ||
      (isDynamicCtrl(ctrl) && op === (if (a) TaskCtrlOpKind.LoadA else TaskCtrlOpKind.LoadB))
  private def isFlagProducer(op: TaskCtrlOpKind.Type, ctrl: AmuCtrlIO): Bool =
    op === TaskCtrlOpKind.Compute ||
      ((op === TaskCtrlOpKind.LoadA || op === TaskCtrlOpKind.LoadB) && isConvertCtrl(ctrl))
  private def readsScale(ctrl: AmuCtrlIO, a: Boolean): Bool =
    isScaledCtrl(ctrl) || (isConvertCtrl(ctrl) && !isDynamicCtrl(ctrl) &&
      (if (a) decodeLsu(ctrl).isA else decodeLsu(ctrl).isB))
  private def scaleReaderReg(ctrl: AmuCtrlIO, a: Boolean): UInt =
    Mux(isConvertCtrl(ctrl), decodeLsu(ctrl).ms, if (a) decodeMma(ctrl).ms1 else decodeMma(ctrl).ms2)
  private def convertKElements(ctrl: AmuCtrlIO): UInt = {
    val lsu = decodeLsu(ctrl)
    Mux(lsu.isA, lsu.column, lsu.row)
  }
  private def convertKVectors(ctrl: AmuCtrlIO): UInt = {
    val k = convertKElements(ctrl)
    val bytes = Mux(AmuConvertLoad.computeType(ctrl) === MteComputeType.Mxfp4F32, (k +& 1.U) >> 1, k)
    (bytes +& (ReduceWidthByte - 1).U) / ReduceWidthByte.U
  }
  private def scaleReaderRows(ctrl: AmuCtrlIO, a: Boolean): UInt =
    Mux(isConvertCtrl(ctrl), if (a) decodeLsu(ctrl).row else decodeLsu(ctrl).column,
      if (a) decodeMma(ctrl).mtilem else decodeMma(ctrl).mtilen)
  private def scaleReaderElements(ctrl: AmuCtrlIO): UInt =
    Mux(isConvertCtrl(ctrl), convertKElements(ctrl), decodeMma(ctrl).mtilek)
  private def scaleReaderKVectors(ctrl: AmuCtrlIO): UInt =
    Mux(isConvertCtrl(ctrl), convertKVectors(ctrl), mmaKVectors(ctrl))
  private def computeTypeForCtrl(ctrl: AmuCtrlIO): UInt =
    Mux(isConvertCtrl(ctrl), AmuConvertLoad.computeType(ctrl),
      Mux(isScaledCtrl(ctrl), AmuScaleLoad.mmaComputeType(ctrl), decodeMmaComputeType(decodeMma(ctrl))))

  private def matchingScaleBanks(
    banks: Vec[ScaleBankState], reg: UInt, generation: UInt, rows: UInt, k: UInt, blocks: UInt, mode: UInt
  ): UInt = VecInit(banks.map(b => b.occupied && b.valid && !b.inUse &&
    b.regId === abIdx(reg) && b.generation === generation &&
    b.rows === rows && b.kVectors === k && b.scaleBlocks === blocks && b.computeType === mode)).asUInt
  private def reusableScaleBanks(banks: Vec[ScaleBankState], reg: UInt): UInt =
    VecInit(banks.map(b => !b.occupied || (b.valid && !b.inUse && b.regId === abIdx(reg)))).asUInt
  val scaleAReaderBusy = scaleA.map(_.inUse).reduce(_ || _)
  val scaleBReaderBusy = scaleB.map(_.inUse).reduce(_ || _)
  private def scaleBlocks(kElements: UInt): UInt = (kElements +& 31.U) >> 5
  private def mmaKVectors(ctrl: AmuCtrlIO): UInt = {
    val mma = decodeMma(ctrl)
    val bytes = computeKFromMsew(mma.mtilek, mma.types1)
    Mux(isScaledCtrl(ctrl), (bytes +& (ReduceWidthByte - 1).U) / ReduceWidthByte.U,
      bytes / ReduceWidthByte.U)
  }

  private def isReadAB(op: TaskCtrlOpKind.Type, mma: AmuMmaIO, lsu: AmuLsuIO, reg: UInt, treatStoreAsABRead: Bool): Bool = {
    (op === TaskCtrlOpKind.Compute) && (abIdx(mma.ms1) === abIdx(reg) || abIdx(mma.ms2) === abIdx(reg)) ||
      (op === TaskCtrlOpKind.Store) && treatStoreAsABRead && (abIdx(lsu.ms) === abIdx(reg))
  }

  private def isReadC(op: TaskCtrlOpKind.Type, mma: AmuMmaIO, lsu: AmuLsuIO, reg: UInt, treatStoreAsCRead: Bool): Bool = {
    ((op === TaskCtrlOpKind.Compute) && (cIdx(mma.md) === cIdx(reg))) ||
      ((op === TaskCtrlOpKind.Store) && treatStoreAsCRead && (cIdx(lsu.ms) === cIdx(reg)))
  }

  private def isWriteAB(op: TaskCtrlOpKind.Type, lsu: AmuLsuIO, arith: AmuArithIO, reg: UInt): Bool = {
    ((op === TaskCtrlOpKind.LoadA || op === TaskCtrlOpKind.LoadB) && (abIdx(lsu.ms) === abIdx(reg))) ||
      ((op === TaskCtrlOpKind.ZeroTr) && (abIdx(arith.md) === abIdx(reg)))
  }

  private def isWriteC(op: TaskCtrlOpKind.Type, mma: AmuMmaIO, lsu: AmuLsuIO, arith: AmuArithIO, reg: UInt): Bool = {
    ((op === TaskCtrlOpKind.Compute) && (cIdx(mma.md) === cIdx(reg))) ||
      ((op === TaskCtrlOpKind.LoadC) && (cIdx(lsu.ms) === cIdx(reg))) ||
      ((op === TaskCtrlOpKind.ZeroAcc) && (cIdx(arith.md) === cIdx(reg)))
  }

  private def isReadAOfCompute(op: TaskCtrlOpKind.Type, mma: AmuMmaIO, reg: UInt): Bool = {
    (op === TaskCtrlOpKind.Compute) && (abIdx(mma.ms1) === abIdx(reg))
  }

  private def isReadBOfCompute(op: TaskCtrlOpKind.Type, mma: AmuMmaIO, reg: UInt): Bool = {
    (op === TaskCtrlOpKind.Compute) && (abIdx(mma.ms2) === abIdx(reg))
  }

  // ===================== Done handshakes (level-ready) =====================
  val amlOwnerValid = fuAML.busy && slots(fuAML.ownerSlot).valid
  val bmlOwnerValid = fuBML.busy && slots(fuBML.ownerSlot).valid
  val cmlLoadOwnerValid = fuCMLLoad.busy && slots(fuCMLLoad.ownerSlot).valid
  val cmlStoreOwnerValid = fuCMLStore.busy && slots(fuCMLStore.ownerSlot).valid
  val computeOwnerValid = fuCompute.busy && slots(fuCompute.ownerSlot).valid
  val aslOwnerValid = fuASL.busy && slots(fuASL.ownerSlot).valid
  val bslOwnerValid = fuBSL.busy && slots(fuBSL.ownerSlot).valid
  val computeOwnerSlot = slots(fuCompute.ownerSlot)
  val computeOwnerMma = decodeMma(computeOwnerSlot.entry.ctrl)
  val computeOwnerScaled = isScaledCtrl(computeOwnerSlot.entry.ctrl)
  val dynamicAOwner = amlOwnerValid && isDynamicCtrl(slots(fuAML.ownerSlot).entry.ctrl)
  val dynamicBOwner = bmlOwnerValid && isDynamicCtrl(slots(fuBML.ownerSlot).entry.ctrl)
  val dynamicAScaleOwner = aslOwnerValid && isDynamicCtrl(slots(fuASL.ownerSlot).entry.ctrl)
  val dynamicBScaleOwner = bslOwnerValid && isDynamicCtrl(slots(fuBSL.ownerSlot).entry.ctrl)
  val aOwnersMatch = amlOwnerValid && aslOwnerValid && fuAML.ownerSlot === fuASL.ownerSlot
  val bOwnersMatch = bmlOwnerValid && bslOwnerValid && fuBML.ownerSlot === fuBSL.ownerSlot
  assert(!(amlOwnerValid && isConvertCtrl(slots(fuAML.ownerSlot).entry.ctrl) && !dynamicAOwner &&
    computeOwnerValid && computeOwnerScaled && !computeOwnerSlot.scaleReadADone),
    "TaskController: AML conversion and ASC cannot own the A scale read port together")
  assert(!(bmlOwnerValid && isConvertCtrl(slots(fuBML.ownerSlot).entry.ctrl) && !dynamicBOwner &&
    computeOwnerValid && computeOwnerScaled && !computeOwnerSlot.scaleReadBDone),
    "TaskController: BML conversion and BSC cannot own the B scale read port together")

  // Both engines hold End independently. Cross-qualify Ready with the OTHER
  // valid, never its ready/fire, so the same-owner join has no combinational loop.
  io.AML_MicroTask_Config.MicroTaskEndReady := amlOwnerValid && (!dynamicAOwner ||
    (aOwnersMatch && io.ASL_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B)))
  io.BML_MicroTask_Config.MicroTaskEndReady := bmlOwnerValid && (!dynamicBOwner ||
    (bOwnersMatch && io.BSL_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B)))
  io.CML_MicroTask_Config.LoadMicroTaskEndReady := cmlLoadOwnerValid
  io.CML_MicroTask_Config.StoreMicroTaskEndReady := cmlStoreOwnerValid
  io.ADC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid
  io.BDC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid
  io.CDC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid && !computeOwnerSlot.writeCDone
  io.ASL_MicroTask_Config.foreach(_.MicroTaskEndReady := aslOwnerValid &&
    (!dynamicAScaleOwner || (aOwnersMatch && io.AML_MicroTask_Config.MicroTaskEndValid)))
  io.BSL_MicroTask_Config.foreach(_.MicroTaskEndReady := bslOwnerValid &&
    (!dynamicBScaleOwner || (bOwnersMatch && io.BML_MicroTask_Config.MicroTaskEndValid)))
  io.ASC_MicroTask_Config.foreach(_.MicroTaskEndReady :=
    computeOwnerValid && computeOwnerScaled && !computeOwnerSlot.scaleReadADone)
  io.BSC_MicroTask_Config.foreach(_.MicroTaskEndReady :=
    computeOwnerValid && computeOwnerScaled && !computeOwnerSlot.scaleReadBDone)

  val amlDone = io.AML_MicroTask_Config.MicroTaskEndValid && io.AML_MicroTask_Config.MicroTaskEndReady
  val bmlDone = io.BML_MicroTask_Config.MicroTaskEndValid && io.BML_MicroTask_Config.MicroTaskEndReady
  val cmlLoadDone = io.CML_MicroTask_Config.LoadMicroTaskEndValid && io.CML_MicroTask_Config.LoadMicroTaskEndReady
  val cmlStoreDone = io.CML_MicroTask_Config.StoreMicroTaskEndValid && io.CML_MicroTask_Config.StoreMicroTaskEndReady
  val adcDone = io.ADC_MicroTask_Config.MicroTaskEndValid && io.ADC_MicroTask_Config.MicroTaskEndReady
  val bdcDone = io.BDC_MicroTask_Config.MicroTaskEndValid && io.BDC_MicroTask_Config.MicroTaskEndReady
  val cdcDone = io.CDC_MicroTask_Config.MicroTaskEndValid && io.CDC_MicroTask_Config.MicroTaskEndReady
  val aslDone = io.ASL_MicroTask_Config.map(c => c.MicroTaskEndValid && c.MicroTaskEndReady).getOrElse(false.B)
  val bslDone = io.BSL_MicroTask_Config.map(c => c.MicroTaskEndValid && c.MicroTaskEndReady).getOrElse(false.B)
  val ascDone = io.ASC_MicroTask_Config.map(c => c.MicroTaskEndValid && c.MicroTaskEndReady).getOrElse(false.B)
  val bscDone = io.BSC_MicroTask_Config.map(c => c.MicroTaskEndValid && c.MicroTaskEndReady).getOrElse(false.B)
  val computeFinished = computeOwnerValid && Mux(computeOwnerScaled,
    (computeOwnerSlot.writeCDone || cdcDone) &&
      (computeOwnerSlot.readADone || adcDone) && (computeOwnerSlot.readBDone || bdcDone) &&
      (computeOwnerSlot.scaleReadADone || ascDone) && (computeOwnerSlot.scaleReadBDone || bscDone),
    cdcDone)

  io.ownedScaleTrace.foreach { traces =>
    Seq((fuASL, scaleA, aslDone), (fuBSL, scaleB, bslDone)).zipWithIndex.foreach {
      case ((fu, banks, done), side) =>
        val owner = slots(fu.ownerSlot)
        val ctrl = owner.entry.ctrl
        val bankId = if (side == 0) owner.scaleABank else owner.scaleBBank
        val bank = banks(bankId)
        val trace = traces(side)
        trace := 0.U.asTypeOf(new TaskCtrlOwnedScaleTrace)
        trace.busy := fu.busy && owner.valid
        trace.complete := done
        trace.completionPublishes.foreach(_ := !isDynamicCtrl(ctrl) ||
          (if (side == 0) io.AML_MicroTask_Config.MicroTaskFlags === 0.U
           else io.BML_MicroTask_Config.MicroTaskFlags === 0.U))
        trace.pc := ctrl.pc.get
        trace.coreid := ctrl.coreid.get
        trace.retireId := ctrl.retireId.get
        trace.epoch := ctrl.epoch.get
        trace.regId := decodeLsu(ctrl).ms
        trace.bankId := bankId
        trace.generation := bank.generation
        trace.rows := bank.rows
        trace.kVectors := bank.kVectors
        trace.blocks := bank.scaleBlocks
        trace.rowPitch := bank.scaleBlocks
        trace.computeType := bank.computeType
        // valid changes on the owned completion edge, never at reservation.
        trace.published := bank.valid
    }
  }

  io.ASL_MicroTask_Config.foreach(c => when(c.MicroTaskEndValid) {
    assert(aslOwnerValid, "TaskController: ASL done without valid owner")
  })
  io.BSL_MicroTask_Config.foreach(c => when(c.MicroTaskEndValid) {
    assert(bslOwnerValid, "TaskController: BSL done without valid owner")
  })
  io.ASC_MicroTask_Config.foreach(c => when(c.MicroTaskEndValid) {
    assert(computeOwnerValid && computeOwnerScaled, "TaskController: ASC done without scaled owner")
  })
  io.BSC_MicroTask_Config.foreach(c => when(c.MicroTaskEndValid) {
    assert(computeOwnerValid && computeOwnerScaled, "TaskController: BSC done without scaled owner")
  })

  when(io.AML_MicroTask_Config.MicroTaskEndValid) {
    assert(amlOwnerValid, "TaskController: AML done without valid owner")
  }
  when(io.BML_MicroTask_Config.MicroTaskEndValid) {
    assert(bmlOwnerValid, "TaskController: BML done without valid owner")
  }
  when(io.CML_MicroTask_Config.LoadMicroTaskEndValid) {
    assert(cmlLoadOwnerValid, "TaskController: CML-load done without valid owner")
  }
  when(io.CML_MicroTask_Config.StoreMicroTaskEndValid) {
    assert(cmlStoreOwnerValid, "TaskController: CML-store done without valid owner")
  }
  when(io.ADC_MicroTask_Config.MicroTaskEndValid) {
    assert(computeOwnerValid, "TaskController: ADC done without valid owner")
  }
  when(io.BDC_MicroTask_Config.MicroTaskEndValid) {
    assert(computeOwnerValid, "TaskController: BDC done without valid owner")
  }
  when(io.CDC_MicroTask_Config.MicroTaskEndValid) {
    assert(computeOwnerValid, "TaskController: CDC done without valid owner")
  }

  // ===================== Enqueue-side classification =====================
  val deqValid = decodedFifo.io.deq.valid
  val deqEntry = decodedFifo.io.deq.bits

  val deqCtrl = deqEntry.ctrl
  val deqIsMma = deqCtrl.isMma()
  val deqIsArith = deqCtrl.isArith()
  val deqIsLsu = deqCtrl.isMls()
  val deqIsRelease = deqCtrl.isRelease()

  val deqLsu = decodeLsu(deqCtrl)
  val deqMma = decodeMma(deqCtrl)
  val deqArith = decodeArith(deqCtrl)

  val deqIsLoad = deqIsLsu && deqLsu.ls === 0.U
  val deqIsScale = AmuScaleLoad.selected(deqCtrl)
  val deqConvertFlag = AmuConvertLoad.selected(deqCtrl)
  val deqConvertMode = AmuConvertLoad.computeType(deqCtrl)
  val deqConvertModeSupported =
    (cuteMatrixExtension.enableMxfp4Fp32.B && deqConvertMode === MteComputeType.Mxfp4F32) ||
      (cuteMatrixExtension.enableMxfp8Fp32.B &&
        (deqConvertMode === MteComputeType.Mxfp8e4m3F32 || deqConvertMode === MteComputeType.Mxfp8e5m2F32))
  val deqConvertRows = Mux(deqLsu.isA, deqLsu.row, deqLsu.column)
  // Invalid transport/static configuration is not a numeric conversion reject.
  // Do not reinterpret a disabled/malformed converting load as an ordinary MLS.
  val deqConvertLegal = ExperimentalMxfpConvertLoad.B && cuteMatrixExtension.enableScalingFactor.B &&
    deqIsLoad && !deqIsScale && !deqLsu.isacc && !deqLsu.transpose && (deqLsu.isA ^ deqLsu.isB) &&
    deqLsu.widths === Bundles.MSew.e32 && deqConvertModeSupported &&
    convertKElements(deqCtrl) === 64.U && deqConvertRows =/= 0.U && deqConvertRows <= Tensor_MN.U &&
    convertKVectors(deqCtrl) <= ReduceGroupSize.U &&
    deqLsu.ms < ABMatrixRegCount.U && deqCtrl.data(149, 133) === 0.U &&
    (!deqCtrl.data(AmuConvertLoad.DynamicBit) ||
      (ExperimentalMxfpDynamicQuant.B && deqConvertMode === MteComputeType.Mxfp4F32 &&
        deqConvertRows === 8.U && convertKVectors(deqCtrl) === 1.U))
  val deqLoadSelOH = Cat(deqLsu.isacc, deqLsu.isB, deqLsu.isA)
  val deqLoadSelOneHot = PopCount(deqLoadSelOH) === 1.U
  val deqIsMzeroLike = deqIsArith && isMzeroLike(deqArith)

  when(deqValid && deqIsLoad) {
    assert(deqLoadSelOneHot, "TaskController: MLS load selector must be onehot among isA/isB/isacc")
  }
  when(deqValid && deqConvertFlag) {
    assert(ExperimentalMxfpConvertLoad.B, "TaskController: experimental MX converting load disabled")
    assert(deqConvertLegal, "TaskController: converting load requires normal F32 A/B, supported MX format and legal K64 shape")
  }
  when(deqValid && deqCtrl.isMls() && deqCtrl.data(AmuConvertLoad.DynamicBit)) {
    assert(deqConvertFlag && deqConvertLegal, "TaskController: malformed/disabled dynamic selector")
  }
  when(deqValid && deqIsScale) {
    assert(cuteMatrixExtension.enableScalingFactor.B && !deqLsu.ls && !deqLsu.isacc &&
      !deqLsu.transpose && (deqLsu.isA ^ deqLsu.isB),
      "TaskController: scale load requires enabled scaling and one non-transposed A/B destination")
    val mode = AmuScaleLoad.computeType(deqCtrl)
    assert((cuteMatrixExtension.enableMxfp4Fp32.B && mode === MteComputeType.Mxfp4F32) ||
      (cuteMatrixExtension.enableMxfp8Fp32.B &&
        (mode === MteComputeType.Mxfp8e4m3F32 || mode === MteComputeType.Mxfp8e5m2F32)),
      "TaskController: unsupported scale-load format")
    assert(deqCtrl.data(149, 131) === 0.U && deqLsu.ms < ABMatrixRegCount.U,
      "TaskController: scale load reserved bits/register index must be valid")
  }
  when(deqValid && deqIsMma) {
    val mode = AmuScaleLoad.mmaComputeType(deqCtrl)
    assert(deqCtrl.data(149, 57) === 0.U, "TaskController: reserved MMA metadata must be zero")
    assert(mode === 0.U || isScaledCtrl(deqCtrl), "TaskController: unsupported MMA scale metadata")
    when(isScaledCtrl(deqCtrl)) {
      val fp4 = mode === MteComputeType.Mxfp4F32
      val fp8e4 = mode === MteComputeType.Mxfp8e4m3F32
      val expectedType = Mux(fp4, "b111".U, Mux(fp8e4, "b100".U, "b000".U))
      assert(deqMma.isfp && deqMma.types1 === expectedType && deqMma.types2 === expectedType,
        "TaskController: scale metadata must match MMA data format")
      assert(Mux(fp4, cuteMatrixExtension.enableMxfp4Fp32.B, cuteMatrixExtension.enableMxfp8Fp32.B),
        "TaskController: scaled MMA format disabled")
      val kBytes = computeKFromMsew(deqMma.mtilek, deqMma.types1)
      assert(kBytes =/= 0.U && kBytes <= Tensor_K.U,
        "TaskController: scaled MMA K exceeds the data register")
      assert(deqMma.mtilem =/= 0.U && deqMma.mtilen =/= 0.U &&
        deqMma.mtilem <= Tensor_MN.U && deqMma.mtilen <= Tensor_MN.U,
        "TaskController: scaled MMA dimensions exceed the data registers")
      assert(deqMma.ms1 < ABMatrixRegCount.U && deqMma.ms2 < ABMatrixRegCount.U,
        "TaskController: scaled MMA register index must be valid")
    }.otherwise {
      assert(!(deqMma.isfp && (deqMma.types1 === "b111".U || deqMma.types1 === "b011".U)),
        "TaskController: FP4 requires explicit supported scale metadata")
    }
  }

  val deqOpKind = Wire(TaskCtrlOpKind())
  deqOpKind := TaskCtrlOpKind.NopLike
  when(deqIsMma) {
    deqOpKind := TaskCtrlOpKind.Compute
  }.elsewhen(deqIsScale && deqIsLoad && deqLsu.isA) {
    deqOpKind := TaskCtrlOpKind.LoadScaleA
  }.elsewhen(deqIsScale && deqIsLoad && deqLsu.isB) {
    deqOpKind := TaskCtrlOpKind.LoadScaleB
  }.elsewhen(deqIsLsu && deqLsu.ls === 1.U) {
    deqOpKind := TaskCtrlOpKind.Store
  }.elsewhen(deqIsLoad && deqLoadSelOneHot && deqLsu.isA) {
    deqOpKind := TaskCtrlOpKind.LoadA
  }.elsewhen(deqIsLoad && deqLoadSelOneHot && deqLsu.isB) {
    deqOpKind := TaskCtrlOpKind.LoadB
  }.elsewhen(deqIsLoad && deqLoadSelOneHot && deqLsu.isacc) {
    deqOpKind := TaskCtrlOpKind.LoadC
  }.elsewhen(deqIsRelease) {
    deqOpKind := TaskCtrlOpKind.Release
  }.elsewhen(deqIsMzeroLike && deqArith.md(2) === 1.U) {
    deqOpKind := TaskCtrlOpKind.ZeroAcc
  }.elsewhen(deqIsMzeroLike && deqArith.md(2) === 0.U) {
    deqOpKind := TaskCtrlOpKind.ZeroTr
  }

  // ===================== Oldest-ready issue selection (based on cycle-start state) =====================
  val completedVec = VecInit(slots.map(s => s.valid && s.completed)).asUInt
  val readAVec = VecInit(slots.map(s => s.valid && s.readADone)).asUInt
  val readBVec = VecInit(slots.map(s => s.valid && s.readBDone)).asUInt
  val scaleReadAVec = VecInit(slots.map(s => s.valid && s.scaleReadADone)).asUInt
  val scaleReadBVec = VecInit(slots.map(s => s.valid && s.scaleReadBDone)).asUInt

  val readyBySlot = Wire(Vec(WinDepth, Bool()))
  readyBySlot.foreach(_ := false.B)
  for (slotIdx <- 0 until WinDepth) {
    val slot = slots(slotIdx)
    val slotAge = (slotIdx.U + WinDepth.U - winHead)(SlotIdxWidth - 1, 0)
    val inWindow = slotAge < winCount
    val slotCanConsider = inWindow && slot.valid && !slot.issued
    // Deliberately use the cycle-start ring view, not depOlderMask's
    // post-retire view: a completed head producer still owns this cycle's enq.
    // Keep all Compute slots here, including integer Compute as before.
    val olderFlagProducerPending = (0 until WinDepth).map { j =>
      val olderAge = (j.U + WinDepth.U - winHead)(SlotIdxWidth - 1, 0)
      slots(j).valid && olderAge < slotAge &&
        isFlagProducer(slots(j).opKind, slots(j).entry.ctrl)
    }.reduce(_ || _)
    val slotLsu = decodeLsu(slot.entry.ctrl)
    val slotScaled = isScaledCtrl(slot.entry.ctrl)
    val slotMode = computeTypeForCtrl(slot.entry.ctrl)
    val slotDynamic = isDynamicCtrl(slot.entry.ctrl)
    val aMatches = matchingScaleBanks(scaleA, scaleReaderReg(slot.entry.ctrl, a = true),
      slot.scaleAGeneration, scaleReaderRows(slot.entry.ctrl, a = true),
      scaleReaderKVectors(slot.entry.ctrl), scaleBlocks(scaleReaderElements(slot.entry.ctrl)), slotMode)
    val bMatches = matchingScaleBanks(scaleB, scaleReaderReg(slot.entry.ctrl, a = false),
      slot.scaleBGeneration, scaleReaderRows(slot.entry.ctrl, a = false),
      scaleReaderKVectors(slot.entry.ctrl), scaleBlocks(scaleReaderElements(slot.entry.ctrl)), slotMode)
    val slotScaleReady =
      (!readsScale(slot.entry.ctrl, a = true) || (aMatches.orR && !scaleAReaderBusy)) &&
      (!readsScale(slot.entry.ctrl, a = false) || (bMatches.orR && !scaleBReaderBusy)) &&
      (!slotScaled || (
        io.ASC_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B) &&
        io.BSC_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B)))

    val slotDepReady = Mux(
      slotCanConsider,
      ((slot.waitCompleteMask & (~completedVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitReadAMask & (~readAVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitReadBMask & (~readBVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitScaleReadAMask & (~scaleReadAVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitScaleReadBMask & (~scaleReadBVec)(WinDepth - 1, 0)) === 0.U),
      false.B
    )

    val slotFuReady = Mux(
      slotCanConsider,
      MuxLookup(slot.opKind.asUInt, true.B)(Seq(
        TaskCtrlOpKind.LoadA.asUInt -> (!fuAML.busy && io.AML_MicroTask_Config.MicroTaskReady &&
          slotScaleReady && (!slotDynamic || (!fuASL.busy &&
            io.ASL_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B) &&
            reusableScaleBanks(scaleA, slotLsu.ms).orR))),
        TaskCtrlOpKind.LoadB.asUInt -> (!fuBML.busy && io.BML_MicroTask_Config.MicroTaskReady &&
          slotScaleReady && (!slotDynamic || (!fuBSL.busy &&
            io.BSL_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B) &&
            reusableScaleBanks(scaleB, slotLsu.ms).orR))),
        TaskCtrlOpKind.LoadScaleA.asUInt -> (!fuASL.busy && reusableScaleBanks(scaleA, slotLsu.ms).orR &&
          io.ASL_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B)),
        TaskCtrlOpKind.LoadScaleB.asUInt -> (!fuBSL.busy && reusableScaleBanks(scaleB, slotLsu.ms).orR &&
          io.BSL_MicroTask_Config.map(_.MicroTaskReady).getOrElse(false.B)),
        TaskCtrlOpKind.LoadC.asUInt -> (!fuCMLLoad.busy && io.CML_MicroTask_Config.LoadMicroTaskReady),
        TaskCtrlOpKind.ZeroAcc.asUInt -> (!fuCMLLoad.busy && io.CML_MicroTask_Config.LoadMicroTaskReady),
        TaskCtrlOpKind.ZeroTr.asUInt -> (!fuAML.busy && io.AML_MicroTask_Config.MicroTaskReady),
        TaskCtrlOpKind.Store.asUInt -> (!fuCMLStore.busy && io.CML_MicroTask_Config.StoreMicroTaskReady),
        TaskCtrlOpKind.Compute.asUInt -> (!fuCompute.busy && slotScaleReady &&
          io.ADC_MicroTask_Config.MicroTaskReady && io.BDC_MicroTask_Config.MicroTaskReady && io.CDC_MicroTask_Config.MicroTaskReady),
        TaskCtrlOpKind.Release.asUInt -> (!olderFlagProducerPending && returnQ.io.enq.ready),
        TaskCtrlOpKind.NopLike.asUInt -> true.B
      )),
      false.B
    )

    readyBySlot(slotIdx) := slotCanConsider && slotDepReady && slotFuReady
  }

  val readyByAge = VecInit(readyBySlot.rotate(winHead))
  val issueFound = readyByAge.asUInt.orR
  val issueAgeOH = PriorityEncoderOH(readyByAge)
  val issueSlotOH = VecInit(issueAgeOH.rotateRight(winHead))
  val issueSlotIdx = WireInit(0.U(SlotIdxWidth.W))
  when(issueFound) {
    issueSlotIdx := OHToUInt(issueSlotOH)
  }

  val issueFire = issueFound
  val issueSlot = slots(issueSlotIdx)

  val deqStoreReadsAB = deqIsLsu && (deqLsu.ls === 1.U) && !deqLsu.isacc
  val deqStoreReadsC = deqIsLsu && (deqLsu.ls === 1.U) && deqLsu.isacc

  // ===================== Retirement lookahead (after done convergence) =====================
  val headSlot = slots(winHead)

  val headDoneByFu =
    (amlDone && fuAML.busy && fuAML.ownerSlot === winHead) ||
    (bmlDone && fuBML.busy && fuBML.ownerSlot === winHead) ||
    (aslDone && fuASL.busy && fuASL.ownerSlot === winHead) ||
    (bslDone && fuBSL.busy && fuBSL.ownerSlot === winHead) ||
    (cmlLoadDone && fuCMLLoad.busy && fuCMLLoad.ownerSlot === winHead) ||
    (cmlStoreDone && fuCMLStore.busy && fuCMLStore.ownerSlot === winHead) ||
    (computeFinished && fuCompute.ownerSlot === winHead)

  val headCompletedAfterDone = headSlot.completed || headDoneByFu
  val retireEligible = headSlot.valid && headCompletedAfterDone
  val headConvert = isConvertCtrl(headSlot.entry.ctrl)
  val headReturnsFlags = (headSlot.opKind === TaskCtrlOpKind.Compute &&
    decodeMma(headSlot.entry.ctrl).isfp) || headConvert
  val flagReturn = retireEligible && headReturnsFlags
  val completionFlagsNext = Mux(headConvert && amlDone && fuAML.ownerSlot === winHead,
    io.AML_MicroTask_Config.MicroTaskFlags,
    Mux(headConvert && bmlDone && fuBML.ownerSlot === winHead,
      io.BML_MicroTask_Config.MicroTaskFlags,
      Mux(cdcDone && fuCompute.ownerSlot === winHead,
        io.CDC_MicroTask_Config.MicroTaskFlags, headSlot.completionFlags)))
  val releaseReturn = issueFire && issueSlot.opKind === TaskCtrlOpKind.Release
  // Explicit single-entry arbitration. The ring-age admission above makes
  // these mutually exclusive; head-producer priority is also encoded in the mux.
  assert(!(flagReturn && releaseReturn), "TaskController: two owners of return enqueue")
  returnQ.io.enq.valid := flagReturn || releaseReturn
  returnQ.io.enq.bits := 0.U.asTypeOf(returnQ.io.enq.bits)
  if (EnableDifftest) {
    val returnOwner = Mux(flagReturn, headSlot.entry.ctrl, issueSlot.entry.ctrl)
    returnQ.io.enq.bits.tracePc.get := returnOwner.pc.get
    returnQ.io.enq.bits.traceCoreid.get := returnOwner.coreid.get
    returnQ.io.enq.bits.traceRetireId.get := returnOwner.retireId.get
    returnQ.io.enq.bits.traceEpoch.get := returnOwner.epoch.get
    returnQ.io.enq.bits.traceComputeType.get := Mux(flagReturn,
      computeTypeForCtrl(headSlot.entry.ctrl), 0.U)
  }
  when(flagReturn) {
    returnQ.io.enq.bits.flagsValid := true.B
    returnQ.io.enq.bits.flags := completionFlagsNext
  }.elsewhen(releaseReturn) {
    val release = issueSlot.entry.ctrl.data.asTypeOf(new AmuReleaseIO)
    returnQ.io.enq.bits.msyncRd(release.msyncRd) := true.B
  }
  val retireFire = retireEligible && (!headReturnsFlags || returnQ.io.enq.ready)
  when(flagReturn && retireFire) {
    assert(returnQ.io.enq.fire, "TaskController: flag-producing owner retired without preserving flags")
  }
  when(releaseReturn) {
    assert(returnQ.io.enq.fire, "TaskController: Release issued without preserving return")
  }
  val returnWasHeld = RegNext(io.ygjkctrl.mrelease.valid && !io.ygjkctrl.mrelease.ready, false.B)
  val heldReturn = RegEnable(io.ygjkctrl.mrelease.bits.asUInt,
    io.ygjkctrl.mrelease.valid && !io.ygjkctrl.mrelease.ready)
  when(returnWasHeld) {
    assert(io.ygjkctrl.mrelease.valid && io.ygjkctrl.mrelease.bits.asUInt === heldReturn,
      "TaskController: held postcommit return changed")
  }

  // Allow retire + enqueue in the same cycle, including full-window reuse of the retired slot
  val windowFull = winCount === WinDepth.U
  val enqueueCanFire = deqValid && (!deqConvertFlag || deqConvertLegal) && (!windowFull || retireFire)
  decodedFifo.io.deq.ready := enqueueCanFire
  val enqueueFire = decodedFifo.io.deq.fire

  val enqueueSlotIdx = Mux(windowFull, winHead, winTail)
  val ownedWork = deqValid || (winCount =/= 0.U)
  val allFuIdle = !Seq(fuAML, fuBML, fuCMLLoad, fuCompute, fuCMLStore, fuASL, fuBSL)
    .map(_.busy).reduce(_ || _)
  val anyEndValid = Seq(
    io.AML_MicroTask_Config.MicroTaskEndValid,
    io.BML_MicroTask_Config.MicroTaskEndValid,
    io.CML_MicroTask_Config.LoadMicroTaskEndValid,
    io.CML_MicroTask_Config.StoreMicroTaskEndValid,
    io.ADC_MicroTask_Config.MicroTaskEndValid,
    io.BDC_MicroTask_Config.MicroTaskEndValid,
    io.CDC_MicroTask_Config.MicroTaskEndValid,
    io.CDC_MicroTask_Config.MicroTask_TEComputeEndValid,
    io.ASL_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B),
    io.BSL_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B),
    io.ASC_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B),
    io.BSC_MicroTask_Config.map(_.MicroTaskEndValid).getOrElse(false.B)
  ).reduce(_ || _)
  io.ygjkctrl.resetReady := !io.ygjkctrl.amuCtrl.valid &&
    !decodedFifo.io.deq.valid && winCount === 0.U && !slots.map(_.valid).reduce(_ || _) &&
    allFuIdle && !anyEndValid && !scaleA.map(_.inUse).reduce(_ || _) &&
    !scaleB.map(_.inUse).reduce(_ || _) && io.downstreamIdle &&
    !returnQ.io.enq.valid && !returnQ.io.deq.valid
  io.ygjkctrl.minitClearAck := RegNext(io.ygjkctrl.minitClear, false.B)

  // ===================== Issue dispatch bridge =====================
  val issueCtrl = issueSlot.entry.ctrl
  if (EnableDifftest) {
    Seq(io.AML_MicroTask_Config.retireId.get, io.BML_MicroTask_Config.retireId.get,
      io.CML_MicroTask_Config.retireId.get, io.CDC_MicroTask_Config.retireId.get)
      .foreach(_ := issueCtrl.retireId.get)
    Seq(io.AML_MicroTask_Config.epoch.get, io.BML_MicroTask_Config.epoch.get,
      io.CML_MicroTask_Config.epoch.get, io.CDC_MicroTask_Config.epoch.get)
      .foreach(_ := issueCtrl.epoch.get)
  }
  val issueLsu = decodeLsu(issueCtrl)
  val issueMma = decodeMma(issueCtrl)
  val issueArith = decodeArith(issueCtrl)
  val issueMode = computeTypeForCtrl(issueCtrl)
  val allocScaleABank = PriorityEncoder(reusableScaleBanks(scaleA, issueLsu.ms))
  val allocScaleBBank = PriorityEncoder(reusableScaleBanks(scaleB, issueLsu.ms))
  val issueScaleABank = PriorityEncoder(matchingScaleBanks(scaleA, scaleReaderReg(issueCtrl, a = true),
    issueSlot.scaleAGeneration, scaleReaderRows(issueCtrl, a = true),
    scaleReaderKVectors(issueCtrl), scaleBlocks(scaleReaderElements(issueCtrl)), issueMode))
  val issueScaleBBank = PriorityEncoder(matchingScaleBanks(scaleB, scaleReaderReg(issueCtrl, a = false),
    issueSlot.scaleBGeneration, scaleReaderRows(issueCtrl, a = false),
    scaleReaderKVectors(issueCtrl), scaleBlocks(scaleReaderElements(issueCtrl)), issueMode))
  // CPU producer MLS contract for BOTH sides: row=logical rows,
  // column=ceil(original K/32) E8M0 BYTES, widths=e8.
  val scaleLoadRows = issueLsu.row
  val scaleLoadBlocks = issueLsu.column
  val scaleLoadMode = AmuScaleLoad.computeType(issueCtrl)
  val scalesPerVector = ScaleVecWidth(scaleLoadMode)
  val scaleLoadK = (scaleLoadBlocks +& (scalesPerVector - 1.U)) / scalesPerVector
  val scaleLoadRowBytes = scaleLoadBlocks
  when(issueFire && (issueSlot.opKind === TaskCtrlOpKind.LoadScaleA ||
      issueSlot.opKind === TaskCtrlOpKind.LoadScaleB)) {
    assert(issueLsu.widths === Bundles.MSew.e8, "TaskController: MX32 scales are E8M0 bytes")
    assert(scaleLoadBlocks =/= 0.U && scaleLoadRows =/= 0.U && scaleLoadRows <= Tensor_MN.U,
      "TaskController: scale-load logical shape must fit the scale register")
    // The canonical scale reader fetches one beat per M/N group. First stage
    // Non-power-of-two row pitches are safe here only when the ENTIRE
    // rectangle fits one beat: later groups must not straddle SRAM entries.
    val powerOfTwoPitch = (scaleLoadBlocks & (scaleLoadBlocks - 1.U)) === 0.U
    assert(scaleLoadK <= ReduceGroupSize.U &&
      (powerOfTwoPitch || scaleLoadRows * scaleLoadBlocks <= outsideDataWidthByte.U) &&
      scaleLoadRowBytes * Matrix_MN.U <= outsideDataWidthByte.U,
      "TaskController: scale group must fit one aligned scratchpad beat")
    val sourceOffset = issueLsu.baseAddr % outsideDataWidthByte.U
    assert(issueLsu.stride >= scaleLoadRowBytes,
      "TaskController: scale source stride is shorter than the logical row")
    assert(issueLsu.stride =/= scaleLoadRowBytes || sourceOffset === 0.U ||
      sourceOffset +& (scaleLoadRows * scaleLoadBlocks) <= outsideDataWidthByte.U,
      "TaskController: compact scale buffers must be aligned or fit one containing cacheline")
  }

  private def computeKFromMsew(k: UInt, msew: UInt): UInt = {
    MuxLookup(msew(1, 0), k)(Seq(
      Bundles.MSew.e8 -> k,
      Bundles.MSew.e16 -> (k << 1),
      Bundles.MSew.e32 -> (k << 2),
      // MMA types use low two bits for width (11=e4); the high bit
      // distinguishes signedness/format and must not enter this comparison.
      "b11".U -> ((k +& 1.U) >> 1)
    ))
  }

  private def loadDataType(widths: UInt): UInt = {
    MuxLookup(widths, ElementDataType.DataTypeWidth32)(Seq(
      Bundles.MSew.e8 -> ElementDataType.DataTypeWidth8,
      Bundles.MSew.e16 -> ElementDataType.DataTypeWidth16,
      Bundles.MSew.e32 -> ElementDataType.DataTypeWidth32,
      Bundles.MSew.e4 -> ElementDataType.DataTypeWidth4
    ))
  }

  private val loadByteCountBits = MatrixRegMaxTensorDimBitSize + 2

  private def loadByteCount(dim: UInt, widths: UInt): UInt = {
    val dimWide = dim.pad(loadByteCountBits)
    val byteCount = WireDefault((dimWide << 2)(loadByteCountBits - 1, 0))
    switch(widths) {
      is(Bundles.MSew.e8)  { byteCount := dimWide }
      is(Bundles.MSew.e16) { byteCount := (dimWide << 1)(loadByteCountBits - 1, 0) }
      is(Bundles.MSew.e32) { byteCount := (dimWide << 2)(loadByteCountBits - 1, 0) }
      is(Bundles.MSew.e4)  { byteCount := (dimWide + 1.U) >> 1 }
    }
    byteCount
  }

  private def loadBeatCount(dim: UInt, widths: UInt): UInt = {
    val beatCount = (loadByteCount(dim, widths) + (outsideDataWidthByte - 1).U) >> log2Ceil(outsideDataWidthByte)
    beatCount.pad(MatrixRegMaxTensorDimBitSize)(MatrixRegMaxTensorDimBitSize - 1, 0)
  }

  private def loadHasTail(dim: UInt, widths: UInt): Bool = {
    loadByteCount(dim, widths)(log2Ceil(outsideDataWidthByte) - 1, 0).orR
  }

  private def loadTailByteMask(dim: UInt, widths: UInt): UInt = {
    Cat(0.U((log2Ceil(outsideDataWidthByte + 1) - log2Ceil(outsideDataWidthByte)).W), loadByteCount(dim, widths)(log2Ceil(outsideDataWidthByte) - 1, 0))
  }

  private def decodeMmaComputeType(mma: AmuMmaIO): UInt = {
    val computeType = WireInit(MteComputeType.ComputeTypeUndef)
    when(mma.isfp) {
      when(mma.types1 === "b001".U && mma.types2 === "b001".U) {
        computeType := MteComputeType.F16F16F32
      }.elsewhen(mma.types1 === "b000".U && mma.types2 === "b000".U) {
        computeType := MteComputeType.Fp8e5m2F32
      }.elsewhen(mma.types1 === "b100".U && mma.types2 === "b100".U) {
        computeType := MteComputeType.Fp8e4m3F32
      }.elsewhen(mma.types1 === "b101".U && mma.types2 === "b101".U) {
        computeType := MteComputeType.BF16BF16F32
      }.elsewhen(mma.types1 === "b010".U && mma.types2 === "b010".U) {
        computeType := MteComputeType.ComputeTypeUndef // FP32FP32FP32 is unsupported.
      }.elsewhen(mma.types1 === "b110".U && mma.types2 === "b110".U) {
        computeType := MteComputeType.TF32TF32F32
      }.elsewhen(mma.types1 === "b011".U && mma.types2 === "b011".U) {
        computeType := MteComputeType.Nvfp4F32
      }.elsewhen(mma.types1 === "b111".U && mma.types2 === "b111".U) {
        computeType := MteComputeType.Mxfp4F32
      }.otherwise {
        computeType := MteComputeType.ComputeTypeUndef
      }
    }.otherwise {
      when(mma.types1 === "b000".U && mma.types2 === "b000".U) {
        computeType := MteComputeType.U8U8I32
      }.elsewhen(mma.types1 === "b100".U && mma.types2 === "b000".U) {
        computeType := MteComputeType.I8U8I32
      }.elsewhen(mma.types1 === "b000".U && mma.types2 === "b100".U) {
        computeType := MteComputeType.U8I8I32
      }.elsewhen(mma.types1 === "b100".U && mma.types2 === "b100".U) {
        computeType := MteComputeType.I8I8I32
      }.otherwise {
        computeType := MteComputeType.ComputeTypeUndef
      }
    }
    computeType
  }

  when(issueFire) {
    switch(issueSlot.opKind) {
      is(TaskCtrlOpKind.LoadScaleA) {
        io.ASL_MicroTask_Config.foreach { cfg =>
          cfg.ApplicationScale_A.ApplicationScale_A_BaseVaddr := issueLsu.baseAddr
          cfg.ApplicationScale_A.BlockScale_A_BaseVaddr := issueLsu.baseAddr
          cfg.ApplicationScale_A.computeType := scaleLoadMode
          cfg.ApplicationScale_A.RowStrideBytes := issueLsu.stride
          cfg.ApplicationScale_A.ScaleBlocksPerRow := scaleLoadBlocks
          cfg.MatrixRegTensor_M := scaleLoadRows
          cfg.MatrixRegTensor_K := scaleLoadK
          cfg.ScaleBankId := allocScaleABank
          cfg.Conherent := true.B
          cfg.MicroTaskValid := true.B
        }
      }
      is(TaskCtrlOpKind.LoadScaleB) {
        io.BSL_MicroTask_Config.foreach { cfg =>
          cfg.ApplicationScale_B.ApplicationScale_B_BaseVaddr := issueLsu.baseAddr
          cfg.ApplicationScale_B.BlockScale_B_BaseVaddr := issueLsu.baseAddr
          cfg.ApplicationScale_B.computeType := scaleLoadMode
          cfg.ApplicationScale_B.RowStrideBytes := issueLsu.stride
          cfg.ApplicationScale_B.ScaleBlocksPerRow := scaleLoadBlocks
          cfg.MatrixRegTensor_N := scaleLoadRows
          cfg.MatrixRegTensor_K := scaleLoadK
          cfg.ScaleBankId := allocScaleBBank
          cfg.Conherent := true.B
          cfg.MicroTaskValid := true.B
        }
      }
      is(TaskCtrlOpKind.LoadA) {
        val regIdx = issueLsu.ms(1, 0)
        val matrixDim = Mux(issueLsu.transpose, issueLsu.column, issueLsu.row)
        val reduceDim = Mux(issueLsu.transpose, issueLsu.row, issueLsu.column)
        val kVal = loadBeatCount(reduceDim, issueLsu.widths)

        io.AML_MicroTask_Config.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := issueLsu.baseAddr
        io.AML_MicroTask_Config.ApplicationTensor_A.ApplicationTensor_A_Stride_M := issueLsu.stride
        io.AML_MicroTask_Config.ApplicationTensor_A.dataType := loadDataType(issueLsu.widths)
        io.AML_MicroTask_Config.ApplicationTensor_A.HasTail := loadHasTail(reduceDim, issueLsu.widths)
        io.AML_MicroTask_Config.ApplicationTensor_A.TailByteMask := loadTailByteMask(reduceDim, issueLsu.widths)
        io.AML_MicroTask_Config.ApplicationTensor_A.K_Beat_Count := kVal
        io.AML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := true.B
        io.AML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := false.B
        io.AML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.AML_MicroTask_Config.MatrixRegTensor_M := matrixDim
        io.AML_MicroTask_Config.MatrixRegTensor_K := Mux(isConvertCtrl(issueCtrl), convertKVectors(issueCtrl), kVal)
        io.AML_MicroTask_Config.MatrixRegId := regIdx
        io.AML_MicroTask_Config.Conherent := true.B
        io.AML_MicroTask_Config.Is_Transpose := issueLsu.transpose
        when(isConvertCtrl(issueCtrl)) {
          io.AML_MicroTask_Config.ConvertF32ToMx := true.B
          io.AML_MicroTask_Config.ConvertComputeType := issueMode
          io.AML_MicroTask_Config.ConvertKElements := reduceDim
          io.AML_MicroTask_Config.ConvertScaleBankId := Mux(isDynamicCtrl(issueCtrl), allocScaleABank, issueScaleABank)
          io.AML_MicroTask_Config.ConvertDynamic.foreach(_ := isDynamicCtrl(issueCtrl))
        }
        when(isDynamicCtrl(issueCtrl)) {
          io.ASL_MicroTask_Config.foreach { cfg =>
            cfg.Generated.foreach(_ := true.B)
            cfg.ApplicationScale_A.computeType := MteComputeType.Mxfp4F32
            cfg.ApplicationScale_A.ScaleBlocksPerRow := 2.U
            cfg.MatrixRegTensor_M := 8.U
            cfg.MatrixRegTensor_K := 1.U
            cfg.ScaleBankId := allocScaleABank
            cfg.MicroTaskValid := true.B
          }
        }
        io.AML_MicroTask_Config.MicroTaskValid := true.B
        if (EnableDifftest) {
          io.AML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.AML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issueSlot.fifoIdx
        loadAllocateEvent.needMask := "b001".U
        loadAllocateEvent.row := issueLsu.row
        loadAllocateEvent.column := issueLsu.column
        loadAllocateEvent.transpose := issueLsu.transpose
        loadAllocateEvent.isAcc := false.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issueSlot.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issueSlot.fifoIdx
        loadIssueEvent.needMask := "b001".U
        loadIssueEvent.row := issueLsu.row
        loadIssueEvent.column := issueLsu.column
        loadIssueEvent.transpose := issueLsu.transpose
        loadIssueEvent.isAcc := false.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issueSlot.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.LoadB) {
        val regIdx = issueLsu.ms(1, 0)
        val matrixDim = Mux(issueLsu.transpose, issueLsu.row, issueLsu.column)
        val reduceDim = Mux(issueLsu.transpose, issueLsu.column, issueLsu.row)
        val kVal = loadBeatCount(reduceDim, issueLsu.widths)

        io.BML_MicroTask_Config.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := issueLsu.baseAddr
        io.BML_MicroTask_Config.ApplicationTensor_B.ApplicationTensor_B_Stride_N := issueLsu.stride
        io.BML_MicroTask_Config.ApplicationTensor_B.BlockTensor_B_BaseVaddr := issueLsu.baseAddr
        io.BML_MicroTask_Config.ApplicationTensor_B.dataType := loadDataType(issueLsu.widths)
        io.BML_MicroTask_Config.ApplicationTensor_B.HasTail := loadHasTail(reduceDim, issueLsu.widths)
        io.BML_MicroTask_Config.ApplicationTensor_B.TailByteMask := loadTailByteMask(reduceDim, issueLsu.widths)
        io.BML_MicroTask_Config.ApplicationTensor_B.K_Beat_Count := kVal
        io.BML_MicroTask_Config.MatrixRegTensor_N := matrixDim
        io.BML_MicroTask_Config.MatrixRegTensor_K := Mux(isConvertCtrl(issueCtrl), convertKVectors(issueCtrl), kVal)
        io.BML_MicroTask_Config.MatrixRegId := regIdx
        io.BML_MicroTask_Config.Conherent := true.B
        io.BML_MicroTask_Config.Is_Transpose := issueLsu.transpose
        when(isConvertCtrl(issueCtrl)) {
          io.BML_MicroTask_Config.ConvertF32ToMx := true.B
          io.BML_MicroTask_Config.ConvertComputeType := issueMode
          io.BML_MicroTask_Config.ConvertKElements := reduceDim
          io.BML_MicroTask_Config.ConvertScaleBankId := Mux(isDynamicCtrl(issueCtrl), allocScaleBBank, issueScaleBBank)
          io.BML_MicroTask_Config.ConvertDynamic.foreach(_ := isDynamicCtrl(issueCtrl))
        }
        when(isDynamicCtrl(issueCtrl)) {
          io.BSL_MicroTask_Config.foreach { cfg =>
            cfg.Generated.foreach(_ := true.B)
            cfg.ApplicationScale_B.computeType := MteComputeType.Mxfp4F32
            cfg.ApplicationScale_B.ScaleBlocksPerRow := 2.U
            cfg.MatrixRegTensor_N := 8.U
            cfg.MatrixRegTensor_K := 1.U
            cfg.ScaleBankId := allocScaleBBank
            cfg.MicroTaskValid := true.B
          }
        }
        io.BML_MicroTask_Config.MicroTaskValid := true.B
        if (EnableDifftest) {
          io.BML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.BML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issueSlot.fifoIdx
        loadAllocateEvent.needMask := "b010".U
        loadAllocateEvent.row := issueLsu.row
        loadAllocateEvent.column := issueLsu.column
        loadAllocateEvent.transpose := issueLsu.transpose
        loadAllocateEvent.isAcc := false.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issueSlot.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issueSlot.fifoIdx
        loadIssueEvent.needMask := "b010".U
        loadIssueEvent.row := issueLsu.row
        loadIssueEvent.column := issueLsu.column
        loadIssueEvent.transpose := issueLsu.transpose
        loadIssueEvent.isAcc := false.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issueSlot.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.LoadC) {
        val regIdx = issueLsu.ms(1, 0)
        val matrixDimM = Mux(issueLsu.transpose, issueLsu.column, issueLsu.row)
        val matrixDimN = Mux(issueLsu.transpose, issueLsu.row, issueLsu.column)
        val nVal = loadBeatCount(matrixDimN, issueLsu.widths)

        io.CML_MicroTask_Config.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := issueLsu.baseAddr
        io.CML_MicroTask_Config.ApplicationTensor_C.ApplicationTensor_C_Stride_M := issueLsu.stride
        io.CML_MicroTask_Config.ApplicationTensor_C.BlockTensor_C_BaseVaddr := issueLsu.baseAddr
        io.CML_MicroTask_Config.ApplicationTensor_C.dataType := loadDataType(issueLsu.widths)
        io.CML_MicroTask_Config.ApplicationTensor_C.HasTail := loadHasTail(matrixDimN, issueLsu.widths)
        io.CML_MicroTask_Config.ApplicationTensor_C.TailByteMask := loadTailByteMask(matrixDimN, issueLsu.widths)
        io.CML_MicroTask_Config.ApplicationTensor_C.N_Beat_Count := nVal
        io.CML_MicroTask_Config.Conherent := true.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := true.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := false.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.CML_MicroTask_Config.MatrixRegTensor_M := matrixDimM
        io.CML_MicroTask_Config.MatrixRegTensor_N := matrixDimN
        io.CML_MicroTask_Config.MatrixRegId := regIdx
        io.CML_MicroTask_Config.Is_Transpose := issueLsu.transpose
        io.CML_MicroTask_Config.LoadMicroTaskValid := true.B
        io.CML_MicroTask_Config.StoreMicroTaskValid := false.B
        if (EnableDifftest) {
          io.CML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.CML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issueSlot.fifoIdx
        loadAllocateEvent.needMask := "b100".U
        loadAllocateEvent.row := issueLsu.row
        loadAllocateEvent.column := issueLsu.column
        loadAllocateEvent.transpose := issueLsu.transpose
        loadAllocateEvent.isAcc := true.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issueSlot.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issueSlot.fifoIdx
        loadIssueEvent.needMask := "b100".U
        loadIssueEvent.row := issueLsu.row
        loadIssueEvent.column := issueLsu.column
        loadIssueEvent.transpose := issueLsu.transpose
        loadIssueEvent.isAcc := true.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issueSlot.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.ZeroAcc) {
        val regIdx = issueArith.md(1, 0)

        io.CML_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
        io.CML_MicroTask_Config.MatrixRegTensor_M := cuteParams.Tensor_MN.U
        io.CML_MicroTask_Config.MatrixRegTensor_N := cuteParams.Tensor_MN.U
        io.CML_MicroTask_Config.MatrixRegId := regIdx
        io.CML_MicroTask_Config.LoadMicroTaskValid := true.B
        io.CML_MicroTask_Config.StoreMicroTaskValid := false.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := true.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := false.B
        io.CML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.CML_MicroTask_Config.Conherent := true.B
        io.CML_MicroTask_Config.Is_Transpose := false.B
        if (EnableDifftest) {
          io.CML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.CML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issueSlot.fifoIdx
        loadAllocateEvent.needMask := "b100".U
        loadAllocateEvent.row := 0.U
        loadAllocateEvent.column := 0.U
        loadAllocateEvent.transpose := false.B
        loadAllocateEvent.isAcc := true.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issueSlot.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issueSlot.fifoIdx
        loadIssueEvent.needMask := "b100".U
        loadIssueEvent.row := 0.U
        loadIssueEvent.column := 0.U
        loadIssueEvent.transpose := false.B
        loadIssueEvent.isAcc := true.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issueSlot.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.ZeroTr) {
        val regIdx = issueArith.md(1, 0)

        io.AML_MicroTask_Config.ApplicationTensor_A.dataType := ElementDataType.DataTypeWidth8
        io.AML_MicroTask_Config.MatrixRegTensor_M := cuteParams.Tensor_MN.U
        io.AML_MicroTask_Config.MatrixRegTensor_K := cuteParams.Tensor_K.U / ReduceWidthByte.U
        io.AML_MicroTask_Config.MatrixRegId := regIdx
        io.AML_MicroTask_Config.MicroTaskValid := true.B
        io.AML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := true.B
        io.AML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := false.B
        io.AML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.AML_MicroTask_Config.Conherent := true.B
        io.AML_MicroTask_Config.Is_Transpose := false.B
        if (EnableDifftest) {
          io.AML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.AML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issueSlot.fifoIdx
        loadAllocateEvent.needMask := "b001".U
        loadAllocateEvent.row := 0.U
        loadAllocateEvent.column := 0.U
        loadAllocateEvent.transpose := false.B
        loadAllocateEvent.isAcc := false.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issueSlot.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issueSlot.fifoIdx
        loadIssueEvent.needMask := "b001".U
        loadIssueEvent.row := 0.U
        loadIssueEvent.column := 0.U
        loadIssueEvent.transpose := false.B
        loadIssueEvent.isAcc := false.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issueSlot.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Compute) {
        val aReg = issueMma.ms1(1, 0)
        val bReg = issueMma.ms2(1, 0)
        val cReg = issueMma.md(1, 0)
        val mVal = issueMma.mtilem
        val nVal = issueMma.mtilen
        val kVal = computeKFromMsew(issueMma.mtilek, issueMma.types1)
        val mmaComputeType = computeTypeForCtrl(issueCtrl)
        val kVectors = mmaKVectors(issueCtrl)
        val workingM = Mux(isScaledCtrl(issueCtrl),
          ((mVal +& (Matrix_MN - 1).U) / Matrix_MN.U) * Matrix_MN.U, mVal)
        val workingN = Mux(isScaledCtrl(issueCtrl),
          ((nVal +& (Matrix_MN - 1).U) / Matrix_MN.U) * Matrix_MN.U, nVal)

        io.ADC_MicroTask_Config.MicroTaskValid := true.B
        io.BDC_MicroTask_Config.MicroTaskValid := true.B
        io.CDC_MicroTask_Config.MicroTaskValid := true.B

        io.ADC_MicroTask_Config.dataType := ElementDataType.DataTypeWidth8
        io.ADC_MicroTask_Config.MatrixRegTensor_M := workingM
        io.ADC_MicroTask_Config.MatrixRegTensor_N := workingN
        io.ADC_MicroTask_Config.MatrixRegTensor_K := kVectors
        io.ADC_MicroTask_Config.MatrixRegId := aReg
        io.ADC_MicroTask_Config.Is_Transpose := false.B

        io.BDC_MicroTask_Config.dataType := ElementDataType.DataTypeWidth8
        io.BDC_MicroTask_Config.MatrixRegTensor_M := workingM
        io.BDC_MicroTask_Config.MatrixRegTensor_N := workingN
        io.BDC_MicroTask_Config.MatrixRegTensor_K := kVectors
        io.BDC_MicroTask_Config.MatrixRegId := bReg
        io.BDC_MicroTask_Config.Is_Transpose := false.B

        io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
        io.CDC_MicroTask_Config.MatrixRegTensor_M := workingM
        io.CDC_MicroTask_Config.MatrixRegTensor_N := workingN
        io.CDC_MicroTask_Config.MatrixRegActive_M := mVal
        io.CDC_MicroTask_Config.MatrixRegActive_N := nVal
        io.CDC_MicroTask_Config.MatrixRegTensor_K := kVectors
        io.CDC_MicroTask_Config.MatrixRegId := cReg
        io.CDC_MicroTask_Config.Is_Transpose := false.B
        io.CDC_MicroTask_Config.Is_AfterOps_Tile := false.B
        if (EnableDifftest) {
          io.CDC_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.CDC_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        io.MTE_MicroTask_Config.MicroTaskValid := true.B
        io.MTE_MicroTask_Config.computeType := mmaComputeType
        when(isScaledCtrl(issueCtrl)) {
          io.ASC_MicroTask_Config.foreach { cfg =>
            cfg.MatrixRegTensor_M := mVal
            cfg.MatrixRegTensor_N := nVal
            cfg.MatrixRegTensor_K := kVectors
            cfg.ScaleBlocksPerRow := scaleBlocks(issueMma.mtilek)
            cfg.KElements := issueMma.mtilek
            cfg.MatrixRegId := aReg
            cfg.ScaleBankId := issueScaleABank
            cfg.computeType := mmaComputeType
            cfg.MicroTaskValid := true.B
          }
          io.BSC_MicroTask_Config.foreach { cfg =>
            cfg.MatrixRegTensor_M := mVal
            cfg.MatrixRegTensor_N := nVal
            cfg.MatrixRegTensor_K := kVectors
            cfg.ScaleBlocksPerRow := scaleBlocks(issueMma.mtilek)
            cfg.KElements := issueMma.mtilek
            cfg.MatrixRegId := bReg
            cfg.ScaleBankId := issueScaleBBank
            cfg.computeType := mmaComputeType
            cfg.MicroTaskValid := true.B
          }
        }

        computeIssueEvent.eventType := 0.U
        computeIssueEvent.aReg := aReg
        computeIssueEvent.bReg := bReg
        computeIssueEvent.cReg := cReg
        computeIssueEvent.fifoIdx := issueSlot.fifoIdx
        computeIssueEvent.mtilem := mVal
        computeIssueEvent.mtilen := nVal
        computeIssueEvent.mtilek := kVal
        computeIssueEvent.isMma := true.B
        computeIssueEvent.isFp := issueMma.isfp
        computeIssueEvent.slotId := issueSlotIdx
        computeIssueEvent.seqId := issueSlot.seqId
        computeIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Store) {
        val regIdx = issueLsu.ms(1, 0)

        io.CML_MicroTask_Config.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := issueLsu.baseAddr
        io.CML_MicroTask_Config.ApplicationTensor_D.ApplicationTensor_D_Stride_M := issueLsu.stride
        io.CML_MicroTask_Config.ApplicationTensor_D.BlockTensor_D_BaseVaddr := issueLsu.baseAddr
        io.CML_MicroTask_Config.ApplicationTensor_D.dataType := loadDataType(issueLsu.widths)
        io.CML_MicroTask_Config.Conherent := true.B
        io.CML_MicroTask_Config.Is_Transpose := issueLsu.transpose
        io.CML_MicroTask_Config.MatrixRegTensor_M := issueLsu.row
        io.CML_MicroTask_Config.MatrixRegTensor_N := issueLsu.column
        io.CML_MicroTask_Config.MatrixRegId := regIdx
        io.CML_MicroTask_Config.LoadMicroTaskValid := false.B
        io.CML_MicroTask_Config.StoreMicroTaskValid := true.B
        if (EnableDifftest) {
          io.CML_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.CML_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        storeIssueEvent.eventType := 0.U
        storeIssueEvent.regId := regIdx
        storeIssueEvent.fifoIdx := issueSlot.fifoIdx
        storeIssueEvent.row := issueLsu.row
        storeIssueEvent.column := issueLsu.column
        storeIssueEvent.transpose := issueLsu.transpose
        storeIssueEvent.isAcc := issueLsu.isacc
        storeIssueEvent.slotId := issueSlotIdx
        storeIssueEvent.seqId := issueSlot.seqId
        storeIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Release) {
        val issueRelease = issueCtrl.data.asTypeOf(new AmuReleaseIO)

        releaseIssueEvent.eventType := 0.U
        releaseIssueEvent.msync := issueRelease.msyncRd
        releaseIssueEvent.slotId := issueSlotIdx
        releaseIssueEvent.seqId := issueSlot.seqId
        releaseIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.NopLike) {
        // NopLike no-op: issue/complete in scheduler only.
      }
    }
  }

  // ===================== State update: done -> retire -> enqueue -> issue =====================
  when(aslDone) {
    val owner = fuASL.ownerSlot
    val bank = scaleA(slots(owner).scaleABank)
    assert(bank.occupied && !bank.valid && bank.generation === slots(owner).scaleAGeneration)
    val success = !dynamicAScaleOwner || io.AML_MicroTask_Config.MicroTaskFlags === 0.U
    bank.valid := success
    bank.occupied := success
    bank.inUse := false.B
    when(dynamicAScaleOwner) {
      assert(aOwnersMatch && amlDone, "dynamic A scale End must join its TR End")
    }
    slots(owner).completed := true.B
    fuASL.busy := false.B
  }
  when(bslDone) {
    val owner = fuBSL.ownerSlot
    val bank = scaleB(slots(owner).scaleBBank)
    assert(bank.occupied && !bank.valid && bank.generation === slots(owner).scaleBGeneration)
    val success = !dynamicBScaleOwner || io.BML_MicroTask_Config.MicroTaskFlags === 0.U
    bank.valid := success
    bank.occupied := success
    bank.inUse := false.B
    when(dynamicBScaleOwner) {
      assert(bOwnersMatch && bmlDone, "dynamic B scale End must join its TR End")
    }
    slots(owner).completed := true.B
    fuBSL.busy := false.B
  }
  when(ascDone) {
    val bank = scaleA(computeOwnerSlot.scaleABank)
    assert(bank.occupied && bank.valid && bank.inUse && bank.generation === computeOwnerSlot.scaleAGeneration,
      "TaskController: ASC must release its exact consumed generation")
    bank.inUse := false.B
    slots(fuCompute.ownerSlot).scaleReadADone := true.B
  }
  when(bscDone) {
    val bank = scaleB(computeOwnerSlot.scaleBBank)
    assert(bank.occupied && bank.valid && bank.inUse && bank.generation === computeOwnerSlot.scaleBGeneration,
      "TaskController: BSC must release its exact consumed generation")
    bank.inUse := false.B
    slots(fuCompute.ownerSlot).scaleReadBDone := true.B
  }
  when(computeFinished) {
    slots(fuCompute.ownerSlot).completed := true.B
    fuCompute.busy := false.B
  }
  when(amlDone) {
    val owner = fuAML.ownerSlot
    assert(slots(owner).valid, "TaskController: AML owner slot invalid on done")
    slots(owner).completed := true.B
    when(isDynamicCtrl(slots(owner).entry.ctrl)) {
      assert(aOwnersMatch && aslDone, "dynamic AML End must join its SF End")
      slots(owner).completionFlags := io.AML_MicroTask_Config.MicroTaskFlags
    }.elsewhen(isConvertCtrl(slots(owner).entry.ctrl)) {
      val bank = scaleA(slots(owner).scaleABank)
      assert(bank.occupied && bank.valid && bank.inUse && bank.generation === slots(owner).scaleAGeneration,
        "TaskController: converting AML must release its exact scale generation")
      bank.inUse := false.B
      slots(owner).scaleReadADone := true.B
      slots(owner).completionFlags := io.AML_MicroTask_Config.MicroTaskFlags
    }
    fuAML.busy := false.B
  }

  when(bmlDone) {
    val owner = fuBML.ownerSlot
    assert(slots(owner).valid, "TaskController: BML owner slot invalid on done")
    slots(owner).completed := true.B
    when(isDynamicCtrl(slots(owner).entry.ctrl)) {
      assert(bOwnersMatch && bslDone, "dynamic BML End must join its SF End")
      slots(owner).completionFlags := io.BML_MicroTask_Config.MicroTaskFlags
    }.elsewhen(isConvertCtrl(slots(owner).entry.ctrl)) {
      val bank = scaleB(slots(owner).scaleBBank)
      assert(bank.occupied && bank.valid && bank.inUse && bank.generation === slots(owner).scaleBGeneration,
        "TaskController: converting BML must release its exact scale generation")
      bank.inUse := false.B
      slots(owner).scaleReadBDone := true.B
      slots(owner).completionFlags := io.BML_MicroTask_Config.MicroTaskFlags
    }
    fuBML.busy := false.B
  }

  when(cmlLoadDone) {
    val owner = fuCMLLoad.ownerSlot
    assert(slots(owner).valid, "TaskController: CML-load owner slot invalid on done")
    slots(owner).completed := true.B
    fuCMLLoad.busy := false.B
  }

  when(cmlStoreDone) {
    val owner = fuCMLStore.ownerSlot
    assert(slots(owner).valid, "TaskController: CML-store owner slot invalid on done")
    slots(owner).completed := true.B
    fuCMLStore.busy := false.B
  }

  when(adcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: ADC owner slot invalid on done")
    assert(!slots(owner).readADone, "TaskController: duplicated ADC done")
    slots(owner).readADone := true.B

    val slot = slots(owner)
    val mma = decodeMma(slot.entry.ctrl)
    computeReadAFinishEvent.eventType := 1.U
    computeReadAFinishEvent.aReg := mma.ms1(1, 0)
    computeReadAFinishEvent.bReg := mma.ms2(1, 0)
    computeReadAFinishEvent.cReg := mma.md(1, 0)
    computeReadAFinishEvent.fifoIdx := slot.fifoIdx
    computeReadAFinishEvent.mtilem := mma.mtilem
    computeReadAFinishEvent.mtilen := mma.mtilen
    computeReadAFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeReadAFinishEvent.isMma := true.B
    computeReadAFinishEvent.isFp := mma.isfp
    computeReadAFinishEvent.slotId := owner
    computeReadAFinishEvent.seqId := slot.seqId
    computeReadAFinishEventEn := true.B
  }

  when(bdcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: BDC owner slot invalid on done")
    assert(!slots(owner).readBDone, "TaskController: duplicated BDC done")
    slots(owner).readBDone := true.B

    val slot = slots(owner)
    val mma = decodeMma(slot.entry.ctrl)
    computeReadBFinishEvent.eventType := 2.U
    computeReadBFinishEvent.aReg := mma.ms1(1, 0)
    computeReadBFinishEvent.bReg := mma.ms2(1, 0)
    computeReadBFinishEvent.cReg := mma.md(1, 0)
    computeReadBFinishEvent.fifoIdx := slot.fifoIdx
    computeReadBFinishEvent.mtilem := mma.mtilem
    computeReadBFinishEvent.mtilen := mma.mtilen
    computeReadBFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeReadBFinishEvent.isMma := true.B
    computeReadBFinishEvent.isFp := mma.isfp
    computeReadBFinishEvent.slotId := owner
    computeReadBFinishEvent.seqId := slot.seqId
    computeReadBFinishEventEn := true.B
  }

  when(cdcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: CDC owner slot invalid on done")
    slots(owner).writeCDone := true.B
    slots(owner).completionFlags := io.CDC_MicroTask_Config.MicroTaskFlags

    val slot = slots(owner)
    val mma = decodeMma(slot.entry.ctrl)
    computeWriteCFinishEvent.eventType := 3.U
    computeWriteCFinishEvent.aReg := mma.ms1(1, 0)
    computeWriteCFinishEvent.bReg := mma.ms2(1, 0)
    computeWriteCFinishEvent.cReg := mma.md(1, 0)
    computeWriteCFinishEvent.fifoIdx := slot.fifoIdx
    computeWriteCFinishEvent.mtilem := mma.mtilem
    computeWriteCFinishEvent.mtilen := mma.mtilen
    computeWriteCFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeWriteCFinishEvent.isMma := true.B
    computeWriteCFinishEvent.isFp := mma.isfp
    computeWriteCFinishEvent.slotId := owner
    computeWriteCFinishEvent.seqId := slot.seqId
    computeWriteCFinishEventEn := true.B
  }

  when(cdcDone) {
    val owner = fuCompute.ownerSlot
    // CDC can legally arrive in the same cycle as ADC/BDC.
    assert(slots(owner).readADone || adcDone, "TaskController: CDC done before ADC done")
    assert(slots(owner).readBDone || bdcDone, "TaskController: CDC done before BDC done")
  }

  when(retireFire) {
    val retireOH = slotOH(winHead)

    for (i <- 0 until WinDepth) {
      when(slots(i).valid) {
        slots(i).waitCompleteMask := slots(i).waitCompleteMask & (~retireOH)(WinDepth - 1, 0)
        slots(i).waitReadAMask := slots(i).waitReadAMask & (~retireOH)(WinDepth - 1, 0)
        slots(i).waitReadBMask := slots(i).waitReadBMask & (~retireOH)(WinDepth - 1, 0)
        slots(i).waitScaleReadAMask := slots(i).waitScaleReadAMask & (~retireOH)(WinDepth - 1, 0)
        slots(i).waitScaleReadBMask := slots(i).waitScaleReadBMask & (~retireOH)(WinDepth - 1, 0)
      }
    }

    slots(winHead).valid := false.B
    slots(winHead).issued := false.B
    slots(winHead).completed := false.B
    slots(winHead).readADone := false.B
    slots(winHead).readBDone := false.B
    slots(winHead).scaleReadADone := false.B
    slots(winHead).scaleReadBDone := false.B
    slots(winHead).writeCDone := false.B
    slots(winHead).completionFlags := 0.U
    slots(winHead).waitScaleReadAMask := 0.U
    slots(winHead).waitScaleReadBMask := 0.U
    slots(winHead).waitCompleteMask := 0.U
    slots(winHead).waitReadAMask := 0.U
    slots(winHead).waitReadBMask := 0.U
    slots(winHead).opKind := TaskCtrlOpKind.NopLike
    slots(winHead).entry := 0.U.asTypeOf(new DecodedAmuCtrlEntry)
    slots(winHead).seqId := 0.U
    slots(winHead).fifoIdx := 0.U
  }

  when(enqueueFire) {
    val newSlot = WireInit(0.U.asTypeOf(new IssueWindowSlot))
    newSlot.valid := true.B
    newSlot.issued := false.B
    newSlot.completed := false.B
    newSlot.readADone := false.B
    newSlot.readBDone := false.B
    newSlot.waitCompleteMask := 0.U
    newSlot.waitReadAMask := 0.U
    newSlot.waitReadBMask := 0.U
    newSlot.opKind := deqOpKind
    newSlot.entry := deqEntry
    newSlot.seqId := seqIdAlloc
    newSlot.fifoIdx := 0.U
    newSlot.scaleAGeneration := latestScaleA(abIdx(scaleReaderReg(deqCtrl, a = true)))
    newSlot.scaleBGeneration := latestScaleB(abIdx(scaleReaderReg(deqCtrl, a = false)))
    when(writesScale(deqOpKind, deqCtrl, a = true)) {
      val next = latestScaleA(abIdx(deqLsu.ms)) + 1.U
      assert(next =/= 0.U, "TaskController: scale generation exhausted; reset before reuse")
      latestScaleA(abIdx(deqLsu.ms)) := next
      newSlot.scaleAGeneration := next
    }
    when(writesScale(deqOpKind, deqCtrl, a = false)) {
      val next = latestScaleB(abIdx(deqLsu.ms)) + 1.U
      assert(next =/= 0.U, "TaskController: scale generation exhausted; reset before reuse")
      latestScaleB(abIdx(deqLsu.ms)) := next
      newSlot.scaleBGeneration := next
    }

    when(
      deqOpKind === TaskCtrlOpKind.LoadA ||
      deqOpKind === TaskCtrlOpKind.LoadB ||
      deqOpKind === TaskCtrlOpKind.LoadC ||
      deqOpKind === TaskCtrlOpKind.ZeroAcc ||
      deqOpKind === TaskCtrlOpKind.ZeroTr
    ) {
      newSlot.fifoIdx := loadFifoIdxAlloc
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Compute) {
      newSlot.fifoIdx := computeIssueIdx
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Store) {
      newSlot.fifoIdx := storeIssueIdx
    }

    val newReadsAB = deqOpKind === TaskCtrlOpKind.Compute || (deqOpKind === TaskCtrlOpKind.Store && deqStoreReadsAB)
    val newReadsC = deqOpKind === TaskCtrlOpKind.Compute || (deqOpKind === TaskCtrlOpKind.Store && deqStoreReadsC)
    val newWritesAB =
      deqOpKind === TaskCtrlOpKind.LoadA ||
      deqOpKind === TaskCtrlOpKind.LoadB ||
      deqOpKind === TaskCtrlOpKind.ZeroTr
    val newWritesC =
      deqOpKind === TaskCtrlOpKind.LoadC ||
      deqOpKind === TaskCtrlOpKind.ZeroAcc ||
      deqOpKind === TaskCtrlOpKind.Compute

    val depCompleteBits = Wire(Vec(WinDepth, Bool()))
    val depReadABits = Wire(Vec(WinDepth, Bool()))
    val depReadBBits = Wire(Vec(WinDepth, Bool()))
    val depScaleReadABits = Wire(Vec(WinDepth, Bool()))
    val depScaleReadBBits = Wire(Vec(WinDepth, Bool()))

    // Build dependencies only against strictly older in-window slots (ring age from head).
    // Age base is post-retire view because state update priority is done -> retire -> enqueue -> issue.
    val depOlderMask = Wire(Vec(WinDepth, Bool()))
    depOlderMask.foreach(_ := false.B)
    val depHead = Mux(retireFire, (winHead + 1.U)(SlotIdxWidth - 1, 0), winHead)
    val depCount = winCount - retireFire.asUInt
    for (age <- 0 until WinDepth) {
      val idx = (depHead + age.U)(SlotIdxWidth - 1, 0)
      when(age.U < depCount) {
        depOlderMask(idx) := true.B
      }
    }

    for (j <- 0 until WinDepth) {
      val older = slots(j)
      val olderValid = older.valid && depOlderMask(j)
      val olderCtrl = older.entry.ctrl
      val olderMma = decodeMma(olderCtrl)
      val olderLsu = decodeLsu(olderCtrl)
      val olderArith = decodeArith(olderCtrl)

      val olderIsStore = older.opKind === TaskCtrlOpKind.Store
      val olderStoreReadsAB = olderIsStore && !olderLsu.isacc
      val olderStoreReadsC = olderIsStore && olderLsu.isacc

      val olderReadABHit =
        isReadAB(older.opKind, olderMma, olderLsu, deqEntry.writeRegs(0), olderStoreReadsAB)
      val olderReadCHit =
        isReadC(older.opKind, olderMma, olderLsu, deqEntry.writeRegs(0), olderStoreReadsC)

      val olderWriteABHitForNewRead =
        (deqEntry.readValid(0) && isWriteAB(older.opKind, olderLsu, olderArith, deqEntry.readRegs(0))) ||
        (deqEntry.readValid(1) && isWriteAB(older.opKind, olderLsu, olderArith, deqEntry.readRegs(1))) ||
        (deqEntry.readValid(2) && isWriteAB(older.opKind, olderLsu, olderArith, deqEntry.readRegs(2)))
      val olderWriteCHitForNewRead =
        (deqEntry.readValid(0) && isWriteC(older.opKind, olderMma, olderLsu, olderArith, deqEntry.readRegs(0))) ||
        (deqEntry.readValid(1) && isWriteC(older.opKind, olderMma, olderLsu, olderArith, deqEntry.readRegs(1))) ||
        (deqEntry.readValid(2) && isWriteC(older.opKind, olderMma, olderLsu, olderArith, deqEntry.readRegs(2)))
      val olderWriteABHitForNewWrite = deqEntry.writeValid(0) && isWriteAB(older.opKind, olderLsu, olderArith, deqEntry.writeRegs(0))
      val olderWriteCHitForNewWrite = deqEntry.writeValid(0) && isWriteC(older.opKind, olderMma, olderLsu, olderArith, deqEntry.writeRegs(0))

      val olderWritesAB =
        older.opKind === TaskCtrlOpKind.LoadA ||
        older.opKind === TaskCtrlOpKind.LoadB ||
        older.opKind === TaskCtrlOpKind.ZeroTr
      val olderWritesC =
        older.opKind === TaskCtrlOpKind.LoadC ||
        older.opKind === TaskCtrlOpKind.ZeroAcc ||
        older.opKind === TaskCtrlOpKind.Compute

      val olderReadsABByComputeA = isReadAOfCompute(older.opKind, olderMma, deqEntry.writeRegs(0))
      val olderReadsABByComputeB = isReadBOfCompute(older.opKind, olderMma, deqEntry.writeRegs(0))
      val newScaleA = writesScale(deqOpKind, deqCtrl, a = true)
      val newScaleB = writesScale(deqOpKind, deqCtrl, a = false)
      val olderScaleA = writesScale(older.opKind, olderCtrl, a = true)
      val olderScaleB = writesScale(older.opKind, olderCtrl, a = false)
      val scaleRaw =
        (readsScale(deqCtrl, a = true) && olderScaleA &&
          abIdx(olderLsu.ms) === abIdx(scaleReaderReg(deqCtrl, a = true))) ||
        (readsScale(deqCtrl, a = false) && olderScaleB &&
          abIdx(olderLsu.ms) === abIdx(scaleReaderReg(deqCtrl, a = false)))
      val scaleWaw = (newScaleA && olderScaleA || newScaleB && olderScaleB) &&
        abIdx(olderLsu.ms) === abIdx(deqLsu.ms)

      val depCompleteJ = Mux(
        deqOpKind === TaskCtrlOpKind.Release,
        olderValid && (older.opKind === TaskCtrlOpKind.Store ||
          isFlagProducer(older.opKind, olderCtrl)),
        olderValid && (
          (olderWritesAB && newReadsAB && olderWriteABHitForNewRead) ||
          (olderWritesC && newReadsC && olderWriteCHitForNewRead) ||
          (olderWritesAB && newWritesAB && olderWriteABHitForNewWrite) ||
          (olderWritesC && newWritesC && olderWriteCHitForNewWrite) ||
          (newWritesAB && olderReadABHit && !olderReadsABByComputeA && !olderReadsABByComputeB) ||
          (newWritesC && olderReadCHit) || scaleRaw || scaleWaw
        )
      )

      depCompleteBits(j) := depCompleteJ
      depReadABits(j) := (deqOpKind =/= TaskCtrlOpKind.Release) && olderValid && newWritesAB && olderReadsABByComputeA
      depReadBBits(j) := (deqOpKind =/= TaskCtrlOpKind.Release) && olderValid && newWritesAB && olderReadsABByComputeB
      depScaleReadABits(j) := olderValid && newScaleA && readsScale(olderCtrl, a = true) &&
        abIdx(scaleReaderReg(olderCtrl, a = true)) === abIdx(deqLsu.ms)
      depScaleReadBBits(j) := olderValid && newScaleB && readsScale(olderCtrl, a = false) &&
        abIdx(scaleReaderReg(olderCtrl, a = false)) === abIdx(deqLsu.ms)
    }

    newSlot.waitCompleteMask := depCompleteBits.asUInt
    newSlot.waitReadAMask := depReadABits.asUInt
    newSlot.waitReadBMask := depReadBBits.asUInt
    newSlot.waitScaleReadAMask := depScaleReadABits.asUInt
    newSlot.waitScaleReadBMask := depScaleReadBBits.asUInt

    slots(enqueueSlotIdx) := newSlot
  }

  when(issueFire) {
    slots(issueSlotIdx).issued := true.B

    switch(issueSlot.opKind) {
      is(TaskCtrlOpKind.LoadScaleA) {
        fuASL.busy := true.B
        fuASL.ownerSlot := issueSlotIdx
        slots(issueSlotIdx).scaleABank := allocScaleABank
        val bank = scaleA(allocScaleABank)
        bank.occupied := true.B
        bank.valid := false.B
        bank.inUse := false.B
        bank.regId := abIdx(issueLsu.ms)
        bank.generation := issueSlot.scaleAGeneration
        bank.computeType := scaleLoadMode
        bank.rows := scaleLoadRows
        bank.kVectors := scaleLoadK
        bank.scaleBlocks := scaleLoadBlocks
      }
      is(TaskCtrlOpKind.LoadScaleB) {
        fuBSL.busy := true.B
        fuBSL.ownerSlot := issueSlotIdx
        slots(issueSlotIdx).scaleBBank := allocScaleBBank
        val bank = scaleB(allocScaleBBank)
        bank.occupied := true.B
        bank.valid := false.B
        bank.inUse := false.B
        bank.regId := abIdx(issueLsu.ms)
        bank.generation := issueSlot.scaleBGeneration
        bank.computeType := scaleLoadMode
        bank.rows := scaleLoadRows
        bank.kVectors := scaleLoadK
        bank.scaleBlocks := scaleLoadBlocks
      }
      is(TaskCtrlOpKind.LoadA) {
        fuAML.busy := true.B
        fuAML.ownerSlot := issueSlotIdx
        when(isDynamicCtrl(issueCtrl)) {
          fuASL.busy := true.B
          fuASL.ownerSlot := issueSlotIdx
          slots(issueSlotIdx).scaleABank := allocScaleABank
          val bank = scaleA(allocScaleABank)
          bank.occupied := true.B
          bank.valid := false.B
          bank.inUse := false.B
          bank.regId := abIdx(issueLsu.ms)
          bank.generation := issueSlot.scaleAGeneration
          bank.computeType := MteComputeType.Mxfp4F32
          bank.rows := 8.U
          bank.kVectors := 1.U
          bank.scaleBlocks := 2.U
        }.elsewhen(isConvertCtrl(issueCtrl)) {
          slots(issueSlotIdx).scaleABank := issueScaleABank
          scaleA(issueScaleABank).inUse := true.B
        }
      }
      is(TaskCtrlOpKind.LoadB) {
        fuBML.busy := true.B
        fuBML.ownerSlot := issueSlotIdx
        when(isDynamicCtrl(issueCtrl)) {
          fuBSL.busy := true.B
          fuBSL.ownerSlot := issueSlotIdx
          slots(issueSlotIdx).scaleBBank := allocScaleBBank
          val bank = scaleB(allocScaleBBank)
          bank.occupied := true.B
          bank.valid := false.B
          bank.inUse := false.B
          bank.regId := abIdx(issueLsu.ms)
          bank.generation := issueSlot.scaleBGeneration
          bank.computeType := MteComputeType.Mxfp4F32
          bank.rows := 8.U
          bank.kVectors := 1.U
          bank.scaleBlocks := 2.U
        }.elsewhen(isConvertCtrl(issueCtrl)) {
          slots(issueSlotIdx).scaleBBank := issueScaleBBank
          scaleB(issueScaleBBank).inUse := true.B
        }
      }
      is(TaskCtrlOpKind.LoadC) {
        fuCMLLoad.busy := true.B
        fuCMLLoad.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.ZeroAcc) {
        fuCMLLoad.busy := true.B
        fuCMLLoad.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.ZeroTr) {
        fuAML.busy := true.B
        fuAML.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.Compute) {
        fuCompute.busy := true.B
        fuCompute.ownerSlot := issueSlotIdx
        when(isScaledCtrl(issueCtrl)) {
          slots(issueSlotIdx).scaleABank := issueScaleABank
          slots(issueSlotIdx).scaleBBank := issueScaleBBank
          scaleA(issueScaleABank).inUse := true.B
          scaleB(issueScaleBBank).inUse := true.B
        }
      }
      is(TaskCtrlOpKind.Store) {
        fuCMLStore.busy := true.B
        fuCMLStore.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.Release) {
        slots(issueSlotIdx).completed := true.B
      }
      is(TaskCtrlOpKind.NopLike) {
        slots(issueSlotIdx).completed := true.B
      }
    }
  }

  // Pointer and count updates (retire + enqueue)
  // Last-connect priority over normal generation/ownership updates. No SRAM,
  // matrix data, FIFO or FU reset: real completion must precede clear.
  when(io.ygjkctrl.minitClear) {
    assert(io.ygjkctrl.resetReady, "MINIT clear before all old CUTE work drained")
    scaleA.foreach(_ := 0.U.asTypeOf(new ScaleBankState))
    scaleB.foreach(_ := 0.U.asTypeOf(new ScaleBankState))
    latestScaleA.foreach(_ := 0.U)
    latestScaleB.foreach(_ := 0.U)
  }

  when(retireFire && !enqueueFire) {
    winHead := (winHead + 1.U)(SlotIdxWidth - 1, 0)
    winCount := winCount - 1.U
  }.elsewhen(!retireFire && enqueueFire) {
    winTail := (winTail + 1.U)(SlotIdxWidth - 1, 0)
    winCount := winCount + 1.U
  }.elsewhen(retireFire && enqueueFire) {
    val nextHead = (winHead + 1.U)(SlotIdxWidth - 1, 0)
    when(windowFull) {
      // retire slot is immediately reused by enqueue; head/tail advance together by one.
      winHead := nextHead
      winTail := nextHead
      winCount := winCount
    }.otherwise {
      winHead := nextHead
      winTail := (winTail + 1.U)(SlotIdxWidth - 1, 0)
      winCount := winCount
    }
  }

  when(enqueueFire) {
    seqIdAlloc := seqIdAlloc + 1.U
    when(
      deqOpKind === TaskCtrlOpKind.LoadA ||
      deqOpKind === TaskCtrlOpKind.LoadB ||
      deqOpKind === TaskCtrlOpKind.LoadC ||
      deqOpKind === TaskCtrlOpKind.ZeroAcc ||
      deqOpKind === TaskCtrlOpKind.ZeroTr
    ) {
      loadFifoIdxAlloc := loadFifoIdxAlloc + 1.U
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Compute) {
      computeIssueIdx := computeIssueIdx + 1.U
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Store) {
      storeIssueIdx := storeIssueIdx + 1.U
    }
  }

  // ===================== Finish events (load/store) =====================
  when(amlDone) {
    val owner = fuAML.ownerSlot
    val slot = slots(owner)
    val op = slot.opKind
    val lsu = decodeLsu(slot.entry.ctrl)
    val arith = decodeArith(slot.entry.ctrl)
    val reg = Mux(op === TaskCtrlOpKind.ZeroTr, arith.md(1, 0), lsu.ms(1, 0))

    loadAFinishEvent.eventType := 2.U
    loadAFinishEvent.regId := reg
    loadAFinishEvent.fifoIdx := slot.fifoIdx
    loadAFinishEvent.needMask := "b001".U
    loadAFinishEvent.row := Mux(op === TaskCtrlOpKind.ZeroTr, 0.U, lsu.row)
    loadAFinishEvent.column := Mux(op === TaskCtrlOpKind.ZeroTr, 0.U, lsu.column)
    loadAFinishEvent.transpose := Mux(op === TaskCtrlOpKind.ZeroTr, false.B, lsu.transpose)
    loadAFinishEvent.isAcc := false.B
    loadAFinishEvent.slotId := owner
    loadAFinishEvent.seqId := slot.seqId
    loadAFinishEventEn := true.B
  }

  when(bmlDone) {
    val owner = fuBML.ownerSlot
    val slot = slots(owner)
    val lsu = decodeLsu(slot.entry.ctrl)

    loadBFinishEvent.eventType := 2.U
    loadBFinishEvent.regId := lsu.ms(1, 0)
    loadBFinishEvent.fifoIdx := slot.fifoIdx
    loadBFinishEvent.needMask := "b010".U
    loadBFinishEvent.row := lsu.row
    loadBFinishEvent.column := lsu.column
    loadBFinishEvent.transpose := lsu.transpose
    loadBFinishEvent.isAcc := false.B
    loadBFinishEvent.slotId := owner
    loadBFinishEvent.seqId := slot.seqId
    loadBFinishEventEn := true.B
  }

  when(cmlLoadDone) {
    val owner = fuCMLLoad.ownerSlot
    val slot = slots(owner)
    val op = slot.opKind
    val lsu = decodeLsu(slot.entry.ctrl)
    val arith = decodeArith(slot.entry.ctrl)
    val reg = Mux(op === TaskCtrlOpKind.ZeroAcc, arith.md(1, 0), lsu.ms(1, 0))
    val row = Mux(op === TaskCtrlOpKind.ZeroAcc, 0.U, lsu.row)
    val col = Mux(op === TaskCtrlOpKind.ZeroAcc, 0.U, lsu.column)

    loadCFinishEvent.eventType := 2.U
    loadCFinishEvent.regId := reg
    loadCFinishEvent.fifoIdx := slot.fifoIdx
    loadCFinishEvent.needMask := "b100".U
    loadCFinishEvent.row := row
    loadCFinishEvent.column := col
    loadCFinishEvent.transpose := Mux(op === TaskCtrlOpKind.ZeroAcc, false.B, lsu.transpose)
    loadCFinishEvent.isAcc := true.B
    loadCFinishEvent.slotId := owner
    loadCFinishEvent.seqId := slot.seqId
    loadCFinishEventEn := true.B
  }

  when(cmlStoreDone) {
    val owner = fuCMLStore.ownerSlot
    val slot = slots(owner)
    val lsu = decodeLsu(slot.entry.ctrl)

    storeFinishEvent.eventType := 1.U
    storeFinishEvent.regId := lsu.ms(1, 0)
    storeFinishEvent.fifoIdx := slot.fifoIdx
    storeFinishEvent.row := lsu.row
    storeFinishEvent.column := lsu.column
    storeFinishEvent.transpose := lsu.transpose
    storeFinishEvent.isAcc := lsu.isacc
    storeFinishEvent.slotId := owner
    storeFinishEvent.seqId := slot.seqId
    storeFinishEventEn := true.B
  }

  // ===================== Release DiffTest alignment =====================
  if (EnableDifftest) {
    val releaseFinish = DifftestModule(new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 3, dontCare = true)
    releaseFinish.clearScaleTrace()
    val releaseIssueOwnerSlot = issueSlotIdx
    val releaseIssueOwnerEntry = issueSlot.entry

    releaseFinish.coreid := Mux(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release, releaseIssueOwnerEntry.ctrl.coreid.get, 0.U)
    releaseFinish.index := 4.U
    releaseFinish.valid := issueFire && issueSlot.opKind === TaskCtrlOpKind.Release
    releaseFinish.pc := Mux(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release, releaseIssueOwnerEntry.ctrl.pc.get, 0.U)
    releaseFinish.retireId := releaseIssueOwnerEntry.ctrl.retireId.get
    releaseFinish.epoch := releaseIssueOwnerEntry.ctrl.epoch.get
    releaseFinish.bankValid.foreach(_ := false.B)
    releaseFinish.bankAddr.foreach(_ := 0.U)
    releaseFinish.bankMask.foreach(_ := 0.U)
    releaseFinish.data.foreach(_ := 0.U)
    releaseFinish.finish := issueFire && issueSlot.opKind === TaskCtrlOpKind.Release

    when(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release) {
      dontTouch(releaseIssueOwnerSlot)
    }
  }

  if (EnableDifftest && cuteMatrixExtension.enableScalingFactor) {
    def describeMma(event: DiffAmuFinishEvent, ctrl: AmuCtrlIO,
                    slot: IssueWindowSlot, aBank: UInt, bBank: UInt): Unit = {
      val mma = decodeMma(ctrl)
      event := 0.U.asTypeOf(event)
      event.coreid := ctrl.coreid.get
      event.pc := ctrl.pc.get
      event.retireId := ctrl.retireId.get
      event.epoch := ctrl.epoch.get
      event.computeType := computeTypeForCtrl(ctrl)
      event.scaleRegA := mma.ms1
      event.scaleRegB := mma.ms2
      event.scaleBankA := aBank
      event.scaleBankB := bBank
      event.scaleGenA := slot.scaleAGeneration
      event.scaleGenB := slot.scaleBGeneration
      event.scaleRowsA := mma.mtilem
      event.scaleRowsB := mma.mtilen
      event.scaleKVectorsA := mmaKVectors(ctrl)
      event.scaleKVectorsB := mmaKVectors(ctrl)
      event.scaleBlocksA := scaleBlocks(mma.mtilek)
      event.scaleBlocksB := scaleBlocks(mma.mtilek)
      event.scaleRowPitchA := scaleBlocks(mma.mtilek)
      event.scaleRowPitchB := scaleBlocks(mma.mtilek)
    }
    val bind = DifftestModule(
      new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 0, dontCare = true)
    describeMma(bind, issueCtrl, issueSlot, issueScaleABank, issueScaleBBank)
    bind.index := 8.U
    bind.kind := AmuFinishKind.MmaBind.U
    bind.valid := issueFire && issueSlot.opKind === TaskCtrlOpKind.Compute && isScaledCtrl(issueCtrl)
    bind.scaleValidA := scaleA(issueScaleABank).valid
    bind.scaleValidB := scaleB(issueScaleBBank).valid
    val completed = DifftestModule(
      new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 0, dontCare = true)
    // A reload after reader end can replace bank metadata before CDC ends.
    // Use the immutable compute slot's selected banks/generations, not live banks.
    describeMma(completed, computeOwnerSlot.entry.ctrl, computeOwnerSlot,
      computeOwnerSlot.scaleABank, computeOwnerSlot.scaleBBank)
    completed.index := 9.U
    completed.kind := AmuFinishKind.MmaDone.U
    completed.valid := computeFinished && computeOwnerScaled
    completed.finish := completed.valid
    completed.scaleValidA := true.B
    completed.scaleValidB := true.B
    completed.flagsValid := completed.valid
    completed.flagsRaised := Mux(cdcDone, io.CDC_MicroTask_Config.MicroTaskFlags,
      computeOwnerSlot.completionFlags)
    io.mmaTrace.get(0) := bind
    io.mmaTrace.get(1) := completed

    val retired = DifftestModule(
      new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 0, dontCare = true)
    retired := 0.U.asTypeOf(retired)
    retired.index := 10.U
    retired.kind := AmuFinishKind.OrderedRetire.U
    retired.valid := retireFire && headReturnsFlags &&
      (isScaledCtrl(headSlot.entry.ctrl) || isConvertCtrl(headSlot.entry.ctrl))
    retired.finish := retired.valid
    retired.coreid := headSlot.entry.ctrl.coreid.get
    retired.pc := headSlot.entry.ctrl.pc.get
    retired.retireId := headSlot.entry.ctrl.retireId.get
    retired.epoch := headSlot.entry.ctrl.epoch.get
    retired.computeType := computeTypeForCtrl(headSlot.entry.ctrl)
    retired.flagsValid := retired.valid
    retired.flagsRaised := completionFlagsNext

    val accepted = DifftestModule(
      new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 0, dontCare = true)
    accepted := 0.U.asTypeOf(accepted)
    accepted.index := 11.U
    accepted.kind := AmuFinishKind.ReturnAccepted.U
    val queued = returnQ.io.deq.bits
    val queuedScaled = Seq(MteComputeType.Mxfp8e4m3F32, MteComputeType.Mxfp8e5m2F32,
      MteComputeType.Mxfp4F32).map(queued.traceComputeType.get === _).reduce(_ || _)
    accepted.valid := returnQ.io.deq.fire && queued.flagsValid && queuedScaled
    accepted.finish := accepted.valid
    accepted.coreid := queued.traceCoreid.get
    accepted.pc := queued.tracePc.get
    accepted.retireId := queued.traceRetireId.get
    accepted.epoch := queued.traceEpoch.get
    accepted.computeType := queued.traceComputeType.get
    accepted.flagsValid := accepted.valid
    accepted.flagsRaised := queued.flags
    io.retirementTrace.get(0) := retired
    io.retirementTrace.get(1) := accepted
  }

  // ===================== ChiselDB log commit =====================
  loadEventTable.log(loadAllocateEvent, loadAllocateEventEn, "LoadAllocate", clock, reset)
  loadEventTable.log(loadIssueEvent, loadIssueEventEn, "LoadIssue", clock, reset)
  loadEventTable.log(loadAFinishEvent, loadAFinishEventEn, "LoadFinish", clock, reset)
  loadEventTable.log(loadBFinishEvent, loadBFinishEventEn, "LoadFinish", clock, reset)
  loadEventTable.log(loadCFinishEvent, loadCFinishEventEn, "LoadFinish", clock, reset)

  computeEventTable.log(computeIssueEvent, computeIssueEventEn, "ComputeIssue", clock, reset)
  computeEventTable.log(computeReadAFinishEvent, computeReadAFinishEventEn, "ComputeReadAFinish", clock, reset)
  computeEventTable.log(computeReadBFinishEvent, computeReadBFinishEventEn, "ComputeReadBFinish", clock, reset)
  computeEventTable.log(computeWriteCFinishEvent, computeWriteCFinishEventEn, "ComputeWriteCFinish", clock, reset)

  storeEventTable.log(storeIssueEvent, storeIssueEventEn, "StoreIssue", clock, reset)
  storeEventTable.log(storeFinishEvent, storeFinishEventEn, "StoreFinish", clock, reset)

  releaseEventTable.log(releaseIssueEvent, releaseIssueEventEn, "ReleaseIssue", clock, reset)

  val mmaDoneType = decodeMmaComputeType(decodeMma(slots(fuCompute.ownerSlot).entry.ctrl))
  val releaseDone = issueFire && issueSlot.opKind === TaskCtrlOpKind.Release
  io.perfProbe.ownedWork := ownedWork
  io.perfProbe.retire := retireFire
  io.perfProbe.loadADone := loadAFinishEventEn
  io.perfProbe.loadBDone := loadBFinishEventEn
  io.perfProbe.loadCDone := loadCFinishEventEn
  io.perfProbe.storeDone := storeFinishEventEn
  io.perfProbe.compDone := computeWriteCFinishEventEn
  io.perfProbe.releaseDone := releaseDone
  io.perfProbe.mmaNonfpDone := computeWriteCFinishEventEn && !decodeMma(slots(fuCompute.ownerSlot).entry.ctrl).isfp
  io.perfProbe.mmaFp16Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.F16F16F32)
  io.perfProbe.mmaBf16Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.BF16BF16F32)
  io.perfProbe.mmaTf32Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.TF32TF32F32)
  io.perfProbe.amlActive := fuAML.busy
  io.perfProbe.bmlActive := fuBML.busy
  io.perfProbe.cmlLoadActive := fuCMLLoad.busy
  io.perfProbe.mteActive := fuCompute.busy
  io.perfProbe.cmlStoreActive := fuCMLStore.busy
}
