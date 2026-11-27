package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util._
import cute.Bundles._
import difftest._
import utility.ChiselDB
import xscache.coupledL2.prefetch.{
  MatrixPrefetchControl,
  MatrixPrefetchDesc,
  MatrixPrefetchStream,
  MatrixPrefetchTagCodec
}

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
  val trLoader0Config = new TrLoadTaskConfigIO
  val trLoader1Config = new TrLoadTaskConfigIO
  val ASL_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new ASLMicroTaskConfigIO)
  val BSL_MicroTask_Config = Option.when(cuteMatrixExtension.enableScalingFactor)(new BSLMicroTaskConfigIO)
  val accMemoryConfig = new AccMemoryTaskConfigIO
  val trStorer0 = new TrStoreControlIO
  val MTE_MicroTask_Config = new MTEMicroTaskConfigIO
  val DebugTimeStampe = Input(UInt(32.W))
  val perfProbe = Output(new TaskControllerPerfProbe)
  val matrixPrefetch = Option.when(EnableMatrixPrefetch)(Output(new MatrixPrefetchControl))
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
  val NopLike, LoadA, LoadB, LoadC, Compute, Store, Release, ZeroAcc, ZeroTr = Value
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

  // ===================== Default output assignments =====================
  io.trStorer0.command.valid := false.B
  io.trStorer0.command.bits := 0.U.asTypeOf(io.trStorer0.command.bits)
  io.trStorer0.done.ready := false.B
  io.ygjkctrl.mrelease.valid := false.B
  io.ygjkctrl.mrelease.bits := 0.U.asTypeOf(new MreleaseIO)

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

  for (cfg <- Seq(io.trLoader0Config, io.trLoader1Config)) {
    cfg.Tensor := 0.U.asTypeOf(cfg.Tensor)
    cfg.LoadTaskInfo := 0.U.asTypeOf(cfg.LoadTaskInfo)
    cfg.MatrixRegTensor_M := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.MatrixRegId := 0.U
    cfg.Conherent := false.B
    cfg.Is_Transpose := false.B
    cfg.PrefetchTaskId.foreach(_ := 0.U)
    cfg.PrefetchStream.foreach(_ := MatrixPrefetchStream.none)
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
    cfg.pc.foreach(_ := 0.U)
    cfg.coreid.foreach(_ := 0.U)
  }

  io.ASL_MicroTask_Config.foreach { cfg =>
    cfg.ApplicationScale_A := 0.U.asTypeOf(cfg.ApplicationScale_A)
    cfg.MatrixRegTensor_M := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.Conherent := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.BSL_MicroTask_Config.foreach { cfg =>
    cfg.ApplicationScale_B := 0.U.asTypeOf(cfg.ApplicationScale_B)
    cfg.MatrixRegTensor_N := 0.U
    cfg.MatrixRegTensor_K := 0.U
    cfg.Conherent := false.B
    cfg.MicroTaskValid := false.B
    cfg.MicroTaskEndReady := false.B
  }

  io.accMemoryConfig.ApplicationTensor_C := 0.U.asTypeOf(io.accMemoryConfig.ApplicationTensor_C)
  io.accMemoryConfig.ApplicationTensor_D := 0.U.asTypeOf(io.accMemoryConfig.ApplicationTensor_D)
  io.accMemoryConfig.LoadTaskInfo := 0.U.asTypeOf(io.accMemoryConfig.LoadTaskInfo)
  io.accMemoryConfig.Conherent := false.B
  io.accMemoryConfig.Is_Transpose := false.B
  io.accMemoryConfig.MatrixRegTensor_M := 0.U
  io.accMemoryConfig.MatrixRegTensor_N := 0.U
  io.accMemoryConfig.MatrixRegId := 0.U
  io.accMemoryConfig.PrefetchTaskId.foreach(_ := 0.U)
  io.accMemoryConfig.PrefetchStream.foreach(_ := MatrixPrefetchStream.none)
  io.accMemoryConfig.StoreTraceTag.foreach(_ := 0.U)
  io.accMemoryConfig.LoadMicroTaskValid := false.B
  io.accMemoryConfig.LoadMicroTaskEndReady := false.B
  io.accMemoryConfig.StoreMicroTaskValid := false.B
  io.accMemoryConfig.StoreMicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.accMemoryConfig.pc.get := 0.U
    io.accMemoryConfig.coreid.get := 0.U
  }

  io.MTE_MicroTask_Config.MicroTaskValid := false.B
  io.MTE_MicroTask_Config.computeType := MteComputeType.ComputeTypeUndef
  io.perfProbe := 0.U.asTypeOf(new TaskControllerPerfProbe)
  io.matrixPrefetch.foreach(_ := 0.U.asTypeOf(new MatrixPrefetchControl))

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
  private val matrixPrefetchDescTable = Option.when(EnableMatrixPrefetch)(
    ChiselDB.createTable("CUTEMatrixPrefetchDesc", new MatrixPrefetchDesc, basicDB = true)
  )
  // C stores are deliberately excluded from the L2 prefetch control path, but
  // still need an independent descriptor for complete trace reconstruction.
  private val cStoreDescTable = Option.when(EnableMatrixPrefetch)(
    ChiselDB.createTable("CUTECStoreDesc", new MatrixPrefetchDesc, basicDB = true)
  )
  private val cStoreDesc = Option.when(EnableMatrixPrefetch)(WireInit(0.U.asTypeOf(new MatrixPrefetchDesc)))
  private val cStoreDescEn = Option.when(EnableMatrixPrefetch)(WireInit(false.B))

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
  when(amuCtrlBits.isMls()) {
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
    val waitCompleteMask = UInt(WinDepth.W)
    val waitReadAMask = UInt(WinDepth.W)
    val waitReadBMask = UInt(WinDepth.W)
    val opKind = TaskCtrlOpKind()
    val trLoadBeats = UInt(MatrixRegMaxTensorDimBitSize.W)
    val trTailBytes = UInt(log2Ceil(outsideDataWidthByte + 1).W)
    val trOddTail = Bool()
  }

  class IssueWindowPayload(implicit p: Parameters) extends CuteBundle {
    val entry = new DecodedAmuCtrlEntry
    val seqId = UInt(SeqIdWidth.W)
    val fifoIdx = UInt(4.W)
  }

  class FuTracker extends Bundle {
    val busy = Bool()
    val ownerSlot = UInt(SlotIdxWidth.W)
  }

  val slots = RegInit(VecInit(Seq.fill(WinDepth)(0.U.asTypeOf(new IssueWindowSlot))))
  // Enqueue initializes the payload before the corresponding slot becomes valid.
  val slotPayloads = Reg(Vec(WinDepth, new IssueWindowPayload))
  val winHead = RegInit(0.U(SlotIdxWidth.W))
  val winTail = RegInit(0.U(SlotIdxWidth.W))
  val winCount = RegInit(0.U(WinCountWidth.W))
  val seqIdAlloc = RegInit(0.U(SeqIdWidth.W))

  val loadFifoIdxAlloc = RegInit(0.U(LoadFifoIdxWidth.W))
  val computeIssueIdx = RegInit(0.U(ComputeFifoIdxWidth.W))
  val storeIssueIdx = RegInit(0.U(StoreFifoIdxWidth.W))

  val fuTrLoader0 = RegInit(0.U.asTypeOf(new FuTracker))
  val fuTrLoader1 = RegInit(0.U.asTypeOf(new FuTracker))
  val fuAccLoader0 = RegInit(0.U.asTypeOf(new FuTracker))
  val fuCompute = RegInit(0.U.asTypeOf(new FuTracker))
  val fuTrStorer0 = RegInit(0.U.asTypeOf(new FuTracker))
  val fuAccStorer0 = RegInit(0.U.asTypeOf(new FuTracker))

  private def slotOH(idx: UInt): UInt = UIntToOH(idx, WinDepth)
  private def abIdx(x: UInt): UInt = x(TrMatrixRegIdWidth - 1, 0)
  private def cIdx(x: UInt): UInt = x(AccMatrixRegIdWidth - 1, 0)

  private def decodeMma(ctrl: AmuCtrlIO): AmuMmaIO = ctrl.data.asTypeOf(new AmuMmaIO)
  private def decodeLsu(ctrl: AmuCtrlIO): AmuLsuIO = ctrl.data.asTypeOf(new AmuLsuIO)
  private def decodeArith(ctrl: AmuCtrlIO): AmuArithIO = ctrl.data.asTypeOf(new AmuArithIO)
  private def isMzeroLike(arith: AmuArithIO): Bool = arith.opType(8, 2) === "b1101110".U

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
  val trLoader0OwnerValid = fuTrLoader0.busy && slots(fuTrLoader0.ownerSlot).valid
  val trLoader1OwnerValid = fuTrLoader1.busy && slots(fuTrLoader1.ownerSlot).valid
  val accLoader0OwnerValid = fuAccLoader0.busy && slots(fuAccLoader0.ownerSlot).valid
  val trStorer0OwnerValid = fuTrStorer0.busy && slots(fuTrStorer0.ownerSlot).valid
  val accStorer0OwnerValid = fuAccStorer0.busy && slots(fuAccStorer0.ownerSlot).valid
  val computeOwnerValid = fuCompute.busy && slots(fuCompute.ownerSlot).valid

  io.trLoader0Config.MicroTaskEndReady := trLoader0OwnerValid
  io.trLoader1Config.MicroTaskEndReady := trLoader1OwnerValid
  io.accMemoryConfig.LoadMicroTaskEndReady := accLoader0OwnerValid
  io.accMemoryConfig.StoreMicroTaskEndReady := accStorer0OwnerValid
  io.trStorer0.done.ready := trStorer0OwnerValid
  io.ADC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid
  io.BDC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid
  io.CDC_MicroTask_Config.MicroTaskEndReady := computeOwnerValid

  val trLoader0Done = io.trLoader0Config.MicroTaskEndValid && io.trLoader0Config.MicroTaskEndReady
  val trLoader1Done = io.trLoader1Config.MicroTaskEndValid && io.trLoader1Config.MicroTaskEndReady
  val accLoader0Done = io.accMemoryConfig.LoadMicroTaskEndValid && io.accMemoryConfig.LoadMicroTaskEndReady
  val trStorer0Done = io.trStorer0.done.fire
  val accStorer0Done = io.accMemoryConfig.StoreMicroTaskEndValid && io.accMemoryConfig.StoreMicroTaskEndReady
  val storeDone = trStorer0Done || accStorer0Done
  when(io.trStorer0.done.valid) { assert(trStorer0OwnerValid, "TaskController: trStorer0 done without owner") }
  // Independent FU owners; all stores remain ordered by waitCompleteMask.
  assert(!(fuTrStorer0.busy && fuAccStorer0.busy), "TaskController: ordered stores must not overlap")
  val adcDone = io.ADC_MicroTask_Config.MicroTaskEndValid && io.ADC_MicroTask_Config.MicroTaskEndReady
  val bdcDone = io.BDC_MicroTask_Config.MicroTaskEndValid && io.BDC_MicroTask_Config.MicroTaskEndReady
  val cdcDone = io.CDC_MicroTask_Config.MicroTaskEndValid && io.CDC_MicroTask_Config.MicroTaskEndReady

  when(io.trLoader0Config.MicroTaskEndValid) {
    assert(trLoader0OwnerValid, "TaskController: trLoader0 done without valid owner")
  }
  when(io.trLoader1Config.MicroTaskEndValid) {
    assert(trLoader1OwnerValid, "TaskController: trLoader1 done without valid owner")
  }
  when(io.accMemoryConfig.LoadMicroTaskEndValid) {
    assert(accLoader0OwnerValid, "TaskController: accMemory-load done without valid owner")
  }
  when(io.accMemoryConfig.StoreMicroTaskEndValid) {
    assert(accStorer0OwnerValid, "TaskController: accMemory-store done without valid owner")
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
  val deqArith = decodeArith(deqCtrl)

  val deqIsLoad = deqIsLsu && deqLsu.ls === 0.U
  val deqLoadSelOH = Cat(deqLsu.isacc, deqLsu.isB, deqLsu.isA)
  val deqLoadSelOneHot = PopCount(deqLoadSelOH) === 1.U
  val deqIsMzeroLike = deqIsArith && isMzeroLike(deqArith)

  when(deqValid && deqIsLoad) {
    assert(deqLoadSelOneHot, "TaskController: MLS load selector must be onehot among isA/isB/isacc")
  }

  val deqOpKind = Wire(TaskCtrlOpKind())
  deqOpKind := TaskCtrlOpKind.NopLike
  when(deqIsMma) {
    deqOpKind := TaskCtrlOpKind.Compute
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

  val trLoader0Ready = !fuTrLoader0.busy && io.trLoader0Config.MicroTaskReady
  val trLoader1Ready = !fuTrLoader1.busy && io.trLoader1Config.MicroTaskReady
  def trLoadReady(lsu: AmuLsuIO): Bool =
    Mux(lsu.transpose, trLoader0Ready, trLoader1Ready || trLoader0Ready)

  val readyBySlot = Wire(Vec(WinDepth, Bool()))
  readyBySlot.foreach(_ := false.B)
  for (slotIdx <- 0 until WinDepth) {
    val slot = slots(slotIdx)
    val slotPayload = slotPayloads(slotIdx)
    val slotAge = (slotIdx.U + WinDepth.U - winHead)(SlotIdxWidth - 1, 0)
    val inWindow = slotAge < winCount
    val slotCanConsider = inWindow && slot.valid && !slot.issued

    val slotDepReady = Mux(
      slotCanConsider,
      ((slot.waitCompleteMask & (~completedVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitReadAMask & (~readAVec)(WinDepth - 1, 0)) === 0.U) &&
        ((slot.waitReadBMask & (~readBVec)(WinDepth - 1, 0)) === 0.U),
      false.B
    )

    val slotFuReady = Mux(
      slotCanConsider,
      MuxLookup(slot.opKind.asUInt, true.B)(Seq(
        TaskCtrlOpKind.LoadA.asUInt -> trLoadReady(decodeLsu(slotPayload.entry.ctrl)),
        TaskCtrlOpKind.LoadB.asUInt -> trLoadReady(decodeLsu(slotPayload.entry.ctrl)),
        TaskCtrlOpKind.LoadC.asUInt -> (!fuAccLoader0.busy && io.accMemoryConfig.LoadMicroTaskReady),
        TaskCtrlOpKind.ZeroAcc.asUInt -> (!fuAccLoader0.busy && io.accMemoryConfig.LoadMicroTaskReady),
        TaskCtrlOpKind.ZeroTr.asUInt -> (!fuTrLoader0.busy && io.trLoader0Config.MicroTaskReady),
        TaskCtrlOpKind.Store.asUInt -> Mux(decodeLsu(slotPayload.entry.ctrl).isacc,
          !fuAccStorer0.busy && io.accMemoryConfig.StoreMicroTaskReady,
          !fuTrStorer0.busy),
        TaskCtrlOpKind.Compute.asUInt -> (!fuCompute.busy && io.ADC_MicroTask_Config.MicroTaskReady && io.BDC_MicroTask_Config.MicroTaskReady && io.CDC_MicroTask_Config.MicroTaskReady),
        TaskCtrlOpKind.Release.asUInt -> true.B,
        TaskCtrlOpKind.NopLike.asUInt -> true.B
      )),
      false.B
    )

    readyBySlot(slotIdx) := slotCanConsider && slotDepReady && slotFuReady
  }

  // Circular oldest-first priority: search from the head to the end, then wrap.
  // Both fixed-priority searches run in parallel on physical slot indices.
  val readyFromHead = VecInit(readyBySlot.zipWithIndex.map { case (ready, i) =>
    ready && winHead <= i.U
  }).asUInt
  val normalIssueFound = readyBySlot.asUInt.orR
  val issueSlotOH = Mux(readyFromHead.orR, PriorityEncoderOH(readyFromHead),
    PriorityEncoderOH(readyBySlot.asUInt))
  val releaseHoldValid = RegInit(false.B)
  val releaseHoldSlotIdx = RegInit(0.U(SlotIdxWidth.W))
  val issueSlotIdx = WireInit(0.U(SlotIdxWidth.W))
  when(releaseHoldValid) {
    issueSlotIdx := releaseHoldSlotIdx
  }.elsewhen(normalIssueFound) {
    issueSlotIdx := OHToUInt(issueSlotOH)
  }

  val issueFound = releaseHoldValid || normalIssueFound
  val issueSlot = slots(issueSlotIdx)
  val issuePayload = slotPayloads(issueSlotIdx)
  val issueSelectedIsRelease = issueFound && issueSlot.opKind === TaskCtrlOpKind.Release
  val selectedRelease = issuePayload.entry.ctrl.data.asTypeOf(new AmuReleaseIO)
  io.ygjkctrl.mrelease.valid := issueSelectedIsRelease
  io.ygjkctrl.mrelease.bits.msyncRd(selectedRelease.msyncRd) := true.B
  val issueFire = issueFound && (!issueSelectedIsRelease || io.ygjkctrl.mrelease.ready)

  when(issueSelectedIsRelease && !io.ygjkctrl.mrelease.ready) {
    releaseHoldValid := true.B
    releaseHoldSlotIdx := issueSlotIdx
  }.elsewhen(issueFire && issueSelectedIsRelease) {
    releaseHoldValid := false.B
  }

  val deqStoreReadsAB = deqIsLsu && (deqLsu.ls === 1.U) && !deqLsu.isacc
  val deqStoreReadsC = deqIsLsu && (deqLsu.ls === 1.U) && deqLsu.isacc

  // ===================== Retirement lookahead (after done convergence) =====================
  val headSlot = slots(winHead)

  val headDoneByFu =
    (trLoader0Done && fuTrLoader0.busy && fuTrLoader0.ownerSlot === winHead) ||
    (trLoader1Done && fuTrLoader1.busy && fuTrLoader1.ownerSlot === winHead) ||
    (accLoader0Done && fuAccLoader0.busy && fuAccLoader0.ownerSlot === winHead) ||
    (trStorer0Done && fuTrStorer0.busy && fuTrStorer0.ownerSlot === winHead) ||
    (accStorer0Done && fuAccStorer0.busy && fuAccStorer0.ownerSlot === winHead) ||
    (cdcDone && fuCompute.busy && fuCompute.ownerSlot === winHead)

  val headCompletedAfterDone = headSlot.completed || headDoneByFu
  val retireFire = headSlot.valid && headCompletedAfterDone

  // Allow retire + enqueue in the same cycle, including full-window reuse of the retired slot
  val windowFull = winCount === WinDepth.U
  val enqueueCanFire = deqValid && (!windowFull || retireFire)
  decodedFifo.io.deq.ready := enqueueCanFire
  val enqueueFire = decodedFifo.io.deq.fire

  val enqueueSlotIdx = Mux(windowFull, winHead, winTail)
  val ownedWork = deqValid || (winCount =/= 0.U)

  // ===================== Issue dispatch bridge =====================
  val issueCtrl = issuePayload.entry.ctrl
  val issueLsu = decodeLsu(issueCtrl)
  val issueTrLoader1 = !issueLsu.transpose && trLoader1Ready
  val issueMma = decodeMma(issueCtrl)
  val issueArith = decodeArith(issueCtrl)

  private def computeKFromMsew(k: UInt, msew: UInt): UInt = {
    MuxLookup(msew(1, 0), k)(Seq(
      Bundles.MSew.e8 -> k,
      Bundles.MSew.e16 -> (k << 1),
      Bundles.MSew.e32 -> (k << 2),
      Bundles.MSew.e4 -> (k >> 1)
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
      is(Bundles.MSew.e4)  { byteCount := (dimWide +& 1.U) >> 1 }
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
      }.otherwise {
        computeType := MteComputeType.ComputeTypeUndef
      }
    }.otherwise {
      if (enableMteInt8) {
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
    }
    computeType
  }

  if (EnableMatrixPrefetch) {
    def prefetchStreamForOp(op: TaskCtrlOpKind.Type): UInt = {
      MuxLookup(op.asUInt, MatrixPrefetchStream.none)(Seq(
        TaskCtrlOpKind.LoadA.asUInt -> MatrixPrefetchStream.a,
        TaskCtrlOpKind.LoadB.asUInt -> MatrixPrefetchStream.b,
        TaskCtrlOpKind.LoadC.asUInt -> MatrixPrefetchStream.cLoad
      ))
    }

    val matrixPrefetch = io.matrixPrefetch.get
    val allocPrefetchStream = prefetchStreamForOp(deqOpKind)
    val allocOuterCount = MuxLookup(deqOpKind.asUInt, 0.U)(Seq(
      TaskCtrlOpKind.LoadA.asUInt -> Mux(deqLsu.transpose, deqLsu.column, deqLsu.row),
      TaskCtrlOpKind.LoadB.asUInt -> Mux(deqLsu.transpose, deqLsu.row, deqLsu.column),
      TaskCtrlOpKind.LoadC.asUInt -> Mux(deqLsu.transpose, deqLsu.column, deqLsu.row)
    ))
    val allocInnerDim = MuxLookup(deqOpKind.asUInt, 0.U)(Seq(
      TaskCtrlOpKind.LoadA.asUInt -> Mux(deqLsu.transpose, deqLsu.row, deqLsu.column),
      TaskCtrlOpKind.LoadB.asUInt -> Mux(deqLsu.transpose, deqLsu.column, deqLsu.row),
      TaskCtrlOpKind.LoadC.asUInt -> Mux(deqLsu.transpose, deqLsu.row, deqLsu.column)
    ))
    val allocDesc = WireInit(0.U.asTypeOf(new MatrixPrefetchDesc))
    allocDesc.taskId := seqIdAlloc
    allocDesc.stream := allocPrefetchStream
    allocDesc.baseAddr := deqLsu.baseAddr
    allocDesc.outerStride := deqLsu.stride
    allocDesc.outerCount := allocOuterCount
    allocDesc.innerCount := loadBeatCount(allocInnerDim, deqLsu.widths)
    allocDesc.rowBytes := loadByteCount(allocInnerDim, deqLsu.widths).pad(32)
    val aNormalPrefetchGroupWidth =
      if (TrLoader0UseLegacyLoader) Matrix_MN else TrMatrixRegNBanks
    allocDesc.groupWidth := MuxLookup(deqOpKind.asUInt, Matrix_MN.U)(Seq(
      TaskCtrlOpKind.LoadA.asUInt -> Mux(
        deqLsu.transpose,
        TrMatrixRegEntryByteSize.U,
        aNormalPrefetchGroupWidth.U
      )
    ))
    allocDesc.transpose := deqLsu.transpose
    if (EnableDifftest) allocDesc.pc := deqEntry.ctrl.pc.get else allocDesc.pc := 0.U

    when(enqueueFire && allocPrefetchStream =/= MatrixPrefetchStream.none) {
      matrixPrefetch.allocate.valid := true.B
      matrixPrefetch.allocate.bits := allocDesc
    }

    val issuePrefetchStream = prefetchStreamForOp(issueSlot.opKind)
    when(issueFire && issuePrefetchStream =/= MatrixPrefetchStream.none) {
      matrixPrefetch.activate.valid := true.B
      matrixPrefetch.activate.bits := MatrixPrefetchTagCodec.encode(true.B, issuePrefetchStream, issuePayload.seqId)
    }

    val retirePrefetchStream = prefetchStreamForOp(headSlot.opKind)
    when(retireFire && retirePrefetchStream =/= MatrixPrefetchStream.none) {
      matrixPrefetch.retire.valid := true.B
      matrixPrefetch.retire.bits := MatrixPrefetchTagCodec.encode(true.B, retirePrefetchStream, slotPayloads(winHead).seqId)
    }

    when(issueFire && issueSlot.opKind === TaskCtrlOpKind.Store && issueLsu.isacc) {
      matrixPrefetch.cStoreStart.valid := true.B
      matrixPrefetch.cStoreStart.bits := MatrixPrefetchTagCodec.encode(
        true.B,
        MatrixPrefetchStream.cStore,
        issuePayload.seqId
      )
    }

    when(accStorer0Done) {
      val storeOwner = slotPayloads(fuAccStorer0.ownerSlot)
      matrixPrefetch.cStoreEnd.valid := true.B
      matrixPrefetch.cStoreEnd.bits := MatrixPrefetchTagCodec.encode(
        true.B,
        MatrixPrefetchStream.cStore,
        storeOwner.seqId
      )
    }
  }

  // The command handshake gates consumption. Route geometry independently of FU readiness.
  val trSwapDims = issueLsu.isB ^ issueLsu.transpose
  for (cfg <- Seq(io.trLoader0Config, io.trLoader1Config)) {
    cfg.Tensor.baseVaddr := issueLsu.baseAddr
    cfg.Tensor.stride := issueLsu.stride
    cfg.Tensor.widths := issueLsu.widths
    cfg.Tensor.rowElements := Mux(trSwapDims, issueLsu.row, issueLsu.column)
    cfg.MatrixRegTensor_M := Mux(trSwapDims, issueLsu.column, issueLsu.row)
    cfg.MatrixRegTensor_K := issueSlot.trLoadBeats
    cfg.Tensor.K_Beat_Count := issueSlot.trLoadBeats
    cfg.Tensor.TailByteMask := issueSlot.trTailBytes
    cfg.Tensor.HasTail := issueSlot.trTailBytes.orR
    cfg.Tensor.oddE4Tail := issueSlot.trOddTail
  }

  // The descriptor is consumed only on command.fire. Keep FU readiness off
  // the data bus, as on the TR load descriptor above.
  io.trStorer0.command.bits.regId := issueLsu.ms(1, 0)
  io.trStorer0.command.bits.base := issueLsu.baseAddr
  io.trStorer0.command.bits.stride := issueLsu.stride
  // Both A and B stores use trStorer0. B packets are K/N; TR images are N/K.
  io.trStorer0.command.bits.rows := Mux(issueLsu.isB, issueLsu.column, issueLsu.row)
  io.trStorer0.command.bits.columns := Mux(issueLsu.isB, issueLsu.row, issueLsu.column)
  io.trStorer0.command.bits.widths := issueLsu.widths
  io.trStorer0.command.bits.transpose := issueLsu.transpose

  when(issueFire) {
    switch(issueSlot.opKind) {
      is(TaskCtrlOpKind.LoadA, TaskCtrlOpKind.LoadB) {
        val isB = issueSlot.opKind === TaskCtrlOpKind.LoadB
        for ((cfg, unit) <- Seq(io.trLoader0Config, io.trLoader1Config).zipWithIndex) {
          cfg.LoadTaskInfo.Is_FullLoad := true.B
          cfg.MatrixRegId := issueLsu.ms(1, 0)
          cfg.PrefetchTaskId.foreach(_ := issuePayload.seqId)
          cfg.PrefetchStream.foreach(_ := Mux(isB, MatrixPrefetchStream.b, MatrixPrefetchStream.a))
          cfg.Conherent := true.B
          cfg.Is_Transpose := issueLsu.transpose
          cfg.MicroTaskValid := (if (unit == 1) issueTrLoader1 else !issueTrLoader1)
          cfg.pc.zip(issueCtrl.pc).foreach { case (to, from) => to := from }
          cfg.coreid.zip(issueCtrl.coreid).foreach { case (to, from) => to := from }
        }

        // Existing event positions identify physical TL0/TL1, not the A/B opcode.
        for ((event, kind) <- Seq(loadAllocateEvent -> 0, loadIssueEvent -> 1)) {
          event.eventType := kind.U
          event.regId := issueLsu.ms(1, 0)
          event.fifoIdx := issuePayload.fifoIdx
          event.needMask := Mux(issueTrLoader1, "b010".U, "b001".U)
          event.row := issueLsu.row
          event.column := issueLsu.column
          event.transpose := issueLsu.transpose
          event.isAcc := false.B
          event.slotId := issueSlotIdx
          event.seqId := issuePayload.seqId
        }
        loadAllocateEventEn := true.B
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.LoadC) {
        val regIdx = issueLsu.ms(1, 0)
        val matrixDimM = Mux(issueLsu.transpose, issueLsu.column, issueLsu.row)
        val matrixDimN = Mux(issueLsu.transpose, issueLsu.row, issueLsu.column)
        val nVal = loadBeatCount(matrixDimN, issueLsu.widths)

        io.accMemoryConfig.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := issueLsu.baseAddr
        io.accMemoryConfig.ApplicationTensor_C.ApplicationTensor_C_Stride_M := issueLsu.stride
        io.accMemoryConfig.ApplicationTensor_C.BlockTensor_C_BaseVaddr := issueLsu.baseAddr
        io.accMemoryConfig.ApplicationTensor_C.dataType := loadDataType(issueLsu.widths)
        io.accMemoryConfig.ApplicationTensor_C.HasTail := loadHasTail(matrixDimN, issueLsu.widths)
        io.accMemoryConfig.ApplicationTensor_C.TailByteMask := loadTailByteMask(matrixDimN, issueLsu.widths)
        io.accMemoryConfig.ApplicationTensor_C.N_Beat_Count := nVal
        io.accMemoryConfig.Conherent := true.B
        io.accMemoryConfig.LoadTaskInfo.Is_FullLoad := true.B
        io.accMemoryConfig.LoadTaskInfo.Is_ZeroLoad := false.B
        io.accMemoryConfig.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.accMemoryConfig.MatrixRegTensor_M := matrixDimM
        io.accMemoryConfig.MatrixRegTensor_N := matrixDimN
        io.accMemoryConfig.MatrixRegId := regIdx
        io.accMemoryConfig.PrefetchTaskId.foreach(_ := issuePayload.seqId)
        io.accMemoryConfig.PrefetchStream.foreach(_ := MatrixPrefetchStream.cLoad)
        io.accMemoryConfig.Is_Transpose := issueLsu.transpose
        io.accMemoryConfig.LoadMicroTaskValid := true.B
        io.accMemoryConfig.StoreMicroTaskValid := false.B
        if (EnableDifftest) {
          io.accMemoryConfig.pc.get := issueCtrl.pc.get
          io.accMemoryConfig.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issuePayload.fifoIdx
        loadAllocateEvent.needMask := "b100".U
        loadAllocateEvent.row := issueLsu.row
        loadAllocateEvent.column := issueLsu.column
        loadAllocateEvent.transpose := issueLsu.transpose
        loadAllocateEvent.isAcc := true.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issuePayload.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issuePayload.fifoIdx
        loadIssueEvent.needMask := "b100".U
        loadIssueEvent.row := issueLsu.row
        loadIssueEvent.column := issueLsu.column
        loadIssueEvent.transpose := issueLsu.transpose
        loadIssueEvent.isAcc := true.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issuePayload.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.ZeroAcc) {
        val regIdx = issueArith.md(1, 0)

        io.accMemoryConfig.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
        io.accMemoryConfig.MatrixRegTensor_M := cuteParams.Tensor_MN.U
        io.accMemoryConfig.MatrixRegTensor_N := cuteParams.Tensor_MN.U
        io.accMemoryConfig.MatrixRegId := regIdx
        io.accMemoryConfig.LoadMicroTaskValid := true.B
        io.accMemoryConfig.StoreMicroTaskValid := false.B
        io.accMemoryConfig.LoadTaskInfo.Is_ZeroLoad := true.B
        io.accMemoryConfig.LoadTaskInfo.Is_FullLoad := false.B
        io.accMemoryConfig.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.accMemoryConfig.Conherent := true.B
        io.accMemoryConfig.Is_Transpose := false.B
        if (EnableDifftest) {
          io.accMemoryConfig.pc.get := issueCtrl.pc.get
          io.accMemoryConfig.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issuePayload.fifoIdx
        loadAllocateEvent.needMask := "b100".U
        loadAllocateEvent.row := 0.U
        loadAllocateEvent.column := 0.U
        loadAllocateEvent.transpose := false.B
        loadAllocateEvent.isAcc := true.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issuePayload.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issuePayload.fifoIdx
        loadIssueEvent.needMask := "b100".U
        loadIssueEvent.row := 0.U
        loadIssueEvent.column := 0.U
        loadIssueEvent.transpose := false.B
        loadIssueEvent.isAcc := true.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issuePayload.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.ZeroTr) {
        val regIdx = issueArith.md(1, 0)

        io.trLoader0Config.Tensor.widths := Bundles.MSew.e8
        io.trLoader0Config.MatrixRegTensor_M := cuteParams.Tensor_MN.U
        io.trLoader0Config.MatrixRegTensor_K := cuteParams.Tensor_K.U / ReduceWidthByte.U
        io.trLoader0Config.MatrixRegId := regIdx
        io.trLoader0Config.MicroTaskValid := true.B
        io.trLoader0Config.LoadTaskInfo.Is_ZeroLoad := true.B
        io.trLoader0Config.LoadTaskInfo.Is_FullLoad := false.B
        io.trLoader0Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
        io.trLoader0Config.Conherent := true.B
        io.trLoader0Config.Is_Transpose := false.B
        if (EnableDifftest) {
          io.trLoader0Config.pc.get := issueCtrl.pc.get
          io.trLoader0Config.coreid.get := issueCtrl.coreid.get
        }

        loadAllocateEvent.eventType := 0.U
        loadAllocateEvent.regId := regIdx
        loadAllocateEvent.fifoIdx := issuePayload.fifoIdx
        loadAllocateEvent.needMask := "b001".U
        loadAllocateEvent.row := 0.U
        loadAllocateEvent.column := 0.U
        loadAllocateEvent.transpose := false.B
        loadAllocateEvent.isAcc := false.B
        loadAllocateEvent.slotId := issueSlotIdx
        loadAllocateEvent.seqId := issuePayload.seqId
        loadAllocateEventEn := true.B

        loadIssueEvent.eventType := 1.U
        loadIssueEvent.regId := regIdx
        loadIssueEvent.fifoIdx := issuePayload.fifoIdx
        loadIssueEvent.needMask := "b001".U
        loadIssueEvent.row := 0.U
        loadIssueEvent.column := 0.U
        loadIssueEvent.transpose := false.B
        loadIssueEvent.isAcc := false.B
        loadIssueEvent.slotId := issueSlotIdx
        loadIssueEvent.seqId := issuePayload.seqId
        loadIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Compute) {
        val aReg = issueMma.ms1(1, 0)
        val bReg = issueMma.ms2(1, 0)
        val cReg = issueMma.md(1, 0)
        val mVal = issueMma.mtilem
        val nVal = issueMma.mtilen
        val kVal = computeKFromMsew(issueMma.mtilek, issueMma.types1)
        val mmaComputeType = decodeMmaComputeType(issueMma)

        io.ADC_MicroTask_Config.MicroTaskValid := true.B
        io.BDC_MicroTask_Config.MicroTaskValid := true.B
        io.CDC_MicroTask_Config.MicroTaskValid := true.B

        io.ADC_MicroTask_Config.dataType := ElementDataType.DataTypeWidth8
        io.ADC_MicroTask_Config.MatrixRegTensor_M := mVal
        io.ADC_MicroTask_Config.MatrixRegTensor_N := nVal
        io.ADC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U
        io.ADC_MicroTask_Config.MatrixRegId := aReg
        io.ADC_MicroTask_Config.Is_Transpose := false.B

        io.BDC_MicroTask_Config.dataType := ElementDataType.DataTypeWidth8
        io.BDC_MicroTask_Config.MatrixRegTensor_M := mVal
        io.BDC_MicroTask_Config.MatrixRegTensor_N := nVal
        io.BDC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U
        io.BDC_MicroTask_Config.MatrixRegId := bReg
        io.BDC_MicroTask_Config.Is_Transpose := false.B

        io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
        io.CDC_MicroTask_Config.MatrixRegTensor_M := mVal
        io.CDC_MicroTask_Config.MatrixRegTensor_N := nVal
        io.CDC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U
        io.CDC_MicroTask_Config.MatrixRegId := cReg
        io.CDC_MicroTask_Config.Is_Transpose := false.B
        io.CDC_MicroTask_Config.Is_AfterOps_Tile := false.B
        if (EnableDifftest) {
          io.CDC_MicroTask_Config.pc.get := issueCtrl.pc.get
          io.CDC_MicroTask_Config.coreid.get := issueCtrl.coreid.get
        }

        io.MTE_MicroTask_Config.MicroTaskValid := true.B
        io.MTE_MicroTask_Config.computeType := mmaComputeType

        computeIssueEvent.eventType := 0.U
        computeIssueEvent.aReg := aReg
        computeIssueEvent.bReg := bReg
        computeIssueEvent.cReg := cReg
        computeIssueEvent.fifoIdx := issuePayload.fifoIdx
        computeIssueEvent.mtilem := mVal
        computeIssueEvent.mtilen := nVal
        computeIssueEvent.mtilek := kVal
        computeIssueEvent.isMma := true.B
        computeIssueEvent.isFp := issueMma.isfp
        computeIssueEvent.slotId := issueSlotIdx
        computeIssueEvent.seqId := issuePayload.seqId
        computeIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Store) {
        val regIdx = issueLsu.ms(1, 0)

        when(issueLsu.isacc) {
          cStoreDesc.foreach { desc =>
            desc.taskId := issuePayload.seqId
            desc.stream := MatrixPrefetchStream.cStore
            desc.baseAddr := issueLsu.baseAddr
            desc.outerStride := issueLsu.stride
            desc.outerCount := issueLsu.row
            desc.innerCount := loadBeatCount(issueLsu.column, issueLsu.widths)
            desc.rowBytes := loadByteCount(issueLsu.column, issueLsu.widths)
            desc.groupWidth := AccMatrixRegNBanks.U
            desc.transpose := issueLsu.transpose
            cStoreDescEn.get := true.B
            if (EnableDifftest) {
              desc.pc := issueCtrl.pc.get
            }
          }
        }

        when(issueLsu.isacc) {
          io.accMemoryConfig.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := issueLsu.baseAddr
          io.accMemoryConfig.ApplicationTensor_D.ApplicationTensor_D_Stride_M := issueLsu.stride
          io.accMemoryConfig.ApplicationTensor_D.BlockTensor_D_BaseVaddr := issueLsu.baseAddr
          io.accMemoryConfig.ApplicationTensor_D.dataType := loadDataType(issueLsu.widths)
          io.accMemoryConfig.Conherent := true.B
          io.accMemoryConfig.Is_Transpose := issueLsu.transpose
          io.accMemoryConfig.MatrixRegTensor_M := issueLsu.row
          io.accMemoryConfig.MatrixRegTensor_N := issueLsu.column
          io.accMemoryConfig.MatrixRegId := regIdx
          io.accMemoryConfig.StoreTraceTag.foreach { tag =>
            tag := MatrixPrefetchTagCodec.encode(
              true.B,
              MatrixPrefetchStream.cStore,
              issuePayload.seqId
            )
          }
          io.accMemoryConfig.LoadMicroTaskValid := false.B
          io.accMemoryConfig.StoreMicroTaskValid := true.B
          if (EnableDifftest) {
            io.accMemoryConfig.pc.get := issueCtrl.pc.get
            io.accMemoryConfig.coreid.get := issueCtrl.coreid.get
          }

        }.otherwise {
          val command = io.trStorer0.command
          command.valid := true.B
          // Both store backends accept only while idle and return to idle on
          // done.fire, exactly when this FU tracker releases its owner.
          assert(command.ready, "TaskController: idle TR store tracker requires a ready backend")
          command.bits.pc.zip(issueCtrl.pc).foreach { case (to, from) => to := from }
          command.bits.coreid.zip(issueCtrl.coreid).foreach { case (to, from) => to := from }
        }

        storeIssueEvent.eventType := 0.U
        storeIssueEvent.regId := regIdx
        storeIssueEvent.fifoIdx := issuePayload.fifoIdx
        storeIssueEvent.row := issueLsu.row
        storeIssueEvent.column := issueLsu.column
        storeIssueEvent.transpose := issueLsu.transpose
        storeIssueEvent.isAcc := issueLsu.isacc
        storeIssueEvent.slotId := issueSlotIdx
        storeIssueEvent.seqId := issuePayload.seqId
        storeIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.Release) {
        val issueRelease = issueCtrl.data.asTypeOf(new AmuReleaseIO)

        releaseIssueEvent.eventType := 0.U
        releaseIssueEvent.msync := issueRelease.msyncRd
        releaseIssueEvent.slotId := issueSlotIdx
        releaseIssueEvent.seqId := issuePayload.seqId
        releaseIssueEventEn := true.B
      }

      is(TaskCtrlOpKind.NopLike) {
        // NopLike no-op: issue/complete in scheduler only.
      }
    }
  }

  // ===================== State update: done -> retire -> enqueue -> issue =====================
  when(trLoader0Done) {
    val owner = fuTrLoader0.ownerSlot
    assert(slots(owner).valid, "TaskController: trLoader0 owner slot invalid on done")
    slots(owner).completed := true.B
    fuTrLoader0.busy := false.B
  }

  when(trLoader1Done) {
    val owner = fuTrLoader1.ownerSlot
    assert(slots(owner).valid, "TaskController: trLoader1 owner slot invalid on done")
    slots(owner).completed := true.B
    fuTrLoader1.busy := false.B
  }

  when(accLoader0Done) {
    val owner = fuAccLoader0.ownerSlot
    assert(slots(owner).valid, "TaskController: accMemory-load owner slot invalid on done")
    slots(owner).completed := true.B
    fuAccLoader0.busy := false.B
  }

  for ((done, fu) <- Seq((trStorer0Done, fuTrStorer0), (accStorer0Done, fuAccStorer0))) {
    when(done) {
      val owner = fu.ownerSlot
      assert(slots(owner).valid, "TaskController: store owner slot invalid on done")
      slots(owner).completed := true.B
      fu.busy := false.B
    }
  }

  when(adcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: ADC owner slot invalid on done")
    assert(!slots(owner).readADone, "TaskController: duplicated ADC done")
    slots(owner).readADone := true.B

    val slotPayload = slotPayloads(owner)
    val mma = decodeMma(slotPayload.entry.ctrl)
    computeReadAFinishEvent.eventType := 1.U
    computeReadAFinishEvent.aReg := mma.ms1(1, 0)
    computeReadAFinishEvent.bReg := mma.ms2(1, 0)
    computeReadAFinishEvent.cReg := mma.md(1, 0)
    computeReadAFinishEvent.fifoIdx := slotPayload.fifoIdx
    computeReadAFinishEvent.mtilem := mma.mtilem
    computeReadAFinishEvent.mtilen := mma.mtilen
    computeReadAFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeReadAFinishEvent.isMma := true.B
    computeReadAFinishEvent.isFp := mma.isfp
    computeReadAFinishEvent.slotId := owner
    computeReadAFinishEvent.seqId := slotPayload.seqId
    computeReadAFinishEventEn := true.B
  }

  when(bdcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: BDC owner slot invalid on done")
    assert(!slots(owner).readBDone, "TaskController: duplicated BDC done")
    slots(owner).readBDone := true.B

    val slotPayload = slotPayloads(owner)
    val mma = decodeMma(slotPayload.entry.ctrl)
    computeReadBFinishEvent.eventType := 2.U
    computeReadBFinishEvent.aReg := mma.ms1(1, 0)
    computeReadBFinishEvent.bReg := mma.ms2(1, 0)
    computeReadBFinishEvent.cReg := mma.md(1, 0)
    computeReadBFinishEvent.fifoIdx := slotPayload.fifoIdx
    computeReadBFinishEvent.mtilem := mma.mtilem
    computeReadBFinishEvent.mtilen := mma.mtilen
    computeReadBFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeReadBFinishEvent.isMma := true.B
    computeReadBFinishEvent.isFp := mma.isfp
    computeReadBFinishEvent.slotId := owner
    computeReadBFinishEvent.seqId := slotPayload.seqId
    computeReadBFinishEventEn := true.B
  }

  when(cdcDone) {
    val owner = fuCompute.ownerSlot
    assert(slots(owner).valid, "TaskController: CDC owner slot invalid on done")
    slots(owner).completed := true.B
    fuCompute.busy := false.B

    val slotPayload = slotPayloads(owner)
    val mma = decodeMma(slotPayload.entry.ctrl)
    computeWriteCFinishEvent.eventType := 3.U
    computeWriteCFinishEvent.aReg := mma.ms1(1, 0)
    computeWriteCFinishEvent.bReg := mma.ms2(1, 0)
    computeWriteCFinishEvent.cReg := mma.md(1, 0)
    computeWriteCFinishEvent.fifoIdx := slotPayload.fifoIdx
    computeWriteCFinishEvent.mtilem := mma.mtilem
    computeWriteCFinishEvent.mtilen := mma.mtilen
    computeWriteCFinishEvent.mtilek := computeKFromMsew(mma.mtilek, mma.types1)
    computeWriteCFinishEvent.isMma := true.B
    computeWriteCFinishEvent.isFp := mma.isfp
    computeWriteCFinishEvent.slotId := owner
    computeWriteCFinishEvent.seqId := slotPayload.seqId
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
      }
    }

    slots(winHead).valid := false.B
    slots(winHead).issued := false.B
    slots(winHead).completed := false.B
    slots(winHead).readADone := false.B
    slots(winHead).readBDone := false.B
    slots(winHead).waitCompleteMask := 0.U
    slots(winHead).waitReadAMask := 0.U
    slots(winHead).waitReadBMask := 0.U
    slots(winHead).opKind := TaskCtrlOpKind.NopLike
  }

  when(enqueueFire) {
    val newSlot = WireInit(0.U.asTypeOf(new IssueWindowSlot))
    val newPayload = Wire(new IssueWindowPayload)
    newSlot.valid := true.B
    newSlot.issued := false.B
    newSlot.completed := false.B
    newSlot.readADone := false.B
    newSlot.readBDone := false.B
    newSlot.waitCompleteMask := 0.U
    newSlot.waitReadAMask := 0.U
    newSlot.waitReadBMask := 0.U
    newSlot.opKind := deqOpKind
    newPayload.entry := deqEntry
    // A beat has a power-of-two number of elements at every supported EEW.
    // Derive both counts from the same unrounded element length before issue.
    val trRowElements = Mux(deqLsu.isB ^ deqLsu.transpose, deqLsu.row, deqLsu.column)
    val trFullBeats = TrElementWidth.select(deqLsu.widths) { bits =>
      trRowElements >> log2Ceil(outsideDataWidthByte * 8 / bits)
    }
    val nibbleOffsetBits = log2Ceil(outsideDataWidthByte * 2)
    val trTailNibbles = TrElementWidth.select(deqLsu.widths) { bits =>
      (trRowElements.pad(nibbleOffsetBits) << log2Ceil(bits / 4))(nibbleOffsetBits - 1, 0)
    }
    when(deqIsLoad && !deqLsu.isacc) {
      newSlot.trLoadBeats := trFullBeats +& trTailNibbles.orR
      newSlot.trOddTail := trTailNibbles(0)
      newSlot.trTailBytes := (trTailNibbles >> 1) +& trTailNibbles(0)
    }
    newPayload.seqId := seqIdAlloc
    newPayload.fifoIdx := 0.U

    when(
      deqOpKind === TaskCtrlOpKind.LoadA ||
      deqOpKind === TaskCtrlOpKind.LoadB ||
      deqOpKind === TaskCtrlOpKind.LoadC ||
      deqOpKind === TaskCtrlOpKind.ZeroAcc ||
      deqOpKind === TaskCtrlOpKind.ZeroTr
    ) {
      newPayload.fifoIdx := loadFifoIdxAlloc
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Compute) {
      newPayload.fifoIdx := computeIssueIdx
    }.elsewhen(deqOpKind === TaskCtrlOpKind.Store) {
      newPayload.fifoIdx := storeIssueIdx
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
      val olderCtrl = slotPayloads(j).entry.ctrl
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

      val depCompleteJ = Mux(
        deqOpKind === TaskCtrlOpKind.Release,
        olderValid && (older.opKind === TaskCtrlOpKind.Store),
        olderValid && (
          // Keep stores in instruction order, even when an older one waits
          // for load data; no address-overlap detector is introduced here.
          (deqOpKind === TaskCtrlOpKind.Store && olderIsStore) ||
          // An older TR store retains its read resource until completion.
          (deqOpKind === TaskCtrlOpKind.Compute && olderStoreReadsAB &&
            (abIdx(olderLsu.ms) === abIdx(decodeMma(deqCtrl).ms1) ||
              abIdx(olderLsu.ms) === abIdx(decodeMma(deqCtrl).ms2))) ||
          (olderWritesAB && newReadsAB && olderWriteABHitForNewRead) ||
          (olderWritesC && newReadsC && olderWriteCHitForNewRead) ||
          (olderWritesAB && newWritesAB && olderWriteABHitForNewWrite) ||
          (olderWritesC && newWritesC && olderWriteCHitForNewWrite) ||
          (newWritesAB && olderReadABHit && !olderReadsABByComputeA && !olderReadsABByComputeB) ||
          (newWritesC && olderReadCHit)
        )
      )

      depCompleteBits(j) := depCompleteJ
      depReadABits(j) := (deqOpKind =/= TaskCtrlOpKind.Release) && olderValid && (
        (newWritesAB && olderReadsABByComputeA) ||
        (deqStoreReadsAB && isReadAOfCompute(older.opKind, olderMma, deqLsu.ms)))
      depReadBBits(j) := (deqOpKind =/= TaskCtrlOpKind.Release) && olderValid && (
        (newWritesAB && olderReadsABByComputeB) ||
        (deqStoreReadsAB && isReadBOfCompute(older.opKind, olderMma, deqLsu.ms)))
    }

    newSlot.waitCompleteMask := depCompleteBits.asUInt
    newSlot.waitReadAMask := depReadABits.asUInt
    newSlot.waitReadBMask := depReadBBits.asUInt

    slots(enqueueSlotIdx) := newSlot
    slotPayloads(enqueueSlotIdx) := newPayload
  }

  when(issueFire) {
    slots(issueSlotIdx).issued := true.B

    switch(issueSlot.opKind) {
      is(TaskCtrlOpKind.LoadA, TaskCtrlOpKind.LoadB) {
        when(issueTrLoader1) {
          fuTrLoader1.busy := true.B
          fuTrLoader1.ownerSlot := issueSlotIdx
        }.otherwise {
          fuTrLoader0.busy := true.B
          fuTrLoader0.ownerSlot := issueSlotIdx
        }
      }
      is(TaskCtrlOpKind.LoadC) {
        fuAccLoader0.busy := true.B
        fuAccLoader0.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.ZeroAcc) {
        fuAccLoader0.busy := true.B
        fuAccLoader0.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.ZeroTr) {
        fuTrLoader0.busy := true.B
        fuTrLoader0.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.Compute) {
        fuCompute.busy := true.B
        fuCompute.ownerSlot := issueSlotIdx
      }
      is(TaskCtrlOpKind.Store) {
        when(issueLsu.isacc) {
          fuAccStorer0.busy := true.B
          fuAccStorer0.ownerSlot := issueSlotIdx
        }.otherwise {
          fuTrStorer0.busy := true.B
          fuTrStorer0.ownerSlot := issueSlotIdx
        }
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
  when(trLoader0Done) {
    val owner = fuTrLoader0.ownerSlot
    val slotPayload = slotPayloads(owner)
    val op = slots(owner).opKind
    val lsu = decodeLsu(slotPayload.entry.ctrl)
    val arith = decodeArith(slotPayload.entry.ctrl)
    val reg = Mux(op === TaskCtrlOpKind.ZeroTr, arith.md(1, 0), lsu.ms(1, 0))

    loadAFinishEvent.eventType := 2.U
    loadAFinishEvent.regId := reg
    loadAFinishEvent.fifoIdx := slotPayload.fifoIdx
    loadAFinishEvent.needMask := "b001".U
    loadAFinishEvent.row := Mux(op === TaskCtrlOpKind.ZeroTr, 0.U, lsu.row)
    loadAFinishEvent.column := Mux(op === TaskCtrlOpKind.ZeroTr, 0.U, lsu.column)
    loadAFinishEvent.transpose := Mux(op === TaskCtrlOpKind.ZeroTr, false.B, lsu.transpose)
    loadAFinishEvent.isAcc := false.B
    loadAFinishEvent.slotId := owner
    loadAFinishEvent.seqId := slotPayload.seqId
    loadAFinishEventEn := true.B
  }

  when(trLoader1Done) {
    val owner = fuTrLoader1.ownerSlot
    val slotPayload = slotPayloads(owner)
    val lsu = decodeLsu(slotPayload.entry.ctrl)

    loadBFinishEvent.eventType := 2.U
    loadBFinishEvent.regId := lsu.ms(1, 0)
    loadBFinishEvent.fifoIdx := slotPayload.fifoIdx
    loadBFinishEvent.needMask := "b010".U
    loadBFinishEvent.row := lsu.row
    loadBFinishEvent.column := lsu.column
    loadBFinishEvent.transpose := lsu.transpose
    loadBFinishEvent.isAcc := false.B
    loadBFinishEvent.slotId := owner
    loadBFinishEvent.seqId := slotPayload.seqId
    loadBFinishEventEn := true.B
  }

  when(accLoader0Done) {
    val owner = fuAccLoader0.ownerSlot
    val slotPayload = slotPayloads(owner)
    val op = slots(owner).opKind
    val lsu = decodeLsu(slotPayload.entry.ctrl)
    val arith = decodeArith(slotPayload.entry.ctrl)
    val reg = Mux(op === TaskCtrlOpKind.ZeroAcc, arith.md(1, 0), lsu.ms(1, 0))
    val row = Mux(op === TaskCtrlOpKind.ZeroAcc, 0.U, lsu.row)
    val col = Mux(op === TaskCtrlOpKind.ZeroAcc, 0.U, lsu.column)

    loadCFinishEvent.eventType := 2.U
    loadCFinishEvent.regId := reg
    loadCFinishEvent.fifoIdx := slotPayload.fifoIdx
    loadCFinishEvent.needMask := "b100".U
    loadCFinishEvent.row := row
    loadCFinishEvent.column := col
    loadCFinishEvent.transpose := Mux(op === TaskCtrlOpKind.ZeroAcc, false.B, lsu.transpose)
    loadCFinishEvent.isAcc := true.B
    loadCFinishEvent.slotId := owner
    loadCFinishEvent.seqId := slotPayload.seqId
    loadCFinishEventEn := true.B
  }

  when(storeDone) {
    val owner = Mux(trStorer0Done, fuTrStorer0.ownerSlot, fuAccStorer0.ownerSlot)
    val slotPayload = slotPayloads(owner)
    val lsu = decodeLsu(slotPayload.entry.ctrl)

    storeFinishEvent.eventType := 1.U
    storeFinishEvent.regId := lsu.ms(1, 0)
    storeFinishEvent.fifoIdx := slotPayload.fifoIdx
    storeFinishEvent.row := lsu.row
    storeFinishEvent.column := lsu.column
    storeFinishEvent.transpose := lsu.transpose
    storeFinishEvent.isAcc := lsu.isacc
    storeFinishEvent.slotId := owner
    storeFinishEvent.seqId := slotPayload.seqId
    storeFinishEventEn := true.B
  }

  // ===================== Release DiffTest alignment =====================
  if (EnableDifftest) {
    val releaseFinish = CuteDifftest(new DiffAmuFinishEvent, delay = 3, dontCare = true)
    val releaseIssueOwnerSlot = issueSlotIdx
    val releaseIssueOwnerEntry = issuePayload.entry

    releaseFinish.coreid := Mux(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release, releaseIssueOwnerEntry.ctrl.coreid.get, 0.U)
    releaseFinish.index := 1.U
    releaseFinish.valid := issueFire && issueSlot.opKind === TaskCtrlOpKind.Release
    releaseFinish.pc := Mux(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release, releaseIssueOwnerEntry.ctrl.pc.get, 0.U)

    when(issueFire && issueSlot.opKind === TaskCtrlOpKind.Release) {
      dontTouch(releaseIssueOwnerSlot)
    }
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
  matrixPrefetchDescTable.zip(io.matrixPrefetch).foreach { case (table, matrixPrefetch) =>
    table.log(
      matrixPrefetch.allocate.bits,
      matrixPrefetch.allocate.valid,
      "DescriptorAllocate",
      clock,
      reset
    )
  }
  for {
    table <- cStoreDescTable
    desc <- cStoreDesc
    enable <- cStoreDescEn
  } table.log(desc, enable, "StoreDescriptor", clock, reset)

  val mmaDoneType = decodeMmaComputeType(decodeMma(slotPayloads(fuCompute.ownerSlot).entry.ctrl))
  val releaseDone = issueFire && issueSlot.opKind === TaskCtrlOpKind.Release
  io.perfProbe.ownedWork := ownedWork
  io.perfProbe.retire := retireFire
  io.perfProbe.trLoader0Done := loadAFinishEventEn
  io.perfProbe.trLoader1Done := loadBFinishEventEn
  io.perfProbe.accLoader0Done := loadCFinishEventEn
  io.perfProbe.storeDone := storeFinishEventEn
  io.perfProbe.compDone := computeWriteCFinishEventEn
  io.perfProbe.releaseDone := releaseDone
  io.perfProbe.mmaNonfpDone := computeWriteCFinishEventEn && !decodeMma(slotPayloads(fuCompute.ownerSlot).entry.ctrl).isfp
  io.perfProbe.mmaFp16Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.F16F16F32)
  io.perfProbe.mmaBf16Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.BF16BF16F32)
  io.perfProbe.mmaTf32Done := computeWriteCFinishEventEn && (mmaDoneType === MteComputeType.TF32TF32F32)
  // Retain observer ABI: trLoader0/trLoader1 are physical TL0/TL1 activity, independent of opcode.
  io.perfProbe.trLoader0Active := fuTrLoader0.busy
  io.perfProbe.trLoader1Active := fuTrLoader1.busy
  io.perfProbe.accLoader0Active := fuAccLoader0.busy
  io.perfProbe.mteActive := fuCompute.busy
  io.perfProbe.accStorer0Active := fuAccStorer0.busy
}
