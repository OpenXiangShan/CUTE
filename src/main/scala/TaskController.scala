package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import cute.Bundles._
import cute.ElementDataType._
import difftest._
import utility.ChiselDB

class TaskControllerIO(implicit p: Parameters) extends CuteBundle {
  val ygjkctrl = Flipped(new YGJKControl)
  val ADC_MicroTask_Config = new ADCMicroTaskConfigIO
  val BDC_MicroTask_Config = new BDCMicroTaskConfigIO
  val ASC_MicroTask_Config = new ADCMicroTaskConfigIO
  val BSC_MicroTask_Config = new BDCMicroTaskConfigIO
  val CDC_MicroTask_Config = new CDCMicroTaskConfigIO
  val AML_MicroTask_Config = new AMLMicroTaskConfigIO
  val BML_MicroTask_Config = new BMLMicroTaskConfigIO
  val ASL_MicroTask_Config = new ASLMicroTaskConfigIO
  val BSL_MicroTask_Config = new BSLMicroTaskConfigIO
  val CML_MicroTask_Config = new CMLMicroTaskConfigIO
  val MTE_MicroTask_Config = new MTEMicroTaskConfigIO
  val DebugTimeStampe = Input(UInt(32.W))
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

class TaskController(implicit p: Parameters) extends BaseTaskController {
  import NewTaskController._

  dontTouch(io)

  io.ygjkctrl.mrelease.valid := false.B
  io.ygjkctrl.mrelease.bits := 0.U.asTypeOf(new MreleaseIO)

  // 默认输出赋值
  io.ADC_MicroTask_Config.ApplicationTensor_A.dataType := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.ADC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.ADC_MicroTask_Config.MatrixRegId := 0.U
  io.ADC_MicroTask_Config.Is_Transpose := false.B
  io.ADC_MicroTask_Config.MicroTaskValid := false.B
  io.ADC_MicroTask_Config.MicroTaskEndReady := false.B

  io.ASC_MicroTask_Config.ApplicationTensor_A.dataType := 0.U
  io.ASC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.ASC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.ASC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.ASC_MicroTask_Config.MatrixRegId := 0.U
  io.ASC_MicroTask_Config.Is_Transpose := false.B
  io.ASC_MicroTask_Config.MicroTaskValid := false.B
  io.ASC_MicroTask_Config.MicroTaskEndReady := false.B

  io.BDC_MicroTask_Config.ApplicationTensor_B.dataType := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BDC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BDC_MicroTask_Config.MatrixRegId := 0.U
  io.BDC_MicroTask_Config.Is_Transpose := false.B
  io.BDC_MicroTask_Config.MicroTaskValid := false.B
  io.BDC_MicroTask_Config.MicroTaskEndReady := false.B

  io.BSC_MicroTask_Config.ApplicationTensor_B.dataType := 0.U
  io.BSC_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.BSC_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BSC_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BSC_MicroTask_Config.MatrixRegId := 0.U
  io.BSC_MicroTask_Config.Is_Transpose := false.B
  io.BSC_MicroTask_Config.MicroTaskValid := false.B
  io.BSC_MicroTask_Config.MicroTaskEndReady := false.B

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

  io.AML_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.AML_MicroTask_Config.ApplicationTensor_A)
  io.AML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.AML_MicroTask_Config.LoadTaskInfo)
  io.AML_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.AML_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.AML_MicroTask_Config.Conherent := false.B
  io.AML_MicroTask_Config.MatrixRegId := 0.U
  io.AML_MicroTask_Config.MicroTaskValid := false.B
  io.AML_MicroTask_Config.MicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.AML_MicroTask_Config.pc.get := 0.U
    io.AML_MicroTask_Config.coreid.get := 0.U
  }

  io.ASL_MicroTask_Config.ApplicationScale_A := 0.U.asTypeOf(io.ASL_MicroTask_Config.ApplicationScale_A)
  io.ASL_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.ASL_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.ASL_MicroTask_Config.Conherent := false.B
  io.ASL_MicroTask_Config.MicroTaskValid := false.B
  io.ASL_MicroTask_Config.MicroTaskEndReady := false.B

  io.BML_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BML_MicroTask_Config.ApplicationTensor_B)
  io.BML_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BML_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BML_MicroTask_Config.Conherent := false.B
  io.BML_MicroTask_Config.MatrixRegId := 0.U
  io.BML_MicroTask_Config.MicroTaskValid := false.B
  io.BML_MicroTask_Config.MicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.BML_MicroTask_Config.pc.get := 0.U
    io.BML_MicroTask_Config.coreid.get := 0.U
  }

  io.BSL_MicroTask_Config.ApplicationScale_B := 0.U.asTypeOf(io.BSL_MicroTask_Config.ApplicationScale_B)
  io.BSL_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.BSL_MicroTask_Config.MatrixRegTensor_K := 0.U
  io.BSL_MicroTask_Config.Conherent := false.B
  io.BSL_MicroTask_Config.MicroTaskValid := false.B
  io.BSL_MicroTask_Config.MicroTaskEndReady := false.B

  io.CML_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_C)
  io.CML_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_D)
  io.CML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.LoadTaskInfo)
  io.CML_MicroTask_Config.StoreTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.StoreTaskInfo)
  io.CML_MicroTask_Config.Conherent := false.B
  io.CML_MicroTask_Config.Is_Transpose := false.B
  io.CML_MicroTask_Config.MatrixRegTensor_M := 0.U
  io.CML_MicroTask_Config.MatrixRegTensor_N := 0.U
  io.CML_MicroTask_Config.MatrixRegId := 0.U
  io.CML_MicroTask_Config.IsLoadMicroTask := false.B
  io.CML_MicroTask_Config.IsStoreMicroTask := false.B
  io.CML_MicroTask_Config.MicroTaskValid := false.B
  io.CML_MicroTask_Config.MicroTaskEndReady := false.B
  if (EnableDifftest) {
    io.CML_MicroTask_Config.pc.get := 0.U
    io.CML_MicroTask_Config.coreid.get := 0.U
  }

  // Scoreboard实例
  private val scoreboard = Module(new Scoreboard)

  // ===================== ChiselDB 事件定义 =====================
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
  }

  class StoreEventEntry extends Bundle {
    val eventType = UInt(3.W)
    val regId = UInt(4.W)
    val fifoIdx = UInt(StoreFifoIdxWidth.W)
    val row = UInt(TileDimWidth.W)
    val column = UInt(TileDimWidth.W)
    val transpose = Bool()
    val isAcc = Bool()
  }

  class ReleaseEventEntry extends Bundle {
    val eventType = UInt(2.W)
    val token = UInt(5.W)
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

  scoreboard.io.update.load_allocate := false.B
  scoreboard.io.update.load_alloc_a_reg := 0.U
  scoreboard.io.update.load_alloc_b_reg := 0.U
  scoreboard.io.update.load_alloc_c_reg := 0.U
  scoreboard.io.update.load_alloc_has_a := false.B
  scoreboard.io.update.load_alloc_has_b := false.B
  scoreboard.io.update.load_alloc_has_c := false.B
  scoreboard.io.update.load_alloc_fifo_idx := 0.U

  scoreboard.io.update.load_finish_a := false.B
  scoreboard.io.update.load_finish_a_reg := 0.U
  scoreboard.io.update.load_finish_b := false.B
  scoreboard.io.update.load_finish_b_reg := 0.U
  scoreboard.io.update.load_finish_c := false.B
  scoreboard.io.update.load_finish_c_reg := 0.U

  scoreboard.io.update.compute_issue := false.B
  scoreboard.io.update.compute_issue_a_reg := 0.U
  scoreboard.io.update.compute_issue_b_reg := 0.U
  scoreboard.io.update.compute_issue_c_reg := 0.U
  scoreboard.io.update.compute_issue_fifo_idx := 0.U

  scoreboard.io.update.compute_read_finish_a := false.B
  scoreboard.io.update.compute_read_finish_a_reg := 0.U
  scoreboard.io.update.compute_read_finish_b := false.B
  scoreboard.io.update.compute_read_finish_b_reg := 0.U

  scoreboard.io.update.compute_write_finish_c := false.B
  scoreboard.io.update.compute_write_finish_c_reg := 0.U

  scoreboard.io.update.store_issue := false.B
  scoreboard.io.update.store_issue_c_reg := 0.U
  scoreboard.io.update.store_issue_fifo_idx := 0.U

  scoreboard.io.update.store_finish := false.B
  scoreboard.io.update.store_finish_c_reg := 0.U

  // Pending bookkeeping for outstanding micro tasks
  val loadAllocIdx = RegInit(0.U(2.W))
  val computeIssueIdx = RegInit(0.U(2.W))
  val storeIssueIdx = RegInit(0.U(2.W))

  val pendingLoadA = RegInit(false.B)
  val pendingLoadAReg = RegInit(0.U(2.W))
  val pendingLoadAFifoIdx = RegInit(0.U(LoadFifoIdxWidth.W))
  val pendingLoadB = RegInit(false.B)
  val pendingLoadBReg = RegInit(0.U(2.W))
  val pendingLoadBFifoIdx = RegInit(0.U(LoadFifoIdxWidth.W))
  val pendingLoadC = RegInit(false.B)
  val pendingLoadCReg = RegInit(0.U(2.W))
  val pendingLoadCFifoIdx = RegInit(0.U(LoadFifoIdxWidth.W))
  val pendingLoadRow = RegInit(0.U(TileDimWidth.W))
  val pendingLoadColumn = RegInit(0.U(TileDimWidth.W))
  val pendingLoadTranspose = RegInit(false.B)

  val pendingComputeA = RegInit(false.B)
  val pendingComputeAReg = RegInit(0.U(2.W))
  val pendingComputeAFifoIdx = RegInit(0.U(ComputeFifoIdxWidth.W))
  val pendingComputeB = RegInit(false.B)
  val pendingComputeBReg = RegInit(0.U(2.W))
  val pendingComputeBFifoIdx = RegInit(0.U(ComputeFifoIdxWidth.W))
  val pendingComputeC = RegInit(false.B)
  val pendingComputeCReg = RegInit(0.U(2.W))
  val pendingComputeCFifoIdx = RegInit(0.U(ComputeFifoIdxWidth.W))
  val pendingComputeM = RegInit(0.U(TileDimWidth.W))
  val pendingComputeN = RegInit(0.U(TileDimWidth.W))
  val pendingComputeK = RegInit(0.U(TileDimWidth.W))
  val pendingComputeIsMma = RegInit(false.B)
  val pendingComputeIsFp = RegInit(false.B)

  val pendingStore = RegInit(false.B)
  val pendingStoreReg = RegInit(0.U(2.W))
  val pendingStoreFifoIdx = RegInit(0.U(StoreFifoIdxWidth.W))
  val pendingStoreRow = RegInit(0.U(TileDimWidth.W))
  val pendingStoreColumn = RegInit(0.U(TileDimWidth.W))
  val pendingStoreTranspose = RegInit(false.B)
  val pendingStoreIsAcc = RegInit(false.B)

  // Completion handshakes and scoreboard updates
  io.AML_MicroTask_Config.MicroTaskEndReady := pendingLoadA
  when(pendingLoadA && io.AML_MicroTask_Config.MicroTaskEndValid) {
    scoreboard.io.update.load_finish_a := true.B
    scoreboard.io.update.load_finish_a_reg := pendingLoadAReg
    pendingLoadA := false.B
    loadAFinishEvent.eventType := 2.U
    loadAFinishEvent.regId := pendingLoadAReg
    loadAFinishEvent.fifoIdx := pendingLoadAFifoIdx
    loadAFinishEvent.needMask := "b001".U
    loadAFinishEvent.row := pendingLoadRow
    loadAFinishEvent.column := pendingLoadColumn
    loadAFinishEvent.transpose := pendingLoadTranspose
    loadAFinishEvent.isAcc := false.B
    loadAFinishEventEn := true.B
  }

  io.BML_MicroTask_Config.MicroTaskEndReady := pendingLoadB
  when(pendingLoadB && io.BML_MicroTask_Config.MicroTaskEndValid) {
    scoreboard.io.update.load_finish_b := true.B
    scoreboard.io.update.load_finish_b_reg := pendingLoadBReg
    pendingLoadB := false.B
    loadBFinishEvent.eventType := 2.U
    loadBFinishEvent.regId := pendingLoadBReg
    loadBFinishEvent.fifoIdx := pendingLoadBFifoIdx
    loadBFinishEvent.needMask := "b010".U
    loadBFinishEvent.row := pendingLoadRow
    loadBFinishEvent.column := pendingLoadColumn
    loadBFinishEvent.transpose := pendingLoadTranspose
    loadBFinishEvent.isAcc := false.B
    loadBFinishEventEn := true.B
  }

  io.CML_MicroTask_Config.MicroTaskEndReady := pendingLoadC || pendingStore
  when(io.CML_MicroTask_Config.MicroTaskEndValid) {
    when(pendingLoadC) {
      scoreboard.io.update.load_finish_c := true.B
      scoreboard.io.update.load_finish_c_reg := pendingLoadCReg
      pendingLoadC := false.B
      loadCFinishEvent.eventType := 2.U
      loadCFinishEvent.regId := pendingLoadCReg
      loadCFinishEvent.fifoIdx := pendingLoadCFifoIdx
      loadCFinishEvent.needMask := "b100".U
      loadCFinishEvent.row := pendingLoadRow
      loadCFinishEvent.column := pendingLoadColumn
      loadCFinishEvent.transpose := pendingLoadTranspose
      loadCFinishEvent.isAcc := true.B
      loadCFinishEventEn := true.B
    }.elsewhen(pendingStore) {
      scoreboard.io.update.store_finish := true.B
      scoreboard.io.update.store_finish_c_reg := pendingStoreReg
      pendingStore := false.B
      storeFinishEvent.eventType := 1.U
      storeFinishEvent.regId := pendingStoreReg
      storeFinishEvent.fifoIdx := pendingStoreFifoIdx
      storeFinishEvent.row := pendingStoreRow
      storeFinishEvent.column := pendingStoreColumn
      storeFinishEvent.transpose := pendingStoreTranspose
      storeFinishEvent.isAcc := pendingStoreIsAcc
      storeFinishEventEn := true.B
    }
  }

  io.ADC_MicroTask_Config.MicroTaskEndReady := pendingComputeA
  when(pendingComputeA && io.ADC_MicroTask_Config.MicroTaskEndValid) {
    scoreboard.io.update.compute_read_finish_a := true.B
    scoreboard.io.update.compute_read_finish_a_reg := pendingComputeAReg
    pendingComputeA := false.B
    computeReadAFinishEvent.eventType := 1.U
    computeReadAFinishEvent.aReg := pendingComputeAReg
    computeReadAFinishEvent.bReg := pendingComputeBReg
    computeReadAFinishEvent.cReg := pendingComputeCReg
    computeReadAFinishEvent.fifoIdx := pendingComputeAFifoIdx
    computeReadAFinishEvent.mtilem := pendingComputeM
    computeReadAFinishEvent.mtilen := pendingComputeN
    computeReadAFinishEvent.mtilek := pendingComputeK
    computeReadAFinishEvent.isMma := pendingComputeIsMma
    computeReadAFinishEvent.isFp := pendingComputeIsFp
    computeReadAFinishEventEn := true.B
  }

  io.BDC_MicroTask_Config.MicroTaskEndReady := pendingComputeB
  when(pendingComputeB && io.BDC_MicroTask_Config.MicroTaskEndValid) {
    scoreboard.io.update.compute_read_finish_b := true.B
    scoreboard.io.update.compute_read_finish_b_reg := pendingComputeBReg
    pendingComputeB := false.B
    computeReadBFinishEvent.eventType := 2.U
    computeReadBFinishEvent.aReg := pendingComputeAReg
    computeReadBFinishEvent.bReg := pendingComputeBReg
    computeReadBFinishEvent.cReg := pendingComputeCReg
    computeReadBFinishEvent.fifoIdx := pendingComputeBFifoIdx
    computeReadBFinishEvent.mtilem := pendingComputeM
    computeReadBFinishEvent.mtilen := pendingComputeN
    computeReadBFinishEvent.mtilek := pendingComputeK
    computeReadBFinishEvent.isMma := pendingComputeIsMma
    computeReadBFinishEvent.isFp := pendingComputeIsFp
    computeReadBFinishEventEn := true.B
  }

  io.CDC_MicroTask_Config.MicroTaskEndReady := pendingComputeC
  when(pendingComputeC && io.CDC_MicroTask_Config.MicroTaskEndValid) {
    scoreboard.io.update.compute_write_finish_c := true.B
    scoreboard.io.update.compute_write_finish_c_reg := pendingComputeCReg
    pendingComputeC := false.B
    computeWriteCFinishEvent.eventType := 3.U
    computeWriteCFinishEvent.aReg := pendingComputeAReg
    computeWriteCFinishEvent.bReg := pendingComputeBReg
    computeWriteCFinishEvent.cReg := pendingComputeCReg
    computeWriteCFinishEvent.fifoIdx := pendingComputeCFifoIdx
    computeWriteCFinishEvent.mtilem := pendingComputeM
    computeWriteCFinishEvent.mtilen := pendingComputeN
    computeWriteCFinishEvent.mtilek := pendingComputeK
    computeWriteCFinishEvent.isMma := pendingComputeIsMma
    computeWriteCFinishEvent.isFp := pendingComputeIsFp
    computeWriteCFinishEventEn := true.B
  }

  // 解码后的指令FIFO
  private val decodedFifo = Module(new Queue(new DecodedAmuCtrlEntry, DecodedAmuCtrlFIFODepth))

  // FIFO出队暂未使用
  decodedFifo.io.deq.ready := false.B

  // AMU指令译码
  decodedFifo.io.enq.valid := io.ygjkctrl.amuCtrl.valid
  io.ygjkctrl.amuCtrl.ready := decodedFifo.io.enq.ready

  val amuCtrlBits = io.ygjkctrl.amuCtrl.bits

  val entry = Wire(new DecodedAmuCtrlEntry)
  entry.ctrl := amuCtrlBits

  // 默认清零
  for (i <- 0 until MaxReadRegs) {
    entry.readRegs(i) := 0.U
    entry.readValid(i) := false.B
  }
  for (i <- 0 until MaxWriteRegs) {
    entry.writeRegs(i) := 0.U
    entry.writeValid(i) := false.B
  }

  when(amuCtrlBits.isMma()) {
    val mma = amuCtrlBits.data.asTypeOf(new AmuMmaIO)
    entry.readRegs(0) := mma.ms1
    entry.readRegs(1) := mma.ms2
    entry.readRegs(2) := mma.md
    entry.readValid(0) := true.B
    entry.readValid(1) := true.B
    entry.readValid(2) := true.B
    entry.writeRegs(0) := mma.md
    entry.writeValid(0) := true.B
  }
  when(amuCtrlBits.isMls()) {
    val lsu = amuCtrlBits.data.asTypeOf(new AmuLsuIO)
    when(lsu.ls === 0.U) { // Load: 写寄存器
      entry.writeRegs(0) := lsu.ms
      entry.writeValid(0) := true.B
    }.otherwise { // Store: 读寄存器
      entry.readRegs(0) := lsu.ms
      entry.readValid(0) := true.B
    }
  }
  when(amuCtrlBits.isArith()) {
    val arith = amuCtrlBits.data.asTypeOf(new AmuArithIO)
    entry.readRegs(0) := arith.md
    entry.readValid(0) := true.B
    entry.writeRegs(0) := arith.md
    entry.writeValid(0) := true.B
  }

  decodedFifo.io.enq.bits := entry

  // 仅查询队首指令能否发射
  val headValid = decodedFifo.io.deq.valid
  val headEntry = decodedFifo.io.deq.bits

  val isMma = headEntry.ctrl.isMma()
  val isArith = headEntry.ctrl.isArith()
  val isLsu = headEntry.ctrl.isMls()
  val isRelease = headEntry.ctrl.isRelease()

  val lsuInfo = headEntry.ctrl.data.asTypeOf(new AmuLsuIO)
  val mmaInfo = headEntry.ctrl.data.asTypeOf(new AmuMmaIO)
  val arithInfo = headEntry.ctrl.data.asTypeOf(new AmuArithIO)
  val releaseInfo = headEntry.ctrl.data.asTypeOf(new AmuReleaseIO)

  val isLoad = isLsu && headEntry.writeValid(0)
  val isStore = isLsu && !headEntry.writeValid(0) && headEntry.readValid(0)
  val arithDestIsAcc = arithInfo.md(2) === 1.U
  val isMzeroAcc = isArith && arithDestIsAcc
  val isMzeroTr = isArith && !arithDestIsAcc

  val scoreboardReq = WireInit(0.U.asTypeOf(new QueryReq))
  val scoreboardReqValid = WireInit(false.B)
  when(headValid) {
    when(isLoad) {
      scoreboardReqValid := true.B
      val fuType = MuxCase(ScoreboardFuType.AML, Seq(
        lsuInfo.isacc -> ScoreboardFuType.CML,
        lsuInfo.isA -> ScoreboardFuType.AML,
        lsuInfo.isB -> ScoreboardFuType.BML
      ))
      scoreboardReq.fuType := fuType
      scoreboardReq.dest.valid := true.B
      scoreboardReq.dest.bits.is_acc := lsuInfo.isacc
      scoreboardReq.dest.bits.accept(lsuInfo.ms)
    }
    when(isMma) {
      scoreboardReqValid := true.B
      scoreboardReq.fuType := ScoreboardFuType.Compute
      scoreboardReq.dest.valid := true.B
      scoreboardReq.dest.bits.is_acc := true.B
      scoreboardReq.dest.bits.accept(mmaInfo.md)
      scoreboardReq.src1.valid := true.B
      scoreboardReq.src1.bits.is_acc := false.B
      scoreboardReq.src1.bits.accept(mmaInfo.ms1)
      scoreboardReq.src2.valid := true.B
      scoreboardReq.src2.bits.is_acc := false.B
      scoreboardReq.src2.bits.accept(mmaInfo.ms2)
      scoreboardReq.src3.valid := true.B
      scoreboardReq.src3.bits.is_acc := true.B
      scoreboardReq.src3.bits.accept(mmaInfo.md)
    }
    when(isArith) {
      scoreboardReqValid := true.B
      scoreboardReq.fuType := Mux(arithDestIsAcc, ScoreboardFuType.CML, ScoreboardFuType.AML)
      scoreboardReq.dest.valid := true.B
      scoreboardReq.dest.bits.is_acc := arithDestIsAcc
      scoreboardReq.dest.bits.accept(arithInfo.md)
    }
    when(isStore) {
      scoreboardReqValid := true.B
      scoreboardReq.fuType := ScoreboardFuType.CML
      scoreboardReq.src1.valid := true.B
      scoreboardReq.src1.bits.is_acc := lsuInfo.isacc
      scoreboardReq.src1.bits.accept(lsuInfo.ms)
    }
  }

  scoreboard.io.query.req.valid := scoreboardReqValid
  scoreboard.io.query.req.bits := scoreboardReq

  val mmaUnitsReady = io.ADC_MicroTask_Config.MicroTaskReady &&
    io.BDC_MicroTask_Config.MicroTaskReady &&
    io.CDC_MicroTask_Config.MicroTaskReady
  val storeUnitsReady = io.CML_MicroTask_Config.MicroTaskReady
  val zeroAccUnitsReady = io.CML_MicroTask_Config.MicroTaskReady
  val zeroTrUnitsReady = io.AML_MicroTask_Config.MicroTaskReady

  val needA = isLoad && lsuInfo.isA
  val needB = isLoad && lsuInfo.isB
  val needC = isLoad && lsuInfo.isacc

  val loadUnitsReady = (!needA || io.AML_MicroTask_Config.MicroTaskReady) &&
    (!needB || io.BML_MicroTask_Config.MicroTaskReady) &&
    (!needC || io.CML_MicroTask_Config.MicroTaskReady)

  // val storeReadersEmpty = scoreboard.io.debug.c_reg_reader_counts.map(_ === 0.U).reduce(_ && _)
  val releaseReady = !pendingStore // && storeReadersEmpty

  val scoreboardReqReady = !scoreboardReqValid || scoreboard.io.query.req.ready

  val headReady = MuxCase(true.B, Seq(
    (isLoad) -> (scoreboardReqReady && loadUnitsReady),
    (isStore) -> (scoreboardReqReady && storeUnitsReady),
    (isMma) -> (scoreboardReqReady && mmaUnitsReady),
    (isMzeroAcc) -> (scoreboardReqReady && zeroAccUnitsReady),
    (isMzeroTr) -> (scoreboardReqReady && zeroTrUnitsReady),
    (isRelease) -> releaseReady
  ))

  decodedFifo.io.deq.ready := headValid && headReady

  val issueFire = decodedFifo.io.deq.fire
  val issueLoad = issueFire && isLoad
  val issueStore = issueFire && isStore
  val issueMma = issueFire && isMma
  val issueZeroAcc = issueFire && isMzeroAcc
  val issueZeroTr = issueFire && isMzeroTr
  val issueRelease = issueFire && isRelease

  when(issueLoad) {
    val regIdx = lsuInfo.ms(1, 0)
    val loadIdx = loadAllocIdx
    when(needA) {
      io.AML_MicroTask_Config.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := lsuInfo.baseAddr
      io.AML_MicroTask_Config.ApplicationTensor_A.ApplicationTensor_A_Stride_M := lsuInfo.stride
      io.AML_MicroTask_Config.ApplicationTensor_A.dataType := MuxLookup(lsuInfo.widths, ElementDataType.DataTypeWidth32)(Seq(
        Bundles.MSew.e8 -> ElementDataType.DataTypeWidth8,
        Bundles.MSew.e16 -> ElementDataType.DataTypeWidth16,
        Bundles.MSew.e32 -> ElementDataType.DataTypeWidth32,
        Bundles.MSew.e4 -> ElementDataType.DataTypeWidth4
      ))
      io.AML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := true.B
      io.AML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := false.B
      io.AML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
      io.AML_MicroTask_Config.MatrixRegTensor_M := lsuInfo.row
      io.AML_MicroTask_Config.MatrixRegTensor_K := MuxLookup(lsuInfo.widths, lsuInfo.column)(Seq(
        Bundles.MSew.e8 -> lsuInfo.column,
        Bundles.MSew.e16 -> lsuInfo.column * 2.U,
        Bundles.MSew.e32 -> lsuInfo.column * 4.U,
        Bundles.MSew.e4 -> lsuInfo.column / 2.U 
      )) / ReduceWidthByte.U
      io.AML_MicroTask_Config.MatrixRegId := regIdx
      if (EnableDifftest) {
        io.AML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
        io.AML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
      }

      io.AML_MicroTask_Config.Conherent := true.B

      io.AML_MicroTask_Config.MicroTaskValid := true.B
      
      pendingLoadA := true.B
      pendingLoadAReg := regIdx
      pendingLoadAFifoIdx := loadIdx
    }
    when(needB) {
      io.BML_MicroTask_Config.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := lsuInfo.baseAddr
      io.BML_MicroTask_Config.ApplicationTensor_B.ApplicationTensor_B_Stride_N := lsuInfo.stride
      io.BML_MicroTask_Config.ApplicationTensor_B.BlockTensor_B_BaseVaddr := lsuInfo.baseAddr
      io.BML_MicroTask_Config.ApplicationTensor_B.dataType := MuxLookup(lsuInfo.widths, ElementDataType.DataTypeWidth32)(Seq(
        Bundles.MSew.e8 -> ElementDataType.DataTypeWidth8,
        Bundles.MSew.e16 -> ElementDataType.DataTypeWidth16,
        Bundles.MSew.e32 -> ElementDataType.DataTypeWidth32,
        Bundles.MSew.e4 -> ElementDataType.DataTypeWidth4
      ))
      io.BML_MicroTask_Config.MatrixRegTensor_N := lsuInfo.column
      io.BML_MicroTask_Config.MatrixRegTensor_K := MuxLookup(lsuInfo.widths, lsuInfo.row)(Seq(
        Bundles.MSew.e8 -> lsuInfo.row,
        Bundles.MSew.e16 -> lsuInfo.row * 2.U,
        Bundles.MSew.e32 -> lsuInfo.row * 4.U,
        Bundles.MSew.e4 -> lsuInfo.row / 2.U 
      )) / ReduceWidthByte.U
      io.BML_MicroTask_Config.MatrixRegId := regIdx
      if (EnableDifftest) {
        io.BML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
        io.BML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
      }
      io.BML_MicroTask_Config.Conherent := true.B
      io.BML_MicroTask_Config.MicroTaskValid := true.B
      pendingLoadB := true.B
      pendingLoadBReg := regIdx
      pendingLoadBFifoIdx := loadIdx
    }
    when(needC) {
      io.CML_MicroTask_Config.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := lsuInfo.baseAddr
      io.CML_MicroTask_Config.ApplicationTensor_C.ApplicationTensor_C_Stride_M := lsuInfo.stride
      io.CML_MicroTask_Config.ApplicationTensor_C.BlockTensor_C_BaseVaddr := lsuInfo.baseAddr
      io.CML_MicroTask_Config.ApplicationTensor_C.dataType := MuxLookup(lsuInfo.widths, ElementDataType.DataTypeWidth32)(Seq(
        Bundles.MSew.e8 -> ElementDataType.DataTypeWidth8,
        Bundles.MSew.e16 -> ElementDataType.DataTypeWidth16,
        Bundles.MSew.e32 -> ElementDataType.DataTypeWidth32,
        Bundles.MSew.e4 -> ElementDataType.DataTypeWidth4
      ))
      io.CML_MicroTask_Config.Conherent := true.B
      io.CML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := true.B
      io.CML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := false.B
      io.CML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
      io.CML_MicroTask_Config.MatrixRegTensor_M := lsuInfo.row
      io.CML_MicroTask_Config.MatrixRegTensor_N := lsuInfo.column
      io.CML_MicroTask_Config.MatrixRegId := regIdx
      if (EnableDifftest) {
        io.CML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
        io.CML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
      }
      io.CML_MicroTask_Config.IsLoadMicroTask := true.B
      io.CML_MicroTask_Config.IsStoreMicroTask := false.B
      io.CML_MicroTask_Config.MicroTaskValid := true.B
      io.CML_MicroTask_Config.Is_Transpose := lsuInfo.transpose
      
      pendingLoadC := true.B
      pendingLoadCReg := regIdx
      pendingLoadCFifoIdx := loadIdx
    }

    when(needA || needB || needC) {
      scoreboard.io.update.load_allocate := true.B
      scoreboard.io.update.load_alloc_fifo_idx := loadIdx
      scoreboard.io.update.load_alloc_a_reg := Mux(needA, regIdx, 0.U)
      scoreboard.io.update.load_alloc_b_reg := Mux(needB, regIdx, 0.U)
      scoreboard.io.update.load_alloc_c_reg := Mux(needC, regIdx, 0.U)
      scoreboard.io.update.load_alloc_has_a := needA
      scoreboard.io.update.load_alloc_has_b := needB
      scoreboard.io.update.load_alloc_has_c := needC
      loadAllocIdx := loadAllocIdx + 1.U
      pendingLoadRow := lsuInfo.row
      pendingLoadColumn := lsuInfo.column
      pendingLoadTranspose := lsuInfo.transpose

      loadAllocateEvent.eventType := 0.U
      loadAllocateEvent.regId := regIdx
      loadAllocateEvent.fifoIdx := loadIdx
      loadAllocateEvent.needMask := Cat(needC.asUInt, needB.asUInt, needA.asUInt)
      loadAllocateEvent.row := lsuInfo.row
      loadAllocateEvent.column := lsuInfo.column
      loadAllocateEvent.transpose := lsuInfo.transpose
      loadAllocateEvent.isAcc := lsuInfo.isacc
      loadAllocateEventEn := true.B
    }

    loadIssueEvent.eventType := 1.U
    loadIssueEvent.regId := regIdx
    loadIssueEvent.fifoIdx := loadIdx
    loadIssueEvent.needMask := Cat(needC.asUInt, needB.asUInt, needA.asUInt)
    loadIssueEvent.row := lsuInfo.row
    loadIssueEvent.column := lsuInfo.column
    loadIssueEvent.transpose := lsuInfo.transpose
    loadIssueEvent.isAcc := lsuInfo.isacc
    loadIssueEventEn := true.B
  }

  when(issueZeroAcc) {
    val regIdx = arithInfo.md(1, 0)
    val loadIdx = loadAllocIdx

    io.CML_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
    io.CML_MicroTask_Config.MatrixRegTensor_M := cuteParams.Tensor_MN.U
    io.CML_MicroTask_Config.MatrixRegTensor_N := cuteParams.Tensor_MN.U
    io.CML_MicroTask_Config.MatrixRegId := regIdx
    io.CML_MicroTask_Config.IsLoadMicroTask := true.B
    io.CML_MicroTask_Config.IsStoreMicroTask := false.B
    io.CML_MicroTask_Config.MicroTaskValid := true.B
    io.CML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := true.B
    io.CML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := false.B
    io.CML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
    io.CML_MicroTask_Config.Conherent := true.B
    io.CML_MicroTask_Config.Is_Transpose := false.B
    if (EnableDifftest) {
      io.CML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
      io.CML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
    }

    scoreboard.io.update.load_allocate := true.B
    scoreboard.io.update.load_alloc_fifo_idx := loadIdx
    scoreboard.io.update.load_alloc_a_reg := 0.U
    scoreboard.io.update.load_alloc_b_reg := 0.U
    scoreboard.io.update.load_alloc_c_reg := regIdx
    scoreboard.io.update.load_alloc_has_a := false.B
    scoreboard.io.update.load_alloc_has_b := false.B
    scoreboard.io.update.load_alloc_has_c := true.B
    loadAllocIdx := loadAllocIdx + 1.U

    pendingLoadC := true.B
    pendingLoadCReg := regIdx
    pendingLoadCFifoIdx := loadIdx
    pendingLoadRow := 0.U
    pendingLoadColumn := 0.U
    pendingLoadTranspose := false.B

    loadAllocateEvent.eventType := 0.U
    loadAllocateEvent.regId := regIdx
    loadAllocateEvent.fifoIdx := loadIdx
    loadAllocateEvent.needMask := "b100".U
    loadAllocateEvent.row := 0.U
    loadAllocateEvent.column := 0.U
    loadAllocateEvent.transpose := false.B
    loadAllocateEvent.isAcc := true.B
    loadAllocateEventEn := true.B

    loadIssueEvent.eventType := 1.U
    loadIssueEvent.regId := regIdx
    loadIssueEvent.fifoIdx := loadIdx
    loadIssueEvent.needMask := "b100".U
    loadIssueEvent.row := 0.U
    loadIssueEvent.column := 0.U
    loadIssueEvent.transpose := false.B
    loadIssueEvent.isAcc := true.B
    loadIssueEventEn := true.B
  }

  when(issueZeroTr) {
    val regIdx = arithInfo.md(1, 0)
    val loadIdx = loadAllocIdx

    io.AML_MicroTask_Config.ApplicationTensor_A.dataType := ElementDataType.DataTypeWidth8
    io.AML_MicroTask_Config.MatrixRegTensor_M := cuteParams.Tensor_MN.U
    io.AML_MicroTask_Config.MatrixRegTensor_K := cuteParams.Tensor_K.U / ReduceWidthByte.U
    io.AML_MicroTask_Config.MatrixRegId := regIdx
    io.AML_MicroTask_Config.MicroTaskValid := true.B
    io.AML_MicroTask_Config.LoadTaskInfo.Is_ZeroLoad := true.B
    io.AML_MicroTask_Config.LoadTaskInfo.Is_FullLoad := false.B
    io.AML_MicroTask_Config.LoadTaskInfo.Is_RepeatRowLoad := false.B
    io.AML_MicroTask_Config.Conherent := true.B
    if (EnableDifftest) {
      io.AML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
      io.AML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
    }

    scoreboard.io.update.load_allocate := true.B
    scoreboard.io.update.load_alloc_fifo_idx := loadIdx
    scoreboard.io.update.load_alloc_a_reg := regIdx
    scoreboard.io.update.load_alloc_b_reg := 0.U
    scoreboard.io.update.load_alloc_c_reg := 0.U
    scoreboard.io.update.load_alloc_has_a := true.B
    scoreboard.io.update.load_alloc_has_b := false.B
    scoreboard.io.update.load_alloc_has_c := false.B
    loadAllocIdx := loadAllocIdx + 1.U

    pendingLoadA := true.B
    pendingLoadAReg := regIdx
    pendingLoadAFifoIdx := loadIdx
    pendingLoadRow := 0.U
    pendingLoadColumn := 0.U
    pendingLoadTranspose := false.B

    loadAllocateEvent.eventType := 0.U
    loadAllocateEvent.regId := regIdx
    loadAllocateEvent.fifoIdx := loadIdx
    loadAllocateEvent.needMask := "b100".U
    loadAllocateEvent.row := 0.U
    loadAllocateEvent.column := 0.U
    loadAllocateEvent.transpose := false.B
    loadAllocateEvent.isAcc := false.B
    loadAllocateEventEn := true.B

    loadIssueEvent.eventType := 1.U
    loadIssueEvent.regId := regIdx
    loadIssueEvent.fifoIdx := loadIdx
    loadIssueEvent.needMask := "b100".U
    loadIssueEvent.row := 0.U
    loadIssueEvent.column := 0.U
    loadIssueEvent.transpose := false.B
    loadIssueEvent.isAcc := false.B
    loadIssueEventEn := true.B
  }

  val mmaDataType = WireInit(0.U.asTypeOf(io.MTE_MicroTask_Config.dataType))
  io.MTE_MicroTask_Config.dataType := mmaDataType  // Latch inside MTE.
  io.MTE_MicroTask_Config.MicroTaskValid := issueMma

  when(issueMma) {
    val aReg = Mux(isMma, mmaInfo.ms1(1, 0), arithInfo.md(1, 0))
    val bReg = Mux(isMma, mmaInfo.ms2(1, 0), arithInfo.md(1, 0))
    val cReg = Mux(isMma, mmaInfo.md(1, 0), arithInfo.md(1, 0))
    val computeIdx = computeIssueIdx

    io.ADC_MicroTask_Config.MicroTaskValid := true.B
    io.BDC_MicroTask_Config.MicroTaskValid := true.B
    io.CDC_MicroTask_Config.MicroTaskValid := true.B

    val mVal = mmaInfo.mtilem
    val nVal = mmaInfo.mtilen
    // val kVal = mmaInfo.mtilek
    val kVal = MuxLookup(mmaInfo.types1(1, 0), mmaInfo.mtilek)(Seq(
      Bundles.MSew.e8 -> mmaInfo.mtilek,
      Bundles.MSew.e16 -> mmaInfo.mtilek * 2.U,
      Bundles.MSew.e32 -> mmaInfo.mtilek * 4.U,
      Bundles.MSew.e4 -> mmaInfo.mtilek / 2.U,
    ))

    io.ADC_MicroTask_Config.ApplicationTensor_A.dataType := ElementDataType.DataTypeWidth8
    io.ADC_MicroTask_Config.MatrixRegTensor_M := mVal
    io.ADC_MicroTask_Config.MatrixRegTensor_N := nVal
    io.ADC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U // TODO: It's not hardware-friendly, but it's ok for now
    io.ADC_MicroTask_Config.MatrixRegId := aReg
    io.ADC_MicroTask_Config.Is_Transpose := false.B

    io.BDC_MicroTask_Config.ApplicationTensor_B.dataType := ElementDataType.DataTypeWidth8
    io.BDC_MicroTask_Config.MatrixRegTensor_M := mVal
    io.BDC_MicroTask_Config.MatrixRegTensor_N := nVal
    io.BDC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U // TODO: It's not hardware-friendly, but it's ok for now
    io.BDC_MicroTask_Config.MatrixRegId := bReg
    io.BDC_MicroTask_Config.Is_Transpose := false.B

    io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
    io.CDC_MicroTask_Config.MatrixRegTensor_M := mVal
    io.CDC_MicroTask_Config.MatrixRegTensor_N := nVal
    io.CDC_MicroTask_Config.MatrixRegTensor_K := kVal / ReduceWidthByte.U // TODO: It's not hardware-friendly, but it's ok for now
    io.CDC_MicroTask_Config.MatrixRegId := cReg
    io.CDC_MicroTask_Config.Is_Transpose := false.B
    io.CDC_MicroTask_Config.Is_AfterOps_Tile := false.B
    if (EnableDifftest) {
      io.CDC_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
      io.CDC_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
    }

    /* 
        types1, types2, typed encoding refereence
          output.typed  := Cat(
    MmulOpType.isFloat(realFuOpType) && MmulOpType.isToE16(realFuOpType) && MmulOpType.isBf16(realFuOpType),
    MmulOpType.getToType(realFuOpType)
  )
  output.types1  := Cat(
    Mux(MmulOpType.isFloat(realFuOpType),
      Mux1H(Seq(
        MmulOpType.isFromE4(realFuOpType) -> "b0".U,
        MmulOpType.isFromE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt,
        MmulOpType.isFromE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt,
        MmulOpType.isFromE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt,
      )),
      MmulOpType.ms1sign(realFuOpType).asUInt
    ),
    MmulOpType.getFromType(realFuOpType)
  )
  output.types2  := Cat(
    Mux(MmulOpType.isFloat(realFuOpType),
      Mux1H(Seq(
        MmulOpType.isToE4(realFuOpType) -> "b0".U,
        MmulOpType.isToE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt,
        MmulOpType.isToE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt,
        MmulOpType.isToE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt,
      )),
      MmulOpType.ms2sign(realFuOpType).asUInt
    ),
    MmulOpType.getFromType(realFuOpType)
  )

  object MmulOpType {
    def placeholder = "b11_111_111".U

    // bit encoding:
    // | 7  | 6  | 5 4 3 | 2 1 | 0 |
    // |int | sgn| from  | to  |sat|
    // int: 0 for int, 1 for float
    // sgn: 0 for unsigned, 1 for signed (only for int)
    // from: source element width, 3'b100 for msew
    // to: target element width, 2'b00 for 1W, 2'b01 for 2W, 2'b10 for 4W
    // sat: 0 for no saturation, 1 for saturation

    def hybridprec    (func: UInt) = func(8) === "b1".U

    def isInt         (func: UInt) = func(7) === "b0".U
    def isFloat       (func: UInt) = func(7) === "b1".U

    def isFp16     (func: UInt) = isFloat(func) && !func(6)
    def isBf16     (func: UInt) = isFloat(func) && func(6)
    def isE5m2     (func: UInt) = isFloat(func) && !func(4)
    def isE4m3     (func: UInt) = isFloat(func) && func(4)
    def isFp32     (func: UInt) = isFloat(func) && !func(4)
    def isTf32     (func: UInt) = isFloat(func) && func(4)

    def ms1sign    (func: UInt) = isInt(func) && func(5)
    def ms1unsigned(func: UInt) = isInt(func) && !func(5)
    def ms2sign    (func: UInt) = isInt(func) && func(4)
    def ms2unsigned(func: UInt) = isInt(func) && !func(4)

    def isFromE4   (func: UInt) = func(3, 2) === "b11".U
    def isFromE8   (func: UInt) = func(3, 2) === "b00".U
    def isFromE16  (func: UInt) = func(3, 2) === "b01".U
    def isFromE32  (func: UInt) = func(3, 2) === "b10".U

    def isToE4     (func: UInt) = func(1, 0) === "b11".U
    def isToE8     (func: UInt) = func(1, 0) === "b00".U
    def isToE16    (func: UInt) = func(1, 0) === "b01".U
    def isToE32    (func: UInt) = func(1, 0) === "b10".U

    def mma_e5m2_fp16 = "b0_1_000_00_01".U
    def mma_e4m3_fp16 = "b0_1_001_00_01".U
    def mma_e5m2_bf16 = "b0_1_100_00_01".U
    def mma_e4m3_bf16 = "b0_1_101_00_01".U
    def mma_e5m2_fp32 = "b0_1_000_00_10".U
    def mma_e4m3_fp32 = "b0_1_001_00_10".U
    def mma_fp16_fp16 = "b0_1_000_01_01".U
    def mma_fp16_fp32 = "b0_1_000_01_10".U
    def mma_bf16_fp32 = "b0_1_100_01_10".U
    def mma_tf32_fp32 = "b0_1_001_10_10".U
    def mma_fp32_fp32 = "b0_1_000_10_10".U

    def mma_int8_int32    = "b0_0_011_00_10".U
    def mma_uint8_int32   = "b0_0_000_00_10".U
    def mma_usint8_int32  = "b0_0_001_00_10".U
    def mma_suint8_uint32 = "b0_0_010_00_10".U
    def mma_int4_int32    = "b0_0_011_11_10".U
    def mma_uint4_uint32  = "b0_0_000_11_10".U
    def mma_usint4_uint32 = "b0_0_001_11_10".U
    def mma_suint4_int32  = "b0_0_010_11_10".U

    def getFromType(func: UInt) = func(3, 2)
    def getToType(func: UInt) = func(1, 0)
  }
     */

    when (mmaInfo.isfp) {
      // types = Cat(isFloat?typeBit:sign, getFromType[1:0])
      // For FP: typeBit = isE4m3(E8)/isBf16(E16)/isTf32(E32)/0(E4)
      // FP16: Cat(0, 01) = b001
      when (mmaInfo.types1 === "b001".U && mmaInfo.types2 === "b001".U) {
        mmaDataType := DataTypeF16F16F32
      // FP8 E5M2: Cat(0, 00) = b000
      }.elsewhen (mmaInfo.types1 === "b000".U && mmaInfo.types2 === "b000".U) {
        mmaDataType := DataTypefp8e5m2F32
      // FP8 E4M3: Cat(1, 00) = b100
      }.elsewhen (mmaInfo.types1 === "b100".U && mmaInfo.types2 === "b100".U) {
        mmaDataType := DataTypefp8e4m3F32
      // BF16: Cat(1, 01) = b101
      }.elsewhen (mmaInfo.types1 === "b101".U && mmaInfo.types2 === "b101".U) {
        mmaDataType := DataTypeBF16BF16F32
      // FP32: Cat(0, 10) = b010
      }.elsewhen (mmaInfo.types1 === "b010".U && mmaInfo.types2 === "b010".U) {
        mmaDataType := DataTypeUndef  // DataTypeFP32FP32FP32
      // TF32: Cat(1, 10) = b110
      }.elsewhen (mmaInfo.types1 === "b110".U && mmaInfo.types2 === "b110".U) {
        mmaDataType := DataTypeTF32TF32F32
      // FP4: Cat(0, 11) = b011
      }.elsewhen (mmaInfo.types1 === "b011".U && mmaInfo.types2 === "b011".U) {
        // NVFP4 vs MXFP4 need to be distinguished by other means
        mmaDataType := DataTypenvfp4F32
      }.otherwise {
        mmaDataType := DataTypeUndef
      }
    }.otherwise { // !mmaInfo.isfp - Integer types
      // types = Cat(sign, getFromType[1:0])
      // U8: Cat(0, 00) = b000
      // I8: Cat(1, 00) = b100
      when (mmaInfo.types1 === "b000".U && mmaInfo.types2 === "b000".U) {
        mmaDataType := DataTypeU8U8I32
      }.elsewhen (mmaInfo.types1 === "b100".U && mmaInfo.types2 === "b000".U) {
        mmaDataType := DataTypeI8U8I32
      }.elsewhen (mmaInfo.types1 === "b000".U && mmaInfo.types2 === "b100".U) {
        mmaDataType := DataTypeU8I8I32
      }.elsewhen (mmaInfo.types1 === "b100".U && mmaInfo.types2 === "b100".U) {
        mmaDataType := DataTypeI8I8I32
      }.otherwise {
        mmaDataType := DataTypeUndef
      }
    }

    scoreboard.io.update.compute_issue := true.B
    scoreboard.io.update.compute_issue_a_reg := aReg
    scoreboard.io.update.compute_issue_b_reg := bReg
    scoreboard.io.update.compute_issue_c_reg := cReg
    scoreboard.io.update.compute_issue_fifo_idx := computeIdx
    computeIssueIdx := computeIssueIdx + 1.U

    pendingComputeA := true.B
    pendingComputeAReg := aReg
    pendingComputeAFifoIdx := computeIdx
    pendingComputeB := true.B
    pendingComputeBReg := bReg
    pendingComputeBFifoIdx := computeIdx
    pendingComputeC := true.B
    pendingComputeCReg := cReg
    pendingComputeCFifoIdx := computeIdx
    pendingComputeM := mVal
    pendingComputeN := nVal
    pendingComputeK := kVal
    pendingComputeIsMma := isMma
    pendingComputeIsFp := Mux(isMma, mmaInfo.isfp, false.B)

    computeIssueEvent.eventType := 0.U
    computeIssueEvent.aReg := aReg
    computeIssueEvent.bReg := bReg
    computeIssueEvent.cReg := cReg
    computeIssueEvent.fifoIdx := computeIdx
    computeIssueEvent.mtilem := mVal
    computeIssueEvent.mtilen := nVal
    computeIssueEvent.mtilek := kVal
    computeIssueEvent.isMma := isMma
    computeIssueEvent.isFp := Mux(isMma, mmaInfo.isfp, false.B)
    computeIssueEventEn := true.B
  }

  when(issueStore) {
    val regIdx = lsuInfo.ms(1, 0)
    val storeIdx = storeIssueIdx
    
    io.CML_MicroTask_Config.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := lsuInfo.baseAddr
    io.CML_MicroTask_Config.ApplicationTensor_D.ApplicationTensor_D_Stride_M := lsuInfo.stride
    io.CML_MicroTask_Config.ApplicationTensor_D.BlockTensor_D_BaseVaddr := lsuInfo.baseAddr
    io.CML_MicroTask_Config.ApplicationTensor_D.dataType := MuxLookup(lsuInfo.widths, ElementDataType.DataTypeWidth32)(Seq(
      Bundles.MSew.e8 -> ElementDataType.DataTypeWidth8,
      Bundles.MSew.e16 -> ElementDataType.DataTypeWidth16,
      Bundles.MSew.e32 -> ElementDataType.DataTypeWidth32,
      Bundles.MSew.e4 -> ElementDataType.DataTypeWidth4
    ))
    
    io.CML_MicroTask_Config.StoreTaskInfo.Is_ZeroStore := false.B
    io.CML_MicroTask_Config.Conherent := true.B
    io.CML_MicroTask_Config.Is_Transpose := lsuInfo.transpose
    io.CML_MicroTask_Config.MatrixRegTensor_M := lsuInfo.row
    io.CML_MicroTask_Config.MatrixRegTensor_N := lsuInfo.column
    io.CML_MicroTask_Config.MatrixRegId := regIdx

    io.CML_MicroTask_Config.IsLoadMicroTask := false.B
    io.CML_MicroTask_Config.IsStoreMicroTask := true.B

    io.CML_MicroTask_Config.MicroTaskValid := true.B
    if (EnableDifftest) {
      io.CML_MicroTask_Config.pc.get := headEntry.ctrl.pc.get
      io.CML_MicroTask_Config.coreid.get := headEntry.ctrl.coreid.get
    }

    scoreboard.io.update.store_issue := true.B
    scoreboard.io.update.store_issue_c_reg := regIdx
    scoreboard.io.update.store_issue_fifo_idx := storeIdx
    storeIssueIdx := storeIssueIdx + 1.U

    pendingStore := true.B
    pendingStoreReg := regIdx
    pendingStoreFifoIdx := storeIdx
    pendingStoreRow := lsuInfo.row
    pendingStoreColumn := lsuInfo.column
    pendingStoreTranspose := lsuInfo.transpose
    pendingStoreIsAcc := lsuInfo.isacc

    storeIssueEvent.eventType := 0.U
    storeIssueEvent.regId := regIdx
    storeIssueEvent.fifoIdx := storeIdx
    storeIssueEvent.row := lsuInfo.row
    storeIssueEvent.column := lsuInfo.column
    storeIssueEvent.transpose := lsuInfo.transpose
    storeIssueEvent.isAcc := lsuInfo.isacc
    storeIssueEventEn := true.B
  }

  when(issueRelease) {
    io.ygjkctrl.mrelease.valid := true.B
    io.ygjkctrl.mrelease.bits.tokenRd(releaseInfo.tokenRd) := true.B
    releaseIssueEvent.eventType := 0.U
    releaseIssueEvent.token := releaseInfo.tokenRd
    releaseIssueEventEn := true.B
  }

  if (EnableDifftest) {
    val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank), delay = 0, dontCare = true)
    difftestAmuFinish.coreid := io.ygjkctrl.amuCtrl.bits.coreid.get
    difftestAmuFinish.index := 4.U
    difftestAmuFinish.valid := io.ygjkctrl.mrelease.valid
    difftestAmuFinish.pc := headEntry.ctrl.pc.get
    difftestAmuFinish.bankValid.foreach(_ := false.B)
    difftestAmuFinish.bankAddr.foreach(_ := 0.U)
    difftestAmuFinish.data.foreach(_ := 0.U)
    difftestAmuFinish.finish := io.ygjkctrl.mrelease.valid
  }

  // ===================== ChiselDB 日志提交 =====================
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
}

