package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

object ScoreboardConsts {
  val NumAbRegs = 4
  val NumCRegs = 2
  val NumAMLUnits = 1
  val NumBMLUnits = 1
  val NumCMLUnits = 1
  val NumComputeUnits = 1
  val TotalUnits = NumAMLUnits + NumBMLUnits + NumCMLUnits + NumComputeUnits

  val TileRegIdxWidth = math.max(1, log2Ceil(NumAbRegs))
  val AccRegIdxWidth = math.max(1, log2Ceil(NumCRegs))
  val RegIdxWidth = TileRegIdxWidth max AccRegIdxWidth
}

class RegIdx(implicit p: Parameters) extends CuteBundle {
  val is_acc = Bool()
  val regIdx = UInt(ScoreboardConsts.RegIdxWidth.W)
}

class RegResultStatus(fuIdWidth: Int) extends Bundle {
  val busy = Bool()
  val fuId = UInt(fuIdWidth.W)
}

class FuncUnitSrcStatus(fuIdWidth: Int)(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val reg = new RegIdx()
  val ready = Bool()
  val waitFu = UInt(fuIdWidth.W)
  val readPending = Bool()
}

class FuncUnitStatus(fuIdWidth: Int)(implicit p: Parameters) extends Bundle {
  val busy = Bool()
  val fifoIdx = UInt(2.W)

  val destValid = Bool()
  val destReg = UInt(ScoreboardConsts.RegIdxWidth.W)

  val srcs = Vec(3, new FuncUnitSrcStatus(fuIdWidth))
}


class QueryReq(implicit p: Parameters) extends CuteBundle {
    val fuType = ScoreboardFuType()
    val dest = Valid(new RegIdx())
    val src1 = Valid(new RegIdx())
    val src2 = Valid(new RegIdx())
    val src3 = Valid(new RegIdx())
}

object ScoreboardFuType extends ChiselEnum {
  val None, AML, BML, CML, Compute = Value
}

// Scoreboard的查询接口：用于依赖检查（使用Valid-Ready握手）
// valid: TaskController有指令要发射
// ready: Scoreboard检查依赖通过，允许发射
class ScoreboardQueryIO(implicit p: Parameters) extends CuteBundle {
    val req = Flipped(DecoupledIO(new QueryReq))
}

// Scoreboard的更新接口：用于更新寄存器状态
class ScoreboardUpdateIO(implicit p: Parameters) extends CuteBundle {
    // 分配寄存器给Load指令
    val load_allocate = Input(Bool())
    val load_alloc_a_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val load_alloc_b_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val load_alloc_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val load_alloc_has_a = Input(Bool())
    val load_alloc_has_b = Input(Bool())
    val load_alloc_has_c = Input(Bool())
    val load_alloc_fifo_idx = Input(UInt(2.W))
    
    // Load完成，标记寄存器为Ready
    val load_finish_a = Input(Bool())
    val load_finish_a_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val load_finish_b = Input(Bool())
    val load_finish_b_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val load_finish_c = Input(Bool())
    val load_finish_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    
    // Compute发射，读取A/B/C，写入C
    val compute_issue = Input(Bool())
    val compute_issue_a_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val compute_issue_b_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val compute_issue_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val compute_issue_fifo_idx = Input(UInt(2.W))
    
    // Compute完成读取A/B
    val compute_read_finish_a = Input(Bool())
    val compute_read_finish_a_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val compute_read_finish_b = Input(Bool())
    val compute_read_finish_b_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    
    // Compute完成写入C
    val compute_write_finish_c = Input(Bool())
    val compute_write_finish_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    
    // Store发射，读取C
    val store_issue = Input(Bool())
    val store_issue_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
    val store_issue_fifo_idx = Input(UInt(2.W))
    
    // Store完成，释放C
    val store_finish = Input(Bool())
    val store_finish_c_reg = Input(UInt(ScoreboardConsts.RegIdxWidth.W))
}

class Scoreboard(implicit p: Parameters) extends CuteModule {
  import ScoreboardConsts._

  val io = IO(new Bundle {
      val query = new ScoreboardQueryIO
      val update = new ScoreboardUpdateIO
  })

  private val fuIdWidth = ScoreboardFuType.getWidth
  private val noneFuId = ScoreboardFuType.None.asUInt
  private val amlFuIdConst = ScoreboardFuType.AML.asUInt
  private val bmlFuIdConst = ScoreboardFuType.BML.asUInt
  private val cmlFuIdConst = ScoreboardFuType.CML.asUInt
  private val computeFuIdConst = ScoreboardFuType.Compute.asUInt

  private val abRegStatus = RegInit(VecInit(Seq.fill(NumAbRegs)(0.U.asTypeOf(new RegResultStatus(fuIdWidth)))))
  private val cRegStatus = RegInit(VecInit(Seq.fill(NumCRegs)(0.U.asTypeOf(new RegResultStatus(fuIdWidth)))))

  private val fuStatuses = RegInit(VecInit(Seq.fill(ScoreboardFuType.all.length)(0.U.asTypeOf(new FuncUnitStatus(fuIdWidth)))))

  private def statusOf(fuType: ScoreboardFuType.Type): FuncUnitStatus =
    fuStatuses(fuType.litValue.toInt)

  private def amlStatus: FuncUnitStatus = statusOf(ScoreboardFuType.AML)
  private def bmlStatus: FuncUnitStatus = statusOf(ScoreboardFuType.BML)
  private def cmlStatus: FuncUnitStatus = statusOf(ScoreboardFuType.CML)
  private def computeStatus: FuncUnitStatus = statusOf(ScoreboardFuType.Compute)

  private def fuBusy(fuType: ScoreboardFuType.Type): Bool = statusOf(fuType).busy
  private def fuFree(fuType: ScoreboardFuType.Type): Bool = !fuBusy(fuType)

  private def computeStatuses: Seq[FuncUnitStatus] =
    if (NumComputeUnits > 0) Seq(computeStatus) else Seq.empty

  private def cmlStatuses: Seq[FuncUnitStatus] =
    if (NumCMLUnits > 0) Seq(cmlStatus) else Seq.empty

  require(NumAMLUnits == 1 && NumBMLUnits == 1 && NumCMLUnits == 1 && NumComputeUnits == 1,
    "Scoreboard FU ID encoding assumes a single instance per functional unit type")

  private val SrcAIdx = 0
  private val SrcBIdx = 1
  private val SrcCIdx = 2

  private def toTileIdx(idx: UInt): UInt = idx(ScoreboardConsts.TileRegIdxWidth - 1, 0)
  private def toAccIdx(idx: UInt): UInt = idx(ScoreboardConsts.AccRegIdxWidth - 1, 0)

  private def resetSrcStatus(src: FuncUnitSrcStatus): Unit = {
    src.valid := false.B
    src.reg.is_acc := false.B
    src.reg.regIdx := 0.U(ScoreboardConsts.RegIdxWidth.W)
    src.ready := false.B
    src.waitFu := noneFuId
    src.readPending := false.B
  }

  private def reduceOr(list: Seq[Bool]): Bool = list.foldLeft(false.B)(_ || _)

  private def hasPendingReaders(reg: RegIdx, excludeFuId: UInt): Bool = {
    def hasPendingReadersAB(regIdx: UInt, excludeFuId: UInt): Bool = {
      val target = toTileIdx(regIdx)
      val fromComputeA = computeStatuses.map { fu =>
        val fuId = computeFuIdConst
        val src = fu.srcs(SrcAIdx)
        fu.busy && (fuId =/= excludeFuId) && src.valid && src.readPending && (toTileIdx(src.reg.regIdx) === target)
      }
      val fromComputeB = computeStatuses.map { fu =>
        val fuId = computeFuIdConst
        val src = fu.srcs(SrcBIdx)
        fu.busy && (fuId =/= excludeFuId) && src.valid && src.readPending && (toTileIdx(src.reg.regIdx) === target)
      }
      reduceOr(fromComputeA ++ fromComputeB)
    }

    def hasPendingReadersC(regIdx: UInt, excludeFuId: UInt): Bool = {
      val target = toAccIdx(regIdx)
      val fromCompute = computeStatuses.map { fu =>
        val fuId = computeFuIdConst
        val src = fu.srcs(SrcCIdx)
        fu.busy && (fuId =/= excludeFuId) && src.valid && src.readPending && (toAccIdx(src.reg.regIdx) === target)
      }
      val fromCmlStore = cmlStatuses.map { fu =>
        val fuId = cmlFuIdConst
        val src = fu.srcs(SrcCIdx)
        src.valid && src.readPending && (toAccIdx(src.reg.regIdx) === target) && (fuId =/= excludeFuId)
      }
      reduceOr(fromCompute ++ fromCmlStore)
    }
    
    Mux(reg.is_acc,
      hasPendingReadersC(reg.regIdx, excludeFuId),
      hasPendingReadersAB(reg.regIdx, excludeFuId))
  }

  private def wakeupAbConsumers(regIdx: UInt, producerFuId: UInt): Unit = {
    val target = toTileIdx(regIdx)
    computeStatuses.foreach { fu =>
      val srcA = fu.srcs(SrcAIdx)
      when(fu.busy && srcA.valid && (srcA.waitFu === producerFuId) && (toTileIdx(srcA.reg.regIdx) === target)) {
        srcA.ready := true.B
        srcA.waitFu := noneFuId
      }
      val srcB = fu.srcs(SrcBIdx)
      when(fu.busy && srcB.valid && (srcB.waitFu === producerFuId) && (toTileIdx(srcB.reg.regIdx) === target)) {
        srcB.ready := true.B
        srcB.waitFu := noneFuId
      }
    }
  }

  private def wakeupCConsumers(regIdx: UInt, producerFuId: UInt): Unit = {
    val target = toAccIdx(regIdx)
    computeStatuses.foreach { fu =>
      val srcC = fu.srcs(SrcCIdx)
      when(fu.busy && srcC.valid && (srcC.waitFu === producerFuId) && (toAccIdx(srcC.reg.regIdx) === target)) {
        srcC.ready := true.B
        srcC.waitFu := noneFuId
      }
    }
    cmlStatuses.foreach { fu =>
      val srcC = fu.srcs(SrcCIdx)
      when(srcC.valid && (srcC.waitFu === producerFuId) && (toAccIdx(srcC.reg.regIdx) === target)) {
        srcC.ready := true.B
        srcC.waitFu := noneFuId
      }
    }
  }

  private def reserveAbRegister(regIdx: UInt, fuId: UInt): Unit = {
    val entry = abRegStatus(toTileIdx(regIdx))
    entry.busy := true.B
    entry.fuId := fuId
  }

  private def releaseAbRegister(regIdx: UInt, fuId: UInt): Unit = {
    val entry = abRegStatus(toTileIdx(regIdx))
    when(entry.busy && (entry.fuId === fuId)) {
      entry.busy := false.B
      entry.fuId := noneFuId
      wakeupAbConsumers(regIdx, fuId)
    }
  }

  private def reserveCRegister(regIdx: UInt, fuId: UInt): Unit = {
    val entry = cRegStatus(toAccIdx(regIdx))
    entry.busy := true.B
    entry.fuId := fuId
  }

  private def releaseCRegister(regIdx: UInt, fuId: UInt): Unit = {
    val entry = cRegStatus(toAccIdx(regIdx))
    when(entry.busy && (entry.fuId === fuId)) {
      entry.busy := false.B
      entry.fuId := noneFuId
      wakeupCConsumers(regIdx, fuId)
    }
  }

  private def regBusy(reg: RegIdx): Bool =
    Mux(reg.is_acc,
      cRegStatus(toAccIdx(reg.regIdx)).busy,
      abRegStatus(toTileIdx(reg.regIdx)).busy)

  private def regReady(reg: Valid[RegIdx]): Bool =
    !reg.valid || !regBusy(reg.bits)

  private def canIssueReq(req: QueryReq): Bool = {
    val isAML = req.fuType === ScoreboardFuType.AML
    val isBML = req.fuType === ScoreboardFuType.BML
    val isCML = req.fuType === ScoreboardFuType.CML
    val isCompute = req.fuType === ScoreboardFuType.Compute
    val isCMLLoad = isCML && req.dest.valid
    val isCMLStore = isCML && !req.dest.valid
    val recognized = req.fuType =/= ScoreboardFuType.None

    val destReg = req.dest.bits
    val destBusy = req.dest.valid && regBusy(destReg)

    val writerFuId = Mux(req.dest.valid, req.fuType.asUInt, noneFuId)

    val destHasConsumers = req.dest.valid && recognized &&
      hasPendingReaders(destReg, writerFuId)
    val writesOk = !req.dest.valid || (!destBusy && !destHasConsumers)

    val srcsReady = regReady(req.src1) && regReady(req.src2) && regReady(req.src3)

    val loadChecks =
      (isAML && fuFree(ScoreboardFuType.AML)) ||
      (isBML && fuFree(ScoreboardFuType.BML)) ||
      (isCMLLoad && fuFree(ScoreboardFuType.CML))

    val computeChecks =
      isCompute &&
        fuFree(ScoreboardFuType.Compute)

    val storeConsumersOk = !hasPendingReaders(req.src1.bits, cmlFuIdConst)
    val storeChecks =
      isCMLStore &&
        fuFree(ScoreboardFuType.CML) &&
        storeConsumersOk

    val issueOk = (loadChecks || computeChecks || storeChecks) && srcsReady && writesOk

    recognized && issueOk
  }

  io.query.req.ready := !io.query.req.valid || canIssueReq(io.query.req.bits)

  // Issue stage updates
  when(io.update.load_allocate) {
    val fifoIdx = io.update.load_alloc_fifo_idx

    when(io.update.load_alloc_has_a) {
      val fu = amlStatus
      assert(!fu.busy, "AML FU allocation while busy")
      fu.busy := true.B
      fu.fifoIdx := fifoIdx
      fu.destValid := true.B
      fu.destReg := io.update.load_alloc_a_reg
      fu.srcs.foreach(resetSrcStatus)
      reserveAbRegister(io.update.load_alloc_a_reg, amlFuIdConst)
    }

    when(io.update.load_alloc_has_b) {
      val fu = bmlStatus
      assert(!fu.busy, "BML FU allocation while busy")
      fu.busy := true.B
      fu.fifoIdx := fifoIdx
      fu.destValid := true.B
      fu.destReg := io.update.load_alloc_b_reg
      fu.srcs.foreach(resetSrcStatus)
      reserveAbRegister(io.update.load_alloc_b_reg, bmlFuIdConst)
    }

    when(io.update.load_alloc_has_c) {
      val fu = cmlStatus
      assert(!fu.busy, "CML load allocation while busy")
      fu.busy := true.B
      fu.fifoIdx := fifoIdx
      fu.destValid := true.B
      fu.destReg := io.update.load_alloc_c_reg
      fu.srcs.foreach(resetSrcStatus)
      reserveCRegister(io.update.load_alloc_c_reg, cmlFuIdConst)
    }
  }

  when(io.update.compute_issue) {
    val fu = computeStatus
    when(!fu.busy) {
      fu.busy := true.B
      fu.fifoIdx := io.update.compute_issue_fifo_idx
      fu.destValid := true.B
      fu.destReg := io.update.compute_issue_c_reg
      reserveCRegister(io.update.compute_issue_c_reg, computeFuIdConst)

      // Source A
      val srcA = fu.srcs(SrcAIdx)
      srcA.valid := true.B
      srcA.reg.is_acc := false.B
      srcA.reg.regIdx := io.update.compute_issue_a_reg
      srcA.readPending := true.B
      val aEntry = abRegStatus(toTileIdx(io.update.compute_issue_a_reg))
      when(aEntry.busy) {
        srcA.ready := false.B
        srcA.waitFu := aEntry.fuId
      }.otherwise {
        srcA.ready := true.B
        srcA.waitFu := noneFuId
      }

      // Source B
      val srcB = fu.srcs(SrcBIdx)
      srcB.valid := true.B
      srcB.reg.is_acc := false.B
      srcB.reg.regIdx := io.update.compute_issue_b_reg
      srcB.readPending := true.B
      val bEntry = abRegStatus(toTileIdx(io.update.compute_issue_b_reg))
      when(bEntry.busy) {
        srcB.ready := false.B
        srcB.waitFu := bEntry.fuId
      }.otherwise {
        srcB.ready := true.B
        srcB.waitFu := noneFuId
      }

      // Source C (old value)
      val srcC = fu.srcs(SrcCIdx)
      srcC.valid := true.B
      srcC.reg.is_acc := true.B
      srcC.reg.regIdx := io.update.compute_issue_c_reg
      srcC.readPending := false.B
      val cEntry = cRegStatus(toAccIdx(io.update.compute_issue_c_reg))
      when(cEntry.busy && (cEntry.fuId =/= computeFuIdConst)) {
        srcC.ready := false.B
        srcC.waitFu := cEntry.fuId
        srcC.readPending := true.B
      }.otherwise {
        srcC.ready := true.B
        srcC.waitFu := noneFuId
      }
    }.otherwise {
      assert(false.B, "Compute FU allocation while busy")
    }
  }

  when(io.update.store_issue) {
    val fu = cmlStatus
    val srcC = fu.srcs(SrcCIdx)
    when(!fu.busy && !srcC.valid) {
      fu.busy := true.B
      fu.fifoIdx := io.update.store_issue_fifo_idx
      fu.destValid := false.B
      resetSrcStatus(fu.srcs(SrcAIdx))
      resetSrcStatus(fu.srcs(SrcBIdx))
      srcC.valid := true.B
      srcC.reg.is_acc := true.B
      srcC.reg.regIdx := io.update.store_issue_c_reg
      srcC.readPending := true.B
      val cEntry = cRegStatus(toAccIdx(io.update.store_issue_c_reg))
      when(cEntry.busy) {
        srcC.ready := false.B
        srcC.waitFu := cEntry.fuId
      }.otherwise {
        srcC.ready := true.B
        srcC.waitFu := noneFuId
      }
    }.otherwise {
      assert(false.B, "CML store allocation while busy")
    }
  }

  // Read stage completions
  when(io.update.compute_read_finish_a) {
    val target = io.update.compute_read_finish_a_reg
    computeStatuses.foreach { fu =>
      val srcA = fu.srcs(SrcAIdx)
      when(fu.busy && srcA.valid && (srcA.reg.regIdx === target)) {
        srcA.readPending := false.B
      }
    }
  }

  when(io.update.compute_read_finish_b) {
    val target = io.update.compute_read_finish_b_reg
    computeStatuses.foreach { fu =>
      val srcB = fu.srcs(SrcBIdx)
      when(fu.busy && srcB.valid && (srcB.reg.regIdx === target)) {
        srcB.readPending := false.B
      }
    }
  }

  // Writebacks
  when(io.update.load_finish_a) {
    val regIdx = io.update.load_finish_a_reg
    val fu = amlStatus
    when(fu.busy && fu.destValid && (fu.destReg === regIdx)) {
      fu.destValid := false.B
      fu.busy := false.B
      releaseAbRegister(regIdx, amlFuIdConst)
    }
  }

  when(io.update.load_finish_b) {
    val regIdx = io.update.load_finish_b_reg
    val fu = bmlStatus
    when(fu.busy && fu.destValid && (fu.destReg === regIdx)) {
      fu.destValid := false.B
      fu.busy := false.B
      releaseAbRegister(regIdx, bmlFuIdConst)
    }
  }

  when(io.update.load_finish_c) {
    val regIdx = io.update.load_finish_c_reg
    val fu = cmlStatus
    when(fu.busy && fu.destValid && (fu.destReg === regIdx)) {
      fu.destValid := false.B
      fu.busy := false.B
      releaseCRegister(regIdx, cmlFuIdConst)
    }
  }

  when(io.update.compute_write_finish_c) {
    val regIdx = io.update.compute_write_finish_c_reg
    computeStatuses.foreach { fu =>
      when(fu.busy && fu.destValid && (fu.destReg === regIdx)) {
        fu.destValid := false.B
        fu.busy := false.B
        releaseCRegister(regIdx, computeFuIdConst)
      }
    }
  }

  when(io.update.store_finish) {
    val regIdx = io.update.store_finish_c_reg
    val fu = cmlStatus
    val srcC = fu.srcs(SrcCIdx)
    when(fu.busy && srcC.valid && (srcC.reg.regIdx === regIdx)) {
      fu.busy := false.B
      resetSrcStatus(srcC)
    }
  }

  if (YJPDebugEnable) {
    printf("[NewScoreboard] AB Busy: [%d,%d,%d,%d], C Busy: [%d,%d]\n",
      abRegStatus(0).busy, abRegStatus(1).busy, abRegStatus(2).busy, abRegStatus(3).busy,
      cRegStatus(0).busy, cRegStatus(1).busy)
  }
}
