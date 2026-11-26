package cute

import chisel3._
import chiseltest._
import cute.Bundles._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

class TaskControllerFp4DecodeSpec extends AnyFlatSpec with ChiselScalatestTester {
  private class DecodeTop(scaleEnabled: Boolean) extends Module {
    implicit val p: Parameters = new Config((_, _, _) => {
      case CuteParamsKey => CuteParams.CUTE_2Tops.copy(
        EnableDifftest = false,
        MatrixExtension = MatrixIsaParams(
          enableInt8Int32 = true,
          enableFp8Fp32 = true,
          enableMxfp4Fp32 = scaleEnabled,
          enableMxfp8Fp32 = scaleEnabled
        )
      )
    })

    val io = IO(new Bundle {
      val typeBits = Input(UInt(3.W))
      val isFp = Input(Bool())
      val start = Input(Bool())
      val complete = Input(Bool())
      val ctrlReady = Output(Bool())
      val computeValid = Output(Bool())
      val computeType = Output(UInt(MteComputeType.ComputeTypeBitWidth.W))
    })

    val dut = Module(new TaskController)
    dut.io.ygjkctrl.reset := false.B
    dut.io.ygjkctrl.mrelease.ready := true.B
    dut.io.ygjkctrl.minitClear := false.B
    dut.io.downstreamIdle := true.B
    dut.io.DebugTimeStampe := 0.U

    dut.io.trLoader0Config.MicroTaskReady := true.B
    dut.io.trLoader0Config.MicroTaskEndValid := false.B
    dut.io.trLoader0Config.MicroTaskFlags := 0.U
    dut.io.trLoader1Config.MicroTaskReady := true.B
    dut.io.trLoader1Config.MicroTaskEndValid := false.B
    dut.io.trLoader1Config.MicroTaskFlags := 0.U
    dut.io.accMemoryConfig.LoadMicroTaskReady := true.B
    dut.io.accMemoryConfig.LoadMicroTaskEndValid := false.B
    dut.io.accMemoryConfig.StoreMicroTaskReady := true.B
    dut.io.accMemoryConfig.StoreMicroTaskEndValid := false.B
    dut.io.trStorer0.command.ready := true.B
    dut.io.trStorer0.done.valid := false.B
    dut.io.trStorer0.done.bits := false.B

    dut.io.ADC_MicroTask_Config.MicroTaskReady := true.B
    dut.io.ADC_MicroTask_Config.MicroTaskEndValid := io.complete
    dut.io.BDC_MicroTask_Config.MicroTaskReady := true.B
    dut.io.BDC_MicroTask_Config.MicroTaskEndValid := io.complete
    dut.io.CDC_MicroTask_Config.MicroTaskReady := true.B
    dut.io.CDC_MicroTask_Config.MicroTaskEndValid := io.complete
    dut.io.CDC_MicroTask_Config.MicroTaskFlags := 0.U
    dut.io.CDC_MicroTask_Config.MicroTask_TEComputeEndValid := false.B

    dut.io.ASC_MicroTask_Config.foreach { cfg =>
      cfg.MicroTaskReady := true.B
      cfg.MicroTaskEndValid := false.B
    }
    dut.io.BSC_MicroTask_Config.foreach { cfg =>
      cfg.MicroTaskReady := true.B
      cfg.MicroTaskEndValid := false.B
    }
    dut.io.ASL_MicroTask_Config.foreach { cfg =>
      cfg.MicroTaskReady := true.B
      cfg.MicroTaskEndValid := false.B
    }
    dut.io.BSL_MicroTask_Config.foreach { cfg =>
      cfg.MicroTaskReady := true.B
      cfg.MicroTaskEndValid := false.B
    }

    val mma = WireInit(0.U.asTypeOf(new AmuMmaIO))
    mma.md := 0.U
    mma.ms1 := 0.U
    mma.ms2 := 1.U
    mma.mtilem := 4.U
    mma.mtilen := 4.U
    mma.mtilek := 64.U
    mma.types1 := io.typeBits
    mma.types2 := io.typeBits
    mma.isfp := io.isFp

    dut.io.ygjkctrl.amuCtrl.valid := io.start
    dut.io.ygjkctrl.amuCtrl.bits.op := AmuCtrlIO.mmaOp()
    dut.io.ygjkctrl.amuCtrl.bits.data := mma.asUInt

    io.ctrlReady := dut.io.ygjkctrl.amuCtrl.ready
    io.computeValid := dut.io.MTE_MicroTask_Config.MicroTaskValid
    io.computeType := dut.io.MTE_MicroTask_Config.computeType
  }

  for (scaleEnabled <- Seq(false, true)) {
  it should s"preserve unscaled FP8 and INT8 decoding with scaling=$scaleEnabled" in {
    test(new DecodeTop(scaleEnabled)) { dut =>
      def issue(typeBits: Int, isFp: Boolean, expected: UInt): Unit = {
        dut.io.typeBits.poke(typeBits.U)
        dut.io.isFp.poke(isFp.B)
        dut.io.complete.poke(false.B)
        dut.io.start.poke(true.B)

        var readyCycles = 0
        while (!dut.io.ctrlReady.peek().litToBoolean && readyCycles < 20) {
          dut.clock.step()
          readyCycles += 1
        }
        assert(dut.io.ctrlReady.peek().litToBoolean, "TaskController input did not become ready")
        dut.clock.step()
        dut.io.start.poke(false.B)

        var issueCycles = 0
        while (!dut.io.computeValid.peek().litToBoolean && issueCycles < 20) {
          dut.clock.step()
          issueCycles += 1
        }
        assert(dut.io.computeValid.peek().litToBoolean, "MMA was not issued")
        assert(dut.io.computeType.peek().litValue == expected.litValue)

        dut.clock.step()
        dut.io.complete.poke(true.B)
        dut.clock.step()
        dut.io.complete.poke(false.B)
        dut.clock.step()
      }

      issue(typeBits = 0b100, isFp = true, expected = MteComputeType.Fp8e4m3F32)
      issue(typeBits = 0b000, isFp = true, expected = MteComputeType.Fp8e5m2F32)
      issue(typeBits = 0b100, isFp = false, expected = MteComputeType.I8I8I32)
    }
  }
  }
}

// Exercise the frozen 150-bit transport, not a second software/ISA producer.
class ScaleSchedulerTop(val reduceBytes: Int = 64, val canonical8: Boolean = false,
                        val traceEnabled: Boolean = false, val convertEnabled: Boolean = false,
                        val convertLanes: Int = 8, val dynamicEnabled: Boolean = false) extends Module {
  implicit val p: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => (if (canonical8) CuteParams.CUTE_8Tops_128SCP else CuteParams.CUTE_2Tops).copy(
      EnableDifftest = traceEnabled,
      ExperimentalMxfpConvertLoad = convertEnabled,
      ExperimentalMxfpDynamicQuant = dynamicEnabled,
      MxfpConvertLanes = convertLanes,
      ReduceWidthByte = reduceBytes,
      Tensor_MN = 128,
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
  })
  val io = IO(new Bundle {
    val start = Input(Bool())
    val op = Input(UInt(2.W))
    // Native test leaves only; the DUT still receives the exact 150-bit ABI.
    // This is the same narrow-word adapter used by the canonical CML probe,
    // avoiding chiseltest 6's obsolete WData pointer API on Verilator 5.052.
    val payload = Input(Vec(5, UInt(32.W)))
    // ASL,BSL,ADC,BDC,ASC,BSC,CDC completions.
    val done = Input(UInt(7.W))
    val ready = Output(Bool())
    val issued = Output(UInt(3.W)) // ASL, BSL, compute
    val bankA = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val bankB = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val loadRegModeA = Output(UInt(4.W))
    val loadRegModeB = Output(UInt(4.W))
    val loadKA = Output(UInt(9.W))
    val loadKB = Output(UInt(9.W))
    val loadRowsA = Output(UInt(9.W))
    val loadRowsB = Output(UInt(9.W))
    val sourceScaleStride = Output(Vec(2, UInt(p(CuteParamsKey).MMUAddrWidth.W)))
    val computeType = Output(UInt(4.W))
    val scaleStarts = Output(UInt(2.W))
    val computeBankA = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val computeBankB = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val ascK = Output(UInt(9.W))
    val bscK = Output(UInt(9.W))
    val adcK = Output(UInt(9.W))
    val bdcK = Output(UInt(9.W))
    val ascBlocks = Output(UInt(9.W))
    val bscBlocks = Output(UInt(9.W))
    val ascElements = Output(UInt(9.W))
    val bscElements = Output(UInt(9.W))
    val active = Output(Bool())
    val retire = Output(Bool())
    val minitClear = Input(Bool())
    val downstreamIdle = Input(Bool())
    val resetReady = Output(Bool())
    val minitClearAck = Output(Bool())
    val residentMask = Output(UInt((2 * p(CuteParamsKey).ABScaleRegCount).W))
    val generationPresent = Output(Bool())
    val loaderDone = Input(UInt(4.W)) // AML, BML, C-load, C-store
    val loaderIssued = Output(UInt(4.W))
    val loaderEndReady = Output(UInt(4.W))
    val dynamic = Option.when(dynamicEnabled)(new Bundle {
      val dataReady = Input(UInt(2.W))
      val scaleReady = Input(UInt(2.W))
      val generated = Output(UInt(2.W))
      val scaleEndReady = Output(UInt(2.W))
      val occupied = Output(UInt((2 * p(CuteParamsKey).ABScaleRegCount).W))
      val latest = Output(Vec(2, UInt(32.W))) // target register 2 on A/B
    })
    val loaderFlags = Input(Vec(2, UInt(5.W)))
    val convertActive = Output(Vec(2, Bool()))
    val convertMode = Output(Vec(2, UInt(4.W)))
    val convertElements = Output(Vec(2, UInt(9.W)))
    val convertBank = Output(Vec(2, UInt(p(CuteParamsKey).ABScaleRegIdWidth.W)))
    val sourceBeats = Output(Vec(2, UInt(9.W)))
    val packedK = Output(Vec(2, UInt(9.W)))
    val matrixRows = Output(Vec(2, UInt(9.W)))
    val sourceDataType = Output(Vec(2, UInt(8.W)))
    val scaleInUse = Output(UInt((2 * p(CuteParamsKey).ABScaleRegCount).W))
    val completionFlags = Input(UInt(5.W))
    val returnReady = Input(Bool())
    val returnValid = Output(Bool())
    val returnFlagsValid = Output(Bool())
    val returnFlags = Output(UInt(5.W))
    val returnMsync = Output(UInt(p(CuteParamsKey).MsyncRegs.W))
    val windowCount = Output(UInt(8.W))
    val headIndex = Output(UInt(8.W))
    val traceRetireId = Input(UInt(64.W))
    val traceEpoch = Input(UInt(64.W))
    val scaleOwners = Output(Vec(2, new TaskCtrlOwnedScaleTrace))
    val mmaTraces = Output(Vec(2, new difftest.DiffAmuMxFinishEvent(
      p(CuteParamsKey).Matrix_MN, p(CuteParamsKey).DiffAmuFinishWordsPerBank,
      p(CuteParamsKey).DiffAmuFinishAddrBits)))
    val retirementTraces = Output(Vec(2, new difftest.DiffAmuMxFinishEvent(
      p(CuteParamsKey).Matrix_MN, p(CuteParamsKey).DiffAmuFinishWordsPerBank,
      p(CuteParamsKey).DiffAmuFinishAddrBits)))
  })
  val dut = Module(new TaskController)
  dut.io.ygjkctrl.reset := false.B
  dut.io.ygjkctrl.mrelease.ready := io.returnReady
  io.returnValid := dut.io.ygjkctrl.mrelease.valid
  io.returnFlagsValid := dut.io.ygjkctrl.mrelease.bits.flagsValid
  io.returnFlags := dut.io.ygjkctrl.mrelease.bits.flags
  io.returnMsync := dut.io.ygjkctrl.mrelease.bits.msyncRd.asUInt
  io.windowCount := chiseltest.experimental.expose(dut.winCount)
  io.headIndex := chiseltest.experimental.expose(dut.winHead)
  dut.io.ygjkctrl.minitClear := io.minitClear
  dut.io.downstreamIdle := io.downstreamIdle
  io.resetReady := dut.io.ygjkctrl.resetReady
  io.minitClearAck := dut.io.ygjkctrl.minitClearAck
  private val observedScaleA = chiseltest.experimental.expose(dut.scaleA)
  private val observedScaleB = chiseltest.experimental.expose(dut.scaleB)
  private val observedLatestA = chiseltest.experimental.expose(dut.latestScaleA)
  private val observedLatestB = chiseltest.experimental.expose(dut.latestScaleB)
  io.residentMask := chisel3.util.Cat(observedScaleB.map(_.valid).reverse ++ observedScaleA.map(_.valid).reverse)
  io.scaleInUse := chisel3.util.Cat(observedScaleB.map(_.inUse).reverse ++ observedScaleA.map(_.inUse).reverse)
  io.dynamic.foreach { dyn =>
    dyn.occupied := chisel3.util.Cat(observedScaleB.map(_.occupied).reverse ++ observedScaleA.map(_.occupied).reverse)
    dyn.latest(0) := observedLatestA(2)
    dyn.latest(1) := observedLatestB(2)
  }
  io.generationPresent := (observedLatestA ++ observedLatestB).map(_.orR).reduce(_ || _)
  dut.io.DebugTimeStampe := 0.U
  dut.io.ygjkctrl.amuCtrl.valid := io.start
  dut.io.ygjkctrl.amuCtrl.bits.op := io.op
  dut.io.ygjkctrl.amuCtrl.bits.data := io.payload.asUInt(149, 0)
  dut.io.ygjkctrl.amuCtrl.bits.pc.foreach(_ := "h80001000".U)
  dut.io.ygjkctrl.amuCtrl.bits.coreid.foreach(_ := 0.U)
  dut.io.ygjkctrl.amuCtrl.bits.retireId.foreach(_ := io.traceRetireId)
  dut.io.ygjkctrl.amuCtrl.bits.epoch.foreach(_ := io.traceEpoch)
  io.scaleOwners := dut.io.ownedScaleTrace.getOrElse(
    0.U.asTypeOf(Vec(2, new TaskCtrlOwnedScaleTrace)))
  io.mmaTraces := dut.io.mmaTrace.getOrElse(
    0.U.asTypeOf(io.mmaTraces))
  io.retirementTraces := dut.io.retirementTrace.getOrElse(
    0.U.asTypeOf(io.retirementTraces))
  dut.io.trLoader0Config.MicroTaskReady := io.dynamic.map(_.dataReady(0)).getOrElse(true.B)
  dut.io.trLoader0Config.MicroTaskEndValid := io.loaderDone(0)
  dut.io.trLoader0Config.MicroTaskFlags := io.loaderFlags(0)
  dut.io.trLoader1Config.MicroTaskReady := io.dynamic.map(_.dataReady(1)).getOrElse(true.B)
  dut.io.trLoader1Config.MicroTaskEndValid := io.loaderDone(1)
  dut.io.trLoader1Config.MicroTaskFlags := io.loaderFlags(1)
  dut.io.accMemoryConfig.LoadMicroTaskReady := true.B
  dut.io.accMemoryConfig.LoadMicroTaskEndValid := io.loaderDone(2)
  dut.io.accMemoryConfig.StoreMicroTaskReady := true.B
  dut.io.accMemoryConfig.StoreMicroTaskEndValid := io.loaderDone(3)
  dut.io.trStorer0.command.ready := true.B
  dut.io.trStorer0.done.valid := false.B
  dut.io.trStorer0.done.bits := false.B
  io.loaderIssued := chisel3.util.Cat(
    dut.io.accMemoryConfig.StoreMicroTaskValid, dut.io.accMemoryConfig.LoadMicroTaskValid,
    dut.io.trLoader1Config.MicroTaskValid, dut.io.trLoader0Config.MicroTaskValid)
  io.loaderEndReady := chisel3.util.Cat(
    dut.io.accMemoryConfig.StoreMicroTaskEndReady, dut.io.accMemoryConfig.LoadMicroTaskEndReady,
    dut.io.trLoader1Config.MicroTaskEndReady, dut.io.trLoader0Config.MicroTaskEndReady)
  io.convertActive(0) := dut.io.trLoader0Config.ConvertF32ToMx
  io.convertMode(0) := dut.io.trLoader0Config.ConvertComputeType
  io.convertElements(0) := dut.io.trLoader0Config.ConvertKElements
  io.convertBank(0) := dut.io.trLoader0Config.ConvertScaleBankId
  io.sourceBeats(0) := dut.io.trLoader0Config.Tensor.K_Beat_Count
  io.packedK(0) := dut.io.trLoader0Config.MatrixRegTensor_K
  io.matrixRows(0) := dut.io.trLoader0Config.MatrixRegTensor_M
  io.sourceDataType(0) := dut.io.trLoader0Config.Tensor.dataType
  io.convertActive(1) := dut.io.trLoader1Config.ConvertF32ToMx
  io.convertMode(1) := dut.io.trLoader1Config.ConvertComputeType
  io.convertElements(1) := dut.io.trLoader1Config.ConvertKElements
  io.convertBank(1) := dut.io.trLoader1Config.ConvertScaleBankId
  io.sourceBeats(1) := dut.io.trLoader1Config.Tensor.K_Beat_Count
  io.packedK(1) := dut.io.trLoader1Config.MatrixRegTensor_K
  io.matrixRows(1) := dut.io.trLoader1Config.MatrixRegTensor_M
  io.sourceDataType(1) := dut.io.trLoader1Config.Tensor.dataType
  dut.io.ADC_MicroTask_Config.MicroTaskReady := true.B
  dut.io.ADC_MicroTask_Config.MicroTaskEndValid := io.done(2)
  dut.io.BDC_MicroTask_Config.MicroTaskReady := true.B
  dut.io.BDC_MicroTask_Config.MicroTaskEndValid := io.done(3)
  dut.io.CDC_MicroTask_Config.MicroTaskReady := true.B
  dut.io.CDC_MicroTask_Config.MicroTaskEndValid := io.done(6)
  dut.io.CDC_MicroTask_Config.MicroTaskFlags := io.completionFlags
  dut.io.CDC_MicroTask_Config.MicroTask_TEComputeEndValid := false.B
  val asl = dut.io.ASL_MicroTask_Config.get
  val bsl = dut.io.BSL_MicroTask_Config.get
  val asc = dut.io.ASC_MicroTask_Config.get
  val bsc = dut.io.BSC_MicroTask_Config.get
  asl.MicroTaskReady := io.dynamic.map(_.scaleReady(0)).getOrElse(true.B)
  asl.MicroTaskEndValid := io.done(0)
  bsl.MicroTaskReady := io.dynamic.map(_.scaleReady(1)).getOrElse(true.B)
  bsl.MicroTaskEndValid := io.done(1)
  io.dynamic.foreach { dyn =>
    dyn.generated := chisel3.util.Cat(bsl.Generated.get, asl.Generated.get)
    dyn.scaleEndReady := chisel3.util.Cat(bsl.MicroTaskEndReady, asl.MicroTaskEndReady)
  }
  asc.MicroTaskReady := true.B
  asc.MicroTaskEndValid := io.done(4)
  bsc.MicroTaskReady := true.B
  bsc.MicroTaskEndValid := io.done(5)
  io.ready := dut.io.ygjkctrl.amuCtrl.ready
  io.issued := chisel3.util.Cat(dut.io.MTE_MicroTask_Config.MicroTaskValid, bsl.MicroTaskValid, asl.MicroTaskValid)
  io.bankA := asl.ScaleBankId
  io.bankB := bsl.ScaleBankId
  io.loadRegModeA := asl.ApplicationScale_A.computeType
  io.loadRegModeB := bsl.ApplicationScale_B.computeType
  io.sourceScaleStride(0) := asl.ApplicationScale_A.RowStrideBytes
  io.sourceScaleStride(1) := bsl.ApplicationScale_B.RowStrideBytes
  io.loadKA := asl.MatrixRegTensor_K
  io.loadKB := bsl.MatrixRegTensor_K
  io.loadRowsA := asl.MatrixRegTensor_M
  io.loadRowsB := bsl.MatrixRegTensor_N
  io.computeType := dut.io.MTE_MicroTask_Config.computeType
  io.scaleStarts := chisel3.util.Cat(bsc.MicroTaskValid, asc.MicroTaskValid)
  io.computeBankA := asc.ScaleBankId
  io.computeBankB := bsc.ScaleBankId
  io.ascK := asc.MatrixRegTensor_K
  io.bscK := bsc.MatrixRegTensor_K
  io.adcK := dut.io.ADC_MicroTask_Config.MatrixRegTensor_K
  io.bdcK := dut.io.BDC_MicroTask_Config.MatrixRegTensor_K
  io.ascBlocks := asc.ScaleBlocksPerRow
  io.bscBlocks := bsc.ScaleBlocksPerRow
  io.ascElements := asc.KElements
  io.bscElements := bsc.KElements
  io.active := dut.io.perfProbe.mteActive
  io.retire := dut.io.perfProbe.retire
}

class TaskControllerScaleSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val scaleRegCount = CuteParams.CUTE_2Tops.ABScaleRegCount
  private val conversionBackendAnnotations =
    if (sys.env.get("CUTE_TEST_VERILATOR").contains("1")) Seq(VerilatorBackendAnnotation,
      chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) else Seq.empty

  private def scale(a: Boolean, reg: Int, mode: Int = 10, rows: Int = 4, k: Int = 128,
                    sourceBase: BigInt = 0x1000, sourceStride: Option[BigInt] = None): BigInt = {
    val blocks = (k + 31) / 32
    (BigInt(mode) << 127) | (BigInt(1) << 126) | (BigInt(reg) << 122) |
      (BigInt(1) << (if (a) 118 else 117)) | (sourceBase << 69) |
      (sourceStride.getOrElse(BigInt(blocks)) << 21) |
      (BigInt(rows) << 12) | (BigInt(blocks) << 3)
  }
  private def mma(a: Int = 0, b: Int = 1, mode: Int = 10, m: Int = 4, n: Int = 4, k: Int = 128): BigInt = {
    val types = if (mode == 10) 7 else if (mode == 7) 4 else 0
    (BigInt(mode) << 53) | (BigInt(a) << 41) | (BigInt(b) << 37) |
      (BigInt(m) << 28) | (BigInt(n) << 19) | (BigInt(k) << 10) |
      (BigInt(types) << 7) | (BigInt(types) << 4) | 1
  }
  private def normalLoad(a: Boolean, reg: Int, rows: Int = 4, k: Int = 64, widths: Int = 2): BigInt =
    (BigInt(reg) << 122) | (BigInt(1) << (if (a) 118 else 117)) |
      (BigInt(0x2000) << 69) | (BigInt(256) << 21) |
      (BigInt(if (a) rows else k) << 12) | (BigInt(if (a) k else rows) << 3) | BigInt(widths)
  private def convert(a: Boolean, reg: Int, mode: Int = 10, rows: Int = 4, k: Int = 64): BigInt =
    normalLoad(a, reg, rows, k) | (BigInt(1) << 131) | (BigInt(mode) << 127)
  private def init(d: ScaleSchedulerTop): Unit = {
    d.io.start.poke(false.B); d.io.op.poke(0.U); d.io.payload.foreach(_.poke(0.U)); d.io.done.poke(0.U)
    d.io.minitClear.poke(false.B); d.io.downstreamIdle.poke(true.B)
    d.io.loaderDone.poke(0.U)
    d.io.loaderFlags.foreach(_.poke(0.U))
    d.io.dynamic.foreach { dyn =>
      dyn.dataReady.poke(3.U)
      dyn.scaleReady.poke(3.U)
    }
    d.io.completionFlags.poke(0.U); d.io.returnReady.poke(true.B)
    d.io.traceRetireId.poke(0.U); d.io.traceEpoch.poke(0.U)
    d.clock.step(2)
  }
  private def push(d: ScaleSchedulerTop, op: Int, data: BigInt): Unit = {
    d.io.op.poke(op.U)
    d.io.payload.zipWithIndex.foreach { case (word, index) =>
      word.poke(((data >> (index * 32)) & ((BigInt(1) << 32) - 1)).U)
    }
    d.io.start.poke(true.B)
    d.io.ready.expect(true.B)
    d.clock.step()
    d.io.start.poke(false.B)
  }
  private def await(d: ScaleSchedulerTop, event: Int): Unit = {
    var cycles = 0
    while (d.io.issued.peek().litValue != event && cycles < 30) {
      d.clock.step(); cycles += 1
    }
    d.io.issued.expect(event.U)
  }
  private def done(d: ScaleSchedulerTop, mask: Int): Unit = {
    d.io.done.poke(mask.U); d.clock.step(); d.io.done.poke(0.U)
  }
  private def awaitLoader(d: ScaleSchedulerTop, side: Int): Unit = {
    var cycles = 0
    while (d.io.loaderIssued.peek().litValue != (1 << side) && cycles < 30) {
      d.clock.step(); cycles += 1
    }
    d.io.loaderIssued.expect((1 << side).U)
  }
  private def finishLoader(d: ScaleSchedulerTop, side: Int, flags: Int = 0): Unit = {
    d.io.loaderFlags(side).poke(flags.U)
    d.io.loaderDone.poke((1 << side).U)
    d.clock.step()
    d.io.loaderDone.poke(0.U)
    d.io.loaderFlags(side).poke(0.U)
  }

  for (enabled <- Seq(false, true)) {
    it should s"leave ordinary MLS geometry and return semantics unchanged with converting loads=$enabled" in {
      test(new ScaleSchedulerTop(32, convertEnabled = enabled)).withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        for (a <- Seq(true, false); widths <- Seq(0, 2, 7)) {
          val side = if (a) 0 else 1
          push(d, 1, normalLoad(a, reg = 2, rows = 3, widths = widths))
          awaitLoader(d, side)
          val beats = if (widths == 2) 4 else 1
          d.io.sourceBeats(side).expect(beats.U)
          d.io.packedK(side).expect(beats.U)
          d.io.matrixRows(side).expect(3.U)
          for (i <- 0 until 2) {
            d.io.convertActive(i).expect(false.B)
            d.io.convertMode(i).expect(0.U)
            d.io.convertElements(i).expect(0.U)
            d.io.convertBank(i).expect(0.U)
          }
          d.clock.step()
          // Ordinary loaders never produce an architectural flags return.
          finishLoader(d, side, flags = 31)
          d.clock.step(3)
          d.io.returnValid.expect(false.B)
          d.io.scaleInUse.expect(0.U)
          d.io.resetReady.expect(true.B)
        }
      }
    }
  }

  it should "reject the explicit converting-load marker while its feature is disabled" in {
    assertThrows[ChiselAssertionError] {
      test(new ScaleSchedulerTop(32)).withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        push(d, 1, convert(a = true, reg = 0))
        d.clock.step(8)
      }
    }
  }

  for ((label, payload) <- Seq(
    "non-F32 source" -> (convert(a = true, reg = 0) & ~BigInt(7)),
    "transpose" -> (convert(a = true, reg = 0) | (BigInt(1) << 120)),
    "C destination" -> ((convert(a = true, reg = 0) & ~(BigInt(1) << 118)) | (BigInt(1) << 119)),
    "scale writer marker" -> (convert(a = true, reg = 0) | (BigInt(1) << 126)),
    "unsupported mode" -> convert(a = true, reg = 0, mode = 6),
    "K32" -> convert(a = true, reg = 0, k = 32),
    "oversize rows" -> convert(a = false, reg = 0, rows = 129)
  )) {
    it should s"reject converting-load $label as configuration rather than numeric NV" in {
      assertThrows[ChiselAssertionError] {
        test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
          init(d)
          push(d, 1, payload)
          d.clock.step(8)
        }
      }
    }
  }

  for (lanes <- Seq(8, 32); reduceBytes <- Seq(32, 64)) {
    it should s"split K64 F32 source beats from packed MX geometry for lanes=$lanes reduceBytes=$reduceBytes" in {
      test(new ScaleSchedulerTop(reduceBytes, convertEnabled = true, convertLanes = lanes))
        .withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        // Put each consumer in bank 1: selecting the default bank or decoding
        // ms1/ms2 from MLS bits must not accidentally satisfy this fixture.
        load(d, a = true, reg = 0, rows = 64, k = 64)
        load(d, a = false, reg = 1, rows = 64, k = 64)
        for (mode <- Seq(10, 7, 8); a <- Seq(true, false)) {
          val side = if (a) 0 else 1
          val rows = if (a) 64 else 3
          val bank = load(d, a, reg = 2, mode = mode, rows = rows, k = 64)
          assert(bank == 1)
          push(d, 1, convert(a, reg = 2, mode = mode, rows = rows))
          awaitLoader(d, side)
          d.io.convertActive(side).expect(true.B)
          d.io.convertActive(1 - side).expect(false.B)
          d.io.convertMode(side).expect(mode.U)
          d.io.convertElements(side).expect(64.U)
          d.io.convertBank(side).expect(bank.U)
          d.io.sourceBeats(side).expect(4.U)
          d.io.packedK(side).expect(((if (mode == 10) 32 else 64) / reduceBytes).max(1).U)
          d.io.sourceDataType(side).expect(ElementDataType.DataTypeWidth32)
          d.io.matrixRows(side).expect(rows.U)
          d.io.scaleStarts.expect(0.U) // No compute scale-controller task.
          d.clock.step()
          d.io.scaleInUse.expect((1 << (bank + scaleRegCount * side)).U)
          d.io.loaderEndReady.expect((1 << side).U)
          finishLoader(d, side)
          d.io.scaleInUse.expect(0.U)
          d.io.returnValid.expect(true.B)
          d.io.returnFlagsValid.expect(true.B)
          d.io.returnFlags.expect(0.U) // Success is still an owned flags event.
          d.clock.step(3)
        }
      }
    }
  }

  it should "reject dynamic quantization while only supplied conversion is enabled" in {
    assertThrows[ChiselAssertionError] {
      test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        push(d, 1, convert(a = true, reg = 2, rows = 8) | (BigInt(1) << 132))
        d.clock.step(8)
      }
    }
  }

  for ((label, payload) <- Seq(
    "rows7" -> convert(a = true, reg = 2, rows = 7),
    "MX8" -> convert(a = false, reg = 2, mode = 7, rows = 8),
    "K32" -> convert(a = true, reg = 2, rows = 8, k = 32),
    "transpose" -> (convert(a = false, reg = 2, rows = 8) | (BigInt(1) << 120)),
    "reserved133" -> (convert(a = true, reg = 2, rows = 8) | (BigInt(1) << 133)),
    "without convert marker" -> normalLoad(a = true, reg = 2, rows = 8)
  )) {
    it should s"reject dynamic malformed $label before any producer is issued" in {
      assertThrows[ChiselAssertionError] {
        test(new ScaleSchedulerTop(32, convertEnabled = true, dynamicEnabled = true))
          .withAnnotations(conversionBackendAnnotations) { d =>
          init(d)
          push(d, 1, payload | (BigInt(1) << 132))
          d.clock.step(8)
        }
      }
    }
  }

  for (a <- Seq(true, false); dataFirst <- Seq(true, false)) {
    it should s"dynamic TR SF joint End publish once and recover rejection A$a dataFirst$dataFirst" in {
      test(new ScaleSchedulerTop(32, traceEnabled = true,
        convertEnabled = true, dynamicEnabled = true))
        .withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        val side = if (a) 0 else 1
        val dyn = d.io.dynamic.get
        val bit = 1 << side
        for ((flags, iteration) <- Seq(16, 0).zipWithIndex) {
          // Reservation cannot begin with only the data engine available.
          dyn.scaleReady.poke((3 ^ bit).U)
          push(d, 1, convert(a, reg = 2, rows = 8) | (BigInt(1) << 132))
          d.clock.step(3)
          d.io.loaderIssued.expect(0.U)
          d.io.issued.expect(0.U)
          d.io.residentMask.expect(0.U)
          dyn.latest(side).expect((iteration + 1).U)
          dyn.dataReady.poke((3 ^ bit).U)
          dyn.scaleReady.poke(3.U)
          for (_ <- 0 until 3) {
            d.io.loaderIssued.expect(0.U)
            d.io.issued.expect(0.U)
            dyn.occupied.expect(0.U)
            d.clock.step()
          }
          dyn.dataReady.poke(3.U)
          awaitLoader(d, side)
          d.io.issued.expect(bit.U)
          dyn.generated.expect(bit.U)
          d.io.matrixRows(side).expect(8.U)
          d.io.sourceBeats(side).expect(4.U)
          d.io.packedK(side).expect(1.U)
          val bank = d.io.convertBank(side).peek().litValue.toInt
          val bankBit = 1 << (side * scaleRegCount + bank)
          d.clock.step()
          dyn.occupied.expect(bankBit.U)
          d.io.scaleInUse.expect(0.U) // Producer, not old-generation reader.
          d.io.returnReady.poke(false.B)
          if (dataFirst) d.io.loaderDone.poke(bit.U) else d.io.done.poke(bit.U)
          d.io.loaderFlags(side).poke(flags.U)
          for (_ <- 0 until 4) {
            // Ready may anticipate the missing peer; only the held valid
            // engine must be blocked, so neither End actually fires early.
            d.io.loaderEndReady.expect((if (dataFirst) 0 else bit).U)
            dyn.scaleEndReady.expect((if (dataFirst) bit else 0).U)
            d.io.residentMask.expect(0.U)
            d.io.retire.expect(false.B)
            d.io.returnValid.expect(false.B)
            d.io.resetReady.expect(false.B)
            d.io.scaleOwners(side).complete.expect(false.B)
            d.io.scaleOwners(side).published.expect(false.B)
            d.clock.step()
          }
          d.io.loaderDone.poke(bit.U)
          d.io.done.poke(bit.U)
          d.io.loaderEndReady.expect(bit.U)
          dyn.scaleEndReady.expect(bit.U)
          d.io.scaleOwners(side).complete.expect(true.B)
          d.io.scaleOwners(side).completionPublishes.get.expect((flags == 0).B)
          d.io.scaleOwners(side).published.expect(false.B) // Pre-edge bank state.
          d.io.retire.expect(true.B)
          d.clock.step()
          d.io.loaderDone.poke(0.U)
          d.io.done.poke(0.U)
          d.io.loaderFlags(side).poke(31.U) // No longer owned.
          d.io.residentMask.expect((if (flags == 0) bankBit else 0).U)
          dyn.occupied.expect((if (flags == 0) bankBit else 0).U)
          d.io.scaleOwners(side).complete.expect(false.B)
          d.io.scaleOwners(side).published.expect((flags == 0).B)
          d.io.returnFlagsValid.expect(true.B)
          d.io.returnFlags.expect(flags.U)
          push(d, 2, 3)
          for (_ <- 0 until 3) {
            d.io.returnFlags.expect(flags.U)
            d.io.returnMsync.expect(0.U)
            d.io.resetReady.expect(false.B)
            d.clock.step()
          }
          val returned = scala.collection.mutable.ArrayBuffer.empty[(Boolean, Int, BigInt)]
          d.io.returnReady.poke(true.B)
          var cycles = 0
          while (!d.io.resetReady.peek().litToBoolean && cycles < 30) {
            if (d.io.returnValid.peek().litToBoolean)
              returned += ((d.io.returnFlagsValid.peek().litToBoolean,
                d.io.returnFlags.peek().litValue.toInt, d.io.returnMsync.peek().litValue))
            d.clock.step()
            cycles += 1
          }
          assert(returned.toSeq == Seq((true, flags, BigInt(0)), (false, 0, BigInt(8))))
          d.io.resetReady.expect(true.B)
          // Do not clear between rejection and success: generation advances,
          // failed bank is reusable, and the old NV must not contaminate it.
          d.io.loaderFlags(side).poke(0.U)
        }
        d.io.minitClear.poke(true.B)
        d.clock.step()
        d.io.minitClear.poke(false.B)
        d.io.minitClearAck.expect(true.B)
        d.io.residentMask.expect(0.U)
        d.io.generationPresent.expect(false.B)
      }
    }
  }

  it should "dynamic joint Ends remain side owned and return isolated flags in order" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true,
      convertEnabled = true, dynamicEnabled = true))
      .withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      d.io.returnReady.poke(false.B)
      push(d, 1, convert(a = true, reg = 2, rows = 8) | (BigInt(1) << 132))
      awaitLoader(d, 0)
      d.io.issued.expect(1.U)
      d.clock.step()
      // A/B share the TR namespace: independent concurrent owners need
      // different destinations, otherwise the original TR WAW must serialize.
      push(d, 1, convert(a = false, reg = 3, rows = 8) | (BigInt(1) << 132))
      awaitLoader(d, 1)
      d.io.issued.expect(2.U)
      d.clock.step()
      d.io.scaleOwners(0).regId.expect(2.U)
      d.io.scaleOwners(1).regId.expect(3.U)
      d.io.loaderFlags(0).poke(0.U)
      d.io.loaderFlags(1).poke(16.U)
      // A data End cannot join B scale End, even though both are valid.
      d.io.loaderDone.poke(1.U)
      d.io.done.poke(2.U)
      for (_ <- 0 until 4) {
        d.io.loaderEndReady.expect(2.U)
        d.io.dynamic.get.scaleEndReady.expect(1.U)
        d.io.scaleOwners.foreach(_.complete.expect(false.B))
        d.io.residentMask.expect(0.U)
        d.io.returnValid.expect(false.B)
        d.clock.step()
      }
      d.io.loaderDone.poke(3.U)
      d.io.scaleOwners(1).complete.expect(true.B)
      d.io.scaleOwners(1).completionPublishes.get.expect(false.B)
      d.io.scaleOwners(0).complete.expect(false.B)
      d.io.retire.expect(false.B) // Older A still has no scale End.
      d.clock.step()
      d.io.loaderDone.poke(1.U)
      d.io.done.poke(0.U)
      d.io.loaderFlags(1).poke(31.U) // Completed B must keep its owned16.
      d.io.residentMask.expect(0.U)
      d.io.done.poke(1.U)
      d.io.scaleOwners(0).complete.expect(true.B)
      d.io.scaleOwners(0).completionPublishes.get.expect(true.B)
      d.clock.step()
      d.io.loaderDone.poke(0.U)
      d.io.done.poke(0.U)
      push(d, 2, 3)
      d.clock.step(3)
      d.io.returnFlagsValid.expect(true.B)
      d.io.returnFlags.expect(0.U)
      val returned = scala.collection.mutable.ArrayBuffer.empty[(Boolean, Int, BigInt)]
      d.io.returnReady.poke(true.B)
      var cycles = 0
      while (!d.io.resetReady.peek().litToBoolean && cycles < 30) {
        if (d.io.returnValid.peek().litToBoolean)
          returned += ((d.io.returnFlagsValid.peek().litToBoolean,
            d.io.returnFlags.peek().litValue.toInt, d.io.returnMsync.peek().litValue))
        d.clock.step()
        cycles += 1
      }
      assert(returned.toSeq == Seq((true, 0, BigInt(0)), (true, 16, BigInt(0)),
        (false, 0, BigInt(8))))
      d.io.resetReady.expect(true.B)
    }
  }

  it should "dynamic generations feed the original consumer and retain scale WAR until reader End" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true,
      convertEnabled = true, dynamicEnabled = true))
      .withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      val banks = scala.collection.mutable.ArrayBuffer.empty[Int]
      for (side <- 0 until 2) {
        push(d, 1, convert(a = side == 0, reg = side, rows = 8) | (BigInt(1) << 132))
        awaitLoader(d, side)
        banks += d.io.convertBank(side).peek().litValue.toInt
        d.clock.step()
        d.io.loaderDone.poke((1 << side).U)
        d.io.done.poke((1 << side).U)
        d.clock.step()
        d.io.loaderDone.poke(0.U)
        d.io.done.poke(0.U)
        d.io.returnValid.expect(true.B)
        d.io.returnFlags.expect(0.U)
        d.clock.step()
      }
      // This success case observes both status returns and drains Release
      // before admitting consumers, matching the required software boundary.
      push(d, 2, 3)
      var waitCycles = 0
      while (!d.io.resetReady.peek().litToBoolean && waitCycles < 30) {
        d.clock.step()
        waitCycles += 1
      }
      d.io.resetReady.expect(true.B)
      push(d, 0, mma(m = 8, n = 8, k = 64))
      await(d, 4)
      d.io.scaleStarts.expect(3.U)
      d.io.computeBankA.expect(banks(0).U)
      d.io.computeBankB.expect(banks(1).U)
      d.io.mmaTraces(0).scaleGenA.expect(1.U)
      d.io.mmaTraces(0).scaleGenB.expect(1.U)
      d.io.ascK.expect(1.U)
      d.io.bscK.expect(1.U)
      d.io.ascBlocks.expect(2.U)
      d.io.bscBlocks.expect(2.U)
      d.clock.step()
      done(d, 0x0c) // Data readers ended, but their scale readers did not.
      push(d, 1, convert(a = true, reg = 0, rows = 8) | (BigInt(1) << 132))
      for (_ <- 0 until 4) {
        d.io.loaderIssued.expect(0.U)
        d.io.issued.expect(0.U)
        d.clock.step()
      }
      done(d, 0x10) // Only the exact A scale reader End permits its writer.
      awaitLoader(d, 0)
      d.io.issued.expect(1.U)
      d.io.dynamic.get.generated.expect(1.U)
      d.io.active.expect(true.B) // CDC and BSC still own the old compute.
      d.clock.step()
      d.io.loaderDone.poke(1.U)
      d.io.done.poke(1.U)
      d.clock.step()
      d.io.loaderDone.poke(0.U)
      d.io.done.poke(0.U)
      done(d, 0x60)
      d.clock.step(6)
      d.io.resetReady.expect(true.B)
      d.io.scaleInUse.expect(0.U)
    }
  }

  it should "keep missing or mismatched converting-load scales blocked without fabricating NV" in {
    test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      push(d, 1, convert(a = true, reg = 2))
      // This younger generation cannot repair the consumer's generation-0
      // snapshot or overwrite its unread scales. Missing SF is not numeric NV.
      push(d, 1, scale(a = true, reg = 2, k = 64))
      for (_ <- 0 until 12) {
        d.io.loaderIssued.expect(0.U)
        d.io.issued.expect(0.U)
        d.io.returnValid.expect(false.B)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
    }
    for ((scaleRows, scaleMode) <- Seq((8, 10), (4, 7))) {
      test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        load(d, a = false, reg = 2, mode = scaleMode, rows = scaleRows, k = 64)
        push(d, 1, convert(a = false, reg = 2))
        for (_ <- 0 until 8) {
          d.io.loaderIssued.expect(0.U)
          d.io.returnValid.expect(false.B)
          d.io.scaleInUse.expect(0.U)
          d.clock.step()
        }
      }
    }
  }

  for (a <- Seq(true, false)) {
    it should s"hold converting-load scale WAR until real loader End and bind the pending reload on A=$a" in {
      test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
        init(d)
        val side = if (a) 0 else 1
        val scaleEvent = 1 << side
        val bank = load(d, a, reg = 2, k = 64)
        push(d, 1, convert(a, reg = 2))
        awaitLoader(d, side); d.clock.step()
        push(d, 1, scale(a, reg = 2, k = 64))
        push(d, 1, convert(a, reg = 2)) // Must snapshot generation 2.
        for (_ <- 0 until 6) {
          d.io.issued.expect(0.U); d.io.loaderIssued.expect(0.U)
          d.io.scaleInUse.expect((1 << (bank + scaleRegCount * side)).U)
          d.clock.step()
        }
        finishLoader(d, side, flags = 16)
        await(d, scaleEvent)
        (if (a) d.io.bankA else d.io.bankB).expect(bank.U)
        d.clock.step()
        // Reload reservation is not publication; the newer load still waits.
        for (_ <- 0 until 4) {
          d.io.loaderIssued.expect(0.U)
          d.clock.step()
        }
        done(d, scaleEvent)
        awaitLoader(d, side)
        d.io.convertBank(side).expect(bank.U)
        d.clock.step()
        finishLoader(d, side)
        d.clock.step(4)
        d.io.resetReady.expect(true.B)
      }
    }
  }

  for (aSide <- Seq(true, false)) {
  it should s"serialize same-side scale readers while retaining distinct banks for A=$aSide" in {
    test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      val side = if (aSide) 0 else 1
      val convertReg = if (aSide) 2 else 3
      val aBank = load(d, a = true, reg = 0, k = 64)
      val bBank = load(d, a = false, reg = 1, k = 64)
      val convertBank = load(d, aSide, reg = convertReg, k = 64)
      assert(convertBank != (if (aSide) aBank else bBank))
      val convertingBank = 1 << (scaleRegCount * side + convertBank)

      // The converting load owns the single scale-read port on its side.
      push(d, 1, convert(aSide, reg = convertReg))
      awaitLoader(d, side); d.clock.step()
      d.io.scaleInUse.expect(convertingBank.U)
      push(d, 0, mma(k = 64))
      for (_ <- 0 until 6) {
        d.io.issued.expect(0.U)
        d.io.scaleInUse.expect(convertingBank.U)
        d.clock.step()
      }

      // Once that reader releases, compute uses its own resident banks.
      finishLoader(d, side)
      await(d, 4); d.clock.step()
      val computeBanks = (1 << aBank) | (1 << (scaleRegCount + bBank))
      d.io.scaleInUse.expect(computeBanks.U)

      // Completing the other side must not release this side's owned bank.
      done(d, if (aSide) 0x08 | 0x20 else 0x04 | 0x10)
      val computeBank = 1 << (scaleRegCount * side + (if (aSide) aBank else bBank))
      push(d, 1, convert(aSide, reg = convertReg))
      for (_ <- 0 until 6) {
        assert((d.io.loaderIssued.peek().litValue & (1 << side)) == 0)
        d.io.scaleInUse.expect(computeBank.U)
        d.clock.step()
      }

      // The second converting load is delayed, not permanently blocked:
      // releasing this side's scale-read owner must admit it.
      done(d, if (aSide) 1 << 4 else 1 << 5)
      d.clock.step()
      var admitCycles = 0
      while (d.io.scaleInUse.peek().litValue != convertingBank && admitCycles < 10) {
        d.clock.step(); admitCycles += 1
      }
      d.io.scaleInUse.expect(convertingBank.U)
      d.io.loaderEndReady.expect((1 << side).U)
      finishLoader(d, side)
      d.io.scaleInUse.expect(0.U)
      done(d, if (aSide) 1 << 2 else 1 << 3)
      done(d, 0x40)
      d.clock.step(4); d.io.resetReady.expect(true.B)
      println(s"MX_SCALE_SERIALIZED_READERS A=$aSide distinct_resident_banks=PASS")
    }
  }
  }

  it should "bypass final loader flags and serialize Release across a full return queue and ring wrap" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true, convertEnabled = true))
      .withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      load(d, a = true, reg = 2, k = 64)
      load(d, a = false, reg = 3, k = 64)
      for (_ <- 0 until 5) push(d, 3, 0)
      d.clock.step(10)
      d.io.headIndex.expect(7.U)
      d.io.returnReady.poke(false.B)
      d.io.traceRetireId.poke(41.U)
      push(d, 1, convert(a = true, reg = 2))
      awaitLoader(d, 0); d.clock.step()
      d.io.traceRetireId.poke(42.U)
      push(d, 1, convert(a = false, reg = 3))
      awaitLoader(d, 1); d.clock.step()
      d.io.traceRetireId.poke(43.U)
      push(d, 2, 3)
      d.clock.step(3)
      d.io.returnValid.expect(false.B)
      d.io.loaderFlags(0).poke(16.U); d.io.loaderDone.poke(1.U)
      d.io.retire.expect(true.B)
      d.io.retirementTraces(0).valid.expect(true.B)
      d.io.retirementTraces(0).retireId.expect(41.U)
      d.io.retirementTraces(0).flagsRaised.expect(16.U)
      d.clock.step(); d.io.loaderDone.poke(0.U)
      d.io.loaderFlags(0).poke(31.U) // Not part of the retired owner.
      d.io.returnFlags.expect(16.U)
      d.io.loaderFlags(1).poke(1.U); d.io.loaderDone.poke(2.U)
      d.io.retire.expect(false.B) // Head finishes, queue still belongs to A.
      d.clock.step(); d.io.loaderDone.poke(0.U)
      d.io.loaderFlags(1).poke(31.U)
      for (_ <- 0 until 4) {
        d.io.returnFlags.expect(16.U)
        d.io.returnMsync.expect(0.U)
        d.io.retire.expect(false.B)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
      // Full pop/push edge: A return is accepted, B retires once, Release
      // cannot also enqueue despite B being completed before this edge.
      d.io.returnReady.poke(true.B)
      d.io.retire.expect(true.B)
      d.io.retirementTraces(0).retireId.expect(42.U)
      d.io.retirementTraces(0).flagsRaised.expect(1.U)
      d.io.retirementTraces(1).retireId.expect(41.U)
      d.clock.step()
      d.io.returnFlagsValid.expect(true.B)
      d.io.returnFlags.expect(1.U)
      d.io.returnMsync.expect(0.U)
      d.io.retirementTraces(1).retireId.expect(42.U)
      d.clock.step() // B flags accepted before pure Release becomes visible.
      d.io.returnFlagsValid.expect(false.B)
      d.io.returnMsync.expect(8.U)
      d.clock.step(4)
      d.io.returnValid.expect(false.B)
      d.io.resetReady.expect(true.B)
      d.io.minitClear.poke(true.B)
      d.clock.step(); d.io.minitClear.poke(false.B)
      d.io.minitClearAck.expect(true.B)
      d.io.residentMask.expect(0.U)
      d.io.generationPresent.expect(false.B)
    }
  }

  it should "retain converted flags through full-window same-slot replacement" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true, convertEnabled = true))
      .withAnnotations(conversionBackendAnnotations) { d =>
      init(d)
      load(d, a = true, reg = 2, k = 64)
      d.io.returnReady.poke(false.B)
      d.io.traceRetireId.poke(41.U)
      push(d, 1, convert(a = true, reg = 2))
      awaitLoader(d, 0); d.clock.step()
      for (id <- 42 until 50) {
        d.io.traceRetireId.poke(id.U)
        push(d, 3, 0)
      }
      d.clock.step(3); d.io.windowCount.expect(8.U)
      d.io.loaderFlags(0).poke(16.U); d.io.loaderDone.poke(1.U)
      d.io.retire.expect(true.B)
      d.io.retirementTraces(0).retireId.expect(41.U)
      d.clock.step(); d.io.loaderDone.poke(0.U); d.io.loaderFlags(0).poke(0.U)
      d.io.windowCount.expect(8.U)
      for (_ <- 0 until 12) {
        d.io.returnFlags.expect(16.U)
        d.io.returnFlagsValid.expect(true.B)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
      d.io.windowCount.expect(0.U)
      d.io.returnReady.poke(true.B)
      d.io.retirementTraces(1).valid.expect(true.B)
      d.io.retirementTraces(1).retireId.expect(41.U)
      d.io.retirementTraces(1).flagsRaised.expect(16.U)
      d.clock.step()
      d.io.returnValid.expect(false.B)
      d.io.resetReady.expect(true.B)
    }
  }

  for (clearPhase <- Seq("loader-end", "held-flags", "release-pop")) {
    it should s"reject MINIT clear before conversion and its ordered return drain at $clearPhase" in {
      assertThrows[ChiselAssertionError] {
        test(new ScaleSchedulerTop(32, convertEnabled = true)).withAnnotations(conversionBackendAnnotations) { d =>
          init(d)
          load(d, a = false, reg = 2, k = 64)
          d.io.returnReady.poke(false.B)
          push(d, 1, convert(a = false, reg = 2))
          awaitLoader(d, 1); d.clock.step()
          d.io.resetReady.expect(false.B)
          if (clearPhase == "loader-end") {
            d.io.loaderFlags(1).poke(16.U); d.io.loaderDone.poke(2.U)
          } else {
            finishLoader(d, 1, flags = 16)
            d.clock.step(3)
            d.io.returnValid.expect(true.B)
            if (clearPhase == "release-pop") {
              push(d, 2, 3)
              d.clock.step(3)
              // The old flags pop and pure Release enqueue share this edge;
              // neither is permission to clear the still-owned return stream.
              d.io.returnReady.poke(true.B)
            }
          }
          d.io.resetReady.expect(false.B)
          d.io.minitClear.poke(true.B)
          d.clock.step()
        }
      }
    }
  }

  it should "admit compact unaligned scale rectangles contained in one cacheline" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      for (aSide <- Seq(true, false)) {
        push(d, 1, scale(aSide, reg = 0, rows = 1, k = 64, sourceBase = 0x1036))
        await(d, if (aSide) 1 else 2)
        if (aSide) d.io.loadRowsA.expect(1.U) else d.io.loadRowsB.expect(1.U)
        d.clock.step()
        done(d, if (aSide) 1 else 2)
        d.clock.step(4)
      }
    }
  }
  it should "forward a noncompact source scale stride without changing the packed geometry" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      for (aSide <- Seq(true, false)) {
        val side = if (aSide) 0 else 1
        val stride = if (aSide) BigInt(129) else (BigInt(1) << 35) + 129
        push(d, 1, scale(aSide, reg = 0, rows = 33, k = 64, sourceBase = 0x103f,
          sourceStride = Some(stride)))
        await(d, 1 << side)
        d.io.sourceScaleStride(side).expect(stride.U)
        (if (aSide) d.io.loadRowsA else d.io.loadRowsB).expect(33.U)
        (if (aSide) d.io.loadKA else d.io.loadKB).expect(1.U)
        d.clock.step()
        done(d, 1 << side)
        d.clock.step(4)
      }
    }
  }
  it should "reject a scale source stride shorter than the logical row" in {
    assertThrows[ChiselAssertionError] {
      test(new ScaleSchedulerTop(32)) { d =>
        init(d)
        push(d, 1, scale(a = true, reg = 0, rows = 4, k = 64, sourceStride = Some(BigInt(1))))
        d.clock.step(8)
      }
    }
  }
  it should "reject an unaligned scale rectangle that crosses its containing cacheline" in {
    assertThrows[ChiselAssertionError] {
      test(new ScaleSchedulerTop(32)) { d =>
        init(d)
        push(d, 1, scale(a = true, reg = 0, rows = 1, k = 64, sourceBase = 0x103f))
        d.clock.step(8)
      }
    }
  }
  it should "trace actual scale FU owners and distinguish reservation from publication" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true)) { d =>
      init(d)
      d.io.traceRetireId.poke(41.U); d.io.traceEpoch.poke(3.U)
      push(d, 1, scale(a = true, reg = 2, rows = 4, k = 64))
      await(d, 1); d.clock.step()
      val a = d.io.scaleOwners(0)
      val b = d.io.scaleOwners(1)
      a.busy.expect(true.B); a.complete.expect(false.B); a.published.expect(false.B)
      a.retireId.expect(41.U); a.epoch.expect(3.U); a.regId.expect(2.U)
      a.generation.expect(1.U); a.rows.expect(4.U); a.blocks.expect(2.U)
      a.rowPitch.expect(2.U); a.kVectors.expect(1.U)
      d.io.traceRetireId.poke(42.U)
      push(d, 1, scale(a = false, reg = 2, rows = 8, k = 64))
      await(d, 2); d.clock.step()
      // Same PC and same architectural reg number, independent side/FU owners.
      a.retireId.expect(41.U); b.retireId.expect(42.U)
      b.busy.expect(true.B); b.regId.expect(2.U); b.rows.expect(8.U)
      b.generation.expect(1.U); b.published.expect(false.B)
      d.io.done.poke(2.U)
      b.complete.expect(true.B); b.published.expect(false.B)
      d.clock.step(); d.io.done.poke(0.U)
      b.busy.expect(false.B); b.published.expect(true.B)
      a.busy.expect(true.B); a.retireId.expect(41.U)
      d.io.done.poke(1.U)
      a.complete.expect(true.B); a.published.expect(false.B)
      d.clock.step(); d.io.done.poke(0.U); d.clock.step(4)
      d.io.traceRetireId.poke(43.U)
      push(d, 1, scale(a = true, reg = 2, rows = 4, k = 64))
      await(d, 1); d.clock.step()
      a.retireId.expect(43.U); a.generation.expect(2.U)
      a.published.expect(false.B); a.complete.expect(false.B)
      done(d, 1); d.clock.step(4)
      a.published.expect(true.B)
    }
  }

  private def checkMxDifftestSchema(before: Int, expectedMxLanes: Int): Unit = {
    val events = difftest.DifftestModule.get_current_interfaces().drop(before).map(_._1)
    val legacyFinish = events.filter(_.desiredCppName == "amu_finish")
    assert(legacyFinish.size == 3 && legacyFinish.forall(_.isInstanceOf[difftest.DiffAmuFinishEvent]))
    assert(events.exists(_.desiredCppName == "amu_hash_finish"))
    assert(events.count(_.desiredCppName == "amu_writeback_block") == 1)
    val mxFinish = events.collect { case event: difftest.DiffAmuMxFinishEvent => event }
    assert(mxFinish.size == expectedMxLanes)
    assert(mxFinish.map(event => (event.nBanks, event.wordsPerBank, event.bankAddrBits)).distinct.size == 1)
    assert(mxFinish.forall(_.nBanks == 4))
  }

  for (dynamic <- Seq(false, true); lanes <- Seq(8, 32)) {
    val description = if (dynamic)
      s"lower the real dynamic converting-load MN128 Difftest top with owned SF routing for lanes=$lanes"
    else s"lower the real converting-load top with owned SF routing for lanes=$lanes"
    it should description in {
      implicit val p: Parameters = baseConfig(6).alterPartial {
        case CuteParamsKey => (if (dynamic) CuteParams.CUTE_2Tops_128SCP
          else CuteParams.CUTE_2Tops_64SCP).copy(
          EnableDifftest = dynamic, Debug = CuteDebugParams.NoDebug,
          ExperimentalMxfpConvertLoad = true, MxfpConvertLanes = lanes,
          ExperimentalMxfpDynamicQuant = dynamic,
          MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
            enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
      }
      utility.ChiselDB.init(false)
      utility.Constantin.init(false)
      val before = difftest.DifftestModule.get_current_interfaces().size
      val opts = Array("-O=release", "--disable-annotation-unknown",
        "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")
      val sv = _root_.circt.stage.ChiselStage.emitSystemVerilog(new CUTEV2Top, firtoolOpts = opts)
      assert(sv.nonEmpty, "CIRCT did not lower the actual converting-load top")
      if (dynamic) {
        // Lower the original wrappers -> sole ASL/BSL writers and real owner
        // traces. This is structural coverage, not a CPU execution/difftest run.
        assert(p(CuteParamsKey).Matrix_MN == 4 && p(CuteParamsKey).Tensor_MN == 128)
        assert(AmuConvertLoad.DynamicBit == 132)
        assert(sv.contains("GeneratedScale"), "actual generated SF stream was not lowered")
        assert(sv.contains("completionPublishes"), "actual owned publication permission was not lowered")
        checkMxDifftestSchema(before, expectedMxLanes = 8)
      }
      val hash = java.security.MessageDigest.getInstance("SHA-256")
        .digest(sv.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        .map(b => f"${b & 255}%02x").mkString
      if (dynamic)
        println(s"MXDYNAMIC-TOP MN128 2T16PE lanes=$lanes bit132=true difftest_structure=true " +
          s"svSHA256=$hash lowering_only=true flags=${opts.mkString(" ")}")
      else
        println(s"MXCONVERT-TOP lanes=$lanes svSHA256=$hash lowering_only=true flags=${opts.mkString(" ")}")
    }
  }

  it should "elaborate real scaled Difftest top and BML multi identity mapping" in {
    implicit val p: Parameters = baseConfig(6).alterPartial {
      case CuteParamsKey => CuteParams.CUTE_2Tops_128SCP.copy(
        EnableDifftest = true, Debug = CuteDebugParams.NoDebug,
        LoaderBridgeChannelConfig = "A1B1CL1CS2",
        MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
          enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
    }
    utility.ChiselDB.init(false)
    utility.Constantin.init(false)
    val before = difftest.DifftestModule.get_current_interfaces().size
    val fir = _root_.circt.stage.ChiselStage.emitCHIRRTL(new CUTEV2Top)
    assert(fir.contains("trLoader0Config.retireId") && fir.contains("trLoader1Config.epoch"))
    assert(fir.contains("ownedScaleTrace"), "actual FU-owned scale observation was not elaborated")
    checkMxDifftestSchema(before, expectedMxLanes = 6)
  }

  it should "trace true scaled MMA completion only after CDC and all readers" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true)) { d =>
      init(d)
      d.io.traceEpoch.poke(3.U); d.io.traceRetireId.poke(41.U)
      load(d, a = true, reg = 2, rows = 4, k = 64)
      d.io.traceRetireId.poke(42.U)
      load(d, a = false, reg = 2, rows = 8, k = 64)
      d.io.traceRetireId.poke(43.U)
      push(d, 0, mma(a = 2, b = 2, m = 4, n = 8, k = 64))
      await(d, 4)
      val bind = d.io.mmaTraces(0)
      val end = d.io.mmaTraces(1)
      bind.valid.expect(true.B); bind.retireId.expect(43.U); bind.epoch.expect(3.U)
      bind.scaleRegA.expect(2.U); bind.scaleRegB.expect(2.U)
      bind.scaleGenA.expect(1.U); bind.scaleGenB.expect(1.U)
      bind.scaleRowsA.expect(4.U); bind.scaleRowsB.expect(8.U)
      bind.scaleBlocksA.expect(2.U); bind.scaleKVectorsA.expect(1.U)
      d.clock.step()
      // CDC plus data readers can finish before the scale readers.
      d.io.completionFlags.poke(21.U); d.io.done.poke((4 | 8 | 64).U)
      end.valid.expect(false.B)
      d.clock.step(); d.io.done.poke(0.U)
      d.io.completionFlags.poke(31.U) // Later input is no longer this owner's flags.
      end.valid.expect(false.B)
      d.io.done.poke((16 | 32).U)
      end.valid.expect(true.B); end.finish.expect(true.B)
      end.retireId.expect(43.U); end.flagsValid.expect(true.B); end.flagsRaised.expect(21.U)
      d.clock.step(); d.io.done.poke(0.U); d.clock.step(4)
    }
  }

  it should "retain MMA bound generation after reader end allows a scale reload" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true)) { d =>
      init(d)
      d.io.traceEpoch.poke(3.U); d.io.traceRetireId.poke(41.U)
      load(d, a = true, reg = 2, rows = 4, k = 64)
      d.io.traceRetireId.poke(42.U)
      load(d, a = false, reg = 2, rows = 8, k = 64)
      d.io.traceRetireId.poke(43.U)
      push(d, 0, mma(a = 2, b = 2, m = 4, n = 8, k = 64))
      await(d, 4); d.clock.step()
      done(d, 4 | 8 | 16 | 32) // All readers end; CDC is still pending.
      d.io.mmaTraces(1).valid.expect(false.B)
      d.io.traceRetireId.poke(44.U)
      push(d, 1, scale(a = true, reg = 2, rows = 4, k = 64))
      await(d, 1); d.clock.step()
      d.io.scaleOwners(0).generation.expect(2.U)
      d.io.completionFlags.poke(3.U); d.io.done.poke(64.U)
      val end = d.io.mmaTraces(1)
      end.valid.expect(true.B); end.retireId.expect(43.U); end.scaleGenA.expect(1.U)
      end.flagsRaised.expect(3.U)
      d.clock.step(); d.io.done.poke(0.U)
      done(d, 1); d.clock.step(4)
    }
  }

  it should "trace queue owned retirement across held full-window same-slot reuse" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true)) { d =>
      init(d)
      d.io.traceEpoch.poke(3.U); d.io.traceRetireId.poke(41.U)
      load(d, a = true, reg = 0, k = 64)
      d.io.traceRetireId.poke(42.U)
      load(d, a = false, reg = 1, k = 64)
      d.io.returnReady.poke(false.B)
      d.io.traceRetireId.poke(43.U)
      push(d, 0, mma(k = 64))
      await(d, 4); d.clock.step()
      for (id <- 44 until 52) {
        d.io.traceRetireId.poke(id.U)
        push(d, 3, 0)
      }
      d.clock.step(3)
      d.io.windowCount.expect(8.U)
      d.io.completionFlags.poke(21.U); d.io.done.poke(0x7c.U)
      val retired = d.io.retirementTraces(0)
      val accepted = d.io.retirementTraces(1)
      retired.valid.expect(true.B); retired.retireId.expect(43.U); retired.epoch.expect(3.U)
      retired.flagsRaised.expect(21.U); accepted.valid.expect(false.B)
      d.clock.step() // The retired slot is reused on this edge.
      d.io.done.poke(0.U); d.io.completionFlags.poke(0.U)
      for (_ <- 0 until 12) {
        accepted.valid.expect(false.B)
        d.io.returnFlags.expect(21.U)
        d.clock.step()
      }
      d.io.windowCount.expect(0.U)
      d.io.returnReady.poke(true.B)
      accepted.valid.expect(true.B); accepted.retireId.expect(43.U); accepted.epoch.expect(3.U)
      accepted.flagsRaised.expect(21.U); accepted.computeType.expect(10.U)
      d.clock.step()
      accepted.valid.expect(false.B)
      d.io.resetReady.expect(true.B)
    }
  }

  it should "trace old return and new retirement on the same full queue edge" in {
    test(new ScaleSchedulerTop(32, traceEnabled = true)) { d =>
      init(d)
      d.io.traceEpoch.poke(3.U); d.io.traceRetireId.poke(41.U)
      load(d, a = true, reg = 0, k = 64)
      d.io.traceRetireId.poke(42.U)
      load(d, a = false, reg = 1, k = 64)
      d.io.returnReady.poke(false.B)
      for ((id, flags) <- Seq((43, 1), (44, 18))) {
        d.io.traceRetireId.poke(id.U)
        push(d, 0, mma(k = 64))
        await(d, 4); d.clock.step()
        d.io.completionFlags.poke(flags.U)
        done(d, 0x4c) // Save this owner's flags before scale reader completion.
        d.io.completionFlags.poke(31.U)
        d.io.done.poke(0x30.U)
        d.io.retirementTraces(0).valid.expect((id == 43).B)
        d.clock.step(); d.io.done.poke(0.U); d.clock.step(2)
      }
      for (_ <- 0 until 3) {
        d.io.retirementTraces(0).valid.expect(false.B)
        d.io.retirementTraces(1).valid.expect(false.B)
        d.clock.step()
      }
      d.io.returnReady.poke(true.B)
      val retired = d.io.retirementTraces(0)
      val accepted = d.io.retirementTraces(1)
      retired.valid.expect(true.B); retired.retireId.expect(44.U); retired.flagsRaised.expect(18.U)
      accepted.valid.expect(true.B); accepted.retireId.expect(43.U); accepted.flagsRaised.expect(1.U)
      d.clock.step()
      retired.valid.expect(false.B)
      accepted.valid.expect(true.B); accepted.retireId.expect(44.U); accepted.flagsRaised.expect(18.U)
      d.clock.step()
      accepted.valid.expect(false.B)
    }
  }

  it should "schedule exact K32 K33 K64 K65 on canonical eight by eight geometry" in {
    test(new ScaleSchedulerTop(reduceBytes = 32, canonical8 = true)) { d =>
      init(d)
      for (k <- Seq(32, 33, 64, 65)) {
        load(d, a = true, reg = 0, rows = 8, k = k)
        load(d, a = false, reg = 1, rows = 8, k = k)
        push(d, 0, mma(m = 8, n = 8, k = k))
        await(d, 4)
        val vectors = (k + 63) / 64
        d.io.ascK.expect(vectors.U); d.io.bscK.expect(vectors.U)
        d.io.adcK.expect(vectors.U); d.io.bdcK.expect(vectors.U)
        d.io.ascElements.expect(k.U); d.io.bscElements.expect(k.U)
        d.io.ascBlocks.expect(((k + 31) / 32).U)
        d.io.bscBlocks.expect(((k + 31) / 32).U)
        d.io.scaleStarts.expect(3.U)
        println(s"CANONICAL8 K$k: vectors=$vectors MX32blocks=${(k+31)/32} exactK=$k")
        d.clock.step()
        done(d, (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5) | (1 << 6))
        d.clock.step(4)
      }
    }
  }
  private def load(d: ScaleSchedulerTop, a: Boolean, reg: Int, mode: Int = 10, rows: Int = 4, k: Int = 128): Int = {
    push(d, 1, scale(a, reg, mode, rows, k))
    await(d, if (a) 1 else 2)
    val bank = (if (a) d.io.bankA else d.io.bankB).peek().litValue.toInt
    (if (a) d.io.loadRegModeA else d.io.loadRegModeB).expect(mode.U)
    (if (a) d.io.loadRowsA else d.io.loadRowsB).expect(rows.U)
    val scalesPerVector = d.reduceBytes / (if (mode == 10) 16 else 32)
    (if (a) d.io.loadKA else d.io.loadKB).expect((((k + 31) / 32 + scalesPerVector - 1) / scalesPerVector).U)
    d.clock.step()
    done(d, if (a) 1 else 2)
    d.clock.step()
    bank
  }

  it should "DFlags bypass final CDC and serialize older flags before Release across ring wrap" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      load(d, a = true, reg = 0, k = 64)
      load(d, a = false, reg = 1, k = 64)
      for (_ <- 0 until 5) push(d, 3, 0)
      d.clock.step(10)
      d.io.windowCount.expect(0.U)
      d.io.headIndex.expect(7.U) // Compute slot7, younger Release slot0.
      d.io.returnReady.poke(false.B)
      push(d, 0, mma(k = 64))
      await(d, 4); d.clock.step()
      push(d, 2, 3) // Existing Release payload: msync3.
      d.clock.step(3)
      d.io.returnValid.expect(false.B)
      done(d, 0x3c) // All data/scale readers before CDC.
      d.io.completionFlags.poke(0x15.U)
      d.io.done.poke(0x40.U)
      d.io.retire.expect(true.B)
      d.io.returnValid.expect(false.B) // flow=false, not yet CPU-visible.
      d.clock.step()
      d.io.done.poke(0.U); d.io.completionFlags.poke(0.U)
      for (_ <- 0 until 4) {
        d.io.returnValid.expect(true.B)
        d.io.returnFlagsValid.expect(true.B)
        d.io.returnFlags.expect(0x15.U)
        d.io.returnMsync.expect(0.U)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
      // Full Queue pop/push: this edge consumes OLD flags only. The queued
      // pure Release is visible next cycle, never on the sticky merge edge.
      d.io.returnReady.poke(true.B)
      d.io.returnFlags.expect(0x15.U)
      d.io.returnMsync.expect(0.U)
      d.clock.step()
      d.io.returnValid.expect(true.B)
      d.io.returnFlagsValid.expect(false.B)
      d.io.returnMsync.expect(8.U)
      d.clock.step()
      d.io.returnValid.expect(false.B)
      d.clock.step(3)
      d.io.resetReady.expect(true.B)
    }
  }

  it should "DFlags save early CDC and replace a full queue exactly once under backpressure" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      load(d, a = true, reg = 0, k = 64)
      load(d, a = false, reg = 1, k = 64)
      d.io.returnReady.poke(false.B)
      for (flags <- Seq(1, 0x12)) {
        push(d, 0, mma(k = 64))
        await(d, 4); d.clock.step()
        d.io.completionFlags.poke(flags.U)
        done(d, 0x4c) // CDC early, ASC/BSC still own the same compute.
        d.io.completionFlags.poke(0.U)
        d.clock.step(2)
        if (flags == 1) d.io.returnValid.expect(false.B)
        else d.io.returnFlags.expect(1.U)
        done(d, 0x30)
        d.clock.step(2)
      }
      for (_ <- 0 until 4) {
        d.io.returnFlags.expect(1.U)
        d.io.returnFlagsValid.expect(true.B)
        d.io.retire.expect(false.B) // New head completed but Queue is full.
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
      d.io.returnReady.poke(true.B)
      d.io.returnFlags.expect(1.U)
      d.io.retire.expect(true.B)
      d.clock.step()
      d.io.returnFlagsValid.expect(true.B)
      d.io.returnFlags.expect(0x12.U)
      d.clock.step()
      d.io.returnValid.expect(false.B)
      d.clock.step(3)
      d.io.resetReady.expect(true.B)
    }
  }

  it should "DFlags retain the enqueued event through full-window same-slot reuse" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      load(d, a = true, reg = 0, k = 64)
      load(d, a = false, reg = 1, k = 64)
      d.io.returnReady.poke(false.B)
      push(d, 0, mma(k = 64))
      await(d, 4); d.clock.step()
      // Head compute + seven younger NopLike slots fill the real window.
      for (_ <- 0 until 8) push(d, 3, 0)
      d.clock.step(3)
      d.io.windowCount.expect(8.U)
      d.io.completionFlags.poke(0x1f.U)
      d.io.done.poke(0x7c.U)
      d.io.retire.expect(true.B)
      d.clock.step() // Full-window retire+enqueue reuses the old head slot.
      d.io.done.poke(0.U); d.io.completionFlags.poke(0.U)
      d.io.windowCount.expect(8.U)
      for (_ <- 0 until 12) {
        d.io.returnValid.expect(true.B)
        d.io.returnFlags.expect(0x1f.U)
        d.io.returnMsync.expect(0.U)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
      d.io.windowCount.expect(0.U)
      d.io.returnReady.poke(true.B)
      d.clock.step()
      d.io.returnValid.expect(false.B)
      d.io.resetReady.expect(true.B)
    }
  }

  it should "MINIT wait for AML BML C-load and the final store acknowledgement" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      for (fu <- 0 until 4) {
        val acc = fu >= 2
        val store = fu == 3
        val data = (BigInt(if (fu == 1) 1 else 0) << 122) |
          (if (store) BigInt(1) << 121 else BigInt(0)) |
          (BigInt(1) << (if (acc) 119 else if (fu == 0) 118 else 117)) |
          (BigInt(0x1000) << 69) | (BigInt(32) << 21) |
          (BigInt(4) << 12) | (BigInt(32) << 3)
        push(d, 1, data)
        d.io.resetReady.expect(false.B)
        var waited = 0
        while (d.io.loaderIssued.peek().litValue != (1 << fu) && waited < 30) {
          d.clock.step(); waited += 1
        }
        d.io.loaderIssued.expect((1 << fu).U)
        d.clock.step()
        for (_ <- 0 until 4) {
          d.io.resetReady.expect(false.B); d.clock.step()
        }
        // A store's FU end is supplied by canonical CML only after all acks.
        d.io.loaderDone.poke((1 << fu).U)
        d.io.resetReady.expect(false.B)
        d.clock.step(); d.io.loaderDone.poke(0.U)
        d.clock.step(3); d.io.resetReady.expect(true.B)
      }
    }
  }

  it should "MINIT clear idle scale metadata on one edge and acknowledge only next cycle" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      d.io.resetReady.expect(true.B)
      load(d, a = true, reg = 0, k = 64)
      load(d, a = false, reg = 1, k = 64)
      d.clock.step(3)
      assert(d.io.residentMask.peek().litValue != 0)
      d.io.generationPresent.expect(true.B)
      // A pending but cancelled CPU MINIT never pulses clear.
      d.io.downstreamIdle.poke(false.B)
      d.io.resetReady.expect(false.B)
      val resident = d.io.residentMask.peek().litValue
      d.clock.step(4)
      d.io.residentMask.expect(resident.U)
      d.io.generationPresent.expect(true.B)
      d.io.minitClearAck.expect(false.B)
      d.io.downstreamIdle.poke(true.B)
      d.io.resetReady.expect(true.B) // Resident valid banks are not work.
      d.io.minitClear.poke(true.B)
      d.io.minitClearAck.expect(false.B)
      d.io.residentMask.expect(resident.U)
      d.clock.step()
      d.io.minitClear.poke(false.B)
      d.io.minitClearAck.expect(true.B)
      d.io.residentMask.expect(0.U)
      d.io.generationPresent.expect(false.B)
      d.clock.step()
      d.io.minitClearAck.expect(false.B)
      // Both formerly occupied banks are reusable with a new owner.
      load(d, a = true, reg = 2, k = 64)
      load(d, a = false, reg = 3, k = 64)
      push(d, 0, mma(a = 2, b = 3, k = 64))
      await(d, 4)
      d.clock.step()
      done(d, 0x7c)
      d.clock.step(3)
      d.io.resetReady.expect(true.B)
      // An old owner must not silently read the pre-MINIT SRAM contents.
      push(d, 0, mma(a = 0, b = 1, k = 64))
      for (_ <- 0 until 8) {
        d.io.issued.expect(0.U)
        d.io.resetReady.expect(false.B)
        d.clock.step()
      }
    }
  }

  it should "MINIT wait for accepted scale loads and every scaled compute end without losing work" in {
    test(new ScaleSchedulerTop(32)) { d =>
      init(d)
      push(d, 1, scale(a = true, reg = 0, k = 64))
      d.io.resetReady.expect(false.B) // Includes decoded FIFO before window.
      await(d, 1)
      d.clock.step(4)
      d.io.resetReady.expect(false.B)
      done(d, 1)
      d.clock.step(3)
      d.io.resetReady.expect(true.B)
      load(d, a = false, reg = 1, k = 64)
      push(d, 0, mma(k = 64))
      await(d, 4)
      d.clock.step()
      // Preserve canonical ADC/BDC-before-CDC completion order, then prove
      // CDC cannot retire a scaled compute before both scale readers finish.
      for (mask <- Seq(0x04, 0x08, 0x40, 0x10)) {
        done(d, mask)
        d.io.resetReady.expect(false.B)
      }
      done(d, 0x20)
      d.clock.step(3)
      d.io.resetReady.expect(true.B)
      d.io.minitClear.poke(true.B)
      d.clock.step()
      d.io.minitClear.poke(false.B)
      d.io.minitClearAck.expect(true.B)
      d.io.residentMask.expect(0.U)
    }
  }

  it should "issue MXFP4 and MXFP8 only after both real load completions and release at read-end" in {
    test(new ScaleSchedulerTop) { d =>
      init(d)
      for (mode <- Seq(10, 7, 8)) {
        val k = if (mode == 10) 128 else 64
        val aBank = load(d, a = true, reg = 0, mode = mode, rows = 4, k = k)
        push(d, 1, scale(a = false, reg = 1, mode = mode, rows = 8, k = k))
        await(d, 2)
        val bBank = d.io.bankB.peek().litValue
        d.clock.step()
        push(d, 0, mma(mode = mode, n = 8, k = k))
        d.clock.step(5)
        d.io.issued.expect(0.U) // B is issued but has not completed.
        done(d, 2)
        await(d, 4)
        d.io.scaleStarts.expect(3.U)
        d.io.computeType.expect(mode.U)
        d.io.computeBankA.expect(aBank.U)
        d.io.computeBankB.expect(bBank.U)
        d.clock.step()
        // CDC and data ends are not permission to retire the scale owner.
        done(d, (1 << 2) | (1 << 3) | (1 << 6))
        d.io.active.expect(true.B)
        done(d, 1 << 4)
        d.io.active.expect(true.B)
        done(d, 1 << 5)
        d.io.active.expect(false.B)
        d.clock.step(3)
      }
    }
  }

  it should "retain all four architectural scale banks per side" in {
    for (aSide <- Seq(true, false)) {
      test(new ScaleSchedulerTop) { d =>
        init(d)
        val banks = (0 until scaleRegCount).map { reg =>
          load(d, aSide, reg = reg)
        }
        assert(banks.distinct.size == scaleRegCount)
        val residentBit = if (aSide) 0 else scaleRegCount
        val residentMask = ((1 << scaleRegCount) - 1) << residentBit
        d.io.residentMask.expect(residentMask.U)

        // With every architectural register resident but no active reader,
        // reloading one register reuses its own bank in place.
        val reloaded = load(d, aSide, reg = 0)
        assert(reloaded == banks(0))
        d.io.residentMask.expect(residentMask.U)
        d.io.scaleInUse.expect(0.U)
      }
    }
  }

  it should "hold same-register WAR until its older consumer finishes scale reads" in {
    test(new ScaleSchedulerTop) { d =>
      init(d)
      val first = load(d, a = true, reg = 0)
      val second = load(d, a = true, reg = 2)
      assert(first != second)
      val b = load(d, a = false, reg = 1)
      push(d, 0, mma())
      await(d, 4)
      d.io.computeBankA.expect(first.U); d.io.computeBankB.expect(b.U)
      d.clock.step()
      push(d, 1, scale(a = true, reg = 0))
      d.clock.step(6)
      d.io.issued.expect(0.U)
      // B completes first: it must not release A's bank.
      done(d, 1 << 5)
      d.clock.step(3)
      d.io.issued.expect(0.U)
      d.io.scaleInUse.expect((1 << first).U)
      done(d, 1 << 4)
      await(d, 1)
      d.io.bankA.expect(first.U)
      d.clock.step()
      done(d, 1)
      done(d, (1 << 2) | (1 << 3) | (1 << 6))
      d.clock.step(3)
    }
  }

  it should "block uninitialized and mismatched scales but retain valid generations for reuse" in {
    test(new ScaleSchedulerTop) { d =>
      init(d)
      push(d, 0, mma())
      d.clock.step(8)
      d.io.issued.expect(0.U)
      // These younger generations must never satisfy the older snapshot.
      push(d, 1, scale(a = true, reg = 0))
      push(d, 1, scale(a = false, reg = 1))
      d.clock.step(8)
      d.io.issued.expect(0.U)
    }
    test(new ScaleSchedulerTop) { d =>
      init(d)
      load(d, a = true, reg = 0, rows = 8)
      load(d, a = false, reg = 1)
      push(d, 0, mma())
      d.clock.step(8)
      d.io.issued.expect(0.U)
    }
    test(new ScaleSchedulerTop) { d =>
      init(d)
      load(d, a = true, reg = 0)
      load(d, a = false, reg = 1)
      push(d, 0, mma())
      await(d, 4); d.clock.step()
      done(d, 0x7c); d.clock.step(3)
      push(d, 0, mma())
      await(d, 4); d.clock.step()
      done(d, 0x7c); d.clock.step(3)
    }
  }

  it should "keep both resident register banks stable through repeated same-register reload generations" in {
    test(new ScaleSchedulerTop) { d =>
      init(d)
      // Force the repeatedly reloaded registers into bank 1. Bank 0 belongs
      // to a different architectural register and must retain its generation.
      val otherA = load(d, a = true, reg = 2)
      val a = load(d, a = true, reg = 0)
      val otherB = load(d, a = false, reg = 3)
      val b = load(d, a = false, reg = 1)
      assert(a != otherA && b != otherB)
      for (mode <- Seq(10, 7, 8, 10, 8, 7)) {
        val k = if (mode == 10) 128 else 64
        assert(load(d, a = true, reg = 0, mode = mode, k = k) == a)
        assert(load(d, a = false, reg = 1, mode = mode, k = k) == b)
        // Reuse each newly loaded generation twice; a read must not consume
        // validity or accidentally select the other resident register's bank.
        for (_ <- 0 until 2) {
          push(d, 0, mma(mode = mode, k = k))
          await(d, 4)
          d.io.computeBankA.expect(a.U)
          d.io.computeBankB.expect(b.U)
          d.io.computeType.expect(mode.U)
          d.clock.step()
          done(d, 0x7c)
          d.clock.step(3)
        }
      }
      push(d, 0, mma(a = 2, b = 3))
      await(d, 4)
      d.io.computeBankA.expect(otherA.U)
      d.io.computeBankB.expect(otherB.U)
      d.clock.step()
      done(d, 0x7c)
      d.clock.step(3)
    }
  }

  it should "wait for A scale WAR and bind a queued consumer to its completed reload generation" in {
    test(new ScaleSchedulerTop) { d =>
      init(d)
      val a = load(d, a = true, reg = 0)
      val b = load(d, a = false, reg = 1)
      push(d, 0, mma())
      await(d, 4)
      d.clock.step()
      push(d, 1, scale(a = true, reg = 0))
      push(d, 0, mma()) // Captures the newer A generation, not the resident old one.
      d.clock.step(5)
      d.io.issued.expect(0.U)

      // Data/CDC completion is insufficient permission to overwrite A scales.
      done(d, (1 << 2) | (1 << 3) | (1 << 6))
      d.clock.step(2)
      d.io.issued.expect(0.U)
      done(d, 1 << 5) // B read-end must not release A.
      d.clock.step(2)
      d.io.issued.expect(0.U)
      done(d, 1 << 4)
      await(d, 1)
      d.io.bankA.expect(a.U)
      d.clock.step()
      // The second compute cannot read the bank while its reload is in flight.
      d.clock.step(5)
      d.io.issued.expect(0.U)
      done(d, 1)
      await(d, 4)
      d.io.computeBankA.expect(a.U)
      d.io.computeBankB.expect(b.U)
      d.clock.step()
      done(d, 0x7c)
      d.clock.step(3)
    }
  }

  for (reduceBytes <- Seq(32, 64)) {
    it should s"accept exact CPU rows3 and65 blocks2 e8 payloads at ReduceWidthByte=$reduceBytes" in {
      test(new ScaleSchedulerTop(reduceBytes)) { d =>
        init(d)
        for (mode <- Seq(10, 7, 8)) {
          val expectedK = (2 + reduceBytes / (if (mode == 10) 16 else 32) - 1) /
            (reduceBytes / (if (mode == 10) 16 else 32))
          load(d, a = true, reg = 0, mode = mode, rows = 3, k = 33)
          load(d, a = false, reg = 1, mode = mode, rows = 65, k = 33)
          for (_ <- 0 until 2) {
            push(d, 0, mma(mode = mode, m = 3, n = 65, k = 33))
            await(d, 4)
            d.io.scaleStarts.expect(3.U)
            d.io.ascK.expect(expectedK.U); d.io.bscK.expect(expectedK.U)
            d.io.adcK.expect(expectedK.U); d.io.bdcK.expect(expectedK.U)
            d.io.ascBlocks.expect(2.U); d.io.bscBlocks.expect(2.U)
            d.clock.step(); done(d, 0x7c); d.clock.step(3)
          }
        }
      }
    }
  }
}
