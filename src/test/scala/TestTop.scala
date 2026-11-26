package cute

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import utility._
import xscache.coupledL2.{MatrixDataBundle, AmeIndexKey, AmeIndexField, DSBlock, MatrixKey}
import chiseltest._
import chiseltest.experimental.expose
import org.scalatest.flatspec.AnyFlatSpec
import firrtl2.options.TargetDirAnnotation

object baseConfig {
  def apply(maxHartIdBits: Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.NORMAL,
        perfDBHartID = 0
      )
    })
  }
}

class TestTop()(implicit p: Parameters) extends LazyModule {
  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.5
  val cuteParams = p(CuteParamsKey)

  def createClientNode(name: String, sources: Int) = {
    val slaveNode = TLManagerNode(Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(0, 0xffffffffffffL)),
            executable = true,
            supportsGet = TransferSizes(1, 64),
            supportsPutFull = TransferSizes(1, 64),
            supportsPutPartial = TransferSizes(1, 64),
            fifoId = Some(0)
          )
        ),
        responseFields = Seq(AmeIndexField()),
        requestKeys = Seq(MatrixKey, AmeIndexKey),
        beatBytes = 32,
        minLatency = 2
      )
    ))
    slaveNode
  }
  val hbl2_node = createClientNode("hbl2", 32)

  val cute_tl = LazyModule(new Cute2TL())
  val hbl2_xbar = TLXbar()

  cute_tl.node.foreach { clientNode =>
    hbl2_xbar :=* TLFragmenter(32, 64) := TLWidthWidget(64) := clientNode
  }
  hbl2_node :*= hbl2_xbar

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl2top = Flipped(new YGJKControl)
      val matrix_data_in = Flipped(DecoupledIO(new MatrixDataBundle()))
      val verification_task_busy = Output(Bool())
    })
    
    val cute = Module(new CUTEV2Top())
    // Signals to top level
    io.ctrl2top <> cute.io.ctrl2top
    io.ctrl2top.resetReady := cute.io.ctrl2top.resetReady && cute_tl.module.io.idle
    when(io.ctrl2top.minitClear) {
      assert(io.ctrl2top.resetReady, "MINIT clear with test transport in flight")
    }
    io.verification_task_busy := cute.io.perf.backendEvents(0).orR

    // memory access between CUTE and HBL2
    cute_tl.module.io.mmu <> cute.io.mmu2llc
    val tl_data_in = cute_tl.module.io.matrix_data_in
    for (channel <- 0 until cute.ABMatrixRegNBanks) {
      tl_data_in(channel).valid := io.matrix_data_in.valid && io.matrix_data_in.bits.channel === channel.U
      tl_data_in(channel).bits := 0.U.asTypeOf(tl_data_in(channel).bits)
      tl_data_in(channel).bits.source := io.matrix_data_in.bits.sourceId
      tl_data_in(channel).bits.data := io.matrix_data_in.bits.data.data
    }
    io.matrix_data_in.ready := MuxLookup(io.matrix_data_in.bits.channel, false.B)(
      (0 until cute.ABMatrixRegNBanks).map(channel => channel.U -> tl_data_in(channel).ready)
    )

    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    hbl2_node.makeIOs()(ValName("slave_port_0"))

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)
  }
}

private[cute] object TestTopFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

object TestTop extends App {
  // Keep this profile aligned with the CUTE instance used by XSAI.
  val config = baseConfig(6).alterPartial({
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      L2NBanks = 8,
      Debug = CuteDebugParams.NoDebug,
      LoaderBridgeChannelConfig = "A1BLCL1CS2",
      EnableDifftest = false,
      v3config = Cutev3extParams(
        TaskCtrl_AutoClear = true,
      ),
      MatrixExtension = MatrixIsaParams(
        enableInt8Int32 = true,
        enableFp8Fp32 = true,
        enableFp8Fp16 = true,
        enableFp8Bf16 = true,
        enableFp16Fp16 = true,
        enableFp16Fp32 = true,
        enableBf16Fp32 = true,
      ),
    )
  })

  ChiselDB.init(false)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop()(p)) )(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}

// Task26 CORE BOUNDARY ONLY: resolved, postcommitted AMU traces and external
// memory. No CPU fence/acquire/store-buffer/GPR-lifetime claim is made here.
// All scheduling, SRAMs, completion, flags and release returns belong to CUTE.
private[cute] object PanelCycleEvents {
  val names = Seq("loadA", "loadB", "loadAS", "loadBS", "loadC", "store",
    "compute", "doneA", "doneB", "doneAS", "doneBS", "doneC", "doneStore",
    "doneCompute", "windowFull", "loadComputeOverlap", "tuple", "D",
    "Crequest", "Cresponse", "Cfire", "Cwrite", "Chold", "drain",
    "missingA", "missingB", "missingC", "missingAS", "missingBS",
    "idle", "computeBusy", "release", "downstreamIdle")
}

private[cute] class PanelCycleProbe(val reduced: Boolean = false) extends Module {
  private val geometry = if (reduced)
    CuteParams.CUTE_2Tops.copy(Matrix_MN = 4, Tensor_MN = 16, Tensor_K = 64,
      ReduceWidthByte = 32, outsideDataWidth = 512)
    else CuteParams.CUTE_8Tops_128SCP
  private val arraySize = geometry.Matrix_MN
  implicit val p: Parameters = baseConfig(6).alterPartial {
    case CuteParamsKey => geometry.copy(
      Debug = CuteDebugParams.NoDebug, EnableDifftest = false,
      LoaderBridgeChannelConfig = "A1BLCL1CS2",
      v3config = Cutev3extParams(TaskCtrl_AutoClear = true),
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp8Fp32 = true))
  }
  val io = IO(new Bundle {
    val core = new CUTETopIO
    val events = Output(Vec(PanelCycleEvents.names.size, Bool()))
    val pending = Output(UInt(3.W))
    val result = Output(UInt((32*arraySize*arraySize).W))
    val flags = Output(UInt((5*arraySize*arraySize).W))
    val evenReads = Output(UInt(log2Ceil(arraySize+1).W))
    val oddReads = Output(UInt(log2Ceil(arraySize+1).W))
    val parityConflicts = Output(UInt(log2Ceil(arraySize+1).W))
  })
  val core = Module(new CUTEV2Top)
  io.core <> core.io
  val tc = core.TaskCtrl.asInstanceOf[TaskController]
  // Unique names also avoid TestSRAMTemplate's repeated-expose identities.
  private var observationId = 0
  private def obs[T <: Data](signal: T): T = {
    observationId += 1
    expose(signal).suggestName(s"panelObservation_$observationId")
  }
  val c = tc.io
  val computeBusy = obs(tc.fuCompute.busy)
  val loadBusy = Seq(tc.fuAML.busy, tc.fuBML.busy, tc.fuASL.busy,
    tc.fuBSL.busy).map(obs(_)).reduce(_ || _)
  val m = core.MTE.io
  val d = core.CDC.io
  val pending = obs(core.MTE.pendingTokens)
  val request = d.FromMatrixRegIO.ReadBankAddr.map(x => obs(x.valid)).reduce(_ && _)
  val response = d.FromMatrixRegIO.ReadResponseData.map(x => obs(x.valid)).reduce(_ && _)
  val write = d.FromMatrixRegIO.WriteRequestData.map(x => obs(x.valid)).reduce(_ && _)
  val as = c.ASL_MicroTask_Config.get
  val bs = c.BSL_MicroTask_Config.get
  val tuple = obs(core.MTE.tupleFire)
  def fire(port: ReadyValidIO[_]): Bool = obs(port.valid) && obs(port.ready)
  val events = Seq(
    obs(c.AML_MicroTask_Config.MicroTaskValid) && obs(c.AML_MicroTask_Config.MicroTaskReady),
    obs(c.BML_MicroTask_Config.MicroTaskValid) && obs(c.BML_MicroTask_Config.MicroTaskReady),
    obs(as.MicroTaskValid) && obs(as.MicroTaskReady),
    obs(bs.MicroTaskValid) && obs(bs.MicroTaskReady),
    obs(c.CML_MicroTask_Config.LoadMicroTaskValid) && obs(c.CML_MicroTask_Config.LoadMicroTaskReady),
    obs(c.CML_MicroTask_Config.StoreMicroTaskValid) && obs(c.CML_MicroTask_Config.StoreMicroTaskReady),
    obs(c.MTE_MicroTask_Config.MicroTaskValid),
    obs(tc.amlDone), obs(tc.bmlDone), obs(tc.aslDone), obs(tc.bslDone),
    obs(tc.cmlLoadDone), obs(tc.cmlStoreDone), obs(c.perfProbe.compDone),
    obs(tc.windowFull), loadBusy && computeBusy, tuple, fire(m.MatrixD),
    request, response, fire(m.MatrixC), write,
    obs(core.CDC.ReadMatrixRegDataHoldValid),
    computeBusy && pending =/= 0.U && !tuple,
    computeBusy && !obs(m.VectorA.valid), computeBusy && !obs(m.VectorB.valid),
    computeBusy && !obs(m.MatrixC.valid), computeBusy && !obs(m.ScaleA.get.valid),
    computeBusy && !obs(m.ScaleB.get.valid),
    obs(core.MTE.io.idle), computeBusy, obs(c.perfProbe.releaseDone), obs(c.downstreamIdle))
  io.events := VecInit(events)
  io.pending := pending
  io.result := obs(m.MatrixD.bits)
  io.flags := VecInit(m.DFlags.map(obs(_))).asUInt
  val bankReads = core.CMatrixRegs.head.sram_banks.map { case (even, odd) =>
    val er = obs(even.io.r.req.valid)
    val ew = obs(even.io.w.req.valid)
    val or = obs(odd.io.r.req.valid)
    val ow = obs(odd.io.w.req.valid)
    (er && obs(even.io.r.req.ready), or && obs(odd.io.r.req.ready),
      (er && ew) || (or && ow))
  }
  io.evenReads := PopCount(bankReads.map(_._1))
  io.oddReads := PopCount(bankReads.map(_._2))
  io.parityConflicts := PopCount(bankReads.map(_._3))
}

class PanelCycleSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val reduced = sys.env.get("CUTE_TEST_PANEL_REDUCED").contains("1")
  private val rows = if (reduced) 4 else 8
  private val columns = if (reduced) 16 else 128
  private val expectedTuples = if (reduced) 8 else 32
  private val scope = if (reduced) "SCHEDULER_CORE_BOUNDARY_REDUCED_16PE"
    else "CANONICAL64PE_CORE_BOUNDARY"
  behavior of s"$scope panel traces"

  private case class Trace(op: Int, data: BigInt)
  private def lsu(reg: Int, addr: Int, stride: Int, rows: Int, columns: Int,
      side: Int, width: Int = 0, store: Boolean = false): BigInt =
    (BigInt(reg) << 122) | (BigInt(if (store) 1 else 0) << 121) |
      (BigInt(1) << side) | (BigInt(addr) << 69) | (BigInt(stride) << 21) |
      (BigInt(rows) << 12) | (BigInt(columns) << 3) | width
  private def panel(index: Int): Seq[Trace] = {
    val a = index*2
    val b = a+1
    val base = 0x10000 + index*0x10000
    Seq(
      Trace(1, lsu(a, base, 32, rows, 32, 118)),
      Trace(1, lsu(b, base+0x2000, 32, 32, columns, 117)),
      Trace(1, (BigInt(7) << 127) | (BigInt(1) << 126) |
        lsu(a, base+0x4000, 1, rows, 1, 118)),
      Trace(1, (BigInt(7) << 127) | (BigInt(1) << 126) |
        lsu(b, base+0x5000, 1, columns, 1, 117)),
      Trace(0, (BigInt(7) << 53) | (BigInt(a) << 41) | (BigInt(b) << 37) |
        (BigInt(rows) << 28) | (BigInt(columns) << 19) | (BigInt(32) << 10) |
        (BigInt(4) << 7) | (BigInt(4) << 4) | 1))
  }

  private case class Measurement(cycles: Int, counts: Seq[Long],
      outputs: Seq[(BigInt, BigInt)], carriers: Seq[(BigInt, Boolean, BigInt, BigInt)],
      stored: Seq[Int], returns: Seq[(BigInt, Boolean, BigInt)],
      overlap: Int, issueOverlap: Int, firstTuple: Int, lastTuple: Int)

  it should "measure one versus two panels with identical MX8 K32 traces" in {
    utility.ChiselDB.init(false)
    utility.Constantin.init(false)
    val verilator = sys.env.get("CUTE_TEST_VERILATOR").contains("1")
    println(s"TASK26 START $scope M${rows}N${columns}K32 MX8E4 " +
      (if (reduced) "Matrix4 Tensor16 Reduce32B 16FPE" else "Matrix8 Tensor128 Reduce32B 64FPE"))
    test(new PanelCycleProbe(reduced)).withAnnotations(
      Seq(TargetDirAnnotation(if (reduced) s"test_run_dir/task26-panel-$scope-r9"
        else s"test_run_dir/task26-panel-${if (verilator) "verilator" else "treadle"}-r5")) ++
        (if (verilator) Seq(VerilatorBackendAnnotation,
          chiseltest.internal.CachingAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))
        else Seq.empty)) { dut =>
      dut.clock.setTimeout(0)
      val ctrl = dut.io.core.ctrl2top
      val mmu = dut.io.core.mmu2llc
      val channels = mmu.Request.size
      val beatBytes = mmu.Response.head.bits.ReseponseData.getWidth/8
      ctrl.reset.poke(false.B)
      ctrl.minitClear.poke(false.B)
      ctrl.amuCtrl.valid.poke(false.B)
      ctrl.amuCtrl.bits.op.poke(0.U)
      ctrl.amuCtrl.bits.data.poke(0.U)
      ctrl.mrelease.ready.poke(true.B)
      mmu.ConherentRequsetSourceID.valid.poke(false.B)
      mmu.ConherentRequsetSourceID.bits.poke(0.U)
      mmu.nonConherentRequsetSourceID.valid.poke(false.B)
      mmu.nonConherentRequsetSourceID.bits.poke(0.U)
      for (i <- 0 until channels) {
        mmu.Request(i).ready.poke(true.B)
        mmu.Response(i).valid.poke(false.B)
        mmu.Response(i).bits.ReseponseData.poke(0.U)
        mmu.Response(i).bits.ReseponseSourceID.poke(0.U)
        mmu.Response(i).bits.ReseponseConherent.poke(true.B)
      }
      dut.clock.step(2)

      def run(twoPanels: Boolean): Measurement = {
        val label = s"$scope ${if (twoPanels) "candidate" else "control"}"
        val memory = scala.collection.mutable.Map.empty[BigInt, Int]
        def fill(addr: Int, size: Int)(byte: Int => Int): Unit =
          (0 until size).foreach(i => memory(BigInt(addr+i)) = byte(i))
        for (p <- 0 until 2) {
          val base = 0x10000+p*0x10000
          // Entire readable last beats are provisioned: not a guard-page test.
          fill(base, rows*32+64)(_ => if (p == 0) 0x38 else 0x40)
          fill(base+0x2000, columns*32+64)(_ => 0x38)
          fill(base+0x4000, 64)(i =>
            if (p == 0) 127+(i%2) else 126+(i%2))
          fill(base+0x5000, ((columns+63)/64)*64)(i =>
            if (p == 0) 127-(i%2) else 128-(i%2))
        }
        fill(0x40000, rows*columns*4+64)(_ => 0) // immutable initial C, provisioned tail
        fill(0x50000, rows*columns*4+64)(_ => 0) // separate D store
        case class Response(due: Int, source: BigInt, data: BigInt, coherent: Boolean)
        val responses = Array.fill(channels)(scala.collection.mutable.Queue.empty[Response])
        val counts = Array.fill[Long](PanelCycleEvents.names.size)(0L)
        val outputs = scala.collection.mutable.ArrayBuffer.empty[(BigInt, BigInt)]
        val carriers = scala.collection.mutable.ArrayBuffer.empty[(BigInt, Boolean, BigInt, BigInt)]
        val returns = scala.collection.mutable.ArrayBuffer.empty[(BigInt, Boolean, BigInt)]
        var cycle = 0
        var first = -1
        var last = -1
        var overlap = 0
        var issueOverlap = 0
        var maxPending = 0
        var evenReads = 0L
        var oddReads = 0L
        var requestCount = 0
        var responseCount = 0
        def bool(x: Bool): Boolean = x.peek().litToBoolean
        def tick(): Unit = {
          require(cycle < 1024, s"$label bounded core trace incomplete at cycle=$cycle")
          for (i <- 0 until channels) {
            val response = mmu.Response(i)
            val next = responses(i).headOption.filter(_.due <= cycle)
            response.valid.poke(next.nonEmpty.B)
            next.foreach { r =>
              response.bits.ReseponseSourceID.poke(r.source.U)
              response.bits.ReseponseData.poke(r.data.U)
              response.bits.ReseponseConherent.poke(r.coherent.B)
            }
          }
          val sampled = dut.io.events.map(bool).toSeq
          sampled.zipWithIndex.foreach { case (v, i) => if (v) counts(i) += 1 }
          def event(name: String): Boolean = sampled(PanelCycleEvents.names.indexOf(name))
          if (event("tuple")) {
            if (first < 0) first = cycle
            last = cycle
          }
          if (event("D")) outputs += ((dut.io.result.peek().litValue, dut.io.flags.peek().litValue))
          if (counts(PanelCycleEvents.names.indexOf("compute")) == 1 &&
              event("loadComputeOverlap")) overlap += 1
          if (counts(PanelCycleEvents.names.indexOf("compute")) == 1 &&
              event("computeBusy") && sampled.take(4).contains(true)) {
            issueOverlap += 1
            println(s"TASK26 $label cycle=$cycle LOAD1_ISSUE_WITH_COMPUTE0_BUSY")
          }
          if (sampled.take(14).contains(true))
            println(s"TASK26 $label cycle=$cycle events=" +
              PanelCycleEvents.names.zip(sampled).collect { case (n, true) => n }.mkString(","))
          maxPending = math.max(maxPending, dut.io.pending.peek().litValue.toInt)
          evenReads += dut.io.evenReads.peek().litValue.toLong
          oddReads += dut.io.oddReads.peek().litValue.toLong
          dut.io.parityConflicts.expect(0.U)
          if (bool(ctrl.mrelease.valid)) {
            val sync = ctrl.mrelease.bits.msyncRd.zipWithIndex.foldLeft(BigInt(0)) {
              case (bits, (bit, index)) => if (bool(bit)) bits.setBit(index) else bits
            }
            returns += ((sync, bool(ctrl.mrelease.bits.flagsValid), ctrl.mrelease.bits.flags.peek().litValue))
            println(s"TASK26 $label cycle=$cycle return=$sync flags=${returns.last._3}")
          }
          for (i <- 0 until channels) {
            if (bool(mmu.Response(i).valid) && bool(mmu.Response(i).ready)) {
              responses(i).dequeue()
              responseCount += 1
            }
            val request = mmu.Request(i)
            if (bool(request.valid) && bool(request.ready)) {
              val r = request.bits
              val addr = r.RequestAddr.peek().litValue
              val write = bool(r.RequestType_isWrite)
              val data = r.RequestData.peek().litValue
              val mask = r.RequestMask.peek().litValue
              val source = r.RequestSourceID.peek().litValue
              val coherent = bool(r.RequestConherent)
              var readData = BigInt(0)
              for (byte <- 0 until beatBytes) {
                if (write && mask.testBit(byte))
                  memory(addr+byte) = ((data >> (8*byte)) & 255).toInt
                if (!write) {
                  require(memory.contains(addr+byte), s"$label unmapped read ${addr+byte}")
                  readData |= BigInt(memory(addr+byte)) << (8*byte)
                }
              }
              // Source IDs are opaque and returned unchanged. No FU completion
              // is synthesized; writes receive real memory-side acknowledgments.
              responses(i).enqueue(Response(cycle+2, source, readData, coherent))
              carriers += ((addr, write, mask, if (write) data else readData))
              requestCount += 1
            }
          }
          dut.clock.step()
          cycle += 1
        }
        def until(condition: => Boolean): Unit = while (!condition) tick()
        def push(t: Trace): Unit = {
          ctrl.amuCtrl.bits.op.poke(t.op.U)
          ctrl.amuCtrl.bits.data.poke(t.data.U)
          ctrl.amuCtrl.valid.poke(true.B)
          until(bool(ctrl.amuCtrl.ready))
          tick()
          ctrl.amuCtrl.valid.poke(false.B)
        }
        until(bool(ctrl.resetReady))
        ctrl.minitClear.poke(true.B); tick()
        ctrl.minitClear.poke(false.B)
        assert(bool(ctrl.minitClearAck))
        push(Trace(1, lsu(0, 0x40000, columns*4, rows, columns, 119, width = 2)))
        panel(0).foreach(push)
        push(Trace(2, 0))
        if (!twoPanels) until(returns.exists(_._1.testBit(0)))
        panel(1).foreach(push)
        push(Trace(1, lsu(0, 0x50000, columns*4, rows, columns, 119, width = 2, store = true)))
        push(Trace(2, 1))
        until(returns.exists(_._1.testBit(1)) && bool(ctrl.resetReady) &&
          responses.forall(_.isEmpty))
        val measuredCycles = cycle
        def count(name: String): Long = counts(PanelCycleEvents.names.indexOf(name))
        for (name <- Seq("loadA", "loadB", "loadAS", "loadBS", "compute",
            "doneA", "doneB", "doneAS", "doneBS", "doneCompute", "release"))
          assert(count(name) == 2, s"$label $name=${count(name)} expected2")
        for (name <- Seq("loadC", "store", "doneC", "doneStore"))
          assert(count(name) == 1, s"$label $name=${count(name)} expected1")
        for (name <- Seq("tuple", "D", "Crequest", "Cresponse", "Cfire", "Cwrite"))
          assert(count(name) == expectedTuples,
            s"$label $name=${count(name)} expected$expectedTuples")
        assert(requestCount > 0 && requestCount == responseCount)
        assert(outputs.size == expectedTuples && outputs.forall(_._2 == 0))
        assert(returns.count(_._2) == 2 && returns.filter(_._2).forall(_._3 == 0))
        assert(returns.toSeq == Seq((BigInt(0), true, BigInt(0)),
          (BigInt(1), false, BigInt(0)), (BigInt(0), true, BigInt(0)),
          (BigInt(2), false, BigInt(0))), s"$label duplicated/reordered release/flags")
        assert(dut.io.pending.peek().litValue == 0)
        val stored = (0 until rows*columns*4).map(i => memory(BigInt(0x50000+i)))
        // Independent exact FP32 golden: each K32 dot contains ones, panel1
        // A contains twos; E8M0 scales vary by output row/column and generation.
        for (r <- 0 until rows; col <- 0 until columns) {
          val dot0 = 32.0 * math.pow(2, (r%2)-(col%2))
          val dot1 = 64.0 * math.pow(2, (r%2)-(col%2))
          val bits = java.lang.Float.floatToRawIntBits((dot0+dot1).toFloat).toLong & 0xffffffffL
          val offset = (r*columns+col)*4
          val actual = (0 until 4).map(b => stored(offset+b).toLong << (8*b)).reduce(_ | _)
          assert(actual == bits, s"$label D[$r,$col]=$actual expected=$bits")
        }
        ctrl.minitClear.poke(true.B); tick()
        ctrl.minitClear.poke(false.B)
        assert(bool(ctrl.minitClearAck), s"$label following MINIT not acknowledged")
        tick()
        println(s"TASK26 $label COMPLETE cycles=$measuredCycles firstTuple=$first lastTuple=$last " +
          s"load1Compute0Overlap=$overlap load1IssueCompute0Busy=$issueOverlap " +
          s"pendingMax=$maxPending carriers=$requestCount/$responseCount " +
          s"physicalCReads=$evenReads+$oddReads minitAck=1 counts=" +
          PanelCycleEvents.names.zip(counts).map { case (n, v) => s"$n:$v" }.mkString(","))
        Measurement(measuredCycles, counts.toSeq, outputs.toSeq,
          carriers.toSeq.sortBy(x => (x._1, x._2, x._3, x._4)), stored, returns.toSeq,
          overlap, issueOverlap, first, last)
      }
      val control = run(false)
      val candidate = run(true)
      assert(control.outputs == candidate.outputs, "tuple bits/flags changed")
      assert(control.carriers == candidate.carriers, "memory carrier multiset changed")
      assert(control.stored == candidate.stored, "final output bits changed")
      assert(control.returns == candidate.returns, "release/flags returns changed")
      assert(control.overlap == 0, "one-panel producer overlapped next panel")
      assert(candidate.overlap > 0, "two-panel trace did not overlap load1/compute0")
      assert(control.issueOverlap == 0, "control issued next panel during compute")
      assert(candidate.issueOverlap > 0, "no real next-panel issue during compute0 busy")
      println(s"TASK26 $scope PASS control=${control.cycles} candidate=${candidate.cycles} " +
        s"delta=${control.cycles-candidate.cycles} cycles; CPU ordering/lifetime OUT_OF_SCOPE")
    }
  }
}

// Task27 TEST-ONLY standalone throughput probe. This wraps the canonical
// CUTEV2Top and exposes scalar observations for a hand-written Verilator driver.
// It does not replace or alter the production TaskController/MTE/FPE/CDC path.
private[cute] object PanelThroughputEvents {
  // Test-only, default OFF. Never changes a production CUTE configuration.
  val diagnostic = sys.env.get("CUTE_PANEL_DIAGNOSTIC").fold(false) { value =>
    require(value == "0" || value == "1", "CUTE_PANEL_DIAGNOSTIC must be 0 or 1")
    value == "1"
  }
  val names = Seq(
    "computeIssue", "computeDone", "tupleFire", "Dfire", "Crequest",
    "Cresponse", "Cfire", "Cwrite", "idle", "windowFull",
    "loadComputeOverlap", "computeWaitTuple", "missingA", "missingB",
    "missingC", "missingScaleA", "missingScaleB", "Chold",
    "parityConflict", "loadIssueWhileCompute"
  ) ++ (if (diagnostic) Seq(
    "CportIssueBlocked", "CreuseIssueBlocked", "CreadGrant", "CreadWaitComputeGo"
  ) else Seq.empty)
}

private[cute] class PanelThroughputProbe extends Module {
  implicit val p: Parameters = baseConfig(6).alterPartial {
    case CuteParamsKey => CuteParams.CUTE_2Tops_128SCP.copy(
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false,
      LoaderBridgeChannelConfig =
        if (PanelThroughputEvents.diagnostic) "ALBLCLLCSL" else "A1BLCL1CS2",
      L2NBanks = if (PanelThroughputEvents.diagnostic) 8 else
        CuteParams.CUTE_2Tops_128SCP.L2NBanks,
      ExperimentalMxfpConvertLoad = PanelThroughputEvents.diagnostic,
      ExperimentalMxfpDynamicQuant = PanelThroughputEvents.diagnostic,
      MxfpConvertLanes = 8,
      MxfpConvertOwners = 4,
      v3config = Cutev3extParams(TaskCtrl_AutoClear = true),
      MatrixExtension = MatrixIsaParams(
        enableInt8Int32 = true,
        enableFp8Fp32 = PanelThroughputEvents.diagnostic,
        enableBf16Fp32 = PanelThroughputEvents.diagnostic,
        enableMxfp4Fp32 = true,
        enableMxfp8Fp32 = true))
  }
  private val lanes = p(CuteParamsKey).Matrix_MN
  require(p(CuteParamsKey).Matrix_MN == 4 && p(CuteParamsKey).Tensor_MN == 128)
  require(p(CuteParamsKey).Tensor_K == 64 && p(CuteParamsKey).ReduceWidthByte == 32)
  if (PanelThroughputEvents.diagnostic) {
    println("[R71] PanelThroughputProbe R39 geometry=4x128x64 reduceBytes=32 " +
      "loaders=ALBLCLLCSL L2NBanks=8 dynamicConvert=8lane_4owner " +
      "observationOnly=true sourceBindingRequired=true")
  }

  val io = IO(new Bundle {
    val core = new CUTETopIO
    val events = Output(Vec(PanelThroughputEvents.names.size, Bool()))
    val pending = Output(UInt(3.W))
    val result = Output(UInt((32 * lanes * lanes).W))
    val flags = Output(UInt((5 * lanes * lanes).W))
    val evenReads = Output(UInt(log2Ceil(lanes + 1).W))
    val oddReads = Output(UInt(log2Ceil(lanes + 1).W))
    val evenWrites = Output(UInt(log2Ceil(lanes + 1).W))
    val oddWrites = Output(UInt(log2Ceil(lanes + 1).W))
    val parityConflicts = Output(UInt(log2Ceil(lanes + 1).W))
  })

  val core = Module(new CUTEV2Top)
  io.core <> core.io
  val tc = core.TaskCtrl.asInstanceOf[TaskController]
  private var observationId = 0
  private def obs[T <: Data](signal: T): T = {
    observationId += 1
    expose(signal).suggestName(s"throughputObservation_$observationId")
  }

  val c = tc.io
  val m = core.MTE.io
  val d = core.CDC.io
  val computeBusy = obs(tc.fuCompute.busy)
  val computeIssue = obs(c.MTE_MicroTask_Config.MicroTaskValid)
  val computeDone = obs(c.perfProbe.compDone)
  val pending = obs(core.MTE.pendingTokens)
  val tuple = obs(core.MTE.tupleFire)
  val dfire = obs(m.MatrixD.valid) && obs(m.MatrixD.ready)
  val crequest = d.FromMatrixRegIO.ReadBankAddr.map(x => obs(x.valid)).reduce(_ && _)
  val cresponse = d.FromMatrixRegIO.ReadResponseData.map(x => obs(x.valid)).reduce(_ && _)
  val cfire = obs(m.MatrixC.valid) && obs(m.MatrixC.ready)
  val cwrite = d.FromMatrixRegIO.WriteRequestData.map(x => obs(x.valid)).reduce(_ && _)
  val loadBusy = Seq(tc.fuAML.busy, tc.fuBML.busy, tc.fuASL.busy,
    tc.fuBSL.busy).map(obs(_)).reduce(_ || _)
  val dataLoadIssue = obs(c.AML_MicroTask_Config.MicroTaskValid) ||
    obs(c.BML_MicroTask_Config.MicroTaskValid)
  val scaleLoadIssue = c.ASL_MicroTask_Config.map(x => obs(x.MicroTaskValid)).getOrElse(false.B) ||
    c.BSL_MicroTask_Config.map(x => obs(x.MicroTaskValid)).getOrElse(false.B)

  val bankTraffic = core.CMatrixRegs.head.sram_banks.map { case (even, odd) =>
    val er = obs(even.io.r.req.valid) && obs(even.io.r.req.ready)
    val ew = obs(even.io.w.req.valid) && obs(even.io.w.req.ready)
    val or = obs(odd.io.r.req.valid) && obs(odd.io.r.req.ready)
    val ow = obs(odd.io.w.req.valid) && obs(odd.io.w.req.ready)
    (er, or, ew, ow, (obs(even.io.r.req.valid) && obs(even.io.w.req.valid)) ||
      (obs(odd.io.r.req.valid) && obs(odd.io.w.req.valid)))
  }
  val parity = bankTraffic.map(_._5).reduce(_ || _)
  val events = Seq(
    computeIssue, computeDone, tuple, dfire, crequest, cresponse, cfire, cwrite,
    obs(core.MTE.io.idle), obs(tc.windowFull), loadBusy && computeBusy,
    computeBusy && pending =/= 0.U && !tuple,
    computeBusy && !obs(m.VectorA.valid),
    computeBusy && !obs(m.VectorB.valid),
    computeBusy && !obs(m.MatrixC.valid),
    computeBusy && !obs(m.ScaleA.get.valid),
    computeBusy && !obs(m.ScaleB.get.valid),
    obs(core.CDC.ReadMatrixRegDataHoldValid), parity,
    (dataLoadIssue || scaleLoadIssue) && computeBusy)
  val diagnosticEvents = if (PanelThroughputEvents.diagnostic) {
    val cd = core.CDC
    // Enum definitions in the canonical CDC: task 1=mm, calculate 2=working.
    val readDemand = obs(cd.state) === 1.U && obs(cd.calculate_state) === 2.U &&
      obs(cd.CRequestVectorCount) < obs(cd.Max_Caculate_Iter)
    val go = obs(m.ComputeGo)
    val reusable = obs(cd.cReuseReady)
    val portConflict = obs(cd.cPortConflict)
    Seq(readDemand && go && reusable && portConflict,
      readDemand && go && !reusable, crequest, readDemand && !go)
  } else Seq.empty
  io.events := VecInit(events ++ diagnosticEvents)
  io.pending := pending
  io.result := obs(m.MatrixD.bits)
  io.flags := VecInit(m.DFlags.map(obs(_))).asUInt
  io.evenReads := PopCount(bankTraffic.map(_._1))
  io.oddReads := PopCount(bankTraffic.map(_._2))
  io.evenWrites := PopCount(bankTraffic.map(_._3))
  io.oddWrites := PopCount(bankTraffic.map(_._4))
  io.parityConflicts := PopCount(bankTraffic.map(_._5))
}

// TEST-ONLY emitter. Example:
// mill -i CUTE.test.runMain cute.PanelThroughputEmitter \
//   -td XSAI/CUTE/out/.../rtl --target systemverilog --split-verilog
object PanelThroughputEmitter extends App {
  ChiselDB.init(false)
  Constantin.init(false)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new PanelThroughputProbe),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--lowering-options=explicitBitcast")
  ))
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}
