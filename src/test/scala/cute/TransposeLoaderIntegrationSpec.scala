package cute

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import utility.{ChiselDB, Constantin}

/** TaskController + both unmodified normal paths + trLoader0 transpose trait +
  * production response bridges. No CPU, cache, FPE, or full-core build.
  */
class TransposeLoaderIntegrationHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new Bundles.AmuLsuIO))
    val memory = Flipped(Vec(2, new LocalMMUIO))
    val target = Flipped(Vec(2, new TrLoadMatrixRegIO))
    val targetId = Output(Vec(2, UInt(TrMatrixRegIdWidth.W)))
    val blockLoader1 = Input(Bool())
    val dispatch = Output(Vec(2, Bool()))
    val completed = Output(Vec(2, Bool()))
  })
  val tc = Module(new TaskController)
  val a = Module(new TrLoader0Wrapper)
  val b = Module(new TrLoader1Wrapper)
  for (store <- Seq(tc.io.trStorer0)) {
    store.command.ready := false.B
    store.done.valid := false.B
    store.done.bits := false.B
  }
  tc.io.DebugTimeStampe := 0.U
  tc.io.ygjkctrl.reset := false.B
  tc.io.ygjkctrl.mrelease.ready := true.B
  tc.io.ygjkctrl.amuCtrl.valid := io.command.valid
  tc.io.ygjkctrl.amuCtrl.bits.op := Bundles.AmuCtrlIO.mlsOp()
  tc.io.ygjkctrl.amuCtrl.bits.data := io.command.bits.asUInt
  tc.io.ygjkctrl.amuCtrl.bits.pc.foreach(_ := "h80001000".U)
  tc.io.ygjkctrl.amuCtrl.bits.coreid.foreach(_ := 0.U)
  io.command.ready := tc.io.ygjkctrl.amuCtrl.ready
  tc.io.ADC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.ADC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.BDC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.BDC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.CDC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.CDC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.CDC_MicroTask_Config.MicroTask_TEComputeEndValid := false.B
  tc.io.ASC_MicroTask_Config.foreach { c => c.MicroTaskReady := true.B; c.MicroTaskEndValid := false.B }
  tc.io.BSC_MicroTask_Config.foreach { c => c.MicroTaskReady := true.B; c.MicroTaskEndValid := false.B }
  tc.io.ASL_MicroTask_Config.foreach { c => c.MicroTaskReady := true.B; c.MicroTaskEndValid := false.B }
  tc.io.BSL_MicroTask_Config.foreach { c => c.MicroTaskReady := true.B; c.MicroTaskEndValid := false.B }
  tc.io.accMemoryConfig.LoadMicroTaskReady := true.B
  tc.io.accMemoryConfig.StoreMicroTaskReady := true.B
  tc.io.accMemoryConfig.LoadMicroTaskEndValid := false.B
  tc.io.accMemoryConfig.StoreMicroTaskEndValid := false.B
  a.io.ConfigInfo <> tc.io.trLoader0Config
  b.io.ConfigInfo <> tc.io.trLoader1Config
  tc.io.trLoader1Config.MicroTaskReady := b.io.ConfigInfo.MicroTaskReady && !io.blockLoader1
  b.io.ConfigInfo.MicroTaskValid := tc.io.trLoader1Config.MicroTaskValid && !io.blockLoader1
  a.io.DebugInfo := 0.U.asTypeOf(a.io.DebugInfo)
  b.io.DebugInfo := 0.U.asTypeOf(b.io.DebugInfo)
  io.target(0) <> a.io.ToMatrixRegIO
  io.target(1) <> b.io.ToMatrixRegIO
  io.targetId(0) := a.io.MatrixRegId
  io.targetId(1) := b.io.MatrixRegId
  io.dispatch(0) := a.io.ConfigInfo.MicroTaskValid && a.io.ConfigInfo.MicroTaskReady
  io.dispatch(1) := b.io.ConfigInfo.MicroTaskValid && b.io.ConfigInfo.MicroTaskReady
  io.completed(0) := a.io.ConfigInfo.MicroTaskEndValid && a.io.ConfigInfo.MicroTaskEndReady
  io.completed(1) := b.io.ConfigInfo.MicroTaskEndValid && b.io.ConfigInfo.MicroTaskEndReady
  for ((mem, i) <- Seq(a.io.LocalMMUIO, b.io.LocalMMUIO).zipWithIndex) {
    mem.ConherentRequsetSourceID := io.memory(i).ConherentRequsetSourceID
    mem.nonConherentRequsetSourceID := io.memory(i).nonConherentRequsetSourceID
    io.memory(i).Request <> mem.Request
    if (TrLoader0UseLegacyLoader) {
      val arb = Module(new Arbiter(new MMUResponseIO, TrMatrixRegNBanks))
      arb.io.in <> io.memory(i).Response
      mem.Response(0) <> arb.io.out
      for (lane <- 1 until TrMatrixRegNBanks) {
        mem.Response(lane).valid := false.B
        mem.Response(lane).bits := 0.U.asTypeOf(new MMUResponseIO)
      }
    } else {
      val bridge = Module(new ResponseChannelBridge(
        inputChannelCount = TrMatrixRegNBanks, respChannelCount = TrLoader0ResponseChannelCount,
        bankCount = TrMatrixRegNBanks, queueDepth = ResponseBridgeQueueDepth,
        dataWidth = outsideDataWidth, sourceIdWidth = 64,
        bankIdWidth = log2Ceil(TrMatrixRegNBanks), bankIdOffset = log2Ceil(TrMatrixRegBankNEntries),
        contextName = s"Test$i",
        identityFriendlyInput = TrMatrixRegNBanks == 8 && TrLoader0ResponseChannelCount == 8))
      bridge.io.timeStamp := 0.U
      bridge.io.serviceTraceActive := false.B
      bridge.io.releaseDebug := false.B
      bridge.io.in <> io.memory(i).Response
      mem.Response <> bridge.io.out
    }
  }
}

class TransposeLoaderIntegrationSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "shared trLoader0 transpose integration"
  for (banks <- Seq(4, 8); mode <- Seq("L", "1", "2", "4", "8") if mode != "8" || banks == 8) {
    it should s"run mlat/mlbt and preserve ordinary A/B loads for $banks banks mode $mode" in {
      ChiselDB.init(false)
      Constantin.init(false)
      val baseParams = TransposeTestParams(banks, mode)
      val testParams = baseParams.alterPartial {
        case CuteParamsKey => baseParams(CuteParamsKey).copy(EnableMatrixPrefetch = mode == "L")
      }
      test(new TransposeLoaderIntegrationHarness()(testParams))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          dut.io.blockLoader1.poke(false.B)
          val rng = new Random(42)
          val base = 0x1000L
          val stride = 640L
          val ids = Array.fill(2)(0)
          case class Response(id: BigInt, address: Long)
          dut.io.command.valid.poke(false.B)
          for (h <- 0 until 2) {
            dut.io.memory(h).ConherentRequsetSourceID.valid.poke(true.B)
            dut.io.memory(h).ConherentRequsetSourceID.bits.poke(0.U)
            dut.io.memory(h).nonConherentRequsetSourceID.valid.poke(false.B)
            dut.io.memory(h).nonConherentRequsetSourceID.bits.poke(0.U)
            for (lane <- 0 until banks) {
              dut.io.memory(h).Request(lane).ready.poke(false.B)
              dut.io.memory(h).Response(lane).valid.poke(false.B)
              dut.io.memory(h).Response(lane).bits.ReseponseSourceID.poke(0.U)
              dut.io.memory(h).Response(lane).bits.ReseponseData.poke(0.U)
              dut.io.memory(h).Response(lane).bits.ReseponseConherent.poke(true.B)
            }
          }
          // Alternate mlat/mlbt and ordinary A/B commands, including register
          // reuse and partial source beats. All e16/e32 element bytes differ.
          for (lg <- 0 to 3; trans <- Seq(true, false); isB <- Seq(false, true)) {
            val nibbles = if (lg == 3) 1 else 2 << lg
            val rows = if (trans && lg == 3 && banks == 4) 128 else if (trans) 9 else 11
            val cols = if (trans) (if (banks == 8) 127 else 63) else if (lg == 3) 127 else 13
            val blockLoader1 = !trans && lg == 1
            dut.io.blockLoader1.poke(blockLoader1.B)
            val host = if (trans || blockLoader1) 0 else 1
            val reg = if (isB) 3 else 2
            def value(r: Int, colByte: Int): Int = (r * 37 + colByte * 17 + (if (isB) 71 else 9)) & 255
            val expected = (for (r <- 0 until rows; c <- 0 until cols; i <- 0 until nibbles) yield {
              val destRow = if (trans) c else r
              val dest = (if (trans) r else c) * nibbles + i
              val src = c * nibbles + i
              val element = (value(r, src / 2) >> (4 * (src % 2))) & 15
              ((destRow % banks, (destRow / banks) * 2 + dest / 64, dest % 64), element)
            }).toMap
            val got = mutable.Map.empty[(Int, Int, Int), Int]
            val pending = Array.fill(2)(mutable.ArrayBuffer.empty[Response])
            val offered = Array.fill[Option[Response]](2, banks)(None)
            var sent = false
            var completed = false
            var dispatched = false
            var cycle = 0
            val cmd = dut.io.command.bits
            cmd.ms.poke(reg.U)
            cmd.ls.poke(false.B)
            cmd.transpose.poke(trans.B)
            cmd.isacc.poke(false.B)
            cmd.isA.poke((!isB).B)
            cmd.isB.poke(isB.B)
            cmd.baseAddr.poke(base.U)
            cmd.stride.poke(stride.U)
            cmd.row.poke((if (isB == trans) rows else cols).U)
            cmd.column.poke((if (isB == trans) cols else rows).U)
            // Normal A expects (row, column); transposed A expects the
            // destination shape (source column, source row). B reverses it.
            cmd.widths.poke((if (lg == 3) 7 else lg).U)
            while (!completed && cycle < 4000) {
              dut.io.command.valid.poke((!sent).B)
              for (h <- 0 until 2) {
                dut.io.memory(h).ConherentRequsetSourceID.bits.poke(ids(h).U)
                for (lane <- 0 until banks) {
                  if (offered(h)(lane).isEmpty && pending(h).nonEmpty && rng.nextInt(4) != 0) {
                    offered(h)(lane) = Some(pending(h).remove(rng.nextInt(pending(h).size)))
                  }
                  val mem = dut.io.memory(h)
                  mem.Request(lane).ready.poke((rng.nextInt(4) != 0).B)
                  mem.Response(lane).valid.poke(offered(h)(lane).nonEmpty.B)
                  offered(h)(lane).foreach { resp =>
                    val row = ((resp.address - base) / stride).toInt
                    val start = ((resp.address - base) % stride).toInt
                    val data = (0 until 64).foldLeft(BigInt(0))((x, i) => x | (BigInt(value(row, start + i)) << (8 * i)))
                    mem.Response(lane).bits.ReseponseSourceID.poke(resp.id.U)
                    mem.Response(lane).bits.ReseponseData.poke(data.U)
                  }
                }
              }
              if (dut.io.command.valid.peek().litToBoolean && dut.io.command.ready.peek().litToBoolean) sent = true
              for (h <- 0 until 2) {
                if (dut.io.dispatch(h).peek().litToBoolean) { assert(h == host); assert(!dispatched); dispatched = true }
                if (dut.io.completed(h).peek().litToBoolean) { assert(h == host); completed = true }
                for (lane <- 0 until banks) {
                  val req = dut.io.memory(h).Request(lane)
                  if (req.valid.peek().litToBoolean && req.ready.peek().litToBoolean) {
                    assert(h == host)
                    if (mode == "L" && !trans) {
                      val tag = req.bits.MatrixPrefetchTag.get.peek().litValue
                      assert(tag.testBit(19))
                      assert(((tag >> 16) & 7) == (if (isB) 2 else 1))
                      assert((tag & 0xffff) == lg * 4 + 2 + (if (isB) 1 else 0))
                    }
                    val alloc = req.bits.UseAllocatedSourceID.peek().litToBoolean
                    val id = if (alloc) BigInt(ids(h)) else req.bits.RequestSourceID.peek().litValue
                    pending(h) += Response(id, req.bits.RequestAddr.peek().litValue.toLong)
                    if (alloc) ids(h) = (ids(h) + 1) % 64
                  }
                  if (offered(h)(lane).nonEmpty && dut.io.memory(h).Response(lane).ready.peek().litToBoolean)
                    offered(h)(lane) = None
                  val target = dut.io.target(h)
                  for (port <- 0 until 2) {
                    val write = target.WriteRequest(lane)(port)
                    if (write.BankAddr.valid.peek().litToBoolean) {
                      assert(h == host)
                      dut.io.targetId(h).expect(reg.U)
                      val address = write.BankAddr.bits.peek().litValue.toInt
                      val mask = write.NibbleMask.bits.peek().litValue
                      val data = write.Data.bits.peek().litValue
                      for (by <- 0 until 64 if mask.testBit(by)) {
                        val key = (lane, address, by)
                        assert(!got.contains(key), s"duplicate byte $key")
                        got(key) = ((data >> (4 * by)) & 15).toInt
                      }
                    }
                  }
                }
              }
              dut.clock.step()
              cycle += 1
            }
            assert(completed && dispatched, s"task timeout mode=$mode transpose=$trans B=$isB")
            assert(pending.forall(_.isEmpty) && offered.flatten.forall(_.isEmpty))
            assert(got.toMap == expected, s"data mismatch mode=$mode e${4 * nibbles} transpose=$trans B=$isB: ${got.size}/${expected.size}")
            dut.io.command.valid.poke(false.B)
            for (h <- 0 until 2; lane <- 0 until banks) dut.io.memory(h).Response(lane).valid.poke(false.B)
            dut.clock.step(4)
          }
        }
    }
  }

  behavior of "the single transpose FU scheduler"
  it should "serialize mlat/mlbt on trLoader0 while a normal mlb progresses on trLoader1" in {
    ChiselDB.init(false)
    Constantin.init(false)
    test(new TransposeLoaderIntegrationHarness()(TransposeTestParams(8, "8")))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.io.blockLoader1.poke(false.B)
        // The first two destinations share trLoader0; the third can issue on trLoader1
        // while trLoader0's first task is waiting for responses.
        case class Task(isB: Boolean, trans: Boolean, reg: Int, base: Int)
        val tasks = Seq(Task(false, true, 0, 0x1000), Task(true, true, 1, 0x2000), Task(true, false, 2, 0x3000))
        case class Resp(id: BigInt, addr: Int)
        val pending = Array.fill(2)(mutable.Queue.empty[Resp])
        val launches = Array.fill(2)(mutable.ArrayBuffer.empty[Int])
        val finishes = Array.fill(2)(0)
        var next = 0
        var cycle = 0
        for (h <- 0 until 2) {
          dut.io.memory(h).ConherentRequsetSourceID.valid.poke(false.B)
          dut.io.memory(h).ConherentRequsetSourceID.bits.poke(0.U)
          dut.io.memory(h).nonConherentRequsetSourceID.valid.poke(false.B)
          dut.io.memory(h).nonConherentRequsetSourceID.bits.poke(0.U)
          for (lane <- 0 until 8) {
            dut.io.memory(h).Request(lane).ready.poke(true.B)
            dut.io.memory(h).Response(lane).valid.poke(false.B)
            dut.io.memory(h).Response(lane).bits.ReseponseData.poke(0.U)
            dut.io.memory(h).Response(lane).bits.ReseponseSourceID.poke(0.U)
            dut.io.memory(h).Response(lane).bits.ReseponseConherent.poke(true.B)
          }
        }
        while (finishes.sum < 3 && cycle < 400) {
          val t = tasks(next min 2)
          dut.io.command.valid.poke((next < tasks.size).B)
          dut.io.command.bits.ms.poke(t.reg.U)
          dut.io.command.bits.ls.poke(false.B)
          dut.io.command.bits.transpose.poke(t.trans.B)
          dut.io.command.bits.isacc.poke(false.B)
          dut.io.command.bits.isA.poke((!t.isB).B)
          dut.io.command.bits.isB.poke(t.isB.B)
          dut.io.command.bits.baseAddr.poke(t.base.U)
          dut.io.command.bits.stride.poke(64.U)
          dut.io.command.bits.row.poke(8.U)
          dut.io.command.bits.column.poke(8.U)
          dut.io.command.bits.widths.poke(0.U)
          for (h <- 0 until 2) {
            // Delay all responses to prove both physical FUs can be busy.
            val resp = dut.io.memory(h).Response(0)
            resp.valid.poke((cycle >= 30 && pending(h).nonEmpty).B)
            if (pending(h).nonEmpty) {
              resp.bits.ReseponseSourceID.poke(pending(h).front.id.U)
              resp.bits.ReseponseData.poke(BigInt("0101010101010101" * 8, 16).U)
            }
          }
          if (next < tasks.size && dut.io.command.ready.peek().litToBoolean) next += 1
          for (h <- 0 until 2) {
            if (dut.io.dispatch(h).peek().litToBoolean) launches(h) += cycle
            if (dut.io.completed(h).peek().litToBoolean) finishes(h) += 1
            val resp = dut.io.memory(h).Response(0)
            if (resp.valid.peek().litToBoolean && resp.ready.peek().litToBoolean) pending(h).dequeue()
            for (lane <- 0 until 8) {
              val req = dut.io.memory(h).Request(lane)
              if (req.valid.peek().litToBoolean) {
                val addr = req.bits.RequestAddr.peek().litValue.toInt
                assert((h == 0 && addr >= 0x1000 && addr < 0x3000) || (h == 1 && addr >= 0x3000))
                pending(h).enqueue(Resp(req.bits.RequestSourceID.peek().litValue, addr))
              }
              for (port <- 0 until 2) {
                if (dut.io.target(h).WriteRequest(lane)(port).BankAddr.valid.peek().litToBoolean) {
                  val id = dut.io.targetId(h).peek().litValue.toInt
                  assert(if (h == 0) id == finishes(0) else id == 2)
                }
              }
            }
          }
          if (cycle == 29) {
            assert(launches(0).size == 1 && launches(1).size == 1,
              "normal B load should overlap first transpose; second transpose must wait")
          }
          dut.clock.step()
          cycle += 1
        }
        assert(finishes.toSeq == Seq(2, 1))
        assert(launches(0).size == 2 && launches(1).size == 1)
        assert(launches(0)(1) > 30 && pending.forall(_.isEmpty))
      }
  }
}

/** Narrow synthesis-RTL/elaboration check, including the optional difftest
  * wiring. Example: CUTE.test.runMain cute.EmitTransposeLoad 8 8 true outDir
  */
object EmitTransposeLoad extends App {
  require(args.length == 4, "banks mode difftest output-directory")
  implicit val p: Parameters = TransposeTestParams(args(0).toInt, args(1), args(2).toBoolean)
  ChiselDB.init(false)
  Constantin.init(false)
  (new ChiselStage).execute(
    Array("-td", args(3), "--target", "systemverilog", "--split-verilog"),
    Seq(ChiselGeneratorAnnotation(() => new TransposeLoaderIntegrationHarness),
      FirtoolOption("--disable-annotation-unknown")))
}

class TrMatrixStoreControlHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new Bundles.AmuLsuIO))
    val op = Input(UInt(2.W))
    val mma = Input(new Bundles.AmuMmaIO)
    val arith = Input(new Bundles.AmuArithIO)
    val loadReady = Input(Vec(2, Bool()))
    val loadRegs = Output(Vec(2, UInt(2.W)))
    val loadRows = Output(Vec(2, UInt(MatrixRegMaxTensorDimBitSize.W)))
    val loadBeats = Output(Vec(2, UInt(MatrixRegMaxTensorDimBitSize.W)))
    val loadTranspose = Output(Vec(2, Bool()))
    val a = new TrStoreControlIO
    val loads = Output(Vec(3, Bool()))
    val loadDone = Input(Vec(3, Bool()))
    val cStore = Output(Bool())
    val cStoreDone = Input(Bool())
    val compute = Output(Bool())
    val computeDone = Input(Vec(3, Bool()))
    val release = Output(Bool())
  })
  val taskController = Module(new TaskController)
  taskController.io.DebugTimeStampe := 0.U
  taskController.io.ygjkctrl.reset := false.B
  taskController.io.ygjkctrl.mrelease.ready := true.B
  taskController.io.ygjkctrl.amuCtrl.valid := io.command.valid
  taskController.io.ygjkctrl.amuCtrl.bits.op := io.op
  taskController.io.ygjkctrl.amuCtrl.bits.data := Mux(io.op === 0.U, io.mma.asUInt,
    Mux(io.op === 2.U, 0.U, Mux(io.op === 3.U, io.arith.asUInt, io.command.bits.asUInt)))
  io.command.ready := taskController.io.ygjkctrl.amuCtrl.ready
  taskController.io.ygjkctrl.amuCtrl.bits.pc.foreach(_ := "h80001000".U)
  taskController.io.ygjkctrl.amuCtrl.bits.coreid.foreach(_ := 0.U)
  io.a <> taskController.io.trStorer0
  taskController.io.trLoader0Config.MicroTaskReady := io.loadReady(0)
  taskController.io.trLoader1Config.MicroTaskReady := io.loadReady(1)
  taskController.io.accMemoryConfig.LoadMicroTaskReady := true.B
  taskController.io.accMemoryConfig.StoreMicroTaskReady := true.B
  taskController.io.trLoader0Config.MicroTaskEndValid := io.loadDone(0)
  taskController.io.trLoader1Config.MicroTaskEndValid := io.loadDone(1)
  taskController.io.accMemoryConfig.LoadMicroTaskEndValid := io.loadDone(2)
  taskController.io.accMemoryConfig.StoreMicroTaskEndValid := io.cStoreDone
  io.loads := VecInit(taskController.io.trLoader0Config.MicroTaskValid,
    taskController.io.trLoader1Config.MicroTaskValid,
    taskController.io.accMemoryConfig.LoadMicroTaskValid)
  io.cStore := taskController.io.accMemoryConfig.StoreMicroTaskValid
  taskController.io.ADC_MicroTask_Config.MicroTaskReady := true.B
  taskController.io.BDC_MicroTask_Config.MicroTaskReady := true.B
  taskController.io.CDC_MicroTask_Config.MicroTaskReady := true.B
  taskController.io.ADC_MicroTask_Config.MicroTaskEndValid := io.computeDone(0)
  taskController.io.BDC_MicroTask_Config.MicroTaskEndValid := io.computeDone(1)
  taskController.io.CDC_MicroTask_Config.MicroTaskEndValid := io.computeDone(2)
  taskController.io.CDC_MicroTask_Config.MicroTask_TEComputeEndValid := false.B
  taskController.io.ASC_MicroTask_Config.foreach { config =>
    config.MicroTaskReady := true.B
    config.MicroTaskEndValid := false.B
  }
  taskController.io.BSC_MicroTask_Config.foreach { config =>
    config.MicroTaskReady := true.B
    config.MicroTaskEndValid := false.B
  }
  taskController.io.ASL_MicroTask_Config.foreach { config =>
    config.MicroTaskReady := true.B
    config.MicroTaskEndValid := false.B
  }
  taskController.io.BSL_MicroTask_Config.foreach { config =>
    config.MicroTaskReady := true.B
    config.MicroTaskEndValid := false.B
  }
  for ((cfg, i) <- Seq(taskController.io.trLoader0Config, taskController.io.trLoader1Config).zipWithIndex) {
    io.loadRegs(i) := cfg.MatrixRegId
    io.loadRows(i) := cfg.MatrixRegTensor_M
    io.loadBeats(i) := cfg.MatrixRegTensor_K
    io.loadTranspose(i) := cfg.Is_Transpose
  }
  io.compute := taskController.io.ADC_MicroTask_Config.MicroTaskValid
  io.release := taskController.io.ygjkctrl.mrelease.valid
}

class TrMatrixStoreControlSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "A/B matrix-store scheduling"

  it should "preserve dependencies, store order, shared transpose dispatch and release ordering" in {
    implicit val p: Parameters = TrMatrixStoreTestParams()
    ChiselDB.init(false)
    Constantin.init(false)
    test(new TrMatrixStoreControlHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.command.valid.poke(false.B)
      dut.io.op.poke(1.U)
      dut.io.loadReady.foreach(_.poke(true.B))
      dut.io.loadDone.foreach(_.poke(false.B))
      dut.io.computeDone.foreach(_.poke(false.B))
      dut.io.cStoreDone.poke(false.B)
      for (port <- Seq(dut.io.a)) {
        port.command.ready.poke(true.B)
        port.done.valid.poke(false.B)
        port.done.bits.poke(true.B)
      }
      val events = mutable.ArrayBuffer.empty[String]
      val descriptors = mutable.ArrayBuffer.empty[(String, BigInt, BigInt, BigInt, Boolean)]
      def tick(cycles: Int = 1): Unit = for (_ <- 0 until cycles) {
        for ((port, name) <- Seq((dut.io.a, "AS"))) {
          if (port.command.valid.peek().litToBoolean && port.command.ready.peek().litToBoolean) {
            events += name
            val command = port.command.bits
            descriptors += ((name, command.regId.peek().litValue, command.rows.peek().litValue,
              command.columns.peek().litValue, command.transpose.peek().litToBoolean))
          }
        }
        if (dut.io.cStore.peek().litToBoolean) events += "CS"
        if (dut.io.compute.peek().litToBoolean) events += "MMA"
        if (dut.io.release.peek().litToBoolean) events += "REL"
        for (i <- 0 until 3) if (dut.io.loads(i).peek().litToBoolean) events += s"L$i"
        dut.clock.step()
      }
      def lsu(reg: Int, store: Boolean = true, isB: Boolean = false,
          transpose: Boolean = false, acc: Boolean = false,
          rows: Int = 64, columns: Int = 64): Unit = {
        dut.io.op.poke(1.U)
        val command = dut.io.command.bits
        command.ms.poke(reg.U)
        command.ls.poke(store.B)
        command.isacc.poke(acc.B)
        command.isA.poke((!isB && !acc).B)
        command.isB.poke(isB.B)
        command.transpose.poke(transpose.B)
        command.baseAddr.poke("h80000000".U)
        command.stride.poke(128.U)
        command.row.poke(rows.U)
        command.column.poke(columns.U)
        command.widths.poke(0.U)
        dut.io.command.valid.poke(true.B)
        var waitCycles = 0
        while (!dut.io.command.ready.peek().litToBoolean && waitCycles < 30) {
          tick()
          waitCycles += 1
        }
        assert(waitCycles < 30)
        tick()
        dut.io.command.valid.poke(false.B)
      }
      def done(port: TrStoreControlIO): Unit = {
        port.done.ready.expect(true.B)
        port.done.valid.poke(true.B)
        tick()
        port.done.valid.poke(false.B)
        tick(4)
      }
      def mma(): Unit = {
        dut.io.op.poke(0.U)
        val command = dut.io.mma
        command.md.poke(4.U)
        command.ms1.poke(0.U)
        command.ms2.poke(1.U)
        command.rm.poke(0.U)
        command.sat.poke(false.B)
        command.isfp.poke(false.B)
        command.mtilem.poke(8.U)
        command.mtilen.poke(8.U)
        command.mtilek.poke(64.U)
        command.types1.poke(4.U)
        command.types2.poke(4.U)
        command.typed.poke(6.U)
        dut.io.command.valid.poke(true.B)
        dut.io.command.ready.expect(true.B)
        tick()
        dut.io.command.valid.poke(false.B)
        tick(5)
      }
      tick(2)
      lsu(0, store = false)
      tick(5)
      assert(events.toSeq == Seq("L1"))
      lsu(0)
      lsu(1, isB = true)
      tick(6)
      assert(events.toSeq == Seq("L1"), "store overtook its load or an older store")
      dut.io.loadDone(1).poke(true.B)
      tick()
      dut.io.loadDone(1).poke(false.B)
      tick(5)
      assert(events.toSeq == Seq("L1", "AS"))
      lsu(0, store = false)
      lsu(2, store = false, isB = true)
      tick(5)
      assert(events.last == "L1")
      dut.io.loadDone(1).poke(true.B)
      tick()
      dut.io.loadDone(1).poke(false.B)
      done(dut.io.a)
      assert(events.count(_ == "AS") == 2 && events.count(_ == "L1") == 3)
      dut.io.loadDone(1).poke(true.B)
      tick()
      dut.io.loadDone(1).poke(false.B)
      done(dut.io.a)
      lsu(3, isB = true, transpose = true, rows = 16, columns = 64)
      tick(5)
      assert(descriptors.last == (("AS", BigInt(3), BigInt(64), BigInt(16), true)))
      lsu(4, acc = true)
      tick(5)
      assert(!events.contains("CS"))
      done(dut.io.a)
      assert(events.last == "CS")
      val storesBeforeAccStoreDone = events.count(_ == "AS")
      lsu(2, isB = true)
      tick(5)
      assert(events.count(_ == "AS") == storesBeforeAccStoreDone, "trStorer0 overtook older accStorer0")
      dut.io.op.poke(2.U)
      dut.io.command.valid.poke(true.B)
      tick()
      dut.io.command.valid.poke(false.B)
      tick(5)
      assert(!events.contains("REL"))
      dut.io.cStoreDone.poke(true.B)
      tick()
      dut.io.cStoreDone.poke(false.B)
      tick(5)
      assert(events.count(_ == "AS") == storesBeforeAccStoreDone + 1)
      assert(!events.contains("REL"), "release bypassed younger trStorer0 after accStorer0 completed")
      done(dut.io.a)
      assert(events.last == "REL")

      mma()
      assert(events.last == "MMA")
      lsu(0)
      tick(5)
      assert(events.last == "MMA")
      dut.io.computeDone(0).poke(true.B)
      tick()
      dut.io.computeDone(0).poke(false.B)
      tick(5)
      assert(events.last == "AS", "store should wait only for the matching compute reader")
      dut.io.computeDone(1).poke(true.B)
      tick()
      dut.io.computeDone(1).poke(false.B)
      dut.io.computeDone(2).poke(true.B)
      tick()
      dut.io.computeDone(2).poke(false.B)
      tick(5)
      val mmaCount = events.count(_ == "MMA")
      mma()
      assert(events.count(_ == "MMA") == mmaCount, "compute overlapped same-TR store")
      done(dut.io.a)
      assert(events.count(_ == "MMA") == mmaCount + 1)
      dut.io.computeDone.foreach(_.poke(true.B))
      tick()
      dut.io.computeDone.foreach(_.poke(false.B))
      tick(5)
    }
  }
  it should "prefer TL1, fall back to TL0 and preserve register owners and zero destinations" in {
    implicit val p: Parameters = TrMatrixStoreTestParams()
    ChiselDB.init(false)
    Constantin.init(false)
    test(new TrMatrixStoreControlHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.command.valid.poke(false.B)
      dut.io.op.poke(1.U)
      dut.io.loadReady.foreach(_.poke(true.B))
      dut.io.loadDone.foreach(_.poke(false.B))
      dut.io.computeDone.foreach(_.poke(false.B))
      dut.io.cStoreDone.poke(false.B)
      dut.io.a.command.ready.poke(true.B)
      dut.io.a.done.valid.poke(false.B)
      dut.io.a.done.bits.poke(true.B)
      val issued = mutable.ArrayBuffer.empty[(Int, Int, Int, Int, Boolean)]
      def tick(n: Int = 1): Unit = for (_ <- 0 until n) {
        for (unit <- 0 until 3 if dut.io.loads(unit).peek().litToBoolean) {
          if (unit < 2) issued += ((unit, dut.io.loadRegs(unit).peek().litValue.toInt,
            dut.io.loadRows(unit).peek().litValue.toInt, dut.io.loadBeats(unit).peek().litValue.toInt,
            dut.io.loadTranspose(unit).peek().litToBoolean))
          else issued += ((2, 0, 0, 0, false))
        }
        dut.clock.step()
      }
      def submit(reg: Int, isB: Boolean, trans: Boolean = false): Unit = {
        dut.io.op.poke(1.U)
        val cmd = dut.io.command.bits
        cmd.ms.poke(reg.U); cmd.ls.poke(false.B); cmd.isacc.poke(false.B)
        cmd.isA.poke((!isB).B); cmd.isB.poke(isB.B); cmd.transpose.poke(trans.B)
        cmd.baseAddr.poke("h80000000".U); cmd.stride.poke(128.U); cmd.widths.poke(1.U)
        // Deliberately unequal dimensions: normalized source shape is 7 x 40 e16.
        cmd.row.poke((if (isB ^ trans) 40 else 7).U)
        cmd.column.poke((if (isB ^ trans) 7 else 40).U)
        dut.io.command.valid.poke(true.B)
        dut.io.command.ready.expect(true.B)
        tick(); dut.io.command.valid.poke(false.B); tick(5)
      }
      def finish(unit: Int): Unit = {
        dut.io.loadDone(unit).poke(true.B); tick()
        dut.io.loadDone(unit).poke(false.B); tick(5)
      }
      submit(0, false)
      submit(1, false)
      assert(issued.toSeq == Seq((1, 0, 7, 2, false), (0, 1, 7, 2, false)))
      submit(2, true, trans = true)
      submit(0, true)
      assert(issued.size == 2, "busy FUs and same-register WAW must block dispatch")
      finish(1)
      assert(issued.last == ((1, 0, 7, 2, false)), "ready ordinary B may bypass a blocked transpose")
      finish(0)
      assert(issued.last == ((0, 2, 7, 2, true)))
      finish(1); finish(0)
      dut.io.loadReady(1).poke(false.B)
      submit(3, true)
      assert(issued.last == ((0, 3, 7, 2, false)), "TL1 backpressure must allow TL0 fallback")
      finish(0)
      dut.io.loadReady(1).poke(true.B)
      for (reg <- Seq(1, 4)) {
        dut.io.op.poke(3.U)
        dut.io.arith.md.poke(reg.U)
        dut.io.arith.opType.poke((0x6e << 2).U)
        dut.io.command.valid.poke(true.B); dut.io.command.ready.expect(true.B)
        tick(); dut.io.command.valid.poke(false.B); tick(5)
        val unit = if (reg < 4) 0 else 2
        assert(issued.last._1 == unit)
        finish(unit)
      }
    }
  }

}
