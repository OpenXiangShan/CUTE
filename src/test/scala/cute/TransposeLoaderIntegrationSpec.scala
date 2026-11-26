package cute

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
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
  tc.io.downstreamIdle := true.B
  tc.io.ygjkctrl.reset := false.B
  tc.io.ygjkctrl.minitClear := false.B
  tc.io.ygjkctrl.mrelease.ready := true.B
  tc.io.ygjkctrl.amuCtrl.valid := io.command.valid
  tc.io.ygjkctrl.amuCtrl.bits.op := Bundles.AmuCtrlIO.mlsOp()
  tc.io.ygjkctrl.amuCtrl.bits.data := io.command.bits.asUInt
  tc.io.ygjkctrl.amuCtrl.bits.pc.foreach(_ := "h80001000".U)
  tc.io.ygjkctrl.amuCtrl.bits.coreid.foreach(_ := 0.U)
  tc.io.ygjkctrl.amuCtrl.bits.retireId.foreach(_ := 0.U)
  tc.io.ygjkctrl.amuCtrl.bits.epoch.foreach(_ := 0.U)
  io.command.ready := tc.io.ygjkctrl.amuCtrl.ready
  tc.io.ADC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.ADC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.BDC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.BDC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.CDC_MicroTask_Config.MicroTaskReady := true.B
  tc.io.CDC_MicroTask_Config.MicroTaskEndValid := false.B
  tc.io.CDC_MicroTask_Config.MicroTaskFlags := 0.U
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
          for (lg <- 0 to 2; trans <- Seq(true, false); isB <- Seq(false, true)) {
            val e = 1 << lg
            val rows = if (trans) 9 else 11
            val cols = if (trans) (if (banks == 8) 127 else 63) else 13
            val blockLoader1 = !trans && lg == 1
            dut.io.blockLoader1.poke(blockLoader1.B)
            val host = if (trans || blockLoader1) 0 else 1
            val reg = if (isB) 3 else 2
            def value(r: Int, colByte: Int): Int = (r * 37 + colByte * 17 + (if (isB) 71 else 9)) & 255
            val expected = (for (r <- 0 until rows; c <- 0 until cols; by <- 0 until e) yield {
              val destRow = if (trans) c else r
              val destByte = (if (trans) r else c) * e + by
              ((destRow % banks, (destRow / banks) * 2 + destByte / 32, destByte % 32), value(r, c * e + by))
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
            cmd.widths.poke(lg.U)
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
                      val mask = write.ByteMask.bits.peek().litValue
                      val data = write.Data.bits.peek().litValue
                      for (by <- 0 until 32 if mask.testBit(by)) {
                        val key = (lane, address, by)
                        assert(!got.contains(key), s"duplicate byte $key")
                        got(key) = ((data >> (8 * by)) & 255).toInt
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
            assert(got.toMap == expected, s"data mismatch mode=$mode e${8 * e} transpose=$trans B=$isB: ${got.size}/${expected.size}")
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
  taskController.io.downstreamIdle := true.B
  taskController.io.ygjkctrl.reset := false.B
  taskController.io.ygjkctrl.minitClear := false.B
  taskController.io.ygjkctrl.mrelease.ready := true.B
  taskController.io.ygjkctrl.amuCtrl.valid := io.command.valid
  taskController.io.ygjkctrl.amuCtrl.bits.op := io.op
  taskController.io.ygjkctrl.amuCtrl.bits.data := Mux(io.op === 0.U, io.mma.asUInt,
    Mux(io.op === 2.U, 0.U, Mux(io.op === 3.U, io.arith.asUInt, io.command.bits.asUInt)))
  io.command.ready := taskController.io.ygjkctrl.amuCtrl.ready
  taskController.io.ygjkctrl.amuCtrl.bits.pc.foreach(_ := "h80001000".U)
  taskController.io.ygjkctrl.amuCtrl.bits.coreid.foreach(_ := 0.U)
  taskController.io.ygjkctrl.amuCtrl.bits.retireId.foreach(_ := 0.U)
  taskController.io.ygjkctrl.amuCtrl.bits.epoch.foreach(_ := 0.U)
  io.a <> taskController.io.trStorer0
  taskController.io.trLoader0Config.MicroTaskReady := io.loadReady(0)
  taskController.io.trLoader1Config.MicroTaskReady := io.loadReady(1)
  taskController.io.accMemoryConfig.LoadMicroTaskReady := true.B
  taskController.io.accMemoryConfig.StoreMicroTaskReady := true.B
  taskController.io.trLoader0Config.MicroTaskEndValid := io.loadDone(0)
  taskController.io.trLoader1Config.MicroTaskEndValid := io.loadDone(1)
  taskController.io.trLoader0Config.MicroTaskFlags := 0.U
  taskController.io.trLoader1Config.MicroTaskFlags := 0.U
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
  taskController.io.CDC_MicroTask_Config.MicroTaskFlags := 0.U
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


/**
  * Gives both native TR loaders the same converting-load test plane while preserving
  * their production LocalMMU and MatrixReg ports.
  */
class MxTrLoaderHarness(isB: Boolean, concurrentScaleReads: Boolean = false)(implicit p: Parameters) extends CuteModule {
  private val configWidth = MatrixRegMaxTensorDimBitSize
  private val testWordBits = 32
  private val testAddrWidth = 32
  private val testSourceWidth = LLCSourceMaxNumBitSize
  require(ExperimentalMxfpConvertLoad && Tensor_MN == 64 && TrMatrixRegNBanks == 4 &&
    TrMatrixRegEntryByteSize == 32 && ReduceGroupSize == 2 && outsideDataWidth == 512)
  require(!concurrentScaleReads || !ExperimentalMxfpDynamicQuant)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val dataType = Input(UInt(ElementDataType.DataTypeBitWidth.W))
    val sourceRows = Input(UInt(configWidth.W))
    val beatsPerRow = Input(UInt(configWidth.W))
    val base = Input(UInt(testAddrWidth.W))
    val stride = Input(UInt(testAddrWidth.W))
    val hasTail = Input(Bool())
    val tailBytes = Input(UInt(log2Ceil(outsideDataWidthByte + 1).W))
    val sourceId = Input(UInt(LLCSourceMaxNumBitSize.W))

    val taskReady = Output(Bool())
    val taskEnd = Output(Bool())
    // Keep only the observed request fields and split wide data into native
    // words, as in the existing cache/scale tests. Production ports stay intact.
    val request = Decoupled(new Bundle {
      val RequestAddr = UInt(testAddrWidth.W)
      val RequestSourceID = UInt(testSourceWidth.W)
    })
    val response = Flipped(Decoupled(new Bundle {
      val ReseponseData = Vec(MMUDataWidth / testWordBits, UInt(testWordBits.W))
      val ReseponseConherent = Bool()
      val ReseponseSourceID = UInt(testSourceWidth.W)
    }))
    val matrix = Output(new Bundle {
      val active = Bool()
      val BankAddr = Vec(TrMatrixRegNBanks, Valid(UInt(log2Ceil(TrMatrixRegBankNEntries).W)))
      val Data = Vec(TrMatrixRegNBanks,
        Valid(Vec(TrMatrixRegEntryBitSize / testWordBits, UInt(testWordBits.W))))
      val ByteMask = Vec(TrMatrixRegNBanks, Valid(UInt(TrMatrixRegEntryByteSize.W)))
    })
    val mx = new Bundle {
      val computeType = Input(UInt(MteComputeType.ComputeTypeBitWidth.W))
      val scaleBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val endReady = Input(Bool())
      val flags = Output(UInt(5.W))
      val selectedScaleBank = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val scaleActive = Output(Bool())
      val scaleRead = Output(Valid(UInt(log2Ceil(ABScaleBankNEntries).W)))
      val scaleWrite = Input(Bool())
      val scaleWriteBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val scaleWriteAddr = Input(UInt(log2Ceil(ABScaleBankNEntries).W))
      val scaleWriteWords = Input(Vec(outsideDataWidth / 32, UInt(32.W)))
      val matrixInit = Input(Bool())
      val matrixInitAddr = Input(UInt(log2Ceil(TrMatrixRegBankNEntries).W))
      val matrixInitWords = Input(Vec(TrMatrixRegNBanks,
        Vec(TrMatrixRegEntryBitSize / 32, UInt(32.W))))
      val read = Flipped(Decoupled(UInt(log2Ceil(TrMatrixRegBankNEntries).W)))
      val data = Output(Valid(Vec(TrMatrixRegNBanks,
        Vec(TrMatrixRegEntryBitSize / 32, UInt(32.W)))))
    }
    val dynamic = Option.when(ExperimentalMxfpDynamicQuant)(new Bundle {
      val enable = Input(Bool())
      val stall = Input(Bool())
      val scaleEndReady = Input(Bool())
      val scaleTaskReady = Output(Bool())
      val scaleEnd = Output(Bool())
      val scaleMemoryRequest = Output(Bool())
      val scaleWrite = Output(Bool())
      val generatedPending = Output(Bool())
      val generated = Output(Valid(new MxfpGeneratedScale))
      val read = Flipped(Decoupled(UInt(log2Ceil(ABScaleBankNEntries).W)))
      val data = Output(Valid(Vec(outsideDataWidth / 32, UInt(32.W))))
    })
    val computeScale = Option.when(concurrentScaleReads)(new Bundle {
      val start = Input(Bool())
      val go = Input(Bool())
      val readEnable = Input(Bool())
      val bank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val rows = Input(UInt(configWidth.W))
      val endReady = Input(Bool())
      val taskReady = Output(Bool())
      val taskEnd = Output(Bool())
      val idle = Output(Bool())
      val selectedBank = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val valid = Output(Bool())
      val words = Output(Vec(ScaleWidth * Matrix_MN / 32, UInt(32.W)))
      val liveK = Output(Vec(Matrix_MN, UInt(log2Ceil(ReduceWidth / 4 + 1).W)))
      val requestFires = Output(UInt(2.W))
    })
  })

  private def connectPorts(mmu: LocalMMUIO, matrix: TrLoadMatrixRegIO,
      scale: Option[ABScaleControlMatrixRegIO], scaleBank: UInt, scaleActive: Bool,
      generated: Option[DecoupledIO[MxfpGeneratedScale]]): Unit = {
    io.request.valid := mmu.Request(0).valid
    io.request.bits.RequestAddr := mmu.Request(0).bits.RequestAddr
    io.request.bits.RequestSourceID := mmu.Request(0).bits.RequestSourceID
    when(mmu.Request(0).valid) {
      assert((mmu.Request(0).bits.RequestAddr >> testAddrWidth) === 0.U,
        "MX fixture address exceeded its observed 32-bit window")
      assert((mmu.Request(0).bits.RequestSourceID >> testSourceWidth) === 0.U,
        "MX fixture source ID exceeded its observed allocator width")
    }
    mmu.Request(0).ready := io.request.ready
    mmu.Response(0).valid := io.response.valid
    mmu.Response(0).bits.ReseponseData := io.response.bits.ReseponseData.asUInt
    mmu.Response(0).bits.ReseponseConherent := io.response.bits.ReseponseConherent
    mmu.Response(0).bits.ReseponseSourceID := io.response.bits.ReseponseSourceID
    io.response.ready := mmu.Response(0).ready
    io.matrix.active := matrix.active
    for (bank <- 0 until TrMatrixRegNBanks) {
      io.matrix.BankAddr(bank) := matrix.BankAddr(bank)
      io.matrix.ByteMask(bank) := matrix.ByteMask(bank)
      io.matrix.Data(bank).valid := matrix.Data(bank).valid
      io.matrix.Data(bank).bits := matrix.Data(bank).bits.asTypeOf(io.matrix.Data(bank).bits)
    }
    val m = io.mx
    val regs = Seq.fill(ABScaleRegCount)(Module(new ABScaleMatrixReg))
    val port = scale.get
    if (concurrentScaleReads) {
      val probe = io.computeScale.get
      def connectCompute(mem: ABScaleControlMatrixRegIO, values: DecoupledIO[UInt],
          bank: UInt, idle: Bool, ready: Bool, end: Bool, liveK: Vec[UInt]): Unit = {
        values.ready := probe.go
        probe.taskReady := ready
        probe.taskEnd := end
        probe.idle := idle
        probe.selectedBank := bank
        probe.valid := values.valid
        probe.words := values.bits.asTypeOf(probe.words)
        probe.liveK := liveK
        CUTEV2Top.connectScaleControlToRegs(
          bank, mem, regs, Some(port), scaleBank, scaleActive)(clock, reset)
        probe.requestFires := VecInit(Seq(mem.BankAddr.fire, port.BankAddr.fire)).asUInt
      }
      if (isB) {
        val c = Module(new BScaleController)
        c.io.DebugInfo.DebugTimeStampe := 0.U
        c.io.ConfigInfo.MatrixRegTensor_M := Matrix_MN.U
        c.io.ConfigInfo.MatrixRegTensor_N := probe.rows
        c.io.ConfigInfo.MatrixRegTensor_K := 1.U
        c.io.ConfigInfo.ScaleBlocksPerRow := 2.U
        c.io.ConfigInfo.KElements := 64.U
        c.io.ConfigInfo.MatrixRegId := 1.U
        c.io.ConfigInfo.ScaleBankId := probe.bank
        c.io.ConfigInfo.computeType := MteComputeType.Mxfp4F32
        c.io.ConfigInfo.Is_Transpose := false.B
        c.io.ConfigInfo.MicroTaskValid := probe.start
        c.io.ConfigInfo.MicroTaskEndReady := probe.endReady
        c.io.ComputeGo := probe.go
        c.io.ReadIssueEnable := probe.readEnable
        connectCompute(c.io.FromMatrixRegIO, c.io.ScaleB, c.io.SpadId, c.io.idle,
          c.io.ConfigInfo.MicroTaskReady, c.io.ConfigInfo.MicroTaskEndValid, c.io.LiveK)
      } else {
        val c = Module(new AScaleController)
        c.io.DebugInfo.DebugTimeStampe := 0.U
        c.io.ConfigInfo.MatrixRegTensor_M := probe.rows
        c.io.ConfigInfo.MatrixRegTensor_N := Matrix_MN.U
        c.io.ConfigInfo.MatrixRegTensor_K := 1.U
        c.io.ConfigInfo.ScaleBlocksPerRow := 2.U
        c.io.ConfigInfo.KElements := 64.U
        c.io.ConfigInfo.MatrixRegId := 0.U
        c.io.ConfigInfo.ScaleBankId := probe.bank
        c.io.ConfigInfo.computeType := MteComputeType.Mxfp4F32
        c.io.ConfigInfo.Is_Transpose := false.B
        c.io.ConfigInfo.MicroTaskValid := probe.start
        c.io.ConfigInfo.MicroTaskEndReady := probe.endReady
        c.io.ComputeGo := probe.go
        c.io.ReadIssueEnable := probe.readEnable
        connectCompute(c.io.FromMatrixRegIO, c.io.ScaleA, c.io.SpadId, c.io.idle,
          c.io.ConfigInfo.MicroTaskReady, c.io.ConfigInfo.MicroTaskEndValid, c.io.LiveK)
      }
    }
    val generatedWriter = if (ExperimentalMxfpDynamicQuant) {
      val dyn = io.dynamic.get
      // These are the original ASL/BSL, not a test-side replacement writer.
      val (writerMmu, writerPort, writerBank, sink, end, ready) = if (isB) {
        val w = Module(new BScaleLoader)
        val c = w.io.ConfigInfo
        c.ApplicationScale_B := 0.U.asTypeOf(c.ApplicationScale_B)
        c.ApplicationScale_B.computeType := MteComputeType.Mxfp4F32
        c.ApplicationScale_B.ScaleBlocksPerRow := 2.U
        c.MatrixRegTensor_N := 8.U
        c.MatrixRegTensor_K := 1.U
        c.ScaleBankId := m.scaleBank
        c.Generated.get := true.B
        c.Conherent := true.B
        c.MicroTaskValid := io.start && dyn.enable
        c.MicroTaskEndReady := dyn.scaleEndReady
        w.io.DebugInfo.DebugTimeStampe := 0.U
        (w.io.LocalMMUIO, w.io.ToMatrixRegIO, w.io.SpadId,
          w.io.GeneratedScale.get, c.MicroTaskEndValid, c.MicroTaskReady)
      } else {
        val w = Module(new AScaleLoader)
        val c = w.io.ConfigInfo
        c.ApplicationScale_A := 0.U.asTypeOf(c.ApplicationScale_A)
        c.ApplicationScale_A.computeType := MteComputeType.Mxfp4F32
        c.ApplicationScale_A.ScaleBlocksPerRow := 2.U
        c.MatrixRegTensor_M := 8.U
        c.MatrixRegTensor_K := 1.U
        c.ScaleBankId := m.scaleBank
        c.Generated.get := true.B
        c.Conherent := true.B
        c.MicroTaskValid := io.start && dyn.enable
        c.MicroTaskEndReady := dyn.scaleEndReady
        w.io.DebugInfo.DebugTimeStampe := 0.U
        (w.io.LocalMMUIO, w.io.ToMatrixRegIO, w.io.SpadId,
          w.io.GeneratedScale.get, c.MicroTaskEndValid, c.MicroTaskReady)
      }
      writerMmu.Request.foreach(_.ready := true.B)
      writerMmu.Response.foreach { r =>
        r.valid := false.B
        r.bits := 0.U.asTypeOf(r.bits)
      }
      writerMmu.ConherentRequsetSourceID.valid := true.B
      writerMmu.ConherentRequsetSourceID.bits := 0.U
      writerMmu.nonConherentRequsetSourceID.valid := false.B
      writerMmu.nonConherentRequsetSourceID.bits := 0.U
      val source = generated.get
      sink.valid := source.valid && !dyn.stall
      sink.bits := source.bits
      source.ready := sink.ready && !dyn.stall
      dyn.generatedPending := source.valid
      dyn.generated.valid := source.fire
      dyn.generated.bits := source.bits
      dyn.scaleMemoryRequest := writerMmu.Request.map(_.valid).reduce(_ || _)
      dyn.scaleTaskReady := ready
      dyn.scaleEnd := end
      dyn.scaleWrite := writerPort.BankAddr.valid && writerPort.Data.valid
      Some((writerPort, writerBank))
    } else None
    for ((reg, bank) <- regs.zipWithIndex) {
      reg.io.FromScaleLoader.BankAddr.valid := m.scaleWrite && m.scaleWriteBank === bank.U
      reg.io.FromScaleLoader.BankAddr.bits := m.scaleWriteAddr
      reg.io.FromScaleLoader.Data.valid := m.scaleWrite && m.scaleWriteBank === bank.U
      reg.io.FromScaleLoader.Data.bits := m.scaleWriteWords.asUInt
        .asTypeOf(reg.io.FromScaleLoader.Data.bits)
      generatedWriter.foreach { case (writer, writerBank) =>
        when(writer.BankAddr.valid && writerBank === bank.U) {
          assert(!m.scaleWrite, "manual SF initialization overlapped actual scale writer")
          reg.io.FromScaleLoader.BankAddr := writer.BankAddr
          reg.io.FromScaleLoader.Data := writer.Data
        }
      }
      if (!concurrentScaleReads) {
        reg.io.FromScaleController.BankAddr.valid :=
          scaleActive && scaleBank === bank.U && port.BankAddr.valid
        reg.io.FromScaleController.BankAddr.bits := port.BankAddr.bits
        io.dynamic.foreach { dyn =>
          when(!scaleActive && dyn.read.valid && m.scaleBank === bank.U) {
            reg.io.FromScaleController.BankAddr.valid := true.B
            reg.io.FromScaleController.BankAddr.bits := dyn.read.bits
          }
        }
      }
    }
    if (!concurrentScaleReads) {
    // Reuse CUTETOP's request-fire bank latch for the real Valid-only SRAM
    // response. There is no replacement scale-memory model in this fixture.
    val readBank = io.dynamic.map(dyn =>
      Mux(!scaleActive && dyn.read.valid, m.scaleBank, scaleBank)).getOrElse(scaleBank)
    val sels = regs.indices.map(readBank === _.U)
    val ready = Mux1H(sels zip regs.map(_.io.FromScaleController.BankAddr.ready))
    port.BankAddr.ready := scaleActive && ready
    io.dynamic.foreach(_.read.ready := !scaleActive && ready)
    val inspectFire = io.dynamic.map(_.read.fire).getOrElse(false.B)
    val responseBank = RegEnable(readBank, 0.U, port.BankAddr.fire || inspectFire)
    val inspectResponse = RegEnable(inspectFire, false.B, port.BankAddr.fire || inspectFire)
    val responseSels = regs.indices.map(responseBank === _.U)
    val sfValid = Mux1H(responseSels zip regs.map(_.io.FromScaleController.Data.valid))
    val sfData = Mux1H(responseSels zip regs.map(_.io.FromScaleController.Data.bits))
    port.Data.valid := sfValid && !inspectResponse
    port.Data.bits := sfData
    io.dynamic.foreach { dyn =>
      dyn.data.valid := sfValid && inspectResponse
      dyn.data.bits := sfData.asUInt.asTypeOf(dyn.data.bits)
    }
    }
    m.selectedScaleBank := scaleBank
    m.scaleActive := scaleActive
    m.scaleRead.valid := port.BankAddr.fire
    m.scaleRead.bits := port.BankAddr.bits

    val mem = Module(new TrMatrixReg(if (isB) 2 else 0))
    val fill = mem.io.MatrixRegIO.FromMemoryLoader
    fill := matrix
    when(m.matrixInit) {
      fill.active := true.B
      for (bank <- 0 until TrMatrixRegNBanks) {
        fill.BankAddr(bank).valid := true.B
        fill.BankAddr(bank).bits := m.matrixInitAddr
        fill.Data(bank).valid := true.B
        fill.Data(bank).bits := m.matrixInitWords(bank).asUInt
        fill.ByteMask(bank).valid := true.B
        fill.ByteMask(bank).bits := Fill(TrMatrixRegEntryByteSize, true.B)
      }
    }
    val store = mem.io.MatrixRegIO.FromStore
    store.BankAddr.valid := false.B
    store.BankAddr.bits := 0.U.asTypeOf(store.BankAddr.bits)
    val read = mem.io.MatrixRegIO.FromDataController
    read.BankAddr.valid := m.read.valid
    read.BankAddr.bits := VecInit(Seq.fill(TrMatrixRegNBanks)(m.read.bits))
    m.read.ready := read.BankAddr.ready
    m.data.valid := read.Data.valid
    m.data.bits := read.Data.bits.asUInt.asTypeOf(m.data.bits)
  }

  val (config, memory, matrix, scale, scaleBank, scaleActive, generated, debug) = if (isB) {
    val loader = Module(new TrLoader1Legacy)
    (loader.io.ConfigInfo, loader.io.LocalMMUIO, loader.io.ToMatrixRegIO,
      loader.io.FromScaleRegIO, loader.io.ScaleBankId, loader.io.ScaleReadActive,
      loader.io.GeneratedScale, loader.io.DebugInfo)
  } else {
    val loader = Module(new TrLoader0Legacy(emitDifftest = false))
    (loader.io.ConfigInfo, loader.io.LocalMMUIO, loader.io.ToMatrixRegIO,
      loader.io.FromScaleRegIO, loader.io.ScaleBankId, loader.io.ScaleReadActive,
      loader.io.GeneratedScale, loader.io.DebugInfo)
  }
  debug.DebugTimeStampe := 0.U
  memory.ConherentRequsetSourceID.valid := true.B
  memory.ConherentRequsetSourceID.bits := io.sourceId
  memory.nonConherentRequsetSourceID.valid := false.B
  memory.nonConherentRequsetSourceID.bits := 0.U
  connectPorts(memory, matrix, scale, scaleBank, scaleActive, generated)
  for (channel <- 1 until TrMatrixRegNBanks) {
    memory.Request(channel).ready := true.B
    memory.Response(channel).valid := false.B
    memory.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
  }

  config.Tensor.baseVaddr := io.base
  config.Tensor.stride := io.stride
  config.Tensor.dataType := io.dataType
  config.Tensor.HasTail := io.hasTail
  config.Tensor.TailByteMask := io.tailBytes
  config.Tensor.K_Beat_Count := io.beatsPerRow
  config.LoadTaskInfo.Is_ZeroLoad := false.B
  config.LoadTaskInfo.Is_RepeatRowLoad := false.B
  config.LoadTaskInfo.Is_FullLoad := true.B
  config.MatrixRegTensor_M := io.sourceRows
  config.MatrixRegTensor_K := Mux(io.mx.computeType === MteComputeType.Mxfp4F32, 1.U, 2.U)
  config.MatrixRegId := 0.U
  config.PrefetchTaskId.foreach(_ := 0.U)
  config.PrefetchStream.foreach(_ := 0.U)
  config.Conherent := true.B
  config.Is_Transpose := false.B
  config.ConvertF32ToI8 := false.B
  config.ConvertF32ToMx := true.B
  config.ConvertComputeType := io.mx.computeType
  config.ConvertKElements := 64.U
  config.ConvertScaleBankId := io.mx.scaleBank
  config.ConvertDynamic.foreach(_ := io.dynamic.get.enable)
  config.MicroTaskValid := io.start
  config.MicroTaskEndReady := io.mx.endReady
  config.pc.foreach(_ := 0.U)
  config.coreid.foreach(_ := 0.U)
  config.retireId.foreach(_ := 0.U)
  config.epoch.foreach(_ := 0.U)
  io.mx.flags := config.MicroTaskFlags
  io.taskReady := config.MicroTaskReady
  io.taskEnd := config.MicroTaskEndValid
}

class MxTrLoaderSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TR converting loads"

  private val responseBytes = 64

  private def pokeWords(words: Seq[UInt], data: BigInt): Unit =
    words.zipWithIndex.foreach { case (word, index) =>
      val width = word.getWidth
      word.poke(((data >> (index * width)) & ((BigInt(1) << width) - 1)).U)
    }

  private def pokeResponseData(dut: MxTrLoaderHarness, data: BigInt): Unit =
    pokeWords(dut.io.response.bits.ReseponseData.toSeq, data)

  private def peekWords(words: Seq[UInt]): BigInt =
    words.zipWithIndex.foldLeft(BigInt(0)) {
      case (data, (word, index)) => data | (word.peek().litValue << (index * word.getWidth))
    }

  private def matrixData(dut: MxTrLoaderHarness, bank: Int): BigInt =
    peekWords(dut.io.matrix.Data(bank).bits.toSeq)

  private def observeMatrixWrites(
    dut: MxTrLoaderHarness,
    banks: Int,
    entries: Int,
    bytes: Int,
    actual: mutable.Map[(Int, Int, Int), Int]
  ): Boolean = {
    var wrote = false
    for (bank <- 0 until banks) {
      val addrValid = dut.io.matrix.BankAddr(bank).valid.peek().litToBoolean
      val dataValid = dut.io.matrix.Data(bank).valid.peek().litToBoolean
      val maskValid = dut.io.matrix.ByteMask(bank).valid.peek().litToBoolean
      assert(addrValid == dataValid && dataValid == maskValid,
        s"bank $bank emitted mismatched write-valid signals")
      if (addrValid) {
        val entry = dut.io.matrix.BankAddr(bank).bits.peek().litValue.toInt
        val mask = dut.io.matrix.ByteMask(bank).bits.peek().litValue
        val data = matrixData(dut, bank)
        assert(entry >= 0 && entry < entries, s"bank $bank wrote out-of-range entry $entry")
        for (byteOffset <- 0 until bytes if ((mask >> byteOffset) & 1) != 0) {
          val key = (bank, entry, byteOffset)
          val value = ((data >> (byteOffset * 8)) & 0xff).toInt
          assert(!actual.contains(key), s"duplicate MatrixReg byte write at $key")
          actual(key) = value
        }
        wrote = true
      }
    }
    wrote
  }

  private def initHarness(dut: MxTrLoaderHarness): Unit = {
    dut.io.start.poke(false.B)
    dut.io.dataType.poke(ElementDataType.DataTypeWidth8)
    dut.io.sourceRows.poke(0.U)
    dut.io.beatsPerRow.poke(0.U)
    dut.io.base.poke(0.U)
    dut.io.stride.poke(0.U)
    dut.io.hasTail.poke(false.B)
    dut.io.tailBytes.poke(0.U)
    dut.io.sourceId.poke(0.U)
    dut.io.request.ready.poke(true.B)
    dut.io.response.valid.poke(false.B)
    dut.io.response.bits.ReseponseConherent.poke(true.B)
    dut.io.response.bits.ReseponseSourceID.poke(0.U)
    pokeResponseData(dut, BigInt(0))
    locally {
      val m = dut.io.mx
      m.computeType.poke(MteComputeType.Mxfp4F32)
      m.scaleBank.poke(0.U)
      m.endReady.poke(false.B)
      m.scaleWrite.poke(false.B)
      m.scaleWriteBank.poke(0.U)
      m.scaleWriteAddr.poke(0.U)
      m.scaleWriteWords.foreach(_.poke(0.U))
      m.matrixInit.poke(false.B)
      m.matrixInitAddr.poke(0.U)
      m.matrixInitWords.foreach(_.foreach(_.poke(0.U)))
      m.read.valid.poke(false.B)
      m.read.bits.poke(0.U)
    }
    dut.io.computeScale.foreach { c =>
      c.start.poke(false.B); c.go.poke(false.B); c.readEnable.poke(false.B)
      c.bank.poke(0.U); c.rows.poke(0.U); c.endReady.poke(false.B)
    }
    dut.io.dynamic.foreach { dyn =>
      dyn.enable.poke(false.B)
      dyn.stall.poke(false.B)
      dyn.scaleEndReady.poke(false.B)
      dyn.read.valid.poke(false.B)
      dyn.read.bits.poke(0.U)
    }
  }

  private case class MxLoadScenario(name: String, rows: Int, offset: Int,
      stride: Int, reject: Option[String] = None, dynamic: Boolean = false,
      rejectIndex: Int = 0, zeroIndex: Int = -1)

  private def mxParams(lanes: Int, owners: Int = 4,
      dynamic: Boolean = false): Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_2Tops_64SCP.copy(
      EnableDifftest = false, ExperimentalMxfpConvertLoad = true,
      ExperimentalMxfpDynamicQuant = dynamic,
      MxfpConvertLanes = lanes,
      MxfpConvertOwners = owners,
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
  })

  private def mxComputeType(format: Int): UInt = format match {
    case 0 => MteComputeType.Mxfp4F32
    case 1 => MteComputeType.Mxfp8e4m3F32
    case 2 => MteComputeType.Mxfp8e5m2F32
  }

  private def mxScale(row: Int, block: Int, bank: Int, tag: Int): Int =
    124 + (row % 5) * 2 + block * 3 + bank * 5 + tag % 2

  // Exact representable centers and their known codes, not a replacement
  // quantizer/rounder. The frozen primitive suites own rounding-edge oracles.
  private def mxExactLane(format: Int, row: Int, block: Int, lane: Int,
      scale: Int, tag: Int): (Int, Int) = {
    val index = (row * 3 + block * 5 + lane + tag) % 8
    val negative = ((row + block + lane / 7 + tag) & 1) != 0
    val values = if (format == 0) Seq(0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0)
      else Seq(0.0, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0)
    val codes = if (format == 0) (0 until 8)
      else if (format == 1) Seq(0, 0x30, 0x38, 0x3c, 0x40, 0x44, 0x48, 0x4c)
      else Seq(0, 0x38, 0x3c, 0x3e, 0x40, 0x42, 0x44, 0x46)
    val signed = if (negative) -values(index) else values(index)
    val value = Math.scalb(signed, scale - (if (format == 0) 128 else 127)).toFloat
    val sign = if (negative && (format != 0 || index != 0)) {
      if (format == 0) 8 else 128
    } else 0
    (java.lang.Float.floatToRawIntBits(value), codes(index) | sign)
  }

  private def mxSentinel(bank: Int, entry: Int, byte: Int): Int =
    (0xa5 + bank * 37 + entry * 11 + byte * 3) & 0xff

  // Test-side memory service controls, not a model of an actual LLC. A Get
  // accepted in cycle t first becomes offerable at t + responseLatencyCycles.
  // Replies are FIFO, at most one 64B beat per responseIntervalCycles after
  // the preceding reply fires; a DUT stall retains the offered source/data.
  private case class MxLoadPerfModel(responseLatencyCycles: Int,
      responseIntervalCycles: Int) {
    require(responseLatencyCycles > 0 && responseIntervalCycles > 0)
  }

  private case class MxLoadStats(taskCycles: Int, sfPreloadCycles: Int,
      sfPreloadBytes: Int, requestedBytes: Int, returnedBytes: Int,
      writtenBytes: Int, k32Blocks: Int, scaleReadCount: Int, endFlags: Int,
      checkedSramBytes: Int, firstRequest: Int, lastRequest: Int,
      firstInput: Int, lastInput: Int, firstOutput: Int, lastOutput: Int,
      endValidCycle: Int, responseStallCycles: Int, requestAddresses: Vector[Int])

  private def runMxLoad(dut: MxTrLoaderHarness, params: Parameters,
      format: Int, scenario: MxLoadScenario, tag: Int,
      perf: Option[MxLoadPerfModel] = None, concurrentScale: Boolean = false): MxLoadStats = {
    val m = dut.io.mx
    val geometry = params(CuteParamsKey)
    val dynamic = scenario.dynamic
    require(concurrentScale == dut.io.computeScale.nonEmpty)
    require(!concurrentScale || (!dynamic && format == 0 && perf.isEmpty))
    require(!dynamic || (geometry.ExperimentalMxfpDynamicQuant && format == 0 &&
      scenario.rows == 8 && scenario.rejectIndex >= 0 && scenario.rejectIndex < 16))
    perf.foreach { _ =>
      require(!dynamic && format == 0 && scenario.rows == 64 && scenario.reject.isEmpty)
      require(scenario.offset % responseBytes == 0 && scenario.stride == 128 * 4)
    }
    val banks = geometry.TrMatrixRegNBanks
    val bytes = geometry.TrMatrixRegEntryByteSize
    val entries = geometry.TrMatrixRegBankNEntries
    val base = 0x4000 + scenario.offset
    val selectedBank = tag % geometry.ABScaleRegCount
    val blockBytes = if (format == 0) 16 else 32
    val codeBits = if (format == 0) 4 else 8
    val rejectedBlock = if (scenario.reject.contains("inf")) 0 else 1
    val rejectedLane = scenario.reject match {
      case Some("nan") => 0
      case Some("inf") => 31
      case _ => 17
    }
    val expectedFlags = if (scenario.reject.nonEmpty) 0x10 else 0
    val expectedWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val actualWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val sourceImage = mutable.Map.empty[Int, Int]
    def blockScale(row: Int, block: Int): Int =
      if (dynamic) {
        if (row * 2 + block == scenario.zeroIndex) 0 else 108 + row * 2 + block
      } else mxScale(row, block, selectedBank, tag)
    def blockRejected(row: Int, block: Int): Boolean = scenario.reject.nonEmpty &&
      (if (dynamic) row * 2 + block == scenario.rejectIndex
       else row == scenario.rows - 1 && block == rejectedBlock)
    for (row <- 0 until scenario.rows; block <- 0 until 2) {
      val e = blockScale(row, block)
      val rejected = blockRejected(row, block)
      val zero = dynamic && row * 2 + block == scenario.zeroIndex
      var payload = BigInt(0)
      for (lane <- 0 until 32) {
        val (centerBits, code) = mxExactLane(format, row, block, lane, e, tag)
        val bits = if (zero) {
          if ((lane & 1) == 0) 0 else Int.MinValue // Both signs of exact zero.
        } else if (rejected && lane == rejectedLane) {
          scenario.reject.get match {
            case "nan" => 0x7f800001 // one active sNaN; reject only this K32
            case "inf" => 0x7f800000
            case "domain" =>
              if (dynamic) ((e + 3) << 23) | 1 // Greater than every center; outside guard.
              else {
                val beyond = if (format == 0) 17.0 else if (format == 1) 512.0 else 65536.0
                java.lang.Float.floatToRawIntBits(
                  Math.scalb(beyond, e - (if (format == 0) 128 else 127)).toFloat)
              }
          }
        } else centerBits
        val address = base + row * scenario.stride + (block * 32 + lane) * 4
        for (byte <- 0 until 4) {
          assert(!sourceImage.contains(address + byte), "fixture rows overlap")
          sourceImage(address + byte) = (bits >>> (byte * 8)) & 0xff
        }
        payload |= BigInt(code) << (lane * codeBits)
      }
      if (rejected || zero) payload = BigInt(0)
      for (byte <- 0 until blockBytes) {
        val offset = block * blockBytes + byte
        val key = (row % banks, row / banks * geometry.ReduceGroupSize + offset / bytes, offset % bytes)
        assert(!expectedWrites.contains(key), s"MX golden mapping aliases $key")
        expectedWrites(key) = ((payload >> (byte * 8)) & 0xff).toInt
      }
    }
    val expectedAddresses = (for {
      row <- 0 until scenario.rows
      beat <- 0 until 4
      logical = base + row * scenario.stride + beat * responseBytes
      line <- (logical & -responseBytes) to ((logical + responseBytes - 1) & -responseBytes) by responseBytes
    } yield line).toSeq

    initHarness(dut)
    var readyCycles = 0
    while (!dut.io.taskReady.peek().litToBoolean && readyCycles < 64) {
      dut.clock.step()
      readyCycles += 1
    }
    assert(dut.io.taskReady.peek().litToBoolean, "MX loader did not become ready")
    // Initialize all physical bytes, including FP4's unused second row entry,
    // so a masked-write/late-write bug cannot hide behind uninitialized SRAM.
    m.matrixInit.poke(true.B)
    for (entry <- 0 until entries) {
      m.matrixInitAddr.poke(entry.U)
      for (bank <- 0 until banks) {
        val word = (0 until bytes).foldLeft(BigInt(0)) { (v, byte) =>
          v | (BigInt(mxSentinel(bank, entry, byte)) << (8 * byte))
        }
        pokeWords(m.matrixInitWords(bank).toSeq, word)
      }
      dut.clock.step()
    }
    m.matrixInit.poke(false.B)
    // Both actual SF banks have distinct row/block envelopes. Unused bytes
    // are poisoned; row32's two scale bytes cross the 64-byte SF boundary.
    // Perf only preloads the live lines of the selected real SF bank. This
    // direct SRAM preload is counted separately, not CPU scale preparation.
    var sfPreloadCycles = 0
    m.scaleWrite.poke(true.B)
    for (bank <- 0 until 2; line <- 0 until geometry.ABScaleBankNEntries) {
      if (perf.isEmpty || (bank == selectedBank &&
          line < (scenario.rows * 2 + responseBytes - 1) / responseBytes)) {
      m.scaleWriteBank.poke(bank.U)
      m.scaleWriteAddr.poke(line.U)
      val word = (0 until responseBytes).foldLeft(BigInt(0)) { (v, byte) =>
        val index = line * responseBytes + byte
        val e = if (!dynamic && index < scenario.rows * 2)
          mxScale(index / 2, index % 2, bank, tag) else 255
        v | (BigInt(e) << (8 * byte))
      }
      pokeWords(m.scaleWriteWords.toSeq, word)
      dut.clock.step()
      sfPreloadCycles += 1
      }
    }
    m.scaleWrite.poke(false.B)
    m.computeType.poke(mxComputeType(format))
    m.scaleBank.poke(selectedBank.U)
    dut.io.dynamic.foreach { dyn =>
      dyn.enable.poke(dynamic.B)
      dyn.stall.poke(dynamic.B)
    }
    dut.io.dataType.poke(ElementDataType.DataTypeWidth32)
    dut.io.sourceRows.poke(scenario.rows.U)
    dut.io.beatsPerRow.poke(4.U)
    dut.io.base.poke(base.U)
    dut.io.stride.poke(scenario.stride.U)
    // No End backpressure in perf: sample the first End and accept it on
    // that edge. Legacy stress still holds End for its original seven cycles.
    if (perf.nonEmpty) m.endReady.poke(true.B)
    dut.io.computeScale.foreach { c =>
      c.rows.poke(scenario.rows.U); c.bank.poke((1 - selectedBank).U)
      c.taskReady.expect(true.B); c.start.poke(true.B)
    }
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    dut.io.computeScale.foreach { c =>
      c.start.poke(false.B)
      c.bank.poke(selectedBank.U) // Invalid descriptor cycles are not ownership.
    }
    // Invalid-cycle descriptor changes must not redirect the owned task.
    m.scaleBank.poke((1 - selectedBank).U)
    m.computeType.poke(mxComputeType((format + 1) % 3))
    m.flags.expect(0.U)

    val pending = mutable.ArrayBuffer.empty[(Int, Int)]
    var active = Option.empty[(Int, Int)]
    val inFlight = mutable.Map.empty[Int, Int] // source -> actual request ordinal
    val observedAddresses = mutable.ArrayBuffer.empty[Int]
    val returnedOrder = mutable.ArrayBuffer.empty[Int]
    val scaleReads = mutable.ArrayBuffer.empty[Int]
    val responseDue = mutable.Map.empty[Int, Int]
    var nextResponseCycle = 0
    var firstRequest = -1
    var lastRequest = -1
    var firstInput = -1
    var lastInput = -1
    var firstOutput = -1
    var lastOutput = -1
    var endValidCycle = -1
    var endFlags = -1
    var responseStallCycles = 0
    val generatedScales = mutable.Map.empty[Int, (Int, Boolean)]
    var sfWrites = 0
    var sinkHeldCycles = 0
    var stalledScale = Option.empty[(Int, Int, Boolean)]
    var source = 0
    var cycles = 0
    var requestStalls = 0
    var stalledRequest = Option.empty[(BigInt, BigInt)]
    var computeTokens = 0
    var simultaneousReads = 0
    var heldCompute = Option.empty[(BigInt, Seq[BigInt])]
    var done = false
    while (!done && cycles < 12000) {
      dut.io.sourceId.poke(source.U)
      dut.io.dynamic.foreach { dyn =>
        // Hold the first real result seven cycles, then periodically stall
        // again while raw source responses and other owners remain live.
        dyn.stall.poke((dynamic && (sinkHeldCycles < 7 || cycles % 13 < 4)).B)
      }
      if (perf.nonEmpty) {
        dut.io.request.ready.poke(true.B)
        if (active.isEmpty && pending.nonEmpty &&
            cycles >= responseDue(pending.head._1) && cycles >= nextResponseCycle)
          active = Some(pending.remove(0))
      } else {
      dut.io.request.ready.poke((cycles % 5 != 1).B)
      // Reuse the existing containing-line response/word driver. Keep a
      // chosen response stable until fire; reverse queued owners, not bytes.
      if (active.isEmpty && pending.nonEmpty && cycles % 3 == 0 &&
          (pending.size >= 2 || cycles % 7 == 0 || observedAddresses.size == expectedAddresses.size))
        active = Some(pending.remove(pending.size - 1))
      }
      dut.io.response.valid.poke(active.nonEmpty.B)
      dut.io.response.bits.ReseponseConherent.poke(true.B)
      dut.io.response.bits.ReseponseSourceID.poke(active.map(_._1).getOrElse(0).U)
      val response = active.map { case (_, address) =>
        (0 until responseBytes).foldLeft(BigInt(0)) { (v, byte) =>
          val value = sourceImage.getOrElse(address + byte, (0xd3 + address + byte) & 0xff)
          v | (BigInt(value) << (8 * byte))
        }
      }.getOrElse(BigInt(0))
      pokeResponseData(dut, response)
      val beforeWriteCount = actualWrites.size
      observeMatrixWrites(dut, banks, entries, bytes, actualWrites)
      // Cycle zero is the accepted taskStart edge immediately before this
      // loop. A channel fire sampled now commits at the next edge (cycles+1).
      val stamp = cycles + 1
      if (actualWrites.size != beforeWriteCount) {
        if (firstOutput < 0) firstOutput = stamp
        lastOutput = stamp
      }
      m.selectedScaleBank.expect(selectedBank.U)
      m.scaleActive.expect((!dynamic).B)
      if (m.scaleRead.valid.peek().litToBoolean)
        scaleReads += m.scaleRead.bits.peek().litValue.toInt
      dut.io.computeScale.foreach { c =>
        val go = cycles % 7 >= 2
        c.go.poke(go.B)
        // Align ready compute requests with real converter reads; no SRAM
        // response or reader is modeled. Both must actually fire together.
        c.readEnable.poke(m.scaleRead.valid.peek().litToBoolean.B)
        c.selectedBank.expect((1 - selectedBank).U)
        if (c.requestFires.peek().litValue == 3) simultaneousReads += 1
        if (c.valid.peek().litToBoolean) {
          val tuple = (peekWords(c.words.toSeq), c.liveK.map(_.peek().litValue).toSeq)
          heldCompute.foreach(previous => assert(tuple == previous, "real compute SF token changed under stall"))
          val expected = (0 until geometry.Matrix_MN).foldLeft(BigInt(0)) { (v, lane) =>
            val row = computeTokens * geometry.Matrix_MN + lane
            c.liveK(lane).expect((if (row < scenario.rows) 64 else 0).U)
            val word = (0 until geometry.ScaleWidth / 8).foldLeft(BigInt(0)) { (w, byte) =>
              val e = if (row < scenario.rows && byte < 2)
                mxScale(row, byte, 1 - selectedBank, tag) else 127
              w | (BigInt(e) << (8 * byte))
            }
            v | (word << (lane * geometry.ScaleWidth))
          }
          assert(tuple._1 == expected, s"wrong real compute SF token $computeTokens on peer bank")
          heldCompute = if (go) None else Some(tuple)
          if (go) computeTokens += 1
        } else assert(heldCompute.isEmpty, "real compute SF valid disappeared under stall")
      }
      if (dynamic) {
        val dyn = dut.io.dynamic.get
        dyn.scaleMemoryRequest.expect(false.B)
        m.scaleRead.valid.expect(false.B)
        val generated = dyn.generated.valid.peek().litToBoolean
        val wrote = actualWrites.size != beforeWriteCount
        assert(wrote == generated, "actual TR write and generated SF handshake must be atomic")
        val index = dyn.generated.bits.row.peek().litValue.toInt * 2 +
          (if (dyn.generated.bits.block.peek().litToBoolean) 1 else 0)
        val e = dyn.generated.bits.e.peek().litValue.toInt
        val rejected = dyn.generated.bits.unsupported.peek().litToBoolean
        val pendingScale = (index, e, rejected)
        stalledScale.foreach(previous =>
          assert(dyn.generatedPending.peek().litToBoolean && pendingScale == previous,
            "generated SF owner/data changed under backpressure"))
        stalledScale = if (dyn.stall.peek().litToBoolean &&
            dyn.generatedPending.peek().litToBoolean) {
          sinkHeldCycles += 1
          assert(!generated && !wrote, "backpressured SF repeated a Valid-only TR write")
          Some(pendingScale)
        } else None
        if (generated) {
          assert(index >= 0 && index < 16 && !generatedScales.contains(index),
            s"lost or duplicated generated K32 index $index")
          val expectedReject = blockRejected(index / 2, index % 2)
          assert(rejected == expectedReject, s"wrong unsupported status for K32 $index")
          assert(e == (if (expectedReject) 0 else blockScale(index / 2, index % 2)),
            s"wrong generated scale for K32 $index: $e")
          assert(actualWrites.size == beforeWriteCount + 16,
            "each actual FP4 K32 write must touch exactly sixteen bytes")
          generatedScales(index) = ((e, rejected))
        }
        if (dyn.scaleWrite.peek().litToBoolean) {
          assert(generatedScales.size == 16 && actualWrites.size == 256,
            "SSCP write preceded the sixteen actual TR/scale handshakes")
          sfWrites += 1
          assert(sfWrites == 1, "generated sole scale writer repeated its SSCP write")
        }
        if (dyn.scaleEnd.peek().litToBoolean)
          assert(sfWrites == 1, "scale End preceded the actual sole-writer SSCP write")
      }

      val requestValid = dut.io.request.valid.peek().litToBoolean
      val requestReady = dut.io.request.ready.peek().litToBoolean
      val address = dut.io.request.bits.RequestAddr.peek().litValue
      val id = dut.io.request.bits.RequestSourceID.peek().litValue
      stalledRequest.foreach { previous =>
        assert(requestValid && previous == ((address, id)), "MX request changed while stalled")
      }
      stalledRequest = if (requestValid && !requestReady) {
        requestStalls += 1
        Some((address, id))
      } else None
      if (requestValid && requestReady) {
        if (firstRequest < 0) firstRequest = stamp
        lastRequest = stamp
        assert(address % responseBytes == 0, s"unaligned MX physical Get $address")
        assert(id == source && !inFlight.contains(source), s"MX reused in-flight source $source")
        assert(observedAddresses.size < expectedAddresses.size, "extra MX source request")
        assert(address == expectedAddresses(observedAddresses.size),
          s"MX physical request mismatch at ${observedAddresses.size}: $address")
        inFlight(source) = observedAddresses.size
        observedAddresses += address.toInt
        pending += ((source, address.toInt))
        perf.foreach(model => responseDue(source) = cycles + model.responseLatencyCycles)
        source = (1 to 64).map(i => (source + i) % 64)
          .find(id => !inFlight.contains(id)).getOrElse(sys.error("MX fixture exhausted source IDs"))
      }
      if (active.nonEmpty && !dut.io.response.ready.peek().litToBoolean)
        responseStallCycles += 1
      if (active.nonEmpty && dut.io.response.ready.peek().litToBoolean) {
        val id = active.get._1
        if (firstInput < 0) firstInput = stamp
        lastInput = stamp
        perf.foreach { model =>
          assert(cycles >= responseDue.remove(id).get, "MX response preceded modeled latency")
          nextResponseCycle = cycles + model.responseIntervalCycles
        }
        assert(inFlight.contains(id), s"MX response has no request owner $id")
        returnedOrder += inFlight.remove(id).get
        active = None
      }
      done = dut.io.taskEnd.peek().litToBoolean &&
        (!dynamic || dut.io.dynamic.get.scaleEnd.peek().litToBoolean)
      if (done) {
        endValidCycle = cycles
        endFlags = m.flags.peek().litValue.toInt
        assert(pending.isEmpty && active.isEmpty && inFlight.isEmpty,
          "MX End preceded outstanding source fragments")
        m.flags.expect(expectedFlags.U)
      }
      dut.clock.step()
      cycles += 1
    }
    assert(done, s"MX task timed out: format=$format ${scenario.name} rows=${scenario.rows}")
    dut.io.computeScale.foreach { c =>
      val expectedTokens = (scenario.rows + geometry.Matrix_MN - 1) / geometry.Matrix_MN
      assert(computeTokens == expectedTokens && simultaneousReads > 0 && heldCompute.isEmpty,
        s"missing real cross-bank work: tokens=$computeTokens/$expectedTokens simultaneous=$simultaneousReads")
      c.taskEnd.expect(true.B)
      c.go.poke(false.B); c.readEnable.poke(false.B)
      c.endReady.poke(true.B); dut.clock.step(); c.endReady.poke(false.B)
      dut.clock.step(); c.idle.expect(true.B); c.taskEnd.expect(false.B)
      println(s"MX_REAL_SCALE_OVERLAP bank=$selectedBank tokens=$computeTokens simultaneous=$simultaneousReads " +
        "actual_ASC_BSC_AML_BML_SRAM=PASS peer_scale_bytes_liveK_stalls_End=PASS")
    }
    assert(observedAddresses.toSeq == expectedAddresses)
    assert(returnedOrder.size == expectedAddresses.size)
    if (perf.isEmpty) {
    assert(returnedOrder.toSeq != returnedOrder.sorted.toSeq, "MX replies never exercised reordering")
    assert(requestStalls > 0, "MX source requests never exercised backpressure")
    } else {
      assert(returnedOrder.toSeq == returnedOrder.sorted.toSeq, "MX perf model reordered replies")
      assert(requestStalls == 0, "MX perf driver injected request backpressure")
      assert(responseDue.isEmpty && firstRequest > 0 && firstInput > firstRequest)
      assert(firstOutput > firstInput && lastOutput >= firstOutput)
      assert(endValidCycle > lastOutput, "MX End did not drain the final actual packed write")
    }
    if (dynamic) {
      assert(scaleReads.isEmpty && generatedScales.size == 16 && sfWrites == 1 && sinkHeldCycles >= 7)
    } else {
      assert(scaleReads.size == scenario.rows * 2, "MX scale reads lost/duplicated a K32 owner")
      assert(scaleReads.toSet == (0 to ((scenario.rows * 2 - 1) / responseBytes)).toSet)
    }
    assert(actualWrites == expectedWrites,
      s"MX byte writes mismatch: missing=${expectedWrites.keySet.diff(actualWrites.keySet).take(8)} extra=${actualWrites.keySet.diff(expectedWrites.keySet).take(8)}")
    dut.io.response.valid.poke(false.B)
    if (perf.isEmpty) {
    for (_ <- 0 until 7) {
      dut.io.taskEnd.expect(true.B)
      dut.io.taskReady.expect(false.B)
      m.flags.expect(expectedFlags.U)
      dut.io.request.valid.expect(false.B)
      m.scaleRead.valid.expect(false.B)
      if (dynamic) {
        val dyn = dut.io.dynamic.get
        dyn.scaleEnd.expect(true.B)
        dyn.scaleWrite.expect(false.B)
        dyn.generatedPending.expect(false.B)
        dyn.scaleMemoryRequest.expect(false.B)
      }
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX write after held End")
      dut.clock.step()
    }
    m.endReady.poke(true.B)
    dut.clock.step()
    m.endReady.poke(false.B)
    if (dynamic) {
      val dyn = dut.io.dynamic.get
      // The integration harness holds both physical End signals separately;
      // TC's same-owner atomic join/publication is checked in its own suite.
      for (_ <- 0 until 4) {
        dut.io.taskEnd.expect(false.B)
        dyn.scaleEnd.expect(true.B)
        dyn.scaleTaskReady.expect(false.B)
        dyn.scaleWrite.expect(false.B)
        assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites))
        dut.clock.step()
      }
      dyn.scaleEndReady.poke(true.B)
      dut.clock.step()
      dyn.scaleEndReady.poke(false.B)
    }
    for (_ <- 0 until 12) {
      dut.io.taskEnd.expect(false.B)
      dut.io.request.valid.expect(false.B)
      m.scaleActive.expect(false.B)
      m.scaleRead.valid.expect(false.B)
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX late write after End.fire")
      dut.clock.step()
    }
    } else {
      m.endReady.poke(false.B)
    }
    // Observe actual SRAM contents, not only the loader's write intentions.
    // Every inactive byte must retain its independently preloaded sentinel.
    for (entry <- 0 until entries) {
      m.read.valid.poke(true.B)
      m.read.bits.poke(entry.U)
      m.read.ready.expect(true.B)
      dut.clock.step()
      m.read.valid.poke(false.B)
      m.data.valid.expect(true.B)
      for (bank <- 0 until banks) {
        val word = peekWords(m.data.bits(bank).toSeq)
        for (byte <- 0 until bytes) {
          val expected = expectedWrites.getOrElse((bank, entry, byte), mxSentinel(bank, entry, byte))
          assert(((word >> (8 * byte)) & 0xff) == expected,
            s"MX SRAM mismatch bank=$bank entry=$entry byte=$byte format=$format ${scenario.name}")
        }
      }
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX write during final readback")
    }
    dut.clock.step()
    m.data.valid.expect(false.B)
    if (dynamic) {
      val dyn = dut.io.dynamic.get
      // Read the actual selected and unselected SSCP banks. The live sixteen
      // E8M0 bytes,127 padding, untouched lines and untouched bank are distinct.
      for (bank <- 0 until 2; line <- 0 until geometry.ABScaleBankNEntries) {
        m.scaleBank.poke(bank.U)
        dyn.read.valid.poke(true.B)
        dyn.read.bits.poke(line.U)
        dyn.read.ready.expect(true.B)
        dut.clock.step()
        dyn.read.valid.poke(false.B)
        dyn.data.valid.expect(true.B)
        val word = peekWords(dyn.data.bits.toSeq)
        for (byte <- 0 until responseBytes) {
          val expected = if (bank == selectedBank && line == 0) {
            if (byte >= 16) 127 else generatedScales(byte)._1
          } else 255
          assert(((word >> (8 * byte)) & 0xff) == expected,
            s"dynamic real SSCP mismatch bank=$bank line=$line byte=$byte ${scenario.name}")
        }
        dyn.scaleWrite.expect(false.B)
        dyn.scaleMemoryRequest.expect(false.B)
      }
      dut.clock.step()
      dyn.data.valid.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      dyn.scaleTaskReady.expect(true.B)
    }
    if (perf.isEmpty) {
    println(s"MX converting-load dynamic=$dynamic format=$format lanes=${geometry.MxfpConvertLanes} ${scenario.name} " +
      s"rows=${scenario.rows} requests=${observedAddresses.size} replies=${returnedOrder.size} " +
      s"packedBytes=${actualWrites.size} scaleReads=${scaleReads.size} requestStalls=$requestStalls " +
      s"flags=$expectedFlags heldEnd=7 lateWindow=12 SRAMbytes=${banks * entries * bytes} cycles=$cycles")
    }
    // Excludes SRAM initialization, SF preload, late-window stress and the
    // SRAM readback checker. Counts from accepted start.fire (origin zero)
    // through first End.fire, without charging the start edge a second time.
    MxLoadStats(cycles, sfPreloadCycles, sfPreloadCycles * responseBytes,
      observedAddresses.size * responseBytes, returnedOrder.size * responseBytes,
      actualWrites.size, actualWrites.size / blockBytes, scaleReads.size, endFlags,
      banks * entries * bytes, firstRequest, lastRequest, firstInput, lastInput,
      firstOutput, lastOutput, endValidCycle, responseStallCycles, observedAddresses.toVector)
  }

  // Same actual source CAM, raw owners, existing encoder, masked TR writes,
  // original ASL/BSL and real SSCP. No supplied-SF preload serves the dynamic
  // tasks; it only poisons physical memory to make padding/isolation observable.
  private def resetDynamicPending(dut: MxTrLoaderHarness): Unit = {
    initHarness(dut)
    val m = dut.io.mx
    val dyn = dut.io.dynamic.get
    dut.io.taskReady.expect(true.B)
    dyn.scaleTaskReady.expect(true.B)
    dyn.enable.poke(true.B)
    dyn.stall.poke(true.B)
    m.computeType.poke(MteComputeType.Mxfp4F32)
    m.scaleBank.poke(1.U)
    dut.io.dataType.poke(ElementDataType.DataTypeWidth32)
    dut.io.sourceRows.poke(8.U)
    dut.io.beatsPerRow.poke(4.U)
    dut.io.base.poke(0x9000.U)
    dut.io.stride.poke(256.U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    val pending = mutable.Queue.empty[Int]
    var active = Option.empty[Int]
    var source = 0
    var cycles = 0
    val beat = (0 until 16).foldLeft(BigInt(0)) { (word, lane) =>
      word | (BigInt(0x3fc00000) << (32 * lane)) // Exactly1.5, guarded max.
    }
    while (!dyn.generatedPending.peek().litToBoolean && cycles < 1000) {
      dut.io.sourceId.poke(source.U)
      if (active.isEmpty && pending.nonEmpty) active = Some(pending.dequeue())
      dut.io.response.valid.poke(active.nonEmpty.B)
      dut.io.response.bits.ReseponseSourceID.poke(active.getOrElse(0).U)
      pokeResponseData(dut, beat)
      if (dut.io.request.valid.peek().litToBoolean) {
        pending.enqueue(source)
        source = (source + 1) % 64
      }
      if (active.nonEmpty && dut.io.response.ready.peek().litToBoolean) active = None
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dyn.scaleWrite.expect(false.B)
      dyn.scaleMemoryRequest.expect(false.B)
      dut.clock.step()
      cycles += 1
    }
    dyn.generatedPending.expect(true.B)
    dut.io.response.valid.poke(false.B)
    dut.io.request.ready.poke(false.B)
    for (_ <- 0 until 7) {
      dyn.generatedPending.expect(true.B)
      dyn.generated.valid.expect(false.B)
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dyn.scaleWrite.expect(false.B)
      dut.io.taskEnd.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      dut.clock.step()
    }
    // A global fixture reset cancels both producer and memory service. This
    // does not claim precise late-fragment-fault recovery: old replies are
    // discarded with the service, and the whole row is reloaded afterwards.
    dut.reset.poke(true.B)
    initHarness(dut)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    for (_ <- 0 until 7) {
      dyn.generatedPending.expect(false.B)
      dyn.generated.valid.expect(false.B)
      dyn.scaleWrite.expect(false.B)
      dyn.scaleMemoryRequest.expect(false.B)
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dut.io.request.valid.expect(false.B)
      dut.io.taskEnd.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      m.flags.expect(0.U)
      dut.clock.step()
    }
    dut.io.taskReady.expect(true.B)
    dyn.scaleTaskReady.expect(true.B)
  }

  for (isB <- Seq(false, true); lanes <- Seq(8, 32)) {
    it should s"dynamic MX4 ${if (isB) "TL1" else "TL0"} TR SSCP atomic16 lanes$lanes recover unsupported" in {
      val params = mxParams(lanes, dynamic = true)
      test(new MxTrLoaderHarness(isB)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(100000)
          initHarness(dut)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          val scenarios = Seq(
            MxLoadScenario("dynamic16-distinct-e", 8, 0, 256, dynamic = true),
            MxLoadScenario("dynamic16-reject0", 8, 61, 283,
              Some("inf"), dynamic = true, rejectIndex = 0),
            MxLoadScenario("dynamic16-reject7", 8, 8, 265,
              Some("nan"), dynamic = true, rejectIndex = 7),
            MxLoadScenario("dynamic16-reject15", 8, 63, 269,
              Some("domain"), dynamic = true, rejectIndex = 15),
            MxLoadScenario("dynamic16-recover-zero", 8, 0, 256,
              dynamic = true, zeroIndex = 3))
          // No reset between accepted/rejected/accepted tasks: old raw,
          // staging, unsupported and matrix NV must not leak into the retry.
          for ((scenario, tag) <- scenarios.zipWithIndex) {
            val stats = runMxLoad(dut, params, format = 0, scenario = scenario, tag = tag)
            assert(stats.writtenBytes == 256 && stats.k32Blocks == 16 &&
              stats.scaleReadCount == 0 &&
              stats.endFlags == (if (scenario.reject.nonEmpty) 16 else 0))
          }
          for (format <- 0 until 3) {
            // Same parameter-on hardware switches back to the original
            // supplied SF reader and all three original encoder formats.
            runMxLoad(dut, params, format,
              MxLoadScenario("supplied-after-dynamic", 5, 8, 265), tag = format)
          }
          resetDynamicPending(dut)
          runMxLoad(dut, params, format = 0,
            scenario = MxLoadScenario("dynamic-after-reset", 8, 0, 256, dynamic = true), tag = 5)
        }
    }
  }

  for (isB <- Seq(false, true)) {
    it should s"R47 overlap real ${if (isB) "BSC and TL1" else "ASC and TL0"} peer-bank readers" in {
      val params = mxParams(8)
      test(new MxTrLoaderHarness(isB, concurrentScaleReads = true)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(250000)
          initHarness(dut); dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
          for (bank <- 0 until 4) {
            runMxLoad(dut, params, format = 0,
              scenario = MxLoadScenario("R47-peer-bank-SF-line-crossing", 33, 61, 283),
              tag = bank, concurrentScale = true)
          }
        }
    }
  }

  for (isB <- Seq(false, true); lanes <- Seq(8, 32)) {
    val description = s"convert supplied-scale MX ${if (isB) "TL1" else "TL0"} through real 2T64 SRAM lanes$lanes"
    it should description in {
      val params = mxParams(lanes)
      test(new MxTrLoaderHarness(isB)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(250000)
          initHarness(dut)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          val scenarios = Seq(
            MxLoadScenario("aligned", 5, 0, 256),
            MxLoadScenario("split", 5, 61, 283),
            MxLoadScenario("nan-K32", 5, 8, 265, Some("nan")),
            MxLoadScenario("inf-K32", 5, 63, 269, Some("inf")),
            MxLoadScenario("domain-K32", 5, 4, 277, Some("domain")),
            MxLoadScenario("cross-SF-recovery", 33, 63, 269))
          for (format <- 0 until 3; (scenario, index) <- scenarios.zipWithIndex)
            runMxLoad(dut, params, format, scenario, format * 11 + index)
        }
    }
  }

}
