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

/** TaskController + both unmodified normal paths + AML transpose trait +
  * production response bridges. No CPU, cache, FPE, or full-core build.
  */
class TransposeLoaderIntegrationHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new Bundles.AmuLsuIO))
    val memory = Flipped(Vec(2, new LocalMMUIO))
    val target = Flipped(Vec(2, new ABMemoryLoaderMatrixRegIO))
    val targetId = Output(Vec(2, UInt(ABMatrixRegIdWidth.W)))
    val dispatch = Output(Vec(2, Bool()))
    val completed = Output(Vec(2, Bool()))
  })
  val tc = Module(new TaskController)
  val a = Module(new AMLWrapper)
  val b = Module(new BMLWrapper)
  tc.io.DebugTimeStampe := 0.U
  tc.io.ygjkctrl.reset := false.B
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
  tc.io.CML_MicroTask_Config.LoadMicroTaskReady := true.B
  tc.io.CML_MicroTask_Config.StoreMicroTaskReady := true.B
  tc.io.CML_MicroTask_Config.LoadMicroTaskEndValid := false.B
  tc.io.CML_MicroTask_Config.StoreMicroTaskEndValid := false.B
  a.io.ConfigInfo <> tc.io.AML_MicroTask_Config
  b.io.ConfigInfo <> tc.io.BML_MicroTask_Config
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
    if (AMLUseLegacyLoader) {
      val arb = Module(new Arbiter(new MMUResponseIO, ABMatrixRegNBanks))
      arb.io.in <> io.memory(i).Response
      mem.Response(0) <> arb.io.out
      for (lane <- 1 until ABMatrixRegNBanks) {
        mem.Response(lane).valid := false.B
        mem.Response(lane).bits := 0.U.asTypeOf(new MMUResponseIO)
      }
    } else {
      val bridge = Module(new ResponseChannelBridge(
        inputChannelCount = ABMatrixRegNBanks, respChannelCount = AMLResponseChannelCount,
        bankCount = ABMatrixRegNBanks, queueDepth = ResponseBridgeQueueDepth,
        dataWidth = outsideDataWidth, sourceIdWidth = 64,
        bankIdWidth = log2Ceil(ABMatrixRegNBanks), bankIdOffset = log2Ceil(ABMatrixRegBankNEntries),
        contextName = s"Test$i"))
      bridge.io.timeStamp := 0.U
      bridge.io.in <> io.memory(i).Response
      mem.Response <> bridge.io.out
    }
  }
}

class TransposeLoaderIntegrationSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "shared AML transpose integration"
  for (banks <- Seq(4, 8); mode <- Seq("L", "1", "2", "4", "8") if mode != "8" || banks == 8) {
    it should s"run mlat/mlbt and preserve ordinary A/B loads for $banks banks mode $mode" in {
      ChiselDB.init(false)
      Constantin.init(false)
      test(new TransposeLoaderIntegrationHarness()(TransposeTestParams(banks, mode)))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
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
            val host = if (trans || !isB) 0 else 1
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
                    val alloc = req.bits.UseAllocatedSourceID.peek().litToBoolean
                    val id = if (alloc) BigInt(ids(h)) else req.bits.RequestSourceID.peek().litValue
                    pending(h) += Response(id, req.bits.RequestAddr.peek().litValue.toLong)
                    if (alloc) ids(h) = (ids(h) + 1) % 64
                  }
                  if (offered(h)(lane).nonEmpty && dut.io.memory(h).Response(lane).ready.peek().litToBoolean)
                    offered(h)(lane) = None
                  val target = dut.io.target(h)
                  if (target.BankAddr(lane).valid.peek().litToBoolean) {
                    assert(h == host)
                    dut.io.targetId(h).expect(reg.U)
                    val address = target.BankAddr(lane).bits.peek().litValue.toInt
                    val mask = target.ByteMask(lane).bits.peek().litValue
                    val data = target.Data(lane).bits.peek().litValue
                    for (by <- 0 until 32 if mask.testBit(by)) {
                      val key = (lane, address, by)
                      assert(!got.contains(key), s"duplicate byte $key")
                      got(key) = ((data >> (8 * by)) & 255).toInt
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
  it should "serialize mlat/mlbt on AML while a normal mlb progresses on BML" in {
    ChiselDB.init(false)
    Constantin.init(false)
    test(new TransposeLoaderIntegrationHarness()(TransposeTestParams(8, "8")))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        // The first two destinations share AML; the third can issue on BML
        // while AML's first task is waiting for responses.
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
              if (dut.io.target(h).BankAddr(lane).valid.peek().litToBoolean) {
                val id = dut.io.targetId(h).peek().litValue.toInt
                assert(if (h == 0) id == finishes(0) else id == 2)
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
