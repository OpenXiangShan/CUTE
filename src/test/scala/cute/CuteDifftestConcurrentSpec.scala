package cute

import chisel3._
import circt.stage.ChiselStage
import difftest.{DiffAmuWritebackEvent, DifftestModule}
import difftest.util.BundleProfile
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

class CuteDifftestConcurrentHarness(async: Boolean) extends Module {
  implicit val p: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.baseParams.copy(
      EnableDifftest = true,
      DifftestAsyncClockName = Option.when(async)("concurrent_hash_test"))
  })
  val io = IO(new Bundle {
    val acceleratorClock = Input(Clock())
    val acceleratorReset = Input(Bool())
    val write = Input(Vec(2, new difftest.DiffAmuWritebackEvent(2, 2)))
  })
  if (async) CuteDifftest.publishSinkClock("concurrent_hash_test", clock, reset.asBool)
  withClockAndReset(io.acceleratorClock, io.acceleratorReset) {
    val writes = CuteDifftest.matrixHash(2, 16, 128, 16, ports = 2)
    writes := io.write
  }
}

class CuteDifftestMaskHarness(rawLoad: Int, async: Boolean) extends Module {
  implicit val p: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.baseParams.copy(
      EnableDifftest = true,
      DifftestAsyncClockName = Option.when(async)(s"writeback_mask_test_$rawLoad"))
  })
  if (async) CuteDifftest.publishSinkClock(s"writeback_mask_test_$rawLoad", clock, reset.asBool)
  val probes = MatrixRegWriteProbes.Params(
    aLoad = if (rawLoad == 1 || rawLoad == 4) MatrixRegWriteProbes.abLoadNoHash else MatrixRegWriteProbes.abLoadHash,
    bLoad = if (rawLoad == 2 || rawLoad == 4) MatrixRegWriteProbes.abLoadNoHash else MatrixRegWriteProbes.abLoadHash,
    cLoad = if (rawLoad == 3 || rawLoad == 4) MatrixRegWriteProbes.cLoadNoHash else MatrixRegWriteProbes.cLoadHash
  )
  assert(probes.writebackHasMask == (rawLoad != 0))
  val writes = MatrixRegWriteProbes.genWriteProbes(MatrixRegWriteProbes.mma)(probes.parameters(probes.mma))
  assert(writes.head.bankMask.nonEmpty, "Local write probes must retain their masks")
  writes.head := 0.U.asTypeOf(writes.head)
}

class CuteDifftestConcurrentSpec extends AnyFlatSpec {
  behavior of "concurrent hash event transport"
  Seq(false, true).foreach { async =>
    it should s"elaborate two accelerator-domain write ports with async=$async" in {
      val chirrtl = ChiselStage.emitCHIRRTL(new CuteDifftestConcurrentHarness(async))
      assert(chirrtl.contains("MatrixHash"))
      assert(chirrtl.contains("AsyncQueueSource") == async)
      assert(chirrtl.contains("AsyncQueueSink") == async)
    }
  }

  for (async <- Seq(false, true); rawLoad <- 0 to 4) {
    it should s"select one global mask layout with rawLoad=$rawLoad and async=$async" in {
      val previous = DifftestModule.get_current_interfaces().size
      ChiselStage.emitCHIRRTL(new CuteDifftestMaskHarness(rawLoad, async))
      val events = DifftestModule.get_current_interfaces().drop(previous).map(_._1)
      val writeback = events.collect { case event: DiffAmuWritebackEvent => event }
      assert(writeback.size == 1)
      val event = writeback.head
      assert(event.bankValid.getWidth == event.nBanks)
      assert(!event.toCppDeclaration(packed = true, aligned = false).contains("bankValid["))
      assert(event.bankMask.isDefined == (rawLoad != 0))
      assert(event.elements.contains("bankMask") == (rawLoad != 0))
      val replay = BundleProfile.fromBundle(event, 0).toBundle.asInstanceOf[DiffAmuWritebackEvent]
      assert(replay.hasMask == event.hasMask)
      assert(replay.getWidth == event.getWidth)
    }
  }
}
