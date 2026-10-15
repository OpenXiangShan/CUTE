package cute

import chisel3._
import circt.stage.ChiselStage
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
    val write = Input(Vec(2, new difftest.DiffAmuFinishEvent(2, 2)))
  })
  if (async) CuteDifftest.publishSinkClock("concurrent_hash_test", clock, reset.asBool)
  withClockAndReset(io.acceleratorClock, io.acceleratorReset) {
    val writes = CuteDifftest.matrixHash(2, 16, 128, 16, ports = 2)
    writes := io.write
  }
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
}
