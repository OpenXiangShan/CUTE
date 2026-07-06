package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SameSendBankProbeSpec extends AnyFlatSpec with ChiselScalatestTester {
  it should "probe progress" in {
    test(new OmegaRouterTestTop(sameSendBankMode = true, totalRequests = 256))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.clock.step(10)
      println(s"After 10 steps: done=${dut.io.done.peek().litToBoolean}, error=${dut.io.error.peek().litToBoolean}")
      dut.clock.step(90)
      println(s"After 100 steps: done=${dut.io.done.peek().litToBoolean}, error=${dut.io.error.peek().litToBoolean}")
    }
  }
}
