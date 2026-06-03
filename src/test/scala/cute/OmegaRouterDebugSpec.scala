package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OmegaRouterDebugSpec extends AnyFlatSpec with ChiselScalatestTester {
  it should "step 10 cycles without hanging" in {
    test(new OmegaRouterTestTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.step(10)
      println("Successfully stepped 10 cycles with Verilator")
    }
  }
}
