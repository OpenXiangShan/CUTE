package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Smoke test: ML -> L2 -> ML direct connect (no router).
  * addrBank == sendBank, so responses return on the same channel.
  */
class SmokeTestSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SmokeTestTop"

  it should "pass requests without loss or duplication" in {
    test(new SmokeTestTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      var cycles = 0
      val maxCycles = 100000

      while (!dut.io.done.peek().litToBoolean && !dut.io.error.peek().litToBoolean && cycles < maxCycles) {
        dut.clock.step()
        cycles += 1
      }

      if (dut.io.error.peek().litToBoolean) {
        val bank = dut.io.errorBank.peek().litValue
        val reg = dut.io.errorRegAddr.peek().litValue
        println(s"ERROR at cycle $cycles: excess response bank=$bank regAddr=$reg")
      }

      dut.io.error.expect(false.B, s"Detected error after $cycles cycles")
      dut.io.done.expect(true.B, s"Test did not finish within $maxCycles cycles (actual=$cycles)")
    }
  }
}

/**
  * Router test: ML -> L2 -> OmegaResponseRouter -> ML.
  * addrBank is randomized w.r.t sendBank, forcing the router to reroute.
  */
class OmegaRouterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "OmegaResponseRouter"

  it should "route all responses correctly without loss or duplication" in {
    test(new OmegaRouterTestTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      var cycles = 0
      val maxCycles = 200000

      while (!dut.io.done.peek().litToBoolean && !dut.io.error.peek().litToBoolean && cycles < maxCycles) {
        dut.clock.step()
        cycles += 1
      }

      if (dut.io.error.peek().litToBoolean) {
        val bank = dut.io.errorBank.peek().litValue
        val reg = dut.io.errorRegAddr.peek().litValue
        println(s"ERROR at cycle $cycles: excess response bank=$bank regAddr=$reg")
      }

      dut.io.error.expect(false.B, s"Detected error after $cycles cycles")
      dut.io.done.expect(true.B, s"Test did not finish within $maxCycles cycles (actual=$cycles)")
    }
  }
}

/**
  * Same-send-bank stress test: all requests claim the same sendBank (0),
  * but addrBank is randomized, forcing L2 to return responses on different
  * channels while the router must route them all back to a single output.
  *
  * Enter this scenario via:
  *   mill CUTE.test.testOnly cute.SameSendBankRouterSpec
  */
class SameSendBankRouterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "OmegaResponseRouter (sameSendBankMode)"

  it should "route concentrated responses to the same bank without loss or duplication" in {
    test(new SameSendBankRouterTestTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      dut.clock.setTimeout(0)

      var cycles = 0
      val maxCycles = 200000

      while (!dut.io.done.peek().litToBoolean && !dut.io.error.peek().litToBoolean && cycles < maxCycles) {
        dut.clock.step()
        cycles += 1
      }

      if (dut.io.error.peek().litToBoolean) {
        val bank = dut.io.errorBank.peek().litValue
        val reg = dut.io.errorRegAddr.peek().litValue
        println(s"ERROR at cycle $cycles: excess response bank=$bank regAddr=$reg")
      }

      dut.io.error.expect(false.B, s"Detected error after $cycles cycles")
      dut.io.done.expect(true.B, s"Test did not finish within $maxCycles cycles (actual=$cycles)")
    }
  }
}

/**
  * OmegaSwitch blocking + round-robin test top.
  *
  * All inputs target the same output (bankId=0).  Each input keeps valid high
  * until it fires, then drops it.  Outputs are always ready.
  *
  * The router should schedule inputs one-by-one via in0-priority arbitration.
  */
class OmegaSwitchBlockTop extends Module {
  val n = 8
  val dataWidth = 512
  val sourceIdWidth = 64
  val bankIdWidth = 3
  val regAddrWidth = 8

  val io = IO(new Bundle {
    val inReady  = Output(Vec(n, Bool()))
    val allFired = Output(Bool())
  })

  val router = Module(new OmegaResponseRouter(
    n, dataWidth, sourceIdWidth, bankIdWidth,
    bankIdOffset = regAddrWidth, debugEnable = false
  ))

  val fired = RegInit(VecInit(Seq.fill(n)(false.B)))

  val timeStamp = RegInit(0.U(64.W))
  timeStamp := timeStamp + 1.U

  for (i <- 0 until n) {
    router.io.in(i).valid := !fired(i)
    router.io.in(i).bits.data := i.U
    router.io.in(i).bits.sourceId := 0.U  // bankId = 0
    when(router.io.in(i).fire) {
      fired(i) := true.B
      printf(cf"[T${timeStamp}][OmegaSwitchBlock] in($i) FIRE\n")
    }
  }

  for (i <- 0 until n) {
    router.io.out(i).ready := true.B
  }

  router.io.timeStamp := timeStamp

  io.inReady  := VecInit(router.io.in.map(_.ready))
  io.allFired := fired.map(_.asBool).reduce(_ && _)
}

class OmegaSwitchBlockSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "OmegaResponseRouter blocking"

  it should "block all but in(0) when all bankIds target out(0)" in {
    test(new OmegaSwitchBlockTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      // 初始状态：所有 in valid=true，直接检查组合逻辑 ready
      dut.io.inReady(0).expect(true.B, "in(0) should be ready (privileged path)")
      for (i <- 1 until 8) {
        dut.io.inReady(i).expect(false.B, s"in($i) should be blocked when in(0) is valid")
      }
    }
  }

  it should "let all 8 inputs fire within 8 cycles when all bankIds target out(0)" in {
    test(new OmegaSwitchBlockTop)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

      dut.clock.step(8)

      dut.io.allFired.expect(true.B, "all 8 inputs should have fired within 8 cycles")
    }
  }
}
