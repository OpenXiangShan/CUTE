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

  private val unitDataWidth = 32
  private val unitSourceIdWidth = 16
  private val unitBankIdWidth = 3
  private val unitBankIdOffset = 8

  private def encodeSourceId(bankId: Int, regAddr: Int = 0): BigInt = {
    (BigInt(bankId) << unitBankIdOffset) | BigInt(regAddr)
  }

  private def initRouterInputs(dut: OmegaResponseRouter, outCount: Int): Unit = {
    dut.io.timeStamp.poke(0.U)
    for (i <- 0 until 8) {
      dut.io.in(i).valid.poke(false.B)
      dut.io.in(i).bits.data.poke(0.U)
      dut.io.in(i).bits.sourceId.poke(0.U)
      dut.io.in(i).bits.coherent.poke(true.B)
    }
    for (i <- 0 until outCount) {
      dut.io.out(i).ready.poke(true.B)
    }
  }

  private def runGroupedRoutingTest(outCount: Int): Unit = {
    test(new OmegaResponseRouter(
      n = 8,
      dataWidth = unitDataWidth,
      sourceIdWidth = unitSourceIdWidth,
      bankIdWidth = unitBankIdWidth,
      bankIdOffset = unitBankIdOffset,
      outCount = outCount,
      bankCount = 8,
      debugEnable = false
    )).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initRouterInputs(dut, outCount)

      for (bank <- 0 until 8) {
        dut.io.in(0).valid.poke(true.B)
        dut.io.in(0).bits.data.poke(bank.U)
        dut.io.in(0).bits.sourceId.poke(encodeSourceId(bank).U)

        val expectedOut = ResponseChannelHelper.groupIdOfBank(bank, outCount, 8)
        for (out <- 0 until outCount) {
          dut.io.out(out).valid.expect(
            (if (out == expectedOut) true.B else false.B),
            s"bank $bank should target out $expectedOut in $outCount-channel mode"
          )
        }
        dut.io.out(expectedOut).bits.data.expect(bank.U)
        dut.io.out(expectedOut).bits.sourceId.expect(encodeSourceId(bank).U)
        dut.io.out(expectedOut).bits.coherent.expect(true.B)
        dut.io.in(0).ready.expect(true.B)

        dut.clock.step()
      }
    }
  }

  private def runOmegaBlockingTest(outCount: Int, bank0: Int, bank1: Int, expectedOut0: Int): Unit = {
    test(new OmegaResponseRouter(
      n = 8,
      dataWidth = unitDataWidth,
      sourceIdWidth = unitSourceIdWidth,
      bankIdWidth = unitBankIdWidth,
      bankIdOffset = unitBankIdOffset,
      outCount = outCount,
      bankCount = 8,
      debugEnable = false
    )).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initRouterInputs(dut, outCount)

      dut.io.in(0).valid.poke(true.B)
      dut.io.in(0).bits.data.poke(bank0.U)
      dut.io.in(0).bits.sourceId.poke(encodeSourceId(bank0).U)
      dut.io.in(1).valid.poke(true.B)
      dut.io.in(1).bits.data.poke(bank1.U)
      dut.io.in(1).bits.sourceId.poke(encodeSourceId(bank1).U)

      dut.io.out(expectedOut0).valid.expect(true.B)
      dut.io.out(expectedOut0).bits.data.expect(bank0.U)
      dut.io.in(0).ready.expect(true.B)
      dut.io.in(1).ready.expect(false.B)

      dut.clock.step()

      dut.io.in(0).valid.poke(false.B)
      dut.io.in(1).valid.poke(true.B)
      dut.io.in(1).bits.data.poke(bank1.U)
      dut.io.in(1).bits.sourceId.poke(encodeSourceId(bank1).U)

      val expectedOut1 = ResponseChannelHelper.groupIdOfBank(bank1, outCount, 8)
      dut.io.out(expectedOut1).valid.expect(true.B)
      dut.io.out(expectedOut1).bits.data.expect(bank1.U)
      dut.io.in(1).ready.expect(true.B)
    }
  }

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

  it should "map banks into 2 grouped outputs with contiguous segmentation" in {
    runGroupedRoutingTest(outCount = 2)
  }

  it should "map banks into 4 grouped outputs with contiguous segmentation" in {
    runGroupedRoutingTest(outCount = 4)
  }

  it should "preserve blocking omega behavior for different 8-channel outputs" in {
    runOmegaBlockingTest(outCount = 8, bank0 = 0, bank1 = 2, expectedOut0 = 0)
  }

  it should "preserve blocking omega behavior for different 4-channel grouped outputs" in {
    runOmegaBlockingTest(outCount = 4, bank0 = 0, bank1 = 2, expectedOut0 = 0)
  }

  it should "keep static priority per grouped output while allowing other groups to progress" in {
    test(new OmegaResponseRouter(
      n = 8,
      dataWidth = unitDataWidth,
      sourceIdWidth = unitSourceIdWidth,
      bankIdWidth = unitBankIdWidth,
      bankIdOffset = unitBankIdOffset,
      outCount = 2,
      bankCount = 8,
      debugEnable = false
    )).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initRouterInputs(dut, outCount = 2)

      dut.io.in(0).valid.poke(true.B)
      dut.io.in(0).bits.data.poke(10.U)
      dut.io.in(0).bits.sourceId.poke(encodeSourceId(0).U)
      dut.io.in(1).valid.poke(true.B)
      dut.io.in(1).bits.data.poke(11.U)
      dut.io.in(1).bits.sourceId.poke(encodeSourceId(1).U)
      dut.io.in(2).valid.poke(true.B)
      dut.io.in(2).bits.data.poke(24.U)
      dut.io.in(2).bits.sourceId.poke(encodeSourceId(4).U)

      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.data.expect(10.U)
      dut.io.out(1).valid.expect(true.B)
      dut.io.out(1).bits.data.expect(24.U)
      dut.io.in(0).ready.expect(true.B)
      dut.io.in(1).ready.expect(false.B)
      dut.io.in(2).ready.expect(true.B)

      dut.clock.step()

      dut.io.in(0).valid.poke(false.B)
      dut.io.in(1).valid.poke(true.B)
      dut.io.in(1).bits.data.poke(11.U)
      dut.io.in(1).bits.sourceId.poke(encodeSourceId(1).U)
      dut.io.in(2).valid.poke(false.B)

      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.data.expect(11.U)
      dut.io.in(1).ready.expect(true.B)
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
    router.io.in(i).bits.coherent := true.B
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
