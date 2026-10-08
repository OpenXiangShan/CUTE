package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class LocalMMUL2BankRouteHarness(implicit p: Parameters) extends CuteModule {
  private val laneCount = 8
  val io = IO(new Bundle {
    val cLoad = Flipped(Vec(laneCount,
      Decoupled(new LocalMMUSchedulerCandidate(laneCount))))
    val a = Flipped(Vec(laneCount,
      Decoupled(new LocalMMUSchedulerCandidate(laneCount))))
    val bankOut = Vec(laneCount, Decoupled(new MMURequestIO))
    val collisionMasked = Output(UInt(laneCount.W))
    val aPriorityBlocked = Output(UInt(laneCount.W))
    val cLoadRefill = Output(UInt(laneCount.W))
  })

  val scheduler = Module(new CollisionAwareLocalMMUScheduler(
    laneCount = laneCount,
    bankCount = laneCount,
    aResponseChannels = 8,
    bResponseChannels = 8,
    cLoadResponseChannels = 8,
    cStoreResponseChannels = 8,
    aLegacy = false,
    bLegacy = false,
    cLoadLegacy = false,
    cStoreLegacy = false
  ))
  val bankRouter = Module(new AddressBankRequestRouter(laneCount))

  scheduler.io.candidates(LocalMMUSourceClass.CLoad) <> io.cLoad
  scheduler.io.candidates(LocalMMUSourceClass.A) <> io.a
  for (source <- Seq(LocalMMUSourceClass.CStore, LocalMMUSourceClass.AScale,
    LocalMMUSourceClass.B, LocalMMUSourceClass.BScale); lane <- 0 until laneCount) {
    scheduler.io.candidates(source)(lane).valid := false.B
    scheduler.io.candidates(source)(lane).bits := 0.U.asTypeOf(
      scheduler.io.candidates(source)(lane).bits)
  }
  bankRouter.io.in <> scheduler.io.llc
  io.bankOut <> bankRouter.io.out
  io.collisionMasked := scheduler.io.collisionMasked(0)
  io.aPriorityBlocked := scheduler.io.priorityBlocked(LocalMMUSourceClass.A)
  io.cLoadRefill := scheduler.io.cLoadRefill
}

class LocalMMUL2BankRouteSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LocalMMU L2-bank route identity"

  private def init(dut: LocalMMUL2BankRouteHarness): Unit = {
    for (lane <- 0 until 8) {
      dut.io.cLoad(lane).valid.poke(false.B)
      dut.io.a(lane).valid.poke(false.B)
      Seq(dut.io.cLoad(lane).bits, dut.io.a(lane).bits).foreach { candidate =>
        candidate.physicalInput.poke(0.U)
        candidate.returnBank.poke(0.U)
        candidate.request.RequestAddr.poke(0.U)
        candidate.request.RequestConherent.poke(false.B)
        candidate.request.RequestData.poke(0.U)
        candidate.request.RequestSourceID.poke(0.U)
        candidate.request.RequestType_isWrite.poke(false.B)
        candidate.request.UseAllocatedSourceID.poke(false.B)
        candidate.request.isA.poke(false.B)
        candidate.request.MatrixIsAcc.poke(false.B)
        candidate.request.RequestMask.poke(0.U)
      }
      dut.io.bankOut(lane).ready.poke(true.B)
    }
  }

  private def incrementalMask(returnBanks: Seq[Int]): BigInt = {
    var accepted = BigInt(0)
    for (physicalInput <- returnBanks.indices) {
      val tentative = accepted | (BigInt(1) << physicalInput)
      val survivors = LocalMMUSchedulerTestUtils.survivorMask(
        returnBanks.indices.map(tentative.testBit), returnBanks, responseChannels = 8)
      if ((survivors & accepted) == accepted && survivors.testBit(physicalInput)) {
        accepted = tentative
      }
    }
    accepted
  }

  Seq(true, false).foreach { transparent =>
    it should s"use addressed L2 banks as response inputs when transparent=$transparent" in {
      implicit val p: Parameters = LocalMMUSchedulerTestUtils.parameters(transparent = transparent)
      test(new LocalMMUL2BankRouteHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        init(dut)
        val physicalInputs = Seq(0, 2, 4, 6, 1, 3, 5, 7)
        for (lane <- 0 until 8) {
          val candidate = dut.io.cLoad(lane)
          candidate.valid.poke(true.B)
          candidate.bits.physicalInput.poke(physicalInputs(lane).U)
          candidate.bits.returnBank.poke(lane.U)
          candidate.bits.request.RequestAddr.poke((BigInt(physicalInputs(lane)) << 6).U)
          candidate.bits.request.RequestSourceID.poke(lane.U)
        }

        dut.io.collisionMasked.expect(0.U)
        for (lane <- 0 until 8) {
          dut.io.cLoad(lane).ready.expect(true.B)
        }
        for (bank <- 0 until 8) {
          dut.io.bankOut(bank).valid.expect(true.B)
          val routedBank = (dut.io.bankOut(bank).bits.RequestAddr.peek().litValue >> 6) & 7
          assert(routedBank == bank)
        }
      }
    }
  }

  it should "route an A refill only after a genuine C physical-input collision" in {
    implicit val p: Parameters = LocalMMUSchedulerTestUtils.parameters(transparent = false)
    test(new LocalMMUL2BankRouteHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      for (lane <- Seq(0, 1)) {
        dut.io.cLoad(lane).valid.poke(true.B)
        dut.io.cLoad(lane).bits.physicalInput.poke(0.U)
        dut.io.cLoad(lane).bits.returnBank.poke(lane.U)
        dut.io.cLoad(lane).bits.request.RequestAddr.poke(0.U)
      }
      dut.io.a(1).valid.poke(true.B)
      dut.io.a(1).bits.physicalInput.poke(7.U)
      dut.io.a(1).bits.returnBank.poke(1.U)
      dut.io.a(1).bits.request.RequestAddr.poke((BigInt(7) << 6).U)

      dut.io.cLoad(0).ready.expect(true.B)
      dut.io.cLoad(1).ready.expect(false.B)
      dut.io.a(1).ready.expect(true.B)
      dut.io.collisionMasked.expect("b00000010".U)
      dut.io.cLoadRefill.expect("b00000010".U)
      dut.io.bankOut(0).valid.expect(true.B)
      dut.io.bankOut(7).valid.expect(true.B)
    }
  }

  it should "apply the exact Omega admission mask to unique seeded-window routes" in {
    implicit val p: Parameters = LocalMMUSchedulerTestUtils.parameters(transparent = false)
    test(new LocalMMUL2BankRouteHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val physicalInputs = 0 until 8
      val returnBanks = 0 until 8
      val expectedMask = incrementalMask(returnBanks)
      assert(expectedMask != 0xff, "fixture must exercise an exact Omega conflict")

      for (lane <- 0 until 8) {
        val candidate = dut.io.cLoad(lane)
        candidate.valid.poke(true.B)
        candidate.bits.physicalInput.poke(physicalInputs(lane).U)
        candidate.bits.returnBank.poke(returnBanks(lane).U)
        candidate.bits.request.RequestAddr.poke((BigInt(physicalInputs(lane)) << 6).U)
        candidate.bits.request.RequestSourceID.poke(lane.U)
        candidate.ready.expect(expectedMask.testBit(lane).B)
      }
      dut.io.collisionMasked.expect(((BigInt(1) << 8) - 1 - expectedMask).U)
      for (bank <- 0 until 8) {
        dut.io.bankOut(bank).valid.expect(expectedMask.testBit(bank).B)
      }
    }
  }

  it should "preserve C-load priority in the final admission mask" in {
    implicit val p: Parameters = LocalMMUSchedulerTestUtils.parameters(transparent = false)
    test(new LocalMMUL2BankRouteHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.cLoad(3).valid.poke(true.B)
      dut.io.cLoad(3).bits.physicalInput.poke(5.U)
      dut.io.cLoad(3).bits.returnBank.poke(3.U)
      dut.io.cLoad(3).bits.request.RequestAddr.poke((BigInt(5) << 6).U)
      dut.io.a(3).valid.poke(true.B)
      dut.io.a(3).bits.physicalInput.poke(5.U)
      dut.io.a(3).bits.returnBank.poke(3.U)
      dut.io.a(3).bits.request.RequestAddr.poke((BigInt(5) << 6).U)

      dut.io.cLoad(3).ready.expect(true.B)
      dut.io.a(3).ready.expect(false.B)
      dut.io.aPriorityBlocked.expect("b00001000".U)
      dut.io.bankOut(5).valid.expect(true.B)
      dut.io.bankOut(5).bits.RequestAddr.expect((BigInt(5) << 6).U)
    }
  }
}
