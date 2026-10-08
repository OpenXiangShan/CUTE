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
}
