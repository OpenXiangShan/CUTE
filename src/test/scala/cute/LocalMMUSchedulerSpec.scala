package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class LocalMMUSchedulerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CollisionAwareLocalMMUScheduler"

  private implicit val params: Parameters = LocalMMUSchedulerTestUtils.parameters()

  private def init(dut: CollisionAwareLocalMMUScheduler): Unit = {
    for (source <- 0 until LocalMMUSourceClass.Count; lane <- dut.io.llc.indices) {
      dut.io.candidates(source)(lane).valid.poke(false.B)
      val request = dut.io.candidates(source)(lane).bits.request
      request.RequestAddr.poke(0.U)
      request.RequestConherent.poke(false.B)
      request.RequestData.poke(0.U)
      request.RequestSourceID.poke(0.U)
      request.RequestType_isWrite.poke(false.B)
      request.UseAllocatedSourceID.poke(false.B)
      request.isA.poke(false.B)
      request.MatrixIsAcc.poke(false.B)
      request.RequestMask.poke(0.U)
      dut.io.candidates(source)(lane).bits.returnBank.poke(0.U)
      dut.io.candidates(source)(lane).bits.physicalInput.poke(0.U)
    }
    dut.io.llc.foreach(_.ready.poke(true.B))
  }

  private def scheduler = new CollisionAwareLocalMMUScheduler(
    laneCount = 8,
    bankCount = 8,
    aResponseChannels = 8,
    bResponseChannels = 8,
    cLoadResponseChannels = 8,
    cStoreResponseChannels = 8,
    aLegacy = false,
    bLegacy = false,
    cLoadLegacy = false,
    cStoreLegacy = false
  )

  it should "mask predictable C collisions and reuse the released lane with A then B" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).bits.physicalInput.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(1).bits.returnBank.poke(2.U)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(1).bits.physicalInput.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.A)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.A)(1).bits.returnBank.poke(7.U)
      dut.io.candidates(LocalMMUSourceClass.A)(1).bits.physicalInput.poke(7.U)
      dut.io.candidates(LocalMMUSourceClass.B)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.B)(1).bits.returnBank.poke(6.U)
      dut.io.candidates(LocalMMUSourceClass.B)(1).bits.physicalInput.poke(6.U)

      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).ready.expect(true.B)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(1).ready.expect(false.B)
      dut.io.candidates(LocalMMUSourceClass.A)(1).ready.expect(true.B)
      dut.io.candidates(LocalMMUSourceClass.B)(1).ready.expect(false.B)
      dut.io.selectedOwner(1).expect(LocalMMUSourceClass.A.U)
      dut.io.collisionMasked(0).expect("b00000010".U)
      dut.io.cLoadRefill.expect("b00000010".U)
      dut.io.priorityBlocked(LocalMMUSourceClass.B).expect("b00000010".U)
    }
  }

  it should "treat C load and C store as independent collision domains" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).bits.physicalInput.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.CStore)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.CStore)(1).bits.returnBank.poke(2.U)
      dut.io.candidates(LocalMMUSourceClass.CStore)(1).bits.physicalInput.poke(2.U)
      dut.io.candidates(LocalMMUSourceClass.CLoad)(0).ready.expect(true.B)
      dut.io.candidates(LocalMMUSourceClass.CStore)(1).ready.expect(true.B)
    }
  }

  it should "issue every lane for a no-conflict batch" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val banks = Seq(0, 4, 1, 5, 2, 6, 3, 7)
      for (lane <- 0 until 8) {
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).valid.poke(true.B)
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).bits.returnBank.poke(banks(lane).U)
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).bits.physicalInput.poke(lane.U)
      }
      val expected = LocalMMUSchedulerTestUtils.survivorMask(
        Seq.fill(8)(true), banks, responseChannels = 8)
      val accepted = (0 until 8).count(lane =>
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).ready.peek().litToBoolean)
      assert(accepted == expected.bitCount)
      assert(accepted == 8)
    }
  }

  it should "treat L2 banks as Omega inputs independently of request lanes" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      // Inverse of the perfect physical-input -> destination permutation used
      // by the topology oracle. Destination remains the request-lane bank.
      val physicalInputs = Seq(0, 2, 4, 6, 1, 3, 5, 7)
      for (lane <- 0 until 8) {
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).valid.poke(true.B)
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).bits.physicalInput.poke(physicalInputs(lane).U)
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).bits.returnBank.poke(lane.U)
      }
      val expected = LocalMMUSchedulerTestUtils.physicalSurvivorMask(
        Seq.fill(8)(true), physicalInputs, (0 until 8), responseChannels = 8)
      assert(expected == 0xff)
      dut.io.survivor(0).expect(expected.U)
      dut.io.collisionMasked(0).expect(0.U)
      for (lane <- 0 until 8) {
        dut.io.candidates(LocalMMUSourceClass.CLoad)(lane).ready.expect(true.B)
      }
    }
  }

  it should "place CStore after A and B in the lane priority" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val lane = 3
      Seq(
        LocalMMUSourceClass.CStore,
        LocalMMUSourceClass.A,
        LocalMMUSourceClass.AScale,
        LocalMMUSourceClass.B,
        LocalMMUSourceClass.BScale
      ).foreach { source =>
        dut.io.candidates(source)(lane).valid.poke(true.B)
        dut.io.candidates(source)(lane).bits.physicalInput.poke(lane.U)
        dut.io.candidates(source)(lane).bits.returnBank.poke(lane.U)
      }
      dut.io.selectedOwner(lane).expect(LocalMMUSourceClass.A.U)
      dut.io.candidates(LocalMMUSourceClass.A)(lane).ready.expect(true.B)
      dut.io.candidates(LocalMMUSourceClass.B)(lane).ready.expect(false.B)
      dut.io.candidates(LocalMMUSourceClass.CStore)(lane).ready.expect(false.B)
      dut.io.priorityBlocked(LocalMMUSourceClass.CStore).expect((1 << lane).U)

      dut.io.candidates(LocalMMUSourceClass.A)(lane).valid.poke(false.B)
      dut.io.selectedOwner(lane).expect(LocalMMUSourceClass.AScale.U)
      dut.io.candidates(LocalMMUSourceClass.AScale)(lane).valid.poke(false.B)
      dut.io.selectedOwner(lane).expect(LocalMMUSourceClass.B.U)
      dut.io.candidates(LocalMMUSourceClass.B)(lane).valid.poke(false.B)
      dut.io.selectedOwner(lane).expect(LocalMMUSourceClass.BScale.U)
      dut.io.candidates(LocalMMUSourceClass.BScale)(lane).valid.poke(false.B)
      dut.io.selectedOwner(lane).expect(LocalMMUSourceClass.CStore.U)
    }
  }

  it should "forward an unchanged candidate through ten stalled cycles" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val source = LocalMMUSourceClass.CLoad
      dut.io.llc(0).ready.poke(false.B)
      dut.io.candidates(source)(0).valid.poke(true.B)
      dut.io.candidates(source)(0).bits.returnBank.poke(3.U)
      dut.io.candidates(source)(0).bits.physicalInput.poke(3.U)
      dut.io.candidates(source)(0).bits.request.RequestAddr.poke("h12340".U)
      dut.io.candidates(source)(0).bits.request.RequestSourceID.poke("h55".U)
      dut.clock.step()
      for (cycle <- 0 until 10) {
        dut.io.candidates(LocalMMUSourceClass.A)((cycle + 1) % 8).valid.poke((cycle % 2 == 0).B)
        dut.io.llc(0).valid.expect(true.B)
        dut.io.llc(0).bits.RequestAddr.expect("h12340".U)
        dut.io.llc(0).bits.RequestSourceID.expect("h55".U)
        dut.io.selectedOwner(0).expect(source.U)
        dut.clock.step()
      }
      dut.io.llc(0).ready.poke(true.B)
      dut.io.candidates(source)(0).ready.expect(true.B)
    }
  }

  it should "drain a perfect batch exactly once after asymmetric ten-cycle stalls" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val source = LocalMMUSourceClass.CLoad
      val physicalInputs = Seq(0, 2, 4, 6, 1, 3, 5, 7)
      val fired = Array.fill(8)(0)
      for (lane <- 0 until 8) {
        val request = dut.io.candidates(source)(lane)
        request.valid.poke(true.B)
        request.bits.physicalInput.poke(physicalInputs(lane).U)
        request.bits.returnBank.poke(lane.U)
        request.bits.request.RequestAddr.poke((0x1000 + lane * 64).U)
        request.bits.request.RequestSourceID.poke((0x40 + lane).U)
        dut.io.llc(lane).ready.poke((lane % 2 == 0).B)
        if (lane % 2 == 0) {
          request.ready.expect(true.B)
          fired(lane) += 1
        }
      }
      dut.clock.step()
      for (lane <- 0 until 8 if lane % 2 == 0) {
        dut.io.candidates(source)(lane).valid.poke(false.B)
      }

      dut.io.held.expect(0.U)
      for (_ <- 0 until 10) {
        for (lane <- 0 until 8 if lane % 2 == 1) {
          dut.io.llc(lane).valid.expect(true.B)
          dut.io.llc(lane).bits.RequestAddr.expect((0x1000 + lane * 64).U)
          dut.io.llc(lane).bits.RequestSourceID.expect((0x40 + lane).U)
          dut.io.selectedOwner(lane).expect(source.U)
          dut.io.candidates(source)(lane).ready.expect(false.B)
        }
        dut.clock.step()
      }

      for (lane <- 0 until 8 if lane % 2 == 1) {
        dut.io.llc(lane).ready.poke(true.B)
        dut.io.candidates(source)(lane).ready.expect(true.B)
        fired(lane) += 1
      }
      dut.clock.step()
      for (lane <- 0 until 8 if lane % 2 == 1) {
        dut.io.candidates(source)(lane).valid.poke(false.B)
      }
      dut.io.held.expect(0.U)
      assert(fired.forall(_ == 1), s"perfect batch fire counts=${fired.toSeq}")
    }
  }

  it should "classify first-stage, later-stage, and duplicate-input C conflicts exactly" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val source = LocalMMUSourceClass.CLoad

      def drivePair(secondPhysicalInput: Int): Unit = {
        Seq(0, 1).foreach { lane =>
          dut.io.candidates(source)(lane).valid.poke(true.B)
          dut.io.candidates(source)(lane).bits.returnBank.poke(0.U)
        }
        dut.io.candidates(source)(0).bits.physicalInput.poke(0.U)
        dut.io.candidates(source)(1).bits.physicalInput.poke(secondPhysicalInput.U)
      }

      // Physical inputs 0/1 meet at the first switch.
      drivePair(secondPhysicalInput = 1)
      dut.io.collisionMasked(0).expect("b00000010".U)
      dut.io.survivor(0).expect("b00000001".U)

      // Physical inputs 0/2 meet only after the intervening shuffles.
      drivePair(secondPhysicalInput = 2)
      dut.io.collisionMasked(0).expect("b00000010".U)
      dut.io.survivor(0).expect("b00000001".U)

      // A duplicate L2 input is rejected before Omega evaluation.
      drivePair(secondPhysicalInput = 0)
      dut.io.collisionMasked(0).expect("b00000010".U)
      dut.io.survivor(0).expect("b00000001".U)
    }
  }

  it should "reselect current Omega candidates after a stall without consuming the loser" in {
    test(scheduler).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val source = LocalMMUSourceClass.CLoad
      dut.io.llc(1).ready.poke(false.B)
      dut.io.candidates(source)(1).valid.poke(true.B)
      dut.io.candidates(source)(1).bits.returnBank.poke(0.U)
      dut.io.candidates(source)(1).bits.physicalInput.poke(0.U)
      dut.clock.step()
      dut.io.candidates(source)(0).valid.poke(true.B)
      dut.io.candidates(source)(0).bits.returnBank.poke(0.U)
      dut.io.candidates(source)(0).bits.physicalInput.poke(0.U)
      dut.io.candidates(source)(0).ready.expect(true.B)
      dut.io.candidates(source)(1).ready.expect(false.B)
      dut.io.survivor(0).expect(1.U)
      dut.clock.step()
      dut.io.candidates(source)(0).valid.poke(false.B)
      dut.io.llc(1).ready.poke(true.B)
      dut.io.candidates(source)(1).ready.expect(true.B)
      dut.io.survivor(0).expect(2.U)
    }
  }

  it should "bypass collision prediction only for legacy domains" in {
    test(new CollisionAwareLocalMMUScheduler(
      laneCount = 8,
      bankCount = 8,
      aResponseChannels = 1,
      bResponseChannels = 2,
      cLoadResponseChannels = 4,
      cStoreResponseChannels = 8,
      aLegacy = true,
      bLegacy = false,
      cLoadLegacy = false,
      cStoreLegacy = false
    )).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.candidates(LocalMMUSourceClass.A)(0).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.A)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.A)(0).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.A)(1).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.A)(0).bits.physicalInput.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.A)(1).bits.physicalInput.poke(0.U)
      // Legacy bypasses Omega conflict prediction, but duplicate L2 inputs
      // still represent one physical bank opportunity.
      dut.io.survivor(2).expect("b00000001".U)
      dut.io.candidates(LocalMMUSourceClass.A)(0).valid.poke(false.B)
      dut.io.candidates(LocalMMUSourceClass.A)(1).valid.poke(false.B)

      dut.io.candidates(LocalMMUSourceClass.B)(0).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.B)(1).valid.poke(true.B)
      dut.io.candidates(LocalMMUSourceClass.B)(0).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.B)(1).bits.returnBank.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.B)(0).bits.physicalInput.poke(0.U)
      dut.io.candidates(LocalMMUSourceClass.B)(1).bits.physicalInput.poke(0.U)
      dut.io.survivor(3).expect("b00000001".U)
    }
  }
  for (source <- Seq(LocalMMUSourceClass.A, LocalMMUSourceClass.B)) {
    it should s"drain non-identity four-bank AB source $source with partial issue" in {
      test(new CollisionAwareLocalMMUScheduler(
        laneCount = 4, bankCount = 4, aResponseChannels = 4, bResponseChannels = 4,
        cLoadResponseChannels = 4, cStoreResponseChannels = 4,
        aLegacy = false, bLegacy = false, cLoadLegacy = false, cStoreLegacy = false
      )).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        init(dut)
        val pending = scala.collection.mutable.Set(0, 1, 2, 3)
        val fired = scala.collection.mutable.Set.empty[Int]
        for (cycle <- 0 until 16) {
          for (lane <- 0 until 4) {
            val candidate = dut.io.candidates(source)(lane)
            candidate.valid.poke(pending.contains(lane).B)
            candidate.bits.physicalInput.poke(((lane + 1) % 4).U)
            candidate.bits.returnBank.poke(lane.U)
            candidate.bits.request.RequestSourceID.poke(lane.U)
            dut.io.llc(lane).ready.poke((cycle >= 3).B)
          }
          val accepted = pending.filter(lane => dut.io.candidates(source)(lane).ready.peek().litToBoolean).toSeq
          for (lane <- accepted) {
            dut.io.llc(lane).bits.RequestSourceID.expect(lane.U)
            assert(fired.add(lane))
          }
          dut.clock.step()
          pending --= accepted
        }
        assert(pending.isEmpty && fired.size == 4)
      }
    }
  }

}
