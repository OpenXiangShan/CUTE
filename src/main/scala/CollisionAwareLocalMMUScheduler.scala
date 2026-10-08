package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate

object LocalMMUSourceClass {
  val CLoad = 0
  val CStore = 1
  val A = 2
  val AScale = 3
  val B = 4
  val BScale = 5
  val Count = 6
  val None = Count

  val names = Seq("CLoad", "CStore", "A", "AScale", "B", "BScale")
}

class LocalMMUSchedulerCandidate(val bankCount: Int)(implicit p: Parameters) extends CuteBundle {
  val request = new MMURequestIO
  // Request-lane ownership is separate from the physical L2 response input.
  // `returnBank` remains the MatrixReg/Omega destination bank.
  val physicalInput = UInt(log2Ceil(bankCount).W)
  val returnBank = UInt(log2Ceil(bankCount).W)
}

/**
  * Predicts each parameterized response fabric independently, then fills every
  * free LLC lane in CLoad > A > AScale > B > BScale > CStore order. A selected
  * transaction that meets backpressure is retained in full until it fires.
  */
class CollisionAwareLocalMMUScheduler(
  laneCount: Int,
  bankCount: Int,
  aResponseChannels: Int,
  bResponseChannels: Int,
  cLoadResponseChannels: Int,
  cStoreResponseChannels: Int,
  aLegacy: Boolean,
  bLegacy: Boolean,
  cLoadLegacy: Boolean,
  cStoreLegacy: Boolean
)(implicit p: Parameters) extends CuteModule {
  require(isPow2(laneCount) && laneCount == bankCount)

  private val ownerWidth = log2Ceil(LocalMMUSourceClass.Count + 1)
  private val bankWidth = log2Ceil(bankCount)

  val io = IO(new Bundle {
    val candidates = Flipped(Vec(LocalMMUSourceClass.Count,
      Vec(laneCount, Decoupled(new LocalMMUSchedulerCandidate(bankCount)))))
    val llc = Vec(laneCount, Decoupled(new MMURequestIO))
    val selectedOwner = Output(Vec(laneCount, UInt(ownerWidth.W)))
    val survivor = Output(Vec(4, UInt(laneCount.W)))
    val collisionMasked = Output(Vec(4, UInt(laneCount.W)))
    val evaluated = Output(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
    val priorityBlocked = Output(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
    val selectedFire = Output(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
    val held = Output(UInt(laneCount.W))
    val cLoadRefill = Output(UInt(laneCount.W))
  })

  val heldValid = RegInit(VecInit(Seq.fill(laneCount)(false.B)))
  val heldOwner = RegInit(VecInit(Seq.fill(laneCount)(LocalMMUSourceClass.None.U(ownerWidth.W))))
  val heldRequest = Reg(Vec(laneCount, new MMURequestIO))
  val heldPhysicalInput = Reg(Vec(laneCount, UInt(bankWidth.W)))
  val heldReturnBank = Reg(Vec(laneCount, UInt(bankWidth.W)))

  for (source <- 0 until LocalMMUSourceClass.Count; lane <- 0 until laneCount) {
    io.candidates(source)(lane).ready := false.B
  }

  private def domainMask(
    source: Int,
    responseChannels: Int,
    legacy: Boolean,
    candidateEligible: Seq[Bool]
  ): UInt = {
    val held = (0 until laneCount).map { lane =>
      heldValid(lane) && heldOwner(lane) === source.U
    }
    val valids = (0 until laneCount).map { lane =>
      held(lane) || (!heldValid(lane) && io.candidates(source)(lane).valid && candidateEligible(lane))
    }
    // Omega input position is the addressed L2 bank, not the request lane.
    // Select one deterministic owner for duplicate physical inputs before
    // evaluating the network, then scatter the physical survivor mask back to
    // the original request lanes.
    val physicalOwners = (0 until bankCount).map { physical =>
      val owner = Wire(Vec(laneCount, Bool()))
      val heldMatches = (0 until laneCount).map { lane =>
        held(lane) && heldPhysicalInput(lane) === physical.U
      }
      val hasHeldOwner = heldMatches.reduce(_ || _)
      var seenHeld: Bool = false.B
      var seenNew: Bool = false.B
      for (lane <- 0 until laneCount) {
        val lanePhysical = Mux(heldValid(lane), heldPhysicalInput(lane),
          io.candidates(source)(lane).bits.physicalInput)
        val newMatch = !held(lane) && valids(lane) && lanePhysical === physical.U
        owner(lane) := Mux(hasHeldOwner,
          heldMatches(lane) && !seenHeld,
          newMatch && !seenNew)
        seenHeld = seenHeld || heldMatches(lane)
        seenNew = seenNew || newMatch
      }
      owner
    }
    val physicalValids = physicalOwners.map(_.asUInt.orR)
    val physicalDestinations = (0 until bankCount).map { physical =>
      val destinations = (0 until laneCount).map { lane =>
        val destinationBank = Mux(heldValid(lane), heldReturnBank(lane),
          io.candidates(source)(lane).bits.returnBank)
        OmegaTopology.destinationLane(destinationBank,
          laneCount, responseChannels, bankCount)
      }
      Mux1H(physicalOwners(physical).toSeq, destinations)
    }
    val survivorPhysical = if (legacy) {
      VecInit(physicalValids).asUInt
    } else {
      val heldMask = VecInit(held).asUInt
      val heldPhysicalValids = physicalOwners.map(owner =>
        (0 until laneCount).map(lane => owner(lane) && held(lane)).reduce(_ || _))
      val heldOnlyPhysical = OmegaTopology.survivorMask(heldPhysicalValids, physicalDestinations)

      // A held batch was admitted as a collision-free set. Do not let a new
      // request exploit the router's static priority and displace one of its
      // routes before the complete batch has left LocalMMU.
      when(heldMask.orR) {
        assert((VecInit(heldPhysicalValids).asUInt & ~heldOnlyPhysical) === 0.U,
          s"LocalMMU $source held routes are not Omega-compatible")
      }
      OmegaTopology.incrementalCompatibleMask(
        physicalValids,
        physicalDestinations,
        heldPhysicalValids
      )
    }
    VecInit((0 until laneCount).map { lane =>
      (0 until bankCount).map { physical =>
        physicalOwners(physical)(lane) && survivorPhysical(physical)
      }.reduce(_ || _)
    }).asUInt
  }

  val priorityOrder = Seq(
    LocalMMUSourceClass.CLoad,
    LocalMMUSourceClass.A,
    LocalMMUSourceClass.AScale,
    LocalMMUSourceClass.B,
    LocalMMUSourceClass.BScale,
    LocalMMUSourceClass.CStore
  )

  // Commit tiers progressively. Request lanes and addressed L2 banks selected
  // by held/higher-priority traffic are unavailable to lower tiers.
  val selectedBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val survivorBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val evaluatedBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val collisionBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  selectedBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  survivorBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  evaluatedBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  collisionBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  var occupiedLanes: Seq[Bool] = (0 until laneCount).map(heldValid(_))
  var occupiedPhysicalInputs: Seq[Bool] = (0 until bankCount).map { physical =>
    (0 until laneCount).map(lane => heldValid(lane) && heldPhysicalInput(lane) === physical.U).reduce(_ || _)
  }
  for (source <- priorityOrder) {
    val tierEligible = (0 until laneCount).map { lane =>
      val candidate = io.candidates(source)(lane)
      val physicalFree = Mux1H((0 until bankCount).map(physical =>
        (candidate.bits.physicalInput === physical.U) -> !occupiedPhysicalInputs(physical)))
      !occupiedLanes(lane) && physicalFree
    }
    val tierSurvivor = source match {
      case LocalMMUSourceClass.CLoad =>
        domainMask(source, cLoadResponseChannels, cLoadLegacy, tierEligible)
      case LocalMMUSourceClass.CStore =>
        domainMask(source, cStoreResponseChannels, cStoreLegacy, tierEligible)
      case LocalMMUSourceClass.A =>
        domainMask(source, aResponseChannels, aLegacy, tierEligible)
      case LocalMMUSourceClass.B =>
        domainMask(source, bResponseChannels, bLegacy, tierEligible)
      case _ => VecInit((0 until laneCount).map(lane =>
        io.candidates(source)(lane).valid && tierEligible(lane))).asUInt
    }
    val evaluated = VecInit((0 until laneCount).map(lane =>
      io.candidates(source)(lane).valid && tierEligible(lane) && !heldValid(lane))).asUInt
    val selected = (0 until laneCount).map { lane =>
      io.candidates(source)(lane).valid && tierEligible(lane) && tierSurvivor(lane)
    }
    survivorBySource(source) := tierSurvivor
    evaluatedBySource(source) := evaluated
    collisionBySource(source) := evaluated & ~tierSurvivor
    selectedBySource(source) := VecInit(selected).asUInt
    occupiedLanes = occupiedLanes.zip(selected).map { case (occupied, wins) => occupied || wins }
    occupiedPhysicalInputs = occupiedPhysicalInputs.zipWithIndex.map { case (occupied, physical) =>
      occupied || (0 until laneCount).map(lane =>
        selected(lane) && io.candidates(source)(lane).bits.physicalInput === physical.U).reduce(_ || _)
    }
  }
  io.survivor := VecInit(
    survivorBySource(LocalMMUSourceClass.CLoad),
    survivorBySource(LocalMMUSourceClass.CStore),
    survivorBySource(LocalMMUSourceClass.A),
    survivorBySource(LocalMMUSourceClass.B)
  )
  io.collisionMasked := VecInit(
    collisionBySource(LocalMMUSourceClass.CLoad),
    collisionBySource(LocalMMUSourceClass.CStore),
    collisionBySource(LocalMMUSourceClass.A),
    collisionBySource(LocalMMUSourceClass.B)
  )
  for (source <- 0 until LocalMMUSourceClass.Count) {
    val offered = VecInit(io.candidates(source).map(_.valid)).asUInt
    io.evaluated(source) := evaluatedBySource(source)
    io.priorityBlocked(source) := offered & ~evaluatedBySource(source) & ~heldValid.asUInt
    io.selectedFire(source) := VecInit(io.candidates(source).map(_.fire)).asUInt
  }
  io.held := heldValid.asUInt
  val abRefillFire = io.selectedFire(LocalMMUSourceClass.A) |
    io.selectedFire(LocalMMUSourceClass.AScale) |
    io.selectedFire(LocalMMUSourceClass.B) |
    io.selectedFire(LocalMMUSourceClass.BScale)
  io.cLoadRefill := io.collisionMasked(0) & abRefillFire

  for (lane <- 0 until laneCount) {
    val selected = Wire(UInt(ownerWidth.W))
    selected := LocalMMUSourceClass.None.U
    for (source <- priorityOrder) {
      when(selectedBySource(source)(lane)) {
        selected := source.U
      }
    }

    val selectedRequest = Wire(new MMURequestIO)
    val selectedPhysicalInput = Wire(UInt(bankWidth.W))
    val selectedDestinationBank = Wire(UInt(bankWidth.W))
    selectedRequest := 0.U.asTypeOf(selectedRequest)
    selectedPhysicalInput := 0.U
    selectedDestinationBank := 0.U
    for (source <- 0 until LocalMMUSourceClass.Count) {
      when(selected === source.U) {
        selectedRequest := io.candidates(source)(lane).bits.request
        selectedPhysicalInput := io.candidates(source)(lane).bits.physicalInput
        selectedDestinationBank := io.candidates(source)(lane).bits.returnBank
      }
    }

    io.llc(lane).valid := Mux(heldValid(lane), true.B, selected =/= LocalMMUSourceClass.None.U)
    io.llc(lane).bits := Mux(heldValid(lane), heldRequest(lane), selectedRequest)
    io.selectedOwner(lane) := Mux(heldValid(lane), heldOwner(lane), selected)

    for (source <- 0 until LocalMMUSourceClass.Count) {
      io.candidates(source)(lane).ready := io.llc(lane).fire && io.selectedOwner(lane) === source.U
    }

    when(!heldValid(lane) && io.llc(lane).valid && !io.llc(lane).ready) {
      heldValid(lane) := true.B
      heldOwner(lane) := selected
      heldRequest(lane) := selectedRequest
      heldPhysicalInput(lane) := selectedPhysicalInput
      heldReturnBank(lane) := selectedDestinationBank
    }.elsewhen(heldValid(lane) && io.llc(lane).fire) {
      heldValid(lane) := false.B
      heldOwner(lane) := LocalMMUSourceClass.None.U
    }

    when(RegNext(io.llc(lane).valid && !io.llc(lane).ready, false.B)) {
      assert(io.llc(lane).valid, s"LocalMMU LLC lane $lane dropped a stalled request")
      assert(io.llc(lane).bits === RegEnable(io.llc(lane).bits, io.llc(lane).valid && !io.llc(lane).ready),
        s"LocalMMU LLC lane $lane changed a stalled request")
    }
    assert(PopCount((0 until LocalMMUSourceClass.Count).map(source => io.candidates(source)(lane).fire)) <= 1.U,
      s"LocalMMU LLC lane $lane accepted multiple owners")
  }

  for (source <- 0 until LocalMMUSourceClass.Count) {
    XSPerfAccumulate(s"CUTE_LocalMMU_${LocalMMUSourceClass.names(source)}_offered",
      PopCount(io.candidates(source).map(_.valid)))
    XSPerfAccumulate(s"CUTE_LocalMMU_${LocalMMUSourceClass.names(source)}_issued",
      PopCount(io.candidates(source).map(_.fire)))
  }
  XSPerfAccumulate("CUTE_LocalMMU_CLoad_collision_masked", PopCount(io.collisionMasked(0)))
  XSPerfAccumulate("CUTE_LocalMMU_CStore_collision_masked", PopCount(io.collisionMasked(1)))
  XSPerfAccumulate("CUTE_LocalMMU_A_collision_masked", PopCount(io.collisionMasked(2)))
  XSPerfAccumulate("CUTE_LocalMMU_B_collision_masked", PopCount(io.collisionMasked(3)))
  XSPerfAccumulate("CUTE_LocalMMU_collision_reused_lane", PopCount(io.cLoadRefill))
}
