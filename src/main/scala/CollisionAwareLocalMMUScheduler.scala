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
 * Admit current candidates in CLoad > A > AScale > B > BScale > CStore order.
 * This is a combinational Decoupled arbiter: only fire transfers ownership.
 * In particular, grouped arbiters and allocated source IDs may change while
 * stalled, so no unaccepted payload or route is reserved here.
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

  for (source <- 0 until LocalMMUSourceClass.Count; lane <- 0 until laneCount) {
    io.candidates(source)(lane).ready := false.B
  }

  private def domainMask(
    source: Int,
    responseChannels: Int,
    legacy: Boolean,
    candidateEligible: Seq[Bool]
  ): UInt = {
    val valids = (0 until laneCount).map { lane =>
      io.candidates(source)(lane).valid && candidateEligible(lane)
    }
    // Omega input position is the addressed L2 bank, not the request lane.
    // Select one deterministic owner for duplicate physical inputs before
    // evaluating the network, then scatter the physical survivor mask back to
    // the original request lanes.
    val physicalOwners = (0 until bankCount).map { physical =>
      val owner = Wire(Vec(laneCount, Bool()))
      var seen: Bool = false.B
      for (lane <- 0 until laneCount) {
        val matches = valids(lane) && io.candidates(source)(lane).bits.physicalInput === physical.U
        owner(lane) := matches && !seen
        seen = seen || matches
      }
      owner
    }
    val physicalValids = physicalOwners.map(_.asUInt.orR)
    val physicalDestinations = (0 until bankCount).map { physical =>
      val destinations = (0 until laneCount).map { lane =>
        val destinationBank = io.candidates(source)(lane).bits.returnBank
        OmegaTopology.destinationLane(destinationBank,
          laneCount, responseChannels, bankCount)
      }
      Mux1H(physicalOwners(physical).toSeq, destinations)
    }
    val survivorPhysical = if (legacy) {
      VecInit(physicalValids).asUInt
    } else {
      OmegaTopology.incrementalCompatibleMask(
        physicalValids,
        physicalDestinations,
        Seq.fill(bankCount)(false.B)
      )
    }
    VecInit((0 until laneCount).map { lane =>
      (0 until bankCount).map { physical =>
        physicalOwners(physical)(lane) && survivorPhysical(physical)
      }.reduce(_ || _)
    }).asUInt
  }

  /** Full-width eight-bank AB admission. Identity layouts use the optimized
    * fabric; other layouts rely on its ordinary response backpressure.
    */
  private def identityMask(source: Int, candidateEligible: Seq[Bool]): UInt = {
    val accepted = (0 until laneCount).map { lane =>
      val candidate = io.candidates(source)(lane)
      val duplicate = (0 until lane).map { earlier =>
        io.candidates(source)(earlier).valid && candidateEligible(earlier) &&
          io.candidates(source)(earlier).bits.physicalInput === candidate.bits.physicalInput
      }.reduceOption(_ || _).getOrElse(false.B)
      candidate.valid && candidateEligible(lane) && !duplicate
    }
    VecInit(accepted).asUInt
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
  // by higher-priority traffic are unavailable to lower tiers.
  val selectedBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val survivorBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val evaluatedBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  val collisionBySource = Wire(Vec(LocalMMUSourceClass.Count, UInt(laneCount.W)))
  selectedBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  survivorBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  evaluatedBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  collisionBySource := VecInit(Seq.fill(LocalMMUSourceClass.Count)(0.U(laneCount.W)))
  var occupiedLanes: Seq[Bool] = Seq.fill(laneCount)(false.B)
  var occupiedPhysicalInputs: Seq[Bool] = Seq.fill(bankCount)(false.B)
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
        if (!aLegacy && laneCount == 8 && aResponseChannels == laneCount) identityMask(source, tierEligible)
        else domainMask(source, aResponseChannels, aLegacy, tierEligible)
      case LocalMMUSourceClass.B =>
        if (!bLegacy && laneCount == 8 && bResponseChannels == laneCount) identityMask(source, tierEligible)
        else domainMask(source, bResponseChannels, bLegacy, tierEligible)
      case _ => VecInit((0 until laneCount).map(lane =>
        io.candidates(source)(lane).valid && tierEligible(lane))).asUInt
    }
    val evaluated = VecInit((0 until laneCount).map(lane =>
      io.candidates(source)(lane).valid && tierEligible(lane))).asUInt
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
    io.priorityBlocked(source) := offered & ~evaluatedBySource(source)
    io.selectedFire(source) := VecInit(io.candidates(source).map(_.fire)).asUInt
  }
  // Preserve the performance interface; no requests are held by this arbiter.
  io.held := 0.U
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
    selectedRequest := 0.U.asTypeOf(selectedRequest)
    for (source <- 0 until LocalMMUSourceClass.Count) {
      when(selected === source.U) {
        selectedRequest := io.candidates(source)(lane).bits.request
      }
    }

    io.llc(lane).valid := selected =/= LocalMMUSourceClass.None.U
    io.llc(lane).bits := selectedRequest
    io.selectedOwner(lane) := selected

    for (source <- 0 until LocalMMUSourceClass.Count) {
      io.candidates(source)(lane).ready := io.llc(lane).ready && selected === source.U
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
