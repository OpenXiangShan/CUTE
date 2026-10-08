package cute

import chisel3._
import chisel3.util._

/**
  * Response bundle for omega network routing.
  * The bankId is stored in a configurable bit-range of sourceId.
  */
class RoutedResponse(val dataWidth: Int, val sourceIdWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val sourceId = UInt(sourceIdWidth.W)
  val coherent = Bool()
}

class OmegaRouteToken(val laneCount: Int) extends Bundle {
  val valid = Bool()
  val destinationLane = UInt(log2Ceil(laneCount max 2).W)
  val origin = UInt(laneCount.W)
}

/** Shared topology equations used by both admission prediction and response routing. */
object OmegaTopology {
  def stageCount(laneCount: Int): Int = log2Ceil(laneCount)

  def perfectShuffle(index: Int, laneCount: Int): Int = {
    require(laneCount > 1)
    ((index << 1) & (laneCount - 1)) | (index >> (stageCount(laneCount) - 1))
  }

  def destinationLane(bankId: UInt, laneCount: Int, outputCount: Int, bankCount: Int): UInt = {
    val group = ResponseChannelHelper.groupIdOfBank(bankId, outputCount, bankCount)
    val laneStride = laneCount / outputCount
    if (laneStride == 1) group else (group << log2Ceil(laneStride))(log2Ceil(laneCount) - 1, 0)
  }

  def routeBit(destinationLane: UInt, laneCount: Int, stageIndex: Int): Bool = {
    destinationLane(stageCount(laneCount) - 1 - stageIndex)
  }

  /**
    * Predict the origins accepted by an Omega fabric when every exposed output is ready.
    * Tokens are removed at the exact stage where the router's static switch arbitration
    * would reject them, so a loser can never reappear at a later stage.
    */
  def survivorMask(valid: Seq[Bool], destinationLanes: Seq[UInt]): UInt = {
    require(valid.nonEmpty && isPow2(valid.size))
    require(valid.size == destinationLanes.size)
    val laneCount = valid.size
    if (laneCount == 1) {
      valid.head.asUInt
    } else {
      val stages = Seq.fill(stageCount(laneCount) + 1)(Wire(Vec(laneCount, new OmegaRouteToken(laneCount))))
      for (lane <- 0 until laneCount) {
        stages.head(lane).valid := valid(lane)
        stages.head(lane).destinationLane := destinationLanes(lane)
        stages.head(lane).origin := UIntToOH(lane.U, laneCount)
      }

      for (stage <- 0 until stageCount(laneCount)) {
        for (switch <- 0 until laneCount / 2) {
          val topIndex = switch * 2
          val bottomIndex = topIndex + 1
          val top = stages(stage)(topIndex)
          val bottom = stages(stage)(bottomIndex)
          val topSelect = routeBit(top.destinationLane, laneCount, stage)
          val bottomSelect = routeBit(bottom.destinationLane, laneCount, stage)
          val topWinsOut0 = top.valid && !topSelect
          val bottomWinsOut0 = bottom.valid && !bottomSelect && !topWinsOut0
          val bottomWinsOut1 = bottom.valid && bottomSelect
          val topWinsOut1 = top.valid && topSelect && !bottomWinsOut1
          val out0 = Wire(new OmegaRouteToken(laneCount))
          val out1 = Wire(new OmegaRouteToken(laneCount))

          out0 := Mux(topWinsOut0, top, bottom)
          out0.valid := topWinsOut0 || bottomWinsOut0
          out1 := Mux(bottomWinsOut1, bottom, top)
          out1.valid := bottomWinsOut1 || topWinsOut1

          val out0Index = if (stage == stageCount(laneCount) - 1) topIndex else perfectShuffle(topIndex, laneCount)
          val out1Index = if (stage == stageCount(laneCount) - 1) bottomIndex else perfectShuffle(bottomIndex, laneCount)
          stages(stage + 1)(out0Index) := out0
          stages(stage + 1)(out1Index) := out1
        }
      }

      stages.last.map(token => Mux(token.valid, token.origin, 0.U)).reduce(_ | _)
    }
  }

  /**
    * Admit routes in physical-input order without allowing a newcomer to
    * displace an already reserved route.  The returned bits retain physical
    * input identity, just like [[survivorMask]].
    */
  def incrementalCompatibleMask(
    valid: Seq[Bool],
    destinationLanes: Seq[UInt],
    reserved: Seq[Bool]
  ): UInt = {
    require(valid.nonEmpty && isPow2(valid.size))
    require(valid.size == destinationLanes.size)
    require(valid.size == reserved.size)

    var accepted = reserved
    for (physicalInput <- valid.indices) {
      val newcomer = valid(physicalInput) && !reserved(physicalInput)
      val tentative = accepted.updated(
        physicalInput,
        accepted(physicalInput) || newcomer
      )
      val tentativeSurvivors = survivorMask(tentative, destinationLanes)
      val preservesAccepted = accepted.zipWithIndex.map { case (route, input) =>
        !route || tentativeSurvivors(input)
      }.reduce(_ && _)
      val acceptNewcomer = newcomer &&
        tentativeSurvivors(physicalInput) && preservesAccepted
      accepted = accepted.updated(
        physicalInput,
        accepted(physicalInput) || acceptNewcomer
      )
    }
    VecInit(accepted).asUInt
  }
}

object VerilogNameHelper {
  def sanitize(raw: String): String = {
    val cleaned = raw.replaceAll("[^A-Za-z0-9_]", "_")
    if (cleaned.isEmpty || !cleaned.head.isLetter) s"CUTE_$cleaned" else cleaned
  }
}

/**
  * A single 2x2 switch for the omega network.
  *
  * Each input is routed to either the top output (out(0)) or bottom output (out(1))
  * based on its `sel` bit.  If both inputs target the same output, in0 wins and
  * in1 is back-pressured.
  */
class OmegaSwitch[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(2, Decoupled(gen)))
    val out = Vec(2, Decoupled(gen))
    val sel = Input(Vec(2, Bool())) // false -> out(0), true -> out(1)
  })

  // Defaults
  io.in(0).ready := false.B
  io.in(1).ready := false.B
  io.out(0).valid := false.B
  io.out(1).valid := false.B
  io.out(0).bits := DontCare
  io.out(1).bits := DontCare

  // Routing decisions
  val in0ToOut0 = io.in(0).valid && !io.sel(0)
  val in0ToOut1 = io.in(0).valid &&  io.sel(0)
  val in1ToOut0 = io.in(1).valid && !io.sel(1)
  val in1ToOut1 = io.in(1).valid &&  io.sel(1)

  // Arbitration: in0 has priority on conflicts
  val in0GoesOut0 = in0ToOut0
  val in1GoesOut0 = in1ToOut0 && !in0ToOut0
  val in0GoesOut1 = in0ToOut1 && !in1ToOut1
  val in1GoesOut1 = in1ToOut1

  // out(0)
  io.out(0).valid := in0GoesOut0 || in1GoesOut0
  io.out(0).bits := Mux(in0GoesOut0, io.in(0).bits, io.in(1).bits)

  // out(1)
  io.out(1).valid := in0GoesOut1 || in1GoesOut1
  io.out(1).bits := Mux(in1GoesOut1, io.in(1).bits, io.in(0).bits)

  // Ready back-pressure
  io.in(0).ready := (in0GoesOut0 && io.out(0).ready) || (in0GoesOut1 && io.out(1).ready)
  io.in(1).ready := (in1GoesOut0 && io.out(0).ready) || (in1GoesOut1 && io.out(1).ready)
}

/**
  * Omega-network based response router.
  *
  * Routes responses from LLC channels to MemoryLoader channels based on the
  * bankId embedded in a configurable bit-range of sourceId.
  *
  * @param n             Number of channels (must be power of 2)
  * @param dataWidth     Width of response data
  * @param sourceIdWidth Width of sourceId
  * @param bankIdWidth   Width of the bankId field within sourceId
  * @param bankIdOffset  Bit offset of the bankId field within sourceId (LSB position)
  * @param debugEnable   When true, enables cycle-by-cycle printf logs
  */
class OmegaResponseRouter(
  val n: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val bankIdWidth: Int,
  val bankIdOffset: Int = 0,
  val outCount: Int = -1,
  val bankCount: Int = -1,
  val debugEnable: Boolean = false,
  val contextName: String = "Generic",
  val traceTag: UInt = 0.U,
  val responseUsesAllocatedSourceId: Boolean = false,
  val allocatedSourceIdWidth: Int = 0
) extends Module {
  private val outputCount = if (outCount == -1) n else outCount
  private val totalBanks = if (bankCount == -1) n else bankCount
  private val nameContext = VerilogNameHelper.sanitize(contextName)

  override def desiredName: String = s"OmegaResponseRouter_${nameContext}_${n}to${outputCount}_banks${totalBanks}"

  require(isPow2(n) && n >= 1, s"input channel count ($n) must be a power of 2 and >= 1")
  require(isPow2(outputCount) && outputCount >= 1, s"output channel count ($outputCount) must be a power of 2 and >= 1")
  require(isPow2(totalBanks) && totalBanks >= 1, s"bankCount ($totalBanks) must be a power of 2 and >= 1")
  require(totalBanks >= outputCount, s"bankCount ($totalBanks) must be >= outputCount ($outputCount)")
  require(totalBanks % outputCount == 0, s"bankCount ($totalBanks) must be divisible by outputCount ($outputCount)")
  require(bankIdWidth >= log2Ceil(totalBanks),
    s"bankIdWidth ($bankIdWidth) must be >= log2(bankCount) (${log2Ceil(totalBanks)}) to address all $totalBanks banks")
  require(bankIdOffset >= 0, s"bankIdOffset ($bankIdOffset) must be >= 0")
  require(bankIdOffset + bankIdWidth <= sourceIdWidth,
    s"bankId field [$bankIdOffset + $bankIdWidth - 1 : $bankIdOffset] exceeds sourceIdWidth ($sourceIdWidth)")

  private val outputIdxWidth = log2Ceil(outputCount max 2)
  private val bankIdxWidth = log2Ceil(totalBanks max 2)
  private val stageCount = log2Ceil(n)
  private val networkIdxWidth = log2Ceil(n max 2)
  private val laneStride = n / outputCount
  private val laneOffsetWidth = log2Ceil(laneStride)
  private val exposedOutputLanes = (0 until outputCount).map(_ * laneStride).toSet

  require(outputCount <= n, s"output channel count ($outputCount) must be <= input channel count ($n)")
  require(n % outputCount == 0, s"input channel count ($n) must be divisible by output channel count ($outputCount)")
  require(!responseUsesAllocatedSourceId || allocatedSourceIdWidth > 0,
    "allocatedSourceIdWidth must be positive when responseUsesAllocatedSourceId is true")

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val in = Flipped(Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth))))
    val out = Vec(outputCount, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))
  })

  /** Local log helper with unified timestamp format. */
  private def log(msg: => Printable): Unit = if (debugEnable) printf(msg + "\n")



  /** Extract bankId from the configured bit-range of sourceId. */
  def extractBankId(sourceId: UInt): UInt = {
    if (bankIdWidth > 0) {
      sourceId(bankIdOffset + bankIdWidth - 1, bankIdOffset)
    } else {
      0.U(1.W)
    }
  }

  /** Normalize bankId width before group mapping. */
  def normalizedBankId(sourceId: UInt): UInt = {
    if (bankIdxWidth == 0) {
      0.U(1.W)
    } else {
      extractBankId(sourceId)(bankIdxWidth - 1, 0)
    }
  }

  /** Route to per-bank output for 8-channel mode, or to contiguous group output for 2/4-channel mode. */
  def destinationOf(sourceId: UInt): UInt = {
    val bankId = normalizedBankId(sourceId)
    if (outputCount == totalBanks) {
      bankId(outputIdxWidth - 1, 0)
    } else {
      ResponseChannelHelper.groupIdOfBank(bankId, outputCount, totalBanks)
    }
  }

  /** Left-align the exposed output group into the internal n-lane omega fabric. */
  def destinationLaneOf(sourceId: UInt): UInt = {
    val destination = destinationOf(sourceId)
    if (laneOffsetWidth == 0) {
      destination
    } else {
      (destination << laneOffsetWidth)(networkIdxWidth - 1, 0)
    }
  }

  private def routeBit(sourceId: UInt, stageIdx: Int): Bool =
    OmegaTopology.routeBit(destinationLaneOf(sourceId), n, stageIdx)

  val inDest = Wire(Vec(n, UInt(outputIdxWidth.W)))
  for (i <- 0 until n) {
    inDest(i) := destinationOf(io.in(i).bits.sourceId)
    when(io.in(i).fire) {
      val bid = normalizedBankId(io.in(i).bits.sourceId)
      log(cf"in[$i] fire bankId=$bid dest=${inDest(i)} sourceId=0x${io.in(i).bits.sourceId}%x")
    }

  }

  if (n == 1) {
    io.out(0).valid := io.in(0).valid
    io.out(0).bits := io.in(0).bits
    io.in(0).ready := io.out(0).ready
  } else {
    val fabric = Seq.fill(stageCount + 1)(Wire(Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))))

    for (i <- 0 until n) {
      fabric.head(i).valid := io.in(i).valid
      fabric.head(i).bits := io.in(i).bits
      io.in(i).ready := fabric.head(i).ready
    }

    for (stageIdx <- 0 until stageCount) {
      for (switchIdx <- 0 until (n / 2)) {
        val switch = Module(new OmegaSwitch(new RoutedResponse(dataWidth, sourceIdWidth)))
        val topIn = switchIdx * 2
        val bottomIn = topIn + 1

        switch.io.in(0) <> fabric(stageIdx)(topIn)
        switch.io.in(1) <> fabric(stageIdx)(bottomIn)
        switch.io.sel(0) := routeBit(fabric(stageIdx)(topIn).bits.sourceId, stageIdx)
        switch.io.sel(1) := routeBit(fabric(stageIdx)(bottomIn).bits.sourceId, stageIdx)

        when(fabric(stageIdx)(topIn).valid && fabric(stageIdx)(bottomIn).valid &&
          switch.io.sel(0) === switch.io.sel(1)) {
          log(cf"stage[$stageIdx].switch[$switchIdx] conflict sel=${switch.io.sel(0)}")
        }

        for (port <- 0 until 2) {
          val switchOut = topIn + port
          val nextIdx = if (stageIdx == stageCount - 1) switchOut else OmegaTopology.perfectShuffle(switchOut, n)
          fabric(stageIdx + 1)(nextIdx) <> switch.io.out(port)
        }
      }
    }

    for (outIdx <- 0 until outputCount) {
      val laneIdx = outIdx * laneStride
      io.out(outIdx).valid := fabric.last(laneIdx).valid
      io.out(outIdx).bits := fabric.last(laneIdx).bits
      fabric.last(laneIdx).ready := io.out(outIdx).ready
    }

    for (laneIdx <- 0 until n if !exposedOutputLanes.contains(laneIdx)) {
      fabric.last(laneIdx).ready := true.B
      assert(!fabric.last(laneIdx).valid, s"OmegaResponseRouter routed a response to unused output $laneIdx")
    }
  }

    for (outIdx <- 0 until outputCount) {
      when(io.out(outIdx).fire) {
        val bid = normalizedBankId(io.out(outIdx).bits.sourceId)
        log(cf"out[$outIdx] fire bankId=$bid sourceId=0x${io.out(outIdx).bits.sourceId}%x")
      }

  }
}
