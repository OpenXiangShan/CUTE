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
  val debugEnable: Boolean = false
) extends Module {
  private val outputCount = if (outCount == -1) n else outCount
  private val totalBanks = if (bankCount == -1) n else bankCount

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

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val in = Flipped(Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth))))
    val out = Vec(outputCount, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))
  })

  /** Local log helper with unified timestamp format. */
  private def log(msg: => Printable): Unit = {
    if (debugEnable) printf(cf"[T${io.timeStamp}][OmegaRouter] " + msg + "\n")
  }

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

  val inDest = Wire(Vec(n, UInt(outputIdxWidth.W)))
  for (i <- 0 until n) {
    inDest(i) := destinationOf(io.in(i).bits.sourceId)
    when(io.in(i).valid) {
      val bid = normalizedBankId(io.in(i).bits.sourceId)
      log(cf"in[$i] valid bankId=$bid dest=${inDest(i)} sourceId=0x${io.in(i).bits.sourceId}%x")
    }
  }

  val selectOHs = Seq.fill(outputCount)(Wire(Vec(n, Bool())))

  for (i <- 0 until n) {
    io.in(i).ready := false.B
  }

  for (outIdx <- 0 until outputCount) {
    val candidateBits = VecInit((0 until n).map(i => io.in(i).valid && inDest(i) === outIdx.U)).asUInt
    val selectOH = PriorityEncoderOH(candidateBits)
    val outValid = candidateBits.orR

    io.out(outIdx).valid := outValid
    io.out(outIdx).bits := 0.U.asTypeOf(io.out(outIdx).bits)
    when(outValid) {
      io.out(outIdx).bits := Mux1H(selectOH, io.in.map(_.bits))
    }

    for (i <- 0 until n) {
      selectOHs(outIdx)(i) := selectOH(i)
    }

    when(PopCount(candidateBits) > 1.U) {
      val winner = PriorityEncoder(candidateBits)
      log(cf"out[$outIdx] conflict candidates=0x${Hexadecimal(candidateBits)} winner=in[$winner]")
    }
  }

  for (i <- 0 until n) {
    val readyCases = (0 until outputCount).map(outIdx => selectOHs(outIdx)(i) -> io.out(outIdx).ready)
    io.in(i).ready := MuxCase(false.B, readyCases)
    assert(
      PopCount(VecInit((0 until outputCount).map(outIdx => selectOHs(outIdx)(i))).asUInt) <= 1.U,
      s"OmegaResponseRouter input $i selected by multiple outputs"
    )
  }

  for (outIdx <- 0 until outputCount) {
    when(io.out(outIdx).valid) {
      val bid = normalizedBankId(io.out(outIdx).bits.sourceId)
      log(cf"out[$outIdx] valid bankId=$bid sourceId=0x${io.out(outIdx).bits.sourceId}%x ready=${io.out(outIdx).ready}")
    }
  }
}
