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
  val debugEnable: Boolean = false
) extends Module {
  require(isPow2(n) && n >= 2, "n must be a power of 2 and >= 2")
  val stages = log2Ceil(n)
  require(bankIdWidth >= stages,
    s"bankIdWidth ($bankIdWidth) must be >= log2(n) ($stages) to address all $n channels")
  require(bankIdOffset >= 0, s"bankIdOffset ($bankIdOffset) must be >= 0")
  require(bankIdOffset + bankIdWidth <= sourceIdWidth,
    s"bankId field [$bankIdOffset + $bankIdWidth - 1 : $bankIdOffset] exceeds sourceIdWidth ($sourceIdWidth)")

  val regAddrWidth = sourceIdWidth - bankIdWidth

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val in = Flipped(Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth))))
    val out = Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))
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

  /** Get the routing bit for a given stage (MSB first). */
  def routingBit(bankId: UInt, stage: Int): Bool = {
    bankId(stages - 1 - stage).asBool
  }

  // Log inputs at the top level
  for (i <- 0 until n) {
    when(io.in(i).valid) {
      val bid = extractBankId(io.in(i).bits.sourceId)
      log(cf"in[$i] valid bankId=$bid sourceId=0x${io.in(i).bits.sourceId}%x")
    }
  }

  // Instantiate all switches stage-by-stage.
  // stageSwitches(stage)(switchIdx)
  val stageSwitches = Seq.fill(stages) {
    Seq.fill(n / 2)(Module(new OmegaSwitch(new RoutedResponse(dataWidth, sourceIdWidth))))
  }

  // ---- Stage 0 wiring (inputs come directly from io.in) ----
  for (sw <- 0 until n / 2) {
    val switch = stageSwitches(0)(sw)

    switch.io.in(0) <> io.in(sw * 2)
    switch.io.in(1) <> io.in(sw * 2 + 1)

    switch.io.sel(0) := routingBit(extractBankId(io.in(sw * 2).bits.sourceId), 0)
    switch.io.sel(1) := routingBit(extractBankId(io.in(sw * 2 + 1).bits.sourceId), 0)
  }

  // ---- Intermediate stages wiring ----
  for (stage <- 1 until stages) {
    for (sw <- 0 until n / 2) {
      val switch = stageSwitches(stage)(sw)
      val prevSwitches = stageSwitches(stage - 1)

      // stage k switch j input inIdx comes from stage k-1 switch
      // (inIdx * (n/4) + j/2) output (j % 2).
      val srcSw0 = sw / 2
      val srcSw1 = sw / 2 + n / 4
      val srcOut = sw % 2

      switch.io.in(0).valid := prevSwitches(srcSw0).io.out(srcOut).valid
      switch.io.in(0).bits  := prevSwitches(srcSw0).io.out(srcOut).bits
      switch.io.in(1).valid := prevSwitches(srcSw1).io.out(srcOut).valid
      switch.io.in(1).bits  := prevSwitches(srcSw1).io.out(srcOut).bits

      prevSwitches(srcSw0).io.out(srcOut).ready := switch.io.in(0).ready
      prevSwitches(srcSw1).io.out(srcOut).ready := switch.io.in(1).ready

      switch.io.sel(0) := routingBit(extractBankId(switch.io.in(0).bits.sourceId), stage)
      switch.io.sel(1) := routingBit(extractBankId(switch.io.in(1).bits.sourceId), stage)

      // Log switch-level arbitration events
      if (debugEnable) {
        val in0Bid = extractBankId(switch.io.in(0).bits.sourceId)
        val in1Bid = extractBankId(switch.io.in(1).bits.sourceId)
        val conflict = switch.io.in(0).valid && switch.io.in(1).valid &&
                       routingBit(in0Bid, stage) === routingBit(in1Bid, stage)
        when(conflict) {
          log(cf"stage=$stage sw=$sw CONFLICT in0Bank=$in0Bid in1Bank=$in1Bid -> in0 wins")
        }
      }
    }
  }

  // ---- Final output wiring ----
  // Because every stage performs an unshuffle (stride-by-n/2), after
  // log2Ceil(n) stages the output order is identical to the input order.
  // Thus io.out(j) is simply wired to last stage switch j/2 output j%2.
  val lastSwitches = stageSwitches(stages - 1)
  for (j <- 0 until n) {
    val srcSw  = j / 2
    val srcOut = j % 2

    io.out(j).valid := lastSwitches(srcSw).io.out(srcOut).valid
    io.out(j).bits  := lastSwitches(srcSw).io.out(srcOut).bits
    lastSwitches(srcSw).io.out(srcOut).ready := io.out(j).ready
  }

  // Log outputs at the top level
  for (j <- 0 until n) {
    when(io.out(j).valid) {
      val bid = extractBankId(io.out(j).bits.sourceId)
      log(cf"out[$j] valid bankId=$bid sourceId=0x${io.out(j).bits.sourceId}%x ready=${io.out(j).ready}")
    }
  }
}
