package cute

import chisel3._
import difftest.{DiffAmuWritebackEvent, DiffAmuHashEvent, DifftestBundle, DifftestModule}
import difftest.common.DifftestWiring
import difftest.util.MatrixHash
import freechips.rocketchip.util.{AsyncQueueParams, AsyncQueueSink, AsyncQueueSource}
import org.chipsalliance.cde.config.{Field, Parameters}

/** Difftest event transport for a CUTE clock domain that may be asynchronous to the core. */
object CuteDifftest {
  private def clockName(tag: String): String = s"${tag}_difftest_clock"
  private def resetName(tag: String): String = s"${tag}_difftest_reset"

  def publishSinkClock(tag: String, clock: Clock, reset: Bool): Unit = {
    DifftestWiring.addSource(clock, clockName(tag))
    DifftestWiring.addSource(reset, resetName(tag))
  }

  def apply[T <: DifftestBundle](
    gen: T,
    dontCare: Boolean = false,
    delay: Int = 0
  )(implicit p: Parameters): T = {
    p(CuteParamsKey).DifftestAsyncClockName match {
      case None => DifftestModule(gen, dontCare = dontCare, delay = delay)
      case Some(tag) =>
        val event = Wire(gen)
        if (dontCare) {
          event := DontCare
          event.bits.getValidOption.foreach(_ := false.B)
        }

        val sinkClock = Wire(Clock())
        val sinkReset = Wire(Bool())
        DifftestWiring.addSink(sinkClock, clockName(tag))
        DifftestWiring.addSink(sinkReset, resetName(tag))

        val params = AsyncQueueParams(depth = 64, sync = 3, safe = true)
        val source = Module(new AsyncQueueSource(chiselTypeOf(event), params))
        val sink = withClockAndReset(sinkClock, sinkReset.asAsyncReset) {
          Module(new AsyncQueueSink(chiselTypeOf(event), params))
        }
        source.io.async <> sink.io.async
        val eventValid = event.bits.getValidOption.getOrElse(
          throw new IllegalArgumentException(s"${gen.desiredModuleName} must provide a valid field for CDC transport")
        )
        source.io.enq.valid := eventValid
        source.io.enq.bits := event
        assert(!eventValid || source.io.enq.ready, "CUTE Difftest CDC queue overflow")

        sink.io.deq.ready := true.B
        val gatewayEvent = withClockAndReset(sinkClock, sinkReset.asAsyncReset) {
          DifftestModule(gen, dontCare = false, delay = delay)
        }
        gatewayEvent := sink.io.deq.bits
        gatewayEvent.bits.getValidOption.get := sink.io.deq.valid
        event
    }
  }

  def applyVec[T <: DifftestBundle](
    gens: Seq[T],
    dontCare: Boolean = false,
    delay: Int = 0
  )(implicit p: Parameters): Seq[T] = {
    gens.map(gen => apply(gen, dontCare = dontCare, delay = delay))
  }

  def matrixHash(
    banks: Int,
    bankBytes: Int,
    regBytes: Int,
    rowBytes: Int
  )(implicit p: Parameters): DiffAmuWritebackEvent =
    matrixHash(banks, bankBytes, regBytes, rowBytes, ports = 1).head

  def matrixHash(
    banks: Int,
    bankBytes: Int,
    regBytes: Int,
    rowBytes: Int,
    ports: Int
  )(implicit p: Parameters): Vec[DiffAmuWritebackEvent] = {
    val module = Module(new MatrixHash(banks, bankBytes, regBytes, rowBytes, ports))
    val writes = Wire(Vec(ports, new DiffAmuWritebackEvent(banks, bankBytes / 8)))
    module.io.in := writes
    // Combine concurrent writes in the accelerator domain, then transport
    // only the instruction's compact completion event across the clock boundary.
    apply(new DiffAmuHashEvent) := module.io.out
    writes
  }
}

/** Matrix-register write probe configuration resolved before hardware generation. */
object MatrixRegWriteProbes {
  private case object ProbeParamsKey extends Field[ProbeParams]

  case class MatrixRegWriteProbe(
    useHash: Boolean,
    banks: CuteParams => Int,
    bankBytes: CuteParams => Int,
    regBytes: CuteParams => Int,
    rowBytes: CuteParams => Int
  )

  case class Params(
    aLoad: MatrixRegWriteProbe = abLoadHash,
    bLoad: MatrixRegWriteProbe = abLoadHash,
    cLoad: MatrixRegWriteProbe = cLoadHash,
    mma: MatrixRegWriteProbe = MatrixRegWriteProbes.mma
  )(implicit p: Parameters) {
    private val probes = Seq(aLoad, bLoad, cLoad, mma)
    val writebackWordsPerBank: Int = maxWritebackWordsPerBank(probes)

    def parameters(description: MatrixRegWriteProbe): Parameters = {
      require(probes.contains(description), "Matrix write probe must belong to this configuration")
      if (!p(CuteParamsKey).EnableDifftest) p
      else {
        val params = ProbeParams(description, writebackWordsPerBank)
        // Forward queries directly: XSAI may supply a frozen Parameters cache.
        Parameters((_, _, _) => {
          case ProbeParamsKey => params
          case key: Field[_] if p.lift(key).isDefined => p(key)
        })
      }
    }
  }

  private case class ProbeParams(description: MatrixRegWriteProbe, writebackWordsPerBank: Int) {
    def genWriteProbes(ports: Int)(implicit p: Parameters): Seq[DiffAmuWritebackEvent] = {
      require(ports > 0, "Matrix probes require at least one port")
      val params = p(CuteParamsKey)
      if (description.useHash) {
        CuteDifftest.matrixHash(description.banks(params), description.bankBytes(params),
          description.regBytes(params), description.rowBytes(params), ports).toSeq
      } else {
        Seq.fill(ports)(CuteDifftest(
          new DiffAmuWritebackEvent(description.banks(params), writebackWordsPerBank), dontCare = true
        ))
      }
    }
  }

  def genWriteProbes(default: MatrixRegWriteProbe, ports: Int = 1)
    (implicit p: Parameters): Seq[DiffAmuWritebackEvent] = {
    require(p(CuteParamsKey).EnableDifftest, "Matrix write probes require DiffTest")
    val params = p.lift(ProbeParamsKey).getOrElse {
      // A standalone producer derives its width from its own description.
      ProbeParams(default, maxWritebackWordsPerBank(Seq(default)))
    }
    params.genWriteProbes(ports)
  }

  private def maxWritebackWordsPerBank(probes: Seq[MatrixRegWriteProbe])(implicit p: Parameters): Int = {
    val params = p(CuteParamsKey)
    if (!params.EnableDifftest) 0
    else {
      val writes = probes.filterNot(_.useHash)
      require(writes.map(_.banks(params)).distinct.size <= 1, "Writeback probes must share a bank count")
      val wordBits = writebackWordBits
      val widths = writes.map(_.bankBytes(params) * 8)
      require(widths.forall(bits => bits > 0 && bits % wordBits == 0),
        s"Writeback bank width must be positive and $wordBits-bit aligned")
      (widths :+ 0).max / wordBits
    }
  }

  val abLoadHash: MatrixRegWriteProbe = MatrixRegWriteProbe(
    useHash = true,
    banks = _.ABMatrixRegNBanks,
    bankBytes = _.ABMatrixRegEntryByteSize,
    regBytes = _.ABMatrixRegSize,
    rowBytes = _.Tensor_K
  )
  val cLoadHash: MatrixRegWriteProbe = MatrixRegWriteProbe(
    useHash = true,
    banks = _.CMatrixRegNBanks,
    bankBytes = _.CMatrixRegEntryByteSize,
    regBytes = _.CMatrixRegSize,
    rowBytes = params => params.Tensor_MN * params.ResultWidthByte
  )
  val abLoadNoHash: MatrixRegWriteProbe = abLoadHash.copy(useHash = false)
  val cLoadNoHash: MatrixRegWriteProbe = cLoadHash.copy(useHash = false)
  val mma: MatrixRegWriteProbe = cLoadHash.copy(useHash = false)

  def writebackWordBits: Int = (new DiffAmuWritebackEvent).data.head.getWidth
}
