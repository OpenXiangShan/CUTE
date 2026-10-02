package cute

import chisel3._
import difftest.{DifftestBundle, DifftestModule}
import difftest.common.DifftestWiring
import freechips.rocketchip.util.{AsyncQueueParams, AsyncQueueSink, AsyncQueueSource}
import org.chipsalliance.cde.config.Parameters

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
}
