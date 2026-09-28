package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random

object TransposeTestParams {
  def apply(banks: Int, mode: String = "8", diff: Boolean = false): Parameters =
    new Config((_, _, _) => {
      case CuteParamsKey => (if (banks == 8) CuteParams.CUTE_8Tops_128SCP else CuteParams.CUTE_2Tops).copy(
        Debug = CuteDebugParams.NoDebug, EnableDifftest = diff,
        LoaderBridgeChannelConfig = s"A${mode}B${mode}CLLCSL")
    })
}

class TransposeBytePlaneSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "SP-2 DFF transpose engine"
  private case class Pending(slot: Boolean, row: Int, address: Long)
  private val base = 0x1000L
  private val stride = 640L // aligned, non-power-of-two, crosses 4KB physically
  private def byteAt(r: Int, c: Int, b: Int): Int = (r * 37 + c * 11 + b * 53 + 0x81) & 255

  for (banks <- Seq(4, 8); ports <- Seq(1, banks)) {
    it should s"transpose tails and sustain overlapping fill/drain with $banks banks and $ports ports" in {
      test(new TransposeLoadEngine(ports)(TransposeTestParams(banks, ports.toString)))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          val rng = new Random(514 + banks + ports)
          dut.io.command.valid.poke(false.B)
          dut.io.done.ready.poke(false.B)
          dut.io.writeback.ready.poke(false.B)
          for (p <- 0 until ports) {
            dut.io.request(p).ready.poke(false.B)
            dut.io.response(p).valid.poke(false.B)
            dut.io.response(p).bits.slot.poke(false.B)
            dut.io.response(p).bits.row.poke(0.U)
            dut.io.response(p).bits.data.poke(0.U)
          }

          def run(rows: Int, columns: Int, lg: Int, stalls: Boolean): Unit = {
            val e = 1 << lg
            val expected = (for (r <- 0 until rows; c <- 0 until columns; b <- 0 until e) yield {
              ((c % banks, (c / banks) * 2 + (r * e) / 32, (r * e + b) % 32), byteAt(r, c, b))
            }).toMap
            val actual = mutable.Map.empty[(Int, Int, Int), Int]
            val requests = mutable.Set.empty[Long]
            val pending = mutable.ArrayBuffer.empty[Pending]
            dut.io.command.ready.expect(true.B)
            dut.io.command.bits.base.poke(base.U)
            dut.io.command.bits.stride.poke(stride.U)
            dut.io.command.bits.rows.poke(rows.U)
            dut.io.command.bits.columns.poke(columns.U)
            dut.io.command.bits.elementLgBytes.poke(lg.U)
            dut.io.command.valid.poke(true.B)
            dut.clock.step()
            dut.io.command.valid.poke(false.B)
            var cycle = 0
            var overlap = false
            var heldOutput: Option[BigInt] = None
            val heldRequests = Array.fill[Option[Pending]](ports)(None)
            while (!dut.io.done.valid.peek().litToBoolean && cycle < 4000) {
              val outReady = !stalls || rng.nextInt(4) != 0
              dut.io.writeback.ready.poke(outReady.B)
              val offered = (0 until ports).map(p =>
                if (stalls && rng.nextInt(5) == 0) None
                else rng.shuffle(pending.filter(_.row % ports == p).toSeq).headOption)
              for (p <- 0 until ports) {
                dut.io.request(p).ready.poke((!stalls || rng.nextInt(4) != 0).B)
                val resp = dut.io.response(p)
                resp.valid.poke(offered(p).nonEmpty.B)
                offered(p).foreach { m =>
                  val r = ((m.address - base) / stride).toInt
                  val colByte = ((m.address - base) % stride).toInt
                  val bits = (0 until 64).foldLeft(BigInt(0)) { (x, i) =>
                    x | (BigInt(byteAt(r, (colByte + i) / e, (colByte + i) % e)) << (8 * i))
                  }
                  resp.bits.slot.poke(m.slot.B)
                  resp.bits.row.poke(m.row.U)
                  resp.bits.data.poke(bits.U)
                }
              }
              var received = false
              for (p <- offered.indices if offered(p).nonEmpty && dut.io.response(p).ready.peek().litToBoolean) {
                assert(pending.contains(offered(p).get))
                pending -= offered(p).get
                received = true
              }
              for (p <- 0 until ports) {
                val req = dut.io.request(p)
                if (req.valid.peek().litToBoolean) {
                  val m = Pending(req.bits.slot.peek().litToBoolean, req.bits.row.peek().litValue.toInt,
                    req.bits.address.peek().litValue.toLong)
                  heldRequests(p).foreach(old => assert(m == old, "request changed under backpressure"))
                  if (req.ready.peek().litToBoolean) {
                    assert(!requests(m.address), s"duplicate address ${m.address}")
                    assert(!pending.exists(x => x.slot == m.slot && x.row == m.row), "slot reused too early")
                    requests += m.address
                    pending += m
                    heldRequests(p) = None
                  } else heldRequests(p) = Some(m)
                } else assert(heldRequests(p).isEmpty, "request withdrawn under backpressure")
              }
              val out = dut.io.writeback
              if (out.valid.peek().litToBoolean) {
                // Pack the hardware fields in software; avoid constructing
                // hardware (asUInt) in the test driver's simulation context.
                val signature = out.bits.data.map(_.peek().litValue).foldLeft(BigInt(0))((a, x) => (a << 256) | x) ^
                  out.bits.mask.map(_.peek().litValue).foldLeft(BigInt(0))((a, x) => (a << 32) | x)
                heldOutput.foreach(old => assert(old == signature, "writeback changed under backpressure"))
                heldOutput = if (outReady) None else Some(signature)
                if (outReady) {
                  overlap ||= received
                  for (bank <- 0 until banks) {
                    val addr = out.bits.address(bank).peek().litValue.toInt
                    val mask = out.bits.mask(bank).peek().litValue
                    val data = out.bits.data(bank).peek().litValue
                    for (b <- 0 until 32 if mask.testBit(b)) {
                      val key = (bank, addr, b)
                      assert(!actual.contains(key), s"duplicate write $key")
                      actual(key) = ((data >> (8 * b)) & 255).toInt
                    }
                  }
                }
              } else assert(heldOutput.isEmpty, "writeback withdrawn under backpressure")
              dut.clock.step()
              cycle += 1
            }
            assert(cycle < 4000, s"timeout rows=$rows columns=$columns lg=$lg pending=$pending")
            assert(pending.isEmpty)
            assert(requests.size == rows * ((columns * e + 63) / 64))
            assert(actual.toMap == expected, s"transpose mismatch rows=$rows columns=$columns lg=$lg")
            dut.io.command.ready.expect(false.B)
            dut.clock.step(3)
            dut.io.done.valid.expect(true.B) // completion must wait for its consumer
            dut.io.done.ready.poke(true.B)
            dut.clock.step()
            dut.io.done.ready.poke(false.B)
            dut.io.command.ready.expect(true.B)
            if (!stalls && rows * e == 64 && columns == (if (banks == 8) 128 else 64)) {
              val groups = ((rows + 7) / 8) * ((columns * e + 63) / 64)
              val ideal = math.max(8 / ports, 64 / (banks * e))
              assert(cycle <= 10 + groups * (ideal + 4), s"unexpected throughput regression: $cycle cycles")
              assert(overlap, "ping-pong never overlapped fill and output")
              println(f"SP2_PERF banks=$banks ports=$ports e${8 * e} bytes=${rows * columns * e} cycles=$cycle B/cycle=${rows * columns * e.toDouble / cycle}%.2f")
            }
          }
          for (lg <- 0 to 2) {
            run(0, 0, lg, stalls = true)
            run(1, 1, lg, stalls = true)
            run(7, 9, lg, stalls = true)
            run(9, 17, lg, stalls = true)
            run(64 / (1 << lg) - 1, (if (banks == 8) 127 else 63), lg, stalls = true)
            run(64 / (1 << lg), (if (banks == 8) 128 else 64), lg, stalls = false)
          }
        }
    }
  }
}
