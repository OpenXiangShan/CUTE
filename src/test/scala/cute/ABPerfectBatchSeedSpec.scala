package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ABPerfectBatchSeedHarness(
    enabled: Boolean = true,
    ordinalWidth: Int = 6) extends Module {
  val io = IO(new Bundle {
    val base = Input(UInt(9.W))
    val stride = Input(UInt(9.W))
    val k = Input(UInt(8.W))
    val minimum = Input(UInt(8.W))
    val streamLengths = Input(Vec(8, UInt(8.W)))
    val usePerLaneLengths = Input(Bool())
    val status = Output(UInt(3.W))
    val seeds = Output(Vec(8, UInt(ordinalWidth.W)))
  })
  val lengths = Mux(io.usePerLaneLengths,
    io.streamLengths,
    VecInit(Seq.fill(8)(io.minimum)))
  val result = ABPerfectBatchSeed.lookup(
    io.base, io.stride, io.k, lengths, enabled, ordinalWidth)
  io.status := result._1
  io.seeds := result._2
}

class ABPerfectBatchSeedSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ABPerfectBatchSeed"

  private val Success = ABPerfectBatchSeed.Success
  private val Disabled = ABPerfectBatchSeed.Disabled
  private val NoEntry = ABPerfectBatchSeed.NoEntry
  private val ShortStream = ABPerfectBatchSeed.ShortStream
  private val Invalid = ABPerfectBatchSeed.InvalidGeometry

  private def drive(dut: ABPerfectBatchSeedHarness, base: Int, stride: Int,
                    k: Int, minimum: Int): Unit = {
    dut.io.base.poke(base.U); dut.io.stride.poke(stride.U)
    dut.io.k.poke(k.U); dut.io.minimum.poke(minimum.U)
    dut.io.usePerLaneLengths.poke(false.B)
    dut.io.streamLengths.foreach(_.poke(minimum.U))
  }

  it should "return the deterministic phase-zero 144B seed" in {
    test(new ABPerfectBatchSeedHarness) { dut =>
      drive(dut, 0, 144, 8, 16)
      dut.io.status.expect(Success.U)
      Seq.fill(8)(0).zipWithIndex.foreach { case (seed, lane) =>
        dut.io.seeds(lane).expect(seed.U)
      }
    }
  }

  it should "classify fixed-bank, short, invalid, and disabled cases" in {
    test(new ABPerfectBatchSeedHarness) { dut =>
      drive(dut, 0, 64, 1, 128); dut.io.status.expect(NoEntry.U)
      drive(dut, 0, 0, 8, 1); dut.io.status.expect(ShortStream.U)
      drive(dut, 0, 145, 8, 16); dut.io.status.expect(Invalid.U)
    }
    test(new ABPerfectBatchSeedHarness(enabled = false)) { dut =>
      drive(dut, 0, 144, 8, 16); dut.io.status.expect(Disabled.U)
      (0 until 8).foreach(lane => dut.io.seeds(lane).expect(0.U))
    }
  }

  it should "be deterministic for repeated key lookups" in {
    test(new ABPerfectBatchSeedHarness) { dut =>
      drive(dut, 0, 144, 8, 16)
      val first = (0 until 8).map(i => dut.io.seeds(i).peek().litValue)
      dut.clock.step()
      val second = (0 until 8).map(i => dut.io.seeds(i).peek().litValue)
      assert(first == second)
    }
  }

  it should "reject a seed that is out of range on an individual short lane" in {
    test(new ABPerfectBatchSeedHarness) { dut =>
      drive(dut, 0, 0, 8, 16)
      dut.io.usePerLaneLengths.poke(true.B)
      dut.io.streamLengths.foreach(_.poke(16.U))
      dut.io.streamLengths(3).poke(1.U)
      dut.io.status.expect(ShortStream.U)
      dut.io.seeds.foreach(_.expect(0.U))
    }
  }
}
