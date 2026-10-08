package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CMLPerfectBatchSelectorHarness extends Module {
  private val laneCount = 8
  val io = IO(new Bundle {
    val valid = Input(Vec(laneCount, Bool()))
    val address = Input(Vec(laneCount, UInt(64.W)))
    val destination = Input(Vec(laneCount, UInt(3.W)))
    val retained = Input(Vec(laneCount, Bool()))
    val seedPointer = Input(UInt(3.W))
    val selected = Output(UInt(laneCount.W))
  })

  io.selected := CMLPerfectBatchSelector.select(
    io.valid,
    io.address,
    io.destination,
    io.retained,
    io.seedPointer,
    l2BankCount = 8)
}

class CMLPerfectBatchSelectorSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CMLPerfectBatchSelector"

  private def drive(
    dut: CMLPerfectBatchSelectorHarness,
    valid: Seq[Boolean],
    banks: Seq[Int],
    destinations: Seq[Int],
    retained: Seq[Boolean] = Seq.fill(8)(false),
    seed: Int = 0
  ): Unit = {
    for (lane <- 0 until 8) {
      dut.io.valid(lane).poke(valid(lane).B)
      dut.io.address(lane).poke((banks(lane) << 6).U)
      dut.io.destination(lane).poke(destinations(lane).U)
      dut.io.retained(lane).poke(retained(lane).B)
    }
    dut.io.seedPointer.poke(seed.U)
  }

  private def expectedSelection(
    valid: Seq[Boolean],
    banks: Seq[Int],
    destinations: Seq[Int],
    retained: Seq[Boolean],
    seed: Int
  ): BigInt = {
    var selected = retained.indices.filter(retained).toSet
    if (selected.isEmpty) {
      (0 until 8).map(offset => (seed + offset) % 8).find(valid).foreach { lane =>
        selected += lane
      }
    }
    for (lane <- 0 until 8 if valid(lane) && !selected(lane)) {
      val tentative = selected + lane
      val uniqueBanks = tentative.map(banks).size == tentative.size
      val tentativeValid = (0 until 8).map(tentative)
      val survivor = LocalMMUSchedulerTestUtils.physicalSurvivorMask(
        tentativeValid, banks, destinations, responseChannels = 8)
      val tentativeMask = tentative.foldLeft(BigInt(0))((mask, owner) => mask.setBit(owner))
      if (uniqueBanks && (survivor & tentativeMask) == tentativeMask) selected = tentative
    }
    if (selected.nonEmpty) {
      selected.foldLeft(BigInt(0))((mask, lane) => mask.setBit(lane))
    } else {
      valid.indexWhere(identity) match {
        case -1 => BigInt(0)
        case lane => BigInt(1) << lane
      }
    }
  }

  it should "preserve every retained seed and add only compatible requests" in {
    test(new CMLPerfectBatchSelectorHarness) { dut =>
      val valid = Seq.fill(8)(true)
      val banks = 0 until 8
      val destinations = Seq(0, 4, 1, 5, 2, 6, 3, 7)
      val retained = Seq(true, false, false, false, false, true, false, false)
      drive(dut, valid, banks, destinations, retained)
      val selected = dut.io.selected.peek().litValue
      assert((selected & 0x21) == 0x21)
      val selectedLanes = (0 until 8).filter(selected.testBit)
      assert(selectedLanes.map(banks).distinct.size == selectedLanes.size)
      val expectedSurvivors = LocalMMUSchedulerTestUtils.survivorMask(
        (0 until 8).map(selected.testBit), destinations, responseChannels = 8)
      assert((expectedSurvivors & selected) == selected)
    }
  }

  it should "rotate the progress seed and always select non-empty work" in {
    test(new CMLPerfectBatchSelectorHarness) { dut =>
      val valid = Seq.fill(8)(true)
      val sameBanks = Seq.fill(8)(0)
      val destinations = 0 until 8
      for (seed <- 0 until 8) {
        drive(dut, valid, sameBanks, destinations, seed = seed)
        val selected = dut.io.selected.peek().litValue
        assert(selected == (BigInt(1) << seed))
      }
    }
  }

  it should "keep conflicting retained requests for deterministic fallback" in {
    test(new CMLPerfectBatchSelectorHarness) { dut =>
      val retained = Seq(true, true) ++ Seq.fill(6)(false)
      drive(dut, Seq.fill(8)(true), Seq.fill(8)(0), 0 until 8, retained)
      assert((dut.io.selected.peek().litValue & 0x3) == 0x3)
    }
  }

  it should "match an address-derived eight-bank route oracle" in {
    val random = new scala.util.Random(0x004005)
    test(new CMLPerfectBatchSelectorHarness) { dut =>
      for (_ <- 0 until 100) {
        val valid = Seq.fill(8)(random.nextBoolean())
        val banks = Seq.fill(8)(random.nextInt(8))
        val destinations = Seq.fill(8)(random.nextInt(8))
        val retained = Seq.fill(8)(false)
        val seed = random.nextInt(8)
        drive(dut, valid, banks, destinations, retained, seed)
        dut.io.selected.expect(
          expectedSelection(valid, banks, destinations, retained, seed).U,
          s"valid=$valid banks=$banks destinations=$destinations seed=$seed"
        )
      }
    }
  }
}
