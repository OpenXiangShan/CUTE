package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ABPerfectBatchSelectorHarness extends Module {
  private val laneCount = 8
  val io = IO(new Bundle {
    val valid = Input(Vec(laneCount, Bool()))
    val address = Input(Vec(laneCount, UInt(64.W)))
    val destination = Input(Vec(laneCount, UInt(3.W)))
    val retained = Input(Vec(laneCount, Bool()))
    val seed = Input(UInt(3.W))
    val selected = Output(UInt(laneCount.W))
  })

  io.selected := ABPerfectBatchSelector.select(
    io.valid,
    io.address,
    io.destination,
    io.retained,
    io.seed,
    l2BankCount = 8
  )
}

class ABPerfectBatchWindowHarness extends Module {
  private val laneCount = ABPerfectBatchTestUtils.LaneCount
  private val indexWidth = 4
  val io = IO(new Bundle {
    val valid = Input(Vec(laneCount, Bool()))
    val bank = Input(Vec(laneCount, UInt(3.W)))
    val destination = Input(Vec(laneCount, UInt(3.W)))
    val ready = Input(Vec(laneCount, Bool()))
    val flush = Input(Bool())
    val selected = Output(UInt(laneCount.W))
    val fire = Output(UInt(laneCount.W))
    val retained = Output(UInt(laneCount.W))
    val index = Output(Vec(laneCount, UInt(indexWidth.W)))
    val requestAddress = Output(Vec(laneCount, UInt(64.W)))
    val totalFired = Output(UInt(8.W))
  })

  val index = RegInit(VecInit(Seq.fill(laneCount)(0.U(indexWidth.W))))
  val retained = RegInit(VecInit(Seq.fill(laneCount)(false.B)))
  val retainedIndex = Reg(Vec(laneCount, UInt(indexWidth.W)))
  val retainedAddress = Reg(Vec(laneCount, UInt(64.W)))
  val retainedDestination = Reg(Vec(laneCount, UInt(3.W)))
  val totalFired = RegInit(0.U(8.W))

  val candidateValid = Wire(Vec(laneCount, Bool()))
  val candidateAddress = Wire(Vec(laneCount, UInt(64.W)))
  val candidateDestination = Wire(Vec(laneCount, UInt(3.W)))
  val candidateIndex = Wire(Vec(laneCount, UInt(indexWidth.W)))
  for (lane <- 0 until laneCount) {
    val freshAddress = Cat(index(lane), io.bank(lane), 0.U(6.W))
    candidateValid(lane) := io.valid(lane) || retained(lane)
    candidateAddress(lane) := Mux(retained(lane), retainedAddress(lane), freshAddress)
    candidateDestination(lane) := Mux(retained(lane), retainedDestination(lane), io.destination(lane))
    candidateIndex(lane) := Mux(retained(lane), retainedIndex(lane), index(lane))
  }

  val selected = ABPerfectBatchSelector.select(
    candidateValid,
    candidateAddress,
    candidateDestination,
    retained,
    0.U(log2Ceil(laneCount).W),
    l2BankCount = laneCount)
  val fire = Wire(Vec(laneCount, Bool()))
  for (lane <- 0 until laneCount) {
    val requestValid = candidateValid(lane) && selected(lane)
    fire(lane) := requestValid && io.ready(lane)
    io.requestAddress(lane) := candidateAddress(lane)
    io.index(lane) := candidateIndex(lane)
  }

  when(io.flush) {
    retained.foreach(_ := false.B)
    index.foreach(_ := 0.U)
    totalFired := 0.U
  }.otherwise {
    for (lane <- 0 until laneCount) {
      val requestValid = candidateValid(lane) && selected(lane)
      when(fire(lane)) {
        retained(lane) := false.B
        index(lane) := index(lane) + 1.U
      }.elsewhen(requestValid && !io.ready(lane)) {
        retained(lane) := true.B
        retainedIndex(lane) := candidateIndex(lane)
        retainedAddress(lane) := candidateAddress(lane)
        retainedDestination(lane) := candidateDestination(lane)
      }
    }
    totalFired := totalFired + PopCount(fire)
  }

  io.selected := selected
  io.fire := fire.asUInt
  io.retained := retained.asUInt
  io.totalFired := totalFired
}

class ABPerfectBatchSelectorSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ABPerfectBatchSelector"

  private def drive(
    dut: ABPerfectBatchSelectorHarness,
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
    dut.io.seed.poke(seed.U)
  }

  it should "preserve retained candidates and reject duplicate physical banks" in {
    test(new ABPerfectBatchSelectorHarness) { dut =>
      drive(dut, Seq.fill(8)(true), Seq(0, 1, 2, 3, 4, 5, 6, 7),
        Seq(0, 4, 1, 5, 2, 6, 3, 7), Seq(true, false, false, false, false, true, false, false))
      val selected = dut.io.selected.peek().litValue
      assert((selected & 0x21) == 0x21)
      assert((0 until 8).filter(selected.testBit).map(Seq(0, 1, 2, 3, 4, 5, 6, 7)).distinct.size ==
        (0 until 8).filter(selected.testBit).size)
    }
  }

  it should "make progress from the rotating seed when all candidates share one bank" in {
    test(new ABPerfectBatchSelectorHarness) { dut =>
      for (seed <- 0 until 8) {
        drive(dut, Seq.fill(8)(true), Seq.fill(8)(0), 0 until 8, seed = seed)
        dut.io.selected.expect((BigInt(1) << seed).U)
      }
    }
  }

  it should "match the reviewed CML selector for randomized candidate windows" in {
    val random = new scala.util.Random(0x006ab)
    test(new ABPerfectBatchSelectorHarness) { dut =>
      for (_ <- 0 until 100) {
        val valid = Seq.fill(8)(random.nextBoolean())
        val banks = Seq.fill(8)(random.nextInt(8))
        val destinations = Seq.fill(8)(random.nextInt(8))
        val retained = Seq.fill(8)(false)
        val seed = random.nextInt(8)
        drive(dut, valid, banks, destinations, retained, seed)
        val expected = ABPerfectBatchTestUtils.expectedSelection(valid, banks, destinations, retained, seed)
        dut.io.selected.expect(expected.U)
      }
    }
  }

  private def driveWindow(
    dut: ABPerfectBatchWindowHarness,
    valid: Seq[Boolean],
    banks: Seq[Int],
    destinations: Seq[Int],
    ready: Seq[Boolean],
    flush: Boolean = false
  ): Unit = {
    for (lane <- 0 until ABPerfectBatchTestUtils.LaneCount) {
      dut.io.valid(lane).poke(valid(lane).B)
      dut.io.bank(lane).poke(banks(lane).U)
      dut.io.destination(lane).poke(destinations(lane).U)
      dut.io.ready(lane).poke(ready(lane).B)
    }
    dut.io.flush.poke(flush.B)
  }

  it should "retain selected indices and keep payloads stable while ready is low" in {
    test(new ABPerfectBatchWindowHarness) { dut =>
      val valid = Seq.fill(8)(true)
      val banks = 0 until 8
      val destinations = Seq(0, 4, 1, 5, 2, 6, 3, 7)
      driveWindow(dut, valid, banks, destinations, Seq.fill(8)(false))
      dut.io.selected.expect("hff".U)
      val heldAddress = dut.io.requestAddress(3).peek().litValue
      dut.clock.step()

      dut.io.retained.expect("hff".U)
      driveWindow(dut, valid, Seq.fill(8)(0), destinations.reverse, Seq.fill(8)(false))
      dut.io.requestAddress(3).expect(heldAddress.U)
      dut.io.index(3).expect(0.U)
      dut.clock.step()

      driveWindow(dut, valid, Seq.fill(8)(0), destinations.reverse, Seq.fill(8)(true))
      dut.clock.step()
      dut.io.retained.expect(0.U)
      dut.io.index.foreach(_.expect(1.U))
      dut.io.totalFired.expect(8.U)
    }
  }

  it should "advance candidate indices only for fired lanes" in {
    test(new ABPerfectBatchWindowHarness) { dut =>
      val valid = Seq.fill(8)(true)
      val banks = 0 until 8
      val destinations = Seq(0, 4, 1, 5, 2, 6, 3, 7)
      driveWindow(dut, valid, banks, destinations, Seq(true, false, true, false, false, false, false, false))
      dut.io.fire.expect("h05".U)
      dut.clock.step()

      dut.io.index(0).expect(1.U)
      dut.io.index(1).expect(0.U)
      dut.io.index(2).expect(1.U)
      dut.io.retained.expect("hfa".U)
      dut.io.totalFired.expect(2.U)
    }
  }

  it should "clear retained candidates and progress on flush or reset" in {
    test(new ABPerfectBatchWindowHarness) { dut =>
      val valid = Seq.fill(8)(true)
      val banks = 0 until 8
      val destinations = Seq(0, 4, 1, 5, 2, 6, 3, 7)
      driveWindow(dut, valid, banks, destinations, Seq.fill(8)(false))
      dut.clock.step()
      dut.io.retained.expect("hff".U)

      driveWindow(dut, Seq.fill(8)(false), banks, destinations, Seq.fill(8)(false), flush = true)
      dut.clock.step()
      dut.io.retained.expect(0.U)
      dut.io.totalFired.expect(0.U)
      dut.io.index.foreach(_.expect(0.U))

      driveWindow(dut, valid, banks, destinations, Seq.fill(8)(false))
      dut.clock.step()
      dut.io.retained.expect("hff".U)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.io.retained.expect(0.U)
      dut.io.totalFired.expect(0.U)
    }
  }
}
