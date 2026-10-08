package cute

import chisel3.util.isPow2

/** Shared AB normal-load stimulus and software-side exact route oracle. */
object ABPerfectBatchTestUtils {
  val LaneCount: Int = 8
  val BaseAddress: BigInt = BigInt("80049e00", 16)
  val RowStride: BigInt = BigInt("440", 16)
  val MatrixM: Int = 128
  val KBeatCount: Int = 8
  val ExpectedRequestCount: Int = MatrixM * KBeatCount
  val BeatLut: Seq[Seq[Int]] = Seq(
    Seq(0, 1, 2, 3, 5, 6, 7, 0),
    Seq(1, 2, 3, 4, 4, 5, 6, 7),
    Seq(2, 7, 4, 1, 7, 4, 1, 6),
    Seq(3, 0, 5, 2, 6, 3, 0, 5),
    Seq(4, 5, 6, 7, 1, 2, 3, 4),
    Seq(5, 6, 7, 0, 0, 1, 2, 3),
    Seq(6, 3, 0, 5, 3, 0, 5, 2),
    Seq(7, 4, 1, 6, 2, 7, 4, 1))

  def l2Bank(address: BigInt, bankCount: Int = LaneCount): Int =
    ((address >> 6) & (bankCount - 1)).toInt

  def address(row: Int, beat: Int, base: BigInt = BaseAddress, stride: BigInt = RowStride): BigInt =
    base + row * stride + beat * 64

  def canonicalOrdinal(ordinal: Int, channel: Int, kBeatCount: Int,
                       base: BigInt = BaseAddress, stride: BigInt = RowStride,
                       matrixRows: Int = MatrixM): (Int, Int, Int, BigInt) = {
    require(kBeatCount > 0 && channel >= 0 && channel < LaneCount && ordinal >= 0)
    val (group, beat) = ordinal / kBeatCount -> (ordinal % kBeatCount)
    val row = group * LaneCount + channel
    require(row < matrixRows, s"ordinal $ordinal is outside channel $channel stream")
    (group, beat, row, address(row, beat, base, stride))
  }

  def channelStreamLength(channel: Int, kBeatCount: Int, matrixRows: Int = MatrixM): Int =
    ((matrixRows + LaneCount - 1 - channel) / LaneCount) * kBeatCount

  def exactSurvivors(
    validByBank: Seq[Boolean],
    destinationByBank: Seq[Int],
    responseChannels: Int = LaneCount,
    bankCount: Int = LaneCount
  ): BigInt = LocalMMUSchedulerTestUtils.survivorMask(
    validByBank,
    destinationByBank,
    responseChannels,
    bankCount)

  def physicalSurvivorMask(
    valid: Seq[Boolean],
    physicalInputs: Seq[Int],
    destinationBanks: Seq[Int],
    responseChannels: Int = LaneCount,
    bankCount: Int = LaneCount
  ): BigInt = LocalMMUSchedulerTestUtils.physicalSurvivorMask(
    valid,
    physicalInputs,
    destinationBanks,
    responseChannels,
    bankCount)

  def selectedLanes(mask: BigInt, laneCount: Int = LaneCount): Seq[Int] =
    (0 until laneCount).filter(mask.testBit)

  def selectedMask(lanes: Iterable[Int]): BigInt =
    lanes.foldLeft(BigInt(0))((mask, lane) => mask.setBit(lane))

  def expectedSelection(
    valid: Seq[Boolean],
    physicalInputs: Seq[Int],
    destinationBanks: Seq[Int],
    retained: Seq[Boolean],
    seed: Int,
    responseChannels: Int = LaneCount,
    bankCount: Int = LaneCount
  ): BigInt = {
    require(valid.nonEmpty && isPow2(valid.size))
    require(valid.size == physicalInputs.size)
    require(valid.size == destinationBanks.size)
    require(valid.size == retained.size)
    val laneCount = valid.size
    var selected = retained.indices.filter(retained).toSet
    if (selected.isEmpty) {
      (0 until laneCount).map(offset => (seed + offset) % laneCount).find(valid).foreach { lane =>
        selected += lane
      }
    }
    for (lane <- 0 until laneCount if valid(lane) && !selected(lane)) {
      val tentative = selected + lane
      val uniqueBanks = tentative.map(physicalInputs).size == tentative.size
      val survivor = physicalSurvivorMask(
        (0 until laneCount).map(tentative),
        physicalInputs,
        destinationBanks,
        responseChannels,
        bankCount)
      val mask = selectedMask(tentative)
      if (uniqueBanks && (survivor & mask) == mask) {
        selected = tentative
      }
    }
    if (selected.nonEmpty) selectedMask(selected)
    else valid.indexWhere(identity) match {
      case -1 => BigInt(0)
      case lane => BigInt(1) << lane
    }
  }
}
