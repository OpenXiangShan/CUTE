package cute

import chisel3._
import chisel3.util.{Cat, MuxLookup, isPow2}

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

/** Test adapter for the ACC selector used as the reference for TR batch windows. */
object ABPerfectBatchSelector {
  def select(
    valid: Seq[Bool],
    address: Seq[UInt],
    destination: Seq[UInt],
    retained: Seq[Bool],
    seedPointer: UInt,
    l2BankCount: Int
  ): UInt = {
    require(valid.nonEmpty && isPow2(valid.size))
    require(address.size == valid.size)
    require(destination.size == valid.size)
    require(retained.size == valid.size)
    require(isPow2(l2BankCount))
    AccLoadPerfectBatchSelector.select(valid, address, destination, retained, seedPointer, l2BankCount)
  }
}

/** Test fixture for the deterministic task-start seed table. */
object ABPerfectBatchSeed {
  val MappingVersion: Int = 2
  val Disabled: Int = 0
  val Success: Int = 1
  val NoEntry: Int = 2
  val ShortStream: Int = 3
  val InvalidGeometry: Int = 4

  private val LaneCount = 8
  private val KValues = Seq(1, 2, 4, 8)
  private case class Request(bank: Int, destination: Int, ordinal: Int)
  private case class Entry(status: Int, seeds: Seq[Int])

  private def shuffle(index: Int): Int = ((index << 1) & 7) | (index >> 2)

  private def survivor(requests: Seq[Request]): Int = {
    var lanes: Seq[Option[(Int, Int)]] = (0 until LaneCount).map { bank =>
      requests.find(_.bank == bank).map(request => bank -> request.destination)
    }
    for (stage <- 0 until 3) {
      val switched = Array.fill[Option[(Int, Int)]](LaneCount)(None)
      for (pair <- 0 until 4) {
        val top = lanes(pair * 2)
        val bottom = lanes(pair * 2 + 1)
        def bit(token: (Int, Int)): Int = (token._2 >> (2 - stage)) & 1
        val top0 = top.filter(bit(_) == 0)
        val bottom0 = bottom.filter(bit(_) == 0)
        val top1 = top.filter(bit(_) == 1)
        val bottom1 = bottom.filter(bit(_) == 1)
        val out0 = top0.orElse(bottom0)
        val out1 = bottom1.orElse(top1)
        val topIndex = if (stage == 2) pair * 2 else shuffle(pair * 2)
        val bottomIndex = if (stage == 2) pair * 2 + 1 else shuffle(pair * 2 + 1)
        switched(topIndex) = out0
        switched(bottomIndex) = out1
      }
      lanes = switched.toSeq
    }
    lanes.flatten.foldLeft(0) { case (mask, (origin, _)) => mask | (1 << origin) }
  }

  private def request(basePhase: Int, stridePhase: Int, k: Int,
                      channel: Int, ordinal: Int): Request = {
    val group = ordinal / k
    val beat = ordinal % k
    val bytePhase = basePhase * 64 +
      (group * LaneCount + channel) * stridePhase * 16 + beat * 64
    Request((bytePhase >> 6) & 7, channel, ordinal)
  }

  private def derive(basePhase: Int, stridePhase: Int, k: Int): Entry = {
    val choices = (0 until LaneCount).map(channel =>
      (0 until 16).map(ordinal => request(basePhase, stridePhase, k, channel, ordinal)))
    def search(channel: Int, selected: Seq[Request]): Option[Seq[Int]] = {
      if (channel == LaneCount) {
        Option.when(selected.map(_.bank).distinct.size == LaneCount && survivor(selected) == 0xff)(
          selected.map(_.ordinal))
      } else {
        choices(channel).iterator
          .filterNot(candidate => selected.exists(_.bank == candidate.bank))
          .flatMap { candidate =>
            val tentative = selected :+ candidate
            Option.when(survivor(tentative) == tentative.map(request => 1 << request.bank).sum)(tentative)
          }
          .flatMap(tentative => search(channel + 1, tentative))
          .take(1).toSeq.headOption
      }
    }
    search(0, Seq.empty).map(seeds => Entry(Success, seeds))
      .getOrElse(Entry(NoEntry, Seq.fill(LaneCount)(0)))
  }

  // Index order matches CUTE/scripts/generated/ab_seed_mapping.json:
  // base phase, stride/16 phase, then K class.
  private lazy val entries: Seq[Entry] = for {
    basePhase <- 0 until 8
    stridePhase <- 0 until 32
    k <- KValues
  } yield derive(basePhase, stridePhase, k)

  def lookup(
    base: UInt,
    stride: UInt,
    kBeatCount: UInt,
    streamLengths: Seq[UInt],
    enabled: Boolean,
    ordinalWidth: Int,
    legacyMinimumCheck: Boolean = false
  ): (UInt, Vec[UInt]) = {
    require(streamLengths.length == LaneCount,
      s"AB seed lookup requires one stream length per lane, got ${streamLengths.length}")
    val status = Wire(UInt(3.W))
    val seeds = Wire(Vec(LaneCount, UInt(ordinalWidth.W)))
    status := Disabled.U
    seeds.foreach(_ := 0.U)
    if (enabled) {
      val kClassValid = kBeatCount === 1.U || kBeatCount === 2.U ||
        kBeatCount === 4.U || kBeatCount === 8.U
      val kClass = MuxLookup(kBeatCount, 0.U(2.W))(Seq(
        1.U -> 0.U, 2.U -> 1.U, 4.U -> 2.U, 8.U -> 3.U))
      val aligned = !base(5, 0).orR && !stride(3, 0).orR
      val index = Cat(base(8, 6), stride(8, 4), kClass)
      val tableStatus = VecInit(entries.map(_.status.U(3.W)))(index)
      val packedSeeds = VecInit(entries.map(entry =>
        entry.seeds.zipWithIndex.foldLeft(BigInt(0)) { case (packed, (seed, channel)) =>
          packed | (BigInt(seed) << (channel * 4))
        }.U(32.W)))(index)
      val seedInRange = (0 until LaneCount).map { channel =>
        packedSeeds(channel * 4 + 3, channel * 4) < streamLengths(channel)
      }.reduce(_ && _)
      val maxSeed = (0 until LaneCount).map(channel =>
        packedSeeds(channel * 4 + 3, channel * 4)).reduce((left, right) => Mux(left > right, left, right))
      val streamCheck = if (legacyMinimumCheck) {
        streamLengths.head <= maxSeed
      } else {
        !seedInRange
      }
      val finalStatus = Mux(!aligned || !kClassValid, InvalidGeometry.U,
        Mux(tableStatus === Success.U && streamCheck,
          ShortStream.U, tableStatus))
      status := finalStatus
      for (channel <- 0 until LaneCount) {
        seeds(channel) := Mux(finalStatus === Success.U,
          packedSeeds(channel * 4 + 3, channel * 4), 0.U)
      }
    }
    (status, seeds)
  }

  /** Compatibility entry point for callers that only know a common lower bound. */
  def lookup(
    base: UInt,
    stride: UInt,
    kBeatCount: UInt,
    minimumStreamLength: UInt,
    enabled: Boolean,
    ordinalWidth: Int
  ): (UInt, Vec[UInt]) =
    lookup(base, stride, kBeatCount, Seq.fill(LaneCount)(minimumStreamLength), enabled, ordinalWidth,
      legacyMinimumCheck = true)
}
