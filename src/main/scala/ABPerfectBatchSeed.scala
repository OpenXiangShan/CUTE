package cute

import chisel3._
import chisel3.util._

/** Deterministic task-start seed table for the default eight-bank topology. */
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
