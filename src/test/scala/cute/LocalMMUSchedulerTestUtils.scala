package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Config, Parameters}
import utility._

object LocalMMUSchedulerTestUtils {
  def parameters(
    loaderConfig: String = "A8B8CL8CS8",
    queueEnabled: Boolean = true,
    l2Banks: Int = 8,
    transparent: Boolean = true
  ): Parameters =
    new Config((_, _, _) => {
      case PerfCounterOptionsKey => PerfCounterOptions(
        enablePerfPrint = false,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.NORMAL,
        perfDBHartID = 0)
      case CuteParamsKey => CuteParams.baseParams.copy(
        outsideDataWidth = 256,
        ReduceWidthByte = 32,
        Matrix_MN = 8,
        L2NBanks = l2Banks,
        LoaderBridgeChannelConfig = loaderConfig,
        EnableOmegaInputQueue = queueEnabled
      )
    })

  def destinationLane(bank: Int, responseChannels: Int, laneCount: Int = 8, bankCount: Int = 8): Int =
    (bank / (bankCount / responseChannels)) * (laneCount / responseChannels)

  def survivorMask(
    valid: Seq[Boolean],
    banks: Seq[Int],
    responseChannels: Int,
    bankCount: Int = 8
  ): BigInt = {
    require(valid.size == banks.size && isPow2(valid.size))
    val laneCount = valid.size
    if (laneCount == 1) return if (valid.head) BigInt(1) else BigInt(0)

    case class Token(origin: Int, destination: Int)
    var lanes: Seq[Option[Token]] = valid.indices.map(i => Option.when(valid(i))(Token(i,
      destinationLane(banks(i), responseChannels, laneCount, bankCount))))
    val stages = log2Ceil(laneCount)
    for (stage <- 0 until stages) {
      val switched = Array.fill[Option[Token]](laneCount)(None)
      for (pair <- 0 until laneCount / 2) {
        val top = lanes(pair * 2)
        val bottom = lanes(pair * 2 + 1)
        def routeBit(token: Token): Int = (token.destination >> (stages - 1 - stage)) & 1
        val top0 = top.filter(routeBit(_) == 0)
        val bottom0 = bottom.filter(routeBit(_) == 0)
        val top1 = top.filter(routeBit(_) == 1)
        val bottom1 = bottom.filter(routeBit(_) == 1)
        switched(pair * 2) = top0.orElse(bottom0)
        switched(pair * 2 + 1) = bottom1.orElse(top1)
      }
      lanes = if (stage == stages - 1) switched.toSeq else {
        val shuffled = Array.fill[Option[Token]](laneCount)(None)
        switched.indices.foreach(i => shuffled(OmegaTopology.perfectShuffle(i, laneCount)) = switched(i))
        shuffled.toSeq
      }
    }
    lanes.flatten.foldLeft(BigInt(0))((mask, token) => mask | (BigInt(1) << token.origin))
  }

  def physicalSurvivorMask(
    valid: Seq[Boolean],
    physicalInputs: Seq[Int],
    destinationBanks: Seq[Int],
    responseChannels: Int,
    bankCount: Int = 8
  ): BigInt = {
    require(valid.size == physicalInputs.size && valid.size == destinationBanks.size)
    val laneCount = valid.size
    val owners = Array.fill[Option[Int]](bankCount)(None)
    valid.indices.foreach { lane =>
      val physical = physicalInputs(lane)
      require(physical >= 0 && physical < bankCount)
      if (valid(lane) && owners(physical).isEmpty) owners(physical) = Some(lane)
    }
    val slotValid = owners.map(_.nonEmpty).toSeq
    val slotDestinations = owners.map(_.map(destinationBanks).getOrElse(0)).toSeq
    val physicalMask = survivorMask(slotValid, slotDestinations, responseChannels, bankCount)
    owners.zipWithIndex.foldLeft(BigInt(0)) { case (mask, (owner, physical)) =>
      owner.filter(_ => physicalMask.testBit(physical))
        .map(lane => mask | (BigInt(1) << lane)).getOrElse(mask)
    }
  }
}

class OmegaSurvivorHarness(
  laneCount: Int = 8,
  responseChannels: Int = 8,
  bankCount: Int = 8
) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Vec(laneCount, Bool()))
    val bank = Input(Vec(laneCount, UInt(log2Ceil(bankCount).W)))
    val survivor = Output(UInt(laneCount.W))
  })
  val destinations = io.bank.map(bank =>
    OmegaTopology.destinationLane(bank, laneCount, responseChannels, bankCount))
  io.survivor := OmegaTopology.survivorMask(io.valid, destinations)
}

class OmegaSurvivorWidthHarness(laneCount: Int = 8, bankCount: Int = 8) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Vec(laneCount, Bool()))
    val bank = Input(Vec(laneCount, UInt(log2Ceil(bankCount).W)))
    val survivor = Output(Vec(4, UInt(laneCount.W)))
    val routerReady = Output(Vec(4, UInt(laneCount.W)))
  })
  Seq(1, 2, 4, 8).zipWithIndex.foreach { case (width, index) =>
    val destinations = io.bank.map(bank =>
      OmegaTopology.destinationLane(bank, laneCount, width, bankCount))
    io.survivor(index) := OmegaTopology.survivorMask(io.valid, destinations)
    val router = Module(new OmegaResponseRouter(
      n = laneCount,
      dataWidth = 8,
      sourceIdWidth = 8,
      bankIdWidth = log2Ceil(bankCount),
      outCount = width,
      bankCount = bankCount
    ))
    router.io.timeStamp := 0.U
    for (lane <- 0 until laneCount) {
      router.io.in(lane).valid := io.valid(lane)
      router.io.in(lane).bits.data := lane.U
      router.io.in(lane).bits.sourceId := io.bank(lane)
      router.io.in(lane).bits.coherent := false.B
    }
    router.io.out.foreach(_.ready := true.B)
    io.routerReady(index) := VecInit(router.io.in.map(_.ready)).asUInt
  }
}
