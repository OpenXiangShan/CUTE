package cute

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import utility._

import scala.collection.mutable

class ABLocalMmuPriorityHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val aConfig = Flipped(new AMLMicroTaskConfigIO)
    val bConfig = Flipped(new BMLMicroTaskConfigIO)
    val llc = Flipped(new MMU2TLIO)
    val aPerf = Output(new ABMLPerfProbe)
    val bPerf = Output(new ABMLPerfProbe)
    val localPerf = Output(new LocalMMUPerfProbe)
    val aRequestValid = Output(UInt(8.W))
    val bRequestValid = Output(UInt(8.W))
    val bRequestAddress = Output(Vec(8, UInt(64.W)))
    val bRequestSourceId = Output(Vec(8, UInt(64.W)))
  })

  private val aml = Module(new AMLWrapper("AB_local_mmu_A"))
  private val bml = Module(new BMLWrapper("AB_local_mmu_B"))
  private val localMmu = Module(new LocalMMU)

  aml.io.ConfigInfo <> io.aConfig
  bml.io.ConfigInfo <> io.bConfig
  aml.io.LocalMMUIO <> localMmu.io.ALocalMMUIO
  bml.io.LocalMMUIO <> localMmu.io.BLocalMMUIO
  localMmu.io.LastLevelCacheTLIO <> io.llc
  aml.io.DebugInfo := 0.U.asTypeOf(new DebugInfoIO)
  bml.io.DebugInfo := 0.U.asTypeOf(new DebugInfoIO)

  private def tieOffSource(source: LocalMMUIO): Unit = {
    source.Request.foreach { request =>
      request.valid := false.B
      request.bits := 0.U.asTypeOf(request.bits)
    }
    source.Response.foreach(_.ready := false.B)
  }

  tieOffSource(localMmu.io.CLoadLocalMMUIO)
  tieOffSource(localMmu.io.CStoreLocalMMUIO)
  tieOffSource(localMmu.io.ASLocalMMUIO)
  tieOffSource(localMmu.io.BSLocalMMUIO)

  io.aPerf := aml.io.perfProbe
  io.bPerf := bml.io.perfProbe
  io.localPerf := localMmu.io.perfProbe
  io.aRequestValid := VecInit(aml.io.LocalMMUIO.Request.map(_.valid)).asUInt
  io.bRequestValid := VecInit(bml.io.LocalMMUIO.Request.map(_.valid)).asUInt
  for (bank <- 0 until 8) {
    io.bRequestAddress(bank) := bml.io.LocalMMUIO.Request(bank).bits.RequestAddr
    io.bRequestSourceId(bank) := bml.io.LocalMMUIO.Request(bank).bits.RequestSourceID
  }
}

/** Historical seed/window performance expectations remain ignored: the active normal
 * path uses per-bank queues and static batch traversal. Current coverage and data
 * identity are checked by ABBatchScheduleSpec; this suite is not acceptance evidence.
 */
class MultiChannelsABPerfectBatchSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MultiChannelsABMemLoader perfect batches"

  private def testParams(seedEnabled: Boolean = true): Parameters = new Config((_, _, _) => {
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0)
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      LoaderBridgeChannelConfig = "A8B8CL8CS8",
      L2NBanks = 8,
      ABPerfectBatchSeedEnable = seedEnabled,
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false)
  })
  private implicit val params: Parameters = testParams()

  private def clearMmu(io: LocalMMUIO, banks: Int): Unit = {
    io.ConherentRequsetSourceID.valid.poke(false.B)
    io.ConherentRequsetSourceID.bits.poke(0.U)
    io.nonConherentRequsetSourceID.valid.poke(false.B)
    io.nonConherentRequsetSourceID.bits.poke(0.U)
    for (bank <- 0 until banks) {
      io.Request(bank).ready.poke(true.B)
      io.Response(bank).valid.poke(false.B)
      io.Response(bank).bits.ReseponseData.poke(0.U)
      io.Response(bank).bits.ReseponseConherent.poke(true.B)
      io.Response(bank).bits.ReseponseSourceID.poke(0.U)
    }
  }

  private def clearLlc(io: MMU2TLIO, banks: Int): Unit = {
    io.ConherentRequsetSourceID.valid.poke(false.B)
    io.ConherentRequsetSourceID.bits.poke(0.U)
    io.nonConherentRequsetSourceID.valid.poke(false.B)
    io.nonConherentRequsetSourceID.bits.poke(0.U)
    for (bank <- 0 until banks) {
      io.Request(bank).ready.poke(true.B)
      io.Response(bank).valid.poke(false.B)
      io.Response(bank).bits.ReseponseData.poke(0.U)
      io.Response(bank).bits.ReseponseConherent.poke(true.B)
      io.Response(bank).bits.ReseponseSourceID.poke(0.U)
    }
  }

  private def configureAConfig(
    config: AMLMicroTaskConfigIO,
    rows: Int = 8,
    kBeatCount: Int = ABPerfectBatchTestUtils.KBeatCount,
    baseAddress: BigInt = ABPerfectBatchTestUtils.BaseAddress,
    rowStride: BigInt = ABPerfectBatchTestUtils.RowStride
  ): Unit = {
    config.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr.poke(
      baseAddress.U)
    config.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(
      rowStride.U)
    config.ApplicationTensor_A.dataType.poke(0.U)
    config.ApplicationTensor_A.HasTail.poke(false.B)
    config.ApplicationTensor_A.TailByteMask.poke(0.U)
    config.ApplicationTensor_A.K_Beat_Count.poke(kBeatCount.U)
    config.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
    config.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
    config.LoadTaskInfo.Is_FullLoad.poke(true.B)
    config.MatrixRegTensor_M.poke(rows.U)
    config.MatrixRegTensor_K.poke(64.U)
    config.MatrixRegId.poke(0.U)
    config.Conherent.poke(true.B)
    config.Is_Transpose.poke(false.B)
    config.MicroTaskValid.poke(false.B)
    config.MicroTaskEndReady.poke(true.B)
  }

  private def configureA(dut: MultiChannelsABMemLoader, rows: Int = 8): Unit = {
    configureAConfig(dut.io.ConfigInfo, rows)
    dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
    clearMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
  }

  private def collectRequests(
    dut: MultiChannelsABMemLoader,
    expectedCount: Int,
    maxCycles: Int = 100
  ): Seq[(Int, BigInt, BigInt)] = {
    val observed = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
    var cycles = 0
    while (observed.size < expectedCount && cycles < maxCycles) {
      for (channel <- 0 until dut.ABMatrixRegNBanks) {
        val request = dut.io.LocalMMUIO.Request(channel)
        if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
          observed += ((channel, request.bits.RequestAddr.peek().litValue,
            request.bits.RequestSourceID.peek().litValue))
        }
      }
      dut.clock.step()
      cycles += 1
    }
    assert(observed.size == expectedCount,
      s"expected $expectedCount requests, observed ${observed.size} after $cycles cycles")
    observed.toSeq
  }

  private def startA(
    dut: MultiChannelsABMemLoader,
    rows: Int,
    kBeatCount: Int,
    baseAddress: BigInt,
    rowStride: BigInt
  ): Unit = {
    configureAConfig(dut.io.ConfigInfo, rows, kBeatCount, baseAddress, rowStride)
    dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
    clearMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
    dut.clock.step()
    dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
  }

  ignore should "sustain ideal perfect batches for AML M128 N128 K64 e8 with one beat per row" in {
    test(new MultiChannelsABMemLoader("AML", "AML_m128_n128_k64_e8_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureAConfig(
          dut.io.ConfigInfo,
          rows = 128,
          kBeatCount = 1,
          baseAddress = BigInt("8004a000", 16),
          // A 64-byte stride pins every channel to one L2 bank for K=1.
          // That geometry is a valid fallback case, but cannot be repaired by
          // a channel-local seed.  144B gives this ideal test a full Omega-
          // compatible bank permutation while retaining a phase-zero base.
          rowStride = 144)
        dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
        clearMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)

        var cycles = 0
        var fired = 0
        var perfectPulses = 0
        val idealIssueCycles = scala.collection.mutable.ArrayBuffer.empty[Int]
        val observed = scala.collection.mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        val sourceAddrWidth = log2Ceil(dut.ABMatrixRegBankNEntries)
        dut.clock.setTimeout(200)
        while (fired < 128 && cycles < 40) {
          val cycleRequests = (0 until dut.ABMatrixRegNBanks).flatMap { channel =>
            val request = dut.io.LocalMMUIO.Request(channel)
            Option.when(request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
              (channel, request.bits.RequestAddr.peek().litValue, request.bits.RequestSourceID.peek().litValue)
            }
          }
          val cycleFires = cycleRequests.size
          if (cycleRequests.nonEmpty) {
            val validByBank = Array.fill(dut.ABMatrixRegNBanks)(false)
            val destinationByBank = Array.fill(dut.ABMatrixRegNBanks)(0)
            cycleRequests.foreach { case (channel, address, source) =>
              val physicalBank = ABPerfectBatchTestUtils.l2Bank(address)
              val destination = ((source >> sourceAddrWidth) & 7).toInt
              assert(destination == channel,
                s"cycle $cycles channel $channel carried destination $destination")
              assert(!validByBank(physicalBank),
                s"cycle $cycles reused physical bank $physicalBank")
              validByBank(physicalBank) = true
              destinationByBank(physicalBank) = destination
              observed += ((channel, address, source))
            }
            val survivors = ABPerfectBatchTestUtils.exactSurvivors(
              validByBank.toSeq,
              destinationByBank.toSeq)
            assert(survivors == 0xff, s"cycle $cycles Omega survivors were 0x${survivors.toString(16)}")
          }
          if (cycleFires > 0) {
            idealIssueCycles += cycleFires
            assert(cycleFires == 8, s"ideal one-beat issue cycle $cycles fired $cycleFires requests")
            fired += cycleFires
          }
          if (dut.io.perfProbe.loadPerfectBatch.peek().litToBoolean) {
            perfectPulses += 1
          }
          dut.clock.step()
          cycles += 1
        }

        assert(fired == 128, s"expected 128 AML requests, observed $fired")
        assert(idealIssueCycles.size == 16, s"expected 16 issue cycles, got ${idealIssueCycles.size}")
        assert(perfectPulses == 16, s"expected 16 perfect-batch pulses, got $perfectPulses")
        val expected = (0 until 128).map { row =>
          val channel = row % dut.ABMatrixRegNBanks
          val group = row / dut.ABMatrixRegNBanks
          // The test profile uses ReduceWidthByte=32, so each 64B memory
          // beat occupies two MatrixReg entries.
          (channel, BigInt("8004a000", 16) + row * 144, BigInt(group * 2))
        }.toSet
        val actual = observed.map { case (channel, address, source) =>
          val regAddress = source & ((BigInt(1) << sourceAddrWidth) - 1)
          (channel, address, regAddress)
        }.toSet
        assert(observed.size == actual.size, "one-beat task emitted a duplicate canonical request")
        assert(actual == expected, s"request coverage mismatch: missing=${expected -- actual} extra=${actual -- expected}")
        dut.io.perfProbe.loadTotalFired.expect(128.U)
        dut.io.perfProbe.loadPerfectBatches.expect(16.U)
        dut.io.perfProbe.loadPerfectRequests.expect(128.U)
        dut.io.perfProbe.windowRepairs.expect(0.U)
    }
  }

  ignore should "reconcile seed fire perfect and nonzero window-repair probes" in {
    implicit val p: Parameters = testParams(seedEnabled = true)
    test(new MultiChannelsABMemLoader("AML", "AML_window_repair_probe")(p))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        val rows = 128
        val base = BigInt("8004a000", 16)
        val stride = BigInt(32)
        startA(dut, rows, kBeatCount = 1, baseAddress = base, rowStride = stride)

        val windowBase = Array.fill(dut.ABMatrixRegNBanks)(0)
        val issued = Array.fill(dut.ABMatrixRegNBanks)(mutable.Set.empty[Int])
        var observedFires = 0
        var observedPerfectBatches = 0
        var observedPerfectRequests = 0
        var observedWindowRepairs = 0
        var cycles = 0
        var firstFire = Seq.empty[(Int, Int)]
        while (observedFires < rows && cycles < 80) {
          val cycleRequests = (0 until dut.ABMatrixRegNBanks).flatMap { channel =>
            val request = dut.io.LocalMMUIO.Request(channel)
            Option.when(request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
              val address = request.bits.RequestAddr.peek().litValue
              val rowDelta = address - base - channel * stride
              assert(rowDelta >= 0 && rowDelta % (dut.ABMatrixRegNBanks * stride) == 0,
                s"channel $channel emitted noncanonical address 0x${address.toString(16)}")
              val ordinal = (rowDelta / (dut.ABMatrixRegNBanks * stride)).toInt
              assert(ordinal < rows / dut.ABMatrixRegNBanks,
                s"channel $channel emitted out-of-range ordinal $ordinal")
              assert(!issued(channel)(ordinal),
                s"channel $channel emitted duplicate ordinal $ordinal")
              (channel, ordinal, address)
            }
          }
          if (cycleRequests.nonEmpty) {
            if (firstFire.isEmpty) {
              firstFire = cycleRequests.map { case (channel, ordinal, _) => channel -> ordinal }
            }
            if (cycleRequests.exists { case (channel, ordinal, _) =>
              ordinal != windowBase(channel)
            }) {
              observedWindowRepairs += 1
            }
            val validByBank = Array.fill(dut.ABMatrixRegNBanks)(false)
            val destinationByBank = Array.fill(dut.ABMatrixRegNBanks)(0)
            cycleRequests.foreach { case (channel, ordinal, address) =>
              val physicalBank = ABPerfectBatchTestUtils.l2Bank(address)
              assert(!validByBank(physicalBank),
                s"cycle $cycles reused physical bank $physicalBank")
              validByBank(physicalBank) = true
              destinationByBank(physicalBank) = channel
              issued(channel) += ordinal
              windowBase(channel) = (ordinal + 1) % (rows / dut.ABMatrixRegNBanks)
            }
            val survivorMask = ABPerfectBatchTestUtils.exactSurvivors(
              validByBank.toSeq, destinationByBank.toSeq)
            val firedBankMask = validByBank.zipWithIndex.collect {
              case (true, bank) => BigInt(1) << bank
            }.foldLeft(BigInt(0))(_ | _)
            val perfect = cycleRequests.size == dut.ABMatrixRegNBanks &&
              survivorMask == firedBankMask
            if (perfect) {
              observedPerfectBatches += 1
              observedPerfectRequests += dut.ABMatrixRegNBanks
            }
            observedFires += cycleRequests.size
          }
          dut.clock.step()
          cycles += 1
        }

        assert(firstFire == Seq(0 -> 0, 2 -> 1, 4 -> 0, 6 -> 1),
          s"unexpected repair fixture first fire $firstFire")
        assert(observedFires == rows, s"observed $observedFires of $rows request fires")
        assert(observedWindowRepairs > 0, "fixture did not exercise a window repair")
        assert(issued.forall(_.size == rows / dut.ABMatrixRegNBanks),
          s"fixture did not cover every channel stream: ${issued.map(_.size).toSeq}")
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.NoEntry.U)
        dut.io.perfProbe.seedAttempts.expect(1.U)
        dut.io.perfProbe.seedSuccesses.expect(0.U)
        dut.io.perfProbe.seedFallbacks.expect(1.U)
        dut.io.perfProbe.loadTotalFired.expect(observedFires.U)
        dut.io.perfProbe.loadPerfectBatches.expect(observedPerfectBatches.U)
        dut.io.perfProbe.loadPerfectRequests.expect(observedPerfectRequests.U)
        dut.io.perfProbe.windowRepairs.expect(observedWindowRepairs.U)
      }
  }

  Seq(true, false).foreach { seedEnabled =>
    ignore should s"preserve the canonical request set when task-start seeding is enabled=$seedEnabled" in {
      implicit val p: Parameters = testParams(seedEnabled)
      test(new MultiChannelsABMemLoader("AML", s"AML_seed_enabled_$seedEnabled")(p))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          startA(dut, rows = 128, kBeatCount = 1,
            baseAddress = BigInt("8004a000", 16), rowStride = 16)
          val observed = collectRequests(dut, expectedCount = 128)
          val actual = observed.map { case (channel, address, _) => channel -> address }.toSet
          val expected = (0 until 128).map { row =>
            (row % 8) -> (BigInt("8004a000", 16) + row * 16)
          }.toSet
          assert(actual == expected,
            s"seed mode coverage mismatch: missing=${expected -- actual} extra=${actual -- expected}")
          dut.io.perfProbe.seedStatus.expect(
            (if (seedEnabled) ABPerfectBatchSeed.Success else ABPerfectBatchSeed.Disabled).U)
        }
    }
  }

  ignore should "wrap a nonzero seed and issue every ordinal exactly once" in {
    implicit val p: Parameters = testParams(seedEnabled = true)
    test(new MultiChannelsABMemLoader("AML", "AML_nonzero_seed_wrap")(p))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        startA(dut, rows = 128, kBeatCount = 1,
          baseAddress = BigInt("8004a000", 16), rowStride = 16)
        dut.clock.step()
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Success.U)
        assert((0 until 8).exists(dut.io.perfProbe.seedVector(_).peek().litValue > 0),
          "fixture did not produce a nonzero seed")
        val observed = collectRequests(dut, expectedCount = 128)
        val identities = observed.map { case (channel, address, _) => channel -> address }
        assert(identities.distinct.size == 128, "nonzero seed traversal duplicated a request")
        val expected = (0 until 128).map { row =>
          (row % 8) -> (BigInt("8004a000", 16) + row * 16)
        }.toSet
        assert(identities.toSet == expected,
          s"nonzero seed traversal coverage mismatch: missing=${expected -- identities.toSet}")
      }
  }

  ignore should "wrap unequal final-partial streams and skip issued ordinals exactly once" in {
    implicit val p: Parameters = testParams(seedEnabled = true)
    test(new MultiChannelsABMemLoader("AML", "AML_partial_seed_wrap")(p))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        val rows = 125
        val base = BigInt("8004a000", 16)
        val stride = BigInt(16)
        startA(dut, rows = rows, kBeatCount = 1, baseAddress = base, rowStride = stride)
        dut.clock.step()
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Success.U)
        val seeds = dut.io.perfProbe.seedVector.map(_.peek().litValue.toInt)
        assert(seeds == Seq(0, 1, 2, 3, 0, 1, 2, 3), s"unexpected partial-stream seeds $seeds")

        val observed = collectRequests(dut, expectedCount = rows, maxCycles = 80)
        val identities = observed.map { case (channel, address, _) => channel -> address }
        val expected = (0 until rows).map { row => (row % 8) -> (base + row * stride) }.toSet
        assert(identities.size == identities.distinct.size,
          "partial seeded traversal re-exposed an issued ordinal")
        assert(identities.toSet == expected,
          s"partial seeded traversal mismatch: missing=${expected -- identities.toSet}")
        val counts = identities.groupMapReduce(_._1)(_ => 1)(_ + _)
        assert((0 until 8).map(counts) == Seq(16, 16, 16, 16, 16, 15, 15, 15))
      }
  }

  ignore should "fall back for fixed-bank one-beat geometry without request loss" in {
    implicit val p: Parameters = testParams(seedEnabled = true)
    test(new MultiChannelsABMemLoader("AML", "AML_fixed_bank_fallback")(p))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        startA(dut, rows = 128, kBeatCount = 1,
          baseAddress = BigInt("8004a000", 16), rowStride = 64)
        dut.clock.step()
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.NoEntry.U)
        dut.io.perfProbe.seedFallbacks.expect(1.U)
        val observed = collectRequests(dut, expectedCount = 128, maxCycles = 160)
        assert(observed.map { case (channel, address, _) => channel -> address }.distinct.size == 128,
          "fixed-bank fallback duplicated or lost a request")
      }
  }

  ignore should "clear retained seed and counter state across reset and task reaccept" in {
    implicit val p: Parameters = testParams(seedEnabled = true)
    test(new MultiChannelsABMemLoader("AML", "AML_reset_reaccept")(p))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        startA(dut, rows = 128, kBeatCount = 1,
          baseAddress = BigInt("8004a000", 16), rowStride = 16)
        dut.io.LocalMMUIO.Request.foreach(_.ready.poke(false.B))
        dut.clock.step(2)
        assert(dut.io.LocalMMUIO.Request.exists(_.valid.peek().litToBoolean),
          "fixture did not retain a stalled request")
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Success.U)

        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.io.LocalMMUIO.Request.foreach(_.valid.expect(false.B))
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Disabled.U)
        dut.io.perfProbe.seedVector.foreach(_.expect(0.U))
        dut.io.perfProbe.seedAttempts.expect(0.U)
        dut.io.perfProbe.seedSuccesses.expect(0.U)
        dut.io.perfProbe.seedFallbacks.expect(0.U)
        dut.io.perfProbe.loadTotalFired.expect(0.U)
        dut.io.perfProbe.windowRepairs.expect(0.U)

        configureAConfig(dut.io.ConfigInfo, rows = 8, kBeatCount = 1,
          baseAddress = BigInt("8004c000", 16), rowStride = 144)
        dut.io.LocalMMUIO.Request.foreach(_.ready.poke(true.B))
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
        val observed = collectRequests(dut, expectedCount = 8, maxCycles = 20)
        assert(observed.forall { case (_, address, _) => address >= BigInt("8004c000", 16) },
          "reaccepted task leaked a retained request from before reset")
      }
  }

  private def configureBConfig(config: BMLMicroTaskConfigIO, rows: Int = 8): Unit = {
    config.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr.poke(
      ABPerfectBatchTestUtils.BaseAddress.U)
    config.ApplicationTensor_B.BlockTensor_B_BaseVaddr.poke(
      ABPerfectBatchTestUtils.BaseAddress.U)
    config.ApplicationTensor_B.ApplicationTensor_B_Stride_N.poke(
      ABPerfectBatchTestUtils.RowStride.U)
    config.ApplicationTensor_B.dataType.poke(0.U)
    config.ApplicationTensor_B.HasTail.poke(false.B)
    config.ApplicationTensor_B.TailByteMask.poke(0.U)
    config.ApplicationTensor_B.K_Beat_Count.poke(ABPerfectBatchTestUtils.KBeatCount.U)
    config.MatrixRegTensor_N.poke(rows.U)
    config.MatrixRegTensor_K.poke(64.U)
    config.MatrixRegId.poke(0.U)
    config.Conherent.poke(true.B)
    config.Is_Transpose.poke(false.B)
    config.MicroTaskValid.poke(false.B)
    config.MicroTaskEndReady.poke(true.B)
  }

  private def configureB(dut: BMLWrapper, rows: Int = 8): Unit = {
    configureBConfig(dut.io.ConfigInfo, rows)
    dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
    clearMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
  }

  ignore should "complete an A-only eight-beat task with full route-compatible fires" in {
    test(new MultiChannelsABMemLoader("AML", "AML_ab_perfect_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureA(dut)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)

        val responseQueues = Array.fill(dut.ABMatrixRegNBanks)(mutable.Queue.empty[BigInt])
        val observed = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        val writebacks = mutable.ArrayBuffer.empty[(Int, BigInt)]
        var perfectBatches = 0
        var perfectRequests = 0
        var rtlPerfectBatchPulses = 0
        var cycles = 0
        var completed = false
        val sourceAddrWidth = log2Ceil(dut.ABMatrixRegBankNEntries)
        val fullMask = (BigInt(1) << dut.ABMatrixRegEntryByteSize) - 1
        dut.clock.setTimeout(1000)

        while (!completed && cycles < 200) {
          dut.io.DebugInfo.DebugTimeStampe.poke(cycles.U)
          for (bank <- 0 until dut.ABMatrixRegNBanks) {
            val response = dut.io.LocalMMUIO.Response(bank)
            response.valid.poke(responseQueues(bank).nonEmpty.B)
            response.bits.ReseponseSourceID.poke(responseQueues(bank).headOption.getOrElse(BigInt(0)).U)
            response.bits.ReseponseData.poke(0.U)
            response.bits.ReseponseConherent.poke(true.B)
          }

          val fired = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
          for (channel <- 0 until dut.ABMatrixRegNBanks) {
            val request = dut.io.LocalMMUIO.Request(channel)
            if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
              val address = request.bits.RequestAddr.peek().litValue
              val source = request.bits.RequestSourceID.peek().litValue
              val regBank = ((source >> sourceAddrWidth) & 7).toInt
              assert(regBank == channel, s"source bank mismatch channel=$channel source=$source")
              fired += ((channel, address, source))
              observed += ((channel, address, source))
              responseQueues(channel).enqueue(source)
            }
          }

          if (fired.nonEmpty) {
            val validByBank = Array.fill(dut.ABMatrixRegNBanks)(false)
            val destinationByBank = Array.fill(dut.ABMatrixRegNBanks)(0)
            fired.foreach { case (_, address, source) =>
              val l2Bank = ABPerfectBatchTestUtils.l2Bank(address, 8)
              val regBank = ((source >> sourceAddrWidth) & 7).toInt
              assert(!validByBank(l2Bank), s"duplicate L2 bank in cycle $cycles")
              validByBank(l2Bank) = true
              destinationByBank(l2Bank) = regBank
            }
            val survivors = ABPerfectBatchTestUtils.exactSurvivors(
              validByBank.toSeq,
              destinationByBank.toSeq,
              responseChannels = 8,
              bankCount = 8)
            val firedMask = validByBank.zipWithIndex.collect {
              case (true, bank) => BigInt(1) << bank
            }.foldLeft(BigInt(0))(_ | _)
            assert((survivors & firedMask) == firedMask,
              s"Omega survivor mismatch at cycle $cycles")
            if (fired.size == dut.ABMatrixRegNBanks) {
              perfectBatches += 1
              perfectRequests += fired.size
            }
          }

          if (dut.io.perfProbe.loadPerfectBatch.peek().litToBoolean) {
            rtlPerfectBatchPulses += 1
          }

          for {
            bank <- 0 until dut.ABMatrixRegNBanks
            lane <- 0 until 2
          } {
            val write = dut.io.ToMatrixRegIO.WriteRequest(bank)(lane)
            if (write.BankAddr.valid.peek().litToBoolean) {
              write.ByteMask.valid.expect(true.B)
              write.ByteMask.bits.expect(fullMask.U)
              writebacks += ((bank, write.BankAddr.bits.peek().litValue))
            }
          }
          for (bank <- 0 until dut.ABMatrixRegNBanks) {
            if (dut.io.LocalMMUIO.Response(bank).valid.peek().litToBoolean &&
                dut.io.LocalMMUIO.Response(bank).ready.peek().litToBoolean) {
              responseQueues(bank).dequeue()
            }
          }

          completed = dut.io.ConfigInfo.MicroTaskEndValid.peek().litToBoolean &&
            dut.io.ConfigInfo.MicroTaskEndReady.peek().litToBoolean
          dut.clock.step()
          cycles += 1
        }

        assert(completed, s"AML task did not complete after $cycles cycles")
        assert(observed.size == 64, s"expected 64 requests, observed ${observed.size}")
        val expected = (0 until dut.ABMatrixRegNBanks).flatMap { bank =>
          (0 until ABPerfectBatchTestUtils.KBeatCount).map { beat =>
            (bank, ABPerfectBatchTestUtils.address(bank, beat), beat * 2)
          }
        }.toSet
        val actual = observed.map { case (channel, address, source) =>
          val regAddr = (source & ((BigInt(1) << sourceAddrWidth) - 1)).toInt
          (channel, address, regAddr)
        }.toSet
        assert(actual == expected, s"request coverage mismatch: missing=${expected -- actual} extra=${actual -- expected}")
        val expectedWritebacks = expected.flatMap { case (bank, _, regAddr) =>
          Seq((bank, BigInt(regAddr)), (bank, BigInt(regAddr + 1)))
        }
        assert(writebacks.toSet == expectedWritebacks)
        assert(perfectRequests == perfectBatches * dut.ABMatrixRegNBanks)
        assert(perfectBatches == 8, s"expected one perfect batch per beat cycle, got $perfectBatches")
        assert(rtlPerfectBatchPulses == perfectBatches)
        dut.io.perfProbe.loadTotalFired.expect(observed.size.U)
        dut.io.perfProbe.loadPerfectBatches.expect(perfectBatches.U)
        dut.io.perfProbe.loadPerfectRequests.expect(perfectRequests.U)
      }
  }

  ignore should "preserve tail masks and exactly-once writeback under reordered responses" in {
    test(new MultiChannelsABMemLoader("AML", "AML_seeded_tail_reorder_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureAConfig(dut.io.ConfigInfo, rows = 8, kBeatCount = 2,
          baseAddress = BigInt("8004a000", 16), rowStride = 144)
        dut.io.ConfigInfo.ApplicationTensor_A.HasTail.poke(true.B)
        dut.io.ConfigInfo.ApplicationTensor_A.TailByteMask.poke(40.U)
        dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
        clearMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
        dut.clock.step()
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Success.U)

        val sourceAddrWidth = log2Ceil(dut.ABMatrixRegBankNEntries)
        val tailBitOffset = sourceAddrWidth + log2Ceil(dut.ABMatrixRegNBanks)
        val requests = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        while (requests.size < 16) {
          for (channel <- 0 until dut.ABMatrixRegNBanks) {
            val request = dut.io.LocalMMUIO.Request(channel)
            if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
              requests += ((channel, request.bits.RequestAddr.peek().litValue,
                request.bits.RequestSourceID.peek().litValue))
            }
          }
          dut.clock.step()
        }
        assert(requests.distinct.size == 16, "tail fixture duplicated a request")
        val expectedRequests = (0 until 8).flatMap { channel =>
          (0 until 2).map { beat =>
            channel -> (BigInt("8004a000", 16) + channel * 144 + beat * 64)
          }
        }.toSet
        assert(requests.map { case (channel, address, _) => channel -> address }.toSet == expectedRequests)

        val pending = Array.tabulate(dut.ABMatrixRegNBanks) { bank =>
          mutable.Queue.from(requests.collect { case (`bank`, _, source) => source }.reverse)
        }
        val writebacks = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        var cycles = 0
        var completed = false
        while (!completed && cycles < 120) {
          val activeBank = (dut.ABMatrixRegNBanks - 1) - (cycles % dut.ABMatrixRegNBanks)
          for (bank <- 0 until dut.ABMatrixRegNBanks) {
            val response = dut.io.LocalMMUIO.Response(bank)
            val send = cycles % 3 != 1 && bank == activeBank && pending(bank).nonEmpty
            response.valid.poke(send.B)
            response.bits.ReseponseSourceID.poke(pending(bank).headOption.getOrElse(BigInt(0)).U)
            response.bits.ReseponseData.poke((BigInt(bank + 1) << 8).U)
            response.bits.ReseponseConherent.poke(true.B)
          }
          for {
            bank <- 0 until dut.ABMatrixRegNBanks
            lane <- 0 until 2
          } {
            val write = dut.io.ToMatrixRegIO.WriteRequest(bank)(lane)
            if (write.BankAddr.valid.peek().litToBoolean) {
              write.ByteMask.valid.expect(true.B)
              writebacks += ((bank, write.BankAddr.bits.peek().litValue,
                write.ByteMask.bits.peek().litValue))
            }
          }
          val firedBanks = (0 until dut.ABMatrixRegNBanks).filter { bank =>
            dut.io.LocalMMUIO.Response(bank).valid.peek().litToBoolean &&
              dut.io.LocalMMUIO.Response(bank).ready.peek().litToBoolean
          }
          completed = dut.io.ConfigInfo.MicroTaskEndValid.peek().litToBoolean
          dut.clock.step()
          firedBanks.foreach(bank => pending(bank).dequeue())
          cycles += 1
        }
        assert(completed, s"seeded tail task did not complete after $cycles cycles")
        assert(pending.forall(_.isEmpty), "not every reordered response was accepted")

        val fullMask = (BigInt(1) << dut.ABMatrixRegEntryByteSize) - 1
        val tailSecondMask = (BigInt(1) << 8) - 1
        val expectedWritebacks = requests.flatMap { case (bank, _, source) =>
          val regAddr = source & ((BigInt(1) << sourceAddrWidth) - 1)
          val isTail = ((source >> tailBitOffset) & 1) == 1
          Seq(
            (bank, regAddr, fullMask),
            (bank, regAddr + 1, if (isTail) tailSecondMask else fullMask))
        }
        assert(writebacks.size == expectedWritebacks.size)
        assert(writebacks.toSet == expectedWritebacks.toSet,
          s"tail writeback mismatch: missing=${expectedWritebacks.toSet -- writebacks.toSet}")
      }
  }

  ignore should "hold selected A requests stable while downstream ready is low" in {
    test(new MultiChannelsABMemLoader("AML", "AML_ab_stall_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureA(dut)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)

        for (bank <- 0 until dut.ABMatrixRegNBanks) {
          dut.io.LocalMMUIO.Request(bank).ready.poke(false.B)
        }
        var waitCycles = 0
        while (!dut.io.LocalMMUIO.Request(0).valid.peek().litToBoolean && waitCycles < 10) {
          dut.clock.step()
          waitCycles += 1
        }
        dut.clock.step()
        val held = (0 until dut.ABMatrixRegNBanks).map { bank =>
          val request = dut.io.LocalMMUIO.Request(bank)
          request.valid.expect(true.B)
          Seq(
            request.bits.RequestAddr.peek().litValue,
            request.bits.RequestConherent.peek().litValue,
            request.bits.RequestData.peek().litValue,
            request.bits.RequestSourceID.peek().litValue,
            request.bits.RequestType_isWrite.peek().litValue,
            request.bits.UseAllocatedSourceID.peek().litValue,
            request.bits.isA.peek().litValue,
            request.bits.MatrixIsAcc.peek().litValue,
            request.bits.RequestMask.peek().litValue)
        }
        dut.clock.step()
        for (bank <- 0 until dut.ABMatrixRegNBanks) {
          val request = dut.io.LocalMMUIO.Request(bank)
          request.valid.expect(true.B)
          Seq(
            request.bits.RequestAddr,
            request.bits.RequestConherent,
            request.bits.RequestData,
            request.bits.RequestSourceID,
            request.bits.RequestType_isWrite,
            request.bits.UseAllocatedSourceID,
            request.bits.isA,
            request.bits.MatrixIsAcc,
            request.bits.RequestMask).zip(held(bank)).foreach { case (field, value) =>
              field.expect(value.U)
            }
          request.ready.poke(true.B)
        }
        dut.clock.step()
        dut.io.perfProbe.loadTotalFired.expect(8.U)
        dut.io.perfProbe.loadPerfectBatches.expect(1.U)
        dut.io.perfProbe.loadPerfectRequests.expect(8.U)
      }
  }

  ignore should "retain stalled lanes while other selected lanes fire" in {
    test(new MultiChannelsABMemLoader("AML", "AML_ab_asymmetric_stall_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        startA(dut, rows = 16, kBeatCount = 1,
          baseAddress = BigInt("8004a000", 16), rowStride = 144)
        for (bank <- 0 until dut.ABMatrixRegNBanks) {
          dut.io.LocalMMUIO.Request(bank).ready.poke(false.B)
        }
        var waitCycles = 0
        while (!(0 until dut.ABMatrixRegNBanks).forall(
            bank => dut.io.LocalMMUIO.Request(bank).valid.peek().litToBoolean) && waitCycles < 20) {
          dut.clock.step()
          waitCycles += 1
        }
        assert(waitCycles < 20, "compatible fixture did not select all request lanes")
        val stalled = (0 until dut.ABMatrixRegNBanks by 2).map { bank =>
          val request = dut.io.LocalMMUIO.Request(bank)
          bank -> Seq(
            request.bits.RequestAddr.peek().litValue,
            request.bits.RequestConherent.peek().litValue,
            request.bits.RequestData.peek().litValue,
            request.bits.RequestSourceID.peek().litValue,
            request.bits.RequestType_isWrite.peek().litValue,
            request.bits.UseAllocatedSourceID.peek().litValue,
            request.bits.isA.peek().litValue,
            request.bits.MatrixIsAcc.peek().litValue,
            request.bits.RequestMask.peek().litValue)
        }.toMap
        for (bank <- 1 until dut.ABMatrixRegNBanks by 2) {
          dut.io.LocalMMUIO.Request(bank).ready.poke(true.B)
        }
        dut.clock.step()
        dut.io.perfProbe.loadTotalFired.expect(4.U)
        stalled.foreach { case (bank, values) =>
          val request = dut.io.LocalMMUIO.Request(bank)
          request.valid.expect(true.B)
          Seq(
            request.bits.RequestAddr,
            request.bits.RequestConherent,
            request.bits.RequestData,
            request.bits.RequestSourceID,
            request.bits.RequestType_isWrite,
            request.bits.UseAllocatedSourceID,
            request.bits.isA,
            request.bits.MatrixIsAcc,
            request.bits.RequestMask).zip(values).foreach { case (field, value) =>
              field.expect(value.U)
            }
          request.bits.RequestMask.expect(((BigInt(1) << dut.MMUMaskWidth) - 1).U)
          request.ready.poke(true.B)
        }
        dut.clock.step()
        dut.io.perfProbe.loadTotalFired.expect(8.U)
      }
  }

  it should "keep AML and BML independent while LocalMMU enforces A priority" in {
    test(new ABLocalMmuPriorityHarness)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureAConfig(dut.io.aConfig)
        configureBConfig(dut.io.bConfig)
        clearLlc(dut.io.llc, 8)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)

        for (bank <- 0 until 8) {
          dut.io.llc.Request(bank).ready.poke(false.B)
        }
        dut.io.aConfig.MicroTaskValid.poke(true.B)
        dut.io.bConfig.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.aConfig.MicroTaskValid.poke(false.B)
        dut.io.bConfig.MicroTaskValid.poke(false.B)

        var waitCycles = 0
        while ((dut.io.aRequestValid.peek().litValue != 0xff ||
            dut.io.bRequestValid.peek().litValue != 0xff) && waitCycles < 10) {
          dut.clock.step()
          waitCycles += 1
        }
        dut.io.aRequestValid.expect("hff".U)
        dut.io.bRequestValid.expect("hff".U)
        val heldB = (0 until 8).map { bank =>
          (dut.io.bRequestAddress(bank).peek().litValue,
            dut.io.bRequestSourceId(bank).peek().litValue)
        }

        dut.io.localPerf.offered(LocalMMUSourceClass.A).expect(8.U)
        dut.io.localPerf.offered(LocalMMUSourceClass.B).expect(8.U)
        dut.io.localPerf.issued(LocalMMUSourceClass.A).expect(0.U)
        dut.io.localPerf.issued(LocalMMUSourceClass.B).expect(0.U)
        dut.io.localPerf.priorityBlocked(LocalMMUSourceClass.B).expect("hff".U)
        for (lane <- 0 until 8) {
          dut.io.localPerf.selectedOwner(lane).expect(LocalMMUSourceClass.A.U)
          dut.io.llc.Request(lane).valid.expect(true.B)
          dut.io.llc.Request(lane).bits.isA.expect(true.B)
          assert((dut.io.llc.Request(lane).bits.RequestSourceID.peek().litValue & 7) == 1,
            s"LLC lane $lane did not carry the A source tag")
        }

        for (bank <- 0 until 8) {
          dut.io.llc.Request(bank).ready.poke(true.B)
        }
        dut.io.localPerf.issued(LocalMMUSourceClass.A).expect(8.U)
        dut.io.localPerf.issued(LocalMMUSourceClass.B).expect(0.U)
        dut.clock.step()

        dut.io.aPerf.loadTotalFired.expect(8.U)
        dut.io.bPerf.loadTotalFired.expect(0.U)
        dut.io.bRequestValid.expect("hff".U)
        for (bank <- 0 until 8) {
          dut.io.bRequestAddress(bank).expect(heldB(bank)._1.U)
          dut.io.bRequestSourceId(bank).expect(heldB(bank)._2.U)
        }
      }
  }

  ignore should "use the same perfect-batch request path through the BML wrapper" in {
    test(new BMLWrapper("BML_ab_perfect_test"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        configureB(dut)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)

        for (bank <- 0 until dut.ABMatrixRegNBanks) {
          dut.io.LocalMMUIO.Request(bank).ready.poke(false.B)
        }
        var waitCycles = 0
        while (!dut.io.LocalMMUIO.Request(0).valid.peek().litToBoolean && waitCycles < 10) {
          dut.clock.step()
          waitCycles += 1
        }
        for (bank <- 0 until dut.ABMatrixRegNBanks) {
          dut.io.LocalMMUIO.Request(bank).valid.expect(true.B)
          dut.io.LocalMMUIO.Request(bank).ready.poke(true.B)
        }
        dut.clock.step()
        dut.io.perfProbe.loadTotalFired.expect(8.U)
        dut.io.perfProbe.loadPerfectBatches.expect(1.U)
        dut.io.perfProbe.loadPerfectRequests.expect(8.U)
      }
  }
}
