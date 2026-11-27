package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import utility.{ChiselDB, Constantin, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

import scala.collection.mutable

object TrMatrixStoreTestParams {
  def apply(banks: Int = 8, entryBytes: Int = 32): Parameters = new Config((_, _, _) => {
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0)
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      Tensor_MN = 64,
      Tensor_K = 64,
      Matrix_MN = banks,
      L2NBanks = banks,
      ReduceWidthByte = entryBytes,
      LoaderBridgeChannelConfig = s"A${banks}B${banks}CL${banks}CS${banks}",
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false)
  })
}

class CUTETopABCTestSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "A8B8CL8CS8 CUTEV2Top with external memory responder"

  private implicit val params: Parameters = new Config((_, _, _) => {
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0)
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      LoaderBridgeChannelConfig = "A8B8CL8CS8",
      L2NBanks = 8,
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false)
  })

  private def lsuData(
    ms: Int,
    isAcc: Boolean,
    isA: Boolean,
    isB: Boolean,
    base: BigInt,
    stride: BigInt,
    row: Int = 128,
    column: Int = 128,
    width: Int = 0
  ): BigInt = {
    (BigInt(ms) << 122) |
      (BigInt(if (isAcc) 1 else 0) << 119) |
      (BigInt(if (isA) 1 else 0) << 118) |
      (BigInt(if (isB) 1 else 0) << 117) |
      ((base & ((BigInt(1) << 48) - 1)) << 69) |
      ((stride & ((BigInt(1) << 48) - 1)) << 21) |
      (BigInt(row) << 12) |
      (BigInt(column) << 3) |
      BigInt(width)
  }

  it should "complete one concurrent A B C load group and report throughput" in {
    test(new CUTEV2Top).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(20000)
      dut.io.ctrl2top.reset.poke(false.B)
      dut.io.ctrl2top.amuCtrl.valid.poke(false.B)
      dut.io.ctrl2top.amuCtrl.bits.op.poke(Bundles.AmuCtrlIO.mlsOp())
      dut.io.ctrl2top.amuCtrl.bits.data.poke(0.U)
      dut.io.mmu2llc.ConherentRequsetSourceID.valid.poke(false.B)
      dut.io.mmu2llc.ConherentRequsetSourceID.bits.poke(0.U)
      dut.io.mmu2llc.nonConherentRequsetSourceID.valid.poke(false.B)
      dut.io.mmu2llc.nonConherentRequsetSourceID.bits.poke(0.U)
      for (channel <- 0 until dut.TrMatrixRegNBanks) {
        dut.io.mmu2llc.Request(channel).ready.poke(true.B)
        dut.io.mmu2llc.Response(channel).valid.poke(false.B)
        dut.io.mmu2llc.Response(channel).bits.ReseponseData.poke(0.U)
        dut.io.mmu2llc.Response(channel).bits.ReseponseConherent.poke(true.B)
        dut.io.mmu2llc.Response(channel).bits.ReseponseSourceID.poke(0.U)
      }

      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.io.perf.localMMU.offered.foreach(_.expect(0.U))
      dut.io.perf.localMMU.evaluated.foreach(_.expect(0.U))
      dut.io.perf.localMMU.priorityBlocked.foreach(_.expect(0.U))
      dut.io.perf.localMMU.selectedFire.foreach(_.expect(0.U))
      dut.io.perf.localMMU.held.expect(0.U)
      dut.io.perf.localMMU.cLoadRefill.expect(0.U)
      dut.reset.poke(false.B)

      def send(data: BigInt): Unit = {
        var waitCycles = 0
        while (!dut.io.ctrl2top.amuCtrl.ready.peek().litToBoolean && waitCycles < 100) {
          dut.clock.step()
          waitCycles += 1
        }
        assert(dut.io.ctrl2top.amuCtrl.ready.peek().litToBoolean, "TaskController did not accept LSU command")
        dut.io.ctrl2top.amuCtrl.valid.poke(true.B)
        dut.io.ctrl2top.amuCtrl.bits.op.poke(Bundles.AmuCtrlIO.mlsOp())
        dut.io.ctrl2top.amuCtrl.bits.data.poke(data.U)
        dut.clock.step()
        dut.io.ctrl2top.amuCtrl.valid.poke(false.B)
      }

      send(lsuData(ms = 0, isAcc = true, isA = false, isB = false,
        base = 0x80049e00L, stride = 0x440L))
      send(lsuData(ms = 0, isAcc = false, isA = true, isB = false,
        base = 0x80000000L, stride = 0x440L))
      send(lsuData(ms = 1, isAcc = false, isA = false, isB = true,
        base = 0x80200000L, stride = 0x440L))

      val responses = Array.fill(dut.TrMatrixRegNBanks)(mutable.Queue.empty[BigInt])
      var cycles = 0
      var activeCycles = 0
      var offered = 0L
      var fired = 0L
      var accepted = 0L
      val done = Array.fill(3)(0)
      val offeredBySource = Array.fill(LocalMMUSourceClass.Count)(0L)
      val issuedBySource = Array.fill(LocalMMUSourceClass.Count)(0L)
      val collisionMaskedByDomain = Array.fill(4)(0L)
      val evaluatedBySource = Array.fill(LocalMMUSourceClass.Count)(0L)
      val priorityBlockedBySource = Array.fill(LocalMMUSourceClass.Count)(0L)
      val selectedFireBySource = Array.fill(LocalMMUSourceClass.Count)(0L)
      var heldLaneCycles = 0L
      var cLoadRefills = 0L
      val selectedOwnerCycles = Array.fill(LocalMMUSourceClass.Count + 1)(0L)
      var accLoadPerfectBatchPulses = 0L

      def samplePerf(): Unit = {
        for (source <- 0 until LocalMMUSourceClass.Count) {
          offeredBySource(source) += dut.io.perf.localMMU.offered(source).peek().litValue.toLong
          issuedBySource(source) += dut.io.perf.localMMU.issued(source).peek().litValue.toLong
          evaluatedBySource(source) += dut.io.perf.localMMU.evaluated(source).peek().litValue.bitCount
          priorityBlockedBySource(source) += dut.io.perf.localMMU.priorityBlocked(source).peek().litValue.bitCount
          selectedFireBySource(source) += dut.io.perf.localMMU.selectedFire(source).peek().litValue.bitCount
        }
        for (domain <- 0 until 4) {
          collisionMaskedByDomain(domain) += dut.io.perf.localMMU.collisionMasked(domain).peek().litValue.bitCount
        }
        for (lane <- 0 until dut.TrMatrixRegNBanks) {
          selectedOwnerCycles(dut.io.perf.localMMU.selectedOwner(lane).peek().litValue.toInt) += 1
        }
        if (dut.io.perf.accLoader0.loadPerfectBatch.peek().litToBoolean) {
          accLoadPerfectBatchPulses += 1
        }
        heldLaneCycles += dut.io.perf.localMMU.held.peek().litValue.bitCount
        cLoadRefills += dut.io.perf.localMMU.cLoadRefill.peek().litValue.bitCount
      }

      while (done.exists(_ == 0) && cycles < 6000) {
        // Independent, periodic sink backpressure exercises LocalMMU held
        // requests and C>A>B refill without adding another admission filter.
        for (channel <- 0 until dut.TrMatrixRegNBanks) {
          dut.io.mmu2llc.Request(channel).ready.poke(((cycles + channel) % 5 != 0).B)
        }
        for (channel <- 0 until dut.TrMatrixRegNBanks) {
          val response = dut.io.mmu2llc.Response(channel)
          response.valid.poke(responses(channel).nonEmpty.B)
          response.bits.ReseponseSourceID.poke(responses(channel).headOption.getOrElse(BigInt(0)).U)
          response.bits.ReseponseData.poke(0.U)
          response.bits.ReseponseConherent.poke(true.B)
        }

        var offeredThisCycle = 0
        var firedThisCycle = 0
        for (channel <- 0 until dut.TrMatrixRegNBanks) {
          val request = dut.io.mmu2llc.Request(channel)
          if (request.valid.peek().litToBoolean) offeredThisCycle += 1
          if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
            firedThisCycle += 1
            val l2Bank = ((request.bits.RequestAddr.peek().litValue >> 6) & 7).toInt
            responses(l2Bank).enqueue(request.bits.RequestSourceID.peek().litValue)
          }
          val response = dut.io.mmu2llc.Response(channel)
          if (response.valid.peek().litToBoolean && response.ready.peek().litToBoolean) {
            responses(channel).dequeue()
            accepted += 1
          }
        }
        if (offeredThisCycle > 0 || firedThisCycle > 0) activeCycles += 1
        offered += offeredThisCycle
        fired += firedThisCycle
        for (index <- 0 until 3) {
          if (dut.io.perf.memEvents(index).peek().litValue != 0) done(index) += 1
        }
        samplePerf()
        dut.clock.step()
        cycles += 1
      }

      // CUTETOP registers the perf bundle once; flush the final scheduler
      // fire and accMemory batch pulse into the test-visible output.
      dut.clock.step()
      samplePerf()

      assert(done.sameElements(Array(1, 1, 1)),
        s"A/B/C completion mismatch: ${done.mkString(",")}; fired=$fired accepted=$accepted " +
          s"offered=${offeredBySource.mkString(",")} issued=${issuedBySource.mkString(",")} " +
          s"pendingResponses=${responses.map(_.size).mkString(",")} " +
          s"accLoader0Total=${dut.io.perf.accLoader0.loadTotalFired.peek().litValue} " +
          s"accLoadPerfect=${dut.io.perf.accLoader0.loadPerfectRequests.peek().litValue}")
      assert(fired > 0 && accepted > 0, s"no useful memory traffic: fired=$fired accepted=$accepted")
      assert(offeredBySource.zip(issuedBySource).forall { case (o, i) => o >= i },
        s"scheduler issued more than offered: offered=${offeredBySource.mkString(",")}, issued=${issuedBySource.mkString(",")}")
      assert(issuedBySource.sum == fired,
        s"scheduler/LLC fire mismatch: issued=${issuedBySource.sum} fired=$fired")
      assert(selectedFireBySource.sameElements(issuedBySource),
        s"selected-fire/issued mismatch: selected=${selectedFireBySource.mkString(",")} issued=${issuedBySource.mkString(",")}")
      val collisionSources = Seq(
        LocalMMUSourceClass.AccLoader0,
        LocalMMUSourceClass.AccStorer0,
        LocalMMUSourceClass.TrLoader0,
        LocalMMUSourceClass.TrLoader1
      )
      assert(collisionSources.indices.forall { domain =>
        evaluatedBySource(collisionSources(domain)) >= collisionMaskedByDomain(domain)
      }, s"collision count exceeded evaluated count: evaluated=${evaluatedBySource.mkString(",")} collision=${collisionMaskedByDomain.mkString(",")}")
      // A conflict-free schedule is a valid outcome. Directed collision and
      // refill behavior is covered by LocalMMUL2BankRouteSpec.
      assert(accLoadPerfectBatchPulses == dut.io.perf.accLoader0.loadPerfectBatches.peek().litValue.toLong,
        s"accMemory perfect pulse/counter mismatch: pulses=$accLoadPerfectBatchPulses counter=${dut.io.perf.accLoader0.loadPerfectBatches.peek().litValue}")
      assert(dut.io.perf.accLoader0.loadTotalFired.peek().litValue.toLong == issuedBySource(LocalMMUSourceClass.AccLoader0),
        s"accMemory total fired mismatch: counter=${dut.io.perf.accLoader0.loadTotalFired.peek().litValue} issuedC=${issuedBySource(LocalMMUSourceClass.AccLoader0)}")
      val acceptedPerActiveCycle = if (activeCycles == 0) 0.0 else accepted.toDouble / activeCycles
      println(f"[CUTETopABCTest] cycles=$cycles active=$activeCycles offered=$offered fired=$fired accepted=$accepted accepted/active=$acceptedPerActiveCycle%.3f " +
        s"schedulerOffered=${offeredBySource.mkString(",")} schedulerIssued=${issuedBySource.mkString(",")} " +
        s"evaluated=${evaluatedBySource.mkString(",")} priorityBlocked=${priorityBlockedBySource.mkString(",")} " +
        s"collisionMasked=${collisionMaskedByDomain.mkString(",")} heldLaneCycles=$heldLaneCycles cLoadRefills=$cLoadRefills " +
        s"selectedOwnerCycles=${selectedOwnerCycles.mkString(",")} " +
        s"accLoadPerfectBatches=${dut.io.perf.accLoader0.loadPerfectBatches.peek().litValue} accLoadPerfectRequests=${dut.io.perf.accLoader0.loadPerfectRequests.peek().litValue}")
    }
  }
}

/** CUTE top-level A/B matrix-store integration in the existing top test file. */
class TrMatrixStoreTopSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CUTE A/B matrix-register store integration"

  for (loaderConfig <- Seq("TL0LTL1LAL0LTS0LAS0L", "TL04TL14AL01TS01AS01", "TL0LTL14AL01TS02AS01")) {
    it should s"round-trip A/B whole images and transpose through $loaderConfig" in {
      implicit val p: Parameters = new Config((_, _, _) => {
        case PerfCounterOptionsKey => PerfCounterOptions(
          enablePerfPrint = false,
          enablePerfDB = false,
          perfLevel = XSPerfLevel.NORMAL,
          perfDBHartID = 0)
        case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
          Tensor_MN = 128,
          Tensor_K = 64,
          LoaderBridgeChannelConfig = loaderConfig,
          MatrixExtension = MatrixIsaParams(enableInt8Int32 = true),
          Debug = CuteDebugParams.NoDebug,
          EnableDifftest = false)
      })
      ChiselDB.init(false)
      Constantin.init(false)
      test(new CUTEV2Top).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(15000)
        dut.io.ctrl2top.reset.poke(false.B)
        dut.io.ctrl2top.amuCtrl.valid.poke(false.B)
        dut.io.ctrl2top.amuCtrl.bits.op.poke(1.U)
        dut.io.ctrl2top.amuCtrl.bits.data.poke(0.U)
        dut.io.ctrl2top.mrelease.ready.poke(true.B)
        dut.io.mmu2llc.ConherentRequsetSourceID.valid.poke(false.B)
        dut.io.mmu2llc.ConherentRequsetSourceID.bits.poke(0.U)
        dut.io.mmu2llc.nonConherentRequsetSourceID.valid.poke(false.B)
        dut.io.mmu2llc.nonConherentRequsetSourceID.bits.poke(0.U)
        for (lane <- 0 until 8) {
          dut.io.mmu2llc.Request(lane).ready.poke(false.B)
          dut.io.mmu2llc.Response(lane).valid.poke(false.B)
          dut.io.mmu2llc.Response(lane).bits.ReseponseSourceID.poke(0.U)
          dut.io.mmu2llc.Response(lane).bits.ReseponseConherent.poke(true.B)
          dut.io.mmu2llc.Response(lane).bits.ReseponseData.poke(0.U)
        }
        dut.clock.step(3)
        val inputA = 0x80000000L
        val inputB = 0x80010000L
        val outputA = 0x80100000L
        val outputB = 0x80110000L
        val outputAT = 0x80120000L
        val outputBT = 0x80130000L
        def dataByte(reg: Int, row: Int, column: Int): Int =
          (reg * 53 + row * 31 + column * 7) & 255
        def word(bytes: Seq[Int]): BigInt = bytes.zipWithIndex.foldLeft(BigInt(0)) {
          case (value, (byte, index)) => value | (BigInt(byte) << (8 * index))
        }
        def lsu(reg: Int, store: Boolean, isB: Boolean, transpose: Boolean,
            base: Long, columns: Int = 64, width: Int = 0, rows: Int = 128): BigInt = {
          val rowBytes = if (width == 7) columns / 2 else columns << width
          val stride = if (transpose) rows << width else rowBytes
          (BigInt(reg) << 122) | (BigInt(if (store) 1 else 0) << 121) |
            (BigInt(if (transpose) 1 else 0) << 120) |
            (BigInt(if (!isB) 1 else 0) << 118) | (BigInt(if (isB) 1 else 0) << 117) |
            (BigInt(base) << 69) | (BigInt(stride) << 21) |
            (BigInt(if (isB) columns else rows) << 12) |
            (BigInt(if (isB) rows else columns) << 3) | BigInt(width)
        }

        val concurrentLoad = lsu(2, store = false, isB = false, transpose = false, inputA)
        val commands = mutable.Queue[(Int, BigInt)](
          1 -> lsu(0, store = false, isB = false, transpose = false, inputA),
          1 -> lsu(0, store = true, isB = false, transpose = false, outputA),
          1 -> concurrentLoad,
          1 -> lsu(1, store = false, isB = true, transpose = false, inputB),
          1 -> lsu(1, store = true, isB = true, transpose = false, outputB),
          1 -> lsu(0, store = true, isB = false, transpose = true, outputAT, 64),
          1 -> lsu(1, store = true, isB = true, transpose = true, outputBT, 41),
          2 -> BigInt(0))
        val typedStores = Seq(
          (false, 0x80140000L, 11, 32, 1),
          (false, 0x80150000L, 9, 16, 2),
          (false, 0x80160000L, 7, 128, 7),
          (true, 0x80170000L, 32, 11, 1),
          (true, 0x80180000L, 16, 7, 2),
          (true, 0x80190000L, 32, 17, 1),
          (true, 0x801a0000L, 32, 32, 1),
          (true, 0x801b0000L, 16, 9, 2),
          (true, 0x801c0000L, 16, 16, 2))
        // Insert typed B stores before release; all must use trStorer0.
        commands.dequeueAll(_._1 == 2)
        for ((transpose, base, rows, columns, width) <- typedStores) {
          commands.enqueue(1 -> lsu(1, true, true, transpose, base, columns, width, rows))
        }
        commands.enqueue(2 -> BigInt(0))
        val expected = mutable.Map.empty[Long, BigInt]
        for ((base, reg) <- Seq(outputA -> 0, outputB -> 1); row <- 0 until 128) {
          expected(base + row * 64) = word((0 until 64).map(column => dataByte(reg, row, column)))
        }
        for ((base, reg, columns) <- Seq((outputAT, 0, 64), (outputBT, 1, 41));
             column <- 0 until columns; beat <- 0 until 2) {
          expected(base + column * 128 + beat * 64) = word(
            (0 until 64).map(row => dataByte(reg, row + beat * 64, column)))
        }
        for ((transpose, base, rows, columns, width) <- typedStores) {
          val bytes = if (width == 7) 1 else 1 << width
          val memoryRows = if (transpose) columns else rows
          for (row <- 0 until memoryRows) {
            expected(base + row * 64) = word((0 until 64).map { byte =>
              if (transpose) dataByte(1, byte / bytes, row * bytes + byte % bytes)
              else dataByte(1, row, byte)
            })
          }
        }
        case class Response(id: BigInt, data: BigInt, due: Int)
        val queued = Array.fill(8)(mutable.Queue.empty[Response])
        val held = Array.fill[Option[Response]](8)(None)
        val seen = mutable.Set.empty[Long]
        var cycle = 0
        var release = false
        var loads = 0
        var storesDone = 0
        var concurrentALoad = false
        var ackOutsideQuota = false
        val storeChannels = dut.TrStorer0RequestChannelCount
        while (!release && cycle < 12000) {
          // Present the independent load after a store has started. Dynamic FU
          // dispatch may otherwise finish it before the first store request.
          val waitForStore = commands.headOption.contains(1 -> concurrentLoad) &&
            !seen.exists(addr => addr >= outputA && addr < outputB)
          dut.io.ctrl2top.amuCtrl.valid.poke((commands.nonEmpty && !waitForStore).B)
          if (commands.nonEmpty) {
            dut.io.ctrl2top.amuCtrl.bits.op.poke(commands.front._1.U)
            dut.io.ctrl2top.amuCtrl.bits.data.poke(commands.front._2.U)
            if (!waitForStore && dut.io.ctrl2top.amuCtrl.ready.peek().litToBoolean) commands.dequeue()
          }
          for (lane <- 0 until 8) {
            val request = dut.io.mmu2llc.Request(lane)
            val ready = (cycle + lane) % 5 != 0
            request.ready.poke(ready.B)
            if (request.valid.peek().litToBoolean && ready) {
              val id = request.bits.RequestSourceID.peek().litValue
              val address = request.bits.RequestAddr.peek().litValue.toLong
              val write = request.bits.RequestType_isWrite.peek().litToBoolean
              val data = if (write) {
                assert(expected.get(address).contains(request.bits.RequestData.peek().litValue),
                  s"bad store at $address")
                assert(seen.add(address), s"duplicate write at $address")
                assert((id & 7) == 6, "all TR stores must use trStorer0")
                assert(lane < storeChannels, "trStorer0 request exceeded its configured channel quota")
                BigInt(0)
              } else {
                val reg = if (address >= inputB) 1 else 0
                val row = ((address - (if (reg == 1) inputB else inputA)) / 64).toInt
                assert(row >= 0 && row < 128)
                loads += 1
                if (reg == 0 && seen.exists(addr => addr >= outputA && addr < outputB)) {
                  concurrentALoad = true
                }
                word((0 until 64).map(column => dataByte(reg, row, column)))
              }
              val responseLane = if (write) ((id >> 3).toInt + 3) % 8 else lane
              queued(responseLane).enqueue(Response(
                id, data, cycle + (if (write) 17 else 3)))
            }
            if (held(lane).isEmpty && queued(lane).headOption.exists(_.due <= cycle)) {
              held(lane) = Some(queued(lane).dequeue())
            }
            val response = dut.io.mmu2llc.Response(lane)
            response.valid.poke(held(lane).nonEmpty.B)
            held(lane).foreach { item =>
              response.bits.ReseponseSourceID.poke(item.id.U)
              response.bits.ReseponseData.poke(item.data.U)
            }
            if (held(lane).nonEmpty && response.ready.peek().litToBoolean) {
              if ((held(lane).get.id & 7) == 6 && lane >= storeChannels) ackOutsideQuota = true
              held(lane) = None
            }
          }
          if (dut.io.perf.memEvents(3).peek().litValue != 0) storesDone += 1
          release = dut.io.ctrl2top.mrelease.valid.peek().litToBoolean
          if (release) {
            assert(commands.isEmpty && seen.size == expected.size && loads == 384 && storesDone == 4 + typedStores.size)
            assert(queued.forall(_.isEmpty) && held.forall(_.isEmpty),
              "release preceded memory drain")
          }
          dut.clock.step()
          cycle += 1
        }
        assert(release, "CUTE A/B matrix-store integration timed out")
        assert(concurrentALoad, "TR load did not make progress while trStorer0 was active")
        assert(ackOutsideQuota, "fixture did not exercise ACK reception outside request quota")
        dut.io.ctrl2top.amuCtrl.valid.poke(false.B)
        dut.io.mmu2llc.Response.foreach(_.valid.poke(false.B))
        dut.clock.step(4)
        dut.io.ctrl2top.amuCtrl.bits.op.poke(1.U)
        dut.io.ctrl2top.amuCtrl.bits.data.poke(lsu(0, true, false, false, outputA, 63, rows = 1).U)
        dut.io.ctrl2top.amuCtrl.ready.expect(true.B)
        dut.io.ctrl2top.amuCtrl.valid.poke(true.B)
        dut.clock.step()
        dut.io.ctrl2top.amuCtrl.valid.poke(false.B)
        assertThrows[ChiselAssertionError] { dut.clock.step(30) }
      }
    }
  }
}

/** Exercise the store backend and real parity SRAMs without the compute array. */
class TrStorerBackendHarness(channels: Option[Int])(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new TrStoreCommand))
    val done = Decoupled(Bool())
    val memory = Flipped(new LocalMMUIO)
    val load = new TrLoadMatrixRegIO
    val readFire = Output(Bool())
    val readAddresses = Output(Vec(TrMatrixRegNBanks,
      Vec(outsideDataWidthByte / TrMatrixRegEntryByteSize, UInt(log2Ceil(TrMatrixRegBankNEntries).W))))
  })
  val store = channels match {
    case Some(count) => Module(new MultiChannelTrStorer(count, "backend_test")).io
    case None => Module(new TrStorerLegacy).io
  }
  val register = Module(new TrMatrixReg(0))
  store.command <> io.command
  store.done <> io.done
  store.memory <> io.memory
  register.io.MatrixRegIO.FromMemoryLoader <> io.load
  register.io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
  register.io.MatrixRegIO.FromDataController.BankAddr.bits :=
    0.U.asTypeOf(register.io.MatrixRegIO.FromDataController.BankAddr.bits)
  register.io.MatrixRegIO.FromStore <> store.read
  io.readFire := store.read.BankAddr.fire
  io.readAddresses := store.read.BankAddr.bits
}

class TrStorerBackendSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TR store SRAM reads and output backpressure"

  for ((tensorRows, banks, entryBytes, channels, tensorBytes) <- Seq(
    (128, 8, 32, Some(4), 128), (128, 8, 32, Some(8), 128), (128, 4, 32, Some(2), 128),
    (128, 4, 64, None, 128), (128, 4, 64, Some(4), 128),
    (8, 4, 32, None, 128), (8, 4, 32, Some(2), 128),
    (128, 8, 32, None, 64), (128, 8, 32, Some(1), 64), (128, 8, 32, Some(2), 64),
    (128, 8, 32, Some(8), 64))) {
    val mode = channels.map(_.toString).getOrElse("L")
    it should s"preserve data, paired reads and ACK drain with $banks banks, $entryBytes bytes, TS0$mode, $tensorRows rows, K$tensorBytes" in {
      implicit val p: Parameters = new Config((_, _, _) => {
        case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
          Tensor_MN = tensorRows, Tensor_K = tensorBytes, Matrix_MN = banks,
          ReduceWidthByte = entryBytes, L2NBanks = banks,
          LoaderBridgeChannelConfig = s"TL0LTL1LAL0LTS0${mode}AS0L",
          Debug = CuteDebugParams.NoDebug, EnableDifftest = false)
      })
      test(new TrStorerBackendHarness(channels)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(20000)
        dut.io.command.valid.poke(false.B)
        dut.io.command.bits.regId.poke(0.U)
        dut.io.command.bits.base.poke(0.U)
        dut.io.command.bits.stride.poke(0.U)
        dut.io.command.bits.rows.poke(0.U)
        dut.io.command.bits.columns.poke(0.U)
        dut.io.command.bits.widths.poke(0.U)
        dut.io.command.bits.transpose.poke(false.B)
        dut.io.done.ready.poke(false.B)
        dut.io.load.active.poke(false.B)
        dut.io.memory.ConherentRequsetSourceID.valid.poke(false.B)
        dut.io.memory.ConherentRequsetSourceID.bits.poke(0.U)
        dut.io.memory.nonConherentRequsetSourceID.valid.poke(false.B)
        dut.io.memory.nonConherentRequsetSourceID.bits.poke(0.U)
        for (bank <- 0 until banks) {
          dut.io.memory.Request(bank).ready.poke(false.B)
          dut.io.memory.Response(bank).valid.poke(false.B)
          dut.io.memory.Response(bank).bits.ReseponseData.poke(0.U)
          dut.io.memory.Response(bank).bits.ReseponseSourceID.poke(0.U)
          dut.io.memory.Response(bank).bits.ReseponseConherent.poke(true.B)
          for (lane <- 0 until 2) {
            val write = dut.io.load.WriteRequest(bank)(lane)
            write.BankAddr.valid.poke(false.B)
            write.BankAddr.bits.poke(0.U)
            write.Data.valid.poke(false.B)
            write.Data.bits.poke(0.U)
            write.NibbleMask.valid.poke(false.B)
            write.NibbleMask.bits.poke(0.U)
          }
        }
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.clock.step()
        def sourceByte(row: Int, byte: Int): Int = (row * 31 + byte * 7 + (byte / 32) * 53) & 255
        def packed(bytes: Seq[Int]): BigInt = bytes.zipWithIndex.foldLeft(BigInt(0)) {
          case (value, (byte, index)) => value | (BigInt(byte) << (8 * index))
        }
        val entriesPerRow = tensorBytes / entryBytes
        dut.io.load.active.poke(true.B)
        for (rowGroup <- 0 until tensorRows / banks; entry <- 0 until entriesPerRow by 2) {
          for (bank <- 0 until banks; lane <- 0 until 2) {
            val write = dut.io.load.WriteRequest(bank)(lane)
            write.BankAddr.valid.poke(true.B)
            write.BankAddr.bits.poke((rowGroup * entriesPerRow + entry + lane).U)
            write.Data.valid.poke(true.B)
            write.Data.bits.poke(packed((0 until entryBytes).map { byte =>
              sourceByte(rowGroup * banks + bank, (entry + lane) * entryBytes + byte)
            }).U)
            write.NibbleMask.valid.poke(true.B)
            write.NibbleMask.bits.poke(((BigInt(1) << (2 * entryBytes)) - 1).U)
          }
          dut.clock.step()
        }
        dut.io.load.active.poke(false.B)
        dut.io.load.WriteRequest.foreach(_.foreach { write =>
          write.BankAddr.valid.poke(false.B)
          write.Data.valid.poke(false.B)
          write.NibbleMask.valid.poke(false.B)
        })
        val readLanes = 64 / entryBytes
        val quota = channels.getOrElse(1)
        var totalWrites = 0
        var totalReads = 0
        // Keep the small-MN, 128-byte row case and the default full-register bound.
        val normalCases = Seq((false, 0, tensorRows, tensorBytes), (false, 0, 1, tensorBytes),
          (false, 7, math.min(11, tensorRows), tensorBytes * 2))
        // Cross the paired-entry boundary and the next pair for every transpose width.
        val transposeCases = (Seq((true, 0, 128, 65), (true, 0, 128, 128),
          (true, 1, 64, 33), (true, 1, 32, 64),
          (true, 2, 32, 17), (true, 2, 16, 32),
          (true, 0, 64, 1)) ++ (0 to 2).map { width =>
            (true, width, tensorRows, tensorBytes >> width)
          }).distinct.filter { case (_, width, rows, columns) =>
            rows <= tensorRows && columns * (1 << width) <= tensorBytes &&
              rows * (1 << width) % 64 == 0
          }
        val packedTransposeCases = if (tensorRows >= 128)
          Seq(1, 3, 2 * entryBytes + 1, 2 * tensorBytes).distinct.map(k => (true, 7, 128, k))
        else Seq.empty
        val cases = normalCases ++ transposeCases ++ packedTransposeCases
        val schedules = mutable.Map.empty[(Boolean, Int, Int, Int), Seq[(Int, Int, BigInt)]]
        for ((transpose, width, rows, columns) <- cases; addressMode <- 0 until 3) {
          val elementBytes = if (width == 7) 1 else 1 << width
          val rowBytes = if (width == 7) columns / 2 else columns * elementBytes
          val outputRowBytes = if (transpose && width == 7) rows / 2 else if (transpose) rows * elementBytes else rowBytes
          val outputRows = if (transpose) columns else rows
          // Exercise long address carries and modulo wrapping at bit 64.
          // One effective row still permits an arbitrary, unaligned stride.
          val stride = if (outputRows == 1) BigInt(7) else addressMode match {
            case 0 => BigInt(outputRowBytes + 64)
            case 1 => BigInt("7fffffff00ffffc0", 16)
            case 2 => (BigInt(1) << 48) + (BigInt(1) << 22) + 64
          }
          val base = addressMode match {
            case 0 => if (width == 7) (BigInt(1) << 64) - 64 else BigInt(0x10000)
            case 1 => (BigInt(1) << 64) - 64
            case 2 => (BigInt(1) << 54) - 64
          }
          val addressMask = (BigInt(1) << 64) - 1
          val expected = (for (row <- 0 until outputRows; offset <- 0 until outputRowBytes by 64) yield {
            val data = packed((offset until offset + 64).map { byte =>
              if (transpose && width == 7) {
                def nibble(r: Int): Int = (sourceByte(r, row / 2) >> (4 * (row % 2))) & 15
                nibble(2 * byte) | (nibble(2 * byte + 1) << 4)
              } else if (transpose) sourceByte(byte / elementBytes, row * elementBytes + byte % elementBytes)
              else sourceByte(row, byte)
            })
            ((base + row * stride + offset) & addressMask) -> data
          }).toMap
          dut.io.command.ready.expect(true.B)
          dut.io.command.bits.base.poke(base.U)
          dut.io.command.bits.stride.poke(stride.U)
          dut.io.command.bits.rows.poke(rows.U)
          dut.io.command.bits.columns.poke(columns.U)
          dut.io.command.bits.widths.poke(width.U)
          dut.io.command.bits.transpose.poke(transpose.B)
          dut.io.command.valid.poke(true.B)
          dut.clock.step()
          dut.io.command.valid.poke(false.B)
          assert(expected.size == outputRows * (outputRowBytes / 64), "test addresses overlap after wrapping")
          val schedule = mutable.ArrayBuffer.empty[(Int, Int, BigInt)]
          val seen = mutable.Set.empty[BigInt]
          val replies = mutable.ArrayBuffer.empty[BigInt]
          val stalled = Array.fill[Option[(BigInt, BigInt, BigInt)]](banks)(None)
          var reads = 0
          var cycles = 0
          while (seen.size < expected.size && cycles < 10000) {
            dut.io.done.valid.expect(false.B)
            // Rewriting the original entry stalls SRAM reads without changing
            // the oracle. Precomputed addresses must survive these write cycles.
            val injectWrite = cycles % 13 == 0
            dut.io.load.active.poke(injectWrite.B)
            val stallWrite = dut.io.load.WriteRequest(0)(0)
            stallWrite.BankAddr.valid.poke(injectWrite.B)
            stallWrite.BankAddr.bits.poke(0.U)
            stallWrite.Data.valid.poke(injectWrite.B)
            stallWrite.Data.bits.poke(packed((0 until entryBytes).map(sourceByte(0, _))).U)
            stallWrite.NibbleMask.valid.poke(injectWrite.B)
            stallWrite.NibbleMask.bits.poke(((BigInt(1) << (2 * entryBytes)) - 1).U)
            if (injectWrite) dut.io.readFire.expect(false.B)
            if (dut.io.readFire.peek().litToBoolean) {
              schedule += ((cycles, -1, BigInt(0)))
              reads += 1
              if (readLanes == 2) {
                dut.io.readAddresses.foreach { addresses =>
                  val first = addresses(0).peek().litValue
                  assert(addresses(1).peek().litValue == first + 1, "store did not read both parity SRAMs")
                }
              }
            }
            // Return earlier requests while newer requests are being accepted,
            // but retain ACKs for the final drain check. Numeric ACKs may use any lane.
            dut.io.memory.Response.foreach(_.valid.poke(false.B))
            if (cycles % 7 == 0 && replies.size > banks) {
              val count = if (channels.isEmpty) 1 else banks
              for (lane <- 0 until count) {
                val response = dut.io.memory.Response(lane)
                response.valid.poke(true.B)
                response.bits.ReseponseSourceID.poke(replies.remove(replies.size - 1).U)
                response.ready.expect(true.B)
              }
            }
            for (lane <- 0 until banks) {
              val request = dut.io.memory.Request(lane)
              val ready = cycles % 9 >= 4 && (cycles + lane) % 3 != 0
              request.ready.poke(ready.B)
              val valid = request.valid.peek().litToBoolean
              val current = (request.bits.RequestSourceID.peek().litValue,
                request.bits.RequestAddr.peek().litValue, request.bits.RequestData.peek().litValue)
              stalled(lane).foreach { previous =>
                assert(valid && current == previous, "store request changed under backpressure")
              }
              stalled(lane) = if (valid && !ready) Some(current) else None
              if (valid) assert(lane < quota, "store request exceeded its configured quota")
              if (valid && ready) {
                request.bits.RequestType_isWrite.expect(true.B)
                request.bits.RequestConherent.expect(true.B)
                request.bits.RequestMask.expect(((BigInt(1) << 64) - 1).U)
                assert(expected.get(current._2).contains(current._3),
                  s"bad data: transpose=$transpose width=$width address=${current._2}")
                assert(seen.add(current._2), "duplicate store request")
                schedule += ((cycles, lane, current._1))
                replies += current._1
              }
            }
            dut.clock.step()
            cycles += 1
          }
          dut.io.load.active.poke(false.B)
          val stallWrite = dut.io.load.WriteRequest(0)(0)
          stallWrite.BankAddr.valid.poke(false.B)
          stallWrite.Data.valid.poke(false.B)
          stallWrite.NibbleMask.valid.poke(false.B)
          assert(seen.toSet == expected.keySet, "store did not emit the complete tile")
          val expectedReads = if (!transpose) {
            ((rows + banks - 1) / banks) * (rowBytes / 64)
          } else {
            val entryRows = if (width == 7) 2 * entryBytes else entryBytes / elementBytes
            val groups = (0 until columns by entryRows * readLanes).map { start =>
              (math.min(entryRows, columns - start) + banks - 1) / banks
            }.sum
            groups * (outputRowBytes / 64) * (if (width == 7) 128 / banks else 64 / (banks * elementBytes))
          }
          assert(reads == expectedReads, s"SRAM read count $reads != $expectedReads")
          totalReads += reads
          totalWrites += seen.size
          dut.io.memory.Request.foreach(_.ready.poke(false.B))
          dut.io.memory.Response.foreach(_.valid.poke(false.B))
          // No task completion is allowed before the last ACK, including out-of-order returns.
          dut.clock.step(5)
          dut.io.done.valid.expect(false.B)
          for (batch <- replies.reverse.grouped(if (channels.isEmpty) 1 else banks)) {
            for ((id, index) <- batch.zipWithIndex) {
              val lane = if (channels.isEmpty) 0 else (index + quota) % banks
              val response = dut.io.memory.Response(lane)
              response.valid.poke(true.B)
              response.bits.ReseponseSourceID.poke(id.U)
              response.ready.expect(true.B)
            }
            dut.io.done.valid.expect(false.B)
            dut.clock.step()
            dut.io.memory.Response.foreach(_.valid.poke(false.B))
          }
          var drain = 0
          while (!dut.io.done.valid.peek().litToBoolean && drain < 8) {
            dut.clock.step()
            drain += 1
          }
          dut.io.done.valid.expect(true.B)
          schedule += ((cycles, -2, BigInt(drain)))
          val shape = (transpose, width, rows, columns)
          if (addressMode == 0) schedules(shape) = schedule.toSeq
          else assert(schedule.toSeq == schedules(shape), "address operands changed read/request/completion cycles")
          println(s"[TrStoreSchedule] B$banks E$entryBytes TS0$mode M$tensorRows K$tensorBytes " +
            s"shape=$shape addressMode=$addressMode cycles=$cycles drain=$drain " +
            s"trace=${scala.util.hashing.MurmurHash3.seqHash(schedule.toSeq)}")
          dut.io.command.ready.expect(false.B)
          dut.clock.step(3)
          dut.io.done.valid.expect(true.B)
          dut.io.done.ready.poke(true.B)
          dut.clock.step()
          dut.io.done.ready.poke(false.B)
          dut.io.command.ready.expect(true.B)
        }
        // Empty commands complete without a read/request, including unaligned metadata.
        for ((rows, columns) <- Seq((0, 0), (0, 64), (1, 0))) {
          dut.io.command.bits.base.poke(3.U)
          dut.io.command.bits.stride.poke(7.U)
          dut.io.command.bits.rows.poke(rows.U)
          dut.io.command.bits.columns.poke(columns.U)
          dut.io.command.bits.widths.poke(0.U)
          dut.io.command.bits.transpose.poke(false.B)
          dut.io.command.valid.poke(true.B)
          dut.io.command.ready.expect(true.B)
          dut.clock.step()
          dut.io.command.valid.poke(false.B)
          dut.io.readFire.expect(false.B)
          dut.io.memory.Request.foreach(_.valid.expect(false.B))
          dut.io.done.valid.expect(true.B)
          dut.io.done.ready.poke(true.B)
          dut.clock.step()
          dut.io.done.ready.poke(false.B)
        }
        println(s"[TrStorerBackend] banks=$banks entry=$entryBytes TS0=$mode reads=$totalReads writes=$totalWrites")
      }
    }
  }
}
