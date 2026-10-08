package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

import scala.collection.mutable

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
      for (channel <- 0 until dut.ABMatrixRegNBanks) {
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

      val responses = Array.fill(dut.ABMatrixRegNBanks)(mutable.Queue.empty[BigInt])
      var cycles = 0
      var activeCycles = 0
      var offered = 0L
      var fired = 0L
      var accepted = 0L
      val done = Array.fill(3)(0)
      val offeredBySource = Array.fill(6)(0L)
      val issuedBySource = Array.fill(6)(0L)
      val collisionMaskedByDomain = Array.fill(4)(0L)
      val evaluatedBySource = Array.fill(6)(0L)
      val priorityBlockedBySource = Array.fill(6)(0L)
      val selectedFireBySource = Array.fill(6)(0L)
      var heldLaneCycles = 0L
      var cLoadRefills = 0L
      val selectedOwnerCycles = Array.fill(7)(0L)
      var cmlPerfectBatchPulses = 0L

      def samplePerf(): Unit = {
        for (source <- 0 until 6) {
          offeredBySource(source) += dut.io.perf.localMMU.offered(source).peek().litValue.toLong
          issuedBySource(source) += dut.io.perf.localMMU.issued(source).peek().litValue.toLong
          evaluatedBySource(source) += dut.io.perf.localMMU.evaluated(source).peek().litValue.bitCount
          priorityBlockedBySource(source) += dut.io.perf.localMMU.priorityBlocked(source).peek().litValue.bitCount
          selectedFireBySource(source) += dut.io.perf.localMMU.selectedFire(source).peek().litValue.bitCount
        }
        for (domain <- 0 until 4) {
          collisionMaskedByDomain(domain) += dut.io.perf.localMMU.collisionMasked(domain).peek().litValue.bitCount
        }
        for (lane <- 0 until dut.ABMatrixRegNBanks) {
          selectedOwnerCycles(dut.io.perf.localMMU.selectedOwner(lane).peek().litValue.toInt) += 1
        }
        if (dut.io.perf.cml.loadPerfectBatch.peek().litToBoolean) {
          cmlPerfectBatchPulses += 1
        }
        heldLaneCycles += dut.io.perf.localMMU.held.peek().litValue.bitCount
        cLoadRefills += dut.io.perf.localMMU.cLoadRefill.peek().litValue.bitCount
      }

      while (done.exists(_ == 0) && cycles < 6000) {
        // Independent, periodic sink backpressure exercises LocalMMU held
        // requests and C>A>B refill without adding another admission filter.
        for (channel <- 0 until dut.ABMatrixRegNBanks) {
          dut.io.mmu2llc.Request(channel).ready.poke(((cycles + channel) % 5 != 0).B)
        }
        for (channel <- 0 until dut.ABMatrixRegNBanks) {
          val response = dut.io.mmu2llc.Response(channel)
          response.valid.poke(responses(channel).nonEmpty.B)
          response.bits.ReseponseSourceID.poke(responses(channel).headOption.getOrElse(BigInt(0)).U)
          response.bits.ReseponseData.poke(0.U)
          response.bits.ReseponseConherent.poke(true.B)
        }

        var offeredThisCycle = 0
        var firedThisCycle = 0
        for (channel <- 0 until dut.ABMatrixRegNBanks) {
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
      // fire and CML batch pulse into the test-visible output.
      dut.clock.step()
      samplePerf()

      assert(done.sameElements(Array(1, 1, 1)),
        s"A/B/C completion mismatch: ${done.mkString(",")}; fired=$fired accepted=$accepted " +
          s"offered=${offeredBySource.mkString(",")} issued=${issuedBySource.mkString(",")} " +
          s"pendingResponses=${responses.map(_.size).mkString(",")} " +
          s"cmlTotal=${dut.io.perf.cml.loadTotalFired.peek().litValue} " +
          s"cmlPerfect=${dut.io.perf.cml.loadPerfectRequests.peek().litValue}")
      assert(fired > 0 && accepted > 0, s"no useful memory traffic: fired=$fired accepted=$accepted")
      assert(offeredBySource.zip(issuedBySource).forall { case (o, i) => o >= i },
        s"scheduler issued more than offered: offered=${offeredBySource.mkString(",")}, issued=${issuedBySource.mkString(",")}")
      assert(issuedBySource.sum == fired,
        s"scheduler/LLC fire mismatch: issued=${issuedBySource.sum} fired=$fired")
      assert(selectedFireBySource.sameElements(issuedBySource),
        s"selected-fire/issued mismatch: selected=${selectedFireBySource.mkString(",")} issued=${issuedBySource.mkString(",")}")
      val collisionSources = Seq(
        LocalMMUSourceClass.CLoad,
        LocalMMUSourceClass.CStore,
        LocalMMUSourceClass.A,
        LocalMMUSourceClass.B
      )
      assert(collisionSources.indices.forall { domain =>
        evaluatedBySource(collisionSources(domain)) >= collisionMaskedByDomain(domain)
      }, s"collision count exceeded evaluated count: evaluated=${evaluatedBySource.mkString(",")} collision=${collisionMaskedByDomain.mkString(",")}")
      assert(collisionMaskedByDomain.sum > 0,
        "concurrent A/B/C run did not exercise any LocalMMU collision masking")
      assert(cmlPerfectBatchPulses == dut.io.perf.cml.loadPerfectBatches.peek().litValue.toLong,
        s"CML perfect pulse/counter mismatch: pulses=$cmlPerfectBatchPulses counter=${dut.io.perf.cml.loadPerfectBatches.peek().litValue}")
      assert(dut.io.perf.cml.loadTotalFired.peek().litValue.toLong == issuedBySource(LocalMMUSourceClass.CLoad),
        s"CML total fired mismatch: counter=${dut.io.perf.cml.loadTotalFired.peek().litValue} issuedC=${issuedBySource(LocalMMUSourceClass.CLoad)}")
      val acceptedPerActiveCycle = if (activeCycles == 0) 0.0 else accepted.toDouble / activeCycles
      println(f"[CUTETopABCTest] cycles=$cycles active=$activeCycles offered=$offered fired=$fired accepted=$accepted accepted/active=$acceptedPerActiveCycle%.3f " +
        s"schedulerOffered=${offeredBySource.mkString(",")} schedulerIssued=${issuedBySource.mkString(",")} " +
        s"evaluated=${evaluatedBySource.mkString(",")} priorityBlocked=${priorityBlockedBySource.mkString(",")} " +
        s"collisionMasked=${collisionMaskedByDomain.mkString(",")} heldLaneCycles=$heldLaneCycles cLoadRefills=$cLoadRefills " +
        s"selectedOwnerCycles=${selectedOwnerCycles.mkString(",")} " +
        s"cmlPerfectBatches=${dut.io.perf.cml.loadPerfectBatches.peek().litValue} cmlPerfectRequests=${dut.io.perf.cml.loadPerfectRequests.peek().litValue}")
    }
  }
}
