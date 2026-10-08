package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.{Config, Parameters}
import utility._

object ResponseChannelBridgeTestConfig {
  def params(queueEnabled: Boolean = false): Parameters = new Config((_, _, _) => {
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0)
    case CuteParamsKey => CuteParams.baseParams.copy(
      outsideDataWidth = 256,
      ReduceWidthByte = 32,
      Matrix_MN = 8,
      EnableOmegaInputQueue = queueEnabled
    )
  })
}

class ResponseChannelBridgeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ResponseChannelBridge"

  private val bankCount = 8
  private val dataWidth = 256
  private val sourceIdWidth = 64
  private val bankIdWidth = 3
  private val bankIdOffset = 8
  private val queueDepth = 2

  private def encodeSourceId(bankId: Int, regAddr: Int = 0): BigInt = {
    (BigInt(bankId) << bankIdOffset) | BigInt(regAddr)
  }

  private def initBridge(dut: ResponseChannelBridge): Unit = {
    dut.io.timeStamp.poke(0.U)
    dut.io.serviceTraceActive.poke(false.B)
    dut.io.releaseDebug.poke(false.B)
    for (i <- 0 until bankCount) {
      dut.io.in(i).valid.poke(false.B)
      dut.io.in(i).bits.ReseponseData.poke(0.U)
      dut.io.in(i).bits.ReseponseSourceID.poke(0.U)
      dut.io.in(i).bits.ReseponseConherent.poke(true.B)
      dut.io.out(i).ready.poke(true.B)
    }
  }

  private def driveResp(
    dut: ResponseChannelBridge,
    channel: Int,
    bankId: Int,
    data: Int,
    valid: Boolean = true,
    regAddr: Int = 0
  ): Unit = {
    dut.io.in(channel).valid.poke((if (valid) true.B else false.B))
    dut.io.in(channel).bits.ReseponseData.poke(data.U)
    dut.io.in(channel).bits.ReseponseSourceID.poke(encodeSourceId(bankId, regAddr).U)
    dut.io.in(channel).bits.ReseponseConherent.poke(true.B)
  }

  private def runGroupedBridgeRoutingTest(respChannelCount: Int, banksToSend: Seq[Int]): Unit = {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = respChannelCount,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params())).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)

      for (i <- 0 until bankCount) {
        dut.io.out(i).ready.poke(false.B)
      }

      val accepted = Array.fill(banksToSend.length)(false)
      var cycles = 0
      val maxCycles = 32

      while (accepted.contains(false) && cycles < maxCycles) {
        banksToSend.zipWithIndex.foreach {
          case (bankId, ch) =>
            driveResp(
              dut,
              channel = ch,
              bankId = bankId,
              data = 1000 + bankId,
              valid = !accepted(ch),
              regAddr = ch + 1
            )
        }

        val fireThisCycle = banksToSend.indices.map { ch =>
          !accepted(ch) && dut.io.in(ch).ready.peek().litToBoolean
        }
        dut.clock.step()
        fireThisCycle.zipWithIndex.foreach {
          case (fired, ch) =>
            if (fired) {
              accepted(ch) = true
            }
        }
        cycles += 1
      }

      assert(accepted.forall(identity), s"not all grouped responses were accepted within $maxCycles cycles")

      for (i <- 0 until bankCount) {
        dut.io.in(i).valid.poke(false.B)
      }

      banksToSend.zipWithIndex.foreach {
        case (bankId, ch) =>
          dut.io.out(bankId).valid.expect(
            true.B,
            s"bank $bankId should receive its response in $respChannelCount-channel mode"
          )
          dut.io.out(bankId).bits.ReseponseSourceID.expect(encodeSourceId(bankId, ch + 1).U)
          dut.io.out(bankId).bits.ReseponseData.expect((1000 + bankId).U)
      }
    }
  }

  it should "restore bank-correct outputs in 2-channel mode without stalling unrelated banks" in {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = 2,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params())).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)

      dut.io.out(0).ready.poke(false.B)
      driveResp(dut, channel = 0, bankId = 0, data = 100, regAddr = 1)
      driveResp(dut, channel = 1, bankId = 1, data = 101, regAddr = 2)
      driveResp(dut, channel = 2, bankId = 4, data = 200, regAddr = 3)

      dut.io.in(0).ready.expect(true.B)
      dut.io.in(1).ready.expect(false.B)
      dut.io.in(2).ready.expect(true.B)
      dut.clock.step()

      driveResp(dut, channel = 0, bankId = 0, data = 0, valid = false)
      driveResp(dut, channel = 1, bankId = 1, data = 101, regAddr = 2)
      driveResp(dut, channel = 2, bankId = 4, data = 0, valid = false)

      dut.io.in(1).ready.expect(true.B)
      dut.clock.step()

      for (i <- 0 until bankCount) {
        dut.io.in(i).valid.poke(false.B)
      }

      dut.io.out(0).valid.expect(true.B)
      dut.io.out(1).valid.expect(true.B)
      dut.io.out(1).bits.ReseponseSourceID.expect(encodeSourceId(1, 2).U)
      dut.io.out(1).bits.ReseponseData.expect(101.U)
      dut.io.out(1).bits.ReseponseConherent.expect(true.B)
      dut.clock.step()

      dut.io.out(0).ready.poke(true.B)
      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.ReseponseSourceID.expect(encodeSourceId(0, 1).U)
      dut.io.out(0).bits.ReseponseData.expect(100.U)
    }
  }

  it should "restore grouped responses back to the correct bank outputs in 4-channel mode" in {
    runGroupedBridgeRoutingTest(respChannelCount = 4, banksToSend = Seq(1, 3, 5, 7))
  }

  it should "restore grouped responses back to the correct bank outputs in 1-channel mode" in {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = 1,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params())).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)
      for (i <- 0 until bankCount) {
        dut.io.out(i).ready.poke(false.B)
      }

      Seq(0, 3, 7).zipWithIndex.foreach {
        case (bankId, ch) =>
          driveResp(dut, channel = ch, bankId = bankId, data = 1000 + bankId, regAddr = ch + 1)
          dut.io.in(ch).ready.expect(true.B)
          dut.clock.step()
          dut.io.in(ch).valid.poke(false.B)
      }

      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.ReseponseSourceID.expect(encodeSourceId(0, 1).U)
      dut.io.out(0).bits.ReseponseData.expect(1000.U)

      dut.io.out(3).valid.expect(true.B)
      dut.io.out(3).bits.ReseponseSourceID.expect(encodeSourceId(3, 2).U)
      dut.io.out(3).bits.ReseponseData.expect(1003.U)

      dut.io.out(7).valid.expect(true.B)
      dut.io.out(7).bits.ReseponseSourceID.expect(encodeSourceId(7, 3).U)
      dut.io.out(7).bits.ReseponseData.expect(1007.U)
    }
  }

  it should "restore grouped responses back to the correct bank outputs in 8-channel mode" in {
    runGroupedBridgeRoutingTest(respChannelCount = 8, banksToSend = Seq(0, 1, 2, 3, 4, 5, 6, 7))
  }

  it should "buffer two responses independently at every enabled Omega input" in {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = 8,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset
    )(ResponseChannelBridgeTestConfig.params(queueEnabled = true)))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)
      // Input 0 continuously wins the first switch, preventing input 1 from
      // reaching Omega while input 1's private queue absorbs two responses.
      driveResp(dut, channel = 0, bankId = 0, data = 10)
      driveResp(dut, channel = 1, bankId = 2, data = 20)
      dut.io.in(1).ready.expect(true.B)
      dut.clock.step()
      driveResp(dut, channel = 1, bankId = 2, data = 21, regAddr = 1)
      dut.io.in(1).ready.expect(true.B)
      dut.clock.step()
      driveResp(dut, channel = 1, bankId = 2, data = 22, regAddr = 2)
      dut.io.in(1).ready.expect(false.B)
      driveResp(dut, channel = 0, bankId = 0, data = 0, valid = false)
      dut.io.in(1).ready.expect(true.B)
    }
  }

  it should "count each Omega-blocked response once while accumulating its stall cycles" in {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = 8,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset
    )(ResponseChannelBridgeTestConfig.params(queueEnabled = false)))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)
      dut.io.out.foreach(_.ready.poke(false.B))

      // Inputs 0 and 1 target the same first-switch output. Input 1 remains
      // the same blocked response while input 0 wins or is downstream-stalled.
      driveResp(dut, channel = 0, bankId = 0, data = 10, regAddr = 0)
      driveResp(dut, channel = 1, bankId = 0, data = 20, regAddr = 1)
      dut.clock.step(3)
      dut.io.omegaBlockedCycles.expect(3.U)
      dut.io.omegaBlockedRespCycles.expect(3.U)
      dut.io.omegaBlockedResponses.expect(1.U)

      // Release prints a snapshot but does not clear any counter.
      dut.io.in.foreach(_.valid.poke(false.B))
      dut.io.releaseDebug.poke(true.B)
      dut.clock.step()
      dut.io.releaseDebug.poke(false.B)
      dut.io.omegaBlockedCycles.expect(3.U)
      dut.io.omegaBlockedRespCycles.expect(3.U)
      dut.io.omegaBlockedResponses.expect(1.U)
    }
  }

  it should "preserve exact FIFO delivery across full pipe and long stalls" in {
    test(new ResponseChannelBridge(
      inputChannelCount = bankCount,
      respChannelCount = 8,
      bankCount = bankCount,
      queueDepth = queueDepth,
      dataWidth = dataWidth,
      sourceIdWidth = sourceIdWidth,
      bankIdWidth = bankIdWidth,
      bankIdOffset = bankIdOffset
    )(ResponseChannelBridgeTestConfig.params(queueEnabled = true)))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      initBridge(dut)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.io.out.foreach(_.valid.expect(false.B))

      dut.io.out.foreach(_.ready.poke(false.B))
      for (cycle <- 0 until 2) {
        driveResp(dut, channel = 0, bankId = 0, data = 10 + cycle, regAddr = cycle)
        driveResp(dut, channel = 1, bankId = 0, data = 20 + cycle, regAddr = cycle + 2)
        dut.io.in(1).ready.expect(true.B)
        dut.clock.step()
      }
      driveResp(dut, channel = 0, bankId = 0, data = 0, valid = false)

      // The post-Omega bank queue and input 1's private queue are both full.
      driveResp(dut, channel = 1, bankId = 0, data = 22, regAddr = 4)
      dut.io.in(1).ready.expect(false.B)
      for (_ <- 0 until 10) {
        dut.io.in(1).ready.expect(false.B)
        dut.clock.step()
      }

      dut.io.out(0).ready.poke(true.B)
      // pipe=true permits replacing the departing full input-queue head.
      dut.io.in(1).ready.expect(true.B)

      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      var replacementAccepted = false
      var cycles = 0
      while ((received.size < 5 || !replacementAccepted) && cycles < 24) {
        val acceptedThisCycle = dut.io.in(1).valid.peek().litToBoolean &&
          dut.io.in(1).ready.peek().litToBoolean
        if (dut.io.out(0).valid.peek().litToBoolean) {
          received += dut.io.out(0).bits.ReseponseData.peek().litValue.toInt
        }
        dut.clock.step()
        replacementAccepted ||= acceptedThisCycle
        if (acceptedThisCycle) {
          dut.io.in(1).valid.poke(false.B)
        }
        cycles += 1
      }

      assert(replacementAccepted, "full input queue did not accept a same-cycle replacement")
      assert(received == Seq(10, 11, 20, 21, 22), s"unexpected delivery sequence: $received")
    }
  }
}
