package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.{Config, Parameters}

object ResponseChannelBridgeTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.baseParams.copy(
      outsideDataWidth = 256,
      ReduceWidthByte = 32,
      Matrix_MN = 8
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
      hasDataPayload = true,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
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
      hasDataPayload = true,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
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
      hasDataPayload = true,
      debugEnable = false
    )(ResponseChannelBridgeTestConfig.params)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
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
}
