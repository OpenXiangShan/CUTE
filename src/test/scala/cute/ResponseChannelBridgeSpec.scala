package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import xscache.coupledL2.{MatrixKey, AmeIndexKey, AmeIndexField}

// Drive the existing Cute2TL at its real 64B boundary, including a response on
// a different physical slice channel. No alternate transport implementation.
private class MinitTransportProbe(implicit p: Parameters) extends LazyModule {
  val dut = LazyModule(new Cute2TL)
  val banks = p(CuteParamsKey).ABMatrixRegNBanks
  val managers = Seq.fill(banks)(TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, 0xffffffffL)),
      supportsGet = TransferSizes(64), supportsPutFull = TransferSizes(64),
      fifoId = Some(0))),
    beatBytes = 64, requestKeys = Seq(MatrixKey, AmeIndexKey),
    responseFields = Seq(AmeIndexField())
  ))))
  (managers zip dut.node).foreach { case (manager, client) => manager := client }
  lazy val module = new MinitTransportProbeImp(this)
}

private class MinitTransportProbeImp(wrapper: MinitTransportProbe) extends LazyModuleImp(wrapper) {
    private val banks = wrapper.banks
    private val dut = wrapper.dut
    private val managers = wrapper.managers
    val io = IO(new Bundle {
      val mmu = new MMU2TLIO
      val matrixData = Flipped(Vec(banks, Decoupled(new MatrixDataIO)))
      val reqReady = Input(Vec(banks, Bool()))
      val ackValid = Input(Vec(banks, Bool()))
      val ackReady = Output(Vec(banks, Bool()))
      val idle = Output(Bool())
    })
    io.mmu <> dut.module.io.mmu
    io.matrixData <> dut.module.io.matrix_data_in
    io.idle := dut.module.io.idle
    for (i <- 0 until banks) {
      val (port, _) = managers(i).in(0)
      port.a.ready := io.reqReady(i)
      port.d.valid := io.ackValid(i)
      port.d.bits := 0.U.asTypeOf(port.d.bits)
      port.d.bits.opcode := TLMessages.AccessAck
      port.d.bits.size := 6.U
      port.d.bits.user.lift(AmeIndexKey).foreach(_ := (i + 16).U)
      io.ackReady(i) := port.d.ready
    }
}

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

  it should "MINIT count cross-channel nonallocated responses and same-cycle request response globally" in {
    val params = ResponseChannelBridgeTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_2Tops.copy(EnableDifftest = false)
    }
    val probe = DisableMonitors(p => LazyModule(new MinitTransportProbe()(p)))(params)
    test(probe.module) { d =>
      for (i <- 0 until probe.banks) {
        d.io.mmu.Request(i).valid.poke(false.B)
        d.io.mmu.Request(i).bits.RequestAddr.poke(0x80.U)
        d.io.mmu.Request(i).bits.RequestMask.poke(((BigInt(1) << 64) - 1).U)
        d.io.mmu.Request(i).bits.RequestType_isWrite.poke(false.B)
        d.io.mmu.Request(i).bits.UseAllocatedSourceID.poke(false.B)
        d.io.mmu.Request(i).bits.RequestSourceID.poke(i.U)
        d.io.mmu.Request(i).bits.RequestConherent.poke(true.B)
        d.io.mmu.Request(i).bits.RequestData.poke(0.U)
        d.io.mmu.Request(i).bits.MatrixIsAcc.poke(false.B)
        d.io.mmu.Request(i).bits.isA.poke(false.B)
        d.io.mmu.Response(i).ready.poke(true.B)
        d.io.matrixData(i).valid.poke(false.B)
        d.io.matrixData(i).bits.source.poke(0.U)
        d.io.matrixData(i).bits.data.poke(0.U)
        d.io.reqReady(i).poke(true.B); d.io.ackValid(i).poke(false.B)
      }
      d.clock.step(2); d.io.idle.expect(true.B)
      d.io.mmu.Request(0).valid.poke(true.B)
      d.io.idle.expect(false.B)
      d.clock.step(); d.io.mmu.Request(0).valid.poke(false.B)
      d.clock.step(2); d.io.idle.expect(false.B)
      // Req0 returns on response1, held for several cycles without decrement.
      d.io.mmu.Response(1).ready.poke(false.B)
      d.io.matrixData(1).valid.poke(true.B)
      d.clock.step(3); d.io.idle.expect(false.B)
      d.io.mmu.Response(1).ready.poke(true.B)
      // A new Put and the old Get response fire on this same edge (net zero).
      d.io.mmu.Request(2).valid.poke(true.B)
      d.io.mmu.Request(2).bits.RequestType_isWrite.poke(true.B)
      d.clock.step()
      d.io.mmu.Request(2).valid.poke(false.B); d.io.matrixData(1).valid.poke(false.B)
      d.io.idle.expect(false.B)
      d.io.ackValid(2).poke(true.B)
      d.clock.step(); d.io.ackValid(2).poke(false.B)
      d.io.idle.expect(true.B)
      // Two independent responses compete: only the selected one may retire.
      d.io.mmu.Request(0).valid.poke(true.B); d.io.mmu.Request(1).valid.poke(true.B)
      d.clock.step()
      d.io.mmu.Request(0).valid.poke(false.B); d.io.mmu.Request(1).valid.poke(false.B)
      d.io.matrixData(3).valid.poke(true.B); d.io.ackValid(3).poke(true.B)
      d.io.ackReady(3).expect(false.B)
      d.clock.step(); d.io.matrixData(3).valid.poke(false.B)
      d.io.idle.expect(false.B); d.io.ackReady(3).expect(true.B)
      d.clock.step(); d.io.ackValid(3).poke(false.B)
      d.io.idle.expect(true.B)
    }
  }

  it should "MINIT keep response bridge busy across bank queue and consumer stalls" in {
    test(new ResponseChannelBridge(
      inputChannelCount = 8, respChannelCount = 2, bankCount = 8,
      queueDepth = 2, dataWidth = 256, sourceIdWidth = 64,
      bankIdWidth = 3, bankIdOffset = 8
    )(ResponseChannelBridgeTestConfig.params)) { d =>
      initBridge(d)
      d.clock.step(2)
      d.io.idle.expect(true.B)
      d.io.out(3).ready.poke(false.B)
      driveResp(d, channel = 0, bankId = 3, data = 123)
      d.io.idle.expect(false.B)
      var waited = 0
      while (!d.io.in(0).ready.peek().litToBoolean && waited < 12) {
        d.clock.step(); waited += 1
      }
      d.io.in(0).ready.expect(true.B)
      d.clock.step(); d.io.in(0).valid.poke(false.B)
      d.clock.step(3)
      d.io.out(3).valid.expect(true.B)
      d.io.idle.expect(false.B)
      d.io.out(3).bits.ReseponseData.expect(123.U)
      d.io.out(3).ready.poke(true.B)
      d.clock.step()
      d.io.idle.expect(true.B)
    }
  }

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
