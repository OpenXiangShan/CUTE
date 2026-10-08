package cute

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class MultiChannelsCMLPerfectBatchSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MultiChannelsCMemLoader perfect batches"

  private implicit val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      LoaderBridgeChannelConfig = "A8B8CL8CS8",
      L2NBanks = 8,
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false)
  })

  private def clearMmu(io: LocalMMUIO, banks: Int): Unit = {
    io.ConherentRequsetSourceID.valid.poke(false.B)
    io.ConherentRequsetSourceID.bits.poke(0.U)
    io.nonConherentRequsetSourceID.valid.poke(false.B)
    io.nonConherentRequsetSourceID.bits.poke(0.U)
    for (bank <- 0 until banks) {
      io.Request(bank).ready.poke(true.B)
      io.Response(bank).valid.poke(false.B)
      io.Response(bank).bits.ReseponseData.poke(0.U)
      io.Response(bank).bits.ReseponseConherent.poke(false.B)
      io.Response(bank).bits.ReseponseSourceID.poke(0.U)
    }
  }

  private def configure(dut: MultiChannelsCMemLoader): Unit = {
    dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr.poke(CMLPerfectBatchTestUtils.baseAddress.U)
    dut.io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr.poke(CMLPerfectBatchTestUtils.baseAddress.U)
    dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M.poke(CMLPerfectBatchTestUtils.rowStride.U)
    dut.io.ConfigInfo.ApplicationTensor_C.dataType.poke(0.U)
    dut.io.ConfigInfo.ApplicationTensor_C.HasTail.poke(false.B)
    dut.io.ConfigInfo.ApplicationTensor_C.TailByteMask.poke(0.U)
    dut.io.ConfigInfo.ApplicationTensor_C.N_Beat_Count.poke(CMLPerfectBatchTestUtils.nBeatCount.U)
    dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr.poke(0.U)
    dut.io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr.poke(0.U)
    dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M.poke(0.U)
    dut.io.ConfigInfo.ApplicationTensor_D.dataType.poke(0.U)
    dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
    dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
    dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
    dut.io.ConfigInfo.Conherent.poke(true.B)
    dut.io.ConfigInfo.Is_Transpose.poke(false.B)
    dut.io.ConfigInfo.MatrixRegTensor_M.poke(CMLPerfectBatchTestUtils.matrixM.U)
    dut.io.ConfigInfo.MatrixRegTensor_N.poke(CMLPerfectBatchTestUtils.matrixN.U)
    dut.io.ConfigInfo.MatrixRegId.poke(0.U)
    dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)
    dut.io.ConfigInfo.LoadMicroTaskEndReady.poke(true.B)
    dut.io.ConfigInfo.StoreMicroTaskValid.poke(false.B)
    dut.io.ConfigInfo.StoreMicroTaskEndReady.poke(true.B)
    dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
    dut.io.ToMatrixRegIO.LoadReadWriteResponse.poke((1 << MatrixRegTaskType.WriteFromMemoryLoaderIndex).U)
    dut.io.ToMatrixRegIO.StoreReadWriteResponse.poke(0.U)
    for (bank <- 0 until dut.CMatrixRegNBanks) {
      dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid.poke(false.B)
      dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits.poke(0.U)
    }
    clearMmu(dut.io.LoadLocalMMUIO, dut.CMatrixRegNBanks)
    clearMmu(dut.io.StoreLocalMMUIO, dut.CMatrixRegNBanks)
  }

  it should "complete the recorded ideal C task with exact coverage and route checks" in {
    test(new MultiChannelsCMemLoader).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      configure(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)

      val responseQueues = Array.fill(dut.CMatrixRegNBanks)(mutable.Queue.empty[BigInt])
      val observed = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
      val perChannel = Array.fill(dut.CMatrixRegNBanks)(mutable.ArrayBuffer.empty[BigInt])
      var perfectBatches = 0
      var perfectRequests = 0
      var rtlPerfectBatchPulses = 0
      var cycles = 0
      var completed = false
      val sourceAddrWidth = log2Ceil(dut.CMatrixRegBankNEntries)
      dut.clock.setTimeout(10000)

      while (!completed && cycles < 3000) {
        for (bank <- 0 until dut.CMatrixRegNBanks) {
          val response = dut.io.LoadLocalMMUIO.Response(bank)
          response.valid.poke(responseQueues(bank).nonEmpty.B)
          response.bits.ReseponseSourceID.poke(responseQueues(bank).headOption.getOrElse(BigInt(0)).U)
          response.bits.ReseponseData.poke(0.U)
          response.bits.ReseponseConherent.poke(true.B)
        }

        val fired = mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        for (channel <- 0 until dut.CMatrixRegNBanks) {
          val request = dut.io.LoadLocalMMUIO.Request(channel)
          if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
            val address = request.bits.RequestAddr.peek().litValue
            val source = request.bits.RequestSourceID.peek().litValue
            fired += ((channel, address, source))
            observed += ((channel, address, source))
            perChannel(channel) += source & ((BigInt(1) << sourceAddrWidth) - 1)
            responseQueues(channel).enqueue(source)
          }
        }

        for (bank <- 0 until dut.CMatrixRegNBanks) {
          if (dut.io.LoadLocalMMUIO.Response(bank).valid.peek().litToBoolean &&
              dut.io.LoadLocalMMUIO.Response(bank).ready.peek().litToBoolean) {
            responseQueues(bank).dequeue()
          }
        }

        if (fired.nonEmpty) {
          val validByBank = Array.fill(dut.CMatrixRegNBanks)(false)
          val destinationByBank = Array.fill(dut.CMatrixRegNBanks)(0)
          fired.foreach { case (_, address, source) =>
            val l2Bank = CMLPerfectBatchTestUtils.l2Bank(address, 8)
            val regBank = ((source >> sourceAddrWidth) & 7).toInt
            assert(!validByBank(l2Bank), s"duplicate L2 bank in cycle $cycles")
            validByBank(l2Bank) = true
            destinationByBank(l2Bank) = regBank
          }
          val survivors = CMLPerfectBatchTestUtils.exactSurvivors(
            validByBank.toSeq,
            destinationByBank.toSeq,
            responseChannels = 8,
            bankCount = 8)
          val firedMask = validByBank.zipWithIndex.collect { case (true, bank) => BigInt(1) << bank }.foldLeft(BigInt(0))(_ | _)
          assert((survivors & firedMask) == firedMask, s"Omega survivor mismatch at cycle $cycles")
          if (fired.size == dut.CMatrixRegNBanks) {
            perfectBatches += 1
            perfectRequests += fired.size
          }
        }

        if (dut.io.perfProbe.loadPerfectBatch.peek().litToBoolean) {
          rtlPerfectBatchPulses += 1
        }

        completed = dut.io.ConfigInfo.LoadMicroTaskEndValid.peek().litToBoolean &&
          dut.io.ConfigInfo.LoadMicroTaskEndReady.peek().litToBoolean
        dut.clock.step()
        cycles += 1
      }

      assert(completed, s"CML task did not complete after $cycles cycles (observed=${observed.size}, pendingResponses=${responseQueues.map(_.size).mkString(",")})")
      assert(observed.size == CMLPerfectBatchTestUtils.expectedRequestCount,
        s"expected ${CMLPerfectBatchTestUtils.expectedRequestCount} requests, observed ${observed.size}")
      val expected = CMLPerfectBatchTestUtils.expectedRequests
      val actual = observed.map { case (channel, address, source) =>
        val regAddr = (source & ((BigInt(1) << sourceAddrWidth) - 1)).toInt
        (channel, address, regAddr)
      }.toSet
      assert(actual == expected, s"request coverage mismatch: missing=${expected -- actual} extra=${actual -- expected}")
      // C response queues are independently reorderable.  Exactly-once source
      // coverage below is the contract; issue order within a register bank is
      // intentionally unconstrained by the global batch constructor.
      assert(perfectRequests == perfectBatches * dut.CMatrixRegNBanks)
      assert(perfectBatches == CMLPerfectBatchTestUtils.expectedRequestCount / dut.CMatrixRegNBanks,
        s"ideal LUT should form a perfect batch every cycle: $perfectBatches")
      assert(rtlPerfectBatchPulses == perfectBatches,
        s"CML perfect pulse mismatch: rtl=$rtlPerfectBatchPulses scoreboard=$perfectBatches")
      assert(dut.io.perfProbe.loadTotalFired.peek().litValue == observed.size,
        s"CML total-fired counter mismatch: rtl=${dut.io.perfProbe.loadTotalFired.peek().litValue} observed=${observed.size}")
      assert(dut.io.perfProbe.loadPerfectBatches.peek().litValue == perfectBatches,
        s"CML perfect-batch counter mismatch: rtl=${dut.io.perfProbe.loadPerfectBatches.peek().litValue} scoreboard=$perfectBatches")
      assert(dut.io.perfProbe.loadPerfectRequests.peek().litValue == perfectRequests,
        s"CML perfect-request counter mismatch: rtl=${dut.io.perfProbe.loadPerfectRequests.peek().litValue} scoreboard=$perfectRequests")
    }
  }
}
