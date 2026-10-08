package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MultiChannelLoaderBankedWriteSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils._

  behavior of "MultiChannel loader pair writeback"

  private def clearLocalMmu(io: LocalMMUIO, banks: Int): Unit = {
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

  it should "drain an AB channel response into both write lanes" in {
    implicit val p: Parameters = parameters
    test(new MultiChannelsABMemLoader()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.dataType.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.HasTail.poke(false.B)
      dut.io.ConfigInfo.ApplicationTensor_A.TailByteMask.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.K_Beat_Count.poke(0.U)
      dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(false.B)
      dut.io.ConfigInfo.MatrixRegTensor_M.poke(0.U)
      dut.io.ConfigInfo.MatrixRegTensor_K.poke(0.U)
      dut.io.ConfigInfo.MatrixRegId.poke(0.U)
      dut.io.ConfigInfo.Conherent.poke(false.B)
      dut.io.ConfigInfo.Is_Transpose.poke(false.B)
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.MicroTaskEndReady.poke(true.B)
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
      dut.io.LocalMMUIO.ConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LocalMMUIO.ConherentRequsetSourceID.bits.poke(0.U)
      dut.io.LocalMMUIO.nonConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LocalMMUIO.nonConherentRequsetSourceID.bits.poke(0.U)
      for (bank <- 0 until dut.ABMatrixRegNBanks) {
        dut.io.LocalMMUIO.Request(bank).ready.poke(true.B)
        dut.io.LocalMMUIO.Response(bank).valid.poke(false.B)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseData.poke(0.U)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseConherent.poke(false.B)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseSourceID.poke(0.U)
      }
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
      dut.io.ConfigInfo.MatrixRegTensor_M.poke(1.U)
      dut.io.ConfigInfo.MatrixRegTensor_K.poke(1.U)
      dut.io.ConfigInfo.ApplicationTensor_A.K_Beat_Count.poke(1.U)
      dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(64.U)
      dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
      dut.clock.step()
      dut.clock.step()

      dut.io.LocalMMUIO.Request(0).valid.expect(true.B)
      val sourceId = dut.io.LocalMMUIO.Request(0).bits.RequestSourceID.peek().litValue
      val data = BigInt("3c" * dut.outsideDataWidthByte, 16)
      dut.io.LocalMMUIO.Response(0).valid.poke(true.B)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseSourceID.poke(sourceId.U)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseData.poke(data.U)
      // The empty response queues forward both rows in the acceptance cycle.
      dut.io.LocalMMUIO.Response(0).ready.expect(true.B)

      val rowMask = (BigInt(1) << dut.ABMatrixRegEntryBitSize) - 1
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.ToMatrixRegIO.WriteRequest(0)(lane)
        write.BankAddr.valid.expect(true.B)
        write.BankAddr.bits.expect(lane.U)
        write.Data.bits.expect(((data >> (lane * dut.ABMatrixRegEntryBitSize)) & rowMask).U)
      }
      dut.clock.step()
      dut.io.LocalMMUIO.Response(0).valid.poke(false.B)
      dut.io.ToMatrixRegIO.WriteRequest(0).foreach(_.BankAddr.valid.expect(false.B))
    }
  }

  it should "complete a minimal transpose task without normal scheduler state" in {
    implicit val p: Parameters = parameters.alterPartial {
      case CuteParamsKey => parameters(CuteParamsKey).copy(LoaderBridgeChannelConfig = "A8B8CL8CS8")
    }
    test(new AMLWrapper("AML_transpose_minimal"))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        clearLocalMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
        dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr.poke(0x1000.U)
        dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(64.U)
        dut.io.ConfigInfo.ApplicationTensor_A.dataType.poke(ElementDataType.DataTypeWidth8)
        dut.io.ConfigInfo.ApplicationTensor_A.HasTail.poke(true.B)
        dut.io.ConfigInfo.ApplicationTensor_A.TailByteMask.poke(8.U)
        dut.io.ConfigInfo.ApplicationTensor_A.K_Beat_Count.poke(1.U)
        dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
        dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
        dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
        dut.io.ConfigInfo.MatrixRegTensor_M.poke(1.U)
        dut.io.ConfigInfo.MatrixRegTensor_K.poke(1.U)
        dut.io.ConfigInfo.MatrixRegId.poke(0.U)
        dut.io.ConfigInfo.Conherent.poke(false.B)
        dut.io.ConfigInfo.Is_Transpose.poke(true.B)
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
        dut.io.ConfigInfo.MicroTaskEndReady.poke(true.B)
        dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)

        dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
        dut.clock.step()
        dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
        var cycles = 0
        var sourceId = BigInt(0)
        var requested = false
        var responded = false
        var completed = false
        var writeCount = 0
        val writeAddresses = collection.mutable.Set.empty[BigInt]
        while (!completed && cycles < 80) {
          val request = dut.io.LocalMMUIO.Request(0)
          if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
            assert(!requested, "minimal transpose emitted more than one request")
            request.bits.RequestAddr.expect(0x1000.U)
            sourceId = request.bits.RequestSourceID.peek().litValue
            requested = true
          }
          val sendResponse = requested && !responded
          dut.io.LocalMMUIO.Response(0).valid.poke(sendResponse.B)
          dut.io.LocalMMUIO.Response(0).bits.ReseponseSourceID.poke(sourceId.U)
          dut.io.LocalMMUIO.Response(0).bits.ReseponseData.poke(BigInt("a5" * dut.outsideDataWidthByte, 16).U)
          dut.io.LocalMMUIO.Response(0).bits.ReseponseConherent.poke(false.B)
          if (sendResponse && dut.io.LocalMMUIO.Response(0).ready.peek().litToBoolean) {
            responded = true
          }
          for {
            bank <- 0 until dut.ABMatrixRegNBanks
            lane <- 0 until WriteLanes
          } {
            val write = dut.io.ToMatrixRegIO.WriteRequest(bank)(lane)
            write.Data.valid.expect(write.BankAddr.valid.peek())
            write.ByteMask.valid.expect(write.BankAddr.valid.peek())
            if (write.BankAddr.valid.peek().litToBoolean) {
              val address = write.BankAddr.bits.peek().litValue
              assert(address < dut.Trans_Load_Size * dut.ReduceGroupSize,
                s"transpose write bank=$bank lane=$lane had out-of-range address $address")
              assert(address % dut.ReduceGroupSize == 0,
                s"transpose write bank=$bank lane=$lane had misaligned address $address")
              assert(write.ByteMask.bits.peek().litValue != 0,
                s"transpose write bank=$bank lane=$lane had an empty mask")
              writeAddresses += address
              writeCount += 1
            }
          }
          completed = dut.io.ConfigInfo.MicroTaskEndValid.peek().litToBoolean
          dut.clock.step()
          cycles += 1
        }
        assert(requested && responded, "minimal transpose did not complete request/response")
        assert(writeCount > 0, "minimal transpose produced no MatrixReg writes")
        assert(writeAddresses == Set(BigInt(0)),
          s"minimal transpose write addresses were ${writeAddresses.toSeq.sorted.mkString(",")}")
        assert(completed, s"minimal transpose did not quiesce after $cycles cycles")
        dut.io.perfProbe.loadTotalFired.expect(0.U)
        dut.io.perfProbe.loadPerfectBatches.expect(0.U)
        dut.io.perfProbe.seedStatus.expect(ABPerfectBatchSeed.Disabled.U)
        dut.io.perfProbe.seedVector.foreach(_.expect(0.U))
        dut.io.perfProbe.seedAttempts.expect(0.U)
        dut.io.perfProbe.seedSuccesses.expect(0.U)
        dut.io.perfProbe.seedFallbacks.expect(0.U)
        dut.io.perfProbe.windowRepairs.expect(0.U)
        dut.clock.step(4)
        for {
          bank <- 0 until dut.ABMatrixRegNBanks
          lane <- 0 until WriteLanes
        } {
          dut.io.LocalMMUIO.Request(bank).valid.expect(false.B)
          dut.io.LocalMMUIO.Response(bank).ready.expect(false.B)
          dut.io.ToMatrixRegIO.WriteRequest(bank)(lane).BankAddr.valid.expect(false.B)
        }
      }
  }

  it should "hold and atomically drain an odd-base C response on its channel" in {
    implicit val p: Parameters = parameters
    test(new MultiChannelsCMemLoader()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearLocalMmu(dut.io.LoadLocalMMUIO, dut.ABMatrixRegNBanks)
      clearLocalMmu(dut.io.StoreLocalMMUIO, dut.ABMatrixRegNBanks)
      dut.io.ToMatrixRegIO.LoadReadWriteResponse.poke(0.U)
      dut.io.ToMatrixRegIO.StoreReadWriteResponse.poke(0.U)
      for (bank <- 0 until dut.CMatrixRegNBanks) {
        dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid.poke(false.B)
        dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits.poke(0.U)
      }
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M.poke(64.U)
      dut.io.ConfigInfo.ApplicationTensor_C.dataType.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.HasTail.poke(false.B)
      dut.io.ConfigInfo.ApplicationTensor_C.TailByteMask.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.N_Beat_Count.poke(1.U)
      dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.dataType.poke(0.U)
      dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
      dut.io.ConfigInfo.Conherent.poke(false.B)
      dut.io.ConfigInfo.Is_Transpose.poke(false.B)
      dut.io.ConfigInfo.MatrixRegTensor_M.poke(1.U)
      dut.io.ConfigInfo.MatrixRegTensor_N.poke(1.U)
      dut.io.ConfigInfo.MatrixRegId.poke(0.U)
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.LoadMicroTaskEndReady.poke(true.B)
      dut.io.ConfigInfo.StoreMicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.StoreMicroTaskEndReady.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.ConfigInfo.LoadMicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)
      dut.clock.step()

      dut.io.LoadLocalMMUIO.Request(0).valid.expect(true.B)
      val requestedSourceId = dut.io.LoadLocalMMUIO.Request(0).bits.RequestSourceID.peek().litValue
      val oddBaseSourceId = requestedSourceId | 1
      val data = BigInt("96" * dut.outsideDataWidthByte, 16)
      dut.io.LoadLocalMMUIO.Response(0).valid.poke(true.B)
      dut.io.LoadLocalMMUIO.Response(0).bits.ReseponseSourceID.poke(oddBaseSourceId.U)
      dut.io.LoadLocalMMUIO.Response(0).bits.ReseponseData.poke(data.U)
      dut.io.LoadLocalMMUIO.Response(0).ready.expect(true.B)
      dut.clock.step()
      dut.io.LoadLocalMMUIO.Response(0).valid.poke(false.B)

      for (_ <- 0 until 10) {
        dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest.foreach(
          _.foreach(_.BankAddr.valid.expect(false.B))
        )
        dut.clock.step()
      }

      dut.io.ToMatrixRegIO.LoadReadWriteResponse.poke(4.U)
      val rowMask = (BigInt(1) << dut.CMatrixRegEntryBitSize) - 1
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(0)(lane)
        write.BankAddr.valid.expect(true.B)
        write.BankAddr.bits.expect((lane + 1).U)
        write.Data.valid.expect(true.B)
        write.Data.bits.expect(
          ((data >> (lane * dut.CMatrixRegEntryBitSize)) & rowMask).U
        )
      }
      for (bank <- 1 until dut.CMatrixRegNBanks) {
        dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(bank).foreach(
          _.BankAddr.valid.expect(false.B)
        )
      }
      dut.clock.step()
      dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(0).foreach(
        _.BankAddr.valid.expect(false.B)
      )
    }
  }
}
