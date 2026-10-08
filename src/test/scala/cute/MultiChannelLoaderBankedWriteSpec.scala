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
      dut.clock.step()
      dut.io.LocalMMUIO.Response(0).valid.poke(false.B)

      val rowMask = (BigInt(1) << dut.ABMatrixRegEntryBitSize) - 1
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.ToMatrixRegIO.WriteRequest(0)(lane)
        write.BankAddr.valid.expect(true.B)
        write.BankAddr.bits.expect(lane.U)
        write.Data.bits.expect(((data >> (lane * dut.ABMatrixRegEntryBitSize)) & rowMask).U)
      }
      dut.clock.step()
      dut.io.ToMatrixRegIO.WriteRequest(0).foreach(_.BankAddr.valid.expect(false.B))
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
