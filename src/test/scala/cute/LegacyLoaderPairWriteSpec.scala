package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class LegacyLoaderPairWriteSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils._

  behavior of "legacy loader pair writeback"

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

  private def expectABPair(io: ABMemoryLoaderMatrixRegIO, data: BigInt, rowBits: Int): Unit = {
    val rowMask = (BigInt(1) << rowBits) - 1
    for (lane <- 0 until WriteLanes) {
      val write = io.WriteRequest(0)(lane)
      write.BankAddr.valid.expect(true.B)
      write.BankAddr.bits.expect(lane.U)
      write.Data.valid.expect(true.B)
      write.Data.bits.expect(((data >> (lane * rowBits)) & rowMask).U)
      write.ByteMask.valid.expect(true.B)
    }
  }

  private def clearCMatrixPort(io: CMemoryLoaderMatrixRegIO, banks: Int): Unit = {
    io.LoadReadWriteResponse.poke(4.U)
    io.StoreReadWriteResponse.poke(0.U)
    for (bank <- 0 until banks) {
      io.ReadRequestToMatrixReg.ReadResponseData(bank).valid.poke(false.B)
      io.ReadRequestToMatrixReg.ReadResponseData(bank).bits.poke(0.U)
    }
  }

  private def expectCPair(io: CMemoryLoaderMatrixRegIO, data: BigInt, rowBits: Int): Unit = {
    val rowMask = (BigInt(1) << rowBits) - 1
    for (lane <- 0 until WriteLanes) {
      val write = io.WriteRequestToMatrixReg.WriteRequest(0)(lane)
      write.BankAddr.valid.expect(true.B)
      write.BankAddr.bits.expect(lane.U)
      write.Data.valid.expect(true.B)
      write.Data.bits.expect(((data >> (lane * rowBits)) & rowMask).U)
      write.ByteMask.valid.expect(true.B)
    }
  }

  it should "drain one A response into two consecutive rows in one cycle" in {
    implicit val p: Parameters = parameters
    test(new AMemoryLoader).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearLocalMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
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

      dut.io.LocalMMUIO.Request(0).valid.expect(true.B)
      val sourceId = dut.io.LocalMMUIO.Request(0).bits.RequestSourceID.peek().litValue
      val data = BigInt("a5" * dut.outsideDataWidthByte, 16)
      dut.io.LocalMMUIO.Response(0).valid.poke(true.B)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseSourceID.poke(sourceId.U)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseData.poke(data.U)
      dut.io.LocalMMUIO.Response(0).ready.expect(true.B)
      dut.clock.step()
      dut.io.LocalMMUIO.Response(0).valid.poke(false.B)

      expectABPair(dut.io.ToMatrixRegIO, data, dut.ABMatrixRegEntryBitSize)
      dut.clock.step()
      dut.io.ToMatrixRegIO.WriteRequest(0).foreach(_.BankAddr.valid.expect(false.B))
    }
  }

  it should "drain one B response into two consecutive rows in one cycle" in {
    implicit val p: Parameters = parameters
    test(new BMemoryLoader).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearLocalMmu(dut.io.LocalMMUIO, dut.ABMatrixRegNBanks)
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N.poke(64.U)
      dut.io.ConfigInfo.ApplicationTensor_B.dataType.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_B.HasTail.poke(false.B)
      dut.io.ConfigInfo.ApplicationTensor_B.TailByteMask.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_B.K_Beat_Count.poke(1.U)
      dut.io.ConfigInfo.MatrixRegTensor_N.poke(1.U)
      dut.io.ConfigInfo.MatrixRegTensor_K.poke(1.U)
      dut.io.ConfigInfo.MatrixRegId.poke(0.U)
      dut.io.ConfigInfo.Conherent.poke(false.B)
      dut.io.ConfigInfo.Is_Transpose.poke(false.B)
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.MicroTaskEndReady.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
      dut.clock.step()

      dut.io.LocalMMUIO.Request(0).valid.expect(true.B)
      val sourceId = dut.io.LocalMMUIO.Request(0).bits.RequestSourceID.peek().litValue
      val data = BigInt("5a" * dut.outsideDataWidthByte, 16)
      dut.io.LocalMMUIO.Response(0).valid.poke(true.B)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseSourceID.poke(sourceId.U)
      dut.io.LocalMMUIO.Response(0).bits.ReseponseData.poke(data.U)
      dut.io.LocalMMUIO.Response(0).ready.expect(true.B)
      dut.clock.step()
      dut.io.LocalMMUIO.Response(0).valid.poke(false.B)

      expectABPair(dut.io.ToMatrixRegIO, data, dut.ABMatrixRegEntryBitSize)
      dut.clock.step()
      dut.io.ToMatrixRegIO.WriteRequest(0).foreach(_.BankAddr.valid.expect(false.B))
    }
  }

  it should "drain one C response into two consecutive rows in one granted cycle" in {
    implicit val p: Parameters = parameters
    test(new CMemoryLoader).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearLocalMmu(dut.io.LoadLocalMMUIO, dut.ABMatrixRegNBanks)
      clearLocalMmu(dut.io.StoreLocalMMUIO, dut.ABMatrixRegNBanks)
      clearCMatrixPort(dut.io.ToMatrixRegIO, dut.CMatrixRegNBanks)
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
      val sourceId = dut.io.LoadLocalMMUIO.Request(0).bits.RequestSourceID.peek().litValue
      val data = BigInt("c3" * dut.outsideDataWidthByte, 16)
      dut.io.LoadLocalMMUIO.Response(0).valid.poke(true.B)
      dut.io.LoadLocalMMUIO.Response(0).bits.ReseponseSourceID.poke(sourceId.U)
      dut.io.LoadLocalMMUIO.Response(0).bits.ReseponseData.poke(data.U)
      dut.io.LoadLocalMMUIO.Response(0).ready.expect(true.B)
      dut.clock.step()
      dut.io.LoadLocalMMUIO.Response(0).valid.poke(false.B)

      expectCPair(dut.io.ToMatrixRegIO, data, dut.CMatrixRegEntryBitSize)
      dut.clock.step()
      dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(0).foreach(
        _.BankAddr.valid.expect(false.B)
      )
    }
  }
}
