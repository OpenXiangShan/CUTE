package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

object CMemoryLoaderStoreAddressTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_512SCP
  })
}

class CMemoryLoaderStoreAddressHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val transpose = Input(Bool())
    val taskReady = Output(Bool())
    val bankAddr = Output(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W))))
  })

  val loader = Module(new CMemoryLoader)
  loader.io.DebugInfo.DebugTimeStampe := 0.U

  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.valid := true.B
  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.valid := true.B
  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  for (channel <- 0 until ABMatrixRegNBanks) {
    loader.io.LoadLocalMMUIO.Request(channel).ready := true.B
    loader.io.LoadLocalMMUIO.Response(channel).valid := false.B
    loader.io.LoadLocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    loader.io.StoreLocalMMUIO.Request(channel).ready := true.B
    loader.io.StoreLocalMMUIO.Response(channel).valid := false.B
    loader.io.StoreLocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
  }

  loader.io.ToMatrixRegIO.LoadReadWriteResponse := loader.io.ToMatrixRegIO.LoadReadWriteRequest
  loader.io.ToMatrixRegIO.StoreReadWriteResponse := loader.io.ToMatrixRegIO.StoreReadWriteRequest
  for (bank <- 0 until CMatrixRegNBanks) {
    loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid := false.B
    loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits := 0.U
  }
  io.bankAddr := loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr

  val config = loader.io.ConfigInfo
  config.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := 0.U
  config.ApplicationTensor_C.BlockTensor_C_BaseVaddr := 0.U
  config.ApplicationTensor_C.ApplicationTensor_C_Stride_M := 0.U
  config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
  config.ApplicationTensor_C.HasTail := false.B
  config.ApplicationTensor_C.TailByteMask := 0.U
  config.ApplicationTensor_C.N_Beat_Count := 0.U
  config.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := 0x1000.U
  config.ApplicationTensor_D.BlockTensor_D_BaseVaddr := 0x1000.U
  config.ApplicationTensor_D.ApplicationTensor_D_Stride_M := 256.U
  config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32
  config.LoadTaskInfo.Is_ZeroLoad := false.B
  config.LoadTaskInfo.Is_RepeatRowLoad := false.B
  config.LoadTaskInfo.Is_FullLoad := true.B
  config.Conherent := true.B
  config.Is_Transpose := io.transpose
  config.MatrixRegTensor_M := 64.U
  config.MatrixRegTensor_N := 64.U
  config.MatrixRegId := 0.U
  config.LoadMicroTaskValid := false.B
  config.LoadMicroTaskEndReady := true.B
  config.StoreMicroTaskValid := io.start
  config.StoreMicroTaskEndReady := true.B
  if (EnableDifftest) {
    config.pc.get := 0.U
    config.coreid.get := 0.U
  }

  io.taskReady := config.StoreMicroTaskReady
}

class CMemoryLoaderStoreAddressSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "the legacy C memory loader store address generator"

  private def checkAddresses(transpose: Boolean, expected: Seq[Int]): Unit = {
    test(new CMemoryLoaderStoreAddressHarness()(CMemoryLoaderStoreAddressTestConfig.params)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.transpose.poke(transpose.B)

      assert(dut.io.taskReady.peek().litToBoolean, "CML store did not become ready")
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      val observed = collection.mutable.ArrayBuffer.empty[Int]
      var cycles = 0
      while (observed.size < expected.size && cycles < 64) {
        val valid = dut.io.bankAddr.head.valid.peek().litToBoolean
        for (bank <- dut.io.bankAddr.indices) {
          assert(dut.io.bankAddr(bank).valid.peek().litToBoolean == valid,
            s"bank $bank read-valid diverged")
        }
        if (valid) {
          val address = dut.io.bankAddr.head.bits.peek().litValue.toInt
          for (bank <- dut.io.bankAddr.indices) {
            assert(dut.io.bankAddr(bank).bits.peek().litValue.toInt == address,
              s"bank $bank read address diverged")
          }
          observed += address
        }
        dut.clock.step()
        cycles += 1
      }

      assert(observed.toSeq == expected,
        s"store MatrixReg address sequence differed: expected=$expected observed=$observed")
    }
  }

  it should "skip unused physical columns between logical row groups" in {
    checkAddresses(transpose = false, Seq(0, 1, 2, 3, 4, 5, 6, 7, 64))
  }

  it should "use the physical row-group stride while traversing transposed blocks" in {
    checkAddresses(transpose = true, Seq(0, 64, 128, 192, 256, 320, 384, 448, 1))
  }
}
