package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Config
import org.chipsalliance.cde.config.Parameters

object LoaderBankedWriteTestUtils {
  val WriteLanes = 2

  def parameters: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      Debug = CuteDebugParams.NoDebug,
      EnableDifftest = false
    )
  })

  def clearABLoader(dut: ABMatrixReg)(implicit p: Parameters): Unit = {
    dut.io.MatrixRegIO.FromMemoryLoader.active.poke(false.B)
    for {
      bank <- 0 until dut.ABMatrixRegNBanks
      lane <- 0 until WriteLanes
    } {
      val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequest(bank)(lane)
      write.BankAddr.valid.poke(false.B)
      write.BankAddr.bits.poke(0.U)
      write.Data.valid.poke(false.B)
      write.Data.bits.poke(0.U)
      write.ByteMask.valid.poke(false.B)
      write.ByteMask.bits.poke(0.U)
    }
  }

  def clearABRead(dut: ABMatrixReg)(implicit p: Parameters): Unit = {
    dut.io.MatrixRegIO.FromDataController.BankAddr.valid.poke(false.B)
    dut.io.MatrixRegIO.FromDataController.BankAddr.bits.foreach(_.poke(0.U))
  }

  def clearCInputs(dut: CMatrixReg)(implicit p: Parameters): Unit = {
    val io = dut.io.MatrixRegIO
    io.FromDataController.ReadWriteRequest.poke(0.U)
    io.FromMemoryLoader.LoadReadWriteRequest.poke(0.U)
    io.FromMemoryLoader.StoreReadWriteRequest.poke(0.U)
    for (bank <- 0 until dut.CMatrixRegNBanks) {
      io.FromDataController.ReadBankAddr(bank).valid.poke(false.B)
      io.FromDataController.ReadBankAddr(bank).bits.poke(0.U)
      io.FromDataController.WriteBankAddr(bank).valid.poke(false.B)
      io.FromDataController.WriteBankAddr(bank).bits.poke(0.U)
      io.FromDataController.WriteRequestData(bank).valid.poke(false.B)
      io.FromDataController.WriteRequestData(bank).bits.poke(0.U)
      io.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(bank).valid.poke(false.B)
      io.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(bank).bits.poke(0.U)
      for (lane <- 0 until WriteLanes) {
        val write = io.FromMemoryLoader.WriteRequestToMatrixReg.WriteRequest(bank)(lane)
        write.BankAddr.valid.poke(false.B)
        write.BankAddr.bits.poke(0.U)
        write.Data.valid.poke(false.B)
        write.Data.bits.poke(0.U)
        write.ByteMask.valid.poke(false.B)
        write.ByteMask.bits.poke(0.U)
      }
    }
  }
}
