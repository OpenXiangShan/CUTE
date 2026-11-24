package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class AccMatrixRegLoaderWriteSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils._

  behavior of "AccMatrixReg two-lane loader writes"

  private def driveLoaderPair(
      dut: AccMatrixReg,
      baseAddr: Int,
      data0: BigInt,
      data1: BigInt
  )(implicit p: Parameters): Unit = {
    clearCInputs(dut)
    val io = dut.io.MatrixRegIO
    io.FromMemoryLoader.LoadReadWriteRequest.poke(4.U)
    val fullMask = (BigInt(1) << dut.AccMatrixRegEntryByteSize) - 1
    Seq((0, baseAddr, data0), (1, baseAddr + 1, data1)).foreach { case (lane, addr, data) =>
      val write = io.FromMemoryLoader.WriteRequestToMatrixReg.WriteRequest(0)(lane)
      write.BankAddr.valid.poke(true.B)
      write.BankAddr.bits.poke(addr.U)
      write.Data.valid.poke(true.B)
      write.Data.bits.poke(data.U)
      write.ByteMask.valid.poke(true.B)
      write.ByteMask.bits.poke(fullMask.U)
    }
    io.FromMemoryLoader.LoadReadWriteResponse.expect(4.U)
    dut.clock.step()
    clearCInputs(dut)
  }

  private def readRow(dut: AccMatrixReg, addr: Int)(implicit p: Parameters): BigInt = {
    val io = dut.io.MatrixRegIO
    io.FromDataController.ReadWriteRequest.poke(1.U)
    io.FromDataController.ReadBankAddr(0).valid.poke(true.B)
    io.FromDataController.ReadBankAddr(0).bits.poke(addr.U)
    dut.clock.step()
    io.FromDataController.ReadWriteRequest.poke(0.U)
    io.FromDataController.ReadBankAddr(0).valid.poke(false.B)
    io.FromDataController.ReadResponseData(0).valid.expect(true.B)
    io.FromDataController.ReadResponseData(0).bits.peek().litValue
  }

  it should "cover atomic pairs, odd bases, reset, and ownership stalls" in {
    implicit val p: Parameters = parameters
    test(new AccMatrixReg(0)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearCInputs(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.clock.step()
      dut.io.MatrixRegIO.FromDataController.ReadResponseData.foreach(_.valid.expect(false.B))

      val data2 = BigInt("12" * dut.AccMatrixRegEntryByteSize, 16)
      val data3 = BigInt("23" * dut.AccMatrixRegEntryByteSize, 16)
      val data4 = BigInt("34" * dut.AccMatrixRegEntryByteSize, 16)
      driveLoaderPair(dut, 2, data2, data3)
      assert(readRow(dut, 2) == data2)
      assert(readRow(dut, 3) == data3)

      driveLoaderPair(dut, 3, data3, data4)
      assert(readRow(dut, 3) == data3)
      assert(readRow(dut, 4) == data4)

      val original = BigInt("55" * dut.AccMatrixRegEntryByteSize, 16)
      val blocked = BigInt("aa" * dut.AccMatrixRegEntryByteSize, 16)
      driveLoaderPair(dut, 0, original, original)

      clearCInputs(dut)
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.WriteRequest(0)(lane)
        write.BankAddr.valid.poke(true.B)
        write.BankAddr.bits.poke(lane.U)
        write.Data.valid.poke(true.B)
        write.Data.bits.poke(blocked.U)
        write.ByteMask.valid.poke(true.B)
        write.ByteMask.bits.poke(((BigInt(1) << dut.AccMatrixRegEntryByteSize) - 1).U)
      }
      dut.clock.step()
      clearCInputs(dut)
      assert(readRow(dut, 0) == original)
      assert(readRow(dut, 1) == original)

      // Nonadjacent addresses, reversed parity, and consecutive full-width reads.
      val reg = dut.io.MatrixRegIO.FromMemoryLoader
      val pairs = Seq(Seq(0, 17), Seq(19, 2))
      def pattern(addr: Int, bank: Int): BigInt =
        (0 until dut.AccMatrixRegEntryByteSize).foldLeft(BigInt(0)) { (word, i) =>
          word | (BigInt((addr * 53 + bank * 17 + i) & 255) << (i * 8))
        }
      for (pair <- pairs) {
        clearCInputs(dut)
        reg.LoadReadWriteRequest.poke(4.U)
        for (bank <- 0 until dut.AccMatrixRegNBanks; (addr, slot) <- pair.zipWithIndex) {
          val w = reg.WriteRequestToMatrixReg.WriteRequest(bank)(slot)
          w.BankAddr.valid.poke(true.B)
          w.BankAddr.bits.poke(addr.U)
          w.Data.valid.poke(true.B)
          w.Data.bits.poke(pattern(addr, bank).U)
          w.ByteMask.valid.poke(true.B)
          w.ByteMask.bits.poke(((BigInt(1) << dut.AccMatrixRegEntryByteSize) - 1).U)
        }
        dut.clock.step()
      }
      clearCInputs(dut)
      reg.StoreReadWriteRequest.poke(8.U)
      for (pair <- pairs) {
        for (bank <- 0 until dut.AccMatrixRegNBanks; (addr, slot) <- pair.zipWithIndex) {
          val port = reg.ReadRequestToMatrixReg.BankAddr(bank)(slot)
          port.valid.poke(true.B)
          port.bits.poke(addr.U)
        }
        dut.clock.step()
        for (bank <- 0 until dut.AccMatrixRegNBanks; (addr, slot) <- pair.zipWithIndex) {
          val response = reg.ReadRequestToMatrixReg.ReadResponseData(bank)(slot)
          response.valid.expect(true.B)
          response.bits.expect(pattern(addr, bank).U)
        }
      }
      clearCInputs(dut)
      reg.StoreReadWriteRequest.poke(8.U)
      reg.ReadRequestToMatrixReg.BankAddr(0)(1).valid.poke(true.B)
      reg.ReadRequestToMatrixReg.BankAddr(0)(1).bits.poke(17.U)
      dut.clock.step()
      reg.ReadRequestToMatrixReg.ReadResponseData(0)(0).valid.expect(false.B)
      reg.ReadRequestToMatrixReg.ReadResponseData(0)(1).valid.expect(true.B)
      reg.ReadRequestToMatrixReg.ReadResponseData(0)(1).bits.expect(pattern(17, 0).U)
    }
  }
}
