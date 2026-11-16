package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class TrMatrixRegBankedWriteSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils._

  behavior of "TrMatrixReg odd/even loader writes"

  private def drivePair(
      dut: TrMatrixReg,
      baseAddr: Int,
      data0: BigInt,
      data1: BigInt,
      mask0: BigInt,
      mask1: BigInt
  )(implicit p: Parameters): Unit = {
    clearABLoader(dut)
    dut.io.MatrixRegIO.FromMemoryLoader.active.poke(true.B)
    Seq((0, baseAddr, data0, mask0), (1, baseAddr + 1, data1, mask1)).foreach {
      case (lane, addr, data, mask) =>
        val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequest(0)(lane)
        write.BankAddr.valid.poke(true.B)
        write.BankAddr.bits.poke(addr.U)
        write.Data.valid.poke(true.B)
        write.Data.bits.poke(data.U)
        write.ByteMask.valid.poke(true.B)
        write.ByteMask.bits.poke(mask.U)
    }
    dut.clock.step()
    clearABLoader(dut)
  }

  private def readRow(dut: TrMatrixReg, addr: Int)(implicit p: Parameters): BigInt = {
    dut.io.MatrixRegIO.FromDataController.BankAddr.bits.foreach(_.poke(addr.U))
    dut.io.MatrixRegIO.FromDataController.BankAddr.valid.poke(true.B)
    dut.io.MatrixRegIO.FromDataController.BankAddr.ready.expect(true.B)
    dut.clock.step()
    dut.io.MatrixRegIO.FromDataController.BankAddr.valid.poke(false.B)
    dut.io.MatrixRegIO.FromDataController.Data.valid.expect(true.B)
    dut.io.MatrixRegIO.FromDataController.Data.bits(0).peek().litValue
  }

  private def readStorePair(dut: TrMatrixReg, baseAddr: Int)(implicit p: Parameters): (BigInt, BigInt) = {
    dut.io.MatrixRegIO.FromStore.BankAddr.bits.foreach { bank =>
      bank(0).poke(baseAddr.U)
      bank(1).poke((baseAddr + 1).U)
    }
    dut.io.MatrixRegIO.FromStore.BankAddr.valid.poke(true.B)
    dut.io.MatrixRegIO.FromStore.BankAddr.ready.expect(true.B)
    dut.clock.step()
    dut.io.MatrixRegIO.FromStore.BankAddr.valid.poke(false.B)
    dut.io.MatrixRegIO.FromStore.Data.valid.expect(true.B)
    (
      dut.io.MatrixRegIO.FromStore.Data.bits(0)(0).peek().litValue,
      dut.io.MatrixRegIO.FromStore.Data.bits(0)(1).peek().litValue
    )
  }

  it should "cover pair writes, masks, write priority, and reset" in {
    implicit val p: Parameters = parameters
    test(new TrMatrixReg(0)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearABRead(dut)
      clearABLoader(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.clock.step()
      dut.io.MatrixRegIO.FromDataController.Data.valid.expect(false.B)

      val fullMask = (BigInt(1) << dut.TrMatrixRegEntryByteSize) - 1
      val data2 = BigInt("22" * dut.TrMatrixRegEntryByteSize, 16)
      val data3 = BigInt("33" * dut.TrMatrixRegEntryByteSize, 16)
      val data4 = BigInt("44" * dut.TrMatrixRegEntryByteSize, 16)
      drivePair(dut, 2, data2, data3, fullMask, fullMask)
      assert(readRow(dut, 2) == data2)
      assert(readRow(dut, 3) == data3)
      assert(readStorePair(dut, 2) == (data2, data3))

      drivePair(dut, 3, data3, data4, fullMask, fullMask)
      assert(readRow(dut, 3) == data3)
      assert(readRow(dut, 4) == data4)
      assert(readStorePair(dut, 3) == (data3, data4))

      val oldData = BigInt("11" * dut.TrMatrixRegEntryByteSize, 16)
      val newData = BigInt("aa" * dut.TrMatrixRegEntryByteSize, 16)
      drivePair(dut, 6, oldData, oldData, fullMask, fullMask)

      dut.io.MatrixRegIO.FromDataController.BankAddr.bits.foreach(_.poke(6.U))
      dut.io.MatrixRegIO.FromDataController.BankAddr.valid.poke(true.B)
      clearABLoader(dut)
      dut.io.MatrixRegIO.FromMemoryLoader.active.poke(true.B)
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequest(0)(lane)
        write.BankAddr.valid.poke(true.B)
        write.BankAddr.bits.poke((6 + lane).U)
        write.Data.valid.poke(true.B)
        write.Data.bits.poke(newData.U)
        write.ByteMask.valid.poke(true.B)
        write.ByteMask.bits.poke((if (lane == 0) BigInt(1) else fullMask).U)
      }
      dut.io.MatrixRegIO.FromDataController.BankAddr.ready.expect(false.B)
      dut.clock.step()
      clearABLoader(dut)
      clearABRead(dut)

      assert(readRow(dut, 6) == ((oldData & ~BigInt(0xff)) | BigInt(0xaa)))
      assert(readRow(dut, 7) == newData)

      // A pending paired store read must wait for loader writes to release both SRAMs.
      dut.io.MatrixRegIO.FromStore.BankAddr.bits.foreach { bank =>
        bank(0).poke(12.U)
        bank(1).poke(13.U)
      }
      dut.io.MatrixRegIO.FromStore.BankAddr.valid.poke(true.B)
      dut.io.MatrixRegIO.FromMemoryLoader.active.poke(true.B)
      for (lane <- 0 until WriteLanes) {
        val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequest(0)(lane)
        write.BankAddr.valid.poke(true.B)
        write.BankAddr.bits.poke((12 + lane).U)
        write.Data.valid.poke(true.B)
        write.Data.bits.poke(newData.U)
        write.ByteMask.valid.poke(true.B)
        write.ByteMask.bits.poke(fullMask.U)
      }
      dut.io.MatrixRegIO.FromStore.BankAddr.ready.expect(false.B)
      dut.clock.step()
      dut.io.MatrixRegIO.FromStore.Data.valid.expect(false.B)
      clearABLoader(dut)
      dut.io.MatrixRegIO.FromStore.BankAddr.ready.expect(true.B)
      dut.clock.step()
      dut.io.MatrixRegIO.FromStore.BankAddr.valid.poke(false.B)
      dut.io.MatrixRegIO.FromStore.Data.valid.expect(true.B)
      dut.io.MatrixRegIO.FromStore.Data.bits(0).foreach(_.expect(newData.U))
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.io.MatrixRegIO.FromStore.Data.valid.expect(false.B)
      dut.reset.poke(false.B)

      clearABLoader(dut)
      dut.io.MatrixRegIO.FromMemoryLoader.active.poke(true.B)
      Seq((0, 8), (1, 10)).foreach { case (lane, addr) =>
        val write = dut.io.MatrixRegIO.FromMemoryLoader.WriteRequest(0)(lane)
        write.BankAddr.valid.poke(true.B)
        write.BankAddr.bits.poke(addr.U)
        write.Data.valid.poke(true.B)
        write.Data.bits.poke(newData.U)
        write.ByteMask.valid.poke(true.B)
        write.ByteMask.bits.poke(fullMask.U)
      }
      assertThrows[Exception](dut.clock.step())
    }
  }
}
