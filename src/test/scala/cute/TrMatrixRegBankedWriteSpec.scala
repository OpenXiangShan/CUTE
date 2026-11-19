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
      // The valid-qualified SRAM ports stay idle until a client requests a read.
      dut.io.MatrixRegIO.FromDataController.BankAddr.ready.expect(false.B)
      dut.io.MatrixRegIO.FromStore.BankAddr.ready.expect(false.B)
      dut.io.MatrixRegIO.FromStore.Data.valid.expect(false.B)

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

  behavior of "TrMatrixReg read routing"

  it should "bind responses to accepted requests across idle IDs, stalls and target changes" in {
    implicit val p: Parameters = parameters
    test(new TrMatrixRegReadPortRouter).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      var accepted = Seq.empty[(Int, Int)]
      for (cycle <- 0 until 48) {
        val active = Seq(cycle % 4 != 0, cycle % 3 != 0)
        // Both idle IDs may coincide; only active readers participate in selection.
        val ids = if (cycle % 4 == 0) Seq(0, 0) else Seq(cycle % 4, (cycle + 1) % 4)
        for (client <- 0 until 2) {
          dut.io.regIds(client).poke(ids(client).U)
          dut.io.clients(client).BankAddr.valid.poke(active(client).B)
          dut.io.clients(client).BankAddr.bits.foreach(_.poke((cycle % 8).U))
        }
        for (reg <- 0 until 4) {
          val port = dut.io.registers(reg)
          port.BankAddr.ready.poke((cycle % 5 != reg).B)
          port.Data.valid.poke(accepted.exists(_._2 == reg).B)
          port.Data.bits.foreach(_.poke((reg * 256 + cycle).U))
          port.BankAddr.valid.expect((0 until 2).exists(c => active(c) && ids(c) == reg).B)
        }
        for (client <- 0 until 2) {
          val previous = accepted.find(_._1 == client)
          dut.io.clients(client).Data.valid.expect(previous.nonEmpty.B)
          previous match {
            case Some((_, reg)) =>
              dut.io.clients(client).Data.bits.foreach(_.expect((reg * 256 + cycle).U))
            case None => dut.io.clients(client).Data.bits.foreach(_.expect(0.U))
          }
          dut.io.clients(client).BankAddr.ready.expect((active(client) && cycle % 5 != ids(client)).B)
        }
        accepted = (0 until 2).filter(c => active(c) && cycle % 5 != ids(c)).map(c => (c, ids(c)))
        dut.clock.step()
      }
    }
  }

}
