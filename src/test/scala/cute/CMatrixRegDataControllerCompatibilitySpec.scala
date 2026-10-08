package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class CMatrixRegDataControllerCompatibilitySpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils._

  behavior of "CMatrixReg data-controller compatibility"

  it should "preserve single-row writes, task response, parity addressing, and one-cycle reads" in {
    implicit val p: Parameters = parameters
    test(new CMatrixReg(0)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      clearCInputs(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      val io = dut.io.MatrixRegIO
      val dataEven = BigInt("6a" * dut.CMatrixRegEntryByteSize, 16)
      val dataOdd = BigInt("b5" * dut.CMatrixRegEntryByteSize, 16)
      Seq((2, dataEven), (3, dataOdd)).foreach { case (addr, data) =>
        io.FromDataController.ReadWriteRequest.poke(2.U)
        io.FromDataController.WriteBankAddr(0).valid.poke(true.B)
        io.FromDataController.WriteBankAddr(0).bits.poke(addr.U)
        io.FromDataController.WriteRequestData(0).valid.poke(true.B)
        io.FromDataController.WriteRequestData(0).bits.poke(data.U)
        io.FromDataController.ReadWriteResponse.expect(2.U)
        dut.clock.step()
        clearCInputs(dut)
      }

      Seq((2, dataEven), (3, dataOdd)).foreach { case (addr, expected) =>
        io.FromDataController.ReadWriteRequest.poke(1.U)
        io.FromDataController.ReadBankAddr(0).valid.poke(true.B)
        io.FromDataController.ReadBankAddr(0).bits.poke(addr.U)
        io.FromDataController.ReadWriteResponse.expect(1.U)
        dut.clock.step()
        io.FromDataController.ReadWriteRequest.poke(0.U)
        io.FromDataController.ReadBankAddr(0).valid.poke(false.B)
        io.FromDataController.ReadResponseData(0).valid.expect(true.B)
        io.FromDataController.ReadResponseData(0).bits.expect(expected.U)
      }
    }
  }
}
