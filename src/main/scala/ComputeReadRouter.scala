package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

/** Route a private TR response using the source of the accepted SRAM read. */
class ComputeReadRouter(sourceCount: Int)(implicit p: Parameters) extends CuteModule {
  require(sourceCount >= 2)
  val io = IO(new Bundle {
    val source = Vec(sourceCount, new ABDataControlMatrixRegIO)
    val registers = Vec(ABMatrixRegCount, Flipped(new ABDataControlMatrixRegIO))
    val ownerActive = Input(Vec(sourceCount, Bool()))
    val regId = Input(Vec(sourceCount, UInt(ABMatrixRegIdWidth.W)))
  })
  val responseSource = Seq.fill(ABMatrixRegCount)(RegInit(0.U(log2Ceil(sourceCount).W)))
  for (regIdx <- 0 until ABMatrixRegCount) {
    val dest = io.registers(regIdx)
    val selected = (0 until sourceCount).map { source =>
      io.ownerActive(source) && io.regId(source) === regIdx.U && io.source(source).BankAddr.valid
    }
    assert(PopCount(VecInit(selected)) <= 1.U, "TR has multiple compute readers")
    dest.BankAddr.valid := selected.reduce(_ || _)
    dest.BankAddr.bits := Mux1H(selected zip io.source.map(_.BankAddr.bits))
    when(dest.BankAddr.fire) { responseSource(regIdx) := OHToUInt(VecInit(selected)) }
  }
  for (source <- 0 until sourceCount) {
    val port = io.source(source)
    port.BankAddr.ready := Mux1H((0 until ABMatrixRegCount).map { regIdx =>
      (io.ownerActive(source) && io.regId(source) === regIdx.U) -> io.registers(regIdx).BankAddr.ready
    })
    val responses = (0 until ABMatrixRegCount).map { regIdx =>
      val response = io.registers(regIdx).Data
      (responseSource(regIdx) === source.U && response.valid) -> response.bits
    }
    port.Data.valid := responses.map(_._1).reduce(_ || _)
    port.Data.bits := Mux1H(responses)
  }
}
