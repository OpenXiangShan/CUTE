package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class BitMicroTaskConfigIO(implicit p: Parameters) extends CuteBundle {
  val MatrixRegId = Output(UInt(MatrixRegIdWidth.W))
  val packedMode = Output(UInt(2.W))
  val isInt4 = Output(Bool())
  val kslice = Output(UInt(3.W))
  val MicroTaskValid = Output(Bool())
  val MicroTaskReady = Input(Bool())
  val MicroTaskEndValid = Input(Bool())
  val MicroTaskEndReady = Output(Bool())
  val pc = Option.when(EnableDifftest)(Output(UInt(64.W)))
  val coreid = Option.when(EnableDifftest)(Output(UInt(8.W)))
}

/** Six register stages, one outer-product contribution per cycle. */
class BitMatrixTE(implicit p: Parameters) extends CuteModule {
  require(Matrix_MN == 8 && ReduceWidth == 256 && ResultWidth == 32)
  val io = IO(new Bundle {
    val VectorA = Flipped(Decoupled(UInt(2048.W)))
    val VectorB = Flipped(Decoupled(UInt(1024.W)))
    val MatrixC = Flipped(Decoupled(UInt(2048.W)))
    val MatrixD = Decoupled(UInt(2048.W))
    val ConfigInfo = Flipped(new BitMicroTaskConfigIO)
    val ComputeGo = Output(Bool())
  })

  val isInt4 = RegInit(false.B)
  val binary = RegInit(false.B)
  val valid = RegInit(VecInit(Seq.fill(6)(false.B)))
  io.ConfigInfo.MicroTaskReady := !valid.asUInt.orR
  io.ConfigInfo.MicroTaskEndValid := false.B
  when(io.ConfigInfo.MicroTaskValid) {
    assert(io.ConfigInfo.MicroTaskReady, "BMTE configuration changed before drain")
    assert(io.ConfigInfo.packedMode === 1.U || io.ConfigInfo.packedMode === 2.U)
    isInt4 := io.ConfigInfo.isInt4
    binary := io.ConfigInfo.packedMode === 2.U
  }

  io.ComputeGo := true.B
  io.VectorA.ready := io.VectorB.valid && io.MatrixC.valid
  io.VectorB.ready := io.VectorA.valid && io.MatrixC.valid
  io.MatrixC.ready := io.VectorA.valid && io.VectorB.valid
  val fire = io.VectorA.fire
  valid(0) := fire
  for (stage <- 1 until 6) valid(stage) := valid(stage - 1)

  val a = Reg(Vec(8, UInt(256.W)))
  val b = Reg(Vec(8, UInt(128.W)))
  val c = Seq.fill(5)(Reg(Vec(64, UInt(32.W))))
  when(fire) {
    a := io.VectorA.bits.asTypeOf(a)
    b := io.VectorB.bits.asTypeOf(b)
    c(0) := io.MatrixC.bits.asTypeOf(c(0))
  }
  for (stage <- 1 until 5) when(valid(stage - 1)) { c(stage) := c(stage - 1) }

  // Share the widened positive and negative activation across all eight columns.
  val plus8 = Wire(Vec(8, Vec(32, SInt(9.W))))
  val minus8 = Wire(Vec(8, Vec(32, SInt(9.W))))
  val plus4 = Wire(Vec(8, Vec(64, SInt(5.W))))
  val minus4 = Wire(Vec(8, Vec(64, SInt(5.W))))
  for (row <- 0 until 8) {
    for (lane <- 0 until 32) {
      plus8(row)(lane) := Mux(isInt4, 0.S, a(row)(lane * 8 + 7, lane * 8).asSInt).pad(9)
      minus8(row)(lane) := -Mux(binary, 0.S(9.W), plus8(row)(lane))
    }
    for (lane <- 0 until 64) {
      plus4(row)(lane) := Mux(isInt4, a(row)(lane * 4 + 3, lane * 4).asSInt, 0.S).pad(5)
      minus4(row)(lane) := -Mux(binary, 0.S(5.W), plus4(row)(lane))
    }
  }
  def term(weight: UInt, positive: SInt, negative: SInt): SInt =
    Mux(weight(0), Mux(!binary && weight(1), negative, positive), 0.S(positive.getWidth.W))

  val pipe0 = Reg(Vec(64, Vec(16, SInt(10.W))))
  val pipe1 = Reg(Vec(64, Vec(8, SInt(11.W))))
  val pipe2 = Reg(Vec(64, Vec(4, SInt(12.W))))
  val pipe3 = Reg(Vec(64, Vec(2, SInt(13.W))))
  val result = Reg(Vec(64, UInt(32.W)))
  for (row <- 0 until 8; col <- 0 until 8) {
    val pe = row * 8 + col
    val terms8 = (0 until 32).map { lane =>
      val weight = Mux(binary, Cat(0.U(1.W), b(col)(lane)), b(col)(lane * 2 + 1, lane * 2))
      term(weight, plus8(row)(lane), minus8(row)(lane))
    }
    val terms4 = (0 until 64).map { lane =>
      val weight = Mux(binary, Cat(0.U(1.W), b(col)(lane)), b(col)(lane * 2 + 1, lane * 2))
      term(weight, plus4(row)(lane), minus4(row)(lane))
    }
    when(valid(0)) {
      for (node <- 0 until 16) {
        val pair4Lo = terms4(node * 4) +& terms4(node * 4 + 1)
        val pair4Hi = terms4(node * 4 + 2) +& terms4(node * 4 + 3)
        // Select operands before the final addition so i8 and i4 share it.
        val operand0 = Mux(isInt4, pair4Lo.pad(9), terms8(node * 2))
        val operand1 = Mux(isInt4, pair4Hi.pad(9), terms8(node * 2 + 1))
        pipe0(pe)(node) := operand0 +& operand1
      }
    }
    when(valid(1)) { for (node <- 0 until 8) pipe1(pe)(node) := pipe0(pe)(node * 2) +& pipe0(pe)(node * 2 + 1) }
    when(valid(2)) { for (node <- 0 until 4) pipe2(pe)(node) := pipe1(pe)(node * 2) +& pipe1(pe)(node * 2 + 1) }
    when(valid(3)) { for (node <- 0 until 2) pipe3(pe)(node) := pipe2(pe)(node * 2) +& pipe2(pe)(node * 2 + 1) }
    when(valid(4)) {
      val sum = pipe3(pe)(0) +& pipe3(pe)(1)
      result(pe) := (c(4)(pe) + sum.pad(32).asUInt)(31, 0)
    }
  }
  io.MatrixD.valid := valid(5)
  io.MatrixD.bits := Mux(valid(5), result.asUInt, 0.U)
  when(io.MatrixD.valid) { assert(io.MatrixD.ready, "BMTE result requires continuous writeback") }
}
