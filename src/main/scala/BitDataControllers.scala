package cute

import chisel3._
import chisel3.util._
import difftest.DiffAmuFinishEvent
import org.chipsalliance.cde.config.Parameters

class BitBBeatRequest extends Bundle {
  val nGroup = UInt(4.W)
  val chunk = Bool()
  val packedMode = UInt(2.W)
  val isInt4 = Bool()
  val kslice = UInt(3.W)
}

/** One outstanding read, with a bypass and a hold for the selected packed data. */
class BitPackedBReader(implicit p: Parameters) extends CuteModule {
  require(ABMatrixRegNBanks == 8 && ABMatrixRegEntryBitSize == 256)
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new BitBBeatRequest))
    val FromMatrixRegIO = Flipped(new ABDataControlMatrixRegIO)
    val VectorB = Decoupled(UInt(1024.W))
    val delivered = Output(new BitBBeatRequest)
  })

  val pending = RegInit(false.B)
  val metadata = Reg(new BitBBeatRequest)
  val offset = Reg(UInt(8.W))
  val holdValid = RegInit(false.B)
  val hold = Reg(UInt(1024.W))
  val holdMetadata = Reg(new BitBBeatRequest)
  val request = io.request.bits
  val step = Mux(request.isInt4,
    Mux(request.packedMode === 1.U, 128.U(10.W), 64.U(10.W)),
    Mux(request.packedMode === 1.U, 64.U(10.W), 32.U(10.W)))
  val bitPosition = request.kslice * (step << 1) + Mux(request.chunk, step, 0.U)
  val address = Cat(request.nGroup, 0.U(1.W)) + (bitPosition >> 8)
  val response = io.FromMatrixRegIO.Data.valid
  val selected = Wire(Vec(8, UInt(128.W)))
  val mask = Mux(metadata.isInt4,
    Mux(metadata.packedMode === 1.U, ((BigInt(1) << 128) - 1).U(128.W), ((BigInt(1) << 64) - 1).U(128.W)),
    Mux(metadata.packedMode === 1.U, ((BigInt(1) << 64) - 1).U(128.W), ((BigInt(1) << 32) - 1).U(128.W)))
  for (bank <- 0 until 8) {
    selected(bank) := (io.FromMatrixRegIO.Data.bits(bank) >> offset)(127, 0) & mask
  }
  io.VectorB.valid := holdValid || (pending && response)
  io.VectorB.bits := Mux(holdValid, hold, Mux(pending && response, selected.asUInt, 0.U))
  io.delivered := Mux(holdValid, holdMetadata, metadata)

  val credit = (!holdValid && !pending) || io.VectorB.fire
  io.FromMatrixRegIO.BankAddr.valid := io.request.valid && credit
  io.FromMatrixRegIO.BankAddr.bits.foreach(_ := address)
  io.request.ready := credit && io.FromMatrixRegIO.BankAddr.ready
  when(response) {
    assert(pending && !holdValid, "BMTE B response without a pending request")
    pending := false.B
    when(!io.VectorB.ready) {
      hold := selected.asUInt
      holdMetadata := metadata
      holdValid := true.B
    }
  }
  when(io.VectorB.fire && holdValid) { holdValid := false.B }
  when(io.FromMatrixRegIO.BankAddr.fire) {
    assert(request.packedMode === 1.U || request.packedMode === 2.U)
    val limit = Mux(request.isInt4,
      Mux(request.packedMode === 1.U, 2.U, 4.U), Mux(request.packedMode === 1.U, 4.U, 8.U))
    assert(request.kslice < limit, "BMTE B selector outside the resident row")
    metadata := request
    offset := bitPosition(7, 0)
    pending := true.B
  }
  assert(!(holdValid && pending), "BMTE B response storage exceeded")
  val wasBlocked = RegNext(io.VectorB.valid && !io.VectorB.ready, false.B)
  val previousBits = RegNext(io.VectorB.bits)
  val previousMetadata = RegNext(io.delivered.asUInt)
  when(wasBlocked) {
    assert(io.VectorB.valid && io.VectorB.bits === previousBits)
    assert(io.delivered.asUInt === previousMetadata)
  }
}

class BitBDataController(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val FromMatrixRegIO = Flipped(new ABDataControlMatrixRegIO)
    val ConfigInfo = Flipped(new BitMicroTaskConfigIO)
    val VectorB = Decoupled(UInt(1024.W))
    val ComputeGo = Input(Bool())
    val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
  })
  val idle :: init :: working :: ending :: Nil = Enum(4)
  val state = RegInit(idle)
  val regId = RegInit(0.U(ABMatrixRegIdWidth.W))
  val mode = Reg(UInt(2.W))
  val int4 = Reg(Bool())
  val slice = Reg(UInt(3.W))
  val requests = RegInit(0.U(10.W))
  val delivered = RegInit(0.U(10.W))
  val reader = Module(new BitPackedBReader)
  reader.io.FromMatrixRegIO <> io.FromMatrixRegIO
  io.VectorB <> reader.io.VectorB
  reader.io.request.valid := state === working && requests < 512.U && io.ComputeGo
  reader.io.request.bits.nGroup := requests(3, 0)
  reader.io.request.bits.chunk := requests(8)
  reader.io.request.bits.packedMode := mode
  reader.io.request.bits.isInt4 := int4
  reader.io.request.bits.kslice := slice
  io.MatrixRegId := regId
  io.ConfigInfo.MicroTaskReady := state === idle
  io.ConfigInfo.MicroTaskEndValid := state === ending
  when(io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
    regId := io.ConfigInfo.MatrixRegId
    mode := io.ConfigInfo.packedMode
    int4 := io.ConfigInfo.isInt4
    slice := io.ConfigInfo.kslice
    requests := 0.U
    delivered := 0.U
    state := init
  }
  when(state === init) { state := working }
  when(reader.io.request.fire) { requests := requests + 1.U }
  when(io.VectorB.fire) {
    delivered := delivered + 1.U
    when(delivered === 511.U) {
      assert(requests === 512.U)
      state := ending
    }
  }
  when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady) { state := idle }
}

/** Native A is two 256-bit chunks, independently of its i8/i4 interpretation. */
class BitADataController(implicit p: Parameters) extends CuteModule {
  require(ABMatrixRegNBanks == 8 && ABMatrixRegEntryBitSize == 256)
  val io = IO(new Bundle {
    val FromMatrixRegIO = Flipped(new ABDataControlMatrixRegIO)
    val ConfigInfo = Flipped(new BitMicroTaskConfigIO)
    val VectorA = Decoupled(UInt(2048.W))
    val ComputeGo = Input(Bool())
    val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
  })
  val idle :: init :: working :: ending :: Nil = Enum(4)
  val state = RegInit(idle)
  val regId = RegInit(0.U(ABMatrixRegIdWidth.W))
  val requests = RegInit(0.U(10.W))
  val delivered = RegInit(0.U(10.W))
  val pending = RegInit(false.B)
  val holdValid = RegInit(false.B)
  val hold = Reg(UInt(2048.W))
  val response = io.FromMatrixRegIO.Data.valid
  io.VectorA.valid := holdValid || (pending && response)
  io.VectorA.bits := Mux(holdValid, hold, Mux(pending && response, io.FromMatrixRegIO.Data.bits.asUInt, 0.U))
  val credit = (!holdValid && !pending) || io.VectorA.fire
  io.FromMatrixRegIO.BankAddr.valid := state === working && requests < 512.U && io.ComputeGo && credit
  io.FromMatrixRegIO.BankAddr.bits.foreach(_ := Cat(requests(7, 4), requests(8)))
  io.MatrixRegId := regId
  io.ConfigInfo.MicroTaskReady := state === idle
  io.ConfigInfo.MicroTaskEndValid := state === ending
  when(io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
    regId := io.ConfigInfo.MatrixRegId
    requests := 0.U
    delivered := 0.U
    state := init
  }
  when(state === init) { state := working }
  when(response) {
    assert(pending && !holdValid)
    pending := false.B
    when(!io.VectorA.ready) {
      hold := io.FromMatrixRegIO.Data.bits.asUInt
      holdValid := true.B
    }
  }
  when(io.VectorA.fire && holdValid) { holdValid := false.B }
  when(io.FromMatrixRegIO.BankAddr.fire) {
    requests := requests + 1.U
    pending := true.B
  }
  when(io.VectorA.fire) {
    delivered := delivered + 1.U
    when(delivered === 511.U) {
      assert(requests === 512.U)
      state := ending
    }
  }
  when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady) { state := idle }
  assert(!(holdValid && pending))
}

/** The continuous 512-beat stream retains the seven-cycle C bank schedule. */
class BitCDataController(implicit p: Parameters) extends CuteModule {
  require(CMatrixRegNBanks == 8 && CMatrixRegEntryBitSize == 256)
  val io = IO(new Bundle {
    val FromMatrixRegIO = Flipped(new CDataControlMatrixRegIO)
    val ConfigInfo = Flipped(new BitMicroTaskConfigIO)
    val Matrix_C = Decoupled(UInt(2048.W))
    val ResultMatrix_D = Flipped(Decoupled(UInt(2048.W)))
    val ComputeGo = Input(Bool())
    val MatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
  })
  val idle :: init :: working :: ending :: Nil = Enum(4)
  val state = RegInit(idle)
  val regId = RegInit(0.U(CMatrixRegIdWidth.W))
  val requests = RegInit(0.U(10.W))
  val fed = RegInit(0.U(10.W))
  val writes = RegInit(0.U(10.W))
  io.ConfigInfo.MicroTaskReady := state === idle
  io.ConfigInfo.MicroTaskEndValid := state === ending
  io.MatrixRegId := regId
  val read = state === working && requests < 512.U && io.ComputeGo
  val response = io.FromMatrixRegIO.ReadResponseData.map(_.valid).reduce(_ && _)
  io.Matrix_C.valid := state === working && response
  io.Matrix_C.bits := Mux(io.Matrix_C.valid, VecInit(io.FromMatrixRegIO.ReadResponseData.map(_.bits)).asUInt, 0.U)
  io.ResultMatrix_D.ready := state === working
  val write = io.ResultMatrix_D.fire
  io.FromMatrixRegIO.ReadWriteRequest := Mux(read, MatrixRegTaskType.EnableReadFromDataController, 0.U) |
    Mux(write, MatrixRegTaskType.EnableWriteFromDataController, 0.U)
  for (bank <- 0 until 8) {
    io.FromMatrixRegIO.ReadBankAddr(bank).valid := read
    io.FromMatrixRegIO.ReadBankAddr(bank).bits := requests(7, 0)
    io.FromMatrixRegIO.WriteBankAddr(bank).valid := write
    io.FromMatrixRegIO.WriteBankAddr(bank).bits := writes(7, 0)
    io.FromMatrixRegIO.WriteRequestData(bank).valid := write
    io.FromMatrixRegIO.WriteRequestData(bank).bits := io.ResultMatrix_D.bits(bank * 256 + 255, bank * 256)
  }
  when(io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
    regId := io.ConfigInfo.MatrixRegId
    requests := 0.U
    fed := 0.U
    writes := 0.U
    state := init
  }
  when(state === init) { state := working }
  when(read) { requests := requests + 1.U }
  when(io.Matrix_C.valid) {
    assert(io.Matrix_C.ready, "BMTE must consume every C response")
    fed := fed + 1.U
  }
  val delayedRead = (0 until 7).foldLeft(read) { (valid, _) => RegNext(valid, false.B) }
  when(state === working) { assert(write === delayedRead, "BMTE C read-to-write must be seven cycles") }
  when(read && write) { assert(requests(0) =/= writes(0), "BMTE C single-port bank conflict") }
  when(write) {
    assert(writes < 512.U)
    assert(io.FromMatrixRegIO.ReadWriteResponse(MatrixRegTaskType.WriteFromDataControllerIndex), "BMTE C write not accepted")
    writes := writes + 1.U
    when(writes === 511.U) {
      assert(requests === 512.U && fed === 512.U)
      state := ending
    }
  }
  when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady) { state := idle }

  if (EnableDifftest) {
    val start = io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady
    val pc = RegEnable(io.ConfigInfo.pc.get, start)
    val coreid = RegEnable(io.ConfigInfo.coreid.get, start)
    val event = CuteDifftest(new DiffAmuFinishEvent(CMatrixRegNBanks, DiffAmuFinishWordsPerBank), delay = 0, dontCare = true)
    event.coreid := coreid
    event.index := 3.U
    event.pc := pc
    event.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    event.valid := write || event.finish
    for (bank <- 0 until 8) {
      event.bankValid(bank) := write
      event.bankAddr(bank) := writes(7, 0)
      event.bankMask(bank) := Fill(CMatrixRegEntryByteSize, true.B)
      for (word <- 0 until DiffAmuFinishWordsPerBank) {
        event.data(bank * DiffAmuFinishWordsPerBank + word) := io.ResultMatrix_D.bits(bank * 256 + word * 64 + 63, bank * 256 + word * 64)
      }
    }
  }
}
