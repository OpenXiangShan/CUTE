package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

object ScaleTestParams {
  def params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_2Tops.copy(
      EnableDifftest = false, Matrix_MN = 2, Tensor_MN = 8,
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
  })
}

// SRAM model has the canonical one-cycle response; production ASC/BSC retain
// ownership and hold registers. No replacement controller is implemented here.
class ScaleReaderTop(val reduceBytes: Int = 32, val partial: Boolean = false,
    val exactK: Int = 0, val probeMte: Boolean = false) extends Module {
  implicit val p: Parameters = ScaleTestParams.params.alterPartial {
    case CuteParamsKey => ScaleTestParams.params(CuteParamsKey).copy(
      ReduceWidthByte = reduceBytes, Tensor_MN = 128)
  }
  val io = IO(new Bundle {
    val start = Input(Bool())
    val mode = Input(UInt(4.W))
    val go = Input(Bool())
    val memReady = Input(Bool())
    val aValid = Output(Bool())
    val bValid = Output(Bool())
    val a = Output(UInt((p(CuteParamsKey).ScaleWidth * 2).W))
    val b = Output(UInt((p(CuteParamsKey).ScaleWidth * 2).W))
    val end = Output(Bool())
    val bankA = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val bankB = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val idle = Output(Bool())
    val liveA = Output(Vec(2, UInt(8.W)))
    val liveB = Output(Vec(2, UInt(8.W)))
    val resultValid = Output(Bool())
    val result = Output(UInt(128.W))
    val resultFlags = Output(Vec(4, UInt(5.W)))
  })
  val a = Module(new AScaleController)
  val b = Module(new BScaleController)
  val blocks = if (exactK > 0) ((exactK + 31) / 32).U else if (partial) 2.U
    else Mux(io.mode === 10.U, (reduceBytes / 16 * 2).U, (reduceBytes / 32 * 2).U)
  val perVector = Mux(io.mode === 10.U, (reduceBytes / 16).U, (reduceBytes / 32).U)
  val kVectors = if (partial || exactK > 0) (blocks +& (perVector - 1.U)) / perVector else 2.U
  a.io.DebugInfo.DebugTimeStampe := 0.U
  b.io.DebugInfo.DebugTimeStampe := 0.U
  a.io.ConfigInfo.MatrixRegTensor_M := (if (partial) 3 else 4).U
  a.io.ConfigInfo.MatrixRegTensor_N := (if (partial) 65 else 4).U
  a.io.ConfigInfo.MatrixRegTensor_K := kVectors
  a.io.ConfigInfo.ScaleBlocksPerRow := blocks
  a.io.ConfigInfo.KElements := (if (exactK > 0) exactK.U else if (partial) 33.U else blocks * 32.U)
  a.io.ConfigInfo.MatrixRegId := 0.U
  a.io.ConfigInfo.ScaleBankId := 1.U
  a.io.ConfigInfo.computeType := io.mode
  a.io.ConfigInfo.Is_Transpose := false.B
  a.io.ConfigInfo.MicroTaskValid := io.start
  a.io.ConfigInfo.MicroTaskEndReady := true.B
  b.io.ConfigInfo.MatrixRegTensor_M := (if (partial) 3 else 4).U
  b.io.ConfigInfo.MatrixRegTensor_N := (if (partial) 65 else 4).U
  b.io.ConfigInfo.MatrixRegTensor_K := kVectors
  b.io.ConfigInfo.ScaleBlocksPerRow := blocks
  b.io.ConfigInfo.KElements := (if (exactK > 0) exactK.U else if (partial) 33.U else blocks * 32.U)
  b.io.ConfigInfo.MatrixRegId := 1.U
  b.io.ConfigInfo.ScaleBankId := 0.U
  b.io.ConfigInfo.computeType := io.mode
  b.io.ConfigInfo.Is_Transpose := false.B
  b.io.ConfigInfo.MicroTaskValid := io.start
  b.io.ConfigInfo.MicroTaskEndReady := true.B
  a.io.ComputeGo := io.go
  b.io.ComputeGo := io.go
  a.io.ReadIssueEnable := true.B
  b.io.ReadIssueEnable := true.B
  a.io.ScaleA.ready := io.go
  b.io.ScaleB.ready := io.go
  for ((mem, rows, aSide) <- Seq((a.io.FromMatrixRegIO, if (partial) 3 else 4, true),
      (b.io.FromMatrixRegIO, if (partial) 65 else 4, false))) {
    mem.BankAddr.ready := io.memReady
    mem.Data.valid := RegNext(mem.BankAddr.fire, false.B)
    val word = Wire(Vec(64, UInt(8.W)))
    for (i <- 0 until 64) {
      val offset = mem.BankAddr.bits * 64.U + i.U
      word(i) := Mux(offset < rows.U * blocks, (offset + 1.U)(7, 0), 255.U)
      if (probeMte) {
        val oddRow = (offset / blocks)(0)
        word(i) := (if (aSide)
          Mux(oddRow, Mux(offset % blocks === 0.U, 128.U, 126.U),
            Mux(offset % blocks === 0.U, 127.U, 129.U))
        else
          Mux(oddRow, Mux(offset % blocks === 0.U, 129.U, 127.U),
            Mux(offset % blocks === 0.U, 127.U, 128.U)))
      }
    }
    mem.Data.bits := RegEnable(word.asUInt, mem.BankAddr.fire).asTypeOf(mem.Data.bits)
  }
  io.aValid := a.io.ScaleA.valid
  io.bValid := b.io.ScaleB.valid
  io.a := a.io.ScaleA.bits
  io.b := b.io.ScaleB.bits
  io.end := a.io.ConfigInfo.MicroTaskEndValid && b.io.ConfigInfo.MicroTaskEndValid
  io.bankA := a.io.SpadId
  io.bankB := b.io.SpadId
  io.idle := a.io.idle && b.io.idle
  io.liveA := a.io.LiveK
  io.liveB := b.io.LiveK
  io.resultValid := false.B
  io.result := 0.U
  io.resultFlags := 0.U.asTypeOf(io.resultFlags)
  if (probeMte) {
    require(reduceBytes == 32 && exactK == 64 && !partial)
    val m = Module(new MatrixTE)
    m.io.DebugInfo.DebugTimeStampe := 0.U
    m.io.ConfigInfo.MicroTaskValid := io.start
    m.io.ConfigInfo.computeType := MteComputeType.Mxfp4F32
    m.io.ScaleA.get <> a.io.ScaleA
    m.io.ScaleB.get <> b.io.ScaleB
    m.io.ScaleALiveK.get := a.io.LiveK
    m.io.ScaleBLiveK.get := b.io.LiveK
    a.io.ComputeGo := m.io.ComputeGo
    b.io.ComputeGo := m.io.ComputeGo
    // Every row has first-block value1 and second-block value3 on A.
    m.io.VectorA.bits := Fill(2, Cat(Fill(32, 5.U(4.W)), Fill(32, 2.U(4.W))))
    m.io.VectorB.bits := Fill(128, 2.U(4.W))
    m.io.VectorA.valid := a.io.ScaleA.valid
    m.io.VectorB.valid := b.io.ScaleB.valid
    // Only PE(1,0) sees a signaling C NaN; attribution must remain local.
    m.io.MatrixC.bits := (BigInt("7f800001", 16) << 64).U
    m.io.MatrixC.valid := a.io.ScaleA.valid && b.io.ScaleB.valid
    m.io.MatrixD.ready := io.go
    io.resultValid := m.io.MatrixD.valid
    io.result := m.io.MatrixD.bits
    io.resultFlags := m.io.DFlags
  }
}

class MatrixScaleTupleTop(val flagsProbe: Boolean = false,
    val paddingProbe: Boolean = false) extends Module {
  // One real FPE is sufficient for the MTE tuple contract; do not pay for a
  // replicated array when this test does not exercise broadcast geometry.
  implicit val p: Parameters = ScaleTestParams.params.alterPartial {
    case CuteParamsKey => ScaleTestParams.params(CuteParamsKey).copy(
      Matrix_MN = if (flagsProbe) 4 else 1,
      ReduceWidthByte = if (flagsProbe) 32 else ScaleTestParams.params(CuteParamsKey).ReduceWidthByte,
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp4Fp32 = true, enableMxfp8Fp32 = flagsProbe))
  }
  private val lanes = p(CuteParamsKey).Matrix_MN
  val io = IO(new Bundle {
    val config = Input(Bool())
    val validMask = Input(UInt(5.W))
    val outReady = Input(Bool())
    val ready = Output(Bool())
    val outValid = Output(Bool())
    val idle = Output(Bool())
    val flags = Output(Vec(lanes*lanes, UInt(5.W)))
    // Test-only lane ports avoid obsolete wide-pointer native harness APIs.
    val result = Output(Vec(lanes*lanes, UInt(32.W)))
  })
  val m = Module(new MatrixTE)
  m.io.DebugInfo.DebugTimeStampe := 0.U
  m.io.ConfigInfo.MicroTaskValid := io.config
  m.io.ConfigInfo.computeType := (if (flagsProbe) MteComputeType.Mxfp8e4m3F32 else MteComputeType.Mxfp4F32)
  m.io.ScaleALiveK.get.foreach(_ := (if (flagsProbe) 32 else 64).U)
  m.io.ScaleBLiveK.get.foreach(_ := (if (flagsProbe) 32 else 64).U)
  if (paddingProbe) {
    require(flagsProbe)
    for (lane <- 1 until lanes) {
      m.io.ScaleALiveK.get(lane) := 0.U
      m.io.ScaleBLiveK.get(lane) := 0.U
    }
  }
  if (flagsProbe) {
    m.io.VectorA.bits := Fill(32*lanes, 0x38.U(8.W))
    m.io.VectorB.bits := Fill(32*lanes, 0x38.U(8.W))
    val c = WireInit(VecInit(Seq.fill(lanes*lanes)(0.U(32.W))))
    c(5) := "h4e000000".U // 2^29 + exact32 is an even half-ULP tie.
    c(15) := "h7f800001".U // sNaN at a PE other than (0,0).
    if (paddingProbe) {
      for (i <- 1 until lanes*lanes) {
        c(i) := Seq("h80000000", "h7f800001", "h7fc12345")(i % 3).U
      }
    }
    m.io.MatrixC.bits := c.asUInt
    val scales = VecInit(Seq(0, 127, 254, 127).map(x => x.U(p(CuteParamsKey).ScaleWidth.W)))
    m.io.ScaleA.get.bits := scales.asUInt
    m.io.ScaleB.get.bits := scales.asUInt
  } else {
    m.io.VectorA.bits := 0.U
    m.io.VectorB.bits := 0.U
    m.io.MatrixC.bits := 0.U
    m.io.ScaleA.get.bits := Fill(4, 127.U(8.W))
    m.io.ScaleB.get.bits := Fill(4, 127.U(8.W))
  }
  m.io.VectorA.valid := io.validMask(0)
  m.io.VectorB.valid := io.validMask(1)
  m.io.MatrixC.valid := io.validMask(2)
  m.io.ScaleA.get.valid := io.validMask(3)
  m.io.ScaleB.get.valid := io.validMask(4)
  m.io.MatrixD.ready := io.outReady
  io.ready := m.io.ComputeGo
  io.outValid := m.io.MatrixD.valid
  io.idle := m.io.idle
  io.flags := m.io.DFlags
  io.result := m.io.MatrixD.bits.asTypeOf(io.result)
}

class ScalePathSpec extends AnyFlatSpec with ChiselScalatestTester {
  private class ScaleBankRoutingTop extends Module {
    implicit val p: Parameters = ScaleTestParams.params.alterPartial {
      case CuteParamsKey => ScaleTestParams.params(CuteParamsKey).copy(
        ReduceWidthByte = 32, Tensor_MN = 128)
    }
    val io = IO(new Bundle {
      val compute = new ABScaleControlMatrixRegIO
      val convert = new ABScaleControlMatrixRegIO
      val computeBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val convertBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val convertActive = Input(Bool())
      val writes = Vec(p(CuteParamsKey).ABScaleRegCount, new ABScaleLoaderMatrixRegIO)
      val requestFires = Output(UInt(p(CuteParamsKey).ABScaleRegCount.W))
    })
    val banks = Seq.fill(p(CuteParamsKey).ABScaleRegCount)(Module(new ABScaleMatrixReg))
    banks.zip(io.writes).foreach { case (bank, write) => bank.io.FromScaleLoader <> write }
    CUTEV2Top.connectScaleControlToRegs(io.computeBank, io.compute, banks,
      Some(io.convert), io.convertBank, io.convertActive)(clock, reset)
    io.requestFires := VecInit(banks.map(_.io.FromScaleController.BankAddr.fire)).asUInt
  }

  it should "route a request across four architectural scale banks" in {
    test(new ScaleBankRoutingTop).withAnnotations(Seq(VerilatorBackendAnnotation,
      chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { d =>
      def pattern(bank: Int, lane: Int): BigInt =
        BigInt(0x50100000) | (BigInt(bank) << 16) | lane
      val targetBank = ScaleTestParams.params(CuteParamsKey).ABScaleRegCount - 1
      d.io.compute.BankAddr.valid.poke(false.B)
      d.io.convert.BankAddr.valid.poke(false.B)
      d.io.convertActive.poke(false.B)
      d.io.writes.zipWithIndex.foreach { case (write, bank) =>
        write.BankAddr.valid.poke(true.B); write.BankAddr.bits.poke(0.U)
        write.Data.valid.poke(true.B)
        write.Data.bits.zipWithIndex.foreach { case (word, lane) =>
          word.poke(pattern(bank, lane).U)
        }
      }
      d.clock.step()
      d.io.writes.foreach { write =>
        write.BankAddr.valid.poke(false.B); write.Data.valid.poke(false.B)
      }
      d.clock.step()

      d.io.convertActive.poke(true.B); d.io.convertBank.poke(targetBank.U)
      d.io.convert.BankAddr.valid.poke(true.B); d.io.convert.BankAddr.bits.poke(0.U)
      d.io.requestFires.expect((1 << targetBank).U); d.clock.step()
      d.io.compute.Data.valid.expect(false.B)
      d.io.convert.Data.valid.expect(true.B)
      d.io.convert.Data.bits.zipWithIndex.foreach { case (word, lane) =>
        word.expect(pattern(targetBank, lane).U)
      }
      println(s"MX_SCALE_BANK_ROUTE actual_SRAMs=${targetBank + 1} request=PASS")
    }
  }

  it should "reject simultaneous compute and convert scale readers" in {
    assertThrows[ChiselAssertionError] {
      test(new ScaleBankRoutingTop).withAnnotations(Seq(VerilatorBackendAnnotation,
        chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { d =>
        d.io.compute.BankAddr.valid.poke(true.B)
        d.io.convert.BankAddr.valid.poke(true.B)
        d.io.convertActive.poke(true.B)
        d.clock.step()
      }
    }
  }

  it should "preserve complete physical finish addresses through the existing profile" in {
    class FinishAddressProbe(geometry: CuteParams) extends Module {
      private val bits = geometry.DiffAmuFinishAddrBits
      val io = IO(new Bundle {
        val in = Input(UInt(bits.W))
        val out = Output(UInt(bits.W))
      })
      val event = Wire(new difftest.DiffAmuFinishEvent(
        geometry.Matrix_MN, geometry.DiffAmuFinishWordsPerBank, bits))
      event := 0.U.asTypeOf(event)
      event.bankAddr.foreach(_ := io.in)
      val profile = difftest.util.BundleProfile.fromBundle(event, 0)
      require(profile.classArgs("bankAddrBits") == bits)
      val restored = Wire(profile.toBundle.asInstanceOf[difftest.DiffAmuFinishEvent])
      require(restored.bankAddr.head.getWidth == bits)
      restored := event
      io.out := restored.bankAddr.head
    }
    for (geometry <- Seq(CuteParams.CUTE_2Tops_128SCP,
        CuteParams.CUTE_8Tops_128SCP, CuteParams.CUTE_8Tops_512SCP)) {
      val last = geometry.CMatrixRegBankNEntries - 1
      test(new FinishAddressProbe(geometry)) { d =>
        for (address <- Seq(0, 15, 16, 31, 32, 255, 256, 511, 512, 1023, last)
            .filter(_ <= last).distinct) {
          d.io.in.poke(address.U)
          d.io.out.expect(address.U)
        }
      }
    }
  }

  private def pokeLanes(lanes: Vec[UInt], bits: BigInt): Unit =
    lanes.zipWithIndex.foreach { case (lane, i) =>
      lane.poke(((bits >> (32*i)) & BigInt("ffffffff", 16)).U)
    }
  private def peekLanes(lanes: Vec[UInt]): BigInt =
    lanes.zipWithIndex.foldLeft(BigInt(0)) { case (bits, (lane, i)) =>
      bits | (lane.peek().litValue << (32*i))
    }
  private def expectLanes(lanes: Vec[UInt], bits: BigInt): Unit =
    lanes.zipWithIndex.foreach { case (lane, i) =>
      lane.expect(((bits >> (32*i)) & BigInt("ffffffff", 16)).U)
    }

  it should "deliver both distinct scales in one K64 tuple through canonical controllers and MTE" in {
    test(new ScaleReaderTop(exactK = 64, probeMte = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation) else Seq.empty) { d =>
      d.io.start.poke(false.B); d.io.mode.poke(10.U)
      d.io.go.poke(false.B); d.io.memReady.poke(true.B)
      d.clock.step(2); d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
      var received = 0
      var cycles = 0
      var stalled = false
      var held = Option.empty[(BigInt, Seq[BigInt])]
      while (received < 4 && cycles < 18) {
        // First observe and hold a real valid result for one clock, then
        // drain. A periodic ready window can miss every potential stall.
        val ready = stalled
        d.io.go.poke(ready.B)
        val valid = d.io.resultValid.peek().litToBoolean
        held.foreach { case (bits, flags) =>
          assert(valid, "held tuple lost valid before consumption")
          d.io.result.expect(bits.U)
          flags.zipWithIndex.foreach { case (f, i) => d.io.resultFlags(i).expect(f.U) }
        }
        held = if (valid && !ready) Some((d.io.result.peek().litValue,
          d.io.resultFlags.map(_.peek().litValue).toSeq)) else None
        if (valid) {
          // Row/column-specific scales: results800,512,qNaN,304.
          d.io.result.expect(BigInt("439800007fc000004400000044480000", 16).U)
          for (i <- 0 until 4) d.io.resultFlags(i).expect((if (i == 2) 16 else 0).U)
          println(s"K64-TUPLE cycle=$cycles ready=$ready received=$received row/column/bits/NV PASS")
          if (ready) received += 1 else stalled = true
        }
        d.clock.step(); cycles += 1
      }
      assert(received == 4 && stalled)
      println(s"K64-TUPLE PASS4 tuples,2 distinct scales/side,row-column/NV attribution,4 functional PEs cycles=$cycles")
    }
  }

  for (k <- Seq(32, 33, 64, 65)) {
    it should s"retain exact K$k lane counts with the synchronous scale response under stalls" in {
      test(new ScaleReaderTop(exactK = k)) { d =>
        d.io.start.poke(false.B); d.io.mode.poke(10.U)
        d.io.go.poke(false.B); d.io.memReady.poke(true.B)
        d.clock.step(2); d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        var accepted = 0
        var cycles = 0
        val vectors = (k + 63) / 64
        var held = Option.empty[(BigInt, BigInt, Seq[BigInt], Seq[BigInt])]
        while (accepted < 4 * vectors && cycles < 100) {
          val go = cycles % 7 >= 3
          d.io.go.poke(go.B); d.io.memReady.poke((cycles % 5 != 2).B)
          if (d.io.aValid.peek().litToBoolean && d.io.bValid.peek().litToBoolean) {
            val live = math.min(64, k - (accepted / 4) * 64)
            d.io.liveA.foreach(_.expect(live.U)); d.io.liveB.foreach(_.expect(live.U))
            val token = (d.io.a.peek().litValue, d.io.b.peek().litValue,
              d.io.liveA.map(_.peek().litValue).toSeq, d.io.liveB.map(_.peek().litValue).toSeq)
            held.foreach(h => assert(h == token, "held scale/live-K token changed"))
            held = if (go) None else Some(token)
            if (go) accepted += 1
          }
          d.clock.step(); cycles += 1
        }
        assert(accepted == 4 * vectors)
      }
    }
  }

  // Real CDC with a result producer and inert scratchpad inputs. This tests
  // flags plumbing/multi-K aggregation, not a second floating-point oracle.
  private class CdcFlagsProbe(val arraySize: Int = 2, val readTail: Boolean = false,
      val tensorSize: Int = 8, val scheduledReads: Boolean = false) extends Module {
    implicit val p: Parameters = ScaleTestParams.params.alterPartial {
      case CuteParamsKey => ScaleTestParams.params(CuteParamsKey).copy(
        Matrix_MN = arraySize, Tensor_MN = tensorSize)
    }
    val io = IO(new Bundle {
      val start = Input(Bool())
      val resultValid = Input(Bool())
      val flags = Input(Vec(arraySize*arraySize, UInt(5.W)))
      val endReady = Input(Bool())
      val configReady = Output(Bool())
      val resultReady = Output(Bool())
      val endValid = Output(Bool())
      val completionFlags = Output(UInt(5.W))
      val read = Option.when(readTail)(new Bundle {
        val rows = Input(UInt(9.W))
        val columns = Input(UInt(9.W))
        val activeRows = Input(UInt(9.W))
        val activeColumns = Input(UInt(9.W))
        val vectors = Input(UInt(9.W))
        val go = Input(Bool())
        val initValid = Input(Bool())
        val initAddr = Input(UInt(16.W))
        val resultBits = Input(Vec(arraySize*arraySize, UInt(32.W)))
        val minitClear = Input(Bool())
        val request = Output(Bool())
        val requestAddr = Output(UInt(16.W))
        val responseValid = Output(Bool())
        val cValid = Output(Bool())
        val cBits = Output(Vec(arraySize*arraySize, UInt(32.W)))
        val writeBits = Output(Vec(arraySize*arraySize, UInt(32.W)))
        val writeAddr = Output(UInt(16.W))
        val writeValid = Output(Bool())
        val readPermit = Output(Bool())
        val evenReads = Output(UInt(8.W))
        val oddReads = Output(UInt(8.W))
        val holdValid = Output(Bool())
        val idle = Output(Bool())
        val minitReady = Output(Bool())
        val minitAck = Output(Bool())
      })
    })
    val cdc = Module(new CDataController)
    cdc.io.DebugInfo.DebugTimeStampe := 0.U
    cdc.io.ConfigInfo.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
    cdc.io.ConfigInfo.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32
    cdc.io.ConfigInfo.MatrixRegTensor_M := (if (readTail) io.read.get.rows else 4.U)
    cdc.io.ConfigInfo.MatrixRegTensor_N := (if (readTail) io.read.get.columns else tensorSize.U)
    cdc.io.ConfigInfo.MatrixRegTensor_K := (if (readTail) io.read.get.vectors else 2.U)
    cdc.io.ConfigInfo.MatrixRegActive_M := (if (readTail) io.read.get.activeRows else 4.U)
    cdc.io.ConfigInfo.MatrixRegActive_N := (if (readTail) io.read.get.activeColumns else tensorSize.U)
    cdc.io.ConfigInfo.MatrixRegId := 0.U
    cdc.io.ConfigInfo.Is_Transpose := false.B
    cdc.io.ConfigInfo.Is_AfterOps_Tile := false.B
    cdc.io.ConfigInfo.Is_Reorder_Only_Ops := false.B
    cdc.io.ConfigInfo.Is_EasyScale_Only_Ops := false.B
    cdc.io.ConfigInfo.Is_VecFIFO_Ops := false.B
    cdc.io.ConfigInfo.MicroTaskValid := io.start
    cdc.io.ConfigInfo.MicroTaskEndReady := io.endReady
    cdc.io.ConfigInfo.MicroTask_TEComputeEndReady := true.B
    cdc.io.ComputeGo := (if (readTail) io.read.get.go else false.B)
    cdc.io.ReadIssueEnable := (if (scheduledReads) cdc.io.ReadIssueReady else true.B)
    cdc.io.Matrix_C.ready := (if (readTail) io.read.get.go else false.B)
    cdc.io.ResultMatrix_D.valid := io.resultValid
    cdc.io.ResultMatrix_D.bits := (if (readTail) io.read.get.resultBits.asUInt else 0.U)
    cdc.io.ResultFlags_D := io.flags
    if (readTail) {
      val r = io.read.get
      // G1 uses a logical one-entry CDC backed by a larger real scratchpad.
      // Keep the physical address/parity paths nondegenerate; only capacity
      // differs, never the one-cycle response or even/odd arbitration.
      val backingParams = p.alterPartial {
        case CuteParamsKey => p(CuteParamsKey).copy(
          Tensor_MN = math.max(tensorSize, 2*arraySize))
      }
      val mem = Module(new CMatrixReg(0)(backingParams))
      mem.io.MatrixRegIO.FromDataController <> cdc.io.FromMatrixRegIO
      val loader = mem.io.MatrixRegIO.FromMemoryLoader
      loader.LoadReadWriteRequest := Mux(r.initValid,
        MatrixRegTaskType.EnableWriteFromMemoryLoader, 0.U)
      loader.StoreReadWriteRequest := 0.U
      for (bank <- 0 until arraySize) {
        loader.ReadRequestToMatrixReg.BankAddr(bank).valid := false.B
        loader.ReadRequestToMatrixReg.BankAddr(bank).bits := 0.U
        loader.WriteRequestToMatrixReg.BankAddr(bank).valid := r.initValid
        loader.WriteRequestToMatrixReg.BankAddr(bank).bits := r.initAddr
        loader.WriteRequestToMatrixReg.Data(bank).valid := r.initValid
        loader.WriteRequestToMatrixReg.Data(bank).bits := VecInit(
          (0 until arraySize).map(lane =>
            ((r.initAddr << 16) | (bank*256 + lane).U).pad(32))).asUInt
        loader.WriteRequestToMatrixReg.ByteMask(bank).valid := r.initValid
        loader.WriteRequestToMatrixReg.ByteMask(bank).bits := Fill(arraySize*4, 1.U(1.W))
      }
      r.request := cdc.io.FromMatrixRegIO.ReadBankAddr.map(_.valid).reduce(_ && _)
      r.requestAddr := cdc.io.FromMatrixRegIO.ReadBankAddr.head.bits
      r.responseValid := cdc.io.FromMatrixRegIO.ReadResponseData.map(_.valid).reduce(_ && _)
      r.cValid := cdc.io.Matrix_C.valid
      r.cBits := cdc.io.Matrix_C.bits.asTypeOf(r.cBits)
      r.writeBits := VecInit(cdc.io.FromMatrixRegIO.WriteRequestData.map(_.bits))
        .asUInt.asTypeOf(r.writeBits)
      r.writeAddr := cdc.io.FromMatrixRegIO.WriteBankAddr.head.bits
      r.writeValid := cdc.io.FromMatrixRegIO.WriteBankAddr.map(_.valid).reduce(_ && _)
      r.readPermit := cdc.io.ReadIssueReady
      r.holdValid := chiseltest.experimental.expose(cdc.ReadMatrixRegDataHoldValid)
      r.idle := cdc.io.idle
      // Count accepted real SRAMTemplate requests, not just controller intent.
      r.evenReads := PopCount(mem.sram_banks.zipWithIndex.map { case ((even, _), bank) =>
        chiseltest.experimental.expose(even.io.r.req.valid).suggestName(s"evenReadValid_$bank") &&
          chiseltest.experimental.expose(even.io.r.req.ready).suggestName(s"evenReadReady_$bank")
      })
      r.oddReads := PopCount(mem.sram_banks.zipWithIndex.map { case ((_, odd), bank) =>
        chiseltest.experimental.expose(odd.io.r.req.valid).suggestName(s"oddReadValid_$bank") &&
          chiseltest.experimental.expose(odd.io.r.req.ready).suggestName(s"oddReadReady_$bank")
      })
      // Reuse the real TaskController reset/drain seam through its existing
      // harness. No instruction is inserted into this independent scheduler;
      // the CDC task is driven by this fixture, and only downstreamIdle gates
      // the MINIT clear/next-cycle acknowledgement under observation.
      if (!scheduledReads) {
        val minit = Module(new ScaleSchedulerTop(32))
        minit.io.start := false.B
        minit.io.op := 0.U
        minit.io.payload := 0.U
        minit.io.done := 0.U
        minit.io.loaderDone := 0.U
        minit.io.completionFlags := 0.U
        minit.io.traceRetireId := 0.U
        minit.io.traceEpoch := 0.U
        minit.io.returnReady := true.B
        minit.io.downstreamIdle := cdc.io.idle
        minit.io.minitClear := r.minitClear
        r.minitReady := minit.io.resetReady
        r.minitAck := minit.io.minitClearAck
      } else {
        // The new address/RAW/parity test does not exercise MINIT. Keep that
        // existing integration fixture only in the tests which observe it.
        r.minitReady := false.B
        r.minitAck := false.B
        chisel3.assert(!r.minitClear, "CDC address-only fixture cannot claim MINIT")
      }
    } else {
      // Preserve the original forced-invalid flags-only control.
      cdc.io.FromMatrixRegIO.ReadResponseData.foreach { bank =>
        bank.valid := false.B
        bank.bits := 0.U
      }
      cdc.io.FromMatrixRegIO.ReadWriteResponse := 0.U
    }
    io.configReady := cdc.io.ConfigInfo.MicroTaskReady
    io.resultReady := cdc.io.ResultMatrix_D.ready
    io.endValid := cdc.io.ConfigInfo.MicroTaskEndValid
    io.completionFlags := cdc.io.ConfigInfo.MicroTaskFlags
  }

  for {
    (array, tile, rows) <- Seq((2, 2, 2), (2, 4, 4), (8, 128, 8))
    variants <- {
      val cases = for (vectors <- Seq(1, 2); stall <- Seq(false, true)) yield (vectors, stall)
      if (array == 8) cases.map(c => Seq(c)) else Seq(cases)
    }
  } {
    val variantName = if (array == 8) s" V${variants.head._1} stall${variants.head._2}" else ""
    it should s"CDC tail exhaust real C SRAM and MINIT at array$array tile$tile$variantName" in {
      test(new CdcFlagsProbe(array, readTail = true, tensorSize = tile)) { d =>
        val r = d.io.read.get
        d.io.start.poke(false.B); d.io.resultValid.poke(false.B)
        d.io.endReady.poke(false.B); d.io.flags.foreach(_.poke(0.U))
        r.rows.poke(rows.U); r.columns.poke(tile.U)
        r.activeRows.poke(rows.U); r.activeColumns.poke(tile.U)
        r.vectors.poke(1.U); r.go.poke(false.B)
        r.initValid.poke(false.B); r.initAddr.poke(0.U)
        pokeLanes(r.resultBits, 0); r.minitClear.poke(false.B)
        d.clock.step(3)
        val groups = (rows/array)*(tile/array)
        val failures = scala.collection.mutable.ArrayBuffer.empty[String]
        def word(addr: Int): BigInt = (0 until array*array).foldLeft(BigInt(0)) { (v, i) =>
          v | (BigInt((addr << 16) | ((i/array) << 8) | (i%array)) << (32*i))
        }
        for ((vectors, stall) <- variants) {
          val label = s"array=$array G=$groups V=$vectors stall=$stall"
          val total = groups*vectors
          // Initialize the real even/odd memory before this independently
          // driven task. No loader reads, concurrent writes, or SRAM model.
          for (addr <- 0 until groups) {
            r.initValid.poke(true.B); r.initAddr.poke(addr.U); d.clock.step()
          }
          r.initValid.poke(false.B); r.vectors.poke(vectors.U)
          d.io.configReady.expect(true.B)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
          d.clock.step() // Canonical s_cal_init; no request issued yet.
          var requests = 0; var responses = 0; var cfire = 0; var dfire = 0
          var evenReads = 0; var oddReads = 0; var holdCycles = 0
          var cOnly = 0; var idleCycles = 0; var endCycles = 0
          var completions = 0; var minitAcks = 0; var cycles = 0
          var held = Option.empty[BigInt]
          def tick(): Unit = {
            val go = r.go.peek().litToBoolean
            val cv = r.cValid.peek().litToBoolean
            held.foreach { bits => assert(cv, label); expectLanes(r.cBits, bits) }
            held = if (cv && !go) Some(peekLanes(r.cBits)) else None
            if (r.request.peek().litToBoolean) {
              r.requestAddr.expect((requests%groups).U); requests += 1
            }
            if (r.responseValid.peek().litToBoolean) responses += 1
            if (cv && cfire >= total) cOnly += 1
            if (cv && go) {
              expectLanes(r.cBits, word(cfire%groups)); cfire += 1
            }
            if (d.io.resultValid.peek().litToBoolean && d.io.resultReady.peek().litToBoolean) {
              expectLanes(r.writeBits, peekLanes(r.resultBits)); dfire += 1
            }
            evenReads += r.evenReads.peek().litValue.toInt
            oddReads += r.oddReads.peek().litValue.toInt
            if (r.holdValid.peek().litToBoolean) holdCycles += 1
            if (r.idle.peek().litToBoolean) idleCycles += 1
            if (d.io.endValid.peek().litToBoolean) {
              endCycles += 1
              if (d.io.endReady.peek().litToBoolean) completions += 1
            }
            if (r.minitAck.peek().litToBoolean) minitAcks += 1
            d.clock.step(); cycles += 1
          }
          while (cfire < total && cycles < total + 16) {
            // Prime once, then stall the real first response for three clocks.
            r.go.poke((!stall || cycles < 1 || cycles >= 4).B)
            tick()
          }
          assert(cfire == total, s"$label Cfire=$cfire/$total")
          // A/B/scale producers have exhausted their exact request budgets.
          // A terminal extra C response now forms a C-only tuple, not a fire.
          r.go.poke(false.B)
          for (_ <- 0 until 4) tick()
          val terminalHold = r.holdValid.peek().litToBoolean
          held = None // Matrix_C is not an output in the subsequent end state.
          var expectedFlags = 0
          for (token <- 0 until total) {
            val bit = 1 << (token%5)
            expectedFlags |= bit
            d.io.flags.foreach(_.poke(0.U))
            d.io.flags(token%(array*array)).poke(bit.U)
            pokeLanes(r.resultBits, word(token%groups))
            d.io.resultValid.poke(true.B)
            d.io.resultReady.expect(true.B)
            tick()
            held = None
          }
          d.io.resultValid.poke(false.B); d.io.flags.foreach(_.poke(31.U))
          d.io.endValid.expect(true.B)
          for (_ <- 0 until 3) {
            d.io.completionFlags.expect(expectedFlags.U); tick(); held = None
          }
          d.io.endReady.poke(true.B); tick(); held = None
          d.io.endReady.poke(false.B)
          for (_ <- 0 until 4) { tick(); held = None }
          val finalIdle = r.idle.peek().litToBoolean
          val minitReady = r.minitReady.peek().litToBoolean
          if (minitReady) {
            r.minitClear.poke(true.B); tick(); held = None
            r.minitClear.poke(false.B); r.minitAck.expect(true.B)
            tick(); held = None
            r.minitAck.expect(false.B)
          } else {
            // Never force a MINIT clear past a real outstanding hold.
            r.minitAck.expect(false.B)
          }
          println(s"CDC-TAIL $label req=$requests rawRespValid=$responses " +
            s"Cfire=$cfire Dfire=$dfire evenSramFire=$evenReads oddSramFire=$oddReads " +
            s"holdCycles=$holdCycles terminalHold=$terminalHold COnly=$cOnly " +
            s"idleCycles=$idleCycles idle=$finalIdle endCycles=$endCycles " +
            s"completion=$completions minitReady=$minitReady minitAck=$minitAcks " +
            s"flags=$expectedFlags cycles=$cycles")
          assert(cfire == total && dfire == total && completions == 1, label)
          assert(responses == requests && evenReads + oddReads == requests*array, label)
          assert(evenReads > 0 && (groups == 1 || oddReads > 0), label)
          assert(!stall || holdCycles > 0, s"$label did not exercise a real response hold")
          if (requests != total || terminalHold || !finalIdle || !minitReady || minitAcks != 1)
            failures += s"$label req=$requests expected=$total terminalHold=$terminalHold " +
              s"idle=$finalIdle minitReady=$minitReady minitAck=$minitAcks"
        }
        assert(failures.isEmpty, failures.mkString("\n"))
      }
    }
  }

  it should "CDC partial N retain physical pitch and serialize C RAW across K vectors" in {
    test(new CdcFlagsProbe(4, readTail = true, tensorSize = 128, scheduledReads = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation,
            chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) else Seq.empty) { d =>
      val r = d.io.read.get
      d.io.start.poke(false.B); d.io.resultValid.poke(false.B)
      d.io.endReady.poke(false.B); d.io.flags.foreach(_.poke(0.U))
      r.rows.poke(4.U); r.columns.poke(4.U)
      r.activeRows.poke(1.U); r.activeColumns.poke(1.U)
      r.vectors.poke(1.U); r.go.poke(false.B)
      r.initValid.poke(false.B); r.initAddr.poke(0.U)
      pokeLanes(r.resultBits, 0); r.minitClear.poke(false.B)
      d.clock.step(3)
      def word(addr: Int, increment: Int = 0): BigInt =
        (0 until 16).foldLeft(BigInt(0)) { (v, i) =>
          v | ((BigInt((addr << 16) | ((i/4) << 8) | (i%4)) + increment) << (32*i))
        }
      // Each uses the real banked SRAM and the production CDC grant. A
      // six-cycle result producer models latency only, not MX arithmetic.
      for {
        (activeM, activeN) <- Seq((1, 1), (5, 1), (5, 5), (5, 128))
        vectors <- Seq(1, 2)
        stall <- Seq(false, true)
      } {
        val mGroups = (activeM + 3)/4
        val nGroups = (activeN + 3)/4
        val addresses = for (m <- 0 until mGroups; n <- 0 until nGroups) yield m*32 + n
        val groups = addresses.size
        val total = groups*vectors
        val label = s"M$activeM/N$activeN/V$vectors/stall$stall"
        for (addr <- addresses) {
          r.initValid.poke(true.B); r.initAddr.poke(addr.U); d.clock.step()
        }
        r.initValid.poke(false.B)
        r.rows.poke((mGroups*4).U); r.columns.poke((nGroups*4).U)
        r.activeRows.poke(activeM.U); r.activeColumns.poke(activeN.U)
        r.vectors.poke(vectors.U)
        d.io.configReady.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        d.clock.step()
        val pending = scala.collection.mutable.Queue.empty[(Int, Int, BigInt)]
        var cycle = 0; var requests = 0; var consumed = 0; var written = 0
        while (written < total && cycle < total*20 + 80) {
          r.go.poke((!stall || cycle % 11 != 3).B)
          val due = pending.headOption.filter { case (when, _, _) =>
            when <= cycle && (!stall || cycle % 13 != 7)
          }
          d.io.resultValid.poke(due.nonEmpty.B)
          due.foreach { case (_, token, bits) =>
            pokeLanes(r.resultBits, bits)
            d.io.flags.foreach(_.poke(0.U))
            d.io.flags(0).poke(1.U)
            r.writeAddr.expect(addresses(token % groups).U)
          }
          if (r.request.peek().litToBoolean) {
            assert(requests < total, label)
            r.requestAddr.expect(addresses(requests % groups).U)
            requests += 1
          }
          if (r.cValid.peek().litToBoolean && r.go.peek().litToBoolean) {
            assert(consumed < total, label)
            expectLanes(r.cBits, word(addresses(consumed % groups), consumed/groups))
            pending.enqueue((cycle + 6, consumed, word(addresses(consumed % groups), consumed/groups + 1)))
            consumed += 1
          }
          if (due.nonEmpty && d.io.resultReady.peek().litToBoolean) {
            assert(r.writeValid.peek().litToBoolean, label)
            pending.dequeue(); written += 1
          }
          d.clock.step(); cycle += 1
        }
        assert(requests == total && consumed == total && written == total && pending.isEmpty, label)
        d.io.resultValid.poke(false.B); r.go.poke(false.B)
        d.io.endValid.expect(true.B); d.io.completionFlags.expect(1.U)
        d.io.endReady.poke(true.B); d.clock.step(); d.io.endReady.poke(false.B)
        d.clock.step(3); r.idle.expect(true.B)
        println(s"CDC-PARTIAL $label addresses=${addresses.mkString(",")} req=$requests C=$consumed D=$written cycles=$cycle")
      }
    }
  }

  it should "MX padded output PEs bypass C bitwise without raising NaN or signed-zero flags" in {
    test(new MatrixScaleTupleTop(flagsProbe = true, paddingProbe = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation,
            chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) else Seq.empty) { d =>
      d.io.config.poke(true.B); d.io.validMask.poke(0.U); d.io.outReady.poke(true.B)
      d.clock.step(); d.io.config.poke(false.B)
      d.io.validMask.poke(31.U)
      var count = 0
      while (!d.io.ready.peek().litToBoolean && count < 30) { d.clock.step(); count += 1 }
      d.io.ready.expect(true.B); d.clock.step(); d.io.validMask.poke(0.U)
      while (!d.io.outValid.peek().litToBoolean && count < 60) { d.clock.step(); count += 1 }
      d.io.outValid.expect(true.B)
      val bits = peekLanes(d.io.result)
      // PE(0,0) has scale0/0 from the existing distributed-flags fixture:
      // its tiny positive dot may underflow, but inactive PEs never evaluate.
      for (i <- 1 until 16) {
        val expected = Seq("80000000", "7f800001", "7fc12345")(i % 3)
        assert(((bits >> (32*i)) & BigInt("ffffffff",16)) == BigInt(expected,16))
        d.io.flags(i).expect(0.U)
      }
    }
  }

  it should "DFlags carry distributed exceptions from all sixteen real PEs atomically through D stall" in {
    test(new MatrixScaleTupleTop(flagsProbe = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation,
            chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) else Seq.empty) { d =>
      d.io.config.poke(true.B); d.io.validMask.poke(0.U); d.io.outReady.poke(false.B)
      d.clock.step()
      d.io.config.poke(false.B); d.io.validMask.poke(31.U)
      var waited = 0
      while (!d.io.ready.peek().litToBoolean && waited < 30) { d.clock.step(); waited += 1 }
      d.io.ready.expect(true.B)
      d.clock.step()
      d.io.validMask.poke(0.U)
      waited = 0
      while (!d.io.outValid.peek().litToBoolean && waited < 30) { d.clock.step(); waited += 1 }
      d.io.outValid.expect(true.B)
      d.io.flags(0).expect(3.U) // UF|NX
      d.io.flags(5).expect(1.U) // NX
      d.io.flags(10).expect(5.U) // OF|NX
      d.io.flags(15).expect(16.U) // NV
      val flags = d.io.flags.map(_.peek().litValue)
      assert(flags.reduce(_ | _) == 0x17)
      val result = peekLanes(d.io.result)
      assert((result & 0xffffffffL) == 0)
      assert(((result >> (5*32)) & 0xffffffffL) == BigInt("4e000000", 16))
      assert(((result >> (10*32)) & 0xffffffffL) == BigInt("7f800000", 16))
      assert(((result >> (15*32)) & 0xffffffffL) == BigInt("7fc00000", 16))
      for (_ <- 0 until 5) {
        d.clock.step()
        d.io.outValid.expect(true.B)
        expectLanes(d.io.result, result)
        flags.zipWithIndex.foreach { case (f, i) => d.io.flags(i).expect(f.U) }
      }
      d.io.outReady.poke(true.B); d.clock.step()
      d.io.outValid.expect(false.B)
    }
  }

  it should "DFlags CDC accumulate all output and K tokens, hold End, and clear only on the next task" in {
    test(new CdcFlagsProbe) { d =>
      d.io.start.poke(false.B); d.io.resultValid.poke(false.B); d.io.endReady.poke(false.B)
      d.io.flags.foreach(_.poke(0.U))
      d.clock.step(2)
      for (expected <- Seq(0x1f, 0x04, 0)) {
        d.io.configReady.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        d.clock.step(2)
        for (token <- 0 until 16) {
          // No fire during gaps: even nonzero flags must not contaminate OR.
          d.io.flags.foreach(_.poke(31.U))
          d.io.resultValid.poke(false.B); d.clock.step()
          val values = if (expected == 0x1f && token == 0) Seq(1, 0, 0, 0)
            else if (expected == 0x1f && token == 7) Seq(0, 2, 0, 0)
            else if (expected == 0x1f && token == 8) Seq(0, 0, 4, 0)
            else if (expected == 0x1f && token == 15) Seq(0, 0, 0, 0x18)
            else if (expected == 4 && token == 3) Seq(0, 0, 4, 0)
            else Seq(0, 0, 0, 0)
          values.zipWithIndex.foreach { case (f, i) => d.io.flags(i).poke(f.U) }
          d.io.resultValid.poke(true.B)
          d.io.resultReady.expect(true.B)
          d.clock.step()
        }
        d.io.resultValid.poke(false.B)
        d.io.endValid.expect(true.B)
        for (_ <- 0 until 4) {
          d.io.completionFlags.expect(expected.U)
          d.clock.step()
          d.io.endValid.expect(true.B)
        }
        d.io.endReady.poke(true.B); d.clock.step(); d.io.endReady.poke(false.B)
        d.clock.step()
      }
    }
  }

  it should "DFlags sixteen-lane CDC plumbing OR every native PE sideband without rerunning FPE arithmetic" in {
    test(new CdcFlagsProbe(arraySize = 4)) { d =>
      d.io.start.poke(false.B); d.io.resultValid.poke(false.B); d.io.endReady.poke(false.B)
      d.io.flags.foreach(_.poke(0.U))
      d.clock.step(2)
      d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B); d.clock.step(2)
      // Four D tokens = (4/4)*(8/4)*K2. Walk a single set flag through each
      // PE lane on separate tasks so an accidentally omitted lane cannot hide
      // behind another lane carrying the same bit. This is native CDC plumbing,
      // combined with full-top wiring, not a full16-FPE runtime proof.
      for (lane <- 0 until 16) {
        if (lane != 0) {
          d.io.configReady.expect(true.B)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B); d.clock.step(2)
        }
        d.io.flags.foreach(_.poke(0.U))
        d.io.flags(lane).poke((1 << (lane % 5)).U)
        d.io.resultValid.poke(true.B); d.io.resultReady.expect(true.B); d.clock.step()
        d.io.flags.foreach(_.poke(0.U))
        d.clock.step(3)
        d.io.resultValid.poke(false.B)
        d.io.endValid.expect(true.B)
        d.io.completionFlags.expect((1 << (lane % 5)).U)
        d.clock.step(2)
        d.io.completionFlags.expect((1 << (lane % 5)).U)
        d.io.endReady.poke(true.B); d.clock.step(); d.io.endReady.poke(false.B)
        d.clock.step()
      }
    }
  }

  it should "MINIT keep LocalMMU allocated work busy through cross-channel held response" in {
    test(new LocalMMU()(baseConfig(6) ++ ScaleTestParams.params)) { d =>
      val locals = Seq(d.io.ALocalMMUIO, d.io.BLocalMMUIO, d.io.CLoadLocalMMUIO,
        d.io.CStoreLocalMMUIO, d.io.ASLocalMMUIO, d.io.BSLocalMMUIO)
      for (local <- locals; i <- local.Request.indices) {
        local.Request(i).valid.poke(false.B)
        val r = local.Request(i).bits
        r.RequestAddr.poke(0x80.U); r.RequestSourceID.poke(0.U)
        r.RequestData.poke(0.U); r.RequestMask.poke(((BigInt(1) << 64) - 1).U)
        r.RequestType_isWrite.poke(false.B); r.RequestConherent.poke(true.B)
        r.UseAllocatedSourceID.poke(true.B); r.isA.poke(false.B); r.MatrixIsAcc.poke(false.B)
        local.Response(i).ready.poke(false.B)
      }
      d.io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid.poke(true.B)
      d.io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits.poke(0.U)
      d.io.LastLevelCacheTLIO.nonConherentRequsetSourceID.valid.poke(false.B)
      d.io.LastLevelCacheTLIO.nonConherentRequsetSourceID.bits.poke(0.U)
      for (i <- d.io.LastLevelCacheTLIO.Request.indices) {
        d.io.LastLevelCacheTLIO.Request(i).ready.poke(false.B)
        d.io.LastLevelCacheTLIO.Response(i).valid.poke(false.B)
        d.io.LastLevelCacheTLIO.Response(i).bits.ReseponseData.poke(0.U)
        d.io.LastLevelCacheTLIO.Response(i).bits.ReseponseSourceID.poke(0.U)
        d.io.LastLevelCacheTLIO.Response(i).bits.ReseponseConherent.poke(true.B)
      }
      d.clock.step(2); d.io.idle.expect(true.B)
      d.io.ASLocalMMUIO.Request(0).valid.poke(true.B)
      d.io.idle.expect(false.B); d.clock.step(3)
      d.io.LastLevelCacheTLIO.Request(0).ready.poke(true.B)
      val encoded = d.io.LastLevelCacheTLIO.Request(0).bits.RequestSourceID.peek().litValue
      d.clock.step(); d.io.ASLocalMMUIO.Request(0).valid.poke(false.B)
      d.clock.step(3); d.io.idle.expect(false.B)
      d.io.LastLevelCacheTLIO.Response(1).valid.poke(true.B)
      d.io.LastLevelCacheTLIO.Response(1).bits.ReseponseSourceID.poke(encoded.U)
      d.io.ASLocalMMUIO.Response(1).valid.expect(true.B)
      d.io.idle.expect(false.B); d.clock.step(3)
      d.io.ASLocalMMUIO.Response(1).ready.poke(true.B)
      d.clock.step(); d.io.LastLevelCacheTLIO.Response(1).valid.poke(false.B)
      d.io.idle.expect(true.B)
    }
  }

  it should "MINIT report scale SRAM response hold busy until real read end" in {
    test(new ScaleReaderTop) { d =>
      d.io.start.poke(false.B); d.io.go.poke(false.B)
      d.io.memReady.poke(true.B); d.io.mode.poke(10.U)
      d.clock.step(2)
      d.io.idle.expect(true.B)
      d.io.start.poke(true.B)
      d.io.idle.expect(false.B)
      d.clock.step(); d.io.start.poke(false.B); d.clock.step(2)
      d.io.go.poke(true.B); d.clock.step(); d.io.go.poke(false.B)
      d.io.aValid.expect(true.B); d.io.bValid.expect(true.B)
      d.clock.step(4)
      d.io.idle.expect(false.B)
      d.io.aValid.expect(true.B); d.io.bValid.expect(true.B)
      var accepted = 0
      var cycles = 0
      d.io.go.poke(true.B)
      while (accepted < 8 && cycles < 60) {
        if (d.io.aValid.peek().litToBoolean) accepted += 1
        d.clock.step(); cycles += 1
      }
      assert(accepted == 8, s"scale hold lost token: $accepted/8")
      d.io.go.poke(false.B)
      d.io.end.expect(true.B)
      d.io.idle.expect(false.B)
      d.clock.step(2)
      d.io.idle.expect(true.B)
    }
  }

  it should "MINIT include partial input and every stalled D token in MatrixTE idle" in {
    test(new MatrixScaleTupleTop) { d =>
      d.io.config.poke(true.B); d.io.validMask.poke(0.U); d.io.outReady.poke(false.B)
      d.clock.step(); d.io.config.poke(false.B)
      d.io.idle.expect(true.B)
      d.io.validMask.poke(7.U)
      d.io.idle.expect(false.B); d.clock.step(2)
      d.io.validMask.poke(31.U); d.io.ready.expect(true.B)
      d.clock.step(); d.io.validMask.poke(0.U)
      d.io.idle.expect(false.B)
      d.clock.step(10)
      d.io.outValid.expect(true.B); d.io.idle.expect(false.B)
      d.clock.step(3)
      d.io.outValid.expect(true.B); d.io.idle.expect(false.B)
      d.io.outReady.poke(true.B); d.clock.step()
      d.io.outValid.expect(false.B); d.io.idle.expect(true.B)
    }
  }

  it should "hold synchronous scale responses and their K metadata through stalls" in {
    test(new ScaleReaderTop) { d =>
      d.io.start.poke(false.B); d.io.go.poke(false.B); d.io.memReady.poke(false.B)
      for (mode <- Seq(10, 7, 8)) {
        d.io.mode.poke(mode.U)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        d.clock.step(2)
        d.io.bankA.expect(1.U); d.io.bankB.expect(0.U)
        var accepted = 0
        var cycles = 0
        val bytes = if (mode == 10) 2 else 1 // ReduceWidthByte=32.
        def expected(group: Int, k: Int): BigInt = (0 until 2).foldLeft(BigInt(0)) { (v, lane) =>
          val offset = ((group * 2 + lane) * 2 + k) * bytes
          val data = (0 until 4).foldLeft(BigInt(0)) { (w, b) =>
            w | (BigInt(if (b < bytes) offset + b + 1 else 127) << (8 * b))
          }
          v | (data << (lane * 32))
        }
        while (accepted < 8 && cycles < 100) {
          val go = cycles % 5 != 2 && cycles % 5 != 3
          d.io.go.poke(go.B)
          d.io.memReady.poke((cycles % 7 != 0).B)
          if (go && d.io.aValid.peek().litToBoolean) {
            assert(d.io.bValid.peek().litToBoolean)
            val k = accepted / 4
            val m = (accepted % 4) / 2
            val n = accepted % 2
            d.io.a.expect(expected(m, k).U)
            d.io.b.expect(expected(n, k).U)
            accepted += 1
          }
          d.clock.step(); cycles += 1
        }
        assert(accepted == 8, s"mode=$mode consumed=$accepted/8")
        d.io.go.poke(false.B)
        d.io.end.expect(true.B)
        d.clock.step(2)
      }
    }
  }

  for (reduceBytes <- Seq(32, 64)) {
    it should s"extract only live E8M0 bytes for rows3 and65 K33 at ReduceWidthByte=$reduceBytes" in {
      test(new ScaleReaderTop(reduceBytes, partial = true)) { d =>
        d.io.start.poke(false.B); d.io.go.poke(false.B); d.io.memReady.poke(true.B)
        for (mode <- Seq(10, 7, 8)) {
          val perVector = reduceBytes / (if (mode == 10) 16 else 32)
          val kVectors = (2 + perVector - 1) / perVector
          val total = 2 * 33 * kVectors
          d.io.mode.poke(mode.U); d.io.start.poke(true.B)
          d.clock.step(); d.io.start.poke(false.B); d.clock.step(2)
          var accepted = 0
          var cycles = 0
          def expected(group: Int, k: Int, rows: Int): BigInt = (0 until 2).foldLeft(BigInt(0)) { (v, lane) =>
            val row = group * 2 + lane
            val bytes = (0 until reduceBytes / 8).foldLeft(BigInt(0)) { (w, s) =>
              val block = k * perVector + s
              val value = if (row < rows && s < perVector && block < 2)
                ((row * 2 + block + 1) & 255) else 127
              w | (BigInt(value) << (s * 8))
            }
            v | (bytes << (lane * reduceBytes))
          }
          while (accepted < total && cycles < 400) {
            val go = cycles % 5 != 2
            d.io.go.poke(go.B)
            if (go && d.io.aValid.peek().litToBoolean) {
              d.io.bValid.expect(true.B)
              val k = accepted / 66
              val m = (accepted % 66) / 33
              val n = accepted % 33
              val first = k * perVector * 32
              val live = math.max(0, math.min(perVector * 32, 33 - first))
              for (i <- 0 until 2) {
                d.io.liveA(i).expect((if (m * 2 + i < 3) live else 0).U)
                d.io.liveB(i).expect((if (n * 2 + i < 65) live else 0).U)
              }
              d.io.a.expect(expected(m, k, 3).U)
              d.io.b.expect(expected(n, k, 65).U)
              accepted += 1
            }
            d.clock.step(); cycles += 1
          }
          assert(accepted == total, s"mode=$mode consumed=$accepted/$total")
          d.io.go.poke(false.B); d.io.end.expect(true.B); d.clock.step(2)
        }
      }
    }
  }

  it should "prime empty SRAM pipelines but never consume partial data-scale tuples" in {
    test(new MatrixScaleTupleTop) { d =>
      d.io.config.poke(true.B); d.io.validMask.poke(0.U); d.io.outReady.poke(true.B)
      d.clock.step(); d.io.config.poke(false.B)
      d.io.ready.expect(true.B) // Required first synchronous read.
      for (mask <- Seq(7, 15, 23, 24, 1)) {
        d.io.validMask.poke(mask.U)
        d.io.ready.expect(false.B)
        d.clock.step(2)
        d.io.outValid.expect(false.B)
      }
      var accepted = 0
      var emitted = 0
      var cycles = 0
      while ((accepted < 24 || emitted < 24) && cycles < 150) {
        val outReady = cycles % 9 >= 4
        d.io.outReady.poke(outReady.B)
        d.io.validMask.poke((if (accepted < 24) 31 else 0).U)
        if (accepted < 24 && d.io.ready.peek().litToBoolean) accepted += 1
        if (outReady && d.io.outValid.peek().litToBoolean) emitted += 1
        d.clock.step(); cycles += 1
      }
      assert(accepted == 24 && emitted == 24, s"tuples accepted=$accepted emitted=$emitted")
      d.io.validMask.poke(0.U); d.io.outReady.poke(true.B)
      d.clock.step(10); d.io.outValid.expect(false.B)
    }
  }
}

class ScaleLoaderTop(aSide: Boolean, val rows: Int = 32, val blocks: Int = 4,
                     val sourceBase: BigInt = 0x1000,
                     sourceStride: Option[BigInt] = None) extends Module {
  implicit val p: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_2Tops.copy(EnableDifftest = false, Tensor_MN = 128,
      MatrixExtension = MatrixIsaParams(enableMxfp4Fp32 = true))
  })
  val io = IO(new Bundle {
    val start = Input(Bool())
    val bank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val badSource = Input(Bool())
    val request = Decoupled(new MMURequestIO)
    val response = Flipped(Decoupled(new MMUResponseIO))
    val writeValid = Output(Bool())
    val writeAddr = Output(UInt(8.W))
    val writeData = Output(UInt(512.W))
    val selectedBank = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
    val ready = Output(Bool())
    val end = Output(Bool())
  })
  val a = Option.when(aSide)(Module(new AScaleLoader))
  val b = Option.when(!aSide)(Module(new BScaleLoader))
  val mmu = a.map(_.io.LocalMMUIO).getOrElse(b.get.io.LocalMMUIO)
  val matrix = a.map(_.io.ToMatrixRegIO).getOrElse(b.get.io.ToMatrixRegIO)
  val sourceId = RegInit(0.U(6.W))
  when(io.request.fire) { sourceId := sourceId + 1.U }
  mmu.ConherentRequsetSourceID.valid := true.B
  mmu.ConherentRequsetSourceID.bits := Mux(io.badSource, 64.U, sourceId)
  mmu.nonConherentRequsetSourceID.valid := false.B
  mmu.nonConherentRequsetSourceID.bits := 0.U
  mmu.Request(0) <> io.request
  mmu.Response(0) <> io.response
  for (i <- 1 until 4) {
    mmu.Request(i).ready := true.B
    mmu.Response(i).valid := false.B
    mmu.Response(i).bits := 0.U.asTypeOf(new MMUResponseIO)
  }
  a.foreach { l =>
    l.io.DebugInfo.DebugTimeStampe := 0.U
    val c = l.io.ConfigInfo
    c.ApplicationScale_A.ApplicationScale_A_BaseVaddr := sourceBase.U
    c.ApplicationScale_A.BlockScale_A_BaseVaddr := sourceBase.U
    c.ApplicationScale_A.computeType := MteComputeType.Mxfp4F32
    c.ApplicationScale_A.RowStrideBytes := sourceStride.getOrElse(BigInt(blocks)).U
    c.ApplicationScale_A.ScaleBlocksPerRow := blocks.U
    c.MatrixRegTensor_M := rows.U
    c.MatrixRegTensor_K := ((blocks + 1) / 2).U
    c.ScaleBankId := io.bank
    c.Conherent := true.B
    c.MicroTaskValid := io.start
    c.MicroTaskEndReady := true.B
  }
  b.foreach { l =>
    l.io.DebugInfo.DebugTimeStampe := 0.U
    val c = l.io.ConfigInfo
    c.ApplicationScale_B.ApplicationScale_B_BaseVaddr := sourceBase.U
    c.ApplicationScale_B.BlockScale_B_BaseVaddr := sourceBase.U
    c.ApplicationScale_B.computeType := MteComputeType.Mxfp4F32
    c.ApplicationScale_B.RowStrideBytes := sourceStride.getOrElse(BigInt(blocks)).U
    c.ApplicationScale_B.ScaleBlocksPerRow := blocks.U
    c.MatrixRegTensor_N := rows.U
    c.MatrixRegTensor_K := ((blocks + 1) / 2).U
    c.ScaleBankId := io.bank
    c.Conherent := true.B
    c.MicroTaskValid := io.start
    c.MicroTaskEndReady := true.B
  }
  io.ready := a.map(_.io.ConfigInfo.MicroTaskReady).getOrElse(b.get.io.ConfigInfo.MicroTaskReady)
  io.end := a.map(_.io.ConfigInfo.MicroTaskEndValid).getOrElse(b.get.io.ConfigInfo.MicroTaskEndValid)
  io.selectedBank := a.map(_.io.SpadId).getOrElse(b.get.io.SpadId)
  io.writeValid := matrix.BankAddr.valid && matrix.Data.valid
  io.writeAddr := matrix.BankAddr.bits
  io.writeData := matrix.Data.bits.asUInt
}

class ScaleLoaderSpec extends AnyFlatSpec with ChiselScalatestTester {
  for (aSide <- Seq(true, false)) {
    it should s"compact noncompact source scale rows on ${if (aSide) "A" else "B"}" in {
      // Shared source lines, a row that spans two lines plus skipped gap-only
      // lines, and a pitch with high bits in the existing MLS stride field.
      for ((base, stride, rows) <- Seq(
        (BigInt(0x1001), BigInt(5), 40),
        (BigInt(0x103f), BigInt(129), 33),
        (BigInt(0x200a), (BigInt(1) << 35) + 67, 2))) {
        val beatBytes = 64
        val blocks = 2
        val addresses = for (r <- 0 until rows; k <- 0 until blocks) yield base + stride * r + k
        val memory = addresses.zipWithIndex.map { case (a, i) => a -> (i + 1) }.toMap
        val expectedRequests = addresses.map(a => a / beatBytes * beatBytes).distinct
        val expectedWrites = (addresses.size + beatBytes - 1) / beatBytes
        def pack(byteAt: Int => Int): BigInt = (0 until beatBytes).foldLeft(BigInt(0)) { (v, i) =>
          v | (BigInt(byteAt(i)) << (8 * i))
        }
        test(new ScaleLoaderTop(aSide, rows, blocks, base, Some(stride))) { d =>
          d.io.start.poke(false.B); d.io.bank.poke(3.U); d.io.badSource.poke(false.B)
          d.io.request.ready.poke(false.B); d.io.response.valid.poke(false.B)
          d.io.response.bits.ReseponseData.poke(0.U)
          d.io.response.bits.ReseponseConherent.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(0.U)
          d.clock.step(2)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
          var pending = Option.empty[(BigInt, BigInt, Int)]
          var requests = 0
          var writes = 0
          var cycle = 0
          while (!d.io.end.peek().litToBoolean && cycle < 1000) {
            val ready = cycle % 3 != 1
            d.io.request.ready.poke(ready.B)
            val response = pending.filter(_._3 <= cycle)
            d.io.response.valid.poke(response.nonEmpty.B)
            response.foreach { case (address, id, _) =>
              d.io.response.bits.ReseponseSourceID.poke(id.U)
              d.io.response.bits.ReseponseData.poke(pack(i => memory.getOrElse(address + i, 255)).U)
              d.io.response.ready.expect(true.B)
              pending = None
            }
            if (ready && d.io.request.valid.peek().litToBoolean) {
              assert(pending.isEmpty && requests < expectedRequests.size)
              val address = expectedRequests(requests)
              d.io.request.bits.RequestAddr.expect(address.U)
              pending = Some((address, d.io.request.bits.RequestSourceID.peek().litValue, cycle + 3))
              requests += 1
            }
            if (d.io.writeValid.peek().litToBoolean) {
              assert(writes < expectedWrites)
              d.io.writeAddr.expect(writes.U)
              d.io.writeData.expect(pack { i =>
                val index = writes * beatBytes + i
                if (index < addresses.size) index + 1 else 127
              }.U)
              d.io.selectedBank.expect(3.U)
              writes += 1
            }
            d.clock.step(); cycle += 1
          }
          d.io.end.expect(true.B); d.io.writeValid.expect(false.B)
          assert(requests == expectedRequests.size && pending.isEmpty && writes == expectedWrites)
        }
      }
    }
    it should s"realign compact unaligned ${if (aSide) "A" else "B"} scales within one containing cacheline" in {
      for ((offset, rows, blocks) <- Seq((1, 3, 2), (54, 1, 2), (63, 1, 1),
                                       (62, 1, 2), (31, 2, 2), (32, 8, 4))) {
        test(new ScaleLoaderTop(aSide, rows, blocks, 0x1000 + offset)) { d =>
          d.io.start.poke(false.B); d.io.bank.poke(1.U); d.io.badSource.poke(false.B)
          d.io.request.ready.poke(false.B); d.io.response.valid.poke(false.B)
          d.io.response.bits.ReseponseData.poke(0.U)
          d.io.response.bits.ReseponseConherent.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(0.U)
          d.clock.step(2)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
          var cycles = 0
          while (!d.io.request.valid.peek().litToBoolean && cycles < 10) {
            d.clock.step(); cycles += 1
          }
          d.io.request.valid.expect(true.B)
          d.io.request.bits.RequestAddr.expect(0x1000.U)
          d.clock.step(3)
          d.io.request.bits.RequestAddr.expect(0x1000.U)
          val id = d.io.request.bits.RequestSourceID.peek().litValue
          d.io.request.ready.poke(true.B); d.clock.step()
          d.io.request.valid.expect(false.B)
          val live = rows * blocks
          val source = (0 until 64).foldLeft(BigInt(0)) { (v, i) =>
            val b = if (i >= offset && i < offset + live) i - offset + 1 else 255
            v | (BigInt(b) << (8 * i))
          }
          val expected = (0 until 64).foldLeft(BigInt(0)) { (v, i) =>
            v | (BigInt(if (i < live) i + 1 else 127) << (8 * i))
          }
          d.io.response.valid.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(id.U)
          d.io.response.bits.ReseponseData.poke(source.U)
          d.io.writeValid.expect(true.B); d.io.writeAddr.expect(0.U)
          d.io.writeData.expect(expected.U); d.io.selectedBank.expect(1.U)
          d.clock.step()
          d.io.response.valid.poke(false.B); d.clock.step()
          d.io.end.expect(true.B)
        }
      }
    }
    it should s"reject an unaligned ${if (aSide) "A" else "B"} scale rectangle crossing a cacheline" in {
      assertThrows[ChiselAssertionError] {
        test(new ScaleLoaderTop(aSide, rows = 1, blocks = 2, sourceBase = 0x103f)) { d =>
          d.io.start.poke(false.B); d.io.bank.poke(0.U); d.io.badSource.poke(false.B)
          d.io.request.ready.poke(true.B); d.io.response.valid.poke(false.B)
          d.io.response.bits.ReseponseData.poke(0.U)
          d.io.response.bits.ReseponseConherent.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(0.U)
          d.clock.step(2)
          d.io.start.poke(true.B); d.clock.step()
        }
      }
    }
    it should s"discard fetched padding after the ${if (aSide) 3 else 65} by2 live scale-byte rectangle" in {
      val rows = if (aSide) 3 else 65
      test(new ScaleLoaderTop(aSide, rows, blocks = 2)) { d =>
        d.io.start.poke(false.B); d.io.bank.poke(0.U); d.io.badSource.poke(false.B)
        d.io.request.ready.poke(true.B); d.io.response.valid.poke(false.B)
        d.io.response.bits.ReseponseData.poke(0.U)
        d.io.response.bits.ReseponseConherent.poke(true.B)
        d.io.response.bits.ReseponseSourceID.poke(0.U)
        d.clock.step(2)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        var waitCycles = 0
        while (!d.io.request.valid.peek().litToBoolean && waitCycles < 10) { d.clock.step(); waitCycles += 1 }
        val beats = (rows * 2 + 63) / 64
        val ids = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        for (beat <- 0 until beats) {
          d.io.request.valid.expect(true.B)
          d.io.request.bits.RequestAddr.expect((0x1000 + beat * 64).U)
          ids += d.io.request.bits.RequestSourceID.peek().litValue
          d.clock.step()
        }
        d.io.request.valid.expect(false.B)
        for (beat <- (0 until beats).reverse) {
          val source = (0 until 64).foldLeft(BigInt(0)) { (v, i) =>
            val offset = beat * 64 + i
            val b = if (offset < rows * 2) (offset + 1) & 255 else 255
            v | (BigInt(b) << (8 * i))
          }
          val expected = (0 until 64).foldLeft(BigInt(0)) { (v, i) =>
            val offset = beat * 64 + i
            val b = if (offset < rows * 2) (offset + 1) & 255 else 127
            v | (BigInt(b) << (8 * i))
          }
          d.io.response.valid.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(ids(beat).U)
          d.io.response.bits.ReseponseData.poke(source.U)
          d.io.writeValid.expect(true.B); d.io.writeAddr.expect(beat.U)
          d.io.writeData.expect(expected.U)
          d.clock.step()
        }
        d.io.response.valid.poke(false.B); d.clock.step()
        d.io.end.expect(true.B)
      }
    }
    it should s"route ${if (aSide) "A" else "B"} load responses by source ID and retain the assigned bank" in {
      test(new ScaleLoaderTop(aSide)) { d =>
        d.io.start.poke(false.B); d.io.bank.poke(1.U); d.io.badSource.poke(false.B)
        d.io.request.ready.poke(false.B); d.io.response.valid.poke(false.B)
        d.io.response.bits.ReseponseData.poke(0.U)
        d.io.response.bits.ReseponseConherent.poke(true.B)
        d.io.response.bits.ReseponseSourceID.poke(0.U)
        d.clock.step(2)
        d.io.ready.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        d.clock.step(2)
        d.io.selectedBank.expect(1.U)
        d.io.request.valid.expect(true.B)
        d.io.request.bits.RequestAddr.expect(0x1000.U)
        d.clock.step(3)
        d.io.request.bits.RequestAddr.expect(0x1000.U)
        val ids = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        d.io.request.ready.poke(true.B)
        for (beat <- 0 until 2) {
          d.io.request.valid.expect(true.B)
          d.io.request.bits.RequestAddr.expect((0x1000 + beat * 64).U)
          ids += d.io.request.bits.RequestSourceID.peek().litValue
          d.clock.step()
        }
        d.io.request.valid.expect(false.B)
        d.io.end.expect(false.B)
        for (beat <- Seq(1, 0)) {
          d.io.response.valid.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(ids(beat).U)
          val data = (BigInt(beat + 1) << 504) | (0x1234 + beat)
          d.io.response.bits.ReseponseData.poke(data.U)
          d.io.response.ready.expect(true.B)
          d.io.writeValid.expect(true.B)
          d.io.writeAddr.expect(beat.U)
          d.io.writeData.expect(data.U)
          d.io.selectedBank.expect(1.U)
          d.clock.step()
        }
        d.io.response.valid.poke(false.B)
        var cycles = 0
        while (!d.io.end.peek().litToBoolean && cycles < 12) { d.clock.step(); cycles += 1 }
        d.io.end.expect(true.B)
        d.clock.step(2)
        d.io.ready.expect(true.B)
        d.io.selectedBank.expect(1.U) // Completion must not toggle the bank.
      }
    }
    it should s"reject an out-of-range ${if (aSide) "A" else "B"} allocated source ID rather than alias CAM entry zero" in {
      assertThrows[ChiselAssertionError] {
        test(new ScaleLoaderTop(aSide)) { d =>
          d.io.start.poke(false.B); d.io.bank.poke(0.U); d.io.badSource.poke(true.B)
          d.io.request.ready.poke(true.B); d.io.response.valid.poke(false.B)
          d.io.response.bits.ReseponseData.poke(0.U)
          d.io.response.bits.ReseponseConherent.poke(true.B)
          d.io.response.bits.ReseponseSourceID.poke(0.U)
          d.clock.step(2)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
          d.clock.step(4)
        }
      }
    }
  }
}
