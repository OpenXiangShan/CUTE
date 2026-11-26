package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

object TransposeLoaderIntegrationTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP
  })
}

/**
  * Gives AML and BML the same small test-facing control plane while preserving
  * their production LocalMMU and MatrixReg ports.
  */
class TransposeLoaderHarness(isB: Boolean, normalLoads: Boolean = false,
    mxLoads: Boolean = false, concurrentScaleReads: Boolean = false)(implicit p: Parameters) extends CuteModule {
  private val configWidth = MatrixRegMaxTensorDimBitSize
  private val testWordBits = if (mxLoads) 32 else 64
  private val testAddrWidth = if (mxLoads) 32 else MMUAddrWidth
  private val testSourceWidth = if (mxLoads) LLCSourceMaxNumBitSize else 64
  if (mxLoads) {
    require(normalLoads && ExperimentalMxfpConvertLoad)
    require(Tensor_MN == 64 && ABMatrixRegNBanks == 4 &&
      ABMatrixRegEntryByteSize == 32 && ReduceGroupSize == 2 && outsideDataWidth == 512)
  }
  require(!ExperimentalMxfpDynamicQuant || mxLoads, "dynamic fixture reuses the real MX SRAM harness")
  require(!concurrentScaleReads || (mxLoads && !ExperimentalMxfpDynamicQuant))

  val io = IO(new Bundle {
    val start = Input(Bool())
    val dataType = Input(UInt(ElementDataType.DataTypeBitWidth.W))
    val sourceRows = Input(UInt(configWidth.W))
    val beatsPerRow = Input(UInt(configWidth.W))
    val base = Input(UInt(testAddrWidth.W))
    val stride = Input(UInt(testAddrWidth.W))
    val hasTail = Input(Bool())
    val tailBytes = Input(UInt(log2Ceil(outsideDataWidthByte + 1).W))
    val sourceId = Input(UInt(LLCSourceMaxNumBitSize.W))

    val taskReady = Output(Bool())
    val taskEnd = Output(Bool())
    // Keep only the observed request fields and split wide data into native
    // words, as in the existing cache/scale tests. Production ports stay intact.
    val request = Decoupled(new Bundle {
      val RequestAddr = UInt(testAddrWidth.W)
      val RequestSourceID = UInt(testSourceWidth.W)
    })
    val response = Flipped(Decoupled(new Bundle {
      val ReseponseData = Vec(MMUDataWidth / testWordBits, UInt(testWordBits.W))
      val ReseponseConherent = Bool()
      val ReseponseSourceID = UInt(testSourceWidth.W)
    }))
    val matrix = Output(new Bundle {
      val active = Bool()
      val BankAddr = Vec(ABMatrixRegNBanks, Valid(UInt(log2Ceil(ABMatrixRegBankNEntries).W)))
      val Data = Vec(ABMatrixRegNBanks,
        Valid(Vec(ABMatrixRegEntryBitSize / testWordBits, UInt(testWordBits.W))))
      val ByteMask = Vec(ABMatrixRegNBanks, Valid(UInt(ABMatrixRegEntryByteSize.W)))
    })
    val mx = Option.when(mxLoads)(new Bundle {
      val computeType = Input(UInt(MteComputeType.ComputeTypeBitWidth.W))
      val scaleBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val endReady = Input(Bool())
      val flags = Output(UInt(5.W))
      val selectedScaleBank = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val scaleActive = Output(Bool())
      val scaleRead = Output(Valid(UInt(log2Ceil(ABScaleBankNEntries).W)))
      val scaleWrite = Input(Bool())
      val scaleWriteBank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val scaleWriteAddr = Input(UInt(log2Ceil(ABScaleBankNEntries).W))
      val scaleWriteWords = Input(Vec(outsideDataWidth / 32, UInt(32.W)))
      val matrixInit = Input(Bool())
      val matrixInitAddr = Input(UInt(log2Ceil(ABMatrixRegBankNEntries).W))
      val matrixInitWords = Input(Vec(ABMatrixRegNBanks,
        Vec(ABMatrixRegEntryBitSize / 32, UInt(32.W))))
      val read = Flipped(Decoupled(UInt(log2Ceil(ABMatrixRegBankNEntries).W)))
      val data = Output(Valid(Vec(ABMatrixRegNBanks,
        Vec(ABMatrixRegEntryBitSize / 32, UInt(32.W)))))
    })
    val dynamic = Option.when(ExperimentalMxfpDynamicQuant)(new Bundle {
      val enable = Input(Bool())
      val stall = Input(Bool())
      val scaleEndReady = Input(Bool())
      val scaleTaskReady = Output(Bool())
      val scaleEnd = Output(Bool())
      val scaleMemoryRequest = Output(Bool())
      val scaleWrite = Output(Bool())
      val generatedPending = Output(Bool())
      val generated = Output(Valid(new MxfpGeneratedScale))
      val read = Flipped(Decoupled(UInt(log2Ceil(ABScaleBankNEntries).W)))
      val data = Output(Valid(Vec(outsideDataWidth / 32, UInt(32.W))))
    })
    val computeScale = Option.when(concurrentScaleReads)(new Bundle {
      val start = Input(Bool())
      val go = Input(Bool())
      val readEnable = Input(Bool())
      val bank = Input(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val rows = Input(UInt(configWidth.W))
      val endReady = Input(Bool())
      val taskReady = Output(Bool())
      val taskEnd = Output(Bool())
      val idle = Output(Bool())
      val selectedBank = Output(UInt(p(CuteParamsKey).ABScaleRegIdWidth.W))
      val valid = Output(Bool())
      val words = Output(Vec(ScaleWidth * Matrix_MN / 32, UInt(32.W)))
      val liveK = Output(Vec(Matrix_MN, UInt(log2Ceil(ReduceWidth / 4 + 1).W)))
      val requestFires = Output(UInt(2.W))
    })
  })

  private def connectPorts(mmu: LocalMMUIO, matrix: ABMemoryLoaderMatrixRegIO,
      scale: Option[ABScaleControlMatrixRegIO], scaleBank: UInt, scaleActive: Bool,
      generated: Option[DecoupledIO[MxfpGeneratedScale]]): Unit = {
    io.request.valid := mmu.Request(0).valid
    io.request.bits.RequestAddr := mmu.Request(0).bits.RequestAddr
    io.request.bits.RequestSourceID := mmu.Request(0).bits.RequestSourceID
    if (mxLoads) {
      when(mmu.Request(0).valid) {
        assert((mmu.Request(0).bits.RequestAddr >> testAddrWidth) === 0.U,
          "MX fixture address exceeded its observed 32-bit window")
        assert((mmu.Request(0).bits.RequestSourceID >> testSourceWidth) === 0.U,
          "MX fixture source ID exceeded its observed allocator width")
      }
    }
    mmu.Request(0).ready := io.request.ready
    mmu.Response(0).valid := io.response.valid
    mmu.Response(0).bits.ReseponseData := io.response.bits.ReseponseData.asUInt
    mmu.Response(0).bits.ReseponseConherent := io.response.bits.ReseponseConherent
    mmu.Response(0).bits.ReseponseSourceID := io.response.bits.ReseponseSourceID
    io.response.ready := mmu.Response(0).ready
    io.matrix.active := matrix.active
    io.matrix.BankAddr := matrix.BankAddr
    io.matrix.ByteMask := matrix.ByteMask
    for (bank <- 0 until ABMatrixRegNBanks) {
      io.matrix.Data(bank).valid := matrix.Data(bank).valid
      io.matrix.Data(bank).bits := matrix.Data(bank).bits.asTypeOf(io.matrix.Data(bank).bits)
    }
    if (mxLoads) {
      val m = io.mx.get
      val regs = Seq.fill(2)(Module(new ABScaleMatrixReg))
      val port = scale.get
      if (concurrentScaleReads) {
        val probe = io.computeScale.get
        def connectCompute(mem: ABScaleControlMatrixRegIO, values: DecoupledIO[UInt],
            bank: UInt, idle: Bool, ready: Bool, end: Bool, liveK: Vec[UInt]): Unit = {
          values.ready := probe.go
          probe.taskReady := ready
          probe.taskEnd := end
          probe.idle := idle
          probe.selectedBank := bank
          probe.valid := values.valid
          probe.words := values.bits.asTypeOf(probe.words)
          probe.liveK := liveK
          CUTEV2Top.connectScaleControlToRegs(
            bank, mem, regs, Some(port), scaleBank, scaleActive)(clock, reset)
          probe.requestFires := VecInit(Seq(mem.BankAddr.fire, port.BankAddr.fire)).asUInt
        }
        if (isB) {
          val c = Module(new BScaleController)
          c.io.DebugInfo.DebugTimeStampe := 0.U
          c.io.ConfigInfo.MatrixRegTensor_M := Matrix_MN.U
          c.io.ConfigInfo.MatrixRegTensor_N := probe.rows
          c.io.ConfigInfo.MatrixRegTensor_K := 1.U
          c.io.ConfigInfo.ScaleBlocksPerRow := 2.U
          c.io.ConfigInfo.KElements := 64.U
          c.io.ConfigInfo.MatrixRegId := 1.U
          c.io.ConfigInfo.ScaleBankId := probe.bank
          c.io.ConfigInfo.computeType := MteComputeType.Mxfp4F32
          c.io.ConfigInfo.Is_Transpose := false.B
          c.io.ConfigInfo.MicroTaskValid := probe.start
          c.io.ConfigInfo.MicroTaskEndReady := probe.endReady
          c.io.ComputeGo := probe.go
          c.io.ReadIssueEnable := probe.readEnable
          connectCompute(c.io.FromMatrixRegIO, c.io.ScaleB, c.io.SpadId, c.io.idle,
            c.io.ConfigInfo.MicroTaskReady, c.io.ConfigInfo.MicroTaskEndValid, c.io.LiveK)
        } else {
          val c = Module(new AScaleController)
          c.io.DebugInfo.DebugTimeStampe := 0.U
          c.io.ConfigInfo.MatrixRegTensor_M := probe.rows
          c.io.ConfigInfo.MatrixRegTensor_N := Matrix_MN.U
          c.io.ConfigInfo.MatrixRegTensor_K := 1.U
          c.io.ConfigInfo.ScaleBlocksPerRow := 2.U
          c.io.ConfigInfo.KElements := 64.U
          c.io.ConfigInfo.MatrixRegId := 0.U
          c.io.ConfigInfo.ScaleBankId := probe.bank
          c.io.ConfigInfo.computeType := MteComputeType.Mxfp4F32
          c.io.ConfigInfo.Is_Transpose := false.B
          c.io.ConfigInfo.MicroTaskValid := probe.start
          c.io.ConfigInfo.MicroTaskEndReady := probe.endReady
          c.io.ComputeGo := probe.go
          c.io.ReadIssueEnable := probe.readEnable
          connectCompute(c.io.FromMatrixRegIO, c.io.ScaleA, c.io.SpadId, c.io.idle,
            c.io.ConfigInfo.MicroTaskReady, c.io.ConfigInfo.MicroTaskEndValid, c.io.LiveK)
        }
      }
      val generatedWriter = if (ExperimentalMxfpDynamicQuant) {
        val dyn = io.dynamic.get
        // These are the original ASL/BSL, not a test-side replacement writer.
        val (writerMmu, writerPort, writerBank, sink, end, ready) = if (isB) {
          val w = Module(new BScaleLoader)
          val c = w.io.ConfigInfo
          c.ApplicationScale_B := 0.U.asTypeOf(c.ApplicationScale_B)
          c.ApplicationScale_B.computeType := MteComputeType.Mxfp4F32
          c.ApplicationScale_B.ScaleBlocksPerRow := 2.U
          c.MatrixRegTensor_N := 8.U
          c.MatrixRegTensor_K := 1.U
          c.ScaleBankId := m.scaleBank
          c.Generated.get := true.B
          c.Conherent := true.B
          c.MicroTaskValid := io.start && dyn.enable
          c.MicroTaskEndReady := dyn.scaleEndReady
          w.io.DebugInfo.DebugTimeStampe := 0.U
          (w.io.LocalMMUIO, w.io.ToMatrixRegIO, w.io.SpadId,
            w.io.GeneratedScale.get, c.MicroTaskEndValid, c.MicroTaskReady)
        } else {
          val w = Module(new AScaleLoader)
          val c = w.io.ConfigInfo
          c.ApplicationScale_A := 0.U.asTypeOf(c.ApplicationScale_A)
          c.ApplicationScale_A.computeType := MteComputeType.Mxfp4F32
          c.ApplicationScale_A.ScaleBlocksPerRow := 2.U
          c.MatrixRegTensor_M := 8.U
          c.MatrixRegTensor_K := 1.U
          c.ScaleBankId := m.scaleBank
          c.Generated.get := true.B
          c.Conherent := true.B
          c.MicroTaskValid := io.start && dyn.enable
          c.MicroTaskEndReady := dyn.scaleEndReady
          w.io.DebugInfo.DebugTimeStampe := 0.U
          (w.io.LocalMMUIO, w.io.ToMatrixRegIO, w.io.SpadId,
            w.io.GeneratedScale.get, c.MicroTaskEndValid, c.MicroTaskReady)
        }
        writerMmu.Request.foreach(_.ready := true.B)
        writerMmu.Response.foreach { r =>
          r.valid := false.B
          r.bits := 0.U.asTypeOf(r.bits)
        }
        writerMmu.ConherentRequsetSourceID.valid := true.B
        writerMmu.ConherentRequsetSourceID.bits := 0.U
        writerMmu.nonConherentRequsetSourceID.valid := false.B
        writerMmu.nonConherentRequsetSourceID.bits := 0.U
        val source = generated.get
        sink.valid := source.valid && !dyn.stall
        sink.bits := source.bits
        source.ready := sink.ready && !dyn.stall
        dyn.generatedPending := source.valid
        dyn.generated.valid := source.fire
        dyn.generated.bits := source.bits
        dyn.scaleMemoryRequest := writerMmu.Request.map(_.valid).reduce(_ || _)
        dyn.scaleTaskReady := ready
        dyn.scaleEnd := end
        dyn.scaleWrite := writerPort.BankAddr.valid && writerPort.Data.valid
        Some((writerPort, writerBank))
      } else None
      for ((reg, bank) <- regs.zipWithIndex) {
        reg.io.FromScaleLoader.BankAddr.valid := m.scaleWrite && m.scaleWriteBank === bank.U
        reg.io.FromScaleLoader.BankAddr.bits := m.scaleWriteAddr
        reg.io.FromScaleLoader.Data.valid := m.scaleWrite && m.scaleWriteBank === bank.U
        reg.io.FromScaleLoader.Data.bits := m.scaleWriteWords.asUInt
          .asTypeOf(reg.io.FromScaleLoader.Data.bits)
        generatedWriter.foreach { case (writer, writerBank) =>
          when(writer.BankAddr.valid && writerBank === bank.U) {
            assert(!m.scaleWrite, "manual SF initialization overlapped actual scale writer")
            reg.io.FromScaleLoader.BankAddr := writer.BankAddr
            reg.io.FromScaleLoader.Data := writer.Data
          }
        }
        if (!concurrentScaleReads) {
          reg.io.FromScaleController.BankAddr.valid :=
            scaleActive && scaleBank === bank.U && port.BankAddr.valid
          reg.io.FromScaleController.BankAddr.bits := port.BankAddr.bits
          io.dynamic.foreach { dyn =>
            when(!scaleActive && dyn.read.valid && m.scaleBank === bank.U) {
              reg.io.FromScaleController.BankAddr.valid := true.B
              reg.io.FromScaleController.BankAddr.bits := dyn.read.bits
            }
          }
        }
      }
      if (!concurrentScaleReads) {
      // Reuse CUTETOP's request-fire bank latch for the real Valid-only SRAM
      // response. There is no replacement scale-memory model in this fixture.
      val readBank = io.dynamic.map(dyn =>
        Mux(!scaleActive && dyn.read.valid, m.scaleBank, scaleBank)).getOrElse(scaleBank)
      val sels = regs.indices.map(readBank === _.U)
      val ready = Mux1H(sels zip regs.map(_.io.FromScaleController.BankAddr.ready))
      port.BankAddr.ready := scaleActive && ready
      io.dynamic.foreach(_.read.ready := !scaleActive && ready)
      val inspectFire = io.dynamic.map(_.read.fire).getOrElse(false.B)
      val responseBank = RegEnable(readBank, 0.U, port.BankAddr.fire || inspectFire)
      val inspectResponse = RegEnable(inspectFire, false.B, port.BankAddr.fire || inspectFire)
      val responseSels = regs.indices.map(responseBank === _.U)
      val sfValid = Mux1H(responseSels zip regs.map(_.io.FromScaleController.Data.valid))
      val sfData = Mux1H(responseSels zip regs.map(_.io.FromScaleController.Data.bits))
      port.Data.valid := sfValid && !inspectResponse
      port.Data.bits := sfData
      io.dynamic.foreach { dyn =>
        dyn.data.valid := sfValid && inspectResponse
        dyn.data.bits := sfData.asUInt.asTypeOf(dyn.data.bits)
      }
      }
      m.selectedScaleBank := scaleBank
      m.scaleActive := scaleActive
      m.scaleRead.valid := port.BankAddr.fire
      m.scaleRead.bits := port.BankAddr.bits

      val mem = Module(new ABMatrixReg(if (isB) 2 else 0))
      val fill = mem.io.MatrixRegIO.FromMemoryLoader
      fill := matrix
      when(m.matrixInit) {
        fill.active := true.B
        for (bank <- 0 until ABMatrixRegNBanks) {
          fill.BankAddr(bank).valid := true.B
          fill.BankAddr(bank).bits := m.matrixInitAddr
          fill.Data(bank).valid := true.B
          fill.Data(bank).bits := m.matrixInitWords(bank).asUInt
          fill.ByteMask(bank).valid := true.B
          fill.ByteMask(bank).bits := Fill(ABMatrixRegEntryByteSize, true.B)
        }
      }
      val read = mem.io.MatrixRegIO.FromDataController
      read.BankAddr.valid := m.read.valid
      read.BankAddr.bits := VecInit(Seq.fill(ABMatrixRegNBanks)(m.read.bits))
      m.read.ready := read.BankAddr.ready
      m.data.valid := read.Data.valid
      m.data.bits := read.Data.bits.asUInt.asTypeOf(m.data.bits)
    } else {
      scale.foreach { port =>
        port.BankAddr.ready := false.B
        port.Data.valid := false.B
        port.Data.bits := 0.U.asTypeOf(port.Data.bits)
      }
    }
  }

  if (isB) {
    val loader = Module(new BMemoryLoader)
    loader.io.DebugInfo.DebugTimeStampe := 0.U
    loader.io.LocalMMUIO.ConherentRequsetSourceID.valid := true.B
    loader.io.LocalMMUIO.ConherentRequsetSourceID.bits := io.sourceId
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    connectPorts(loader.io.LocalMMUIO, loader.io.ToMatrixRegIO,
      loader.io.FromScaleRegIO, loader.io.ScaleBankId, loader.io.ScaleReadActive,
      loader.io.GeneratedScale)
    for (channel <- 1 until ABMatrixRegNBanks) {
      loader.io.LocalMMUIO.Request(channel).ready := true.B
      loader.io.LocalMMUIO.Response(channel).valid := false.B
      loader.io.LocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    }

    val config = loader.io.ConfigInfo
    config.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := io.base
    config.ApplicationTensor_B.BlockTensor_B_BaseVaddr := io.base
    config.ApplicationTensor_B.ApplicationTensor_B_Stride_N := io.stride
    config.ApplicationTensor_B.dataType := io.dataType
    config.ApplicationTensor_B.HasTail := io.hasTail
    config.ApplicationTensor_B.TailByteMask := io.tailBytes
    config.ApplicationTensor_B.K_Beat_Count := io.beatsPerRow
    config.MatrixRegTensor_N := io.sourceRows
    config.MatrixRegTensor_K := io.beatsPerRow
    config.MatrixRegId := 0.U
    config.Conherent := true.B
    config.Is_Transpose := (!normalLoads).B
    config.ConvertF32ToI8 := false.B
    config.ConvertF32ToMx := false.B
    config.ConvertComputeType := 0.U
    config.ConvertKElements := 0.U
    config.ConvertScaleBankId := 0.U
    config.ConvertDynamic.foreach(_ := io.dynamic.get.enable)
    config.MicroTaskValid := io.start
    config.MicroTaskEndReady := true.B
    if (mxLoads) {
      config.ConvertF32ToMx := true.B
      config.ConvertComputeType := io.mx.get.computeType
      config.ConvertKElements := 64.U
      config.ConvertScaleBankId := io.mx.get.scaleBank
      config.MatrixRegTensor_K := Mux(io.mx.get.computeType === MteComputeType.Mxfp4F32, 1.U, 2.U)
      config.MicroTaskEndReady := io.mx.get.endReady
      io.mx.get.flags := config.MicroTaskFlags
    }
    if (EnableDifftest) {
      config.pc.get := 0.U
      config.coreid.get := 0.U
      config.retireId.get := 0.U
      config.epoch.get := 0.U
    }
    io.taskReady := config.MicroTaskReady
    io.taskEnd := config.MicroTaskEndValid
  } else {
    val loader = Module(new AMemoryLoader)
    loader.io.DebugInfo.DebugTimeStampe := 0.U
    loader.io.LocalMMUIO.ConherentRequsetSourceID.valid := true.B
    loader.io.LocalMMUIO.ConherentRequsetSourceID.bits := io.sourceId
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    connectPorts(loader.io.LocalMMUIO, loader.io.ToMatrixRegIO,
      loader.io.FromScaleRegIO, loader.io.ScaleBankId, loader.io.ScaleReadActive,
      loader.io.GeneratedScale)
    for (channel <- 1 until ABMatrixRegNBanks) {
      loader.io.LocalMMUIO.Request(channel).ready := true.B
      loader.io.LocalMMUIO.Response(channel).valid := false.B
      loader.io.LocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    }

    val config = loader.io.ConfigInfo
    config.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := io.base
    config.ApplicationTensor_A.ApplicationTensor_A_Stride_M := io.stride
    config.ApplicationTensor_A.dataType := io.dataType
    config.ApplicationTensor_A.HasTail := io.hasTail
    config.ApplicationTensor_A.TailByteMask := io.tailBytes
    config.ApplicationTensor_A.K_Beat_Count := io.beatsPerRow
    config.LoadTaskInfo.Is_ZeroLoad := false.B
    config.LoadTaskInfo.Is_RepeatRowLoad := false.B
    config.LoadTaskInfo.Is_FullLoad := true.B
    config.MatrixRegTensor_M := io.sourceRows
    config.MatrixRegTensor_K := io.beatsPerRow
    config.MatrixRegId := 0.U
    config.Conherent := true.B
    config.Is_Transpose := (!normalLoads).B
    config.ConvertF32ToMx := false.B
    config.ConvertComputeType := 0.U
    config.ConvertKElements := 0.U
    config.ConvertScaleBankId := 0.U
    config.ConvertDynamic.foreach(_ := io.dynamic.get.enable)
    config.MicroTaskValid := io.start
    config.MicroTaskEndReady := true.B
    if (mxLoads) {
      config.ConvertF32ToMx := true.B
      config.ConvertComputeType := io.mx.get.computeType
      config.ConvertKElements := 64.U
      config.ConvertScaleBankId := io.mx.get.scaleBank
      config.MatrixRegTensor_K := Mux(io.mx.get.computeType === MteComputeType.Mxfp4F32, 1.U, 2.U)
      config.MicroTaskEndReady := io.mx.get.endReady
      io.mx.get.flags := config.MicroTaskFlags
    }
    if (EnableDifftest) {
      config.pc.get := 0.U
      config.coreid.get := 0.U
      config.retireId.get := 0.U
      config.epoch.get := 0.U
    }
    io.taskReady := config.MicroTaskReady
    io.taskEnd := config.MicroTaskEndValid
  }
}

class TransposeLoaderIntegrationSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "the legacy AML/BML streaming transpose loaders"

  private val responseBytes = 64
  private val bankCount = 8
  private val entryBytes = 32
  private val entriesPerBank = 32
  private val reduceGroupSize = 2
  private val sourceMajorElements = 128

  private case class RequestMeta(sourceId: Int, row: Int, beat: Int)

  private def elementDataType(elementBytes: Int): UInt = elementBytes match {
    case 1 => ElementDataType.DataTypeWidth8
    case 2 => ElementDataType.DataTypeWidth16
    case 4 => ElementDataType.DataTypeWidth32
  }

  private def byteValue(row: Int, sourceByte: Int): Int =
    (0x19 + row * 37 + sourceByte * 29) & 0xff

  private def responseData(row: Int, beat: Int): BigInt = {
    (0 until responseBytes).foldLeft(BigInt(0)) { case (packed, byteIndex) =>
      packed | (BigInt(byteValue(row, beat * responseBytes + byteIndex)) << (byteIndex * 8))
    }
  }

  private def pokeResponse(dut: TransposeLoaderHarness, meta: Option[RequestMeta]): Unit = {
    dut.io.response.valid.poke(meta.nonEmpty.B)
    dut.io.response.bits.ReseponseConherent.poke(true.B)
    pokeResponseData(dut, meta.map(m => responseData(m.row, m.beat)).getOrElse(BigInt(0)))
    dut.io.response.bits.ReseponseSourceID.poke(meta.map(_.sourceId).getOrElse(0).U)
  }

  private def pokeWords(words: Seq[UInt], data: BigInt): Unit =
    words.zipWithIndex.foreach { case (word, index) =>
      val width = word.getWidth
      word.poke(((data >> (index * width)) & ((BigInt(1) << width) - 1)).U)
    }

  private def pokeResponseData(dut: TransposeLoaderHarness, data: BigInt): Unit =
    pokeWords(dut.io.response.bits.ReseponseData.toSeq, data)

  private def peekWords(words: Seq[UInt]): BigInt =
    words.zipWithIndex.foldLeft(BigInt(0)) {
      case (data, (word, index)) => data | (word.peek().litValue << (index * word.getWidth))
    }

  private def matrixData(dut: TransposeLoaderHarness, bank: Int): BigInt =
    peekWords(dut.io.matrix.Data(bank).bits.toSeq)

  private def observeMatrixWrites(
    dut: TransposeLoaderHarness,
    banks: Int,
    entries: Int,
    bytes: Int,
    actual: mutable.Map[(Int, Int, Int), Int]
  ): Boolean = {
    var wrote = false
    for (bank <- 0 until banks) {
      val addrValid = dut.io.matrix.BankAddr(bank).valid.peek().litToBoolean
      val dataValid = dut.io.matrix.Data(bank).valid.peek().litToBoolean
      val maskValid = dut.io.matrix.ByteMask(bank).valid.peek().litToBoolean
      assert(addrValid == dataValid && dataValid == maskValid,
        s"bank $bank emitted mismatched write-valid signals")
      if (addrValid) {
        val entry = dut.io.matrix.BankAddr(bank).bits.peek().litValue.toInt
        val mask = dut.io.matrix.ByteMask(bank).bits.peek().litValue
        val data = matrixData(dut, bank)
        assert(entry >= 0 && entry < entries, s"bank $bank wrote out-of-range entry $entry")
        for (byteOffset <- 0 until bytes if ((mask >> byteOffset) & 1) != 0) {
          val key = (bank, entry, byteOffset)
          val value = ((data >> (byteOffset * 8)) & 0xff).toInt
          assert(!actual.contains(key), s"duplicate MatrixReg byte write at $key")
          actual(key) = value
        }
        wrote = true
      }
    }
    wrote
  }

  private def initHarness(dut: TransposeLoaderHarness): Unit = {
    dut.io.start.poke(false.B)
    dut.io.dataType.poke(ElementDataType.DataTypeWidth8)
    dut.io.sourceRows.poke(0.U)
    dut.io.beatsPerRow.poke(0.U)
    dut.io.base.poke(0.U)
    dut.io.stride.poke(0.U)
    dut.io.hasTail.poke(false.B)
    dut.io.tailBytes.poke(0.U)
    dut.io.sourceId.poke(0.U)
    dut.io.request.ready.poke(true.B)
    pokeResponse(dut, None)
    dut.io.mx.foreach { m =>
      m.computeType.poke(MteComputeType.Mxfp4F32)
      m.scaleBank.poke(0.U)
      m.endReady.poke(false.B)
      m.scaleWrite.poke(false.B)
      m.scaleWriteBank.poke(0.U)
      m.scaleWriteAddr.poke(0.U)
      m.scaleWriteWords.foreach(_.poke(0.U))
      m.matrixInit.poke(false.B)
      m.matrixInitAddr.poke(0.U)
      m.matrixInitWords.foreach(_.foreach(_.poke(0.U)))
      m.read.valid.poke(false.B)
      m.read.bits.poke(0.U)
    }
    dut.io.computeScale.foreach { c =>
      c.start.poke(false.B); c.go.poke(false.B); c.readEnable.poke(false.B)
      c.bank.poke(0.U); c.rows.poke(0.U); c.endReady.poke(false.B)
    }
    dut.io.dynamic.foreach { dyn =>
      dyn.enable.poke(false.B)
      dyn.stall.poke(false.B)
      dyn.scaleEndReady.poke(false.B)
      dyn.read.valid.poke(false.B)
      dyn.read.bits.poke(0.U)
    }
  }

  private def startTask(
    dut: TransposeLoaderHarness,
    elementBytes: Int,
    sourceRows: Int,
    beatsPerRow: Int,
    stride: Int,
    tailBytes: Option[Int]
  ): Unit = {
    var waitCycles = 0
    while (!dut.io.taskReady.peek().litToBoolean && waitCycles < 64) {
      dut.clock.step()
      waitCycles += 1
    }
    assert(dut.io.taskReady.peek().litToBoolean, "loader did not become ready for a new task")

    dut.io.dataType.poke(elementDataType(elementBytes))
    dut.io.sourceRows.poke(sourceRows.U)
    dut.io.beatsPerRow.poke(beatsPerRow.U)
    dut.io.base.poke(0x1000.U)
    dut.io.stride.poke(stride.U)
    dut.io.hasTail.poke(tailBytes.nonEmpty.B)
    dut.io.tailBytes.poke(tailBytes.getOrElse(0).U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
  }

  private def runTransposeTask(
    dut: TransposeLoaderHarness,
    elementBytes: Int,
    sourceRows: Int,
    beatsPerRow: Int,
    tailBytes: Option[Int] = None
  ): Unit = {
    val groupRows = entryBytes / elementBytes
    val elementSlots = responseBytes / (bankCount * elementBytes)
    val stride = sourceMajorElements * elementBytes
    val expectedRequestCount = sourceRows * beatsPerRow
    val expectedWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val actualWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val pendingRequests = mutable.ArrayBuffer.empty[RequestMeta]
    val pendingResponses = mutable.Queue.empty[RequestMeta]
    var activeResponse = Option.empty[RequestMeta]
    var sourceId = 0
    var requestCount = 0
    var responseCount = 0
    var lastWriteCycle = -1
    var endCycle = -1

    startTask(dut, elementBytes, sourceRows, beatsPerRow, stride, tailBytes)

    for (row <- 0 until sourceRows; beat <- 0 until beatsPerRow) {
      for (bank <- 0 until bankCount; phase <- 0 until responseBytes / bankCount) {
        val q = phase / elementBytes
        val plane = phase % elementBytes
        val sourceByte = elementBytes * (bank + bankCount * q) + plane
        val valid = tailBytes.forall(bytes => beat != beatsPerRow - 1 || sourceByte < bytes)
        if (valid) {
          val group = row / groupRows
          val rowOffset = row % groupRows
          val entry = group + beat * (elementSlots * reduceGroupSize) + q * reduceGroupSize
          val byteOffset = rowOffset * elementBytes + plane
          val key = (bank, entry, byteOffset)
          assert(!expectedWrites.contains(key), s"golden mapping aliases $key")
          expectedWrites(key) = byteValue(row, beat * responseBytes + sourceByte)
        }
      }
    }

    var cycles = 0
    while (endCycle < 0 && cycles < 12000) {
      if (activeResponse.isEmpty && pendingResponses.nonEmpty) {
        activeResponse = Some(pendingResponses.dequeue())
      }

      // Deliberately create valid gaps while retaining the same source-ID/data on stalls.
      val injectGap = activeResponse.nonEmpty && ((responseCount + cycles) % 7 == 3)
      pokeResponse(dut, if (injectGap) None else activeResponse)
      dut.io.sourceId.poke(sourceId.U)

      if (observeMatrixWrites(dut, bankCount, entriesPerBank, entryBytes, actualWrites))
        lastWriteCycle = cycles

      val requestFire = dut.io.request.valid.peek().litToBoolean && dut.io.request.ready.peek().litToBoolean
      val responseFire = activeResponse.nonEmpty && !injectGap && dut.io.response.ready.peek().litToBoolean
      val taskEnd = dut.io.taskEnd.peek().litToBoolean

      if (requestFire) {
        val requestAddr = dut.io.request.bits.RequestAddr.peek().litValue.toInt
        val requestSourceId = dut.io.request.bits.RequestSourceID.peek().litValue.toInt
        val offset = requestAddr - 0x1000
        assert(offset >= 0, s"request address 0x${requestAddr.toHexString} precedes the source base")
        val row = offset / stride
        val beat = (offset % stride) / responseBytes
        assert(row >= 0 && row < sourceRows, s"request row $row outside source rows $sourceRows")
        assert(beat >= 0 && beat < beatsPerRow, s"request beat $beat outside $beatsPerRow")
        assert(requestSourceId == sourceId, s"request source ID $requestSourceId did not match allocator $sourceId")
        pendingRequests += RequestMeta(sourceId, row, beat)
        requestCount += 1
        sourceId = (sourceId + 1) % 64
        if (pendingRequests.size == math.min(groupRows, sourceRows - (row / groupRows) * groupRows)) {
          pendingRequests.reverse.foreach(pendingResponses.enqueue(_))
          pendingRequests.clear()
        }
      }

      if (responseFire) {
        responseCount += 1
        activeResponse = None
      }
      if (taskEnd) {
        endCycle = cycles
      }

      dut.clock.step()
      cycles += 1
    }

    assert(endCycle >= 0, s"transpose task did not complete after $cycles cycles")
    assert(requestCount == expectedRequestCount,
      s"expected $expectedRequestCount requests, observed $requestCount")
    assert(responseCount == expectedRequestCount,
      s"expected $expectedRequestCount responses, observed $responseCount")
    assert(endCycle >= lastWriteCycle, s"task ended at $endCycle before the final write at $lastWriteCycle")
    assert(actualWrites == expectedWrites,
      s"loader byte writes differ from the frozen transpose mapping; missing=${expectedWrites.keySet.diff(actualWrites.keySet).take(8)}, extra=${actualWrites.keySet.diff(expectedWrites.keySet).take(8)}")
  }

  private def runAllPrecisions(isB: Boolean): Unit = {
    test(new TransposeLoaderHarness(isB)(TransposeLoaderIntegrationTestConfig.params))
      .withAnnotations(Seq(VerilatorBackendAnnotation,
        chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        initHarness(dut)

        for (elementBytes <- Seq(1, 2, 4)) {
          val sourceRows = 64 / elementBytes
          val beatsPerRow = sourceMajorElements * elementBytes / responseBytes
          runTransposeTask(dut, elementBytes, sourceRows, beatsPerRow)
        }

        // A legal e16 tail proves invalid bytes never become masked writes.
        runTransposeTask(dut, elementBytes = 2, sourceRows = 1, beatsPerRow = 1, tailBytes = Some(62))
      }
  }

  it should "place AML e8/e16/e32 bytes at the correct bank, entry, and offset" in {
    runAllPrecisions(isB = false)
  }

  it should "place BML e8/e16/e32 bytes at the correct bank, entry, and offset" in {
    runAllPrecisions(isB = true)
  }

  private case class MxLoadScenario(name: String, rows: Int, offset: Int,
      stride: Int, reject: Option[String] = None, dynamic: Boolean = false,
      rejectIndex: Int = 0, zeroIndex: Int = -1)

  private def mxParams(lanes: Int, owners: Int = 4,
      dynamic: Boolean = false): Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_2Tops_64SCP.copy(
      EnableDifftest = false, ExperimentalMxfpConvertLoad = true,
      ExperimentalMxfpDynamicQuant = dynamic,
      MxfpConvertLanes = lanes,
      MxfpConvertOwners = owners,
      MatrixExtension = MatrixIsaParams(enableInt8Int32 = true,
        enableMxfp4Fp32 = true, enableMxfp8Fp32 = true))
  })

  private def mxComputeType(format: Int): UInt = format match {
    case 0 => MteComputeType.Mxfp4F32
    case 1 => MteComputeType.Mxfp8e4m3F32
    case 2 => MteComputeType.Mxfp8e5m2F32
  }

  private def mxScale(row: Int, block: Int, bank: Int, tag: Int): Int =
    124 + (row % 5) * 2 + block * 3 + bank * 5 + tag % 2

  // Exact representable centers and their known codes, not a replacement
  // quantizer/rounder. The frozen primitive suites own rounding-edge oracles.
  private def mxExactLane(format: Int, row: Int, block: Int, lane: Int,
      scale: Int, tag: Int): (Int, Int) = {
    val index = (row * 3 + block * 5 + lane + tag) % 8
    val negative = ((row + block + lane / 7 + tag) & 1) != 0
    val values = if (format == 0) Seq(0.0, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0, 12.0)
      else Seq(0.0, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0)
    val codes = if (format == 0) (0 until 8)
      else if (format == 1) Seq(0, 0x30, 0x38, 0x3c, 0x40, 0x44, 0x48, 0x4c)
      else Seq(0, 0x38, 0x3c, 0x3e, 0x40, 0x42, 0x44, 0x46)
    val signed = if (negative) -values(index) else values(index)
    val value = Math.scalb(signed, scale - (if (format == 0) 128 else 127)).toFloat
    val sign = if (negative && (format != 0 || index != 0)) {
      if (format == 0) 8 else 128
    } else 0
    (java.lang.Float.floatToRawIntBits(value), codes(index) | sign)
  }

  private def mxSentinel(bank: Int, entry: Int, byte: Int): Int =
    (0xa5 + bank * 37 + entry * 11 + byte * 3) & 0xff

  // Test-side memory service controls, not a model of an actual LLC. A Get
  // accepted in cycle t first becomes offerable at t + responseLatencyCycles.
  // Replies are FIFO, at most one 64B beat per responseIntervalCycles after
  // the preceding reply fires; a DUT stall retains the offered source/data.
  private case class MxLoadPerfModel(responseLatencyCycles: Int,
      responseIntervalCycles: Int) {
    require(responseLatencyCycles > 0 && responseIntervalCycles > 0)
  }

  private case class MxLoadStats(taskCycles: Int, sfPreloadCycles: Int,
      sfPreloadBytes: Int, requestedBytes: Int, returnedBytes: Int,
      writtenBytes: Int, k32Blocks: Int, scaleReadCount: Int, endFlags: Int,
      checkedSramBytes: Int, firstRequest: Int, lastRequest: Int,
      firstInput: Int, lastInput: Int, firstOutput: Int, lastOutput: Int,
      endValidCycle: Int, responseStallCycles: Int, requestAddresses: Vector[Int])

  private def runMxLoad(dut: TransposeLoaderHarness, params: Parameters,
      format: Int, scenario: MxLoadScenario, tag: Int,
      perf: Option[MxLoadPerfModel] = None, concurrentScale: Boolean = false): MxLoadStats = {
    val m = dut.io.mx.get
    val geometry = params(CuteParamsKey)
    val dynamic = scenario.dynamic
    require(concurrentScale == dut.io.computeScale.nonEmpty)
    require(!concurrentScale || (!dynamic && format == 0 && perf.isEmpty))
    require(!dynamic || (geometry.ExperimentalMxfpDynamicQuant && format == 0 &&
      scenario.rows == 8 && scenario.rejectIndex >= 0 && scenario.rejectIndex < 16))
    perf.foreach { _ =>
      require(!dynamic && format == 0 && scenario.rows == 64 && scenario.reject.isEmpty)
      require(scenario.offset % responseBytes == 0 && scenario.stride == 128 * 4)
    }
    val banks = geometry.ABMatrixRegNBanks
    val bytes = geometry.ABMatrixRegEntryByteSize
    val entries = geometry.ABMatrixRegBankNEntries
    val base = 0x4000 + scenario.offset
    val selectedBank = tag & 1
    val blockBytes = if (format == 0) 16 else 32
    val codeBits = if (format == 0) 4 else 8
    val rejectedBlock = if (scenario.reject.contains("inf")) 0 else 1
    val rejectedLane = scenario.reject match {
      case Some("nan") => 0
      case Some("inf") => 31
      case _ => 17
    }
    val expectedFlags = if (scenario.reject.nonEmpty) 0x10 else 0
    val expectedWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val actualWrites = mutable.Map.empty[(Int, Int, Int), Int]
    val sourceImage = mutable.Map.empty[Int, Int]
    def blockScale(row: Int, block: Int): Int =
      if (dynamic) {
        if (row * 2 + block == scenario.zeroIndex) 0 else 108 + row * 2 + block
      } else mxScale(row, block, selectedBank, tag)
    def blockRejected(row: Int, block: Int): Boolean = scenario.reject.nonEmpty &&
      (if (dynamic) row * 2 + block == scenario.rejectIndex
       else row == scenario.rows - 1 && block == rejectedBlock)
    for (row <- 0 until scenario.rows; block <- 0 until 2) {
      val e = blockScale(row, block)
      val rejected = blockRejected(row, block)
      val zero = dynamic && row * 2 + block == scenario.zeroIndex
      var payload = BigInt(0)
      for (lane <- 0 until 32) {
        val (centerBits, code) = mxExactLane(format, row, block, lane, e, tag)
        val bits = if (zero) {
          if ((lane & 1) == 0) 0 else Int.MinValue // Both signs of exact zero.
        } else if (rejected && lane == rejectedLane) {
          scenario.reject.get match {
            case "nan" => 0x7f800001 // one active sNaN; reject only this K32
            case "inf" => 0x7f800000
            case "domain" =>
              if (dynamic) ((e + 3) << 23) | 1 // Greater than every center; outside guard.
              else {
                val beyond = if (format == 0) 17.0 else if (format == 1) 512.0 else 65536.0
                java.lang.Float.floatToRawIntBits(
                  Math.scalb(beyond, e - (if (format == 0) 128 else 127)).toFloat)
              }
          }
        } else centerBits
        val address = base + row * scenario.stride + (block * 32 + lane) * 4
        for (byte <- 0 until 4) {
          assert(!sourceImage.contains(address + byte), "fixture rows overlap")
          sourceImage(address + byte) = (bits >>> (byte * 8)) & 0xff
        }
        payload |= BigInt(code) << (lane * codeBits)
      }
      if (rejected || zero) payload = BigInt(0)
      for (byte <- 0 until blockBytes) {
        val offset = block * blockBytes + byte
        val key = (row % banks, row / banks * geometry.ReduceGroupSize + offset / bytes, offset % bytes)
        assert(!expectedWrites.contains(key), s"MX golden mapping aliases $key")
        expectedWrites(key) = ((payload >> (byte * 8)) & 0xff).toInt
      }
    }
    val expectedAddresses = (for {
      row <- 0 until scenario.rows
      beat <- 0 until 4
      logical = base + row * scenario.stride + beat * responseBytes
      line <- (logical & -responseBytes) to ((logical + responseBytes - 1) & -responseBytes) by responseBytes
    } yield line).toSeq

    initHarness(dut)
    var readyCycles = 0
    while (!dut.io.taskReady.peek().litToBoolean && readyCycles < 64) {
      dut.clock.step()
      readyCycles += 1
    }
    assert(dut.io.taskReady.peek().litToBoolean, "MX loader did not become ready")
    // Initialize all physical bytes, including FP4's unused second row entry,
    // so a masked-write/late-write bug cannot hide behind uninitialized SRAM.
    m.matrixInit.poke(true.B)
    for (entry <- 0 until entries) {
      m.matrixInitAddr.poke(entry.U)
      for (bank <- 0 until banks) {
        val word = (0 until bytes).foldLeft(BigInt(0)) { (v, byte) =>
          v | (BigInt(mxSentinel(bank, entry, byte)) << (8 * byte))
        }
        pokeWords(m.matrixInitWords(bank).toSeq, word)
      }
      dut.clock.step()
    }
    m.matrixInit.poke(false.B)
    // Both actual SF banks have distinct row/block envelopes. Unused bytes
    // are poisoned; row32's two scale bytes cross the 64-byte SF boundary.
    // Perf only preloads the live lines of the selected real SF bank. This
    // direct SRAM preload is counted separately, not CPU scale preparation.
    var sfPreloadCycles = 0
    m.scaleWrite.poke(true.B)
    for (bank <- 0 until 2; line <- 0 until geometry.ABScaleBankNEntries) {
      if (perf.isEmpty || (bank == selectedBank &&
          line < (scenario.rows * 2 + responseBytes - 1) / responseBytes)) {
      m.scaleWriteBank.poke(bank.U)
      m.scaleWriteAddr.poke(line.U)
      val word = (0 until responseBytes).foldLeft(BigInt(0)) { (v, byte) =>
        val index = line * responseBytes + byte
        val e = if (!dynamic && index < scenario.rows * 2)
          mxScale(index / 2, index % 2, bank, tag) else 255
        v | (BigInt(e) << (8 * byte))
      }
      pokeWords(m.scaleWriteWords.toSeq, word)
      dut.clock.step()
      sfPreloadCycles += 1
      }
    }
    m.scaleWrite.poke(false.B)
    m.computeType.poke(mxComputeType(format))
    m.scaleBank.poke(selectedBank.U)
    dut.io.dynamic.foreach { dyn =>
      dyn.enable.poke(dynamic.B)
      dyn.stall.poke(dynamic.B)
    }
    dut.io.dataType.poke(ElementDataType.DataTypeWidth32)
    dut.io.sourceRows.poke(scenario.rows.U)
    dut.io.beatsPerRow.poke(4.U)
    dut.io.base.poke(base.U)
    dut.io.stride.poke(scenario.stride.U)
    // No End backpressure in perf: sample the first End and accept it on
    // that edge. Legacy stress still holds End for its original seven cycles.
    if (perf.nonEmpty) m.endReady.poke(true.B)
    dut.io.computeScale.foreach { c =>
      c.rows.poke(scenario.rows.U); c.bank.poke((1 - selectedBank).U)
      c.taskReady.expect(true.B); c.start.poke(true.B)
    }
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    dut.io.computeScale.foreach { c =>
      c.start.poke(false.B)
      c.bank.poke(selectedBank.U) // Invalid descriptor cycles are not ownership.
    }
    // Invalid-cycle descriptor changes must not redirect the owned task.
    m.scaleBank.poke((1 - selectedBank).U)
    m.computeType.poke(mxComputeType((format + 1) % 3))
    m.flags.expect(0.U)

    val pending = mutable.ArrayBuffer.empty[(Int, Int)]
    var active = Option.empty[(Int, Int)]
    val inFlight = mutable.Map.empty[Int, Int] // source -> actual request ordinal
    val observedAddresses = mutable.ArrayBuffer.empty[Int]
    val returnedOrder = mutable.ArrayBuffer.empty[Int]
    val scaleReads = mutable.ArrayBuffer.empty[Int]
    val responseDue = mutable.Map.empty[Int, Int]
    var nextResponseCycle = 0
    var firstRequest = -1
    var lastRequest = -1
    var firstInput = -1
    var lastInput = -1
    var firstOutput = -1
    var lastOutput = -1
    var endValidCycle = -1
    var endFlags = -1
    var responseStallCycles = 0
    val generatedScales = mutable.Map.empty[Int, (Int, Boolean)]
    var sfWrites = 0
    var sinkHeldCycles = 0
    var stalledScale = Option.empty[(Int, Int, Boolean)]
    var source = 0
    var cycles = 0
    var requestStalls = 0
    var stalledRequest = Option.empty[(BigInt, BigInt)]
    var computeTokens = 0
    var simultaneousReads = 0
    var heldCompute = Option.empty[(BigInt, Seq[BigInt])]
    var done = false
    while (!done && cycles < 12000) {
      dut.io.sourceId.poke(source.U)
      dut.io.dynamic.foreach { dyn =>
        // Hold the first real result seven cycles, then periodically stall
        // again while raw source responses and other owners remain live.
        dyn.stall.poke((dynamic && (sinkHeldCycles < 7 || cycles % 13 < 4)).B)
      }
      if (perf.nonEmpty) {
        dut.io.request.ready.poke(true.B)
        if (active.isEmpty && pending.nonEmpty &&
            cycles >= responseDue(pending.head._1) && cycles >= nextResponseCycle)
          active = Some(pending.remove(0))
      } else {
      dut.io.request.ready.poke((cycles % 5 != 1).B)
      // Reuse the existing containing-line response/word driver. Keep a
      // chosen response stable until fire; reverse queued owners, not bytes.
      if (active.isEmpty && pending.nonEmpty && cycles % 3 == 0 &&
          (pending.size >= 2 || cycles % 7 == 0 || observedAddresses.size == expectedAddresses.size))
        active = Some(pending.remove(pending.size - 1))
      }
      dut.io.response.valid.poke(active.nonEmpty.B)
      dut.io.response.bits.ReseponseConherent.poke(true.B)
      dut.io.response.bits.ReseponseSourceID.poke(active.map(_._1).getOrElse(0).U)
      val response = active.map { case (_, address) =>
        (0 until responseBytes).foldLeft(BigInt(0)) { (v, byte) =>
          val value = sourceImage.getOrElse(address + byte, (0xd3 + address + byte) & 0xff)
          v | (BigInt(value) << (8 * byte))
        }
      }.getOrElse(BigInt(0))
      pokeResponseData(dut, response)
      val beforeWriteCount = actualWrites.size
      observeMatrixWrites(dut, banks, entries, bytes, actualWrites)
      // Cycle zero is the accepted taskStart edge immediately before this
      // loop. A channel fire sampled now commits at the next edge (cycles+1).
      val stamp = cycles + 1
      if (actualWrites.size != beforeWriteCount) {
        if (firstOutput < 0) firstOutput = stamp
        lastOutput = stamp
      }
      m.selectedScaleBank.expect(selectedBank.U)
      m.scaleActive.expect((!dynamic).B)
      if (m.scaleRead.valid.peek().litToBoolean)
        scaleReads += m.scaleRead.bits.peek().litValue.toInt
      dut.io.computeScale.foreach { c =>
        val go = cycles % 7 >= 2
        c.go.poke(go.B)
        // Align ready compute requests with real converter reads; no SRAM
        // response or reader is modeled. Both must actually fire together.
        c.readEnable.poke(m.scaleRead.valid.peek().litToBoolean.B)
        c.selectedBank.expect((1 - selectedBank).U)
        if (c.requestFires.peek().litValue == 3) simultaneousReads += 1
        if (c.valid.peek().litToBoolean) {
          val tuple = (peekWords(c.words.toSeq), c.liveK.map(_.peek().litValue).toSeq)
          heldCompute.foreach(previous => assert(tuple == previous, "real compute SF token changed under stall"))
          val expected = (0 until geometry.Matrix_MN).foldLeft(BigInt(0)) { (v, lane) =>
            val row = computeTokens * geometry.Matrix_MN + lane
            c.liveK(lane).expect((if (row < scenario.rows) 64 else 0).U)
            val word = (0 until geometry.ScaleWidth / 8).foldLeft(BigInt(0)) { (w, byte) =>
              val e = if (row < scenario.rows && byte < 2)
                mxScale(row, byte, 1 - selectedBank, tag) else 127
              w | (BigInt(e) << (8 * byte))
            }
            v | (word << (lane * geometry.ScaleWidth))
          }
          assert(tuple._1 == expected, s"wrong real compute SF token $computeTokens on peer bank")
          heldCompute = if (go) None else Some(tuple)
          if (go) computeTokens += 1
        } else assert(heldCompute.isEmpty, "real compute SF valid disappeared under stall")
      }
      if (dynamic) {
        val dyn = dut.io.dynamic.get
        dyn.scaleMemoryRequest.expect(false.B)
        m.scaleRead.valid.expect(false.B)
        val generated = dyn.generated.valid.peek().litToBoolean
        val wrote = actualWrites.size != beforeWriteCount
        assert(wrote == generated, "actual TR write and generated SF handshake must be atomic")
        val index = dyn.generated.bits.row.peek().litValue.toInt * 2 +
          (if (dyn.generated.bits.block.peek().litToBoolean) 1 else 0)
        val e = dyn.generated.bits.e.peek().litValue.toInt
        val rejected = dyn.generated.bits.unsupported.peek().litToBoolean
        val pendingScale = (index, e, rejected)
        stalledScale.foreach(previous =>
          assert(dyn.generatedPending.peek().litToBoolean && pendingScale == previous,
            "generated SF owner/data changed under backpressure"))
        stalledScale = if (dyn.stall.peek().litToBoolean &&
            dyn.generatedPending.peek().litToBoolean) {
          sinkHeldCycles += 1
          assert(!generated && !wrote, "backpressured SF repeated a Valid-only TR write")
          Some(pendingScale)
        } else None
        if (generated) {
          assert(index >= 0 && index < 16 && !generatedScales.contains(index),
            s"lost or duplicated generated K32 index $index")
          val expectedReject = blockRejected(index / 2, index % 2)
          assert(rejected == expectedReject, s"wrong unsupported status for K32 $index")
          assert(e == (if (expectedReject) 0 else blockScale(index / 2, index % 2)),
            s"wrong generated scale for K32 $index: $e")
          assert(actualWrites.size == beforeWriteCount + 16,
            "each actual FP4 K32 write must touch exactly sixteen bytes")
          generatedScales(index) = ((e, rejected))
        }
        if (dyn.scaleWrite.peek().litToBoolean) {
          assert(generatedScales.size == 16 && actualWrites.size == 256,
            "SSCP write preceded the sixteen actual TR/scale handshakes")
          sfWrites += 1
          assert(sfWrites == 1, "generated sole scale writer repeated its SSCP write")
        }
        if (dyn.scaleEnd.peek().litToBoolean)
          assert(sfWrites == 1, "scale End preceded the actual sole-writer SSCP write")
      }

      val requestValid = dut.io.request.valid.peek().litToBoolean
      val requestReady = dut.io.request.ready.peek().litToBoolean
      val address = dut.io.request.bits.RequestAddr.peek().litValue
      val id = dut.io.request.bits.RequestSourceID.peek().litValue
      stalledRequest.foreach { previous =>
        assert(requestValid && previous == ((address, id)), "MX request changed while stalled")
      }
      stalledRequest = if (requestValid && !requestReady) {
        requestStalls += 1
        Some((address, id))
      } else None
      if (requestValid && requestReady) {
        if (firstRequest < 0) firstRequest = stamp
        lastRequest = stamp
        assert(address % responseBytes == 0, s"unaligned MX physical Get $address")
        assert(id == source && !inFlight.contains(source), s"MX reused in-flight source $source")
        assert(observedAddresses.size < expectedAddresses.size, "extra MX source request")
        assert(address == expectedAddresses(observedAddresses.size),
          s"MX physical request mismatch at ${observedAddresses.size}: $address")
        inFlight(source) = observedAddresses.size
        observedAddresses += address.toInt
        pending += ((source, address.toInt))
        perf.foreach(model => responseDue(source) = cycles + model.responseLatencyCycles)
        source = (1 to 64).map(i => (source + i) % 64)
          .find(id => !inFlight.contains(id)).getOrElse(sys.error("MX fixture exhausted source IDs"))
      }
      if (active.nonEmpty && !dut.io.response.ready.peek().litToBoolean)
        responseStallCycles += 1
      if (active.nonEmpty && dut.io.response.ready.peek().litToBoolean) {
        val id = active.get._1
        if (firstInput < 0) firstInput = stamp
        lastInput = stamp
        perf.foreach { model =>
          assert(cycles >= responseDue.remove(id).get, "MX response preceded modeled latency")
          nextResponseCycle = cycles + model.responseIntervalCycles
        }
        assert(inFlight.contains(id), s"MX response has no request owner $id")
        returnedOrder += inFlight.remove(id).get
        active = None
      }
      done = dut.io.taskEnd.peek().litToBoolean &&
        (!dynamic || dut.io.dynamic.get.scaleEnd.peek().litToBoolean)
      if (done) {
        endValidCycle = cycles
        endFlags = m.flags.peek().litValue.toInt
        assert(pending.isEmpty && active.isEmpty && inFlight.isEmpty,
          "MX End preceded outstanding source fragments")
        m.flags.expect(expectedFlags.U)
      }
      dut.clock.step()
      cycles += 1
    }
    assert(done, s"MX task timed out: format=$format ${scenario.name} rows=${scenario.rows}")
    dut.io.computeScale.foreach { c =>
      val expectedTokens = (scenario.rows + geometry.Matrix_MN - 1) / geometry.Matrix_MN
      assert(computeTokens == expectedTokens && simultaneousReads > 0 && heldCompute.isEmpty,
        s"missing real cross-bank work: tokens=$computeTokens/$expectedTokens simultaneous=$simultaneousReads")
      c.taskEnd.expect(true.B)
      c.go.poke(false.B); c.readEnable.poke(false.B)
      c.endReady.poke(true.B); dut.clock.step(); c.endReady.poke(false.B)
      dut.clock.step(); c.idle.expect(true.B); c.taskEnd.expect(false.B)
      println(s"MX_REAL_SCALE_OVERLAP bank=$selectedBank tokens=$computeTokens simultaneous=$simultaneousReads " +
        "actual_ASC_BSC_AML_BML_SRAM=PASS peer_scale_bytes_liveK_stalls_End=PASS")
    }
    assert(observedAddresses.toSeq == expectedAddresses)
    assert(returnedOrder.size == expectedAddresses.size)
    if (perf.isEmpty) {
    assert(returnedOrder.toSeq != returnedOrder.sorted.toSeq, "MX replies never exercised reordering")
    assert(requestStalls > 0, "MX source requests never exercised backpressure")
    } else {
      assert(returnedOrder.toSeq == returnedOrder.sorted.toSeq, "MX perf model reordered replies")
      assert(requestStalls == 0, "MX perf driver injected request backpressure")
      assert(responseDue.isEmpty && firstRequest > 0 && firstInput > firstRequest)
      assert(firstOutput > firstInput && lastOutput >= firstOutput)
      assert(endValidCycle > lastOutput, "MX End did not drain the final actual packed write")
    }
    if (dynamic) {
      assert(scaleReads.isEmpty && generatedScales.size == 16 && sfWrites == 1 && sinkHeldCycles >= 7)
    } else {
      assert(scaleReads.size == scenario.rows * 2, "MX scale reads lost/duplicated a K32 owner")
      assert(scaleReads.toSet == (0 to ((scenario.rows * 2 - 1) / responseBytes)).toSet)
    }
    assert(actualWrites == expectedWrites,
      s"MX byte writes mismatch: missing=${expectedWrites.keySet.diff(actualWrites.keySet).take(8)} extra=${actualWrites.keySet.diff(expectedWrites.keySet).take(8)}")
    dut.io.response.valid.poke(false.B)
    if (perf.isEmpty) {
    for (_ <- 0 until 7) {
      dut.io.taskEnd.expect(true.B)
      dut.io.taskReady.expect(false.B)
      m.flags.expect(expectedFlags.U)
      dut.io.request.valid.expect(false.B)
      m.scaleRead.valid.expect(false.B)
      if (dynamic) {
        val dyn = dut.io.dynamic.get
        dyn.scaleEnd.expect(true.B)
        dyn.scaleWrite.expect(false.B)
        dyn.generatedPending.expect(false.B)
        dyn.scaleMemoryRequest.expect(false.B)
      }
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX write after held End")
      dut.clock.step()
    }
    m.endReady.poke(true.B)
    dut.clock.step()
    m.endReady.poke(false.B)
    if (dynamic) {
      val dyn = dut.io.dynamic.get
      // The integration harness holds both physical End signals separately;
      // TC's same-owner atomic join/publication is checked in its own suite.
      for (_ <- 0 until 4) {
        dut.io.taskEnd.expect(false.B)
        dyn.scaleEnd.expect(true.B)
        dyn.scaleTaskReady.expect(false.B)
        dyn.scaleWrite.expect(false.B)
        assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites))
        dut.clock.step()
      }
      dyn.scaleEndReady.poke(true.B)
      dut.clock.step()
      dyn.scaleEndReady.poke(false.B)
    }
    for (_ <- 0 until 12) {
      dut.io.taskEnd.expect(false.B)
      dut.io.request.valid.expect(false.B)
      m.scaleActive.expect(false.B)
      m.scaleRead.valid.expect(false.B)
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX late write after End.fire")
      dut.clock.step()
    }
    } else {
      m.endReady.poke(false.B)
    }
    // Observe actual SRAM contents, not only the loader's write intentions.
    // Every inactive byte must retain its independently preloaded sentinel.
    for (entry <- 0 until entries) {
      m.read.valid.poke(true.B)
      m.read.bits.poke(entry.U)
      m.read.ready.expect(true.B)
      dut.clock.step()
      m.read.valid.poke(false.B)
      m.data.valid.expect(true.B)
      for (bank <- 0 until banks) {
        val word = peekWords(m.data.bits(bank).toSeq)
        for (byte <- 0 until bytes) {
          val expected = expectedWrites.getOrElse((bank, entry, byte), mxSentinel(bank, entry, byte))
          assert(((word >> (8 * byte)) & 0xff) == expected,
            s"MX SRAM mismatch bank=$bank entry=$entry byte=$byte format=$format ${scenario.name}")
        }
      }
      assert(!observeMatrixWrites(dut, banks, entries, bytes, actualWrites), "MX write during final readback")
    }
    dut.clock.step()
    m.data.valid.expect(false.B)
    if (dynamic) {
      val dyn = dut.io.dynamic.get
      // Read the actual selected and unselected SSCP banks. The live sixteen
      // E8M0 bytes,127 padding, untouched lines and untouched bank are distinct.
      for (bank <- 0 until 2; line <- 0 until geometry.ABScaleBankNEntries) {
        m.scaleBank.poke(bank.U)
        dyn.read.valid.poke(true.B)
        dyn.read.bits.poke(line.U)
        dyn.read.ready.expect(true.B)
        dut.clock.step()
        dyn.read.valid.poke(false.B)
        dyn.data.valid.expect(true.B)
        val word = peekWords(dyn.data.bits.toSeq)
        for (byte <- 0 until responseBytes) {
          val expected = if (bank == selectedBank && line == 0) {
            if (byte >= 16) 127 else generatedScales(byte)._1
          } else 255
          assert(((word >> (8 * byte)) & 0xff) == expected,
            s"dynamic real SSCP mismatch bank=$bank line=$line byte=$byte ${scenario.name}")
        }
        dyn.scaleWrite.expect(false.B)
        dyn.scaleMemoryRequest.expect(false.B)
      }
      dut.clock.step()
      dyn.data.valid.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      dyn.scaleTaskReady.expect(true.B)
    }
    if (perf.isEmpty) {
    println(s"MX converting-load dynamic=$dynamic format=$format lanes=${geometry.MxfpConvertLanes} ${scenario.name} " +
      s"rows=${scenario.rows} requests=${observedAddresses.size} replies=${returnedOrder.size} " +
      s"packedBytes=${actualWrites.size} scaleReads=${scaleReads.size} requestStalls=$requestStalls " +
      s"flags=$expectedFlags heldEnd=7 lateWindow=12 SRAMbytes=${banks * entries * bytes} cycles=$cycles")
    }
    // Excludes SRAM initialization, SF preload, late-window stress and the
    // SRAM readback checker. Counts from accepted start.fire (origin zero)
    // through first End.fire, without charging the start edge a second time.
    MxLoadStats(cycles, sfPreloadCycles, sfPreloadCycles * responseBytes,
      observedAddresses.size * responseBytes, returnedOrder.size * responseBytes,
      actualWrites.size, actualWrites.size / blockBytes, scaleReads.size, endFlags,
      banks * entries * bytes, firstRequest, lastRequest, firstInput, lastInput,
      firstOutput, lastOutput, endValidCycle, responseStallCycles, observedAddresses.toVector)
  }

  // Same actual source CAM, raw owners, existing encoder, masked TR writes,
  // original ASL/BSL and real SSCP. No supplied-SF preload serves the dynamic
  // tasks; it only poisons physical memory to make padding/isolation observable.
  private def resetDynamicPending(dut: TransposeLoaderHarness): Unit = {
    initHarness(dut)
    val m = dut.io.mx.get
    val dyn = dut.io.dynamic.get
    dut.io.taskReady.expect(true.B)
    dyn.scaleTaskReady.expect(true.B)
    dyn.enable.poke(true.B)
    dyn.stall.poke(true.B)
    m.computeType.poke(MteComputeType.Mxfp4F32)
    m.scaleBank.poke(1.U)
    dut.io.dataType.poke(ElementDataType.DataTypeWidth32)
    dut.io.sourceRows.poke(8.U)
    dut.io.beatsPerRow.poke(4.U)
    dut.io.base.poke(0x9000.U)
    dut.io.stride.poke(256.U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    val pending = mutable.Queue.empty[Int]
    var active = Option.empty[Int]
    var source = 0
    var cycles = 0
    val beat = (0 until 16).foldLeft(BigInt(0)) { (word, lane) =>
      word | (BigInt(0x3fc00000) << (32 * lane)) // Exactly1.5, guarded max.
    }
    while (!dyn.generatedPending.peek().litToBoolean && cycles < 1000) {
      dut.io.sourceId.poke(source.U)
      if (active.isEmpty && pending.nonEmpty) active = Some(pending.dequeue())
      dut.io.response.valid.poke(active.nonEmpty.B)
      dut.io.response.bits.ReseponseSourceID.poke(active.getOrElse(0).U)
      pokeResponseData(dut, beat)
      if (dut.io.request.valid.peek().litToBoolean) {
        pending.enqueue(source)
        source = (source + 1) % 64
      }
      if (active.nonEmpty && dut.io.response.ready.peek().litToBoolean) active = None
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dyn.scaleWrite.expect(false.B)
      dyn.scaleMemoryRequest.expect(false.B)
      dut.clock.step()
      cycles += 1
    }
    dyn.generatedPending.expect(true.B)
    dut.io.response.valid.poke(false.B)
    dut.io.request.ready.poke(false.B)
    for (_ <- 0 until 7) {
      dyn.generatedPending.expect(true.B)
      dyn.generated.valid.expect(false.B)
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dyn.scaleWrite.expect(false.B)
      dut.io.taskEnd.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      dut.clock.step()
    }
    // A global fixture reset cancels both producer and memory service. This
    // does not claim precise late-fragment-fault recovery: old replies are
    // discarded with the service, and the whole row is reloaded afterwards.
    dut.reset.poke(true.B)
    initHarness(dut)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    for (_ <- 0 until 7) {
      dyn.generatedPending.expect(false.B)
      dyn.generated.valid.expect(false.B)
      dyn.scaleWrite.expect(false.B)
      dyn.scaleMemoryRequest.expect(false.B)
      dut.io.matrix.BankAddr.foreach(_.valid.expect(false.B))
      dut.io.request.valid.expect(false.B)
      dut.io.taskEnd.expect(false.B)
      dyn.scaleEnd.expect(false.B)
      m.flags.expect(0.U)
      dut.clock.step()
    }
    dut.io.taskReady.expect(true.B)
    dyn.scaleTaskReady.expect(true.B)
  }

  for (isB <- Seq(false, true); lanes <- Seq(8, 32)) {
    it should s"dynamic MX4 ${if (isB) "BML" else "AML"} TR SSCP atomic16 lanes$lanes recover unsupported" in {
      val params = mxParams(lanes, dynamic = true)
      test(new TransposeLoaderHarness(isB, normalLoads = true, mxLoads = true)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(100000)
          initHarness(dut)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          val scenarios = Seq(
            MxLoadScenario("dynamic16-distinct-e", 8, 0, 256, dynamic = true),
            MxLoadScenario("dynamic16-reject0", 8, 61, 283,
              Some("inf"), dynamic = true, rejectIndex = 0),
            MxLoadScenario("dynamic16-reject7", 8, 8, 265,
              Some("nan"), dynamic = true, rejectIndex = 7),
            MxLoadScenario("dynamic16-reject15", 8, 63, 269,
              Some("domain"), dynamic = true, rejectIndex = 15),
            MxLoadScenario("dynamic16-recover-zero", 8, 0, 256,
              dynamic = true, zeroIndex = 3))
          // No reset between accepted/rejected/accepted tasks: old raw,
          // staging, unsupported and matrix NV must not leak into the retry.
          for ((scenario, tag) <- scenarios.zipWithIndex) {
            val stats = runMxLoad(dut, params, format = 0, scenario = scenario, tag = tag)
            assert(stats.writtenBytes == 256 && stats.k32Blocks == 16 &&
              stats.scaleReadCount == 0 &&
              stats.endFlags == (if (scenario.reject.nonEmpty) 16 else 0))
          }
          for (format <- 0 until 3) {
            // Same parameter-on hardware switches back to the original
            // supplied SF reader and all three original encoder formats.
            runMxLoad(dut, params, format,
              MxLoadScenario("supplied-after-dynamic", 5, 8, 265), tag = format)
          }
          resetDynamicPending(dut)
          runMxLoad(dut, params, format = 0,
            scenario = MxLoadScenario("dynamic-after-reset", 8, 0, 256, dynamic = true), tag = 5)
        }
    }
  }

  for (isB <- Seq(false, true)) {
    it should s"R47 overlap real ${if (isB) "BSC and BML" else "ASC and AML"} peer-bank readers" in {
      val params = mxParams(8)
      test(new TransposeLoaderHarness(isB, normalLoads = true, mxLoads = true,
        concurrentScaleReads = true)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(250000)
          initHarness(dut); dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
          for (bank <- 0 until 2) {
            runMxLoad(dut, params, format = 0,
              scenario = MxLoadScenario("R47-peer-bank-SF-line-crossing", 33, 61, 283),
              tag = bank, concurrentScale = true)
          }
        }
    }
  }

  for (isB <- Seq(false, true); lanes <- Seq(8, 32); owners <- Seq(4, 16)) {
    val description = if (owners == 4) {
      s"convert supplied-scale MX ${if (isB) "BML" else "AML"} through real 2T64 SRAM lanes$lanes"
    } else {
      s"R33 stress MX ${if (isB) "BML" else "AML"} owners$owners lanes$lanes through real SRAM"
    }
    it should description in {
      val params = mxParams(lanes, owners)
      test(new TransposeLoaderHarness(isB, normalLoads = true, mxLoads = true)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(250000)
          initHarness(dut)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          val scenarios = Seq(
            MxLoadScenario("aligned", 5, 0, 256),
            MxLoadScenario("split", 5, 61, 283),
            MxLoadScenario("nan-K32", 5, 8, 265, Some("nan")),
            MxLoadScenario("inf-K32", 5, 63, 269, Some("inf")),
            MxLoadScenario("domain-K32", 5, 4, 277, Some("domain")),
            MxLoadScenario("cross-SF-recovery", 33, 63, 269))
          for (format <- 0 until 3; (scenario, index) <- scenarios.zipWithIndex)
            runMxLoad(dut, params, format, scenario, format * 11 + index)
        }
    }
  }

  for (isB <- Seq(false, true)) {
  it should s"preserve normal ${if (isB) "BML" else "AML"} bytes across containing-line and split reads on native 2T" in {
    val params = new Config((_, _, _) => {
      case CuteParamsKey => CuteParams.CUTE_2Tops_128SCP
    })
    test(new TransposeLoaderHarness(isB = isB, normalLoads = true)(params))
      .withAnnotations(Seq(VerilatorBackendAnnotation,
        chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      initHarness(dut)

      // Reuse the same real source CAM/fill FIFO over successive tasks. Each
      // logical row may straddle lines; replies arrive in reverse source order.
      val layouts = (for {
        offset <- Seq(0, 8, 61, 63)
        liveBytes <- Seq(3, 33, 64)
      } yield (offset, liveBytes, 1, 64)) ++ Seq(
        (8, 33, 5, 49), (0, 64, 5, 128),
        (0, 16, 8, 16), (0, 32, 8, 32), (0, 64, 8, 64))
      for ((offset, liveBytes, rows, stride) <- layouts) {
        var readyCycles = 0
        while (!dut.io.taskReady.peek().litToBoolean && readyCycles < 64) {
          dut.clock.step()
          readyCycles += 1
        }
        assert(dut.io.taskReady.peek().litToBoolean, "normal loader did not become ready")
        val base = 0x1000 + offset
        val beats = (liveBytes + responseBytes - 1) / responseBytes
        val tail = liveBytes % responseBytes
        val expectedAddresses = (for {
          row <- 0 until rows
          beat <- 0 until beats
          logical = base + row * stride + beat * responseBytes
          count = math.min(responseBytes, liveBytes - beat * responseBytes)
          line <- (logical & -responseBytes) to ((logical + count - 1) & -responseBytes) by responseBytes
        } yield line).toSeq
        val expected = (for (row <- 0 until rows; k <- 0 until liveBytes)
          yield (row, k) -> ((base + row * stride + k) * 29 + 0x31 & 0xff)).toMap
        val actual = mutable.Map.empty[(Int, Int), Int]
        val pending = mutable.ArrayBuffer.empty[(Int, Int)]
        val observedAddresses = mutable.ArrayBuffer.empty[Int]
        var active = Option.empty[(Int, Int)]
        var source = 0
        var replies = 0
        var cycles = 0
        var done = false

        dut.io.base.poke(base.U)
        dut.io.stride.poke(stride.U)
        dut.io.sourceRows.poke(rows.U)
        dut.io.beatsPerRow.poke(beats.U)
        dut.io.hasTail.poke((tail != 0).B)
        dut.io.tailBytes.poke(tail.U)
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)

        while (!done && cycles < 1000) {
          dut.io.sourceId.poke(source.U)
          dut.io.request.ready.poke((cycles % 5 != 1).B)
          // Single-row boundary cases collect every physical request before
          // returning strictly in reverse order. M5 retains concurrent replies.
          val canRespond = rows != 1 || observedAddresses.size == expectedAddresses.size
          if (active.isEmpty && pending.nonEmpty && canRespond && cycles % 4 == 0)
            active = Some(pending.remove(pending.size - 1))
          dut.io.response.valid.poke(active.nonEmpty.B)
          dut.io.response.bits.ReseponseConherent.poke(true.B)
          dut.io.response.bits.ReseponseSourceID.poke(active.map(_._1).getOrElse(0).U)
          val data = active.map { case (_, addr) =>
            (0 until responseBytes).foldLeft(BigInt(0)) { case (word, b) =>
              word | (BigInt(((addr + b) * 29 + 0x31) & 0xff) << (8 * b))
            }
          }.getOrElse(BigInt(0))
          pokeResponseData(dut, data)

          if (dut.io.request.valid.peek().litToBoolean && dut.io.request.ready.peek().litToBoolean) {
            val addr = dut.io.request.bits.RequestAddr.peek().litValue.toInt
            assert(addr % responseBytes == 0, s"unaligned physical Get: $addr")
            assert(dut.io.request.bits.RequestSourceID.peek().litValue.toInt == source)
            observedAddresses += addr
            pending += ((source, addr))
            source = (source + 1) % 64
          }
          if (dut.io.response.valid.peek().litToBoolean && dut.io.response.ready.peek().litToBoolean) {
            replies += 1
            active = None
          }
          for (bank <- 0 until 4 if dut.io.matrix.BankAddr(bank).valid.peek().litToBoolean) {
            val addr = dut.io.matrix.BankAddr(bank).bits.peek().litValue.toInt
            val word = matrixData(dut, bank)
            val mask = dut.io.matrix.ByteMask(bank).bits.peek().litValue
            val row = addr / 2 * 4 + bank
            for (b <- 0 until entryBytes if mask.testBit(b)) {
              val key = (row, addr % 2 * entryBytes + b)
              assert(!actual.contains(key), s"duplicate byte write $key")
              actual(key) = ((word >> (8 * b)) & 0xff).toInt
            }
          }
          done = dut.io.taskEnd.peek().litToBoolean
          if (done) {
            assert(pending.isEmpty && active.isEmpty)
            assert(replies == expectedAddresses.size)
          }
          dut.clock.step()
          cycles += 1
        }
        assert(done, s"normal task timed out: offset=$offset bytes=$liveBytes rows=$rows")
        assert(observedAddresses == expectedAddresses)
        assert(actual == expected, s"normal byte mapping mismatch: offset=$offset bytes=$liveBytes rows=$rows")
        dut.io.response.valid.poke(false.B)
      }
    }
  }
  }

  // R33 perf cases: each unique name is independently selectable in a fresh
  // JVM, with exactly one test/DUT and no reset between its four K64 tasks.
  // This is supplied-scale AML quantization only, not GEMM, dynamic scale
  // selection, CPU preparation, or an actual LLC/hardware performance claim.
  for (lanes <- Seq(8, 32); owners <- Seq(4, 8, 16);
      model <- Seq(MxLoadPerfModel(8, 1), MxLoadPerfModel(32, 1))) {
    val ownerTag = s"-owners$owners"
    it should s"R33-MX4-A128x128-lanes$lanes-lat${model.responseLatencyCycles}-ii${model.responseIntervalCycles}$ownerTag" in {
      val params = mxParams(lanes, owners)
      val geometry = params(CuteParamsKey)
      test(new TransposeLoaderHarness(isB = false, normalLoads = true, mxLoads = true)(params))
        .withAnnotations(Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))) { dut =>
          dut.clock.setTimeout(100000)
          initHarness(dut)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          val stats = (for (rowTile <- 0 until 2; kTile <- 0 until 2) yield {
            val task = rowTile * 2 + kTile
            val scenario = MxLoadScenario(s"A128-rowTile$rowTile-kTile$kTile",
              rows = 64, offset = rowTile * 64 * 128 * 4 + kTile * 64 * 4,
              stride = 128 * 4)
            val result = runMxLoad(dut, params, format = 0, scenario = scenario,
              tag = task, perf = Some(model))
            assert(result.requestedBytes == 64 * 64 * 4 &&
              result.returnedBytes == result.requestedBytes)
            assert(result.writtenBytes == 64 * 64 / 2 && result.k32Blocks == 128)
            assert(result.scaleReadCount == 128 && result.endFlags == 0)
            assert(result.sfPreloadCycles == 2 && result.sfPreloadBytes == 128)
            println(s"R33_MX4_TASK lanes=$lanes owners=$owners latency=${model.responseLatencyCycles} " +
              s"response_interval=${model.responseIntervalCycles} task=$task " +
              s"row0=${rowTile * 64} k0=${kTile * 64} rows=64 K=64 " +
              s"task_cycles=${result.taskCycles} SF_preload_cycles=${result.sfPreloadCycles} " +
              s"first_request=${result.firstRequest} last_request=${result.lastRequest} " +
              s"first_input=${result.firstInput} last_input=${result.lastInput} " +
              s"first_output=${result.firstOutput} last_output=${result.lastOutput} " +
              s"End_valid=${result.endValidCycle} End_fire=${result.taskCycles} " +
              s"response_stall_cycles=${result.responseStallCycles} " +
              s"requested_bytes=${result.requestedBytes} returned_bytes=${result.returnedBytes} " +
              s"written_bytes=${result.writtenBytes} K32=${result.k32Blocks} " +
              s"scale_reads=${result.scaleReadCount} End_flags=${result.endFlags} " +
              s"SRAM_checked_bytes=${result.checkedSramBytes}")
            result
          }).toVector
          val addresses = stats.flatMap(_.requestAddresses)
          val expectedAddresses = (0 until 128 * 128 * 4 by responseBytes).map(_ + 0x4000)
          assert(addresses.distinct.size == addresses.size &&
            addresses.sorted == expectedAddresses,
            "R33 four tasks did not cover each byte of one row-major A[128][128] exactly once")
          assert(stats.size == 4 && stats.map(_.requestedBytes).sum == 65536 &&
            stats.map(_.returnedBytes).sum == 65536)
          assert(stats.map(_.k32Blocks).sum == 512 && stats.map(_.writtenBytes).sum == 8192)
          assert(stats.map(_.scaleReadCount).sum == 512 && stats.forall(_.endFlags == 0))
          assert(stats.map(_.checkedSramBytes).sum ==
            4 * geometry.ABMatrixRegNBanks * geometry.ABMatrixRegBankNEntries *
              geometry.ABMatrixRegEntryByteSize)
          val taskCycles = stats.map(_.taskCycles).sum
          val sfCycles = stats.map(_.sfPreloadCycles).sum
          println(s"R33_MX4_A128x128 lanes=$lanes owners=$owners " +
            s"model_min_response_latency=${model.responseLatencyCycles} " +
            s"model_response_interval=${model.responseIntervalCycles} " +
            s"model_bytes_per_beat=$responseBytes model_request_ready=always " +
            s"model_responses=in_order_held_until_fire task_cap=64rows_K64 tasks=4 resets=1 " +
            s"input_F32=16384 requested_bytes=${stats.map(_.requestedBytes).sum} " +
            s"returned_bytes=${stats.map(_.returnedBytes).sum} K32=${stats.map(_.k32Blocks).sum} " +
            s"written_bytes=${stats.map(_.writtenBytes).sum} scale_reads=${stats.map(_.scaleReadCount).sum} " +
            s"SF_preload_bytes=${stats.map(_.sfPreloadBytes).sum} " +
            s"task_cycles_sum=$taskCycles SF_preload_cycles_sum=$sfCycles " +
            s"component_cycles_sum=${taskCycles + sfCycles} " +
            s"SRAM_checked_bytes=${stats.map(_.checkedSramBytes).sum} End_flags=0 " +
            "scope=provided_scale_component CPU_scale_preparation=outside_DUT_not_measured " +
            "sequential_subtiles_overwrite_SCP=yes output_collection_readback=checker_excluded " +
            "full_output_simultaneous_residency=no DRAM_quant_completion=no " +
            "matrix_init_and_SRAM_readback=excluded extra_End_hold=0 " +
            "dynamic_quantization=no CPU_comparison=no actual_LLC_model=no")
        }
    }
  }
}
