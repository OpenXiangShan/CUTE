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
class TransposeLoaderHarness(isB: Boolean)(implicit p: Parameters) extends CuteModule {
  private val configWidth = MatrixRegMaxTensorDimBitSize

  val io = IO(new Bundle {
    val start = Input(Bool())
    val dataType = Input(UInt(ElementDataType.DataTypeBitWidth.W))
    val sourceRows = Input(UInt(configWidth.W))
    val beatsPerRow = Input(UInt(configWidth.W))
    val base = Input(UInt(MMUAddrWidth.W))
    val stride = Input(UInt(MMUAddrWidth.W))
    val hasTail = Input(Bool())
    val tailBytes = Input(UInt(log2Ceil(outsideDataWidthByte + 1).W))
    val sourceId = Input(UInt(LLCSourceMaxNumBitSize.W))

    val taskReady = Output(Bool())
    val taskEnd = Output(Bool())
    val request = Decoupled(new MMURequestIO)
    val response = Flipped(Decoupled(new MMUResponseIO))
    val matrix = Flipped(new ABMemoryLoaderMatrixRegIO)
  })

  if (isB) {
    val loader = Module(new BMemoryLoader)
    loader.io.DebugInfo.DebugTimeStampe := 0.U
    loader.io.LocalMMUIO.ConherentRequsetSourceID.valid := true.B
    loader.io.LocalMMUIO.ConherentRequsetSourceID.bits := io.sourceId
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    loader.io.LocalMMUIO.Request(0) <> io.request
    loader.io.LocalMMUIO.Response(0) <> io.response
    for (channel <- 1 until ABMatrixRegNBanks) {
      loader.io.LocalMMUIO.Request(channel).ready := true.B
      loader.io.LocalMMUIO.Response(channel).valid := false.B
      loader.io.LocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    }
    loader.io.ToMatrixRegIO <> io.matrix

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
    config.Is_Transpose := true.B
    config.MicroTaskValid := io.start
    config.MicroTaskEndReady := true.B
    if (EnableDifftest) {
      config.pc.get := 0.U
      config.coreid.get := 0.U
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
    loader.io.LocalMMUIO.Request(0) <> io.request
    loader.io.LocalMMUIO.Response(0) <> io.response
    for (channel <- 1 until ABMatrixRegNBanks) {
      loader.io.LocalMMUIO.Request(channel).ready := true.B
      loader.io.LocalMMUIO.Response(channel).valid := false.B
      loader.io.LocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    }
    loader.io.ToMatrixRegIO <> io.matrix

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
    config.Is_Transpose := true.B
    config.MicroTaskValid := io.start
    config.MicroTaskEndReady := true.B
    if (EnableDifftest) {
      config.pc.get := 0.U
      config.coreid.get := 0.U
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
    dut.io.response.bits.ReseponseData.poke(meta.map(m => responseData(m.row, m.beat)).getOrElse(BigInt(0)).U)
    dut.io.response.bits.ReseponseSourceID.poke(meta.map(_.sourceId).getOrElse(0).U)
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

      for (bank <- 0 until bankCount) {
        val addrValid = dut.io.matrix.BankAddr(bank).valid.peek().litToBoolean
        val dataValid = dut.io.matrix.Data(bank).valid.peek().litToBoolean
        val maskValid = dut.io.matrix.ByteMask(bank).valid.peek().litToBoolean
        assert(addrValid == dataValid && dataValid == maskValid,
          s"bank $bank emitted mismatched write-valid signals")
        if (addrValid) {
          val entry = dut.io.matrix.BankAddr(bank).bits.peek().litValue.toInt
          val mask = dut.io.matrix.ByteMask(bank).bits.peek().litValue
          val data = dut.io.matrix.Data(bank).bits.peek().litValue
          assert(entry >= 0 && entry < entriesPerBank, s"bank $bank wrote out-of-range entry $entry")
          for (byteOffset <- 0 until entryBytes if ((mask >> byteOffset) & 1) != 0) {
            val key = (bank, entry, byteOffset)
            val value = ((data >> (byteOffset * 8)) & 0xff).toInt
            assert(!actualWrites.contains(key), s"duplicate MatrixReg byte write at $key")
            actualWrites(key) = value
          }
          lastWriteCycle = cycles
        }
      }

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
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
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
}
