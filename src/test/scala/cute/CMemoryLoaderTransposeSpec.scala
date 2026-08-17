package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

object CMemoryLoaderTransposeTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_1Tops_64SCP
  })
}

class CMemoryLoaderTransposeHarness(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val sourceRows = Input(UInt(MatrixRegMaxTensorDimBitSize.W))
    val sourceColumns = Input(UInt(MatrixRegMaxTensorDimBitSize.W))
    val beatsPerRow = Input(UInt(MatrixRegMaxTensorDimBitSize.W))
    val base = Input(UInt(MMUAddrWidth.W))
    val stride = Input(UInt(MMUAddrWidth.W))
    val sourceId = Input(UInt(LLCSourceMaxNumBitSize.W))

    val taskReady = Output(Bool())
    val taskEnd = Output(Bool())
    val request = Decoupled(new MMURequestIO)
    val response = Flipped(Decoupled(new MMUResponseIO))
    val bankAddr = Output(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W))))
    val data = Output(Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryBitSize.W))))
    val byteMask = Output(Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryByteSize.W))))
  })

  val loader = Module(new CMemoryLoader)
  loader.io.DebugInfo.DebugTimeStampe := 0.U

  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.valid := true.B
  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.bits := io.sourceId
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  loader.io.LoadLocalMMUIO.Request(0) <> io.request
  loader.io.LoadLocalMMUIO.Response(0) <> io.response
  for (channel <- 1 until ABMatrixRegNBanks) {
    loader.io.LoadLocalMMUIO.Request(channel).ready := true.B
    loader.io.LoadLocalMMUIO.Response(channel).valid := false.B
    loader.io.LoadLocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
  }

  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.valid := false.B
  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  for (channel <- 0 until ABMatrixRegNBanks) {
    loader.io.StoreLocalMMUIO.Request(channel).ready := true.B
    loader.io.StoreLocalMMUIO.Response(channel).valid := false.B
    loader.io.StoreLocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
  }

  loader.io.ToMatrixRegIO.LoadReadWriteResponse := loader.io.ToMatrixRegIO.LoadReadWriteRequest
  loader.io.ToMatrixRegIO.StoreReadWriteResponse := loader.io.ToMatrixRegIO.StoreReadWriteRequest
  for (bank <- 0 until CMatrixRegNBanks) {
    loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid := false.B
    loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits := 0.U
  }
  io.bankAddr := loader.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr
  io.data := loader.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data
  io.byteMask := loader.io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask

  val config = loader.io.ConfigInfo
  config.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := io.base
  config.ApplicationTensor_C.BlockTensor_C_BaseVaddr := io.base
  config.ApplicationTensor_C.ApplicationTensor_C_Stride_M := io.stride
  config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
  config.ApplicationTensor_C.HasTail := false.B
  config.ApplicationTensor_C.TailByteMask := 0.U
  config.ApplicationTensor_C.N_Beat_Count := io.beatsPerRow
  config.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := 0.U
  config.ApplicationTensor_D.BlockTensor_D_BaseVaddr := 0.U
  config.ApplicationTensor_D.ApplicationTensor_D_Stride_M := 0.U
  config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32
  config.LoadTaskInfo.Is_ZeroLoad := false.B
  config.LoadTaskInfo.Is_RepeatRowLoad := false.B
  config.LoadTaskInfo.Is_FullLoad := true.B
  config.Conherent := true.B
  config.Is_Transpose := true.B
  config.MatrixRegTensor_M := io.sourceRows
  config.MatrixRegTensor_N := io.sourceColumns
  config.MatrixRegId := 0.U
  config.LoadMicroTaskValid := io.start
  config.LoadMicroTaskEndReady := true.B
  config.StoreMicroTaskValid := false.B
  config.StoreMicroTaskEndReady := true.B
  if (EnableDifftest) {
    config.pc.get := 0.U
    config.coreid.get := 0.U
  }

  io.taskReady := config.LoadMicroTaskReady
  io.taskEnd := config.LoadMicroTaskEndValid
}

class CMemoryLoaderTransposeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "the legacy C memory loader transpose path"

  private val base = 0x1000
  private val responseBytes = 64
  private val resultBytes = 4
  private val bankCount = 4
  private val entryBytes = 16
  private val entriesPerRowGroup = 16
  private val sourceRows = 8
  private val sourceColumns = 32
  private val beatsPerRow = sourceColumns * resultBytes / responseBytes

  private case class RequestMeta(sourceId: Int, row: Int, beat: Int)

  private def elementValue(row: Int, column: Int): Int =
    0x01000000 | (row << 12) | column

  private def responseData(row: Int, beat: Int): BigInt = {
    (0 until responseBytes / resultBytes).foldLeft(BigInt(0)) { case (packed, element) =>
      val column = beat * (responseBytes / resultBytes) + element
      packed | (BigInt(elementValue(row, column)) << (element * 32))
    }
  }

  private def expectedBytes: Map[(Int, Int, Int), Int] = {
    (for {
      row <- 0 until sourceRows
      column <- 0 until sourceColumns
      byte <- 0 until resultBytes
    } yield {
      val bank = column % bankCount
      val entry = (column / bankCount) * entriesPerRowGroup + row / bankCount
      val byteOffset = (row % bankCount) * resultBytes + byte
      val value = (elementValue(row, column) >> (byte * 8)) & 0xff
      (bank, entry, byteOffset) -> value
    }).toMap
  }

  private def pokeResponse(dut: CMemoryLoaderTransposeHarness, meta: Option[RequestMeta]): Unit = {
    dut.io.response.valid.poke(meta.nonEmpty.B)
    dut.io.response.bits.ReseponseConherent.poke(true.B)
    dut.io.response.bits.ReseponseSourceID.poke(meta.map(_.sourceId).getOrElse(0).U)
    dut.io.response.bits.ReseponseData.poke(meta.map(m => responseData(m.row, m.beat)).getOrElse(BigInt(0)).U)
  }

  it should "scatter out-of-order e32 cacheline responses into transposed C-bank entries" in {
    test(new CMemoryLoaderTransposeHarness()(CMemoryLoaderTransposeTestConfig.params))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)

        dut.io.start.poke(false.B)
        dut.io.sourceRows.poke(sourceRows.U)
        dut.io.sourceColumns.poke(sourceColumns.U)
        dut.io.beatsPerRow.poke(beatsPerRow.U)
        dut.io.base.poke(base.U)
        dut.io.stride.poke((sourceColumns * resultBytes).U)
        dut.io.sourceId.poke(0.U)
        dut.io.request.ready.poke(true.B)
        pokeResponse(dut, None)

        var readyWait = 0
        while (!dut.io.taskReady.peek().litToBoolean && readyWait < 32) {
          dut.clock.step()
          readyWait += 1
        }
        assert(dut.io.taskReady.peek().litToBoolean, "CML did not become ready")
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)

        val requests = mutable.ArrayBuffer.empty[RequestMeta]
        val actualBytes = mutable.Map.empty[(Int, Int, Int), Int]
        var activeResponse = Option.empty[RequestMeta]
        var nextSourceId = 0
        var responseCount = 0
        var endCycle = -1
        var cycle = 0
        val requestCount = sourceRows * beatsPerRow

        while (endCycle < 0 && cycle < 512) {
          if (activeResponse.isEmpty && requests.size == requestCount && responseCount < requestCount) {
            activeResponse = Some(requests(requestCount - 1 - responseCount))
          }
          dut.io.sourceId.poke(nextSourceId.U)
          pokeResponse(dut, activeResponse)

          for (bank <- 0 until bankCount) {
            val addrValid = dut.io.bankAddr(bank).valid.peek().litToBoolean
            val dataValid = dut.io.data(bank).valid.peek().litToBoolean
            val maskValid = dut.io.byteMask(bank).valid.peek().litToBoolean
            assert(addrValid == dataValid && dataValid == maskValid,
              s"bank $bank emitted mismatched write-valid signals")
            if (addrValid) {
              val entry = dut.io.bankAddr(bank).bits.peek().litValue.toInt
              val data = dut.io.data(bank).bits.peek().litValue
              val mask = dut.io.byteMask(bank).bits.peek().litValue
              assert(entry >= 0 && entry < 256, s"bank $bank wrote out-of-range entry $entry")
              assert(mask.bitCount == resultBytes,
                s"bank $bank entry $entry expected a $resultBytes-byte write, got mask 0x${mask.toString(16)}")
              for (byteOffset <- 0 until entryBytes if ((mask >> byteOffset) & 1) != 0) {
                val key = (bank, entry, byteOffset)
                assert(!actualBytes.contains(key), s"duplicate C MatrixReg byte write at $key")
                actualBytes(key) = ((data >> (byteOffset * 8)) & 0xff).toInt
              }
            }
          }

          val requestFire = dut.io.request.valid.peek().litToBoolean && dut.io.request.ready.peek().litToBoolean
          val responseFire = activeResponse.nonEmpty && dut.io.response.ready.peek().litToBoolean
          if (requestFire) {
            val requestAddr = dut.io.request.bits.RequestAddr.peek().litValue.toInt
            val requestSourceId = dut.io.request.bits.RequestSourceID.peek().litValue.toInt
            val stride = sourceColumns * resultBytes
            val offset = requestAddr - base
            val row = offset / stride
            val beat = (offset % stride) / responseBytes
            assert(requestAddr == base + row * stride + beat * responseBytes,
              s"unexpected request address 0x${requestAddr.toHexString}")
            assert(row >= 0 && row < sourceRows, s"request row $row outside source matrix")
            assert(beat >= 0 && beat < beatsPerRow, s"request beat $beat outside source row")
            assert(requestSourceId == nextSourceId,
              s"request source ID $requestSourceId did not match allocator $nextSourceId")
            requests += RequestMeta(requestSourceId, row, beat)
            nextSourceId += 1
          }
          if (responseFire) {
            responseCount += 1
            activeResponse = None
          }
          if (dut.io.taskEnd.peek().litToBoolean) {
            endCycle = cycle
          }

          dut.clock.step()
          cycle += 1
        }

        assert(endCycle >= 0, s"CML transpose task did not complete after $cycle cycles")
        assert(requests.size == requestCount, s"expected $requestCount requests, observed ${requests.size}")
        assert(responseCount == requestCount, s"expected $requestCount responses, observed $responseCount")
        assert(actualBytes.toMap == expectedBytes,
          s"transposed C writes differ: missing=${expectedBytes.keySet.diff(actualBytes.keySet).take(8)}, extra=${actualBytes.keySet.diff(expectedBytes.keySet).take(8)}")
      }
  }
}
