package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.reflect.DataMirror
import chisel3.experimental.UnlocatableSourceInfo
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import xscache.coupledL2._
import xscache.common.BankBitsKey
import xscache.chi.{CHIDataCheckKey, CHIPoisonKey, HasCHIOpcodes}

object CMemoryLoaderStoreAddressTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_512SCP
    case utility.LogUtilsOptionsKey => utility.LogUtilsOptions(false, false, false)
    case utility.PerfCounterOptionsKey => utility.PerfCounterOptions(
      false, false, utility.XSPerfLevel.VERBOSE, 0)
  })
}

class CMemoryLoaderStoreAddressHarness(val storeProbe: Boolean = false,
    val externalAck: Boolean = false, val base: BigInt = 0x1000,
    val stride: Int = 256, val loadProbe: Boolean = false,
    val localMmuProbe: Boolean = false,
    val multiTileProbe: Boolean = false)(implicit p: Parameters) extends CuteModule {
  require(!multiTileProbe || (storeProbe && externalAck && !loadProbe && !localMmuProbe))
  val io = IO(new Bundle {
    val start = Input(Bool())
    val transpose = Input(Bool())
    val rows = Input(UInt(9.W))
    val columns = Input(UInt(9.W))
    val memoryReady = Input(Bool())
    val allocatedId = Input(UInt(LLCSourceMaxNumBitSize.W))
    val ack = Input(Valid(UInt(SoureceMaxNumBitSize.W)))
    val loadAddress = Input(UInt(MMUAddrWidth.W))
    val memoryData = Input(UInt(MMUDataWidth.W))
    val taskReady = Output(Bool())
    val done = Output(Bool())
    val request = Output(Valid(new MMURequestIO))
    val pending = Output(Valid(new MMURequestIO))
    val bankAddr = Output(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W))))
    val loadData = Output(Vec(CMatrixRegNBanks, Valid(UInt((CMatrixRegEntryByteSize*8).W))))
    val loadMask = Output(Vec(CMatrixRegNBanks, Valid(UInt(CMatrixRegEntryByteSize.W))))
    val transportIdle = Output(Bool())
    // Optional owner-level observation only: old harness call sites retain
    // their constant base/pattern and unchanged default 8T configuration.
    val storeBase = Option.when(multiTileProbe)(Input(UInt(MMUAddrWidth.W)))
    val storeTag = Option.when(multiTileProbe)(Input(UInt(8.W)))
    val storeWords = Option.when(multiTileProbe)(Output(Vec(outsideDataWidth / 32, UInt(32.W))))
    val ackReady = Option.when(multiTileProbe)(Output(Bool()))
  })

  val loader = Module(new CMemoryLoader)
  loader.io.DebugInfo.DebugTimeStampe := 0.U

  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.valid := true.B
  loader.io.LoadLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.LoadLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.valid := true.B
  loader.io.StoreLocalMMUIO.ConherentRequsetSourceID.bits := (if (externalAck) io.allocatedId else 0.U)
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
  loader.io.StoreLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
  for (channel <- 0 until ABMatrixRegNBanks) {
    loader.io.LoadLocalMMUIO.Request(channel).ready := true.B
    loader.io.LoadLocalMMUIO.Response(channel).valid := false.B
    loader.io.LoadLocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
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
  io.bankAddr := loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr
  io.done := loader.io.ConfigInfo.StoreMicroTaskEndValid
  io.request.valid := loader.io.StoreLocalMMUIO.Request(0).fire
  io.request.bits := loader.io.StoreLocalMMUIO.Request(0).bits
  io.pending.valid := loader.io.StoreLocalMMUIO.Request(0).valid
  io.pending.bits := loader.io.StoreLocalMMUIO.Request(0).bits
  io.loadData := loader.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data
  io.loadMask := loader.io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask
  io.storeWords.foreach(words =>
    words := loader.io.StoreLocalMMUIO.Request(0).bits.RequestData.asTypeOf(words))
  io.ackReady.foreach(_ := loader.io.StoreLocalMMUIO.Response(0).ready)
  val storePatternTag = if (multiTileProbe) {
    Some(RegEnable(io.storeTag.get, 0.U(8.W),
      io.start && loader.io.ConfigInfo.StoreMicroTaskReady))
  } else None
  if (loadProbe) {
    val pending = Module(new Queue(new MMUResponseIO, 4))
    val request = loader.io.LoadLocalMMUIO.Request(0)
    request.ready := pending.io.enq.ready && io.memoryReady
    pending.io.enq.valid := request.fire
    pending.io.enq.bits := 0.U.asTypeOf(new MMUResponseIO)
    pending.io.enq.bits.ReseponseData := io.memoryData
    pending.io.enq.bits.ReseponseSourceID := request.bits.RequestSourceID
    pending.io.enq.bits.ReseponseConherent := true.B
    loader.io.LoadLocalMMUIO.Response(0) <> pending.io.deq
    io.request.valid := request.fire
    io.request.bits := request.bits
    io.done := loader.io.ConfigInfo.LoadMicroTaskEndValid
  }
  if (storeProbe) {
    loader.io.StoreLocalMMUIO.Request(0).ready := io.memoryReady
    loader.io.StoreLocalMMUIO.Response(0).valid := (if (externalAck) io.ack.valid
      else RegNext(loader.io.StoreLocalMMUIO.Request(0).fire, false.B))
    if (externalAck) {
      loader.io.StoreLocalMMUIO.Response(0).bits.ReseponseSourceID := io.ack.bits
    }
    for (bank <- 0 until CMatrixRegNBanks) {
      val request = loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank)
      val address = request.bits
      val row = (address / (Tensor_MN / Matrix_MN).U) * Matrix_MN.U + bank.U
      val column = (address % (Tensor_MN / Matrix_MN).U) * Matrix_MN.U
      val words = VecInit((0 until Matrix_MN).map { lane =>
        val pattern = ("h01000000".U(32.W) | (row << 12) | (column + lane.U)).pad(32)
        if (multiTileProbe) pattern | (storePatternTag.get << 24) else pattern
      })
      val response = loader.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank)
      response.valid := RegNext(request.valid, false.B)
      response.bits := RegEnable(words.asUInt, request.valid)
    }
  }

  val config = loader.io.ConfigInfo
  config.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := (if (loadProbe) io.loadAddress else 0.U)
  config.ApplicationTensor_C.BlockTensor_C_BaseVaddr := (if (loadProbe) io.loadAddress else 0.U)
  config.ApplicationTensor_C.ApplicationTensor_C_Stride_M := (if (loadProbe) stride.U else 0.U)
  config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
  config.ApplicationTensor_C.HasTail := (if (loadProbe) (io.columns * 4.U) % 64.U =/= 0.U else false.B)
  config.ApplicationTensor_C.TailByteMask := (if (loadProbe) (io.columns * 4.U) % 64.U else 0.U)
  config.ApplicationTensor_C.N_Beat_Count := (if (loadProbe) (io.columns * 4.U +& 63.U) / 64.U else 0.U)
  config.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := (if (multiTileProbe) io.storeBase.get else base.U)
  config.ApplicationTensor_D.BlockTensor_D_BaseVaddr := (if (multiTileProbe) io.storeBase.get else base.U)
  config.ApplicationTensor_D.ApplicationTensor_D_Stride_M := stride.U
  config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32
  config.LoadTaskInfo.Is_ZeroLoad := false.B
  config.LoadTaskInfo.Is_RepeatRowLoad := false.B
  config.LoadTaskInfo.Is_FullLoad := true.B
  config.Conherent := true.B
  config.Is_Transpose := io.transpose
  config.MatrixRegTensor_M := (if (storeProbe || loadProbe) io.rows else 64.U)
  config.MatrixRegTensor_N := (if (storeProbe || loadProbe) io.columns else 64.U)
  config.MatrixRegId := 0.U
  config.LoadMicroTaskValid := (if (loadProbe) io.start else false.B)
  config.LoadMicroTaskEndReady := true.B
  config.StoreMicroTaskValid := (if (loadProbe) false.B else io.start)
  config.StoreMicroTaskEndReady := true.B
  if (EnableDifftest) {
    config.pc.get := 0.U
    config.coreid.get := 0.U
  }

  io.taskReady := (if (loadProbe) config.LoadMicroTaskReady else config.StoreMicroTaskReady)
  io.transportIdle := true.B
  if (localMmuProbe) {
    val mmu = Module(new LocalMMU)
    mmu.io.CLoadLocalMMUIO <> loader.io.LoadLocalMMUIO
    mmu.io.CStoreLocalMMUIO <> loader.io.StoreLocalMMUIO
    for (port <- Seq(mmu.io.ALocalMMUIO, mmu.io.BLocalMMUIO,
        mmu.io.ASLocalMMUIO, mmu.io.BSLocalMMUIO);
        channel <- 0 until ABMatrixRegNBanks) {
      port.Request(channel).valid := false.B
      port.Request(channel).bits := 0.U.asTypeOf(port.Request(channel).bits)
      port.Response(channel).ready := true.B
    }
    mmu.io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid := true.B
    mmu.io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits := 0.U
    mmu.io.LastLevelCacheTLIO.nonConherentRequsetSourceID.valid := false.B
    mmu.io.LastLevelCacheTLIO.nonConherentRequsetSourceID.bits := 0.U
    for (channel <- 0 until ABMatrixRegNBanks) {
      val req = mmu.io.LastLevelCacheTLIO.Request(channel)
      val replies = Module(new Queue(new MMUResponseIO, 4))
      req.ready := replies.io.enq.ready && io.memoryReady
      replies.io.enq.valid := req.fire
      replies.io.enq.bits := 0.U.asTypeOf(new MMUResponseIO)
      replies.io.enq.bits.ReseponseSourceID := req.bits.RequestSourceID
      replies.io.enq.bits.ReseponseConherent := true.B
      mmu.io.LastLevelCacheTLIO.Response(channel) <> replies.io.deq
    }
    io.transportIdle := mmu.io.idle
  }
}

// Match the existing MaskedStoreSliceTestTop / MXFP4 probe adapters: keep
// every native-test top-level leaf <=64 bits, not merely the observed data.
// The unchanged child harness still carries the full 512-bit CML request
// and 128-bit SCP response; no data, mask or handshake is truncated here.
class CMemoryLoaderStoreRequestMeta(implicit p: Parameters) extends CuteBundle {
  val RequestAddr = UInt(MMUAddrWidth.W)
  val RequestMask = UInt(MMUMaskWidth.W)
  val RequestSourceID = UInt(64.W)
  val RequestType_isWrite = Bool()
  val RequestConherent = Bool()
  val UseAllocatedSourceID = Bool()
}

class CMemoryLoaderStoreNarrowIO(implicit p: Parameters) extends CuteBundle {
  val start = Input(Bool())
  val transpose = Input(Bool())
  val rows = Input(UInt(9.W))
  val columns = Input(UInt(9.W))
  val memoryReady = Input(Bool())
  val allocatedId = Input(UInt(LLCSourceMaxNumBitSize.W))
  val ack = Input(Valid(UInt(SoureceMaxNumBitSize.W)))
  val storeBase = Input(UInt(MMUAddrWidth.W))
  val storeTag = Input(UInt(8.W))
  val taskReady = Output(Bool())
  val done = Output(Bool())
  val request = Output(Valid(new CMemoryLoaderStoreRequestMeta))
  val pending = Output(Valid(new CMemoryLoaderStoreRequestMeta))
  val bankAddr = Output(Vec(CMatrixRegNBanks, Valid(UInt(log2Ceil(CMatrixRegBankNEntries).W))))
  val storeWords = Output(Vec(outsideDataWidth / 32, UInt(32.W)))
  val ackReady = Output(Bool())
}

// The inline text is the entire canonical CIRCT emission, never a hand-coded
// substitute for CML arithmetic. BlackBox removes its outer "io" prefix, so
// the nested io below matches the emitted io_* ports without rewriting SV.
class CMemoryLoaderStoreLoweredBlackBox(sv: String)(implicit p: Parameters)
    extends BlackBox with HasBlackBoxInline {
  override def desiredName: String = "CMemoryLoaderStoreNarrowTestTop"
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val io = new CMemoryLoaderStoreNarrowIO
  })
  setInline("CMemoryLoaderStoreCIRCTRelease.sv", sv)
}

class CMemoryLoaderStoreNarrowTestTop(val stride: Int, val loweredSv: Option[String] = None)
    (implicit p: Parameters) extends CuteModule {
  override def desiredName: String =
    if (loweredSv.nonEmpty) "CMemoryLoaderStoreCIRCTTestTop" else "CMemoryLoaderStoreNarrowTestTop"
  require(MMUAddrWidth <= 64 && MMUMaskWidth <= 64)
  require(SoureceMaxNumBitSize <= 64 && LLCSourceMaxNumBitSize <= 64)
  require(log2Ceil(CMatrixRegBankNEntries) <= 64)
  val io = IO(new CMemoryLoaderStoreNarrowIO)
  if (loweredSv.nonEmpty) {
    val lowered = Module(new CMemoryLoaderStoreLoweredBlackBox(loweredSv.get))
    lowered.io.clock := clock
    lowered.io.reset := reset.asBool
    io <> lowered.io.io
  } else {
    val harness = Module(new CMemoryLoaderStoreAddressHarness(
      storeProbe = true, externalAck = true, stride = stride, multiTileProbe = true))
    harness.io.start := io.start
    harness.io.transpose := io.transpose
    harness.io.rows := io.rows
    harness.io.columns := io.columns
    harness.io.memoryReady := io.memoryReady
    harness.io.allocatedId := io.allocatedId
    harness.io.ack := io.ack
    harness.io.storeBase.get := io.storeBase
    harness.io.storeTag.get := io.storeTag
    // This adapter is store-only. Do not expose unused wide child inputs or
    // load-data outputs to chiseltest6's pre-VlWide native bridge.
    harness.io.loadAddress := 0.U
    harness.io.memoryData := 0.U
    io.taskReady := harness.io.taskReady
    io.done := harness.io.done
    io.bankAddr := harness.io.bankAddr
    io.storeWords := harness.io.storeWords.get
    io.ackReady := harness.io.ackReady.get
    for ((narrow, full) <- Seq(
        (io.request, harness.io.request), (io.pending, harness.io.pending))) {
      narrow.valid := full.valid
      narrow.bits.RequestAddr := full.bits.RequestAddr
      narrow.bits.RequestMask := full.bits.RequestMask
      narrow.bits.RequestSourceID := full.bits.RequestSourceID
      narrow.bits.RequestType_isWrite := full.bits.RequestType_isWrite
      narrow.bits.RequestConherent := full.bits.RequestConherent
      narrow.bits.UseAllocatedSourceID := full.bits.UseAllocatedSourceID
    }
  }
}

class CMemoryLoaderStoreAddressSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "the legacy C memory loader store address generator"

  it should "issue split stores through real LocalMMU allocation without a valid feedback loop" in {
    implicit val p: Parameters = CMemoryLoaderStoreAddressTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(Matrix_MN = 4, EnableDifftest = false)
    }
    test(new CMemoryLoaderStoreAddressHarness(storeProbe = true,
        base = 0x103d, localMmuProbe = true)) { d =>
      d.io.start.poke(false.B); d.io.transpose.poke(false.B)
      d.io.rows.poke(5.U); d.io.columns.poke(17.U); d.io.memoryReady.poke(true.B)
      d.clock.step(3)
      d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
      var stores = 0; var cycle = 0
      while (!d.io.done.peek().litToBoolean && cycle < 1000) {
        d.io.memoryReady.poke((cycle%5 >= 2).B)
        if (d.io.request.valid.peek().litToBoolean) {
          assert((d.io.request.bits.RequestAddr.peek().litValue & 63) == 0)
          assert(d.io.request.bits.RequestMask.peek().litValue != 0)
          stores += 1
        }
        d.clock.step(); cycle += 1
      }
      assert(cycle < 1000 && stores == 20, s"real MMU stores=$stores cycles=$cycle")
      d.io.transportIdle.expect(true.B)
    }
  }

  it should "extract contained unaligned FP32 C input bytes using the original source owner" in {
    implicit val p: Parameters = CMemoryLoaderStoreAddressTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(Matrix_MN = 4, EnableDifftest = false)
    }
    test(new CMemoryLoaderStoreAddressHarness(loadProbe = true)) { d =>
      d.io.start.poke(false.B); d.io.transpose.poke(false.B)
      d.io.rows.poke(1.U); d.io.columns.poke(1.U); d.io.memoryReady.poke(true.B)
      d.io.loadAddress.poke(0x1000.U); d.io.memoryData.poke(0.U)
      d.clock.step(3)
      for ((offset, columns) <- Seq((0,1), (1,1), (37,1), (33,3), (60,1), (52,3))) {
        val bytes = columns*4
        val expected = (0 until bytes).foldLeft(BigInt(0))((v,i) =>
          v | (BigInt((i*17+31)&255) << (8*i)))
        val poison = ((BigInt(1)<<512)-1)
        val mask = ((BigInt(1)<<(bytes*8))-1) << (offset*8)
        d.io.loadAddress.poke((0x1000+offset).U); d.io.columns.poke(columns.U)
        d.io.memoryData.poke(((poison & ~mask) | (expected << (offset*8))).U)
        d.io.taskReady.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        var cycle = 0; var checked = false; var requests = 0
        while (!d.io.done.peek().litToBoolean && cycle < 200) {
          if (d.io.request.valid.peek().litToBoolean) {
            d.io.request.bits.RequestAddr.expect(0x1000.U); requests += 1
          }
          if (d.io.loadData(0).valid.peek().litToBoolean && !checked) {
            val data = d.io.loadData(0).bits.peek().litValue
            assert((data & ((BigInt(1)<<(bytes*8))-1)) == expected)
            d.io.loadMask(0).bits.expect(((BigInt(1)<<bytes)-1).U)
            checked = true
          }
          d.clock.step(); cycle += 1
        }
        assert(cycle < 200 && checked && requests == 1,
          s"C load offset=$offset N=$columns requests=$requests checked=$checked")
        d.clock.step(3)
        println(s"CML-CONTAINED-READ offset=$offset N=$columns PASS")
      }
    }
  }

  it should "split unaligned row bytes into owned lines and wait for every out-of-order ACK" in {
    implicit val params: Parameters = CMemoryLoaderStoreAddressTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
        Matrix_MN = 4, EnableDifftest = false)
    }
    // One elaboration covers original offsets33/41, crossing offsets61/63,
    // shared-line rows and full-width rows. None changes the frozen guest.
    test(new CMemoryLoaderStoreAddressHarness(storeProbe = true,
        externalAck = true, base = 0x1021, stride = 4)) { d =>
      d.io.start.poke(false.B); d.io.transpose.poke(false.B)
      d.io.memoryReady.poke(true.B); d.io.ack.valid.poke(false.B)
      d.io.ack.bits.poke(0.U); d.io.allocatedId.poke(0.U)
      d.io.rows.poke(1.U); d.io.columns.poke(1.U); d.clock.step(3)
      for ((rows, columns) <- Seq((1,1), (3,1), (8,1), (9,1), (5,17), (1,128))) {
        d.io.rows.poke(rows.U); d.io.columns.poke(columns.U)
        d.io.taskReady.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        val expected = collection.mutable.Map.empty[(BigInt, BigInt), BigInt]
        // Each logical collector beat may split into two physical requests.
        // Match by address and mask, and compare every stored byte, not padding.
        for (row <- 0 until rows; col <- 0 until columns by 16) {
          val addr = BigInt(0x1021 + row*4 + col*4)
          val live = math.min(16, columns-col) * 4
          val payload = (0 until live/4).foldLeft(BigInt(0)) { (v, lane) =>
            v | (BigInt(0x01000000 | (row << 12) | (col+lane)) << (lane*32))
          }
          val offset = (addr & 63).toInt
          val wideMask = ((BigInt(1) << live)-1) << offset
          val wideData = payload << (offset*8)
          for (part <- 0 until 2) {
            val mask = (wideMask >> (part*64)) & ((BigInt(1)<<64)-1)
            if (mask != 0) {
              val key = ((addr & ~BigInt(63)) + part*64, mask)
              assert(!expected.contains(key), s"ambiguous test fragment $key")
              expected(key) = wideData >> (part*512)
            }
          }
        }
        var source = 0
        var cycle = 0
        while (expected.nonEmpty && cycle < 4000) {
          d.io.memoryReady.poke((cycle % 7 >= 3).B)
          d.io.allocatedId.poke(source.U)
          d.io.done.expect(false.B)
          if (d.io.request.valid.peek().litToBoolean) {
            val addr = d.io.request.bits.RequestAddr.peek().litValue
            val mask = d.io.request.bits.RequestMask.peek().litValue
            val key = (addr, mask)
            assert(expected.contains(key), s"unexpected fragment M$rows/N$columns $key")
            val data = d.io.request.bits.RequestData.peek().litValue
            for (byte <- 0 until 64 if mask.testBit(byte)) {
              assert(((data >> (8*byte)) & 255) == ((expected(key) >> (8*byte)) & 255),
                s"fragment $key changed architectural byte$byte")
            }
            d.io.request.bits.RequestSourceID.expect(source.U)
            expected.remove(key); source += 1
          }
          d.clock.step(); cycle += 1
        }
        assert(expected.isEmpty && source > 0, s"missing fragments: $expected")
        // Completion must remain low until all original physical owners ACK,
        // including a crossing row's first fragment. Cache tests cover merging.
        for (id <- (0 until source).reverse) {
          d.io.memoryReady.poke(false.B)
          d.io.done.expect(false.B)
          d.clock.step(2)
          d.io.ack.bits.poke(id.U); d.io.ack.valid.poke(true.B)
          d.clock.step(); d.io.ack.valid.poke(false.B)
        }
        var drain = 0
        while (!d.io.done.peek().litToBoolean && drain < 12) { d.clock.step(); drain += 1 }
        d.io.done.expect(true.B); d.clock.step(3)
        println(s"CML-SPLIT M$rows/N$columns fragments=$source reverseAck=PASS")
      }
    }
  }

  private def checkAddresses(transpose: Boolean, expected: Seq[Int]): Unit = {
    test(new CMemoryLoaderStoreAddressHarness()(CMemoryLoaderStoreAddressTestConfig.params)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.transpose.poke(transpose.B)
      dut.io.rows.poke(64.U); dut.io.columns.poke(64.U); dut.io.memoryReady.poke(true.B)

      assert(dut.io.taskReady.peek().litToBoolean, "CML store did not become ready")
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      val observed = collection.mutable.ArrayBuffer.empty[Int]
      var cycles = 0
      while (observed.size < expected.size && cycles < 64) {
        val valid = dut.io.bankAddr.head.valid.peek().litToBoolean
        for (bank <- dut.io.bankAddr.indices) {
          assert(dut.io.bankAddr(bank).valid.peek().litToBoolean == valid,
            s"bank $bank read-valid diverged")
        }
        if (valid) {
          val address = dut.io.bankAddr.head.bits.peek().litValue.toInt
          for (bank <- dut.io.bankAddr.indices) {
            assert(dut.io.bankAddr(bank).bits.peek().litValue.toInt == address,
              s"bank $bank read address diverged")
          }
          observed += address
        }
        dut.clock.step()
        cycles += 1
      }

      assert(observed.toSeq == expected,
        s"store MatrixReg address sequence differed: expected=$expected observed=$observed")
    }
  }

  it should "skip unused physical columns between logical row groups" in {
    checkAddresses(transpose = false, Seq(0, 1, 2, 3, 4, 5, 6, 7, 64))
  }

  it should "use the physical row-group stride while traversing transposed blocks" in {
    checkAddresses(transpose = true, Seq(0, 64, 128, 192, 256, 320, 384, 448, 1))
  }

  it should "store small FP32 N with exact row bytes, tail flush, backpressure and completion at 2T" in {
    implicit val params: Parameters = CMemoryLoaderStoreAddressTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
        Matrix_MN = 4, EnableDifftest = false)
    }
    test(new CMemoryLoaderStoreAddressHarness(storeProbe = true)) { d =>
      d.io.start.poke(false.B); d.io.transpose.poke(false.B)
      d.io.rows.poke(1.U); d.io.columns.poke(1.U); d.io.memoryReady.poke(true.B)
      d.clock.step(3)
      for {
        (rows, columns) <- Seq((1,1), (5,1), (5,5), (5,17), (5,128))
        stall <- Seq(false, true)
      } {
        d.io.rows.poke(rows.U); d.io.columns.poke(columns.U)
        d.io.taskReady.expect(true.B)
        d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
        val scpExpected = for (m <- 0 until (rows+3)/4; n <- 0 until (columns+3)/4) yield m*32+n
        val scpSeen = collection.mutable.ArrayBuffer.empty[Int]
        val beatsPerRow = (columns+15)/16
        var stores = 0; var cycle = 0
        while (!d.io.done.peek().litToBoolean && cycle < 4000) {
          d.io.memoryReady.poke((!stall || cycle%7 >= 3).B)
          if (d.io.bankAddr.head.valid.peek().litToBoolean)
            scpSeen += d.io.bankAddr.head.bits.peek().litValue.toInt
          if (d.io.request.valid.peek().litToBoolean) {
            assert(stores < rows*beatsPerRow)
            // Collector sends every row of one bus-column group, before
            // proceeding to the next group of columns.
            val mGroup = stores / (4*beatsPerRow)
            val rowsHere = math.min(4, rows - mGroup*4)
            val inGroup = stores - mGroup*4*beatsPerRow
            val row = mGroup*4 + inGroup%rowsHere
            val column = (inGroup/rowsHere)*16
            val live = math.min(16, columns-column)
            d.io.request.bits.RequestAddr.expect((0x1000 + row*256 + column*4).U)
            d.io.request.bits.RequestMask.expect(((BigInt(1) << (live*4))-1).U)
            val data = d.io.request.bits.RequestData.peek().litValue
            for (lane <- 0 until live) {
              val expected = 0x01000000 | (row << 12) | (column+lane)
              assert(((data >> (lane*32)) & BigInt("ffffffff",16)) == expected,
                s"M$rows/N$columns row$row col${column+lane} data=$data")
            }
            stores += 1
          }
          d.clock.step(); cycle += 1
        }
        assert(cycle < 4000 && stores == rows*beatsPerRow,
          s"store M$rows/N$columns stall$stall stores=$stores cycles=$cycle")
        assert(scpSeen.toSeq == scpExpected, s"scp $scpSeen != $scpExpected")
        println(s"CML-PARTIAL M$rows/N$columns stall$stall scp=${scpSeen.size} stores=$stores cycles=$cycle")
        d.clock.step(3)
      }
    }
  }

  private def checkTwo2T64StoreTiles(circtRelease: Boolean): Unit = {
    implicit val p: Parameters = CMemoryLoaderStoreAddressTestConfig.params.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_2Tops_64SCP.copy(EnableDifftest = false)
    }
    require(p(CuteParamsKey).Tensor_MN == 64 && p(CuteParamsKey).Matrix_MN == 4)
    val rows = 64
    val columns = 64
    val rowPitch = 2048
    val rowBytes = columns * 4
    val fullMask = (BigInt(1) << 64) - 1
    val sourceCount = p(CuteParamsKey).LLCSourceMaxNum
    require(sourceCount == 64)
    // Exact current harness row/column pattern, with an independent high-byte
    // tile tag so a stale collector/FIFO cannot make the second task pass.
    def word(tag: Int, row: Int, column: Int): BigInt =
      BigInt(0x01000000 | (tag << 24) | (row << 12) | column)
    def sentinel(address: BigInt): Int =
      ((BigInt("7fc56789", 16) >> ((address.toInt & 3) * 8)) & 255).toInt

    // A separate opt-in case leaves the existing PASS case's DUT unchanged.
    // Both cases use the ONE driver/byte oracle/ACK scoreboard below. Merely
    // passing FirtoolOption to chiseltest would not exercise this emitted SV.
    val loweredSv = if (circtRelease) {
      val firtoolOpts = Array(
        "-O=release",
        "--disable-annotation-unknown",
        "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")
      val sv = _root_.circt.stage.ChiselStage.emitSystemVerilog(
        new CMemoryLoaderStoreNarrowTestTop(stride = rowPitch), firtoolOpts = firtoolOpts)
      require(sv.nonEmpty, "CIRCT returned no CML SystemVerilog")
      val hash = java.security.MessageDigest.getInstance("SHA-256")
        .digest(sv.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        .map(b => f"${b & 255}%02x").mkString
      println(s"CML-CIRCT-RELEASE svSHA256=$hash firtoolOpts=${firtoolOpts.mkString(" ")}")
      Some(sv)
    } else None

    test(new CMemoryLoaderStoreNarrowTestTop(stride = rowPitch, loweredSv = loweredSv))
      .withAnnotations(if (circtRelease || sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
        Seq(VerilatorBackendAnnotation,
          chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) else Seq.empty) { d =>
        d.io.start.poke(false.B); d.io.transpose.poke(false.B)
        d.io.rows.poke(rows.U); d.io.columns.poke(columns.U)
        d.io.memoryReady.poke(false.B); d.io.allocatedId.poke(0.U)
        d.io.ack.valid.poke(false.B); d.io.ack.bits.poke(0.U)
        d.io.storeBase.poke(0x1000.U); d.io.storeTag.poke(0.U)
        d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); d.clock.step(2)

        // This byte image applies accepted CML requests, not a cache or CPU.
        // Missing bytes read as the guest's poison; every written byte must be
        // in one of the two active 256B intervals, with exactly one writer.
        val written = collection.mutable.Map.empty[BigInt, Int]
        val actualBytes = collection.mutable.Map.empty[BigInt, Int]
        val expectedBytes = collection.mutable.Map.empty[BigInt, Int]
        var totalRequests = 0
        var totalAcks = 0

        for ((tileBase, tileTag) <- Vector((0x1000, 2), (0x1100, 4))) {
          d.io.taskReady.expect(true.B)
          d.io.done.expect(false.B)
          d.io.storeBase.poke(tileBase.U); d.io.storeTag.poke(tileTag.U)
          d.io.start.poke(true.B); d.clock.step(); d.io.start.poke(false.B)
          // Both the real CML base and the fixture's data tag must be captured
          // by the start handshake, not taken live during later row reads.
          d.io.storeBase.poke(0xdead0000L.U); d.io.storeTag.poke(0xff.U)

          val remaining = collection.mutable.Map.empty[BigInt, BigInt]
          for (row <- 0 until rows; column <- 0 until columns by 16) {
            val address = BigInt(tileBase + row * rowPitch + column * 4)
            val data = (0 until 16).foldLeft(BigInt(0)) { (bits, lane) =>
              bits | (word(tileTag, row, column + lane) << (32 * lane))
            }
            remaining(address) = data
          }
          val free = collection.mutable.ArrayBuffer((0 until sourceCount): _*)
          val pending = collection.mutable.ArrayBuffer.empty[Int]
          var offeredAck: Option[Int] = None
          var requests = 0
          var acks = 0
          var scpReads = 0
          var cycles = 0
          var stalledRequests = 0
          var creditStalls = 0
          var reverseAcks = 0
          var terminalAckHold = 0
          var allowAcks = false
          var held: Option[(BigInt, BigInt, Vector[BigInt])] = None

          while (!d.io.done.peek().litToBoolean && cycles < 6000) {
            if (free.isEmpty) creditStalls += 1
            if (creditStalls >= 3) allowAcks = true
            val ready = free.nonEmpty && cycles % 7 >= 3 && requests < rows * 4
            val allocated = free.headOption.getOrElse(0)
            d.io.memoryReady.poke(ready.B); d.io.allocatedId.poke(allocated.U)
            // Fill all source credits first, then ACK newest owners in gapped
            // reverse order. Keep the terminal owner for seven extra cycles.
            val holdLast = requests == rows * 4 && pending.length == 1 && terminalAckHold < 7
            if (holdLast) terminalAckHold += 1
            if (offeredAck.isEmpty && allowAcks && pending.nonEmpty && !holdLast && cycles % 5 == 4)
              offeredAck = Some(pending.last)
            d.io.ack.valid.poke(offeredAck.nonEmpty.B)
            d.io.ack.bits.poke(offeredAck.getOrElse(0).U)
            if (pending.nonEmpty || requests < rows * 4) d.io.done.expect(false.B)
            d.io.taskReady.expect(false.B)
            val ackFire = offeredAck.nonEmpty && d.io.ackReady.peek().litToBoolean

            val readValid = d.io.bankAddr.head.valid.peek().litToBoolean
            for (bank <- d.io.bankAddr.indices) {
              assert(d.io.bankAddr(bank).valid.peek().litToBoolean == readValid)
              if (readValid) d.io.bankAddr(bank).bits.expect(scpReads.U)
            }
            if (readValid) {
              assert(scpReads < 256, s"extra MatrixReg read tileBase=$tileBase")
              scpReads += 1
            }

            val valid = d.io.pending.valid.peek().litToBoolean
            val words = if (valid) d.io.storeWords.map(_.peek().litValue).toVector
              else Vector.fill(16)(BigInt(0))
            val frame = if (valid) Some((d.io.pending.bits.RequestAddr.peek().litValue,
              d.io.pending.bits.RequestMask.peek().litValue, words)) else None
            held.foreach(previous => assert(frame.contains(previous),
              s"pending address/mask/data changed across backpressure: tileBase=$tileBase cycle=$cycles"))
            held = if (valid && !ready) frame else None
            if (valid && !ready) stalledRequests += 1

            if (d.io.request.valid.peek().litToBoolean) {
              assert(ready && requests < 256 && free.nonEmpty)
              val address = d.io.request.bits.RequestAddr.peek().litValue
              val mask = d.io.request.bits.RequestMask.peek().litValue
              // Also validate the canonical collector order independently of
              // the address-keyed coverage map.
              val group = requests / 16
              val inGroup = requests % 16
              val row = group * 4 + inGroup % 4
              val column = (inGroup / 4) * 16
              assert(address == BigInt(tileBase + row * rowPitch + column * 4),
                s"tileBase=$tileBase request=$requests address=0x${address.toString(16)}")
              assert(mask == fullMask && remaining.contains(address),
                s"unexpected or duplicate request address=0x${address.toString(16)} mask=$mask")
              d.io.request.bits.RequestSourceID.expect(allocated.U)
              d.io.request.bits.RequestType_isWrite.expect(true.B)
              d.io.request.bits.RequestConherent.expect(true.B)
              d.io.request.bits.UseAllocatedSourceID.expect(true.B)
              assert(!pending.contains(allocated), s"source reused before ACK: $allocated")
              val actual = words.zipWithIndex.foldLeft(BigInt(0)) { case (bits, (value, lane)) =>
                bits | (value << (32 * lane))
              }
              assert(actual == remaining(address),
                s"tileBase=$tileBase row=$row column=$column: expected=${remaining(address)} actual=$actual")
              for (byte <- 0 until 64 if mask.testBit(byte)) {
                val byteAddress = address + byte
                val delta = byteAddress - tileBase
                assert(delta >= 0 && delta / rowPitch < rows && delta % rowPitch < rowBytes,
                  s"write outside active row interval: $byteAddress")
                assert(!written.contains(byteAddress), s"byte overwritten: $byteAddress")
                written(byteAddress) = 1
                actualBytes(byteAddress) = ((actual >> (8 * byte)) & 255).toInt
              }
              remaining.remove(address)
              free.remove(0)
              pending += allocated
              requests += 1
            }
            if (ackFire) {
              val source = offeredAck.get
              assert(pending.contains(source), s"ACK without an original owner: $source")
              if (pending.head != source) reverseAcks += 1
              pending.remove(pending.indexOf(source))
              free += source
              offeredAck = None
              acks += 1
            }
            d.clock.step(); cycles += 1
          }
          assert(cycles < 6000 && requests == 256 && acks == 256 && scpReads == 256,
            s"tileBase=$tileBase req=$requests ack=$acks scp=$scpReads cycles=$cycles")
          assert(remaining.isEmpty && pending.isEmpty && free.distinct.size == sourceCount)
          assert(stalledRequests > 0 && creditStalls >= 3 && reverseAcks > 0 && terminalAckHold == 7)
          d.io.done.expect(true.B)
          totalRequests += requests; totalAcks += acks
          for (row <- 0 until rows; column <- 0 until columns; byte <- 0 until 4) {
            val address = BigInt(tileBase + row * rowPitch + column * 4 + byte)
            expectedBytes(address) = ((word(tileTag, row, column) >> (8 * byte)) & 255).toInt
            assert(written.get(address).contains(1), s"missing active byte: $address")
          }
          // Check the whole stride, including the adjacent tile before it is
          // active, row padding, and guard bytes on both sides of the image.
          for (address <- (0x1000 - 64) until (0x1000 + rows * rowPitch + 64)) {
            val key = BigInt(address)
            assert(actualBytes.getOrElse(key, sentinel(key)) ==
              expectedBytes.getOrElse(key, sentinel(key)), s"byte image mismatch address=0x${key.toString(16)}")
            assert(written.getOrElse(key, 0) == (if (expectedBytes.contains(key)) 1 else 0),
              s"unexpected/missing byte coverage address=0x${key.toString(16)}")
          }
          println(s"CML-2T64-TWO-TILE base=0x${tileBase.toHexString} tag=$tileTag " +
            s"scp=$scpReads requests=$requests acks=$acks bytes=${rows * rowBytes} " +
            s"reverseAcks=$reverseAcks creditStalls=$creditStalls heldLastAck=$terminalAckHold cycles=$cycles")
          d.io.ack.valid.poke(false.B); d.io.memoryReady.poke(false.B)
          d.clock.step()
          d.io.taskReady.expect(true.B)
          for (_ <- 0 until 3) {
            d.io.done.expect(false.B); d.io.request.valid.expect(false.B)
            d.io.pending.valid.expect(false.B); d.io.bankAddr.head.valid.expect(false.B)
            d.clock.step()
          }
          // No reset: the next task must reinitialize its FIFO/collector,
          // counters and base while retaining only acknowledged ownership.
        }
        assert(totalRequests == 512 && totalAcks == 512 && written.size == 32768)
        println("CML-2T64-TWO-TILE owner-level PASS requests=512 acks=512 bytes=32768 coherence=NOT_TESTED")
      }
  }

  it should "2T64 consecutive FP32 tiles cover col0 and col64 with owned reverse ACKs" in {
    checkTwo2T64StoreTiles(circtRelease = false)
  }

  it should "CIRCT release lowering preserves two 2T64 FP32 tiles with owned reverse ACKs" in {
    checkTwo2T64StoreTiles(circtRelease = true)
  }
}

// ChiselTest6's native harness predates Verilator5.052's VlWide API. Expose
// only 64-bit words to that harness; concatenate them at the unchanged Slice
// boundary. No cache, SRAM, clock gate, handshake or scheduling is replaced.
class MaskedStoreDataBeat(implicit val p: Parameters) extends Bundle
    with xscache.chi.HasCHIMsgParameters {
  val opcode = UInt(DAT_OPCODE_WIDTH.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val dbID = UInt(DBID_WIDTH.W)
  val srcID = UInt(SRCID_WIDTH.W)
  val homeNID = UInt(HOMENID_WIDTH.W)
  val dataID = UInt(DATAID_WIDTH.W)
  val resp = UInt(RESP_WIDTH.W)
  val respErr = UInt(RESPERR_WIDTH.W)
  val be = UInt(BE_WIDTH.W)
  val data = Vec(DATA_WIDTH/64, UInt(64.W))
}

class MaskedStoreSliceTestTop(implicit p: Parameters) extends L2Module {
  val cache = Module(new Slice)
  val io = IO(new Bundle {
    val in = new Bundle {
      val a = Flipped(Decoupled(new Bundle {
        val opcode = UInt(4.W); val param = UInt(3.W); val size = UInt(3.W)
        val source = UInt(sourceIdBits.W); val address = UInt(fullAddressBits.W)
        val mask = UInt(beatBytes.W); val data = Vec(beatBytes/8, UInt(64.W))
        val matrix = UInt(2.W); val ameIndex = UInt(64.W)
      }))
      val b = Decoupled(new Bundle { val address = UInt(fullAddressBits.W) })
      val c = Flipped(Decoupled(new Bundle {
        val opcode = UInt(3.W); val param = UInt(3.W); val size = UInt(3.W)
        val address = UInt(fullAddressBits.W); val data = Vec(beatBytes/8, UInt(64.W))
      }))
      val d = Decoupled(new Bundle {
        val opcode = UInt(4.W); val sink = UInt(edgeIn.bundle.sinkBits.W)
        val source = UInt(sourceIdBits.W)
        val denied = Bool(); val data = Vec(beatBytes/8, UInt(64.W))
      })
      val e = Flipped(Decoupled(new TLBundleE(edgeIn.bundle)))
    }
    val out = new Bundle {
      val tx = new Bundle {
        val req = Decoupled(new xscache.chi.CHIREQ)
        val rsp = Decoupled(new xscache.chi.CHIRSP)
        val dat = Decoupled(new MaskedStoreDataBeat)
      }
      val rx = new Bundle {
        val rsp = Flipped(Decoupled(new xscache.chi.CHIRSP))
        val dat = Flipped(Decoupled(new MaskedStoreDataBeat))
        val snp = Flipped(Decoupled(new xscache.chi.CHISNP))
      }
    }
    val l1Hint = Decoupled(new L2ToL1HintInsideL2)
    val error = Decoupled(new L2CacheErrorInfo)
    val matrix = Decoupled(new Bundle {
      val source = UInt(64.W)
      val data = Vec(blockBytes/8, UInt(64.W))
    })
  })
  cache.io.sliceId := 0.U
  cache.io_pCrd.foreach(_.grant := false.B)
  cache.io.matrixDataOut.get.ready := io.matrix.ready
  io.matrix.valid := cache.io.matrixDataOut.get.valid
  io.matrix.bits.source := cache.io.matrixDataOut.get.bits.sourceId
  io.matrix.bits.data := cache.io.matrixDataOut.get.bits.data.asUInt.asTypeOf(io.matrix.bits.data)
  io.error <> cache.io.error
  io.l1Hint <> cache.io.l1Hint
  io.out.tx.req <> cache.io.out.tx.req
  io.out.tx.rsp <> cache.io.out.tx.rsp
  cache.io.out.rx.rsp <> io.out.rx.rsp
  cache.io.out.rx.snp <> io.out.rx.snp
  cache.io.in.e <> io.in.e
  cache.io.in.a.valid := io.in.a.valid
  io.in.a.ready := cache.io.in.a.ready
  cache.io.in.a.bits := 0.U.asTypeOf(cache.io.in.a.bits)
  cache.io.in.a.bits.opcode := io.in.a.bits.opcode
  cache.io.in.a.bits.param := io.in.a.bits.param
  cache.io.in.a.bits.size := io.in.a.bits.size
  cache.io.in.a.bits.source := io.in.a.bits.source
  cache.io.in.a.bits.address := io.in.a.bits.address
  cache.io.in.a.bits.mask := io.in.a.bits.mask
  cache.io.in.a.bits.data := io.in.a.bits.data.asUInt
  cache.io.in.a.bits.user.lift(MatrixKey).get := io.in.a.bits.matrix
  cache.io.in.a.bits.user.lift(AmeIndexKey).get := io.in.a.bits.ameIndex
  cache.io.in.c.valid := io.in.c.valid
  io.in.c.ready := cache.io.in.c.ready
  cache.io.in.c.bits := 0.U.asTypeOf(cache.io.in.c.bits)
  cache.io.in.c.bits.opcode := io.in.c.bits.opcode
  cache.io.in.c.bits.param := io.in.c.bits.param
  cache.io.in.c.bits.size := io.in.c.bits.size
  cache.io.in.c.bits.address := io.in.c.bits.address
  cache.io.in.c.bits.data := io.in.c.bits.data.asUInt
  io.in.b.valid := cache.io.in.b.valid
  cache.io.in.b.ready := io.in.b.ready
  io.in.b.bits.address := cache.io.in.b.bits.address
  io.in.d.valid := cache.io.in.d.valid
  cache.io.in.d.ready := io.in.d.ready
  io.in.d.bits.opcode := cache.io.in.d.bits.opcode
  io.in.d.bits.sink := cache.io.in.d.bits.sink
  io.in.d.bits.source := cache.io.in.d.bits.source
  io.in.d.bits.denied := cache.io.in.d.bits.denied
  io.in.d.bits.data := cache.io.in.d.bits.data.asTypeOf(io.in.d.bits.data)
  cache.io.out.rx.dat.valid := io.out.rx.dat.valid
  io.out.rx.dat.ready := cache.io.out.rx.dat.ready
  cache.io.out.rx.dat.bits := 0.U.asTypeOf(cache.io.out.rx.dat.bits)
  val rx = cache.io.out.rx.dat.bits
  val input = io.out.rx.dat.bits
  rx.opcode := input.opcode; rx.txnID := input.txnID; rx.dbID := input.dbID
  rx.srcID := input.srcID; rx.homeNID := input.homeNID; rx.dataID := input.dataID
  rx.resp := input.resp; rx.respErr := input.respErr; rx.be := input.be
  rx.data := input.data.asUInt
  io.out.tx.dat.valid := cache.io.out.tx.dat.valid
  cache.io.out.tx.dat.ready := io.out.tx.dat.ready
  val tx = cache.io.out.tx.dat.bits
  val output = io.out.tx.dat.bits
  output.opcode := tx.opcode; output.txnID := tx.txnID; output.dbID := tx.dbID
  output.srcID := tx.srcID; output.homeNID := tx.homeNID; output.dataID := tx.dataID
  output.resp := tx.resp; output.respErr := tx.respErr; output.be := tx.be
  output.data := tx.data.asTypeOf(output.data)
}

// Exercise the real cache transaction, directory, data SRAM, PutBuffer and
// MSHRs. The bounded CHI/L1 peer below is a test stimulus, not a store path.
class MatrixMaskedStoreCacheSpec extends AnyFlatSpec with ChiselScalatestTester {
  private class Opcodes(val p: Parameters) extends HasCHIOpcodes
  private def pokeWords(words: Vec[UInt], value: BigInt): Unit =
    words.zipWithIndex.foreach { case (w,i) => w.poke(((value >> (i*64)) & ((BigInt(1)<<64)-1)).U) }
  private def peekWords(words: Vec[UInt]): BigInt =
    words.zipWithIndex.foldLeft(BigInt(0)) { case (v,(w,i)) => v | (w.peek().litValue << (i*64)) }
  private def parameters: Parameters = {
    val client = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(name = "masked-store-test",
        sourceId = IdRange(0, 16), supportsProbe = TransferSizes(64,64))),
      requestFields = Seq(MatrixField(2), AmeIndexField(), AmeChannelField()))
    val manager = TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0, 0xffffffffL)),
        regionType = RegionType.CACHED, supportsGet = TransferSizes(64,64),
        supportsPutFull = TransferSizes(64,64), supportsPutPartial = TransferSizes(64,64),
        supportsAcquireB = TransferSizes(64,64), supportsAcquireT = TransferSizes(64,64))),
      beatBytes = 32, endSinkId = 16,
      requestKeys = Seq(MatrixKey, AmeIndexKey, AmeChannelKey))
    val edge = new TLEdgeIn(client, manager, Parameters.empty, UnlocatableSourceInfo)
    new Config((_, _, _) => {
      case EdgeInKey => edge
      case EdgeOutKey => new TLEdgeOut(client, manager, Parameters.empty, UnlocatableSourceInfo)
      case BankBitsKey => 0
      case L2NBanksKey => 1
      case SliceIdKey => 0
      case freechips.rocketchip.tile.MaxHartIdBits => 1
      case utility.LogUtilsOptionsKey => utility.LogUtilsOptions(false, false, false)
      case utility.PerfCounterOptionsKey => utility.PerfCounterOptions(
        false, false, utility.XSPerfLevel.VERBOSE, 0)
      case L2ParamKey => L2Param(sets = 16, ways = 2, mshrs = 4,
        clientCaches = Seq(L1Param()), replacement = "plru",
        enablePerf = false, enableRollingDB = false, enableTLLog = false,
        enableCHILog = false, elaboratedTopDown = false, enableMonitor = false)
      case EnableMatrix => true
      case EnableL2ClockGate => false
      case CHIDataCheckKey => "none"
      case CHIPoisonKey => false
    })
  }

  private def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
    case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
    case _ =>
  }

  it should "honor each way mask when splitting into one-way SRAMs" in {
    test(new xscache.coupledL2.utils.SplittedSRAM(UInt(24.W),
        set = 16, way = 2, waySplit = 2, singlePort = true,
        readMCP2 = false, shouldReset = true)) { d =>
      zeroInputs(d.io); d.clock.step(20)
      val expected = Array.fill(2)(BigInt(0))
      for ((mask, index) <- Seq(1,2,0,1,3,2,0).zipWithIndex) {
        val data = Seq(BigInt(0x1200+index), BigInt(0xab00+index))
        d.io.w.req.valid.poke(true.B); d.io.w.req.bits.setIdx.poke(0.U)
        d.io.w.req.bits.waymask.get.poke(mask.U)
        data.zipWithIndex.foreach { case (x,i) => d.io.w.req.bits.data(i).poke(x.U) }
        d.io.r.req.valid.poke(true.B); d.io.r.req.bits.setIdx.poke(0.U)
        d.io.r.req.ready.expect(false.B)
        d.clock.step()
        for (i <- 0 until 2 if (mask & (1<<i)) != 0) expected(i) = data(i)
        d.io.w.req.valid.poke(false.B); d.io.r.req.ready.expect(true.B)
        d.clock.step()
        expected.zipWithIndex.foreach { case (x,i) => d.io.r.resp.data(i).expect(x.U) }
        d.io.r.req.valid.poke(false.B); d.clock.step()
      }
    }
  }

  it should "keep internal CMO tasks independent of blocked or stale external partial opcodes" in {
    implicit val p: Parameters = parameters.alterPartial {
      case L2ParamKey => parameters(L2ParamKey).copy(enableL2Flush = true)
    }
    test(new SinkA) { d =>
      zeroInputs(d.io); d.clock.step(2)
      d.io.task.ready.poke(false.B); d.io.putBufWrite.ready.poke(true.B)
      d.io.a.bits.opcode.poke(1.U); d.io.a.bits.address.poke(0x2000.U)
      d.io.a.bits.size.poke(6.U); d.io.a.bits.mask.poke(15.U)
      d.io.a.bits.user.lift(MatrixKey).get.poke(1.U)
      d.io.a.valid.poke(false.B); d.io.cmoAll.get.l2Flush.poke(true.B)
      d.clock.step()
      for (valid <- Seq(false, true)) {
        d.io.a.valid.poke(valid.B)
        d.io.task.valid.expect(true.B)
        d.io.task.bits.cmoAll.expect(true.B)
        d.io.task.bits.putPartial.expect(false.B)
        d.io.a.ready.expect(false.B)
        d.clock.step(3)
      }
    }
  }

  it should "deny corrupt old-line data and retain its MSHR until terminal commit" in {
    implicit val p: Parameters = parameters
    test(new MSHR) { d =>
      zeroInputs(d.io); d.clock.step(2)
      d.io.alloc.bits.state.elements.values.foreach {
        case b: Bool => b.poke(true.B)
        case _ =>
      }
      d.io.alloc.bits.state.s_refill.poke(false.B)
      d.io.alloc.bits.task.channel.poke(1.U)
      d.io.alloc.bits.task.opcode.poke(1.U)
      d.io.alloc.bits.task.putPartial.poke(true.B)
      d.io.alloc.bits.dirResult.hit.poke(true.B)
      d.io.alloc.bits.dirResult.meta.state.poke(3.U)
      d.io.alloc.valid.poke(true.B); d.clock.step(); d.io.alloc.valid.poke(false.B)
      d.io.tasks.mainpipe.valid.expect(false.B)
      d.io.partialReadDone.poke(true.B); d.io.partialReadCorrupt.poke(true.B)
      d.clock.step(); d.io.partialReadDone.poke(false.B)
      d.io.tasks.mainpipe.valid.expect(true.B)
      d.io.tasks.mainpipe.bits.denied.expect(true.B)
      d.io.tasks.mainpipe.bits.corrupt.expect(false.B)
      d.io.tasks.mainpipe.bits.dsWen.expect(false.B)
      d.io.tasks.mainpipe.bits.metaWen.expect(false.B)
      d.io.tasks.mainpipe.ready.poke(true.B); d.clock.step()
      d.io.tasks.mainpipe.ready.poke(false.B); d.clock.step(4)
      d.io.msInfo.valid.expect(true.B)
      d.io.msInfo.bits.willFree.expect(false.B)
      d.io.partialWriteDone.poke(true.B); d.clock.step()
      d.io.partialWriteDone.poke(false.B); d.clock.step(2)
      d.io.msInfo.valid.expect(false.B)
    }
  }

  it should "allow pre-ownership snoops but protect the partial merge and local-probe windows" in {
    implicit val p: Parameters = parameters
    val op = new Opcodes(p)
    test(new RXSNP) { d =>
      zeroInputs(d.io); d.clock.step(2)
      d.io.task.ready.poke(false.B)
      val owner = d.io.msInfo(0)
      owner.valid.poke(true.B)
      owner.bits.set.poke(0.U); owner.bits.reqTag.poke(8.U)
      owner.bits.dirHit.poke(true.B); owner.bits.meta.state.poke(3.U)
      owner.bits.s_cmoresp.poke(true.B)
      owner.bits.isPartialPut.poke(true.B)
      owner.bits.w_rprobeacklast.poke(true.B)
      owner.bits.w_grantfirst.poke(false.B)
      d.io.rxsnp.bits.addr.poke((0x2000/8).U)
      d.io.rxsnp.bits.opcode.poke(op.SnpUnique)
      d.io.rxsnp.valid.poke(true.B); d.clock.step()
      d.io.rxsnp.valid.poke(false.B); d.clock.step()
      d.io.task.valid.expect(true.B) // Home may need this snoop to finish ReadUnique.
      owner.bits.w_grantfirst.poke(true.B)
      for (released <- Seq(false, true)) {
        owner.bits.w_releaseack.poke(released.B)
        d.io.task.valid.expect(false.B)
        d.clock.step(3)
      }
      owner.bits.willFree.poke(true.B)
      d.io.task.valid.expect(true.B) // Commit completed; permit the queued snoop.
      owner.bits.willFree.poke(false.B); owner.bits.w_grantfirst.poke(false.B)
      owner.bits.w_rprobeacklast.poke(false.B)
      d.io.task.valid.expect(false.B)
      owner.bits.w_rprobeacklast.poke(true.B)
      d.io.task.valid.expect(true.B)
      d.io.task.ready.poke(true.B); d.clock.step()
      d.io.task.valid.expect(false.B)
    }
  }

  it should "retain the original partial owner across replacement retry until actual commit" in {
    implicit val p: Parameters = parameters
    test(new MSHR) { d =>
      zeroInputs(d.io); d.clock.step(2)
      d.io.alloc.bits.state.elements.values.foreach {
        case b: Bool => b.poke(true.B)
        case _ =>
      }
      d.io.alloc.bits.state.s_refill.poke(false.B)
      d.io.alloc.bits.state.w_replResp.poke(false.B)
      d.io.alloc.bits.task.channel.poke(1.U)
      d.io.alloc.bits.task.opcode.poke(1.U)
      d.io.alloc.bits.task.putPartial.poke(true.B)
      d.io.alloc.bits.task.bufIdx.poke(1.U)
      d.io.alloc.bits.task.tag.poke(8.U)
      d.io.alloc.valid.poke(true.B); d.clock.step(); d.io.alloc.valid.poke(false.B)
      d.io.tasks.mainpipe.valid.expect(true.B)
      d.io.tasks.mainpipe.bits.bufIdx.expect(1.U)
      d.io.tasks.mainpipe.ready.poke(true.B); d.clock.step()
      d.io.tasks.mainpipe.ready.poke(false.B)
      d.io.replResp.valid.poke(true.B); d.io.replResp.bits.retry.poke(true.B)
      d.clock.step(); d.io.replResp.valid.poke(false.B)
      d.io.msInfo.valid.expect(true.B)
      d.io.tasks.mainpipe.valid.expect(true.B)
      d.io.tasks.mainpipe.bits.bufIdx.expect(1.U)
      d.io.tasks.mainpipe.bits.putPartial.expect(true.B)
      d.io.tasks.mainpipe.ready.poke(true.B); d.clock.step()
      d.io.tasks.mainpipe.ready.poke(false.B)
      d.io.replResp.valid.poke(true.B); d.io.replResp.bits.retry.poke(false.B)
      d.io.replResp.bits.meta.state.poke(0.U)
      d.clock.step(); d.io.replResp.valid.poke(false.B); d.clock.step(3)
      d.io.msInfo.valid.expect(true.B)
      d.io.msInfo.bits.willFree.expect(false.B)
      d.io.partialWriteDone.poke(true.B); d.clock.step()
      d.io.partialWriteDone.poke(false.B); d.clock.step(2)
      d.io.msInfo.valid.expect(false.B)
    }
  }

  for (mcp2 <- Seq(true, false)) {
  it should s"preserve neighboring bytes across real hit, miss, upgrade, dirty-probe and replacement transactions MCP2=$mcp2" in {
    val baseParams = parameters
    implicit val p: Parameters = baseParams.alterPartial {
      case L2ParamKey => baseParams(L2ParamKey).copy(enableMCP2 = mcp2)
    }
    val op = new Opcodes(p)
    test(new MaskedStoreSliceTestTop).withAnnotations(Seq(VerilatorBackendAnnotation,
        chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t"))) ++
        (if (sys.env.get("CUTE_TEST_MASKED_WAVE").contains("1"))
          Seq(WriteVcdAnnotation) else Seq.empty)) { d =>
      zeroInputs(d.io)
      d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B)
      d.io.in.b.ready.poke(true.B); d.io.in.d.ready.poke(true.B)
      d.io.l1Hint.ready.poke(true.B); d.io.error.ready.poke(true.B)
      d.io.out.tx.req.ready.poke(true.B); d.io.out.tx.rsp.ready.poke(true.B)
      d.io.out.tx.dat.ready.poke(true.B)
      d.clock.step(24)
      val memory = collection.mutable.Map.empty[BigInt, BigInt]
      val l1 = collection.mutable.Map.empty[BigInt, BigInt]
      val returns = collection.mutable.ArrayBuffer.empty[(Int, BigInt, Int, Boolean)]
      val matrixReturns = collection.mutable.ArrayBuffer.empty[(BigInt, BigInt)]
      val reads = collection.mutable.ArrayBuffer.empty[(BigInt, Int)]
      val makeUniques = collection.mutable.ArrayBuffer.empty[BigInt]
      val probeAddresses = collection.mutable.ArrayBuffer.empty[BigInt]
      val probeDataReplies = collection.mutable.ArrayBuffer.empty[BigInt]
      val fullPutBranch = BigInt(0xb000)
      // Monitor real TL handshakes, not an alternate store/memory path.
      // Sources 1..8 belong to the burst, 9/10 to the later coherence checks.
      // Retain retired owners to catch a duplicate ACK even during readback.
      val fullPutOwners = collection.mutable.Map.empty[Int, (BigInt, BigInt)]
      val fullPutBeats = collection.mutable.Map.empty[Int, Int]
      val fullPutAcked = collection.mutable.Set.empty[Int]
      var fullPutBurstActive = false
      var fullPutPeakOutstanding = 0
      var fullPutBurstFirstAccept = -1
      var fullPutBurstLastAck = -1
      val byteMask = (BigInt(1)<<256)-1
      var ticks = 0
      var aAccepted = false
      var eAccepted = false
      case class DataBeat(due: Int, txn: BigInt, beat: Int, data: BigInt, shared: Boolean)
      val incoming = collection.mutable.Queue.empty[DataBeat]
      case class ProbeBeat(addr: BigInt, beat: Int, data: Option[BigInt])
      val probes = collection.mutable.Queue.empty[ProbeBeat]
      case class Response(txn: BigInt, opcode: Int, db: BigInt)
      val responses = collection.mutable.Queue.empty[Response]
      val writebacks = collection.mutable.Map.empty[BigInt, BigInt]
      def pattern(seed: Int): BigInt =
        (0 until 64).foldLeft(BigInt(0))((v,i) => v | (BigInt((seed+i*13)&255) << (i*8)))
      def line(addr: BigInt): BigInt = memory.getOrElseUpdate(addr, pattern(addr.toInt))
      def step(): Unit = {
        d.io.in.d.ready.poke((ticks%7 >= 2).B)
        d.io.matrix.ready.poke((ticks%9 >= 5).B)
        d.io.out.tx.req.ready.poke((ticks%5 != 0).B)
        d.io.out.tx.rsp.ready.poke(true.B); d.io.out.tx.dat.ready.poke(true.B)
        val dat = incoming.headOption.filter(_.due <= ticks)
        d.io.out.rx.dat.valid.poke(dat.nonEmpty.B)
        dat.foreach { x =>
          val b = d.io.out.rx.dat.bits
          b.opcode.poke(op.CompData); b.txnID.poke(x.txn.U); b.dbID.poke(x.txn.U)
          b.srcID.poke(1.U); b.homeNID.poke(1.U); b.dataID.poke((x.beat*2).U)
          b.resp.poke((if (x.shared) 1 else 2).U); b.respErr.poke(0.U)
          b.be.poke(((BigInt(1)<<32)-1).U); pokeWords(b.data, x.data)
        }
        d.io.out.rx.rsp.valid.poke(responses.nonEmpty.B)
        responses.headOption.foreach { x =>
          d.io.out.rx.rsp.bits.txnID.poke(x.txn.U)
          d.io.out.rx.rsp.bits.opcode.poke(x.opcode.U)
          d.io.out.rx.rsp.bits.dbID.poke(x.db.U)
          d.io.out.rx.rsp.bits.srcID.poke(1.U)
          d.io.out.rx.rsp.bits.resp.poke(2.U)
        }
        d.io.in.c.valid.poke(probes.nonEmpty.B)
        probes.headOption.foreach { x =>
          d.io.in.c.bits.opcode.poke((if (x.data.nonEmpty) 5 else 4).U)
          d.io.in.c.bits.param.poke(1.U) // TtoN
          d.io.in.c.bits.size.poke(6.U); d.io.in.c.bits.address.poke(x.addr.U)
          pokeWords(d.io.in.c.bits.data, x.data.getOrElse(BigInt(0)))
        }
        if (dat.nonEmpty && d.io.out.rx.dat.ready.peek().litToBoolean) incoming.dequeue()
        if (responses.nonEmpty && d.io.out.rx.rsp.ready.peek().litToBoolean) responses.dequeue()
        if (probes.nonEmpty && d.io.in.c.ready.peek().litToBoolean) {
          val reply = probes.dequeue()
          if (reply.data.nonEmpty) probeDataReplies += reply.addr
        }
        if (d.io.in.b.valid.peek().litToBoolean) {
          val addr = d.io.in.b.bits.address.peek().litValue
          probeAddresses += addr
          val data = l1.remove(addr)
          if (data.nonEmpty) for (beat <- 0 until 2)
            probes.enqueue(ProbeBeat(addr, beat, Some((data.get >> (256*beat)) & byteMask)))
          else probes.enqueue(ProbeBeat(addr, 0, None))
        }
        if (d.io.out.tx.req.valid.peek().litToBoolean && d.io.out.tx.req.ready.peek().litToBoolean) {
          val b = d.io.out.tx.req.bits
          val code = b.opcode.peek().litValue.toInt
          val addr = b.addr.peek().litValue
          val txn = b.txnID.peek().litValue
          if (code == op.ReadUnique.litValue.toInt || code == op.ReadNotSharedDirty.litValue.toInt) {
            reads += ((addr, code))
            val shared = (addr == 0x3000 || addr == fullPutBranch) &&
              code != op.ReadUnique.litValue.toInt
            for (beat <- 0 until 2)
              incoming.enqueue(DataBeat(ticks+4+beat*3, txn, beat,
                (line(addr) >> (256*beat)) & byteMask, shared))
          } else if (code == op.MakeUnique.litValue.toInt) {
            makeUniques += addr
            responses.enqueue(Response(txn, op.Comp.litValue.toInt, txn))
          } else if (code == op.WriteBackFull.litValue.toInt) {
            writebacks(txn) = addr
            responses.enqueue(Response(txn, op.CompDBIDResp.litValue.toInt, txn))
          } else if (code == op.Evict.litValue.toInt) {
            responses.enqueue(Response(txn, op.Comp.litValue.toInt, txn))
          } else assert(false, s"unhandled canonical CHI test request opcode=$code")
        }
        if (d.io.out.tx.dat.valid.peek().litToBoolean) {
          val b = d.io.out.tx.dat.bits
          if (b.opcode.peek().litValue == op.CopyBackWrData.litValue) {
            val txn = b.txnID.peek().litValue
            val addr = writebacks(txn)
            val beat = b.dataID.peek().litValue.toInt/2
            val old = line(addr); val bits = peekWords(b.data)
            memory(addr) = (old & ~(byteMask << (256*beat))) | (bits << (256*beat))
          }
        }
        // Count accepted A beats before D so a legal ACK on the same cycle
        // as the final accepted beat is not mistaken for an early response.
        if (d.io.in.a.valid.peek().litToBoolean && d.io.in.a.ready.peek().litToBoolean) {
          val b = d.io.in.a.bits
          val source = b.source.peek().litValue.toInt
          if (fullPutOwners.contains(source)) {
            val (addr, payload) = fullPutOwners(source)
            val beat = fullPutBeats.getOrElse(source, 0)
            assert(beat < 2 && !fullPutAcked.contains(source), s"reused full-Put owner $source")
            b.opcode.expect(0.U); b.param.expect(0.U); b.size.expect(6.U)
            b.address.expect(addr.U); b.matrix.expect(1.U)
            b.mask.expect(((BigInt(1)<<32)-1).U)
            assert(peekWords(b.data) == ((payload >> (256*beat)) & byteMask),
              s"full-Put source=$source beat=$beat sent wrong data")
            fullPutBeats(source) = beat + 1
            if (fullPutBurstActive && fullPutBurstFirstAccept < 0) fullPutBurstFirstAccept = ticks
            fullPutPeakOutstanding = fullPutPeakOutstanding.max(
              fullPutBeats.count { case (s, beats) => beats == 2 && !fullPutAcked.contains(s) })
          } else assert(!fullPutBurstActive, s"unregistered A owner during full-Put burst: $source")
        }
        if (d.io.in.d.valid.peek().litToBoolean && d.io.in.d.ready.peek().litToBoolean) {
          val b = d.io.in.d.bits
          val source = b.source.peek().litValue.toInt
          if (fullPutOwners.contains(source)) {
            b.opcode.expect(0.U); b.denied.expect(false.B)
            assert(fullPutBeats.getOrElse(source, 0) == 2,
              s"full-Put source=$source acknowledged before both A beats were accepted")
            assert(fullPutAcked.add(source), s"duplicate full-Put AccessAck source=$source")
            if (fullPutBurstActive) fullPutBurstLastAck = ticks
          } else assert(!fullPutBurstActive, s"ACK has no full-Put burst owner: $source")
          returns += ((b.opcode.peek().litValue.toInt, peekWords(b.data),
            b.sink.peek().litValue.toInt, b.denied.peek().litToBoolean))
        }
        if (d.io.matrix.valid.peek().litToBoolean && d.io.matrix.ready.peek().litToBoolean) {
          matrixReturns += ((d.io.matrix.bits.source.peek().litValue, peekWords(d.io.matrix.bits.data)))
        }
        aAccepted = d.io.in.a.valid.peek().litToBoolean && d.io.in.a.ready.peek().litToBoolean
        eAccepted = d.io.in.e.valid.peek().litToBoolean && d.io.in.e.ready.peek().litToBoolean
        d.clock.step(); ticks += 1
        assert(ticks < 10000, "cache peer timed out")
      }
      def send(code: Int, addr: BigInt, payload: BigInt = 0,
          mask: BigInt = (BigInt(1)<<64)-1, param: Int = 0,
          matrix: Int = -1, ameIndex: BigInt = 0, source: Int = 0): Unit = {
        println(s"CACHE-TL cycle=$ticks opcode=$code address=0x${addr.toString(16)} mask=0x${mask.toString(16)}")
        for (beat <- 0 until (if (code <= 1) 2 else 1)) {
          val b = d.io.in.a.bits
          b.opcode.poke(code.U); b.address.poke(addr.U); b.size.poke(6.U)
          b.param.poke(param.U); b.source.poke(source.U)
          pokeWords(b.data, (payload >> (256*beat)) & byteMask)
          b.mask.poke(((mask >> (32*beat)) & ((BigInt(1)<<32)-1)).U)
          b.matrix.poke((if (matrix >= 0) matrix else if (code <= 1) 1 else 0).U)
          b.ameIndex.poke(ameIndex.U)
          d.io.in.a.valid.poke(true.B)
          aAccepted = false
          while (!aAccepted) step()
        }
        d.io.in.a.valid.poke(false.B)
      }
      def waitReturns(count: Int): Seq[(Int,BigInt,Int,Boolean)] = {
        while (returns.size < count) step()
        val result = returns.take(count).toSeq
        returns.remove(0, count)
        assert(!result.exists(_._4), s"unexpected denied store/read $result")
        result
      }
      def read(addr: BigInt): BigInt = {
        send(4, addr)
        val result = waitReturns(2)
        assert(result.forall(_._1 == 1), "Get did not return AccessAckData")
        result.head._2 | (result(1)._2 << 256)
      }
      var matrixReadCount = 0
      def readMatrix(addr: BigInt, key: Int = 1): BigInt = {
        val token = (BigInt(1)<<48) + matrixReadCount + 7
        val normalCount = returns.size
        send(4, addr, matrix = key, ameIndex = token)
        while (matrixReturns.isEmpty) step()
        val (source, bits) = matrixReturns.remove(0)
        assert(source == token && returns.size == normalCount,
          s"matrix Get lost its owned source or leaked to TL-D: $source != $token")
        matrixReadCount += 1
        bits
      }
      def makeL1Dirty(addr: BigInt, data: BigInt): Unit = {
        send(6, addr, param = 1) // AcquireBlock NtoT
        val grant = waitReturns(2); assert(grant.forall(_._1 == 5))
        d.io.in.e.bits.sink.poke(grant.head._3.U); d.io.in.e.valid.poke(true.B)
        eAccepted = false
        while (!eAccepted) step()
        d.io.in.e.valid.poke(false.B)
        l1(addr) = data
      }
      def store(addr: BigInt, data: BigInt, mask: BigInt, source: Int = 0): Unit = {
        send(if (mask == (BigInt(1)<<64)-1) 0 else 1, addr, data, mask, source = source)
        assert(waitReturns(1).head._1 == 0, "store did not return AccessAck")
      }
      def merged(old: BigInt, data: BigInt, mask: BigInt): BigInt =
        (0 until 64).foldLeft(BigInt(0)) { (v,i) =>
          val src = if (mask.testBit(i)) data else old
          v | (((src >> (8*i)) & 255) << (8*i))
        }
      val addr = BigInt(0x2000); val old = line(addr)
      val mask41 = BigInt(15)<<41; val data = BigInt("a1b2c3d4",16)<<328
      store(addr, data, mask41)
      assert(read(addr) == merged(old,data,mask41), "miss merge changed neighboring bytes")
      val mask33 = BigInt(15)<<33; val data33 = BigInt("11223344",16)<<264
      store(addr, data33, mask33)
      val expected = merged(merged(old,data,mask41),data33,mask33)
      assert(read(addr) == expected, "MCP2 hit merge used stale/wrong old line")
      assert(readMatrix(addr) == expected, "matrix Get clean-hit data differed from normal Get")
      assert(readMatrix(addr, key = 3) == expected, "accumulator matrix Get data differed")
      val matrixMiss = BigInt(0x4000)
      assert(readMatrix(matrixMiss) == line(matrixMiss), "matrix Get miss lost refill data")
      // Get returns Branch on this address; Partial must upgrade with data.
      val branch = BigInt(0x3000); val branchOld = read(branch)
      store(branch, data, mask41)
      assert(read(branch) == merged(branchOld,data,mask41))
      assert(reads.count(x => x._1 == branch && x._2 == op.ReadUnique.litValue.toInt) == 1)
      // Give L1 exclusive ownership, mutate only the peer copy, then ensure
      // the invalidating probe's dirty bytes override the cache DS fallback.
      val matrixDirty = pattern(51)
      makeL1Dirty(addr, matrixDirty)
      assert(readMatrix(addr) == matrixDirty, "matrix Get lost latest dirty-L1 probe bytes")
      val dirty = pattern(73); makeL1Dirty(addr, dirty)
      store(addr, data33, mask33)
      assert(read(addr) == merged(dirty,data33,mask33), "dirty L1 data lost at partial merge")
      // More tags in one two-way set force live dirty-victim replacement.
      for (tag <- 1 to 4) {
        val victimAddr = addr + tag*1024
        val prior = line(victimAddr)
        store(victimAddr, data, mask41)
        assert(read(victimAddr) == merged(prior,data,mask41), "replacement merge failed")
      }
      val full = pattern(119); store(addr, full, (BigInt(1)<<64)-1)
      assert(read(addr) == full, "full Put regression")
      // Keep two dirty, valid ways in the target set before offering more
      // tags than either its two ways or the four MSHRs. +1024 preserves the
      // set for this 16-set/64B fixture, but does not prove an internal retry.
      require(p(L2ParamKey).sets == 16 && p(L2ParamKey).ways == 2 &&
        p(L2ParamKey).mshrs == 4 && p(L2ParamKey).blockBytes == 64)
      val fullMask = (BigInt(1)<<64)-1
      for ((primeAddr, seed) <- Seq((BigInt(0x6000), 131), (BigInt(0x6400), 149))) {
        val payload = pattern(seed)
        store(primeAddr, payload, fullMask)
        assert(read(primeAddr) == payload, "full-Put burst dirty-victim precondition failed")
      }
      assert(returns.isEmpty && matrixReturns.isEmpty, "undrained replies before full-Put burst")
      val burst = (0 until 8).map { i =>
        (i + 1, BigInt(0x8000 + i*1024), pattern(17 + i*23))
      }
      assert(burst.map(_._1).distinct.size == 8 && burst.forall(x => x._1 < 16))
      assert(burst.map(x => (x._2 >> 6) & 15).distinct == Seq(BigInt(0)))
      burst.foreach { case (source, address, payload) => fullPutOwners(source) = (address, payload) }
      fullPutBurstActive = true
      // A back-to-back stream: wait only for each A beat's real ready, never
      // for a preceding transaction's D response before sending the next.
      burst.foreach { case (source, address, payload) =>
        send(0, address, payload, fullMask, matrix = 1, source = source)
      }
      val burstReplies = waitReturns(burst.size)
      assert(burstReplies.forall(_._1 == 0), "full-Put burst did not return only AccessAcks")
      assert(fullPutAcked.toSet == burst.map(_._1).toSet,
        s"full-Put burst owners did not each ACK once: $fullPutAcked")
      assert(fullPutPeakOutstanding >= 2,
        "coverage failure: the full-Put stimulus did not overlap two complete-A TL owners")
      fullPutBurstActive = false
      assert(returns.isEmpty, "extra full-Put burst response")
      for ((source, address, payload) <- burst)
        assert(read(address) == payload, s"ACKed full-Put source=$source lost/stale full 64B")

      // A full overwrite must invalidate dirty L1 ownership, wait for the
      // probe response, then retain the NEW full line rather than probe data.
      val fullDirtyAddr = BigInt(0xa000)
      val fullDirtyPayload = pattern(197)
      makeL1Dirty(fullDirtyAddr, pattern(181))
      val beforeProbes = probeAddresses.count(_ == fullDirtyAddr)
      val beforeProbeData = probeDataReplies.count(_ == fullDirtyAddr)
      fullPutOwners(9) = (fullDirtyAddr, fullDirtyPayload)
      store(fullDirtyAddr, fullDirtyPayload, fullMask, source = 9)
      assert(fullPutAcked.contains(9) && !l1.contains(fullDirtyAddr))
      assert(probeAddresses.count(_ == fullDirtyAddr) == beforeProbes + 1 &&
        probeDataReplies.count(_ == fullDirtyAddr) == beforeProbeData + 2,
        "full-Put dirty-L1 ACK preceded its complete invalidating data probe")
      assert(read(fullDirtyAddr) == fullDirtyPayload, "full-Put dirty-L1 probe clobbered new 64B")

      // This cold Get is supplied Shared by the existing CHI peer. A full
      // overwrite must upgrade the resulting BRANCH with MakeUnique, not
      // silently acknowledge without ownership or fetch an old data line.
      val beforeBranchReads = reads.count(_._1 == fullPutBranch)
      assert(read(fullPutBranch) == line(fullPutBranch))
      assert(reads.count(x => x._1 == fullPutBranch &&
        x._2 == op.ReadNotSharedDirty.litValue.toInt) == beforeBranchReads + 1)
      val beforeMakeUnique = makeUniques.count(_ == fullPutBranch)
      val branchPayload = pattern(219)
      fullPutOwners(10) = (fullPutBranch, branchPayload)
      store(fullPutBranch, branchPayload, fullMask, source = 10)
      assert(fullPutAcked.contains(10) &&
        makeUniques.count(_ == fullPutBranch) == beforeMakeUnique + 1,
        "full-Put BRANCH did not complete its MakeUnique upgrade")
      assert(reads.count(_._1 == fullPutBranch) == beforeBranchReads + 1,
        "full-Put BRANCH unexpectedly fetched old-line data")
      assert(read(fullPutBranch) == branchPayload, "full-Put BRANCH upgrade lost new 64B")
      for (_ <- 0 until 20) {
        step()
        assert(returns.isEmpty && matrixReturns.isEmpty, "extra/duplicate response after full-Put checks")
      }
      assert(fullPutAcked.toSet == fullPutOwners.keySet.toSet)
      println(s"CACHE-FULLPUT realSlice MCP2=$mcp2 burst=${burst.size} owners=${fullPutAcked.size} " +
        s"maxTLOutstanding=$fullPutPeakOutstanding " +
        s"burstCycles=${fullPutBurstLastAck-fullPutBurstFirstAccept+1} " +
        "readbackBytes=512 dirtyL1=1 branchUpgrade=1 retry=NOT_INSTRUMENTED")
      println(s"CACHE-MASK realSlice miss/hit/upgrade/dirtyProbe/replacement/full matrixGets=$matrixReadCount PASS cycles=$ticks")
    }
  }
  }
}
