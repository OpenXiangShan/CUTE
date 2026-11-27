package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class MultiChannelAccPayloadSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val base = 0x10000
  private val stride = 192
  private val elementsPerBeat = 16
  private val bytesPerBeat = 64

  private def parameters(banks: Int): Parameters = new Config((_, _, _) => {
    case CuteParamsKey =>
      val geometry = if (banks == 4) CuteParams.CUTE_1Tops_64SCP else CuteParams.CUTE_8Tops_128SCP
      geometry.copy(LoaderBridgeChannelConfig = s"TL0${banks}TL1${banks}AL0${banks}TS0${banks}AS0${banks}",
        L2NBanks = banks, EnableDifftest = false, EnableMatrixPrefetch = false, Debug = CuteDebugParams.NoDebug)
  })

  private def element(row: Int, col: Int): BigInt =
    BigInt("a574391b", 16) ^ (BigInt(row) * 0x10101) ^ (BigInt(col) * 0x1000101)

  private def pack(words: Seq[BigInt]): BigInt =
    words.zipWithIndex.foldLeft(BigInt(0)) { case (data, (word, index)) => data | (word << (32 * index)) }

  private def start(dut: MultiChannelAccMemory, m: Int, n: Int, transpose: Boolean, load: Boolean): Unit = {
    val cfg = dut.io.ConfigInfo
    cfg.LoadMicroTaskValid.poke(false.B)
    cfg.StoreMicroTaskValid.poke(false.B)
    cfg.Conherent.poke(true.B)
    cfg.Is_Transpose.poke(transpose.B)
    cfg.MatrixRegTensor_M.poke(m.U)
    cfg.MatrixRegTensor_N.poke(n.U)
    cfg.LoadTaskInfo.Is_FullLoad.poke(true.B)
    cfg.ApplicationTensor_C.BlockTensor_C_BaseVaddr.poke(base.U)
    cfg.ApplicationTensor_C.ApplicationTensor_C_Stride_M.poke(stride.U)
    cfg.ApplicationTensor_C.dataType.poke(ElementDataType.DataTypeWidth32)
    cfg.ApplicationTensor_C.N_Beat_Count.poke(((n + 15) / 16).U)
    cfg.ApplicationTensor_C.HasTail.poke((n % 16 != 0).B)
    cfg.ApplicationTensor_C.TailByteMask.poke(((n % 16) * 4).U)
    cfg.ApplicationTensor_D.BlockTensor_D_BaseVaddr.poke(base.U)
    cfg.ApplicationTensor_D.ApplicationTensor_D_Stride_M.poke(stride.U)
    cfg.ApplicationTensor_D.dataType.poke(ElementDataType.DataTypeWidth32)
    cfg.LoadMicroTaskEndReady.poke(false.B)
    cfg.StoreMicroTaskEndReady.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    val ready = if (load) cfg.LoadMicroTaskReady else cfg.StoreMicroTaskReady
    val valid = if (load) cfg.LoadMicroTaskValid else cfg.StoreMicroTaskValid
    ready.expect(true.B)
    valid.poke(true.B)
    dut.clock.step()
    valid.poke(false.B)
  }

  for (transpose <- Seq(false, true)) {
    val operation = if (transpose) "msct" else "msc"
    it should s"assemble four-bank $operation beats under lane backpressure and wait for every ack" in {
      test(new MultiChannelAccMemory()(parameters(4))).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        val rows = 25
        val cols = 32
        val banks = dut.AccMatrixRegNBanks
        val blockCols = dut.Tensor_MN / banks
        val port = dut.io.ToMatrixRegIO.ReadRequestToMatrixReg
        val mmu = dut.io.StoreLocalMMUIO
        start(dut, if (transpose) cols else rows, if (transpose) rows else cols, transpose, load = false)
        val seen = mutable.Set.empty[Int]
        val pending = mutable.ArrayBuffer.empty[(Int, BigInt)]
        val responses = Array.fill[Option[Int]](banks, 2)(None)
        val reads = Array.fill(banks)(0)
        var held = Option.empty[(BigInt, BigInt, BigInt)]
        val beats = cols / elementsPerBeat
        val release = 80
        var cycle = 0
        while (seen.size < rows * beats && cycle < 400) {
          dut.io.ConfigInfo.StoreMicroTaskEndValid.expect(false.B)
          dut.io.ToMatrixRegIO.StoreReadWriteResponse.poke((1 << MatrixRegTaskType.ReadFromMemoryLoaderIndex).U)
          for (bank <- 0 until banks; slot <- 0 until 2) {
            val response = port.ReadResponseData(bank)(slot)
            val entry = responses(bank)(slot)
            response.valid.poke(entry.nonEmpty.B)
            response.bits.poke(entry.map { addr =>
              val group = addr / blockCols
              val firstCol = ((addr % blockCols) ^ (group & 1)) * banks
              pack((0 until banks).map(i => element(group * banks + bank, firstCol + i)))
            }.getOrElse(BigInt(0)).U)
          }
          for (lane <- 0 until banks) {
            val req = mmu.Request(lane)
            val ready = lane != 0 || cycle >= release
            req.ready.poke(ready.B)
            if (req.valid.peek().litToBoolean) {
              val addr = req.bits.RequestAddr.peek().litValue
              val data = req.bits.RequestData.peek().litValue
              val source = req.bits.RequestSourceID.peek().litValue
              if (lane == 0) {
                held.foreach(previous => assert(previous == ((addr, data, source))))
                held = if (ready) None else Some((addr, data, source))
              }
              if (ready) {
                val offset = addr.toInt - base
                val row = offset / stride
                val beat = (offset % stride) / bytesPerBeat
                assert(row >= 0 && row < rows && row % banks == lane && beat < beats)
                assert(addr == base + row * stride + beat * bytesPerBeat && seen.add(addr.toInt))
                req.bits.RequestType_isWrite.expect(true.B)
                req.bits.RequestMask.expect(((BigInt(1) << bytesPerBeat) - 1).U)
                val first = beat * elementsPerBeat
                assert(data == pack((first until first + elementsPerBeat).map { col =>
                  if (transpose) element(col, row) else element(row, col)
                }), s"$operation payload mismatch at row $row beat $beat")
                pending += ((lane, source))
              }
            } else if (lane == 0) assert(held.isEmpty, "blocked request lost valid")
            for (slot <- 0 until 2) {
              val addr = port.BankAddr(lane)(slot)
              responses(lane)(slot) = if (addr.valid.peek().litToBoolean) Some(addr.bits.peek().litValue.toInt) else None
            }
            if (responses(lane)(0).nonEmpty) reads(lane) += 1
          }
          if (cycle == release - 1) {
            assert(held.nonEmpty && reads(0) == 8, "four reserved beats need two paired reads each")
            if (!transpose) for (lane <- 1 until banks)
              assert(seen.count(addr => ((addr - base) / stride) % banks == lane) > 4, s"lane $lane stalled behind lane 0")
          }
          dut.clock.step()
          cycle += 1
        }
        assert(seen.size == rows * beats)
        // Hold the oldest request until every other acknowledgement has been accepted.
        for (((lane, source), index) <- pending.reverse.zipWithIndex) {
          if (index == pending.size - 1) {
            for (_ <- 0 until 5) { dut.io.ConfigInfo.StoreMicroTaskEndValid.expect(false.B); dut.clock.step() }
          }
          dut.io.ConfigInfo.StoreMicroTaskEndValid.expect(false.B)
          val ack = mmu.Response(lane)
          ack.valid.poke(true.B)
          ack.bits.ReseponseSourceID.poke(source.U)
          ack.ready.expect(true.B)
          dut.clock.step()
          ack.valid.poke(false.B)
        }
        dut.clock.step(3)
        dut.io.ConfigInfo.StoreMicroTaskEndValid.expect(true.B)
      }
    }
  }

  it should "arbitrate distinct mlct response heads and merge matching rows on a partial last beat" in {
    test(new MultiChannelAccMemory()(parameters(8))).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val banks = dut.AccMatrixRegNBanks
      val cols = 27
      val blockCols = dut.Tensor_MN / banks
      val mmu = dut.io.LoadLocalMMUIO
      start(dut, banks, cols, transpose = true, load = true)
      val requests = mutable.Map.empty[(Int, Int), BigInt]
      mmu.Request.foreach(_.ready.poke(true.B))
      val grant = dut.io.ToMatrixRegIO.LoadReadWriteResponse
      grant.poke(0.U)
      var cycles = 0
      while (requests.size < banks * 2 && cycles < 128) {
        for (lane <- 0 until banks if mmu.Request(lane).valid.peek().litToBoolean) {
          val req = mmu.Request(lane).bits
          val offset = req.RequestAddr.peek().litValue.toInt - base
          val beat = (offset % stride) / bytesPerBeat
          assert(beat >= 0 && beat < 2 && offset == lane * stride + beat * bytesPerBeat)
          assert(requests.put((lane, beat), req.RequestSourceID.peek().litValue).isEmpty)
        }
        dut.clock.step()
        cycles += 1
      }
      assert(requests.size == banks * 2)
      // Even lanes return the tail first. Odd lanes return the full beat first.
      val writes = dut.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest
      for (round <- 0 until 2) {
        for (lane <- 0 until banks) {
          val beat = (lane + 1 + round) % 2
          val response = mmu.Response(lane)
          response.valid.poke(true.B)
          response.bits.ReseponseSourceID.poke(requests((lane, beat)).U)
          response.bits.ReseponseData.poke(pack((0 until elementsPerBeat).map(i => element(lane, beat * elementsPerBeat + i))).U)
          response.bits.ReseponseConherent.poke(true.B)
          response.ready.expect(true.B)
        }
        dut.clock.step()
      }
      mmu.Response.foreach(_.valid.poke(false.B))
      val remaining = mutable.Set.from(for (r <- 0 until banks; c <- 0 until cols; byte <- 0 until 4) yield (r, c, byte))
      var bursts = 0
      for (cycle <- 0 until 24) {
        val allow = cycle >= 4 && cycle % 3 != 0
        grant.poke((if (allow) 1 << MatrixRegTaskType.WriteFromMemoryLoaderIndex else 0).U)
        if (remaining.nonEmpty) dut.io.ConfigInfo.LoadMicroTaskEndValid.expect(false.B)
        val groups = mutable.Set.empty[Int]
        for (bank <- 0 until banks; slot <- 0 until 2) {
          val w = writes(bank)(slot)
          val active = w.BankAddr.valid.peek().litToBoolean
          w.Data.valid.expect(active.B)
          w.ByteMask.valid.expect(active.B)
          if (active) {
            assert(allow)
            val addr = w.BankAddr.bits.peek().litValue.toInt
            val blockRow = addr / blockCols
            assert((addr % blockCols) == (blockRow & 1))
            val col = blockRow * banks + bank
            groups += col / elementsPerBeat
            val mask = w.ByteMask.bits.peek().litValue
            if (bursts == 0) assert(mask.bitCount == (banks / 2) * 4, "matching first heads must coalesce")
            val data = w.Data.bits.peek().litValue
            for (b <- 0 until banks * 4 if mask.testBit(b)) {
              val row = b / 4
              assert(remaining.remove((row, col, b % 4)), s"duplicate or unexpected byte ($row,$col,$b)")
              assert(((data >> (8 * b)) & 255) == ((element(row, col) >> (8 * (b % 4))) & 255))
            }
          }
        }
        if (groups.nonEmpty) {
          assert(groups.size == 1, "different vertical block pairs must be arbitrated")
          bursts += 1
        }
        dut.clock.step()
      }
      assert(bursts >= 3 && remaining.isEmpty, s"missing ${remaining.size} destination bytes")
      dut.io.ConfigInfo.LoadMicroTaskEndValid.expect(true.B)
    }
  }
}
