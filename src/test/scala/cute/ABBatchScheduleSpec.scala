package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

import scala.collection.mutable

/** Expected identities are derived from matrix coordinates, never DUT scheduling helpers. */
class ABBatchScheduleSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "shared AML/BML batch traversal"

  private def params(mode: String, enabled: Boolean, bridgeConfig: String): Parameters = new Config((_, _, _) => {
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL, perfDBHartID = 0)
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      LoaderBridgeChannelConfig = bridgeConfig, L2NBanks = 8, Tensor_K = 512,
      ABBatchScheduleEnable = enabled, ABBatchScheduleMode = mode,
      ABBatchRotateOffset = 1, ABBatchPermutationMul = 17,
      Debug = CuteDebugParams.NoDebug, EnableDifftest = false)
  })

  for {
    label <- Seq("AML", "BML")
    (mode, enabled, bridgeConfig) <- Seq(
      ("Natural", true, "A8B8CL8CS8"), ("Rotate", true, "A8B8CL8CS8"),
      ("Permute", true, "A8B8CL8CS8"), ("Rotate", false, "A8B8CL8CS8"),
      ("Rotate", true, "A4B4CL8CS8"))
    beats <- Seq(1, 2, 4, 8)
  } {
    it should s"preserve $label data identities for $mode enabled=$enabled $bridgeConfig K=$beats under stalls" in {
      implicit val p: Parameters = params(mode, enabled, bridgeConfig)
      test(new MultiChannelsABMemLoader(label)) { dut =>
        for (epoch <- 0 until 2) {
          val rows = 27 // three complete groups distinguish Rotate from Permute; partial final group
          val base = BigInt("80046200", 16)
          val stride = BigInt(576)
          val pending = Array.fill(8)(mutable.Queue.empty[(BigInt, BigInt)])
          val requests = mutable.Set.empty[(Int, Int)]
          val laneRequests = Array.fill(8)(0)
          val firstRow = Array.fill[Option[Int]](8)(None)
          var responseCount = 0
          var aggregateEight = 0
          var sameBatchEight = 0
          val writes = mutable.Set.empty[(Int, Int)]
          val held = Array.fill[Option[(BigInt, BigInt)]](8)(None)
          val cfg = dut.io.ConfigInfo
          cfg.MicroTaskValid.poke(false.B)
          cfg.MicroTaskEndReady.poke(true.B)
          cfg.MatrixRegId.poke(0.U)
          cfg.Conherent.poke(true.B)
          cfg.Is_Transpose.poke(false.B)
          cfg.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
          cfg.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
          cfg.PrefetchTaskId.foreach(_.poke(0.U))
          cfg.PrefetchStream.foreach(_.poke(0.U))
          dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
          dut.io.LocalMMUIO.ConherentRequsetSourceID.valid.poke(false.B)
          dut.io.LocalMMUIO.ConherentRequsetSourceID.bits.poke(0.U)
          dut.io.LocalMMUIO.nonConherentRequsetSourceID.valid.poke(false.B)
          dut.io.LocalMMUIO.nonConherentRequsetSourceID.bits.poke(0.U)
          for (lane <- 0 until 8) {
            dut.io.LocalMMUIO.Request(lane).ready.poke(false.B)
            dut.io.LocalMMUIO.Response(lane).valid.poke(false.B)
            dut.io.LocalMMUIO.Response(lane).bits.ReseponseSourceID.poke(0.U)
            dut.io.LocalMMUIO.Response(lane).bits.ReseponseData.poke(0.U)
            dut.io.LocalMMUIO.Response(lane).bits.ReseponseConherent.poke(true.B)
          }
          if (epoch == 0) {
            dut.reset.poke(true.B)
            dut.clock.step(2)
            dut.reset.poke(false.B)
          }
          cfg.MatrixRegTensor_M.poke(rows.U)
          cfg.MatrixRegTensor_K.poke((beats * 64).U)
          cfg.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr.poke(base.U)
          cfg.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(stride.U)
          cfg.ApplicationTensor_A.K_Beat_Count.poke(beats.U)
          cfg.ApplicationTensor_A.dataType.poke(1.U)
          cfg.ApplicationTensor_A.HasTail.poke(true.B)
          cfg.ApplicationTensor_A.TailByteMask.poke(37.U)
          cfg.LoadTaskInfo.Is_FullLoad.poke(true.B)
          if (epoch == 0) {
            // Abort an accepted task while every request is backpressured.
            cfg.MicroTaskValid.poke(true.B)
            dut.clock.step(4)
            cfg.MicroTaskValid.poke(false.B)
            dut.reset.poke(true.B)
            dut.clock.step(2)
            dut.reset.poke(false.B)
            for (lane <- 0 until 8) {
              dut.io.LocalMMUIO.Request(lane).valid.expect(false.B)
            }
          }
          cfg.MicroTaskValid.poke(true.B)
          cfg.MicroTaskEndReady.poke(true.B)
          dut.clock.step()
          cfg.MicroTaskValid.poke(false.B)
          var done = false
          var cycle = 0
          val slots = dut.outsideDataWidthByte / dut.ABMatrixRegEntryByteSize
          val sourceAddrWidth = chisel3.util.log2Ceil(dut.ABMatrixRegBankNEntries)
          while (!done && cycle < 2000) {
            val responseBatchIds = mutable.ArrayBuffer.empty[(Int, Int)]
            for (lane <- 0 until 8) {
              val req = dut.io.LocalMMUIO.Request(lane)
              val resp = dut.io.LocalMMUIO.Response(lane)
              val ready = (cycle + lane) % 7 != 0 && cycle > 3
              req.ready.poke(ready.B)
              resp.valid.poke(pending(lane).nonEmpty.B)
              resp.bits.ReseponseSourceID.poke(pending(lane).headOption.map(_._1).getOrElse(BigInt(0)).U)
              resp.bits.ReseponseData.poke(pending(lane).headOption.map(_._2).getOrElse(BigInt(0)).U)
              resp.bits.ReseponseConherent.poke(true.B)
              if (held(lane).nonEmpty) assert(req.valid.peek().litToBoolean, "stalled request lost valid")
              if (req.valid.peek().litToBoolean) {
                val addr = req.bits.RequestAddr.peek().litValue
                val source = req.bits.RequestSourceID.peek().litValue
                held(lane).foreach(previous => assert(previous == ((addr, source))))
                held(lane) = if (ready) None else Some((addr, source))
              }
              if (resp.valid.peek().litToBoolean && resp.ready.peek().litToBoolean) {
                val source = pending(lane).dequeue()._1
                val reg = (source & ((BigInt(1) << sourceAddrWidth) - 1)).toInt
                responseBatchIds += ((reg / dut.ReduceGroupSize, (reg % dut.ReduceGroupSize) / slots))
                responseCount += 1
              }
              if (req.valid.peek().litToBoolean && ready) {
                val addr = req.bits.RequestAddr.peek().litValue
                val source = req.bits.RequestSourceID.peek().litValue
                val row = ((addr - base) / stride).toInt
                val beat = (((addr - base) % stride) / 64).toInt
                assert(row >= 0 && row < rows && row % 8 == lane && beat >= 0 && beat < beats)
                val ordinalGroup = laneRequests(lane) / beats
                val expectedBeat = laneRequests(lane) % beats
                val completeGroups = rows / 8
                val reordered = enabled && bridgeConfig == "A8B8CL8CS8" && ordinalGroup < completeGroups
                val expectedGroup = if (!reordered || mode == "Natural") ordinalGroup
                  else if (mode == "Rotate") (ordinalGroup + 1) % completeGroups
                  else (17 * ordinalGroup + 1) % completeGroups
                assert(row == expectedGroup * 8 + lane && beat == expectedBeat)
                laneRequests(lane) += 1
                if (firstRow(lane).isEmpty) {
                  firstRow(lane) = Some(row)
                  val reordered = enabled && mode != "Natural" && bridgeConfig == "A8B8CL8CS8"
                  assert(row == lane + (if (reordered) 8 else 0))
                  if (beats == 1) {
                    val bank = ((addr >> 6) & 7).toInt
                    val set = ((addr >> 9) & 511).toInt
                    val hitsCTail = Set((3, 52), (7, 56)).contains((bank, set))
                    assert(hitsCTail == (!reordered && Set(3, 7).contains(lane)))
                  }
                }
                assert(requests.add((row, beat)), "duplicate matrix request")
                val reg = (source & ((BigInt(1) << sourceAddrWidth) - 1)).toInt
                assert(reg == row / 8 * dut.ReduceGroupSize + beat * slots)
                assert(((source >> sourceAddrWidth) & 7).toInt == lane)
                assert(source.testBit(sourceAddrWidth + 3) == (beat == beats - 1))
                val payload = (0 until 64).foldLeft(BigInt(0)) { (data, byte) =>
                  data | (BigInt((row * 11 + beat * 3 + byte) & 255) << (byte * 8))
                }
                pending(lane).enqueue((source, payload))
              }
              for (write <- dut.io.ToMatrixRegIO.WriteRequest(lane)) {
                if (write.BankAddr.valid.peek().litToBoolean) {
                  val reg = write.BankAddr.bits.peek().litValue.toInt
                  assert(writes.add((lane, reg)), "duplicate matrix write")
                  val row = reg / dut.ReduceGroupSize * 8 + lane
                  val rowSlot = reg % dut.ReduceGroupSize
                  val beat = rowSlot / slots
                  val slot = rowSlot % slots
                  val expected = (0 until dut.ABMatrixRegEntryByteSize).foldLeft(BigInt(0)) { (data, byte) =>
                    data | (BigInt((row * 11 + beat * 3 + slot * dut.ABMatrixRegEntryByteSize + byte) & 255) << (byte * 8))
                  }
                  assert(write.Data.bits.peek().litValue == expected)
                  val validBytes = if (beat == beats - 1)
                    math.max(0, math.min(dut.ABMatrixRegEntryByteSize, 37 - slot * dut.ABMatrixRegEntryByteSize))
                  else dut.ABMatrixRegEntryByteSize
                  assert(write.ByteMask.bits.peek().litValue == (BigInt(1) << validBytes) - 1)
                }
              }
            }
            if (responseBatchIds.size == 8) {
              aggregateEight += 1
              if (responseBatchIds.distinct.size == 1) sameBatchEight += 1
            }
            done = cfg.MicroTaskEndValid.peek().litToBoolean
            dut.clock.step()
            cycle += 1
          }
          assert(done && requests.size == rows * beats && writes.size == rows * beats * slots)
          assert(responseCount == requests.size && sameBatchEight <= aggregateEight)
          info(s"$label $mode requests=${requests.size} responses=$responseCount " +
            s"aggregate8=$aggregateEight sameBatch8=$sameBatchEight epoch=$epoch")
          dut.clock.step(2)
        }
      }
    }
  }
}
