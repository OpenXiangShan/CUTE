package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

object TransposeBytePlaneTestConfig {
  val params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP
  })
}

class TransposeAddressScaleHarness(implicit p: Parameters) extends CuteModule {
  private val indexWidth = MatrixRegMaxTensorDimBitSize
  private val groupBaseWidth = indexWidth + log2Ceil(ABMatrixRegEntryByteSize + 1)
  private val beatBaseWidth = indexWidth + log2Ceil(Trans_Load_Size + 1) + log2Ceil(ReduceGroupSize + 1)

  val io = IO(new Bundle {
    val elementBytes = Input(UInt(3.W))
    val groupIndex = Input(UInt(indexWidth.W))
    val beatIndex = Input(UInt(indexWidth.W))
    val elementSlot = Input(UInt(log2Ceil(Trans_Load_Size).W))
    val groupBase = Output(UInt(groupBaseWidth.W))
    val beatBase = Output(UInt(beatBaseWidth.W))
    val writeOffset = Output(UInt((log2Ceil(Trans_Load_Size) + log2Ceil(ReduceGroupSize)).W))
  })

  io.groupBase := TransposeBytePlane.groupBase(
    io.groupIndex, io.elementBytes, ABMatrixRegEntryByteSize
  )
  io.beatBase := TransposeBytePlane.beatEntryBase(
    io.beatIndex, io.elementBytes, Trans_Load_Size, ReduceGroupSize
  )
  io.writeOffset := TransposeBytePlane.reduceGroupOffset(io.elementSlot, ReduceGroupSize)
}

class TransposeBytePlaneSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "the streaming transpose byte-plane path"

  private val responseBytes = 64
  private val bankCount = 8
  private val entryBytes = 32
  private val reduceGroupSize = 2
  private val byteSlots = responseBytes / bankCount
  private val testBank = 3

  private def sourceByteIndex(elementBytes: Int, bank: Int, slot: Int): Int =
    elementBytes * (bank + bankCount * (slot / elementBytes)) + (slot % elementBytes)

  private def byteValue(row: Int, byteIndex: Int): Int =
    (0x31 + row * 67 + byteIndex * 13) & 0xff

  private def responseData(row: Int): BigInt =
    (0 until responseBytes).foldLeft(BigInt(0)) { case (packed, byteIndex) =>
      packed | (BigInt(byteValue(row, byteIndex)) << (byteIndex * 8))
    }

  private def responseMask(elementBytes: Int, maskedPhases: Set[Int]): BigInt = {
    val invalidBytes = maskedPhases.map(phase => sourceByteIndex(elementBytes, testBank, phase))
    (0 until responseBytes).foldLeft(BigInt(0)) { case (mask, byteIndex) =>
      if (invalidBytes.contains(byteIndex)) mask else mask | (BigInt(1) << byteIndex)
    }
  }

  private def initPipe(dut: TransAlignPipe): Unit = {
    dut.io.in_data.poke(0.U)
    dut.io.in_mask.poke(0.U)
    dut.io.resp_beat_cnt.poke(0.U)
    dut.io.entry_offset.poke(0.U)
    dut.io.bytes_per_element.poke(1.U)
    dut.io.debug_time.poke(0.U)
    dut.io.is_drain_trigger.poke(false.B)
    dut.io.in_valid.poke(false.B)
    dut.io.out.ready.poke(true.B)
  }

  private def runPipeCase(elementBytes: Int, rows: Int, permutation: Seq[Int], maskedPhases: Set[Int] = Set.empty): Unit = {
    test(new TransAlignPipe(testBank)(TransposeBytePlaneTestConfig.params))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        initPipe(dut)

        val observed = mutable.ArrayBuffer.empty[(Int, Int)]
        var emptyPhaseTransactions = 0

        def observeOutput(): Unit = {
          if (dut.io.out.valid.peek().litToBoolean) {
            val phase = dut.io.out.bits(0).phase.peek().litValue.toInt
            var anyByteValid = false
            for (lane <- 0 until byteSlots) {
              dut.io.out.bits(lane).phase.expect(phase.U)
              if (dut.io.out.bits(lane).mask.peek().litToBoolean) {
                anyByteValid = true
                val byteOffset = dut.io.out.bits(lane).entry_offset.peek().litValue.toInt
                val plane = phase % elementBytes
                assert(byteOffset >= plane && (byteOffset - plane) % elementBytes == 0,
                  s"e$elementBytes phase $phase produced non-element-aligned offset $byteOffset")
                val row = (byteOffset - plane) / elementBytes
                assert(row >= 0 && row < rows, s"e$elementBytes row $row outside group")
                val expectedByteIndex = sourceByteIndex(elementBytes, testBank, phase)
                dut.io.out.bits(lane).data.expect(byteValue(row, expectedByteIndex).U)
                observed += phase -> row
              }
            }
            if (!anyByteValid) {
              emptyPhaseTransactions += 1
            }
          }
        }

        permutation.zipWithIndex.foreach { case (row, arrival) =>
          dut.io.in_data.poke(responseData(row).U)
          dut.io.in_mask.poke(responseMask(elementBytes, maskedPhases).U)
          dut.io.resp_beat_cnt.poke(arrival.U)
          dut.io.entry_offset.poke(row.U)
          dut.io.bytes_per_element.poke(elementBytes.U)
          dut.io.is_drain_trigger.poke((arrival == rows - 1).B)
          dut.io.in_valid.poke(true.B)
          observeOutput()
          dut.clock.step()
        }

        dut.io.in_valid.poke(false.B)
        dut.io.is_drain_trigger.poke(false.B)
        for (_ <- 0 until 96) {
          observeOutput()
          dut.clock.step()
        }

        val expected = for {
          row <- 0 until rows
          phase <- 0 until byteSlots
          if !maskedPhases.contains(phase)
        } yield phase -> row
        assert(observed.sorted == expected.sorted,
          s"e$elementBytes pipe mapping did not preserve the expected phase/row set")
        assert(observed.distinct.size == observed.size,
          s"e$elementBytes pipe emitted a duplicate byte-plane packet")
        if (maskedPhases.nonEmpty) {
          assert(emptyPhaseTransactions > 0,
            "an all-byte-invalid logical phase must continue through the pipe")
          assert(observed.exists(_._1 > maskedPhases.max),
            "a valid phase after an all-byte-invalid phase was lost")
        }
      }
  }

  it should "match the frozen e8/e16/e32 mapping model" in {
    for (elementBytes <- Seq(1, 2, 4); bank <- Seq(0, 3, 7); slot <- 0 until byteSlots) {
      val q = slot / elementBytes
      val plane = slot % elementBytes
      val expectedSource = elementBytes * (bank + bankCount * q) + plane
      assert(sourceByteIndex(elementBytes, bank, slot) == expectedSource)
      assert(TransposeBytePlane.sourceByteIndex(bank, slot, elementBytes, bankCount) == expectedSource)
    }

    for (elementBytes <- Seq(1, 2, 4); group <- Seq(0, 1); beat <- Seq(0, 1); row <- 0 until entryBytes / elementBytes; phase <- 0 until byteSlots) {
      val q = phase / elementBytes
      val plane = phase % elementBytes
      val base = group + beat * ((byteSlots / elementBytes) * reduceGroupSize)
      val entry = base + q * reduceGroupSize
      val byteOffset = row * elementBytes + plane
      assert(entry >= 0 && entry < 32, s"e$elementBytes entry $entry outside the physical bank")
      assert(byteOffset >= 0 && byteOffset < entryBytes, s"e$elementBytes byte offset $byteOffset outside the entry")
    }
  }

  it should "select constant shifts for transpose group and beat bases" in {
    test(new TransposeAddressScaleHarness()(TransposeBytePlaneTestConfig.params))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        for {
          elementBytes <- Seq(1, 2, 4)
          groupIndex <- Seq(0, 1, 7, 31)
          beatIndex <- Seq(0, 1, 3, 7)
          elementSlot <- 0 until byteSlots
        } {
          dut.io.elementBytes.poke(elementBytes.U)
          dut.io.groupIndex.poke(groupIndex.U)
          dut.io.beatIndex.poke(beatIndex.U)
          dut.io.elementSlot.poke(elementSlot.U)

          dut.io.groupBase.expect((groupIndex * (entryBytes / elementBytes)).U)
          dut.io.beatBase.expect((beatIndex * ((byteSlots / elementBytes) * reduceGroupSize)).U)
          dut.io.writeOffset.expect((elementSlot * reduceGroupSize).U)
        }
      }
  }

  it should "cover every physical entry for a 128 by 64-byte raw tile" in {
    val sourceMajorElements = 128
    val sourceReduceBytes = 64

    for (elementBytes <- Seq(1, 2, 4)) {
      val sourceRows = sourceReduceBytes / elementBytes
      val groupRows = entryBytes / elementBytes
      val responseBeats = sourceMajorElements * elementBytes / responseBytes
      val groups = sourceRows / groupRows

      assert(groups == 2, s"e$elementBytes expected two source-row groups")
      assert(sourceRows % groupRows == 0, s"e$elementBytes source rows must be group aligned")
      assert(sourceMajorElements * elementBytes % responseBytes == 0,
        s"e$elementBytes source rows must be response aligned")

      val writeLocations = for {
        group <- 0 until groups
        beat <- 0 until responseBeats
        row <- 0 until groupRows
        phase <- 0 until byteSlots
      } yield {
        val q = phase / elementBytes
        val plane = phase % elementBytes
        val entry = group + beat * ((byteSlots / elementBytes) * reduceGroupSize) + q * reduceGroupSize
        val byteOffset = row * elementBytes + plane
        assert(entry >= 0 && entry < 32, s"e$elementBytes entry $entry outside the physical bank")
        assert(byteOffset >= 0 && byteOffset < entryBytes,
          s"e$elementBytes byte offset $byteOffset outside the entry")
        entry -> byteOffset
      }

      assert(writeLocations.distinct.size == writeLocations.size,
        s"e$elementBytes physical byte mapping aliases two source bytes")
      assert(writeLocations.size == 32 * entryBytes,
        s"e$elementBytes did not cover every byte in one physical bank")
      assert(writeLocations.map(_._1).toSet == (0 until 32).toSet,
        s"e$elementBytes did not cover physical entries 0 through 31")
    }
  }

  it should "preserve e8 byte planes through a response permutation" in {
    runPipeCase(elementBytes = 1, rows = 32, permutation = (0 until 32).reverse)
  }

  it should "preserve e16 byte planes through a response permutation" in {
    runPipeCase(elementBytes = 2, rows = 16, permutation = Seq(4, 0, 12, 8, 1, 13, 5, 9, 2, 14, 6, 10, 3, 15, 7, 11))
  }

  it should "preserve e32 byte planes through a response permutation" in {
    runPipeCase(elementBytes = 4, rows = 8, permutation = Seq(5, 0, 7, 2, 6, 1, 4, 3))
  }

  it should "advance a fully invalid e16 phase without shifting the following phase" in {
    runPipeCase(elementBytes = 2, rows = 16, permutation = (0 until 16).reverse, maskedPhases = Set(1))
  }

  it should "pipe router phase metadata independently from the final byte mask" in {
    test(new OOORouter()(TransposeBytePlaneTestConfig.params))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.in.valid.poke(false.B)
        for (lane <- 0 until byteSlots) {
          dut.io.in.bits(lane).data.poke(0.U)
          dut.io.in.bits(lane).mask.poke(false.B)
          dut.io.in.bits(lane).entry_offset.poke(0.U)
          dut.io.in.bits(lane).phase.poke(0.U)
        }

        dut.io.in.valid.poke(true.B)
        for (lane <- 0 until byteSlots) {
          dut.io.in.bits(lane).phase.poke(1.U)
        }
        dut.clock.step()
        dut.io.in.valid.poke(false.B)
        dut.clock.step(2)
        dut.io.txn_valid.expect(true.B)
        dut.io.valid.expect(false.B)
        dut.io.phase.expect(1.U)

        dut.clock.step()
        dut.io.in.valid.poke(true.B)
        for (lane <- 0 until byteSlots) {
          dut.io.in.bits(lane).data.poke(0.U)
          dut.io.in.bits(lane).mask.poke(false.B)
          dut.io.in.bits(lane).entry_offset.poke(0.U)
          dut.io.in.bits(lane).phase.poke(6.U)
        }
        for (plane <- 0 until 4) {
          dut.io.in.bits(plane).data.poke((0xa0 + plane).U)
          dut.io.in.bits(plane).mask.poke(true.B)
          dut.io.in.bits(plane).entry_offset.poke((20 + plane).U)
        }
        dut.clock.step()
        dut.io.in.valid.poke(false.B)
        dut.clock.step(2)

        val expectedMask = (0 until 4).foldLeft(BigInt(0))((mask, plane) => mask | (BigInt(1) << (20 + plane)))
        val expectedData = (0 until 4).foldLeft(BigInt(0)) { case (data, plane) =>
          data | (BigInt(0xa0 + plane) << ((20 + plane) * 8))
        }
        dut.io.txn_valid.expect(true.B)
        dut.io.valid.expect(true.B)
        dut.io.phase.expect(6.U)
        dut.io.final_mask.expect(expectedMask.U)
        dut.io.final_data.expect(expectedData.U)
      }
  }
}
