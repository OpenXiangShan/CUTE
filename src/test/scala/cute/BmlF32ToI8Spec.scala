package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BmlF32ToI8Spec extends AnyFlatSpec with ChiselScalatestTester {
  private def params: Parameters = new Config((_, _, _) => {
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP
  })

  private def reference(bits: Int): Int = {
    val value = java.lang.Float.intBitsToFloat(bits)
    if (java.lang.Float.isNaN(value)) {
      0
    } else if (java.lang.Float.isInfinite(value)) {
      if (value < 0.0f) -127 else 127
    } else {
      val scaled = value * (1.0f / 64.0f)
      val rounded = Math.rint(scaled.toDouble).toLong
      math.max(-127L, math.min(127L, rounded)).toInt
    }
  }

  private def asByte(value: Int): BigInt = BigInt(value & 0xff)

  private class ConvertProbe extends Module {
    implicit val p: Parameters = params
    val io = IO(new Bundle {
      val in = Input(UInt(32.W))
      val out = Output(UInt(8.W))
    })
    io.out := F32ToI8FixedScale(io.in)
  }

  it should "match fixed-scale F32 to INT8 reference semantics" in {
    val edgeValues = Seq(
      0x00000000L, 0x80000000L, 0x3f000000L, 0x3f400000L, 0x3f7fffffL,
      0x3f800000L, 0xbf400000L, 0xbf800000L, 0x46fe0000L, 0x46ff0000L,
      0x47000000L, 0x477fe000L, 0x7f800000L, 0xff800000L,
      0x7fc00000L, 0x7fffffffL, 0x00000001L, 0x80000001L
    ).map(_.toInt)
    val random = new Random(0x484f31)
    val randomValues = (0 until 128).map(_ => random.nextInt())
    val testValues = (edgeValues ++ randomValues).distinct

    test(new ConvertProbe) { dut =>
      testValues.foreach { bits =>
        dut.io.in.poke((bits & 0xffffffffL).U)
        val expected = reference(bits)
        val actual = dut.io.out.peek().litValue.toInt
        assert(actual == (expected & 0xff),
          f"bits=0x$bits%08x expected=$expected actual=${actual.toByte.toInt}")
      }
    }
  }

  private class ConvertHarness(val rows: Int = 128) extends Module {
    implicit val p: Parameters = params
    val io = IO(new Bundle {
      val start = Input(Bool())
      val taskReady = Output(Bool())
      val taskEnd = Output(Bool())
      val request = Decoupled(new MMURequestIO)
      val response = Flipped(Decoupled(new MMUResponseIO))
      val matrix = Flipped(new ABMemoryLoaderMatrixRegIO)
    })

    val loader = Module(new BMemoryLoader)
    loader.io.DebugInfo.DebugTimeStampe := 0.U
    loader.io.LocalMMUIO.ConherentRequsetSourceID.valid := true.B
    loader.io.LocalMMUIO.ConherentRequsetSourceID.bits := 0.U
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    loader.io.LocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    loader.io.LocalMMUIO.Request(0) <> io.request
    loader.io.LocalMMUIO.Response(0) <> io.response
    for (channel <- 1 until p(CuteParamsKey).Matrix_MN) {
      loader.io.LocalMMUIO.Request(channel).ready := true.B
      loader.io.LocalMMUIO.Response(channel).valid := false.B
      loader.io.LocalMMUIO.Response(channel).bits := 0.U.asTypeOf(new MMUResponseIO)
    }
    loader.io.ToMatrixRegIO <> io.matrix

    val config = loader.io.ConfigInfo
    config.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := 0.U
    config.ApplicationTensor_B.BlockTensor_B_BaseVaddr := 0.U
    config.ApplicationTensor_B.ApplicationTensor_B_Stride_N := 256.U
    config.ApplicationTensor_B.dataType := ElementDataType.DataTypeWidth32
    config.ApplicationTensor_B.HasTail := false.B
    config.ApplicationTensor_B.TailByteMask := 0.U
    config.ApplicationTensor_B.K_Beat_Count := 4.U
    config.MatrixRegTensor_N := rows.U
    config.MatrixRegTensor_K := 1.U
    config.MatrixRegId := 0.U
    config.Conherent := true.B
    config.Is_Transpose := false.B
    config.ConvertF32ToI8 := true.B
    config.ConvertF32ToMx := false.B
    config.ConvertComputeType := 0.U
    config.ConvertKElements := 0.U
    config.ConvertScaleBankId := 0.U
    config.MicroTaskValid := io.start
    config.MicroTaskEndReady := true.B
    if (p(CuteParamsKey).EnableDifftest) {
      config.pc.get := 0.U
      config.coreid.get := 0.U
      config.retireId.get := 0.U
      config.epoch.get := 0.U
    }

    io.taskReady := config.MicroTaskReady
    io.taskEnd := config.MicroTaskEndValid
  }

  private def sourceBits(row: Int, beat: Int, lane: Int): Int = {
    val code = row * 257 + beat * 41 + lane * 7
    code match {
      case 29 => 0x3f400000 // +0.75
      case 30 => 0xbf400000 // -0.75
      case 31 => 0x7f800000 // +Inf
      case 32 => 0xff800000 // -Inf
      case 33 => 0x7fc00000 // NaN
      case _ =>
        val magnitude = ((code % 1024) - 512) * 0.125f
        java.lang.Float.floatToRawIntBits(magnitude)
    }
  }

  private def sourceAddress(addr: BigInt): Int = addr.toInt

  private def responseData(addr: Int): BigInt = {
    val row = addr / 256
    val beat = (addr % 256) / 64
    (0 until 16).foldLeft(BigInt(0)) { case (packed, lane) =>
      packed | (BigInt(sourceBits(row, beat, lane) & 0xffffffffL) << (lane * 32))
    }
  }

  private def expectedEntry(row: Int, entry: Int): BigInt = {
    val startLane = entry * 32
    (0 until 32).foldLeft(BigInt(0)) { case (packed, lane) =>
      val sourceLane = startLane + lane
      val beat = sourceLane / 16
      val laneInBeat = sourceLane % 16
      val converted = reference(sourceBits(row, beat, laneInBeat))
      packed | (asByte(converted) << (lane * 8))
    }
  }

  it should "finish a small converting loader tile without deadlock" in {
    val rows = 8
    test(new ConvertHarness(rows)) { dut =>
      implicit val p: Parameters = params
      dut.io.start.poke(false.B)
      dut.io.request.ready.poke(false.B)
      dut.io.response.valid.poke(false.B)
      dut.io.response.bits.ReseponseData.poke(0.U)
      dut.io.response.bits.ReseponseConherent.poke(false.B)
      dut.io.response.bits.ReseponseSourceID.poke(0.U)
      dut.clock.step(2)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      var outstanding: Option[Int] = None
      var sourceReads = 0
      var responses = 0
      var writes = 0
      var cycles = 0
      while (!dut.io.taskEnd.peek().litToBoolean && cycles < 2048) {
        dut.io.request.ready.poke(outstanding.isEmpty.B)
        dut.io.response.valid.poke(outstanding.nonEmpty.B)
        dut.io.response.bits.ReseponseData.poke(
          outstanding.map(a => responseData(a).U).getOrElse(0.U)
        )
        dut.io.response.bits.ReseponseConherent.poke(true.B)
        dut.io.response.bits.ReseponseSourceID.poke(0.U)
        writes += (0 until p(CuteParamsKey).Matrix_MN).count(i =>
          dut.io.matrix.BankAddr(i).valid.peek().litToBoolean)
        if (dut.io.request.valid.peek().litToBoolean &&
            dut.io.request.ready.peek().litToBoolean) {
          outstanding = Some(sourceAddress(dut.io.request.bits.RequestAddr.peek().litValue))
          sourceReads += 1
        }
        if (dut.io.response.valid.peek().litToBoolean &&
            dut.io.response.ready.peek().litToBoolean) {
          outstanding = None
          responses += 1
        }
        dut.clock.step()
        cycles += 1
      }
      assert(dut.io.taskEnd.peek().litToBoolean, s"small conversion task timed out after $cycles cycles")
      assert(sourceReads == rows * 4)
      assert(responses == rows * 4)
      assert(writes == rows * 2)
    }
  }

  it should "convert a 128x64 F32 tile into 256 INT8 matrix entries" in {
    test(new ConvertHarness(128)) { dut =>
      implicit val p: Parameters = params
      val bankCount = p(CuteParamsKey).Matrix_MN
      val reduceGroupSize = p(CuteParamsKey).ReduceGroupSize
      dut.io.start.poke(false.B)
      dut.io.request.ready.poke(false.B)
      dut.io.response.valid.poke(false.B)
      dut.io.response.bits.ReseponseData.poke(0.U)
      dut.io.response.bits.ReseponseConherent.poke(false.B)
      dut.io.response.bits.ReseponseSourceID.poke(0.U)
      dut.clock.step(2)

      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      var outstanding: Option[Int] = None
      var sourceReads = 0
      var responses = 0
      val writes = ArrayBuffer.empty[(Int, Int, BigInt)]
      var cycles = 0

      while (!dut.io.taskEnd.peek().litToBoolean && cycles < 20000) {
        dut.io.request.ready.poke(outstanding.isEmpty.B)
        dut.io.response.valid.poke(outstanding.nonEmpty.B)
        dut.io.response.bits.ReseponseData.poke(
          outstanding.map(a => responseData(a).U).getOrElse(0.U)
        )
        dut.io.response.bits.ReseponseConherent.poke(true.B)
        dut.io.response.bits.ReseponseSourceID.poke(0.U)
        for (bank <- 0 until bankCount) {
          if (dut.io.matrix.BankAddr(bank).valid.peek().litToBoolean) {
            assert(dut.io.matrix.Data(bank).valid.peek().litToBoolean)
            assert(dut.io.matrix.ByteMask(bank).valid.peek().litToBoolean)
            writes += ((bank, dut.io.matrix.BankAddr(bank).bits.peek().litValue.toInt,
              dut.io.matrix.Data(bank).bits.peek().litValue))
          }
        }

        if (dut.io.request.valid.peek().litToBoolean &&
            dut.io.request.ready.peek().litToBoolean) {
          assert(outstanding.isEmpty)
          outstanding = Some(sourceAddress(dut.io.request.bits.RequestAddr.peek().litValue))
          sourceReads += 1
        }

        if (dut.io.response.valid.peek().litToBoolean &&
            dut.io.response.ready.peek().litToBoolean) {
          assert(outstanding.nonEmpty)
          outstanding = None
          responses += 1
        }
        dut.clock.step()
        cycles += 1
      }

      assert(dut.io.taskEnd.peek().litToBoolean, s"conversion task timed out after $cycles cycles")
      assert(sourceReads == 512, s"expected 512 source reads, got $sourceReads")
      assert(responses == 512, s"expected 512 responses, got $responses")
      assert(writes.length == 256, s"expected 256 matrix writes, got ${writes.length}")

      val expectedWrites = (0 until 128).flatMap { row =>
        val base = (row / bankCount) * reduceGroupSize
        Seq(
          (row % bankCount, base, expectedEntry(row, 0)),
          (row % bankCount, base + 1, expectedEntry(row, 1))
        )
      }
      val actualByKey = writes.map { case (bank, addr, data) => (bank, addr) -> data }.toMap
      expectedWrites.foreach { case (bank, addr, data) =>
        assert(actualByKey.get((bank, addr)).contains(data),
          s"missing or incorrect converted entry bank=$bank addr=$addr")
      }
    }
  }

  // Reuse the frozen supplied-scale fixture's independent strict-first
  // sixteen-candidate oracle. This checks codes/domain, not CPU log2/fflags.
  private val fp4Candidates = Vector(0, 1, 2, 3, 4, 6, 8, 12, 0, -1, -2, -3, -4, -6, -8, -12)
  private def fp4LaneReference(bits: Int, e: Int, active: Boolean): (Int, Boolean, Boolean) = {
    if (!active) return (0, false, true)
    val exponent = (bits >>> 23) & 255
    if (exponent == 255) return (0, (bits & 0x7fffff) != 0, (bits & 0x7fffff) != 0)
    val x = java.lang.Float.intBitsToFloat(bits).toDouble
    val d = Math.scalb(1.0, e - 128)
    var best = 0
    var distance = math.abs(x)
    for (candidate <- 1 until fp4Candidates.length) {
      val next = math.abs(fp4Candidates(candidate).toDouble * d - x)
      if (next < distance) {
        best = candidate
        distance = next
      }
    }
    (best, false, math.abs(x) <= 16.0 * d)
  }

  private class Fp4LaneProbe extends Module {
    val io = IO(new Bundle {
      val values = Input(Vec(4, UInt(32.W)))
      val scales = Input(Vec(4, UInt(8.W)))
      val active = Input(Vec(4, Bool()))
      val codes = Output(Vec(4, UInt(4.W)))
      val invalid = Output(Vec(4, Bool()))
      val eligible = Output(Vec(4, Bool()))
    })
    for (lane <- 0 until 4) {
      val (code, invalid, eligible) = Mxfp4EncodeLane(io.values(lane), io.active(lane),
        Mxfp4EncodeLane.boundaries(io.scales(lane)), Mxfp4EncodeLane.maxMagnitude(io.scales(lane)))
      io.codes(lane) := code
      io.invalid(lane) := invalid
      io.eligible(lane) := eligible
    }
  }

  it should "R37 share strict FP4 lane encoding for all admitted scales and masked specials" in {
    val cases = (0 until 252).flatMap { e =>
      val d = Math.scalb(1.0, e - 128)
      val centers = fp4Candidates.map(c => java.lang.Float.floatToRawIntBits((c * d).toFloat))
      val neighbors = Seq(0.5, 1.5, 2.5, 3.5, 5.0, 7.0, 10.0).flatMap { midpoint =>
        val value = (midpoint * d).toFloat
        Seq(Math.nextDown(value), value, Math.nextUp(value)).flatMap(v =>
          Seq(java.lang.Float.floatToRawIntBits(v), java.lang.Float.floatToRawIntBits(-v)))
      }
      val specials = Seq(0, 0x80000000, 1, 0x80000001, 0x7fc12345, 0xff800001,
        0x7f800000, 0xff800000)
      (centers ++ neighbors ++ specials).map(bits => (bits, e, true)) ++
        specials.map(bits => (bits, e, false))
    }
    test(new Fp4LaneProbe) { dut =>
      for (group <- cases.grouped(4)) {
        for (lane <- 0 until 4) {
          val (bits, e, active) = group.lift(lane).getOrElse((0, 0, false))
          dut.io.values(lane).poke((bits & 0xffffffffL).U)
          dut.io.scales(lane).poke(e.U)
          dut.io.active(lane).poke(active.B)
        }
        for (lane <- 0 until 4) {
          val (bits, e, active) = group.lift(lane).getOrElse((0, 0, false))
          val (code, invalid, eligible) = fp4LaneReference(bits, e, active)
          dut.io.codes(lane).expect(code.U)
          dut.io.invalid(lane).expect(invalid.B)
          dut.io.eligible(lane).expect(eligible.B)
        }
      }
      println(s"R37 shared FP4 lane oracle PASS lanes=${cases.size}, scales=252")
    }
  }

  private class Fp4BlockProbe(lanes: Int) extends Module {
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(new Bundle {
        val values = Vec(32, UInt(32.W))
        val e = UInt(8.W)
        val active = UInt(32.W)
      }))
      val out = Decoupled(new Bundle {
        val words = Vec(4, UInt(32.W))
        val e = UInt(8.W)
        val invalid = Bool()
        val eligible = Bool()
      })
    })
    val block = Module(new Mxfp4EncodeBlock(lanes))
    block.io.in <> io.in
    io.out.valid := block.io.out.valid
    block.io.out.ready := io.out.ready
    io.out.bits.words := block.io.out.bits.payload.asTypeOf(io.out.bits.words)
    io.out.bits.e := block.io.out.bits.scale
    io.out.bits.invalid := block.io.out.bits.invalid
    io.out.bits.eligible := block.io.out.bits.eligible
  }

  for (lanes <- Seq(8, 32)) {
    it should s"R37 retain existing FP4 block status packing and held output lanes$lanes" in {
      val random = new Random(0x13b8f04L)
      val scales = Seq(0, 1, 2, 3, 128, 251, 252, 255)
      val inputs = for (e <- scales; variant <- 0 until 8) yield {
        val d = Math.scalb(1.0, e - 128)
        val values = Vector.tabulate(32) { lane =>
          if (variant == 1 && lane == 31) 0x7fc12345
          else if (variant == 2 && lane == 7) 0xff800001
          else if (variant == 3 && lane == 16) 0x7f800000
          else if (variant == 4 && lane == 8)
            java.lang.Float.floatToRawIntBits((17.0 * d).toFloat)
          else java.lang.Float.floatToRawIntBits(
            (fp4Candidates((lane * 5 + variant) % 16).toDouble * d).toFloat)
        }
        val mask = if (variant == 5) BigInt(0) else if (variant == 6)
          BigInt(random.nextInt() & 0xffffffffL) else (BigInt(1) << 32) - 1
        (values, e, mask)
      }
      test(new Fp4BlockProbe(lanes)) { dut =>
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.values.foreach(_.poke(0.U))
        dut.io.in.bits.e.poke(0.U); dut.io.in.bits.active.poke(0.U)
        dut.io.out.ready.poke(false.B)
        dut.clock.step(2)
        for ((values, e, mask) <- inputs) {
          val results = values.zipWithIndex.map { case (bits, lane) =>
            fp4LaneReference(bits, e, mask.testBit(lane))
          }
          val eligible = e <= 251 && results.forall(_._3)
          val invalid = results.exists(_._2)
          val payload = if (!eligible) BigInt(0) else results.zipWithIndex.foldLeft(BigInt(0)) {
            case (word, ((code, _, _), lane)) => word | (BigInt(code) << (4 * lane))
          }
          dut.io.in.bits.values.zip(values).foreach { case (port, bits) =>
            port.poke((bits & 0xffffffffL).U)
          }
          dut.io.in.bits.e.poke(e.U); dut.io.in.bits.active.poke(mask.U)
          dut.io.in.valid.poke(true.B)
          dut.io.in.ready.expect(true.B)
          dut.clock.step()
          dut.io.in.valid.poke(false.B)
          var waited = 0
          while (!dut.io.out.valid.peek().litToBoolean && waited < 8) {
            dut.clock.step(); waited += 1
          }
          dut.io.out.valid.expect(true.B)
          for (_ <- 0 until 3) {
            dut.io.out.bits.words.zipWithIndex.foreach { case (word, index) =>
              word.expect(((payload >> (32 * index)) & 0xffffffffL).U)
            }
            dut.io.out.bits.e.expect(e.U)
            dut.io.out.bits.invalid.expect(invalid.B)
            dut.io.out.bits.eligible.expect(eligible.B)
            dut.clock.step()
          }
          dut.io.out.ready.poke(true.B)
          dut.clock.step()
          dut.io.out.ready.poke(false.B)
          dut.io.out.valid.expect(false.B)
        }
        println(s"R37 existing FP4 block oracle PASS blocks=${inputs.size}, lanes=$lanes")
      }
    }
  }

  // Independent bit-domain oracle for the APPROVED scale-result guard.
  // Deliberately no host log2/floor or scalar-fflags equivalence claim.
  private def dynamicScaleReference(values: Seq[Int]): (Long, Int, Boolean) = {
    require(values.size == 32)
    val maximum = values.map(bits => bits & 0x7fffffffL).max
    val finite = values.forall(bits => ((bits >>> 23) & 255) != 255)
    val exponent = (maximum >>> 23).toInt
    val fraction = maximum & 0x7fffffL
    val guardedFraction = fraction == 0 || (fraction >= 0x400 && fraction <= 0x7ffc00)
    val supported = finite && (maximum == 0 ||
      (exponent >= 2 && exponent <= 253 && guardedFraction))
    val e = if (supported && maximum != 0) exponent - 2 else 0
    (maximum, e, supported)
  }

  private def pokeDynamicValues(ports: Vec[UInt], values: Seq[Int]): Unit =
    ports.zip(values).foreach { case (port, bits) => port.poke((bits & 0xffffffffL).U) }

  it should "select guarded K32 scale at every exponent and mantissa boundary" in {
    val fractions = Seq(0, 1, 0x3ff, 0x400, 0x401,
      0x400000, 0x7ffbff, 0x7ffc00, 0x7ffc01, 0x7fffff)
    test(new Mxfp4DynamicScale) { dut =>
      var checked = 0
      for (exponent <- 0 until 256; fraction <- fractions; negative <- Seq(false, true)) {
        val bits = (exponent << 23) | fraction | (if (negative) 0x80000000 else 0)
        val values = Vector.tabulate(32)(lane => if ((lane & 1) == 0) 0 else 0x80000000)
          .updated(checked % 32, bits)
        val (maximum, e, supported) = dynamicScaleReference(values)
        pokeDynamicValues(dut.io.values, values)
        dut.io.amax.expect(maximum.U)
        dut.io.e.expect(e.U)
        dut.io.supported.expect(supported.B)
        checked += 1
      }
      println(s"guarded K32 scale bit-domain PASS blocks=$checked exponents=256; no libm/fflags claim")
    }
  }

  it should "reject guarded K32 scale specials in every lane and retain no prior result" in {
    val acceptedMax = (127 << 23) | 0x400000
    val signedZeros = Vector.tabulate(32)(lane => if ((lane & 1) == 0) 0 else 0x80000000)
    test(new Mxfp4DynamicScale) { dut =>
      var checked = 0
      def check(values: Seq[Int]): Unit = {
        val (maximum, e, supported) = dynamicScaleReference(values)
        pokeDynamicValues(dut.io.values, values)
        dut.io.amax.expect(maximum.U)
        dut.io.e.expect(e.U)
        dut.io.supported.expect(supported.B)
        dut.clock.step()
        checked += 1
      }
      check(Vector.fill(32)(0))
      check(Vector.fill(32)(0x80000000))
      check(signedZeros)
      for (maximumLane <- 0 until 32; negative <- Seq(false, true)) {
        // Every comparator subtree must propagate its winner, independent
        // of sign and position. Other finite lanes span smaller exponents.
        val values = Vector.tabulate(32) { lane =>
          val magnitude = ((2 + lane * 3) << 23) | (0x400 + lane)
          magnitude | (if ((lane & 1) != 0) 0x80000000 else 0)
        }.updated(maximumLane, acceptedMax | (if (negative) 0x80000000 else 0))
        check(values)
        for (special <- Seq(0x7f800000, 0xff800000, // +/-Inf
          0x7fc12345, 0xffc12345, 0x7f800001, 0xff800001)) { // qNaN/sNaN, both signs
          // Reject each lane even while an ordinary supported max is in a
          // different lane, then recover without reset or a stale status/e.
          check(values.updated((maximumLane + 1) % 32, acceptedMax).updated(maximumLane, special))
          check(values)
        }
        check(signedZeros)
        check(values)
      }
      // A supported maximum does not require all non-maximal finite lanes
      // to pass the nonzero guard: subnormal/tiny lanes are valid operands.
      check(Vector.fill(32)(1).updated(31, acceptedMax))
      check(Vector.fill(32)(0x80000001).updated(0, acceptedMax))
      println(s"guarded K32 scale lane/sign/special/recovery PASS blocks=$checked; no libm/fflags claim")
    }
  }

  private class DynamicFp4BlockProbe(lanes: Int) extends Module {
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(Vec(32, UInt(32.W))))
      val out = Decoupled(new Bundle {
        val words = Vec(4, UInt(32.W))
        val e = UInt(8.W)
        val invalid = Bool()
        val eligible = Bool()
      })
      val amax = Output(UInt(31.W))
      val scaleSupported = Output(Bool())
    })
    val scale = Module(new Mxfp4DynamicScale)
    val block = Module(new Fp4BlockProbe(lanes))
    scale.io.values := io.in.bits
    io.amax := scale.io.amax
    io.scaleSupported := scale.io.supported
    // Test-only composition: unsupported inputs must NOT be submitted to
    // the encoder. No row owner, publication or fallback is implemented here.
    block.io.in.valid := io.in.valid && scale.io.supported
    io.in.ready := block.io.in.ready && scale.io.supported
    block.io.in.bits.values := io.in.bits
    block.io.in.bits.e := scale.io.e
    block.io.in.bits.active := Fill(32, true.B)
    io.out <> block.io.out
  }

  for (lanes <- Seq(8, 32)) {
    it should s"compose guarded K32 scale with the existing FP4 block encoder lanes$lanes" in {
      val inputs = for (exponent <- Seq(2, 3, 64, 127, 128, 252, 253);
        fraction <- Seq(0, 0x400, 0x400000, 0x7ffc00); negative <- Seq(false, true)) yield {
        val maximum = (exponent << 23) | fraction
        val values = Vector.tabulate(32) { lane =>
          val smaller = maximum - lane * 0x101
          smaller | (if ((lane & 1) != 0) 0x80000000 else 0)
        }.updated(31, maximum | (if (negative) 0x80000000 else 0))
        values
      }
      val allInputs = Vector.fill(32)(0) +: Vector.fill(32)(0x80000000) +: inputs
      test(new DynamicFp4BlockProbe(lanes)) { dut =>
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.foreach(_.poke(0.U))
        dut.io.out.ready.poke(false.B)
        dut.clock.step(2)
        for (values <- allInputs) {
          val (maximum, e, supported) = dynamicScaleReference(values)
          assert(supported)
          val results = values.map(bits => fp4LaneReference(bits, e, active = true))
          assert(results.forall(result => !result._2 && result._3))
          val payload = results.zipWithIndex.foldLeft(BigInt(0)) {
            case (word, ((code, _, _), lane)) => word | (BigInt(code) << (4 * lane))
          }
          pokeDynamicValues(dut.io.in.bits, values)
          dut.io.amax.expect(maximum.U)
          dut.io.scaleSupported.expect(true.B)
          dut.io.in.valid.poke(true.B)
          dut.io.in.ready.expect(true.B)
          dut.clock.step()
          dut.io.in.valid.poke(false.B)
          // The existing encoder owns the submitted e/values while held.
          // A subsequent rejected input must not change that output.
          dut.io.in.bits.foreach(_.poke("h7f800000".U))
          dut.io.scaleSupported.expect(false.B)
          dut.io.in.ready.expect(false.B)
          var waited = 0
          while (!dut.io.out.valid.peek().litToBoolean && waited < 8) {
            dut.clock.step()
            waited += 1
          }
          dut.io.out.valid.expect(true.B)
          for (_ <- 0 until 3) {
            dut.io.out.bits.words.zipWithIndex.foreach { case (word, index) =>
              word.expect(((payload >> (32 * index)) & 0xffffffffL).U)
            }
            dut.io.out.bits.e.expect(e.U)
            dut.io.out.bits.invalid.expect(false.B)
            dut.io.out.bits.eligible.expect(true.B)
            dut.clock.step()
          }
          dut.io.out.ready.poke(true.B)
          dut.clock.step()
          dut.io.out.ready.poke(false.B)
          dut.io.out.valid.expect(false.B)
        }
        println(s"guarded K32 scale -> original FP4 encoder PASS blocks=${allInputs.size} lanes=$lanes; " +
          "no CPU libm/fflags or dynamic-load claim")
      }
    }
  }
}
