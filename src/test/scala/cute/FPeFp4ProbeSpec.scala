package cute

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.{Config, Parameters}
import org.json4s._
import org.json4s.jackson.JsonMethods.parse

// Local bring-up probes: handshake controls and independent raw-bit arithmetic
// goldens on the existing FPE. These do not establish ISA-level, ScaleLoader,
// TaskController, or whole-GEMM correctness/throughput.
class FPeFp4ProbeSpec extends AnyFlatSpec with ChiselScalatestTester {
  private class ProbeTop(
    enableMxfp4: Boolean,
    enableNvfp4: Boolean,
    enableMxfp8: Boolean,
    rawInputs: Boolean = false,
    liveInputs: Boolean = false
  ) extends Module {
    private val matrixExtension = MatrixIsaParams(
      enableInt8Int32 = true,
      enableMxfp4Fp32 = enableMxfp4,
      enableNvfp4Fp32 = enableNvfp4,
      enableMxfp8Fp32 = enableMxfp8
    )
    implicit val p: Parameters = new Config((_, _, _) => {
      case CuteParamsKey => CuteParams.CUTE_2Tops.copy(MatrixExtension = matrixExtension)
      case CuteFpeParamsKey => CuteFpeParametersFromCuteParams(
        CuteParams.CUTE_2Tops.copy(MatrixExtension = matrixExtension)
      )
    })
    val io = IO(new Bundle {
      val opcode = Input(UInt(FReduceComputeType.ComputeTypeBitWidth.W))
      val inValid = Input(Bool())
      val outReady = Input(Bool())
      val inReady = Output(Bool())
      val outValid = Output(Bool())
      val out = Output(UInt(32.W))
      val flags = Output(UInt(5.W))
      val scaleA = Input(UInt(32.W))
      val scaleB = Input(UInt(32.W))
      val finiteByte = Input(UInt(8.W))
      val rawA = Option.when(rawInputs)(Input(UInt(256.W)))
      val rawB = Option.when(rawInputs)(Input(UInt(256.W)))
      val rawC = Option.when(rawInputs)(Input(UInt(32.W)))
      val liveK = Option.when(liveInputs)(Input(UInt(7.W)))
    })
    val fpe = Module(new FReducePE()(p))
    fpe.io.AVector.valid := io.inValid
    fpe.io.AVector.bits := io.rawA.getOrElse(Fill(32, io.finiteByte))
    fpe.io.BVector.valid := io.inValid
    fpe.io.BVector.bits := io.rawB.getOrElse(Fill(32, io.finiteByte))
    fpe.io.CAdd.valid := io.inValid
    fpe.io.CAdd.bits := io.rawC.getOrElse(0.U)
    fpe.io.DResult.ready := io.outReady
    fpe.io.AScale.foreach(_.valid := io.inValid)
    fpe.io.AScale.foreach(_.bits := io.scaleA)
    fpe.io.BScale.foreach(_.valid := io.inValid)
    fpe.io.BScale.foreach(_.bits := io.scaleB)
    fpe.io.opcode := io.opcode
    fpe.io.LiveK.foreach(_ := io.liveK.getOrElse(Mux(io.opcode === 10.U, 64.U, 32.U)))
    io.inReady := fpe.io.AVector.ready
    io.outValid := fpe.io.DResult.valid
    io.out := fpe.io.DResult.bits
    io.flags := fpe.io.DFlags
  }

  it should "sustain FP4 and INT8 input vectors at the same rate" in {
    test(new ProbeTop(enableMxfp4 = true, enableNvfp4 = true, enableMxfp8 = true)) { dut =>
      dut.io.inValid.poke(false.B)
      dut.io.scaleA.poke(0.U); dut.io.scaleB.poke(0.U); dut.io.finiteByte.poke(0.U)
      dut.io.outReady.poke(true.B)

      def run(opcode: UInt, label: String): (Int, Int) = {
        val requested = 64
        dut.io.opcode.poke(opcode)
        dut.io.inValid.poke(false.B)
        dut.clock.step(12)

        var accepted = 0
        var emitted = 0
        dut.io.inValid.poke(true.B)
        for (_ <- 0 until requested) {
          if (dut.io.inReady.peek().litToBoolean) accepted += 1
          if (dut.io.outValid.peek().litToBoolean) emitted += 1
          dut.clock.step()
        }

        dut.io.inValid.poke(false.B)
        for (_ <- 0 until 12) {
          if (dut.io.outValid.peek().litToBoolean) emitted += 1
          dut.clock.step()
        }

        println(s"$label accepted=$accepted/$requested emitted=$emitted")
        (accepted, emitted)
      }

      val fp4 = run(FReduceComputeType.Mxfp4F32, "FP4")
      val int8 = run(FReduceComputeType.I8I8I32, "INT8")

      assert(fp4._1 == 64, s"FP4 accepted ${fp4._1}/64 vectors")
      assert(int8._1 == 64, s"INT8 accepted ${int8._1}/64 vectors")
      assert(fp4._2 == fp4._1, s"FP4 drained ${fp4._2} outputs for ${fp4._1} inputs")
      assert(int8._2 == int8._1, s"INT8 drained ${int8._2} outputs for ${int8._1} inputs")
    }
  }

  it should "elaborate MXFP4 without NVFP4" in {
    test(new ProbeTop(enableMxfp4 = true, enableNvfp4 = false, enableMxfp8 = false)) { dut =>
      dut.io.opcode.poke(FReduceComputeType.Mxfp4F32)
      dut.io.scaleA.poke(0.U); dut.io.scaleB.poke(0.U); dut.io.finiteByte.poke(0.U)
      dut.io.outReady.poke(true.B)
      dut.io.inValid.poke(true.B)
      dut.clock.step()
      assert(dut.io.inReady.peek().litToBoolean)
    }
  }

  it should "elaborate NVFP4 without MXFP4" in {
    test(new ProbeTop(enableMxfp4 = false, enableNvfp4 = true, enableMxfp8 = false)) { dut =>
      dut.io.opcode.poke(FReduceComputeType.Nvfp4F32)
      dut.io.scaleA.poke(0.U); dut.io.scaleB.poke(0.U); dut.io.finiteByte.poke(0.U)
      dut.io.outReady.poke(true.B)
      dut.io.inValid.poke(true.B)
      dut.clock.step()
      assert(dut.io.inReady.peek().litToBoolean)
    }
  }

  it should "produce MXFP8 NaN from E8M0 255 with finite FP8 data on either side" in {
    test(new ProbeTop(enableMxfp4 = false, enableNvfp4 = false, enableMxfp8 = true)) { dut =>
      dut.io.inValid.poke(false.B)
      dut.io.outReady.poke(true.B)
      for ((opcode, one) <- Seq((FReduceComputeType.Mxfp8e4m3F32, 0x38),
          (FReduceComputeType.Mxfp8e5m2F32, 0x3c))) {
        dut.io.opcode.poke(opcode)
        dut.io.finiteByte.poke(one.U) // Finite 1.0, never a data NaN.
        for ((a, b, expectNaN) <- Seq(
            (127, 127, false), (255, 127, true), (127, 255, true),
            (127 | (255 << 8), 127 | (255 << 8), false))) {
          dut.io.scaleA.poke(a.U); dut.io.scaleB.poke(b.U)
          dut.io.inValid.poke(true.B)
          dut.io.inReady.expect(true.B)
          dut.clock.step(); dut.io.inValid.poke(false.B)
          var cycles = 0
          while (!dut.io.outValid.peek().litToBoolean && cycles < 16) { dut.clock.step(); cycles += 1 }
          dut.io.outValid.expect(true.B)
          val bits = dut.io.out.peek().litValue
          val isNaN = ((bits >> 23) & 255) == 255 && (bits & 0x7fffff) != 0
          assert(isNaN == expectNaN, s"opcode=${opcode.litValue} scaleA=$a scaleB=$b out=$bits")
          dut.clock.step(3)
        }
      }
    }
  }

  // Pack raw codes only. Expected bits below are exact dyadic identities;
  // neither the production FPE nor a host floating-point reduction is an oracle.
  private def packCodes(width: Int, codes: Seq[Int]): BigInt = {
    require(codes.length * width <= 256)
    codes.zipWithIndex.foldLeft(BigInt(0)) { case (word, (code, lane)) =>
      require(code >= 0 && code < (1 << width))
      word | (BigInt(code) << (lane * width))
    }
  }
  private def packScales(codes: Seq[Int]): BigInt =
    packCodes(8, codes.padTo(4, 127))

  private case class RawProbe(
    name: String, opcode: Int, a: BigInt, b: BigInt,
    scaleA: BigInt, scaleB: BigInt, c: BigInt, expected: BigInt, flags: Int, carrierLanes: Int,
    liveLanes: Int = -1
  )
  private def raw(
    name: String, opcode: Int, width: Int, a: Seq[Int], b: Seq[Int],
    expected: Long, c: Long = 0, sa: Seq[Int] = Seq(127), sb: Seq[Int] = Seq(127),
    flags: Int = 0
  ): RawProbe = {
    // Default to full256b carriers (including every frozen historical case).
    // Partial-K probes explicitly override liveLanes on the returned token.
    val carrierLanes = 256 / width
    val aCarrier = a.padTo(carrierLanes, 0)
    val bCarrier = b.padTo(carrierLanes, 0)
    RawProbe(name, opcode, packCodes(width, aCarrier), packCodes(width, bCarrier),
      packScales(sa), packScales(sb), BigInt(c), BigInt(expected), flags, carrierLanes)
  }

  // Independent finite partial-K oracle: signed integers times powers of two.
  // No host Float/Double, production decoder, reduction or HardFloat helper.
  // Exceptional values/signed-zero/flags retain explicit raw-bit goldens below.
  private def finiteFp4Golden(p: RawProbe): BigInt = {
    def unpack(bits: BigInt): (BigInt, Int) = {
      val e = ((bits >> 23) & 255).toInt
      require(e != 255)
      val m = (bits & 0x7fffff) | (if (e == 0) BigInt(0) else BigInt(1) << 23)
      (if (bits.testBit(31)) -m else m, if (e == 0) -149 else e - 150)
    }
    def rounded(n: BigInt, e: Int): BigInt = {
      if (n == 0) return BigInt(0)
      val sign = if (n < 0) BigInt(1) << 31 else BigInt(0)
      val mag = n.abs
      val unit = math.max(-149, mag.bitLength - 24 + e)
      val shift = unit - e
      val q = if (shift <= 0) mag << -shift else {
        val floor = mag >> shift
        val rem = mag - (floor << shift)
        val half = BigInt(1) << (shift - 1)
        floor + (if (rem > half || (rem == half && floor.testBit(0))) 1 else 0)
      }
      if (q == 0) sign
      else {
        val exp = q.bitLength - 1 + unit
        if (exp > 127) sign | BigInt("7f800000", 16)
        else if (exp < -126) sign | q
        else {
          val shiftTo24 = q.bitLength - 24
          val mantissa = if (shiftTo24 >= 0) q >> shiftTo24 else q << -shiftTo24
          sign | (BigInt(exp + 127) << 23) | (mantissa & 0x7fffff)
        }
      }
    }
    val lattice = Vector(0, 1, 2, 3, 4, 6, 8, 12) // E2M1 in half-units.
    def coefficient(bits: BigInt, lane: Int): Int = {
      val code = ((bits >> (4 * lane)) & 15).toInt
      lattice(code & 7) * (if ((code & 8) != 0) -1 else 1)
    }
    val k = if (p.liveLanes < 0) 64 else p.liveLanes
    var c = p.c
    for (block <- 0 until (k + 31) / 32) {
      val sa = ((p.scaleA >> (block * 8)) & 255).toInt
      val sb = ((p.scaleB >> (block * 8)) & 255).toInt
      require(sa != 255 && sb != 255)
      val dot = (block * 32 until math.min(k, (block + 1) * 32)).map { lane =>
        BigInt(coefficient(p.a, lane) * coefficient(p.b, lane))
      }.sum
      val e = sa + sb - 256 // two E8M0 biases plus two half-unit factors.
      val (cm, ce) = unpack(c)
      val common = math.min(e, ce)
      c = rounded((dot << (e - common)) + (cm << (ce - common)), common)
    }
    c
  }

  private def checkFrozenFixture(probes: Seq[RawProbe]): Unit = {
    val path = sys.env.getOrElse("CUTE_MX_FIXTURE",
      fail("CUTE_MX_FIXTURE must name the independently frozen NEMU full-carrier fixture"))
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path))
    val hash = java.security.MessageDigest.getInstance("SHA-256").digest(bytes)
      .map(b => f"${b & 255}%02x").mkString
    assert(hash == "c36163fec9a6c4920c06f31d9ce5c22fb6c1c882a55a16bea25254d03d825d13")
    val rows = new String(bytes, java.nio.charset.StandardCharsets.UTF_8).linesIterator
      .map(parse(_)).filter(r => (r \ "group") == JString("cute22")).toVector
    assert(rows.length == 22 && probes.length == 22)
    for ((row, probe) <- rows.zip(probes)) {
      def integer(key: String): BigInt = (row \ key) match {
        case JInt(value) => value
        case other => fail(s"fixture $key is not an integer: $other")
      }
      def codes(key: String): Seq[Int] = (row \ key) match {
        case JArray(values) => values.map {
          case JInt(value) => value.toInt
          case other => fail(s"fixture $key code is not an integer: $other")
        }
        case other => fail(s"fixture $key is not an array: $other")
      }
      def bits(key: String): BigInt = (row \ key) match {
        case JString(value) => BigInt(value.stripPrefix("0x"), 16)
        case other => fail(s"fixture $key is not a raw-bit string: $other")
      }
      val width = if (probe.opcode == 10) 4 else 8
      assert((row \ "name") == JString(probe.name))
      assert(integer("active_lanes") == probe.carrierLanes)
      assert(codes("a").length == probe.carrierLanes && codes("b").length == probe.carrierLanes)
      assert(integer("compute_type") == probe.opcode && integer("mfrm") == 0)
      assert(packCodes(width, codes("a")) == probe.a && packCodes(width, codes("b")) == probe.b)
      assert(codes("sa").length == probe.carrierLanes / 32 && codes("sb").length == probe.carrierLanes / 32)
      assert(packScales(codes("sa")) == probe.scaleA && packScales(codes("sb")) == probe.scaleB)
      assert(bits("c") == probe.c && bits("result") == probe.expected)
      assert(integer("flags_in") == 0 && integer("flags_raised") == probe.flags &&
        integer("flags_out") == probe.flags)
    }
    println(s"FROZEN-FIXTURE PASS full-carrier inputs/results/flags 22/22 SHA256=$hash")
  }

  private def runRawProbes(probes: Seq[RawProbe], frozenFixture: Boolean = false): Unit = {
    if (frozenFixture) checkFrozenFixture(probes)
    test(new ProbeTop(enableMxfp4 = true, enableNvfp4 = false,
        enableMxfp8 = true, rawInputs = true, liveInputs = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation) else Seq.empty) { dut =>
      dut.io.inValid.poke(false.B)
      dut.io.outReady.poke(true.B)
      dut.io.finiteByte.poke(0.U)
      val mismatches = scala.collection.mutable.ArrayBuffer.empty[String]
      for (p <- probes) {
        dut.io.opcode.poke(p.opcode.U)
        dut.io.liveK.get.poke((if (p.liveLanes < 0) p.carrierLanes else p.liveLanes).U)
        dut.io.rawA.get.poke(p.a.U)
        dut.io.rawB.get.poke(p.b.U)
        dut.io.rawC.get.poke(p.c.U)
        dut.io.scaleA.poke(p.scaleA.U)
        dut.io.scaleB.poke(p.scaleB.U)
        dut.io.outReady.poke(false.B)
        dut.io.inValid.poke(true.B)
        dut.io.inReady.expect(true.B)
        dut.clock.step()
        dut.io.inValid.poke(false.B)
        var cycles = 0
        while (!dut.io.outValid.peek().litToBoolean && cycles < 20) {
          dut.clock.step()
          cycles += 1
        }
        dut.io.outValid.expect(true.B)
        val got = dut.io.out.peek().litValue
        val gotFlags = dut.io.flags.peek().litValue
        val pass = got == p.expected && gotFlags == p.flags
        val result = s"${p.name}: got=0x${got.toString(16)} expected=0x${p.expected.toString(16)} flags=$gotFlags expectedFlags=${p.flags}"
        println(s"RAW-MX ${if (pass) "PASS" else "FAIL"} $result")
        if (!pass) mismatches += result
        // The pending second block and accumulated flags must stall with D.
        for (_ <- 0 until 2) {
          dut.clock.step()
          dut.io.outValid.expect(true.B)
          dut.io.out.expect(got.U)
          dut.io.flags.expect(gotFlags.U)
        }
        dut.io.outReady.poke(true.B)
        dut.clock.step(3)
        dut.io.outValid.expect(false.B)
      }
      // Collect every mismatch in one bounded run; a known failure must remain
      // red, not be converted into an expected-failure/approximate-accuracy pass.
      assert(mismatches.isEmpty, mismatches.mkString("\n"))
    }
  }

  it should "preserve independent raw-bit unscaled INT8 and FP8 controls" in {
    runRawProbes(Seq(
      raw("INT8 signed C accumulation", 0, 8, Seq.fill(32)(1), Seq.fill(32)(0xfe),
        expected = 0xffffffc5L, c = 5), // 5 + 32 * (1 * -2) = -59.
      raw("unscaled E4M3", 11, 8, Seq.fill(32)(0x38), Seq.fill(32)(0x38),
        expected = 0x42000000L), // 32.
      raw("unscaled E5M2", 12, 8, Seq.fill(32)(0x3c), Seq.fill(32)(0x3c),
        expected = 0x42000000L)
    ))
  }

  it should "match frozen exact MX32 contributions and FP32 RNE raw-bit goldens" in {
    val fp4A = Seq.fill(16)(Seq(1, 2)).flatten
    val fp4B = Seq.fill(16)(Seq(3, 4)).flatten
    val twoBlockOnes = Seq.tabulate(64)(k => if (k == 0 || k == 32) 2 else 0)
    val twoBlockOpposites = Seq.tabulate(64)(k => if (k == 0) 2 else if (k == 32) 10 else 0)
    runRawProbes(Seq(
      raw("FP4 low/high consecutive K = 44", 10, 4, fp4A, fp4B,
        expected = 0x42300000L), // 16 * (.5*1.5 + 1*2).
      raw("FP4 swapped A nibbles = 40", 10, 4, Seq.fill(16)(Seq(2, 1)).flatten, fp4B,
        expected = 0x42200000L), // 16 * (1*1.5 + .5*2), NOT 72.
      raw("FP4 alternating bytes = 72", 10, 4,
        Seq.fill(8)(Seq(1, 2, 3, 4)).flatten, fp4B,
        expected = 0x42900000L), // 8 * (2.75 + 6.25).
      raw("FP4 odd tie", 10, 4, Seq(2), Seq(2),
        expected = 0x3f800002L, c = 0x3f800001L, sa = Seq(103), flags = 1),
      raw("FP4 tiny negative addend", 10, 4, Seq(10), Seq(2),
        expected = 0x3f800000L, c = 0x3f800000L, sa = Seq(0), sb = Seq(0), flags = 1),
      raw("FP4 ordered two half-ulp blocks", 10, 4, twoBlockOnes, Seq.fill(64)(2),
        expected = 0x3f800000L, c = 0x3f800000L, sa = Seq(103, 103), flags = 1),
      raw("FP4 opposite half-ulp blocks", 10, 4, twoBlockOpposites, Seq.fill(64)(2),
        expected = 0x3f7fffffL, c = 0x3f800000L, sa = Seq(103, 103), flags = 1),
      // Each block is finite in the exact domain. First insertion overflows;
      // the second adds a finite -2^128 to +Inf, so it remains +Inf, not NaN.
      raw("FP4 ordered overflowing opposite blocks", 10, 4, twoBlockOpposites, Seq.fill(64)(2),
        expected = 0x7f800000L, sa = Seq(254, 254), sb = Seq(128, 128), flags = 5),
      raw("E5 exact cancellation", 8, 8, Seq(0x7b, 0x3c, 0xfb), Seq(0x7b, 0x3c, 0x7b),
        expected = 0x3f800000L),
      raw("E5 full product lattice cancellation", 8, 8,
        Seq(0x7b, 0x01, 0xfb), Seq(0x7b, 0x01, 0x7b),
        expected = 0x2f800000L), // 2^-32 survives cancellation of max products.
      raw("E5 do not preround block before C", 8, 8, Seq(0x3c, 0x0c), Seq(0x3c, 0x0c),
        expected = 0x33800000L, c = 0xbf800000L), // (1 + 2^-24) - 1.
      raw("E5 midpoint plus minsub C", 8, 8, Seq(0x3c, 0x0c), Seq(0x3c, 0x0c),
        expected = 0x3f800001L, c = 0x00000001L, flags = 1),
      raw("E5 midpoint minus minsub C", 8, 8, Seq(0x3c, 0x0c), Seq(0x3c, 0x0c),
        expected = 0x3f800000L, c = 0x80000001L, flags = 1),
      raw("E4 extreme paired scales stay finite", 7, 8, Seq(0x7e), Seq(0x38),
        expected = 0x43e00000L, sa = Seq(254), sb = Seq(0)), // 448 * 2^0.
      raw("E4 wide contribution cancels finite C", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x73800000L, c = 0xff7fffffL, sa = Seq(254), sb = Seq(128)),
      raw("E4 minsub exact", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x00000001L, sa = Seq(0), sb = Seq(105)),
      raw("E4 half minsub RNE even", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x00000000L, sa = Seq(0), sb = Seq(104), flags = 3),
      raw("E4 half minsub negative zero", 7, 8, Seq(0xb8), Seq(0x38),
        expected = 0x80000000L, sa = Seq(0), sb = Seq(104), flags = 3),
      raw("E4 odd tie", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x3f800002L, c = 0x3f800001L, sa = Seq(103), flags = 1),
      raw("E5 odd tie", 8, 8, Seq(0x3c), Seq(0x3c),
        expected = 0x3f800002L, c = 0x3f800001L, sa = Seq(103), flags = 1),
      raw("FP4 NaN A scale with zero data", 10, 4, Seq(0), Seq(2),
        expected = 0x7fc00000L, sa = Seq(255)),
      raw("E4 NaN B scale", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x7fc00000L, sb = Seq(255))
    ), frozenFixture = true)
  }

  it should "preserve MX block IEEE classification and sticky flags" in {
    runRawProbes(Seq(
      raw("FP4 full-width maximum coefficient", 10, 4, Seq.fill(64)(7), Seq.fill(64)(7),
        expected = 0x45100000L), // 64*36=2304; catches overflow in the old 16-term sum.
      raw("E4 maximum coefficient", 7, 8, Seq.fill(32)(0x7e), Seq.fill(32)(0x7e),
        expected = 0x4ac40000L), // 32*448^2=6422528.
      raw("E5 maximum coefficient", 8, 8, Seq.fill(32)(0x7b), Seq.fill(32)(0x7b),
        expected = 0x51c40000L), // 32*57344^2.
      raw("FP4 all negative zeros", 10, 4, Seq.fill(64)(8), Seq.fill(64)(2),
        expected = 0x80000000L, c = 0x80000000L),
      raw("E5 all negative zeros", 8, 8, Seq.fill(32)(0x80), Seq.fill(32)(0x3c),
        expected = 0x80000000L, c = 0x80000000L),
      raw("E5 sNaN product", 8, 8, Seq(0x7d), Seq(0x3c),
        expected = 0x7fc00000L, flags = 16),
      raw("E5 qNaN product", 8, 8, Seq(0x7e), Seq(0x3c),
        expected = 0x7fc00000L),
      raw("E5 zero times infinity", 8, 8, Seq(0), Seq(0x7c),
        expected = 0x7fc00000L, flags = 16),
      raw("E5 opposite real infinities", 8, 8, Seq(0x7c, 0xfc), Seq(0x3c, 0x3c),
        expected = 0x7fc00000L, flags = 16),
      raw("E5 qNaN prefix suppresses opposite-inf add", 8, 8,
        Seq(0x7e, 0x7c, 0xfc), Seq(0x3c, 0x3c, 0x3c), expected = 0x7fc00000L),
      raw("E5 qNaN prefix retains product NV", 8, 8,
        Seq(0x7e, 0), Seq(0x3c, 0x7c), expected = 0x7fc00000L, flags = 16),
      raw("E5 NaN A scale ignores zero times infinity", 8, 8, Seq(0), Seq(0x7c),
        expected = 0x7fc00000L, sa = Seq(255)),
      raw("E5 NaN B scale ignores sNaN", 8, 8, Seq(0x7d), Seq(0x3c),
        expected = 0x7fc00000L, sb = Seq(255)),
      raw("E5 NaN A scale ignores qNaN", 8, 8, Seq(0x7e), Seq(0x3c),
        expected = 0x7fc00000L, sa = Seq(255)),
      raw("E5 NaN B scale ignores positive infinity", 8, 8, Seq(0x7c), Seq(0x3c),
        expected = 0x7fc00000L, sb = Seq(255)),
      raw("E5 NaN A scale ignores negative infinity", 8, 8, Seq(0xfc), Seq(0x3c),
        expected = 0x7fc00000L, sa = Seq(255)),
      raw("E5 NaN B scale ignores opposite infinities", 8, 8, Seq(0x7c, 0xfc), Seq(0x3c, 0x3c),
        expected = 0x7fc00000L, sb = Seq(255)),
      // E4M3 has no Inf/sNaN encodings; 0x7f/0xff are quiet NaNs.
      raw("E4 NaN A scale ignores NaN elements", 7, 8, Seq(0x7f), Seq(0x38),
        expected = 0x7fc00000L, sa = Seq(255)),
      raw("E4 NaN B scale ignores signed maximum elements", 7, 8, Seq(0x7e, 0xfe), Seq(0x38, 0x38),
        expected = 0x7fc00000L, sb = Seq(255)),
      raw("E5 NaN scale does not suppress external C sNaN", 8, 8, Seq(0x7d), Seq(0x3c),
        expected = 0x7fc00000L, sa = Seq(255), c = 0x7f800001L, flags = 16),
      raw("E4 C signaling NaN", 7, 8, Seq(0x38), Seq(0x38),
        expected = 0x7fc00000L, c = 0x7f800001L, flags = 16),
      raw("E4 far negative contribution from positive zero", 7, 8, Seq(0xb8), Seq(0x38),
        expected = 0x80000000L, sa = Seq(0), sb = Seq(0), flags = 3),
      raw("E5 negative-zero C plus positive-zero block", 8, 8, Seq.fill(32)(0), Seq.fill(32)(0x3c),
        expected = 0x00000000L, c = 0x80000000L),
      raw("E5 positive-zero C plus negative-zero block", 8, 8, Seq.fill(32)(0x80), Seq.fill(32)(0x3c),
        expected = 0x00000000L),
      raw("E5 C opposite real infinity", 8, 8, Seq(0xfc), Seq(0x3c),
        expected = 0x7fc00000L, c = 0x7f800000L, flags = 16),
      raw("FP4 sticky OF then qNaN", 10, 4,
        Seq.tabulate(64)(k => if (k == 0 || k == 32) 2 else 0), Seq.fill(64)(2),
        expected = 0x7fc00000L, sa = Seq(254, 255), sb = Seq(128, 127), flags = 5),
      raw("FP4 sticky UF then exact", 10, 4,
        Seq.tabulate(64)(k => if (k == 0 || k == 32) 2 else 0), Seq.fill(64)(2),
        expected = 0x3f800000L, sa = Seq(0, 127), sb = Seq(104, 127), flags = 3)
    ))
  }

  it should "execute exact live K32 K33 K64 and K65 tails without evaluating padding" in {
    val negative33 = Seq.tabulate(64)(i => if (i < 33) 8 else 0)
    val distinctA = Seq.fill(32)(2) ++ Seq.fill(32)(5) // 1 then 3.
    val probes = Seq(
      raw("K64 two distinct A/B scales", 10, 4, distinctA, Seq.fill(64)(2),
        0x44480000L, sa = Seq(127, 129), sb = Seq(127, 128)).copy(liveLanes = 64), // 32+768=800.
      raw("K64 swapped A scales adversary", 10, 4, distinctA, Seq.fill(64)(2),
        0x43a00000L, sa = Seq(129, 127), sb = Seq(127, 128)).copy(liveLanes = 64), // 128+192=320.
      raw("K64 swapped B scales adversary", 10, 4, distinctA, Seq.fill(64)(2),
        0x43e00000L, sa = Seq(127, 129), sb = Seq(128, 127)).copy(liveLanes = 64), // 64+384=448.
      raw("K64 inverse A/B scale order", 10, 4, distinctA, Seq.fill(64)(2),
        0x43b00000L, sa = Seq(129, 127), sb = Seq(128, 127)).copy(liveLanes = 64), // 256+96=352.
      raw("K31 excludes stale upper carrier", 10, 4,
        Seq.fill(31)(2) ++ Seq.fill(33)(7), Seq.fill(64)(2),
        0x41f80000L).copy(liveLanes = 31),
      raw("K63 excludes stale final lane", 10, 4,
        Seq.fill(63)(2) ++ Seq(7), Seq.fill(64)(2),
        0x427c0000L).copy(liveLanes = 63),
      raw("K64 two RNE not global reduction", 10, 4,
        Seq.tabulate(64)(i => if (i == 0) 2 else if (i == 32) 10 else 0),
        Seq.fill(64)(2), 0x4b7fffffL, c = 0x4b800000L, flags = 1).copy(liveLanes = 64),
      raw("K32 inactive second block preserves negative zero", 10, 4,
        negative33, Seq.fill(64)(2), 0x80000000L, c = 0x80000000L,
        sa = Seq(127, 255)).copy(liveLanes = 32),
      raw("K33 ignores 31 positive zero padding lanes", 10, 4,
        negative33, Seq.fill(64)(2), 0x80000000L, c = 0x80000000L).copy(liveLanes = 33),
      raw("K64 real positive zero lanes are not padding", 10, 4,
        negative33, Seq.fill(64)(2), 0, c = 0x80000000L).copy(liveLanes = 64),
      raw("K64 real all-zero second block must add", 10, 4,
        Seq.fill(32)(8) ++ Seq.fill(32)(0), Seq.fill(64)(2),
        0, c = 0x80000000L).copy(liveLanes = 64),
      raw("K64 active zero block with NaN scale", 10, 4,
        Seq.fill(64)(0), Seq.fill(64)(2), 0x7fc00000L,
        sa = Seq(127, 255)).copy(liveLanes = 64),
      raw("K33 excludes poisoned finite padding products", 10, 4,
        Seq.fill(33)(2) ++ Seq.fill(31)(7), Seq.fill(64)(2),
        0x42040000L).copy(liveLanes = 33), // 33.
      raw("K65 first K64 tuple", 10, 4, Seq.fill(64)(2), Seq.fill(64)(2),
        0x42800000L).copy(liveLanes = 64), // 64.
      raw("K65 second tuple has one live lane", 10, 4,
        Seq.fill(64)(2), Seq.fill(64)(2), 0x42820000L, c = 0x42800000L,
        sa = Seq(127, 255)).copy(liveLanes = 1), // 64+1=65.
      raw("E5 partial classification ignores padding sNaNs", 8, 8,
        Seq(0x80) ++ Seq.fill(31)(0x7d), Seq.fill(32)(0x3c),
        0x80000000L, c = 0x80000000L).copy(liveLanes = 1),
      raw("E5 active sNaN still raises NV", 8, 8,
        Seq(0x7d), Seq(0x3c), 0x7fc00000L, flags = 16).copy(liveLanes = 1),
      raw("E5 active scale NaN suppresses product NV", 8, 8,
        Seq(0x7d), Seq(0x3c), 0x7fc00000L, sa = Seq(255)).copy(liveLanes = 1),
      raw("E5 scale NaN does not suppress external C NV", 8, 8,
        Seq(0x7d), Seq(0x3c), 0x7fc00000L, c = 0x7f800001L,
        sa = Seq(255), flags = 16).copy(liveLanes = 1)
    )
    val finiteCases = probes.filter { p =>
      val live = if (p.liveLanes < 0) 64 else p.liveLanes
      p.opcode == 10 && !p.c.testBit(31) && ((p.c >> 23) & 255) != 255 &&
        (0 until (live + 31) / 32).forall { block =>
          ((p.scaleA >> (block * 8)) & 255) != 255 && ((p.scaleB >> (block * 8)) & 255) != 255
        }
    }
    finiteCases.foreach(p => assert(finiteFp4Golden(p) == p.expected, s"integer partial-K oracle: ${p.name}"))
    println(s"PARTIAL-K-GOLDEN PASS ${finiteCases.size} exact integer/dyadic checks")
    runRawProbes(probes)
  }

  it should "keep mixed MX results and flags atomic through pipeline backpressure" in {
    val modes = Seq(
      raw("FP4 odd", 10, 4, Seq(2), Seq(2), 0x3f800002L,
        c = 0x3f800001L, sa = Seq(103), flags = 1),
      raw("E5 exact", 8, 8, Seq(0x7b, 1, 0xfb), Seq(0x7b, 1, 0x7b), 0x2f800000L),
      raw("E4 negative underflow", 7, 8, Seq(0xb8), Seq(0x38), 0x80000000L,
        sa = Seq(0), sb = Seq(104), flags = 3),
      raw("INT8", 0, 8, Seq.fill(32)(1), Seq.fill(32)(0xfe), 0xffffffc5L, c = 5),
      raw("E5 invalid", 8, 8, Seq(0x7d), Seq(0x3c), 0x7fc00000L, flags = 16),
      raw("FP4 ordered overflow", 10, 4,
        Seq.tabulate(64)(k => if (k == 0) 2 else if (k == 32) 10 else 0),
        Seq.fill(64)(2), 0x7f800000L, sa = Seq(254, 254), sb = Seq(128, 128), flags = 5),
      raw("K32 held live mask", 10, 4, Seq.fill(32)(8), Seq.fill(64)(2),
        0x80000000L, c = 0x80000000L, sa = Seq(127, 255)).copy(liveLanes = 32),
      raw("K64 immediately after held K32", 10, 4, Seq.fill(64)(2), Seq.fill(64)(2),
        0x42800000L).copy(liveLanes = 64),
      raw("K33 held partial block", 10, 4, Seq.fill(33)(8), Seq.fill(64)(2),
        0x80000000L, c = 0x80000000L).copy(liveLanes = 33),
      raw("K65 held tail", 10, 4, Seq.fill(64)(2), Seq.fill(64)(2),
        0x42820000L, c = 0x42800000L, sa = Seq(127, 255)).copy(liveLanes = 1)
    )
    val probes = modes ++ modes
    test(new ProbeTop(enableMxfp4 = true, enableNvfp4 = false,
        enableMxfp8 = true, rawInputs = true, liveInputs = true))
        .withAnnotations(if (sys.env.get("CUTE_TEST_VERILATOR").contains("1"))
          Seq(VerilatorBackendAnnotation) else Seq.empty) { dut =>
      dut.io.inValid.poke(false.B)
      dut.io.outReady.poke(false.B)
      dut.io.finiteByte.poke(0.U)
      var sent = 0
      var received = 0
      var cycle = 0
      var inputStalled = false
      var outputStalled = false
      var held = Option.empty[(BigInt, BigInt)]
      val accepts = scala.collection.mutable.ArrayBuffer.empty[Int]
      while ((sent < probes.length || received < probes.length) && cycle < 100) {
        val probe = probes(math.min(sent, probes.length - 1))
        dut.io.opcode.poke(probe.opcode.U)
        dut.io.liveK.get.poke((if (probe.liveLanes < 0) probe.carrierLanes else probe.liveLanes).U)
        dut.io.rawA.get.poke(probe.a.U); dut.io.rawB.get.poke(probe.b.U)
        dut.io.rawC.get.poke(probe.c.U)
        dut.io.scaleA.poke(probe.scaleA.U); dut.io.scaleB.poke(probe.scaleB.U)
        dut.io.inValid.poke((sent < probes.length).B)
        val ready = cycle % 17 >= 10
        dut.io.outReady.poke(ready.B)
        val valid = dut.io.outValid.peek().litToBoolean
        held.foreach { case (data, flags) =>
          assert(valid)
          dut.io.out.expect(data.U)
          dut.io.flags.expect(flags.U)
        }
        held = if (valid && !ready) {
          outputStalled = true
          Some((dut.io.out.peek().litValue, dut.io.flags.peek().litValue))
        } else None
        if (valid && ready) {
          assert(received < sent, "output must belong to an accepted token")
          dut.io.out.expect(probes(received).expected.U)
          dut.io.flags.expect(probes(received).flags.U)
          received += 1
        }
        if (sent < probes.length) {
          if (dut.io.inReady.peek().litToBoolean) {
            accepts += cycle
            sent += 1
          } else inputStalled = true
        }
        dut.clock.step()
        cycle += 1
      }
      assert(sent == probes.length && received == probes.length)
      assert(inputStalled && outputStalled)
      assert(accepts.sliding(6).exists(w => w.length == 6 && w.last - w.head == 5),
        "the unchanged six-register pipeline must still accept at II=1")
      dut.io.inValid.poke(false.B); dut.io.outReady.poke(true.B)
      dut.clock.step(2)
      dut.io.outValid.expect(false.B)
      println(s"MIXED-MX PASS accepted=$sent emitted=$received cycles=$cycle II=1 output/flags held under stalls")
    }
  }
}
