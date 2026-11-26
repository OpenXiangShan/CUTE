
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._

//BMemoryLoader，用于加载B矩阵的数据，供给MatrixReg使用
//从不同的存储介质中加载数据，供给MatrixReg使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据MatrixReg的设计，完成Tensor的切分以及将数据的填入MatrixReg

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

class BSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(ABMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(ABMatrixRegBankNEntries).W)
    val MatrixRegisTail = Bool()
    val BeatIndex = UInt(log2Ceil(ABMatrixRegEntryByteSize).W)
    val ReadByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val WriteByteOffset = UInt(log2Ceil(outsideDataWidthByte).W)
    val FragmentBytes = UInt(log2Ceil(outsideDataWidthByte + 1).W)
    val ConvertRow = UInt(MatrixRegMaxTensorDimBitSize.W)
    val ConvertBeat = UInt(2.W)
}

// Exact F32 * (1/64) -> INT8 with RNE, NaN->0, and saturation to [-127,127].
// The fixed power-of-two scale is folded into the F32 exponent, avoiding a
// floating multiplier in the loader fill path.
object F32ToI8FixedScale {
    def apply(bits: UInt): UInt = {
        require(bits.getWidth == 32)
        val sign = bits(31)
        val exp = bits(30, 23)
        val mantissa = bits(22, 0)
        val isNaN = exp.andR && mantissa.orR
        val isInf = exp.andR && !mantissa.orR
        val significand = Cat(1.U(1.W), mantissa).pad(32)
        val shift = 156.U(9.W) - exp.pad(9)
        val truncated = (significand >> shift)(31, 0)
        val remainderMask = (1.U(32.W) << shift) - 1.U(32.W)
        val remainder = significand & remainderMask
        val halfway = 1.U(32.W) << (shift - 1.U)
        val roundUp = remainder > halfway || (remainder === halfway && truncated(0))
        val roundedWide = truncated + roundUp.asUInt
        val rounded = roundedWide(7, 0)
        val magnitude = Mux(
            exp <= 131.U,
            0.U(8.W),
            Mux(
                exp >= 140.U,
                127.U(8.W),
                Mux(rounded > 127.U, 127.U(8.W), rounded)
            )
        )
        val signedMagnitude = Mux(sign, (0.U(8.W) - magnitude), magnitude)
        Mux(
            isNaN,
            0.U(8.W),
            Mux(isInf, Mux(sign, 129.U(8.W), 127.U(8.W)), signedMagnitude)
        )
    }
}

// Supplied-scale MX encoder primitives. These do not connect to production
// BML, scale SRAMs, dynamic scale selection or architectural flags.
// The FP4 caller still owns architectural admission, including NX-sticky.

// MXFP8 supplied-scale lane prototype. Reuse Fudian's exact normalization and
// rounding rather than introducing a second IEEE converter. Neither fflags
// output below is an architectural flag update. CPU scale selection and
// admission remain outside this primitive.
object Mxfp8EncodeLane {
    def apply(bits: UInt, e: UInt, active: Bool, e4m3: Boolean): (UInt, Bool, Bool) = {
        val magnitude = bits(30, 0)
        val special = bits(30, 23).andR
        val invalid = active && special && bits(22, 0).orR
        // max finite is 1.75*2^8 or 1.75*2^15. These envelope thresholds
        // are normal FP32 values even at e=0; larger exponents admit every
        // finite FP32 without wrapping the threshold's exponent field.
        val envelopeExponent = e.pad(9) + (if (e4m3) 8 else 15).U(9.W)
        val envelope = Cat(envelopeExponent(7, 0), 0x600000.U(23.W))
        val eligible = !active || (!special &&
            (envelopeExponent >= 255.U || magnitude <= envelope))

        val widen = Module(new fudian.FPUpConverter(8, 24, 11, 25))
        widen.io.in := Mux(active, bits, 0.U(32.W))
        widen.io.rm := 0.U // exact widening, including original subnormals
        val wideExponent = widen.io.result(34, 24)
        // MXFP8 d=2^(e-127). E4M3FN is represented temporarily as E5M3
        // y/256: identical subnormal grid and biased exponent0..15, with
        // no IEEE-special exponent in the admitted E4M3FN envelope.
        val adjustedExponent = wideExponent.zext +
            (if (e4m3) 119 else 127).S(13.W) - e.zext
        val adjustedExponentBits = adjustedExponent.asUInt
        val scaled = Cat(widen.io.result(35),
            Mux(wideExponent.orR, adjustedExponentBits(10, 0), 0.U(11.W)),
            widen.io.result(23, 0))
        val narrow = Module(new fudian.FPDownConverter(11, 25, 5, if (e4m3) 4 else 3))
        narrow.io.in := scaled
        narrow.io.rm := 0.U // fixed RNE, independent of scalar/matrix CSR
        val encoded = if (e4m3) {
            Cat(narrow.io.result(8), narrow.io.result(6, 0))
        } else {
            narrow.io.result
        }
        (Mux(active, encoded, 0.U(8.W)), invalid, eligible)
    }
}

// Stage-A scale-result primitive for one fully active K32. This is NOT an
// unrestricted replacement for CPU log2f/floorf or their accrued flags.
// The caller owns scalar-NX admission and CPU fallback outside this guard.
// No architectural rounding mode or flags are read or written here.
class Mxfp4DynamicScale extends Module {
    val io = IO(new Bundle {
        val values = Input(Vec(32, UInt(32.W)))
        val amax = Output(UInt(31.W)) // unsigned raw-F32 magnitude, including rejects
        val e = Output(UInt(8.W))
        val supported = Output(Bool())
    })

    // Pair adjacent nodes at each elaboration level: 32 -> 16 -> 8 -> 4 -> 2
    // -> 1, i.e. five comparator levels, not a left-fold's 31-level chain.
    private def balancedMax(nodes: Seq[UInt]): UInt = {
        if (nodes.size == 1) nodes.head
        else balancedMax(nodes.grouped(2).map { pair =>
            Mux(pair(0) >= pair(1), pair(0), pair(1))
        }.toSeq)
    }
    val maximum = balancedMax(io.values.map(_(30, 0)).toSeq)
    // Inspect EVERY lane: magnitude max alone is not a finite-input check.
    val hasSpecial = VecInit(io.values.map(_(30, 23).andR)).asUInt.orR
    val exponent = maximum(30, 23)
    val fraction = maximum(22, 0)
    val zero = maximum === 0.U
    val guardedFraction = fraction === 0.U ||
        (fraction >= "h000400".U(23.W) && fraction <= "h7ffc00".U(23.W))
    val guardedNonzero = exponent >= 2.U && exponent <= 253.U && guardedFraction

    io.amax := maximum
    io.supported := !hasSpecial && (zero || guardedNonzero)
    // All +/-zero has e0. Exact powers in the guarded exponent range are
    // supported; rejected near-power/tiny/special inputs return e0 as well.
    io.e := Mux(io.supported && !zero, exponent - 2.U, 0.U(8.W))
}

// Shared supplied-scale FP4 lane logic. The block encoder shares one boundary
// set across its lanes; other execution owners may supply per-lane scales.
// This is the existing strict-first dyadic search, not a generic RNE converter.
object Mxfp4EncodeLane {
    def boundaries(e: UInt): Seq[UInt] = {
        require(e.getWidth == 8)
        def boundary(exponentOffset: Int, fraction: Int, subnormals: Seq[Int] = Seq.empty): UInt = {
            val exponent = if (exponentOffset >= 0) {
                e.pad(9) + exponentOffset.U(9.W)
            } else {
                e.pad(9) - (-exponentOffset).U(9.W)
            }
            val normal = Cat(exponent(7, 0), fraction.U(23.W))
            if (subnormals.isEmpty) {
                normal
            } else {
                MuxLookup(e, normal)(subnormals.zipWithIndex.map { case (bits, scale) =>
                    scale.U -> bits.U(31.W)
                })
            }
        }
        Seq(
            boundary(-2, 0x000000, Seq(0x00100000, 0x00200000, 0x00400000)), // 0.5d
            boundary(-1, 0x400000, Seq(0x00300000, 0x00600000)),             // 1.5d
            boundary(0, 0x200000, Seq(0x00500000)),                          // 2.5d
            boundary(0, 0x600000, Seq(0x00700000)),                          // 3.5d
            boundary(1, 0x200000),                                         // 5d
            boundary(1, 0x600000),                                         // 7d
            boundary(2, 0x200000)                                          // 10d
        )
    }

    def maxMagnitude(e: UInt): UInt = {
        require(e.getWidth == 8)
        val exponent = e.pad(9) + 3.U(9.W)
        Cat(exponent(7, 0), 0.U(23.W)) // 16d; finite through e=251.
    }

    // Scale-range qualification remains the caller's responsibility, just as
    // in the original block encoder. Inactive lanes have code0/no invalid.
    def apply(bits: UInt, active: Bool, thresholds: Seq[UInt], maximum: UInt): (UInt, Bool, Bool) = {
        require(bits.getWidth == 32 && thresholds.length == 7)
        val magnitude = bits(30, 0)
        val special = bits(30, 23).andR
        val isNaN = special && bits(22, 0).orR
        val isInf = special && !bits(22, 0).orR
        val magnitudeCode = PopCount(thresholds.map(magnitude > _))
        val code = Mux(
            active && !special && magnitudeCode.orR,
            Cat(bits(31), magnitudeCode),
            0.U(4.W)
        )
        val invalid = active && isNaN
        val eligible = !active || isNaN || (!isInf && magnitude <= maximum)
        (code, invalid, eligible)
    }
}

// d=2^(e-128). Elaboration selects either the C two-slot/four-group control
// with eight real lane datapaths, or the A/E single-output-slot control with
// 32 datapaths. Payload nibble j belongs to continuous K lane j.
class Mxfp4EncodeBlock(val lanes: Int = 8) extends Module {
    require(lanes == 8 || lanes == 32, "MXFP4 encoder supports only 8 or 32 lanes")
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val values = Vec(32, UInt(32.W))
            val e = UInt(8.W)
            val active = UInt(32.W)
        }))
        val out = Decoupled(new Bundle {
            val payload = UInt(128.W)
            val scale = UInt(8.W)
            val invalid = Bool()
            val eligible = Bool()
        })
    })

    // The eight-lane branch selects the registered group BEFORE these lane
    // datapaths. The 32-lane branch instead connects the current full input.
    val laneValues = Wire(Vec(lanes, UInt(32.W)))
    val laneActive = Wire(Vec(lanes, Bool()))
    val laneScale = Wire(UInt(8.W))

    // C's seven exact dyadic boundaries, shared across the selected lanes.
    // Unsigned positive FP32 bit order also covers the e=0/1/2 subnormals.
    val boundaries = Mxfp4EncodeLane.boundaries(laneScale)
    val maxMagnitude = Mxfp4EncodeLane.maxMagnitude(laneScale)
    val laneCodes = Wire(Vec(lanes, UInt(4.W)))
    val laneInvalid = Wire(Vec(lanes, Bool()))
    val laneEligible = Wire(Vec(lanes, Bool()))
    for (lane <- 0 until lanes) {
        val (code, invalid, eligible) = Mxfp4EncodeLane(
            laneValues(lane), laneActive(lane), boundaries, maxMagnitude)
        laneCodes(lane) := code
        laneInvalid(lane) := invalid
        laneEligible(lane) := eligible
    }

    val resetting = reset.asBool
    if (lanes == 8) {
        val s_idle :: s_encode :: Nil = Enum(2)
        val state = RegInit(s_idle)
        val valuesReg = Reg(Vec(32, UInt(32.W)))
        val activeReg = Reg(UInt(32.W))
        val scaleReg = Reg(UInt(8.W))
        val group = RegInit(0.U(2.W))
        val packedGroups = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
        val invalidReg = RegInit(false.B)
        val eligibleReg = RegInit(false.B)
        for (lane <- 0 until lanes) {
            val sourceIndex = Cat(group, lane.U(3.W))
            laneValues(lane) := valuesReg(sourceIndex)
            laneActive(lane) := activeReg(sourceIndex)
        }
        laneScale := scaleReg

        val outputValidReg = RegInit(false.B)
        val outputPayloadReg = Reg(UInt(128.W))
        val outputScaleReg = Reg(UInt(8.W))
        val outputInvalidReg = Reg(Bool())
        val outputEligibleReg = Reg(Bool())
        val outputAccept = !outputValidReg || io.out.ready
        val finalGroup = !resetting && state === s_encode && group === 3.U
        val completionAccepted = finalGroup && outputAccept
        io.in.ready := !resetting && (state === s_idle || completionAccepted)
        io.out.valid := !resetting && outputValidReg
        io.out.bits.payload := outputPayloadReg
        io.out.bits.scale := outputScaleReg
        io.out.bits.invalid := outputInvalidReg
        io.out.bits.eligible := outputEligibleReg

        val finalInvalid = invalidReg || laneInvalid.asUInt.orR
        val finalEligible = eligibleReg && laneEligible.asUInt.andR
        val finalPayload = VecInit(
            packedGroups(0), packedGroups(1), packedGroups(2), laneCodes.asUInt
        ).asUInt
        val replacementFire = completionAccepted && io.in.valid
        when(completionAccepted) {
            outputPayloadReg := Mux(finalEligible, finalPayload, 0.U(128.W))
            outputScaleReg := scaleReg
            outputInvalidReg := finalInvalid
            outputEligibleReg := finalEligible
            when(replacementFire) {
                valuesReg := io.in.bits.values
                activeReg := io.in.bits.active
                scaleReg := io.in.bits.e
                group := 0.U
                packedGroups := 0.U.asTypeOf(packedGroups)
                invalidReg := false.B
                eligibleReg := io.in.bits.e <= 251.U
            }.otherwise {
                state := s_idle
            }
        }.elsewhen(state === s_idle && io.in.fire) {
            valuesReg := io.in.bits.values
            activeReg := io.in.bits.active
            scaleReg := io.in.bits.e
            group := 0.U
            packedGroups := 0.U.asTypeOf(packedGroups)
            invalidReg := false.B
            eligibleReg := io.in.bits.e <= 251.U
            state := s_encode
        }.elsewhen(state === s_encode && group =/= 3.U) {
            packedGroups(group) := laneCodes.asUInt
            invalidReg := invalidReg || laneInvalid.asUInt.orR
            eligibleReg := eligibleReg && laneEligible.asUInt.andR
            group := group + 1.U
        }

        // Completion must win over simultaneous retirement of the old output.
        when(completionAccepted) {
            outputValidReg := true.B
        }.elsewhen(io.out.fire) {
            outputValidReg := false.B
        }
    } else {
        laneValues := io.in.bits.values
        laneActive := VecInit(io.in.bits.active.asBools)
        laneScale := io.in.bits.e
        val payloadReg = Reg(UInt(128.W))
        val scaleReg = Reg(UInt(8.W))
        val invalidReg = Reg(Bool())
        val eligibleReg = Reg(Bool())
        val validReg = RegInit(false.B)
        io.in.ready := !resetting && (!validReg || io.out.ready)
        io.out.valid := !resetting && validReg
        io.out.bits.payload := payloadReg
        io.out.bits.scale := scaleReg
        io.out.bits.invalid := invalidReg
        io.out.bits.eligible := eligibleReg
        val outputFire = io.out.fire
        val inputFire = io.in.fire
        when(outputFire) {
            validReg := false.B
        }
        when(inputFire) {
            val eligible = io.in.bits.e <= 251.U && laneEligible.asUInt.andR
            payloadReg := Mux(eligible, laneCodes.asUInt, 0.U(128.W))
            scaleReg := io.in.bits.e
            invalidReg := laneInvalid.asUInt.orR
            eligibleReg := eligible
            validReg := true.B
        }
    }
}

// d=2^(e-127). The F lane helper owns exact normalization, RNE and signed
// zero. Select F's eight-lane work/output slots or E's 32-lane output slot
// at elaboration; payload byte j always belongs to continuous K lane j.
class Mxfp8EncodeBlock(val e4m3: Boolean, val lanes: Int = 8) extends Module {
    require(lanes == 8 || lanes == 32, "MXFP8 encoder supports only 8 or 32 lanes")
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val values = Vec(32, UInt(32.W))
            val e = UInt(8.W)
            val active = UInt(32.W)
        }))
        val out = Decoupled(new Bundle {
            val payload = UInt(256.W)
            val scale = UInt(8.W)
            val invalid = Bool()
            val eligible = Bool()
        })
    })

    val laneValues = Wire(Vec(lanes, UInt(32.W)))
    val laneActive = Wire(Vec(lanes, Bool()))
    val laneScale = Wire(UInt(8.W))
    val laneCodes = Wire(Vec(lanes, UInt(8.W)))
    val laneInvalid = Wire(Vec(lanes, Bool()))
    val laneEligible = Wire(Vec(lanes, Bool()))
    for (lane <- 0 until lanes) {
        val (code, invalid, eligible) = Mxfp8EncodeLane(
            laneValues(lane), laneScale, laneActive(lane), e4m3)
        laneCodes(lane) := code
        laneInvalid(lane) := invalid
        laneEligible(lane) := eligible
    }

    val resetting = reset.asBool
    if (lanes == 8) {
        val s_idle :: s_encode :: Nil = Enum(2)
        val state = RegInit(s_idle)
        val valuesReg = Reg(Vec(32, UInt(32.W)))
        val activeReg = Reg(UInt(32.W))
        val scaleReg = Reg(UInt(8.W))
        val group = RegInit(0.U(2.W))
        val packedGroups = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
        val invalidReg = RegInit(false.B)
        val eligibleReg = RegInit(false.B)
        for (lane <- 0 until lanes) {
            val sourceIndex = Cat(group, lane.U(3.W))
            laneValues(lane) := valuesReg(sourceIndex)
            laneActive(lane) := activeReg(sourceIndex)
        }
        laneScale := scaleReg

        val outputValidReg = RegInit(false.B)
        val outputPayloadReg = Reg(UInt(256.W))
        val outputScaleReg = Reg(UInt(8.W))
        val outputInvalidReg = Reg(Bool())
        val outputEligibleReg = Reg(Bool())
        val outputAccept = !outputValidReg || io.out.ready
        val finalGroup = !resetting && state === s_encode && group === 3.U
        val completionAccepted = finalGroup && outputAccept
        io.in.ready := !resetting && (state === s_idle || completionAccepted)
        io.out.valid := !resetting && outputValidReg
        io.out.bits.payload := outputPayloadReg
        io.out.bits.scale := outputScaleReg
        io.out.bits.invalid := outputInvalidReg
        io.out.bits.eligible := outputEligibleReg

        val finalInvalid = invalidReg || laneInvalid.asUInt.orR
        val finalEligible = eligibleReg && laneEligible.asUInt.andR
        val finalPayload = VecInit(
            packedGroups(0), packedGroups(1), packedGroups(2), laneCodes.asUInt
        ).asUInt
        val replacementFire = completionAccepted && io.in.valid
        when(completionAccepted) {
            outputPayloadReg := Mux(finalEligible, finalPayload, 0.U(256.W))
            outputScaleReg := scaleReg
            outputInvalidReg := finalInvalid
            outputEligibleReg := finalEligible
            when(replacementFire) {
                valuesReg := io.in.bits.values
                activeReg := io.in.bits.active
                scaleReg := io.in.bits.e
                group := 0.U
                packedGroups := 0.U.asTypeOf(packedGroups)
                invalidReg := false.B
                eligibleReg := io.in.bits.e <= 254.U
            }.otherwise {
                state := s_idle
            }
        }.elsewhen(state === s_idle && io.in.fire) {
            valuesReg := io.in.bits.values
            activeReg := io.in.bits.active
            scaleReg := io.in.bits.e
            group := 0.U
            packedGroups := 0.U.asTypeOf(packedGroups)
            invalidReg := false.B
            eligibleReg := io.in.bits.e <= 254.U
            state := s_encode
        }.elsewhen(state === s_encode && group =/= 3.U) {
            packedGroups(group) := laneCodes.asUInt
            invalidReg := invalidReg || laneInvalid.asUInt.orR
            eligibleReg := eligibleReg && laneEligible.asUInt.andR
            group := group + 1.U
        }

        when(completionAccepted) {
            outputValidReg := true.B
        }.elsewhen(io.out.fire) {
            outputValidReg := false.B
        }
    } else {
        laneValues := io.in.bits.values
        laneActive := VecInit(io.in.bits.active.asBools)
        laneScale := io.in.bits.e
        val payloadReg = Reg(UInt(256.W))
        val scaleReg = Reg(UInt(8.W))
        val invalidReg = Reg(Bool())
        val eligibleReg = Reg(Bool())
        val validReg = RegInit(false.B)
        io.in.ready := !resetting && (!validReg || io.out.ready)
        io.out.valid := !resetting && validReg
        io.out.bits.payload := payloadReg
        io.out.bits.scale := scaleReg
        io.out.bits.invalid := invalidReg
        io.out.bits.eligible := eligibleReg
        val outputFire = io.out.fire
        val inputFire = io.in.fire
        when(outputFire) {
            validReg := false.B
        }
        when(inputFire) {
            val eligible = io.in.bits.e <= 254.U && laneEligible.asUInt.andR
            payloadReg := Mux(eligible, laneCodes.asUInt, 0.U(256.W))
            scaleReg := io.in.bits.e
            invalidReg := laneInvalid.asUInt.orR
            eligibleReg := eligible
            validReg := true.B
        }
    }
}

//对于卷积，数据摆放是[khkwoc][ic],对于矩阵乘，数据摆放是[N][K]

class BMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个 MatrixReg 的接口的总体设计
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new BMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
        val FromScaleRegIO = Option.when(ExperimentalMxfpConvertLoad)(
            Flipped(new ABScaleControlMatrixRegIO))
        val ScaleBankId = Output(UInt(ABScaleRegIdWidth.W))
        val ScaleReadActive = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(
            Decoupled(new MxfpGeneratedScale))
    })
    // 对外统一使用 ToMatrixRegIO

    io.ToMatrixRegIO.active := false.B
    io.ToMatrixRegIO.BankAddr.map(_.valid := false.B)
    io.ToMatrixRegIO.BankAddr.map(_.bits := DontCare)
    io.ToMatrixRegIO.Data.map(_.valid := false.B)
    io.ToMatrixRegIO.Data.map(_.bits := DontCare)
    io.ToMatrixRegIO.ByteMask.map(_.valid := false.B)
    io.ToMatrixRegIO.ByteMask.map(_.bits := Fill(ABMatrixRegEntryByteSize, true.B))

    // Initialize all channels, but legacy BML only uses channel 0
    for (i <- 0 until ABMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := DontCare
        io.LocalMMUIO.Response(i).ready := false.B
    }

    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTaskFlags := 0.U

    if (EnableDifftest) {
      val pcReg = RegInit(0.U(64.W))
      val ownerRetireId = RegEnable(io.ConfigInfo.retireId.get, 0.U(64.W), io.ConfigInfo.MicroTaskValid)
      val ownerEpoch = RegEnable(io.ConfigInfo.epoch.get, 0.U(64.W), io.ConfigInfo.MicroTaskValid)
      val ownerCoreId = RegEnable(io.ConfigInfo.coreid.get, 0.U(8.W), io.ConfigInfo.MicroTaskValid)
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent(ABMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits), delay = 0, dontCare = true)
        difftestAmuFinish.clearScaleTrace()
        // 默认值初始化
        difftestAmuFinish.coreid := ownerCoreId
        difftestAmuFinish.retireId := ownerRetireId
        difftestAmuFinish.epoch := ownerEpoch
        difftestAmuFinish.index := 1.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        // DiffAmuFinishEvent packing is parameterized by words-per-bank.
        val eventWordsPerBank = difftestAmuFinish.data.length / ABMatrixRegNBanks
        val abMRegWordsPerBank = ABMatrixRegEntryBitSize / 64
        require(difftestAmuFinish.data.length % ABMatrixRegNBanks == 0, "DiffAmuFinishEvent.data should divide by AB bank count")
        require(ABMatrixRegEntryBitSize % 64 == 0, s"ABMatrixRegEntryBitSize must be 64-bit aligned, got $ABMatrixRegEntryBitSize")
        require(abMRegWordsPerBank <= eventWordsPerBank, s"DiffAmuFinishEvent only supports up to $eventWordsPerBank words per AB bank, got $abMRegWordsPerBank")
        for (i <- 0 until ABMatrixRegNBanks) {
          difftestAmuFinish.bankValid(i) := io.ToMatrixRegIO.BankAddr(i).valid
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.bankMask(i) := io.ToMatrixRegIO.ByteMask(i).bits
          for (w <- 0 until eventWordsPerBank) {
            if (w < abMRegWordsPerBank) {
              val lo = w * 64
              val hi = lo + 63
              difftestAmuFinish.data(i * eventWordsPerBank + w) := io.ToMatrixRegIO.Data(i).bits(hi, lo)
            } else {
              difftestAmuFinish.data(i * eventWordsPerBank + w) := 0.U(64.W)
            }
          }
        }
        difftestAmuFinish.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    }

    val MatrixRegBankAddr = io.ToMatrixRegIO.BankAddr
    val MatrixRegData = io.ToMatrixRegIO.Data


    val ConfigInfo = io.ConfigInfo
    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    val MatrixRegTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val HasTail = RegInit(false.B)
    val TailByteMask = RegInit(0.U(log2Ceil(outsideDataWidthByte + 1).W))
    val K_Beat_Count = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val dataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val ConvertF32ToI8 = RegInit(false.B)
    val ConvertF32ToMx = RegInit(false.B)
    val ConvertDynamic = ConfigInfo.ConvertDynamic.map(d =>
        RegEnable(d, false.B, ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady)).getOrElse(false.B)
    val ConvertScaleBankId = RegInit(0.U(ABScaleRegIdWidth.W))
    io.ScaleBankId := ConvertScaleBankId
    val transposeBytesPerElement = RegInit(1.U(3.W))
    val Tensor_B_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
    val ApplicationTensor_B_Stride_N = RegInit(0.U(MMUAddrWidth.W))

    
    //任务状态机
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)
    io.ScaleReadActive := ConvertF32ToMx && !ConvertDynamic && state === s_mm_task


    //访存状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_quiesce :: s_load_end :: Nil = Enum(5)
    val memoryload_state = RegInit(s_load_idle)
    private val transposeEndDrainCycles = Trans_Load_Size + 2 + 3
    val transposeEndDrainCnt = RegInit(0.U(log2Ceil(transposeEndDrainCycles + 1).W))
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

    val Conherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供
    val Is_Transpose = RegInit(false.B) //是否转置load，由TaskController提供

    
    //如果configinfo有效

    when(state === s_idle){
        //idel状态才可以接受新的配置信息
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            //当前配置的指令有效
            state := s_mm_task
            memoryload_state := s_load_init
            // ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
            MatrixRegTensor_N := io.ConfigInfo.MatrixRegTensor_N
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := io.ConfigInfo.MatrixRegId
            Tensor_B_BaseVaddr := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr //这个不重要
            Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr //这个是关键
            Conherent := io.ConfigInfo.Conherent
            Is_Transpose := io.ConfigInfo.Is_Transpose
            HasTail := io.ConfigInfo.ApplicationTensor_B.HasTail
            TailByteMask := io.ConfigInfo.ApplicationTensor_B.TailByteMask
            K_Beat_Count := io.ConfigInfo.ApplicationTensor_B.K_Beat_Count
            dataType := io.ConfigInfo.ApplicationTensor_B.dataType
            ConvertF32ToI8 := io.ConfigInfo.ConvertF32ToI8
            ConvertF32ToMx := ConfigInfo.ConvertF32ToMx
            ConvertScaleBankId := ConfigInfo.ConvertScaleBankId
            when(ConfigInfo.ConvertF32ToMx) {
                assert(ExperimentalMxfpConvertLoad.B, "BML converting load is disabled")
                assert(!ConfigInfo.Is_Transpose && !ConfigInfo.ConvertF32ToI8 &&
                    !ConfigInfo.ApplicationTensor_B.HasTail,
                    "BML MX conversion requires a normal F32 tile, not the INT8 experiment")
                assert(ConfigInfo.ConvertKElements === 64.U &&
                    ConfigInfo.ApplicationTensor_B.K_Beat_Count === 4.U &&
                    ConfigInfo.ApplicationTensor_B.dataType === ElementDataType.DataTypeWidth32,
                    "BML converting load currently requires K64 / four F32 source beats")
                when(ConfigInfo.ConvertDynamic.getOrElse(false.B)) {
                    assert(ConfigInfo.MatrixRegTensor_N === 8.U &&
                        ConfigInfo.ConvertComputeType === MteComputeType.Mxfp4F32,
                        "dynamic BML requires exactly eight K64 FP4 rows")
                }
            }
            transposeBytesPerElement := TransposeBytePlane.bytesPerElement(io.ConfigInfo.ApplicationTensor_B.dataType)
            ApplicationTensor_B_Stride_N := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N //下一个N，需要增加多少地址偏移量
            when(io.ConfigInfo.ConvertF32ToI8) {
                assert(!io.ConfigInfo.Is_Transpose, "BML F32-to-I8 conversion does not support transpose")
                assert(!io.ConfigInfo.ApplicationTensor_B.HasTail, "BML F32-to-I8 conversion requires an aligned tile")
                assert(io.ConfigInfo.ApplicationTensor_B.K_Beat_Count === 4.U,
                    "BML F32-to-I8 conversion expects 64 F32 values (4 source beats)")
                assert(io.ConfigInfo.ApplicationTensor_B.dataType === ElementDataType.DataTypeWidth32,
                    "BML F32-to-I8 conversion requires an F32 source tensor")
            }
            if(YJPBMLDebugEnable)
            {
                printf("[BML<%d>]BMemoryLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BML<%d>]MatrixRegTensor_N:%d,MatrixRegTensor_K:%d,Is_Transpose:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.MatrixRegTensor_N,io.ConfigInfo.MatrixRegTensor_K,io.ConfigInfo.Is_Transpose)
                printf("[BML<%d>]Tensor_B_BaseVaddr:%x,Tensor_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr,io.ConfigInfo.ApplicationTensor_B.BlockTensor_B_BaseVaddr)
                printf("[BML<%d>]ApplicationTensor_B_Stride_N:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N)
            }
        }
    }

    //三个张量的虚拟地址，肯定得是连续的，这个可以交给操作系统和编译器来保证

    //A的数据已经完成了reorder
    //32×32×4B的数据    --->    一个4K页
    //32×128×1B的数据   --->    一个4K页
    //64×64×1B的数据    --->    一个4K页

    //页面内数据怎么排好像也无所谓，只要数据对齐且数据连续的就行了
    //这里的数据排布、更多的是为了memory连续读取时的性能考虑
    //那最好把单次读取的数据，都先放在一个页内不去连续的处理N个页？
    //那首先，每次连续读取的Tensor的数据是   AScartchpad = Tensor_MN×Tensor_K×ReduceWidth = 64×64×256bit = 128KB
    //                                  BSctatchpad = Tensor_K×Tensor_MN×ReduceWidth = 64×64×256bit = 128KB
    //                                  CScartchpad = Tensor_MN×Tensor_MN×ResultWidth = 64×64×32bit = 16KB


    //这里的MatrixReg，有可以节省大小的方案，就是尽可能早的去标记某个数据是无效的，然后对下一个数据发出请求，这样对SRAM的读写端口数量要求就高了，多读写端口vsdoublebufferSRAM
    //LLC的访存带宽我们设定成和每个bank的每个entry的大小一样。

    //处理取数逻辑，BScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
    //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
    //如果是memoryload_state === s_load_working，那么我们就要开始取数
    //如果是memoryload_state === s_load_end，那么我们就要结束取数
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W)) //总共要加载的数据量
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val normalPhysicalRequests = RegInit(0.U((TotalLoadSize.getWidth + 1).W))
    val normalSecondFragment = RegInit(false.B)
    val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_N_Iter_Time = RegInit(0.U(log2Ceil(math.max(Matrix_MN, ABMatrixRegEntryByteSize)).W))

    val group_req_cnt = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
    val group_resp_cnt = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
    val group_size_reg = RegInit(0.U(log2Ceil(ABMatrixRegEntryByteSize + 1).W))

    private val transBaseAddrBits = log2Ceil(ABMatrixRegBankNEntries)
    

    //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
    //用sourceid做索引，存储MatrixReg的地址和bank号，是一组寄存器
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new BSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(ABMatrixRegBankNEntries).W)))))//记录这个LLC回的数是在scp的哪个地址
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/ABMatrixRegEntryByteSize)+1).W)))))//记录这个LLC回的数需要回填的次数，完成就可以将数据释放了
    val MReg_Fill_Table_ByteMask = RegInit(VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidthByte.W))))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)//记录这个FIFO能否能填数据
    val MReg_Fill_Table_Valid = MReg_Fill_Table_Time.map(_ =/= 0.U)//记录这个FIFO里的数据是否有效
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)//返回第一个空位的index
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)//这个FIFO是否还有空位
    val MAX_Fill_Times = outsideDataWidthByte/ABMatrixRegEntryByteSize

    private val ConvertSourceBeatsPerRow = 4
    private val ConvertSlotBytes = (outsideDataWidthByte / 4)
    private val ConvertEntryDataBits = ABMatrixRegEntryByteSize * 8
    private val ConvertSlotDataBits = ConvertSlotBytes * 8
    private val ConvertWriteCountWidth = log2Ceil(Tensor_MN * 2 + 1)
    val ConvertSlotData = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks)(
        VecInit(Seq.fill(ConvertSourceBeatsPerRow)(0.U(ConvertSlotDataBits.W)))
    )))
    val ConvertSlotValid = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks)(
        VecInit(Seq.fill(ConvertSourceBeatsPerRow)(false.B))
    )))
    val ConvertPendingSecondValid = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val ConvertPendingSecondAddr = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(ABMatrixRegBankNEntries).W))))
    val ConvertPendingSecondData = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(ConvertEntryDataBits.W))))
    val ConvertWriteCount = RegInit(0.U(ConvertWriteCountWidth.W))

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(VecInit(Seq.fill(BMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))))//记录fifo里的数据是哪个bank的
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))//想要往scp里bank(x)写的最后一个scp_fill_fifo的index
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(BMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//每个bank，是否有数据需要写scp
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//是否有数据需要写scp

    for(i <- 0 until ABMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), BMemoryLoaderReadFromMemoryFIFODepth)//fifo满了
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//这个bank不需要写scp
    }

    val transAlignPipes = Seq.tabulate(ABMatrixRegNBanks)(i => Module(new TransAlignPipe(i, YJPBMLDebugEnable)))
    val transRouters = Seq.tabulate(ABMatrixRegNBanks)(_ => Module(new OOORouter))
    val transPipeInValid = WireInit(false.B)
    val transPipeInData = WireInit(0.U(outsideDataWidth.W))
    val transPipeInMask = WireInit(0.U(outsideDataWidthByte.W))
    val transPipeRespBeatCnt = WireInit(0.U(group_resp_cnt.getWidth.W))
    val transPipeEntryOffset = WireInit(0.U(log2Ceil(ABMatrixRegEntryByteSize).W))
    val transPipeDrainTrigger = WireInit(false.B)
    val transBusStall = transAlignPipes.map(_.io.bus_stall).reduce(_ || _)
    val transWriteBaseAddr = RegInit(0.U(transBaseAddrBits.W))
    val transposeGroupRows = TransposeBytePlane.groupRows(transposeBytesPerElement, ABMatrixRegEntryByteSize)

    for (i <- 0 until ABMatrixRegNBanks) {
        transAlignPipes(i).io.in_data := transPipeInData
        transAlignPipes(i).io.in_mask := transPipeInMask
        transAlignPipes(i).io.resp_beat_cnt := transPipeRespBeatCnt
        transAlignPipes(i).io.entry_offset := transPipeEntryOffset
        transAlignPipes(i).io.bytes_per_element := transposeBytesPerElement
        transAlignPipes(i).io.debug_time := io.DebugInfo.DebugTimeStampe
        transAlignPipes(i).io.is_drain_trigger := transPipeDrainTrigger
        transAlignPipes(i).io.in_valid := transPipeInValid
        transRouters(i).io.in <> transAlignPipes(i).io.out
    }
    val transAlignEmpty = transAlignPipes.map(_.io.empty).reduce(_ && _)
    val transRouterEmpty = transRouters.map(_.io.empty).reduce(_ && _)
    val transPipelineEmpty = transAlignEmpty && transRouterEmpty
    val transRouterValidVec = VecInit(transRouters.map(_.io.valid)).asUInt
    val transRouterWriteValid = transRouters.map(_.io.valid).reduce(_ || _)
    val transRouterTxnValid = transRouters.map(_.io.txn_valid).reduce(_ || _)
    val transWritePhase = transRouters.head.io.phase
    val transWriteElementSlot = TransposeBytePlane.elementSlot(transWritePhase, transposeBytesPerElement)
    val transWriteAddrOffset = TransposeBytePlane.reduceGroupOffset(transWriteElementSlot, ReduceGroupSize)
    val transWriteAddrWide = transWriteBaseAddr +& transWriteAddrOffset
    val transWriteAddr = transWriteAddrWide(transBaseAddrBits - 1, 0)

    when(Is_Transpose && transRouterTxnValid) {
        assert(transWriteAddrWide < ABMatrixRegBankNEntries.U,
            "transpose write address must stay within an AB MatrixReg bank")
    }

    
    // Legacy BML only uses channel 0 for requests
    val Request = io.LocalMMUIO.Request(0)
    val Response = io.LocalMMUIO.Response(0)
    val convertFill = Option.when(ExperimentalMxfpConvertLoad)(Module(new MxfpConvertFill))
    convertFill.foreach { fill =>
        fill.io.start := ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady && ConfigInfo.ConvertF32ToMx
        fill.io.computeType := ConfigInfo.ConvertComputeType
        fill.io.dynamic.foreach(_ := ConfigInfo.ConvertDynamic.get)
        io.GeneratedScale.zip(fill.io.GeneratedScale).foreach { case (outer, inner) => outer <> inner }
        fill.io.request.valid := false.B
        fill.io.request.bits := 0.U.asTypeOf(fill.io.request.bits)
        fill.io.response.valid := false.B
        val meta = SoureceIdSearchTable(Response.bits.ReseponseSourceID).asTypeOf(new BSourceIdSearch)
        fill.io.response.bits.source.row := meta.ConvertRow
        fill.io.response.bits.source.beat := meta.ConvertBeat
        fill.io.response.bits.data := Response.bits.ReseponseData
        fill.io.response.bits.readByteOffset := meta.ReadByteOffset
        fill.io.response.bits.writeByteOffset := meta.WriteByteOffset
        fill.io.response.bits.fragmentBytes := meta.FragmentBytes
        fill.io.FromScaleRegIO <> io.FromScaleRegIO.get
        io.ConfigInfo.MicroTaskFlags := Mux(ConvertF32ToMx, fill.io.flags, 0.U)
    }
    switch(memoryload_state) {
        is(s_load_init) {
            memoryload_state := s_load_working
            TotalLoadSize := 0.U
            TotalRequestSize := 0.U
            normalPhysicalRequests := 0.U
            normalSecondFragment := false.B
            CurrentLoaded_BlockTensor_N_Iter := 0.U
            CurrentLoaded_BlockTensor_K_Iter := 0.U
            Request_N_Iter_Time := 0.U
            group_req_cnt := 0.U
            group_resp_cnt := 0.U
            group_size_reg := 0.U
            transposeEndDrainCnt := 0.U
            transWriteBaseAddr := 0.U
            MaxRequestIter := MatrixRegTensor_N * K_Beat_Count //总共要发出的访存请求的次数
            Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
            Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
            Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
            MReg_Fill_Table := 0.U.asTypeOf(MReg_Fill_Table)
            MReg_Fill_Table_MReg_Addr := 0.U.asTypeOf(MReg_Fill_Table_MReg_Addr)
            MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
            MReg_Fill_Table_ByteMask := 0.U.asTypeOf(MReg_Fill_Table_ByteMask)
            ConvertSlotData := 0.U.asTypeOf(ConvertSlotData)
            ConvertSlotValid := 0.U.asTypeOf(ConvertSlotValid)
            ConvertPendingSecondValid := 0.U.asTypeOf(ConvertPendingSecondValid)
            ConvertPendingSecondAddr := 0.U.asTypeOf(ConvertPendingSecondAddr)
            ConvertPendingSecondData := 0.U.asTypeOf(ConvertPendingSecondData)
            ConvertWriteCount := 0.U
        }
        is(s_load_working) {
            io.ToMatrixRegIO.active := true.B
            //根据不同的MemoryOrder，执行不同的访存模式

            when(Is_Transpose) {
                assert(TransposeBytePlane.isSupported(transposeBytesPerElement),
                    "BML transpose supports only e8/e16/e32 element widths")
                when(HasTail) {
                    assert(TransposeBytePlane.tailAligned(TailByteMask, transposeBytesPerElement),
                        "BML transpose tail must end on a complete element")
                }
            }

            //先转换成独热码然后进行减一即可计算出掩码
            val tailTaskMask = UIntToOH(TailByteMask, outsideDataWidthByte + 1).asUInt - 1.U(outsideDataWidthByte.W)
            val fullTaskMask = Fill(outsideDataWidthByte, true.B)
            val RequestBeatIsTail = HasTail && (CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U))

            // Transpose request path reuses the normal iter registers:
            //   CurrentLoaded_BlockTensor_N_Iter -> large_N group index
            //   CurrentLoaded_BlockTensor_K_Iter -> K/N-beat index
            //   Request_N_Iter_Time              -> small_N inside current group
            val transpose_large_n_base = TransposeBytePlane.groupBase(
                CurrentLoaded_BlockTensor_N_Iter, transposeBytesPerElement, ABMatrixRegEntryByteSize
            )
            val transpose_current_n = transpose_large_n_base + Request_N_Iter_Time
            val transpose_group_in_range = transpose_large_n_base < MatrixRegTensor_N
            val transpose_group_remain = Mux(transpose_group_in_range, MatrixRegTensor_N - transpose_large_n_base, 0.U)
            val current_group_size = Wire(UInt(log2Ceil(ABMatrixRegEntryByteSize + 1).W))
            current_group_size := Mux(
                transpose_group_remain < transposeGroupRows,
                transpose_group_remain(current_group_size.getWidth - 1, 0),
                transposeGroupRows
            )
            val group_has_no_requests = group_req_cnt === 0.U && group_resp_cnt === 0.U
            val group_is_idle = group_has_no_requests && transPipelineEmpty
            val active_group_size = Mux(group_has_no_requests, current_group_size, group_size_reg)
            val transpose_group_can_issue = Mux(
                group_has_no_requests,
                group_is_idle && (current_group_size =/= 0.U),
                group_req_cnt < active_group_size
            )
            val transpose_req_enable = (TotalRequestSize < MaxRequestIter) && transpose_group_can_issue

            // 访存顺序与AML保持一致：先沿N维分4个bank发射，再推进K维，最后推进下一组N block
            val RequestMatrixRegNIndex = Mux(Is_Transpose, transpose_current_n, CurrentLoaded_BlockTensor_N_Iter + Request_N_Iter_Time)
            val NormalRequestMatrixRegBankId = RequestMatrixRegNIndex % ABMatrixRegNBanks.U
            val NormalRequestMatrixRegBaseAddr = ((RequestMatrixRegNIndex / ABMatrixRegNBanks.U) * ReduceGroupSize.U)
            val NormalRequestMatrixRegAddr = NormalRequestMatrixRegBaseAddr + (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(MAX_Fill_Times))
            val TransposeRequestMatrixRegAddr = TransposeBytePlane.beatEntryBase(
                CurrentLoaded_BlockTensor_K_Iter, transposeBytesPerElement, Trans_Load_Size, ReduceGroupSize
            ) + CurrentLoaded_BlockTensor_N_Iter
            val RequestMatrixRegBankId = Mux(Is_Transpose, 0.U, NormalRequestMatrixRegBankId)
            val RequestMatrixRegAddr = Mux(Is_Transpose, TransposeRequestMatrixRegAddr, NormalRequestMatrixRegAddr)

            // Reuse AML's source-owned containing-line fragments. Transpose
            // and the fixed-scale converting-load keep their existing paths.
            val logicalRequestAddr = Tensor_Block_BaseAddr + RequestMatrixRegNIndex * ApplicationTensor_B_Stride_N +
                (CurrentLoaded_BlockTensor_K_Iter << log2Ceil(outsideDataWidthByte))
            val lineOffset = logicalRequestAddr(log2Ceil(outsideDataWidthByte) - 1, 0)
            val liveBytes = Mux(RequestBeatIsTail, TailByteMask, outsideDataWidthByte.U)
            val firstLineRoom = outsideDataWidthByte.U - lineOffset
            val firstFragmentBytes = Mux(liveBytes < firstLineRoom, liveBytes, firstLineRoom)
            val normalNeedsSecond = liveBytes > firstLineRoom
            val normalBeatComplete = normalSecondFragment || !normalNeedsSecond
            val normalAlignedAddr = logicalRequestAddr & (~(outsideDataWidthByte - 1).U(MMUAddrWidth.W))
            Request.bits.RequestAddr := Mux(Is_Transpose || ConvertF32ToI8, logicalRequestAddr,
                normalAlignedAddr + Mux(normalSecondFragment, outsideDataWidthByte.U, 0.U))
            
            val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
            Request.bits.RequestConherent := Conherent
            Request.bits.RequestSourceID := sourceId.bits
            Request.bits.RequestType_isWrite := false.B
            Request.bits.UseAllocatedSourceID := true.B
            Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
            Request.valid := Mux(Is_Transpose, transpose_req_enable, TotalRequestSize < MaxRequestIter)
            convertFill.foreach { fill =>
                when(ConvertF32ToMx) {
                    Request.valid := TotalRequestSize < MaxRequestIter && fill.io.request.ready
                    fill.io.request.valid := Request.fire && sourceId.valid
                    fill.io.request.bits.row := RequestMatrixRegNIndex
                    fill.io.request.bits.beat := CurrentLoaded_BlockTensor_K_Iter(1, 0)
                }
            }

            when(Request.fire && sourceId.valid){//符合条件的话，这条访存请求一定会被发出
                //Request.ready表明了LocalMMU会处理这条访存请求，sourceID valid，表明这条访存请求的sourceID是被LocalMMU认可有效才发送到这个模块的
                val TableItem = Wire(new BSourceIdSearch)
                TableItem.MatrixRegBankId := RequestMatrixRegBankId
                TableItem.MatrixRegAddr := Mux(
                    ConvertF32ToI8,
                    NormalRequestMatrixRegBaseAddr,
                    RequestMatrixRegAddr
                )
                TableItem.MatrixRegisTail := RequestBeatIsTail
                TableItem.BeatIndex := Mux(
                    ConvertF32ToI8,
                    CurrentLoaded_BlockTensor_K_Iter(TableItem.BeatIndex.getWidth - 1, 0),
                    Request_N_Iter_Time(TableItem.BeatIndex.getWidth - 1, 0)
                )
                TableItem.ReadByteOffset := Mux(Is_Transpose || ConvertF32ToI8 || normalSecondFragment, 0.U, lineOffset)
                TableItem.WriteByteOffset := Mux(!Is_Transpose && !ConvertF32ToI8 && normalSecondFragment, firstFragmentBytes, 0.U)
                TableItem.FragmentBytes := Mux(Is_Transpose || ConvertF32ToI8, outsideDataWidthByte.U,
                    Mux(normalSecondFragment, liveBytes - firstFragmentBytes, firstFragmentBytes))
                TableItem.ConvertRow := Mux(ConvertF32ToMx, RequestMatrixRegNIndex, 0.U)
                TableItem.ConvertBeat := Mux(ConvertF32ToMx, CurrentLoaded_BlockTensor_K_Iter(1, 0), 0.U)
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
                if (YJPBMLDebugEnable) {
                    printf("[BML_RequestHandshake<%d>] sourceId:%d, MatrixRegBankId:%d, MatrixRegAddr:%d, RequestAddr:%x, RequestConherent:%d, RequestType_isWrite:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegBankId,TableItem.MatrixRegAddr,Request.bits.RequestAddr,Request.bits.RequestConherent,Request.bits.RequestType_isWrite,RequestBeatIsTail)
                }
                when(Is_Transpose) {
                    if (YJPBMLDebugEnable) {
                        printf("[BML_TRANS_REQ<%d>] totalReq:%d groupReq:%d groupResp:%d idle:%d largeN:%d kBeat:%d beat:%d groupSize:%d activeGroupSize:%d regBase:%d addr:%x source:%d tail:%d\n",
                          io.DebugInfo.DebugTimeStampe, TotalRequestSize, group_req_cnt, group_resp_cnt,
                          group_is_idle.asUInt, CurrentLoaded_BlockTensor_N_Iter, CurrentLoaded_BlockTensor_K_Iter,
                          Request_N_Iter_Time, current_group_size, active_group_size, RequestMatrixRegAddr,
                          Request.bits.RequestAddr, sourceId.bits, RequestBeatIsTail.asUInt)
                    }

                    val small_n_reach_group_boundary = Request_N_Iter_Time === (transposeGroupRows - 1.U)
                    val small_n_reach_tensor_boundary = transpose_current_n === (MatrixRegTensor_N - 1.U)
                    val small_n_wrap = small_n_reach_group_boundary || small_n_reach_tensor_boundary
                    val k_wrap = CurrentLoaded_BlockTensor_K_Iter === (K_Beat_Count - 1.U)

                    when(group_is_idle) {
                        group_size_reg := current_group_size
                    }
                    group_req_cnt := group_req_cnt + 1.U

                    Request_N_Iter_Time := Request_N_Iter_Time + 1.U
                    when(small_n_wrap) {
                        Request_N_Iter_Time := 0.U
                        CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                        when(k_wrap) {
                            CurrentLoaded_BlockTensor_K_Iter := 0.U
                            CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + 1.U
                        }
                    }
                }.otherwise {
                    when(!ConvertF32ToI8) {
                        normalPhysicalRequests := normalPhysicalRequests + 1.U
                        normalSecondFragment := normalNeedsSecond && !normalSecondFragment
                    }
                    when(ConvertF32ToI8 || normalBeatComplete) {
                        when(ConvertF32ToMx) {
                            CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                            when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count) {
                                CurrentLoaded_BlockTensor_K_Iter := 0.U
                                CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + 1.U
                            }
                        }.otherwise {
                            Request_N_Iter_Time := Request_N_Iter_Time + 1.U
                            when(Request_N_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_N_Iter + Request_N_Iter_Time) === MatrixRegTensor_N - 1.U){
                                Request_N_Iter_Time := 0.U
                                CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + 1.U
                                when(CurrentLoaded_BlockTensor_K_Iter + 1.U === K_Beat_Count){
                                    CurrentLoaded_BlockTensor_K_Iter := 0.U
                                    CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + Matrix_MN.U
                                }
                            }
                        }
                    }
                }
                when(TotalRequestSize =/= MaxRequestIter && (Is_Transpose || ConvertF32ToI8 || normalBeatComplete)){
                    TotalRequestSize := TotalRequestSize + 1.U
                }
            }
            val current_fill_fifo_full = WireInit(false.B)
            val responseSearchEntry = SoureceIdSearchTable(Response.bits.ReseponseSourceID).asTypeOf(new BSourceIdSearch)
            when(Response.valid && !Is_Transpose)
            {
                current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(responseSearchEntry.MatrixRegBankId)
            }
            //接受访存的返回值
            //一个cam来存储访存请求的source_id对应的MatrixReg的地址和bank号
            //根据response的sourceid，找到对应的MatrixReg的Fill_Table的队伍头的索引，填充到Fill_Table中
            val normalRespReady = if (ABMLNeedMRegFillTable) {
                MReg_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
            } else {
                true.B
            }
            val convertRespReady = !ConvertPendingSecondValid(responseSearchEntry.MatrixRegBankId)
            Response.ready := Mux(
                Is_Transpose,
                !transBusStall,
                Mux(ConvertF32ToI8, convertRespReady, normalRespReady)
            )
            convertFill.foreach { fill =>
                when(ConvertF32ToMx) {
                    fill.io.response.valid := Response.valid
                    Response.ready := fill.io.response.ready
                }
            }
            val convertFirstWrite = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
            val convertPendingWrite = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
            when(Response.fire){
                //Trick注意这个设计，是doublebuffer的，AB只能是doublebuffer，回数一定是不会堵的，而且我们有时间对数据进行压缩解压缩～
                //如果要做release设计，要么数据位宽翻倍，腾出周期来使得有空泡能给写任务进行，要么就是数据位宽不变，将读写端口变成独立的读和独立的写端口
                val sourceId = Response.bits.ReseponseSourceID
                val searchEntry = SoureceIdSearchTable(sourceId).asTypeOf(new BSourceIdSearch)
                val MatrixRegBankId = searchEntry.MatrixRegBankId
                val MatrixRegAddr = searchEntry.MatrixRegAddr
                val ResponseData = Response.bits.ReseponseData
                val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)//该bank的fill_fifo_index，标注了它当前在fillfifo的哪个位置，我们一共有bank个fill_fifo
                val convertedBeatData = VecInit((0 until outsideDataWidth / 32).map { lane =>
                    F32ToI8FixedScale(ResponseData(lane * 32 + 31, lane * 32))
                }).asUInt

                if (YJPBMLDebugEnable) {
                    printf("[BML_ResponseHandshake<%d>] ResponseData:%x, MatrixRegBankId:%d, MatrixRegAddr:%d, SourceId:%d, FIFOIndex:%d, Tail:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr,sourceId,FIFOIndex,searchEntry.MatrixRegisTail)
                }

                when(ConvertF32ToMx) {
                    // Captured by the same shared K32 owner used by AML.
                }.elsewhen(Is_Transpose) {
                    val next_group_resp_cnt = group_resp_cnt + 1.U
                    val drain_trigger = next_group_resp_cnt === active_group_size

                    if (YJPBMLDebugEnable) {
                        printf("[BML_TRANS_RESP<%d>] source:%d data:%x tail:%d mask:%x base:%d beatIndex:%d respCnt:%d nextResp:%d activeGroupSize:%d drainTrig:%d writeBase:%d\n",
                          io.DebugInfo.DebugTimeStampe, sourceId, ResponseData, searchEntry.MatrixRegisTail.asUInt,
                          Mux(searchEntry.MatrixRegisTail, tailTaskMask, fullTaskMask), MatrixRegAddr,
                          searchEntry.BeatIndex, group_resp_cnt, next_group_resp_cnt, active_group_size,
                          drain_trigger.asUInt, transWriteBaseAddr)
                    }

                    transPipeInValid := true.B
                    transPipeInData := ResponseData
                    transPipeInMask := Mux(searchEntry.MatrixRegisTail, tailTaskMask, fullTaskMask)
                    transPipeRespBeatCnt := group_resp_cnt
                    transPipeEntryOffset := searchEntry.BeatIndex
                    transPipeDrainTrigger := drain_trigger

                    when(group_resp_cnt === 0.U) {
                        transWriteBaseAddr := MatrixRegAddr
                    }

                    when(next_group_resp_cnt === active_group_size) {
                        group_req_cnt := 0.U
                        group_resp_cnt := 0.U
                        group_size_reg := 0.U
                    }.otherwise {
                        group_resp_cnt := next_group_resp_cnt
                    }
                }.otherwise {
                    when(ConvertF32ToI8) {
                        val slot = searchEntry.BeatIndex(log2Ceil(ConvertSourceBeatsPerRow) - 1, 0)
                        val nextSlotValid = VecInit(ConvertSlotValid(MatrixRegBankId).zipWithIndex.map {
                            case (valid, index) => valid || slot === index.U
                        })
                        val currentSlotData = VecInit((0 until ConvertSourceBeatsPerRow).map { index =>
                            Mux(slot === index.U, convertedBeatData, ConvertSlotData(MatrixRegBankId)(index))
                        })

                        ConvertSlotData(MatrixRegBankId)(slot) := convertedBeatData
                        ConvertSlotValid(MatrixRegBankId)(slot) := true.B
                        when(nextSlotValid.asUInt.andR) {
                            ConvertSlotValid(MatrixRegBankId) := 0.U.asTypeOf(ConvertSlotValid(MatrixRegBankId))
                            convertFirstWrite(MatrixRegBankId) := true.B
                            ConvertPendingSecondValid(MatrixRegBankId) := true.B
                            ConvertPendingSecondAddr(MatrixRegBankId) := MatrixRegAddr + 1.U
                            ConvertPendingSecondData(MatrixRegBankId) := Cat(currentSlotData(3), currentSlotData(2))
                            io.ToMatrixRegIO.BankAddr(MatrixRegBankId).bits := MatrixRegAddr
                            io.ToMatrixRegIO.BankAddr(MatrixRegBankId).valid := true.B
                            io.ToMatrixRegIO.Data(MatrixRegBankId).bits := Cat(currentSlotData(1), currentSlotData(0))
                            io.ToMatrixRegIO.Data(MatrixRegBankId).valid := true.B
                            io.ToMatrixRegIO.ByteMask(MatrixRegBankId).bits := Fill(ABMatrixRegEntryByteSize, true.B)
                            io.ToMatrixRegIO.ByteMask(MatrixRegBankId).valid := true.B
                        }
                    }.otherwise {
                        val alignedData = Wire(UInt(outsideDataWidth.W))
                        alignedData := Mux(searchEntry.WriteByteOffset === 0.U,
                            ResponseData >> (searchEntry.ReadByteOffset << 3),
                            ResponseData << (searchEntry.WriteByteOffset << 3))
                        val fragmentMask = UIntToOH(searchEntry.FragmentBytes, outsideDataWidthByte + 1) - 1.U
                        val byteMask = (fragmentMask << searchEntry.WriteByteOffset)(outsideDataWidthByte - 1, 0)
                        if (!ABMLNeedMRegFillTable)
                        {
                            TotalLoadSize := TotalLoadSize + 1.U
                            for (i <- 0 until ABMatrixRegNBanks)
                            {
                                when(MatrixRegBankId === i.U)
                                {
                                    io.ToMatrixRegIO.BankAddr(i).bits := MatrixRegAddr
                                    io.ToMatrixRegIO.Data(i).bits := alignedData(ABMatrixRegEntryBitSize - 1, 0)
                                    io.ToMatrixRegIO.BankAddr(i).valid := true.B
                                    io.ToMatrixRegIO.Data(i).valid := true.B
                                    io.ToMatrixRegIO.ByteMask(i).bits := byteMask(ABMatrixRegEntryByteSize - 1, 0)
                                    io.ToMatrixRegIO.ByteMask(i).valid := true.B
                                }
                            }
                        }

                        MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := alignedData
                        MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                        MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                        MReg_Fill_Table_ByteMask(MReg_Fill_Table_Insert_Index) := byteMask

                        Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                        Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), BMemoryLoaderReadFromMemoryFIFODepth)
                    }
                }
                //需要一个fifo？TODO:需要fifo的设计是可能这里会堵，实际上我们满吞吐的doublebuff的设计，咱们这里是不会堵的，直接填就完事了？还是等总线上去握手？
                //MatrixReg->MemoryLoader->MMU->Memory Bus->Memory上的长组合逻辑链，可以实现一下，为后续的开发做准备
                //否则就靠软件来保证数据流和访存流，保证访存流的稳定性，一定不会堵，就可以省下这个长组合逻辑的延迟？
                //还有一点，我们的MatrixReg是写优先的呀！！！所以只要写端口数唯一，就不会堵，不需要fifo～～～
                //Trick:写优先是真的很有说法，本来外部存储就是慢的，读快速存储器的任务等一等就好了，但是所有的MatrixReg都想要读数据的，不能等，所以写优先
                
                //根据response的的id
                //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
                if (YJPBMLDebugEnable)
                {
                    //输出这次response的信息
                    printf("[BML<%d>]ResponseData:%x,MatrixRegBankId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,ResponseData,MatrixRegBankId,MatrixRegAddr)
                }
            }

            when(ConvertF32ToI8 && !Is_Transpose) {
                for (i <- 0 until ABMatrixRegNBanks) {
                    when(ConvertPendingSecondValid(i)) {
                        convertPendingWrite(i) := true.B
                        io.ToMatrixRegIO.BankAddr(i).bits := ConvertPendingSecondAddr(i)
                        io.ToMatrixRegIO.BankAddr(i).valid := true.B
                        io.ToMatrixRegIO.Data(i).bits := ConvertPendingSecondData(i)
                        io.ToMatrixRegIO.Data(i).valid := true.B
                        io.ToMatrixRegIO.ByteMask(i).bits := Fill(ABMatrixRegEntryByteSize, true.B)
                        io.ToMatrixRegIO.ByteMask(i).valid := true.B
                        ConvertPendingSecondValid(i) := false.B
                    }
                }
                val convertWritesThisCycle = PopCount((convertFirstWrite.asUInt | convertPendingWrite.asUInt))
                ConvertWriteCount := ConvertWriteCount + convertWritesThisCycle
                when(ConvertWriteCount + convertWritesThisCycle === (MatrixRegTensor_N << 1)) {
                    memoryload_state := s_load_end
                }
            }

            when(Is_Transpose) {
                for (i <- 0 until ABMatrixRegNBanks) {
                    val routerValid = transRouters(i).io.valid
                    io.ToMatrixRegIO.BankAddr(i).bits := transWriteAddr
                    io.ToMatrixRegIO.BankAddr(i).valid := routerValid
                    io.ToMatrixRegIO.Data(i).bits := transRouters(i).io.final_data
                    io.ToMatrixRegIO.Data(i).valid := routerValid
                    io.ToMatrixRegIO.ByteMask(i).bits := transRouters(i).io.final_mask
                    io.ToMatrixRegIO.ByteMask(i).valid := routerValid
                    if (YJPBMLDebugEnable) {
                        when(routerValid) {
                            printf("[BML_TransposeWrite<%d>] bank:%d, Addr:%d, Data:%x, Mask:%x\n",
                              io.DebugInfo.DebugTimeStampe, i.U, io.ToMatrixRegIO.BankAddr(i).bits,
                              transRouters(i).io.final_data, transRouters(i).io.final_mask)
                        }
                    }
                }
                when(transRouterWriteValid) {
                    if (YJPBMLDebugEnable) {
                        printf("[BML_TRANS_WRITE<%d>] validVec:%b base:%d phase:%d q:%d addr:%d bank0Data:%x bank0Mask:%x totalLoad:%d pipelineEmpty:%d\n",
                          io.DebugInfo.DebugTimeStampe, transRouterValidVec, transWriteBaseAddr, transWritePhase, transWriteElementSlot,
                          transWriteAddr, transRouters(0).io.final_data, transRouters(0).io.final_mask,
                          TotalLoadSize, transPipelineEmpty.asUInt)
                    }
                }
                val Current_Load_Fill_Size = transRouterWriteValid.asUInt
                val nextTotalLoadSize = TotalLoadSize + Current_Load_Fill_Size
                val transposeDone = TotalRequestSize === MaxRequestIter &&
                    group_req_cnt === 0.U && group_resp_cnt === 0.U &&
                    transPipelineEmpty
                TotalLoadSize := nextTotalLoadSize
                if(YJPBMLDebugEnable){
                    when(Current_Load_Fill_Size =/= 0.U) {
                        printf("[BML_TransposeLoad<%d>]TotalLoadSize:%d, FillSize:%d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize, Current_Load_Fill_Size)
                    }
                }
                when(transposeDone){
                    memoryload_state := s_load_quiesce
                    transposeEndDrainCnt := (transposeEndDrainCycles - 1).U
                    if (YJPBMLDebugEnable) printf("[BML<%d>]TransposeFullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
                }
            }.otherwise {
                when(!ConvertF32ToI8 && !ConvertF32ToMx) {
                    // Fill_Table的回填优先级最高，一旦有回填任务就立即执行
                    val HasScarhpadWrite = Have_Bank_Fill
                    val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))
                    if (ABMLNeedMRegFillTable)
                    {
                        for (i <- 0 until ABMatrixRegNBanks){
                        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                            val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                            val fillSlot = MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex)
                            val fillSlotOH = UIntToOH(fillSlot, MAX_Fill_Times)
                            Current_Fill_MReg_Time(i) := 1.U
                            val MatrixRegWriteRequest = io.ToMatrixRegIO
                            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*ABMatrixRegEntryByteSize).W)))))
                            FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                            val maskSlots = VecInit((0 until MAX_Fill_Times).map { j =>
                                MReg_Fill_Table_ByteMask(CurrentFIFOIndex)((j + 1) * ABMatrixRegEntryByteSize - 1, j * ABMatrixRegEntryByteSize)
                            })
                            MatrixRegWriteRequest.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + fillSlot
                            MatrixRegWriteRequest.BankAddr(i).valid := true.B
                            MatrixRegWriteRequest.Data(i).bits := FIFOData(fillSlot)
                            MatrixRegWriteRequest.Data(i).valid := true.B
                            MatrixRegWriteRequest.ByteMask(i).bits := maskSlots(fillSlot)
                            MatrixRegWriteRequest.ByteMask(i).valid := true.B
                            if (YJPBMLDebugEnable) {
                                printf("[BML_MRegWriteHandshake<%d>] bankid: %d, CurrentFIFOIndex: %d, ScartchPadAddr: %x, BankAddr: %x, Data: %x, ByteMask: %x\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits, MatrixRegWriteRequest.ByteMask(i).bits)
                            }

                            MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                            when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                                Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), BMemoryLoaderReadFromMemoryFIFODepth)
                            }

                            if (YJPBMLDebugEnable)
                            {
                                //输出fill_time 和 fifoindex
                                printf("[BML BMemoryLoader_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, MReg_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                                printf("[BML BMemoryLoader_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MatrixRegWriteRequest.BankAddr(i).bits, MatrixRegWriteRequest.Data(i).bits)
                            }
                        }
                        }
                    }

                    val Current_Load_Fill_Size = WireInit(0.U((log2Ceil(ABMatrixRegNBanks)+1).W))
                    Current_Load_Fill_Size := PopCount(Current_Fill_MReg_Time.asUInt)

                    if (ABMLNeedMRegFillTable)
                    {
                        TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
                    }
                    if (YJPBMLDebugEnable)
                    {
                        when(Current_Load_Fill_Size =/= 0.U)
                        {
                            printf("[BMemoryLoader_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
                        }
                    }
                    //状态机切换
                    val normalWritesPerResponse = if (ABMLNeedMRegFillTable) MAX_Fill_Times else 1
                    when(TotalRequestSize === MaxRequestIter && !normalSecondFragment &&
                        TotalLoadSize === normalPhysicalRequests * normalWritesPerResponse.U){
                        memoryload_state := s_load_end
                        if (YJPBMLDebugEnable)
                        {
                            printf("[BMemoryLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
                        }
                    }
                }
            }
        }
        is(s_load_quiesce) {
            io.ToMatrixRegIO.active := true.B
            for (i <- 0 until ABMatrixRegNBanks) {
                val routerValid = transRouters(i).io.valid
                io.ToMatrixRegIO.BankAddr(i).bits := transWriteAddr
                io.ToMatrixRegIO.BankAddr(i).valid := routerValid
                io.ToMatrixRegIO.Data(i).bits := transRouters(i).io.final_data
                io.ToMatrixRegIO.Data(i).valid := routerValid
                io.ToMatrixRegIO.ByteMask(i).bits := transRouters(i).io.final_mask
                io.ToMatrixRegIO.ByteMask(i).valid := routerValid
                if (YJPBMLDebugEnable) {
                    when(routerValid) {
                        printf("[BML_TransposeQuiesceWrite<%d>] bank:%d, Addr:%d, Data:%x, Mask:%x\n",
                          io.DebugInfo.DebugTimeStampe, i.U, io.ToMatrixRegIO.BankAddr(i).bits,
                          transRouters(i).io.final_data, transRouters(i).io.final_mask)
                    }
                }
            }
            when(transRouterWriteValid) {
                if (YJPBMLDebugEnable) {
                    printf("[BML_TRANS_QUIESCE_WRITE<%d>] validVec:%b base:%d phase:%d q:%d addr:%d bank0Data:%x bank0Mask:%x drainCnt:%d pipelineEmpty:%d\n",
                      io.DebugInfo.DebugTimeStampe, transRouterValidVec, transWriteBaseAddr, transWritePhase, transWriteElementSlot,
                      transWriteAddr, transRouters(0).io.final_data, transRouters(0).io.final_mask,
                      transposeEndDrainCnt, transPipelineEmpty.asUInt)
                }
            }
            when(transposeEndDrainCnt === 0.U) {
                memoryload_state := s_load_end
                if (YJPBMLDebugEnable) printf("[BML<%d>]TransposeQuiesceEnd\n", io.DebugInfo.DebugTimeStampe)
            }.otherwise {
                transposeEndDrainCnt := transposeEndDrainCnt - 1.U
            }
        }
        is(s_load_end) {
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                memoryload_state := s_load_idle
                state := s_idle
                if(YJPBMLDebugEnable)
                {
                    printf("[BML<%d>]BMemoryLoader Task End\n",io.DebugInfo.DebugTimeStampe)
                }
            }
        }
    }
    convertFill.foreach { fill =>
        when(ConvertF32ToMx && state === s_mm_task) {
            io.ToMatrixRegIO := fill.io.ToMatrixRegIO
            io.ToMatrixRegIO.active := memoryload_state === s_load_working
            when(memoryload_state === s_load_working &&
                TotalRequestSize === MaxRequestIter && !normalSecondFragment && fill.io.idle) {
                memoryload_state := s_load_end
            }
        }
    }
}
