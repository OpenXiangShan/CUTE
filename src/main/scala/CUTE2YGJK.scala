package cute

import chisel3._
import chisel3.util._
// import boom.acc._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._ //for rocc
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
// import boom.common._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk.{YGJKParameters}
import utility.{ChiselDB, MemReqSource, ReqSourceField, ReqSourceKey}
import xscache.coupledL2.{
  AmeIndexField,
  AmeIndexKey,
  MatrixField,
  MatrixKey
}
import xscache.coupledL2.prefetch.{
  MatrixPrefetchStream,
  MatrixPrefetchTagCodec,
  MatrixPrefetchTagField,
  MatrixPrefetchTagKey
}

class WithCuteCoustomParams(val CoustomCuteParam:CuteParams = CuteParams.baseParams) extends WithCuteParams(CoustomCuteParam)

class Cute2TL(implicit p: Parameters) extends LazyModule with CUTEImplParameters {
  lazy val module = new CUTE2TLImp(this)
  // Changed from single TLClientNode with Seq.tabulate to Seq of independent TLClientNodes
  val node = Seq.tabulate(TrMatrixRegNBanks) { i =>
    TLClientNode(
      Seq(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name = s"CUTE_$i",
          sourceId = IdRange(0, 1)  // Use a fixed source ID; carry request identity in AmeIndex.
        )),
        requestFields = Seq(
          MatrixField(2),
          AmeIndexField()
        ) ++ (if (EnableMatrixPrefetch) Seq(MatrixPrefetchTagField()) else Nil) ++ Seq(
          ReqSourceField()
        ),
        responseKeys = Seq(AmeIndexKey)
      ))
    )
  }
}

// CUTE-to-LLC connection node.

class MatrixDataIO(implicit p: Parameters) extends CuteBundle {
  val data = UInt(512.W)
  val source = UInt(64.W)
}

class CUTE2TLImp(outer: Cute2TL) extends LazyModuleImp(outer) with CUTEImplParameters{
  // Updated for Seq of independent TLClientNodes
  val edges = outer.node.map(_.edges.out(0))
  val tl_outs = outer.node.map(_.out(0))

  val io = IO(new Bundle{
    val mmu = (new MMU2TLIO)
    val idle = Output(Bool())
    val matrix_data_in = Flipped(Vec(TrMatrixRegNBanks, Decoupled(new MatrixDataIO)))
  })

  val time_stamp = RegInit(0.U(64.W))
  time_stamp := time_stamp + 1.U
  // Six memory FUs, each with at most (2^dim-1)^2 vector positions.
  // Four input bytes per result byte bounds BML's widening conversion; round
  // the vector/cacheline ratio up. This bounds all FUs across all channels.
  private val maxDim = (BigInt(1) << MatrixRegMaxTensorDimBitSize) - 1
  private val beatsPerVector = ((4 * ReduceWidthByte + outsideDataWidthByte - 1) /
    outsideDataWidthByte) max 1
  private val maxOutstanding = 6 * maxDim * maxDim * beatsPerVector
  private val pendingWidth = maxOutstanding.bitLength
  val transportIdle = Wire(Vec(TrMatrixRegNBanks, Bool()))

  class MemoryTraceEntry extends Bundle {
    val cycle = UInt(64.W)
    val channel = UInt(log2Ceil(TrMatrixRegNBanks).W)
    val address = UInt(MMUAddrWidth.W)
    val sourceId = UInt(64.W)
    val isWrite = Bool()
    val isAcc = Bool()
    val prefetchTag = UInt(MatrixPrefetchTagCodec.width.W)
    val prefetchStream = UInt(MatrixPrefetchStream.width.W)
    val prefetchTaskId = UInt(MatrixPrefetchTagCodec.taskIdWidth.W)
    val traceTag = UInt(MatrixPrefetchTagCodec.width.W)
    val traceStream = UInt(MatrixPrefetchStream.width.W)
    val traceTaskId = UInt(MatrixPrefetchTagCodec.taskIdWidth.W)
  }
  val memoryTraceTable = Option.when(EnableMatrixPrefetch)(
    ChiselDB.createTable("CUTEMemoryTrace", new MemoryTraceEntry, basicDB = true)
  )

  for (channel <- 0 until TrMatrixRegNBanks) {
    val edge_ch = edges(channel)
    val (tl_out_ch, _) = tl_outs(channel)

    // Extract repeated IOs and bits to local vals
    val mmuReq        = io.mmu.Request(channel)
    val mmuReqBits    = mmuReq.bits
    val mmuResp       = io.mmu.Response(channel)
    val mmuRespBits   = mmuResp.bits
    val matrixDataIn  = io.matrix_data_in(channel)
    val tlA           = tl_out_ch.a
    val tlABits       = tlA.bits
    val tlD           = tl_out_ch.d
    val tlDBits       = tlD.bits

    val useAllocId = mmuReqBits.UseAllocatedSourceID

    tlA.valid := mmuReq.valid
    mmuReq.ready := tlA.ready

    when (mmuReq.valid && mmuReqBits.RequestType_isWrite) {
      assert(mmuReqBits.RequestMask.orR &&
        mmuReqBits.RequestAddr(log2Ceil(outsideDataWidthByte)-1, 0) === 0.U,
        "CUTE store fragments require aligned containing lines and nonempty byte masks")
    }

    tlABits := Mux1H(Seq(
      (mmuReqBits.RequestType_isWrite === 0.U) -> edge_ch.Get(
        0.U,
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U
      )._2,
      (mmuReqBits.RequestType_isWrite === 1.U) -> Mux(mmuReqBits.RequestMask.andR, edge_ch.Put(
        0.U,
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U,
        mmuReqBits.RequestData
      )._2, edge_ch.Put(
        0.U,
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U,
        mmuReqBits.RequestData,
        mmuReqBits.RequestMask
      )._2)
    ))

    // Only C matrix reads use the accumulator MatrixKey. C stores still use the normal matrix key.
    val isMatrixCread = mmuReqBits.MatrixIsAcc && !mmuReqBits.RequestType_isWrite
    tlABits.user.lift(MatrixKey).foreach { matrixKey =>
      matrixKey := Mux(isMatrixCread, "b11".U, "b01".U)
    }

    tlABits.user.lift(AmeIndexKey).foreach { ameIndex =>
      ameIndex := mmuReqBits.RequestSourceID
    }

    tlABits.user.lift(MatrixPrefetchTagKey).foreach { tag =>
      tag := mmuReqBits.MatrixPrefetchTag.get
    }

    tlABits.user.lift(ReqSourceKey).foreach { reqSource =>
      reqSource := Mux(
        mmuReqBits.RequestType_isWrite,
        MemReqSource.MatrixWrite.id.U,
        MemReqSource.MatrixRead.id.U
      )
    }

    memoryTraceTable.foreach { table =>
      val prefetchTag = mmuReqBits.MatrixPrefetchTag.get
      val traceTag = mmuReqBits.MatrixTraceTag.get
      val memoryTrace = WireInit(0.U.asTypeOf(new MemoryTraceEntry))
      memoryTrace.cycle := time_stamp
      memoryTrace.channel := channel.U
      memoryTrace.address := mmuReqBits.RequestAddr
      memoryTrace.sourceId := mmuReqBits.RequestSourceID
      memoryTrace.isWrite := mmuReqBits.RequestType_isWrite
      memoryTrace.isAcc := mmuReqBits.MatrixIsAcc
      memoryTrace.prefetchTag := prefetchTag
      memoryTrace.prefetchStream := MatrixPrefetchTagCodec.stream(prefetchTag)
      memoryTrace.prefetchTaskId := MatrixPrefetchTagCodec.taskId(prefetchTag)
      memoryTrace.traceTag := traceTag
      memoryTrace.traceStream := MatrixPrefetchTagCodec.stream(traceTag)
      memoryTrace.traceTaskId := MatrixPrefetchTagCodec.taskId(traceTag)
      table.log(memoryTrace, mmuReq.fire, "MemoryRequest", clock, reset)
    }

    // Direct pass-through for TrLoader0 responses from TL-D channel
    // Recover AmeIndex from the user field; tlDBits.source is not used for this identity.
    val ameIndexFromUser = tlDBits.user.lift(AmeIndexKey).getOrElse(0.U)

    // AmeIndex may be zero because RequestSourceID starts at zero.

    mmuResp.valid := tlD.valid || matrixDataIn.valid
    mmuRespBits.ReseponseData := matrixDataIn.bits.data
    mmuRespBits.ReseponseSourceID := Mux(
      matrixDataIn.valid,
      matrixDataIn.bits.source,
      ameIndexFromUser
    )
    mmuRespBits.ReseponseConherent := true.B

    // Direct pass-through for TrLoader0 matrix_data_in
    matrixDataIn.ready := mmuResp.ready
    tlD.ready := mmuResp.ready && !matrixDataIn.valid

    transportIdle(channel) := !mmuReq.valid &&
      !matrixDataIn.valid && !tlD.valid && !mmuResp.valid

    // Debug output for TrLoader0 channels
    when(mmuReq.fire){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d Req.fire sourceId=%d, useAllocId=%d, addr=0x%x, isWrite=%d, matrixIsAcc=%d\n",
          time_stamp, channel.U, mmuReqBits.RequestSourceID, useAllocId,
          mmuReqBits.RequestAddr, mmuReqBits.RequestType_isWrite, mmuReqBits.MatrixIsAcc)
      }
    }

    // T7: Log TR loader responses.
    when(mmuResp.fire){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d Resp.fire sourceId=%d, matrixDataIn.valid=%d, tlD.valid=%d, data=0x%x\n",
          time_stamp, channel.U, mmuRespBits.ReseponseSourceID, matrixDataIn.valid, tlD.valid, mmuRespBits.ReseponseData)
      }
    }

    // T7: Log TileLink D-channel state.
    when(tlD.valid){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d TL-D valid=%x, ready=%x, source=%x, ameIndexUser=%x\n",
          time_stamp, channel.U, tlD.valid, tlD.ready, tlDBits.source,
          tlDBits.user.lift(AmeIndexKey).getOrElse(0.U))
      }
    }
  }

  // L2 routes matrix responses by physical slice, NOT originating CUTE
  // channel. Only the all-channel totals have a 1:1 request/response contract.
  val pending = RegInit(0.U(pendingWidth.W))
  val accepted = PopCount(io.mmu.Request.map(_.fire))
  val returned = PopCount(io.mmu.Response.map(_.fire))
  val available = pending +& accepted
  val remaining = available - returned
  assert(returned <= available, "CUTE2TL extra/unowned response")
  assert(remaining <= maxOutstanding.U, "CUTE2TL outstanding bound exceeded")
  when(accepted =/= returned) {
    pending := remaining
  }
  io.idle := pending === 0.U && transportIdle.asUInt.andR
  io.mmu.ConherentRequsetSourceID.bits := 0.U
  io.mmu.ConherentRequsetSourceID.valid := true.B
  io.mmu.nonConherentRequsetSourceID.bits := 0.U
  io.mmu.nonConherentRequsetSourceID.valid := false.B
}
