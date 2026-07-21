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
import xscache.coupledL2.{MatrixKey, MatrixField, AmeIndexKey, AmeIndexField}

class WithCuteCoustomParams(val CoustomCuteParam:CuteParams = CuteParams.baseParams) extends WithCuteParams(CoustomCuteParam)

class Cute2TL(implicit p: Parameters) extends LazyModule with CUTEImplParameters {
  lazy val module = new CUTE2TLImp(this)
  // Changed from single TLClientNode with Seq.tabulate to Seq of independent TLClientNodes
  val node = Seq.tabulate(ABMatrixRegNBanks) { i =>
    TLClientNode(
      Seq(TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name = s"CUTE_$i",
          sourceId = IdRange(0, 1)  // 固定 source ID，信息通过 AmeIndex 传递
        )),
        requestFields = Seq(MatrixField(2), AmeIndexField()),
        responseKeys = Seq(AmeIndexKey)
      ))
    )
  }
}

//这里的CUTE到LLC的节点

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
    val matrix_data_in = Flipped(Vec(ABMatrixRegNBanks, Decoupled(new MatrixDataIO)))
  })

  val time_stamp = RegInit(0.U(64.W))
  time_stamp := time_stamp + 1.U

  for (channel <- 0 until ABMatrixRegNBanks) {
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

    assert(
      !(mmuReq.valid && mmuReqBits.RequestType_isWrite) || mmuReqBits.RequestMask.andR,
      "HBL2 supports PutFullData only, so Mask must be set to all 1"
    )

    tlABits := Mux1H(Seq(
      (mmuReqBits.RequestType_isWrite === 0.U) -> edge_ch.Get(
        0.U,
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U
      )._2,
      (mmuReqBits.RequestType_isWrite === 1.U) -> edge_ch.Put(
        0.U,
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U,
        mmuReqBits.RequestData
      )._2
    ))

    // Only C matrix reads use the accumulator MatrixKey. C stores still use the normal matrix key.
    val isMatrixCread = mmuReqBits.MatrixIsAcc && !mmuReqBits.RequestType_isWrite
    tlABits.user.lift(MatrixKey).foreach { matrixKey =>
      matrixKey := Mux(isMatrixCread, "b11".U, "b01".U)
    }

    tlABits.user.lift(AmeIndexKey).foreach { ameIndex =>
      ameIndex := mmuReqBits.RequestSourceID
    }

    // Direct pass-through for AML responses from TL-D channel
    // 从 user 字段获取 AmeIndex（完全忽略 tlDBits.source）
    val ameIndexFromUser = tlDBits.user.lift(AmeIndexKey).getOrElse(0.U)

    // 注意：ameIndex 可以为 0（RequestSourceID 从 0 开始），不检查是否为 0

    mmuResp.valid := tlD.valid || matrixDataIn.valid
    mmuRespBits.ReseponseData := matrixDataIn.bits.data
    mmuRespBits.ReseponseSourceID := Mux(
      matrixDataIn.valid,
      matrixDataIn.bits.source,
      ameIndexFromUser
    )
    mmuRespBits.ReseponseConherent := true.B

    // Direct pass-through for AML matrix_data_in
    matrixDataIn.ready := mmuResp.ready
    tlD.ready := mmuResp.ready && !matrixDataIn.valid

    // Debug output for AML channels
    when(mmuReq.fire){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d Req.fire sourceId=%d, useAllocId=%d, addr=0x%x, isWrite=%d, matrixIsAcc=%d\n",
          time_stamp, channel.U, mmuReqBits.RequestSourceID, useAllocId,
          mmuReqBits.RequestAddr, mmuReqBits.RequestType_isWrite, mmuReqBits.MatrixIsAcc)
      }
    }

    // T7: 添加 AML Response 调试日志
    when(mmuResp.fire){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d Resp.fire sourceId=%d, matrixDataIn.valid=%d, tlD.valid=%d, data=0x%x\n",
          time_stamp, channel.U, mmuRespBits.ReseponseSourceID, matrixDataIn.valid, tlD.valid, mmuRespBits.ReseponseData)
      }
    }

    // T7: 添加 TL-D 通道状态调试日志
    when(tlD.valid){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Ch%d TL-D valid=%x, ready=%x, source=%x, ameIndexUser=%x\n",
          time_stamp, channel.U, tlD.valid, tlD.ready, tlDBits.source,
          tlDBits.user.lift(AmeIndexKey).getOrElse(0.U))
      }
    }
  }

  io.idle := true.B
  io.mmu.ConherentRequsetSourceID.bits := 0.U
  io.mmu.ConherentRequsetSourceID.valid := true.B
  io.mmu.nonConherentRequsetSourceID.bits := 0.U
  io.mmu.nonConherentRequsetSourceID.valid := false.B
}
