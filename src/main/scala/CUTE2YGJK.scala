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
import coupledL2.{MatrixKey, MatrixField, AmeIndexKey, AmeIndexField}

class WithCuteCoustomParams(val CoustomCuteParam:CuteParams = CuteParams.baseParams) extends Config((site, here, up) => {
    case CuteParamsKey => CoustomCuteParam
})

// class RoCC2CUTE(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) with CUTEImplParameters{
//   override lazy val module = new CUTETile(this)
//  lazy val LLCMemPort = LazyModule(new Cute2TL)
//  atlNode := TLWidthWidget(outsideDataWidthByte) := LLCMemPort.node
// }






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
  val source = UInt(32.W)
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

  // // Initialize all MMU Response channels.
  // for (i <- 0 until ABMatrixRegNBanks) {
  //   io.mmu.Response(i).valid := false.B
  //   io.mmu.Response(i).bits.ReseponseData := 0.U
  //   io.mmu.Response(i).bits.ReseponseConherent := false.B
  // }

  // ============================================================
  // AML Direct Path: Channels 1-7, simplified bypass logic
  // ============================================================
  // AML uses channels 1-7 with direct connection to TileLink
  // No complex source ID management, just direct pass-through

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

    // Direct pass-through for AML requests
    tlA.valid := mmuReq.valid
    mmuReq.ready := tlA.ready

    tlABits := Mux1H(Seq(
      (mmuReqBits.RequestType_isWrite === 0.U) -> edge_ch.Get(
        0.U,  // 固定 source，不再使用 RequestSourceID
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U
      )._2,
      (mmuReqBits.RequestType_isWrite === 1.U) -> edge_ch.Put(
        0.U,  // 固定 source，不再使用 RequestSourceID
        mmuReqBits.RequestAddr,
        log2Ceil(outsideDataWidthByte).U,
        mmuReqBits.RequestData,
        mmuReqBits.RequestMask
      )._2
    ))

    // Assign MatrixKey for AML channels (A matrix: "b01")
    tlABits.user.lift(MatrixKey).foreach { matrixKey =>
      matrixKey := Mux(io.mmu.Request(0).bits.MatrixIsAcc, "b11".U, "b01".U)
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
        printf("[CUTE2TL][%d] Channel %d: A.fire, AmeIndex(RequestSourceID)=%d, source=%d, addr=0x%x, isWrite=%d, MatrixIsAcc=%d\n",
          time_stamp, channel.U, mmuReqBits.RequestSourceID, tlABits.source,
          mmuReqBits.RequestAddr, mmuReqBits.RequestType_isWrite, mmuReqBits.MatrixIsAcc)
      }
    }

    // T7: 添加 AML Response 调试日志
    when(mmuResp.fire){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Channel %d: Resp.fire, source(from_user)=%d, matrixDataIn.valid=%d, tlD.valid=%d, data=0x%x\n",
          time_stamp, channel.U, mmuRespBits.ReseponseSourceID, matrixDataIn.valid, tlD.valid, mmuRespBits.ReseponseData)
      }
    }

    // T7: 添加 TL-D 通道状态调试日志
    when(tlD.valid){
      if (YJPDebugEnable)
      {
        printf("[CUTE2TL][%d] Channel %d TL-D: valid=%x, ready=%x, source=%x, AmeIndex(user)=%x\n",
          time_stamp, channel.U, tlD.valid, tlD.ready, tlDBits.source,
          tlDBits.user.lift(AmeIndexKey).getOrElse(0.U))
      }
    }
  }

  // id-related IO wiring.
  io.idle := true.B
  io.mmu.ConherentRequsetSourceID.bits := 0.U           // Only BC ML need this.
  io.mmu.ConherentRequsetSourceID.valid := true.B    // Only BC ML need this.
  io.mmu.nonConherentRequsetSourceID.valid := false.B
  io.mmu.nonConherentRequsetSourceID.bits := 0.U
}


// class CUTETile(outer: RoCC2CUTE) extends LazyRoCCModuleImp(outer) with CUTEImplParameters {
//     val acc = Module(new CUTEV2Top)
//     val mem = (outer.LLCMemPort.module)

//     val rs1 = RegInit(0.U(64.W))
//     val rs2 = RegInit(0.U(64.W))
//     val rd_data = RegInit(0.U(64.W))
//     val rd = RegInit(0.U(5.W))
//     val func = RegInit(0.U(7.W))
//     val canResp = RegInit(false.B)
//     val ac_busy = RegInit(false.B)
//     val configV = RegInit(false.B)

//     val count = RegInit(0.U(64.W))
//     when(ac_busy){
//       count := count + 1.U
//     }
//     val compute = RegInit(0.U(64.W))
//     when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 0.U){
//       compute := 0.U
//     }.elsewhen(ac_busy){
//       compute := compute + 1.U
//     }

//     val memNum_r = RegInit(0.U(64.W))
//     val memNum_w = RegInit(0.U(64.W))

//     val missAddr = RegInit(0.U(vaddrBits.W))

//     val jk_idle :: jk_compute :: jk_resp :: jk_lmmu_miss :: Nil = Enum(4)
//     val jk_state = RegInit(jk_idle)

//     mem.io.mmu <> acc.io.mmu2llc


//     //一拍的时间接受指令，下一拍的时间返回结果
//     //后面可以设置成一个指令fifo
//     io.cmd.ready := !canResp
//     when(io.cmd.fire && io.cmd.bits.inst.xd === true.B){
//       canResp := true.B
//     }.elsewhen(io.resp.fire){
//       canResp := false.B
//     }
//     // if (YJPDebugEnable)
//     // {
//     //     //输出io.cmd的信息和io.resp的信息
//     //     printf("[CUTE2YGJK.top]io.cmd.fire: %x, io.cmd.bits.inst: %x, io.cmd.bits.rs1: %x, io.cmd.bits.rs2: %x, io.cmd.bits.inst.rd: %x, io.cmd.bits.inst.funct: %x\n", io.cmd.fire, io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.inst.rd, io.cmd.bits.inst.funct)
//     //     //输出valid和ready信息
//     //     printf("[CUTE2YGJK.top]io.cmd.valid: %x, io.cmd.ready: %x, io.resp.valid: %x, io.resp.ready: %x\n", io.cmd.valid, io.cmd.ready, io.resp.valid, io.resp.ready)
//     }

//     rd := io.cmd.bits.inst.rd    //下一拍一定会返回
//     io.resp.bits.rd := rd
//     io.resp.bits.data := rd_data
//     io.resp.valid := canResp

//     when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 1.U){ //查询加速器是否在运行
//       rd_data := ac_busy
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 2.U){ //查询加速器运行时间
//       rd_data := count
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 3.U){ //查询加速器对外访存读次数
//       rd_data := memNum_r
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 4.U){ //查询加速器对外访存写次数
//       rd_data := memNum_w
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 5.U){ //查询加速器计算时间
//       rd_data := compute
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 6.U){ //查询CUTE宏指令的完成情况
//       rd_data := acc.io.ctrl2top.InstFIFO_Finish
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 7.U){ //查询CUTE宏指令队列是否已满
//       rd_data := acc.io.ctrl2top.InstFIFO_Full
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 8.U){ //查询CUTE宏指令队列目前有多少指令
//       rd_data := acc.io.ctrl2top.InstFIFO_Info
//     }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct >= 64.U){
//       rd_data := acc.io.ctrl2top.cute_return_val
//     }

//     when(acc.io.mmu2llc.Request.fire){
//         when(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 0.U){
//             memNum_r := memNum_r + 1.U
//         }.elsewhen(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 1.U){
//             memNum_w := memNum_w + 1.U
//         }
//     }
//     io.interrupt := false.B
//     // io.badvaddr_ygjk := Mux(jk_state=/=jk_resp, missAddr, missAddr+1.U)
//     switch(jk_state){
//       is(jk_idle){
//         when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct(5,0) === 0.U){
//           ac_busy := true.B
//           jk_state := jk_compute
//           count := 0.U
//           memNum_r := 0.U
//           memNum_w := 0.U
//         }
//       }

//       is(jk_compute){
//         if (YJPDebugEnable)
//         {
//             printf(p"[CUTE2YGJK.top]ac_busy = $ac_busy\n")
//         }
//         when(acc.io.ctrl2top.acc_running === false.B && mem.io.idle){
//           jk_state := jk_resp
//         }
//       }

//       is(jk_resp){
// //        io.interrupt := true.B
//         ac_busy := false.B
//         when(io.cmd.fire && io.cmd.bits.inst.opcode === "h2B".U && io.cmd.bits.inst.funct === 0.U){
//           // 收到中断响应
//           jk_state := jk_idle
//         }
//       }
//     }
//     //opcode对应的是路由到某个加速器用的，CUSTOM0、CUSTOM1、CUSTOM2、CUSTOM3这四组opcode
//     //我们这里默认使用opcode为0x0B的指令，将funct的最高位为1的指令作为配置指令。
//     acc.io.ctrl2top.config.valid := io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct(6) === 1.U
//     //输出指令信息,io.cmd.bits.inst.funct
//     // printf("funct: %x\n", io.cmd.bits.inst.funct)
//     when(io.cmd.fire){
//         if (YJPDebugEnable)
//         {
//             printf("CUTE: opcode: %x, rs1: %x, rs2: %x, rd: %x, funct: %x\n", io.cmd.bits.inst.opcode, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.rd, io.cmd.bits.inst.funct)
//         }
//     }
//     acc.io.ctrl2top.config.bits.cfgData1 := io.cmd.bits.rs1
//     acc.io.ctrl2top.config.bits.cfgData2 := io.cmd.bits.rs2
//     acc.io.ctrl2top.config.bits.func := io.cmd.bits.inst.funct
//     acc.io.ctrl2top.reset := false.B  //多次重启时置位，未实现

// }
