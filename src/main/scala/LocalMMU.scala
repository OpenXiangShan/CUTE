package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility._

class LocalMMU()(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ALocalMMUIO = (new LocalMMUIO)
        val BLocalMMUIO = (new LocalMMUIO)
        val BSLocalMMUIO = Option.when(cuteMatrixExtension.enableScalingFactor)(new LocalMMUIO)
        val ASLocalMMUIO = Option.when(cuteMatrixExtension.enableScalingFactor)(new LocalMMUIO)
        val CLoadLocalMMUIO = (new LocalMMUIO)
        val CStoreLocalMMUIO = (new LocalMMUIO)
        val LastLevelCacheTLIO = Flipped(new MMU2TLIO)
        val perfProbe = Output(new LocalMMUPerfProbe)
    })

    val FirstRequestIndex = RegInit(0.U(LocalMMUTaskType.TaskTypeBitWidth.W))
    val FirstIndex = FirstRequestIndex
    val SecIndex = WrapInc(FirstIndex, LocalMMUTaskType.TaskTypeMax)
    val ThirdIndex = WrapInc(SecIndex, LocalMMUTaskType.TaskTypeMax)
    val FourthIndex = WrapInc(ThirdIndex, LocalMMUTaskType.TaskTypeMax)
    val FifthIndex = WrapInc(FourthIndex, LocalMMUTaskType.TaskTypeMax)
    val SixthIndex = WrapInc(FifthIndex, LocalMMUTaskType.TaskTypeMax)

    // Bit index follows LocalMMUTaskType encoding directly.
    val AllRequestValid = Cat(
      io.ASLocalMMUIO.map(_.Request.valid).getOrElse(false.B),
      io.BSLocalMMUIO.map(_.Request.valid).getOrElse(false.B),
      io.CStoreLocalMMUIO.Request.valid,
      io.CLoadLocalMMUIO.Request.valid,
      io.BLocalMMUIO.Request.valid,
      io.ALocalMMUIO.Request.valid
    )
    val HasRequest = AllRequestValid.orR
    val ChoseIndex_0 = Mux(AllRequestValid(FirstIndex), FirstIndex,
                        Mux(AllRequestValid(SecIndex), SecIndex,
                        Mux(AllRequestValid(ThirdIndex), ThirdIndex,
                        Mux(AllRequestValid(FourthIndex), FourthIndex,
                        Mux(AllRequestValid(FifthIndex), FifthIndex,
                        Mux(AllRequestValid(SixthIndex), SixthIndex, LocalMMUTaskType.TaskTypeMax.U))))))

    FirstRequestIndex := WrapInc(ChoseIndex_0, LocalMMUTaskType.TaskTypeMax)

    io.ALocalMMUIO.Request.ready := false.B
    io.BLocalMMUIO.Request.ready := false.B
    io.ASLocalMMUIO.foreach(_.Request.ready := false.B)
    io.BSLocalMMUIO.foreach(_.Request.ready := false.B)
    io.CLoadLocalMMUIO.Request.ready := false.B
    io.CStoreLocalMMUIO.Request.ready := false.B

    io.ALocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.BLocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.ASLocalMMUIO.foreach(_.ConherentRequsetSourceID.valid := false.B)
    io.BSLocalMMUIO.foreach(_.ConherentRequsetSourceID.valid := false.B)
    io.CLoadLocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.CStoreLocalMMUIO.ConherentRequsetSourceID.valid := false.B

    io.ALocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.BLocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.ASLocalMMUIO.foreach(_.ConherentRequsetSourceID.bits := DontCare)
    io.BSLocalMMUIO.foreach(_.ConherentRequsetSourceID.bits := DontCare)
    io.CLoadLocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.CStoreLocalMMUIO.ConherentRequsetSourceID.bits := DontCare

    io.ALocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.BLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.ASLocalMMUIO.foreach(_.nonConherentRequsetSourceID.valid := false.B)
    io.BSLocalMMUIO.foreach(_.nonConherentRequsetSourceID.valid := false.B)
    io.CLoadLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.CStoreLocalMMUIO.nonConherentRequsetSourceID.valid := false.B

    io.ALocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    io.BLocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    io.ASLocalMMUIO.foreach(_.nonConherentRequsetSourceID.bits := DontCare)
    io.BSLocalMMUIO.foreach(_.nonConherentRequsetSourceID.bits := DontCare)
    io.CLoadLocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    io.CStoreLocalMMUIO.nonConherentRequsetSourceID.bits := DontCare

    val sourceid2port = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))))

    io.LastLevelCacheTLIO.Request.valid := false.B
    io.LastLevelCacheTLIO.Request.bits := DontCare
    io.LastLevelCacheTLIO.Response.ready := false.B

    when(io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid && HasRequest)
    {
        switch(ChoseIndex_0) {
            is(LocalMMUTaskType.AFirst) {
                io.ALocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.ALocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.ALocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := false.B
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.AFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B
            }
            is(LocalMMUTaskType.AScaleFirst){
                io.ASLocalMMUIO.foreach { asLocalMMUIO =>
                    asLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                    asLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                    io.LastLevelCacheTLIO.Request.bits.RequestData := asLocalMMUIO.Request.bits.RequestData
                    io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := asLocalMMUIO.Request.bits.RequestType_isWrite
                    sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.AScaleFirst
                    io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B
                }
            }
            is(LocalMMUTaskType.BFirst) {
                io.BLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.BLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.BLocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := false.B
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.BFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B
            }
            is(LocalMMUTaskType.BScaleFirst) {
                io.BSLocalMMUIO.foreach { bsLocalMMUIO =>
                    bsLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                    bsLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                    io.LastLevelCacheTLIO.Request.bits.RequestData := bsLocalMMUIO.Request.bits.RequestData
                    io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := bsLocalMMUIO.Request.bits.RequestType_isWrite
                    sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.BScaleFirst
                    io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B
                }
            }
            is(LocalMMUTaskType.CLoadFirst) {
                io.CLoadLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.CLoadLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.CLoadLocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestData := io.CLoadLocalMMUIO.Request.bits.RequestData
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := io.CLoadLocalMMUIO.Request.bits.RequestType_isWrite
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.CLoadFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := true.B
            }
            is(LocalMMUTaskType.CStoreFirst) {
                io.CStoreLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.CStoreLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.CStoreLocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestData := io.CStoreLocalMMUIO.Request.bits.RequestData
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := io.CStoreLocalMMUIO.Request.bits.RequestType_isWrite
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.CStoreFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := true.B
            }
        }

        io.LastLevelCacheTLIO.Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LastLevelCacheTLIO.Request.bits.RequestConherent := true.B
        io.LastLevelCacheTLIO.Request.bits.RequestSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits
        io.LastLevelCacheTLIO.Request.valid := true.B
    }

    io.ALocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.BLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.ASLocalMMUIO.foreach(_.Response.bits := io.LastLevelCacheTLIO.Response.bits)
    io.BSLocalMMUIO.foreach(_.Response.bits := io.LastLevelCacheTLIO.Response.bits)
    io.CLoadLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.CStoreLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits

    io.ALocalMMUIO.Response.valid := false.B
    io.BLocalMMUIO.Response.valid := false.B
    io.ASLocalMMUIO.foreach(_.Response.valid := false.B)
    io.BSLocalMMUIO.foreach(_.Response.valid := false.B)
    io.CLoadLocalMMUIO.Response.valid := false.B
    io.CStoreLocalMMUIO.Response.valid := false.B

    switch(sourceid2port(io.LastLevelCacheTLIO.Response.bits.ReseponseSourceID)) {
        is(LocalMMUTaskType.AFirst) {
            io.ALocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.ALocalMMUIO.Response.ready
        }
        is(LocalMMUTaskType.AScaleFirst) {
            io.ASLocalMMUIO.foreach { asLocalMMUIO =>
                asLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
                io.LastLevelCacheTLIO.Response.ready := asLocalMMUIO.Response.ready
            }
        }
        is(LocalMMUTaskType.BFirst) {
            io.BLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.BLocalMMUIO.Response.ready
        }
        is(LocalMMUTaskType.BScaleFirst){
            io.BSLocalMMUIO.foreach { bsLocalMMUIO =>
                bsLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
                io.LastLevelCacheTLIO.Response.ready := bsLocalMMUIO.Response.ready
            }
        }
        is(LocalMMUTaskType.CLoadFirst) {
            io.CLoadLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.CLoadLocalMMUIO.Response.ready
        }
        is(LocalMMUTaskType.CStoreFirst) {
            io.CStoreLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.CStoreLocalMMUIO.Response.ready
        }
    }

    XSPerfAccumulate("CUTE_MMU_A_rd_request", io.ALocalMMUIO.Request.fire & !io.ALocalMMUIO.Request.bits.RequestType_isWrite)
    XSPerfAccumulate("CUTE_MMU_A_wr_request", io.ALocalMMUIO.Request.fire & io.ALocalMMUIO.Request.bits.RequestType_isWrite)
    XSPerfAccumulate("CUTE_MMU_B_rd_request", io.BLocalMMUIO.Request.fire & !io.BLocalMMUIO.Request.bits.RequestType_isWrite)
    XSPerfAccumulate("CUTE_MMU_B_wr_request", io.BLocalMMUIO.Request.fire & io.BLocalMMUIO.Request.bits.RequestType_isWrite)

    val cLoadRd = io.CLoadLocalMMUIO.Request.fire & !io.CLoadLocalMMUIO.Request.bits.RequestType_isWrite
    val cStoreRd = io.CStoreLocalMMUIO.Request.fire & !io.CStoreLocalMMUIO.Request.bits.RequestType_isWrite
    val cLoadWr = io.CLoadLocalMMUIO.Request.fire & io.CLoadLocalMMUIO.Request.bits.RequestType_isWrite
    val cStoreWr = io.CStoreLocalMMUIO.Request.fire & io.CStoreLocalMMUIO.Request.bits.RequestType_isWrite
    XSPerfAccumulate("CUTE_MMU_C_rd_request", cLoadRd || cStoreRd)
    XSPerfAccumulate("CUTE_MMU_C_wr_request", cLoadWr || cStoreWr)

    val outReqFire = io.LastLevelCacheTLIO.Request.fire
    val outReqIsWr = io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite
    val outReqMask32B = PopCount(io.LastLevelCacheTLIO.Request.bits.RequestMask) >> 5
    io.perfProbe.rdReq := outReqFire && !outReqIsWr
    io.perfProbe.wrReq := outReqFire && outReqIsWr
    io.perfProbe.rd32BReq := Mux(outReqFire && !outReqIsWr, outReqMask32B, 0.U).asUInt
    io.perfProbe.wr32BReq := Mux(outReqFire && outReqIsWr, outReqMask32B, 0.U).asUInt
}
