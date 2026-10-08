package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility._

class LocalMMU()(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ALocalMMUIO = (new LocalMMUIO)
        val BLocalMMUIO = (new LocalMMUIO)
        val BSLocalMMUIO = (new LocalMMUIO)
        val ASLocalMMUIO = (new LocalMMUIO)
        val CLoadLocalMMUIO = (new LocalMMUIO)
        val CStoreLocalMMUIO = (new LocalMMUIO)
        val LastLevelCacheTLIO = Flipped(new MMU2TLIO)
        val perfProbe = Output(new LocalMMUPerfProbe)
    })

    val logTime = RegInit(0.U(64.W))
    logTime := logTime + 1.U

    //8通道并行访存：同时处理所有8个通道的请求
    //每个通道独立处理，不需要仲裁

    // 统一处理 ABCScale 五类 MMUIO 的 Request/Response
    val abcss = Seq(io.ALocalMMUIO, io.BLocalMMUIO, io.CLoadLocalMMUIO, io.CStoreLocalMMUIO, io.ASLocalMMUIO, io.BSLocalMMUIO)

    // Initialize all channels to false.B
    for (mmuio <- abcss; i <- 0 until ABMatrixRegNBanks) {
        mmuio.Request(i).ready := false.B
        mmuio.Response(i).valid := false.B
        mmuio.Response(i).bits := 0.U.asTypeOf(mmuio.Response(i).bits)
        mmuio.ConherentRequsetSourceID.valid := false.B
        mmuio.ConherentRequsetSourceID.bits := 0.U
        mmuio.nonConherentRequsetSourceID.valid := false.B
        mmuio.nonConherentRequsetSourceID.bits := 0.U
    }

    // Initialize LLC IO
    for (i <- 0 until ABMatrixRegNBanks) {
        io.LastLevelCacheTLIO.Request(i).valid := false.B
        io.LastLevelCacheTLIO.Request(i).bits := 0.U.asTypeOf(io.LastLevelCacheTLIO.Request(i).bits)
        // 默认拉低：仅在选择 A 矩阵时置位
        io.LastLevelCacheTLIO.Request(i).bits.isA := false.B
        io.LastLevelCacheTLIO.Response(i).ready := false.B
    }

    // Low three bits identify the request source.
    // encodedId = (origId << 3) | sourceTag
    val SourceTagWidth = 3
    val AReadTag  = 0.U(SourceTagWidth.W) // A 读
    val BReadTag  = 1.U(SourceTagWidth.W) // B 读
    val CReadTag  = 2.U(SourceTagWidth.W) // C 读（与 CUTE2YGJK 中 CReadTag 保持一致）
    val CWriteTag = 3.U(SourceTagWidth.W) // C 写（与 CUTE2YGJK 中 CWriteTag 保持一致）
    val ASReadTag = 4.U(SourceTagWidth.W) // AS 读 - 使用独立channel，tag可以复用
    val BSReadTag = 5.U(SourceTagWidth.W) // BS 读 - 使用独立channel，tag可以复用

    def encodeSourceId(origId: UInt, tag: UInt): UInt = {
        (origId << SourceTagWidth) | tag
    }

    val AllocatedOrigIdFlagBit = 60
    def markAllocatedOrigId(id: UInt): UInt = {
        Cat(1.U(1.W), 0.U((AllocatedOrigIdFlagBit - LLCSourceMaxNumBitSize).W), id.pad(LLCSourceMaxNumBitSize))
    }
    def isAllocatedOrigId(id: UInt): Bool = id.pad(64)(AllocatedOrigIdFlagBit)
    def restoreLoaderSourceId(id: UInt): UInt = {
        val id64 = id.pad(64)
        Mux(
            isAllocatedOrigId(id64),
            Cat(0.U((64 - LLCSourceMaxNumBitSize).W), id64(LLCSourceMaxNumBitSize - 1, 0)),
            id64
        )
    }

    val allocatedBusy = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(false.B)))
    val allocatedTag = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(0.U(SourceTagWidth.W))))
    val allocatedId = Wire(UInt(LLCSourceMaxNumBitSize.W))
    allocatedId := 0.U
    for (i <- 0 until LLCSourceMaxNum) {
        when(!allocatedBusy(i)) {
            allocatedId := i.U
        }
    }
    val allocFull = allocatedBusy.reduce(_&&_)

    val scheduler = Module(new CollisionAwareLocalMMUScheduler(
        laneCount = ABMatrixRegNBanks,
        bankCount = ABMatrixRegNBanks,
        aResponseChannels = AMLResponseChannelCount,
        bResponseChannels = BMLResponseChannelCount,
        cLoadResponseChannels = CLoadBridgeResponseChannelCount,
        cStoreResponseChannels = CStoreBridgeResponseChannelCount,
        aLegacy = AMLUseLegacyLoader,
        bLegacy = BMLUseLegacyLoader,
        cLoadLegacy = CLoadUseLegacyLoader,
        cStoreLegacy = CStoreUseLegacyLoader || CStoreDirectAckCountMode
    ))

    val requestSources = Seq(
        io.CLoadLocalMMUIO.Request,
        io.CStoreLocalMMUIO.Request,
        io.ALocalMMUIO.Request,
        io.ASLocalMMUIO.Request,
        io.BLocalMMUIO.Request,
        io.BSLocalMMUIO.Request
    )
    val sourceTags = Seq(CReadTag, CWriteTag, AReadTag, ASReadTag, BReadTag, BSReadTag)
    val routeDescriptors = Seq(
        CLoadRouteDescriptor,
        CStoreRouteDescriptor,
        AMLRouteDescriptor,
        AMLRouteDescriptor,
        BMLRouteDescriptor,
        BMLRouteDescriptor
    )
    val sourceIsWrite = Seq(false, true, false, false, false, false)
    val sourceIsAcc = Seq(true, true, false, false, false, false)
    val sourceIsA = Seq(false, false, true, false, false, false)

    for (mmuio <- abcss) {
        mmuio.ConherentRequsetSourceID.valid := !allocFull
        mmuio.ConherentRequsetSourceID.bits := allocatedId
        mmuio.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID
    }

    for (source <- 0 until LocalMMUSourceClass.Count; i <- 0 until ABMatrixRegNBanks) {
        val req = requestSources(source)(i)
        val candidate = scheduler.io.candidates(source)(i)
        val useAllocatedId = req.bits.UseAllocatedSourceID
        val effectiveOrigId = Mux(
            useAllocatedId,
            markAllocatedOrigId(allocatedId),
            req.bits.RequestSourceID
        )
        val legalType = req.bits.RequestType_isWrite === sourceIsWrite(source).B

        candidate.valid := req.valid && legalType && (!useAllocatedId || !allocFull)
        candidate.bits.request := req.bits
        if (!sourceIsWrite(source)) {
            candidate.bits.request.MatrixTraceTag.foreach(_ := 0.U)
        }
        candidate.bits.request.MatrixIsAcc := sourceIsAcc(source).B
        candidate.bits.request.isA := sourceIsA(source).B
        candidate.bits.request.RequestSourceID := encodeSourceId(effectiveOrigId, sourceTags(source))
        candidate.bits.physicalInput := IdealL2Banking.bankOf(req.bits.RequestAddr, L2NBanks)
        candidate.bits.returnBank := routeDescriptors(source).extractBankId(effectiveOrigId)
        req.ready := candidate.ready

        when(candidate.valid) {
            assert(candidate.bits.physicalInput < L2NBanks.U,
                cf"LocalMMU request lane $i selected invalid L2 bank ${candidate.bits.physicalInput}")
        }

        when(candidate.fire && useAllocatedId) {
            allocatedBusy(allocatedId) := true.B
            allocatedTag(allocatedId) := sourceTags(source)
        }
    }

    for (i <- 0 until ABMatrixRegNBanks) {
        scheduler.io.llc(i).ready := io.LastLevelCacheTLIO.Request(i).ready
        io.LastLevelCacheTLIO.Request(i).valid := scheduler.io.llc(i).valid
        io.LastLevelCacheTLIO.Request(i).bits := scheduler.io.llc(i).bits

        
    }

    //8通道并行响应处理：按 encodedId 低位标签路由到对应的 MMU IO
    for (i <- 0 until ABMatrixRegNBanks) {
        val llc_response = io.LastLevelCacheTLIO.Response(i)
        val a_mmu_response = io.ALocalMMUIO.Response(i)
        val b_mmu_response = io.BLocalMMUIO.Response(i)
        val c_mmu_response = io.CLoadLocalMMUIO.Response(i)
        val c_store_mmu_response = io.CStoreLocalMMUIO.Response(i)
        val as_mmu_response = io.ASLocalMMUIO.Response(i)
        val bs_mmu_response = io.BSLocalMMUIO.Response(i)

        val encodedId = llc_response.bits.ReseponseSourceID
        val tag = encodedId(SourceTagWidth - 1, 0)
        val origId = Wire(UInt(64.W))
        origId := encodedId >> SourceTagWidth
        val loaderSourceId = Wire(UInt(64.W))
        loaderSourceId := restoreLoaderSourceId(origId)
        val data = llc_response.bits.ReseponseData

        // 默认保持 ready 低电平，后续根据路由目的端覆盖
        llc_response.ready := false.B

        // 需要根据tag和channel来路由
        // AReadTag(0) -> A
        // BReadTag(1) -> B
        // CReadTag(2), CWriteTag(3) -> C
        // ASReadTag(0) on AS channel -> AS (使用不同channel区分)
        // BSReadTag(1) on BS channel -> BS
        // 这里简化处理：检查原始请求哪个有效来决定路由
        // 更精确的做法是记录每个channel当前处理的请求类型

        when(llc_response.valid) {
            // 简化路由：根据tag直接路由
            // 注意：AS/BS 使用与 A/B 相同的 tag，但请求源不同
            // 由于 AS/BS 有独立的channel，我们需要额外信息来区分
            // 这里使用 sourceid 的高位来记录原始请求类型

            // 简化方案：直接根据tag路由
            when(tag === AReadTag) {
                // 需要区分 A 和 AS - 通过检查哪个请求在飞行中
                a_mmu_response.valid := true.B
                a_mmu_response.bits := llc_response.bits
                a_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := a_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === BReadTag) {
                b_mmu_response.valid := true.B
                b_mmu_response.bits := llc_response.bits
                b_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := b_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === ASReadTag) {
                as_mmu_response.valid := true.B
                as_mmu_response.bits := llc_response.bits
                as_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := as_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === BSReadTag) {
                bs_mmu_response.valid := true.B
                bs_mmu_response.bits := llc_response.bits
                bs_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := bs_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === CReadTag) {
                c_mmu_response.valid := true.B
                c_mmu_response.bits := llc_response.bits
                c_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := c_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === CWriteTag) {
                c_store_mmu_response.valid := true.B
                c_store_mmu_response.bits := llc_response.bits
                c_store_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := c_store_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.otherwise{
                assert(false.B, cf"Unsupported tag ${tag} in LLC response routing")
            }
        }

        Seq(
          ("A", a_mmu_response),
          ("B", b_mmu_response),
          ("AS", as_mmu_response),
          ("BS", bs_mmu_response),
          ("CLoad", c_mmu_response),
          ("CStore", c_store_mmu_response)
        ).foreach { case (targetName, resp) =>

        }
    }

    val outReqFire = VecInit(io.LastLevelCacheTLIO.Request.map(_.fire))
    val outReqIsWr = VecInit(io.LastLevelCacheTLIO.Request.map(_.bits.RequestType_isWrite))
    val outReqMask32B = VecInit(io.LastLevelCacheTLIO.Request.map(req => PopCount(req.bits.RequestMask) >> 5))
    val outReqRdFire = VecInit((0 until ABMatrixRegNBanks).map(i => outReqFire(i) && !outReqIsWr(i)))
    val outReqWrFire = VecInit((0 until ABMatrixRegNBanks).map(i => outReqFire(i) && outReqIsWr(i)))

    io.perfProbe.rdReq := outReqRdFire.asUInt.orR
    io.perfProbe.wrReq := outReqWrFire.asUInt.orR
    io.perfProbe.rd32BReq := (0 until ABMatrixRegNBanks).map(i => Mux(outReqRdFire(i), outReqMask32B(i), 0.U)).reduce(_ +& _)
    io.perfProbe.wr32BReq := (0 until ABMatrixRegNBanks).map(i => Mux(outReqWrFire(i), outReqMask32B(i), 0.U)).reduce(_ +& _)
    for (source <- 0 until LocalMMUSourceClass.Count) {
        io.perfProbe.offered(source) := PopCount(scheduler.io.candidates(source).map(_.valid))
        io.perfProbe.issued(source) := PopCount(scheduler.io.candidates(source).map(_.fire))
    }
    io.perfProbe.collisionMasked := scheduler.io.collisionMasked
    io.perfProbe.selectedOwner := scheduler.io.selectedOwner
    io.perfProbe.evaluated := scheduler.io.evaluated
    io.perfProbe.priorityBlocked := scheduler.io.priorityBlocked
    io.perfProbe.selectedFire := scheduler.io.selectedFire
    io.perfProbe.held := scheduler.io.held
    io.perfProbe.cLoadRefill := scheduler.io.cLoadRefill

    def countRequestFire(reqs: Vec[DecoupledIO[MMURequestIO]], predicate: DecoupledIO[MMURequestIO] => Bool): UInt = {
        PopCount(reqs.map(req => req.fire && predicate(req)))
    }
    def countAllRequestFire(reqs: Vec[DecoupledIO[MMURequestIO]]): UInt = {
        PopCount(reqs.map(_.fire))
    }

    XSPerfAccumulate("CUTE_MMU_A_rd_request", countRequestFire(io.ALocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_A_wr_request", countRequestFire(io.ALocalMMUIO.Request, req => req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_B_rd_request", countRequestFire(io.BLocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_B_wr_request", countRequestFire(io.BLocalMMUIO.Request, req => req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_CLoad_request", countAllRequestFire(io.CLoadLocalMMUIO.Request))
    XSPerfAccumulate("CUTE_MMU_CStore_request", countAllRequestFire(io.CStoreLocalMMUIO.Request))
    XSPerfAccumulate("CUTE_MMU_AS_rd_request", countRequestFire(io.ASLocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_BS_rd_request", countRequestFire(io.BSLocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
}
