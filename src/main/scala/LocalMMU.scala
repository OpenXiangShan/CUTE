package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility._

class LocalMMU()(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val trLoader0Memory = (new LocalMMUIO)
        val trLoader1Memory = (new LocalMMUIO)
        val BSLocalMMUIO = (new LocalMMUIO)
        val ASLocalMMUIO = (new LocalMMUIO)
        val accLoader0Memory = (new LocalMMUIO)
        val accStorer0Memory = (new LocalMMUIO)
        val trStorer0Memory = new LocalMMUIO
        val LastLevelCacheTLIO = Flipped(new MMU2TLIO)
        val perfProbe = Output(new LocalMMUPerfProbe)
        val idle = Output(Bool())
    })

    val logTime = RegInit(0.U(64.W))
    logTime := logTime + 1.U

    // Process up to eight memory-request channels in parallel.
    // Each physical channel handles its selected request independently.

    // Route requests and responses for the TR, ACC and optional scale memory clients.
    val abcss = Seq(io.trLoader0Memory, io.trLoader1Memory, io.accLoader0Memory, io.accStorer0Memory, io.ASLocalMMUIO, io.BSLocalMMUIO, io.trStorer0Memory)

    // Initialize all channels to false.B
    for (mmuio <- abcss; i <- 0 until TrMatrixRegNBanks) {
        mmuio.Request(i).ready := false.B
        mmuio.Response(i).valid := false.B
        mmuio.Response(i).bits := 0.U.asTypeOf(mmuio.Response(i).bits)
        mmuio.ConherentRequsetSourceID.valid := false.B
        mmuio.ConherentRequsetSourceID.bits := 0.U
        mmuio.nonConherentRequsetSourceID.valid := false.B
        mmuio.nonConherentRequsetSourceID.bits := 0.U
    }

    // Initialize LLC IO
    for (i <- 0 until TrMatrixRegNBanks) {
        io.LastLevelCacheTLIO.Request(i).valid := false.B
        io.LastLevelCacheTLIO.Request(i).bits := 0.U.asTypeOf(io.LastLevelCacheTLIO.Request(i).bits)
        // Default low; assert only for the selected memory client.
        io.LastLevelCacheTLIO.Request(i).bits.isA := false.B
        io.LastLevelCacheTLIO.Response(i).ready := false.B
    }

    // Low three bits identify the request source.
    // encodedId = (origId << 3) | sourceTag
    val SourceTagWidth = 3
    val TrLoad0Tag  = 0.U(SourceTagWidth.W) // TR loader 0 reads.
    val TrLoad1Tag  = 1.U(SourceTagWidth.W) // TR loader 1 reads.
    val AccLoad0Tag  = 2.U(SourceTagWidth.W) // ACC loader 0 reads; keep this tag consistent with CUTE2YGJK.
    val AccStore0Tag = 3.U(SourceTagWidth.W) // ACC storer 0 writes; keep this tag consistent with CUTE2YGJK.
    val ASReadTag = 4.U(SourceTagWidth.W) // A-side scale reads use their own source tag.
    val BSReadTag = 5.U(SourceTagWidth.W) // B-side scale reads use their own source tag.

    val TrStore0Tag = 6.U(SourceTagWidth.W)

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
    io.idle := !allocatedBusy.asUInt.orR &&
        !abcss.flatMap(m => m.Request.map(_.valid) ++ m.Response.map(_.valid)).reduce(_ || _) &&
        !io.LastLevelCacheTLIO.Request.map(_.valid).reduce(_ || _) &&
        !io.LastLevelCacheTLIO.Response.map(_.valid).reduce(_ || _)
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
        laneCount = TrMatrixRegNBanks,
        bankCount = TrMatrixRegNBanks,
        aResponseChannels = TrLoader0ResponseChannelCount,
        bResponseChannels = TrLoader1ResponseChannelCount,
        cLoadResponseChannels = AccLoader0BridgeResponseChannelCount,
        cStoreResponseChannels = AccStorer0BridgeResponseChannelCount,
        aLegacy = TrLoader0UseLegacyLoader,
        bLegacy = TrLoader1UseLegacyLoader,
        cLoadLegacy = AccLoader0UseLegacyLoader,
        cStoreLegacy = AccStorer0UseLegacyLoader || AccStorer0DirectAckCountMode
    ))

    val requestSources = Seq(
        io.accLoader0Memory.Request,
        io.accStorer0Memory.Request,
        io.trLoader0Memory.Request,
        io.ASLocalMMUIO.Request,
        io.trLoader1Memory.Request,
        io.BSLocalMMUIO.Request,
        io.trStorer0Memory.Request
    )
    val sourceTags = Seq(AccLoad0Tag, AccStore0Tag, TrLoad0Tag, ASReadTag, TrLoad1Tag, BSReadTag, TrStore0Tag)
    val routeDescriptors = Seq(
        AccLoader0RouteDescriptor,
        AccStorer0RouteDescriptor,
        TrLoader0RouteDescriptor,
        TrLoader0RouteDescriptor,
        TrLoader1RouteDescriptor,
        TrLoader1RouteDescriptor,
        TrLoader0RouteDescriptor
    )
    val sourceIsWrite = Seq(false, true, false, false, false, false, true)
    val sourceIsAcc = Seq(true, true, false, false, false, false, false)
    val sourceIsA = Seq(false, false, true, false, false, false, true)

    // The combinational scheduler consumes this ID on the same fire as the loader.
    // No unaccepted request retains an earlier allocation across backpressure.
    for (mmuio <- abcss) {
        mmuio.ConherentRequsetSourceID.valid := !allocFull
        mmuio.ConherentRequsetSourceID.bits := allocatedId
        mmuio.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID
    }

    for (source <- 0 until LocalMMUSourceClass.Count; i <- 0 until TrMatrixRegNBanks) {
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

    for (i <- 0 until TrMatrixRegNBanks) {
        scheduler.io.llc(i).ready := io.LastLevelCacheTLIO.Request(i).ready
        io.LastLevelCacheTLIO.Request(i).valid := scheduler.io.llc(i).valid
        io.LastLevelCacheTLIO.Request(i).bits := scheduler.io.llc(i).bits


    }

    // Route parallel responses using the low source-tag bits of encodedId.
    for (i <- 0 until TrMatrixRegNBanks) {
        val llc_response = io.LastLevelCacheTLIO.Response(i)
        val a_mmu_response = io.trLoader0Memory.Response(i)
        val b_mmu_response = io.trLoader1Memory.Response(i)
        val c_mmu_response = io.accLoader0Memory.Response(i)
        val c_store_mmu_response = io.accStorer0Memory.Response(i)
        val as_mmu_response = io.ASLocalMMUIO.Response(i)
        val bs_mmu_response = io.BSLocalMMUIO.Response(i)

        val encodedId = llc_response.bits.ReseponseSourceID
        val tag = encodedId(SourceTagWidth - 1, 0)
        val origId = Wire(UInt(64.W))
        origId := encodedId >> SourceTagWidth
        val loaderSourceId = Wire(UInt(64.W))
        loaderSourceId := restoreLoaderSourceId(origId)
        val data = llc_response.bits.ReseponseData

        // Default ready low; override it for the selected response destination.
        llc_response.ready := false.B

        // Response tags identify physical FUs, independently of the A/B opcode.
        when(llc_response.valid) {
            when(tag === TrLoad0Tag) {
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
            }.elsewhen(tag === TrLoad1Tag) {
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
            }.elsewhen(tag === AccLoad0Tag) {
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
            }.elsewhen(tag === AccStore0Tag) {
                c_store_mmu_response.valid := true.B
                c_store_mmu_response.bits := llc_response.bits
                // Store acknowledgements carry no data. The shared response data
                // bus may change while an ACK is stalled, so do not forward it
                // into the response bridge's queued payload or stability checks.
                c_store_mmu_response.bits.ReseponseData := 0.U
                c_store_mmu_response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := c_store_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
            }.elsewhen(tag === TrStore0Tag) {
                val response = io.trStorer0Memory.Response(i)
                response.valid := true.B
                response.bits := llc_response.bits
                response.bits.ReseponseData := 0.U
                response.bits.ReseponseSourceID := loaderSourceId
                llc_response.ready := response.ready
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
    val outReqRdFire = VecInit((0 until TrMatrixRegNBanks).map(i => outReqFire(i) && !outReqIsWr(i)))
    val outReqWrFire = VecInit((0 until TrMatrixRegNBanks).map(i => outReqFire(i) && outReqIsWr(i)))

    io.perfProbe.rdReq := outReqRdFire.asUInt.orR
    io.perfProbe.wrReq := outReqWrFire.asUInt.orR
    io.perfProbe.rd32BReq := (0 until TrMatrixRegNBanks).map(i => Mux(outReqRdFire(i), outReqMask32B(i), 0.U)).reduce(_ +& _)
    io.perfProbe.wr32BReq := (0 until TrMatrixRegNBanks).map(i => Mux(outReqWrFire(i), outReqMask32B(i), 0.U)).reduce(_ +& _)
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

    XSPerfAccumulate("CUTE_MMU_A_rd_request", countRequestFire(io.trLoader0Memory.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_A_wr_request", countRequestFire(io.trLoader0Memory.Request, req => req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_B_rd_request", countRequestFire(io.trLoader1Memory.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_B_wr_request", countRequestFire(io.trLoader1Memory.Request, req => req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_CLoad_request", countAllRequestFire(io.accLoader0Memory.Request))
    XSPerfAccumulate("CUTE_MMU_CStore_request", countAllRequestFire(io.accStorer0Memory.Request))
    XSPerfAccumulate("CUTE_MMU_AS_rd_request", countRequestFire(io.ASLocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
    XSPerfAccumulate("CUTE_MMU_BS_rd_request", countRequestFire(io.BSLocalMMUIO.Request, req => !req.bits.RequestType_isWrite))
}
