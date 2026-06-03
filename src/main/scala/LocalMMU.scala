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
    })

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

    // SourceId 编码配置：低 2bit 作为来源标签
    // encodedId = (origId << 2) | sourceTag
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
        Cat(1.U(1.W), 0.U((AllocatedOrigIdFlagBit - LLCSourceMaxNumBitSize).W), id)
    }
    def isAllocatedOrigId(id: UInt): Bool = id(AllocatedOrigIdFlagBit)
    def restoreLoaderSourceId(id: UInt): UInt = {
        Mux(
            isAllocatedOrigId(id),
            Cat(0.U((64 - LLCSourceMaxNumBitSize).W), id(LLCSourceMaxNumBitSize - 1, 0)),
            id
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

    //8通道并行请求处理：为每个通道选择优先级最高的矩阵请求
    //优先级：A读 > AS读 > B读 > BS读 > C读 > C写
    for (i <- 0 until ABMatrixRegNBanks) {
        val aReq = io.ALocalMMUIO.Request(i)
        val bReq = io.BLocalMMUIO.Request(i)
        val cLoadReq = io.CLoadLocalMMUIO.Request(i)
        val cStoreReq = io.CStoreLocalMMUIO.Request(i)
        val asReq = io.ASLocalMMUIO.Request(i)
        val bsReq = io.BSLocalMMUIO.Request(i)

        // A/B/AS/BS 目前只有读请求，C有读写
        def reqCanIssue(req: DecoupledIO[MMURequestIO]): Bool = !req.bits.UseAllocatedSourceID || !allocFull

        val aReadValid = aReq.valid && !aReq.bits.RequestType_isWrite && reqCanIssue(aReq)
        val asReadValid = asReq.valid && !asReq.bits.RequestType_isWrite && reqCanIssue(asReq)
        val bReadValid = bReq.valid && !bReq.bits.RequestType_isWrite && reqCanIssue(bReq)
        val bsReadValid = bsReq.valid && !bsReq.bits.RequestType_isWrite && reqCanIssue(bsReq)
        val cReadValid = cLoadReq.valid && !cLoadReq.bits.RequestType_isWrite && reqCanIssue(cLoadReq)
        val cWriteValid = cStoreReq.valid && cStoreReq.bits.RequestType_isWrite && reqCanIssue(cStoreReq)

        // 优先级：A读 > AS读 > B读 > BS读 > C读 > C写
        val choseMatrix = Mux(aReadValid,     LocalMMUTaskType.AFirst,
                  Mux(asReadValid,    LocalMMUTaskType.AScaleFirst,
                  Mux(bReadValid,     LocalMMUTaskType.BFirst,
                  Mux(bsReadValid,    LocalMMUTaskType.BScaleFirst,
                  Mux(cReadValid,     LocalMMUTaskType.CLoadFirst,
                  Mux(cWriteValid,    LocalMMUTaskType.CStoreFirst,
                              LocalMMUTaskType.TaskTypeMax.U))))))

        val llcReq = io.LastLevelCacheTLIO.Request(i)

        switch(choseMatrix) {
            is(LocalMMUTaskType.AFirst) {
                // A：8 通道并行读，来源标签 AReadTag
                val useAllocId = aReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = aReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, AReadTag)

                aReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := aReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := aReq.bits
                llcReq.bits.MatrixIsAcc := false.B // A matrix is tile matrix register
                llcReq.bits.isA := true.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.ALocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.ALocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.ALocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := AReadTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] AReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
            is(LocalMMUTaskType.AScaleFirst) {
                // AS：8 通道并行读
                val useAllocId = asReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = asReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, ASReadTag)

                asReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := asReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := asReq.bits
                llcReq.bits.MatrixIsAcc := false.B
                llcReq.bits.isA := false.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.ASLocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.ASLocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.ASLocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := ASReadTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] ASReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
            is(LocalMMUTaskType.BFirst) {
                // B：8 通道并行读，来源标签 BReadTag
                val useAllocId = bReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = bReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, BReadTag)

                bReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := bReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := bReq.bits
                llcReq.bits.MatrixIsAcc := false.B // B matrix is tile matrix register
                llcReq.bits.isA := false.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.BLocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.BLocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.BLocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := BReadTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] BReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
            is(LocalMMUTaskType.BScaleFirst) {
                // BS：8 通道并行读
                val useAllocId = bsReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = bsReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, BSReadTag)

                bsReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := bsReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := bsReq.bits
                llcReq.bits.MatrixIsAcc := false.B
                llcReq.bits.isA := false.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.BSLocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.BSLocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.BSLocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := BSReadTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] BSReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
            is(LocalMMUTaskType.CLoadFirst) {
                // C Load：来源标签 CReadTag
                val useAllocId = cLoadReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = cLoadReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, CReadTag)

                cLoadReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := cLoadReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := cLoadReq.bits
                llcReq.bits.MatrixIsAcc := true.B // C matrix is accumulation matrix register
                llcReq.bits.isA := false.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.CLoadLocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.CLoadLocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.CLoadLocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := CReadTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] CLoadReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
            is(LocalMMUTaskType.CStoreFirst) {
                // C Store：来源标签 CWriteTag
                val useAllocId = cStoreReq.bits.UseAllocatedSourceID
                val reqLoaderOrigId = cStoreReq.bits.RequestSourceID
                val reqLlcOrigId = Mux(useAllocId, markAllocatedOrigId(allocatedId), reqLoaderOrigId)
                val reqSourceId = encodeSourceId(reqLlcOrigId, CWriteTag)

                cStoreReq.ready := llcReq.ready && (!useAllocId || !allocFull)
                llcReq.valid := cStoreReq.valid && (!useAllocId || !allocFull)
                llcReq.bits := cStoreReq.bits
                llcReq.bits.MatrixIsAcc := true.B // C matrix is accumulation matrix register
                llcReq.bits.isA := false.B
                llcReq.bits.RequestSourceID := reqSourceId
                io.CStoreLocalMMUIO.ConherentRequsetSourceID.valid := !allocFull
                io.CStoreLocalMMUIO.ConherentRequsetSourceID.bits := allocatedId
                io.CStoreLocalMMUIO.nonConherentRequsetSourceID := io.LastLevelCacheTLIO.nonConherentRequsetSourceID

                when(llcReq.fire && useAllocId) {
                    allocatedBusy(allocatedId) := true.B
                    allocatedTag(allocatedId) := CWriteTag
                }

                when(llcReq.fire) {
                    if (YJPDebugEnable) {
                        printf(cf"[LocalMMU] Channel[$i] CStoreReq fire: origId=${reqLoaderOrigId}, useAlloc=${useAllocId}, encoded=${llcReq.bits.RequestSourceID}\n")
                    }
                }
            }
        }

        // 握手监控日志：请求侧
        when(io.LastLevelCacheTLIO.Request(i).fire) {
            printf(cf"[LocalMMU] Channel[$i] Request fired! encoded SourceID: ${io.LastLevelCacheTLIO.Request(i).bits.RequestSourceID}, Addr: ${Hexadecimal(io.LastLevelCacheTLIO.Request(i).bits.RequestAddr)}\n")
        }
    }

    //8通道并行响应处理：按 encodedId 低位标签路由到对应的 MMU IO
    // 握手监控日志：响应侧
    def logResponse(channel: Int, tagStr: String, tag: UInt, origId: UInt, data: UInt): Unit = {
        val resp = io.LastLevelCacheTLIO.Response(channel)
         when(resp.fire) {
            printf(cf"[LocalMMU] Channel[$channel] to ${tagStr}ML Response fired! encodedId: ${resp.bits.ReseponseSourceID} tag: ${tag}, origId: ${origId}, data: ${Hexadecimal(data)}\n")
        }
    }

    for (i <- 0 until ABMatrixRegNBanks) {
        val llc_response = io.LastLevelCacheTLIO.Response(i)
        val a_mmu_response = io.ALocalMMUIO.Response(i)
        val b_mmu_response = io.BLocalMMUIO.Response(i)
        val c_mmu_response = io.CLoadLocalMMUIO.Response(i)
        val as_mmu_response = io.ASLocalMMUIO.Response(i)
        val bs_mmu_response = io.BSLocalMMUIO.Response(i)

        val encodedId = llc_response.bits.ReseponseSourceID
        val tag = encodedId(SourceTagWidth - 1, 0)
        val origId = encodedId >> SourceTagWidth
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
                a_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := a_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "A", tag, origId, data)
            }.elsewhen(tag === BReadTag) {
                b_mmu_response.valid := true.B
                b_mmu_response.bits := llc_response.bits
                b_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := b_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "B", tag, origId, data)
            }.elsewhen(tag === ASReadTag) {
                as_mmu_response.valid := true.B
                as_mmu_response.bits := llc_response.bits
                as_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := as_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "AS", tag, origId, data)
            }.elsewhen(tag === BSReadTag) {
                bs_mmu_response.valid := true.B
                bs_mmu_response.bits := llc_response.bits
                bs_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := bs_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "BS", tag, origId, data)
            }.elsewhen(tag === CReadTag) {
                c_mmu_response.valid := true.B
                c_mmu_response.bits := llc_response.bits
                c_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := c_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "CLoad", tag, origId, data)
            }.elsewhen(tag === CWriteTag) {
                val c_store_mmu_response = io.CStoreLocalMMUIO.Response(i)
                c_store_mmu_response.valid := true.B
                c_store_mmu_response.bits := llc_response.bits
                c_store_mmu_response.bits.ReseponseSourceID := restoreLoaderSourceId(origId)
                llc_response.ready := c_store_mmu_response.ready
                when(llc_response.fire) {
                    val allocIdx = origId(LLCSourceMaxNumBitSize - 1, 0)
                    when(isAllocatedOrigId(origId) && allocatedBusy(allocIdx) && allocatedTag(allocIdx) === tag) {
                        allocatedBusy(allocIdx) := false.B
                    }
                }
                logResponse(i, "CStore", tag, origId, data)
            }.otherwise{
                assert(false.B, cf"Unsupported tag ${tag} in LLC response routing")
            }
        }
    }


    //输出每次的请求统计（原逻辑保持不变）
    XSPerfAccumulate("CUTE_MMU_A_rd_request", io.ALocalMMUIO.Request.map(r => r.fire & !r.bits.RequestType_isWrite).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_A_wr_request", io.ALocalMMUIO.Request.map(r => r.fire & r.bits.RequestType_isWrite).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_B_rd_request", io.BLocalMMUIO.Request.map(r => r.fire & !r.bits.RequestType_isWrite).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_B_wr_request", io.BLocalMMUIO.Request.map(r => r.fire & r.bits.RequestType_isWrite).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_CLoad_request", io.CLoadLocalMMUIO.Request.map(r => r.fire).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_CStore_request", io.CStoreLocalMMUIO.Request.map(r => r.fire).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_AS_rd_request", io.ASLocalMMUIO.Request.map(r => r.fire & !r.bits.RequestType_isWrite).reduce(_ || _))
    XSPerfAccumulate("CUTE_MMU_BS_rd_request", io.BSLocalMMUIO.Request.map(r => r.fire & !r.bits.RequestType_isWrite).reduce(_ || _))
}
