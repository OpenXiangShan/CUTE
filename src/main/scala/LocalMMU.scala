package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class LocalMMU()(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ALocalMMUIO = (new LocalMMUIO)
        val BLocalMMUIO = (new LocalMMUIO)
        val CLocalMMUIO = (new LocalMMUIO)
        val LastLevelCacheTLIO = Flipped(new MMU2TLIO)
    })

    //8通道并行访存：同时处理所有8个通道的请求
    //每个通道独立处理，不需要仲裁

    // 统一处理 ABC 三类 MMUIO 的 Request/Response
    val abcs = Seq(io.ALocalMMUIO, io.BLocalMMUIO, io.CLocalMMUIO)
    
    // Initialize all channels to false.B
    for (mmuio <- abcs; i <- 0 until ABMatrixRegNBanks) {
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
    val SourceTagWidth = 2
    val AReadTag  = 0.U(SourceTagWidth.W) // A 读
    val BReadTag  = 1.U(SourceTagWidth.W) // B 读
    val CReadTag  = 2.U(SourceTagWidth.W) // C 读（与 CUTE2YGJK 中 CReadTag 保持一致）
    val CWriteTag = 3.U(SourceTagWidth.W) // C 写（与 CUTE2YGJK 中 CWriteTag 保持一致）

    def encodeSourceId(origId: UInt, tag: UInt): UInt = {
        (origId << SourceTagWidth) | tag
    }

    //8通道并行请求处理：为每个通道选择优先级最高的矩阵请求
    //优先级：A读 > B读 > C读 > C写
    for (i <- 0 until ABMatrixRegNBanks) {
        val aReq = io.ALocalMMUIO.Request(i)
        val bReq = io.BLocalMMUIO.Request(i)
        val cReq = io.CLocalMMUIO.Request(i)

        // A/B 目前只有读请求，但这里仍按读写拆分，便于扩展
        val aReadValid = aReq.valid && !aReq.bits.RequestType_isWrite
        val bReadValid = bReq.valid && !bReq.bits.RequestType_isWrite
        val cReadValid = cReq.valid && !cReq.bits.RequestType_isWrite
        val cWriteValid = cReq.valid && cReq.bits.RequestType_isWrite

        val aValid = aReadValid
        val bValid = bReadValid
        val cValid = cReadValid || cWriteValid

        // 优先级调整：C读 > A读 > B读 > C写
        val choseMatrix = Mux(cReadValid,  LocalMMUTaskType.CFirst,
                          Mux(aValid,      LocalMMUTaskType.AFirst,
                          Mux(bValid,      LocalMMUTaskType.BFirst,
                          Mux(cWriteValid, LocalMMUTaskType.CFirst,
                                           LocalMMUTaskType.TaskTypeMax.U))))

        val llcReq = io.LastLevelCacheTLIO.Request(i)

        when(choseMatrix === LocalMMUTaskType.AFirst) {
            // A：8 通道并行读，来源标签 AReadTag
            aReq.ready := llcReq.ready
            // T7: 修复 - AML 通道应该发送到对应的 TL 通道，而不是都发送到通道 0
            llcReq.valid := true.B
            llcReq.bits := aReq.bits
            llcReq.bits.MatrixIsAcc := false.B // A matrix is tile matrix register
            llcReq.bits.isA := true.B

            val origId = aReq.bits.RequestSourceID
            llcReq.bits.RequestSourceID := encodeSourceId(origId, AReadTag)

            // A 实际不会用这个 ID 分配，但保持接线一致
            io.ALocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID

            when(llcReq.fire) {
                if (YJPDebugEnable) {
                    printf(cf"[LocalMMU] Channel[$i] AReq fire: origId=${origId}, encoded=${llcReq.bits.RequestSourceID}\n")
                }
            }
        }.elsewhen(choseMatrix === LocalMMUTaskType.BFirst) {
            // B：8 通道并行读，来源标签 BReadTag
            bReq.ready := llcReq.ready
            llcReq.valid := true.B
            llcReq.bits := bReq.bits
            llcReq.bits.MatrixIsAcc := false.B // B matrix is tile matrix register
            llcReq.bits.isA := false.B

            val origId = bReq.bits.RequestSourceID
            llcReq.bits.RequestSourceID := encodeSourceId(origId, BReadTag)

            io.BLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID

            when(llcReq.fire) {
                if (YJPDebugEnable) {
                    printf(cf"[LocalMMU] Channel[$i] BReq fire: origId=${origId}, encoded=${llcReq.bits.RequestSourceID}\n")
                }
            }
        }.elsewhen(choseMatrix === LocalMMUTaskType.CFirst) {
            // C：单通道（C MemoryLoader 只使用 Request(0)），但从 LLC 角度看仍是按 bank 口号分散
            cReq.ready := llcReq.ready
            llcReq.valid := true.B
            llcReq.bits := cReq.bits
            llcReq.bits.MatrixIsAcc := true.B // C matrix is accumulation matrix register
            llcReq.bits.isA := false.B

            val origId = cReq.bits.RequestSourceID // = Cute2TL 分配的 id
            val isWrite = cReq.bits.RequestType_isWrite
            val tag = Mux(isWrite, CWriteTag, CReadTag)
            llcReq.bits.RequestSourceID := encodeSourceId(origId, tag)

            io.CLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID

            when(llcReq.fire) {
                if (YJPDebugEnable) {
                    printf(cf"[LocalMMU] Channel[$i] CReq fire: origId=${origId}, conherentId=${io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits}(valid=${io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid}), encoded=${llcReq.bits.RequestSourceID}, isWrite=${isWrite}\n")
                }
            }
        }

        // 握手监控日志：请求侧
        when(io.LastLevelCacheTLIO.Request(i).fire) {
            printf(cf"[LocalMMU] Channel[$i] Request fired! encoded SourceID: ${io.LastLevelCacheTLIO.Request(i).bits.RequestSourceID}, Addr: ${Hexadecimal(io.LastLevelCacheTLIO.Request(i).bits.RequestAddr)}\n")
        }
    }

    // C 通道单口响应仲裁：基于低位标签路由

    //8通道并行响应处理：按 encodedId 低位标签路由到对应的 MMU IO
    for (i <- 0 until ABMatrixRegNBanks) {
        val llc_response = io.LastLevelCacheTLIO.Response(i)
        val a_mmu_response = io.ALocalMMUIO.Response(i)
        val b_mmu_response = io.BLocalMMUIO.Response(i)
        val c_mmu_response = io.CLocalMMUIO.Response(i)

        val encodedId = llc_response.bits.ReseponseSourceID
        val tag = encodedId(SourceTagWidth - 1, 0)
        val origId = encodedId >> SourceTagWidth
        val data = llc_response.bits.ReseponseData

        // 默认保持 ready 低电平，后续根据路由目的端覆盖
        llc_response.ready := false.B

        when(llc_response.valid) {
            when(tag === AReadTag) {
                a_mmu_response.valid := true.B
                a_mmu_response.bits := llc_response.bits
                // 上游 AMemoryLoader 看到的是未经编码的 origId（一般为 bank 内地址）
                a_mmu_response.bits.ReseponseSourceID := origId
                llc_response.ready := a_mmu_response.ready
            }.elsewhen(tag === BReadTag) {
                b_mmu_response.valid := true.B
                b_mmu_response.bits := llc_response.bits
                b_mmu_response.bits.ReseponseSourceID := origId
                llc_response.ready := b_mmu_response.ready
            }.elsewhen(tag === CReadTag || tag === CWriteTag) {
                c_mmu_response.valid := true.B
                c_mmu_response.bits := llc_response.bits
                c_mmu_response.bits.ReseponseSourceID := origId
                llc_response.ready := c_mmu_response.ready
            }
        }

        // 握手监控日志：响应侧
        when(io.LastLevelCacheTLIO.Response(i).fire) {
            printf(cf"[LocalMMU] Channel[$i] Response fired! encoded SourceID: ${io.LastLevelCacheTLIO.Response(i).bits.ReseponseSourceID}, origId: ${origId}, tag: ${tag}, data: ${data}%x\n")
        }
    }

    
    //输出每次的请求统计（原逻辑保持不变）
    if (YJPDebugEnable)
    {
        val AML_Read_Request_times = RegInit(0.U(64.W))
        val AML_Write_Request_times = RegInit(0.U(64.W))
        val BML_Read_Request_times = RegInit(0.U(64.W))
        val BML_Write_Request_times = RegInit(0.U(64.W))
        val CML_Read_Request_times = RegInit(0.U(64.W))
        val CML_Write_Request_times = RegInit(0.U(64.W))

        // 统计所有通道的请求
        for (i <- 0 until ABMatrixRegNBanks) {
            when(io.ALocalMMUIO.Request(i).valid && io.ALocalMMUIO.Request(i).ready)
            {
                when(io.ALocalMMUIO.Request(i).bits.RequestType_isWrite)
                {
                    AML_Write_Request_times := AML_Write_Request_times + 1.U
                }.otherwise{
                    AML_Read_Request_times := AML_Read_Request_times + 1.U
                }
            }

            when(io.BLocalMMUIO.Request(i).valid && io.BLocalMMUIO.Request(i).ready)
            {
                when(io.BLocalMMUIO.Request(i).bits.RequestType_isWrite)
                {
                    BML_Write_Request_times := BML_Write_Request_times + 1.U
                }.otherwise{
                    BML_Read_Request_times := BML_Read_Request_times + 1.U
                }
            }

            when(io.CLocalMMUIO.Request(i).valid && io.CLocalMMUIO.Request(i).ready)
            {
                when(io.CLocalMMUIO.Request(i).bits.RequestType_isWrite)
                {
                    CML_Write_Request_times := CML_Write_Request_times + 1.U
                }.otherwise{
                    CML_Read_Request_times := CML_Read_Request_times + 1.U
                }
            }
        }

        //每次请求发出时，输出统计
        val anyFire = (0 until ABMatrixRegNBanks).map(i =>
            io.ALocalMMUIO.Request(i).fire ||
            io.BLocalMMUIO.Request(i).fire ||
            io.CLocalMMUIO.Request(i).fire
        ).reduce(_ || _)
        when(anyFire)
        {
            printf(p"[LocalMMU] Stats: AML_Read=${AML_Read_Request_times} AML_Write=${AML_Write_Request_times} BML_Read=${BML_Read_Request_times} BML_Write=${BML_Write_Request_times} CML_Read=${CML_Read_Request_times} CML_Write=${CML_Write_Request_times}\n")
        }
    }
}
