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

    //比较低的性能方式，轮询的方式，但是doublebuffer是可以的
    //访存流的设计，可以通过设计一些标志位来实现～！
    //设计当前访存流处于哪种优先级。通过一些参数可以控制这个访存流。

    val FirstRequestIndex = RegInit(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))//只有3个请求来源，2位就够用了
    FirstRequestIndex := WrapInc(FirstRequestIndex, LocalMMUTaskType.TaskTypeMax)
    // //printf(p"FirstRequestIndex ${FirstRequestIndex}\n")
    //选择一个离FirstRequestIndex最近的请求
    val FirstIndex = FirstRequestIndex
    val SecIndex = WrapInc(FirstRequestIndex, LocalMMUTaskType.TaskTypeMax)
    val ThirdIndex = WrapInc(SecIndex, LocalMMUTaskType.TaskTypeMax)
    // val ForthIndex = WrapInc(ThirdIndex, LocalMMUTaskType.TaskTypeMax)

    //假设目前只有一个LLC的访存端口。所以只能选择一个LLC的访存请求，进行服务。
    //循环服务和连续顺序服务，要考虑Cache连续读和Memory连续读的性能啊！！
    //如果这样循环发出请求，可能会导致访存性能下降了，尤其是Memory，他是有bank切换和line切换的代价的！！
    //这里先写一个循环的，后面再修改成局部连续的
    val AllRequestValid = Cat(io.CLocalMMUIO.Request.valid, io.BLocalMMUIO.Request.valid, io.ALocalMMUIO.Request.valid)
    val HasRequest = AllRequestValid.orR
    val ChoseIndex_0 = Mux(AllRequestValid(FirstIndex), FirstIndex,
                        Mux(AllRequestValid(SecIndex), SecIndex,
                        Mux(AllRequestValid(ThirdIndex), ThirdIndex,LocalMMUTaskType.TaskTypeMax.U)))
    
    //如果是AFirst，就服务A，如果是B，就服务B，如果是C，就服务C

    //这里的设计是，只有一个LLC的访存端口，所以只能选择一个访存请求，进行服务。
    //如果有多个访存端口，就可以同时服务多个访存请求。

    io.ALocalMMUIO.Request.ready := false.B
    io.BLocalMMUIO.Request.ready := false.B
    io.CLocalMMUIO.Request.ready := false.B
    io.ALocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.BLocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.CLocalMMUIO.ConherentRequsetSourceID.valid := false.B
    io.ALocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.BLocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.CLocalMMUIO.ConherentRequsetSourceID.bits := DontCare
    io.ALocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.BLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.CLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.ALocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    io.BLocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    io.CLocalMMUIO.nonConherentRequsetSourceID.bits := DontCare
    // io.DLocalMMUIO.Request.ready := false.B
    //如果sourceid是valid，则LLC可以接受这个请求，开始送入到LLC的访存端口
    //这里得到谁先服务，送入LLC的访存端口，如果这里需要切流水也简单,提前锁定sourceid即可，将TLnode内的sourceid锁定的逻辑放到这里来写
    // val sourceid2port = VecInit(Seq.fill(LLCSourceMaxNum)(RegInit(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))))
    val sourceid2port = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))))
    //输出一下sourceid2port的数据类型
    println("[LocalMMU] sourceid2port: " + sourceid2port)


    io.LastLevelCacheTLIO.Request.valid := false.B
    io.LastLevelCacheTLIO.Request.bits := DontCare
    io.LastLevelCacheTLIO.Response.ready := false.B
    // //输出ABC的信息和valid和hasrequest
    // printf(p"ALocalMMUIO ${io.ALocalMMUIO.Request.bits} request_valid ${io.ALocalMMUIO.Request.valid} ${io.ALocalMMUIO.Request.ready} ${io.ALocalMMUIO.Response}\n")
    // printf(p"BLocalMMUIO ${io.BLocalMMUIO.Request.bits} request_valid ${io.BLocalMMUIO.Request.valid} ${io.BLocalMMUIO.Request.ready} ${io.BLocalMMUIO.Response}\n")
    // printf(p"CLocalMMUIO ${io.CLocalMMUIO.Request.bits} request_valid ${io.CLocalMMUIO.Request.valid} ${io.CLocalMMUIO.Request.ready} ${io.CLocalMMUIO.Response}\n")
    // //输出io.LastLevelCacheTLIO.ConherentRequsetSourceID
    // printf(p"ConherentRequsetSourceID ${io.LastLevelCacheTLIO.ConherentRequsetSourceID}\n")
    // printf(p"HasRequest ${HasRequest}\n")
    // printf(p"ChoseIndex_0 ${ChoseIndex_0}\n")
    // val last_sourceid = RegInit(0.U(LLCSourceMaxNumBitSize.W))
    

    //如果HasRequest，输出其他两个信息
    when(HasRequest)
    {
        //输出io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid
        //输出io.LastLevelCacheTLIO.Request.ready
        // printf(p"[localmmu]io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid ${io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid} io.LastLevelCacheTLIO.Request.ready ${io.LastLevelCacheTLIO.Request.ready}\n")
    }

    when(io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid && HasRequest)
    {
        // printf(p"last_sourceid ${last_sourceid} last_sourceid2port ${sourceid2port(last_sourceid)}\n")
        // last_sourceid := io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits
        switch(ChoseIndex_0) {
            is(LocalMMUTaskType.AFirst) {
                io.ALocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.ALocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.ALocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := false.B
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.AFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B // A matrix is tile matrix register
            }
            is(LocalMMUTaskType.BFirst) {
                io.BLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.BLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.BLocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := false.B
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.BFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := false.B // B matrix is tile matrix register
            }
            is(LocalMMUTaskType.CFirst) {
                io.CLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
                io.CLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
                io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := io.CLocalMMUIO.Request.bits.RequestVirtualAddr
                io.LastLevelCacheTLIO.Request.bits.RequestData := io.CLocalMMUIO.Request.bits.RequestData
                io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := io.CLocalMMUIO.Request.bits.RequestType_isWrite
                sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.CFirst
                io.LastLevelCacheTLIO.Request.bits.MatrixIsAcc := true.B // C matrix is accumulation matrix register
            }
        }

        // TODO: Support Request Mask
        io.LastLevelCacheTLIO.Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LastLevelCacheTLIO.Request.bits.RequestConherent := true.B
        io.LastLevelCacheTLIO.Request.bits.RequestSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits
        io.LastLevelCacheTLIO.Request.valid := true.B
    }

    io.ALocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.BLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.CLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.ALocalMMUIO.Response.valid := false.B
    io.BLocalMMUIO.Response.valid := false.B
    io.CLocalMMUIO.Response.valid := false.B

    switch(sourceid2port(io.LastLevelCacheTLIO.Response.bits.ReseponseSourceID)) {
        is(LocalMMUTaskType.AFirst) {
            io.ALocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.ALocalMMUIO.Response.ready
        }
        is(LocalMMUTaskType.BFirst) {
            io.BLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.BLocalMMUIO.Response.ready
        }
        is(LocalMMUTaskType.CFirst) {
            io.CLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
            io.LastLevelCacheTLIO.Response.ready := io.CLocalMMUIO.Response.ready
        }
    }

    //输出每次的请求
    if (YJPDebugEnable)
    {
        val AML_Read_Request_times = RegInit(0.U(64.W))
        val AML_Write_Request_times = RegInit(0.U(64.W))
        val BML_Read_Request_times = RegInit(0.U(64.W))
        val BML_Write_Request_times = RegInit(0.U(64.W))
        val CML_Read_Request_times = RegInit(0.U(64.W))
        val CML_Write_Request_times = RegInit(0.U(64.W))

        when(io.ALocalMMUIO.Request.fire) {
            when(io.ALocalMMUIO.Request.bits.RequestType_isWrite) {
                AML_Write_Request_times := AML_Write_Request_times + 1.U
            }.otherwise {
                AML_Read_Request_times := AML_Read_Request_times + 1.U
            }
        }

        when(io.BLocalMMUIO.Request.fire) {
            when(io.BLocalMMUIO.Request.bits.RequestType_isWrite) {
                BML_Write_Request_times := BML_Write_Request_times + 1.U
            }.otherwise {
                BML_Read_Request_times := BML_Read_Request_times + 1.U
            }
        }

        when(io.CLocalMMUIO.Request.fire) {
            when(io.CLocalMMUIO.Request.bits.RequestType_isWrite) {
                CML_Write_Request_times := CML_Write_Request_times + 1.U
            }.otherwise {
                CML_Read_Request_times := CML_Read_Request_times + 1.U
            }
        }

        //每次请求发出时，输出一下
        when(io.ALocalMMUIO.Request.fire || io.BLocalMMUIO.Request.fire || io.CLocalMMUIO.Request.fire)
        {
            printf(p"[LocalMMU] AML_Read_Request_times ${AML_Read_Request_times} AML_Write_Request_times ${AML_Write_Request_times} BML_Read_Request_times ${BML_Read_Request_times} BML_Write_Request_times ${BML_Write_Request_times} CML_Read_Request_times ${CML_Read_Request_times} CML_Write_Request_times ${CML_Write_Request_times}\n")
        }
    }
}
