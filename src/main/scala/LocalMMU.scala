
// package boom.exu.ygjk
package cute

import chisel3._
import chisel3.util._
// import boom.exu.ygjk._
import org.chipsalliance.cde.config._
import boom.v3.common._ 
import boom.v3.util._

trait LocalTLBParameters{
	val entry = 32
    //boom里支持对32个22位的cam进行一拍查询，这个是可以的,cacti和我说...这个可以20Ghz....22nm下，cam完全不是问题
    //这个cam的size还是有待商榷的，可以后面交给cacti和eda
    //另外，我们的大页支持，可以省很多cam的资源，可以省很多功耗
    //谈localTLB的功耗资源等，可以和gemmni直接进行比较！！！
    //加速器设计范式的探索～！～！～！～！
}

class MMUConfigIO() extends Bundle with HWParameters {
	val useVM_v = Input(Bool())
	val useVM = Input(Bool())
	val refill_v = Input(Bool())
	val refillVaddr = Input(UInt(vpnBits.W))
	val refillPaddr = Input(UInt(ppnBits.W))
}

class LocalTLBReq() extends Bundle with HWParameters{
	val vaddr0 = Input(UInt(vaddrBits.W))
	val vaddr0_v = Input(Bool())
	val vaddr1 = Input(UInt(vaddrBits.W))
	val vaddr1_v = Input(Bool())
}

class LocalTLBResp() extends Bundle with HWParameters{
	val paddr0 = Output(UInt(paddrBits.W))
	val paddr1 = Output(UInt(paddrBits.W))
	val paddr0_v = Output(Bool())
	val paddr1_v = Output(Bool())
	val miss = Output(Bool())
	val missAddr = Output(UInt(vaddrBits.W))

}

class LocalTLBIO() extends Bundle with HWParameters{
	val config = new MMUConfigIO
	val req  = new LocalTLBReq
	val resp = new LocalTLBResp
    val paddr0 = Output(UInt(corePAddrBits.W))
	val paddr1 = Output(UInt(corePAddrBits.W))
}

class LocalTLB() extends Module with LocalTLBParameters with HWParameters{
	val io = IO(new LocalTLBIO)

	val VTable = RegInit(VecInit(Seq.fill(entry)(0.U(vaddrBits.W))))
	val PTable = RegInit(VecInit(Seq.fill(entry)(0.U(paddrBits.W))))
	val VMusing = RegInit(false.B)

	io.paddr0 := PTable(0)
	io.paddr1 := PTable(1)


	//vaddr -> paddr
    //默认通路
    // println("[LocalTLB] paddrBits: " + paddrBits)
	io.resp.paddr0 := io.req.vaddr0(paddrBits - 1 , 0)
	io.resp.paddr1 := io.req.vaddr1(paddrBits - 1 , 0)

	val hit0 = Wire(Vec(entry,Bool()))
	val hit1 = Wire(Vec(entry,Bool()))
	for(i <- 0 until entry){
		hit0(i) := (io.req.vaddr0(vaddrBits-1, pgIdxBits) === VTable(i)) & io.req.vaddr0_v
		when((io.req.vaddr0(vaddrBits-1, pgIdxBits) === VTable(i)) & io.req.vaddr0_v & VMusing){
			io.resp.paddr0 := Cat(PTable(i)(19,0),io.req.vaddr0(pgIdxBits - 1 , 0))
		}

		hit1(i) := (io.req.vaddr1(vaddrBits-1, pgIdxBits) === VTable(i)) & io.req.vaddr1_v
		when((io.req.vaddr1(vaddrBits-1, pgIdxBits) === VTable(i)) & io.req.vaddr1_v & VMusing){
			io.resp.paddr1 := Cat(PTable(i)(19,0),io.req.vaddr1(pgIdxBits - 1 , 0))
		}
	}

	io.resp.miss := ((!hit0.reduce(_|_) & io.req.vaddr0_v) | (!hit1.reduce(_|_) & io.req.vaddr1_v)) & VMusing
	io.resp.paddr0_v := io.req.vaddr0_v & !io.resp.miss
	io.resp.paddr1_v := io.req.vaddr1_v & !io.resp.miss



	when(!hit0.reduce(_|_) & io.req.vaddr0_v & VMusing){
		io.resp.missAddr := io.req.vaddr0
	}.otherwise{
		io.resp.missAddr := io.req.vaddr1
	}

	when(io.resp.miss){
		//printf(p"io.resp.missAddr ${io.resp.missAddr}\n")
	}

	//CPU config Local MMU
	val count = RegInit(0.U(log2Ceil(entry).W))
 	when(io.config.useVM_v){
 		VMusing := io.config.useVM
 	}

    when(io.config.refill_v){
 		VTable(count) := io.config.refillVaddr
 		PTable(count) := io.config.refillPaddr
 		when(count === (entry -1).U){
 			count := 0.U
 		}.otherwise{
 			count := count + 1.U
 		}
 		//printf(p"io.core.refillVaddr ${io.config.refillVaddr} io.core.refillPaddr ${io.config.refillPaddr}\n")
 	}

}


class LocalMMU() extends Module with HWParameters{
	// val io = IO(new LocalTLBIO)
    val io = IO(new Bundle{
        val ALocalMMUIO = (new LocalMMUIO)
        val BLocalMMUIO = (new LocalMMUIO)
        val CLocalMMUIO = (new LocalMMUIO)
        // val DLocalMMUIO = new LocalMMUIO
        val Config = (new MMUConfigIO)
        val LastLevelCacheTLIO = Flipped(new MMU2TLIO)//改！TODO:
        // val DramReq = Input(UInt(64.W))//改！TODO:
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
    io.ALocalMMUIO.ConherentRequsetSourceID.bits := 0.U
    io.BLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
    io.CLocalMMUIO.ConherentRequsetSourceID.bits := 0.U
    io.ALocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    io.BLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    io.CLocalMMUIO.nonConherentRequsetSourceID.bits := 0.U
    io.ALocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.BLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.CLocalMMUIO.nonConherentRequsetSourceID.valid := false.B
    io.LastLevelCacheTLIO.Request.bits.RequestConherent := false.B
    io.LastLevelCacheTLIO.Request.bits.RequestSourceID := 0.U
    // io.DLocalMMUIO.Request.ready := false.B
    //如果sourceid是valid，则LLC可以接受这个请求，开始送入到LLC的访存端口
    //如果这里TLB查询要时间，一个周期做不完，这里就要有一个buffer，来存储这个请求，等TLB查询完了，再送入LLC的访存端口
    //TLB的查询也可以优化，直到数据跨页，才发出TLB查询请求，相对于每次都发出TLB查询请求，尤其是那种大页的情况下，不够看一下也没多少消耗对于tlb来说
    //可以留到操作系统相关的那篇论文时，再写这个优化。可以和Gemmini比，先裸机能跑
    //这里得到谁先服务，送入TLB，送入LLC的访存端口，如果这里需要切流水也简单,提前锁定sourceid即可，将TLnode内的sourceid锁定的逻辑放到这里来写
    // val sourceid2port = VecInit(Seq.fill(LLCSourceMaxNum)(RegInit(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))))
    val sourceid2port = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(0.U(log2Ceil(LocalMMUTaskType.TaskTypeMax).W))))
    //输出一下sourceid2port的数据类型
    println("[LocalMMU] sourceid2port: " + sourceid2port)

    val ltlb = Module(new LocalTLB)
    ltlb.io.config := io.Config
    ltlb.io.req.vaddr0 := 0.U
    ltlb.io.req.vaddr1 := 0.U
    ltlb.io.req.vaddr0_v := false.B
    ltlb.io.req.vaddr1_v := false.B

    io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := 0.U
    io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := false.B
    io.LastLevelCacheTLIO.Request.bits.RequestData := 0.U
    io.LastLevelCacheTLIO.Request.valid := false.B
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
        when(ChoseIndex_0 === LocalMMUTaskType.AFirst){
            io.ALocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
            io.ALocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
            //输出sourceid的信息
            // printf(p"[localmmu]ALocalMMUIO.ConherentRequsetSourceID ${io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits}\n")
            ltlb.io.req.vaddr0 := io.ALocalMMUIO.Request.bits.RequestVirtualAddr
            ltlb.io.req.vaddr0_v := true.B
            sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.AFirst
        }.elsewhen(ChoseIndex_0 === LocalMMUTaskType.BFirst){
            io.BLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
            io.BLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
            // printf(p"[localmmu]BLocalMMUIO.ConherentRequsetSourceID ${io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits}\n")
            ltlb.io.req.vaddr0 := io.BLocalMMUIO.Request.bits.RequestVirtualAddr
            ltlb.io.req.vaddr0_v := true.B
            sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.BFirst
            //输出LocalMMUTaskType.BFirst
            // printf(p"[localmmu]LocalMMUTaskType.BFirst ${LocalMMUTaskType.BFirst}\n")
        }.elsewhen(ChoseIndex_0 === LocalMMUTaskType.CFirst){
            io.CLocalMMUIO.Request.ready := io.LastLevelCacheTLIO.Request.ready
            io.CLocalMMUIO.ConherentRequsetSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID
            // printf(p"[localmmu]CLocalMMUIO.ConherentRequsetSourceID ${io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits}\n")
            ltlb.io.req.vaddr0 := io.CLocalMMUIO.Request.bits.RequestVirtualAddr
            ltlb.io.req.vaddr0_v := true.B
            io.LastLevelCacheTLIO.Request.bits.RequestData := io.CLocalMMUIO.Request.bits.RequestData
            io.LastLevelCacheTLIO.Request.bits.RequestType_isWrite := io.CLocalMMUIO.Request.bits.RequestType_isWrite
            sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits) := LocalMMUTaskType.CFirst
        }.otherwise{

        }
        io.LastLevelCacheTLIO.Request.bits.RequestConherent := true.B
        io.LastLevelCacheTLIO.Request.bits.RequestPhysicalAddr := ltlb.io.resp.paddr0
        io.LastLevelCacheTLIO.Request.bits.RequestSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID.bits
        io.LastLevelCacheTLIO.Request.valid := true.B
    }

    io.ALocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.BLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.CLocalMMUIO.Response.bits := io.LastLevelCacheTLIO.Response.bits
    io.ALocalMMUIO.Response.valid := false.B
    io.BLocalMMUIO.Response.valid := false.B
    io.CLocalMMUIO.Response.valid := false.B

    when(sourceid2port(io.LastLevelCacheTLIO.Response.bits.ReseponseSourceID) === LocalMMUTaskType.AFirst){
        io.ALocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
        io.LastLevelCacheTLIO.Response.ready := io.ALocalMMUIO.Response.ready
    }.elsewhen(sourceid2port(io.LastLevelCacheTLIO.Response.bits.ReseponseSourceID) === LocalMMUTaskType.BFirst){
        io.BLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
        io.LastLevelCacheTLIO.Response.ready := io.BLocalMMUIO.Response.ready
    }.elsewhen(sourceid2port(io.LastLevelCacheTLIO.Response.bits.ReseponseSourceID) === LocalMMUTaskType.CFirst){
        io.CLocalMMUIO.Response.valid := io.LastLevelCacheTLIO.Response.valid
        io.LastLevelCacheTLIO.Response.ready := io.CLocalMMUIO.Response.ready
    }.otherwise{
        
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

        when(io.ALocalMMUIO.Request.valid && io.ALocalMMUIO.Request.ready)
        {
            when(io.ALocalMMUIO.Request.bits.RequestType_isWrite)
            {
                AML_Write_Request_times := AML_Write_Request_times + 1.U
            }.otherwise{
                AML_Read_Request_times := AML_Read_Request_times + 1.U
            }
        }

        when(io.BLocalMMUIO.Request.valid && io.BLocalMMUIO.Request.ready)
        {
            when(io.BLocalMMUIO.Request.bits.RequestType_isWrite)
            {
                BML_Write_Request_times := BML_Write_Request_times + 1.U
            }.otherwise{
                BML_Read_Request_times := BML_Read_Request_times + 1.U
            }
        }

        when(io.CLocalMMUIO.Request.valid && io.CLocalMMUIO.Request.ready)
        {
            when(io.CLocalMMUIO.Request.bits.RequestType_isWrite)
            {
                CML_Write_Request_times := CML_Write_Request_times + 1.U
            }.otherwise{
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
