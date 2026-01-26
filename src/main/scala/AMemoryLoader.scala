
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

//AMemoryLoader，用于加载A矩阵的数据，供给MatrixReg使用
//从不同的存储介质中加载数据，供给MatrixReg使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据MatrixReg的设计，完成Tensor的切分以及将数据的填入MatrixReg

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

//本模块的核心任务是从外部接口加载数据到MatrixReg中
//所有的访存任务会通过TL-Link接口发出，每次的读请求的宽度是ReduceWidthByte

//总写回MReg的次数 = Tensor_MN * ReduceGroupSize
//需要解决的核心问题为，计算出每次访存的地址，然后发出访存请求，然后接受访存的返回值，然后将返回值写入到MatrixReg中
//1.计算访存地址，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.每次Load的微任务，KH_Index、KW_Index不变，根据OH、OW、stride_H、stride_W即可得到当前Load任务的起始地址。
////b.我们的卷积数据的Input是[NHW][C]排布的,所以ApplicationTensor_A_Stride_M就是下一个[NHW]的地址偏移量
////c.每次迭代下一个Load请求，IH_Index、IW_Index会变，根据IH_Index、IW_Index，即可得到当前的Load任务是否需要发生真实的Load请求，还是直接发生0填充
//2.发出访存请求，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要记录mmu发送的source_id，用于后续的数据写回，这里的source_id是唯一的，且会记录该source_id对应的MatrixReg的地址和bank号
////b.需要0填充的情况，需要单独处理，找机会和某一次写回合并，根据此刻某个bank是否被占用来偷时点(由于MatrixReg的总读写带宽肯定比LLC的总带宽大，所以这里的偷时点是可行的)
//3.接受访存返回值，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要根据source_id找到对应的MatrixReg的地址和bank号
////b.同时找机会和某一次写回合并！根据此刻某个bank是否被占用来偷时点写回MatrixReg

//重要的几个逻辑部分
//1.计算IH、IW是否超界，如果超界，需要进行0填充，而不是发出访存请求(这里可能是时序不满足的点，如果不满足，可以提前就算好)
//2.0填充的逻辑，需要单独处理，需要找机会和某一次写回合并。故需要一个NACK寄存器来记录哪个bank此刻有ZeroFill任务，如果无法记录NACK任务，则停止发出访存请求，直到有空闲的bank

//数据在MatrixReg中的编排
//数据会先排M，再排K，
//AVector一定是不同M的数据，K不断送入，直到K迭代完成，再换新的M，
//这里的0 1 2 3是一个K连续的ReduceWidth宽的数据
//   K 0 1 2 3 4 5 6 7     time     AVector     MatrixRegData也这么排布
// M                        0       0 8 g o             {bank[0] [1] [2] [3]}
// 0   0 1 2 3 4 5 6 7      1       1 9 h p   |addr    0 |    0   8   g   o
// 1   8 9 a b c d e f      2       2 a i q   |        1 |    1   9   h   p
// 2   g h i j k l m n      3       3 b j r   |        2 |    2   a   i   q
// 3   o p g r s t u v      4       4 c k s   |        3 |    3   b   j   r
// 4   w x y z .......      5       5 d l t   |        4 |    4   c   k   s
// 5   !..............      6       6 e m u   |        5 |    5   d   l   t
// 6   @..............      7       7 f n v   |        6 |    6   e   m   u
// 7   #..............      8       w ! @ #   |        7 |    7   f   n   v
// 8   $..............      9       .......   | ...........................
//
//
// 在内存中的排布则是 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z .......

class AMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        //先整一个 MatrixReg 的接口的总体设计
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
        val running = Output(Bool())
    })

    val timer = RegInit(0.U(log2Ceil(50000).W))
    timer := timer + 1.U
    private def log(s: Printable, end: String="\n"): Unit = if (YJPAMLDebugEnable) printf(cf"[$timer][AML] " + s + end)

    //任务状态机,顺序读取所有分块矩阵
    val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
    val state = RegInit(s_idle)

    //访存状态机，用来配合流水线刷新
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
    val ConfigInfo = io.ConfigInfo
    val BaseVAddr = RegInit(0.U(MMUAddrWidth.W))
    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))//需要加载到MatrixReg的Tensor的M
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))//需要加载到MatrixReg的Tensor的K
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize)+1).W)) //总共要加载的张量大小，总加载的数据量不会超过Tensor_M*ReduceGroupSize*ruduceWidthByte，这个是不会变的
    val MAX_Fill_Times = outsideDataWidthByte / ABMatrixRegEntryByteSize
    val Conherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供
    // 向上取到4的倍数
    val max_M_index = MatrixRegTensor_M / Matrix_MN.U * Matrix_MN.U + ((MatrixRegTensor_M % Matrix_MN.U) =/= 0.U) * 4.U
    val reqCountMax = (MatrixRegTensor_M / ABMatrixRegNBanks.U) * (MatrixRegTensor_K / MAX_Fill_Times.U)
    val max_K_index = MatrixRegTensor_K
    val current_M_index = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val current_K_index = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val ExpectedLoadSize = reqCountMax * ABMatrixRegNBanks.U * MAX_Fill_Times.U

    // sourceId 直接回传 MatrixReg bank 内地址，所以必须保证位宽足够
    require(
      log2Ceil(ABMatrixRegBankNEntrys) <= SoureceMaxNumBitSize,
      "RequestSourceID 位宽不足以直接回传 MatrixRegBankAddr，请增大 SoureceMaxNumBitSize 或改用其它编码方式"
    )

    val Stride = RegInit(0.U((MMUAddrWidth).W))

    val bankReqCount = RegInit(VecInit(Seq.fill(ABMatrixRegNBanks){0.U(16.W)}))

    /**
      * 每个 bank 独立的响应 FIFO + 多拍写回器（深度=2）
      * 使用 chisel3.util.Queue 作为 FIFO 存储 {respData, baseAddr}，
      * 额外用本地寄存器管理当前正在写回的 entry 的 remainSlices。
      * - 入队：在 Response.fire 时把 {respData, baseAddr} 写入 Queue
      * - 出队：每拍最多写回 1 个 entry（若 respData 比 entry 宽，需要 MAX_Fill_Times 拍写完）
      * - 反压：Queue 满则拉低该 bank 的 Response.ready
      */
    class BankRespFifo(bankIdx: Int) {
        class Entry extends Bundle {
            val data = UInt(outsideDataWidth.W)
            val baseAddr = UInt(log2Ceil(ABMatrixRegBankNEntrys).W)
        }

        val q = Module(new Queue(new Entry, 128, pipe=true, flow=true))

        // 当前正在写回的 entry 状态
        val processing = RegInit(false.B)
        val curData = WireInit(0.U(outsideDataWidth.W))
        val curBaseAddr = WireInit(0.U(log2Ceil(ABMatrixRegBankNEntrys).W))
        val curRemain = RegInit(MAX_Fill_Times.U((log2Ceil(MAX_Fill_Times) + 1).W))

        // 默认连接
        q.io.enq.valid := false.B
        q.io.enq.bits := 0.U.asTypeOf(q.io.enq.bits)
        q.io.deq.ready := false.B

        def readyForResp: Bool = q.io.enq.ready

        /** 入队：调用点必须保证 Response.fire 为真（因此处不再显式 when 包裹） */
        def enqFromResp(sourceId: UInt, respData: UInt): Unit = {
            q.io.enq.valid := true.B
            q.io.enq.bits.data := respData
            q.io.enq.bits.baseAddr := sourceId(log2Ceil(ABMatrixRegBankNEntrys) - 1, 0)
            log(cf"Response[$bankIdx] enqueue sourceId=$sourceId")
        }

        /**
          * 每拍写回：最多写 1 个 entry 到该 bank 的 MatrixReg
          * @return 本拍是否发生写回（用于统计 Load_Size）
          */
        def stepWriteback(toMReg: ABMemoryLoaderMatrixRegIO): Bool = {
            val haveWritten = WireInit(false.B)
            val sliceIdx = MAX_Fill_Times.U - curRemain
            val slices = Wire(Vec(MAX_Fill_Times, UInt((8 * ABMatrixRegEntryByteSize).W)))
            slices := q.io.deq.bits.data.asTypeOf(slices)

            // 若当前没有在处理 entry，则尝试从 Queue 取一个作为当前 entry
            when (q.io.deq.valid) {
                toMReg.BankAddr(bankIdx).bits := q.io.deq.bits.baseAddr + sliceIdx
                toMReg.BankAddr(bankIdx).valid := true.B
                toMReg.Data(bankIdx).bits := slices(sliceIdx)
                toMReg.Data(bankIdx).valid := true.B
                haveWritten := true.B
                // Indicate continous reg write, and the last beat deq.
                when (curRemain > 1.U) {
                    curRemain := curRemain - 1.U
                }.elsewhen (curRemain === 1.U) {
                    // Restart reg write as soon as possible. If no entry is available, deq.valid will guard this.
                    curRemain := MAX_Fill_Times.U
                }
            }

            // See only one beat, can deq the entry.
            when (curRemain === 1.U) {
                q.io.deq.ready := true.B
            }

            haveWritten
        }
    }

    val bankFifos = Seq.tabulate(ABMatrixRegNBanks)(i => new BankRespFifo(i))

    /** load 子状态机：初始化寄存器（进入 working 前的一拍） */
    def stepLoadInit(): Unit = {
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        current_M_index := 0.U
        current_K_index := 0.U
    }

    /** load 子状态机：工作态（发请求/处理 zerofill/接收 response/回填/累计完成度） */
    def stepLoadWorking(): Unit = {
        log(
          cf"current_M_index $current_M_index, " +
          cf"current_K_index $current_K_index, " +
          cf"Stride $Stride, " +
          cf"reqCountMax $reqCountMax, " +
          cf"TotalLoadSize $TotalLoadSize, " +
          cf"ExpectedLoadSize $ExpectedLoadSize"
        )

        val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))

        // 计算当前迭代的初始地址和步进地址
        for (i <- 0 until ABMatrixRegNBanks) {
            val request = io.LocalMMUIO.Request(i)
            val reqCount = bankReqCount(i)
            request.valid := reqCount < reqCountMax
            request.bits.RequestAddr := BaseVAddr + bankReqCount(i) * ABMatrixRegNBanks.U * Stride + i.U * Stride
            request.bits.RequestConherent := Conherent
            request.bits.RequestType_isWrite := false.B

            // sourceId 直接回传该请求对应的 MatrixReg bank 内地址（base addr）
            // 这样 Response 侧无需反查表，即可知道写回的 BankAddr
            val mregBaseAddr = (bankReqCount(i) * ReduceGroupSize.U) + current_K_index
            request.bits.RequestSourceID := mregBaseAddr

            val msg = cf"Request[$i] addr ${request.bits.RequestAddr}%x, curr bankReqCount ${bankReqCount(i)}, sourceId=setAddr $mregBaseAddr"
            when(request.fire) {
                bankReqCount(i) := bankReqCount(i) + 1.U
                log(msg)
            }.elsewhen(request.valid && !request.ready) {
                log(cf"Stuck! " + msg)
            }

            // 当本 bank fifo 满时，必须反压 response
            io.LocalMMUIO.Response(i).ready := bankFifos(i).readyForResp

            when(io.LocalMMUIO.Response(i).fire) {
                val sourceId = io.LocalMMUIO.Response(i).bits.ReseponseSourceID
                val responseData = io.LocalMMUIO.Response(i).bits.ReseponseData
                bankFifos(i).enqFromResp(sourceId, responseData)
            }

            // 每 bank 每拍最多写回 1 个 entry（如果 respData 比 entry 宽，则需要多拍写完）
            when(bankFifos(i).stepWriteback(io.ToMatrixRegIO)) {
                Current_Fill_MReg_Time(i) := 1.U
            }
        }

        val Load_Size = PopCount(Current_Fill_MReg_Time.asUInt)
        TotalLoadSize := TotalLoadSize + Load_Size

        when(TotalLoadSize === ExpectedLoadSize){
            log(cf"memoryload_state -> s_load_end, TotalLoadSize $TotalLoadSize MAX_Fill_Times $MAX_Fill_Times")
            memoryload_state := s_load_end
        }
    }

    /** load 子状态机：结束态（通知上游 task 完成，等待 ready 后回到 idle） */
    def stepLoadEnd(): Unit = {
        ConfigInfo.MicroTaskEndValid := true.B
        when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
            memoryload_state := s_load_idle
            state := s_idle
            log(cf"AMemoryLoader Task End")
        }
    }

    /** 主任务状态机：只在 idle 时接受一条 micro-task 配置，并切换到 load 状态机开始工作 */
    def acceptMicroTaskInIdle(): Unit = {
        // idle 状态才可以接受新的配置信息
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady) {
            // 当前配置有效：进入任务执行
            state := s_mm_task
            memoryload_state := s_load_init

            MatrixRegTensor_M := ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := ConfigInfo.MatrixRegId

            BaseVAddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
            // Tensor_Block_BaseAddr := ConfigInfo.ApplicationTensor_A.BlockTensor_A_BaseVaddr
            // Conherent := io.ConfigInfo.bits.ApplicationTensor_A.Conherent

            Stride := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M          // 每移动一次IW的地址偏移
            assert(ConfigInfo.MatrixRegTensor_K === ReduceGroupSize.U)

            // T7: 添加调试日志
            log(cf"Config received: MatrixRegTensor_M=${ConfigInfo.MatrixRegTensor_M}, MatrixRegTensor_K=${ConfigInfo.MatrixRegTensor_K}, ReduceGroupSize.U=${ReduceGroupSize.U}")

            log(cf"AMemoryLoader Task Start")
            log(cf"MatrixRegTensor_M:${ConfigInfo.MatrixRegTensor_M}, MatrixRegTensor_K:${ConfigInfo.MatrixRegTensor_K}, " +
                cf"Tensor_A_BaseVaddr:${ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr}%x, " +
                cf"ApplicationTensor_A_Stride_M:${ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M}%x, " +
                cf"dataType:${ConfigInfo.ApplicationTensor_A.dataType}"
            )
        }
    }

    // 对外统一使用 ToMatrixRegIO
    io.ToMatrixRegIO.BankId.valid := false.B
    io.ToMatrixRegIO.BankId.bits := 0.U
    io.ToMatrixRegIO.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.BankAddr)
    io.ToMatrixRegIO.Data := 0.U.asTypeOf(io.ToMatrixRegIO.Data)
    io.ToMatrixRegIO.ZeroFill := 0.U.asTypeOf(io.ToMatrixRegIO.ZeroFill)

    // 初始化多路请求接口
    for (i <- 0 until ABMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LocalMMUIO.Request(i).bits)
        io.LocalMMUIO.Response(i).ready := false.B
    }
    
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
    io.MatrixRegId := CurrentMatrixRegId

    io.running := false.B

    // 主状态机：只负责“是否接受新任务”
    when(state === s_idle) {
        acceptMicroTaskInIdle()
    }.otherwise {
        io.running := true.B
    }

    when(memoryload_state === s_load_init) {
        stepLoadInit()
    }.elsewhen(memoryload_state === s_load_working) {
        stepLoadWorking()

    }.elsewhen(memoryload_state === s_load_end) {
        stepLoadEnd()
    }
}
