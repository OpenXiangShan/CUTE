package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._
import freechips.rocketchip.rocket.Instructions.MAX
import freechips.rocketchip.util.SeqToAugmentedSeq
// Load A-side scale data into its register storage.
// Load data from memory into register storage.

class AScaleSourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegAddr = UInt(log2Ceil(TrMatrixRegBankNEntries).W)
}

// Convolution data uses [khkwoc][ic]; matrix multiplication data uses [N][K].

class AScaleLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // Register-storage interface.
        val ToMatrixRegIO = Flipped(new ABScaleLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new ASLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val SpadId = Output(UInt(1.W))// Double-buffer selector, either 0 or 1.
    })

    io.ToMatrixRegIO.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.BankAddr)
    io.ToMatrixRegIO.Data := 0.U.asTypeOf(io.ToMatrixRegIO.Data)
    for (i <- 0 until TrMatrixRegNBanks) {
        io.LocalMMUIO.Request(i).valid := false.B
        io.LocalMMUIO.Request(i).bits := 0.U.asTypeOf(io.LocalMMUIO.Request(i).bits)
        io.LocalMMUIO.Request(i).bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        io.LocalMMUIO.Response(i).ready := false.B
    }
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B


    // val MatrixRegBankAddr = io.ToMatrixRegIO.BankAddr
    // val MatrixRegData = io.ToMatrixRegIO.Data

    val ConfigInfo = io.ConfigInfo

    val computeType = RegInit(MteComputeType.ComputeTypeUndef)

    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val Scale_A_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


    val MemoryRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W))

    
    // Task state machine.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)


    // Memory state machine, including pipeline draining.
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) // Base address of the current tensor block.

    val Conherent = RegInit(true.B) // Coherent-access flag supplied by TaskController.

    val CurrentSpadId = RegInit(0.U(1.W))
    io.SpadId := CurrentSpadId

    // Accept a valid configuration.

    when(state === s_idle){
        // Accept new configuration only while idle.
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            // The configuration describes a valid instruction.
            state := s_mm_task
            memoryload_state := s_load_init
            // ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
            computeType := io.ConfigInfo.ApplicationScale_A.computeType
            MatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            Scale_A_BaseVaddr := io.ConfigInfo.ApplicationScale_A.ApplicationScale_A_BaseVaddr // Full tensor base address.
            Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationScale_A.BlockScale_A_BaseVaddr // Base address used for this block's requests.
            Conherent := io.ConfigInfo.Conherent

            if(YJPTrLoader0DebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
                printf("[BSL<%d>]MatrixRegTensor_N:%d,MatrixRegTensor_K:%d\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.MatrixRegTensor_M,io.ConfigInfo.MatrixRegTensor_K)
                printf("[BSL<%d>]Scale_B_BaseVaddr:%x,Scale_Block_BaseAddr:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationScale_A.ApplicationScale_A_BaseVaddr,io.ConfigInfo.ApplicationScale_A.BlockScale_A_BaseVaddr)
                // printf("[BSL<%d>]ApplicationTensor_B_Stride_N:%x\n",io.DebugInfo.DebugTimeStampe,io.ConfigInfo.ApplicationScale_A.ApplicationTensor_A_Stride_M)
            }
        }
    }

    // Design note: contiguous tensor virtual ranges would be the responsibility of the OS and compiler.

    // A-side data is already reordered.
    // 32 x 32 x 4 B fits in one 4 KiB page.
    // 32 x 128 x 1 B fits in one 4 KiB page.
    // 64 x 64 x 1 B fits in one 4 KiB page.

    // Page-local data must be aligned and contiguous.
    // The layout favors sequential memory reads.
    // Design question: keep each read within a page instead of interleaving N pages?
    // Historical size example: A scratchpad = Tensor_MN x Tensor_K x ReduceWidth = 64 x 64 x 256 bits = 128 KiB.
    //                                  BSctatchpad = Tensor_K×Tensor_MN×ReduceWidth = 64×64×256bit = 128KB
    //                                  CScartchpad = Tensor_MN×Tensor_MN×ResultWidth = 64×64×32bit = 16KB


    // Design alternative: release consumed data early to reduce storage, at the cost of more SRAM ports instead of double buffering.
    // LLC transfer bandwidth is matched to the per-bank entry size.

    // Read the scale data through LLC, where it is expected to be cached.
    // Initialize registers in s_load_init.
    // Issue and receive data in s_load_working.
    // Complete the transfer in s_load_end.
    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W)) // Total amount of data loaded.
    val CurrentLoaded_BlockTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    
    val MaxBlockTensor_M_Index = MatrixRegTensor_M
    val MaxBlockTensor_K_Index = MatrixRegTensor_K

    // Map each in-flight source ID to its scratchpad bank and address.
    // This is a register table indexed by source ID.
    
    // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new BScaleSourceIdSearch)})
    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new AScaleSourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    
    val Request = io.LocalMMUIO.Request(0)  // TODO: Support multiple channels for AScaleLoader
    Request.valid := false.B
    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        MemoryRequestSize := 0.U
        TotalLoadSize := 0.U
        CurrentLoaded_BlockTensor_M := 0.U
        CurrentLoaded_BlockTensor_K := 0.U
        MaxRequestIter := MatrixRegTensor_K * MatrixRegTensor_M * ScaleVecWidth(computeType) / (outsideDataWidthByte.U) // Total number of memory requests to issue.
        printf("[bSL] MaxRequestIter:%d. MatrixRegTensor_K:%d,MatrixRegTensor_M:%d,ScaleVecWidth:%d,outsideDataWidthByte:%d\n",MaxRequestIter,MatrixRegTensor_K,MatrixRegTensor_M,ScaleVecWidth(computeType), outsideDataWidthByte.U)
    }.elsewhen(memoryload_state === s_load_working){
        // Select the access traversal from MemoryOrder.

        // A ready request is accepted by LocalMMU, allowing the next request to proceed.
        // Address products can be computed earlier and queued if multiplication timing becomes critical.
        Request.bits.RequestAddr := Tensor_Block_BaseAddr + outsideDataWidthByte.U * MemoryRequestSize
        
        val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
        Request.bits.RequestConherent := Conherent
        Request.bits.RequestSourceID := sourceId.bits
        Request.bits.RequestType_isWrite := false.B
        Request.bits.UseAllocatedSourceID := true.B
        Request.bits.RequestMask := Fill(MMUMaskWidth, 1.U(1.W))
        Request.valid := true.B
        when(MemoryRequestSize === MaxRequestIter)// For invalid IH/IW coordinates, use zero fill instead of issuing a memory request.
        {
            Request.valid := false.B
        }

        // Scale factors for each subtensor are contiguous; see the layout documentation.

        
        when(Request.fire && sourceId.valid){// Both the memory request and its source ID are accepted.
            // Request.ready indicates LocalMMU acceptance; sourceId.valid indicates an allocated source ID.
            val TableItem = Wire(new AScaleSourceIdSearch)
            TableItem.MatrixRegAddr := MemoryRequestSize 
            SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
            if (YJPTrLoader1DebugEnable)
            {
                // Log the source ID and request metadata.
                printf("[TrLoader1<%d>]sourceId:%d,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,sourceId.bits,TableItem.MatrixRegAddr)
                // Log this memory request.
                printf("[TrLoader1<%d>]RequestAddr:%x,RequestConherent:%d,RequestSourceID:%d,RequestType_isWrite:%d\n",io.DebugInfo.DebugTimeStampe,Request.bits.RequestAddr,Request.bits.RequestConherent,Request.bits.RequestSourceID,Request.bits.RequestType_isWrite)
            }
            when(MemoryRequestSize < MaxRequestIter){
                MemoryRequestSize := MemoryRequestSize + 1.U
            }
        }
        
        // Receive a memory response.
        // Map each in-flight source ID to its scratchpad bank and address.
        // Use the response source ID to find the target scratchpad fill-table entry.
        val Response = io.LocalMMUIO.Response(0)
        Response.ready := true.B
        
        when(Response.fire){
            // The double-buffered A/B design assumes responses can always be accepted, leaving time for data conversion.
            // A release-based design could double the data width to leave write slots, or use separate read and write ports.
            val sourceId = Response.bits.ReseponseSourceID
            val MatrixRegAddr = SoureceIdSearchTable(sourceId).asTypeOf(new AScaleSourceIdSearch).MatrixRegAddr
            val ResponseData = Wire(Vec(ABScaleNSlices,UInt((ScaleWidth * ReduceGroupSize).W)))
            ResponseData := Response.bits.ReseponseData.asTypeOf(ResponseData)

            TotalLoadSize := TotalLoadSize + 1.U
            io.ToMatrixRegIO.BankAddr.valid := true.B
            io.ToMatrixRegIO.BankAddr.bits := MatrixRegAddr
            io.ToMatrixRegIO.Data.valid := true.B
            for (i <- 0 until ABScaleNSlices)
            {
                io.ToMatrixRegIO.Data.bits(i) := ResponseData(i)
            }


            // TODO: Revisit whether a FIFO is needed if the double-buffered path can experience backpressure.
            // Design alternative: pipeline the scratchpad-to-loader-to-MMU-to-memory combinational path.
            // Alternatively, software could schedule stable data and memory streams to avoid stalls on that path.
            // Scratchpad writes have priority, so the single writer can proceed without a FIFO under this assumption.
            // Write priority favors slow external-memory returns while local readers wait.
            
            // Look up metadata using the response source ID.
            // TODO: Generalize the fixed read amount to support boundary cases.
            if (YJPTrLoader1DebugEnable)
            {
                // Log this memory response.
                printf("[BSL<%d>]ResponseData:%x,MatrixRegAddr:%d\n",io.DebugInfo.DebugTimeStampe,Response.bits.ReseponseData.asUInt,MatrixRegAddr)
            }
        }

        // Advance the state machine.
        when(TotalLoadSize === (MaxRequestIter)){
            memoryload_state := s_load_end
            if (YJPAccMemoryDebugEnable)
            {
                printf("[BScaleLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
            }
        }
        
    }.elsewhen(memoryload_state === s_load_end){
        io.ConfigInfo.MicroTaskEndValid := true.B
        when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
            memoryload_state := s_load_idle
            state := s_idle
            CurrentSpadId := ~CurrentSpadId  // Switch the double buffer.
            if(YJPTrLoader1DebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task End\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
    }

}
