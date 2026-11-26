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
    val MatrixRegAddr = UInt(log2Ceil(ABScaleBankNEntries).W)
}

// Convolution data uses [khkwoc][ic]; matrix multiplication data uses [N][K].

class AScaleLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // Register-storage interface.
        val ToMatrixRegIO = Flipped(new ABScaleLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new ASLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val SpadId = Output(UInt(ABScaleRegIdWidth.W))
        val idle = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(
            Flipped(Decoupled(new MxfpGeneratedScale)))
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
    val generated = ConfigInfo.Generated.map(g =>
        RegEnable(g, false.B, ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady)).getOrElse(false.B)

    val computeType = RegInit(MteComputeType.ComputeTypeUndef)

    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val ScaleBlocksPerRow = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val RowStrideBytes = RegInit(0.U(MMUAddrWidth.W))
    val strided = !generated && RowStrideBytes > ScaleBlocksPerRow
    private val scaleBeatBits = log2Ceil(outsideDataWidthByte)
    val SourceByteOffset = RegInit(0.U(scaleBeatBits.W))

    val Scale_A_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


    val MemoryRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)+1).W))

    
    // Task state machine.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)


    // Memory state machine, including pipeline draining.
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)
    io.idle := state === s_idle && memoryload_state === s_load_idle &&
        !ConfigInfo.MicroTaskValid && !ConfigInfo.MicroTaskEndValid &&
        !io.LocalMMUIO.Request.map(_.valid).reduce(_ || _) &&
        !io.LocalMMUIO.Response.map(_.valid).reduce(_ || _) &&
        !io.GeneratedScale.map(_.valid).getOrElse(false.B) &&
        !io.ToMatrixRegIO.BankAddr.valid && !io.ToMatrixRegIO.Data.valid
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

    val Conherent = RegInit(true.B) // Coherent-access flag supplied by TaskController.

    val CurrentSpadId = RegInit(0.U(ABScaleRegIdWidth.W))
    io.SpadId := CurrentSpadId

    // Accept a valid configuration.

    when(state === s_idle){
        // Accept new configuration only while idle.
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            val mode = ConfigInfo.ApplicationScale_A.computeType
            val beats = (ConfigInfo.ApplicationScale_A.ScaleBlocksPerRow * ConfigInfo.MatrixRegTensor_M +
                (outsideDataWidthByte - 1).U) / outsideDataWidthByte.U
            val perVector = ScaleVecWidth(mode)
            assert(ConfigInfo.MatrixRegTensor_K ===
                (ConfigInfo.ApplicationScale_A.ScaleBlocksPerRow +& (perVector - 1.U)) / perVector,
                "ASL: K is data-vector count, source rows contain one byte per MX32 block")
            assert(ConfigInfo.MatrixRegTensor_M =/= 0.U && ConfigInfo.MatrixRegTensor_K =/= 0.U &&
                beats =/= 0.U && beats <= ABScaleBankNEntries.U,
                "ASL: scale transfer must fit the allocated bank")
            assert(mode === MteComputeType.Mxfp4F32 || mode === MteComputeType.Mxfp8e4m3F32 ||
                mode === MteComputeType.Mxfp8e5m2F32, "ASL: first stage supports MX32/E8M0 only")
            when(ConfigInfo.Generated.getOrElse(false.B)) {
                assert(mode === MteComputeType.Mxfp4F32 && ConfigInfo.MatrixRegTensor_M === 8.U &&
                    ConfigInfo.MatrixRegTensor_K === 1.U &&
                    ConfigInfo.ApplicationScale_A.ScaleBlocksPerRow === 2.U,
                    "generated ASL requires SF8x2 / one packed FP4 vector")
            }
            val sourceBase = ConfigInfo.ApplicationScale_A.BlockScale_A_BaseVaddr
            val sourceOffset = sourceBase(scaleBeatBits - 1, 0)
            val stride = ConfigInfo.ApplicationScale_A.RowStrideBytes
            val rowBytes = ConfigInfo.ApplicationScale_A.ScaleBlocksPerRow
            when(!ConfigInfo.Generated.getOrElse(false.B)) {
                assert(stride >= rowBytes, "ASL: scale source stride is shorter than the logical row")
                assert(stride =/= rowBytes || sourceOffset === 0.U ||
                    sourceOffset +& (rowBytes * ConfigInfo.MatrixRegTensor_M) <= outsideDataWidthByte.U,
                    "ASL: an unaligned compact scale rectangle must fit its containing cacheline")
            }
            RowStrideBytes := stride
            //当前配置的指令有效
            state := s_mm_task
            memoryload_state := s_load_init
            // ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
            computeType := io.ConfigInfo.ApplicationScale_A.computeType
            MatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
            ScaleBlocksPerRow := ConfigInfo.ApplicationScale_A.ScaleBlocksPerRow
            Scale_A_BaseVaddr := io.ConfigInfo.ApplicationScale_A.ApplicationScale_A_BaseVaddr //这个不重要
            Tensor_Block_BaseAddr := Cat(sourceBase(MMUAddrWidth - 1, scaleBeatBits), 0.U(scaleBeatBits.W))
            SourceByteOffset := sourceOffset
            Conherent := io.ConfigInfo.Conherent
            CurrentSpadId := ConfigInfo.ScaleBankId

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
        // The final physical beat is padded by the scale-buffer contract.
        MaxRequestIter := (ScaleBlocksPerRow * MatrixRegTensor_M +
            (outsideDataWidthByte - 1).U) / outsideDataWidthByte.U
    }.elsewhen(memoryload_state === s_load_working && !strided){
        //根据不同的MemoryOrder，执行不同的访存模式

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

        
        when(Request.fire && sourceId.valid){//符合条件的话，这条访存请求一定会被发出
            assert(sourceId.bits < SoureceMaxNum.U, "ASL: allocated source ID outside CAM")
            //Request.ready表明了LocalMMU会处理这条访存请求，sourceID valid，表明这条访存请求的sourceID是被LocalMMU认可有效才发送到这个模块的
            val TableItem = Wire(new AScaleSourceIdSearch)
            TableItem.MatrixRegAddr := MemoryRequestSize 
            SoureceIdSearchTable(sourceId.bits(log2Ceil(SoureceMaxNum) - 1, 0)) := TableItem.asUInt
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
            assert(sourceId < SoureceMaxNum.U, "ASL: response source ID outside CAM")
            assert(TotalLoadSize < MaxRequestIter, "ASL: excess scale response")
            val MatrixRegAddr = SoureceIdSearchTable(sourceId(log2Ceil(SoureceMaxNum) - 1, 0))
                .asTypeOf(new AScaleSourceIdSearch).MatrixRegAddr
            val ResponseData = Wire(Vec(ABScaleNSlices,UInt((ScaleWidth * ReduceGroupSize).W)))
            // CUTE2TL fetches a physical cacheline. Only declared compact source
            // bytes enter the scale buffer; owned padding is E8M0 127, not NaN.
            val sourceBytes = (Response.bits.ReseponseData >> (SourceByteOffset << 3))
                .asTypeOf(Vec(outsideDataWidthByte, UInt(8.W)))
            val liveBytes = MatrixRegTensor_M * ScaleBlocksPerRow
            val padded = Wire(Vec(outsideDataWidthByte, UInt(8.W)))
            for (i <- 0 until outsideDataWidthByte) {
                padded(i) := Mux(MatrixRegAddr * outsideDataWidthByte.U + i.U < liveBytes,
                    sourceBytes(i), 127.U)
            }
            ResponseData := padded.asUInt.asTypeOf(ResponseData)

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
            if(YJPTrLoader1DebugEnable)
            {
                printf("[BSL<%d>]BScaleLoader Task End\n",io.DebugInfo.DebugTimeStampe)
            }
        }
    }.otherwise{
    }

    val stridedDone = StridedScaleFill(
        strided && memoryload_state === s_load_init,
        strided && memoryload_state === s_load_working,
        Tensor_Block_BaseAddr | SourceByteOffset, RowStrideBytes, MatrixRegTensor_M,
        ScaleBlocksPerRow, Conherent, io.LocalMMUIO, io.ToMatrixRegIO)
    when(stridedDone) { memoryload_state := s_load_end }

    // This is an alternate INPUT of the original sole scale writer. No
    // second SSCP port and no external Get in generated mode.
    io.GeneratedScale.foreach { port =>
        val staging = Reg(Vec(outsideDataWidthByte, UInt(8.W)))
        val received = RegInit(0.U(16.W))
        when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady) {
            staging := VecInit(Seq.fill(outsideDataWidthByte)(127.U(8.W)))
            received := 0.U
        }
        port.ready := generated && memoryload_state === s_load_working &&
            !received.andR && !reset.asBool
        val index = Cat(port.bits.row(2, 0), port.bits.block)
        when(port.fire) {
            assert(port.bits.row < 8.U, "generated ASL row out of range")
            assert(!received(index), "generated ASL duplicate K32 scale")
            staging(index) := Mux(port.bits.unsupported, 0.U, port.bits.e)
            received := received | UIntToOH(index, 16)
        }
        when(generated) {
            io.LocalMMUIO.Request.foreach(_.valid := false.B)
            io.LocalMMUIO.Response.foreach(_.ready := false.B)
            when(memoryload_state === s_load_working && received.andR) {
                io.ToMatrixRegIO.BankAddr.valid := !reset.asBool
                io.ToMatrixRegIO.BankAddr.bits := 0.U
                io.ToMatrixRegIO.Data.valid := !reset.asBool
                io.ToMatrixRegIO.Data.bits := staging.asUInt.asTypeOf(io.ToMatrixRegIO.Data.bits)
                // The Valid-only write occurs on this edge, BEFORE held End.
                memoryload_state := s_load_end
            }
        }
    }
}

// Noncompact input fill logic shared by the existing ASL/BSL state machines.
// Reuse their channel 0, selected bank, sole write port and completion handshake.
private[cute] object StridedScaleFill {
    def apply(init: Bool, active: Bool, sourceBase: UInt, stride: UInt, rows: UInt,
              rowBytes: UInt, coherent: Bool, mmu: LocalMMUIO,
              writer: ABScaleLoaderMatrixRegIO)(implicit p: Parameters): Bool = {
        val params = p(CuteParamsKey)
        val beatBytes = params.outsideDataWidthByte
        val beatBits = log2Ceil(beatBytes)
        val addrBits = params.MMUAddrWidth
        val rowBase = Reg(UInt(addrBits.W))
        val row = Reg(UInt(rows.getWidth.W))
        val column = Reg(UInt(rowBytes.getWidth.W))
        val dstByte = Reg(UInt(beatBits.W))
        val dstBeat = Reg(UInt(log2Ceil(params.ABScaleBankNEntries).W))
        val staged = Reg(Vec(beatBytes, UInt(8.W)))
        val neutral = VecInit(Seq.fill(beatBytes)(127.U(8.W)))

        // One outstanding aligned Get. Keep its beat for other live bytes/rows
        // in the same cacheline, but skip cachelines that contain only padding.
        val request = mmu.Request(0)
        val response = mmu.Response(0)
        val sourceId = Mux(coherent, mmu.ConherentRequsetSourceID, mmu.nonConherentRequsetSourceID)
        val pendingId = Reg(chiselTypeOf(sourceId.bits))
        val waiting = RegInit(false.B)
        val cacheValid = RegInit(false.B)
        val cacheAddr = Reg(UInt(addrBits.W))
        val cacheData = Reg(Vec(beatBytes, UInt(8.W)))
        val srcAddr = rowBase + column
        val alignedAddr = Cat(srcAddr(addrBits - 1, beatBits), 0.U(beatBits.W))
        val hit = cacheValid && cacheAddr === alignedAddr
        val done = WireDefault(false.B)

        when(init) {
            rowBase := sourceBase
            row := 0.U
            column := 0.U
            dstByte := 0.U
            dstBeat := 0.U
            staged := neutral
            waiting := false.B
            cacheValid := false.B
        }
        when(active) {
            request.valid := !waiting && !hit
            request.bits.RequestAddr := alignedAddr
            request.bits.RequestConherent := coherent
            request.bits.RequestSourceID := sourceId.bits
            request.bits.UseAllocatedSourceID := true.B
            response.ready := waiting
            when(request.fire) {
                assert(sourceId.valid && sourceId.bits < params.SoureceMaxNum.U,
                    "strided scale fill: invalid allocated source ID")
                pendingId := sourceId.bits
                waiting := true.B
                cacheValid := false.B
                cacheAddr := alignedAddr
            }
            when(response.fire) {
                assert(response.bits.ReseponseSourceID === pendingId,
                    "strided scale fill: response does not own the pending beat")
                waiting := false.B
                cacheValid := true.B
                cacheData := response.bits.ReseponseData.asTypeOf(cacheData)
            }
            when(hit) {
                val next = WireDefault(staged)
                next(dstByte) := cacheData(srcAddr(beatBits - 1, 0))
                val rowDone = column === rowBytes - 1.U
                val last = rowDone && row === rows - 1.U
                column := Mux(rowDone, 0.U, column + 1.U)
                when(rowDone && !last) {
                    row := row + 1.U
                    rowBase := rowBase + stride
                }
                dstByte := dstByte + 1.U
                when(dstByte.andR || last) {
                    writer.BankAddr.valid := true.B
                    writer.BankAddr.bits := dstBeat
                    writer.Data.valid := true.B
                    writer.Data.bits := next.asUInt.asTypeOf(writer.Data.bits)
                    dstBeat := dstBeat + 1.U
                    staged := neutral
                }.otherwise {
                    staged := next
                }
                done := last
            }
        }
        done
    }
}
