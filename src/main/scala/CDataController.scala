
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._


// Select matrix registers and rows to supply data to MatrixTE.
// Arrange matrix-register banks to supply matrix and convolution operands without bank conflicts.
// ConfigInfo initializes the registers, address generators, and state machines for each task.
class CDataController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        // Matrix-register interface.
        val FromMatrixRegIO = Flipped(new AccDataControlMatrixRegIO)
        val ConfigInfo = Flipped(new CDCMicroTaskConfigIO)
        val Matrix_C = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
        val ResultMatrix_D = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
        val ComputeGo = Input(Bool())// TE lockstep signal: the next operand can be accepted.
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
    })

    io.Matrix_C.valid := false.B
    val ResponseData = Wire(Vec(AccMatrixRegNBanks, UInt(AccMatrixRegEntryBitSize.W)))
    for (i <- 0 until AccMatrixRegNBanks) {
        ResponseData(i) := io.FromMatrixRegIO.ReadResponseData(i).bits
    }
    io.Matrix_C.bits := ResponseData.asUInt
    io.ResultMatrix_D.ready := false.B
    io.FromMatrixRegIO.WriteBankAddr.map(_.valid := false.B)
    io.FromMatrixRegIO.WriteBankAddr.map(_.bits := DontCare)
    io.FromMatrixRegIO.WriteRequestData.map(_.valid := false.B)
    io.FromMatrixRegIO.WriteRequestData.map(_.bits := DontCare)
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTask_TEComputeEndValid := false.B

    io.FromMatrixRegIO.ReadBankAddr.map(_.valid := false.B)
    io.FromMatrixRegIO.ReadBankAddr.map(_.bits := DontCare)

    val MatrixRegReadResponseData = io.FromMatrixRegIO.ReadResponseData // One-cycle read latency.
    // val MatrixRegChosen = io.FromMatrixRegIO.Chosen

    val ConfigInfo = io.ConfigInfo
    val CurrentMatrixRegId = RegInit(0.U(AccMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    // Task state machine: visit banks in order and return their data.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    // Compute state machine, including pipeline drain.
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val MatrixRegWorkingTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    

    val Is_Transpose                        = RegInit(false.B)      // Whether the task requires a transpose.
    val Is_AfterOps_Tile                    = RegInit(false.B)      // Whether this tile requires after-operations, including transpose.
    val Is_Reorder_Only_Ops                 = RegInit(false.B)      // Reorder data without computation.
    val Is_EasyScale_Only_Ops               = RegInit(false.B)      // Apply simple scaling without additional after-operations.
    val Is_VecFIFO_Ops                      = RegInit(false.B)      // Whether VecFIFO participates in this task.
    val D_Datatype                          = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))     // Data type: 0=int8, 1=int16, 2=int32, 3=fp16.


    assert(io.ComputeGo === io.Matrix_C.ready, "CDC: ComputeGo and Matrix_C.ready should be the same!")
    // State machine.
    when(state === s_idle){
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            // The current task configuration is valid.
            if (YJPCDCDebugEnable)
            {
                // Debug information.
                printf("[CDataController<%d>]CDataController: ConfigInfo is valid! MatrixRegWorkingTensor_M = %d,MatrixRegWorkingTensor_N = %d,MatrixRegWorkingTensor_K = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_N, ConfigInfo.MatrixRegTensor_K)
                printf("[CDataController<%d>]CDataController: Is_Transpose = %d,Is_AfterOps_Tile = %d,Is_Reorder_Only_Ops = %d,Is_EasyScale_Only_Ops = %d,Is_VecFIFO_Ops = %d,D_Datatype = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.Is_Transpose, ConfigInfo.Is_AfterOps_Tile, ConfigInfo.Is_Reorder_Only_Ops, ConfigInfo.Is_EasyScale_Only_Ops, ConfigInfo.Is_VecFIFO_Ops, ConfigInfo.ApplicationTensor_D.dataType)
            }
            state := s_mm_task  // Enter the matrix multiplication task.
            MatrixRegWorkingTensor_M := ConfigInfo.MatrixRegTensor_M / Matrix_MN.U * Matrix_MN.U + (ConfigInfo.MatrixRegTensor_M % Matrix_MN.U =/= 0.U) * Matrix_MN.U    // M extent of the current task, rounded up to a matrix row group.

            MatrixRegWorkingTensor_N := ConfigInfo.MatrixRegTensor_N    // N extent of the current task.
            MatrixRegWorkingTensor_K := ConfigInfo.MatrixRegTensor_K    // Number of K reduce vectors in the current task.
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            

            Is_Transpose                := ConfigInfo.Is_Transpose          // Whether the task requires a transpose.
            Is_AfterOps_Tile            := ConfigInfo.Is_AfterOps_Tile      // Whether this tile requires after-operations, including transpose.
            Is_Reorder_Only_Ops         := ConfigInfo.Is_Reorder_Only_Ops   // Reorder data without computation.
            Is_EasyScale_Only_Ops       := ConfigInfo.Is_EasyScale_Only_Ops // Apply simple scaling without additional after-operations.
            Is_VecFIFO_Ops              := ConfigInfo.Is_VecFIFO_Ops        // Whether VecFIFO participates in this task.

            D_Datatype                  := ConfigInfo.ApplicationTensor_D.dataType // Data type: 0=int8, 1=int16, 2=int32, 3=fp16.
            // Stage 0: initialize the compute state machine.
            calculate_state := s_cal_init
        }
    }

    // ACC matrix-register data layout.
    // N varies before M.
    // N 0 1 2 3 4 5 6 7              AccMatrixRegData layout
    // M                               {bank  [0]     [1]    [2]     [3]  }
    // 0   0 1 2 3 4 5 6 7   |addr    0 |    0123    89ab   ghij    opgr 
    // 1   8 9 a b c d e f   |        1 |    4567    cdef   klmn    stuv 
    // 2   g h i j k l m n   |        2 |    wxyz    !...   @...    #... 
    // 3   o p g r s t u v   |        3 |    ....    ....   ....    ....
    // 4   w x y z .......   |        4 |    ....    ....   ....    .... 
    // 5   !..............   |        5 |    ....    ....   ....    ....
    // 6   @..............   |        6 |    ....    ....   ....    ....
    // 7   #..............   |        7 |    ....    ....   ....    .... 
    // 8   $..............   | ....................................

    // Traverse the matrix data for the multiplication task.
    // TODO: consider changing traversal order to reduce bandwidth usage.
    // The register contains Tensor_M * Tensor_K vectors of ReduceWidth bits.
    // Supply Matrix_M vectors to TE, each containing Matrix_N * ReduceWidth bits.
    // Supply K before M; repeat each K group for Tensor_N / Matrix_N column groups, then advance M.
    val M_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // Write back compute results.
    val addr_Iterator = RegInit(0.U(32.W)) // Address iterator.
    val result_K_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val Store_M_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Store_N_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val M_IteratorMax = (MatrixRegWorkingTensor_M / Matrix_MN.U)
    val N_IteratorMax = (MatrixRegWorkingTensor_N / Matrix_MN.U)
    val K_IteratorMax = (MatrixRegWorkingTensor_K)

    val Max_Caculate_Iter = M_IteratorMax * N_IteratorMax * K_IteratorMax
    val addr_IteratorMax = M_IteratorMax * N_IteratorMax

    val Max_Store_Iter   = (MatrixRegWorkingTensor_M * MatrixRegWorkingTensor_N * D_Datatype) / (ResultWidthByte*Matrix_MN*Matrix_MN).U

    val CVectorCount = RegInit(0.U(32.W))
    val DVectorCount = RegInit(0.U(32.W))
    // Matrix-register read requests.
    val ReadRequest = WireInit(false.B)
    val WriteRequset = WireInit(false.B)

    val ReadMatrixRegDataHoldReg = Reg(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)) // Hold data across MTE stalls; used only when ReadMatrixRegDataHoldValid is true, so no reset is needed.
    val ReadMatrixRegDataHoldValid = RegInit(false.B) // Hold matrix-register data across MTE stalls to avoid rereading it.

    // Initialize when the task is active and the compute state machine is in init.
    when(state === s_mm_task){
        switch(calculate_state) {
            is(s_cal_init) {
                M_Iterator := 0.U
                N_Iterator := 0.U
                K_Iterator := 0.U
                addr_Iterator := 0.U
                result_K_Iterator := 0.U
                Store_M_Iterator := 0.U
                Store_N_Iterator := 0.U
                CVectorCount := 0.U
                DVectorCount := 0.U
                ReadMatrixRegDataHoldReg := 0.U
                ReadMatrixRegDataHoldValid := false.B

                assert(MatrixRegWorkingTensor_N === Tensor_MN.U, "MatrixRegWorkingTensor_N = %d is not Full!", MatrixRegWorkingTensor_N)
                // Stage 1: compute initialization is complete.
                calculate_state := s_cal_working

                if (YJPCDCDebugEnable)
                {
                    //Max_Caculate_Iter
                    //Max_Store_Iter
                    printf("[CDataController<%d>]CDataController: Max_Caculate_Iter is %d, Max_Store_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Max_Caculate_Iter, Max_Store_Iter)
                }
            }
            is(s_cal_working) {
                // Stage 2: generate matrix-register read addresses.

                // Iterate M in the outer loop, then N.
                val load_addr =  M_Iterator * N_IteratorMax + N_Iterator
                dontTouch(load_addr)

                when(io.ComputeGo && CVectorCount < Max_Caculate_Iter){
                    // Calculate the operand read address.
                    ReadRequest := true.B
                    io.FromMatrixRegIO.ReadBankAddr.map(_.valid := true.B)
                    io.FromMatrixRegIO.ReadBankAddr.map(_.bits := load_addr)
                    N_Iterator := N_Iterator + 1.U
                    when(N_Iterator === N_IteratorMax - 1.U){
                        N_Iterator := 0.U
                        M_Iterator := M_Iterator + 1.U
                        when(M_Iterator === M_IteratorMax - 1.U){
                            // Finish computation when the M iterator reaches its limit.
                            M_Iterator := 0.U
                            K_Iterator := K_Iterator + 1.U
                        }
                    }
                }

                val Response_valid = MatrixRegReadResponseData.map(_.valid).reduce(_&&_)
                when(Response_valid|| ReadMatrixRegDataHoldValid){
                    io.Matrix_C.valid := true.B
                }
                when (ReadMatrixRegDataHoldValid) {
                    io.Matrix_C.bits := ReadMatrixRegDataHoldReg
                }


                when(io.Matrix_C.fire && io.ComputeGo)
                {
                    // Increment AVectorCount only when an operand is consumed.
                    CVectorCount := CVectorCount + 1.U
                    ReadMatrixRegDataHoldValid := false.B   // Consume the held operand first whenever data is accepted.
                    when(CVectorCount === Max_Caculate_Iter - 1.U){// Print read completion after all operands have been consumed.
                        if (YJPCDCDebugEnable)
                        {
                            printf("[CDataController<%d>]CDataController: CVectorCount is %d, we finish the load work\n",io.DebugInfo.DebugTimeStampe, CVectorCount)
                        }
                    }
                    // Print the operand count and vector for debugging.
                    if (YJPCDCDebugEnable)
                    {
                        printf("[CDataController<%d>]CDataController: CVectorCount is %d,CMatrix is %x\n",io.DebugInfo.DebugTimeStampe, CVectorCount,io.Matrix_C.bits)
                    }
                }.elsewhen(io.Matrix_C.valid && !io.Matrix_C.ready && !io.ComputeGo && Response_valid){
                    // Hold matrix-register data when it cannot be consumed.
                    // Capture data only when the SRAM response is valid.
                    ReadMatrixRegDataHoldReg := ResponseData.asUInt
                    ReadMatrixRegDataHoldValid := true.B
                }.elsewhen(io.Matrix_C.valid && !io.Matrix_C.ready && !io.ComputeGo && ReadMatrixRegDataHoldValid)
                {
                    // Keep the held operand while computation remains stalled.
                    ReadMatrixRegDataHoldReg := ReadMatrixRegDataHoldReg
                    ReadMatrixRegDataHoldValid := true.B
                }

                when(io.ResultMatrix_D.valid)
                {
                    
                    io.FromMatrixRegIO.WriteBankAddr.map(_.valid := true.B)
                    io.FromMatrixRegIO.WriteRequestData.map(_.valid := true.B)
                    val MReg_Wrie_data = Wire((Vec(AccMatrixRegNBanks, (UInt(AccMatrixRegEntryBitSize.W)))))
                    MReg_Wrie_data := io.ResultMatrix_D.bits.asTypeOf(MReg_Wrie_data)
                    val transpose_data = Wire((Vec(AccMatrixRegNBanks, Vec(AccMatrixRegNBanks, (UInt(ResultWidth.W))))))
                    for (i <- 0 until AccMatrixRegNBanks){
                        for (j <- 0 until AccMatrixRegNBanks){
                            transpose_data(i)(j) := MReg_Wrie_data(j)((i+1)*ResultWidth - 1 , i*ResultWidth)
                        }
                    }
                    
                    for (i <- 0 until AccMatrixRegNBanks){
                        when(Is_Transpose && Is_AfterOps_Tile && result_K_Iterator === K_IteratorMax - 1.U)
                        {
                            // Transpose the result when requested.
                            io.FromMatrixRegIO.WriteRequestData(i).bits := transpose_data(i).asUInt
                        }.otherwise{
                            // Otherwise write the result directly.
                            io.FromMatrixRegIO.WriteRequestData(i).bits := MReg_Wrie_data(i)
                        }
                    }
                    
                    
                    WriteRequset := true.B// Write the matrix register whenever result data is valid.
                    io.ResultMatrix_D.ready := true.B
                    
                    val store_addr = WireInit(addr_Iterator)
                    io.FromMatrixRegIO.WriteBankAddr.map(_.bits := store_addr)// The write address follows the DVector counter.
                    when(io.ResultMatrix_D.fire){
                        addr_Iterator := addr_Iterator + 1.U
                        when(addr_Iterator === addr_IteratorMax - 1.U){
                            // Finish computation when the address iterator reaches its limit.
                            addr_Iterator := 0.U
                            result_K_Iterator := result_K_Iterator + 1.U
                        }
                        DVectorCount := DVectorCount + 1.U
                        if (YJPCDCDebugEnable)
                        {
                            printf("[CDataController<%d>]CDataController: DVectorCount is %d,Data is %x,store_addr is %x\n", io.DebugInfo.DebugTimeStampe, DVectorCount, io.ResultMatrix_D.bits,store_addr)
                        }
                        when(DVectorCount === Max_Caculate_Iter - 1.U)
                        {// Compute results retain full precision; use Max_Caculate_Iter as the iteration limit.
                            calculate_state := s_cal_end
                        }
                    }
                }
            }
            is(s_cal_end) {
                // The compute task has finished; wait for TaskController acknowledgement.
                io.ConfigInfo.MicroTaskEndValid := true.B
                when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                    // Start another task after TaskController acknowledges completion.
                    state := s_idle
                    calculate_state := s_cal_idle
                    if (YJPCDCDebugEnable)
                    {
                        printf("[CDataController<%d>]CDataController: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
            }
            is(s_cal_idle) {
                // The compute state machine is idle.
                // No accelerator work is active.
            }
        }
    }
    
    if (EnableDifftest) {
        val pcReg = RegInit(0.U(64.W))
        val coreidReg = RegInit(0.U(8.W))
        when (io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady) {
          pcReg := io.ConfigInfo.pc.get
          coreidReg := io.ConfigInfo.coreid.get
        }
        val difftestAmuFinish = MatrixRegWriteProbes.genWriteProbes(MatrixRegWriteProbes.mma, ports = 1).head
        // Initialize defaults.
        difftestAmuFinish.coreid := coreidReg
        difftestAmuFinish.index := 0.U
        // Each K sweep overwrites the same output entries. Only the final sweep
        // survives in the host result buffer; intermediate physical writes still run.
        val finalKWriteback = result_K_Iterator === K_IteratorMax - 1.U
        difftestAmuFinish.valid := ((io.FromMatrixRegIO.WriteBankAddr.map(_.valid).reduce(_||_) && finalKWriteback)
          || (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        difftestAmuFinish.bankValid := VecInit(io.FromMatrixRegIO.WriteBankAddr.map(_.valid)).asUInt
        for (i <- 0 until AccMatrixRegNBanks) {
          difftestAmuFinish.bankAddr(i) := io.FromMatrixRegIO.WriteBankAddr(i).bits
          difftestAmuFinish.bankMask.get(i) := Fill(AccMatrixRegEntryByteSize, true.B)
        }
        val bankBits = difftestAmuFinish.wordsPerBank * MatrixRegWriteProbes.writebackWordBits
        // Pad each bank before flattening into the common writeback layout.
        difftestAmuFinish.data := VecInit(io.FromMatrixRegIO.WriteRequestData.map(_.bits.pad(bankBits)))
          .asUInt.asTypeOf(difftestAmuFinish.data)
        difftestAmuFinish.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    }

    // Combine requests to the matrix register.
    // Pack read in bit 0, compute write in bit 1, and memory write in bit 2.
    // Let the matrix register arbitrate these requests.
    // TODO: use a dedicated request bundle here.
    val request = Wire(new MatrixRegTask)
    request.ReadFromMemoryLoader := false.B
    request.WriteFromMemoryLoader := false.B
    request.WriteFromDataController := WriteRequset
    request.ReadFromDataController := ReadRequest
    io.FromMatrixRegIO.ReadWriteRequest := request.asUInt
    
}
