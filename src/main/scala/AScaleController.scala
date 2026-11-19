package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.v3.exu.ygjk._

// Select scratchpad rows to supply scale data to MatrixTE.
// Arrange scratchpad banks to supply matrix and convolution operands without bank conflicts.
// TE consumes Reduce_Width per vector; Tensor_shape describes capacity, organized across Matrix_N banks.
// ConfigInfo initializes the registers, address generators, and state machines for each task.

class AScaleController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        // Matrix-register interface.
        val FromMatrixRegIO = Flipped(new ABScaleControlMatrixRegIO)
        val ConfigInfo = Flipped(new ASCMicroTaskConfigIO)
        val ScaleA = DecoupledIO(UInt((ScaleWidth*Matrix_MN).W))
        val ComputeGo = Input(Bool())// TE lockstep signal: the next operand can be accepted.
        val DebugInfo = Input(new DebugInfoIO)
        val SpadId = Output(UInt(1.W))// Select one of the two scratchpad buffers.
    })

    //TODO:init
    io.ScaleA.valid := false.B
    io.ScaleA.bits := 0.U
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B

    val ConfigInfo = io.ConfigInfo

    // Task state machine.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    // Compute state machine, including pipeline drain.
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val MatrixRegWorkingTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val computeType = RegInit(MteComputeType.ComputeTypeUndef)
    val sliceid = RegInit(0.U(log2Ceil(ABScaleNSlices).W))

    val CurrentSpadId = RegInit(0.U(1.W))
    io.SpadId := CurrentSpadId

    assert(io.ComputeGo === io.ScaleA.ready)

    // Print state for debugging.
    when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady)// The current task configuration is valid.
    {
        if(YJPBDCDebugEnable)
        {
            printf("[ADataController<%d>]ADataController: state is %d\n",io.DebugInfo.DebugTimeStampe, state)
            printf("[ADataController<%d>]ADataController: calculate_state is %d\n",io.DebugInfo.DebugTimeStampe, calculate_state)
        }
    }

    // State machine.
    when(state === s_idle){
        // Accept a new configuration only while idle.
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            // The current task configuration is valid.
            if (YJPBDCDebugEnable)
            {
                // Debug information.
                printf("[BDataController<%d>]BDataController: ConfigInfo is valid! MatrixRegWorkingTensor_M = %d,MatrixRegWorkingTensor_N = %d,MatrixRegWorkingTensor_K = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_N, ConfigInfo.MatrixRegTensor_K)
            }
            state := s_mm_task  // Enter the matrix multiplication task.
            MatrixRegWorkingTensor_M := ConfigInfo.MatrixRegTensor_M    // M extent of the current task.
            MatrixRegWorkingTensor_N := ConfigInfo.MatrixRegTensor_N    // N extent of the current task.
            MatrixRegWorkingTensor_K := ConfigInfo.MatrixRegTensor_K    // Number of K reduce vectors in the current task.

            computeType := ConfigInfo.computeType
            
            // Stage 0: initialize the compute state machine.
            calculate_state := s_cal_init
        }
    }


    // Traverse the matrix data for the multiplication task.
    // The register contains Tensor_M * Tensor_K vectors of ReduceWidth bits.
    // Supply Matrix_MN vectors to TE, each containing Matrix_N * ReduceWidth bits.
    // Supply K before M; repeat each K group for Tensor_N / Matrix_N column groups, then advance M.
    val M_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // Division by a power of two becomes a hardware shift.
    // Matrix_MN is a power of two, so division becomes a shift without a divider.
    val M_IteratorMax = (MatrixRegWorkingTensor_M / Matrix_MN.U) + ((MatrixRegWorkingTensor_M % Matrix_MN.U) =/= 0.U) // Supply Matrix_MN rows per step; round up Tensor_M / Matrix_MN for a partial group.
    val N_IteratorMax = (MatrixRegWorkingTensor_N / Matrix_MN.U)// Supply Matrix_N columns per step; use Tensor_N / Matrix_MN column groups.
    val K_IteratorMax = (MatrixRegWorkingTensor_K)// K already counts reduce vectors; no further division is needed.

    val Max_Caculate_Iter = M_IteratorMax * N_IteratorMax * K_IteratorMax   // Total iteration count.

    // Count matrix-register read requests.
    val AVectorCount = RegInit(0.U(32.W))// Number of iterations completed by the current compute task.
    val ARequestVectorCount = RegInit(0.U(32.W))// Number of iterations completed by the current compute task.

    val MatrixRegRequestBankAddr = io.FromMatrixRegIO.BankAddr  // Addresses requested from the matrix register.
    MatrixRegRequestBankAddr.bits := 0.U.asTypeOf(MatrixRegRequestBankAddr.bits)        // Initialize every address to zero.
    MatrixRegRequestBankAddr.valid := false.B                                           // Invalid by default.
    val MatrixRegData = io.FromMatrixRegIO.Data // Matrix-register read data arrives one cycle after the request.

    val MatrixRegDataHoldReg = RegInit(VecInit(Seq.fill(ABScaleNSlices)(0.U((ScaleWidth * ReduceGroupSize).W)))) // Hold matrix-register data across MTE stalls to avoid rereading it.
    // val MatrixRegDataHoldReg = RegInit(0.U.asTypeOf(MatrixRegData.bits)) // Hold data across MTE stalls.
    val MatrixRegDataHoldValid = RegInit(false.B) // Hold matrix-register data across MTE stalls to avoid rereading it.

    val bit_index = RegInit(VecInit(Seq.fill(Matrix_MN)(0.U(log2Ceil(MatrixRegMaxTensorDimBitSize * ScaleWidth).W))))
    for (i <- 0 until Matrix_MN){
        bit_index(i) := ((i * ReduceGroupSize).U + K_Iterator) * (ScaleVecWidth(computeType) * 8.U)
    }

    // Initialize when the task is active and the compute state machine is in init.
    when(state === s_mm_task){
        when(calculate_state === s_cal_init){
            // Initialize all iterators.
            M_Iterator := 0.U
            N_Iterator := 0.U
            K_Iterator := 0.U
            AVectorCount := 0.U
            ARequestVectorCount := 0.U
            MatrixRegDataHoldReg := 0.U.asTypeOf(MatrixRegDataHoldReg)
            MatrixRegDataHoldValid := false.B
            sliceid := 0.U
            // Stage 1: initialization is complete; start supplying operands.
            calculate_state := s_cal_working
        }.elsewhen(calculate_state === s_cal_working){
            // Stage 2: generate scratchpad read addresses.

            // if (YJPBDCDebugEnable)
            // {
            //     printf("[BDataController<%d>]BDataController: M_Iterator is %d, N_Iterator is %d, K_Iterator is %d\n",io.DebugInfo.DebugTimeStampe, M_Iterator, N_Iterator, K_Iterator)
            //     printf("[BDataController<%d>]BDataController: M_IteratorMax is %d, N_IteratorMax is %d, K_IteratorMax is %d\n",io.DebugInfo.DebugTimeStampe, M_IteratorMax, N_IteratorMax, K_IteratorMax)
            // }
            // MTE iterates M, then N, then K; ComputeGo advances the Max_Caculate_Iter operand requests.
            val next_addr = Wire(UInt(TrMatrixRegBankNEntries.W))
            next_addr := (M_Iterator * Matrix_MN.U * K_IteratorMax) * ScaleVecWidth(computeType) * 8.U / outsideDataWidth.U 
            sliceid := ((M_Iterator * Matrix_MN.U * K_IteratorMax) * ScaleVecWidth(computeType) * 8.U / (ScaleWidth * ReduceGroupSize).U) % ABScaleNSlices.U
            MatrixRegRequestBankAddr.bits := next_addr
            
            // ComputeGo consumes one operand and permits the next read.
            // The SRAM read takes one cycle; hold its response if computation stalls before it arrives.
            // When ComputeGo resumes, forward the held operand to TE.
            when(io.ComputeGo && ARequestVectorCount < Max_Caculate_Iter){
                // Calculate the operand read address.
                MatrixRegRequestBankAddr.valid := true.B
                ARequestVectorCount := ARequestVectorCount + 1.U
                N_Iterator := N_Iterator + 1.U
                when(N_Iterator === N_IteratorMax - 1.U){
                    N_Iterator := 0.U
                    M_Iterator := M_Iterator + 1.U
                    when(M_Iterator === M_IteratorMax - 1.U){
                        M_Iterator := 0.U
                        K_Iterator := K_Iterator + 1.U
                    }
                }
            }.otherwise{
                MatrixRegRequestBankAddr.valid := false.B
            }

            // Output an operand when either the SRAM response or the hold register is valid.
            when(MatrixRegData.valid || MatrixRegDataHoldValid){
                io.ScaleA.valid := true.B
                val ResponseData = Mux(MatrixRegDataHoldValid,MatrixRegDataHoldReg, MatrixRegData.bits)// Prefer the held operand.
                val scalea_vec = Wire(Vec(Matrix_MN, UInt(ScaleWidth.W)))
                io.ScaleA.bits := scalea_vec.asUInt
                for (i <- 0 until Matrix_MN){
                    val slice_offset = bit_index(i) / (ScaleWidth * ReduceGroupSize).U
                    val mxfp8ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / mxfp8ScaleWidth, UInt(mxfp8ScaleWidth.W)))// Division by a power of two becomes a right shift.
                    val mxfp4ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / mxfp4ScaleWidth, UInt(mxfp4ScaleWidth.W)))// Division by a power of two becomes a right shift.
                    val nvfp4ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / nvfp4ScaleWidth, UInt(nvfp4ScaleWidth.W)))// Division by a power of two becomes a right shift.
                    mxfp8ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(mxfp8ScaleSlut)
                    mxfp4ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(mxfp4ScaleSlut)
                    nvfp4ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(nvfp4ScaleSlut)
                    val mxfp8SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / mxfp8ScaleWidth.U// Power-of-two modulo selects low bits; division becomes a right shift.
                    val mxfp4SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / mxfp4ScaleWidth.U// Power-of-two modulo selects low bits; division becomes a right shift.
                    val nvfp4SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / nvfp4ScaleWidth.U// Power-of-two modulo selects low bits; division becomes a right shift.
                    scalea_vec(i) := Mux(computeType === MteComputeType.Mxfp8e4m3F32 || computeType === MteComputeType.Mxfp8e5m2F32, mxfp8ScaleSlut(mxfp8SlutId), Mux(computeType === MteComputeType.Mxfp4F32, mxfp4ScaleSlut(mxfp4SlutId), nvfp4ScaleSlut(nvfp4SlutId))).asUInt.pad(ScaleWidth)
                }
            }

            when(io.ScaleA.fire && io.ComputeGo)
            {
                // Increment AVectorCount only when an operand is consumed.
                AVectorCount := AVectorCount + 1.U
                MatrixRegDataHoldValid := false.B   // Consume the held operand first whenever data is accepted.
                when(AVectorCount === Max_Caculate_Iter - 1.U){// Finish computation after all operands have been consumed.
                    calculate_state := s_cal_end
                    if (YJPBDCDebugEnable)
                    {
                        printf("[BDataController<%d>]BDataController: AVectorCount is %d, we can end this task\n",io.DebugInfo.DebugTimeStampe, AVectorCount)
                    }
                }
                // Print the operand count and vector for debugging.
                if (YJPBDCDebugEnable)
                {
                    printf("[BDataController<%d>]BDataController: AVectorCount is %d,AVector is %d\n",io.DebugInfo.DebugTimeStampe, AVectorCount,io.ScaleA.bits)
                }
            }.elsewhen(io.ScaleA.valid && !io.ScaleA.ready && !io.ComputeGo && MatrixRegData.valid){
                // Hold matrix-register data when it cannot be consumed.
                // Capture data only when the SRAM response is valid.
                MatrixRegDataHoldReg := MatrixRegData.bits
                MatrixRegDataHoldValid := true.B
            }.elsewhen(io.ScaleA.valid && !io.ScaleA.ready && !io.ComputeGo && MatrixRegDataHoldValid)
            {
                // Keep the held operand while computation remains stalled.
                MatrixRegDataHoldReg := MatrixRegDataHoldReg
                MatrixRegDataHoldValid := true.B
            }
        }.elsewhen(calculate_state === s_cal_end){
            // The compute task has finished; wait for TaskController acknowledgement.
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                // Start another task after TaskController acknowledges completion.
                state := s_idle
                calculate_state := s_cal_idle
                CurrentSpadId := ~CurrentSpadId  // Switch scratchpad buffers.
                if (YJPBDCDebugEnable)
                {
                    printf("[BDataController<%d>]BDataController: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
                }
            }

        }.elsewhen(calculate_state === s_cal_idle){
            // The compute state machine is idle.
            // No accelerator work is active.
        }.otherwise{
            // Undefined state.
            // No accelerator work is active.
        }
    }    
}
