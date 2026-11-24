
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

// Select matrix registers and rows to supply data to MatrixTE.
// Arrange matrix-register banks to supply matrix and convolution operands without bank conflicts.
// TE consumes Reduce_Width per vector; Tensor_shape describes capacity, organized across Matrix_N banks.
// ConfigInfo initializes the registers, address generators, and state machines for each task.
class BDataController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        // Matrix-register interface.
        val FromMatrixRegIO = Flipped(new TrDataControlMatrixRegIO)
        val ConfigInfo = Flipped(new BDCMicroTaskConfigIO)
        val VectorB = DecoupledIO(UInt((ReduceWidth*Matrix_MN).W))
        val ComputeGo = Input(Bool())// TE lockstep signal: the next operand can be accepted.
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
    })

    //TODO:init
    io.VectorB.valid := false.B
    io.VectorB.bits := io.FromMatrixRegIO.Data.bits.asUInt
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B

    val ConfigInfo = io.ConfigInfo
    val CurrentMatrixRegId = RegInit(0.U(TrMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    // Task state machine.
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    // Compute state machine, including pipeline drain.
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val MatrixRegWorkingTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_K = RegInit(0.U(cuteParams.mmaKGroupCountWidth.W))

    assert(io.ComputeGo === io.VectorB.ready)

    // Print state for debugging.
    when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady)// The current task configuration is valid.
    {
        if(YJPBDCDebugEnable)
        {
            printf("[BDataController<%d>]BDataController: state is %d\n",io.DebugInfo.DebugTimeStampe, state)
            printf("[BDataController<%d>]BDataController: calculate_state is %d\n",io.DebugInfo.DebugTimeStampe, calculate_state)
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
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            
            // Stage 0: initialize the compute state machine.
            calculate_state := s_cal_init
        }
    }

    // Matrix-register data layout.
    // K varies before M (Reduce_DIM_First).
    // AVector contains distinct M rows; supply K vectors until the K iteration completes.
    // Each numbered item is a ReduceWidth vector: 256 bits, or 32 int8 elements in this example.
    //      DATA IN MEMORY          DATA IN AVECTOR     DATA IN MATRIXREG
    // K 0 1 2 3 4 5 6 7     time     AVector     MatrixRegData layout
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

    // Traverse the matrix data for the multiplication task.
    // The register contains Tensor_M * Tensor_K vectors of ReduceWidth bits.
    // Supply Matrix_M vectors to TE, each containing Matrix_N * ReduceWidth bits.
    // Supply K before M; repeat each K group for Tensor_N / Matrix_N column groups, then advance M.
    val M_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // Division by a power of two becomes a hardware shift.
    // Matrix_M is a power of two, so division becomes a shift without a divider.
    val M_IteratorMax = Wire(UInt(cuteParams.mmaMNGroupCountWidth.W))
    val N_IteratorMax = Wire(UInt(cuteParams.mmaMNGroupCountWidth.W))
    M_IteratorMax := (MatrixRegWorkingTensor_M / Matrix_MN.U) + ((MatrixRegWorkingTensor_M % Matrix_MN.U) =/= 0.U) // Supply Matrix_M rows per step; round up Tensor_M / Matrix_MN for a partial group.
    N_IteratorMax := MatrixRegWorkingTensor_N / Matrix_MN.U // Supply Matrix_N columns per step; use Tensor_N / Matrix_MN column groups.
    val K_IteratorMax = (MatrixRegWorkingTensor_K)// K already counts reduce vectors; no further division is needed.

    val Max_Caculate_Iter = Wire(UInt(cuteParams.mmaIterationCountWidth.W))
    Max_Caculate_Iter := M_IteratorMax * N_IteratorMax * K_IteratorMax

    // Count matrix-register read requests.
    val BVectorCount = RegInit(0.U(cuteParams.mmaIterationCountWidth.W))
    val BRequestVectorCount = RegInit(0.U(cuteParams.mmaIterationCountWidth.W))

    val MatrixRegRequestBankAddr = io.FromMatrixRegIO.BankAddr  // Addresses requested from the matrix register.
    MatrixRegRequestBankAddr.bits := DontCare
    MatrixRegRequestBankAddr.valid := false.B                                           // Invalid by default.
    val MatrixRegData = io.FromMatrixRegIO.Data // Matrix-register read data arrives one cycle after the request.

    val MatrixRegDataHoldReg = Reg(UInt(MatrixRegData.bits.asUInt.getWidth.W)) // Hold matrix-register data across MTE stalls to avoid rereading it.
    val MatrixRegDataHoldValid = RegInit(false.B) // Hold matrix-register data across MTE stalls to avoid rereading it.

    // Initialize when the task is active and the compute state machine is in init.
    when(state === s_mm_task){
        switch(calculate_state) {
            is(s_cal_init) {
                // Initialize all iterators.
                M_Iterator := 0.U
                N_Iterator := 0.U
                K_Iterator := 0.U
                BVectorCount := 0.U
                BRequestVectorCount := 0.U
                MatrixRegDataHoldValid := false.B
                // Stage 1: initialization is complete; start supplying operands.
                calculate_state := s_cal_working
            }
            is(s_cal_working) {
                // Stage 2: generate matrix-register read addresses.

                // if (YJPBDCDebugEnable)
                // {
                //     printf("[BDataController<%d>]BDataController: M_Iterator is %d, N_Iterator is %d, K_Iterator is %d\n",io.DebugInfo.DebugTimeStampe, M_Iterator, N_Iterator, K_Iterator)
                //     printf("[BDataController<%d>]BDataController: M_IteratorMax is %d, N_IteratorMax is %d, K_IteratorMax is %d\n",io.DebugInfo.DebugTimeStampe, M_IteratorMax, N_IteratorMax, K_IteratorMax)
                // }
                // MTE iterates M, then N, then K; ComputeGo advances the Max_Caculate_Iter operand requests.
                val logicalColumn = AccBlockLayout.swapColumn(M_Iterator, N_Iterator)
                val next_addr = Wire(UInt(TrMatrixRegBankNEntries.W))
                next_addr := logicalColumn * K_IteratorMax + K_Iterator
                MatrixRegRequestBankAddr.bits.foreach(_ := next_addr)
                
                // ComputeGo consumes one operand and permits the next read.
                // The SRAM read takes one cycle; hold its response if computation stalls before it arrives.
                // When ComputeGo resumes, forward the held operand to TE.
                when(io.ComputeGo && BRequestVectorCount < Max_Caculate_Iter){
                    // Calculate the operand read address.
                    MatrixRegRequestBankAddr.valid := true.B
                    BRequestVectorCount := BRequestVectorCount + 1.U
                    N_Iterator := N_Iterator + 1.U
                    when(N_Iterator === N_IteratorMax - 1.U){
                        N_Iterator := 0.U
                        M_Iterator := M_Iterator + 1.U
                        when(M_Iterator === M_IteratorMax - 1.U){
                            M_Iterator := 0.U
                            K_Iterator := K_Iterator + 1.U
                        }
                    }
                }

                // Output an operand when either the SRAM response or the hold register is valid.
                when(MatrixRegData.valid || MatrixRegDataHoldValid){
                    io.VectorB.valid := true.B
                }
                when (MatrixRegDataHoldValid) {
                    io.VectorB.bits := MatrixRegDataHoldReg
                }

                when(io.VectorB.fire && io.ComputeGo)
                {
                    // Increment AVectorCount only when an operand is consumed.
                    BVectorCount := BVectorCount + 1.U
                    MatrixRegDataHoldValid := false.B   // Consume the held operand first whenever data is accepted.
                    when(BVectorCount === Max_Caculate_Iter - 1.U){// Finish computation after all operands have been consumed.
                        calculate_state := s_cal_end
                        if (YJPBDCDebugEnable)
                        {
                            printf("[BDataController<%d>]BDataController: AVectorCount is %d, we can end this task\n",io.DebugInfo.DebugTimeStampe, BVectorCount)
                        }
                    }
                    // Print the operand count and vector for debugging.
                    if (YJPBDCDebugEnable)
                    {
                        printf("[BDataController<%d>]BDataController: BVectorCount is %d,BVector is %d\n",io.DebugInfo.DebugTimeStampe, BVectorCount,io.VectorB.bits)
                    }
                }.elsewhen(io.VectorB.valid && !io.VectorB.ready && !io.ComputeGo && MatrixRegData.valid){
                    // Hold matrix-register data when it cannot be consumed.
                    // Capture data only when the SRAM response is valid.
                    MatrixRegDataHoldReg := MatrixRegData.bits.asUInt
                    MatrixRegDataHoldValid := true.B
                }.elsewhen(io.VectorB.valid && !io.VectorB.ready && !io.ComputeGo && MatrixRegDataHoldValid)
                {
                    // Keep the held operand while computation remains stalled.
                    MatrixRegDataHoldReg := MatrixRegDataHoldReg
                    MatrixRegDataHoldValid := true.B
                }
            }
            is(s_cal_end) {
                // The compute task has finished; wait for TaskController acknowledgement.
                io.ConfigInfo.MicroTaskEndValid := true.B
                when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                    // Start another task after TaskController acknowledges completion.
                    state := s_idle
                    calculate_state := s_cal_idle
                    if (YJPBDCDebugEnable)
                    {
                        printf("[BDataController<%d>]BDataController: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
                    }
                }
            }
            is(s_cal_idle) {
                // The compute state machine is idle.
                // No accelerator work is active.
            }
        }
    }    
}
