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
        // Atomic with ScaleA, including its synchronous-response hold token.
        val LiveK = Output(Vec(Matrix_MN, UInt(log2Ceil(ReduceWidth / 4 + 1).W)))
        val ComputeGo = Input(Bool())//由TE发出的计算同步锁步信号，指可以接收新的数据了
        val ReadIssueEnable = Input(Bool())
        val DebugInfo = Input(new DebugInfoIO)
        val SpadId = Output(UInt(ABScaleRegIdWidth.W))
        val idle = Output(Bool())
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
    val ScaleBlocksPerRow = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val KElements = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    val computeType = RegInit(MteComputeType.ComputeTypeUndef)

    val CurrentSpadId = RegInit(0.U(ABScaleRegIdWidth.W))
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
            ScaleBlocksPerRow := ConfigInfo.ScaleBlocksPerRow
            KElements := ConfigInfo.KElements
            CurrentSpadId := ConfigInfo.ScaleBankId
            
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

    //我们这里scala写做除法，但其实硬件里面是移位，所以不会有除法的延迟
    //Matrix_MN一定是2的幂次，所有这个除法一定会被优化成移位，一定是一拍完成的，一定会优化成移位电路
    val M_IteratorMax = (MatrixRegWorkingTensor_M / Matrix_MN.U) + ((MatrixRegWorkingTensor_M % Matrix_MN.U) =/= 0.U) //每次送入的数据是Matrix_MN个，所以M的迭代器是Tensor_M/Matrix_MN, 如果不能整除，那么就要多迭代一次
    val N_IteratorMax = (MatrixRegWorkingTensor_N +& (Matrix_MN - 1).U) / Matrix_MN.U
    val K_IteratorMax = (MatrixRegWorkingTensor_K)//K已经是ReduceVector的数量了不需要再除了

    val Max_Caculate_Iter = M_IteratorMax * N_IteratorMax * K_IteratorMax   // Total iteration count.

    // Count matrix-register read requests.
    val AVectorCount = RegInit(0.U(32.W))// Number of iterations completed by the current compute task.
    val ARequestVectorCount = RegInit(0.U(32.W))// Number of iterations completed by the current compute task.

    val MatrixRegRequestBankAddr = io.FromMatrixRegIO.BankAddr  // Addresses requested from the matrix register.
    MatrixRegRequestBankAddr.bits := 0.U.asTypeOf(MatrixRegRequestBankAddr.bits)        // Initialize every address to zero.
    MatrixRegRequestBankAddr.valid := false.B                                           // Invalid by default.
    val MatrixRegData = io.FromMatrixRegIO.Data // Matrix-register read data arrives one cycle after the request.

    val MatrixRegDataHoldReg = RegInit(VecInit(Seq.fill(ABScaleNSlices)(0.U((ScaleWidth * ReduceGroupSize).W)))) //保存MatrixReg的数据，当发生MTE的NACK时，可以不需要重新从MatrixReg读数
    // val MatrixRegDataHoldReg = RegInit(0.U.asTypeOf(MatrixRegData.bits)) //保存MatrixReg的数据，当发生MTE的NACK时，可以不需要重新从MatrixReg读数
    val MatrixRegDataHoldValid = RegInit(false.B) //保存MatrixReg的数据，当发生MTE的NACK时，可以不需要重新从MatrixReg读数
    io.idle := state === s_idle && calculate_state === s_cal_idle &&
        !MatrixRegDataHoldValid && !MatrixRegData.valid && !MatrixRegRequestBankAddr.valid &&
        !io.ScaleA.valid && !ConfigInfo.MicroTaskValid && !ConfigInfo.MicroTaskEndValid

    // Metadata belongs to the synchronous read response, not the next iterator.
    // Compact source layout: [row][K-vector][MX32 scales]. The scheduler limits
    // one M/N group to one physical beat in this first stage.
    val bit_index = RegInit(VecInit(Seq.fill(Matrix_MN)(0.U(log2Ceil(outsideDataWidth).W))))
    val rowValid = RegInit(VecInit(Seq.fill(Matrix_MN)(false.B)))
    val firstScaleBlock = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val liveK = RegInit(VecInit(Seq.fill(Matrix_MN)(0.U(log2Ceil(ReduceWidth / 4 + 1).W))))
    io.LiveK := liveK

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
            //阶段1，初始化完成，开始供数任务
            calculate_state := s_cal_working
        }.elsewhen(calculate_state === s_cal_working){
            // Stage 2: generate scratchpad read addresses.

            // if (YJPBDCDebugEnable)
            // {
            //     printf("[BDataController<%d>]BDataController: M_Iterator is %d, N_Iterator is %d, K_Iterator is %d\n",io.DebugInfo.DebugTimeStampe, M_Iterator, N_Iterator, K_Iterator)
            //     printf("[BDataController<%d>]BDataController: M_IteratorMax is %d, N_IteratorMax is %d, K_IteratorMax is %d\n",io.DebugInfo.DebugTimeStampe, M_IteratorMax, N_IteratorMax, K_IteratorMax)
            // }
            //MTE循环的最外层是M，然后是N，最后是K,所以这里在同步信号的ComputeGo的协同下，执行Max_Caculate_Iter次取数
            val groupByteOffset = M_Iterator * Matrix_MN.U * ScaleBlocksPerRow
            MatrixRegRequestBankAddr.bits := groupByteOffset / outsideDataWidthByte.U
            
            //只要ComputeGo有效，就表示一定会有一个数据被消耗，我们可以继续取数
            //但我们有一个周期的读数延迟，所以如果当前拍不能再继续计算，则我们取得数会在NACK，我们将NACK的数据保存在holdreg中
            //只要等Computgo有效，就可以继续取数，我们会将NACK的数据输出给TE
            when(io.ComputeGo && io.ReadIssueEnable && ARequestVectorCount < Max_Caculate_Iter){
                //计算取数地址
                MatrixRegRequestBankAddr.valid := true.B
            }
            when(MatrixRegRequestBankAddr.fire){
                val vectorElements = ScaleVecWidth(computeType) * 32.U
                val firstElement = K_Iterator * vectorElements
                val remaining = Mux(KElements > firstElement, KElements - firstElement, 0.U)
                for (i <- 0 until Matrix_MN) {
                    bit_index(i) := (groupByteOffset % outsideDataWidthByte.U +
                        i.U * ScaleBlocksPerRow + K_Iterator * ScaleVecWidth(computeType)) * 8.U
                    rowValid(i) := M_Iterator * Matrix_MN.U + i.U < MatrixRegWorkingTensor_M
                    liveK(i) := Mux(M_Iterator * Matrix_MN.U + i.U < MatrixRegWorkingTensor_M,
                        Mux(remaining < vectorElements, remaining, vectorElements), 0.U)
                }
                firstScaleBlock := K_Iterator * ScaleVecWidth(computeType)
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
            }

            // Output an operand when either the SRAM response or the hold register is valid.
            when(MatrixRegData.valid || MatrixRegDataHoldValid){
                io.ScaleA.valid := true.B
                val ResponseData = Mux(MatrixRegDataHoldValid,MatrixRegDataHoldReg, MatrixRegData.bits)// Prefer the held operand.
                val scalea_vec = Wire(Vec(Matrix_MN, UInt(ScaleWidth.W)))
                io.ScaleA.bits := scalea_vec.asUInt
                for (i <- 0 until Matrix_MN){
                    val shifted = ResponseData.asUInt >> bit_index(i)
                    val bytes = Wire(Vec(ScaleWidth / 8, UInt(8.W)))
                    for (s <- 0 until ScaleWidth / 8) {
                        val live = rowValid(i) && s.U < ScaleVecWidth(computeType) &&
                            firstScaleBlock + s.U < ScaleBlocksPerRow
                        bytes(s) := Mux(live, shifted(s * 8 + 7, s * 8), 127.U)
                    }
                    scalea_vec(i) := bytes.asUInt
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
