package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

class BScaleController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        //先整一个ScarchPad的接口的总体设计
        val FromScarchPadIO = Flipped(new BScaleControlScaratchpadIO)
        val ConfigInfo = Flipped(new BDCMicroTaskConfigIO)
        val ScaleB = DecoupledIO(UInt((ScaleWidth*Matrix_N).W))
        val ComputeGo = Input(Bool())//由TE发出的计算同步锁步信号，指可以接收新的数据了
        val DebugInfo = Input(new DebugInfoIO)
    })

    //TODO:init
    io.ScaleB.valid := false.B
    io.ScaleB.bits := 0.U
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B

    val ConfigInfo = io.ConfigInfo

    //任务状态机
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    //计算状态机，用来配合流水线刷新
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val ScaratchpadWorkingTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val dataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val sliceid = RegInit(0.U(log2Ceil(BScaleNSlices).W))

    assert(io.ComputeGo === io.ScaleB.ready)

    //输出state，用于debug
    when(ConfigInfo.MicroTaskValid && ConfigInfo.MicroTaskReady)//当前配置的指令有效
    {
        if(YJPBDCDebugEnable)
        {
            printf("[BDataController<%d>]BDataController: state is %d\n",io.DebugInfo.DebugTimeStampe, state)
            printf("[BDataController<%d>]BDataController: calculate_state is %d\n",io.DebugInfo.DebugTimeStampe, calculate_state)
        }
    }

    //状态机
    when(state === s_idle){
        //idel状态才可以接受新的配置信息
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            //当前配置的指令有效
            if (YJPBDCDebugEnable)
            {
                //debug信息
                printf("[BDataController<%d>]BDataController: ConfigInfo is valid! ScaratchpadWorkingTensor_M = %d,ScaratchpadWorkingTensor_N = %d,ScaratchpadWorkingTensor_K = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.ScaratchpadTensor_M, ConfigInfo.ScaratchpadTensor_N, ConfigInfo.ScaratchpadTensor_K)
            }
            state := s_mm_task  //切换到矩阵乘状态
            ScaratchpadWorkingTensor_M := ConfigInfo.ScaratchpadTensor_M    //当前执行的矩阵乘任务的M
            ScaratchpadWorkingTensor_N := ConfigInfo.ScaratchpadTensor_N    //当前执行的矩阵乘任务的N
            ScaratchpadWorkingTensor_K := ConfigInfo.ScaratchpadTensor_K    //当前执行的矩阵乘任务的K的ReduceVector的数量

            dataType := ConfigInfo.ApplicationTensor_B.dataType
            
            //阶段0，让计算状态机开始初始化，开始计算状态机开始工作
            calculate_state := s_cal_init
        }
    }


    //矩阵乘的状态机，遍历所有数据就完事了
    //首先Scaratchpad的数据有Tensor_M*Tensor_K个，每个数据是ReduceWidth位
    //然后我们要把这些数据送入TE，每次送入的数据是Matrix_M个，每个数据是Matrix_N*ReduceWidth位
    //我们的Scaratchpad是先排K再排M，所以我们的数据送入也是先送K再送M，每次送完一批K，重复Tensor_N/Matrix_N次，再切换M
    val M_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    //我们这里scala写做除法，但其实硬件里面是移位，所以不会有除法的延迟
    //Matrix_M一定是2的幂次，所有这个除法一定会被优化成移位，一定是一拍完成的，一定会优化成移位电路
    val M_IteratorMax = (ScaratchpadWorkingTensor_M / Matrix_M.U) + ((ScaratchpadWorkingTensor_M % Matrix_M.U) =/= 0.U) //每次送入的数据是Matrix_M个，所以M的迭代器是Tensor_M/Matrix_M, 如果不能整除，那么就要多迭代一次
    val N_IteratorMax = (ScaratchpadWorkingTensor_N / Matrix_N.U)//每次送入的数据是Matrix_N个，所以N的迭代器是Tensor_N/Matrix_N
    val K_IteratorMax = (ScaratchpadWorkingTensor_K)//K已经是ReduceVector的数量了不需要再除了

    val Max_Caculate_Iter = M_IteratorMax * N_IteratorMax * K_IteratorMax   //总共的迭代次数

    //统计读数请求次数
    val BVectorCount = RegInit(0.U(32.W))//当前计算任务实际上的迭代次数
    val BRequestVectorCount = RegInit(0.U(32.W))//当前计算任务实际上的迭代次数

    val ScarchPadRequestBankAddr = io.FromScarchPadIO.BankAddr  //往ScarchPad请求数据的地址
    ScarchPadRequestBankAddr.bits := 0.U.asTypeOf(ScarchPadRequestBankAddr.bits)        //全部初始化为0
    ScarchPadRequestBankAddr.valid := false.B                                           //默认无效
    val ScarchPadData = io.FromScarchPadIO.Data //从ScarchPad读数，会有1周期的延迟

    val ScarchPadDataHoldReg = RegInit(VecInit(Seq.fill(BScaleNSlices)(0.U((ScaleWidth * ReduceGroupSize).W)))) //保存ScarchPad的数据，当发生MTE的NACK时，可以不需要重新从ScarchPad读数
    // val ScarchPadDataHoldReg = RegInit(0.U.asTypeOf(ScarchPadData.bits)) //保存ScarchPad的数据，当发生MTE的NACK时，可以不需要重新从ScarchPad读数
    val ScarchPadDataHoldValid = RegInit(false.B) //保存ScarchPad的数据，当发生MTE的NACK时，可以不需要重新从ScarchPad读数

    val bit_index = RegInit(VecInit(Seq.fill(Matrix_N)(0.U(log2Ceil(ScaratchpadMaxTensorDimBitSize * ScaleWidth).W))))
    for (i <- 0 until Matrix_N){
        bit_index(i) := ((i * ReduceGroupSize).U + K_Iterator) * (ScaleVecWidth(dataType) * 8.U)
    }

    //如果是mm_task,且计算状态机是init，那么就开始初始化
    when(state === s_mm_task){
        when(calculate_state === s_cal_init){
            //初始化阶段，要将所有的迭代器初始化
            M_Iterator := 0.U
            N_Iterator := 0.U
            K_Iterator := 0.U
            BVectorCount := 0.U
            BRequestVectorCount := 0.U
            ScarchPadDataHoldReg := 0.U.asTypeOf(ScarchPadDataHoldReg)
            ScarchPadDataHoldValid := false.B
            sliceid := 0.U
            //阶段1，初始化完成，开始供数任务
            calculate_state := s_cal_working
        }.elsewhen(calculate_state === s_cal_working){
            //阶段2，计算开始，计算对Scarchpad的取数地址

            // if (YJPBDCDebugEnable)
            // {
            //     printf("[BDataController<%d>]BDataController: M_Iterator is %d, N_Iterator is %d, K_Iterator is %d\n",io.DebugInfo.DebugTimeStampe, M_Iterator, N_Iterator, K_Iterator)
            //     printf("[BDataController<%d>]BDataController: M_IteratorMax is %d, N_IteratorMax is %d, K_IteratorMax is %d\n",io.DebugInfo.DebugTimeStampe, M_IteratorMax, N_IteratorMax, K_IteratorMax)
            // }
            //MTE循环的最外层是M，然后是N，最后是K,所以这里在同步信号的ComputeGo的协同下，执行Max_Caculate_Iter次取数
            val next_addr = Wire(UInt(AScratchpadBankNEntrys.W))
            next_addr := (N_Iterator * Matrix_N.U * K_IteratorMax) * ScaleVecWidth(dataType) * 8.U / outsideDataWidth.U 
            sliceid := ((N_Iterator * Matrix_N.U * K_IteratorMax) * ScaleVecWidth(dataType) * 8.U / (ScaleWidth * ReduceGroupSize).U) % BScaleNSlices.U
            ScarchPadRequestBankAddr.bits := next_addr
            
            //只要ComputeGo有效，就表示一定会有一个数据被消耗，我们可以继续取数
            //但我们有一个周期的读数延迟，所以如果当前拍不能再继续计算，则我们取得数会在NACK，我们将NACK的数据保存在holdreg中
            //只要等Computgo有效，就可以继续取数，我们会将NACK的数据输出给TE
            when(io.ComputeGo && BRequestVectorCount < Max_Caculate_Iter){
                //计算取数地址
                ScarchPadRequestBankAddr.valid := true.B
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
            }.otherwise{
                ScarchPadRequestBankAddr.valid := false.B
            }

            //只要ScarchPadData是valid或者holdreg是valid，就可以输出数据
            when(ScarchPadData.valid || ScarchPadDataHoldValid){
                io.ScaleB.valid := true.B
                val ResponseData = Mux(ScarchPadDataHoldValid,ScarchPadDataHoldReg, ScarchPadData.bits)//优先输出holdreg的数据
                val scaleb_vec = Wire(Vec(Matrix_N, UInt((ScaleWidth).W)))
                io.ScaleB.bits := scaleb_vec.asUInt
                for (i <- 0 until Matrix_N){
                    val slice_offset = bit_index(i) / (ScaleWidth * ReduceGroupSize).U
                    val mxfp8ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / mxfp8ScaleWidth, UInt(mxfp8ScaleWidth.W)))
                    val mxfp4ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / mxfp4ScaleWidth, UInt(mxfp4ScaleWidth.W)))
                    val nvfp4ScaleSlut = Wire(Vec(ScaleWidth * ReduceGroupSize / nvfp4ScaleWidth, UInt(nvfp4ScaleWidth.W)))
                    mxfp8ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(mxfp8ScaleSlut)
                    mxfp4ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(mxfp4ScaleSlut)
                    nvfp4ScaleSlut := ResponseData(sliceid + slice_offset).asTypeOf(nvfp4ScaleSlut)
                    val mxfp8SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / mxfp8ScaleWidth.U
                    val mxfp4SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / mxfp4ScaleWidth.U
                    val nvfp4SlutId = (bit_index(i) % (ScaleWidth * ReduceGroupSize).U) / nvfp4ScaleWidth.U
                    scaleb_vec(i) := Mux(dataType === ElementDataType.DataTypeMxfp8e4m3F32 || dataType === ElementDataType.DataTypeMxfp8e5m2F32, mxfp8ScaleSlut(mxfp8SlutId), Mux(dataType === ElementDataType.DataTypemxfp4F32, mxfp4ScaleSlut(mxfp4SlutId), nvfp4ScaleSlut(nvfp4SlutId))).asUInt.pad(ScaleWidth)
                }
            }

            when(io.ScaleB.fire && io.ComputeGo)
            {
                //只有当数据被消耗的时候，才会增加AVectorCount
                BVectorCount := BVectorCount + 1.U
                ScarchPadDataHoldValid := false.B   //只要数据被消耗，肯定优先消耗holdreg的数据
                when(BVectorCount === Max_Caculate_Iter - 1.U){//如果数据全部被消耗，那么我们就结束计算
                    calculate_state := s_cal_end
                    if (YJPBDCDebugEnable)
                    {
                        printf("[BDataController<%d>]BDataController: AVectorCount is %d, we can end this task\n",io.DebugInfo.DebugTimeStampe, BVectorCount)
                    }
                }
                //输出AVectorCount，VectorA的信息
                if (YJPBDCDebugEnable)
                {
                    printf("[BDataController<%d>]BDataController: BVectorCount is %d,BVector is %d\n",io.DebugInfo.DebugTimeStampe, BVectorCount,io.ScaleB.bits)
                }
            }.elsewhen(io.ScaleB.valid && !io.ScaleB.ready && !io.ComputeGo && ScarchPadData.valid){
                //如果数据没有被消耗，那么我们就要保存ScarchPad的数据
                //但我们得看看ScarchPad的数据是不是有效的
                ScarchPadDataHoldReg := ScarchPadData.bits
                ScarchPadDataHoldValid := true.B
            }.elsewhen(io.ScaleB.valid && !io.ScaleB.ready && !io.ComputeGo && ScarchPadDataHoldValid)
            {
                //如果数据没有被消耗，且我们HlodReg中有数据，我们就继续Hlod这份数据
                ScarchPadDataHoldReg := ScarchPadDataHoldReg
                ScarchPadDataHoldValid := true.B
            }
        }.elsewhen(calculate_state === s_cal_end){
            //当前计算任务结束，等待TaskCtrl的确认
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                //TaskCtrl确认后，我们就可以进入下一个任务了
                state := s_idle
                calculate_state := s_cal_idle
                if (YJPBDCDebugEnable)
                {
                    printf("[BDataController<%d>]BDataController: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
                }
            }

        }.elsewhen(calculate_state === s_cal_idle){
            //计算状态机空闲
            //加速器闲闲没事做
        }.otherwise{
            //未定义状态
            //加速器闲闲没事做
        }
    }    
}