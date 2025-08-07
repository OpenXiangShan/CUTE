
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
import boom.v3.util._


//代表对MatrixTE供数的供数逻辑控制单元，隶属于TE，负责选取Scarchpad，选取Scarchpad的行，向TE供数。
//主要问题在如何设计Scarchpad，在为两种模式供数时(矩阵乘运算和卷积运算)，不存在bank冲突，数据每拍都能完整供应上。
//本模块的核心设计是以ConfigInfo为输入进行配置的，以模块内部寄存器为基础的，长时间运行的取数地址计算和状态机设计。
class CDataController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        //先整一个ScarchPad的接口的总体设计
        val FromScarchPadIO = Flipped(new CDataControlScaratchpadIO)
        val ConfigInfo = Flipped(new CDCMicroTaskConfigIO)
        val Matrix_C = DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W))
        val ResultMatrix_D = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W)))
        val AfterOpsInterface = (new AfterOpsInterface)
        val ComputeGo = Input(Bool())//由TE发出的计算同步锁步信号，指可以接收新的数据了
        val DebugInfo = Input(new DebugInfoIO)
    })

    io.Matrix_C.valid := false.B
    io.Matrix_C.bits := 0.U
    io.ResultMatrix_D.ready := false.B
    io.FromScarchPadIO.WriteBankAddr := 0.U.asTypeOf(io.FromScarchPadIO.WriteBankAddr)
    io.FromScarchPadIO.WriteRequestData := 0.U.asTypeOf(io.FromScarchPadIO.WriteRequestData)
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
    io.ConfigInfo.MicroTask_TEComputeEndValid := false.B

    io.AfterOpsInterface.CDCDataToInterface.valid := false.B
    io.AfterOpsInterface.CDCDataToInterface.bits := 0.U.asTypeOf(io.AfterOpsInterface.CDCDataToInterface.bits)
    io.AfterOpsInterface.InterfaceToCDCData.ready := false.B
    io.AfterOpsInterface.VecInstQueueID := 0.U

    

    io.FromScarchPadIO.ReadBankAddr := 0.U.asTypeOf(io.FromScarchPadIO.ReadBankAddr)

    val ScarchPadReadResponseData = io.FromScarchPadIO.ReadResponseData //1周期的延迟
    // val ScarchPadChosen = io.FromScarchPadIO.Chosen

    io.FromScarchPadIO.WriteRequestData := 0.U.asTypeOf(io.FromScarchPadIO.WriteRequestData)

    val ConfigInfo = io.ConfigInfo

    //任务状态机 先来个简单的，顺序遍历所有bank，返回数据
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    //计算状态机，用来配合流水线刷新
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_after_vecops :: s_cal_end :: Nil = Enum(5)
    val calculate_state = RegInit(s_cal_idle)
    val ScaratchpadWorkingTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    

    val Is_Transpose                        = RegInit(false.B)      //是否需要转置
    val Is_AfterOps_Tile                    = RegInit(false.B)      //是否是需要执行后操作的Tile，包括转置等
    val Is_Reorder_Only_Ops                 = RegInit(false.B)      //是否只是重排，不需要计算
    val Is_EasyScale_Only_Ops               = RegInit(false.B)      //是否只是简单的缩放，不需要额外的后操作计算
    val Is_VecFIFO_Ops                      = RegInit(false.B)      //是否真的需要同有VecFIFO的参与
    val D_Datatype                          = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))     //数据类型，0是int8，1是int16，2是int32，3是fp16


    assert(io.ComputeGo === io.Matrix_C.ready, "CDC: ComputeGo and Matrix_C.ready should be the same!")
    //状态机
    when(state === s_idle){
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            //当前配置的指令有效
            if (YJPCDCDebugEnable)
            {
                //debug信息
                printf("[CDataController<%d>]CDataController: ConfigInfo is valid! ScaratchpadWorkingTensor_M = %d,ScaratchpadWorkingTensor_N = %d,ScaratchpadWorkingTensor_K = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.ScaratchpadTensor_M, ConfigInfo.ScaratchpadTensor_N, ConfigInfo.ScaratchpadTensor_K)
                printf("[CDataController<%d>]CDataController: Is_Transpose = %d,Is_AfterOps_Tile = %d,Is_Reorder_Only_Ops = %d,Is_EasyScale_Only_Ops = %d,Is_VecFIFO_Ops = %d,D_Datatype = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.Is_Transpose, ConfigInfo.Is_AfterOps_Tile, ConfigInfo.Is_Reorder_Only_Ops, ConfigInfo.Is_EasyScale_Only_Ops, ConfigInfo.Is_VecFIFO_Ops, ConfigInfo.ApplicationTensor_D.dataType)
            }
            state := s_mm_task  //切换到矩阵乘状态
            ScaratchpadWorkingTensor_M := ConfigInfo.ScaratchpadTensor_M / Matrix_M.U * Matrix_M.U + (ConfigInfo.ScaratchpadTensor_M % Matrix_M.U =/= 0.U) * Matrix_M.U    //当前执行的矩阵乘任务的M, 取4的整数

            ScaratchpadWorkingTensor_N := ConfigInfo.ScaratchpadTensor_N    //当前执行的矩阵乘任务的N
            ScaratchpadWorkingTensor_K := ConfigInfo.ScaratchpadTensor_K    //当前执行的矩阵乘任务的K的ReduceVector的数量
            

            Is_Transpose                := ConfigInfo.Is_Transpose          //是否需要转置
            Is_AfterOps_Tile            := ConfigInfo.Is_AfterOps_Tile      //是否是需要执行后操作的Tile，包括转置等
            Is_Reorder_Only_Ops         := ConfigInfo.Is_Reorder_Only_Ops   //是否只是重排，不需要计算
            Is_EasyScale_Only_Ops       := ConfigInfo.Is_EasyScale_Only_Ops //是否只是简单的缩放，不需要额外的后操作计算
            Is_VecFIFO_Ops              := ConfigInfo.Is_VecFIFO_Ops        //是否真的需要同有VecFIFO的参与

            D_Datatype                  := ConfigInfo.ApplicationTensor_D.dataType //数据类型，0是int8，1是int16，2是int32，3是fp16
            //阶段0，让计算状态机开始初始化，开始计算状态机开始工作
            calculate_state := s_cal_init
        }
    }

    //数据在CScarachpad中的编排
    //数据会先排N，再排M
    //   N 0 1 2 3 4 5 6 7              CScaratchpadData里的排布
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

    //矩阵乘的状态机，遍历所有数据就完事了
    //TODO:这里可以修改遍历顺序来节省带宽
    //首先Scaratchpad的数据有Tensor_M*Tensor_K个，每个数据是ReduceWidth位
    //然后我们要把这些数据送入TE，每次送入的数据是Matrix_M个，每个数据是Matrix_N*ReduceWidth位
    //我们的Scaratchpad是先排K再排M，所以我们的数据送入也是先送K再送M，每次送完一批K，重复Tensor_N/Matrix_N次，再切换M
    val M_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    // 计算数据结果回填
    val addr_Iterator = RegInit(0.U(32.W)) //地址迭代器
    val result_K_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val Store_M_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val Store_N_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val M_IteratorMax = (ScaratchpadWorkingTensor_M / Matrix_M.U)
    val N_IteratorMax = (ScaratchpadWorkingTensor_N / Matrix_N.U)
    val K_IteratorMax = (ScaratchpadWorkingTensor_K)

    val Max_Caculate_Iter = M_IteratorMax * N_IteratorMax * K_IteratorMax
    val addr_IteratorMax = M_IteratorMax * N_IteratorMax

    val Max_Store_Iter   = (ScaratchpadWorkingTensor_M * ScaratchpadWorkingTensor_N * D_Datatype) / (ResultWidthByte*Matrix_M*Matrix_N).U

    val CVectorCount = RegInit(0.U(32.W))
    val DVectorCount = RegInit(0.U(32.W))
    //对Scaratchpad的数据请求
    val ReadRequest = WireInit(false.B)
    val WriteRequset = WireInit(false.B)

    val ReadScarchPadDataHoldReg = RegInit(0.U((ResultWidth*Matrix_M*Matrix_N).W)) //保存ScarchPad的数据，当发生MTE的NACK时，可以不需要重新从ScarchPad读数
    val ReadScarchPadDataHoldValid = RegInit(false.B) //保存ScarchPad的数据，当发生MTE的NACK时，可以不需要重新从ScarchPad读数

    val After_ops_issue_iter = RegInit(0.U(32.W)) //后操作的迭代器，用来计算后操作的迭代次数



    //如果是mm_task,且计算状态机是init，那么就开始初始化
    when(state === s_mm_task){
        when(calculate_state === s_cal_init){
            M_Iterator := 0.U
            N_Iterator := 0.U
            K_Iterator := 0.U
            addr_Iterator := 0.U
            result_K_Iterator := 0.U
            Store_M_Iterator := 0.U
            Store_N_Iterator := 0.U
            CVectorCount := 0.U
            DVectorCount := 0.U
            ReadScarchPadDataHoldReg := 0.U
            After_ops_issue_iter := 0.U
            ReadScarchPadDataHoldValid := false.B

            assert(ScaratchpadWorkingTensor_N === Tensor_N.U, "ScaratchpadWorkingTensor_N = %d is not Full!", ScaratchpadWorkingTensor_N)
            //阶段1，计算初始化完成，开始工作
            calculate_state := s_cal_working

            if (YJPCDCDebugEnable)
            {
                //Max_Caculate_Iter
                //Max_Store_Iter
                printf("[CDataController<%d>]CDataController: Max_Caculate_Iter is %d, Max_Store_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Max_Caculate_Iter, Max_Store_Iter)
            }

        }.elsewhen(calculate_state === s_cal_working){
            //阶段2，计算开始，计算对Scarchpad的取数地址

            //循环的最外层是M，然后是N
            io.FromScarchPadIO.ReadBankAddr := 0.U.asTypeOf(io.FromScarchPadIO.ReadBankAddr)
            val load_addr =  M_Iterator * N_IteratorMax + N_Iterator

            when(io.ComputeGo && CVectorCount < Max_Caculate_Iter){
                //计算取数地址
                ReadRequest := true.B
                io.FromScarchPadIO.ReadBankAddr.map(_.valid := true.B)
                io.FromScarchPadIO.ReadBankAddr.map(_.bits := load_addr)
                N_Iterator := N_Iterator + 1.U
                when(N_Iterator === N_IteratorMax - 1.U){
                    N_Iterator := 0.U
                    M_Iterator := M_Iterator + 1.U
                    when(M_Iterator === M_IteratorMax - 1.U){
                        //如果M迭代器到达最大值，那么我们就结束计算
                        M_Iterator := 0.U
                        K_Iterator := K_Iterator + 1.U
                    }
                }
            }.otherwise{
                io.FromScarchPadIO.ReadBankAddr := 0.U.asTypeOf(io.FromScarchPadIO.ReadBankAddr)
            }

            val Response_valid = ScarchPadReadResponseData.map(_.valid).reduce(_&&_)
            val Response_data = Wire(Vec(CScratchpadNBanks, (UInt(CScratchpadEntryBitSize.W))))
            for (i <- 0 until CScratchpadNBanks){
                Response_data(i) := ScarchPadReadResponseData(i).bits
            }
            when(Response_valid|| ReadScarchPadDataHoldValid){
                io.Matrix_C.valid := true.B
                io.Matrix_C.bits := Mux(ReadScarchPadDataHoldValid,ReadScarchPadDataHoldReg,Response_data.asUInt) 
            }


            when(io.Matrix_C.fire && io.ComputeGo)
            {
                //只有当数据被消耗的时候，才会增加AVectorCount
                CVectorCount := CVectorCount + 1.U
                ReadScarchPadDataHoldValid := false.B   //只要数据被消耗，肯定优先消耗holdreg的数据
                when(CVectorCount === Max_Caculate_Iter - 1.U){//如果数据全部被消耗，那么我们就输出调试的读数完成的信息
                    if (YJPCDCDebugEnable)
                    {
                        printf("[CDataController<%d>]CDataController: CVectorCount is %d, we finish the load work\n",io.DebugInfo.DebugTimeStampe, CVectorCount)
                    }
                }
                //输出AVectorCount，VectorA的信息
                if (YJPCDCDebugEnable)
                {
                    printf("[CDataController<%d>]CDataController: CVectorCount is %d,CMatrix is %x\n",io.DebugInfo.DebugTimeStampe, CVectorCount,io.Matrix_C.bits)
                }
            }.elsewhen(io.Matrix_C.valid && !io.Matrix_C.ready && !io.ComputeGo && Response_valid){
                //如果数据没有被消耗，那么我们就要保存ScarchPad的数据
                //但我们得看看ScarchPad的数据是不是有效的
                ReadScarchPadDataHoldReg := Response_data.asUInt
                ReadScarchPadDataHoldValid := true.B
            }.elsewhen(io.Matrix_C.valid && !io.Matrix_C.ready && !io.ComputeGo && ReadScarchPadDataHoldValid)
            {
                //如果数据没有被消耗，且我们HlodReg中有数据，我们就继续Hlod这份数据
                ReadScarchPadDataHoldReg := ReadScarchPadDataHoldReg
                ReadScarchPadDataHoldValid := true.B
            }

            when(io.ResultMatrix_D.valid)
            {
                
                io.FromScarchPadIO.WriteBankAddr.map(_.valid := true.B)
                io.FromScarchPadIO.WriteRequestData.map(_.valid := true.B)
                val SCP_Wrie_data = Wire((Vec(CScratchpadNBanks, (UInt(CScratchpadEntryBitSize.W)))))
                SCP_Wrie_data := io.ResultMatrix_D.bits.asTypeOf(SCP_Wrie_data)
                val transpose_data = Wire((Vec(CScratchpadNBanks, Vec(CScratchpadNBanks, (UInt(ResultWidth.W))))))
                for (i <- 0 until CScratchpadNBanks){
                    for (j <- 0 until CScratchpadNBanks){
                        transpose_data(i)(j) := SCP_Wrie_data(j)((i+1)*ResultWidth - 1 , i*ResultWidth)
                    }
                }
                
                for (i <- 0 until CScratchpadNBanks){
                    when(Is_Transpose && Is_AfterOps_Tile && result_K_Iterator === K_IteratorMax - 1.U)
                    {
                        //如果需要转置，那么我们就转置数据
                        io.FromScarchPadIO.WriteRequestData(i).bits := transpose_data(i).asUInt
                    }.otherwise{
                        //否则，我们就直接写入数据
                        io.FromScarchPadIO.WriteRequestData(i).bits := SCP_Wrie_data(i)
                    }
                }
                
                
                WriteRequset := true.B//此时只要数据有效，就喜欢进行写SCP
                io.ResultMatrix_D.ready := true.B
                
                val store_addr = WireInit(addr_Iterator)
                io.FromScarchPadIO.WriteBankAddr.map(_.bits := store_addr)//写地址就是DVector的计数
                when(io.ResultMatrix_D.fire){
                    addr_Iterator := addr_Iterator + 1.U
                    when(addr_Iterator === addr_IteratorMax - 1.U){
                        //如果地址迭代器到达最大值，那么我们就结束计算
                        addr_Iterator := 0.U
                        result_K_Iterator := result_K_Iterator + 1.U
                    }
                    DVectorCount := DVectorCount + 1.U
                    if (YJPCDCDebugEnable)
                    {
                        printf("[CDataController<%d>]CDataController: DVectorCount is %d,Data is %x,store_addr is %x\n", io.DebugInfo.DebugTimeStampe, DVectorCount, io.ResultMatrix_D.bits,store_addr)
                    }
                    when(DVectorCount === Max_Caculate_Iter - 1.U)
                    {//执行计算时，一直是全精度的数据，所以用Max_Caculate_Iter作为迭代器的结束条件
                        calculate_state := s_cal_end
                    }
                }
            }
            
        }.elsewhen(calculate_state === s_cal_end){
            //当前计算任务结束，等待TaskCtrl的确认
            io.ConfigInfo.MicroTaskEndValid := true.B
            when(io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady){
                //TaskCtrl确认后，我们就可以进入下一个任务了
                state := s_idle
                calculate_state := s_cal_idle
                if (YJPCDCDebugEnable)
                {
                    printf("[CDataController<%d>]CDataController: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
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
    
    //像Scrartchpad请求数据的汇总
    //使用各自的值进行拼接，最低位代表Read，次低位代表Write，最高位代表Memory的Write
    //叫给Scarchpad来进行仲裁
    //TODO:这里最好是拼一个正常的
    val request = Wire(new ScaratchpadTask)
    request.ReadFromMemoryLoader := false.B
    request.WriteFromMemoryLoader := false.B
    request.WriteFromDataController := WriteRequset
    request.ReadFromDataController := ReadRequest
    io.FromScarchPadIO.ReadWriteRequest := request.asUInt
    
}
