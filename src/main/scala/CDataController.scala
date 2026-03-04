
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._


//代表对MatrixTE供数的供数逻辑控制单元，隶属于TE，负责选取 MatrixReg、选取行，向TE供数。
//主要问题在如何设计MatrixReg，在为两种模式供数时(矩阵乘运算和卷积运算)，不存在bank冲突，数据每拍都能完整供应上。
//本模块的核心设计是以ConfigInfo为输入进行配置的，以模块内部寄存器为基础的，长时间运行的取数地址计算和状态机设计。
class CDataController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{

        //先整一个 MatrixReg 的接口的总体设计
        val FromMatrixRegIO = Flipped(new CDataControlMatrixRegIO)
        val ConfigInfo = Flipped(new CDCMicroTaskConfigIO)
        val Matrix_C = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
        val ResultMatrix_D = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
        val ComputeGo = Input(Bool())//由TE发出的计算同步锁步信号，指可以接收新的数据了
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
    })

    io.Matrix_C.valid := false.B
    val ResponseData = Wire(Vec(CMatrixRegNBanks, UInt(CMatrixRegEntryBitSize.W)))
    for (i <- 0 until CMatrixRegNBanks) {
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

    val MatrixRegReadResponseData = io.FromMatrixRegIO.ReadResponseData //1周期的延迟
    // val MatrixRegChosen = io.FromMatrixRegIO.Chosen

    val ConfigInfo = io.ConfigInfo
    val CurrentMatrixRegId = RegInit(0.U(CMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    if (EnableDifftest) {
      val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent, delay = 0, dontCare = true)
        // 默认值初始化
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        difftestAmuFinish.index := 3.U
        difftestAmuFinish.valid := (io.FromMatrixRegIO.WriteBankAddr.map(_.valid).reduce(_||_)
          || (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        for (i <- 0 until ABMatrixRegNBanks) {
          difftestAmuFinish.bankValid(i) := io.FromMatrixRegIO.WriteBankAddr(i).valid
          difftestAmuFinish.bankAddr(i) := io.FromMatrixRegIO.WriteBankAddr(i).bits
          difftestAmuFinish.data(i * 4 + 0) := io.FromMatrixRegIO.WriteRequestData(i).bits(63,0)
          difftestAmuFinish.data(i * 4 + 1) := io.FromMatrixRegIO.WriteRequestData(i).bits(127,64)
          difftestAmuFinish.data(i * 4 + 2) := io.FromMatrixRegIO.WriteRequestData(i).bits(191,128)
          difftestAmuFinish.data(i * 4 + 3) := io.FromMatrixRegIO.WriteRequestData(i).bits(255,192)
        }
        difftestAmuFinish.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    }

    //任务状态机 先来个简单的，顺序遍历所有bank，返回数据
    val s_idle :: s_mm_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    //计算状态机，用来配合流水线刷新
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val MatrixRegWorkingTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_N = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegWorkingTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    

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
                printf("[CDataController<%d>]CDataController: ConfigInfo is valid! MatrixRegWorkingTensor_M = %d,MatrixRegWorkingTensor_N = %d,MatrixRegWorkingTensor_K = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_N, ConfigInfo.MatrixRegTensor_K)
                printf("[CDataController<%d>]CDataController: Is_Transpose = %d,Is_AfterOps_Tile = %d,Is_Reorder_Only_Ops = %d,Is_EasyScale_Only_Ops = %d,Is_VecFIFO_Ops = %d,D_Datatype = %d\n",io.DebugInfo.DebugTimeStampe, ConfigInfo.Is_Transpose, ConfigInfo.Is_AfterOps_Tile, ConfigInfo.Is_Reorder_Only_Ops, ConfigInfo.Is_EasyScale_Only_Ops, ConfigInfo.Is_VecFIFO_Ops, ConfigInfo.ApplicationTensor_D.dataType)
            }
            state := s_mm_task  //切换到矩阵乘状态
            MatrixRegWorkingTensor_M := ConfigInfo.MatrixRegTensor_M / Matrix_MN.U * Matrix_MN.U + (ConfigInfo.MatrixRegTensor_M % Matrix_MN.U =/= 0.U) * Matrix_MN.U    //当前执行的矩阵乘任务的M, 取4的整数

            MatrixRegWorkingTensor_N := ConfigInfo.MatrixRegTensor_N    //当前执行的矩阵乘任务的N
            MatrixRegWorkingTensor_K := ConfigInfo.MatrixRegTensor_K    //当前执行的矩阵乘任务的K的ReduceVector的数量
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            

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

    //数据在C MatrixReg中的编排
    //数据会先排N，再排M
    //   N 0 1 2 3 4 5 6 7              CMatrixRegData里的排布
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
    //首先MatrixReg的数据有Tensor_M*Tensor_K个，每个数据是ReduceWidth位
    //然后我们要把这些数据送入TE，每次送入的数据是Matrix_M个，每个数据是Matrix_N*ReduceWidth位
    //我们的MatrixReg是先排K再排M，所以我们的数据送入也是先送K再送M，每次送完一批K，重复Tensor_N/Matrix_N次，再切换M
    val M_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val N_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val K_Iterator = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))

    // 计算数据结果回填
    val addr_Iterator = RegInit(0.U(32.W)) //地址迭代器
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
    //对MatrixReg的数据请求
    val ReadRequest = WireInit(false.B)
    val WriteRequset = WireInit(false.B)

    val ReadMatrixRegDataHoldReg = Reg(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)) //保存MatrixReg的数据，当发生MTE的NACK时，可以不需要重新从MatrixReg读数；仅当ReadMatrixRegDataHoldValid为true时被使用，故无需初始化
    val ReadMatrixRegDataHoldValid = RegInit(false.B) //保存MatrixReg的数据，当发生MTE的NACK时，可以不需要重新从MatrixReg读数

    //如果是mm_task,且计算状态机是init，那么就开始初始化
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
                //阶段1，计算初始化完成，开始工作
                calculate_state := s_cal_working

                if (YJPCDCDebugEnable)
                {
                    //Max_Caculate_Iter
                    //Max_Store_Iter
                    printf("[CDataController<%d>]CDataController: Max_Caculate_Iter is %d, Max_Store_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Max_Caculate_Iter, Max_Store_Iter)
                }
            }
            is(s_cal_working) {
                //阶段2，计算开始，计算对MatrixReg的取数地址

                //循环的最外层是M，然后是N
                val load_addr =  M_Iterator * N_IteratorMax + N_Iterator
                dontTouch(load_addr)

                when(io.ComputeGo && CVectorCount < Max_Caculate_Iter){
                    //计算取数地址
                    ReadRequest := true.B
                    io.FromMatrixRegIO.ReadBankAddr.map(_.valid := true.B)
                    io.FromMatrixRegIO.ReadBankAddr.map(_.bits := load_addr)
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
                    //只有当数据被消耗的时候，才会增加AVectorCount
                    CVectorCount := CVectorCount + 1.U
                    ReadMatrixRegDataHoldValid := false.B   //只要数据被消耗，肯定优先消耗holdreg的数据
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
                    //如果数据没有被消耗，那么我们就要保存MatrixReg的数据
                    //但我们得看看MatrixReg的数据是不是有效的
                    ReadMatrixRegDataHoldReg := ResponseData.asUInt
                    ReadMatrixRegDataHoldValid := true.B
                }.elsewhen(io.Matrix_C.valid && !io.Matrix_C.ready && !io.ComputeGo && ReadMatrixRegDataHoldValid)
                {
                    //如果数据没有被消耗，且我们HlodReg中有数据，我们就继续Hlod这份数据
                    ReadMatrixRegDataHoldReg := ReadMatrixRegDataHoldReg
                    ReadMatrixRegDataHoldValid := true.B
                }

                when(io.ResultMatrix_D.valid)
                {
                    
                    io.FromMatrixRegIO.WriteBankAddr.map(_.valid := true.B)
                    io.FromMatrixRegIO.WriteRequestData.map(_.valid := true.B)
                    val MReg_Wrie_data = Wire((Vec(CMatrixRegNBanks, (UInt(CMatrixRegEntryBitSize.W)))))
                    MReg_Wrie_data := io.ResultMatrix_D.bits.asTypeOf(MReg_Wrie_data)
                    val transpose_data = Wire((Vec(CMatrixRegNBanks, Vec(CMatrixRegNBanks, (UInt(ResultWidth.W))))))
                    for (i <- 0 until CMatrixRegNBanks){
                        for (j <- 0 until CMatrixRegNBanks){
                            transpose_data(i)(j) := MReg_Wrie_data(j)((i+1)*ResultWidth - 1 , i*ResultWidth)
                        }
                    }
                    
                    for (i <- 0 until CMatrixRegNBanks){
                        when(Is_Transpose && Is_AfterOps_Tile && result_K_Iterator === K_IteratorMax - 1.U)
                        {
                            //如果需要转置，那么我们就转置数据
                            io.FromMatrixRegIO.WriteRequestData(i).bits := transpose_data(i).asUInt
                        }.otherwise{
                            //否则，我们就直接写入数据
                            io.FromMatrixRegIO.WriteRequestData(i).bits := MReg_Wrie_data(i)
                        }
                    }
                    
                    
                    WriteRequset := true.B//此时只要数据有效，就喜欢进行写MReg
                    io.ResultMatrix_D.ready := true.B
                    
                    val store_addr = WireInit(addr_Iterator)
                    io.FromMatrixRegIO.WriteBankAddr.map(_.bits := store_addr)//写地址就是DVector的计数
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
            }
            is(s_cal_end) {
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
            }
            is(s_cal_idle) {
                //计算状态机空闲
                //加速器闲闲没事做
            }
        }
    }
    
    //像MatrixReg请求数据的汇总
    //使用各自的值进行拼接，最低位代表Read，次低位代表Write，最高位代表Memory的Write
    //叫给MatrixReg来进行仲裁
    //TODO:这里最好是拼一个正常的
    val request = Wire(new MatrixRegTask)
    request.ReadFromMemoryLoader := false.B
    request.WriteFromMemoryLoader := false.B
    request.WriteFromDataController := WriteRequset
    request.ReadFromDataController := ReadRequest
    io.FromMatrixRegIO.ReadWriteRequest := request.asUInt
    
}
