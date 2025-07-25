package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
import boom.v3.util._

class FakeVPU(implicit p: Parameters) extends Module with HWParameters{
    val io = IO(new Bundle{
        val VPUInterface  = Flipped(new VPUInterfaceIO)
    })

    io.VPUInterface.VPU_Input.ready := false.B
    io.VPUInterface.VPU_Output.bits.stream_data := 0.U
    io.VPUInterface.VPU_Output.bits.stream_id := 0.U
    io.VPUInterface.VPU_Output.valid := false.B

    val fake = Module(new Queue(UInt(VectorWidth.W),4))
    fake.io.enq.valid := io.VPUInterface.VPU_Input.valid
    fake.io.enq.bits := io.VPUInterface.VPU_Input.bits.inst_src0
    io.VPUInterface.VPU_Input.ready := fake.io.enq.ready

    fake.io.deq.ready := io.VPUInterface.VPU_Output.ready
    io.VPUInterface.VPU_Output.bits.stream_data := fake.io.deq.bits
    io.VPUInterface.VPU_Output.bits.stream_id := 0.U
    io.VPUInterface.VPU_Output.valid := fake.io.deq.valid

}

class VectorStreamInterface(implicit p: Parameters) extends Module with HWParameters{
    val io = IO(new Bundle{
        val VectorInterface = Flipped(new VectorInterfaceIO)//与AfterOps的数据交互接口
        val VPUInterface       = (new VPUInterfaceIO)//与VPU的数据交互接口
    })

    // val VecTask = DecoupledIO(UInt(log2Ceil(VecTaskInstBufferSize).W))
    // val VectorDataIn     = DecoupledIO(UInt((VectorWidth).W))
    // val VectorDataOut     = Flipped(DecoupledIO(UInt((VectorWidth).W)))
    // io.VectorInterface.VecTask.bits := 0.U
    io.VectorInterface.VecTask.ready := false.B
    io.VectorInterface.VectorDataIn.ready := false.B
    io.VectorInterface.VectorDataOut.valid := false.B
    io.VectorInterface.VectorDataOut.bits := 0.U
    io.VPUInterface.VPU_Input.bits := 0.U.asTypeOf(io.VPUInterface.VPU_Input.bits)
    io.VPUInterface.VPU_Input.valid := false.B
    io.VPUInterface.VPU_Output.ready := false.B

    //queue
    val VecFIFO = Module(new Queue(UInt(VectorWidth.W),VecTaskInstBufferDepth))
    
    //enqueue
    io.VectorInterface.VectorDataIn.ready := VecFIFO.io.enq.ready
    VecFIFO.io.enq.valid := io.VectorInterface.VectorDataIn.valid
    VecFIFO.io.enq.bits := io.VectorInterface.VectorDataIn.bits

    io.VPUInterface.VPU_Input.valid := VecFIFO.io.deq.valid
    VecFIFO.io.deq.ready := io.VPUInterface.VPU_Input.ready

    //to VPU
    //译码递送到vpu
    val inst_reg = RegInit(0.U(VecTaskInstBufferSize.W))
    io.VPUInterface.VPU_Input.bits.inst_src0 := VecFIFO.io.deq.bits
    io.VPUInterface.VPU_Input.bits.inst_src0_type := 0.U//from TC data stream
    io.VPUInterface.VPU_Input.bits.inst_src1 := 0.U
    io.VPUInterface.VPU_Input.bits.inst_src1_type := 0.U


    //译码Stream码流，递送uop。
    //stream码流就是TC的SCP中的数据流形式。
    //stream码流有固定的阶段，配置阶段(初始化TC驻留VPU的VREG和设置VEC Config)、计算阶段(根据数据流配置，递送数据；同时接受返回的数据)、结束阶段(接受驻留统计VREG的结果，并完成CSCP的同步)。
                                      

    //dequeue
    io.VectorInterface.VectorDataOut.valid := VecFIFO.io.deq.valid
    VecFIFO.io.deq.ready := io.VectorInterface.VectorDataOut.ready
    io.VectorInterface.VectorDataOut.bits := VecFIFO.io.deq.bits
}


//该模块的核心是4个FIFO
//1.用于暂存从CDC来的数据流                                         = Ready_to_Get_toVector->Reorder_ToVector
//2.用于暂存的从(1.)数据流转换而来的VPU可用的数据流,并递送给VPU          = Reorder_ToVector->Ready_to_Send
//3.用于暂存从VPU来的数据流                                         = Reorder_ToSCP_Reg_Ready_Get->Reorder_ToSCP
//4.用于暂存从(3.)数据流转换而来的CDC可用的数据流,并递送给CDC           = Reorder_ToSCP->Ready_to_Return

//核心的逻辑是：
//1.如何接受CDC的数据流
//2.*如何重组CDC的数据流成为VPU可用的数据流，VPU的处理带宽，VPU的执行微码，VPU的配置等
//3.如何接受VPU的数据流
//4.如何重组VPU的数据流成为CDC可用的数据流，CDC的处理带宽，写回CDC时的CDC的写回地址等

//在2.*的逻辑中，已经确定好了VPU可用的数据流，这就绝定了送入VPU时，VPU的Stream码流的形式。
//目前准备支持的融合码流有两种，这两种码流的整体延迟相对于解耦的任意形式码流而言，都是最低的。
//这两种码流实际上是一种码流，只是是否执行了Trasnpose，这也导致了数据流对应的矩阵元素的索引不同。
//VPU输入——常规融合码流：
//    <--------VLen---------->
// M=0 [0,1,2,3,4,5,6,7]        cycle 0 
// M=1 [0,1,2,3,4,5,6,7]        cycle 1
// M=2 [0,1,2,3,4,5,6,7]        cycle 2
// M=3 [0,1,2,3,4,5,6,7]        cycle 3
// M=0 [8,9,10,11,12,13,14,15]  cycle 4
// M=1 [8,9,10,11,12,13,14,15]  cycle 5
// M=2 [8,9,10,11,12,13,14,15]  cycle 6
// .........
//M_index [N_index,N_index+1,N_index+2,N_index+3,N_index+4,N_index+5,N_index+6,N_index+7] cycle x
// .........


//VPU输入——转置融合码流：
//    <--------VLen---------->
// N=0 [0,1,2,3,4,5,6,7]        cycle 0
// N=1 [0,1,2,3,4,5,6,7]        cycle 1
// N=2 [0,1,2,3,4,5,6,7]        cycle 2
// N=3 [0,1,2,3,4,5,6,7]        cycle 3
// N=0 [8,9,10,11,12,13,14,15]  cycle 4
// N=1 [8,9,10,11,12,13,14,15]  cycle 5
// N=2 [8,9,10,11,12,13,14,15]  cycle 6
// .........
//N_index [M_index,M_index+1,M_index+2,M_index+3,M_index+4,M_index+5,M_index+6,M_index+7] cycle x
//

//VPU输入码流的不同，导致了VPU的输出码流的不同，另外，VPU的计算结果也会导致输出码流的变化*，比如发生量化，会导致数据码流量会变少
//*不同码流的VPU的执行算子也不同！
//VPU输出——常规融合码流：
//    <--------VLen---------->(32bit->32bit)        //    <--------VLen---------->(32bit->8bit)
// M=0 [0,1,2,3,4,5,6,7]        cycle 0             // M=0 [0~7,8~15,16~23,24~31]        cycle 0 
// M=1 [0,1,2,3,4,5,6,7]        cycle 1             // M=1 [0~7,8~15,16~23,24~31]        cycle 1
// M=2 [0,1,2,3,4,5,6,7]        cycle 2             // M=2 [0~7,8~15,16~23,24~31]        cycle 2
// M=3 [0,1,2,3,4,5,6,7]        cycle 3             // M=3 [0~7,8~15,16~23,24~31]        cycle 3
// M=0 [8,9,10,11,12,13,14,15]  cycle 4             // M=0 [32~39,40~47,48~55,56~63]     cycle 4
// M=1 [8,9,10,11,12,13,14,15]  cycle 5             // M=1 [32~39,40~47,48~55,56~63]     cycle 5
// M=2 [8,9,10,11,12,13,14,15]  cycle 6             // M=2 [32~39,40~47,48~55,56~63]     cycle 6

//VPU输出——转置融合码流：
//    <--------VLen---------->(32bit->32bit)        //    <--------VLen---------->(32bit->8bit)
// N=0 [0,1,2,3,4,5,6,7]        cycle 0             // N=0 [0~7,8~15,16~23,24~31]        cycle 0
// N=1 [0,1,2,3,4,5,6,7]        cycle 1             // N=1 [0~7,8~15,16~23,24~31]        cycle 1
// N=2 [0,1,2,3,4,5,6,7]        cycle 2             // N=2 [0~7,8~15,16~23,24~31]        cycle 2
// N=3 [0,1,2,3,4,5,6,7]        cycle 3             // N=3 [0~7,8~15,16~23,24~31]        cycle 3
// N=0 [8,9,10,11,12,13,14,15]  cycle 4             // N=0 [32~39,40~47,48~55,56~63]     cycle 4
// N=1 [8,9,10,11,12,13,14,15]  cycle 5             // N=1 [32~39,40~47,48~55,56~63]     cycle 5
// N=2 [8,9,10,11,12,13,14,15]  cycle 6             // N=2 [32~39,40~47,48~55,56~63]     cycle 6


//VPU的输出码流的不同，导致了CDC的输入码流的不同，导致了我们生成的CDC写回地址不同

//CDC输入——常规融合码流：
//    <--------CDC_BandWidth------------------->(32bit->32bit)                //    <-----------------CDC_BandWidth------------------>(32bit->8bit)
// M=0 [0,1,2,3,4,5,6,7],[8,9,10,11,12,13,14,15]        cycle 0               // M=0 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 0
// M=1 [0,1,2,3,4,5,6,7],[8,9,10,11,12,13,14,15]        cycle 1               // M=1 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 1
// M=2 [0,1,2,3,4,5,6,7],[8,9,10,11,12,13,14,15]        cycle 2               // M=2 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 2
// M=3 [0,1,2,3,4,5,6,7],[8,9,10,11,12,13,14,15]        cycle 3               // M=3 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 3
// M=0 [16~23],[24~31]                                  cycle 4               // M=4 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 4
// M=1 [16~23],[24~31]                                  cycle 5               // M=5 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 5
// M=2 [16~23],[24~31]                                  cycle 6               // M=6 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 6
// M=3 [16~23],[24~31]                                  cycle 7               // M=7 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 7
// M=0 [32~39],[40~47]                                  cycle 8               // M=8 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 8
// M=1 [32~39],[40~47]                                  cycle 9               // M=9 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]        cycle 9
// M=2 [32~39],[40~47]                                  cycle 10              // M=10 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 10
// M=3 [32~39],[40~47]                                  cycle 11              // M=11 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 11
// M=0 [48~55],[56~63]                                  cycle 12              // M=12 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 12
// M=1 [48~55],[56~63]                                  cycle 13              // M=13 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 13
// M=2 [48~55],[56~63]                                  cycle 14              // M=14 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 14
// M=3 [48~55],[56~63]                                  cycle 15              // M=15 [0~7,8~15,16~23,24~31],[32~39,40~47,48~55,56~63]       cycle 15
// .........                                            .........             //                                                             ........
// .........                                            cycle 256             // .........                                                   cycle 64

class AfterOpsModule(implicit p: Parameters) extends Module with HWParameters{

    val io = IO(new Bundle{
        //先整一个ScarchPad的接口的总体设计
        val ConfigInfo = Flipped(new AfterOpsMicroTaskConfigIO)
        val AfterOpsInterface = Flipped(new AfterOpsInterface)//与CDC的数据交互接口
        val VectorInterface = (new VectorInterfaceIO)//与VPU的数据交互接口
        val DebugInfo = Input(new DebugInfoIO)
    })

    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B
    io.AfterOpsInterface.CDCDataToInterface.ready := false.B
    io.AfterOpsInterface.InterfaceToCDCData.valid := false.B
    io.AfterOpsInterface.InterfaceToCDCData.bits  := 0.U
    // io.AfterOpsInterface.CDCStoreAddr := 0.U


    io.VectorInterface.VectorDataOut.ready := false.B
    io.VectorInterface.VectorDataIn.valid := false.B
    io.VectorInterface.VectorDataIn.bits := 0.U
    io.VectorInterface.VecTask.bits := 0.U
    io.VectorInterface.VecTask.valid := false.B





    //接受后操作的任务，有可能是重排序，有可能是缩放，有可能是转置，有可能是其他复杂后操作任务
    val Is_Transpose                        = RegInit(false.B)      //是否需要转置
    val Is_Reorder_Only_Ops                 = RegInit(false.B)      //是否只是重排，不需要计算
    val Is_EasyScale_Only_Ops               = RegInit(false.B)      //是否只是简单的缩放，不需要额外的后操作计算
    val Is_VecFIFO_Ops                      = RegInit(false.B)      //是否真的需要通用VecFIFO的参与


    //任务状态机
    val s_idle :: s_afterOps_task :: Nil = Enum(2)
    val state = RegInit(s_idle)

    //计算状态机，用来配合流水线刷新
    val s_cal_idle :: s_cal_init :: s_cal_working :: s_cal_end :: Nil = Enum(4)
    val calculate_state = RegInit(s_cal_idle)
    val ScaratchpadWorkingTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ScaratchpadWorkingTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val D_Datatype = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))

    //输出state，用于debug
    when(io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady)//当前配置的指令有效
    {
        if(YJPAfterOpsDebugEnable)
        {
            printf("[AfterOps<%d>]AfterOps: state is %d\n",io.DebugInfo.DebugTimeStampe, state)
            printf("[AfterOps<%d>]AfterOps: calculate_state is %d\n",io.DebugInfo.DebugTimeStampe, calculate_state)
        }
    }

    //任务状态机
    when(state === s_idle)
    {
        io.ConfigInfo.MicroTaskReady := true.B
        when(io.ConfigInfo.MicroTaskValid && io.ConfigInfo.MicroTaskReady)//当前配置的指令有效
        {
            state := s_afterOps_task
            Is_Transpose := io.ConfigInfo.Is_Transpose
            Is_Reorder_Only_Ops := io.ConfigInfo.Is_Reorder_Only_Ops
            Is_EasyScale_Only_Ops := io.ConfigInfo.Is_EasyScale_Only_Ops
            Is_VecFIFO_Ops := io.ConfigInfo.Is_VecFIFO_Ops

            ScaratchpadWorkingTensor_M := io.ConfigInfo.ScaratchpadTensor_M
            ScaratchpadWorkingTensor_N := io.ConfigInfo.ScaratchpadTensor_N

            D_Datatype := io.ConfigInfo.ApplicationTensor_D.dataType

            calculate_state := s_cal_init
            if(YJPAfterOpsDebugEnable)
            {
                printf("[AfterOps<%d>]AfterOps: Is_Transpose is %d, Is_Reorder_Only_Ops is %d, Is_EasyScale_Only_Ops is %d, Is_VecFIFO_Ops is %d\n",io.DebugInfo.DebugTimeStampe, io.ConfigInfo.Is_Transpose, io.ConfigInfo.Is_Reorder_Only_Ops, io.ConfigInfo.Is_EasyScale_Only_Ops, io.ConfigInfo.Is_VecFIFO_Ops)
                printf("[AfterOps<%d>]AfterOps: ScaratchpadWorkingTensor_M is %d, ScaratchpadWorkingTensor_N is %d\n",io.DebugInfo.DebugTimeStampe, io.ConfigInfo.ScaratchpadTensor_M, io.ConfigInfo.ScaratchpadTensor_N)
            }

            assert(Mux(Is_Transpose,io.ConfigInfo.ScaratchpadTensor_M===Tensor_M.U,io.ConfigInfo.ScaratchpadTensor_N===Tensor_N.U), "ScaratchpadWorkingTensor_N or ScaratchpadWorkingTensor_M is not Full!")
        }
    }


    val Get_M_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val Get_N_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

    val M_Get_IteratorMax = (ScaratchpadWorkingTensor_M / Matrix_M.U) + ((ScaratchpadWorkingTensor_M % Matrix_M.U) =/= 0.U)
    val N_Get_IteratorMax = (ScaratchpadWorkingTensor_N / Matrix_N.U)

    val Max_Caculate_Iter = M_Get_IteratorMax * N_Get_IteratorMax

    val GetCount = RegInit(0.U(32.W))


    //一组reoder的寄存器，来存储重排的数据
    //存一个Matrix_M*Matrix_N*T的数据矩阵寄存器，用于存储重排的数据，T = CSCP的总带宽/Matrix_N*ResultWidth
    val Per_GetMatrix_NDim_Width = Matrix_N*ResultWidth //每次Get的N连续的数据的宽度
    val Reorder_ToVector_GroupSize = VectorWidth / (Per_GetMatrix_NDim_Width)//填满一个VectorWidth需要这么多次
    val Reorder_ToVector_Reg = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(Matrix_M)(VecInit(Seq.fill(Reorder_ToVector_GroupSize)(0.U((Per_GetMatrix_NDim_Width).W))))))))
    val Reorder_ToVector_Reg_Valid = RegInit(VecInit(Seq.fill(2)(false.B)))
    val Reorder_ToVector_Reg_Get_Index  = RegInit(0.U(log2Ceil(2).W))//双缓冲
    val Reorder_ToVector_Reg_Send_Index = RegInit(0.U(log2Ceil(2).W))//双缓冲

    //分组描述数据，将打包分组，然后进行重排
    val Fill_Vector_Iter = RegInit(0.U(log2Ceil(Reorder_ToVector_GroupSize).W))
    val Fill_Vector_Max_Iter = Reorder_ToVector_GroupSize

    val Send_Vector_Iter = RegInit(0.U(log2Ceil(Matrix_M).W))
    val Send_Vector_Max_Iter = Matrix_M


    val Return_M_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val Return_N_Iterator = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val ReturnCount = RegInit(0.U(32.W))

    //VPU回到interface，需要填充到CDC同宽度的数据
    val Per_Return_Vector_GroupSize = CScratchpad_Total_Bandwidth_Bit / VectorWidth //每次Return会接受多少个VectorWidth的数据
    val Reorder_ToSCP_Reg = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(Matrix_M)(VecInit(Seq.fill(Per_Return_Vector_GroupSize)((0.U(VectorWidth.W)))))))))
    val Reorder_ToSCP_Reg_Valid = RegInit(VecInit(Seq.fill(2)(false.B)))
    val Reorder_ToSCP_Reg_Get_Index  = RegInit(0.U(log2Ceil(2).W))//双缓冲
    val Reorder_ToSCP_Reg_Return_Index = RegInit(0.U(log2Ceil(2).W))//双缓冲

    val Fill_SCP_Iter = RegInit(0.U(log2Ceil(Per_Return_Vector_GroupSize).W))
    val Fill_SCP_Max_Iter = Per_Return_Vector_GroupSize

    val Fill_Return_M_Iter = RegInit(0.U(log2Ceil(Matrix_M).W))
    val Fill_Return_M_Iter_Max = Matrix_M

    val Send_Return_Sub_Major_Iter = RegInit(0.U(log2Ceil(Matrix_M).W))
    val Send_Return_Sub_Major_Iter_Max = Matrix_M
    
    
    val Reduce_Dim_Fill_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val Major_Dim_Fill_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
    val Reduce_Dim_Iterator_Max = Tensor_N.U * D_Datatype * 8.U / CScratchpad_Total_Bandwidth_Bit.U //Reduce_Dim需要连续填几次
    val Major_Dim_Iterator_Max = Mux(!Is_Transpose,M_Get_IteratorMax,N_Get_IteratorMax) //Major_Dim需要连续填几次
    val Next_Major_Dim_Addr_Inc = Reduce_Dim_Iterator_Max

    
    //如果是mm_task,且计算状态机是init，那么就开始初始化
    when(state === s_afterOps_task){
        when(calculate_state === s_cal_init){
            Get_M_Iterator := 0.U
            Get_N_Iterator := 0.U
            Return_M_Iterator := 0.U
            Return_N_Iterator := 0.U
            GetCount := 0.U
            ReturnCount := 0.U
            Reorder_ToVector_Reg_Valid := 0.U.asTypeOf(Reorder_ToVector_Reg_Valid)
            Reorder_ToVector_Reg_Get_Index := 0.U
            Reorder_ToVector_Reg := 0.U.asTypeOf(Reorder_ToVector_Reg)

            Reorder_ToSCP_Reg_Valid := 0.U.asTypeOf(Reorder_ToSCP_Reg_Valid)
            Reorder_ToSCP_Reg_Get_Index := 0.U
            Reorder_ToSCP_Reg := 0.U.asTypeOf(Reorder_ToSCP_Reg)
            Fill_Vector_Iter := 0.U

            Fill_SCP_Iter := 0.U
            Fill_Return_M_Iter := 0.U
            Reduce_Dim_Fill_Iter := 0.U
            Major_Dim_Fill_Iter := 0.U

            //阶段1，计算初始化完成，开始工作
            calculate_state := s_cal_working

            if (YJPAfterOpsDebugEnable)
            {
                //Max_Caculate_Iter
                //Max_Store_Iter
                printf("[AfterOps<%d>]AfterOps: Max_Caculate_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Max_Caculate_Iter)
                //Reduce_Dim_Iterator_Max
                //Major_Dim_Iterator_Max
                printf("[AfterOps<%d>]AfterOps: Reduce_Dim_Iterator_Max is %d,Major_Dim_Iterator_Max is %d\n",io.DebugInfo.DebugTimeStampe, Reduce_Dim_Iterator_Max,Major_Dim_Iterator_Max)
            }

        }.elsewhen(calculate_state === s_cal_working){
            //阶段2，holding数据，重新编排数据

            val Reorder_ToVector_Reg_Ready_Get = !(Reorder_ToVector_Reg_Valid.reduce(_&&_))//只要有一个不是Valid就是true,表示可以接受CDC的数据
            when(GetCount < Max_Caculate_Iter){
                //每次Get得到一个Matrix_M*Matrix_N的矩阵，共Get Max_Caculate_Iter次
                //需要填满连续的Matrix_M个向量，直到每个向量的长度与向量任务的长度相等
                //然后依据任务类型，进行后操作
                
                io.AfterOpsInterface.CDCDataToInterface.ready := Reorder_ToVector_Reg_Ready_Get
                when(io.AfterOpsInterface.CDCDataToInterface.fire)//接收该数据
                {
                    val GetData_To_Group = WireInit(VecInit(Seq.fill(Matrix_M)(0.U(Per_GetMatrix_NDim_Width.W))))
                    GetData_To_Group := io.AfterOpsInterface.CDCDataToInterface.bits.asTypeOf(GetData_To_Group)

                    //将数据填入Reorder_ToVector_Reg
                    for (i <- 0 until Matrix_M){
                        Reorder_ToVector_Reg(Reorder_ToVector_Reg_Get_Index)(i)(Fill_Vector_Iter) := GetData_To_Group(i)
                    }
                    //更新相关迭代器
                    Fill_Vector_Iter := WrapInc(Fill_Vector_Iter, Fill_Vector_Max_Iter)
                    when(Fill_Vector_Iter === (Fill_Vector_Max_Iter - 1).U){
                        Fill_Vector_Iter := 0.U
                        Reorder_ToVector_Reg_Valid(Reorder_ToVector_Reg_Get_Index) := true.B
                        Reorder_ToVector_Reg_Get_Index := WrapInc(Reorder_ToVector_Reg_Get_Index, 2)
                        //完成一组数据，4*4数据的填充输出这一组数据
                        if (YJPAfterOpsDebugEnable)
                        {
                            val Groups_Iter = GetCount / (Matrix_M.U)
                            // printf("[AfterOps<%d>]AfterOps: Fill data to Reorder_ToVector_Reg, GetCount is %d(Groups %d),Fill_Reg_data is %x\n",io.DebugInfo.DebugTimeStampe, GetCount,Groups_Iter,Reorder_ToVector_Reg(Reorder_ToVector_Reg_Get_Index).asUInt)
                        }
                    }
                    GetCount := GetCount + 1.U
                    if (YJPAfterOpsDebugEnable)
                    {
                        printf("[AfterOps<%d>]AfterOps: Get data from CDC, GetCount is %d\n",io.DebugInfo.DebugTimeStampe, GetCount)
                    }
                }
            }
            val Reorder_ToVector_Reg_Ready_Send= Reorder_ToVector_Reg_Valid.reduce(_||_)//只要有一个是Valid就是ture，表示可以发往后操作执行
            //完成后操作后，数据可能会变短，需要继续填满Matrix_M个向量至CSCP的读写带宽，然后数据返回
            when(Reorder_ToVector_Reg_Ready_Send)
            {
                //将数据发送到向量模块
                io.VectorInterface.VectorDataIn.valid := true.B
                io.VectorInterface.VectorDataIn.bits := Reorder_ToVector_Reg(Reorder_ToVector_Reg_Send_Index)(Send_Vector_Iter).asUInt
                when(io.VectorInterface.VectorDataIn.fire)
                {
                    Send_Vector_Iter := WrapInc(Send_Vector_Iter, Send_Vector_Max_Iter)
                    if (YJPAfterOpsDebugEnable)
                    {
                        // printf("[AfterOps<%d>]AfterOps: Send data to Vector, Send_Vector_Iter is %d,Send_Vector_Data is %x\n",io.DebugInfo.DebugTimeStampe, Send_Vector_Iter,io.VectorInterface.VectorDataIn.bits)
                    }
                    when(Send_Vector_Iter === (Send_Vector_Max_Iter - 1).U){
                        Send_Vector_Iter := 0.U
                        Reorder_ToVector_Reg_Valid(Reorder_ToVector_Reg_Send_Index) := false.B
                        Reorder_ToVector_Reg_Send_Index := WrapInc(Reorder_ToVector_Reg_Send_Index, 2)
                        printf("[AfterOps<%d>]AfterOps: Send Reorder Group finish, Send_Vector_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Send_Vector_Iter)
                    }
                }
            }

            val Reorder_ToSCP_Reg_Ready_Get = !Reorder_ToSCP_Reg_Valid(Reorder_ToSCP_Reg_Get_Index)
            when(Reorder_ToSCP_Reg_Ready_Get){
                io.VectorInterface.VectorDataOut.ready := true.B
                when(io.VectorInterface.VectorDataOut.fire)
                {
                    Reorder_ToSCP_Reg(Reorder_ToSCP_Reg_Get_Index)(Fill_Return_M_Iter)(Fill_SCP_Iter) := io.VectorInterface.VectorDataOut.bits //M不会变，M个向量回来，向量的有效宽度可能发生变化了
                    Fill_Return_M_Iter := WrapInc(Fill_Return_M_Iter, Fill_Return_M_Iter_Max)
                    when(Fill_Return_M_Iter === (Fill_Return_M_Iter_Max - 1).U){
                        Fill_Return_M_Iter := 0.U
                        Fill_SCP_Iter := WrapInc(Fill_SCP_Iter, Fill_SCP_Max_Iter)//这里M个向量一直填，填到能填满C_SCP的写回带宽为止，所以这里填2次
                        when(Fill_SCP_Iter === (Fill_SCP_Max_Iter - 1).U){
                            Fill_SCP_Iter := 0.U
                            Reorder_ToSCP_Reg_Valid(Reorder_ToSCP_Reg_Get_Index) := true.B
                            Reorder_ToSCP_Reg_Get_Index := WrapInc(Reorder_ToSCP_Reg_Get_Index, 2)
                            //输出，成功填满一个C_SCP的回填数据组
                            if (YJPAfterOpsDebugEnable)
                            {
                                val debug_Reorder_ToSCP_Reg = WireInit(VecInit(Seq.fill(Matrix_M)(VecInit(Seq.fill(Per_Return_Vector_GroupSize)((0.U(VectorWidth.W)))))))
                                debug_Reorder_ToSCP_Reg := Reorder_ToSCP_Reg(Reorder_ToSCP_Reg_Get_Index)
                                debug_Reorder_ToSCP_Reg(Fill_Return_M_Iter)(Fill_SCP_Iter) := io.VectorInterface.VectorDataOut.bits
                                // printf("[AfterOps<%d>]AfterOps: Fill data to Reorder_ToSCP_Reg one Groupg down, Fill_Return_M_Iter is %d, Fill_SCP_Iter is %d,Fill_Reg_data is %x\n",io.DebugInfo.DebugTimeStampe, Fill_Return_M_Iter, Fill_SCP_Iter,debug_Reorder_ToSCP_Reg.asUInt)
                                //输出Reorder_ToSCP_Reg_Valid和Reorder_ToSCP_Reg_Get_Index
                                printf("[AfterOps<%d>{Reorder_ToSCP_Reg_Valid}]AfterOps: Reorder_ToSCP_Reg_Valid is %x, Reorder_ToSCP_Reg_Get_Index is %d\n",io.DebugInfo.DebugTimeStampe, Reorder_ToSCP_Reg_Valid.asUInt, Reorder_ToSCP_Reg_Get_Index)
                            }
                        }
                    }
                    if (YJPAfterOpsDebugEnable)
                    {
                        //输出各个迭代器
                        printf("[AfterOps<%d>]AfterOps: Fill data to SCP, Fill_Return_M_Iter is %d, Fill_SCP_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Fill_Return_M_Iter, Fill_SCP_Iter)
                    }
                }
            }

            //CDCStoreAddr，根据是否Transpose，Dataout的element的宽度，DataIn的stream，DataOut的Stream有关
            //首先回来的stream一定是DIM_N first或者DIM_M first，Vector内部肯定有洗牌指令，不需要我们来重排，现在只需要关心，来的数据的顺序即可
            //通常是M/4,4(M),N/VectorWidth,VectorWidth(N)。(如果是Transpose，那么就是N/4,4(N),M/VectorWidth,VectorWidth(M))
            //对应的写回到SCP的地址，需要重新排成M,N/SCPWidth,SCPWidth(N)。
            //VectorWidth不变，所以那些改变数据位宽的操作，需要连续的完成操作，然后再写回到SCP。
            //我们通过Reorder_ToSCP_Reg将N/VectorWidth,VectorWidth(N)，重排成了SCPWidth(N)。
            //不同的VPUInStream和VPUOutStream，需要不同的重排方式，故我们需要分类并计算正确的写回地址

            //Transpose的计算方式得思考思考
            //在ACSP、BCSP里的数据永远是DIM_K First的，Transpose时，要让DIM_M作为连续的数据，故计算时，迭代的地址会发生变化

            // val CDC_Store_addr = WireInit(0.U(log2Ceil(CScratchpadBankNEntrys).W))
            val Reorder_ToSCP_Reg_Ready_Return = Reorder_ToSCP_Reg_Valid(Reorder_ToSCP_Reg_Return_Index)
            when(Reorder_ToSCP_Reg_Ready_Return)
            {
                io.AfterOpsInterface.InterfaceToCDCData.valid := true.B
                io.AfterOpsInterface.InterfaceToCDCData.bits := Reorder_ToSCP_Reg(Reorder_ToSCP_Reg_Return_Index)(Send_Return_Sub_Major_Iter).asTypeOf(io.AfterOpsInterface.InterfaceToCDCData.bits)
                // io.AfterOpsInterface.CDCStoreAddr := (Send_Return_Sub_Major_Iter + Major_Dim_Fill_Iter*Matrix_M.U) * Reduce_Dim_Iterator_Max + Reduce_Dim_Fill_Iter// 写回CSCP的地址
                //输出io.AfterOpsInterface.InterfaceToCDCData.ready
                // if (YJPAfterOpsDebugEnable)
                // {
                //     printf("[AfterOps<%d>]AfterOps: io.AfterOpsInterface.InterfaceToCDCData.ready is %d\n",io.DebugInfo.DebugTimeStampe, io.AfterOpsInterface.InterfaceToCDCData.ready)
                // }
                when(io.AfterOpsInterface.InterfaceToCDCData.fire)
                {
                    Send_Return_Sub_Major_Iter := WrapInc(Send_Return_Sub_Major_Iter, Send_Return_Sub_Major_Iter_Max)
                    when(Send_Return_Sub_Major_Iter === (Send_Return_Sub_Major_Iter_Max - 1).U){
                        Send_Return_Sub_Major_Iter := 0.U
                        Reorder_ToSCP_Reg_Valid(Reorder_ToSCP_Reg_Return_Index) := false.B
                        Reorder_ToSCP_Reg_Return_Index := WrapInc(Reorder_ToSCP_Reg_Return_Index, 2)
                        //输出Reorder_ToSCP_Reg_Valid
                        if (YJPAfterOpsDebugEnable)
                        {
                            //输出Reorder_ToSCP_Reg_Valid和Reorder_ToSCP_Reg_Get_Index
                            printf("[AfterOps<%d>{Reorder_ToSCP_Reg_Valid}]AfterOps: Reorder_ToSCP_Reg_Valid is %x, Reorder_ToSCP_Reg_Return_Index is %d\n",io.DebugInfo.DebugTimeStampe, Reorder_ToSCP_Reg_Valid.asUInt, Reorder_ToSCP_Reg_Return_Index)
                        }

                        Reduce_Dim_Fill_Iter := Reduce_Dim_Fill_Iter + 1.U
                        when(Reduce_Dim_Fill_Iter === Reduce_Dim_Iterator_Max - 1.U){
                            Reduce_Dim_Fill_Iter := 0.U
                            Major_Dim_Fill_Iter := Major_Dim_Fill_Iter + 1.U
                            when(Major_Dim_Fill_Iter === Major_Dim_Iterator_Max - 1.U){
                                Major_Dim_Fill_Iter := 0.U
                                //完成所有的计算，进入下一个任务
                                calculate_state := s_cal_end
                            }
                        }
                    }
                    if (YJPAfterOpsDebugEnable)
                    {
                        //输出所有iter
                        printf("[AfterOps<%d>]AfterOps: Send data to SCP, Send_Return_Sub_Major_Iter is %d, Reduce_Dim_Fill_Iter is %d, Major_Dim_Fill_Iter is %d\n",io.DebugInfo.DebugTimeStampe, Send_Return_Sub_Major_Iter, Reduce_Dim_Fill_Iter, Major_Dim_Fill_Iter)
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
                    printf("[AfterOps<%d>]AfterOps: TaskCtrl confirm, we can go to next task\n",io.DebugInfo.DebugTimeStampe)
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