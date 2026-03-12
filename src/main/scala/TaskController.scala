
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import boom.v3.util._

//TaskController代表,
class TaskController(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ygjkctrl = Flipped(new YGJKControl)
        val instfifo_head_id = Output(UInt(MarcoInstFIFODepthBitSize.W))
        val instfifo_tail_id = Output(UInt(MarcoInstFIFODepthBitSize.W))
        val instfifo_release = Output(Bool())
        // val ConfigInfo = DecoupledIO(new ConfigInfoIO)
        // val TaskCtrlInfo = (new TaskCtrlInfo)
        val ADC_MicroTask_Config = (new ADCMicroTaskConfigIO)
        val BDC_MicroTask_Config = (new BDCMicroTaskConfigIO)
        val ASC_MicroTask_Config = (new ADCMicroTaskConfigIO)
        val BSC_MicroTask_Config = (new BDCMicroTaskConfigIO)
        val CDC_MicroTask_Config = (new CDCMicroTaskConfigIO)
        val AML_MicroTask_Config = (new AMLMicroTaskConfigIO)
        val BML_MicroTask_Config = (new BMLMicroTaskConfigIO)
        val ASL_MicroTask_Config = (new ASLMicroTaskConfigIO)
        val BSL_MicroTask_Config = (new BSLMicroTaskConfigIO)
        val CML_MicroTask_Config = (new CMLMicroTaskConfigIO)
        val MTE_MicroTask_Config = (new MTEMicroTaskConfigIO)
        val AOP_MicroTask_Config = (new AfterOpsMicroTaskConfigIO)
        val SCP_CtrlInfo               = (new SCPControlInfo)
        val DebugTimeStampe = Input(UInt(32.W))
        val ctrlCounter = Output(new CTRLCounter)
        // val MMU_Config_Info = (new MMUConfigInfo)
        // val MatrixTE_MicroTask_Config = DecoupledIO(new MatrixTEMicroTaskConfigIO)
    })

    val get_configred = RegInit(false.B)
    io.ctrlCounter := 0.U.asTypeOf(io.ctrlCounter)

    if (EnablePerfCounter)
    {
        // val get_configred = RegInit(false.B)
        io.ctrlCounter.getConfigured := get_configred
    }

    var PEDataType = new FReducePEDataType

    //ADC_MicroTask_Config 的 默认配置
    io.ADC_MicroTask_Config.Is_Transpose := false.B
    io.ADC_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.ADC_MicroTask_Config.ApplicationTensor_A)
    io.ADC_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.ADC_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.ADC_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.ADC_MicroTask_Config.MicroTaskValid := false.B
    io.ADC_MicroTask_Config.MicroTaskEndReady := false.B

    // ASC_MicroTask_Config 的 默认配置
    io.ASC_MicroTask_Config.Is_Transpose := false.B
    io.ASC_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.ASC_MicroTask_Config.ApplicationTensor_A)
    io.ASC_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.ASC_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.ASC_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.ASC_MicroTask_Config.MicroTaskValid := false.B
    io.ASC_MicroTask_Config.MicroTaskEndReady := false.B

    //BDC_MicroTask_Config 的 默认配置
    io.BDC_MicroTask_Config.Is_Transpose := false.B
    io.BDC_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BDC_MicroTask_Config.ApplicationTensor_B)
    io.BDC_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.BDC_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.BDC_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.BDC_MicroTask_Config.MicroTaskValid := false.B
    io.BDC_MicroTask_Config.MicroTaskEndReady := false.B

    //BSC_MicroTask_Config 的 默认配置
    io.BSC_MicroTask_Config.Is_Transpose := false.B
    io.BSC_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BSC_MicroTask_Config.ApplicationTensor_B)
    io.BSC_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.BSC_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.BSC_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.BSC_MicroTask_Config.MicroTaskValid := false.B
    io.BSC_MicroTask_Config.MicroTaskEndReady := false.B

    //CDC_MicroTask_Config 的 默认配置
    io.CDC_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CDC_MicroTask_Config.ApplicationTensor_C)
    io.CDC_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CDC_MicroTask_Config.ApplicationTensor_D)
    io.CDC_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.CDC_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.CDC_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.CDC_MicroTask_Config.Is_Transpose := false.B
    io.CDC_MicroTask_Config.Is_AfterOps_Tile := false.B
    io.CDC_MicroTask_Config.Is_Reorder_Only_Ops := false.B
    io.CDC_MicroTask_Config.Is_EasyScale_Only_Ops := false.B
    io.CDC_MicroTask_Config.Is_VecFIFO_Ops := false.B
    io.CDC_MicroTask_Config.MicroTaskValid := false.B
    io.CDC_MicroTask_Config.MicroTaskEndReady := false.B
    io.CDC_MicroTask_Config.MicroTask_TEComputeEndReady := false.B

    //AML_MicroTask_Config 的 默认配置
    io.AML_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.AML_MicroTask_Config.ApplicationTensor_A)
    io.AML_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.AML_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.AML_MicroTask_Config.Convolution_Current_OH_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_OW_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_KH_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_KW_Index := 0.U
    io.AML_MicroTask_Config.Conherent := false.B
    io.AML_MicroTask_Config.MicroTaskValid := false.B
    io.AML_MicroTask_Config.MicroTaskEndReady := false.B

    //ASL_MicroTask_Config 的 默认配置
    io.ASL_MicroTask_Config.ApplicationScale_A := 0.U.asTypeOf(io.ASL_MicroTask_Config.ApplicationScale_A)
    io.ASL_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.ASL_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.ASL_MicroTask_Config.Conherent := false.B
    io.ASL_MicroTask_Config.MicroTaskValid := false.B
    io.ASL_MicroTask_Config.MicroTaskEndReady := false.B

    //BML_MicroTask_Config 的 默认配置
    io.BML_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BML_MicroTask_Config.ApplicationTensor_B)
    io.BML_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.BML_MicroTask_Config.ScaratchpadTensor_K := 0.U
    // io.BML_MicroTask_Config.Convolution_Current_KH_Index := 0.U
    // io.BML_MicroTask_Config.Convolution_Current_KW_Index := 0.U
    io.BML_MicroTask_Config.Conherent := false.B
    io.BML_MicroTask_Config.MicroTaskValid := false.B
    io.BML_MicroTask_Config.MicroTaskEndReady := false.B

    // BSL_MicroTask_Config 的 默认配置
    io.BSL_MicroTask_Config.ApplicationScale_B := 0.U.asTypeOf(io.BSL_MicroTask_Config.ApplicationScale_B)
    io.BSL_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.BSL_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.BSL_MicroTask_Config.Conherent := false.B
    io.BSL_MicroTask_Config.MicroTaskValid := false.B
    io.BSL_MicroTask_Config.MicroTaskEndReady := false.B

    //CML_MicroTask_Config 的 默认配置
    io.CML_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_C)
    io.CML_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_D)
    io.CML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.LoadTaskInfo)
    io.CML_MicroTask_Config.StoreTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.StoreTaskInfo)
    io.CML_MicroTask_Config.Conherent := false.B
    io.CML_MicroTask_Config.Is_Transpose := false.B
    io.CML_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.CML_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.CML_MicroTask_Config.IsLoadMicroTask := false.B
    io.CML_MicroTask_Config.IsStoreMicroTask := false.B
    io.CML_MicroTask_Config.MicroTaskValid := false.B
    io.CML_MicroTask_Config.MicroTaskEndReady := false.B

    io.SCP_CtrlInfo.ADC_SCP_ID := 0.U
    io.SCP_CtrlInfo.BDC_SCP_ID := 0.U
    io.SCP_CtrlInfo.CDC_SCP_ID := 0.U
    io.SCP_CtrlInfo.AML_SCP_ID := 1.U
    io.SCP_CtrlInfo.BML_SCP_ID := 1.U
    io.SCP_CtrlInfo.CML_SCP_ID := 1.U

    io.MTE_MicroTask_Config.MicroTaskValid := false.B
    io.MTE_MicroTask_Config.dataType := ElementDataType.DataTypeUndef
    // io.MTE_MicroTask_Config.valid := false.B

    //AOP_MicroTask_Config 的 默认配置
    io.AOP_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeUndef
    io.AOP_MicroTask_Config.ApplicationTensor_D.dataType := ElementDataType.DataTypeUndef
    io.AOP_MicroTask_Config.ScaratchpadTensor_M := 0.U
    io.AOP_MicroTask_Config.ScaratchpadTensor_N := 0.U
    io.AOP_MicroTask_Config.ScaratchpadTensor_K := 0.U
    io.AOP_MicroTask_Config.Is_Transpose := false.B
    io.AOP_MicroTask_Config.Is_Reorder_Only_Ops := false.B
    io.AOP_MicroTask_Config.Is_EasyScale_Only_Ops := false.B
    io.AOP_MicroTask_Config.Is_VecFIFO_Ops := false.B
    io.AOP_MicroTask_Config.MicroTaskValid := false.B
    io.AOP_MicroTask_Config.MicroTaskEndReady := false.B
    io.AOP_MicroTask_Config.CUTEuop := 0.U.asTypeOf(io.AOP_MicroTask_Config.CUTEuop)

    io.ygjkctrl.acc_running := false.B
    io.ygjkctrl.cute_return_val := 0xdeadbeefL.U
    // io.ygjkctrl.cute_return_val.valid := false.B
    io.ygjkctrl.InstFIFO_Finish := 0.U
    io.ygjkctrl.InstFIFO_Full := 0.U
    io.ygjkctrl.InstFIFO_Info := 0.U
    io.instfifo_release := 0.U

    //TODO:构思微指令Test的流程
    

    //TODO:12.17先完成宏指令的流程，然后再完成微指令的流程
    
    //宏指令描述寄存器
    val MacroInst_Reg = RegInit(0.U(new MacroInst().getWidth.W))
    //宏指令描述的是矩阵乘任务或者卷积任务的描述
// void CUTE_MATMUL_MarcoTask(void *A,void *B,void *C,void *D,int Application_M,int Application_N,int Application_K,int element_type,int bias_type,\
// uint64_t stride_A,uint64_t stride_B,uint64_t stride_C,uint64_t stride_D,bool transpose_result,int conv_oh_index,int conv_ow_index,int conv_oh_max,int conv_ow_max,void * VectorOp,int VectorInst_Length)

    // val Application_M = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的M的大小，对于卷积来说[ohow][oc][ic]的[ohow]的大小
    // val Application_N = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的N的大小，对于卷积来说[ohow][oc][ic]的[oc]的大小
    // val Application_K = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的K的大小，对于卷积来说[ohow][oc][ic]的[ic]的大小

    // val element_type = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵元素的数据类型
    // val bias_type = UInt(CMemoryLoaderTaskType.TypeBitWidth.W) //矩阵乘的bias的数据类型

    // val transpose_result = Bool() //结果是否需要转置，用于attention加速
    // // val conv_oh_index = UInt(log2Ceil(ConvolutionDIM_Max).W)
    // // val conv_ow_index = UInt(log2Ceil(ConvolutionDIM_Max).W)
    // val conv_stride = UInt(log2Ceil(StrideSizeMax).W) //卷积的stride步长
    // val conv_oh_max = UInt(log2Ceil(ConvolutionDIM_Max).W) //卷积的oh长度，用于和stride配合完成padding等操作
    // val conv_ow_max = UInt(log2Ceil(ConvolutionDIM_Max).W) //卷积的ow长度，用于和stride配合完成padding等操作
    // val kernel_size = UInt(log2Ceil(KernelSizeMax).W) //卷积核的大小
    // val kernel_stride = UInt((64.W)) //kernel_stride是每一个index的卷积核的大小，我们要求卷积核的数据排布是(kh,kw,oc,ic)

    //宏指令MarcroInst_FIFO,深度为4
    //宏指令描述的是矩阵乘任务或者卷积任务的描述
    val MacroInst_FIFO = RegInit(VecInit(Seq.fill(MarcoInstFIFODepth)(0.U(new MacroInst().getWidth.W))))
    val MacroInst_FIFO_Head = RegInit(0.U(MarcoInstFIFODepthBitSize.W))
    val MacroInst_FIFO_Tail = RegInit(0.U(MarcoInstFIFODepthBitSize.W))
    // performance-counter 宏指令不空是代表cute memory bound, 空的时候代表cute idle
    // performance-counter cute idle可分为start up idle,具体表现为cpu发出第一条rocc指令发起配置直到cute的计算部件开始进入working
    // performance-counter cutememory bound可分为A B C D load/store bound,具体在伪指令阶段分析
    val MacroInst_FIFO_Empty = MacroInst_FIFO_Head === MacroInst_FIFO_Tail
    val MacroInst_FIFO_Full = WrapInc(MacroInst_FIFO_Head, MarcoInstFIFODepth) === MacroInst_FIFO_Tail

    val MacroInst_FIFO_Valid = RegInit(VecInit(Seq.fill(MarcoInstFIFODepth)(false.B)))
    val MacroInst_FIFO_Decode_Finish = RegInit(VecInit(Seq.fill(MarcoInstFIFODepth)(false.B)))
    val MacroInst_FIFO_Total_Finish = RegInit(VecInit(Seq.fill(MarcoInstFIFODepth)(false.B)))

    val MarcoInst_FIFO_Decode_Head = RegInit(0.U(MarcoInstFIFODepthBitSize.W))
    val MarcoInst_FIFO_Finish_Head = RegInit(0.U(MarcoInstFIFODepthBitSize.W))

    io.instfifo_head_id := MacroInst_FIFO_Head
    io.instfifo_tail_id := MacroInst_FIFO_Tail

    //autoclear
    if(TaskCtrl_AutoClear)
    {
        when(MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Tail) === true.B)
        {
            MacroInst_FIFO_Valid(MacroInst_FIFO_Tail) := false.B
            MacroInst_FIFO_Decode_Finish(MacroInst_FIFO_Tail) := false.B
            MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Tail) := false.B
            MacroInst_FIFO_Tail := WrapInc(MacroInst_FIFO_Tail, MarcoInstFIFODepth)
            io.instfifo_release := true.B
            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Inst auto Clear!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n", io.DebugTimeStampe,MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
            }
        }
    }

    io.ygjkctrl.InstFIFO_Info := MacroInst_FIFO_Valid.asUInt
    io.ygjkctrl.InstFIFO_Full := MacroInst_FIFO_Full
    io.ygjkctrl.InstFIFO_Finish := MacroInst_FIFO_Total_Finish.asUInt

    if (EnablePerfCounter)
    {io.ctrlCounter.InstQueueEmpty := MacroInst_FIFO_Empty}

    when(io.ygjkctrl.config.valid)
    {
        // //输出指令
        // if (YJPDebugEnable)
        // {
        //     printf("TaskController: func = %d, cfgData1 = %d, cfgData2 = %d\n",io.DebugTimeStampe, io.ygjkctrl.config.bits.func, io.ygjkctrl.config.bits.cfgData1, io.ygjkctrl.config.bits.cfgData2)
        // }

        val MacroInst_Reg_Wire = Wire(new MacroInst)
        MacroInst_Reg_Wire := MacroInst_Reg.asTypeOf(MacroInst_Reg_Wire)
        
        //funct为func去除最高位的部分
        val funct = io.ygjkctrl.config.bits.func(5,0)
        //funct === 0，将配置好的Marco指令加入指令FIFO

        //funct === 1    配置加速器，cfgData1 = ATensor的起始地址，cfgData2 = next_reduce_dim的stride
        //funct === 2    配置加速器，cfgData1 = BTensor的起始地址，cfgData2 = next_reduce_dim的stride
        //funct === 3    配置加速器，cfgData1 = CTensor的起始地址，cfgData2 = next_reduce_dim的stride
        //funct === 4    配置加速器，cfgData1 = DTensor的起始地址，cfgData2 = next_reduce_dim的stride

        //funct === 5    配置加速器，cfgData1 = (M[0~19bit]，N[20~39bit]，K[40~59bit])
        //               对于卷积就是cfgData1 = (ohow[0~19bit]，oc[20~39bit]，ic[40~59bit])
        //                         cfgData2 = kernel_stride 对于矩阵乘来说是0，对于卷积来说是下一个卷积核的起始地址卷积核是(kh,kw,oc,ic)排的

        //funct === 6    配置加速器，cfgData1 = (element_type[0~7bit]，bias_type[8~15bit]，transpose_result[16~23bit],conv_stride[24~31bit],conv_oh_max[32~47bit],conv_ow_max[48~63bit])
        //               配置加速器，cfgData2 = (kernel_size[0~7bit]，kernel_stride[8~15bit]，conv_oh_per_add[16~25]，conv_ow_per_add[26~35]， conv_oh_index[36~45bit],conv_oh_index[46~55bit])
        //funct === 8    配置加速器，cfgData1 = BScale的起始位置
        //val conv_oh_per_add //避免在计算过程中进行除法运算，这里可以提前计算好
        //val conv_ow_per_add //避免在计算过程中进行取余运算，这里可以提前计算好

        when(funct === 0.U)
        {
            //这里最好是生成一条VLSW送到加速器的指令buff里，然后在TaskController继续分解成不同期间的指令
            //目前先实现成单条指令触发

            // assert(!MacroInst_FIFO_Full, "MacroInst FIFO is full")
            when(!MacroInst_FIFO_Full)
            {
                val is_matmul_inst = MacroInst_Reg.asTypeOf(new MacroInst).conv_oh_max === 0.U && MacroInst_Reg.asTypeOf(new MacroInst).conv_ow_max === 0.U
                val matmul_inst = MacroInst_Reg.asTypeOf(new MacroInst)
                when(is_matmul_inst)
                {
                    matmul_inst.is_matmul := true.B
                    matmul_inst.conv_oh_max := 1.U
                    matmul_inst.conv_ow_max := MacroInst_Reg.asTypeOf(new MacroInst).Application_M
                    matmul_inst.conv_ow_per_add := Tensor_M.U
                }
                MacroInst_FIFO(MacroInst_FIFO_Head) := Mux(is_matmul_inst, matmul_inst.asUInt, MacroInst_Reg)
                MacroInst_FIFO_Valid(MacroInst_FIFO_Head) := true.B
                MacroInst_FIFO_Decode_Finish(MacroInst_FIFO_Head) := false.B
                MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Head) := false.B
                MacroInst_FIFO_Head := WrapInc(MacroInst_FIFO_Head, MarcoInstFIFODepth)
                io.ygjkctrl.cute_return_val := MacroInst_FIFO_Head
                // io.ygjkctrl.cute_return_val.valid := true.B
                if (YJPDebugEnable)
                {
                    when(is_matmul_inst === false.B)
                    {
                        printf("[TaskController<%d>]:CONV Inst Insert!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n", io.DebugTimeStampe,MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
                        //输出宏指令的的信息
                        printf("[TaskController<%d>]:ApplicationTensor_A_BaseVaddr = %x, ApplicationTensor_A_Stride = %x, ApplicationTensor_B_BaseVaddr = %x, ApplicationTensor_B_Stride = %x, ApplicationTensor_C_BaseVaddr = %x, ApplicationTensor_C_Stride = %x, ApplicationTensor_D_BaseVaddr = %x, ApplicationTensor_D_Stride = %x, Application_M = %x, Application_N = %x, Application_K = %x, kernel_stride = %x, element_type = %x, bias_type = %x, transpose_result = %x, conv_stride = %x, conv_oh_max = %x, conv_ow_max = %x, kernel_size = %x, conv_oh_per_add = %x, conv_ow_per_add = %x, conv_oh_index = %x, conv_ow_index = %x\n", io.DebugTimeStampe, MacroInst_Reg_Wire.ApplicationTensor_A_BaseVaddr, MacroInst_Reg_Wire.ApplicationTensor_A_Stride, MacroInst_Reg_Wire.ApplicationTensor_B_BaseVaddr, MacroInst_Reg_Wire.ApplicationTensor_B_Stride, MacroInst_Reg_Wire.ApplicationTensor_C_BaseVaddr, MacroInst_Reg_Wire.ApplicationTensor_C_Stride, MacroInst_Reg_Wire.ApplicationTensor_D_BaseVaddr, MacroInst_Reg_Wire.ApplicationTensor_D_Stride, MacroInst_Reg_Wire.Application_M, MacroInst_Reg_Wire.Application_N, MacroInst_Reg_Wire.Application_K, MacroInst_Reg_Wire.kernel_stride, MacroInst_Reg_Wire.element_type, MacroInst_Reg_Wire.bias_type, MacroInst_Reg_Wire.transpose_result, MacroInst_Reg_Wire.conv_stride, MacroInst_Reg_Wire.conv_oh_max, MacroInst_Reg_Wire.conv_ow_max, MacroInst_Reg_Wire.kernel_size, MacroInst_Reg_Wire.conv_oh_per_add, MacroInst_Reg_Wire.conv_ow_per_add, MacroInst_Reg_Wire.conv_oh_index, MacroInst_Reg_Wire.conv_ow_index)
                    }.elsewhen(is_matmul_inst === true.B)
                    {
                        printf("[TaskController<%d>]:MATMUL Inst Insert!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n", io.DebugTimeStampe,MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
                        //输出宏指令的的信息
                        printf("[TaskController<%d>]:ApplicationTensor_A_BaseVaddr = %x, ApplicationTensor_A_Stride = %x, ApplicationTensor_B_BaseVaddr = %x, ApplicationTensor_B_Stride = %x, ApplicationTensor_C_BaseVaddr = %x, ApplicationTensor_C_Stride = %x, ApplicationTensor_D_BaseVaddr = %x, ApplicationTensor_D_Stride = %x, Application_M = %x, Application_N = %x, Application_K = %x, kernel_stride = %x, element_type = %x, bias_type = %x, transpose_result = %x, conv_stride = %x, conv_oh_max = %x, conv_ow_max = %x, kernel_size = %x, conv_oh_per_add = %x, conv_ow_per_add = %x, conv_oh_index = %x, conv_ow_index = %x\n", io.DebugTimeStampe, matmul_inst.ApplicationTensor_A_BaseVaddr, matmul_inst.ApplicationTensor_A_Stride, matmul_inst.ApplicationTensor_B_BaseVaddr, matmul_inst.ApplicationTensor_B_Stride, matmul_inst.ApplicationTensor_C_BaseVaddr, matmul_inst.ApplicationTensor_C_Stride, matmul_inst.ApplicationTensor_D_BaseVaddr, matmul_inst.ApplicationTensor_D_Stride, matmul_inst.Application_M, matmul_inst.Application_N, matmul_inst.Application_K, matmul_inst.kernel_stride, matmul_inst.element_type, matmul_inst.bias_type, matmul_inst.transpose_result, matmul_inst.conv_stride, matmul_inst.conv_oh_max, matmul_inst.conv_ow_max, matmul_inst.kernel_size, matmul_inst.conv_oh_per_add, matmul_inst.conv_ow_per_add, matmul_inst.conv_oh_index, matmul_inst.conv_ow_index)
                    }


                }

                if(EnablePerfCounter) {get_configred := false.B}
            }.otherwise
            {
                io.ygjkctrl.cute_return_val := 0xdeadbeefL.U
                // io.ygjkctrl.cute_return_val.valid := true.B
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Inst Insert!  MacroInst FIFO is Full!\n", io.DebugTimeStampe)
                }
            }


        }.elsewhen(funct === 1.U)
        {
            MacroInst_Reg_Wire.ApplicationTensor_A_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg_Wire.ApplicationTensor_A_Stride := io.ygjkctrl.config.bits.cfgData2
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
            
        }.elsewhen(funct === 2.U)
        {
            MacroInst_Reg_Wire.ApplicationTensor_B_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg_Wire.ApplicationTensor_B_Stride := io.ygjkctrl.config.bits.cfgData2
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 3.U)
        {
            MacroInst_Reg_Wire.ApplicationTensor_C_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg_Wire.ApplicationTensor_C_Stride := io.ygjkctrl.config.bits.cfgData2
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 4.U)
        {
            MacroInst_Reg_Wire.ApplicationTensor_D_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg_Wire.ApplicationTensor_D_Stride := io.ygjkctrl.config.bits.cfgData2
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 5.U)
        {
            assert(MacroInst_Reg_Wire.Application_M.getWidth <= 20)
            assert(MacroInst_Reg_Wire.Application_N.getWidth <= 20)
            assert(MacroInst_Reg_Wire.Application_K.getWidth <= 20)
            MacroInst_Reg_Wire.Application_M := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.Application_M.getWidth-1,0)
            MacroInst_Reg_Wire.Application_N := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.Application_N.getWidth+19,20)
            MacroInst_Reg_Wire.Application_K := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.Application_K.getWidth+39,40)
            MacroInst_Reg_Wire.kernel_stride := io.ygjkctrl.config.bits.cfgData2
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 6.U)
        {
            assert(MacroInst_Reg_Wire.element_type.getWidth <= 8)
            assert(MacroInst_Reg_Wire.bias_type.getWidth <= 8)
            assert(MacroInst_Reg_Wire.transpose_result.getWidth <= 8)
            assert(MacroInst_Reg_Wire.conv_stride.getWidth <= 8)
            assert(MacroInst_Reg_Wire.conv_oh_max.getWidth <= 16)
            assert(MacroInst_Reg_Wire.conv_ow_max.getWidth <= 16)

            assert(MacroInst_Reg_Wire.kernel_size.getWidth <= 4)
            assert(MacroInst_Reg_Wire.conv_oh_per_add.getWidth <= 15)
            assert(MacroInst_Reg_Wire.conv_ow_per_add.getWidth <= 15)
            assert(MacroInst_Reg_Wire.conv_oh_index.getWidth <= 15)
            assert(MacroInst_Reg_Wire.conv_ow_index.getWidth <= 15)



            MacroInst_Reg_Wire.element_type := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.element_type.getWidth-1,0)
            MacroInst_Reg_Wire.bias_type := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.bias_type.getWidth-1+8,8)
            MacroInst_Reg_Wire.transpose_result := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.transpose_result.getWidth-1+16,16)
            MacroInst_Reg_Wire.conv_stride := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.conv_stride.getWidth-1+24,24)
            MacroInst_Reg_Wire.conv_oh_max := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.conv_oh_max.getWidth-1+32,32)
            MacroInst_Reg_Wire.conv_ow_max := io.ygjkctrl.config.bits.cfgData1(MacroInst_Reg_Wire.conv_ow_max.getWidth-1+48,48)
            MacroInst_Reg_Wire.kernel_size := io.ygjkctrl.config.bits.cfgData2(MacroInst_Reg_Wire.kernel_size.getWidth-1,0)
            MacroInst_Reg_Wire.conv_oh_per_add := io.ygjkctrl.config.bits.cfgData2(MacroInst_Reg_Wire.conv_oh_per_add.getWidth-1+4,4)
            MacroInst_Reg_Wire.conv_ow_per_add := io.ygjkctrl.config.bits.cfgData2(MacroInst_Reg_Wire.conv_ow_per_add.getWidth-1+19,19)
            MacroInst_Reg_Wire.conv_oh_index := io.ygjkctrl.config.bits.cfgData2(MacroInst_Reg_Wire.conv_oh_index.getWidth-1+34,34)
            MacroInst_Reg_Wire.conv_ow_index := io.ygjkctrl.config.bits.cfgData2(MacroInst_Reg_Wire.conv_ow_index.getWidth-1+49,49)
            MacroInst_Reg_Wire.bias_data_type := PEDataType.CdataByteWidth(MacroInst_Reg_Wire.element_type)
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 7.U){
            MacroInst_Reg_Wire.ApplicationScale_A_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 8.U){
            MacroInst_Reg_Wire.ApplicationScale_B_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
            MacroInst_Reg := MacroInst_Reg_Wire.asUInt

            if(EnablePerfCounter) {get_configred := true.B} 
        }.elsewhen(funct === 16.U)
        {
            if(TaskCtrl_AutoClear)
            {
                io.ygjkctrl.cute_return_val := 0xdeadbeefL.U
                printf("[TaskController<%d>]:Inst auto Clear!  MacroInst_FIFO is Empty!\n", io.DebugTimeStampe)
            }
            else
            {
                //clear指令，将队尾的宏指令清除
                when(!MacroInst_FIFO_Empty&&MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Tail))
                {
                    MacroInst_FIFO_Valid(MacroInst_FIFO_Tail) := false.B
                    MacroInst_FIFO_Decode_Finish(MacroInst_FIFO_Tail) := false.B
                    MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Tail) := false.B
                    MacroInst_FIFO_Tail := WrapInc(MacroInst_FIFO_Tail, MarcoInstFIFODepth)
                    io.ygjkctrl.cute_return_val := MacroInst_FIFO_Tail
                    // io.ygjkctrl.cute_return_val.valid := true.B
                    if (YJPDebugEnable)
                    {
                        printf("[TaskController<%d>]:Inst Clear!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n", io.DebugTimeStampe,MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
                    }
                }.otherwise
                {
                    io.ygjkctrl.cute_return_val := 0xdeadbeefL.U
                    // io.ygjkctrl.cute_return_val.valid := true.B
                    if (YJPDebugEnable)
                    {
                        printf("[TaskController<%d>]:Inst Clear!  MacroInst_FIFO is Empty or Not Finish!\n", io.DebugTimeStampe)
                    }
                }   
            }
        }.elsewhen(funct === 17.U)
        {
            //查询当前完成宏指令的尾编号的位置
            io.ygjkctrl.cute_return_val := MacroInst_FIFO_Tail
            // io.ygjkctrl.cute_return_val.valid := true.B
            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Inst Query!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n", io.DebugTimeStampe,MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
            }
        }.elsewhen(funct === 18.U)
        {
        }
    }
    

    //当宏指令FIFO不为空时，且宏指令不在译码，将宏指令FIFO的指令取出
    val Decoding_MacroInst = MacroInst_FIFO(MarcoInst_FIFO_Decode_Head).asTypeOf(new MacroInst)
    val MarcoInst_Can_Decode = MacroInst_FIFO_Valid(MarcoInst_FIFO_Decode_Head) && !MacroInst_FIFO_Decode_Finish(MarcoInst_FIFO_Decode_Head)

    val Decoding_MarcoInst_Going = RegInit(false.B)

    if (EnablePerfCounter)
    {io.ctrlCounter.InstCanDecode := MarcoInst_Can_Decode}

    //宏指令译码时，会不断向微指令发射队列发射微指令
    //微指令发射队列，深度为8,4,4
    //Load,Compute,Store，三个阶段的微指令

    // SCP  SCP MTE  SCP
    //  |    |   |    |
    //  v    v   v    v
    //Load->Compute->Store，A->B，B阶段的微指令只由A阶段的微指令的完成情况和B阶段的资源情况决定

    val A_SCP_Free = RegInit(VecInit(Seq.fill(2)(true.B)))
    val B_SCP_Free = RegInit(VecInit(Seq.fill(2)(true.B)))
    val C_SCP_Free = RegInit(VecInit(Seq.fill(2)(true.B)))

    //微指令队列
    val Load_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new LoadMicroInst().getWidth.W))))
    val Load_MicroInst_FINISH_Ready_GO = RegInit(VecInit(Seq.fill(4)(false.B)))//Load微指令是否完成
    val Load_MicroInst_FINISH_Ready_Commit = RegInit(VecInit(Seq.fill(4)(false.B)))//Load微指令是否可以提交
    val Compute_MicroInst_FINISH_Ready_GO = RegInit(VecInit(Seq.fill(4)(false.B)))//Compute微指令是否完成
    val Compute_MicroInst_FINISH_Ready_Commit = RegInit(VecInit(Seq.fill(4)(false.B)))//Compute微指令是否可以提交
    val Compute_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new ComputeMicroInst().getWidth.W))))
    val Store_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new StoreMicroInst().getWidth.W))))

    val Load_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    val Load_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    val Load_MicroInst_FIFO_Empty = Load_MicroInst_FIFO_Head === Load_MicroInst_FIFO_Tail
    val Load_MicroInst_FIFO_Full = WrapInc(Load_MicroInst_FIFO_Head, 4) === Load_MicroInst_FIFO_Tail
    val Load_MicroInst_FINISH_Head = RegInit(0.U(2.W))
    val Load_MicroInst_FINISH_All = Load_MicroInst_FINISH_Head === Load_MicroInst_FIFO_Head//所有的Load微指令都已经完成

    val Store_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    val Store_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    val Store_MicroInst_FIFO_Empty = Store_MicroInst_FIFO_Head === Store_MicroInst_FIFO_Tail
    val Store_MicroInst_FIFO_Full = WrapInc(Store_MicroInst_FIFO_Head, 4) === Store_MicroInst_FIFO_Tail
    val Store_MicroInst_FINISH_HEAD = RegInit(0.U(2.W))

    val Compute_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    val Compute_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    val Compute_MicroInst_FIFO_Empty = Compute_MicroInst_FIFO_Head === Compute_MicroInst_FIFO_Tail
    val Compute_MicroInst_FIFO_Full = WrapInc(Compute_MicroInst_FIFO_Head, 4) === Compute_MicroInst_FIFO_Tail
    val Compute_MicroInst_FINISH_HEAD = RegInit(0.U(2.W))
    val Compute_MicroInst_FINISH_All = Compute_MicroInst_FINISH_HEAD === Compute_MicroInst_FIFO_Head//所有的Compute微指令都已经完成

    //微指令执行状态队列
    val Load_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new LoadMicroInst_Resource_Info().getWidth.W))))
    val Store_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new StoreMicroInst_Resource_Info().getWidth.W))))
    val Compute_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new ComputeMicroInst_Resource_Info().getWidth.W))))

    //Load指令只能被Compute/clear指令从信息队列中取出
    //Compute指令只能被Store/clear指令从信息队列中取出
    //Store指令完成后，会取检查是否需要给某一条宏指令标记完成

    //目前假设所有资源信息都ok，不考虑资源信息变化

    // val ApplicationTensor_A_BaseVaddr = UInt(64.W) //矩阵A的起始地址
    // val ApplicationTensor_B_BaseVaddr = UInt(64.W) //矩阵B的起始地址
    // val ApplicationTensor_C_BaseVaddr = UInt(64.W) //矩阵C的起始地址
    // val ApplicationTensor_D_BaseVaddr = UInt(64.W) //矩阵D的起始地址

    // val ApplicationTensor_A_Stride = UInt(64.W) //矩阵A的stride,代表下一组Reduce_DIM需要增加多少地址偏移量，对于矩阵A[M][N]来说就是M+1需要增加多少地址偏移量，对于卷积[hw][c]来说，就是hw+1需要增加多少地址偏移量
    // val ApplicationTensor_B_Stride = UInt(64.W) //矩阵B的stride,代表下一组Reduce_DIM需要增加多少地址偏移量
    // val ApplicationTensor_C_Stride = UInt(64.W) //矩阵C的stride,代表下一组Reduce_DIM需要增加多少地址偏移量
    // val ApplicationTensor_D_Stride = UInt(64.W) //矩阵D的stride,代表下一组Reduce_DIM需要增加多少地址偏移量

    // val Application_M = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的M的大小，对于卷积来说[ohow][oc][ic]的[ohow]的大小
    // val Application_N = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的N的大小，对于卷积来说[ohow][oc][ic]的[oc]的大小
    // val Application_K = UInt(ApplicationMaxTensorSizeBitSize.W) //矩阵乘的K的大小，对于卷积来说[ohow][oc][ic]的[ic]的大小

    // val element_type = UInt(ElementDataType.DataTypeBitWidth.W) //矩阵元素的数据类型
    // val bias_type = UInt(CMemoryLoaderTaskType.TypeBitWidth.W) //矩阵乘的bias的数据类型

    // val transpose_result = Bool() //结果是否需要转置，用于attention加速
    // val conv_oh_index = UInt(log2Ceil(ConvolutionDIM_Max).W)
    // val conv_ow_index = UInt(log2Ceil(ConvolutionDIM_Max).W)
    // val conv_stride = UInt(log2Ceil(StrideSizeMax).W) //卷积的stride步长
    // val conv_oh_max = UInt(log2Ceil(ConvolutionDIM_Max).W) //卷积的oh长度，用于和stride配合完成padding等操作
    // val conv_ow_max = UInt(log2Ceil(ConvolutionDIM_Max).W) //卷积的ow长度，用于和stride配合完成padding等操作
    // val kernel_size = UInt(log2Ceil(KernelSizeMax).W) //卷积核的大小
    // val kernel_stride = UInt((64.W)) //kernel_stride是每一个index的卷积核的大小，我们要求卷积核的数据排布是(kh,kw,oc,ic)

    val Current_Tile_M_Iter = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))//MNK切块
    val Current_Tile_N_Iter = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))//MNK切块
    val Current_Tile_K_Iter = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))//MNK切块
    val Current_Tile_OH_Index = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))//ohow切块
    val Current_Tile_OW_Index = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))//ohow切块
    val Current_Tile_KH_Index = RegInit(0.U(log2Ceil(KernelSizeMax).W))//ohow切块
    val Current_Tile_KW_Index = RegInit(0.U(log2Ceil(KernelSizeMax).W))//ohow切块
    val Current_Tile_Tensor_K_Iter = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))//MNK切块
    val Decode_Tensor_K_iter_Add = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))//一次K迭代的增量
    
    val Decode_A_SCP_ID = RegInit(0.U(2.W))
    val Decode_B_SCP_ID = RegInit(0.U(2.W))
    val Decode_C_SCP_ID = RegInit(0.U(2.W))

    //解码宏指令,宏指令在被解码时，不响应微指令的发射和新的宏指令
    when(MarcoInst_Can_Decode)
    {
        //卷积&矩阵乘切块
        //大任务切成64*64*K的小任务
        //每个64*64*K的小任务，完成可以执行与处理器交互的后操作

        //切成不同的Tile_load,Tile_compute,Tile_store
        //解码出Load指令后再解码出Compute指令
        //解码出特定的Compute指令后再解码出Store指令
        //在解码的过程中将Load,Compute,Store的信息放入对应的队列中，及把依赖关系放入对应的队列中

        //解码出Load指令，其实就是分块Load的逻辑
        //解码出Compute指令，其实就是计算的逻辑
        //解码出Store指令，其实就是存储的逻辑

        when(Decoding_MarcoInst_Going === false.B)
        {
            //如果宏指令未开始解析，且当前有未被解码的宏指令，则初始化相关寄存器
            Current_Tile_M_Iter := 0.U
            Current_Tile_N_Iter := 0.U
            Current_Tile_K_Iter := 0.U
            Current_Tile_OH_Index := Decoding_MacroInst.conv_oh_index
            Current_Tile_OW_Index := Decoding_MacroInst.conv_ow_index
            Current_Tile_KH_Index := 0.U
            Current_Tile_KW_Index := 0.U
            Decode_Tensor_K_iter_Add := ((Tensor_K * 8).U / PEDataType.AdataBitWidth(Decoding_MacroInst.element_type))
            Current_Tile_Tensor_K_Iter := 0.U
            Decoding_MarcoInst_Going := true.B

            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:MacroInst Decode Start!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d, Decode_Tensor_K_iter_Add = %d, AdataBitWidth = %d \n",io.DebugTimeStampe, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, Decode_Tensor_K_iter_Add, PEDataType.AdataBitWidth(Decoding_MacroInst.element_type))
            }
        }.otherwise
        {
            val Have_Load_Micro_Inst    = WireInit(true.B)//由宏指令拆解出的微指令，每次拆解都有一个Load指令
            val Have_Compute_Micro_Inst = WireInit(true.B)//由宏指令拆解出来的微指令，每次拆解都有一个Compute指令
            val Have_Store_Micro_Inst   = WireInit(false.B)//由宏指令拆解出来的微指令，只在暂存器切换时才发射Store指令
            val Current_ScaratchpadTensor_M = WireInit(Tensor_M.U)
            val Current_ScaratchpadTensor_N = WireInit(Tensor_N.U)
            val Current_ScaratchpadTensor_K = WireInit(ReduceGroupSize.U)       // 这个K指的是一个Tensor_K里有几个ReduceWidth长度

            val Can_Issue_Load_Micro_Inst = !Load_MicroInst_FIFO_Full
            val Can_Issue_Compute_Micro_Inst = !Compute_MicroInst_FIFO_Full
            val Can_Issue_Store_Micro_Inst = !Store_MicroInst_FIFO_Full
            
            //                                                                                                        VVVVVVV 其实这个给紧了，不过store毕竟很少，所以不会有问题
            val Can_Decode_More_Micro_Inst = Can_Issue_Load_Micro_Inst && Can_Issue_Compute_Micro_Inst && Can_Issue_Store_Micro_Inst

            val LoadMicroInst_Have_A_work = WireInit(true.B)//由宏指令拆解出的微指令，每个微指令都有AB的Load任务
            val LoadMicroInst_Have_B_work = WireInit(true.B)//由宏指令拆解出的微指令，每个微指令都有AB的Load任务
            val LoadMicroInst_Have_A_Scale_work = WireInit(false.B)//由宏指令拆解出的微指令，只有block scale float格式才有AScale的Load任务
            val LoadMicroInst_Have_B_Scale_work = WireInit(false.B)//由宏指令拆解出的微指令，只有block scale float格式才有BScale的Load任务
            val LoadMicroInst_Have_C_work = WireInit(false.B)//由宏指令拆解出的微指令，只有K/IC迭代完成后才有C的Load任务

            val StoreMicroInst_Is_Last_Store = WireInit(false.B)//由宏指令拆解出的微指令，只有最后一次Store任务将指令置位已完成

            // if (YJPDebugEnable)
            // {
            //     //Load_MicroInst_FIFO_Full，Load_MicroInst_FIFO_Head，Load_MicroInst_FIFO_Tail，Load_MicroInst_FINISH_Head等值
            //     printf("[TaskController<%d>]:MacroInst Decode!  Load_MicroInst_FIFO_Full = %d, Load_MicroInst_FIFO_Head = %d, Load_MicroInst_FIFO_Tail = %d, Load_MicroInst_FINISH_Head = %d\n",io.DebugTimeStampe, Load_MicroInst_FIFO_Full, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, Load_MicroInst_FINISH_Head)
            //     //Store_MicroInst_FIFO_Full，Store_MicroInst_FIFO_Head，Store_MicroInst_FIFO_Tail，Store_MicroInst_FINISH_HEAD等值
            //     printf("[TaskController<%d>]:MacroInst Decode!  Store_MicroInst_FIFO_Full = %d, Store_MicroInst_FIFO_Head = %d, Store_MicroInst_FIFO_Tail = %d, Store_MicroInst_FINISH_HEAD = %d\n",io.DebugTimeStampe, Store_MicroInst_FIFO_Full, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail, Store_MicroInst_FINISH_HEAD)
            //     //Compute_MicroInst_FIFO_Full，Compute_MicroInst_FIFO_Head，Compute_MicroInst_FIFO_Tail，Compute_MicroInst_FINISH_HEAD等值
            //     printf("[TaskController<%d>]:MacroInst Decode!  Compute_MicroInst_FIFO_Full = %d, Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d, Compute_MicroInst_FINISH_HEAD = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Full, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, Compute_MicroInst_FINISH_HEAD)

            // }

            when(!Can_Decode_More_Micro_Inst)
            {
                //不能译码就输出微指令队列的头尾
                // if (YJPDebugEnable)
                // {
                //     printf("[TaskController<%d>]:MacroInst cant Decode!  Load_MicroInst_FIFO_Head = %d, Load_MicroInst_FIFO_Tail = %d, Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d, Store_MicroInst_FIFO_Head = %d, Store_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail)
                // }
            }

            when(Can_Decode_More_Micro_Inst)
            {

                if (YJPDebugEnable)
                {
                    //输出宏指令当前的迭代情况
                    printf("[TaskController<%d>]:MacroInst Decode!  Current_Tile_M_Iter = %d, Current_Tile_N_Iter = %d, Current_Tile_K_Iter = %d, Current_Tile_OH_Index = %d, Current_Tile_OW_Index = %d, Current_Tile_KH_Index = %d, Current_Tile_KW_Index = %d\n",io.DebugTimeStampe, Current_Tile_M_Iter, Current_Tile_N_Iter, Current_Tile_K_Iter, Current_Tile_OH_Index, Current_Tile_OW_Index, Current_Tile_KH_Index, Current_Tile_KW_Index)
                }
                //Current_ScaratchpadTensor_M必须4的倍数
                Current_ScaratchpadTensor_M := Mux(Current_Tile_M_Iter + Tensor_M.U >= Decoding_MacroInst.Application_M, (Decoding_MacroInst.Application_M - Current_Tile_M_Iter), Tensor_M.U)
                Current_ScaratchpadTensor_N := Mux(Current_Tile_N_Iter + Tensor_N.U >= Decoding_MacroInst.Application_N, Decoding_MacroInst.Application_N - Current_Tile_N_Iter, Tensor_N.U)
                Current_ScaratchpadTensor_K := ReduceGroupSize.U
                assert(Current_ScaratchpadTensor_N === Tensor_N.U, "Current_ScaratchpadTensor_N is not equal to Tensor_N")
                assert(Decoding_MacroInst.Application_K % 64.U === 0.U, "Decoding_MacroInst.Application_K is not 64 align")

                LoadMicroInst_Have_C_work := Current_Tile_K_Iter === 0.U && Current_Tile_KW_Index === 0.U && Current_Tile_KH_Index === 0.U

                Decode_A_SCP_ID := Mux(Decode_A_SCP_ID === 0.U, 1.U, 0.U)
                Decode_B_SCP_ID := Mux(Decode_B_SCP_ID === 0.U, 1.U, 0.U)

                LoadMicroInst_Have_A_Scale_work := (Decoding_MacroInst.element_type === ElementDataType.DataTypeMxfp8e4m3F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypeMxfp8e5m2F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypenvfp4F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypemxfp4F32) && Decoding_MacroInst.is_matmul
                LoadMicroInst_Have_B_Scale_work := (Decoding_MacroInst.element_type === ElementDataType.DataTypeMxfp8e4m3F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypeMxfp8e5m2F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypenvfp4F32 || Decoding_MacroInst.element_type === ElementDataType.DataTypemxfp4F32) && Decoding_MacroInst.is_matmul
                //迭代生成微指令
                Current_Tile_K_Iter := Current_Tile_K_Iter + Decode_Tensor_K_iter_Add
                Current_Tile_Tensor_K_Iter := Current_Tile_Tensor_K_Iter + 1.U
                when(Current_Tile_K_Iter + Decode_Tensor_K_iter_Add >= Decoding_MacroInst.Application_K)
                {
                    //                                                      vv
                    //如果最后一次K的迭代，不是满的K，那么就要调整K的大小[ohow][oc][ic]
                    //                                            [ M ] [N] [K]
                    //                                                      ^^
                    
                    // Current_ScaratchpadTensor_K := Mux(Current_Tile_K_Iter + Tensor_K.U>= Decoding_MacroInst.Application_K, Decoding_MacroInst.Application_K - Current_Tile_K_Iter, Tensor_K.U)
                    //K要求默认是满的,不够需要程序补零
                    Current_Tile_K_Iter := 0.U
                    Current_Tile_Tensor_K_Iter := 0.U
                    Current_Tile_KW_Index := Current_Tile_KW_Index + 1.U

                    when(Current_Tile_KW_Index + 1.U >= Decoding_MacroInst.kernel_size)//kernel_size=0就G了，死循环了就
                    {
                        //说明是最后一次KW的迭代
                        Current_Tile_KW_Index := 0.U
                        Current_Tile_KH_Index := Current_Tile_KH_Index + 1.U
                        when(Current_Tile_KH_Index + 1.U >= Decoding_MacroInst.kernel_size)
                        {
                            //说明是最后一次KH的迭代
                            Current_Tile_KH_Index := 0.U
                            Have_Store_Micro_Inst := true.B
                            Decode_C_SCP_ID := Mux(Decode_C_SCP_ID === 0.U, 1.U, 0.U)
                            Current_Tile_N_Iter := Current_Tile_N_Iter + Tensor_N.U
                            when(Current_Tile_N_Iter + Tensor_N.U >= Decoding_MacroInst.Application_N)
                            {
                                //                                                                vv
                                //如果最后一次N的迭代，不是满的N，那么就要调整N的大小，大概率不会发生,[ohow][oc][ic]
                                //                                                          [ M ] [N] [K]
                                //                                                                ^^

                                //output-stationary，此时需要发射Store的微指令
                                Current_Tile_N_Iter := 0.U
                                Current_Tile_M_Iter := Current_Tile_M_Iter + Tensor_M.U
                                Current_Tile_OW_Index := Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add
                                Current_Tile_OH_Index := Current_Tile_OH_Index + Decoding_MacroInst.conv_oh_per_add
                                when(Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add >= Decoding_MacroInst.conv_ow_max)
                                {
                                    Current_Tile_OW_Index := Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add - Decoding_MacroInst.conv_ow_max
                                    Current_Tile_OH_Index := Current_Tile_OH_Index + Decoding_MacroInst.conv_oh_per_add + 1.U
                                }
                                when(Current_Tile_M_Iter + Tensor_M.U >= Decoding_MacroInst.Application_M)
                                {
                                    Current_Tile_M_Iter := 0.U
                                    Decoding_MarcoInst_Going := false.B
                                    // Have_Load_Micro_Inst := false.B
                                    // Have_Compute_Micro_Inst := false.B
                                    // Have_Store_Micro_Inst := false.B
                                    //                                                     vvvv
                                    //最后一次M的迭代，不是满的M，那么就要调整M的大小，大概率会发生,[ohow][oc][ic]
                                    //                                                     [ M ] [N] [K]
                                    //                                                     ^^^^
                                    
                                    //宏指令解码结束
                                    //解码完成后，将指令指针指向下一条指令
                                    MarcoInst_FIFO_Decode_Head := WrapInc(MarcoInst_FIFO_Decode_Head, 4)
                                    MacroInst_FIFO_Decode_Finish(MarcoInst_FIFO_Decode_Head) := true.B
                                    StoreMicroInst_Is_Last_Store := true.B
                                    if (YJPDebugEnable)
                                    {
                                        printf("[TaskController<%d>]:MacroInst Decode End!  MacroInst_FIFO_Head = %d, MacroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, MacroInst_FIFO_Head, MacroInst_FIFO_Tail)
                                    }
                                    
                                }
                            }
                        }
                    }
                }

            }

            //生成Load指令
            val Load_MicroInst = Wire(new LoadMicroInst)
            Load_MicroInst.ConherentA := true.B
            Load_MicroInst.ConherentB := true.B
            Load_MicroInst.ConherentC := true.B
            Load_MicroInst.Convolution_Current_KH_Index := Current_Tile_KH_Index
            Load_MicroInst.Convolution_Current_KW_Index := Current_Tile_KW_Index
            Load_MicroInst.Convolution_Current_OH_Index := Current_Tile_OH_Index
            Load_MicroInst.Convolution_Current_OW_Index := Current_Tile_OW_Index

            Load_MicroInst.Is_A_Work := LoadMicroInst_Have_A_work
            Load_MicroInst.Is_B_Work := LoadMicroInst_Have_B_work
            Load_MicroInst.Is_A_Scale_Work := LoadMicroInst_Have_A_Scale_work
            Load_MicroInst.Is_B_Scale_Work := LoadMicroInst_Have_B_Scale_work
            Load_MicroInst.Is_C_Work := LoadMicroInst_Have_C_work

            Load_MicroInst.A_SCPID := Decode_A_SCP_ID
            Load_MicroInst.B_SCPID := Decode_B_SCP_ID
            Load_MicroInst.C_SCPID := Decode_C_SCP_ID

            Load_MicroInst.ScaratchpadTensor_K := Current_ScaratchpadTensor_K
            Load_MicroInst.ScaratchpadTensor_N := Current_ScaratchpadTensor_N
            Load_MicroInst.ScaratchpadTensor_M := Current_ScaratchpadTensor_M

            Load_MicroInst.IsTranspose := Decoding_MacroInst.transpose_result

            Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := Decoding_MacroInst.ApplicationTensor_A_BaseVaddr + Current_Tile_Tensor_K_Iter*(ReduceWidthByte*ReduceGroupSize).U  //TODO:初始地址需要改！因为当前的K移动了！
            Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_Stride_M := Decoding_MacroInst.ApplicationTensor_A_Stride
            // Load_MicroInst.ApplicationTensor_A.BlockTensor_A_BaseVaddr := Decoding_MacroInst.ApplicationTensor_A_BaseVaddr + Current_Tile_Tensor_K_Iter*(ReduceWidthByte*ReduceGroupSize).U
            Load_MicroInst.ApplicationTensor_A.Convolution_OH_DIM_Length := Decoding_MacroInst.conv_oh_max
            Load_MicroInst.ApplicationTensor_A.Convolution_OW_DIM_Length := Decoding_MacroInst.conv_ow_max
            Load_MicroInst.ApplicationTensor_A.Convolution_KH_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_A.Convolution_KW_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_A.Convolution_Stride_H := Decoding_MacroInst.conv_stride
            Load_MicroInst.ApplicationTensor_A.Convolution_Stride_W := Decoding_MacroInst.conv_stride
            Load_MicroInst.ApplicationTensor_A.dataType := Decoding_MacroInst.element_type

            Load_MicroInst.ApplicationScale_A.ApplicationScale_A_BaseVaddr := Decoding_MacroInst.ApplicationScale_A_BaseVaddr
            Load_MicroInst.ApplicationScale_A.BlockScale_A_BaseVaddr := Decoding_MacroInst.ApplicationScale_A_BaseVaddr + Current_ScaratchpadTensor_M * Current_Tile_Tensor_K_Iter * ScaleVecWidth(Decoding_MacroInst.element_type) * ReduceGroupSize.U + Current_Tile_M_Iter * Decoding_MacroInst.Application_K * PEDataType.AdataBitWidth(Decoding_MacroInst.element_type) / ReduceWidth.U * ScaleVecWidth(Decoding_MacroInst.element_type)
            Load_MicroInst.ApplicationScale_A.dataType := Decoding_MacroInst.element_type
            
            Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := Decoding_MacroInst.ApplicationTensor_B_BaseVaddr
            Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_Stride_N := Decoding_MacroInst.ApplicationTensor_B_Stride
            Load_MicroInst.ApplicationTensor_B.BlockTensor_B_BaseVaddr := Decoding_MacroInst.ApplicationTensor_B_BaseVaddr + Current_Tile_K_Iter * PEDataType.AdataBitWidth(Decoding_MacroInst.element_type) / 8.U + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_B_Stride + (Current_Tile_KH_Index * Decoding_MacroInst.kernel_size + Current_Tile_KW_Index)*Decoding_MacroInst.kernel_stride //TODO:kernel_conv_stride!!!
            Load_MicroInst.ApplicationTensor_B.Convolution_KH_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_B.Convolution_KW_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_B.dataType := Decoding_MacroInst.element_type

            Load_MicroInst.ApplicationScale_B.ApplicationScale_B_BaseVaddr := Decoding_MacroInst.ApplicationScale_B_BaseVaddr
            Load_MicroInst.ApplicationScale_B.BlockScale_B_BaseVaddr := Decoding_MacroInst.ApplicationScale_B_BaseVaddr + Current_ScaratchpadTensor_N * Current_Tile_Tensor_K_Iter * ScaleVecWidth(Decoding_MacroInst.element_type) * ReduceGroupSize.U + Current_Tile_N_Iter * Decoding_MacroInst.Application_K * PEDataType.BdataBitWidth(Decoding_MacroInst.element_type) / ReduceWidth.U * ScaleVecWidth(Decoding_MacroInst.element_type)  //TODO:kernel_conv_stride!!!
            Load_MicroInst.ApplicationScale_B.dataType := Decoding_MacroInst.element_type

            Load_MicroInst.ApplicationTensor_C.dataType := Decoding_MacroInst.element_type
            Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr
            Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_Stride_M := Decoding_MacroInst.ApplicationTensor_C_Stride
            // Load_MicroInst.ApplicationTensor_C.BlockTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr + Current_Tile_M_Iter * Decoding_MacroInst.element_type + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_C_Stride//转置？
            Load_MicroInst.ApplicationTensor_C.BlockTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr + Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorLoad,Current_Tile_M_Iter * Decoding_MacroInst.ApplicationTensor_C_Stride,0.U) + Current_Tile_N_Iter * Decoding_MacroInst.bias_data_type
            Load_MicroInst.CLoadTaskInfo.Is_FullLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorLoad, true.B, false.B)
            Load_MicroInst.CLoadTaskInfo.Is_ZeroLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorZeroLoad, true.B, false.B)
            Load_MicroInst.CLoadTaskInfo.Is_RepeatRowLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorRepeatRowLoad, true.B, false.B)

            when(Have_Load_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Load_MicroInst_FIFO(Load_MicroInst_FIFO_Head) := Load_MicroInst.asUInt
                Load_MicroInst_FIFO_Head := WrapInc(Load_MicroInst_FIFO_Head, 4)
                Load_MicroInst_FINISH_Ready_GO(Load_MicroInst_FIFO_Head) := false.B
                Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Head) := false.B
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst Insert!  Load_MicroInst_FIFO_Head = %d, Load_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail)
                    //输出Load_MicroInst.ApplicationTensor_A的所有信息
                    printf("[TaskController<%d>]:Load_MicroInst.ApplicationTensor_A_BaseVaddr = %x, ApplicationTensor_A_Stride = %x\n, ApplicationTensor_B_BaseVaddr = %x, ApplicationTensor_B_Stride = %x\n, ApplicationTensor_C_BaseVaddr = %x, ApplicationTensor_C_Stride = %x\n, ScaratchpadTensor_M = %x, ScaratchpadTensor_N = %x, ScaratchpadTensor_K = %x, conv_stride = %x\n, element_type = %x, bias_type = %x, transpose_result = %x,  conv_oh_max = %x, conv_ow_max = %x, kernel_size = %x, conv_oh_per_add = %x, conv_ow_per_add = %x, conv_oh_index = %x, conv_ow_index = %x\n", io.DebugTimeStampe, Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr, Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_Stride_M, Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr, Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_Stride_N, Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr, Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_Stride_M, Load_MicroInst.ScaratchpadTensor_M, Load_MicroInst.ScaratchpadTensor_N, Load_MicroInst.ScaratchpadTensor_K, Decoding_MacroInst.conv_stride, Decoding_MacroInst.element_type, Decoding_MacroInst.bias_type, Decoding_MacroInst.transpose_result, Decoding_MacroInst.conv_oh_max, Decoding_MacroInst.conv_ow_max, Decoding_MacroInst.kernel_size, Decoding_MacroInst.conv_oh_per_add, Decoding_MacroInst.conv_ow_per_add, Load_MicroInst.Convolution_Current_OH_Index, Load_MicroInst.Convolution_Current_OW_Index)
                    //输出SCPID,分别标注ABC
                    printf("[TaskController<%d>]:Load_MicroInst.SCPID = A:%x, B:%x, C:%x\n", io.DebugTimeStampe, Load_MicroInst.A_SCPID, Load_MicroInst.B_SCPID, Load_MicroInst.C_SCPID)
                }
            }

            //生成Compute指令
            val Compute_MicroInst = Wire(new ComputeMicroInst)
            Compute_MicroInst.ScaratchpadTensor_M := Current_ScaratchpadTensor_M
            Compute_MicroInst.ScaratchpadTensor_N := Current_ScaratchpadTensor_N
            Compute_MicroInst.ScaratchpadTensor_K := Current_ScaratchpadTensor_K
            Compute_MicroInst.Have_Store_Micro_Inst := Have_Store_Micro_Inst
            Compute_MicroInst.DataType   := Decoding_MacroInst.element_type
            Compute_MicroInst.DataType_A := PEDataType.AdataByteWidth(Decoding_MacroInst.element_type) //8bit
            Compute_MicroInst.DataType_B := PEDataType.BdataByteWidth(Decoding_MacroInst.element_type) //8bit
            Compute_MicroInst.DataType_C := PEDataType.CdataByteWidth(Decoding_MacroInst.element_type) //32bit
            Compute_MicroInst.DataType_D := PEDataType.DdataByteWidth(Decoding_MacroInst.element_type) //32bit

            val Have_After_Ops = WireInit(false.B)
            Have_After_Ops := Have_Store_Micro_Inst
            Compute_MicroInst.Is_AfterOps_Tile := Have_After_Ops
            Compute_MicroInst.Have_Aops := Have_After_Ops
            Compute_MicroInst.Is_Transpose := Decoding_MacroInst.transpose_result
            Compute_MicroInst.Is_Reorder_Only_Ops := true.B
            Compute_MicroInst.Is_EasyScale_Only_Ops := false.B
            Compute_MicroInst.Is_VecFIFO_Ops := false.B


            val Compute_Resource_Info = Wire(new ComputeMicroInst_Resource_Info)
            Compute_Resource_Info.A_SCPID := Decode_A_SCP_ID
            Compute_Resource_Info.B_SCPID := Decode_B_SCP_ID
            Compute_Resource_Info.C_SCPID := Decode_C_SCP_ID
            Compute_Resource_Info.Load_Micro_Inst_FIFO_Index := Load_MicroInst_FIFO_Head //由于指令一定同时被入队，所以这里不会有问题
            
            when(Have_Compute_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Compute_MicroInst_FIFO(Compute_MicroInst_FIFO_Head) := Compute_MicroInst.asUInt
                Compute_MicroInst_Resource_Info_FIFO(Compute_MicroInst_FIFO_Head) := Compute_Resource_Info.asUInt
                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FIFO_Head) := false.B
                Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Head) := false.B
                Compute_MicroInst_FIFO_Head := WrapInc(Compute_MicroInst_FIFO_Head, 4)
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst Insert!  Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail)
                    //输出Compute_MicroInst的所有信息
                    printf("[TaskController<%d>]:Compute_MicroInst.ScaratchpadTensor_M = %x, ScaratchpadTensor_N = %x, ScaratchpadTensor_K = %x\n", io.DebugTimeStampe, Compute_MicroInst.ScaratchpadTensor_M, Compute_MicroInst.ScaratchpadTensor_N, Compute_MicroInst.ScaratchpadTensor_K)
                    //输出SCPID
                    printf("[TaskController<%d>]:Compute_MicroInst.A_SCPID = %x, B_SCPID = %x, C_SCPID = %x\n", io.DebugTimeStampe, Compute_Resource_Info.A_SCPID, Compute_Resource_Info.B_SCPID, Compute_Resource_Info.C_SCPID)
                }
            }

            //生成Store指令
            val Store_MicroInst = Wire(new StoreMicroInst)
            Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := Decoding_MacroInst.ApplicationTensor_D_BaseVaddr
            Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_Stride_M := Decoding_MacroInst.ApplicationTensor_D_Stride
            Store_MicroInst.ApplicationTensor_D.BlockTensor_D_BaseVaddr := Mux(Decoding_MacroInst.transpose_result,
                        Decoding_MacroInst.ApplicationTensor_D_BaseVaddr + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_D_Stride + Current_Tile_M_Iter * ResultWidthByte.U,
                        Decoding_MacroInst.ApplicationTensor_D_BaseVaddr + Current_Tile_M_Iter * Decoding_MacroInst.ApplicationTensor_D_Stride + Current_Tile_N_Iter * ResultWidthByte.U)//TODO:转置？result_data_width?
            Store_MicroInst.ApplicationTensor_D.dataType := PEDataType.DdataByteWidth(Decoding_MacroInst.element_type)
            Store_MicroInst.Conherent := true.B
            Store_MicroInst.Is_Transpose := Decoding_MacroInst.transpose_result
            Store_MicroInst.ScaratchpadTensor_M := Current_ScaratchpadTensor_M
            Store_MicroInst.ScaratchpadTensor_N := Current_ScaratchpadTensor_N
            Store_MicroInst.Is_Last_Store := StoreMicroInst_Is_Last_Store

            val Store_Resource_Info = Wire(new StoreMicroInst_Resource_Info)
            Store_Resource_Info.C_SCPID := Decode_C_SCP_ID
            Store_Resource_Info.Compute_Micro_Inst_FIFO_Index := Compute_MicroInst_FIFO_Head
            Store_Resource_Info.Marco_Inst_FIFO_Index := MarcoInst_FIFO_Decode_Head
            when(Have_Store_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Store_MicroInst_FIFO(Store_MicroInst_FIFO_Head) := Store_MicroInst.asUInt
                Store_MicroInst_Resource_Info_FIFO(Store_MicroInst_FIFO_Head) := Store_Resource_Info.asUInt
                Store_MicroInst_FIFO_Head := WrapInc(Store_MicroInst_FIFO_Head, 4)
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Store MicroInst Insert!  Store_MicroInst_FIFO_Head = %d, Store_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail)
                    //输出Store_MicroInst的所有信息
                    printf("[TaskController<%d>]:Store_MicroInst.ApplicationTensor_D_BaseVaddr = %x, ApplicationTensor_D_Stride = %x\n, ScaratchpadTensor_M = %x, ScaratchpadTensor_N = %x, Is_Transpose = %x\n", io.DebugTimeStampe, Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr, Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_Stride_M, Store_MicroInst.ScaratchpadTensor_M, Store_MicroInst.ScaratchpadTensor_N, Store_MicroInst.Is_Transpose)
                    //输出SCPID
                    printf("[TaskController<%d>]:Store_MicroInst.C_SCPID = %x\n", io.DebugTimeStampe, Store_Resource_Info.C_SCPID)
                }
            }
        }
    }

    // 微指令队列
    // val Load_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new LoadMicroInst().getWidth.W))))
    // val Store_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new StoreMicroInst().getWidth.W))))
    // val Compute_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new ComputeMicroInst().getWidth.W))))

    // val Load_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    // val Load_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    // val Load_MicroInst_FIFO_Empty = Load_MicroInst_FIFO_Head === Load_MicroInst_FIFO_Tail
    // val Load_MicroInst_FIFO_Full = WrapInc(Load_MicroInst_FIFO_Head, 4) === Load_MicroInst_FIFO_Tail

    // val Store_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    // val Store_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    // val Store_MicroInst_FIFO_Empty = Store_MicroInst_FIFO_Head === Store_MicroInst_FIFO_Tail
    // val Store_MicroInst_FIFO_Full = WrapInc(Store_MicroInst_FIFO_Head, 4) === Store_MicroInst_FIFO_Tail

    // val Compute_MicroInst_FIFO_Head = RegInit(0.U(2.W))
    // val Compute_MicroInst_FIFO_Tail = RegInit(0.U(2.W))
    // val Compute_MicroInst_FIFO_Empty = Compute_MicroInst_FIFO_Head === Compute_MicroInst_FIFO_Tail
    // val Compute_MicroInst_FIFO_Full = WrapInc(Compute_MicroInst_FIFO_Head, 4) === Compute_MicroInst_FIFO_Tail

    // //微指令执行状态队列
    // val Load_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new LoadMicroInst_Resource_Info().getWidth.W))))
    // val Store_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new StoreMicroInst_Resource_Info().getWidth.W))))
    // val Compute_MicroInst_Resource_Info_FIFO = RegInit(VecInit(Seq.fill(4)(0.U(new ComputeMicroInst_Resource_Info().getWidth.W))))

    val Current_ADC_SCP_ID = RegInit(0.U(2.W))
    val Current_BDC_SCP_ID = RegInit(0.U(2.W))
    val Current_CDC_SCP_ID = RegInit(0.U(2.W))
    val Current_AML_SCP_ID = RegInit(0.U(2.W))
    val Current_BML_SCP_ID = RegInit(0.U(2.W))
    val Current_CML_SCP_ID = RegInit(0.U(2.W))

    io.SCP_CtrlInfo.ADC_SCP_ID := Current_ADC_SCP_ID
    io.SCP_CtrlInfo.BDC_SCP_ID := Current_BDC_SCP_ID
    io.SCP_CtrlInfo.CDC_SCP_ID := Current_CDC_SCP_ID
    io.SCP_CtrlInfo.AML_SCP_ID := Current_AML_SCP_ID
    io.SCP_CtrlInfo.BML_SCP_ID := Current_BML_SCP_ID
    io.SCP_CtrlInfo.CML_SCP_ID := Current_CML_SCP_ID

    //看每个队列里面的微指令，如果有可以发射的微指令，就发射
    val Will_Issuse_CML_Load = WireInit(false.B)

    //只要有可以发射的Load微指令
    val issue_state_idle :: issue_state_issue :: Nil = Enum(2)
    val Load_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Load_Micro_Inst_Wait_A_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_B_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_A_Scale_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_B_Scale_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_C_Finish = RegInit(false.B)

    if (EnablePerfCounter)
    {io.ctrlCounter.ALoad := Load_Micro_Inst_Wait_A_Finish
    io.ctrlCounter.BLoad := Load_Micro_Inst_Wait_B_Finish
    io.ctrlCounter.CLoad := Load_Micro_Inst_Wait_C_Finish}
    when(!Load_MicroInst_FINISH_All)
    {
        val Load_MicroInst = Load_MicroInst_FIFO(Load_MicroInst_FINISH_Head).asTypeOf(new LoadMicroInst)//取出的Load指令

        //发射这条指令，填AML、BML、CML的config输入
        val Can_Issue_AML_Micro_Inst = io.AML_MicroTask_Config.MicroTaskReady && A_SCP_Free(Load_MicroInst.A_SCPID) //缺少ASCP的空闲状态,保证同时任务被发射
        val Can_Issue_BML_Micro_Inst = io.BML_MicroTask_Config.MicroTaskReady && B_SCP_Free(Load_MicroInst.B_SCPID) //缺少BSCP的空闲状态,保证同时任务被发射
        val Can_Issue_ASL_Micro_Inst = io.ASL_MicroTask_Config.MicroTaskReady && A_SCP_Free(Load_MicroInst.A_SCPID) //缺少ASCP的空闲状态,保证同时任务被发射
        val Can_Issue_BSL_Micro_Inst = io.BSL_MicroTask_Config.MicroTaskReady && B_SCP_Free(Load_MicroInst.B_SCPID) //缺少BSCP的空闲状态,保证同时任务被发射 
        val Can_Issue_CML_Micro_Inst = io.CML_MicroTask_Config.MicroTaskReady && C_SCP_Free(Load_MicroInst.C_SCPID) //缺少CSCP的空闲状态,保证同时任务被发射

        val Need_Issue_AML_Micro_Inst = Load_MicroInst.Is_A_Work
        val Need_Issue_BML_Micro_Inst = Load_MicroInst.Is_B_Work
        val Need_Issue_ASL_Micro_Inst = Load_MicroInst.Is_A_Scale_Work
        val Need_Issue_BSL_Micro_Inst = Load_MicroInst.Is_B_Scale_Work
        val Need_Issue_CML_Micro_Inst = Load_MicroInst.Is_C_Work

        val Can_Issue_Load_Micro_Inst = (Can_Issue_AML_Micro_Inst | !Need_Issue_AML_Micro_Inst) && (Can_Issue_BML_Micro_Inst | !Need_Issue_BML_Micro_Inst) && (Can_Issue_BSL_Micro_Inst | !Need_Issue_BSL_Micro_Inst) && (Can_Issue_ASL_Micro_Inst | !Need_Issue_ASL_Micro_Inst) && (Can_Issue_CML_Micro_Inst | !Need_Issue_CML_Micro_Inst)
        // 即need又can，才能发射这条Load微指令
        printf("[TaskController<%d>]:Can_Issue_AML_Micro_Inst = %d, Can_Issue_BML_Micro_Inst = %d, Can_Issue_CML_Micro_Inst = %d, Need_Issue_AML_Micro_Inst = %d, Need_Issue_BML_Micro_Inst = %d, Need_Issue_CML_Micro_Inst = %d\n", io.DebugTimeStampe, Can_Issue_AML_Micro_Inst, Can_Issue_BML_Micro_Inst, Can_Issue_CML_Micro_Inst, Need_Issue_AML_Micro_Inst, Need_Issue_BML_Micro_Inst, Need_Issue_CML_Micro_Inst)
        if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Can_Issue_AML_Micro_Inst = %d, Can_Issue_BML_Micro_Inst = %d, Can_Issue_CML_Micro_Inst = %d, Need_Issue_AML_Micro_Inst = %d, Need_Issue_BML_Micro_Inst = %d, Need_Issue_CML_Micro_Inst = %d\n", io.DebugTimeStampe, Can_Issue_AML_Micro_Inst, Can_Issue_BML_Micro_Inst, Can_Issue_CML_Micro_Inst, Need_Issue_AML_Micro_Inst, Need_Issue_BML_Micro_Inst, Need_Issue_CML_Micro_Inst)
            }
        when(Can_Issue_Load_Micro_Inst && Load_Micro_Inst_Issue_State_Reg === issue_state_idle)
        {
            //发射这条指令

            io.AML_MicroTask_Config.ApplicationTensor_A := Load_MicroInst.ApplicationTensor_A
            io.AML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentA
            io.AML_MicroTask_Config.Convolution_Current_KH_Index := Load_MicroInst.Convolution_Current_KH_Index
            io.AML_MicroTask_Config.Convolution_Current_KW_Index := Load_MicroInst.Convolution_Current_KW_Index
            io.AML_MicroTask_Config.Convolution_Current_OH_Index := Load_MicroInst.Convolution_Current_OH_Index
            io.AML_MicroTask_Config.Convolution_Current_OW_Index := Load_MicroInst.Convolution_Current_OW_Index
            io.AML_MicroTask_Config.ScaratchpadTensor_M := Load_MicroInst.ScaratchpadTensor_M
            io.AML_MicroTask_Config.ScaratchpadTensor_K := Load_MicroInst.ScaratchpadTensor_K

            io.ASL_MicroTask_Config.ApplicationScale_A := Load_MicroInst.ApplicationScale_A
            io.ASL_MicroTask_Config.Conherent        := Load_MicroInst.ConherentA
            io.ASL_MicroTask_Config.ScaratchpadTensor_M := Load_MicroInst.ScaratchpadTensor_M
            io.ASL_MicroTask_Config.ScaratchpadTensor_K := Load_MicroInst.ScaratchpadTensor_K
            
            io.BML_MicroTask_Config.ApplicationTensor_B := Load_MicroInst.ApplicationTensor_B
            io.BML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentB
            io.BML_MicroTask_Config.ScaratchpadTensor_K := Load_MicroInst.ScaratchpadTensor_K
            io.BML_MicroTask_Config.ScaratchpadTensor_N := Load_MicroInst.ScaratchpadTensor_N

            io.BSL_MicroTask_Config.ApplicationScale_B := Load_MicroInst.ApplicationScale_B
            io.BSL_MicroTask_Config.Conherent        := Load_MicroInst.ConherentB
            io.BSL_MicroTask_Config.ScaratchpadTensor_K := Load_MicroInst.ScaratchpadTensor_K
            io.BSL_MicroTask_Config.ScaratchpadTensor_N := Load_MicroInst.ScaratchpadTensor_N
            
            io.CML_MicroTask_Config.ApplicationTensor_C := Load_MicroInst.ApplicationTensor_C
            io.CML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentC
            io.CML_MicroTask_Config.ScaratchpadTensor_M := Load_MicroInst.ScaratchpadTensor_M
            io.CML_MicroTask_Config.ScaratchpadTensor_N := Load_MicroInst.ScaratchpadTensor_N
            io.CML_MicroTask_Config.IsLoadMicroTask := true.B
            io.CML_MicroTask_Config.LoadTaskInfo := Load_MicroInst.CLoadTaskInfo
            io.CML_MicroTask_Config.Is_Transpose := Load_MicroInst.IsTranspose

            A_SCP_Free(Load_MicroInst.A_SCPID)  := false.B
            Current_AML_SCP_ID := Load_MicroInst.A_SCPID
            B_SCP_Free(Load_MicroInst.B_SCPID)  := false.B
            Current_BML_SCP_ID := Load_MicroInst.B_SCPID
            when(Will_Issuse_CML_Load)
            {
                C_SCP_Free(Load_MicroInst.C_SCPID) := false.B
                Current_CML_SCP_ID := Load_MicroInst.C_SCPID
            }

            io.AML_MicroTask_Config.MicroTaskValid := Need_Issue_AML_Micro_Inst
            io.BML_MicroTask_Config.MicroTaskValid := Need_Issue_BML_Micro_Inst
            io.ASL_MicroTask_Config.MicroTaskValid := Need_Issue_ASL_Micro_Inst
            io.BSL_MicroTask_Config.MicroTaskValid := Need_Issue_BSL_Micro_Inst
            io.CML_MicroTask_Config.MicroTaskValid := Need_Issue_CML_Micro_Inst

            Load_Micro_Inst_Issue_State_Reg := issue_state_issue
            Load_Micro_Inst_Wait_A_Finish := Need_Issue_AML_Micro_Inst
            Load_Micro_Inst_Wait_B_Finish := Need_Issue_BML_Micro_Inst
            Load_Micro_Inst_Wait_A_Scale_Finish := Need_Issue_ASL_Micro_Inst
            Load_Micro_Inst_Wait_B_Scale_Finish := Need_Issue_BSL_Micro_Inst
            Load_Micro_Inst_Wait_C_Finish := Need_Issue_CML_Micro_Inst

            Will_Issuse_CML_Load := Need_Issue_CML_Micro_Inst

            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Load MicroInst Issue! Issue AML_MicroTask = %d, Issue BML_MicroTask = %d, Issue CML_MicroTask = %d\n",io.DebugTimeStampe, Need_Issue_AML_Micro_Inst, Need_Issue_BML_Micro_Inst, Need_Issue_CML_Micro_Inst)
                //SCPID
                printf("[TaskController<%d>]:Load MicroInst Issue! AML_SCP_ID = %d, BML_SCP_ID = %d, CML_SCP_ID = %d\n",io.DebugTimeStampe, Current_AML_SCP_ID, Current_BML_SCP_ID, Current_CML_SCP_ID)
            }
        }.elsewhen(Load_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            //等待这条指令完成
            io.AML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_A_Finish
            io.BML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_B_Finish
            io.ASL_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_A_Scale_Finish
            io.BSL_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_B_Scale_Finish
            io.CML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_C_Finish
            when(Load_Micro_Inst_Wait_A_Finish && io.AML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_A_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst A Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Load_Micro_Inst_Wait_B_Finish && io.BML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_B_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst B Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Load_Micro_Inst_Wait_A_Scale_Finish && io.ASL_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_A_Scale_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst A Scale Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Load_Micro_Inst_Wait_B_Scale_Finish && io.BSL_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_B_Scale_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst B Scale Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Load_Micro_Inst_Wait_C_Finish && io.CML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_C_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst C Finish! \n",io.DebugTimeStampe)
                }
            }
            when(!Load_Micro_Inst_Wait_A_Finish && !Load_Micro_Inst_Wait_B_Finish && !Load_Micro_Inst_Wait_A_Scale_Finish && !Load_Micro_Inst_Wait_B_Scale_Finish && !Load_Micro_Inst_Wait_C_Finish)
            {
                Load_MicroInst_FINISH_Head := WrapInc(Load_MicroInst_FINISH_Head, 4)
                Load_MicroInst_FINISH_Ready_GO(Load_MicroInst_FINISH_Head) := true.B
                Load_Micro_Inst_Issue_State_Reg := issue_state_idle
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Load MicroInst Finish!  Load_MicroInst_FIFO_Head = %d, Load_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail)
                }
            }
        }
        
    }

    //提交Load微指令
    when(Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Tail) === true.B)
    {
        Load_MicroInst_FIFO_Tail := WrapInc(Load_MicroInst_FIFO_Tail, 4)
        Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Tail) := false.B
        if (YJPDebugEnable)
        {
            printf("[TaskController<%d>]:Load MicroInst Commit!  Load_MicroInst_FIFO_Head = %d, Load_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail)
        }
    }

    // val issue_state_idle :: issue_state_issue :: Nil = Enum(2)
    val Compute_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Compute_Micro_Inst_Wait_A_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_B_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_A_Scale_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_B_Scale_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_C_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_Aop_Finish = RegInit(false.B)

    if (EnablePerfCounter)
    {io.ctrlCounter.AOPBusy := Compute_Micro_Inst_Wait_Aop_Finish
    io.ctrlCounter.computeInstQueueEmpty := Compute_MicroInst_FINISH_All 
    io.ctrlCounter.computeInstCanIssue := false.B}

    when(!Compute_MicroInst_FINISH_All)
    {
        val Compute_MicroInst = Compute_MicroInst_FIFO(Compute_MicroInst_FINISH_HEAD).asTypeOf(new ComputeMicroInst)//取出的Compute指令
        val Compute_MicroInst_Resource_Info = Compute_MicroInst_Resource_Info_FIFO(Compute_MicroInst_FINISH_HEAD).asTypeOf(new ComputeMicroInst_Resource_Info)

        val Dependent_Load_Finish_Ready_Go = Load_MicroInst_FINISH_Ready_GO(Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index)

        val Is_Block_Scale = Compute_MicroInst.DataType === ElementDataType.DataTypeMxfp8e4m3F32 || Compute_MicroInst.DataType === ElementDataType.DataTypeMxfp8e5m2F32 || Compute_MicroInst.DataType === ElementDataType.DataTypenvfp4F32 || Compute_MicroInst.DataType === ElementDataType.DataTypemxfp4F32

        val Can_Issue_ADC_Micro_Inst = io.ADC_MicroTask_Config.MicroTaskReady //缺少ASCP的空闲状态,保证同时任务被发射
        val Can_Issue_BDC_Micro_Inst = io.BDC_MicroTask_Config.MicroTaskReady //缺少BSCP的空闲状态,保证同时任务被发射
        val Can_Issue_ASC_Micro_Inst = io.ASC_MicroTask_Config.MicroTaskReady
        val Can_Issue_BSC_Micro_Inst = io.BSC_MicroTask_Config.MicroTaskReady
        val Can_Issue_CDC_Micro_Inst = io.CDC_MicroTask_Config.MicroTaskReady //缺少CSCP的空闲状态,保证同时任务被发射
        val Can_Issue_AOP_Micro_Inst = true.B

        val Can_Issue_Compute_Micro_Inst = Can_Issue_ADC_Micro_Inst && Can_Issue_BDC_Micro_Inst && (!Is_Block_Scale || (Can_Issue_BSC_Micro_Inst && Can_Issue_ASC_Micro_Inst)) && Can_Issue_CDC_Micro_Inst && Dependent_Load_Finish_Ready_Go && Can_Issue_AOP_Micro_Inst

        if (EnablePerfCounter)
        {io.ctrlCounter.computeInstCanIssue := Can_Issue_Compute_Micro_Inst && Compute_Micro_Inst_Issue_State_Reg === issue_state_idle}

        // if (YJPDebugEnable)
        // {
        //     printf("[TaskController<%d>]:Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail)
        //     printf("[TaskController<%d>]:Compute_MicroInst_FINISH_HEAD = %d\n",io.DebugTimeStampe, Compute_MicroInst_FINISH_HEAD)
        //     printf("[TaskController<%d>]:Load_MicroInst_FINISH_Ready_GO = %x, Load_MicroInst_FINISH_Ready_Commit = %x\n",io.DebugTimeStampe, Load_MicroInst_FINISH_Ready_GO.asUInt, Load_MicroInst_FINISH_Ready_Commit.asUInt)
        //     //io.ADC_MicroTask_Config.MicroTaskReady, io.BDC_MicroTask_Config.MicroTaskReady, io.CDC_MicroTask_Config.MicroTaskReady Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index
        //     printf("[TaskController<%d>]:ADC_MicroTaskReady = %d, BDC_MicroTaskReady = %d, CDC_MicroTaskReady = %d, Load_Micro_Inst_FIFO_Index = %d\n",io.DebugTimeStampe, io.ADC_MicroTask_Config.MicroTaskReady, io.BDC_MicroTask_Config.MicroTaskReady, io.CDC_MicroTask_Config.MicroTaskReady, Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index)
        // }

        when(Can_Issue_Compute_Micro_Inst && Compute_Micro_Inst_Issue_State_Reg === issue_state_idle)
        {
            io.ADC_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            io.ADC_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            io.ADC_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            io.ADC_MicroTask_Config.ApplicationTensor_A.dataType := Compute_MicroInst.DataType_A //TODO:需要修改

            io.ASC_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            io.ASC_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            io.ASC_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            io.ASC_MicroTask_Config.ApplicationTensor_A.dataType := Compute_MicroInst.DataType //TODO:需要修改

            io.BDC_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            io.BDC_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            io.BDC_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            io.BDC_MicroTask_Config.ApplicationTensor_B.dataType := Compute_MicroInst.DataType_B //TODO:需要修改

            io.BSC_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            io.BSC_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            io.BSC_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            io.BSC_MicroTask_Config.ApplicationTensor_B.dataType := Compute_MicroInst.DataType //TODO:需要修改

            io.CDC_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            io.CDC_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            io.CDC_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := Compute_MicroInst.DataType_C //TODO:需要修改
            io.CDC_MicroTask_Config.ApplicationTensor_D.dataType := Compute_MicroInst.DataType_D //TODO:需要修改
            io.CDC_MicroTask_Config.Is_AfterOps_Tile := Compute_MicroInst.Is_AfterOps_Tile
            io.CDC_MicroTask_Config.Is_Transpose := Compute_MicroInst.Is_Transpose
            io.CDC_MicroTask_Config.Is_Reorder_Only_Ops := Compute_MicroInst.Is_Reorder_Only_Ops
            io.CDC_MicroTask_Config.Is_EasyScale_Only_Ops := false.B        //TODO:需要修改
            io.CDC_MicroTask_Config.Is_VecFIFO_Ops := false.B               //TODO:需要修改

            // io.AOP_MicroTask_Config.ScaratchpadTensor_M := Compute_MicroInst.ScaratchpadTensor_M
            // io.AOP_MicroTask_Config.ScaratchpadTensor_N := Compute_MicroInst.ScaratchpadTensor_N
            // io.AOP_MicroTask_Config.ScaratchpadTensor_K := Compute_MicroInst.ScaratchpadTensor_K
            // io.AOP_MicroTask_Config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32 //TODO:需要修改
            // io.AOP_MicroTask_Config.Is_EasyScale_Only_Ops := Compute_MicroInst.Is_EasyScale_Only_Ops
            // io.AOP_MicroTask_Config.Is_VecFIFO_Ops := Compute_MicroInst.Is_VecFIFO_Ops
            // io.AOP_MicroTask_Config.Is_Transpose := Compute_MicroInst.Is_Transpose
            // io.AOP_MicroTask_Config.Is_Reorder_Only_Ops := Compute_MicroInst.Is_Reorder_Only_Ops
            // io.AOP_MicroTask_Config.CUTEuop := Compute_MicroInst.CUTEuop

            io.MTE_MicroTask_Config.dataType := Compute_MicroInst.DataType
            io.MTE_MicroTask_Config.MicroTaskValid := true.B

            io.ADC_MicroTask_Config.MicroTaskValid := true.B
            io.BDC_MicroTask_Config.MicroTaskValid := true.B
            io.ASC_MicroTask_Config.MicroTaskValid := Is_Block_Scale
            io.BSC_MicroTask_Config.MicroTaskValid := Is_Block_Scale
            io.CDC_MicroTask_Config.MicroTaskValid := true.B
            // io.MTE_MicroTask_Config.valid := true.B
            // io.AOP_MicroTask_Config.MicroTaskValid := Compute_MicroInst.Have_Aops

            Current_ADC_SCP_ID := Compute_MicroInst_Resource_Info.A_SCPID
            Current_BDC_SCP_ID := Compute_MicroInst_Resource_Info.B_SCPID
            Current_CDC_SCP_ID := Compute_MicroInst_Resource_Info.C_SCPID

            Compute_Micro_Inst_Issue_State_Reg := issue_state_issue
            Compute_Micro_Inst_Wait_A_Finish := true.B
            Compute_Micro_Inst_Wait_B_Finish := true.B
            Compute_Micro_Inst_Wait_A_Scale_Finish := Is_Block_Scale
            Compute_Micro_Inst_Wait_B_Scale_Finish := Is_Block_Scale

            Compute_Micro_Inst_Wait_C_Finish := true.B
            Compute_Micro_Inst_Wait_Aop_Finish := Compute_MicroInst.Have_Aops

            Load_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index) := true.B//标记这条Load指令已经可以被提交了
            
            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Compute MicroInst Issue! \n",io.DebugTimeStampe)
                //SCPID
                printf("[TaskController<%d>]:Compute MicroInst Issue! ADC_SCP_ID = %d, BDC_SCP_ID = %d, CDC_SCP_ID = %d\n",io.DebugTimeStampe, Compute_MicroInst_Resource_Info.A_SCPID, Compute_MicroInst_Resource_Info.B_SCPID, Compute_MicroInst_Resource_Info.C_SCPID)
            }
        }.elsewhen(Compute_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            io.ADC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_A_Finish
            io.BDC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_B_Finish
            io.ASC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_A_Scale_Finish
            io.BSC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_B_Scale_Finish
            io.CDC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_C_Finish
            io.AOP_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_Aop_Finish
            when(Compute_Micro_Inst_Wait_A_Finish && io.ADC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_A_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst A Finish! \n",io.DebugTimeStampe)
                }
                A_SCP_Free(Current_ADC_SCP_ID) := true.B
            }
            when(Compute_Micro_Inst_Wait_B_Finish && io.BDC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_B_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst B Finish! \n",io.DebugTimeStampe)
                }
                B_SCP_Free(Current_BDC_SCP_ID) := true.B
            }
            when(Compute_Micro_Inst_Wait_A_Scale_Finish && io.ASC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_A_Scale_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst A Scale Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Compute_Micro_Inst_Wait_B_Scale_Finish && io.BSC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_B_Scale_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst B Scale Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Compute_Micro_Inst_Wait_C_Finish && io.CDC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_C_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst C Finish! \n",io.DebugTimeStampe)
                }
            }
            when(Compute_Micro_Inst_Wait_Aop_Finish && io.AOP_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_Aop_Finish := false.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst Aop Finish! \n",io.DebugTimeStampe)
                }
            }
            when(!Compute_Micro_Inst_Wait_A_Finish && !Compute_Micro_Inst_Wait_B_Finish && !Compute_Micro_Inst_Wait_C_Finish && !Compute_Micro_Inst_Wait_B_Scale_Finish && !Compute_Micro_Inst_Wait_A_Scale_Finish)
            {
                Compute_MicroInst_FINISH_HEAD := WrapInc(Compute_MicroInst_FINISH_HEAD, 4)
                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FINISH_HEAD) := true.B
                Compute_Micro_Inst_Issue_State_Reg := issue_state_idle

                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FINISH_HEAD) := true.B
            
                when(Compute_MicroInst.Have_Store_Micro_Inst === false.B)
                {
                    Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FINISH_HEAD) := true.B
                    // Compute_Micro_Inst_Issue_State_Reg := issue_state_idle
                    if (YJPDebugEnable)
                    {
                        printf("[TaskController<%d>]:Compute MicroInst Finish Without Store!Go to commit!!  Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail)
                    }
                }

                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Compute MicroInst Finish!  Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail)
                }
            }
        }
    }

    //提交Compute微指令
    when(Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Tail) === true.B)
    {
        Compute_MicroInst_FIFO_Tail := WrapInc(Compute_MicroInst_FIFO_Tail, 4)
        Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Tail) := false.B
        if (YJPDebugEnable)
        {
            printf("[TaskController<%d>]:Compute MicroInst Commit!  Compute_MicroInst_FIFO_Head = %d, Compute_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail)
        }
    }

    val Store_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Store_Micro_Inst_Wait_C_Finish = RegInit(false.B)
    val Store_Micro_Inst_Is_Last_Store = RegInit(false.B)
    if (EnablePerfCounter)
    {io.ctrlCounter.DStore := Store_Micro_Inst_Wait_C_Finish}
    when(!Store_MicroInst_FIFO_Empty)
    {
        val Store_MicroInst = Store_MicroInst_FIFO(Store_MicroInst_FIFO_Tail).asTypeOf(new StoreMicroInst)//取出的Store指令
        val Store_MicroInst_Resource_Info = Store_MicroInst_Resource_Info_FIFO(Store_MicroInst_FIFO_Tail).asTypeOf(new StoreMicroInst_Resource_Info)

        val Dependent_Compute_Finish_Ready_Go = Compute_MicroInst_FINISH_Ready_GO(Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index)

        val Can_Issue_CML_Micro_Inst = io.CML_MicroTask_Config.MicroTaskReady //缺少DSCP的空闲状态,保证同时任务被发射

        val Can_Issue_Store_Micro_Inst = Can_Issue_CML_Micro_Inst && Dependent_Compute_Finish_Ready_Go

        // if (YJPDebugEnable)
        // {
        //     printf("[TaskController<%d>]:Store_MicroInst_FIFO_Head = %d, Store_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail)
        //     printf("[TaskController<%d>]:Store_MicroInst_FINISH_HEAD = %d\n",io.DebugTimeStampe, Store_MicroInst_FINISH_HEAD)
        //     printf("[TaskController<%d>]:Compute_MicroInst_FINISH_Ready_GO = %x, Compute_MicroInst_FINISH_Ready_Commit = %x\n",io.DebugTimeStampe, Compute_MicroInst_FINISH_Ready_GO.asUInt, Compute_MicroInst_FINISH_Ready_Commit.asUInt)
        //     //io.ADC_MicroTask_Config.MicroTaskReady, io.BDC_MicroTask_Config.MicroTaskReady, io.CDC_MicroTask_Config.MicroTaskReady Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index
        //     printf("[TaskController<%d>]:DDC_MicroTaskReady = %d, Compute_Micro_Inst_FIFO_Index = %d\n",io.DebugTimeStampe, Can_Issue_CML_Micro_Inst, Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index)
        // }

        when((!Will_Issuse_CML_Load)&& Can_Issue_Store_Micro_Inst && Store_Micro_Inst_Issue_State_Reg === issue_state_idle)
        {
            io.CML_MicroTask_Config.ApplicationTensor_D := Store_MicroInst.ApplicationTensor_D
            io.CML_MicroTask_Config.Conherent        := Store_MicroInst.Conherent
            io.CML_MicroTask_Config.ScaratchpadTensor_M := Store_MicroInst.ScaratchpadTensor_M
            io.CML_MicroTask_Config.ScaratchpadTensor_N := Store_MicroInst.ScaratchpadTensor_N
            io.CML_MicroTask_Config.IsLoadMicroTask := false.B
            io.CML_MicroTask_Config.IsStoreMicroTask := true.B
            io.CML_MicroTask_Config.Is_Transpose := Store_MicroInst.Is_Transpose
            Store_Micro_Inst_Is_Last_Store := Store_MicroInst.Is_Last_Store

            Current_CML_SCP_ID := Store_MicroInst_Resource_Info.C_SCPID
            io.CML_MicroTask_Config.MicroTaskValid := true.B
            Store_Micro_Inst_Wait_C_Finish := true.B
            Store_Micro_Inst_Issue_State_Reg := issue_state_issue
            Compute_MicroInst_FINISH_Ready_Commit(Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index) := true.B//标记这条Compute指令已经可以被提交了
            if (YJPDebugEnable)
            {
                printf("[TaskController<%d>]:Store MicroInst Issue! \n",io.DebugTimeStampe)
            }

        }.elsewhen(Store_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            io.CML_MicroTask_Config.MicroTaskEndReady := Store_Micro_Inst_Wait_C_Finish
            when(Store_Micro_Inst_Wait_C_Finish && io.CML_MicroTask_Config.MicroTaskEndValid)
            {
                Store_Micro_Inst_Wait_C_Finish := false.B
                C_SCP_Free(Store_MicroInst_Resource_Info.C_SCPID) := true.B
                if(YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Store MicroInst C Finish! \n",io.DebugTimeStampe)
                }
            }
            when(!Store_Micro_Inst_Wait_C_Finish)
            {
                Store_MicroInst_FIFO_Tail := WrapInc(Store_MicroInst_FIFO_Tail, 4)
                Store_Micro_Inst_Issue_State_Reg := issue_state_idle
                when(Store_Micro_Inst_Is_Last_Store)
                {
                    MacroInst_FIFO_Total_Finish(Store_MicroInst_Resource_Info.Marco_Inst_FIFO_Index) := true.B
                    if (YJPDebugEnable)
                    {
                        printf("[TaskController<%d>]:MacroInst Finish!  MarcoInst_FIFO_Index = %d\n",io.DebugTimeStampe, Store_MicroInst_Resource_Info.Marco_Inst_FIFO_Index)
                    }
                }
                if (YJPDebugEnable)
                {
                    printf("[TaskController<%d>]:Store MicroInst Finish!  Store_MicroInst_FIFO_Head = %d, Store_MicroInst_FIFO_Tail = %d\n",io.DebugTimeStampe, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail)
                }
            }
        }


    }


}
