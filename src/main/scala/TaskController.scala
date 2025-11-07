
package cute

import chisel3._
import chisel3.util._
import cute.Bundles._
import org.chipsalliance.cde.config._
import utility.ChiselDB
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
        val CDC_MicroTask_Config = (new CDCMicroTaskConfigIO)
        val AML_MicroTask_Config = (new AMLMicroTaskConfigIO)
        val BML_MicroTask_Config = (new BMLMicroTaskConfigIO)
        val CML_MicroTask_Config = (new CMLMicroTaskConfigIO)
        val MTE_MicroTask_Config = (new MTEMicroTaskConfigIO)
        val AOP_MicroTask_Config = (new AfterOpsMicroTaskConfigIO)
        val MReg_CtrlInfo               = (new MRegControlInfo)
        val DebugTimeStampe = Input(UInt(32.W))
        val ctrlCounter = Output(new CTRLCounter)
        // val MMU_Config_Info = (new MMUConfigInfo)
        // val MatrixTE_MicroTask_Config = DecoupledIO(new MatrixTEMicroTaskConfigIO)
    })

    val get_configred = RegInit(false.B)

    io.ctrlCounter.getConfigured := get_configred

    // ChiselDB Bundle definitions for TaskController
    // Base class for all event entries to reduce code duplication
    // All events share: eventType, head, tail, and FIFO_Index fields
    abstract class EventEntryBase(FifoDepthWidth: Int) extends Bundle {
      val eventType = UInt(4.W)
      val head = UInt(FifoDepthWidth.W)
      val tail = UInt(FifoDepthWidth.W)
      val FIFO_Index = UInt(FifoDepthWidth.W)  // All FIFOs use 3-bit index
    }

    // Macro Instruction Event Entry
    // EventType: 0: AutoClear, 1: Insert, 2: InsertFull, 3: DecodeStart, 4: Decode, 5: DecodeEnd, 6: Finish
    class MacroInstEventEntry extends EventEntryBase(MarcoInstFIFODepthBitSize) {
      val Macro_Inst = new MacroInst
    }

    // Load Micro Instruction Event Entry
    // EventType: 0: Insert, 1: Issue, 2: AFinish, 3: BFinish, 4: CFinish, 5: Finish, 6: Commit
    class LoadMicroInstEventEntry extends EventEntryBase(2) {
      val Load_MicroInst = new LoadMicroInst
    }

    // Compute Micro Instruction Event Entry
    // EventType: 0: Insert, 1: Issue, 2: AFinish, 3: BFinish, 4: CFinish, 5: AopFinish, 6: Finish, 7: FinishWithoutStore, 8: Commit
    class ComputeMicroInstEventEntry extends EventEntryBase(2) {
      val Compute_MicroInst = new ComputeMicroInst
    }

    // Store Micro Instruction Event Entry
    // EventType: 0: Insert, 1: Issue, 2: CFinish, 3: Finish
    class StoreMicroInstEventEntry extends EventEntryBase(2) {
      val Store_MicroInst = new StoreMicroInst
    }

    // Create ChiselDB tables
    val macroInstEventTable = ChiselDB.createTable("MacroInstEvent", new MacroInstEventEntry, basicDB = true)
    val loadMicroInstEventTable = ChiselDB.createTable("LoadMicroInstEvent", new LoadMicroInstEventEntry, basicDB = true)
    val computeMicroInstEventTable = ChiselDB.createTable("ComputeMicroInstEvent", new ComputeMicroInstEventEntry, basicDB = true)
    val storeMicroInstEventTable = ChiselDB.createTable("StoreMicroInstEvent", new StoreMicroInstEventEntry, basicDB = true)

    // Helper class to bundle entry and enable signal together with logging capability
    class EventLogger[T <: EventEntryBase](entryType: T) {
      val entry = WireInit(0.U.asTypeOf(entryType))
      val en = WireInit(false.B)
      
      // Log method that can be called with eventType, head, tail, table, and site
      def log(eventType: UInt, head: UInt, tail: UInt, table: utility.Table[T], site: String): Unit = {
        entry.eventType := eventType
        entry.head := head
        entry.tail := tail
        table.log(entry, en, site, clock, reset)
      }
    }

    // Macro instruction event loggers
    val macro_autoclear = new EventLogger(new MacroInstEventEntry)
    val macro_insert = new EventLogger(new MacroInstEventEntry)
    val macro_insertfull = new EventLogger(new MacroInstEventEntry)
    val macro_decodestart = new EventLogger(new MacroInstEventEntry)
    val macro_decodeend = new EventLogger(new MacroInstEventEntry)
    val macro_finish = new EventLogger(new MacroInstEventEntry)

    // Load micro instruction event loggers
    val load_insert = new EventLogger(new LoadMicroInstEventEntry)
    val load_issue = new EventLogger(new LoadMicroInstEventEntry)
    val load_afinish = new EventLogger(new LoadMicroInstEventEntry)
    val load_bfinish = new EventLogger(new LoadMicroInstEventEntry)
    val load_cfinish = new EventLogger(new LoadMicroInstEventEntry)
    val load_finish = new EventLogger(new LoadMicroInstEventEntry)
    val load_commit = new EventLogger(new LoadMicroInstEventEntry)
    
    // Compute micro instruction event loggers
    val comp_insert = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_issue = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_afinish = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_bfinish = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_cfinish = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_aopfinish = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_finish = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_finish_without_store = new EventLogger(new ComputeMicroInstEventEntry)
    val comp_commit = new EventLogger(new ComputeMicroInstEventEntry)

    // Store micro instruction event loggers
    val store_insert = new EventLogger(new StoreMicroInstEventEntry)
    val store_issue = new EventLogger(new StoreMicroInstEventEntry)
    val store_cfinish = new EventLogger(new StoreMicroInstEventEntry)
    val store_finish = new EventLogger(new StoreMicroInstEventEntry)

    //ADC_MicroTask_Config 的 默认配置
    io.ADC_MicroTask_Config.Is_Transpose := false.B
    io.ADC_MicroTask_Config.ApplicationTensor_A := 0.U.asTypeOf(io.ADC_MicroTask_Config.ApplicationTensor_A)
    io.ADC_MicroTask_Config.MatrixRegTensor_K := 0.U
    io.ADC_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.ADC_MicroTask_Config.MatrixRegTensor_M := 0.U
    io.ADC_MicroTask_Config.MicroTaskValid := false.B
    io.ADC_MicroTask_Config.MicroTaskEndReady := false.B

    //BDC_MicroTask_Config 的 默认配置
    io.BDC_MicroTask_Config.Is_Transpose := false.B
    io.BDC_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BDC_MicroTask_Config.ApplicationTensor_B)
    io.BDC_MicroTask_Config.MatrixRegTensor_K := 0.U
    io.BDC_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.BDC_MicroTask_Config.MatrixRegTensor_M := 0.U
    io.BDC_MicroTask_Config.MicroTaskValid := false.B
    io.BDC_MicroTask_Config.MicroTaskEndReady := false.B

    //CDC_MicroTask_Config 的 默认配置
    io.CDC_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CDC_MicroTask_Config.ApplicationTensor_C)
    io.CDC_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CDC_MicroTask_Config.ApplicationTensor_D)
    io.CDC_MicroTask_Config.MatrixRegTensor_K := 0.U
    io.CDC_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.CDC_MicroTask_Config.MatrixRegTensor_M := 0.U
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
    io.AML_MicroTask_Config.MatrixRegTensor_M := 0.U
    io.AML_MicroTask_Config.MatrixRegTensor_K := 0.U
    io.AML_MicroTask_Config.Convolution_Current_OH_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_OW_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_KH_Index := 0.U
    io.AML_MicroTask_Config.Convolution_Current_KW_Index := 0.U
    io.AML_MicroTask_Config.Conherent := false.B
    io.AML_MicroTask_Config.MicroTaskValid := false.B
    io.AML_MicroTask_Config.MicroTaskEndReady := false.B

    //BML_MicroTask_Config 的 默认配置
    io.BML_MicroTask_Config.ApplicationTensor_B := 0.U.asTypeOf(io.BML_MicroTask_Config.ApplicationTensor_B)
    io.BML_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.BML_MicroTask_Config.MatrixRegTensor_K := 0.U
    // io.BML_MicroTask_Config.Convolution_Current_KH_Index := 0.U
    // io.BML_MicroTask_Config.Convolution_Current_KW_Index := 0.U
    io.BML_MicroTask_Config.Conherent := false.B
    io.BML_MicroTask_Config.MicroTaskValid := false.B
    io.BML_MicroTask_Config.MicroTaskEndReady := false.B

    //CML_MicroTask_Config 的 默认配置
    io.CML_MicroTask_Config.ApplicationTensor_C := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_C)
    io.CML_MicroTask_Config.ApplicationTensor_D := 0.U.asTypeOf(io.CML_MicroTask_Config.ApplicationTensor_D)
    io.CML_MicroTask_Config.LoadTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.LoadTaskInfo)
    io.CML_MicroTask_Config.StoreTaskInfo := 0.U.asTypeOf(io.CML_MicroTask_Config.StoreTaskInfo)
    io.CML_MicroTask_Config.Conherent := false.B
    io.CML_MicroTask_Config.Is_Transpose := false.B
    io.CML_MicroTask_Config.MatrixRegTensor_M := 0.U
    io.CML_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.CML_MicroTask_Config.IsLoadMicroTask := false.B
    io.CML_MicroTask_Config.IsStoreMicroTask := false.B
    io.CML_MicroTask_Config.MicroTaskValid := false.B
    io.CML_MicroTask_Config.MicroTaskEndReady := false.B

    io.MReg_CtrlInfo.ADC_MReg_ID := 0.U
    io.MReg_CtrlInfo.BDC_MReg_ID := 0.U
    io.MReg_CtrlInfo.CDC_MReg_ID := 0.U
    io.MReg_CtrlInfo.AML_MReg_ID := 1.U
    io.MReg_CtrlInfo.BML_MReg_ID := 1.U
    io.MReg_CtrlInfo.CML_MReg_ID := 1.U

    io.MTE_MicroTask_Config.dataType := ElementDataType.DataTypeUndef
    // io.MTE_MicroTask_Config.valid := false.B

    //AOP_MicroTask_Config 的 默认配置
    io.AOP_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeUndef
    io.AOP_MicroTask_Config.ApplicationTensor_D.dataType := ElementDataType.DataTypeUndef
    io.AOP_MicroTask_Config.MatrixRegTensor_M := 0.U
    io.AOP_MicroTask_Config.MatrixRegTensor_N := 0.U
    io.AOP_MicroTask_Config.MatrixRegTensor_K := 0.U
    io.AOP_MicroTask_Config.Is_Transpose := false.B
    io.AOP_MicroTask_Config.Is_Reorder_Only_Ops := false.B
    io.AOP_MicroTask_Config.Is_EasyScale_Only_Ops := false.B
    io.AOP_MicroTask_Config.Is_VecFIFO_Ops := false.B
    io.AOP_MicroTask_Config.MicroTaskValid := false.B
    io.AOP_MicroTask_Config.MicroTaskEndReady := false.B
    io.AOP_MicroTask_Config.CUTEuop := 0.U.asTypeOf(io.AOP_MicroTask_Config.CUTEuop)

    io.ygjkctrl.acc_running := false.B
    io.ygjkctrl.InstFIFO_Finish := 0.U
    io.ygjkctrl.InstFIFO_Full := 0.U
    io.ygjkctrl.InstFIFO_Info := 0.U
    io.ygjkctrl.mrelease.valid := false.B
    io.ygjkctrl.mrelease.bits.tokenRd.foreach(_ := false.B)
    io.instfifo_release := 0.U

    //TODO:构思微指令Test的流程
    

    //TODO:12.17先完成宏指令的流程，然后再完成微指令的流程
    
    //宏指令描述寄存器
    val MacroInst_Init = WireInit(0.U.asTypeOf(new MacroInst()))
    MacroInst_Init.conv_stride := 1.U
    MacroInst_Init.conv_oh_max := 1.U
    MacroInst_Init.conv_ow_max := 16384.U
    MacroInst_Init.kernel_size := 1.U
    MacroInst_Init.conv_oh_per_add := 0.U
    MacroInst_Init.conv_ow_per_add := Tensor_MN.U
    MacroInst_Init.conv_oh_index := 0.U
    MacroInst_Init.conv_ow_index := 0.U // TODO: make sure this is correct
    val MacroInst_Reg = RegInit(MacroInst_Init)
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
    val MacroInst_FIFO = RegInit(VecInit(Seq.fill(MarcoInstFIFODepth)(0.U.asTypeOf(new MacroInst()))))
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
            io.ygjkctrl.mrelease.valid := MacroInst_FIFO(MacroInst_FIFO_Tail).need_mrelease
            io.ygjkctrl.mrelease.bits.tokenRd := MacroInst_FIFO(MacroInst_FIFO_Tail).token
            MacroInst_FIFO(MacroInst_FIFO_Tail).need_mrelease := false.B

            // ChiselDB
            macro_autoclear.entry.Macro_Inst := MacroInst_FIFO(MacroInst_FIFO_Tail)
            macro_autoclear.entry.FIFO_Index := MacroInst_FIFO_Tail
            macro_autoclear.en := true.B
        }
    }

    io.ygjkctrl.InstFIFO_Info := MacroInst_FIFO_Valid.asUInt
    io.ygjkctrl.InstFIFO_Full := MacroInst_FIFO_Full
    io.ygjkctrl.InstFIFO_Finish := MacroInst_FIFO_Total_Finish.asUInt
    io.ygjkctrl.amuCtrl.ready := !MacroInst_FIFO_Full

    io.ctrlCounter.InstQueueEmpty := MacroInst_FIFO_Empty

    when(io.ygjkctrl.amuCtrl.fire) {
      val MacroInst_Reg_Wire = Wire(new MacroInst)
      MacroInst_Reg_Wire := MacroInst_Reg
      MacroInst_Reg_Wire.need_mrelease := false.B // default: no mrelease

      val amuCtrl_Wire = Wire(new AmuCtrlIO)
      amuCtrl_Wire := io.ygjkctrl.amuCtrl.bits

      val amuMma_Wire = Wire(new AmuMmaIO)
      amuMma_Wire := amuCtrl_Wire.data.asTypeOf(new AmuMmaIO)
      dontTouch(amuMma_Wire) // for debug
      val amuLsu_Wire = Wire(new AmuLsuIO)
      amuLsu_Wire := amuCtrl_Wire.data.asTypeOf(new AmuLsuIO)
      dontTouch(amuLsu_Wire) // for debug
      val amuRelease_Wire = Wire(new AmuReleaseIO)
      amuRelease_Wire := amuCtrl_Wire.data.asTypeOf(new AmuReleaseIO)
      dontTouch(amuRelease_Wire) // for debug

      when(amuCtrl_Wire.op === AmuCtrlIO.mmaOp()) {
        MacroInst_Reg_Wire.Application_M := amuMma_Wire.mtilem
        MacroInst_Reg_Wire.Application_N := amuMma_Wire.mtilen
        MacroInst_Reg_Wire.Application_K := amuMma_Wire.mtilek
        // MacroInst_Reg_Wire.element_type := amuMma_Wire.types // TODO: translate me!
        // MacroInst_Reg_Wire.bias_data_type := amuMma_Wire.typed // TODO: translate me!
        MacroInst_Reg_Wire.bias_type := CMemoryLoaderTaskType.TaskTypeTensorLoad
        MacroInst_Reg_Wire.is_fp := amuMma_Wire.isfp

        MacroInst_Reg := MacroInst_Reg_Wire
        get_configred := true.B
      }.elsewhen(amuCtrl_Wire.op === AmuCtrlIO.mlsOp()) {
        when(amuLsu_Wire.ls === 0.U) { // matrix load
          when(amuLsu_Wire.isA === true.B) {
            MacroInst_Reg_Wire.ApplicationTensor_A_BaseVaddr := amuLsu_Wire.baseAddr
            MacroInst_Reg_Wire.ApplicationTensor_A_Stride := amuLsu_Wire.stride

            MacroInst_Reg_Wire.Application_M := amuLsu_Wire.row
            MacroInst_Reg_Wire.Application_K := amuLsu_Wire.column

            switch(amuLsu_Wire.widths.asUInt) {
              is(MSew.e32) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth32
              }
              is(MSew.e16) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth16
              }
              is(MSew.e8) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth8
              }
              is(MSew.e4) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth4
              }
            }

            MacroInst_Reg := MacroInst_Reg_Wire
            get_configred := true.B
          }.elsewhen(amuLsu_Wire.isB === true.B) {
            MacroInst_Reg_Wire.ApplicationTensor_B_BaseVaddr := amuLsu_Wire.baseAddr
            MacroInst_Reg_Wire.ApplicationTensor_B_Stride := amuLsu_Wire.stride

            MacroInst_Reg_Wire.Application_N := amuLsu_Wire.row
            MacroInst_Reg_Wire.Application_K := amuLsu_Wire.column

            switch(amuLsu_Wire.widths.asUInt) {
              is(MSew.e32) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth32
              }
              is(MSew.e16) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth16
              }
              is(MSew.e8) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth8
              }
              is(MSew.e4) {
                MacroInst_Reg_Wire.element_type := ElementDataType.DataTypeWidth4
              }
            }

            MacroInst_Reg := MacroInst_Reg_Wire
            get_configred := true.B
          }.elsewhen(amuLsu_Wire.isC === true.B) {
            MacroInst_Reg_Wire.ApplicationTensor_C_BaseVaddr := amuLsu_Wire.baseAddr
            MacroInst_Reg_Wire.ApplicationTensor_C_Stride := amuLsu_Wire.stride

            MacroInst_Reg_Wire.Application_M := amuLsu_Wire.row
            MacroInst_Reg_Wire.Application_N := amuLsu_Wire.column

            switch(amuLsu_Wire.widths.asUInt) {
              is(MSew.e32) {
                MacroInst_Reg_Wire.bias_data_type := ElementDataType.DataTypeWidth32
              }
              is(MSew.e16) {
                MacroInst_Reg_Wire.bias_data_type := ElementDataType.DataTypeWidth16
              }
              is(MSew.e8) {
                MacroInst_Reg_Wire.bias_data_type := ElementDataType.DataTypeWidth8
              }
              is(MSew.e4) {
                MacroInst_Reg_Wire.bias_data_type := ElementDataType.DataTypeWidth4
              }
            }
            MacroInst_Reg_Wire.bias_type := CMemoryLoaderTaskType.TaskTypeTensorLoad

            MacroInst_Reg := MacroInst_Reg_Wire
            get_configred := true.B
          }
        }.otherwise { // matrix store
          assert(amuLsu_Wire.isC === true.B, "AMU LSU is not C")
          MacroInst_Reg_Wire.ApplicationTensor_D_BaseVaddr := amuLsu_Wire.baseAddr
          MacroInst_Reg_Wire.ApplicationTensor_D_Stride := amuLsu_Wire.stride

          MacroInst_Reg_Wire.Application_M := amuLsu_Wire.row
          MacroInst_Reg_Wire.Application_N := amuLsu_Wire.column

          MacroInst_Reg_Wire.bias_type := CMemoryLoaderTaskType.TaskTypeTensorLoad

          MacroInst_Reg_Wire.conv_oh_max := 1.U
          MacroInst_Reg_Wire.conv_ow_max := MacroInst_Reg_Wire.Application_M

          assert(amuLsu_Wire.widths.asUInt === MSew.e32, "Store matrix only support e32")
          
          when (!MacroInst_FIFO_Full) {
            MacroInst_FIFO(MacroInst_FIFO_Head) := MacroInst_Reg_Wire
            MacroInst_FIFO_Valid(MacroInst_FIFO_Head) := true.B
            MacroInst_FIFO_Decode_Finish(MacroInst_FIFO_Head) := false.B
            MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Head) := false.B
            MacroInst_FIFO_Head := WrapInc(MacroInst_FIFO_Head, MarcoInstFIFODepth)
            get_configred := false.B

            macro_insert.entry.Macro_Inst := MacroInst_Reg_Wire
            macro_insert.entry.FIFO_Index := MacroInst_FIFO_Head
            macro_insert.en := true.B
          }.otherwise {
            // ChiselDB
            macro_insertfull.entry.Macro_Inst := MacroInst_Reg_Wire
            macro_insertfull.entry.FIFO_Index := MacroInst_FIFO_Head
            macro_insertfull.en := true.B
          }
        }
      }.elsewhen(amuCtrl_Wire.op === AmuCtrlIO.releaseOp()) {
        when(MacroInst_FIFO_Empty || MacroInst_FIFO_Total_Finish(MacroInst_FIFO_Head)) {
          // When there's no matrix store in fifo, then directly return in mrelease
          io.ygjkctrl.mrelease.valid := true.B
          io.ygjkctrl.mrelease.bits.tokenRd(amuRelease_Wire.tokenRd) := true.B
        }.otherwise {
          // Update mrelease info in MacroInst_FIFO
          MacroInst_FIFO(WrapDec(MacroInst_FIFO_Head, MarcoInstFIFODepth)).need_mrelease := true.B
          MacroInst_FIFO(WrapDec(MacroInst_FIFO_Head, MarcoInstFIFODepth)).token(amuRelease_Wire.tokenRd) := true.B
        }
      }.otherwise {
        // panic
        printf("Invalid AMU Ctrl Op: %d\n", amuCtrl_Wire.op)
      }
    }
    

    //当宏指令FIFO不为空时，且宏指令不在译码，将宏指令FIFO的指令取出
    val Decoding_MacroInst = MacroInst_FIFO(MarcoInst_FIFO_Decode_Head)
    val MarcoInst_Can_Decode = MacroInst_FIFO_Valid(MarcoInst_FIFO_Decode_Head) && !MacroInst_FIFO_Decode_Finish(MarcoInst_FIFO_Decode_Head)

    val Decoding_MarcoInst_Going = RegInit(false.B)

    io.ctrlCounter.InstCanDecode := MarcoInst_Can_Decode

    //宏指令译码时，会不断向微指令发射队列发射微指令
    //微指令发射队列，深度为8,4,4
    //Load,Compute,Store，三个阶段的微指令

    // MReg  MReg MTE  MReg
    //  |    |   |    |
    //  v    v   v    v
    //Load->Compute->Store，A->B，B阶段的微指令只由A阶段的微指令的完成情况和B阶段的资源情况决定

    // 合并A和B的寄存器堆状态：AB_MReg_Free[0-1]对应A矩阵，AB_MReg_Free[2-3]对应B矩阵
    val AB_MReg_Free = RegInit(VecInit(Seq.fill(4)(true.B)))
    val C_MReg_Free = RegInit(VecInit(Seq.fill(2)(true.B)))
    
    // ====================================================================
    // Scoreboard 实例
    // 【步骤3/3：Load、Compute、Store 全部切换到 Scoreboard】
    // - Load发射：查询scoreboard.io.query.load（已完成 - 步骤3.3）
    // - Compute发射：查询scoreboard.io.query.compute（已完成 - 步骤3.4）
    // - Store发射：查询scoreboard.io.query.store（已完成 - 步骤3.5）
    // - 过渡期：
    //   * 仍保留 MReg_Free 更新，用于验证 Scoreboard 正确性
    //   * 后续可以移除所有 MReg_Free 相关代码（步骤3.6）
    // ====================================================================
    val scoreboard = Module(new Scoreboard)
    
    // 查询接口默认值（将在Load/Compute/Store发射时动态设置）
    scoreboard.io.query.load.valid := false.B
    scoreboard.io.query.load.bits := DontCare
    scoreboard.io.query.compute.valid := false.B
    scoreboard.io.query.compute.bits := DontCare
    scoreboard.io.query.store.valid := false.B
    scoreboard.io.query.store.bits := DontCare
    
    // Update 接口默认值
    scoreboard.io.update.load_allocate := false.B
    scoreboard.io.update.load_alloc_a_reg := 0.U
    scoreboard.io.update.load_alloc_b_reg := 0.U
    scoreboard.io.update.load_alloc_c_reg := 0.U
    scoreboard.io.update.load_alloc_has_a := false.B
    scoreboard.io.update.load_alloc_has_b := false.B
    scoreboard.io.update.load_alloc_has_c := false.B
    scoreboard.io.update.load_alloc_fifo_idx := 0.U
    
    scoreboard.io.update.load_finish_a := false.B
    scoreboard.io.update.load_finish_a_reg := 0.U
    scoreboard.io.update.load_finish_b := false.B
    scoreboard.io.update.load_finish_b_reg := 0.U
    scoreboard.io.update.load_finish_c := false.B
    scoreboard.io.update.load_finish_c_reg := 0.U
    
    scoreboard.io.update.compute_issue := false.B
    scoreboard.io.update.compute_issue_a_reg := 0.U
    scoreboard.io.update.compute_issue_b_reg := 0.U
    scoreboard.io.update.compute_issue_c_reg := 0.U
    scoreboard.io.update.compute_issue_fifo_idx := 0.U
    
    scoreboard.io.update.compute_read_finish_a := false.B
    scoreboard.io.update.compute_read_finish_a_reg := 0.U
    scoreboard.io.update.compute_read_finish_b := false.B
    scoreboard.io.update.compute_read_finish_b_reg := 0.U
    
    scoreboard.io.update.compute_write_finish_c := false.B
    scoreboard.io.update.compute_write_finish_c_reg := 0.U
    
    scoreboard.io.update.store_issue := false.B
    scoreboard.io.update.store_issue_c_reg := 0.U
    scoreboard.io.update.store_issue_fifo_idx := 0.U
    
    scoreboard.io.update.store_finish := false.B
    scoreboard.io.update.store_finish_c_reg := 0.U
    
    // ====================================================================
    // Scoreboard 验证：对比 Scoreboard 状态与 MReg_Free 的一致性
    // ====================================================================
    
    // 验证 AB 寄存器堆状态的一致性
    for (i <- 0 until 4) {
        // Assertion 1: 如果 MReg_Free 为 false，Scoreboard 状态应该不是 Idle
        assert(
            !(!AB_MReg_Free(i) && (scoreboard.io.debug.ab_reg_states(i) === RegState.Idle)),
            cf"[Scoreboard Check] AB_Reg[$i] is allocated (Free=false) but Scoreboard shows Idle!"
        )
        
        // Assertion 2: 如果 MReg_Free 为 true，Scoreboard 状态应该是 Idle
        assert(
            !(AB_MReg_Free(i) && (scoreboard.io.debug.ab_reg_states(i) =/= RegState.Idle)),
            cf"[Scoreboard Check] AB_Reg[$i] is free (Free=true) but Scoreboard shows busy (state=${scoreboard.io.debug.ab_reg_states(i)})!"
        )
    }
    
    // 验证 C 寄存器堆状态的一致性
    for (i <- 0 until 2) {
        // Assertion 1: 如果 MReg_Free 为 false，Scoreboard 状态应该不是 Idle
        assert(
            !(!C_MReg_Free(i) && (scoreboard.io.debug.c_reg_states(i) === RegState.Idle)),
            cf"[Scoreboard Check] C_Reg[$i] is allocated (Free=false) but Scoreboard shows Idle!"
        )
        
        // Assertion 2: 如果 MReg_Free 为 true，Scoreboard 状态应该是 Idle
        assert(
            !(C_MReg_Free(i) && (scoreboard.io.debug.c_reg_states(i) =/= RegState.Idle)),
            cf"[Scoreboard Check] C_Reg[$i] is free (Free=true) but Scoreboard shows busy (state=${scoreboard.io.debug.c_reg_states(i)})!"
        )
    }
    
    // 调试输出（可选）：定期打印 Scoreboard 状态
    if (YJPDebugEnable) {
        when(io.DebugTimeStampe % 100.U === 0.U && !MacroInst_FIFO_Empty) {
            printf("[Scoreboard Shadow @%d] AB_Free: [%d,%d,%d,%d], AB_States: [%d,%d,%d,%d]\n",
                io.DebugTimeStampe,
                AB_MReg_Free(0), AB_MReg_Free(1), AB_MReg_Free(2), AB_MReg_Free(3),
                scoreboard.io.debug.ab_reg_states(0), scoreboard.io.debug.ab_reg_states(1),
                scoreboard.io.debug.ab_reg_states(2), scoreboard.io.debug.ab_reg_states(3)
            )
            printf("[Scoreboard Shadow @%d] C_Free: [%d,%d], C_States: [%d,%d]\n",
                io.DebugTimeStampe,
                C_MReg_Free(0), C_MReg_Free(1),
                scoreboard.io.debug.c_reg_states(0), scoreboard.io.debug.c_reg_states(1)
            )
            printf("[Scoreboard Shadow @%d] AB_Readers: [%d,%d,%d,%d], C_Readers: [%d,%d]\n",
                io.DebugTimeStampe,
                scoreboard.io.debug.ab_reg_reader_counts(0), scoreboard.io.debug.ab_reg_reader_counts(1),
                scoreboard.io.debug.ab_reg_reader_counts(2), scoreboard.io.debug.ab_reg_reader_counts(3),
                scoreboard.io.debug.c_reg_reader_counts(0), scoreboard.io.debug.c_reg_reader_counts(1)
            )
        }
        
        // 状态变化时立即打印
        when(scoreboard.io.update.load_allocate) {
            printf("[Scoreboard Update @%d] Load Allocate: A%d=%d, B%d=%d, C%d=%d (FIFO_idx=%d)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.load_alloc_a_reg, scoreboard.io.update.load_alloc_has_a,
                scoreboard.io.update.load_alloc_b_reg, scoreboard.io.update.load_alloc_has_b,
                scoreboard.io.update.load_alloc_c_reg, scoreboard.io.update.load_alloc_has_c,
                scoreboard.io.update.load_alloc_fifo_idx
            )
        }
        
        // 【步骤1】Load查询日志
        when(scoreboard.io.query.load.valid) {
            printf("[Scoreboard Query @%d] 【步骤1】Load Query: A%d, B%d, C%d (need: A=%d,B=%d,C=%d) -> ready=%d\n",
                io.DebugTimeStampe,
                scoreboard.io.query.load.bits.a_reg,
                scoreboard.io.query.load.bits.b_reg,
                scoreboard.io.query.load.bits.c_reg,
                scoreboard.io.query.load.bits.has_a,
                scoreboard.io.query.load.bits.has_b,
                scoreboard.io.query.load.bits.has_c,
                scoreboard.io.query.load.ready
            )
        }
        
        // 【步骤2】Compute查询日志
        when(scoreboard.io.query.compute.valid) {
            printf("[Scoreboard Query @%d] 【步骤2】Compute Query: A%d, B%d, C%d -> ready=%d\n",
                io.DebugTimeStampe,
                scoreboard.io.query.compute.bits.a_reg,
                scoreboard.io.query.compute.bits.b_reg,
                scoreboard.io.query.compute.bits.c_reg,
                scoreboard.io.query.compute.ready
            )
        }
        
        when(scoreboard.io.update.compute_issue) {
            printf("[Scoreboard Update @%d] 【步骤2】Compute Issue: A%d, B%d, C%d (FIFO_idx=%d)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.compute_issue_a_reg,
                scoreboard.io.update.compute_issue_b_reg,
                scoreboard.io.update.compute_issue_c_reg,
                scoreboard.io.update.compute_issue_fifo_idx
            )
        }
        
        when(scoreboard.io.update.compute_read_finish_a) {
            printf("[Scoreboard Update @%d] 【步骤2】Compute Read A Finish: A%d (释放供后续Load使用)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.compute_read_finish_a_reg
            )
        }
        
        when(scoreboard.io.update.compute_read_finish_b) {
            printf("[Scoreboard Update @%d] 【步骤2】Compute Read B Finish: B%d (释放供后续Load使用)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.compute_read_finish_b_reg
            )
        }
        
        when(scoreboard.io.update.compute_write_finish_c) {
            printf("[Scoreboard Update @%d] 【步骤2】Compute Write C Finish: C%d (可供Store读取)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.compute_write_finish_c_reg
            )
        }
        
        when(scoreboard.io.update.store_issue) {
            printf("[Scoreboard Update @%d] Store Issue: C%d (FIFO_idx=%d)\n",
                io.DebugTimeStampe,
                scoreboard.io.update.store_issue_c_reg,
                scoreboard.io.update.store_issue_fifo_idx
            )
        }
    }

    //微指令队列
    val Load_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new LoadMicroInst()))))
    val Load_MicroInst_FINISH_Ready_GO = RegInit(VecInit(Seq.fill(4)(false.B)))//Load微指令是否完成
    val Load_MicroInst_FINISH_Ready_Commit = RegInit(VecInit(Seq.fill(4)(false.B)))//Load微指令是否可以提交
    val Compute_MicroInst_FINISH_Ready_GO = RegInit(VecInit(Seq.fill(4)(false.B)))//Compute微指令是否完成
    val Compute_MicroInst_FINISH_Ready_Commit = RegInit(VecInit(Seq.fill(4)(false.B)))//Compute微指令是否可以提交
    val Compute_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new ComputeMicroInst()))))
    val Store_MicroInst_FIFO = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new StoreMicroInst()))))

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
    
    val Decode_A_MReg_ID = RegInit(0.U(2.W))
    val Decode_B_MReg_ID = RegInit(2.U(2.W))
    val Decode_C_MReg_ID = RegInit(0.U(2.W))

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
            Decode_Tensor_K_iter_Add := (Tensor_K.U / Decoding_MacroInst.element_type)
            Current_Tile_Tensor_K_Iter := 0.U
            Decoding_MarcoInst_Going := true.B

            // ChiselDB
            macro_decodestart.entry.Macro_Inst := Decoding_MacroInst
            macro_decodestart.entry.FIFO_Index := MarcoInst_FIFO_Decode_Head
            macro_decodestart.en := true.B
        }.otherwise
        {
            val Have_Load_Micro_Inst    = WireInit(true.B)//由宏指令拆解出的微指令，每次拆解都有一个Load指令
            val Have_Compute_Micro_Inst = WireInit(true.B)//由宏指令拆解出来的微指令，每次拆解都有一个Compute指令
            val Have_Store_Micro_Inst   = WireInit(false.B)//由宏指令拆解出来的微指令，只在暂存器切换时才发射Store指令
            val Current_MatrixRegTensor_M = WireInit(Tensor_MN.U)
            val Current_MatrixRegTensor_N = WireInit(Tensor_MN.U)
            val Current_MatrixRegTensor_K = WireInit(ReduceGroupSize.U)

            val Can_Issue_Load_Micro_Inst = !Load_MicroInst_FIFO_Full
            val Can_Issue_Compute_Micro_Inst = !Compute_MicroInst_FIFO_Full
            val Can_Issue_Store_Micro_Inst = !Store_MicroInst_FIFO_Full
            
            //                                                                                                        VVVVVVV 其实这个给紧了，不过store毕竟很少，所以不会有问题
            val Can_Decode_More_Micro_Inst = Can_Issue_Load_Micro_Inst && Can_Issue_Compute_Micro_Inst && Can_Issue_Store_Micro_Inst

            val LoadMicroInst_Have_A_work = WireInit(true.B)//由宏指令拆解出的微指令，每个微指令都有AB的Load任务
            val LoadMicroInst_Have_B_work = WireInit(true.B)//由宏指令拆解出的微指令，每个微指令都有AB的Load任务
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
                //输出宏指令当前的迭代情况
                // TODO: store it with chiselDB
                // printf("[TaskController<%d>]:MacroInst Decode!  Current_Tile_M_Iter = %d, Current_Tile_N_Iter = %d, Current_Tile_K_Iter = %d, Current_Tile_OH_Index = %d, Current_Tile_OW_Index = %d, Current_Tile_KH_Index = %d, Current_Tile_KW_Index = %d\n",io.DebugTimeStampe, Current_Tile_M_Iter, Current_Tile_N_Iter, Current_Tile_K_Iter, Current_Tile_OH_Index, Current_Tile_OW_Index, Current_Tile_KH_Index, Current_Tile_KW_Index)
                // val entry = Wire(new MacroInstEventEntry)
                // entry.eventType := 4.U  // Decode
                // entry.head := MacroInst_FIFO_Head
                // entry.tail := MacroInst_FIFO_Tail
                // entry.Macro_Inst := Decoding_MacroInst
                // entry.FIFO_Index := 0.U
                // macroInstEventTable.log(
                //     data = entry,
                //     en = true.B,
                //     site = "MacroInstDecode",
                //     clock = clock,
                //     reset = reset
                // )
                //Current_MatrixRegTensor_M必须4的倍数
                Current_MatrixRegTensor_M := Mux(Current_Tile_M_Iter + Tensor_MN.U >= Decoding_MacroInst.Application_M, (Decoding_MacroInst.Application_M - Current_Tile_M_Iter), Tensor_MN.U)
                Current_MatrixRegTensor_N := Mux(Current_Tile_N_Iter + Tensor_MN.U >= Decoding_MacroInst.Application_N, Decoding_MacroInst.Application_N - Current_Tile_N_Iter, Tensor_MN.U)
                Current_MatrixRegTensor_K := ReduceGroupSize.U
                assert(Current_MatrixRegTensor_N === Tensor_MN.U, "Current_MatrixRegTensor_N is not equal to Tensor_MN")
                assert(Decoding_MacroInst.Application_K % 64.U === 0.U, "Decoding_MacroInst.Application_K is not 64 align")

                LoadMicroInst_Have_C_work := Current_Tile_K_Iter === 0.U && Current_Tile_KW_Index === 0.U && Current_Tile_KH_Index === 0.U

                Decode_A_MReg_ID := Mux(Decode_A_MReg_ID === 0.U, 1.U, 0.U)
                Decode_B_MReg_ID := Mux(Decode_B_MReg_ID === 2.U, 3.U, 2.U)
                //迭代生成微指令
                Current_Tile_K_Iter := Current_Tile_K_Iter + Decode_Tensor_K_iter_Add
                Current_Tile_Tensor_K_Iter := Current_Tile_Tensor_K_Iter + 1.U
                when(Current_Tile_K_Iter + Decode_Tensor_K_iter_Add >= Decoding_MacroInst.Application_K)
                {
                    //                                                      vv
                    //如果最后一次K的迭代，不是满的K，那么就要调整K的大小[ohow][oc][ic]
                    //                                            [ M ] [N] [K]
                    //                                                      ^^
                    
                    // Current_MatrixRegTensor_K := Mux(Current_Tile_K_Iter + Tensor_K.U>= Decoding_MacroInst.Application_K, Decoding_MacroInst.Application_K - Current_Tile_K_Iter, Tensor_K.U)
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
                            Decode_C_MReg_ID := Mux(Decode_C_MReg_ID === 0.U, 1.U, 0.U)
                            Current_Tile_N_Iter := Current_Tile_N_Iter + Tensor_MN.U
                            when(Current_Tile_N_Iter + Tensor_MN.U >= Decoding_MacroInst.Application_N)
                            {
                                //                                                                vv
                                //如果最后一次N的迭代，不是满的N，那么就要调整N的大小，大概率不会发生,[ohow][oc][ic]
                                //                                                          [ M ] [N] [K]
                                //                                                                ^^

                                //output-stationary，此时需要发射Store的微指令
                                Current_Tile_N_Iter := 0.U
                                Current_Tile_M_Iter := Current_Tile_M_Iter + Tensor_MN.U
                                Current_Tile_OW_Index := Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add
                                Current_Tile_OH_Index := Current_Tile_OH_Index + Decoding_MacroInst.conv_oh_per_add
                                when(Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add >= Decoding_MacroInst.conv_ow_max)
                                {
                                    Current_Tile_OW_Index := Current_Tile_OW_Index + Decoding_MacroInst.conv_ow_per_add - Decoding_MacroInst.conv_ow_max
                                    Current_Tile_OH_Index := Current_Tile_OH_Index + Decoding_MacroInst.conv_oh_per_add + 1.U
                                }
                                when(Current_Tile_M_Iter + Tensor_MN.U >= Decoding_MacroInst.Application_M)
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
                                    
                                    // ChiselDB
                                    macro_decodeend.entry.Macro_Inst := Decoding_MacroInst
                                    macro_decodeend.entry.FIFO_Index := MarcoInst_FIFO_Decode_Head
                                    macro_decodeend.en := true.B
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
            Load_MicroInst.Is_C_Work := LoadMicroInst_Have_C_work

            Load_MicroInst.A_MRegID := Decode_A_MReg_ID
            Load_MicroInst.B_MRegID := Decode_B_MReg_ID
            Load_MicroInst.C_MRegID := Decode_C_MReg_ID

            Load_MicroInst.MatrixRegTensor_K := Current_MatrixRegTensor_K
            Load_MicroInst.MatrixRegTensor_N := Current_MatrixRegTensor_N
            Load_MicroInst.MatrixRegTensor_M := Current_MatrixRegTensor_M

            Load_MicroInst.IsTranspose := Decoding_MacroInst.transpose_result

            Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := Decoding_MacroInst.ApplicationTensor_A_BaseVaddr + Current_Tile_Tensor_K_Iter*(ReduceWidthByte*ReduceGroupSize).U  //TODO:初始地址需要改！因为当前的K移动了！
            Load_MicroInst.ApplicationTensor_A.ApplicationTensor_A_Stride_M := Decoding_MacroInst.ApplicationTensor_A_Stride
            Load_MicroInst.ApplicationTensor_A.Convolution_OH_DIM_Length := Decoding_MacroInst.conv_oh_max
            Load_MicroInst.ApplicationTensor_A.Convolution_OW_DIM_Length := Decoding_MacroInst.conv_ow_max
            Load_MicroInst.ApplicationTensor_A.Convolution_KH_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_A.Convolution_KW_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_A.Convolution_Stride_H := Decoding_MacroInst.conv_stride
            Load_MicroInst.ApplicationTensor_A.Convolution_Stride_W := Decoding_MacroInst.conv_stride
            Load_MicroInst.ApplicationTensor_A.dataType := Decoding_MacroInst.element_type
            
            Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := Decoding_MacroInst.ApplicationTensor_B_BaseVaddr
            Load_MicroInst.ApplicationTensor_B.ApplicationTensor_B_Stride_N := Decoding_MacroInst.ApplicationTensor_B_Stride
            Load_MicroInst.ApplicationTensor_B.BlockTensor_B_BaseVaddr := Decoding_MacroInst.ApplicationTensor_B_BaseVaddr + Current_Tile_K_Iter * Decoding_MacroInst.element_type + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_B_Stride + (Current_Tile_KH_Index * Decoding_MacroInst.kernel_size + Current_Tile_KW_Index)*Decoding_MacroInst.kernel_stride //TODO:kernel_conv_stride!!!
            Load_MicroInst.ApplicationTensor_B.Convolution_KH_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_B.Convolution_KW_DIM_Length := Decoding_MacroInst.kernel_size
            Load_MicroInst.ApplicationTensor_B.dataType := Decoding_MacroInst.element_type

            Load_MicroInst.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32
            Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr
            Load_MicroInst.ApplicationTensor_C.ApplicationTensor_C_Stride_M := Decoding_MacroInst.ApplicationTensor_C_Stride
            // Load_MicroInst.ApplicationTensor_C.BlockTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr + Current_Tile_M_Iter * Decoding_MacroInst.element_type + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_C_Stride//转置？
            Load_MicroInst.ApplicationTensor_C.BlockTensor_C_BaseVaddr := Decoding_MacroInst.ApplicationTensor_C_BaseVaddr + Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorLoad,Current_Tile_M_Iter * Decoding_MacroInst.ApplicationTensor_C_Stride,0.U) + Current_Tile_N_Iter * Decoding_MacroInst.bias_data_type
            Load_MicroInst.CLoadTaskInfo.Is_FullLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorLoad, true.B, false.B)
            Load_MicroInst.CLoadTaskInfo.Is_ZeroLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorZeroLoad, true.B, false.B)
            Load_MicroInst.CLoadTaskInfo.Is_RepeatRowLoad := Mux(Decoding_MacroInst.bias_type === CMemoryLoaderTaskType.TaskTypeTensorRepeatRowLoad, true.B, false.B)

            when(Have_Load_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Load_MicroInst_FIFO(Load_MicroInst_FIFO_Head) := Load_MicroInst
                Load_MicroInst_FIFO_Head := WrapInc(Load_MicroInst_FIFO_Head, 4)
                Load_MicroInst_FINISH_Ready_GO(Load_MicroInst_FIFO_Head) := false.B
                Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Head) := false.B
                
                load_insert.entry.Load_MicroInst := Load_MicroInst
                load_insert.entry.FIFO_Index := Load_MicroInst_FIFO_Head
                load_insert.en := true.B
            }

            //生成Compute指令
            val Compute_MicroInst = Wire(new ComputeMicroInst)
            Compute_MicroInst.MatrixRegTensor_M := Current_MatrixRegTensor_M
            Compute_MicroInst.MatrixRegTensor_N := Current_MatrixRegTensor_N
            Compute_MicroInst.MatrixRegTensor_K := Current_MatrixRegTensor_K
            Compute_MicroInst.Have_Store_Micro_Inst := Have_Store_Micro_Inst
            Compute_MicroInst.DataType_A := ElementDataType.DataTypeWidth8 //8bit
            Compute_MicroInst.DataType_B := ElementDataType.DataTypeWidth8 //8bit
            Compute_MicroInst.DataType_C := ElementDataType.DataTypeWidth32 //32bit
            Compute_MicroInst.DataType_D := ElementDataType.DataTypeWidth32 //32bit

            val Have_After_Ops = WireInit(false.B)
            Have_After_Ops := Have_Store_Micro_Inst
            Compute_MicroInst.Is_AfterOps_Tile := Have_After_Ops
            Compute_MicroInst.Have_Aops := Have_After_Ops
            Compute_MicroInst.Is_Transpose := Decoding_MacroInst.transpose_result
            Compute_MicroInst.Is_Reorder_Only_Ops := true.B
            Compute_MicroInst.Is_EasyScale_Only_Ops := false.B
            Compute_MicroInst.Is_VecFIFO_Ops := false.B
            Compute_MicroInst.Is_Fp := Decoding_MacroInst.is_fp

            val Compute_Resource_Info = Wire(new ComputeMicroInst_Resource_Info)
            Compute_Resource_Info.A_MRegID := Decode_A_MReg_ID
            Compute_Resource_Info.B_MRegID := Decode_B_MReg_ID
            Compute_Resource_Info.C_MRegID := Decode_C_MReg_ID
            Compute_Resource_Info.Load_Micro_Inst_FIFO_Index := Load_MicroInst_FIFO_Head //由于指令一定同时被入队，所以这里不会有问题
            
            when(Have_Compute_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Compute_MicroInst_FIFO(Compute_MicroInst_FIFO_Head) := Compute_MicroInst
                Compute_MicroInst_Resource_Info_FIFO(Compute_MicroInst_FIFO_Head) := Compute_Resource_Info.asUInt
                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FIFO_Head) := false.B
                Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Head) := false.B
                Compute_MicroInst_FIFO_Head := WrapInc(Compute_MicroInst_FIFO_Head, 4)

                comp_insert.entry.Compute_MicroInst := Compute_MicroInst
                comp_insert.entry.FIFO_Index := Compute_MicroInst_FIFO_Head
                comp_insert.en := true.B
            }

            //生成Store指令
            val Store_MicroInst = Wire(new StoreMicroInst)
            Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := Decoding_MacroInst.ApplicationTensor_D_BaseVaddr
            Store_MicroInst.ApplicationTensor_D.ApplicationTensor_D_Stride_M := Decoding_MacroInst.ApplicationTensor_D_Stride
            Store_MicroInst.ApplicationTensor_D.BlockTensor_D_BaseVaddr := Mux(Decoding_MacroInst.transpose_result,
                        Decoding_MacroInst.ApplicationTensor_D_BaseVaddr + Current_Tile_N_Iter * Decoding_MacroInst.ApplicationTensor_D_Stride + Current_Tile_M_Iter * ResultWidthByte.U,
                        Decoding_MacroInst.ApplicationTensor_D_BaseVaddr + Current_Tile_M_Iter * Decoding_MacroInst.ApplicationTensor_D_Stride + Current_Tile_N_Iter * ResultWidthByte.U)//TODO:转置？result_data_width?
            Store_MicroInst.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32
            Store_MicroInst.Conherent := true.B
            Store_MicroInst.Is_Transpose := Decoding_MacroInst.transpose_result
            Store_MicroInst.MatrixRegTensor_M := Current_MatrixRegTensor_M
            Store_MicroInst.MatrixRegTensor_N := Current_MatrixRegTensor_N
            Store_MicroInst.Is_Last_Store := StoreMicroInst_Is_Last_Store

            val Store_Resource_Info = Wire(new StoreMicroInst_Resource_Info)
            Store_Resource_Info.C_MRegID := Decode_C_MReg_ID
            Store_Resource_Info.Compute_Micro_Inst_FIFO_Index := Compute_MicroInst_FIFO_Head
            Store_Resource_Info.Marco_Inst_FIFO_Index := MarcoInst_FIFO_Decode_Head
            when(Have_Store_Micro_Inst && Can_Decode_More_Micro_Inst)
            {
                Store_MicroInst_FIFO(Store_MicroInst_FIFO_Head) := Store_MicroInst
                Store_MicroInst_Resource_Info_FIFO(Store_MicroInst_FIFO_Head) := Store_Resource_Info.asUInt
                Store_MicroInst_FIFO_Head := WrapInc(Store_MicroInst_FIFO_Head, 4)

                store_insert.entry.Store_MicroInst := Store_MicroInst
                store_insert.entry.FIFO_Index := Store_MicroInst_FIFO_Head
                store_insert.en := true.B
            }
        }
    }

    val Current_ADC_MReg_ID = RegInit(0.U(2.W))
    val Current_BDC_MReg_ID = RegInit(0.U(2.W))
    val Current_CDC_MReg_ID = RegInit(0.U(2.W))
    val Current_AML_MReg_ID = RegInit(0.U(2.W))
    val Current_BML_MReg_ID = RegInit(0.U(2.W))
    val Current_CML_MReg_ID = RegInit(0.U(2.W))

    io.MReg_CtrlInfo.ADC_MReg_ID := Current_ADC_MReg_ID
    io.MReg_CtrlInfo.BDC_MReg_ID := Current_BDC_MReg_ID
    io.MReg_CtrlInfo.CDC_MReg_ID := Current_CDC_MReg_ID
    io.MReg_CtrlInfo.AML_MReg_ID := Current_AML_MReg_ID
    io.MReg_CtrlInfo.BML_MReg_ID := Current_BML_MReg_ID
    io.MReg_CtrlInfo.CML_MReg_ID := Current_CML_MReg_ID

    //看每个队列里面的微指令，如果有可以发射的微指令，就发射
    val Will_Issuse_CML_Load = WireInit(false.B)

    //只要有可以发射的Load微指令
    val issue_state_idle :: issue_state_issue :: Nil = Enum(2)
    val Load_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Load_Micro_Inst_Wait_A_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_B_Finish = RegInit(false.B)
    val Load_Micro_Inst_Wait_C_Finish = RegInit(false.B)

    io.ctrlCounter.ALoad := Load_Micro_Inst_Wait_A_Finish
    io.ctrlCounter.BLoad := Load_Micro_Inst_Wait_B_Finish
    io.ctrlCounter.CLoad := Load_Micro_Inst_Wait_C_Finish
    when(!Load_MicroInst_FINISH_All)
    {
        val Load_MicroInst = Load_MicroInst_FIFO(Load_MicroInst_FINISH_Head) //取出的Load指令

        val Need_Issue_AML_Micro_Inst = Load_MicroInst.Is_A_Work
        val Need_Issue_BML_Micro_Inst = Load_MicroInst.Is_B_Work
        val Need_Issue_CML_Micro_Inst = Load_MicroInst.Is_C_Work

        // ====================================================================
        // 【步骤1：Load切换到Scoreboard】查询Scoreboard获取寄存器可用性
        // ====================================================================
        scoreboard.io.query.load.valid := true.B
        scoreboard.io.query.load.bits.a_reg := Load_MicroInst.A_MRegID
        scoreboard.io.query.load.bits.b_reg := Load_MicroInst.B_MRegID
        scoreboard.io.query.load.bits.c_reg := Load_MicroInst.C_MRegID
        scoreboard.io.query.load.bits.has_a := Need_Issue_AML_Micro_Inst
        scoreboard.io.query.load.bits.has_b := Need_Issue_BML_Micro_Inst
        scoreboard.io.query.load.bits.has_c := Need_Issue_CML_Micro_Inst
        
        // Scoreboard检查：需要的寄存器是否都空闲
        val Scoreboard_Can_Issue_Load = scoreboard.io.query.load.ready
        
        // 硬件资源检查：AML/BML/CML是否ready
        val Hardware_Ready = io.AML_MicroTask_Config.MicroTaskReady && 
                             io.BML_MicroTask_Config.MicroTaskReady && 
                             io.CML_MicroTask_Config.MicroTaskReady
        
        // 综合判断：Scoreboard允许 && 硬件资源ready
        val Can_Issue_Load_Micro_Inst = Scoreboard_Can_Issue_Load && Hardware_Ready
        //即need又can，才能发射这条Load微指令
        // if (YJPDebugEnable)
        //     {
        //         printf("[TaskController<%d>]:Can_Issue_AML_Micro_Inst = %d, Can_Issue_BML_Micro_Inst = %d, Can_Issue_CML_Micro_Inst = %d, Need_Issue_AML_Micro_Inst = %d, Need_Issue_BML_Micro_Inst = %d, Need_Issue_CML_Micro_Inst = %d\n", io.DebugTimeStampe, Can_Issue_AML_Micro_Inst, Can_Issue_BML_Micro_Inst, Can_Issue_CML_Micro_Inst, Need_Issue_AML_Micro_Inst, Need_Issue_BML_Micro_Inst, Need_Issue_CML_Micro_Inst)
        //     }
        when(Can_Issue_Load_Micro_Inst && Load_Micro_Inst_Issue_State_Reg === issue_state_idle)
        {
            //发射这条指令

            io.AML_MicroTask_Config.ApplicationTensor_A := Load_MicroInst.ApplicationTensor_A
            io.AML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentA
            io.AML_MicroTask_Config.Convolution_Current_KH_Index := Load_MicroInst.Convolution_Current_KH_Index
            io.AML_MicroTask_Config.Convolution_Current_KW_Index := Load_MicroInst.Convolution_Current_KW_Index
            io.AML_MicroTask_Config.Convolution_Current_OH_Index := Load_MicroInst.Convolution_Current_OH_Index
            io.AML_MicroTask_Config.Convolution_Current_OW_Index := Load_MicroInst.Convolution_Current_OW_Index
            io.AML_MicroTask_Config.MatrixRegTensor_M := Load_MicroInst.MatrixRegTensor_M
            io.AML_MicroTask_Config.MatrixRegTensor_K := Load_MicroInst.MatrixRegTensor_K
            
            io.BML_MicroTask_Config.ApplicationTensor_B := Load_MicroInst.ApplicationTensor_B
            io.BML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentB
            io.BML_MicroTask_Config.MatrixRegTensor_K := Load_MicroInst.MatrixRegTensor_K
            io.BML_MicroTask_Config.MatrixRegTensor_N := Load_MicroInst.MatrixRegTensor_N
            
            io.CML_MicroTask_Config.ApplicationTensor_C := Load_MicroInst.ApplicationTensor_C
            io.CML_MicroTask_Config.Conherent        := Load_MicroInst.ConherentC
            io.CML_MicroTask_Config.MatrixRegTensor_M := Load_MicroInst.MatrixRegTensor_M
            io.CML_MicroTask_Config.MatrixRegTensor_N := Load_MicroInst.MatrixRegTensor_N
            io.CML_MicroTask_Config.IsLoadMicroTask := true.B
            io.CML_MicroTask_Config.LoadTaskInfo := Load_MicroInst.CLoadTaskInfo
            io.CML_MicroTask_Config.Is_Transpose := Load_MicroInst.IsTranspose

            // ====================================================================
            // 【步骤1过渡期】继续更新MReg_Free（因为Compute/Store还在用旧机制）
            // ====================================================================
            AB_MReg_Free(Load_MicroInst.A_MRegID)  := false.B  // A矩阵占用AB[0-1]
            Current_AML_MReg_ID := Load_MicroInst.A_MRegID
            AB_MReg_Free(Load_MicroInst.B_MRegID)  := false.B  // B矩阵占用AB[2-3]
            Current_BML_MReg_ID := Load_MicroInst.B_MRegID
            when(Will_Issuse_CML_Load)
            {
                C_MReg_Free(Load_MicroInst.C_MRegID) := false.B
                Current_CML_MReg_ID := Load_MicroInst.C_MRegID
            }
            
            // Scoreboard Update: Load 分配
            scoreboard.io.update.load_allocate := true.B
            scoreboard.io.update.load_alloc_a_reg := Load_MicroInst.A_MRegID
            scoreboard.io.update.load_alloc_b_reg := Load_MicroInst.B_MRegID
            scoreboard.io.update.load_alloc_c_reg := Load_MicroInst.C_MRegID
            scoreboard.io.update.load_alloc_has_a := Need_Issue_AML_Micro_Inst
            scoreboard.io.update.load_alloc_has_b := Need_Issue_BML_Micro_Inst
            scoreboard.io.update.load_alloc_has_c := Need_Issue_CML_Micro_Inst
            scoreboard.io.update.load_alloc_fifo_idx := Load_MicroInst_FINISH_Head

            io.AML_MicroTask_Config.MicroTaskValid := Need_Issue_AML_Micro_Inst
            io.BML_MicroTask_Config.MicroTaskValid := Need_Issue_BML_Micro_Inst
            io.CML_MicroTask_Config.MicroTaskValid := Need_Issue_CML_Micro_Inst

            Load_Micro_Inst_Issue_State_Reg := issue_state_issue
            Load_Micro_Inst_Wait_A_Finish := Need_Issue_AML_Micro_Inst
            Load_Micro_Inst_Wait_B_Finish := Need_Issue_BML_Micro_Inst
            Load_Micro_Inst_Wait_C_Finish := Need_Issue_CML_Micro_Inst

            Will_Issuse_CML_Load := Need_Issue_CML_Micro_Inst

            load_issue.entry.Load_MicroInst := Load_MicroInst
            load_issue.entry.FIFO_Index := Load_MicroInst_FINISH_Head
            load_issue.en := true.B
        }.elsewhen(Load_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            //等待这条指令完成
            io.AML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_A_Finish
            io.BML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_B_Finish
            io.CML_MicroTask_Config.MicroTaskEndReady := Load_Micro_Inst_Wait_C_Finish
            when(Load_Micro_Inst_Wait_A_Finish && io.AML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_A_Finish := false.B

                load_afinish.entry.Load_MicroInst := Load_MicroInst
                load_afinish.entry.FIFO_Index := Load_MicroInst_FINISH_Head
                load_afinish.en := true.B
                
                // Scoreboard Update: Load A 完成
                scoreboard.io.update.load_finish_a := true.B
                scoreboard.io.update.load_finish_a_reg := Load_MicroInst.A_MRegID
            }
            when(Load_Micro_Inst_Wait_B_Finish && io.BML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_B_Finish := false.B

                load_bfinish.entry.Load_MicroInst := Load_MicroInst
                load_bfinish.entry.FIFO_Index := Load_MicroInst_FINISH_Head
                load_bfinish.en := true.B
                
                // Scoreboard Update: Load B 完成
                scoreboard.io.update.load_finish_b := true.B
                scoreboard.io.update.load_finish_b_reg := Load_MicroInst.B_MRegID
            }
            when(Load_Micro_Inst_Wait_C_Finish && io.CML_MicroTask_Config.MicroTaskEndValid)
            {
                Load_Micro_Inst_Wait_C_Finish := false.B
                
                load_cfinish.entry.Load_MicroInst := Load_MicroInst
                load_cfinish.entry.FIFO_Index := Load_MicroInst_FINISH_Head
                load_cfinish.en := true.B
                
                // Scoreboard Update: Load C 完成
                scoreboard.io.update.load_finish_c := true.B
                scoreboard.io.update.load_finish_c_reg := Load_MicroInst.C_MRegID
            }
            when(!Load_Micro_Inst_Wait_A_Finish && !Load_Micro_Inst_Wait_B_Finish && !Load_Micro_Inst_Wait_C_Finish)
            {
                Load_MicroInst_FINISH_Head := WrapInc(Load_MicroInst_FINISH_Head, 4)
                Load_MicroInst_FINISH_Ready_GO(Load_MicroInst_FINISH_Head) := true.B
                Load_Micro_Inst_Issue_State_Reg := issue_state_idle

                load_finish.entry.Load_MicroInst := Load_MicroInst
                load_finish.entry.FIFO_Index := Load_MicroInst_FINISH_Head
                load_finish.en := true.B
            }
        }
        
    }

    //提交Load微指令
    when(Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Tail) === true.B)
    {
        Load_MicroInst_FIFO_Tail := WrapInc(Load_MicroInst_FIFO_Tail, 4)
        Load_MicroInst_FINISH_Ready_Commit(Load_MicroInst_FIFO_Tail) := false.B
        
        load_commit.entry.Load_MicroInst := Load_MicroInst_FIFO(Load_MicroInst_FIFO_Tail)
        load_commit.entry.FIFO_Index := Load_MicroInst_FIFO_Tail
        load_commit.en := true.B
    }

    // val issue_state_idle :: issue_state_issue :: Nil = Enum(2)
    val Compute_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Compute_Micro_Inst_Wait_A_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_B_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_C_Finish = RegInit(false.B)
    val Compute_Micro_Inst_Wait_Aop_Finish = RegInit(false.B)

    io.ctrlCounter.AOPBusy := Compute_Micro_Inst_Wait_Aop_Finish
    io.ctrlCounter.computeInstQueueEmpty := Compute_MicroInst_FINISH_All 
    io.ctrlCounter.computeInstCanIssue := false.B

    when(!Compute_MicroInst_FINISH_All)
    {
        val Compute_MicroInst = Compute_MicroInst_FIFO(Compute_MicroInst_FINISH_HEAD) //取出的Compute指令
        val Compute_MicroInst_Resource_Info = Compute_MicroInst_Resource_Info_FIFO(Compute_MicroInst_FINISH_HEAD).asTypeOf(new ComputeMicroInst_Resource_Info)

        // ====================================================================
        // 步骤 3.4: 使用 Scoreboard 进行 Compute 依赖检查
        // - Scoreboard 检查 A/B/C 寄存器是否 Ready（替代 Dependent_Load_Finish_Ready_Go）
        // - Scoreboard 的 checkComputeDependency 已包含 Load 完成的检查
        // ====================================================================
        
        // Scoreboard 查询：检查 A/B/C 寄存器依赖
        scoreboard.io.query.compute.valid := true.B
        scoreboard.io.query.compute.bits.a_reg := Compute_MicroInst_Resource_Info.A_MRegID
        scoreboard.io.query.compute.bits.b_reg := Compute_MicroInst_Resource_Info.B_MRegID
        scoreboard.io.query.compute.bits.c_reg := Compute_MicroInst_Resource_Info.C_MRegID
        
        // Scoreboard 返回的 ready 信号表示依赖检查通过
        // 包含了：A/B/C 都是 Ready 状态（Load 已完成），且 C 没有其他写者
        val Scoreboard_Dependency_Ready = scoreboard.io.query.compute.ready
        
        // 保留旧的 Dependent_Load_Finish_Ready_Go 用于过渡期验证（可在后续移除）
        val Dependent_Load_Finish_Ready_Go = Load_MicroInst_FINISH_Ready_GO(Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index)

        val Can_Issue_ADC_Micro_Inst = io.ADC_MicroTask_Config.MicroTaskReady //缺少AMReg的空闲状态,保证同时任务被发射
        val Can_Issue_BDC_Micro_Inst = io.BDC_MicroTask_Config.MicroTaskReady //缺少BMReg的空闲状态,保证同时任务被发射
        val Can_Issue_CDC_Micro_Inst = io.CDC_MicroTask_Config.MicroTaskReady //缺少CMReg的空闲状态,保证同时任务被发射
        val Can_Issue_AOP_Micro_Inst = true.B

        // 使用 Scoreboard 依赖检查替代 Dependent_Load_Finish_Ready_Go
        val Can_Issue_Compute_Micro_Inst = Can_Issue_ADC_Micro_Inst && Can_Issue_BDC_Micro_Inst && Can_Issue_CDC_Micro_Inst && Scoreboard_Dependency_Ready && Can_Issue_AOP_Micro_Inst

        io.ctrlCounter.computeInstCanIssue := Can_Issue_Compute_Micro_Inst && Compute_Micro_Inst_Issue_State_Reg === issue_state_idle
        
        // ====================================================================
        // 过渡期验证：确保 Scoreboard 和旧机制的一致性
        // ====================================================================
        // 验证1: Scoreboard_Dependency_Ready 应该包含 Dependent_Load_Finish_Ready_Go 的语义
        // Scoreboard 检查 A/B/C 寄存器 Ready，等价于 Load 已完成
        when(Scoreboard_Dependency_Ready && !Dependent_Load_Finish_Ready_Go) {
            printf("[WARNING @%d] Scoreboard allows Compute issue but Dependent_Load not ready! " +
                   "A_MRegID=%d, B_MRegID=%d, C_MRegID=%d, Load_FIFO_idx=%d\n",
                io.DebugTimeStampe,
                Compute_MicroInst_Resource_Info.A_MRegID,
                Compute_MicroInst_Resource_Info.B_MRegID,
                Compute_MicroInst_Resource_Info.C_MRegID,
                Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index
            )
        }
        
        when(!Scoreboard_Dependency_Ready && Dependent_Load_Finish_Ready_Go) {
            printf("[INFO @%d] Scoreboard blocks Compute issue but old mechanism allows. " +
                   "This is expected if there's a WAW hazard on C. " +
                   "A_MRegID=%d, B_MRegID=%d, C_MRegID=%d\n",
                io.DebugTimeStampe,
                Compute_MicroInst_Resource_Info.A_MRegID,
                Compute_MicroInst_Resource_Info.B_MRegID,
                Compute_MicroInst_Resource_Info.C_MRegID
            )
        }

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
            io.ADC_MicroTask_Config.MatrixRegTensor_M := Compute_MicroInst.MatrixRegTensor_M
            io.ADC_MicroTask_Config.MatrixRegTensor_N := Compute_MicroInst.MatrixRegTensor_N
            io.ADC_MicroTask_Config.MatrixRegTensor_K := Compute_MicroInst.MatrixRegTensor_K
            io.ADC_MicroTask_Config.ApplicationTensor_A.dataType := ElementDataType.DataTypeWidth8 //TODO:需要修改

            io.BDC_MicroTask_Config.MatrixRegTensor_M := Compute_MicroInst.MatrixRegTensor_M
            io.BDC_MicroTask_Config.MatrixRegTensor_N := Compute_MicroInst.MatrixRegTensor_N
            io.BDC_MicroTask_Config.MatrixRegTensor_K := Compute_MicroInst.MatrixRegTensor_K
            io.BDC_MicroTask_Config.ApplicationTensor_B.dataType := ElementDataType.DataTypeWidth8 //TODO:需要修改

            io.CDC_MicroTask_Config.MatrixRegTensor_M := Compute_MicroInst.MatrixRegTensor_M
            io.CDC_MicroTask_Config.MatrixRegTensor_N := Compute_MicroInst.MatrixRegTensor_N
            io.CDC_MicroTask_Config.MatrixRegTensor_K := Compute_MicroInst.MatrixRegTensor_K
            io.CDC_MicroTask_Config.ApplicationTensor_C.dataType := ElementDataType.DataTypeWidth32 //TODO:需要修改
            io.CDC_MicroTask_Config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32 //TODO:需要修改
            io.CDC_MicroTask_Config.Is_AfterOps_Tile := Compute_MicroInst.Is_AfterOps_Tile
            io.CDC_MicroTask_Config.Is_Transpose := Compute_MicroInst.Is_Transpose
            io.CDC_MicroTask_Config.Is_Reorder_Only_Ops := Compute_MicroInst.Is_Reorder_Only_Ops
            io.CDC_MicroTask_Config.Is_EasyScale_Only_Ops := false.B        //TODO:需要修改
            io.CDC_MicroTask_Config.Is_VecFIFO_Ops := false.B               //TODO:需要修改

            // io.AOP_MicroTask_Config.MatrixRegTensor_M := Compute_MicroInst.MatrixRegTensor_M
            // io.AOP_MicroTask_Config.MatrixRegTensor_N := Compute_MicroInst.MatrixRegTensor_N
            // io.AOP_MicroTask_Config.MatrixRegTensor_K := Compute_MicroInst.MatrixRegTensor_K
            // io.AOP_MicroTask_Config.ApplicationTensor_D.dataType := ElementDataType.DataTypeWidth32 //TODO:需要修改
            // io.AOP_MicroTask_Config.Is_EasyScale_Only_Ops := Compute_MicroInst.Is_EasyScale_Only_Ops
            // io.AOP_MicroTask_Config.Is_VecFIFO_Ops := Compute_MicroInst.Is_VecFIFO_Ops
            // io.AOP_MicroTask_Config.Is_Transpose := Compute_MicroInst.Is_Transpose
            // io.AOP_MicroTask_Config.Is_Reorder_Only_Ops := Compute_MicroInst.Is_Reorder_Only_Ops
            // io.AOP_MicroTask_Config.CUTEuop := Compute_MicroInst.CUTEuop

            io.MTE_MicroTask_Config.dataType := ElementDataType.DataTypeI8I8I32

            io.ADC_MicroTask_Config.MicroTaskValid := true.B
            io.BDC_MicroTask_Config.MicroTaskValid := true.B
            io.CDC_MicroTask_Config.MicroTaskValid := true.B
            // io.MTE_MicroTask_Config.valid := true.B
            // io.AOP_MicroTask_Config.MicroTaskValid := Compute_MicroInst.Have_Aops

            Current_ADC_MReg_ID := Compute_MicroInst_Resource_Info.A_MRegID
            Current_BDC_MReg_ID := Compute_MicroInst_Resource_Info.B_MRegID
            Current_CDC_MReg_ID := Compute_MicroInst_Resource_Info.C_MRegID

            Compute_Micro_Inst_Issue_State_Reg := issue_state_issue
            Compute_Micro_Inst_Wait_A_Finish := true.B
            Compute_Micro_Inst_Wait_B_Finish := true.B
            Compute_Micro_Inst_Wait_C_Finish := true.B
            Compute_Micro_Inst_Wait_Aop_Finish := Compute_MicroInst.Have_Aops

            Load_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_Resource_Info.Load_Micro_Inst_FIFO_Index) := true.B//标记这条Load指令已经可以被提交了
            
            // Scoreboard Update: Compute 发射
            scoreboard.io.update.compute_issue := true.B
            scoreboard.io.update.compute_issue_a_reg := Compute_MicroInst_Resource_Info.A_MRegID
            scoreboard.io.update.compute_issue_b_reg := Compute_MicroInst_Resource_Info.B_MRegID
            scoreboard.io.update.compute_issue_c_reg := Compute_MicroInst_Resource_Info.C_MRegID
            scoreboard.io.update.compute_issue_fifo_idx := Compute_MicroInst_FINISH_HEAD
            
            comp_issue.entry.Compute_MicroInst := Compute_MicroInst
            comp_issue.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
            comp_issue.en := true.B
        }.elsewhen(Compute_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            io.ADC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_A_Finish
            io.BDC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_B_Finish
            io.CDC_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_C_Finish
            io.AOP_MicroTask_Config.MicroTaskEndReady := Compute_Micro_Inst_Wait_Aop_Finish
            when(Compute_Micro_Inst_Wait_A_Finish && io.ADC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_A_Finish := false.B

                comp_afinish.entry.Compute_MicroInst := Compute_MicroInst
                comp_afinish.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                comp_afinish.en := true.B
                AB_MReg_Free(Current_ADC_MReg_ID) := true.B  // A矩阵释放AB[0-1]
                
                // Scoreboard Update: Compute 读取 A 完成
                scoreboard.io.update.compute_read_finish_a := true.B
                scoreboard.io.update.compute_read_finish_a_reg := Current_ADC_MReg_ID
            }
            when(Compute_Micro_Inst_Wait_B_Finish && io.BDC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_B_Finish := false.B
                
                comp_bfinish.entry.Compute_MicroInst := Compute_MicroInst
                comp_bfinish.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                comp_bfinish.en := true.B
                AB_MReg_Free(Current_BDC_MReg_ID) := true.B  // B矩阵释放AB[2-3]
                
                // Scoreboard Update: Compute 读取 B 完成
                scoreboard.io.update.compute_read_finish_b := true.B
                scoreboard.io.update.compute_read_finish_b_reg := Current_BDC_MReg_ID
            }
            when(Compute_Micro_Inst_Wait_C_Finish && io.CDC_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_C_Finish := false.B
                
                comp_cfinish.entry.Compute_MicroInst := Compute_MicroInst
                comp_cfinish.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                comp_cfinish.en := true.B
                
                // Scoreboard Update: Compute 写入 C 完成
                scoreboard.io.update.compute_write_finish_c := true.B
                scoreboard.io.update.compute_write_finish_c_reg := Compute_MicroInst_Resource_Info.C_MRegID
            }
            when(Compute_Micro_Inst_Wait_Aop_Finish && io.AOP_MicroTask_Config.MicroTaskEndValid)
            {
                Compute_Micro_Inst_Wait_Aop_Finish := false.B
                
                comp_aopfinish.entry.Compute_MicroInst := Compute_MicroInst
                comp_aopfinish.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                comp_aopfinish.en := true.B
            }
            when(!Compute_Micro_Inst_Wait_A_Finish && !Compute_Micro_Inst_Wait_B_Finish && !Compute_Micro_Inst_Wait_C_Finish)
            {
                Compute_MicroInst_FINISH_HEAD := WrapInc(Compute_MicroInst_FINISH_HEAD, 4)
                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FINISH_HEAD) := true.B
                Compute_Micro_Inst_Issue_State_Reg := issue_state_idle

                Compute_MicroInst_FINISH_Ready_GO(Compute_MicroInst_FINISH_HEAD) := true.B
            
                when(Compute_MicroInst.Have_Store_Micro_Inst === false.B)
                {
                    Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FINISH_HEAD) := true.B
                    // Compute_Micro_Inst_Issue_State_Reg := issue_state_idle

                    // ChiselDB
                    comp_finish_without_store.entry.Compute_MicroInst := Compute_MicroInst
                    comp_finish_without_store.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                    comp_finish_without_store.en := true.B
                }

                comp_finish.entry.Compute_MicroInst := Compute_MicroInst
                comp_finish.entry.FIFO_Index := Compute_MicroInst_FINISH_HEAD
                comp_finish.en := true.B
            }
        }
    }

    //提交Compute微指令
    when(Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Tail) === true.B)
    {
        Compute_MicroInst_FIFO_Tail := WrapInc(Compute_MicroInst_FIFO_Tail, 4)
        Compute_MicroInst_FINISH_Ready_Commit(Compute_MicroInst_FIFO_Tail) := false.B
        
        comp_commit.entry.Compute_MicroInst := Compute_MicroInst_FIFO(Compute_MicroInst_FIFO_Tail)
        comp_commit.entry.FIFO_Index := Compute_MicroInst_FIFO_Tail
        comp_commit.en := true.B
    }

    val Store_Micro_Inst_Issue_State_Reg = RegInit(issue_state_idle)
    val Store_Micro_Inst_Wait_C_Finish = RegInit(false.B)
    val Store_Micro_Inst_Is_Last_Store = RegInit(false.B)
    io.ctrlCounter.DStore := Store_Micro_Inst_Wait_C_Finish
    when(!Store_MicroInst_FIFO_Empty)
    {
        val Store_MicroInst = Store_MicroInst_FIFO(Store_MicroInst_FIFO_Tail)
        val Store_MicroInst_Resource_Info = Store_MicroInst_Resource_Info_FIFO(Store_MicroInst_FIFO_Tail).asTypeOf(new StoreMicroInst_Resource_Info)

        // ====================================================================
        // 步骤 3.5: 使用 Scoreboard 进行 Store 依赖检查（临时方案）
        // - Scoreboard 检查 C 是否 Ready 且没有写者（writer_valid = false）
        // - 用数据依赖（writer_valid）模拟程序顺序依赖
        // 
        // 注意：这是过渡期临时方案，因为当前架构缺少全局指令队列
        //       Compute 和 Store 虽来自同一宏指令，但被拆分到不同 FIFO
        //       Scoreboard 无法感知程序顺序，只能通过 writer_valid 间接保证顺序
        //       步骤4将建立全局指令队列，正确处理程序顺序依赖
        // ====================================================================
        
        // Scoreboard 查询：检查 C 寄存器依赖
        scoreboard.io.query.store.valid := true.B
        scoreboard.io.query.store.bits.c_reg := Store_MicroInst_Resource_Info.C_MRegID
        
        // Scoreboard 返回的 ready 信号表示：
        // 1. C Ready（Load 已完成）
        // 2. C 无写者（Compute 已完成写入）
        val Scoreboard_Dependency_Ready = scoreboard.io.query.store.ready
        
        // 保留旧的 Dependent_Compute_Finish_Ready_Go 用于过渡期验证
        val Dependent_Compute_Finish_Ready_Go = Compute_MicroInst_FINISH_Ready_GO(Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index)

        val Can_Issue_CML_Micro_Inst = io.CML_MicroTask_Config.MicroTaskReady //缺少DMReg的空闲状态,保证同时任务被发射

        // 使用 Scoreboard 依赖检查（临时方案）
        val Can_Issue_Store_Micro_Inst = Can_Issue_CML_Micro_Inst && Scoreboard_Dependency_Ready
        
        // ====================================================================
        // 过渡期验证：检查 Scoreboard 和旧机制的一致性
        // ====================================================================
        when(Scoreboard_Dependency_Ready && !Dependent_Compute_Finish_Ready_Go) {
            printf("[WARNING @%d] Store temporary solution: Scoreboard allows but Compute not finished! " +
                   "C_MRegID=%d, Compute_FIFO_idx=%d\n",
                io.DebugTimeStampe,
                Store_MicroInst_Resource_Info.C_MRegID,
                Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index
            )
        }
        
        when(!Scoreboard_Dependency_Ready && Dependent_Compute_Finish_Ready_Go) {
            printf("[INFO @%d] Store temporary solution: Scoreboard blocks but old mechanism allows. " +
                   "C_MRegID=%d (Compute is writing)\n",
                io.DebugTimeStampe,
                Store_MicroInst_Resource_Info.C_MRegID
            )
        }

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
            io.CML_MicroTask_Config.MatrixRegTensor_M := Store_MicroInst.MatrixRegTensor_M
            io.CML_MicroTask_Config.MatrixRegTensor_N := Store_MicroInst.MatrixRegTensor_N
            io.CML_MicroTask_Config.IsLoadMicroTask := false.B
            io.CML_MicroTask_Config.IsStoreMicroTask := true.B
            io.CML_MicroTask_Config.Is_Transpose := Store_MicroInst.Is_Transpose
            Store_Micro_Inst_Is_Last_Store := Store_MicroInst.Is_Last_Store

            Current_CML_MReg_ID := Store_MicroInst_Resource_Info.C_MRegID
            io.CML_MicroTask_Config.MicroTaskValid := true.B
            Store_Micro_Inst_Wait_C_Finish := true.B
            Store_Micro_Inst_Issue_State_Reg := issue_state_issue
            Compute_MicroInst_FINISH_Ready_Commit(Store_MicroInst_Resource_Info.Compute_Micro_Inst_FIFO_Index) := true.B//标记这条Compute指令已经可以被提交了
            
            // Scoreboard Update: Store 发射
            scoreboard.io.update.store_issue := true.B
            scoreboard.io.update.store_issue_c_reg := Store_MicroInst_Resource_Info.C_MRegID
            scoreboard.io.update.store_issue_fifo_idx := Store_MicroInst_FIFO_Tail
            
            store_issue.entry.Store_MicroInst := Store_MicroInst_FIFO(Store_MicroInst_FIFO_Tail)
            store_issue.entry.FIFO_Index := Store_MicroInst_FIFO_Tail
            store_issue.en := true.B
        }.elsewhen(Store_Micro_Inst_Issue_State_Reg === issue_state_issue)
        {
            io.CML_MicroTask_Config.MicroTaskEndReady := Store_Micro_Inst_Wait_C_Finish
            when(Store_Micro_Inst_Wait_C_Finish && io.CML_MicroTask_Config.MicroTaskEndValid)
            {
                Store_Micro_Inst_Wait_C_Finish := false.B
                C_MReg_Free(Store_MicroInst_Resource_Info.C_MRegID) := true.B
                
                // Scoreboard Update: Store 完成
                scoreboard.io.update.store_finish := true.B
                scoreboard.io.update.store_finish_c_reg := Store_MicroInst_Resource_Info.C_MRegID
                
                store_cfinish.entry.Store_MicroInst := Store_MicroInst_FIFO(Store_MicroInst_FIFO_Tail)
                store_cfinish.entry.FIFO_Index := Store_MicroInst_FIFO_Tail
                store_cfinish.en := true.B
            }
            when(!Store_Micro_Inst_Wait_C_Finish)
            {
                Store_MicroInst_FIFO_Tail := WrapInc(Store_MicroInst_FIFO_Tail, 4)
                Store_Micro_Inst_Issue_State_Reg := issue_state_idle
                when(Store_Micro_Inst_Is_Last_Store)
                {
                    MacroInst_FIFO_Total_Finish(Store_MicroInst_Resource_Info.Marco_Inst_FIFO_Index) := true.B
                    
                    macro_finish.entry.Macro_Inst := MacroInst_FIFO(MacroInst_FIFO_Tail)
                    macro_finish.entry.FIFO_Index := Store_MicroInst_Resource_Info.Marco_Inst_FIFO_Index
                    macro_finish.en := true.B
                }

                store_finish.entry.Store_MicroInst := Store_MicroInst_FIFO(Store_MicroInst_FIFO_Tail)
                store_finish.entry.FIFO_Index := Store_MicroInst_FIFO_Tail
                store_finish.en := true.B
            }
        }
    }

    // ========== ChiselDB Logging Section ==========
    // Macro Instruction Events
    macro_autoclear.log(0.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstAutoClear")
    macro_insert.log(1.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstInsert")
    macro_insertfull.log(2.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstInsertFull")
    macro_decodestart.log(3.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstDecodeStart")
    macro_decodeend.log(5.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstDecodeEnd")
    macro_finish.log(6.U, MacroInst_FIFO_Head, MacroInst_FIFO_Tail, macroInstEventTable, "MacroInstFinish")

    // Load Micro Instruction Events
    load_insert.log(0.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstInsert")
    load_issue.log(1.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstIssue")
    load_afinish.log(2.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstAFinish")
    load_bfinish.log(3.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstBFinish")
    load_cfinish.log(4.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstCFinish")
    load_finish.log(5.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstFinish")
    load_commit.log(6.U, Load_MicroInst_FIFO_Head, Load_MicroInst_FIFO_Tail, loadMicroInstEventTable, "LoadMicroInstCommit")

    // Compute Micro Instruction Events
    comp_insert.log(0.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstInsert")
    comp_issue.log(1.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstIssue")
    comp_afinish.log(2.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstAFinish")
    comp_bfinish.log(3.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstBFinish")
    comp_cfinish.log(4.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstCFinish")
    comp_aopfinish.log(5.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstAopFinish")
    comp_finish_without_store.log(6.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstFinishWithoutStore")
    comp_finish.log(7.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstFinish")
    comp_commit.log(8.U, Compute_MicroInst_FIFO_Head, Compute_MicroInst_FIFO_Tail, computeMicroInstEventTable, "ComputeMicroInstCommit")

    // Store Micro Instruction Events
    store_insert.log(0.U, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail, storeMicroInstEventTable, "StoreMicroInstInsert")
    store_issue.log(1.U, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail, storeMicroInstEventTable, "StoreMicroInstIssue")
    store_cfinish.log(2.U, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail, storeMicroInstEventTable, "StoreMicroInstCFinish")
    store_finish.log(3.U, Store_MicroInst_FIFO_Head, Store_MicroInst_FIFO_Tail, storeMicroInstEventTable, "StoreMicroInstFinish")
}
