
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import scala.collection.parallel.Task

class CUTETopIO()(implicit p: Parameters) extends CuteBundle{
    val mmu2llc = Flipped(new MMU2TLIO)
    val ctrl2top = Flipped(new YGJKControl)
    val instfifo_head_id = Output(UInt(MarcoInstFIFODepthBitSize.W))
    val instfifo_tail_id = Output(UInt(MarcoInstFIFODepthBitSize.W))
    val instfifo_release = Output(Bool())
    val mrelease = Valid(new MreleaseIO)
}
class CUTEV2Top()(implicit p: Parameters) extends CuteModule{
    val io = IO(new CUTETopIO)

    val cutecounter = Wire(new CUTECounter)

    val time_stamp = RegInit(0.U(40.W))
    time_stamp := time_stamp + 1.U

    // printf("[CUTE perf %d] %x %x %x %x %x %x %x %x %x %x %x %x %x \n", time_stamp, cutecounter.ALoad, cutecounter.BLoad, cutecounter.CLoad, cutecounter.DStore, 
    //     cutecounter.InstQueueEmpty, cutecounter.getConfigured, cutecounter.AOPBusy, cutecounter.computeBusy, cutecounter.computeInstQueueEmpty, cutecounter.computeInstCanIssue, cutecounter.InstCanDecode,
    //     cutecounter.mmu_req_valid, cutecounter.mmu_req_ready)
    
    val AMatrixRegs = Seq.tabulate(2)(i => Module(new AMatrixReg(i))).toVector//双缓冲（MatrixReg）
    val ADC = Module(new ADataController)
    val AML = Module(new AMemoryLoader)

    val BMatrixRegs = Seq.tabulate(2)(i => Module(new BMatrixReg(i))).toVector//双缓冲（MatrixReg）
    val BDC = Module(new BDataController)
    val BML = Module(new BMemoryLoader)

    val CMatrixRegs = Seq.tabulate(2)(i => Module(new CMatrixReg(i))).toVector//双缓冲（MatrixReg）
    val CDC = Module(new CDataController)
    val CML = Module(new CMemoryLoader)

    val AOp = Module(new AfterOpsModule)
    val VecSIf = Module(new VectorStreamInterface)

    val TaskCtrl = Module(new TaskController)
    
    val MTE = Module(new MatrixTE)

    val MMU = Module(new LocalMMU)

    cutecounter.ALoad := TaskCtrl.io.ctrlCounter.ALoad
    cutecounter.BLoad := TaskCtrl.io.ctrlCounter.BLoad
    cutecounter.CLoad := TaskCtrl.io.ctrlCounter.CLoad
    cutecounter.DStore := TaskCtrl.io.ctrlCounter.DStore
    cutecounter.InstQueueEmpty := TaskCtrl.io.ctrlCounter.InstQueueEmpty
    cutecounter.getConfigured := TaskCtrl.io.ctrlCounter.getConfigured
    cutecounter.AOPBusy := TaskCtrl.io.ctrlCounter.AOPBusy
    cutecounter.computeInstQueueEmpty := TaskCtrl.io.ctrlCounter.computeInstQueueEmpty
    cutecounter.computeInstCanIssue := TaskCtrl.io.ctrlCounter.computeInstCanIssue
    cutecounter.InstCanDecode := TaskCtrl.io.ctrlCounter.InstCanDecode
    cutecounter.mmu_req_valid := io.mmu2llc.Request.valid
    cutecounter.mmu_req_ready := io.mmu2llc.Request.ready

    //debug reg
    val DebugTimeStampe = RegInit(0.U(32.W))
    DebugTimeStampe := DebugTimeStampe + 1.U

    TaskCtrl.io.DebugTimeStampe := DebugTimeStampe
    //ADC的默认输入
    ADC.io.FromMatrixRegIO.Data.valid := false.B
    ADC.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(ADC.io.FromMatrixRegIO.Data.bits)
    ADC.io.FromMatrixRegIO.BankAddr.ready := false.B
    ADC.io.ConfigInfo <> TaskCtrl.io.ADC_MicroTask_Config
    ADC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    //AML的默认输入
    AML.io.ConfigInfo <> TaskCtrl.io.AML_MicroTask_Config
    AML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    AML.io.LocalMMUIO <> MMU.io.ALocalMMUIO

    //BDC的默认输入
    BDC.io.FromMatrixRegIO.Data.valid := false.B
    BDC.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(BDC.io.FromMatrixRegIO.Data.bits)
    BDC.io.FromMatrixRegIO.BankAddr.ready := false.B
    BDC.io.ConfigInfo <> TaskCtrl.io.BDC_MicroTask_Config
    BDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    //BML的默认输入
    BML.io.ConfigInfo <> TaskCtrl.io.BML_MicroTask_Config
    BML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    BML.io.LocalMMUIO <> MMU.io.BLocalMMUIO

    //CDC的默认输入
    CDC.io.FromMatrixRegIO.ReadResponseData := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadResponseData)
    CDC.io.FromMatrixRegIO.ReadWriteResponse := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadWriteResponse)
    CDC.io.ConfigInfo <> TaskCtrl.io.CDC_MicroTask_Config
    CDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    CDC.io.AfterOpsInterface<>AOp.io.AfterOpsInterface

    //CML的默认输入
    CML.io.ConfigInfo <> TaskCtrl.io.CML_MicroTask_Config
    CML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    CML.io.LocalMMUIO <> MMU.io.CLocalMMUIO
    CML.io.ToMatrixRegIO.ReadWriteResponse := 0.U
    CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData := 0.U.asTypeOf(CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData)

    //AOP的默认输入
    //AOp的要连接到vpu,目前先空接
    AOp.io.ConfigInfo <> TaskCtrl.io.AOP_MicroTask_Config
    AOp.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    AOp.io.VectorInterface <> VecSIf.io.VectorInterface

    cutecounter.AOPBusy := !AOp.io.ConfigInfo.MicroTaskReady

    //VecSIF应该把VPU的输出输出接出去，但现在先空接
    val VPUIO = Module(new FakeVPU).io
    VPUIO.VPUInterface <> VecSIf.io.VPUInterface


    //MTE的默认输入
    MTE.io.VectorA <> ADC.io.VectorA
    MTE.io.VectorB <> BDC.io.VectorB
    MTE.io.MatirxC <> CDC.io.Matrix_C
    MTE.io.MatrixD <> CDC.io.ResultMatrix_D
    MTE.io.ConfigInfo <> TaskCtrl.io.MTE_MicroTask_Config
    MTE.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    ADC.io.ComputeGo := MTE.io.ComputeGo
    BDC.io.ComputeGo := MTE.io.ComputeGo
    CDC.io.ComputeGo := MTE.io.ComputeGo

    cutecounter.computeBusy := MTE.io.VectorA.valid

    // TaskCtrl.io.MTE_MicroTask_Config.ready := true.B
    // ADC.io.VectorA.ready := true.B
    // BDC.io.VectorB.ready := true.B
    // CDC.io.Matrix_C.ready := true.B
    // CDC.io.ResultMatrix_D.bits := 0xdeadbeefL.U
    // CDC.io.ResultMatrix_D.valid := true.B
    // ADC.io.ComputeGo := true.B
    // BDC.io.ComputeGo := true.B
    // CDC.io.ComputeGo := true.B
    
    //后续需要连入CPU的MMU或者IOMMU
    MMU.io.Config.refillPaddr := 0.U
    MMU.io.Config.refillVaddr := 0.U
    MMU.io.Config.refill_v := false.B
    MMU.io.Config.useVM := false.B
    MMU.io.Config.useVM_v := false.B
    MMU.io.LastLevelCacheTLIO <> io.mmu2llc

    io.ctrl2top <> TaskCtrl.io.ygjkctrl
    io.instfifo_head_id := TaskCtrl.io.instfifo_tail_id//原先代码里head/tail写反了
    io.instfifo_tail_id := TaskCtrl.io.instfifo_head_id//原先代码里head/tail写反了
    io.instfifo_release := TaskCtrl.io.instfifo_release

    //给每个 MatrixReg 页的输入进行默认赋值
    for (i <- 0 until 2){
        //A MatrixReg
        //ADC的请求
        AMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        AMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(AMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        // ASpad(i).io.MatrixRegIO.FromDataController.Data.valid := false.B
        // ASpad(i).io.MatrixRegIO.FromDataController.Data.bits := 0.U.asTypeOf(ASpad(i).io.MatrixRegIO.FromDataController.Data.bits)
        //AML的请求
        AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr := 0.U.asTypeOf(AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr)
        AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.valid := false.B
        AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits := 0.U.asTypeOf(AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits)
        AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data := 0.U.asTypeOf(AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data)
        AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ZeroFill := 0.U.asTypeOf(AMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ZeroFill)

        //B MatrixReg
        //BDC的请求
        BMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        BMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(BMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        // BSpad(i).io.MatrixRegIO.FromDataController.Data.valid := false.B
        // BSpad(i).io.MatrixRegIO.FromDataController.Data.bits := 0.U.asTypeOf(BSpad(i).io.MatrixRegIO.FromDataController.Data.bits)
        //BML的请求
        BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr := 0.U.asTypeOf(BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr)
        BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.valid := false.B
        BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits := 0.U.asTypeOf(BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits)
        BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data := 0.U.asTypeOf(BMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data)

        //C MatrixReg
        //CDC的请求
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadBankAddr := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadBankAddr)
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.WriteBankAddr := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromDataController.WriteBankAddr)
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.WriteRequestData := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromDataController.WriteRequestData)
        //CML的请求
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr)
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.BankAddr := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.BankAddr)
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.Data := 0.U.asTypeOf(CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.Data)
        //多个请求的仲裁器输入
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteRequest := 0.U
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadWriteRequest := 0.U
    }
    
    //根据MReg_CtrlInfo的值，选择对应的MReg

    when (TaskCtrl.io.MReg_CtrlInfo.ADC_MReg_ID === 0.U){
        ADC.io.FromMatrixRegIO <> AMatrixRegs(0).io.MatrixRegIO.FromDataController
    }.otherwise{
        ADC.io.FromMatrixRegIO <> AMatrixRegs(1).io.MatrixRegIO.FromDataController
    }

    when (TaskCtrl.io.MReg_CtrlInfo.BDC_MReg_ID === 0.U){
        BDC.io.FromMatrixRegIO <> BMatrixRegs(0).io.MatrixRegIO.FromDataController
    }.otherwise{
        BDC.io.FromMatrixRegIO <> BMatrixRegs(1).io.MatrixRegIO.FromDataController
    }

    when (TaskCtrl.io.MReg_CtrlInfo.CDC_MReg_ID === 0.U){
        CDC.io.FromMatrixRegIO <> CMatrixRegs(0).io.MatrixRegIO.FromDataController
    }.otherwise{
        CDC.io.FromMatrixRegIO <> CMatrixRegs(1).io.MatrixRegIO.FromDataController
    }

    when (TaskCtrl.io.MReg_CtrlInfo.AML_MReg_ID === 0.U){
        AML.io.ToMatrixRegIO <> AMatrixRegs(0).io.MatrixRegIO.FromMemoryLoader
    }.otherwise{
        AML.io.ToMatrixRegIO <> AMatrixRegs(1).io.MatrixRegIO.FromMemoryLoader
    }

    when (TaskCtrl.io.MReg_CtrlInfo.BML_MReg_ID === 0.U){
        BML.io.ToMatrixRegIO <> BMatrixRegs(0).io.MatrixRegIO.FromMemoryLoader
    }.otherwise{
        BML.io.ToMatrixRegIO <> BMatrixRegs(1).io.MatrixRegIO.FromMemoryLoader
    }

    when (TaskCtrl.io.MReg_CtrlInfo.CML_MReg_ID === 0.U){
        CML.io.ToMatrixRegIO <> CMatrixRegs(0).io.MatrixRegIO.FromMemoryLoader
    }.otherwise{
        CML.io.ToMatrixRegIO <> CMatrixRegs(1).io.MatrixRegIO.FromMemoryLoader
    }

    TaskCtrl.io.ygjkctrl.mrelease <> io.mrelease


}


