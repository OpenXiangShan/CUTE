
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
    
    // 统一的AB矩阵寄存器堆：4个实例
    // AB(0)=A(0), AB(1)=A(1), AB(2)=B(0), AB(3)=B(1)
    val ABMatrixRegs = Seq.tabulate(4)(i => Module(new ABMatrixReg(i))).toVector
    val ADC = Module(new ADataController)
    val AML = Module(new AMemoryLoader)
    val BDC = Module(new BDataController)
    val BML = Module(new BMemoryLoader)

    val CMatrixRegs = Seq.tabulate(2)(i => Module(new CMatrixReg(i))).toVector//双缓冲（MatrixReg）
    val CDC = Module(new CDataController)
    val CML = Module(new CMemoryLoader)

    val AOp = Module(new AfterOpsModule)
    val VecSIf = Module(new VectorStreamInterface)

    val TaskCtrl: BaseTaskController = Module(new TaskController)
    
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

    //给每个 MatrixReg 的输入进行默认赋值
    
    // AB MatrixReg：4个实例的初始化
    for (i <- 0 until 4){
        //DataController的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        //MemoryLoader的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.valid := false.B
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankId.bits)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ZeroFill := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ZeroFill)
    }

    // C MatrixReg：2个实例的初始化
    for (i <- 0 until 2){
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
    //映射关系：A(0)->AB(0), A(1)->AB(1), B(0)->AB(2), B(1)->AB(3)

    if (UseNewTaskController) {
        def abLoaderActivity(ioPort: ABMemoryLoaderMatrixRegIO): Bool = {
            ioPort.BankId.valid ||
            ioPort.BankAddr.map(_.valid).reduce(_||_) ||
            ioPort.Data.map(_.valid).reduce(_||_) ||
            ioPort.ZeroFill.map(_.valid).reduce(_||_)
        }

        def initABLoaderPort(dest: ABMemoryLoaderMatrixRegIO): Unit = {
            dest.BankId.valid := false.B
            dest.BankId.bits := 0.U
            for (b <- 0 until ABMatrixRegNBanks) {
                dest.BankAddr(b).valid := false.B
                dest.BankAddr(b).bits := 0.U
                dest.Data(b).valid := false.B
                dest.Data(b).bits := 0.U
                dest.ZeroFill(b).valid := false.B
                dest.ZeroFill(b).bits := 0.U
            }
        }

        def copyABLoaderPort(dest: ABMemoryLoaderMatrixRegIO, src: ABMemoryLoaderMatrixRegIO): Unit = {
            dest.BankId.valid := src.BankId.valid
            dest.BankId.bits := src.BankId.bits
            for (b <- 0 until ABMatrixRegNBanks) {
                dest.BankAddr(b).valid := src.BankAddr(b).valid
                dest.BankAddr(b).bits := src.BankAddr(b).bits
                dest.Data(b).valid := src.Data(b).valid
                dest.Data(b).bits := src.Data(b).bits
                dest.ZeroFill(b).valid := src.ZeroFill(b).valid
                dest.ZeroFill(b).bits := src.ZeroFill(b).bits
            }
        }

        for (regIdx <- 0 until ABMatrixRegCount) {
            val dest = ABMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
            initABLoaderPort(dest)
            val amlSel = AML.io.MatrixRegId === regIdx.U
            val bmlSel = BML.io.MatrixRegId === regIdx.U
            val amlActive = abLoaderActivity(AML.io.ToMatrixRegIO)
            val bmlActive = abLoaderActivity(BML.io.ToMatrixRegIO)
            when(amlSel && bmlSel && (amlActive || bmlActive)) {
                assert(false.B, cf"[CUTETop] AML and BML choose the same AB MatrixReg($regIdx)")
            }
            when(amlSel) {
                copyABLoaderPort(dest, AML.io.ToMatrixRegIO)
            }.elsewhen(bmlSel) {
                copyABLoaderPort(dest, BML.io.ToMatrixRegIO)
            }
        }

        for (regIdx <- 0 until ABMatrixRegCount) {
            val dest = ABMatrixRegs(regIdx).io.MatrixRegIO.FromDataController
            dest.BankAddr.valid := false.B
            dest.BankAddr.bits := 0.U.asTypeOf(dest.BankAddr.bits)
            val adcSel = ADC.io.MatrixRegId === regIdx.U
            val bdcSel = BDC.io.MatrixRegId === regIdx.U
            val adcActive = ADC.io.FromMatrixRegIO.BankAddr.valid
            val bdcActive = BDC.io.FromMatrixRegIO.BankAddr.valid
            when(adcSel && bdcSel && (adcActive || bdcActive)) {
                assert(false.B, cf"[CUTETop] ADC and BDC choose the same AB MatrixReg($regIdx)")
            }
            when(adcSel) {
                dest.BankAddr.valid := ADC.io.FromMatrixRegIO.BankAddr.valid
                dest.BankAddr.bits := ADC.io.FromMatrixRegIO.BankAddr.bits
            }.elsewhen(bdcSel) {
                dest.BankAddr.valid := BDC.io.FromMatrixRegIO.BankAddr.valid
                dest.BankAddr.bits := BDC.io.FromMatrixRegIO.BankAddr.bits
            }
        }

        val adcSelVec = VecInit((0 until ABMatrixRegCount).map(i => ADC.io.MatrixRegId === i.U))
        val adcHasSel = adcSelVec.asUInt.orR
        val adcReadyChoices = (0 until ABMatrixRegCount).map(i => adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.ready) :+
            (!adcHasSel -> false.B)
        ADC.io.FromMatrixRegIO.BankAddr.ready := Mux1H(adcReadyChoices)
        val adcDataValidChoices = (0 until ABMatrixRegCount).map(i => adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.valid) :+
            (!adcHasSel -> false.B)
        ADC.io.FromMatrixRegIO.Data.valid := Mux1H(adcDataValidChoices)
        for (bank <- 0 until ABMatrixRegNBanks) {
            val adcDataBitsChoices = (0 until ABMatrixRegCount).map(i => adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)) :+
                (!adcHasSel -> 0.U(ABMatrixRegEntryBitSize.W))
            ADC.io.FromMatrixRegIO.Data.bits(bank) := Mux1H(adcDataBitsChoices)
        }

        val bdcSelVec = VecInit((0 until ABMatrixRegCount).map(i => BDC.io.MatrixRegId === i.U))
        val bdcHasSel = bdcSelVec.asUInt.orR
        val bdcReadyChoices = (0 until ABMatrixRegCount).map(i => bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.ready) :+
            (!bdcHasSel -> false.B)
        BDC.io.FromMatrixRegIO.BankAddr.ready := Mux1H(bdcReadyChoices)
        val bdcDataValidChoices = (0 until ABMatrixRegCount).map(i => bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.valid) :+
            (!bdcHasSel -> false.B)
        BDC.io.FromMatrixRegIO.Data.valid := Mux1H(bdcDataValidChoices)
        for (bank <- 0 until ABMatrixRegNBanks) {
            val bdcDataBitsChoices = (0 until ABMatrixRegCount).map(i => bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)) :+
                (!bdcHasSel -> 0.U(ABMatrixRegEntryBitSize.W))
            BDC.io.FromMatrixRegIO.Data.bits(bank) := Mux1H(bdcDataBitsChoices)
        }

        for (regIdx <- 0 until CMatrixRegCount) {
            val dest = CMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
            dest.ReadRequestToMatrixReg.BankAddr := 0.U.asTypeOf(dest.ReadRequestToMatrixReg.BankAddr)
            dest.WriteRequestToMatrixReg.BankAddr := 0.U.asTypeOf(dest.WriteRequestToMatrixReg.BankAddr)
            dest.WriteRequestToMatrixReg.Data := 0.U.asTypeOf(dest.WriteRequestToMatrixReg.Data)
            dest.ReadWriteRequest := 0.U
            when(CML.io.MatrixRegId === regIdx.U) {
                dest.ReadRequestToMatrixReg.BankAddr := CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr
                dest.WriteRequestToMatrixReg.BankAddr := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr
                dest.WriteRequestToMatrixReg.Data := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data
                dest.ReadWriteRequest := CML.io.ToMatrixRegIO.ReadWriteRequest
            }
        }

        val cmlSelVec = VecInit((0 until CMatrixRegCount).map(i => CML.io.MatrixRegId === i.U))
        val cmlHasSel = cmlSelVec.asUInt.orR
        for (bank <- 0 until CMatrixRegNBanks) {
            val readRespValidChoices = (0 until CMatrixRegCount).map(i => cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).valid) :+
                (!cmlHasSel -> false.B)
            val readRespBitsChoices = (0 until CMatrixRegCount).map(i => cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).bits) :+
                (!cmlHasSel -> 0.U(CMatrixRegEntryBitSize.W))
            CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid := Mux1H(readRespValidChoices)
            CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits := Mux1H(readRespBitsChoices)
        }
        val cmlRwRespChoices = (0 until CMatrixRegCount).map(i => cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadWriteResponse) :+
            (!cmlHasSel -> 0.U.asTypeOf(CMatrixRegs.head.io.MatrixRegIO.FromMemoryLoader.ReadWriteResponse))
        CML.io.ToMatrixRegIO.ReadWriteResponse := Mux1H(cmlRwRespChoices)

        for (regIdx <- 0 until CMatrixRegCount) {
            val dest = CMatrixRegs(regIdx).io.MatrixRegIO.FromDataController
            dest.ReadBankAddr := 0.U.asTypeOf(dest.ReadBankAddr)
            dest.WriteBankAddr := 0.U.asTypeOf(dest.WriteBankAddr)
            dest.WriteRequestData := 0.U.asTypeOf(dest.WriteRequestData)
            dest.ReadWriteRequest := 0.U
            when(CDC.io.MatrixRegId === regIdx.U) {
                dest.ReadBankAddr := CDC.io.FromMatrixRegIO.ReadBankAddr
                dest.WriteBankAddr := CDC.io.FromMatrixRegIO.WriteBankAddr
                dest.WriteRequestData := CDC.io.FromMatrixRegIO.WriteRequestData
                dest.ReadWriteRequest := CDC.io.FromMatrixRegIO.ReadWriteRequest
            }
        }

        val cdcSelVec = VecInit((0 until CMatrixRegCount).map(i => CDC.io.MatrixRegId === i.U))
        val cdcHasSel = cdcSelVec.asUInt.orR
        for (bank <- 0 until CMatrixRegNBanks) {
            val cdcReadRespValidChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).valid) :+
                (!cdcHasSel -> false.B)
            val cdcReadRespBitsChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).bits) :+
                (!cdcHasSel -> 0.U(CMatrixRegEntryBitSize.W))
            CDC.io.FromMatrixRegIO.ReadResponseData(bank).valid := Mux1H(cdcReadRespValidChoices)
            CDC.io.FromMatrixRegIO.ReadResponseData(bank).bits := Mux1H(cdcReadRespBitsChoices)
        }
        val cdcRwRespChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteResponse) :+
            (!cdcHasSel -> 0.U.asTypeOf(CMatrixRegs.head.io.MatrixRegIO.FromDataController.ReadWriteResponse))
        CDC.io.FromMatrixRegIO.ReadWriteResponse := Mux1H(cdcRwRespChoices)
    } else {
        // ADC连接到AB(0)或AB(1)
        when (TaskCtrl.io.MReg_CtrlInfo.ADC_MReg_ID === 0.U){
            ADC.io.FromMatrixRegIO <> ABMatrixRegs(0).io.MatrixRegIO.FromDataController
        }.otherwise{
            ADC.io.FromMatrixRegIO <> ABMatrixRegs(1).io.MatrixRegIO.FromDataController
        }

        // BDC连接到AB(2)或AB(3)
        when (TaskCtrl.io.MReg_CtrlInfo.BDC_MReg_ID === 0.U){
            BDC.io.FromMatrixRegIO <> ABMatrixRegs(2).io.MatrixRegIO.FromDataController
        }.otherwise{
            BDC.io.FromMatrixRegIO <> ABMatrixRegs(3).io.MatrixRegIO.FromDataController
        }

        // AML连接到AB(0)或AB(1)
        when (TaskCtrl.io.MReg_CtrlInfo.AML_MReg_ID === 0.U){
            AML.io.ToMatrixRegIO <> ABMatrixRegs(0).io.MatrixRegIO.FromMemoryLoader
        }.otherwise{
            AML.io.ToMatrixRegIO <> ABMatrixRegs(1).io.MatrixRegIO.FromMemoryLoader
        }

        // BML连接到AB(2)或AB(3)
        when (TaskCtrl.io.MReg_CtrlInfo.BML_MReg_ID === 0.U){
            BML.io.ToMatrixRegIO <> ABMatrixRegs(2).io.MatrixRegIO.FromMemoryLoader
        }.otherwise{
            BML.io.ToMatrixRegIO <> ABMatrixRegs(3).io.MatrixRegIO.FromMemoryLoader
        }
    }

    // CDC连接到C(0)或C(1)
    when (TaskCtrl.io.MReg_CtrlInfo.CDC_MReg_ID === 0.U){
        CDC.io.FromMatrixRegIO <> CMatrixRegs(0).io.MatrixRegIO.FromDataController
    }.otherwise{
        CDC.io.FromMatrixRegIO <> CMatrixRegs(1).io.MatrixRegIO.FromDataController
    }

    // CML连接到C(0)或C(1)
    when (TaskCtrl.io.MReg_CtrlInfo.CML_MReg_ID === 0.U){
        CML.io.ToMatrixRegIO <> CMatrixRegs(0).io.MatrixRegIO.FromMemoryLoader
    }.otherwise{
        CML.io.ToMatrixRegIO <> CMatrixRegs(1).io.MatrixRegIO.FromMemoryLoader
    }

    TaskCtrl.io.ygjkctrl.mrelease <> io.mrelease


}


