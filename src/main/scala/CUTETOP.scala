
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import scala.collection.parallel.Task

class CUTETopIO()(implicit p: Parameters) extends CuteBundle{
    val mmu2llc = Flipped(new MMU2TLIO)
    val ctrl2top = Flipped(new YGJKControl)
}
class CUTEV2Top()(implicit p: Parameters) extends CuteModule{
    val io = IO(new CUTETopIO)

    val time_stamp = RegInit(0.U(40.W))
    time_stamp := time_stamp + 1.U
    
    val ABMatrixRegs = Seq.tabulate(ABMatrixRegCount)(i => Module(new ABMatrixReg(i))).toVector
    val ADC = Module(new ADataController)
    val AML = Module(new AMemoryLoader)
    val BDC = Module(new BDataController)
    val BML = Module(new BMemoryLoader)

    val CMatrixRegs = Seq.tabulate(CMatrixRegCount)(i => Module(new CMatrixReg(i))).toVector
    val CDC = Module(new CDataController)
    val CML = Module(new CMemoryLoader)

    val TaskCtrl: BaseTaskController = Module(new TaskController)
    
    val MTE = Module(new MatrixTE)

    val MMU = Module(new LocalMMU)

    //debug reg
    val DebugTimeStampe = RegInit(0.U(32.W))
    DebugTimeStampe := DebugTimeStampe + 1.U

    TaskCtrl.io.DebugTimeStampe := DebugTimeStampe
    //ADC的默认输入
    ADC.io.FromMatrixRegIO.Data.valid := false.B
    ADC.io.FromMatrixRegIO.BankAddr.ready := false.B
    ADC.io.ConfigInfo <> TaskCtrl.io.ADC_MicroTask_Config
    ADC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    //AML的默认输入
    AML.io.ConfigInfo <> TaskCtrl.io.AML_MicroTask_Config
    AML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    AML.io.LocalMMUIO <> MMU.io.ALocalMMUIO

    //BDC的默认输入
    BDC.io.FromMatrixRegIO.Data.valid := false.B
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

    //CML的默认输入
    CML.io.ConfigInfo <> TaskCtrl.io.CML_MicroTask_Config
    CML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    CML.io.LocalMMUIO <> MMU.io.CLocalMMUIO
    CML.io.ToMatrixRegIO.ReadWriteResponse := 0.U
    CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData := 0.U.asTypeOf(CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData)

    //MTE的默认输入
    MTE.io.VectorA <> ADC.io.VectorA
    MTE.io.VectorB <> BDC.io.VectorB
    MTE.io.MatrixC <> CDC.io.Matrix_C
    MTE.io.MatrixD <> CDC.io.ResultMatrix_D
    MTE.io.ConfigInfo <> TaskCtrl.io.MTE_MicroTask_Config
    MTE.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    ADC.io.ComputeGo := MTE.io.ComputeGo
    BDC.io.ComputeGo := MTE.io.ComputeGo
    CDC.io.ComputeGo := MTE.io.ComputeGo
    
    //后续需要连入CPU的MMU或者IOMMU
    MMU.io.LastLevelCacheTLIO <> io.mmu2llc

    io.ctrl2top <> TaskCtrl.io.ygjkctrl

    //给每个 MatrixReg 的输入进行默认赋值
    
    // AB MatrixReg
    for (i <- 0 until ABMatrixRegCount){
        //DataController的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        //MemoryLoader的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data)
    }

    // C MatrixReg
    for (i <- 0 until CMatrixRegCount){
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteRequest := 0.U
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadWriteRequest := 0.U
    }

    def abLoaderActivity(ioPort: ABMemoryLoaderMatrixRegIO): Bool = {
        ioPort.BankAddr.map(_.valid).reduce(_||_)
    }

    def disableABLoaderPort(dest: ABMemoryLoaderMatrixRegIO): Unit = {
        for (b <- 0 until ABMatrixRegNBanks) {
            dest.BankAddr(b).valid := false.B
            dest.BankAddr(b).bits := DontCare
            dest.Data(b).valid := false.B
            dest.Data(b).bits := DontCare
        }
    }

    def copyABLoaderPort(dest: ABMemoryLoaderMatrixRegIO, src: ABMemoryLoaderMatrixRegIO): Unit = {
        for (b <- 0 until ABMatrixRegNBanks) {
            dest.BankAddr(b).valid := src.BankAddr(b).valid
            dest.BankAddr(b).bits := src.BankAddr(b).bits
            dest.Data(b).valid := src.Data(b).valid
            dest.Data(b).bits := src.Data(b).bits
        }
    }
    
    for (regIdx <- 0 until ABMatrixRegCount) {
        val dest = ABMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        val amlSel = AML.io.MatrixRegId === regIdx.U
        val bmlSel = BML.io.MatrixRegId === regIdx.U
        val amlActive = abLoaderActivity(AML.io.ToMatrixRegIO)
        val bmlActive = abLoaderActivity(BML.io.ToMatrixRegIO)
        when(amlSel && bmlSel && amlActive && bmlActive) {
            assert(false.B, cf"[CUTETop] AML and BML choose the same AB MatrixReg($regIdx)")
        }
        when(amlSel && amlActive) {
            copyABLoaderPort(dest, AML.io.ToMatrixRegIO)
        }.elsewhen(bmlSel && bmlActive) {
            copyABLoaderPort(dest, BML.io.ToMatrixRegIO)
        }.otherwise {
            disableABLoaderPort(dest)
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
        }.otherwise {
            dest.BankAddr.valid := false.B
            dest.BankAddr.bits := DontCare
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
        val adcDataBitsChoices = (0 until ABMatrixRegCount).map(i =>
            adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)
        )
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
        val bdcDataBitsChoices = (0 until ABMatrixRegCount).map(i =>
            bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)
        )
        BDC.io.FromMatrixRegIO.Data.bits(bank) := Mux1H(bdcDataBitsChoices)
    }

    for (regIdx <- 0 until CMatrixRegCount) {
        val dest = CMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        
        when(CML.io.MatrixRegId === regIdx.U) {
            for (bank <- 0 until CMatrixRegNBanks) {
                dest.ReadRequestToMatrixReg.BankAddr(bank).valid := CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).valid
                dest.WriteRequestToMatrixReg.BankAddr(bank).valid := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(bank).valid
                dest.WriteRequestToMatrixReg.Data(bank).valid := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(bank).valid
            }
            dest.ReadWriteRequest := CML.io.ToMatrixRegIO.ReadWriteRequest
        }.otherwise {
            for (bank <- 0 until CMatrixRegNBanks) {
                dest.ReadRequestToMatrixReg.BankAddr(bank).valid := false.B
                dest.WriteRequestToMatrixReg.BankAddr(bank).valid := false.B
                dest.WriteRequestToMatrixReg.Data(bank).valid := false.B
            }
        }

        for (bank <- 0 until CMatrixRegNBanks) {
            dest.ReadRequestToMatrixReg.BankAddr(bank).bits := CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).bits
            dest.WriteRequestToMatrixReg.BankAddr(bank).bits := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(bank).bits
            dest.WriteRequestToMatrixReg.Data(bank).bits := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(bank).bits
        }
    }

    val cmlSelVec = VecInit((0 until CMatrixRegCount).map(i => CML.io.MatrixRegId === i.U))
    val cmlHasSel = cmlSelVec.asUInt.orR
    for (bank <- 0 until CMatrixRegNBanks) {
        val readRespValidChoices = (0 until CMatrixRegCount).map(i =>
            cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).valid
        ) :+ (!cmlHasSel -> false.B)
        val readRespBitsChoices = (0 until CMatrixRegCount).map(i =>
            cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).bits
        )
        CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid := Mux1H(readRespValidChoices)
        CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits := Mux1H(readRespBitsChoices)
    }
    val cmlRwRespChoices = (0 until CMatrixRegCount).map(i => cmlSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadWriteResponse) :+
        (!cmlHasSel -> 0.U.asTypeOf(CMatrixRegs.head.io.MatrixRegIO.FromMemoryLoader.ReadWriteResponse))
    CML.io.ToMatrixRegIO.ReadWriteResponse := Mux1H(cmlRwRespChoices)

    for (regIdx <- 0 until CMatrixRegCount) {
        val dest = CMatrixRegs(regIdx).io.MatrixRegIO.FromDataController
        
        when(CDC.io.MatrixRegId === regIdx.U) {
            for (bank <- 0 until CMatrixRegNBanks) {
                dest.ReadBankAddr(bank).valid := CDC.io.FromMatrixRegIO.ReadBankAddr(bank).valid
                dest.WriteBankAddr(bank).valid := CDC.io.FromMatrixRegIO.WriteBankAddr(bank).valid
                dest.WriteRequestData(bank).valid := CDC.io.FromMatrixRegIO.WriteRequestData(bank).valid
            }
            dest.ReadWriteRequest := CDC.io.FromMatrixRegIO.ReadWriteRequest
        }.otherwise {
            for (bank <- 0 until CMatrixRegNBanks) {
                dest.ReadBankAddr(bank).valid := false.B
                dest.WriteBankAddr(bank).valid := false.B
                dest.WriteRequestData(bank).valid := false.B
            }
        }

        for (bank <- 0 until CMatrixRegNBanks) {
            dest.ReadBankAddr(bank).bits := CDC.io.FromMatrixRegIO.ReadBankAddr(bank).bits
            dest.WriteBankAddr(bank).bits := CDC.io.FromMatrixRegIO.WriteBankAddr(bank).bits
            dest.WriteRequestData(bank).bits := CDC.io.FromMatrixRegIO.WriteRequestData(bank).bits
        }
    }

    val cdcSelVec = VecInit((0 until CMatrixRegCount).map(i => CDC.io.MatrixRegId === i.U))
    val cdcHasSel = cdcSelVec.asUInt.orR
    for (bank <- 0 until CMatrixRegNBanks) {
        val cdcReadRespValidChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) ->
            CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).valid
        ) :+ (!cdcHasSel -> false.B)
        val cdcReadRespBitsChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) ->
            CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).bits
        )
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).valid := Mux1H(cdcReadRespValidChoices)
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).bits := Mux1H(cdcReadRespBitsChoices)
    }
    val cdcRwRespChoices = (0 until CMatrixRegCount).map(i =>
          cdcSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteResponse
      ) :+ (
          !cdcHasSel -> 0.U.asTypeOf(CMatrixRegs.head.io.MatrixRegIO.FromDataController.ReadWriteResponse)
      )
    CDC.io.FromMatrixRegIO.ReadWriteResponse := Mux1H(cdcRwRespChoices)
}


