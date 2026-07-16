
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
// import scala.collection.parallel.Task

class CUTETopIO()(implicit p: Parameters) extends CuteBundle{
    val mmu2llc = Flipped(new MMU2TLIO)
    val ctrl2top = Flipped(new YGJKControl)
    val perf = Output(new CutePerfToCoreIO)
}

class AMLWrapper(contextName: String = "AML")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (AMLUseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })

    if (!AMLUseLegacyLoader) {
        val inner = Module(new MultiChannelsABMemLoader("AML", nameContext))
            .suggestName(s"${nameContext}_multi_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
    } else {
        val inner = Module(new AMemoryLoader).suggestName(s"${nameContext}_legacy_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
    }
}

class BMLWrapper(contextName: String = "BML")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (BMLUseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new BMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })

    if (!BMLUseLegacyLoader) {
        val inner = Module(new MultiChannelsABMemLoader("BML", nameContext))
            .suggestName(s"${nameContext}_multi_loader")
        val bToAConfig = Wire(new AMLMicroTaskConfigIO)
        bToAConfig.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr
        bToAConfig.ApplicationTensor_A.ApplicationTensor_A_Stride_M := io.ConfigInfo.ApplicationTensor_B.ApplicationTensor_B_Stride_N
        bToAConfig.ApplicationTensor_A.dataType := io.ConfigInfo.ApplicationTensor_B.dataType
        bToAConfig.ApplicationTensor_A.HasTail := io.ConfigInfo.ApplicationTensor_B.HasTail
        bToAConfig.ApplicationTensor_A.TailByteMask := io.ConfigInfo.ApplicationTensor_B.TailByteMask
        bToAConfig.ApplicationTensor_A.K_Beat_Count := io.ConfigInfo.ApplicationTensor_B.K_Beat_Count
        bToAConfig.MatrixRegTensor_M := io.ConfigInfo.MatrixRegTensor_N
        bToAConfig.MatrixRegTensor_K := io.ConfigInfo.MatrixRegTensor_K
        bToAConfig.MatrixRegId := io.ConfigInfo.MatrixRegId
        bToAConfig.Conherent := io.ConfigInfo.Conherent
        bToAConfig.Is_Transpose := io.ConfigInfo.Is_Transpose
        bToAConfig.MicroTaskValid := io.ConfigInfo.MicroTaskValid
        bToAConfig.MicroTaskEndReady := io.ConfigInfo.MicroTaskEndReady
        bToAConfig.LoadTaskInfo.Is_FullLoad := true.B
        bToAConfig.LoadTaskInfo.Is_ZeroLoad := false.B
        bToAConfig.LoadTaskInfo.Is_RepeatRowLoad := false.B
        bToAConfig.pc.foreach(_ := io.ConfigInfo.pc.getOrElse(0xDEAD1234.U))
        bToAConfig.coreid.foreach(_ := io.ConfigInfo.coreid.getOrElse(0xDEAD5678.U))

        io.ConfigInfo.MicroTaskReady := bToAConfig.MicroTaskReady
        io.ConfigInfo.MicroTaskEndValid := bToAConfig.MicroTaskEndValid

        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> bToAConfig
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
    } else {
        val inner = Module(new BMemoryLoader).suggestName(s"${nameContext}_legacy_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
    }
}

class CMLWrapper(contextName: String = "CML")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (CLoadUseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new CMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new CMLMicroTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(CMatrixRegIdWidth.W))
    })

    if (!CLoadUseLegacyLoader) {
        val inner = Module(new MultiChannelsCMemLoader).suggestName(s"${nameContext}_multi_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LoadLocalMMUIO <> io.LoadLocalMMUIO
        inner.io.StoreLocalMMUIO <> io.StoreLocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.LoadMatrixRegId := inner.io.LoadMatrixRegId
        io.StoreMatrixRegId := inner.io.StoreMatrixRegId
    } else {
        val inner = Module(new CMemoryLoader).suggestName(s"${nameContext}_legacy_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LoadLocalMMUIO <> io.LoadLocalMMUIO
        inner.io.StoreLocalMMUIO <> io.StoreLocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.LoadMatrixRegId := inner.io.LoadMatrixRegId
        io.StoreMatrixRegId := inner.io.StoreMatrixRegId
    }
}

class CUTEV2Top()(implicit p: Parameters) extends CuteModule{
    private def summarizeLoaderMode(mode: ResponseChannelHelper.LoaderMode): String = {
        if (mode.isLegacy) "legacy(1resp)"
        else s"bridge(${mode.responseChannelCount}resp)"
    }

    private def loaderBridgeConfigSummary: String = {
        val config = parsedLoaderBridgeChannelConfig
        Seq(
            s"AML=${summarizeLoaderMode(config.a)}",
            s"BML=${summarizeLoaderMode(config.b)}",
            s"CLoad=${summarizeLoaderMode(config.cLoad)}",
            s"CStore=${summarizeLoaderMode(config.cStore)}"
        ).mkString(s"${config.render} [", ", ", "]")
    }

    println(
        s"[CUTE][elab][CUTEV2Top] LoaderBridgeChannelConfig $loaderBridgeConfigSummary"
    )

    val io = IO(new CUTETopIO)

    val time_stamp = RegInit(0.U(40.W))
    time_stamp := time_stamp + 1.U

    private def loaderBridgeContext(loaderName: String, bankCount: Int, reqCount: Int, respCount: Int): String = {
        VerilogNameHelper.sanitize(s"${loaderName}_${bankCount}bank_${reqCount}req_${respCount}resp")
    }

    private def multiOrLegacyBridgeContext(
        loaderName: String,
        useLegacy: Boolean,
        bankCount: Int,
        reqCount: Int,
        respCount: Int
    ): String = {
        val baseContext = loaderBridgeContext(loaderName, bankCount, reqCount, respCount)
        if (useLegacy) VerilogNameHelper.sanitize(s"${baseContext}_legacy")
        else baseContext
    }

    def connectLoaderRequests(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int
    ): Unit = {
        for (i <- 0 until channelCount) {
            mmu.Request(i) <> loader.Request(i)
        }
        loader.ConherentRequsetSourceID := mmu.ConherentRequsetSourceID
        loader.nonConherentRequsetSourceID := mmu.nonConherentRequsetSourceID
    }

    def connectLoaderRequestsWithGroupedArbiter(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int,
        reqChannelCount: Int,
        contextName: String
    ): Unit = {
        require(reqChannelCount >= 1 && reqChannelCount <= channelCount, s"reqChannelCount ($reqChannelCount) must be within [1, $channelCount]")

        val nameContext = VerilogNameHelper.sanitize(contextName)
        loader.ConherentRequsetSourceID := mmu.ConherentRequsetSourceID
        loader.nonConherentRequsetSourceID := mmu.nonConherentRequsetSourceID

        if (reqChannelCount == channelCount) {
            for (i <- 0 until channelCount) {
                mmu.Request(i) <> loader.Request(i)
            }
        } else {
            require(channelCount % reqChannelCount == 0, s"channelCount ($channelCount) must be divisible by reqChannelCount ($reqChannelCount)")

            for (group <- 0 until reqChannelCount) {
                val banks = ResponseChannelHelper.banksInGroup(group, reqChannelCount, channelCount)
                val arb = Module(new Arbiter(new MMURequestIO, banks.size))
                    .suggestName(s"${nameContext}_req_group${group}_arb")

                for ((bank, idx) <- banks.zipWithIndex) {
                    arb.io.in(idx) <> loader.Request(bank)
                }

                mmu.Request(group).valid := arb.io.out.valid
                mmu.Request(group).bits := arb.io.out.bits
                arb.io.out.ready := mmu.Request(group).ready
            }

            for (group <- reqChannelCount until channelCount) {
                mmu.Request(group).valid := false.B
                mmu.Request(group).bits := DontCare
            }
        }
    }

    def connectWithResponseBridge(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int,
        respChannelCount: Int,
        reqChannelCount: Int,
        bankIdWidth: Int,
        bankIdOffset: Int,
        baseDepth: Int,
        contextName: String
    ): Unit = {
        val nameContext = VerilogNameHelper.sanitize(contextName)
        connectLoaderRequestsWithGroupedArbiter(loader, mmu, channelCount, reqChannelCount, nameContext)

        val bridge = Module(new ResponseChannelBridge(
            inputChannelCount = channelCount,
            respChannelCount = respChannelCount,
            bankCount = channelCount,
            baseDepth = baseDepth,
            dataWidth = outsideDataWidth,
            sourceIdWidth = 64,
            bankIdWidth = bankIdWidth,
            bankIdOffset = bankIdOffset,
            debugEnable = YJPDebugEnable,
            contextName = nameContext
        )).suggestName(s"${nameContext}_response_bridge")
        bridge.io.timeStamp := time_stamp

        for (i <- 0 until channelCount) {
            bridge.io.in(i).valid := mmu.Response(i).valid
            bridge.io.in(i).bits := mmu.Response(i).bits
            mmu.Response(i).ready := bridge.io.in(i).ready

            loader.Response(i).valid := bridge.io.out(i).valid
            loader.Response(i).bits := bridge.io.out(i).bits
            bridge.io.out(i).ready := loader.Response(i).ready
        }
    }

    def connectWithResponseArbiter(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int,
        contextName: String
    ): Unit = {
        val nameContext = VerilogNameHelper.sanitize(contextName)
        connectLoaderRequests(loader, mmu, channelCount)

        val arb = Module(new Arbiter(new MMUResponseIO, channelCount))
            .suggestName(s"${nameContext}_legacy_resp_arbiter")
        for (i <- 0 until channelCount) {
            arb.io.in(i).valid := mmu.Response(i).valid
            arb.io.in(i).bits := mmu.Response(i).bits
            mmu.Response(i).ready := arb.io.in(i).ready
        }

        loader.Response(0).valid := arb.io.out.valid
        loader.Response(0).bits := arb.io.out.bits
        arb.io.out.ready := loader.Response(0).ready
        for (i <- 1 until channelCount) {
            loader.Response(i).valid := false.B
            loader.Response(i).bits := DontCare
        }
    }
    
    val cStoreUsesGroupedRequest =
        CStoreBridgeResponseChannelCount == 2 || CStoreBridgeResponseChannelCount == 4
    val cStoreRequestChannelCount =
        if (cStoreUsesGroupedRequest) CStoreBridgeResponseChannelCount else CMatrixRegNBanks

    val amlBridgeContext = multiOrLegacyBridgeContext(
        "AML",
        AMLUseLegacyLoader,
        ABMatrixRegNBanks,
        ABMatrixRegNBanks,
        AMLResponseChannelCount
    )
    val bmlBridgeContext = multiOrLegacyBridgeContext(
        "BML",
        BMLUseLegacyLoader,
        ABMatrixRegNBanks,
        ABMatrixRegNBanks,
        BMLResponseChannelCount
    )
    val cLoadBridgeContext = multiOrLegacyBridgeContext(
        "CLoad",
        CLoadUseLegacyLoader,
        CMatrixRegNBanks,
        CMatrixRegNBanks,
        CLoadBridgeResponseChannelCount
    )
    val cStoreBridgeContext = multiOrLegacyBridgeContext(
        "CStore",
        CLoadUseLegacyLoader,
        CMatrixRegNBanks,
        cStoreRequestChannelCount,
        CStoreBridgeResponseChannelCount
    )
    val cmlWrapperContext = VerilogNameHelper.sanitize(s"CML_${cLoadBridgeContext}_${cStoreBridgeContext}")

    val ABMatrixRegs = Seq.tabulate(ABMatrixRegCount)(i => Module(new ABMatrixReg(i))).toVector
    val ADC = Module(new ADataController)
    val AML = Module(new AMLWrapper(amlBridgeContext)).suggestName(s"${amlBridgeContext}_wrapper")
    val BDC = Module(new BDataController)
    val BML = Module(new BMLWrapper(bmlBridgeContext)).suggestName(s"${bmlBridgeContext}_wrapper")

    val CMatrixRegs = Seq.tabulate(CMatrixRegCount)(i => Module(new CMatrixReg(i))).toVector

    val ASMRegs = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Seq.tabulate(2)(i => Module(new ABScaleMatrixReg)).toVector
    ) //双缓冲
    val ASC = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleController)
    )
    val ASL = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleLoader)
    )

    val BSMRegs = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Seq.tabulate(2)(i => Module(new ABScaleMatrixReg)).toVector
    ) //双缓冲
    val BSC = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new BScaleController)
    )
    val BSL = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new BScaleLoader)
    )

    val CDC = Module(new CDataController)
    val CML = Module(new CMLWrapper(cmlWrapperContext)).suggestName(s"${cmlWrapperContext}_wrapper")

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

    ASC.foreach { asc =>
        //ASC的默认输入
        asc.io.FromMatrixRegIO.Data.valid := false.B
        asc.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(asc.io.FromMatrixRegIO.Data.bits)
        asc.io.FromMatrixRegIO.BankAddr.ready := false.B
        asc.io.ConfigInfo <> TaskCtrl.io.ASC_MicroTask_Config.get
        asc.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    }

    //AML的默认输入
    AML.io.ConfigInfo <> TaskCtrl.io.AML_MicroTask_Config
    AML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!AMLUseLegacyLoader) {
        connectWithResponseBridge(
            AML.io.LocalMMUIO,
            MMU.io.ALocalMMUIO,
            ABMatrixRegNBanks,
            AMLResponseChannelCount,
            ABMatrixRegNBanks,
            log2Ceil(ABMatrixRegNBanks),
            log2Ceil(ABMatrixRegBankNEntries),
            AMemoryLoaderReadFromMemoryFIFODepth,
            amlBridgeContext
        )
    } else {
        connectWithResponseArbiter(AML.io.LocalMMUIO, MMU.io.ALocalMMUIO, ABMatrixRegNBanks, amlBridgeContext)
    }

    ASL.foreach { asl =>
        //ASL的默认输入
        asl.io.ConfigInfo <> TaskCtrl.io.ASL_MicroTask_Config.get
        asl.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
        connectWithResponseArbiter(asl.io.LocalMMUIO, MMU.io.ASLocalMMUIO, ABMatrixRegNBanks, "ASL_legacy_1resp")
    }

    //BDC的默认输入
    BDC.io.FromMatrixRegIO.Data.valid := false.B
    BDC.io.FromMatrixRegIO.BankAddr.ready := false.B
    BDC.io.ConfigInfo <> TaskCtrl.io.BDC_MicroTask_Config
    BDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    BSC.foreach { bsc =>
        //BSC的默认输入
        bsc.io.FromMatrixRegIO.Data.valid := false.B
        bsc.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(bsc.io.FromMatrixRegIO.Data.bits)
        bsc.io.FromMatrixRegIO.BankAddr.ready := false.B
        bsc.io.ConfigInfo <> TaskCtrl.io.BSC_MicroTask_Config.get
        bsc.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    }

    //BML的默认输入
    BML.io.ConfigInfo <> TaskCtrl.io.BML_MicroTask_Config
    BML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!BMLUseLegacyLoader) {
        connectWithResponseBridge(
            BML.io.LocalMMUIO,
            MMU.io.BLocalMMUIO,
            ABMatrixRegNBanks,
            BMLResponseChannelCount,
            ABMatrixRegNBanks,
            log2Ceil(ABMatrixRegNBanks),
            log2Ceil(ABMatrixRegBankNEntries),
            BMemoryLoaderReadFromMemoryFIFODepth,
            bmlBridgeContext
        )
    } else {
        connectWithResponseArbiter(BML.io.LocalMMUIO, MMU.io.BLocalMMUIO, ABMatrixRegNBanks, bmlBridgeContext)
    }

    BSL.foreach { bsl =>
        //BSL的默认输入
        bsl.io.ConfigInfo <> TaskCtrl.io.BSL_MicroTask_Config.get
        bsl.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
        connectWithResponseArbiter(bsl.io.LocalMMUIO, MMU.io.BSLocalMMUIO, ABMatrixRegNBanks, "BSL_legacy_1resp")
    }

    if (!cuteMatrixExtension.enableScalingFactor) {
        for (i <- 0 until ABMatrixRegNBanks) {
            MMU.io.ASLocalMMUIO.Request(i).valid := false.B
            MMU.io.ASLocalMMUIO.Request(i).bits := 0.U.asTypeOf(MMU.io.ASLocalMMUIO.Request(i).bits)
            MMU.io.ASLocalMMUIO.Response(i).ready := false.B
            MMU.io.BSLocalMMUIO.Request(i).valid := false.B
            MMU.io.BSLocalMMUIO.Request(i).bits := 0.U.asTypeOf(MMU.io.BSLocalMMUIO.Request(i).bits)
            MMU.io.BSLocalMMUIO.Response(i).ready := false.B
        }
    }

    //CDC的默认输入
    CDC.io.FromMatrixRegIO.ReadResponseData := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadResponseData)
    CDC.io.FromMatrixRegIO.ReadWriteResponse := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadWriteResponse)
    CDC.io.ConfigInfo <> TaskCtrl.io.CDC_MicroTask_Config
    CDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    //CML的默认输入
    CML.io.ConfigInfo <> TaskCtrl.io.CML_MicroTask_Config
    CML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!CLoadUseLegacyLoader) {
        connectWithResponseBridge(
            CML.io.LoadLocalMMUIO,
            MMU.io.CLoadLocalMMUIO,
            CMatrixRegNBanks,
            CLoadBridgeResponseChannelCount,
            CMatrixRegNBanks,
            log2Ceil(CMatrixRegNBanks),
            log2Ceil(CMatrixRegBankNEntries),
            CMemoryLoaderReadFromMemoryFIFODepth,
            cLoadBridgeContext
        )
        connectWithResponseBridge(
            CML.io.StoreLocalMMUIO,
            MMU.io.CStoreLocalMMUIO,
            CMatrixRegNBanks,
            CStoreBridgeResponseChannelCount,
            cStoreRequestChannelCount,
            log2Ceil(CMatrixRegNBanks),
            log2Ceil(CMatrixRegBankNEntries),
            CMemoryLoaderReadFromMemoryFIFODepth,
            cStoreBridgeContext
        )
    } else {
        connectWithResponseArbiter(
            CML.io.LoadLocalMMUIO,
            MMU.io.CLoadLocalMMUIO,
            CMatrixRegNBanks,
            cLoadBridgeContext
        )
        connectWithResponseArbiter(
            CML.io.StoreLocalMMUIO,
            MMU.io.CStoreLocalMMUIO,
            CMatrixRegNBanks,
            cStoreBridgeContext
        )
    }
    CML.io.ToMatrixRegIO.LoadReadWriteResponse := 0.U
    CML.io.ToMatrixRegIO.StoreReadWriteResponse := 0.U
    CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData := 0.U.asTypeOf(CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData)

    MTE.io.VectorA <> ADC.io.VectorA
    MTE.io.VectorB <> BDC.io.VectorB
    MTE.io.ScaleA.zip(ASC).foreach { case (scaleA, asc) =>
        scaleA <> asc.io.ScaleA
    }
    MTE.io.ScaleB.zip(BSC).foreach { case (scaleB, bsc) =>
        scaleB <> bsc.io.ScaleB
    }
    MTE.io.MatrixC <> CDC.io.Matrix_C
    MTE.io.MatrixD <> CDC.io.ResultMatrix_D
    MTE.io.ConfigInfo <> TaskCtrl.io.MTE_MicroTask_Config
    MTE.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    ADC.io.ComputeGo := MTE.io.ComputeGo
    BDC.io.ComputeGo := MTE.io.ComputeGo
    ASC.foreach(_.io.ComputeGo := MTE.io.ComputeGo)
    BSC.foreach(_.io.ComputeGo := MTE.io.ComputeGo)
    CDC.io.ComputeGo := MTE.io.ComputeGo
    
    //后续需要连入CPU的MMU或者IOMMU
    MMU.io.LastLevelCacheTLIO <> io.mmu2llc

    io.ctrl2top <> TaskCtrl.io.ygjkctrl
    val perf = WireInit(0.U.asTypeOf(new CutePerfToCoreIO))
    perf.backendEvents(0) := TaskCtrl.io.perfProbe.ownedWork
    perf.backendEvents(1) := TaskCtrl.io.perfProbe.retire
    perf.backendEvents(2) := TaskCtrl.io.perfProbe.compDone
    perf.backendEvents(3) := TaskCtrl.io.perfProbe.releaseDone
    perf.backendEvents(4) := TaskCtrl.io.perfProbe.mteActive
    perf.backendEvents(5) := TaskCtrl.io.perfProbe.mmaNonfpDone
    perf.backendEvents(6) := TaskCtrl.io.perfProbe.mmaFp16Done
    perf.backendEvents(7) := TaskCtrl.io.perfProbe.mmaBf16Done
    perf.backendEvents(8) := TaskCtrl.io.perfProbe.mmaTf32Done
    perf.memEvents(0) := TaskCtrl.io.perfProbe.loadADone
    perf.memEvents(1) := TaskCtrl.io.perfProbe.loadBDone
    perf.memEvents(2) := TaskCtrl.io.perfProbe.loadCDone
    perf.memEvents(3) := TaskCtrl.io.perfProbe.storeDone
    perf.memEvents(4) := TaskCtrl.io.perfProbe.amlActive
    perf.memEvents(5) := TaskCtrl.io.perfProbe.bmlActive
    perf.memEvents(6) := TaskCtrl.io.perfProbe.cmlLoadActive
    perf.memEvents(7) := TaskCtrl.io.perfProbe.cmlStoreActive
    perf.memEvents(8) := MMU.io.perfProbe.rdReq
    perf.memEvents(9) := MMU.io.perfProbe.wrReq
    perf.memEvents(10) := MMU.io.perfProbe.rd32BReq
    perf.memEvents(11) := MMU.io.perfProbe.wr32BReq
    io.perf := RegNext(perf, 0.U.asTypeOf(new CutePerfToCoreIO))

    //给每个 MatrixReg 的输入进行默认赋值
    
    // AB MatrixReg
    for (i <- 0 until ABMatrixRegCount){
        //DataController的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        //MemoryLoader的请求
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.BankAddr)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.Data)
        ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ByteMask := 0.U.asTypeOf(ABMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ByteMask)
    }

    if (cuteMatrixExtension.enableScalingFactor) {
        // AB Scale Regs
        (ASMRegs.get ++ BSMRegs.get).foreach { reg =>
            reg.io.FromScaleController.BankAddr.valid := false.B
            reg.io.FromScaleController.BankAddr.bits := 0.U.asTypeOf(reg.io.FromScaleController.BankAddr.bits)
            reg.io.FromScaleLoader.BankAddr := 0.U.asTypeOf(reg.io.FromScaleLoader.BankAddr)
            reg.io.FromScaleLoader.Data := 0.U.asTypeOf(reg.io.FromScaleLoader.Data)
        }
    }

    // C MatrixReg
    for (i <- 0 until CMatrixRegCount){
        CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteRequest := 0.U
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.LoadReadWriteRequest := 0.U
        CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.StoreReadWriteRequest := 0.U
    }

    // ============================================
    // A Scale MatrixReg 路由逻辑 (双缓冲)
    // ============================================
    def connectScaleControlToRegs(
        spadId: UInt,
        ScaleCtrlIO: ABScaleControlMatrixRegIO,
        ScaleRegs: Seq[ABScaleMatrixReg]
    ): Unit = {
        // ASC 选择 ScaleRegs，根据 SpadId 选择对应的 MatrixReg
        for (spadIdx <- 0 until ScaleRegs.length) {
            val dest = ScaleRegs(spadIdx).io.FromScaleController
            val ascSel = spadId === spadIdx.U
            when(ascSel) {
                dest.BankAddr.valid := ScaleCtrlIO.BankAddr.valid
                dest.BankAddr.bits := ScaleCtrlIO.BankAddr.bits
            }.otherwise {
                dest.BankAddr.valid := false.B
                dest.BankAddr.bits := DontCare
            }
        }

        // ASC 接收 ScaleRegs 返回的数据
        val sels = ScaleRegs.indices.map(spadId === _.U)
        ScaleCtrlIO.BankAddr.ready := Mux1H(sels zip ScaleRegs.map(_.io.FromScaleController.BankAddr.ready))
        ScaleCtrlIO.Data.valid     := Mux1H(sels zip ScaleRegs.map(_.io.FromScaleController.Data.valid))
        ScaleCtrlIO.Data.bits      := Mux1H(sels zip ScaleRegs.map(_.io.FromScaleController.Data.bits))
    }

    def connectScaleLoaderToRegs(
        spadId: UInt,
        ScaleLoaderIO: ABScaleLoaderMatrixRegIO,
        ScaleRegs: Seq[ABScaleMatrixReg]
    ): Unit = {
        // ASL 选择 ScaleRegs，根据 SpadId 选择对应的 MatrixReg
        for (spadIdx <- 0 until ScaleRegs.length) {
            val dest = ScaleRegs(spadIdx).io.FromScaleLoader
            val aslSel = spadId === spadIdx.U
            when(aslSel) {
                dest.BankAddr := ScaleLoaderIO.BankAddr
                dest.Data := ScaleLoaderIO.Data
            }.otherwise {
                dest.BankAddr.valid := false.B
                dest.BankAddr.bits := DontCare
                dest.Data.valid := false.B
                dest.Data.bits := DontCare
            }
        }
    }

    ASC.zip(ASL).zip(ASMRegs).foreach { case ((asc, asl), asmRegs) =>
        connectScaleControlToRegs(asc.io.SpadId, asc.io.FromMatrixRegIO, asmRegs)
        connectScaleLoaderToRegs(asl.io.SpadId, asl.io.ToMatrixRegIO, asmRegs)
    }

    BSC.zip(BSL).zip(BSMRegs).foreach { case ((bsc, bsl), bsmRegs) =>
        connectScaleControlToRegs(bsc.io.SpadId, bsc.io.FromMatrixRegIO, bsmRegs)
        connectScaleLoaderToRegs(bsl.io.SpadId, bsl.io.ToMatrixRegIO, bsmRegs)
    }

    def disableABLoaderPort(dest: ABMemoryLoaderMatrixRegIO): Unit = {
        for (b <- 0 until ABMatrixRegNBanks) {
            dest.BankAddr(b).valid := false.B
            dest.BankAddr(b).bits := DontCare
            dest.Data(b).valid := false.B
            dest.Data(b).bits := DontCare
            dest.ByteMask(b).valid := false.B
            dest.ByteMask(b).bits := Fill(ABMatrixRegEntryByteSize, true.B)
        }
    }

    def copyABLoaderPort(dest: ABMemoryLoaderMatrixRegIO, src: ABMemoryLoaderMatrixRegIO): Unit = {
        for (b <- 0 until ABMatrixRegNBanks) {
            dest.BankAddr(b).valid := src.BankAddr(b).valid
            dest.BankAddr(b).bits := src.BankAddr(b).bits
            dest.Data(b).valid := src.Data(b).valid
            dest.Data(b).bits := src.Data(b).bits
            dest.ByteMask(b).valid := src.ByteMask(b).valid
            dest.ByteMask(b).bits := src.ByteMask(b).bits
        }
    }
    
    for (regIdx <- 0 until ABMatrixRegCount) {
        val dest = ABMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        val amlSel = AML.io.MatrixRegId === regIdx.U
        val bmlSel = BML.io.MatrixRegId === regIdx.U
        val amlActive = AML.io.ToMatrixRegIO.active
        val bmlActive = BML.io.ToMatrixRegIO.active
        dest.active := DontCare
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
    val adcReadyChoices = (0 until ABMatrixRegCount).map(i => adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.ready)
    ADC.io.FromMatrixRegIO.BankAddr.ready := Mux1H(adcReadyChoices)
    val adcDataValidChoices = (0 until ABMatrixRegCount).map(i => adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.valid)
    ADC.io.FromMatrixRegIO.Data.valid := Mux1H(adcDataValidChoices)
    for (bank <- 0 until ABMatrixRegNBanks) {
        val adcDataBitsChoices = (0 until ABMatrixRegCount).map(i =>
            adcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)
        )
        ADC.io.FromMatrixRegIO.Data.bits(bank) := Mux1H(adcDataBitsChoices)
    }

    val bdcSelVec = VecInit((0 until ABMatrixRegCount).map(i => BDC.io.MatrixRegId === i.U))
    val bdcReadyChoices = (0 until ABMatrixRegCount).map(i => bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.ready)
    BDC.io.FromMatrixRegIO.BankAddr.ready := Mux1H(bdcReadyChoices)
    val bdcDataValidChoices = (0 until ABMatrixRegCount).map(i => bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.valid)
    BDC.io.FromMatrixRegIO.Data.valid := Mux1H(bdcDataValidChoices)
    for (bank <- 0 until ABMatrixRegNBanks) {
        val bdcDataBitsChoices = (0 until ABMatrixRegCount).map(i =>
            bdcSelVec(i) -> ABMatrixRegs(i).io.MatrixRegIO.FromDataController.Data.bits(bank)
        )
        BDC.io.FromMatrixRegIO.Data.bits(bank) := Mux1H(bdcDataBitsChoices)
    }

    for (regIdx <- 0 until CMatrixRegCount) {
        val dest = CMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        val loadSel = CML.io.LoadMatrixRegId === regIdx.U
        val storeSel = CML.io.StoreMatrixRegId === regIdx.U
        val loadWriteReq = CML.io.ToMatrixRegIO.LoadReadWriteRequest(MatrixRegTaskType.WriteFromMemoryLoaderIndex)
        val storeReadReq = CML.io.ToMatrixRegIO.StoreReadWriteRequest(MatrixRegTaskType.ReadFromMemoryLoaderIndex)

        when(loadSel && storeSel && loadWriteReq && storeReadReq) {
            assert(false.B, cf"[CUTETop] CML load/store target the same C MatrixReg($regIdx) in one cycle")
        }

        for (bank <- 0 until CMatrixRegNBanks) {
            dest.ReadRequestToMatrixReg.BankAddr(bank).valid := storeSel && CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).valid
            dest.WriteRequestToMatrixReg.BankAddr(bank).valid := loadSel && CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(bank).valid
            dest.WriteRequestToMatrixReg.Data(bank).valid := loadSel && CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(bank).valid
            dest.WriteRequestToMatrixReg.ByteMask(bank).valid := loadSel && CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(bank).valid
            dest.ReadRequestToMatrixReg.BankAddr(bank).bits := CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank).bits
            dest.WriteRequestToMatrixReg.BankAddr(bank).bits := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.BankAddr(bank).bits
            dest.WriteRequestToMatrixReg.Data(bank).bits := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.Data(bank).bits
            dest.WriteRequestToMatrixReg.ByteMask(bank).bits := CML.io.ToMatrixRegIO.WriteRequestToMatrixReg.ByteMask(bank).bits
        }

        val loadReqForThisReg = Mux(loadSel, CML.io.ToMatrixRegIO.LoadReadWriteRequest, 0.U)
        val storeReqForThisReg = Mux(storeSel, CML.io.ToMatrixRegIO.StoreReadWriteRequest, 0.U)
        dest.LoadReadWriteRequest := loadReqForThisReg
        dest.StoreReadWriteRequest := storeReqForThisReg
    }

    val cmlLoadSelVec = VecInit((0 until CMatrixRegCount).map(i => CML.io.LoadMatrixRegId === i.U))
    val cmlStoreSelVec = VecInit((0 until CMatrixRegCount).map(i => CML.io.StoreMatrixRegId === i.U))
    for (bank <- 0 until CMatrixRegNBanks) {
        val readRespValidChoices = (0 until CMatrixRegCount).map(i =>
            cmlStoreSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).valid
        )
        val readRespBitsChoices = (0 until CMatrixRegCount).map(i =>
            cmlStoreSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank).bits
        )
        CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid := Mux1H(readRespValidChoices)
        CML.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits := Mux1H(readRespBitsChoices)
    }

    val cmlLoadRwRespChoices = (0 until CMatrixRegCount).map(i => cmlLoadSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.LoadReadWriteResponse)
    val cmlStoreRwRespChoices = (0 until CMatrixRegCount).map(i => cmlStoreSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.StoreReadWriteResponse)
    CML.io.ToMatrixRegIO.LoadReadWriteResponse := Mux1H(cmlLoadRwRespChoices)
    CML.io.ToMatrixRegIO.StoreReadWriteResponse := Mux1H(cmlStoreRwRespChoices)

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
    for (bank <- 0 until CMatrixRegNBanks) {
        val cdcReadRespValidChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) ->
            CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).valid
        )
        val cdcReadRespBitsChoices = (0 until CMatrixRegCount).map(i => cdcSelVec(i) ->
            CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).bits
        )
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).valid := Mux1H(cdcReadRespValidChoices)
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).bits := Mux1H(cdcReadRespBitsChoices)
    }
    val cdcRwRespChoices = (0 until CMatrixRegCount).map(i =>
        cdcSelVec(i) -> CMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteResponse
      )
    CDC.io.FromMatrixRegIO.ReadWriteResponse := Mux1H(cdcRwRespChoices)
}
