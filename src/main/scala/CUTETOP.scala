
package cute

import chisel3._
import chisel3.util._
import difftest._
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
        val FromScaleRegIO = Option.when(ExperimentalMxfpConvertLoad)(
            Flipped(new ABScaleControlMatrixRegIO))
        val ScaleBankId = Output(UInt(ABScaleRegIdWidth.W))
        val ScaleReadActive = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(
            Decoupled(new MxfpGeneratedScale))
    })
    io.ScaleBankId := 0.U
    io.ScaleReadActive := false.B

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
        io.FromScaleRegIO.foreach(_ <> inner.io.FromScaleRegIO.get)
        io.ScaleBankId := inner.io.ScaleBankId
        io.ScaleReadActive := inner.io.ScaleReadActive
        io.GeneratedScale.zip(inner.io.GeneratedScale).foreach {
            case (outer, source) => outer <> source
        }
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
        val FromScaleRegIO = Option.when(ExperimentalMxfpConvertLoad)(
            Flipped(new ABScaleControlMatrixRegIO))
        val ScaleBankId = Output(UInt(ABScaleRegIdWidth.W))
        val ScaleReadActive = Output(Bool())
        val GeneratedScale = Option.when(ExperimentalMxfpDynamicQuant)(
            Decoupled(new MxfpGeneratedScale))
    })
    io.ScaleBankId := 0.U
    io.ScaleReadActive := false.B

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
        bToAConfig.ConvertF32ToMx := io.ConfigInfo.ConvertF32ToMx
        bToAConfig.ConvertComputeType := io.ConfigInfo.ConvertComputeType
        bToAConfig.ConvertKElements := io.ConfigInfo.ConvertKElements
        bToAConfig.ConvertScaleBankId := io.ConfigInfo.ConvertScaleBankId
        bToAConfig.MicroTaskValid := io.ConfigInfo.MicroTaskValid
        bToAConfig.MicroTaskEndReady := io.ConfigInfo.MicroTaskEndReady
        bToAConfig.LoadTaskInfo.Is_FullLoad := true.B
        bToAConfig.LoadTaskInfo.Is_ZeroLoad := false.B
        bToAConfig.LoadTaskInfo.Is_RepeatRowLoad := false.B
        bToAConfig.pc.foreach(_ := io.ConfigInfo.pc.getOrElse(0xDEAD1234.U))
        bToAConfig.coreid.foreach(_ := io.ConfigInfo.coreid.getOrElse(0xDEAD5678.U))
        bToAConfig.retireId.foreach(_ := io.ConfigInfo.retireId.get)
        bToAConfig.epoch.foreach(_ := io.ConfigInfo.epoch.get)

        io.ConfigInfo.MicroTaskReady := bToAConfig.MicroTaskReady
        io.ConfigInfo.MicroTaskEndValid := bToAConfig.MicroTaskEndValid
        io.ConfigInfo.MicroTaskFlags := bToAConfig.MicroTaskFlags

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
        io.FromScaleRegIO.foreach(_ <> inner.io.FromScaleRegIO.get)
        io.ScaleBankId := inner.io.ScaleBankId
        io.ScaleReadActive := inner.io.ScaleReadActive
        io.GeneratedScale.zip(inner.io.GeneratedScale).foreach {
            case (outer, source) => outer <> source
        }
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

object CUTEV2Top {
    private[cute] def connectScaleControlToRegs(
        spadId: UInt,
        ScaleCtrlIO: ABScaleControlMatrixRegIO,
        ScaleRegs: Seq[ABScaleMatrixReg],
        convertIO: Option[ABScaleControlMatrixRegIO],
        convertSpadId: UInt,
        convertActive: Bool
    )(implicit clock: Clock, reset: Reset): Unit = {
        // TC serializes same-side task readers. Latch BOTH response owner and
        // bank on request fire: the SRAM response is Valid-only, not readyable.
        val selectedSpad = Mux(convertActive, convertSpadId, spadId)
        val requestValid = Mux(convertActive, convertIO.map(_.BankAddr.valid).getOrElse(false.B),
            ScaleCtrlIO.BankAddr.valid)
        val requestAddr = Mux(convertActive, convertIO.map(_.BankAddr.bits).getOrElse(0.U),
            ScaleCtrlIO.BankAddr.bits)
        for (spadIdx <- 0 until ScaleRegs.length) {
            val dest = ScaleRegs(spadIdx).io.FromScaleController
            when(selectedSpad === spadIdx.U) {
                dest.BankAddr.valid := requestValid
                dest.BankAddr.bits := requestAddr
            }.otherwise {
                dest.BankAddr.valid := false.B
                dest.BankAddr.bits := DontCare
            }
        }

        val sels = ScaleRegs.indices.map(selectedSpad === _.U)
        val ready = Mux1H(sels zip ScaleRegs.map(_.io.FromScaleController.BankAddr.ready))
        val requestFire = requestValid && ready
        val responseSpad = RegEnable(selectedSpad, 0.U, requestFire)
        val responseConvert = RegEnable(convertActive, false.B, requestFire)
        val responseSels = ScaleRegs.indices.map(responseSpad === _.U)
        val responseValid = Mux1H(responseSels zip ScaleRegs.map(_.io.FromScaleController.Data.valid))
        val responseData = Mux1H(responseSels zip ScaleRegs.map(_.io.FromScaleController.Data.bits))
        ScaleCtrlIO.BankAddr.ready := !convertActive && ready
        ScaleCtrlIO.Data.valid := responseValid && !responseConvert
        ScaleCtrlIO.Data.bits := responseData
        convertIO.foreach { port =>
            port.BankAddr.ready := convertActive && ready
            port.Data.valid := responseValid && responseConvert
            port.Data.bits := responseData
            when(convertActive) {
                assert(!ScaleCtrlIO.BankAddr.valid, "SF port has two simultaneous task readers")
            }
        }
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
    private val responseBridgeIdles = scala.collection.mutable.ArrayBuffer[Bool]()

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

    def connectRequestChannels(
        loaderReq: Vec[DecoupledIO[MMURequestIO]],
        loaderCoherentId: Valid[UInt],
        loaderNonCoherentId: Valid[UInt],
        mmuReq: Vec[DecoupledIO[MMURequestIO]],
        mmuCoherentId: Valid[UInt],
        mmuNonCoherentId: Valid[UInt],
        channelCount: Int
    ): Unit = {
        for (i <- 0 until channelCount) {
            mmuReq(i) <> loaderReq(i)
        }
        loaderCoherentId := mmuCoherentId
        loaderNonCoherentId := mmuNonCoherentId
    }

    def connectRequestChannelsWithGroupedArbiter(
        loaderReq: Vec[DecoupledIO[MMURequestIO]],
        loaderCoherentId: Valid[UInt],
        loaderNonCoherentId: Valid[UInt],
        mmuReq: Vec[DecoupledIO[MMURequestIO]],
        mmuCoherentId: Valid[UInt],
        mmuNonCoherentId: Valid[UInt],
        channelCount: Int,
        reqChannelCount: Int,
        contextName: String
    ): Unit = {
        require(reqChannelCount >= 1 && reqChannelCount <= channelCount, s"reqChannelCount ($reqChannelCount) must be within [1, $channelCount]")

        val nameContext = VerilogNameHelper.sanitize(contextName)
        loaderCoherentId := mmuCoherentId
        loaderNonCoherentId := mmuNonCoherentId

        if (reqChannelCount == channelCount) {
            for (i <- 0 until channelCount) {
                mmuReq(i) <> loaderReq(i)
            }
        } else {
            require(channelCount % reqChannelCount == 0, s"channelCount ($channelCount) must be divisible by reqChannelCount ($reqChannelCount)")

            for (group <- 0 until reqChannelCount) {
                val banks = ResponseChannelHelper.banksInGroup(group, reqChannelCount, channelCount)
                val arb = Module(new Arbiter(new MMURequestIO, banks.size))
                    .suggestName(s"${nameContext}_req_group${group}_arb")

                for ((bank, idx) <- banks.zipWithIndex) {
                    arb.io.in(idx) <> loaderReq(bank)
                }

                mmuReq(group).valid := arb.io.out.valid
                mmuReq(group).bits := arb.io.out.bits
                arb.io.out.ready := mmuReq(group).ready
            }

            for (group <- reqChannelCount until channelCount) {
                mmuReq(group).valid := false.B
                mmuReq(group).bits := DontCare
            }
        }
    }

    def connectLoaderRequestsWithGroupedArbiter(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int,
        reqChannelCount: Int,
        contextName: String
    ): Unit = {
        connectRequestChannelsWithGroupedArbiter(
            loader.Request,
            loader.ConherentRequsetSourceID,
            loader.nonConherentRequsetSourceID,
            mmu.Request,
            mmu.ConherentRequsetSourceID,
            mmu.nonConherentRequsetSourceID,
            channelCount,
            reqChannelCount,
            contextName
        )
    }

    def connectWithResponseBridge(
        loader: LocalMMUIO,
        mmu: LocalMMUIO,
        channelCount: Int,
        respChannelCount: Int,
        reqChannelCount: Int,
        bankIdWidth: Int,
        bankIdOffset: Int,
        queueDepth: Int,
        hasDataPayload: Boolean,
        contextName: String
    ): Unit = {
        val nameContext = VerilogNameHelper.sanitize(contextName)
        connectLoaderRequestsWithGroupedArbiter(loader, mmu, channelCount, reqChannelCount, nameContext)

        val bridge = Module(new ResponseChannelBridge(
            inputChannelCount = channelCount,
            respChannelCount = respChannelCount,
            bankCount = channelCount,
            queueDepth = queueDepth,
            dataWidth = outsideDataWidth,
            sourceIdWidth = 64,
            bankIdWidth = bankIdWidth,
            bankIdOffset = bankIdOffset,
            hasDataPayload = hasDataPayload,
            debugEnable = YJPDebugEnable,
            contextName = nameContext
        )).suggestName(s"${nameContext}_response_bridge")
        bridge.io.timeStamp := time_stamp
        responseBridgeIdles += bridge.io.idle

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
      Seq.tabulate(ABScaleRegCount)(i => Module(new ABScaleMatrixReg)).toVector
    )
    val ASC = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleController)
    )
    val ASL = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleLoader)
    )

    val BSMRegs = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Seq.tabulate(ABScaleRegCount)(i => Module(new ABScaleMatrixReg)).toVector
    )
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
            ResponseBridgeQueueDepth,
            true,
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
            ResponseBridgeQueueDepth,
            true,
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
            ResponseBridgeQueueDepth,
            true,
            cLoadBridgeContext
        )
        if (CStoreDirectAckCountMode) {
            connectRequestChannelsWithGroupedArbiter(
                CML.io.StoreLocalMMUIO.Request,
                CML.io.StoreLocalMMUIO.ConherentRequsetSourceID,
                CML.io.StoreLocalMMUIO.nonConherentRequsetSourceID,
                MMU.io.CStoreLocalMMUIO.Request,
                MMU.io.CStoreLocalMMUIO.ConherentRequsetSourceID,
                MMU.io.CStoreLocalMMUIO.nonConherentRequsetSourceID,
                CMatrixRegNBanks,
                cStoreRequestChannelCount,
                cStoreBridgeContext
            )
            for (i <- 0 until CMatrixRegNBanks) {
                CML.io.StoreLocalMMUIO.Response(i).valid := MMU.io.CStoreLocalMMUIO.Response(i).valid
                CML.io.StoreLocalMMUIO.Response(i).bits := MMU.io.CStoreLocalMMUIO.Response(i).bits
                MMU.io.CStoreLocalMMUIO.Response(i).ready := CML.io.StoreLocalMMUIO.Response(i).ready
            }
        } else {
            connectWithResponseBridge(
                CML.io.StoreLocalMMUIO,
                MMU.io.CStoreLocalMMUIO,
                CMatrixRegNBanks,
                CStoreBridgeResponseChannelCount,
                cStoreRequestChannelCount,
                log2Ceil(CMatrixRegNBanks),
                log2Ceil(CMatrixRegBankNEntries),
                ResponseBridgeQueueDepth,
                false,
                cStoreBridgeContext
            )
        }
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
        MTE.io.ScaleALiveK.get := asc.io.LiveK
    }
    MTE.io.ScaleB.zip(BSC).foreach { case (scaleB, bsc) =>
        scaleB <> bsc.io.ScaleB
        MTE.io.ScaleBLiveK.get := bsc.io.LiveK
    }
    MTE.io.MatrixC <> CDC.io.Matrix_C
    MTE.io.MatrixD <> CDC.io.ResultMatrix_D
    CDC.io.ResultFlags_D := MTE.io.DFlags
    MTE.io.ConfigInfo <> TaskCtrl.io.MTE_MicroTask_Config
    MTE.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    ADC.io.ComputeGo := MTE.io.ComputeGo
    BDC.io.ComputeGo := MTE.io.ComputeGo
    ASC.foreach(_.io.ComputeGo := MTE.io.ComputeGo)
    BSC.foreach(_.io.ComputeGo := MTE.io.ComputeGo)
    CDC.io.ComputeGo := MTE.io.ComputeGo
    // One request grant for the complete A/B/C/scale tuple. Consumers keep
    // their existing ComputeGo/ready handshake for responses already in flight.
    ADC.io.ReadIssueEnable := CDC.io.ReadIssueReady
    BDC.io.ReadIssueEnable := CDC.io.ReadIssueReady
    ASC.foreach(_.io.ReadIssueEnable := CDC.io.ReadIssueReady)
    BSC.foreach(_.io.ReadIssueEnable := CDC.io.ReadIssueReady)
    CDC.io.ReadIssueEnable := CDC.io.ReadIssueReady
    
    //后续需要连入CPU的MMU或者IOMMU
    MMU.io.LastLevelCacheTLIO <> io.mmu2llc

    io.ctrl2top <> TaskCtrl.io.ygjkctrl
    TaskCtrl.io.downstreamIdle := MMU.io.idle && MTE.io.idle &&
        ADC.io.idle && BDC.io.idle && CDC.io.idle &&
        ASC.map(_.io.idle).getOrElse(true.B) && BSC.map(_.io.idle).getOrElse(true.B) &&
        ASL.map(_.io.idle).getOrElse(true.B) && BSL.map(_.io.idle).getOrElse(true.B) &&
        responseBridgeIdles.foldLeft(true.B)(_ && _) &&
        ASMRegs.map(_.map(r => !r.io.FromScaleController.Data.valid &&
            !r.io.FromScaleController.BankAddr.valid && !r.io.FromScaleLoader.BankAddr.valid &&
            !r.io.FromScaleLoader.Data.valid).reduce(_ && _)).getOrElse(true.B) &&
        BSMRegs.map(_.map(r => !r.io.FromScaleController.Data.valid &&
            !r.io.FromScaleController.BankAddr.valid && !r.io.FromScaleLoader.BankAddr.valid &&
            !r.io.FromScaleLoader.Data.valid).reduce(_ && _)).getOrElse(true.B)
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
    // A Scale MatrixReg 路由逻辑（四个架构 bank）
    // ============================================
    def connectScaleControlToRegs(
        spadId: UInt,
        ScaleCtrlIO: ABScaleControlMatrixRegIO,
        ScaleRegs: Seq[ABScaleMatrixReg],
        convertIO: Option[ABScaleControlMatrixRegIO],
        convertSpadId: UInt,
        convertActive: Bool
    ): Unit = CUTEV2Top.connectScaleControlToRegs(
        spadId, ScaleCtrlIO, ScaleRegs, convertIO, convertSpadId, convertActive)(clock, reset)

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
        asl.io.GeneratedScale.zip(AML.io.GeneratedScale).foreach {
            case (sink, source) => sink <> source
        }
        connectScaleControlToRegs(asc.io.SpadId, asc.io.FromMatrixRegIO, asmRegs,
            AML.io.FromScaleRegIO, AML.io.ScaleBankId, AML.io.ScaleReadActive)
        connectScaleLoaderToRegs(asl.io.SpadId, asl.io.ToMatrixRegIO, asmRegs)
        when(asl.io.ToMatrixRegIO.BankAddr.valid && asc.io.FromMatrixRegIO.BankAddr.valid) {
            assert(asl.io.SpadId =/= asc.io.SpadId, "CUTE: A scale bank must not be loaded while owned by a reader")
        }
        AML.io.FromScaleRegIO.foreach { port =>
            when(asl.io.ToMatrixRegIO.BankAddr.valid && port.BankAddr.valid) {
                assert(asl.io.SpadId =/= AML.io.ScaleBankId,
                    "CUTE: A scale writer overlaps a converting-load reader")
            }
        }
    }

    BSC.zip(BSL).zip(BSMRegs).foreach { case ((bsc, bsl), bsmRegs) =>
        bsl.io.GeneratedScale.zip(BML.io.GeneratedScale).foreach {
            case (sink, source) => sink <> source
        }
        connectScaleControlToRegs(bsc.io.SpadId, bsc.io.FromMatrixRegIO, bsmRegs,
            BML.io.FromScaleRegIO, BML.io.ScaleBankId, BML.io.ScaleReadActive)
        connectScaleLoaderToRegs(bsl.io.SpadId, bsl.io.ToMatrixRegIO, bsmRegs)
        when(bsl.io.ToMatrixRegIO.BankAddr.valid && bsc.io.FromMatrixRegIO.BankAddr.valid) {
            assert(bsl.io.SpadId =/= bsc.io.SpadId, "CUTE: B scale bank must not be loaded while owned by a reader")
        }
        BML.io.FromScaleRegIO.foreach { port =>
            when(bsl.io.ToMatrixRegIO.BankAddr.valid && port.BankAddr.valid) {
                assert(bsl.io.SpadId =/= BML.io.ScaleBankId,
                    "CUTE: B scale writer overlaps a converting-load reader")
            }
        }
    }

    if (EnableDifftest && cuteMatrixExtension.enableScalingFactor) {
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_SCALE_BEAT_BYTES", outsideDataWidthByte)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_SCALE_VECTOR_DATA_BYTES", ReduceWidthByte)
        DifftestModule.addCppMacro("CONFIG_DIFF_AMU_SCALE_BANK_BYTES",
            ABScaleBankNEntries * outsideDataWidthByte)
        val eventBankBytes = DiffAmuFinishWordsPerBank * 8
        require(outsideDataWidth % 64 == 0)
        require(outsideDataWidthByte <= ABMatrixRegNBanks * eventBankBytes,
            "scale observation must fit the existing finish payload")
        // Existing lanes0..5 remain AML/BML/C-load/CDC/C-store/Release.
        // New lanes follow them; canonical Difftest generation counts width.
        Seq(ASMRegs.get, BSMRegs.get).zipWithIndex.foreach { case (regs, side) =>
            val owner = TaskCtrl.io.ownedScaleTrace.get(side)
            val writes = regs.map(r => r.io.FromScaleLoader.BankAddr.valid &&
                r.io.FromScaleLoader.Data.valid)
            val write = VecInit(writes).asUInt.orR
            val bank = OHToUInt(VecInit(writes).asUInt)
            val addr = Mux1H(writes zip regs.map(_.io.FromScaleLoader.BankAddr.bits))
            val data = Mux1H(writes zip regs.map(_.io.FromScaleLoader.Data.bits.asUInt))
            when(write) {
                assert(PopCount(VecInit(writes)) === 1.U && owner.busy,
                    "scale destination write without a unique owned loader")
                assert(bank === owner.bankId, "scale destination differs from FU-owned bank")
            }
            val event = DifftestModule(
                new DiffAmuFinishEvent(ABMatrixRegNBanks, DiffAmuFinishWordsPerBank, DiffAmuFinishAddrBits),
                delay = 0, dontCare = true)
            event.clearScaleTrace()
            event.coreid := owner.coreid
            event.index := (6 + side).U
            event.valid := write || owner.complete
            event.pc := owner.pc
            event.retireId := owner.retireId
            event.epoch := owner.epoch
            event.kind := (if (side == 0) AmuFinishKind.ScaleA else AmuFinishKind.ScaleB).U
            event.computeType := owner.computeType
            val actualBank = Mux(write, bank, owner.bankId)
            (if (side == 0) event.scaleRegA else event.scaleRegB) := owner.regId
            (if (side == 0) event.scaleBankA else event.scaleBankB) := actualBank
            (if (side == 0) event.scaleGenA else event.scaleGenB) := owner.generation
            // This bit means publication on the current owned completion edge.
            (if (side == 0) event.scaleValidA else event.scaleValidB) :=
                owner.complete && owner.completionPublishes.getOrElse(true.B)
            (if (side == 0) event.scaleRowsA else event.scaleRowsB) := owner.rows
            (if (side == 0) event.scaleKVectorsA else event.scaleKVectorsB) := owner.kVectors
            (if (side == 0) event.scaleBlocksA else event.scaleBlocksB) := owner.blocks
            (if (side == 0) event.scaleRowPitchA else event.scaleRowPitchB) := owner.rowPitch
            val liveBytes = owner.rows * owner.blocks
            val flat = data.pad(ABMatrixRegNBanks * eventBankBytes * 8)
            for (b <- 0 until ABMatrixRegNBanks) {
                event.bankValid(b) := write && (b * eventBankBytes < outsideDataWidthByte).B
                event.bankAddr(b) := addr
                event.bankMask(b) := VecInit((0 until eventBankBytes).map(k =>
                    write && (b * eventBankBytes + k < outsideDataWidthByte).B &&
                    addr * outsideDataWidthByte.U + (b * eventBankBytes + k).U < liveBytes)).asUInt
                for (w <- 0 until DiffAmuFinishWordsPerBank) {
                    val lo = (b * DiffAmuFinishWordsPerBank + w) * 64
                    event.data(b * DiffAmuFinishWordsPerBank + w) := flat(lo + 63, lo)
                }
            }
            event.finish := owner.complete
        }
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
