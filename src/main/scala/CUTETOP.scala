
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xscache.coupledL2.prefetch.MatrixPrefetchControl
// import boom.exu.ygjk._
// import scala.collection.parallel.Task

class CUTETopIO()(implicit p: Parameters) extends CuteBundle{
    val mmu2llc = Flipped(new MMU2TLIO)
    val ctrl2top = Flipped(new YGJKControl)
    val perf = Output(new CutePerfToCoreIO)
    val matrixPrefetch = Option.when(EnableMatrixPrefetch)(Output(new MatrixPrefetchControl))
}

/** Route ADC/BDC reads to the selected TR matrix register. */
class TrMatrixRegReadPortRouter(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val clients = Vec(2, new TrDataControlMatrixRegIO)
        val regIds = Input(Vec(2, UInt(TrMatrixRegIdWidth.W)))
        val registers = Flipped(Vec(TrMatrixRegCount, new TrDataControlMatrixRegIO))
    })

    io.clients.foreach { client =>
        client.BankAddr.ready := false.B
        client.Data.valid := false.B
        client.Data.bits := 0.U.asTypeOf(client.Data.bits)
    }
    for (regIdx <- 0 until TrMatrixRegCount) {
        val registerPort = io.registers(regIdx)
        val selected = VecInit((0 until 2).map { clientIdx =>
            io.clients(clientIdx).BankAddr.valid && io.regIds(clientIdx) === regIdx.U
        })
        assert(PopCount(selected) <= 1.U,
            "TR MatrixReg has one read port; readers must be excluded by task scheduling")
        registerPort.BankAddr.valid := selected.asUInt.orR
        registerPort.BankAddr.bits := Mux1H(selected, io.clients.map(_.BankAddr.bits))
        for (clientIdx <- 0 until 2) {
            when (selected(clientIdx)) {
                io.clients(clientIdx).BankAddr.ready := registerPort.BankAddr.ready
            }
            val owner = RegNext(selected(clientIdx) && registerPort.BankAddr.fire, false.B)
            when (owner) {
                io.clients(clientIdx).Data := registerPort.Data
                assert(registerPort.Data.valid,
                    "TR MatrixReg read response must return after one cycle")
            }
        }
    }
}

class TrLoader0Wrapper(contextName: String = "trLoader0")(implicit p: Parameters)
    extends CuteModule with HasTransposeLoadEngine {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (TrLoader0UseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
        val perfProbe = Output(new TrLoadPerfProbe)
    })

    if (!TrLoader0UseLegacyLoader) {
        val inner = Module(new MultiChannelTrLoader("trLoader0", nameContext, emitDifftest = false))
            .suggestName(s"${nameContext}_multi_loader")
        connectTransposeLoadEngine(io.ConfigInfo, io.LocalMMUIO, io.ToMatrixRegIO, io.MatrixRegId,
          inner.io.ConfigInfo, inner.io.LocalMMUIO, inner.io.ToMatrixRegIO, inner.io.MatrixRegId,
          legacy = false, diffIndex = 0)
        inner.io.DebugInfo <> io.DebugInfo
        io.perfProbe := inner.io.perfProbe
    } else {
        val inner = Module(new TrLoader0Legacy(emitDifftest = false)).suggestName(s"${nameContext}_legacy_loader")
        connectTransposeLoadEngine(io.ConfigInfo, io.LocalMMUIO, io.ToMatrixRegIO, io.MatrixRegId,
          inner.io.ConfigInfo, inner.io.LocalMMUIO, inner.io.ToMatrixRegIO, inner.io.MatrixRegId,
          legacy = true, diffIndex = 0)
        inner.io.DebugInfo <> io.DebugInfo
        io.perfProbe := 0.U.asTypeOf(new TrLoadPerfProbe)
    }
}

class TrLoader1Wrapper(contextName: String = "trLoader1")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (TrLoader1UseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new TrLoadMatrixRegIO)
        val ConfigInfo = Flipped(new TrLoadTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(TrMatrixRegIdWidth.W))
        val perfProbe = Output(new TrLoadPerfProbe)
    })

    if (!TrLoader1UseLegacyLoader) {
        val inner = Module(new MultiChannelTrLoader("trLoader1", nameContext))
            .suggestName(s"${nameContext}_multi_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
        io.perfProbe := inner.io.perfProbe
    } else {
        val inner = Module(new TrLoader1Legacy).suggestName(s"${nameContext}_legacy_loader")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LocalMMUIO <> io.LocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.MatrixRegId := inner.io.MatrixRegId
        io.perfProbe := 0.U.asTypeOf(new TrLoadPerfProbe)
    }
}

class AccMemoryWrapper(contextName: String = "accMemory")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (AccLoader0UseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new Bundle {
        val ToMatrixRegIO = Flipped(new AccMemoryMatrixRegIO)
        val ConfigInfo = Flipped(new AccMemoryTaskConfigIO)
        val LoadLocalMMUIO = Flipped(new LocalMMUIO)
        val StoreLocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val LoadMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val StoreMatrixRegId = Output(UInt(AccMatrixRegIdWidth.W))
        val perfProbe = Output(new AccLoadPerfProbe)
    })

    if (!AccLoader0UseLegacyLoader) {
        val inner = Module(new MultiChannelAccMemory).suggestName(s"${nameContext}_multi_memory")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LoadLocalMMUIO <> io.LoadLocalMMUIO
        inner.io.StoreLocalMMUIO <> io.StoreLocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.LoadMatrixRegId := inner.io.LoadMatrixRegId
        io.StoreMatrixRegId := inner.io.StoreMatrixRegId
        io.perfProbe := inner.io.perfProbe
    } else {
        val inner = Module(new AccMemoryLegacy).suggestName(s"${nameContext}_legacy_memory")
        inner.io.ToMatrixRegIO <> io.ToMatrixRegIO
        inner.io.ConfigInfo <> io.ConfigInfo
        inner.io.LoadLocalMMUIO <> io.LoadLocalMMUIO
        inner.io.StoreLocalMMUIO <> io.StoreLocalMMUIO
        inner.io.DebugInfo <> io.DebugInfo
        io.LoadMatrixRegId := inner.io.LoadMatrixRegId
        io.StoreMatrixRegId := inner.io.StoreMatrixRegId
        io.perfProbe := 0.U.asTypeOf(new AccLoadPerfProbe)
    }
}

class TrStorer0Wrapper(contextName: String = "trStorer0")(implicit p: Parameters) extends CuteModule {
    private val nameContext = VerilogNameHelper.sanitize(contextName)
    override def desiredName: String = s"${nameContext}Wrapper_${if (TrStorer0UseLegacyLoader) "Legacy" else "Multi"}"

    val io = IO(new TrStoreIO)
    if (TrStorer0UseLegacyLoader) {
        val inner = Module(new TrStorerLegacy).suggestName(s"${nameContext}_legacy_storer")
        io <> inner.io
    } else {
        val inner = Module(new MultiChannelTrStorer(TrStorer0RequestChannelCount, nameContext))
            .suggestName(s"${nameContext}_multi_storer")
        io <> inner.io
    }
}

// Keep the existing Module entry point used by XSAI and standalone harnesses.
class CUTEV2Top()(implicit p: Parameters) extends CuteModule {
    private val outer = LazyModule(new LazyCUTEV2Top)
    val inner = Module(outer.module)
    val io = IO(new CUTETopIO)
    io <> inner.io
}

class LazyCUTEV2Top(implicit p: Parameters) extends LazyModule {
    val probeParams = MatrixRegWriteProbes.Params()
    // Probe positions retain their ABI: aLoad/bLoad denote physical TL0/TL1.
    val trLoader0Parameters = probeParams.parameters(probeParams.aLoad)
    val trLoader1Parameters = probeParams.parameters(probeParams.bLoad)
    val cdcParameters = probeParams.parameters(probeParams.mma)
    val accMemoryParameters = probeParams.parameters(probeParams.cLoad)

    lazy val module = new CUTEV2TopImp(this)
}

class CUTEV2TopImp(outer: LazyCUTEV2Top) extends LazyModuleImp(outer) with CUTEImplParameters{
    private def summarizeLoaderMode(mode: ResponseChannelHelper.LoaderMode): String = {
        if (mode.isLegacy) "legacy(1resp)"
        else s"bridge(${mode.responseChannelCount}resp)"
    }

    private def loaderBridgeConfigSummary: String = {
        val config = parsedLoaderBridgeChannelConfig
        Seq(
            s"trLoader0=${summarizeLoaderMode(config.trLoader0)}",
            s"trLoader1=${summarizeLoaderMode(config.trLoader1)}",
            s"AL0=${summarizeLoaderMode(config.accLoader0)}",
            s"AS0=${summarizeLoaderMode(config.accStorer0)}",
            s"TS0=${TrStorer0RequestChannelCount}req/${if (TrStorer0UseLegacyLoader) "1" else "all-lane"}ack",
            s"omegaInputQueue=$EnableOmegaInputQueue"
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
        contextName: String,
        identityFriendlyInput: Boolean = false,
        serviceTraceActive: Bool = false.B,
        releaseDebug: Bool = false.B
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
            contextName = nameContext,
            identityFriendlyInput = identityFriendlyInput
        )).suggestName(s"${nameContext}_response_bridge")
        bridge.io.timeStamp := time_stamp
        bridge.io.serviceTraceActive := false.B
        bridge.io.releaseDebug := false.B

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
    
    // The parameterized bridge uses the same request and response width.  In
    // particular, CS1 must arbitrate all C-store banks onto request channel 0;
    // otherwise the nominal A1B1CL1CS1 baseline still issues C stores through
    // all eight CUTE-to-L2 channels.
    val cStoreRequestChannelCount =
        if (AccStorer0UseLegacyLoader) AccMatrixRegNBanks else AccStorer0BridgeResponseChannelCount
    val aRequestChannelCount =
        if (TrLoader0UseLegacyLoader) TrMatrixRegNBanks else TrLoader0ResponseChannelCount
    val bRequestChannelCount =
        if (TrLoader1UseLegacyLoader) TrMatrixRegNBanks else TrLoader1ResponseChannelCount
    val cLoadRequestChannelCount =
        if (AccLoader0UseLegacyLoader) AccMatrixRegNBanks else AccLoader0BridgeResponseChannelCount

    val trLoader0BridgeContext = multiOrLegacyBridgeContext(
        "trLoader0",
        TrLoader0UseLegacyLoader,
        TrMatrixRegNBanks,
        aRequestChannelCount,
        TrLoader0ResponseChannelCount
    )
    val trLoader1BridgeContext = multiOrLegacyBridgeContext(
        "trLoader1",
        TrLoader1UseLegacyLoader,
        TrMatrixRegNBanks,
        bRequestChannelCount,
        TrLoader1ResponseChannelCount
    )
    val accLoader0BridgeContext = multiOrLegacyBridgeContext(
        "CLoad",
        AccLoader0UseLegacyLoader,
        AccMatrixRegNBanks,
        cLoadRequestChannelCount,
        AccLoader0BridgeResponseChannelCount
    )
    val accStorer0BridgeContext = multiOrLegacyBridgeContext(
        "CStore",
        AccLoader0UseLegacyLoader,
        AccMatrixRegNBanks,
        cStoreRequestChannelCount,
        AccStorer0BridgeResponseChannelCount
    )
    val accMemoryContext = VerilogNameHelper.sanitize(s"AccMemory_${accLoader0BridgeContext}_${accStorer0BridgeContext}")

    val TrMatrixRegs = Seq.tabulate(TrMatrixRegCount)(i => Module(new TrMatrixReg(i))).toVector
    val ADC = Module(new ADataController)
    val trLoader0 = Module(new TrLoader0Wrapper(trLoader0BridgeContext)(outer.trLoader0Parameters)).suggestName(s"${trLoader0BridgeContext}_wrapper")
    val BDC = Module(new BDataController)
    val trLoader1 = Module(new TrLoader1Wrapper(trLoader1BridgeContext)(outer.trLoader1Parameters)).suggestName(s"${trLoader1BridgeContext}_wrapper")
    val trStorer0 = Module(new TrStorer0Wrapper("trStorer0")).suggestName("trStorer0_wrapper")

    val AccMatrixRegs = Seq.tabulate(AccMatrixRegCount)(i => Module(new AccMatrixReg(i))).toVector

    val ASMRegs = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Seq.tabulate(2)(i => Module(new ABScaleMatrixReg)).toVector
    ) // Double-buffered storage.
    val ASC = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleController)
    )
    val ASL = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new AScaleLoader)
    )

    val BSMRegs = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Seq.tabulate(2)(i => Module(new ABScaleMatrixReg)).toVector
    ) // Double-buffered storage.
    val BSC = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new BScaleController)
    )
    val BSL = Option.when(cuteMatrixExtension.enableScalingFactor)(
      Module(new BScaleLoader)
    )

    val CDC = Module(new CDataController()(outer.cdcParameters))
    val accMemory = Module(new AccMemoryWrapper(accMemoryContext)(outer.accMemoryParameters)).suggestName(s"${accMemoryContext}_wrapper")

    val TaskCtrl: BaseTaskController = Module(new TaskController)
    io.matrixPrefetch.zip(TaskCtrl.io.matrixPrefetch).foreach { case (to, from) => to := from }
    
    val MTE = Module(new MatrixTE)

    val MMU = Module(new LocalMMU)

    //debug reg
    val DebugTimeStampe = RegInit(0.U(32.W))
    DebugTimeStampe := DebugTimeStampe + 1.U

    TaskCtrl.io.DebugTimeStampe := DebugTimeStampe
    // Default ADC inputs.
    ADC.io.FromMatrixRegIO.Data.valid := false.B
    ADC.io.FromMatrixRegIO.BankAddr.ready := false.B
    ADC.io.ConfigInfo <> TaskCtrl.io.ADC_MicroTask_Config
    ADC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    ASC.foreach { asc =>
        // Default ASC inputs.
        asc.io.FromMatrixRegIO.Data.valid := false.B
        asc.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(asc.io.FromMatrixRegIO.Data.bits)
        asc.io.FromMatrixRegIO.BankAddr.ready := false.B
        asc.io.ConfigInfo <> TaskCtrl.io.ASC_MicroTask_Config.get
        asc.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    }

    // Default trLoader0 inputs.
    trLoader0.io.ConfigInfo <> TaskCtrl.io.trLoader0Config
    trLoader0.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!TrLoader0UseLegacyLoader) {
        connectWithResponseBridge(
            trLoader0.io.LocalMMUIO,
            MMU.io.trLoader0Memory,
            TrMatrixRegNBanks,
            TrLoader0ResponseChannelCount,
            aRequestChannelCount,
            log2Ceil(TrMatrixRegNBanks),
            log2Ceil(TrMatrixRegBankNEntries),
            ResponseBridgeQueueDepth,
            true,
            trLoader0BridgeContext,
            identityFriendlyInput = TrMatrixRegNBanks == 8 && TrLoader0ResponseChannelCount == 8
        )
    } else {
        connectWithResponseArbiter(trLoader0.io.LocalMMUIO, MMU.io.trLoader0Memory, TrMatrixRegNBanks, trLoader0BridgeContext)
    }

    ASL.foreach { asl =>
    // Default ASL inputs.
        asl.io.ConfigInfo <> TaskCtrl.io.ASL_MicroTask_Config.get
        asl.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
        connectWithResponseArbiter(asl.io.LocalMMUIO, MMU.io.ASLocalMMUIO, TrMatrixRegNBanks, "ASL_legacy_1resp")
    }

    // Default BDC inputs.
    BDC.io.FromMatrixRegIO.Data.valid := false.B
    BDC.io.FromMatrixRegIO.BankAddr.ready := false.B
    BDC.io.ConfigInfo <> TaskCtrl.io.BDC_MicroTask_Config
    BDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    BSC.foreach { bsc =>
    // Default BSC inputs.
        bsc.io.FromMatrixRegIO.Data.valid := false.B
        bsc.io.FromMatrixRegIO.Data.bits := 0.U.asTypeOf(bsc.io.FromMatrixRegIO.Data.bits)
        bsc.io.FromMatrixRegIO.BankAddr.ready := false.B
        bsc.io.ConfigInfo <> TaskCtrl.io.BSC_MicroTask_Config.get
        bsc.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    }

    // Default trLoader1 inputs.
    trLoader1.io.ConfigInfo <> TaskCtrl.io.trLoader1Config
    trLoader1.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!TrLoader1UseLegacyLoader) {
        connectWithResponseBridge(
            trLoader1.io.LocalMMUIO,
            MMU.io.trLoader1Memory,
            TrMatrixRegNBanks,
            TrLoader1ResponseChannelCount,
            bRequestChannelCount,
            log2Ceil(TrMatrixRegNBanks),
            log2Ceil(TrMatrixRegBankNEntries),
            ResponseBridgeQueueDepth,
            true,
            trLoader1BridgeContext,
            identityFriendlyInput = TrMatrixRegNBanks == 8 && TrLoader1ResponseChannelCount == 8
        )
    } else {
        connectWithResponseArbiter(trLoader1.io.LocalMMUIO, MMU.io.trLoader1Memory, TrMatrixRegNBanks, trLoader1BridgeContext)
    }

    BSL.foreach { bsl =>
    // Default BSL inputs.
        bsl.io.ConfigInfo <> TaskCtrl.io.BSL_MicroTask_Config.get
        bsl.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
        connectWithResponseArbiter(bsl.io.LocalMMUIO, MMU.io.BSLocalMMUIO, TrMatrixRegNBanks, "BSL_legacy_1resp")
    }

    if (!cuteMatrixExtension.enableScalingFactor) {
        for (i <- 0 until TrMatrixRegNBanks) {
            MMU.io.ASLocalMMUIO.Request(i).valid := false.B
            MMU.io.ASLocalMMUIO.Request(i).bits := 0.U.asTypeOf(MMU.io.ASLocalMMUIO.Request(i).bits)
            MMU.io.ASLocalMMUIO.Response(i).ready := false.B
            MMU.io.BSLocalMMUIO.Request(i).valid := false.B
            MMU.io.BSLocalMMUIO.Request(i).bits := 0.U.asTypeOf(MMU.io.BSLocalMMUIO.Request(i).bits)
            MMU.io.BSLocalMMUIO.Response(i).ready := false.B
        }
    }

    // Default CDC inputs.
    CDC.io.FromMatrixRegIO.ReadResponseData := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadResponseData)
    CDC.io.FromMatrixRegIO.ReadWriteResponse := 0.U.asTypeOf(CDC.io.FromMatrixRegIO.ReadWriteResponse)
    CDC.io.ConfigInfo <> TaskCtrl.io.CDC_MicroTask_Config
    CDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

    trStorer0.io.command <> TaskCtrl.io.trStorer0.command
    trStorer0.io.done <> TaskCtrl.io.trStorer0.done
    connectLoaderRequests(trStorer0.io.memory, MMU.io.trStorer0Memory, TrMatrixRegNBanks)
    if (TrStorer0UseLegacyLoader) {
        // All physical lanes remain reachable; the legacy interface delivers one ACK.
        val ack = Module(new Arbiter(new MMUResponseIO, TrMatrixRegNBanks))
        for (lane <- 0 until TrMatrixRegNBanks) {
            ack.io.in(lane) <> MMU.io.trStorer0Memory.Response(lane)
        }
        trStorer0.io.memory.Response(0) <> ack.io.out
        for (lane <- 1 until TrMatrixRegNBanks) {
            trStorer0.io.memory.Response(lane).valid := false.B
            trStorer0.io.memory.Response(lane).bits := 0.U.asTypeOf(new MMUResponseIO)
        }
    } else {
        trStorer0.io.memory.Response <> MMU.io.trStorer0Memory.Response
    }

    // Default accMemory inputs.
    accMemory.io.ConfigInfo <> TaskCtrl.io.accMemoryConfig
    accMemory.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
    if (!AccLoader0UseLegacyLoader) {
        connectWithResponseBridge(
            accMemory.io.LoadLocalMMUIO,
            MMU.io.accLoader0Memory,
            AccMatrixRegNBanks,
            AccLoader0BridgeResponseChannelCount,
            cLoadRequestChannelCount,
            log2Ceil(AccMatrixRegNBanks),
            log2Ceil(AccMatrixRegBankNEntries),
            ResponseBridgeQueueDepth,
            true,
            accLoader0BridgeContext
        )
        if (AccStorer0DirectAckCountMode) {
            connectRequestChannelsWithGroupedArbiter(
                accMemory.io.StoreLocalMMUIO.Request,
                accMemory.io.StoreLocalMMUIO.ConherentRequsetSourceID,
                accMemory.io.StoreLocalMMUIO.nonConherentRequsetSourceID,
                MMU.io.accStorer0Memory.Request,
                MMU.io.accStorer0Memory.ConherentRequsetSourceID,
                MMU.io.accStorer0Memory.nonConherentRequsetSourceID,
                AccMatrixRegNBanks,
                cStoreRequestChannelCount,
                accStorer0BridgeContext
            )
            for (i <- 0 until AccMatrixRegNBanks) {
                accMemory.io.StoreLocalMMUIO.Response(i).valid := MMU.io.accStorer0Memory.Response(i).valid
                accMemory.io.StoreLocalMMUIO.Response(i).bits := MMU.io.accStorer0Memory.Response(i).bits
                MMU.io.accStorer0Memory.Response(i).ready := accMemory.io.StoreLocalMMUIO.Response(i).ready
            }
        } else {
            connectWithResponseBridge(
                accMemory.io.StoreLocalMMUIO,
                MMU.io.accStorer0Memory,
                AccMatrixRegNBanks,
                AccStorer0BridgeResponseChannelCount,
                cStoreRequestChannelCount,
                log2Ceil(AccMatrixRegNBanks),
                log2Ceil(AccMatrixRegBankNEntries),
                ResponseBridgeQueueDepth,
                false,
                accStorer0BridgeContext
            )
        }
    } else {
        connectWithResponseArbiter(
            accMemory.io.LoadLocalMMUIO,
            MMU.io.accLoader0Memory,
            AccMatrixRegNBanks,
            accLoader0BridgeContext
        )
        connectWithResponseArbiter(
            accMemory.io.StoreLocalMMUIO,
            MMU.io.accStorer0Memory,
            AccMatrixRegNBanks,
            accStorer0BridgeContext
        )
    }
    accMemory.io.ToMatrixRegIO.LoadReadWriteResponse := 0.U
    accMemory.io.ToMatrixRegIO.StoreReadWriteResponse := 0.U
    accMemory.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData := 0.U.asTypeOf(accMemory.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData)

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
    
    // Connect to the CPU MMU or IOMMU in a future integration.
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
    perf.memEvents(0) := TaskCtrl.io.perfProbe.trLoader0Done
    perf.memEvents(1) := TaskCtrl.io.perfProbe.trLoader1Done
    perf.memEvents(2) := TaskCtrl.io.perfProbe.accLoader0Done
    perf.memEvents(3) := TaskCtrl.io.perfProbe.storeDone
    perf.memEvents(4) := TaskCtrl.io.perfProbe.trLoader0Active
    perf.memEvents(5) := TaskCtrl.io.perfProbe.trLoader1Active
    perf.memEvents(6) := TaskCtrl.io.perfProbe.accLoader0Active
    perf.memEvents(7) := TaskCtrl.io.perfProbe.accStorer0Active
    perf.memEvents(8) := MMU.io.perfProbe.rdReq
    perf.memEvents(9) := MMU.io.perfProbe.wrReq
    perf.memEvents(10) := MMU.io.perfProbe.rd32BReq
    perf.memEvents(11) := MMU.io.perfProbe.wr32BReq
    perf.localMMU := MMU.io.perfProbe
    perf.trLoader0 := trLoader0.io.perfProbe
    perf.trLoader1 := trLoader1.io.perfProbe
    perf.accLoader0 := accMemory.io.perfProbe
    io.perf := RegNext(perf, 0.U.asTypeOf(new CutePerfToCoreIO))

    // Assign default inputs for each MatrixReg.
    
    // TR MatrixReg
    for (i <- 0 until TrMatrixRegCount){
        // DataController requests.
        TrMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.valid := false.B
        TrMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(TrMatrixRegs(i).io.MatrixRegIO.FromDataController.BankAddr.bits)
        // Loader memory requests.
        TrMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequest :=
            0.U.asTypeOf(TrMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.WriteRequest)
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

    // ACC MatrixReg
    for (i <- 0 until AccMatrixRegCount){
        AccMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteRequest := 0.U
        AccMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.LoadReadWriteRequest := 0.U
        AccMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.StoreReadWriteRequest := 0.U
    }

    // ============================================
        // A Scale MatrixReg routing logic (double-buffered).
    // ============================================
    def connectScaleControlToRegs(
        spadId: UInt,
        ScaleCtrlIO: ABScaleControlMatrixRegIO,
        ScaleRegs: Seq[ABScaleMatrixReg]
    ): Unit = {
        // ASC selects ScaleRegs and the MatrixReg selected by SpadId.
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

        // ASC receives ScaleRegs responses.
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
        // ASL selects ScaleRegs and the MatrixReg selected by SpadId.
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

    def disableTrLoadPort(dest: TrLoadMatrixRegIO): Unit = {
        for (b <- 0 until TrMatrixRegNBanks) {
            for (lane <- 0 until 2) {
                val write = dest.WriteRequest(b)(lane)
                write.BankAddr.valid := false.B
                write.BankAddr.bits := DontCare
                write.Data.valid := false.B
                write.Data.bits := DontCare
                write.ByteMask.valid := false.B
                write.ByteMask.bits := Fill(TrMatrixRegEntryByteSize, true.B)
            }
        }
    }

    def copyTrLoadPort(dest: TrLoadMatrixRegIO, src: TrLoadMatrixRegIO): Unit = {
        for (b <- 0 until TrMatrixRegNBanks) {
            for (lane <- 0 until 2) {
                dest.WriteRequest(b)(lane) := src.WriteRequest(b)(lane)
            }
        }
    }
    
    for (regIdx <- 0 until TrMatrixRegCount) {
        val dest = TrMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        val trLoader0Sel = trLoader0.io.MatrixRegId === regIdx.U
        val trLoader1Sel = trLoader1.io.MatrixRegId === regIdx.U
        val trLoader0Active = trLoader0.io.ToMatrixRegIO.active
        val trLoader1Active = trLoader1.io.ToMatrixRegIO.active
        dest.active := DontCare
        when(trLoader0Sel && trLoader1Sel && trLoader0Active && trLoader1Active) {
            assert(false.B, cf"[CUTETop] trLoader0 and trLoader1 choose the same TR MatrixReg($regIdx)")
        }
        when(trLoader0Sel && trLoader0Active) {
            copyTrLoadPort(dest, trLoader0.io.ToMatrixRegIO)
        }.elsewhen(trLoader1Sel && trLoader1Active) {
            copyTrLoadPort(dest, trLoader1.io.ToMatrixRegIO)
        }.otherwise {
            disableTrLoadPort(dest)
        }
    }

    val trReadRouter = Module(new TrMatrixRegReadPortRouter)
    val trReaders = Seq(ADC.io.FromMatrixRegIO, BDC.io.FromMatrixRegIO)
    val trReaderIds = Seq(ADC.io.MatrixRegId, BDC.io.MatrixRegId)
    for (c <- trReaders.indices) {
        trReadRouter.io.clients(c) <> trReaders(c)
        trReadRouter.io.regIds(c) := trReaderIds(c)
    }
    for (r <- 0 until TrMatrixRegCount) {
        TrMatrixRegs(r).io.MatrixRegIO.FromDataController <> trReadRouter.io.registers(r)
    }

    val trStorePorts = TrMatrixRegs.map(_.io.MatrixRegIO.FromStore)
    val trStoreSelect = VecInit((0 until TrMatrixRegCount).map(r => trStorer0.io.regId === r.U))
    trStorer0.io.read.BankAddr.ready := Mux1H(trStoreSelect, trStorePorts.map(_.BankAddr.ready))
    trStorer0.io.read.Data := Mux1H(trStoreSelect, trStorePorts.map(_.Data))
    for (r <- 0 until TrMatrixRegCount) {
        trStorePorts(r).BankAddr.valid := trStoreSelect(r) && trStorer0.io.read.BankAddr.valid
        trStorePorts(r).BankAddr.bits := trStorer0.io.read.BankAddr.bits
    }

    for (regIdx <- 0 until AccMatrixRegCount) {
        val dest = AccMatrixRegs(regIdx).io.MatrixRegIO.FromMemoryLoader
        val loadSel = accMemory.io.LoadMatrixRegId === regIdx.U
        val storeSel = accMemory.io.StoreMatrixRegId === regIdx.U
        val loadWriteReq = accMemory.io.ToMatrixRegIO.LoadReadWriteRequest(MatrixRegTaskType.WriteFromMemoryLoaderIndex)
        val storeReadReq = accMemory.io.ToMatrixRegIO.StoreReadWriteRequest(MatrixRegTaskType.ReadFromMemoryLoaderIndex)

        when(loadSel && storeSel && loadWriteReq && storeReadReq) {
            assert(false.B, cf"[CUTETop] accMemory load/store target the same ACC MatrixReg($regIdx) in one cycle")
        }

        for (bank <- 0 until AccMatrixRegNBanks) {
            for (lane <- 0 until 2) {
                val src = accMemory.io.ToMatrixRegIO.ReadRequestToMatrixReg.BankAddr(bank)(lane)
                dest.ReadRequestToMatrixReg.BankAddr(bank)(lane).valid := storeSel && src.valid
                dest.ReadRequestToMatrixReg.BankAddr(bank)(lane).bits := src.bits
                val destWrite = dest.WriteRequestToMatrixReg.WriteRequest(bank)(lane)
                val srcWrite = accMemory.io.ToMatrixRegIO.WriteRequestToMatrixReg.WriteRequest(bank)(lane)
                destWrite.BankAddr.valid := loadSel && srcWrite.BankAddr.valid
                destWrite.Data.valid := loadSel && srcWrite.Data.valid
                destWrite.ByteMask.valid := loadSel && srcWrite.ByteMask.valid
                destWrite.BankAddr.bits := srcWrite.BankAddr.bits
                destWrite.Data.bits := srcWrite.Data.bits
                destWrite.ByteMask.bits := srcWrite.ByteMask.bits
            }
        }

        val loadReqForThisReg = Mux(loadSel, accMemory.io.ToMatrixRegIO.LoadReadWriteRequest, 0.U)
        val storeReqForThisReg = Mux(storeSel, accMemory.io.ToMatrixRegIO.StoreReadWriteRequest, 0.U)
        dest.LoadReadWriteRequest := loadReqForThisReg
        dest.StoreReadWriteRequest := storeReqForThisReg
    }

    val accLoader0SelVec = VecInit((0 until AccMatrixRegCount).map(i => accMemory.io.LoadMatrixRegId === i.U))
    val accStorer0SelVec = VecInit((0 until AccMatrixRegCount).map(i => accMemory.io.StoreMatrixRegId === i.U))
    // Select the register that received the read address in the previous cycle.
    val storeResponseSelect = RegNext(accStorer0SelVec)
    for (bank <- 0 until AccMatrixRegNBanks; lane <- 0 until 2) {
        val responses = AccMatrixRegs.map(_.io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bank)(lane))
        accMemory.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank)(lane) := Mux1H(storeResponseSelect, responses)
    }

    val accLoader0RwRespChoices = (0 until AccMatrixRegCount).map(i => accLoader0SelVec(i) -> AccMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.LoadReadWriteResponse)
    val accStorer0RwRespChoices = (0 until AccMatrixRegCount).map(i => accStorer0SelVec(i) -> AccMatrixRegs(i).io.MatrixRegIO.FromMemoryLoader.StoreReadWriteResponse)
    accMemory.io.ToMatrixRegIO.LoadReadWriteResponse := Mux1H(accLoader0RwRespChoices)
    accMemory.io.ToMatrixRegIO.StoreReadWriteResponse := Mux1H(accStorer0RwRespChoices)

    for (regIdx <- 0 until AccMatrixRegCount) {
        val dest = AccMatrixRegs(regIdx).io.MatrixRegIO.FromDataController
        
        when(CDC.io.MatrixRegId === regIdx.U) {
            for (bank <- 0 until AccMatrixRegNBanks) {
                dest.ReadBankAddr(bank).valid := CDC.io.FromMatrixRegIO.ReadBankAddr(bank).valid
                dest.WriteBankAddr(bank).valid := CDC.io.FromMatrixRegIO.WriteBankAddr(bank).valid
                dest.WriteRequestData(bank).valid := CDC.io.FromMatrixRegIO.WriteRequestData(bank).valid
            }
            dest.ReadWriteRequest := CDC.io.FromMatrixRegIO.ReadWriteRequest
        }.otherwise {
            for (bank <- 0 until AccMatrixRegNBanks) {
                dest.ReadBankAddr(bank).valid := false.B
                dest.WriteBankAddr(bank).valid := false.B
                dest.WriteRequestData(bank).valid := false.B
            }
        }

        for (bank <- 0 until AccMatrixRegNBanks) {
            dest.ReadBankAddr(bank).bits := CDC.io.FromMatrixRegIO.ReadBankAddr(bank).bits
            dest.WriteBankAddr(bank).bits := CDC.io.FromMatrixRegIO.WriteBankAddr(bank).bits
            dest.WriteRequestData(bank).bits := CDC.io.FromMatrixRegIO.WriteRequestData(bank).bits
        }
    }

    val cdcSelVec = VecInit((0 until AccMatrixRegCount).map(i => CDC.io.MatrixRegId === i.U))
    for (bank <- 0 until AccMatrixRegNBanks) {
        val cdcReadRespValidChoices = (0 until AccMatrixRegCount).map(i => cdcSelVec(i) ->
            AccMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).valid
        )
        val cdcReadRespBitsChoices = (0 until AccMatrixRegCount).map(i => cdcSelVec(i) ->
            AccMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadResponseData(bank).bits
        )
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).valid := Mux1H(cdcReadRespValidChoices)
        CDC.io.FromMatrixRegIO.ReadResponseData(bank).bits := Mux1H(cdcReadRespBitsChoices)
    }
    val cdcRwRespChoices = (0 until AccMatrixRegCount).map(i =>
        cdcSelVec(i) -> AccMatrixRegs(i).io.MatrixRegIO.FromDataController.ReadWriteResponse
      )
    CDC.io.FromMatrixRegIO.ReadWriteResponse := Mux1H(cdcRwRespChoices)
}
