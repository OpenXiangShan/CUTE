package cute

import chisel3._
import chisel3.util.log2Ceil
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import xscache.chi.{CHIIssue, DecoupledPortIO, HasCHIMsgParameters, Issue}
import xscache.common.{AliasField, BankBitsKey}
import xscache.coupledL2._
import utility._

class TestTopWithL2()(implicit p: Parameters) extends LazyModule with HasCHIMsgParameters {
  override lazy val desiredName: String = "TestTopWithL2"
  private val cuteParams = p(CuteParamsKey)
  private val l2Params = p(L2ParamKey)
  private val l2Banks = cuteParams.L2NBanks

  val cuteTl = LazyModule(new Cute2TL)
  val l2 = LazyModule(new CoupledL2()(new Config((site, here, up) => {
    case L2ParamKey => l2Params.copy(name = "L2_CUTE", hartId = 0)
    case EnableMatrix => true
    case EnableL2DecoupledDownstreamCHI => true
    case BankBitsKey => log2Ceil(l2Banks)
    case MaxHartIdBits => 1
    case CHIIssue => p(CHIIssue)
    case PerfCounterOptionsKey => p(PerfCounterOptionsKey)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
  })))

  private val l2UpstreamXbar = TLXbar()
  private val bankBinders = BankBinder(l2Banks, 64)
  l2.managerNode := TLXbar() :=* bankBinders :*= l2.node :*= l2UpstreamXbar
  private val coreNode = TLClientNode(Seq(TLMasterPortParameters.v2(
    masters = Seq(TLMasterParameters.v1(name = "core_l2", sourceId = IdRange(0, 1), supportsProbe = TransferSizes(64))),
    channelBytes = TLChannelBeatBytes(32),
    requestFields = Seq(AliasField(2))
  )))
  l2UpstreamXbar := coreNode
  private val mmioClientNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(name = "uncache", sourceId = IdRange(0, 16)))
  )))
  l2.mmioBridge.mmioNode := mmioClientNode
  cuteTl.node.foreach { node => l2UpstreamXbar :=* node }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl2top = Flipped(new YGJKControl)
      val verification_task_busy = Output(Bool())
      val chi = new DecoupledPortIO
      val nodeId = Input(UInt(NODEID_WIDTH.W))
    })
    val cute = Module(new CUTEV2Top)
    io.ctrl2top <> cute.io.ctrl2top
    io.verification_task_busy := cute.io.perf.backendEvents(0).orR
    cuteTl.module.io.mmu <> cute.io.mmu2llc

    val (coreTl, _) = coreNode.out.head
    coreTl.a.valid := false.B
    coreTl.a.bits := DontCare
    coreTl.b.ready := true.B
    coreTl.c.valid := false.B
    coreTl.c.bits := DontCare
    coreTl.d.ready := true.B
    coreTl.e.valid := false.B
    coreTl.e.bits := DontCare

    val l2MatrixData = l2.module.io.matrixDataOut.get
    val cuteMatrixData = cuteTl.module.io.matrix_data_in
    for (bank <- 0 until l2Banks) {
      cuteMatrixData(bank).valid := l2MatrixData(bank).valid
      cuteMatrixData(bank).bits.source := l2MatrixData(bank).bits.sourceId
      cuteMatrixData(bank).bits.data := l2MatrixData(bank).bits.data.data
      l2MatrixData(bank).ready := cuteMatrixData(bank).ready
    }
    l2.module.io.l2_hint := DontCare
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.hartId := 0.U
    l2.module.io.nodeID := io.nodeId
    l2.module.io.debugTopDown := DontCare
    l2.module.io.l2_tlb_req := DontCare
    l2.module.io.decoupledCHI.get <> io.chi
    dontTouch(l2.module.io)
  }
}

private[cute] object TestTopWithL2FirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTopWithL2.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

object TestTopWithL2 extends App {
  val config = baseConfig(6).alterPartial({
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      L2NBanks = 8,
      Debug = CuteDebugParams.NoDebug,
      LoaderBridgeChannelConfig = "A1BLCL1CS2",
      EnableDifftest = false,
      v3config = Cutev3extParams(TaskCtrl_AutoClear = true),
      MatrixExtension = MatrixIsaParams(
        enableInt8Int32 = true, enableFp8Fp32 = true, enableFp8Fp16 = true,
        enableFp8Bf16 = true, enableFp16Fp16 = true, enableFp16Fp32 = true,
        enableBf16Fp32 = true
      )
    )
    case L2ParamKey => L2Param(
      name = "L2_CUTE", ways = 8, sets = 64,
      clientCaches = Seq(L1Param(name = "CUTE", aliasBitsOpt = Some(2))),
      enablePerf = false, enableRollingDB = false, enableMonitor = false,
      enableTLLog = false, enableCHILog = false, enableMCP2 = false,
      dataCheck = Some("oddparity"), sam = Seq(AddressSet.everything -> 0)
    )
    case CHIIssue => Issue.Eb
    case EnableMatrix => true
    case EnableL2DecoupledDownstreamCHI => true
  })
  ChiselDB.init(false)
  Constantin.init(false)
  val top = DisableMonitors(p => LazyModule(new TestTopWithL2()(p)))(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopWithL2FirtoolOptions())
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}
