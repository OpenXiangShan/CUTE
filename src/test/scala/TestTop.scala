package cute

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import coupledL2._
import utility._

object baseConfig {
  def apply(maxHartIdBits: Int) = {
    new Config((_, _, _) => {
      case MaxHartIdBits => maxHartIdBits
    })
  }
}

class TestTop()(implicit p: Parameters) extends LazyModule {
  override lazy val desiredName: String = "TestTop"
  val delayFactor = 0.5
  val cuteParams = p(CuteParamsKey)

  def createClientNode(name: String, sources: Int) = {
    val slaveNode = TLManagerNode(Seq(
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address = Seq(AddressSet(0, 0xffffffffffffL)),
            executable = true,
            supportsGet = TransferSizes(1, 32),
            supportsPutFull = TransferSizes(1, 32),
            supportsPutPartial = TransferSizes(1, 32)
          )
        ),
        beatBytes = 32,
        minLatency = 2
      )
    ))
    slaveNode
  }
  val hbl2_nodes = Seq(createClientNode("hbl2", 32))
  val slave_nodes = hbl2_nodes

  val cute_tl = LazyModule(new Cute2TL())

  slave_nodes.foreach { node =>
    node := cute_tl.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl2top = Flipped(new YGJKControl)
      val mrelease = Valid(new MreleaseIO)
      val matrix_data_in = Flipped(DecoupledIO(new MatrixDataBundle()))
    })
    
    val cute = Module(new CUTEV2Top())
    // Signals to top level
    io.ctrl2top <> cute.io.ctrl2top
    io.mrelease := cute.io.mrelease

    // memory access between CUTE and HBL2
    cute_tl.module.io.mmu <> cute.io.mmu2llc
    val tl_data_in = cute_tl.module.io.matrix_data_in
    tl_data_in.valid := io.matrix_data_in.valid
    tl_data_in.bits := 0.U.asTypeOf(tl_data_in.bits)
    tl_data_in.bits.opcode := TLMessages.AccessAckData
    tl_data_in.bits.source := io.matrix_data_in.bits.sourceId
    tl_data_in.bits.data := io.matrix_data_in.bits.data.data
    io.matrix_data_in.ready := tl_data_in.ready

    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    slave_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"slave_port_$i"))
    }

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)
  }
}

private[cute] object TestTopFirtoolOptions {
  def apply() = Seq(
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--repl-seq-mem"),
    FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
    FirtoolOption("--lowering-options=explicitBitcast")
  )
}

object TestTop extends App {
  val config = baseConfig(1).alterPartial({
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      Debug = CuteDebugParams.NoDebug,
      v3config = Cutev3extParams(
        TaskCtrl_AutoClear = true,
      ),
    )
  })

  ChiselDB.init(false)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TestTop()(p)) )(config)
  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions()
  )

  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write("./build")
}