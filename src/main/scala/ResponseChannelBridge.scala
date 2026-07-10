package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class ResponseChannelBridge(
  val inputChannelCount: Int,
  val respChannelCount: Int,
  val bankCount: Int,
  val queueDepth: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val bankIdWidth: Int,
  val bankIdOffset: Int = 0,
  val hasDataPayload: Boolean = true,
  val debugEnable: Boolean = false,
  val contextName: String = "Generic"
)(implicit p: Parameters) extends CuteModule {
  require(inputChannelCount >= 1, s"inputChannelCount ($inputChannelCount) must be >= 1")
  require(respChannelCount >= 1, s"respChannelCount ($respChannelCount) must be >= 1")
  require(bankCount >= respChannelCount, s"bankCount ($bankCount) must be >= respChannelCount ($respChannelCount)")
  require(queueDepth >= 1, s"queueDepth ($queueDepth) must be >= 1")

  private val nameContext = VerilogNameHelper.sanitize(contextName)
  private val bankIdxWidth = log2Ceil(bankCount max 2)
  private val metaWidth = if (hasDataPayload) sourceIdWidth + 1 else sourceIdWidth

  override def desiredName: String =
    s"ResponseChannelBridge_${nameContext}_${inputChannelCount}in_${respChannelCount}resp_${bankCount}banks"

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val in = Flipped(Vec(inputChannelCount, Decoupled(new MMUResponseIO)))
    val out = Vec(bankCount, Decoupled(new MMUResponseIO))
  })

  private def log(msg: => Printable): Unit = {
    if (debugEnable) printf(cf"[T${io.timeStamp}][RespBridge] " + msg + "\n")
  }

  private def extractBankId(sourceId: UInt): UInt = {
    sourceId(bankIdOffset + bankIdWidth - 1, bankIdOffset)(bankIdxWidth - 1, 0)
  }

  val router = Module(new OmegaResponseRouter(
    n = inputChannelCount,
    dataWidth = dataWidth,
    sourceIdWidth = sourceIdWidth,
    bankIdWidth = bankIdWidth,
    bankIdOffset = bankIdOffset,
    outCount = respChannelCount,
    bankCount = bankCount,
    debugEnable = debugEnable,
    contextName = nameContext
  )).suggestName(s"${nameContext}_omega_router")
  router.io.timeStamp := io.timeStamp

  for (channel <- 0 until inputChannelCount) {
    router.io.in(channel).valid := io.in(channel).valid
    router.io.in(channel).bits.data := Mux(hasDataPayload.B, io.in(channel).bits.ReseponseData, 0.U)
    router.io.in(channel).bits.sourceId := io.in(channel).bits.ReseponseSourceID
    router.io.in(channel).bits.coherent := Mux(hasDataPayload.B, io.in(channel).bits.ReseponseConherent, false.B)
    io.in(channel).ready := router.io.in(channel).ready
  }

  val bankDataQueues = Option.when(hasDataPayload) {
    Seq.tabulate(bankCount) { bank =>
      Module(new Queue(UInt(dataWidth.W), queueDepth, pipe = true))
        .suggestName(s"${nameContext}_bank${bank}_resp_data_queue")
    }
  }
  val bankMetaQueues = Seq.tabulate(bankCount) { bank =>
    Module(new Queue(UInt(metaWidth.W), queueDepth, pipe = true))
      .suggestName(s"${nameContext}_bank${bank}_resp_meta_queue")
  }

  for (bank <- 0 until bankCount) {
    val metaQueue = bankMetaQueues(bank)
    val dataQueueOpt = bankDataQueues.map(_(bank))

    dataQueueOpt.foreach { dataQueue =>
      dataQueue.io.enq.valid := false.B
      dataQueue.io.enq.bits := 0.U
      dataQueue.io.deq.ready := io.out(bank).ready
    }
    metaQueue.io.enq.valid := false.B
    metaQueue.io.enq.bits := 0.U

    dataQueueOpt.foreach { dataQueue =>
      assert(
        dataQueue.io.enq.ready === metaQueue.io.enq.ready,
        s"ResponseChannelBridge bank $bank enqueue queues diverged"
      )
      assert(
        dataQueue.io.deq.valid === metaQueue.io.deq.valid,
        s"ResponseChannelBridge bank $bank dequeue queues diverged"
      )
    }

    io.out(bank).valid := dataQueueOpt.map(_.io.deq.valid).getOrElse(metaQueue.io.deq.valid)
    io.out(bank).bits.ReseponseData := dataQueueOpt.map(_.io.deq.bits).getOrElse(0.U)
    io.out(bank).bits.ReseponseSourceID := metaQueue.io.deq.bits(sourceIdWidth - 1, 0)
    io.out(bank).bits.ReseponseConherent := Mux(hasDataPayload.B, metaQueue.io.deq.bits(metaWidth - 1), false.B)
    metaQueue.io.deq.ready := io.out(bank).ready

    when(io.out(bank).fire) {
      log(cf"bank[$bank] dequeue sourceId=0x${io.out(bank).bits.ReseponseSourceID}%x")
    }
  }

  for (group <- 0 until respChannelCount) {
    val groupedResp = router.io.out(group)
    val targetBank = extractBankId(groupedResp.bits.sourceId)
    val groupBanks = ResponseChannelHelper.banksInGroup(group, respChannelCount, bankCount)
    val targetIsInGroup = VecInit(groupBanks.map(bank => targetBank === bank.U)).asUInt.orR

    groupedResp.ready := MuxCase(
      false.B,
      groupBanks.map { bank =>
        val dataReady = bankDataQueues.map(_(bank).io.enq.ready).getOrElse(true.B)
        val metaReady = bankMetaQueues(bank).io.enq.ready
        if (hasDataPayload) {
          assert(
            dataReady === metaReady,
            s"ResponseChannelBridge group $group bank $bank enqueue ready diverged"
          )
        }
        (targetBank === bank.U) -> (dataReady && metaReady)
      }
    )

    when(groupedResp.valid) {
      assert(targetIsInGroup,
        cf"ResponseChannelBridge: group $group received bankId=$targetBank outside legal banks.")
      assert(
        ResponseChannelHelper.groupIdOfBank(targetBank, respChannelCount, bankCount) === group.U,
        cf"ResponseChannelBridge: bankId=$targetBank mapped to wrong group $group."
      )
    }

    for (bank <- groupBanks) {
      val enqueue = groupedResp.valid && targetBank === bank.U
      bankDataQueues.foreach { queues =>
        queues(bank).io.enq.valid := enqueue
        queues(bank).io.enq.bits := groupedResp.bits.data
      }
      bankMetaQueues(bank).io.enq.valid := enqueue
      bankMetaQueues(bank).io.enq.bits :=
        Mux(hasDataPayload.B, Cat(groupedResp.bits.coherent, groupedResp.bits.sourceId), groupedResp.bits.sourceId)

      when(bankMetaQueues(bank).io.enq.fire) {
        log(cf"group[$group] enqueue bank[$bank] sourceId=0x${groupedResp.bits.sourceId}%x")
      }
    }
  }
}
