package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class ResponseChannelBridge(
  val inputChannelCount: Int,
  val respChannelCount: Int,
  val bankCount: Int,
  val baseDepth: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val bankIdWidth: Int,
  val bankIdOffset: Int = 0,
  val debugEnable: Boolean = false
)(implicit p: Parameters) extends CuteModule {
  require(inputChannelCount >= 1, s"inputChannelCount ($inputChannelCount) must be >= 1")
  require(respChannelCount >= 1, s"respChannelCount ($respChannelCount) must be >= 1")
  require(bankCount >= respChannelCount, s"bankCount ($bankCount) must be >= respChannelCount ($respChannelCount)")
  require(baseDepth >= 1, s"baseDepth ($baseDepth) must be >= 1")

  private val perGroupBankCount = ResponseChannelHelper.banksPerGroup(respChannelCount, bankCount)
  private val queueDepth = math.max(2, baseDepth * perGroupBankCount)
  private val bankIdxWidth = log2Ceil(bankCount max 2)

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
    debugEnable = debugEnable
  ))
  router.io.timeStamp := io.timeStamp

  for (channel <- 0 until inputChannelCount) {
    router.io.in(channel).valid := io.in(channel).valid
    router.io.in(channel).bits.data := io.in(channel).bits.ReseponseData
    router.io.in(channel).bits.sourceId := io.in(channel).bits.ReseponseSourceID
    router.io.in(channel).bits.coherent := io.in(channel).bits.ReseponseConherent
    io.in(channel).ready := router.io.in(channel).ready
  }

  val bankQueues = Seq.fill(bankCount)(Module(new Queue(new RoutedResponse(dataWidth, sourceIdWidth), queueDepth, pipe = true)))

  for (bank <- 0 until bankCount) {
    bankQueues(bank).io.enq.valid := false.B
    bankQueues(bank).io.enq.bits := 0.U.asTypeOf(bankQueues(bank).io.enq.bits)

    io.out(bank).valid := bankQueues(bank).io.deq.valid
    io.out(bank).bits.ReseponseData := bankQueues(bank).io.deq.bits.data
    io.out(bank).bits.ReseponseSourceID := bankQueues(bank).io.deq.bits.sourceId
    io.out(bank).bits.ReseponseConherent := bankQueues(bank).io.deq.bits.coherent
    bankQueues(bank).io.deq.ready := io.out(bank).ready

    when(io.out(bank).fire) {
      log(cf"bank[$bank] dequeue sourceId=0x${io.out(bank).bits.ReseponseSourceID}%x")
    }
  }

  for (group <- 0 until respChannelCount) {
    val groupedResp = router.io.out(group)
    val targetBank = extractBankId(groupedResp.bits.sourceId)
    val groupBanks = ResponseChannelHelper.banksInGroup(group, respChannelCount, bankCount)
    val targetIsInGroup = VecInit(groupBanks.map(bank => targetBank === bank.U)).asUInt.orR

    groupedResp.ready := MuxCase(false.B, groupBanks.map(bank => (targetBank === bank.U) -> bankQueues(bank).io.enq.ready))

    when(groupedResp.valid) {
      assert(targetIsInGroup,
        cf"ResponseChannelBridge: group $group received bankId=$targetBank outside legal banks.")
      assert(
        ResponseChannelHelper.groupIdOfBank(targetBank, respChannelCount, bankCount) === group.U,
        cf"ResponseChannelBridge: bankId=$targetBank mapped to wrong group $group."
      )
    }

    for (bank <- groupBanks) {
      bankQueues(bank).io.enq.valid := groupedResp.valid && targetBank === bank.U
      bankQueues(bank).io.enq.bits := groupedResp.bits

      when(bankQueues(bank).io.enq.fire) {
        log(cf"group[$group] enqueue bank[$bank] sourceId=0x${groupedResp.bits.sourceId}%x")
      }
    }
  }
}
