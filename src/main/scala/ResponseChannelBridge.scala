package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility._

class ResponseChannelBridge(
  val inputChannelCount: Int,
  val respChannelCount: Int,
  val bankCount: Int,
  val baseDepth: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val bankIdWidth: Int,
  val bankIdOffset: Int = 0,
  val debugEnable: Boolean = false,
  val contextName: String = "Generic",
  val traceTag: UInt = 0.U,
  val responseUsesAllocatedSourceId: Boolean = false,
  val identityFriendlyInput: Boolean = false
)(implicit p: Parameters) extends CuteModule {
  require(inputChannelCount >= 1, s"inputChannelCount ($inputChannelCount) must be >= 1")
  require(respChannelCount >= 1, s"respChannelCount ($respChannelCount) must be >= 1")
  require(bankCount >= respChannelCount, s"bankCount ($bankCount) must be >= respChannelCount ($respChannelCount)")
  require(baseDepth >= 1, s"baseDepth ($baseDepth) must be >= 1")

  private val nameContext = VerilogNameHelper.sanitize(contextName)
  private val perGroupBankCount = ResponseChannelHelper.banksPerGroup(respChannelCount, bankCount)
  private val queueDepth = math.max(2, baseDepth * perGroupBankCount)
  private val bankIdxWidth = log2Ceil(bankCount max 2)

  override def desiredName: String =
    s"ResponseChannelBridge_${nameContext}_${inputChannelCount}in_${respChannelCount}resp_${bankCount}banks"

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val serviceTraceActive = Input(Bool())
    val releaseDebug = Input(Bool())
    val in = Flipped(Vec(inputChannelCount, Decoupled(new MMUResponseIO)))
    val out = Vec(bankCount, Decoupled(new MMUResponseIO))
    val omegaBlockedCycles = Output(UInt(64.W))
    val omegaBlockedRespCycles = Output(UInt(64.W))
    val omegaBlockedResponses = Output(UInt(64.W))
  })

  private def log(msg: => Printable): Unit = if (debugEnable) printf(msg + "\n")



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
    contextName = nameContext,
    traceTag = traceTag,
    responseUsesAllocatedSourceId = responseUsesAllocatedSourceId,
    allocatedSourceIdWidth = LLCSourceMaxNumBitSize,
    identityFriendlyInput = identityFriendlyInput
  )).suggestName(s"${nameContext}_omega_router")
  router.io.timeStamp := io.timeStamp

  val omegaInputs = Seq.tabulate(inputChannelCount) { channel =>
    val routed = Wire(Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))
    routed.valid := io.in(channel).valid
    routed.bits.data := io.in(channel).bits.ReseponseData
    routed.bits.sourceId := io.in(channel).bits.ReseponseSourceID
    routed.bits.coherent := io.in(channel).bits.ReseponseConherent
    if (EnableOmegaInputQueue) {
      val queue = Module(new Queue(new RoutedResponse(dataWidth, sourceIdWidth), 2, pipe = true, flow = true))
        .suggestName(s"${nameContext}_omega_input${channel}_queue")
      queue.io.enq <> routed
      io.in(channel).ready := routed.ready
      XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_head_stall_$channel",
        queue.io.deq.valid && !queue.io.deq.ready)
      XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_occupancy_$channel", queue.io.count)
      XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_full_$channel", queue.io.count === 2.U)
      queue.io.deq
    } else {
      io.in(channel).ready := routed.ready
      routed
    }
  }

  for (channel <- 0 until inputChannelCount) {
    router.io.in(channel) <> omegaInputs(channel)

    when(io.in(channel).valid) {
      assert(extractBankId(io.in(channel).bits.ReseponseSourceID) < bankCount.U,
        cf"ResponseChannelBridge: input $channel has illegal bank id")
    }



    XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_offer_$channel", io.in(channel).valid)
    XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_accept_$channel", io.in(channel).fire)
    XSPerfAccumulate(s"CUTE_${nameContext}_omega_accept_$channel", router.io.in(channel).fire)
    XSPerfAccumulate(s"CUTE_${nameContext}_omega_input_stall_$channel", io.in(channel).valid && !io.in(channel).ready)
  }

  // Count only at the Omega input head. This avoids counting the same response
  // once at the bridge input queue and again at the router. `blockedActive`
  // remains set until that exact head fires, so a multi-cycle stall contributes
  // one response but multiple response-cycles.
  val omegaBlockedMask = router.io.blockedByOmega
  val blockedActive = RegInit(VecInit(Seq.fill(inputChannelCount)(false.B)))
  val newBlockedMask = omegaBlockedMask & ~blockedActive.asUInt
  for (channel <- 0 until inputChannelCount) {
    when(omegaBlockedMask(channel)) {
      blockedActive(channel) := true.B
    }
    when(router.io.in(channel).fire) {
      blockedActive(channel) := false.B
    }
  }

  val blockedCycleIncrement = omegaBlockedMask.orR.asUInt
  val blockedRespCycleIncrement = PopCount(omegaBlockedMask)
  val blockedResponseIncrement = PopCount(newBlockedMask)
  val blockedCycles = RegInit(0.U(64.W))
  val blockedRespCycles = RegInit(0.U(64.W))
  val blockedResponses = RegInit(0.U(64.W))
  val blockedCyclesNext = blockedCycles + blockedCycleIncrement
  val blockedRespCyclesNext = blockedRespCycles + blockedRespCycleIncrement
  val blockedResponsesNext = blockedResponses + blockedResponseIncrement
  blockedCycles := blockedCyclesNext
  blockedRespCycles := blockedRespCyclesNext
  blockedResponses := blockedResponsesNext
  io.omegaBlockedCycles := blockedCycles
  io.omegaBlockedRespCycles := blockedRespCycles
  io.omegaBlockedResponses := blockedResponses

  XSPerfAccumulate(s"CUTE_${nameContext}_omega_blocked_cycles", blockedCycleIncrement)
  XSPerfAccumulate(s"CUTE_${nameContext}_omega_blocked_resp_cycles", blockedRespCycleIncrement)
  XSPerfAccumulate(s"CUTE_${nameContext}_omega_blocked_responses", blockedResponseIncrement)



  val bankQueues = Seq.tabulate(bankCount) { bank =>
    Module(new Queue(new RoutedResponse(dataWidth, sourceIdWidth), queueDepth, pipe = true))
      .suggestName(s"${nameContext}_bank${bank}_resp_queue")
  }

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

  val bridgeInValidMask = VecInit(io.in.map(_.valid)).asUInt
  val bridgeInReadyMask = VecInit(io.in.map(_.ready)).asUInt
  val bridgeInFireMask = VecInit(io.in.map(_.fire)).asUInt
  val omegaInFireMask = VecInit(router.io.in.map(_.fire)).asUInt
  val omegaOutFireMask = VecInit(router.io.out.map(_.fire)).asUInt
  val bankValidMask = VecInit(io.out.map(_.valid)).asUInt
  val bankReadyMask = VecInit(io.out.map(_.ready)).asUInt
  val bankFireMask = VecInit(io.out.map(_.fire)).asUInt

}
