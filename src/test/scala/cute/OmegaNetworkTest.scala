package cute

import chisel3._
import chisel3.util._

// ---------------------------------------------------------------------------
// Unified log helper for test modules
// ---------------------------------------------------------------------------
trait TestLogHelper {
  def timeStamp: UInt
  def log(enable: Boolean, prefix: String, msg: => Printable): Unit = {
    if (enable) printf(cf"[T${timeStamp}][$prefix] " + msg + "\n")
  }
}

// ---------------------------------------------------------------------------
// Dummy L2 Cache
// ---------------------------------------------------------------------------
class DummyL2Cache(
  val n: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val addrWidth: Int,
  val regAddrWidth: Int,
  val queueDepth: Int = 8,
  val debugEnable: Boolean = false
) extends Module with TestLogHelper {
  require(n >= 1)
  require(queueDepth >= 2)

  def timeStamp = io.timeStamp

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val req = Flipped(Vec(n, Decoupled(new Bundle {
      val addr = UInt(addrWidth.W)
      val sourceId = UInt(sourceIdWidth.W)
    })))
    val resp = Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth)))
  })

  val bankMask = if (n > 1) (n - 1).U else 1.U

  // ---- LFSR for pseudo-random delay targets ----
  val lfsr = RegInit(0xACE1.U(16.W))
  lfsr := Cat(lfsr(0) ^ lfsr(2) ^ lfsr(3) ^ lfsr(5), lfsr(15, 1))

  class Entry extends Bundle {
    val addr = UInt(addrWidth.W)
    val sourceId = UInt(sourceIdWidth.W)
  }

  // ---- Per-channel request queues using Chisel Queue ----
  val queues = Seq.fill(n)(Module(new Queue(new Entry, queueDepth)))

  for (i <- 0 until n) {
    queues(i).io.enq.valid := io.req(i).valid
    queues(i).io.enq.bits.addr := io.req(i).bits.addr
    queues(i).io.enq.bits.sourceId := io.req(i).bits.sourceId
    io.req(i).ready := queues(i).io.enq.ready

    when(io.req(i).fire) {
      log(debugEnable, "DummyL2", cf"enqueue ch=$i addr=0x${io.req(i).bits.addr}%x sourceId=0x${io.req(i).bits.sourceId}%x")
    }
  }

  // ---- Per-queue delay state ----
  val delayTarget = Seq.fill(n)(RegInit(0.U(4.W)))
  val delayCnt    = Seq.fill(n)(RegInit(0.U(4.W)))

  // ---- Compute each queue's target channel ----
  val targetCh = Wire(Vec(n, UInt(log2Ceil(n max 2).W)))
  val readyToSend = Wire(Vec(n, Bool()))

  for (i <- 0 until n) {
    val bank = if (n > 1) {
      (queues(i).io.deq.bits.addr >> regAddrWidth) & bankMask
    } else {
      0.U(1.W)
    }
    targetCh(i) := bank
    readyToSend(i) := queues(i).io.deq.valid && delayCnt(i) === delayTarget(i)
  }

  // ---- Per-output-channel arbitration using RRArbiter ----
  val arbiterReady = Seq.fill(n)(Wire(Vec(n, Bool())))

  for (ch <- 0 until n) {
    val arb = Module(new RRArbiter(new Entry, n))

    for (i <- 0 until n) {
      arb.io.in(i).valid := readyToSend(i) && targetCh(i) === ch.U
      arb.io.in(i).bits.addr := queues(i).io.deq.bits.addr
      arb.io.in(i).bits.sourceId := queues(i).io.deq.bits.sourceId
      arbiterReady(ch)(i) := arb.io.in(i).ready
    }

    io.resp(ch).valid := arb.io.out.valid
    io.resp(ch).bits.data := arb.io.out.bits.addr
    io.resp(ch).bits.sourceId := arb.io.out.bits.sourceId
    arb.io.out.ready := io.resp(ch).ready

    when(arb.io.out.fire) {
      val winnerIdx = PriorityEncoder(VecInit((0 until n).map(i => arb.io.in(i).valid)).asUInt)
      log(debugEnable, "DummyL2", cf"dequeue winner=$winnerIdx -> respCh=$ch addr=0x${arb.io.out.bits.addr}%x sourceId=0x${arb.io.out.bits.sourceId}%x")
    }
  }

  // ---- Connect deq.ready back to each queue (OR across all arbiters) ----
  for (i <- 0 until n) {
    queues(i).io.deq.ready := VecInit(arbiterReady.map(_(i))).asUInt.orR
  }

  // ---- Assertions: at most one deq fire and at most one resp fire per cycle ----
  val deqFires = VecInit(queues.map(_.io.deq.fire)).asUInt
  val respFires = VecInit((0 until n).map(ch => io.resp(ch).fire)).asUInt
  assert(PopCount(deqFires) <= 1.U, "DummyL2: multiple queue deqs fired in same cycle")
  assert(PopCount(respFires) <= 1.U, "DummyL2: multiple resp channels fired in same cycle")

  // ---- Advance delay counters for non-winner queues ----
  for (i <- 0 until n) {
    val isWinnerAnyCh = VecInit((0 until n).map(ch => {
      val wants = readyToSend(i) && targetCh(i) === ch.U
      val winner = PriorityEncoder(VecInit((0 until n).map(j => readyToSend(j) && targetCh(j) === ch.U)).asUInt)
      wants && winner === i.U && io.resp(ch).ready
    })).asUInt.orR

    when(queues(i).io.deq.valid && delayCnt(i) < delayTarget(i) && !isWinnerAnyCh) {
      delayCnt(i) := delayCnt(i) + 1.U
    }
  }
}

// ---------------------------------------------------------------------------
// Dummy MemoryLoader
// ---------------------------------------------------------------------------
class DummyMemoryLoader(
  val n: Int,
  val dataWidth: Int,
  val sourceIdWidth: Int,
  val bankIdWidth: Int,
  val regAddrWidth: Int,
  val totalRequests: Int,
  val bankIdOffset: Int = 0,
  val smokeTest: Boolean = false,
  val sameSendBankMode: Boolean = false,
  val debugEnable: Boolean = false
) extends Module with TestLogHelper {
  require(n >= 1)
  require(totalRequests > 0)

  def timeStamp = io.timeStamp

  val io = IO(new Bundle {
    val timeStamp = Input(UInt(64.W))
    val req = Vec(n, Decoupled(new Bundle {
      val addr = UInt((bankIdWidth + regAddrWidth).W)
      val sourceId = UInt(sourceIdWidth.W)
    }))
    val resp = Flipped(Vec(n, Decoupled(new RoutedResponse(dataWidth, sourceIdWidth))))
    val done = Output(Bool())
    val error = Output(Bool())
    val errorBank = Output(UInt(bankIdWidth.W))
    val errorRegAddr = Output(UInt(regAddrWidth.W))
  })

  val bankIdxWidth = log2Ceil(n)

  // ---- LFSR for random addrBank and stall ----
  val lfsr = RegInit(0xBABE.U(16.W))
  lfsr := Cat(lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4), lfsr(15, 1))

  // ---- Per-bank request/response counters ----
  val sendCount = RegInit(VecInit(Seq.fill(n)(0.U(log2Ceil(totalRequests + 1).W))))
  val recvCount = RegInit(VecInit(Seq.fill(n)(0.U(log2Ceil(totalRequests + 1).W))))

  val errorReg  = RegInit(false.B)
  val errorBankReg = RegInit(0.U(bankIdWidth.W))
  val errorRegAddrReg = RegInit(0.U(regAddrWidth.W))
  io.error := errorReg
  io.errorBank := errorBankReg
  io.errorRegAddr := errorRegAddrReg
  io.done  := false.B

  // ---- Request generation state machine ----
  val s_idle :: s_send :: s_wait :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val reqLimit = if (sameSendBankMode) totalRequests else n * totalRequests
  val reqCount    = RegInit(0.U(log2Ceil(reqLimit + 1).W))
  val sendBank    = RegInit(0.U(bankIdxWidth.W))
  val sendRegAddr = RegInit(0.U(regAddrWidth.W))

  for (i <- 0 until n) {
    io.req(i).valid := false.B
    io.req(i).bits  := DontCare
  }

  val ch = if (sameSendBankMode) 0.U(bankIdxWidth.W) else sendBank
  val sbBank = if (sameSendBankMode) 0.U(bankIdWidth.W) else sendBank

  val sourceId = Cat(sendBank, sendRegAddr)

  // addrBank: smokeTest 时与 sendBank 一致；sameSendBankMode 时由 LFSR 随机生成；
  // 正常模式下由 sendBank/sendRegAddr 混合生成以制造跨 channel 场景
  val addrBank = if (smokeTest) {
    sendBank
  } else if (sameSendBankMode) {
    lfsr(bankIdWidth - 1, 0)
  } else {
    (lfsr + (sendBank * 13.U) + (sendRegAddr * 7.U))(bankIdWidth - 1, 0)
  }
  val addr = Cat(addrBank, sendRegAddr)

  switch(state) {
    is(s_idle) { state := s_send }

    is(s_send) {
      io.req(ch).valid := true.B
      io.req(ch).bits.addr := addr
      io.req(ch).bits.sourceId := sourceId

      when(io.req(ch).fire) {
        sendCount(sbBank) := sendCount(sbBank) + 1.U

        log(debugEnable, "DummyML", cf"send ch=$ch bank=$sendBank regAddr=$sendRegAddr addrBank=$addrBank addr=0x${addr}%x sourceId=0x${sourceId}%x sendCnt=${sendCount(sbBank) + 1.U}")

        if (sameSendBankMode) {
          sendRegAddr := sendRegAddr + 1.U
          // sendBank stays 0
        } else {
          val nextBank = Mux(sendBank === (n - 1).U, 0.U, sendBank + 1.U)
          val nextRegAddr = Mux(sendBank === (n - 1).U, sendRegAddr + 1.U, sendRegAddr)
          sendRegAddr := nextRegAddr
          sendBank := nextBank
        }
        reqCount := reqCount + 1.U

        when(reqCount === (reqLimit - 1).U) {
          state := s_wait
        }
      }
    }

    is(s_wait) {
      val allSent = if (sameSendBankMode) {
        sendCount(0) === totalRequests.U
      } else {
        VecInit((0 until n).map(b => sendCount(b.U) === totalRequests.U)).asUInt.andR
      }
      val allRecv = if (sameSendBankMode) {
        recvCount(0) === totalRequests.U
      } else {
        VecInit((0 until n).map(b => recvCount(b.U) === totalRequests.U)).asUInt.andR
      }
      when(allSent && allRecv) {
        state := s_done
      }
    }

    is(s_done) { io.done := true.B }
  }

  // ---- Response handling with random per-channel ready stalls ----
  val stallCnt = RegInit(VecInit(Seq.fill(n)(0.U(3.W))))

  for (i <- 0 until n) {
    io.resp(i).ready := stallCnt(i) === 0.U

    when(io.resp(i).fire) {
      val bankId  = io.resp(i).bits.sourceId(bankIdOffset + bankIdWidth - 1, bankIdOffset)

      log(debugEnable, "DummyML", cf"recv ch=$i bank=$bankId recvCnt=${recvCount(bankId) + 1.U}")

      when(recvCount(bankId) + 1.U > sendCount(bankId)) {
        when(!errorReg) {
          errorReg := true.B
          errorBankReg := bankId
          errorRegAddrReg := io.resp(i).bits.sourceId(bankIdOffset - 1, 0)
        }
        log(debugEnable, "DummyML", cf"ERROR excess response ch=$i bank=$bankId recv=${recvCount(bankId) + 1.U} send=${sendCount(bankId)}")
      }

      recvCount(bankId) := recvCount(bankId) + 1.U

      stallCnt(i) := (lfsr + (i * 3).U)(2, 0)
    }.elsewhen(stallCnt(i) > 0.U) {
      stallCnt(i) := stallCnt(i) - 1.U
    }
  }
}

// ---------------------------------------------------------------------------
// Smoke Test Top: ML -> L2 -> ML (no router, addrBank == sendBank)
// ---------------------------------------------------------------------------
class SmokeTestTop extends Module {
  val n = 8
  val dataWidth = 512
  val sourceIdWidth = 64
  val bankIdWidth = 3
  val regAddrWidth = 8
  val addrWidth = bankIdWidth + regAddrWidth
  val totalRequests = 256

  val io = IO(new Bundle {
    val done  = Output(Bool())
    val error = Output(Bool())
    val errorBank = Output(UInt(bankIdWidth.W))
    val errorRegAddr = Output(UInt(regAddrWidth.W))
  })

  val timeStamp = RegInit(0.U(64.W))
  timeStamp := timeStamp + 1.U

  val dummyLoader = Module(new DummyMemoryLoader(
    n, dataWidth, sourceIdWidth, bankIdWidth, regAddrWidth, totalRequests,
    bankIdOffset = regAddrWidth, smokeTest = true, debugEnable = false
  ))
  val dummyL2 = Module(new DummyL2Cache(
    n, dataWidth, sourceIdWidth, addrWidth, regAddrWidth, queueDepth = 8, debugEnable = false
  ))

  dummyLoader.io.timeStamp := timeStamp
  dummyL2.io.timeStamp := timeStamp

  // Loader request -> L2 request
  for (i <- 0 until n) {
    dummyL2.io.req(i).valid := dummyLoader.io.req(i).valid
    dummyL2.io.req(i).bits.addr := dummyLoader.io.req(i).bits.addr
    dummyL2.io.req(i).bits.sourceId := dummyLoader.io.req(i).bits.sourceId
    dummyLoader.io.req(i).ready := dummyL2.io.req(i).ready
  }

  // L2 response -> Loader response (direct connect, channel to channel)
  for (i <- 0 until n) {
    dummyLoader.io.resp(i).valid := dummyL2.io.resp(i).valid
    dummyLoader.io.resp(i).bits.data := dummyL2.io.resp(i).bits.data
    dummyLoader.io.resp(i).bits.sourceId := dummyL2.io.resp(i).bits.sourceId
    dummyL2.io.resp(i).ready := dummyLoader.io.resp(i).ready
  }

  io.done  := dummyLoader.io.done
  io.error := dummyLoader.io.error
  io.errorBank := dummyLoader.io.errorBank
  io.errorRegAddr := dummyLoader.io.errorRegAddr
}

// ---------------------------------------------------------------------------
// Omega Router Test Top: ML -> L2 -> Router -> ML
// ---------------------------------------------------------------------------
class OmegaRouterTestTop(
  val sameSendBankMode: Boolean = false,
  val totalRequests: Int = 100
) extends Module {
  val n = 8
  val dataWidth = 512
  val sourceIdWidth = 64
  val bankIdWidth = 3
  val regAddrWidth = 8
  val addrWidth = bankIdWidth + regAddrWidth

  val io = IO(new Bundle {
    val done  = Output(Bool())
    val error = Output(Bool())
    val errorBank = Output(UInt(bankIdWidth.W))
    val errorRegAddr = Output(UInt(regAddrWidth.W))
  })

  val timeStamp = RegInit(0.U(64.W))
  timeStamp := timeStamp + 1.U

  val dummyLoader = Module(new DummyMemoryLoader(
    n, dataWidth, sourceIdWidth, bankIdWidth, regAddrWidth, totalRequests,
    bankIdOffset = regAddrWidth, smokeTest = false,
    sameSendBankMode = sameSendBankMode, debugEnable = true
  ))
  val dummyL2 = Module(new DummyL2Cache(
    n, dataWidth, sourceIdWidth, addrWidth, regAddrWidth, queueDepth = 8, debugEnable = true
  ))
  val router = Module(new OmegaResponseRouter(
    n, dataWidth, sourceIdWidth, bankIdWidth,
    bankIdOffset = regAddrWidth, debugEnable = true
  ))

  dummyLoader.io.timeStamp := timeStamp
  dummyL2.io.timeStamp := timeStamp
  router.io.timeStamp := timeStamp

  // Loader request -> L2 request
  for (i <- 0 until n) {
    dummyL2.io.req(i).valid := dummyLoader.io.req(i).valid
    dummyL2.io.req(i).bits.addr := dummyLoader.io.req(i).bits.addr
    dummyL2.io.req(i).bits.sourceId := dummyLoader.io.req(i).bits.sourceId
    dummyLoader.io.req(i).ready := dummyL2.io.req(i).ready
  }

  // L2 response -> Router input
  for (i <- 0 until n) {
    router.io.in(i) <> dummyL2.io.resp(i)
  }

  // Router output -> Loader response
  for (i <- 0 until n) {
    dummyLoader.io.resp(i) <> router.io.out(i)
  }

  io.done  := dummyLoader.io.done
  io.error := dummyLoader.io.error
  io.errorBank := dummyLoader.io.errorBank
  io.errorRegAddr := dummyLoader.io.errorRegAddr
}

class SameSendBankRouterTestTop extends OmegaRouterTestTop(sameSendBankMode = true)