package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class PFEventAme(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val csrW = Flipped(ValidIO(new AmeCSRWriteBundle))
    val hpmevent = Output(Vec(AmeCounterNum, UInt(64.W)))
  })

  private val ameMhpmeventBase = "hBC3".U(12.W)
  private val perfEvents = RegInit(VecInit(Seq.fill(AmeCounterNum)(0.U(64.W))))
  private val lastAddr = ameMhpmeventBase + (AmeCounterNum - 1).U
  private val hit = io.csrW.valid && io.csrW.bits.addr >= ameMhpmeventBase && io.csrW.bits.addr <= lastAddr
  private val idx = if (AmeCounterNum == 1) 0.U else (io.csrW.bits.addr - ameMhpmeventBase)(log2Ceil(AmeCounterNum) - 1, 0)

  when(hit) {
    perfEvents(idx) := io.csrW.bits.data
  }

  io.hpmevent := perfEvents
}

class HPerfCounterAme(val eventNum: Int) extends Module {
  require(eventNum > 0, "eventNum should be larger than 0")

  val io = IO(new Bundle {
    val hpmEvent = Input(UInt(64.W))
    val events = Input(Vec(eventNum, new PerfEventAme))
    val selected = Output(new PerfEventAme)
  })

  private def eventValueById(id: UInt): UInt = {
    val idx = id(log2Ceil(eventNum) - 1, 0)
    Mux(id < eventNum.U, io.events(idx).value, 0.U(8.W))
  }

  private def combineEvents(a: UInt, b: UInt, op: UInt): UInt = {
    val addRes = (a +& b)(7, 0)
    Mux(op(0), a & b,
      Mux(op(1), a ^ b,
        Mux(op(2), addRes, a | b)))
  }

  private val event0 = eventValueById(io.hpmEvent(9, 0))
  private val event1 = eventValueById(io.hpmEvent(19, 10))
  private val event2 = eventValueById(io.hpmEvent(29, 20))
  private val event3 = eventValueById(io.hpmEvent(39, 30))
  private val op0 = io.hpmEvent(44, 40)
  private val op1 = io.hpmEvent(49, 45)
  private val op2 = io.hpmEvent(54, 50)

  private val result0 = combineEvents(event0, event1, op0)
  private val result1 = combineEvents(event2, event3, op1)
  private val result2 = combineEvents(result0, result1, op2)

  io.selected.value := result2
}

class HPerfMonitorAme(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val hpmevent = Input(Vec(AmeCounterNum, UInt(64.W)))
    val events = Input(Vec(20, new PerfEventAme))
    val perfEventsAme = Output(Vec(AmeCounterNum, new PerfEventAme))
  })

  private val counters = Seq.fill(AmeCounterNum)(Module(new HPerfCounterAme(20)))
  counters.zipWithIndex.foreach { case (counter, idx) =>
    counter.io.hpmEvent := io.hpmevent(idx)
    counter.io.events := io.events
    io.perfEventsAme(idx) := counter.io.selected
  }
}

class CUTEPMU(implicit p: Parameters) extends CuteModule {
  val io = IO(new Bundle {
    val fromCSR = Flipped(new AmePerfFromCSRIO)
    val taskProbe = Input(new TaskControllerPerfProbe)
    val mmuProbe = Input(new LocalMMUPerfProbe)
    val toCore = Output(new AmePerfToCoreIO)
  })

  val pfEvent = Module(new PFEventAme)
  val hPerf = Module(new HPerfMonitorAme)

  val eventPool = Wire(Vec(20, new PerfEventAme))
  eventPool.foreach(_.value := 0.U)

  eventPool(1).value := io.taskProbe.loadADone
  eventPool(2).value := io.taskProbe.loadBDone
  eventPool(3).value := io.taskProbe.loadCDone
  eventPool(4).value := io.taskProbe.storeDone
  eventPool(5).value := io.taskProbe.compDone
  eventPool(6).value := io.taskProbe.releaseDone
  eventPool(7).value := io.taskProbe.mmaNonfpDone
  eventPool(8).value := io.taskProbe.mmaFp16Done
  eventPool(9).value := io.taskProbe.mmaBf16Done
  eventPool(10).value := io.taskProbe.mmaTf32Done
  eventPool(11).value := io.taskProbe.amlActive
  eventPool(12).value := io.taskProbe.bmlActive
  eventPool(13).value := io.taskProbe.cmlLoadActive
  eventPool(14).value := io.taskProbe.mteActive
  eventPool(15).value := io.taskProbe.cmlStoreActive
  eventPool(16).value := io.mmuProbe.rdReq
  eventPool(17).value := io.mmuProbe.wrReq
  eventPool(18).value := io.mmuProbe.rdBytesReq
  eventPool(19).value := io.mmuProbe.wrBytesReq

  pfEvent.io.csrW := io.fromCSR.csrW
  hPerf.io.hpmevent := pfEvent.io.hpmevent
  hPerf.io.events := eventPool

  io.toCore.perfEventsAme := hPerf.io.perfEventsAme
  io.toCore.fixedPerfAme.foreach(_.value := 0.U)
  io.toCore.fixedPerfAme(0).value := io.taskProbe.ownedWork
  io.toCore.fixedPerfAme(1).value := io.taskProbe.retire
}
