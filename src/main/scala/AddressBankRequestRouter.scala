package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

object IdealL2Banking {
  val BankCount = 8
  val BankLowBit = 6
  val BankHighBit = 8

  def bankOf(address: UInt): UInt = address(BankHighBit, BankLowBit)

  def bankOf(address: UInt, bankCount: Int): UInt = {
    require(isPow2(bankCount) && bankCount >= 1 && bankCount <= BankCount)
    if (bankCount == 1) 0.U(1.W)
    else if (bankCount == BankCount) bankOf(address)
    else address(BankLowBit + log2Ceil(bankCount) - 1, BankLowBit)
  }
}

/**
  * Routes complete memory requests to the ideal L2 bank selected by address[8:6].
  * Each input is accepted by exactly one output, and each bank accepts at most
  * one request per cycle. RR arbitration prevents a busy bank from starving an
  * input indefinitely.
  */
class AddressBankRequestRouter(channelCount: Int)(implicit p: Parameters) extends CuteModule {
  require(isPow2(channelCount) && channelCount >= 1 && channelCount <= IdealL2Banking.BankCount,
    s"ideal L2 request routing requires 1 to ${IdealL2Banking.BankCount} power-of-two channels, got $channelCount")

  val io = IO(new Bundle {
    val in = Flipped(Vec(channelCount, Decoupled(new MMURequestIO)))
    val out = Vec(channelCount, Decoupled(new MMURequestIO))
  })

  val inputReady = WireInit(VecInit(Seq.fill(channelCount)(false.B)))
  val arbiters = Seq.tabulate(channelCount) { bank =>
    val arbiter = Module(new RRArbiter(new MMURequestIO, channelCount))
      .suggestName(s"l2_bank${bank}_request_arbiter")

    for (input <- 0 until channelCount) {
      val targetsBank = IdealL2Banking.bankOf(io.in(input).bits.RequestAddr, channelCount) === bank.U
      arbiter.io.in(input).valid := io.in(input).valid && targetsBank
      arbiter.io.in(input).bits := io.in(input).bits
      when(targetsBank) {
        inputReady(input) := arbiter.io.in(input).ready
      }
    }
    io.out(bank) <> arbiter.io.out
    arbiter
  }

  for (input <- 0 until channelCount) {
    io.in(input).ready := inputReady(input)
    when(io.in(input).valid) {
      assert(IdealL2Banking.bankOf(io.in(input).bits.RequestAddr, channelCount) < channelCount.U,
        s"request input $input selected an invalid ideal L2 bank")
    }
    assert(PopCount(arbiters.map(_.io.in(input).fire)) <= 1.U,
      s"request input $input was accepted by multiple ideal L2 banks")
  }
}
