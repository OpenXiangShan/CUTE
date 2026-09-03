package cute

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import xscache.coupledL2.MatrixPrefetchStream
import xscache.coupledL2.prefetch.MatrixPrefetchAddressGenerator

class MatrixPrefetchAddressGeneratorSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MatrixPrefetchAddressGenerator"

  private def clearDescriptor(dut: MatrixPrefetchAddressGenerator): Unit = {
    dut.io.load.bits.taskId.poke(0.U)
    dut.io.load.bits.stream.poke(0.U)
    dut.io.load.bits.baseAddr.poke(0.U)
    dut.io.load.bits.outerStride.poke(0.U)
    dut.io.load.bits.outerCount.poke(0.U)
    dut.io.load.bits.innerCount.poke(0.U)
    dut.io.load.bits.rowBytes.poke(0.U)
    dut.io.load.bits.groupWidth.poke(0.U)
    dut.io.load.bits.transpose.poke(false.B)
    dut.io.load.bits.pc.poke(0.U)
  }

  private def initialize(dut: MatrixPrefetchAddressGenerator): Unit = {
    dut.io.load.valid.poke(false.B)
    clearDescriptor(dut)
    dut.io.step.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }

  private def loadDescriptor(
    dut: MatrixPrefetchAddressGenerator,
    base: BigInt,
    stride: BigInt,
    rows: Int,
    rowBytes: Int
  ): Unit = {
    clearDescriptor(dut)
    dut.io.load.bits.taskId.poke(7.U)
    dut.io.load.bits.stream.poke(MatrixPrefetchStream.a)
    dut.io.load.bits.baseAddr.poke(base.U)
    dut.io.load.bits.outerStride.poke(stride.U)
    dut.io.load.bits.outerCount.poke(rows.U)
    dut.io.load.bits.innerCount.poke(0.U)
    dut.io.load.bits.rowBytes.poke(rowBytes.U)
    dut.io.load.bits.groupWidth.poke(64.U)
    dut.io.load.bits.transpose.poke(false.B)
    dut.io.load.bits.pc.poke(0.U)
    dut.io.load.valid.poke(true.B)
    dut.clock.step()
    dut.io.load.valid.poke(false.B)
  }

  it should "enumerate exactly the cache lines touched by unaligned strided rows" in {
    test(new MatrixPrefetchAddressGenerator(blockBytes = 64)) { dut =>
      initialize(dut)
      loadDescriptor(dut, base = 0x1030, stride = 0x180, rows = 2, rowBytes = 0x90)

      val expected = Seq(0x1000, 0x1180, 0x1040, 0x11c0, 0x1080, 0x1200)
      expected.foreach { address =>
        dut.io.done.expect(false.B)
        dut.io.valid.expect(true.B)
        dut.io.address.expect(address.U)
        dut.io.step.poke(true.B)
        dut.clock.step()
        dut.io.step.poke(false.B)
      }
      dut.io.done.expect(true.B)
    }
  }

  it should "hold its address under backpressure and stop after the final line" in {
    test(new MatrixPrefetchAddressGenerator(blockBytes = 64)) { dut =>
      initialize(dut)
      loadDescriptor(dut, base = 0x2048, stride = 0x400, rows = 1, rowBytes = 16)

      dut.io.address.expect(0x2040.U)
      dut.clock.step(3)
      dut.io.address.expect(0x2040.U)
      dut.io.done.expect(false.B)

      dut.io.step.poke(true.B)
      dut.clock.step()
      dut.io.step.poke(false.B)
      dut.io.done.expect(true.B)
    }
  }

  it should "skip non-existent line candidates when row alignments differ" in {
    test(new MatrixPrefetchAddressGenerator(blockBytes = 64)) { dut =>
      initialize(dut)
      loadDescriptor(dut, base = 0x1030, stride = 0x50, rows = 2, rowBytes = 0x20)

      Seq(0x1000, 0x1080, 0x1040).foreach { address =>
        dut.io.valid.expect(true.B)
        dut.io.address.expect(address.U)
        dut.io.step.poke(true.B)
        dut.clock.step()
        dut.io.step.poke(false.B)
      }

      dut.io.valid.expect(false.B)
      dut.clock.step()
      dut.io.done.expect(true.B)
    }
  }
}
