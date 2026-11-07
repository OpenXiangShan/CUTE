package cute

import chisel3._
import chiseltest._
import difftest.util.MatrixHash
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer

class MatrixHashConcurrentSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "concurrent CRC32C MatrixHash writes"

  private case class Write(port: Int, bank: Int, address: Int, data: BigInt, mask: Int = 65535)
  private val polynomial = BigInt("82f63b78", 16)

  private def reference(writes: Seq[Write]): (BigInt, Int) = {
    val symbols = Array.fill(128)(0)
    var count = 0
    for (w <- writes; byte <- 0 until 16 if (w.mask & (1 << byte)) != 0) {
      val offset = (w.address * 2 + w.bank) * 16 + byte
      symbols(offset) ^= 256 | ((w.data >> (byte * 8)) & 255).toInt
      count += 1
    }
    var crc = BigInt("ffffffff", 16)
    for (symbol <- symbols.reverse; bit <- 8 to 0 by -1) {
      val feedback = crc.testBit(0) != ((symbol & (1 << bit)) != 0)
      crc = (crc >> 1) ^ (if (feedback) polynomial else BigInt(0))
    }
    (crc ^ BigInt("ffffffff", 16), count)
  }

  for (ports <- Seq(1, 2)) {
    it should s"include final-cycle writes and isolate back-to-back instructions with $ports ports" in {
      test(if (ports == 1) new MatrixHash(2, 16, 128, 16) else new MatrixHash(2, 16, 128, 16, ports))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          assert(dut.io.out.hashLo.getWidth == 32)
          assert(!dut.io.out.elements.contains("hashHi"))
          val expected = ArrayBuffer.empty[(BigInt, Int, BigInt)]
          val received = ArrayBuffer.empty[(BigInt, Int, BigInt)]
          def sample(): Unit = {
            if (dut.io.out.valid.peek().litToBoolean) {
              val hash = dut.io.out.hashLo.peek().litValue
              received += ((hash, dut.io.out.byteCount.peek().litValue.toInt, dut.io.out.pc.peek().litValue))
              dut.io.out.index.expect(2.U)
              dut.io.out.coreid.expect(0.U)
            }
          }
          def step(writes: Seq[Write], finish: Boolean = false, pc: Int = 0): Unit = {
            for (port <- 0 until ports) {
              val event = dut.io.in(port)
              event.valid.poke((writes.exists(_.port == port) || (port == 0 && finish)).B)
              event.finish.poke((port == 0 && finish).B)
              event.pc.poke(pc.U)
              event.coreid.poke(0.U)
              event.index.poke(2.U)
              val bankValid = writes.filter(_.port == port).foldLeft(BigInt(0))((bits, write) => bits.setBit(write.bank))
              event.bankValid.poke(bankValid.U)
              for (bank <- 0 until 2) {
                val write = writes.find(w => w.port == port && w.bank == bank)
                event.bankAddr(bank).poke(write.map(_.address).getOrElse(0).U)
                event.bankMask.get(bank).poke(write.map(_.mask).getOrElse(0).U)
                for (word <- 0 until 2) {
                  val data = (write.map(_.data).getOrElse(BigInt(0)) >> (word * 64)) & ((BigInt(1) << 64) - 1)
                  event.data(bank * 2 + word).poke(data.U)
                }
              }
            }
            sample()
            dut.clock.step()
          }
          step(Seq.empty)
          val first = Seq(Write(ports - 1, 0, 0, BigInt("1020304050607080", 16)))
          step(first)
          val finalBeat = Seq(Write(0, 1, 1, BigInt("ffeeddccbbaa9988", 16))) ++
            (if (ports == 2) Seq(Write(1, 0, 2, BigInt("123456789abcdef0", 16), 0x55)) else Seq.empty)
          val (firstHash, firstCount) = reference(first ++ finalBeat)
          expected += ((firstHash, firstCount, BigInt(0x1000)))
          step(finalBeat, finish = true, pc = 0x1000)
          // The next instruction finishes immediately, with disjoint masks on
          // the same entry when both ports are present.
          val second = Seq(Write(0, 0, 3, BigInt("0011223344556677", 16), 0x0f)) ++
            (if (ports == 2) Seq(Write(1, 0, 3, BigInt("8899aabbccddeeff", 16), 0xf0)) else Seq.empty)
          val (secondHash, secondCount) = reference(second)
          expected += ((secondHash, secondCount, BigInt(0x2000)))
          step(second, finish = true, pc = 0x2000)
          // Finish without a write still produces one empty completion.
          val (emptyHash, emptyCount) = reference(Seq.empty)
          expected += ((emptyHash, emptyCount, BigInt(0x3000)))
          step(Seq.empty, finish = true, pc = 0x3000)
          for (_ <- 0 until 8) step(Seq.empty)
          assert(received == expected, s"completion mismatch: $received != $expected")
        }
    }
  }
}
