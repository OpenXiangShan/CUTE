package cute

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class AddressBankRequestRouterSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LoaderBankedWriteTestUtils.parameters

  behavior of "address-banked MultiChannel requests"

  private def clearRequest(request: MMURequestIO): Unit = {
    request.RequestAddr.poke(0.U)
    request.RequestData.poke(0.U)
    request.RequestMask.poke(0.U)
    request.RequestConherent.poke(false.B)
    request.RequestSourceID.poke(0.U)
    request.RequestType_isWrite.poke(false.B)
    request.UseAllocatedSourceID.poke(false.B)
    request.MatrixIsAcc.poke(false.B)
    request.isA.poke(false.B)
  }

  it should "use the same bank bits and one-request-per-bank rule in every memory mode" in {
    for {
      bankCount <- Seq(1, 2, 4, 8)
      transparent <- Seq(false, true)
    } {
      implicit val p: Parameters = LocalMMUSchedulerTestUtils.parameters(
        l2Banks = bankCount,
        transparent = transparent
      )
      test(new AddressBankRequestRouter(bankCount)) { dut =>
        for (input <- 0 until bankCount) {
          dut.io.in(input).valid.poke(true.B)
          clearRequest(dut.io.in(input).bits)
          val targetBank = (input * 3 + 1) % bankCount
          dut.io.in(input).bits.RequestAddr.poke((0x1000 + targetBank * 64).U)
          dut.io.in(input).bits.RequestSourceID.poke(input.U)
        }
        dut.io.out.foreach(_.ready.poke(true.B))
        val outputSources = (0 until bankCount).map { bank =>
          dut.io.out(bank).valid.expect(true.B)
          dut.io.out(bank).bits.RequestAddr.expect((0x1000 + bank * 64).U)
          dut.io.out(bank).bits.RequestSourceID.peek().litValue.toInt
        }
        assert(outputSources.distinct.size == bankCount,
          s"bankCount=$bankCount transparent=$transparent duplicated an input owner")
        dut.io.in.foreach(_.ready.expect(true.B))
      }
    }
  }

  it should "route every request exactly once through address bits 8 through 6" in {
    implicit val p: Parameters = parameters
    test(new AddressBankRequestRouter(8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val pending = Array.tabulate(8) { input =>
        mutable.Queue((0 until 4).map(index =>
          (BigInt(0x1000 + (((input * 3 + index) & 7) << 6)), BigInt(input * 16 + index))): _*)
      }
      val received = mutable.ArrayBuffer.empty[(Int, BigInt)]

      dut.io.out.foreach(_.ready.poke(true.B))
      dut.io.in.foreach { input =>
        input.valid.poke(false.B)
        clearRequest(input.bits)
      }
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      var cycles = 0
      while (pending.exists(_.nonEmpty) && cycles < 100) {
        for (input <- 0 until 8) {
          dut.io.in(input).valid.poke(pending(input).nonEmpty.B)
          pending(input).headOption.foreach { case (address, source) =>
            dut.io.in(input).bits.RequestAddr.poke(address.U)
            dut.io.in(input).bits.RequestSourceID.poke(source.U)
          }
        }

        for (bank <- 0 until 8) {
          if (dut.io.out(bank).valid.peek().litToBoolean) {
            val address = dut.io.out(bank).bits.RequestAddr.peek().litValue
            val source = dut.io.out(bank).bits.RequestSourceID.peek().litValue
            assert(((address >> 6) & 7) == bank)
            received += bank -> source
          }
        }
        for (input <- 0 until 8) {
          if (dut.io.in(input).valid.peek().litToBoolean && dut.io.in(input).ready.peek().litToBoolean) {
            pending(input).dequeue()
          }
        }
        dut.clock.step()
        cycles += 1
      }

      assert(pending.forall(_.isEmpty), s"requests did not drain after $cycles cycles")
      assert(received.size == 32)
      assert(received.map(_._2).toSet == (0 until 8).flatMap(input =>
        (0 until 4).map(index => BigInt(input * 16 + index))).toSet)
    }
  }

  it should "emit the complete AML 8-row by 2-column request set" in {
    implicit val p: Parameters = parameters
    test(new MultiChannelsABMemLoader()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val base = BigInt(0x2000)
      val stride = BigInt(128)
      dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr.poke(base.U)
      dut.io.ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M.poke(stride.U)
      dut.io.ConfigInfo.ApplicationTensor_A.dataType.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.HasTail.poke(false.B)
      dut.io.ConfigInfo.ApplicationTensor_A.TailByteMask.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_A.K_Beat_Count.poke(2.U)
      dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
      dut.io.ConfigInfo.MatrixRegTensor_M.poke(8.U)
      dut.io.ConfigInfo.MatrixRegTensor_K.poke(2.U)
      dut.io.ConfigInfo.MatrixRegId.poke(0.U)
      dut.io.ConfigInfo.Conherent.poke(true.B)
      dut.io.ConfigInfo.Is_Transpose.poke(false.B)
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.MicroTaskEndReady.poke(true.B)
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
      dut.io.LocalMMUIO.ConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LocalMMUIO.ConherentRequsetSourceID.bits.poke(0.U)
      dut.io.LocalMMUIO.nonConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LocalMMUIO.nonConherentRequsetSourceID.bits.poke(0.U)
      for (bank <- 0 until 8) {
        dut.io.LocalMMUIO.Request(bank).ready.poke(true.B)
        dut.io.LocalMMUIO.Response(bank).valid.poke(false.B)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseData.poke(0.U)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseSourceID.poke(0.U)
        dut.io.LocalMMUIO.Response(bank).bits.ReseponseConherent.poke(false.B)
      }

      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.ConfigInfo.MicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.MicroTaskValid.poke(false.B)

      val observed = mutable.ArrayBuffer.empty[(BigInt, BigInt)]
      val regAddrWidth = chisel3.util.log2Ceil(dut.ABMatrixRegBankNEntries)
      var cycles = 0
      while (observed.size < 16 && cycles < 40) {
        for (channel <- 0 until 8) {
          val request = dut.io.LocalMMUIO.Request(channel)
          if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
            val address = request.bits.RequestAddr.peek().litValue
            val source = request.bits.RequestSourceID.peek().litValue
            val destination = ((source >> regAddrWidth) & 7).toInt
            assert(destination == channel)
            observed += address -> source
          }
        }
        dut.clock.step()
        cycles += 1
      }

      val expected = (for {
        row <- 0 until 8
        column <- 0 until 2
      } yield {
        val address = base + row * stride + column * dut.outsideDataWidthByte
        val regAddr = column * (dut.outsideDataWidthByte / dut.ABMatrixRegEntryByteSize)
        val source = (BigInt(row) << regAddrWidth) | regAddr
        address -> source
      }).toSet
      assert(observed.size == 16)
      assert(observed.toSet == expected)
    }
  }

  it should "emit the complete CML 8-row by 2-column load request set" in {
    implicit val p: Parameters = parameters
    test(new MultiChannelsCMemLoader()).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val base = BigInt(0x3000)
      val stride = BigInt(128)
      dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr.poke(base.U)
      dut.io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr.poke(base.U)
      dut.io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M.poke(stride.U)
      dut.io.ConfigInfo.ApplicationTensor_C.dataType.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.HasTail.poke(false.B)
      dut.io.ConfigInfo.ApplicationTensor_C.TailByteMask.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_C.N_Beat_Count.poke(2.U)
      dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M.poke(0.U)
      dut.io.ConfigInfo.ApplicationTensor_D.dataType.poke(0.U)
      dut.io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad.poke(false.B)
      dut.io.ConfigInfo.LoadTaskInfo.Is_FullLoad.poke(true.B)
      dut.io.ConfigInfo.Conherent.poke(true.B)
      dut.io.ConfigInfo.Is_Transpose.poke(false.B)
      dut.io.ConfigInfo.MatrixRegTensor_M.poke(8.U)
      dut.io.ConfigInfo.MatrixRegTensor_N.poke(2.U)
      dut.io.ConfigInfo.MatrixRegId.poke(0.U)
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.LoadMicroTaskEndReady.poke(true.B)
      dut.io.ConfigInfo.StoreMicroTaskValid.poke(false.B)
      dut.io.ConfigInfo.StoreMicroTaskEndReady.poke(true.B)
      dut.io.DebugInfo.DebugTimeStampe.poke(0.U)
      dut.io.ToMatrixRegIO.LoadReadWriteResponse.poke(0.U)
      dut.io.ToMatrixRegIO.StoreReadWriteResponse.poke(0.U)
      for (bank <- 0 until 8) {
        dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).valid.poke(false.B)
        dut.io.ToMatrixRegIO.ReadRequestToMatrixReg.ReadResponseData(bank).bits.poke(0.U)
        Seq(dut.io.LoadLocalMMUIO, dut.io.StoreLocalMMUIO).foreach { mmu =>
          mmu.Request(bank).ready.poke(true.B)
          mmu.Response(bank).valid.poke(false.B)
          mmu.Response(bank).bits.ReseponseData.poke(0.U)
          mmu.Response(bank).bits.ReseponseSourceID.poke(0.U)
          mmu.Response(bank).bits.ReseponseConherent.poke(false.B)
        }
      }
      dut.io.LoadLocalMMUIO.ConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LoadLocalMMUIO.ConherentRequsetSourceID.bits.poke(0.U)
      dut.io.LoadLocalMMUIO.nonConherentRequsetSourceID.valid.poke(false.B)
      dut.io.LoadLocalMMUIO.nonConherentRequsetSourceID.bits.poke(0.U)
      dut.io.StoreLocalMMUIO.ConherentRequsetSourceID.valid.poke(false.B)
      dut.io.StoreLocalMMUIO.ConherentRequsetSourceID.bits.poke(0.U)
      dut.io.StoreLocalMMUIO.nonConherentRequsetSourceID.valid.poke(false.B)
      dut.io.StoreLocalMMUIO.nonConherentRequsetSourceID.bits.poke(0.U)

      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(true.B)
      dut.clock.step()
      dut.io.ConfigInfo.LoadMicroTaskValid.poke(false.B)

      val observed = mutable.ArrayBuffer.empty[(BigInt, BigInt)]
      val regAddrWidth = chisel3.util.log2Ceil(dut.CMatrixRegBankNEntries)
      var cycles = 0
      while (observed.size < 16 && cycles < 40) {
        for (channel <- 0 until 8) {
          val request = dut.io.LoadLocalMMUIO.Request(channel)
          if (request.valid.peek().litToBoolean && request.ready.peek().litToBoolean) {
            val address = request.bits.RequestAddr.peek().litValue
            val source = request.bits.RequestSourceID.peek().litValue
            val destination = ((source >> regAddrWidth) & 7).toInt
            assert(destination == channel)
            observed += address -> source
          }
        }
        dut.clock.step()
        cycles += 1
      }

      val expected = (for {
        row <- 0 until 8
        column <- 0 until 2
      } yield {
        val address = base + row * stride + column * dut.outsideDataWidthByte
        val regAddr = column * (dut.outsideDataWidthByte / dut.CMatrixRegEntryByteSize)
        val source = (BigInt(row) << regAddrWidth) | regAddr
        address -> source
      }).toSet
      assert(observed.size == 16)
      assert(observed.toSet == expected)
    }
  }
}
