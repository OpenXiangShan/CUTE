
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility.sram.SRAMTemplate

class ABMatrixRegIO(implicit p: Parameters) extends CuteBundle {
    val FromDataController = new ABDataControlMatrixRegIO
    val FromMemoryLoader = new ABMemoryLoaderMatrixRegIO
}

class ABMatrixReg(scp_id: Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val MatrixRegIO = new ABMatrixRegIO
    })

    val dataControllerBankAddr = io.MatrixRegIO.FromDataController.BankAddr.bits
    val dataControllerData = io.MatrixRegIO.FromDataController.Data.bits
    val memoryLoaderWrites = io.MatrixRegIO.FromMemoryLoader.WriteRequest

    val laneValid = Wire(Vec(ABMatrixRegNBanks, Vec(2, Bool())))
    for (bank <- 0 until ABMatrixRegNBanks) {
        for (lane <- 0 until 2) {
            val write = memoryLoaderWrites(bank)(lane)
            val valids = Cat(write.BankAddr.valid, write.Data.valid, write.ByteMask.valid)
            laneValid(bank)(lane) := valids.andR
            assert(valids === 0.U || valids.andR, "ABMatrixReg: loader lane valids must agree")
        }
        when(laneValid(bank)(1)) {
            assert(laneValid(bank)(0), "ABMatrixReg: loader lane 1 requires lane 0")
            assert(
                memoryLoaderWrites(bank)(1).BankAddr.bits === memoryLoaderWrites(bank)(0).BankAddr.bits + 1.U,
                "ABMatrixReg: loader pair addresses must be consecutive"
            )
            assert(
                memoryLoaderWrites(bank)(1).BankAddr.bits(0) =/= memoryLoaderWrites(bank)(0).BankAddr.bits(0),
                "ABMatrixReg: loader pair must target opposite parity banks"
            )
        }
    }

    val writeGo = laneValid.asUInt.orR
    val readGo = io.MatrixRegIO.FromDataController.BankAddr.valid && !writeGo
    io.MatrixRegIO.FromDataController.BankAddr.ready := readGo
    io.MatrixRegIO.FromDataController.Data.valid := RegNext(readGo, false.B)

    val debugS1BankAddr = RegNext(dataControllerBankAddr)

    val sramBanks = (0 until ABMatrixRegNBanks).map { bankIdx =>
        val bankDepthHalf = (ABMatrixRegBankNEntries + 1) / 2
        val evenBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = ABMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false
        )).suggestName("CUTE-AB-MatrixReg-Even-SRAM")
        val oddBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = ABMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false
        )).suggestName("CUTE-AB-MatrixReg-Odd-SRAM")

        val readAddr = dataControllerBankAddr(bankIdx)
        val readParity = readAddr(0)
        val readIdx = readAddr >> 1
        val s1ReadParity = RegNext(readParity)

        evenBank.io.r.req.valid := readGo && !readParity
        evenBank.io.r.req.bits.setIdx := readIdx
        oddBank.io.r.req.valid := readGo && readParity
        oddBank.io.r.req.bits.setIdx := readIdx

        val evenReadData = evenBank.io.r.resp.data.asUInt
        val oddReadData = oddBank.io.r.resp.data.asUInt
        val readData = Mux(s1ReadParity, oddReadData, evenReadData)
        dataControllerData(bankIdx) := readData

        val evenSelect = VecInit((0 until 2).map { lane =>
            laneValid(bankIdx)(lane) && !memoryLoaderWrites(bankIdx)(lane).BankAddr.bits(0)
        })
        val oddSelect = VecInit((0 until 2).map { lane =>
            laneValid(bankIdx)(lane) && memoryLoaderWrites(bankIdx)(lane).BankAddr.bits(0)
        })
        assert(PopCount(evenSelect) <= 1.U, "ABMatrixReg: multiple writes target the even parity bank")
        assert(PopCount(oddSelect) <= 1.U, "ABMatrixReg: multiple writes target the odd parity bank")

        val evenWrite = Mux1H(evenSelect, memoryLoaderWrites(bankIdx))
        val oddWrite = Mux1H(oddSelect, memoryLoaderWrites(bankIdx))

        evenBank.io.w.req.valid := evenSelect.asUInt.orR
        evenBank.io.w.req.bits.setIdx := evenWrite.BankAddr.bits >> 1
        evenBank.io.w.req.bits.waymask.get := evenWrite.ByteMask.bits
        evenBank.io.w.req.bits.data := evenWrite.Data.bits.asTypeOf(Vec(ABMatrixRegEntryByteSize, UInt(8.W)))

        oddBank.io.w.req.valid := oddSelect.asUInt.orR
        oddBank.io.w.req.bits.setIdx := oddWrite.BankAddr.bits >> 1
        oddBank.io.w.req.bits.waymask.get := oddWrite.ByteMask.bits
        oddBank.io.w.req.bits.data := oddWrite.Data.bits.asTypeOf(Vec(ABMatrixRegEntryByteSize, UInt(8.W)))

        if (YJPDebugEnable) {
            when(RegNext(readGo)) {
                printf(
                    "[ABMatrixReg_Read(%d)]Bank(%d): debug_s1_bank_addr = %d, data = %x\n",
                    scp_id.U,
                    bankIdx.U,
                    debugS1BankAddr(bankIdx),
                    readData
                )
            }
            for (lane <- 0 until 2) {
                when(laneValid(bankIdx)(lane)) {
                    printf(
                        "[ABMatrixReg_Write(%d)]Bank(%d) Lane(%d): addr = %d, data = %x, mask = %x\n",
                        scp_id.U,
                        bankIdx.U,
                        lane.U,
                        memoryLoaderWrites(bankIdx)(lane).BankAddr.bits,
                        memoryLoaderWrites(bankIdx)(lane).Data.bits,
                        memoryLoaderWrites(bankIdx)(lane).ByteMask.bits
                    )
                }
            }
        }

        (evenBank, oddBank)
    }
}
