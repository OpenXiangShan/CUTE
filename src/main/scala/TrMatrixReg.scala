
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility.sram.SRAMTemplate

class TrMatrixRegIO(implicit p: Parameters) extends CuteBundle {
    val FromDataController = new TrDataControlMatrixRegIO
    val FromStore = new TrStoreMatrixRegReadIO
    val FromMemoryLoader = new TrLoadMatrixRegIO
}

class TrMatrixReg(scp_id: Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val MatrixRegIO = new TrMatrixRegIO
    })

    val dataControllerBankAddr = io.MatrixRegIO.FromDataController.BankAddr.bits
    val dataControllerData = io.MatrixRegIO.FromDataController.Data.bits
    val memoryLoaderWrites = io.MatrixRegIO.FromMemoryLoader.WriteRequest

    val laneValid = Wire(Vec(TrMatrixRegNBanks, Vec(2, Bool())))
    for (bank <- 0 until TrMatrixRegNBanks) {
        for (lane <- 0 until 2) {
            val write = memoryLoaderWrites(bank)(lane)
            val valids = Cat(write.BankAddr.valid, write.Data.valid, write.ByteMask.valid)
            laneValid(bank)(lane) := valids.andR
            assert(valids === 0.U || valids.andR, "TrMatrixReg: loader lane valids must agree")
        }
        when(laneValid(bank)(1)) {
            assert(laneValid(bank)(0), "TrMatrixReg: loader lane 1 requires lane 0")
            assert(
                memoryLoaderWrites(bank)(1).BankAddr.bits === memoryLoaderWrites(bank)(0).BankAddr.bits + 1.U,
                "TrMatrixReg: loader pair addresses must be consecutive"
            )
            assert(
                memoryLoaderWrites(bank)(1).BankAddr.bits(0) =/= memoryLoaderWrites(bank)(0).BankAddr.bits(0),
                "TrMatrixReg: loader pair must target opposite parity banks"
            )
        }
    }

    val writeGo = laneValid.asUInt.orR
    val computeReadValid = io.MatrixRegIO.FromDataController.BankAddr.valid
    val storeReadValid = io.MatrixRegIO.FromStore.BankAddr.valid
    assert(!(computeReadValid && storeReadValid),
        "TrMatrixReg: compute and TR store cannot read the same register in one cycle")
    val computeReadGo = computeReadValid && !storeReadValid && !writeGo
    val storeReadGo = storeReadValid && !computeReadValid && !writeGo
    io.MatrixRegIO.FromDataController.BankAddr.ready := computeReadGo
    io.MatrixRegIO.FromDataController.Data.valid := RegNext(computeReadGo, false.B)
    io.MatrixRegIO.FromStore.BankAddr.ready := storeReadGo
    io.MatrixRegIO.FromStore.Data.valid := RegNext(storeReadGo, false.B)

    val debugS1BankAddr = RegNext(dataControllerBankAddr)

    val sramBanks = (0 until TrMatrixRegNBanks).map { bankIdx =>
        val bankDepthHalf = (TrMatrixRegBankNEntries + 1) / 2
        val evenBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = TrMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false,
            suffix = Some("cute_abmat")
        )).suggestName("CUTE-TR-MatrixReg-Even-SRAM")
        val oddBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = TrMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false,
            suffix = Some("cute_abmat")
        )).suggestName("CUTE-TR-MatrixReg-Odd-SRAM")

        val readAddr = dataControllerBankAddr(bankIdx)
        val readParity = readAddr(0)
        val readIdx = readAddr >> 1
        val s1ReadParity = RegNext(readParity)
        val storeReadAddrs = io.MatrixRegIO.FromStore.BankAddr.bits(bankIdx)
        val storeEvenSelect = VecInit(storeReadAddrs.map(addr => !addr(0)))
        val storeOddSelect = VecInit(storeReadAddrs.map(_.apply(0)))
        if (io.MatrixRegIO.FromStore.readLanes == 2) {
            when(storeReadGo) {
                assert(storeReadAddrs(1) === storeReadAddrs(0) + 1.U,
                    "TrMatrixReg: paired TR store reads must use consecutive entries")
                assert(storeReadAddrs(1)(0) =/= storeReadAddrs(0)(0),
                    "TrMatrixReg: paired TR store reads must select both parity SRAMs")
            }
        }
        when(storeReadGo) {
            assert(PopCount(storeEvenSelect) <= 1.U,
                "TrMatrixReg: TR store read lanes must target distinct parity SRAMs")
            assert(PopCount(storeOddSelect) <= 1.U,
                "TrMatrixReg: TR store read lanes must target distinct parity SRAMs")
        }
        val storeEvenAddr = Mux1H(storeEvenSelect, storeReadAddrs)
        val storeOddAddr = Mux1H(storeOddSelect, storeReadAddrs)
        val s1StoreParity = RegNext(VecInit(storeReadAddrs.map(_.apply(0))))

        evenBank.io.r.req.valid :=
            (computeReadGo && !readParity) || (storeReadGo && storeEvenSelect.asUInt.orR)
        evenBank.io.r.req.bits.setIdx := Mux(
            computeReadGo,
            readIdx,
            storeEvenAddr >> 1
        )
        oddBank.io.r.req.valid :=
            (computeReadGo && readParity) || (storeReadGo && storeOddSelect.asUInt.orR)
        oddBank.io.r.req.bits.setIdx := Mux(
            computeReadGo,
            readIdx,
            storeOddAddr >> 1
        )

        val evenReadData = evenBank.io.r.resp.data.asUInt
        val oddReadData = oddBank.io.r.resp.data.asUInt
        val readData = Mux(s1ReadParity, oddReadData, evenReadData)
        dataControllerData(bankIdx) := readData
        for (lane <- 0 until io.MatrixRegIO.FromStore.readLanes) {
            io.MatrixRegIO.FromStore.Data.bits(bankIdx)(lane) :=
                Mux(s1StoreParity(lane), oddReadData, evenReadData)
        }

        val evenSelect = VecInit((0 until 2).map { lane =>
            laneValid(bankIdx)(lane) && !memoryLoaderWrites(bankIdx)(lane).BankAddr.bits(0)
        })
        val oddSelect = VecInit((0 until 2).map { lane =>
            laneValid(bankIdx)(lane) && memoryLoaderWrites(bankIdx)(lane).BankAddr.bits(0)
        })
        assert(PopCount(evenSelect) <= 1.U, "TrMatrixReg: multiple writes target the even parity bank")
        assert(PopCount(oddSelect) <= 1.U, "TrMatrixReg: multiple writes target the odd parity bank")

        val evenWrite = Mux1H(evenSelect, memoryLoaderWrites(bankIdx))
        val oddWrite = Mux1H(oddSelect, memoryLoaderWrites(bankIdx))

        evenBank.io.w.req.valid := evenSelect.asUInt.orR
        evenBank.io.w.req.bits.setIdx := evenWrite.BankAddr.bits >> 1
        evenBank.io.w.req.bits.waymask.get := evenWrite.ByteMask.bits
        evenBank.io.w.req.bits.data := evenWrite.Data.bits.asTypeOf(Vec(TrMatrixRegEntryByteSize, UInt(8.W)))

        oddBank.io.w.req.valid := oddSelect.asUInt.orR
        oddBank.io.w.req.bits.setIdx := oddWrite.BankAddr.bits >> 1
        oddBank.io.w.req.bits.waymask.get := oddWrite.ByteMask.bits
        oddBank.io.w.req.bits.data := oddWrite.Data.bits.asTypeOf(Vec(TrMatrixRegEntryByteSize, UInt(8.W)))

        if (YJPDebugEnable) {
            when(RegNext(computeReadGo)) {
                printf(
                    "[TrMatrixReg_Read(%d)]Bank(%d): debug_s1_bank_addr = %d, data = %x\n",
                    scp_id.U,
                    bankIdx.U,
                    debugS1BankAddr(bankIdx),
                    readData
                )
            }
            for (lane <- 0 until 2) {
                when(laneValid(bankIdx)(lane)) {
                    printf(
                        "[TrMatrixReg_Write(%d)]Bank(%d) Lane(%d): addr = %d, data = %x, mask = %x\n",
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
