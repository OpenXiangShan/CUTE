
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility.sram.SRAMTemplate

// Bit zero selects the SRAM. Swapping adjacent columns on odd block rows
// leaves the set unchanged and makes horizontal and vertical neighbors disjoint.
object AccBlockLayout {
    def swapColumn(row: UInt, col: UInt): UInt = col ^ row(0)
    def remap(addr: UInt, columns: Int): UInt = {
        require(isPow2(columns))
        if (columns == 1) addr else addr ^ (addr >> log2Ceil(columns))(0)
    }
}

class AccMatrixRegIO(implicit p: Parameters) extends CuteBundle {
    val FromDataController = new AccDataControlMatrixRegIO
    val FromMemoryLoader = new AccMemoryMatrixRegIO
}

class AccMatrixReg(scp_id: Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val MatrixRegIO = new AccMatrixRegIO
    })

    io.MatrixRegIO.FromDataController.ReadWriteResponse := io.MatrixRegIO.FromDataController.ReadWriteRequest
    io.MatrixRegIO.FromMemoryLoader.LoadReadWriteResponse := io.MatrixRegIO.FromMemoryLoader.LoadReadWriteRequest
    io.MatrixRegIO.FromMemoryLoader.StoreReadWriteResponse := io.MatrixRegIO.FromMemoryLoader.StoreReadWriteRequest

    val request = io.MatrixRegIO.FromDataController.ReadWriteRequest |
      io.MatrixRegIO.FromMemoryLoader.LoadReadWriteRequest |
      io.MatrixRegIO.FromMemoryLoader.StoreReadWriteRequest
    val preRequest = RegNext(request)
    val decodeRequest = new MatrixRegTaskDecode(request)
    val decodePreRequest = new MatrixRegTaskDecode(preRequest)

    assert(
        !(decodeRequest.IsReadFromDataController && decodeRequest.IsReadFromMemoryLoader),
        "AccMatrixReg: data controller and memory loader reads cannot be active together"
    )
    assert(
        !(decodeRequest.IsWriteFromDataController && decodeRequest.IsWriteFromMemoryLoader),
        "AccMatrixReg: data controller and memory loader writes cannot be active together"
    )

    val sramBanks = (0 until AccMatrixRegNBanks).map { bankIdx =>
        val bankDepthHalf = (AccMatrixRegBankNEntries + 1) / 2
        val evenBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = AccMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false,
            suffix = Some("cute_cmat")
        )).suggestName("CUTE-Acc-MatrixReg-Even-SRAM")
        val oddBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = AccMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false,
            suffix = Some("cute_cmat")
        )).suggestName("CUTE-Acc-MatrixReg-Odd-SRAM")

        val dc = io.MatrixRegIO.FromDataController
        val store = io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg
        val readAddr = Seq(
            Mux(decodeRequest.IsReadFromDataController, dc.ReadBankAddr(bankIdx).bits, store.BankAddr(bankIdx)(0).bits),
            store.BankAddr(bankIdx)(1).bits
        )
        val readValid = Seq(
            (decodeRequest.IsReadFromDataController && dc.ReadBankAddr(bankIdx).valid) ||
                (decodeRequest.IsReadFromMemoryLoader && store.BankAddr(bankIdx)(0).valid),
            decodeRequest.IsReadFromMemoryLoader && store.BankAddr(bankIdx)(1).valid
        )
        val memories = Seq(evenBank, oddBank)
        for ((mem, parity) <- memories.zipWithIndex) {
            val select = readAddr.indices.map(i => readValid(i) && readAddr(i)(0) === parity.U)
            assert(PopCount(select) <= 1.U, "ACC read slots must select different SRAMs")
            mem.io.r.req.valid := select.reduce(_ || _)
            mem.io.r.req.bits.setIdx := Mux(select(0), readAddr(0), readAddr(1)) >> 1
        }
        for (slot <- readAddr.indices) {
            val odd = RegNext(readAddr(slot)(0))
            val data = Mux(odd, oddBank.io.r.resp.data.asUInt, evenBank.io.r.resp.data.asUInt)
            val valid = RegNext(readValid(slot), false.B)
            store.ReadResponseData(bankIdx)(slot).bits := data
            store.ReadResponseData(bankIdx)(slot).valid := valid && decodePreRequest.IsReadFromMemoryLoader
            if (slot == 0) {
                dc.ReadResponseData(bankIdx).valid := valid && decodePreRequest.IsReadFromDataController
                dc.ReadResponseData(bankIdx).bits := data
            }
        }

        val dcWriteAddr = io.MatrixRegIO.FromDataController.WriteBankAddr(bankIdx).bits
        val dcWriteData = io.MatrixRegIO.FromDataController.WriteRequestData(bankIdx).bits
        val dcWriteValid = decodeRequest.IsWriteFromDataController &&
          io.MatrixRegIO.FromDataController.WriteRequestData(bankIdx).valid

        val loaderWrites = io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.WriteRequest(bankIdx)
        val loaderLaneValid = Wire(Vec(2, Bool()))
        for (lane <- 0 until 2) {
            val valids = Cat(
                loaderWrites(lane).BankAddr.valid,
                loaderWrites(lane).Data.valid,
                loaderWrites(lane).ByteMask.valid
            )
            loaderLaneValid(lane) := decodeRequest.IsWriteFromMemoryLoader && valids.andR
            assert(valids === 0.U || valids.andR, "AccMatrixReg: loader lane valids must agree")
        }
        val loaderEvenSelect = VecInit((0 until 2).map { lane =>
            loaderLaneValid(lane) && !loaderWrites(lane).BankAddr.bits(0)
        })
        val loaderOddSelect = VecInit((0 until 2).map { lane =>
            loaderLaneValid(lane) && loaderWrites(lane).BankAddr.bits(0)
        })
        assert(PopCount(loaderEvenSelect) <= 1.U, "AccMatrixReg: multiple loader writes target even SRAM")
        assert(PopCount(loaderOddSelect) <= 1.U, "AccMatrixReg: multiple loader writes target odd SRAM")

        val loaderEvenWrite = Mux1H(loaderEvenSelect, loaderWrites)
        val loaderOddWrite = Mux1H(loaderOddSelect, loaderWrites)
        val dcWritesEven = dcWriteValid && !dcWriteAddr(0)
        val dcWritesOdd = dcWriteValid && dcWriteAddr(0)

        val evenWriteValid = dcWritesEven || loaderEvenSelect.asUInt.orR
        val evenWriteAddr = Mux(dcWritesEven, dcWriteAddr, loaderEvenWrite.BankAddr.bits)
        val evenWriteData = Mux(dcWritesEven, dcWriteData, loaderEvenWrite.Data.bits)
        val evenWriteMask = Mux(
            dcWritesEven,
            Fill(AccMatrixRegEntryByteSize, true.B),
            loaderEvenWrite.ByteMask.bits
        )
        val oddWriteValid = dcWritesOdd || loaderOddSelect.asUInt.orR
        val oddWriteAddr = Mux(dcWritesOdd, dcWriteAddr, loaderOddWrite.BankAddr.bits)
        val oddWriteData = Mux(dcWritesOdd, dcWriteData, loaderOddWrite.Data.bits)
        val oddWriteMask = Mux(
            dcWritesOdd,
            Fill(AccMatrixRegEntryByteSize, true.B),
            loaderOddWrite.ByteMask.bits
        )

        evenBank.io.w.req.valid := evenWriteValid
        evenBank.io.w.req.bits.setIdx := evenWriteAddr >> 1
        evenBank.io.w.req.bits.waymask.get := evenWriteMask
        evenBank.io.w.req.bits.data := evenWriteData.asTypeOf(Vec(AccMatrixRegEntryByteSize, UInt(8.W)))
        oddBank.io.w.req.valid := oddWriteValid
        oddBank.io.w.req.bits.setIdx := oddWriteAddr >> 1
        oddBank.io.w.req.bits.waymask.get := oddWriteMask
        oddBank.io.w.req.bits.data := oddWriteData.asTypeOf(Vec(AccMatrixRegEntryByteSize, UInt(8.W)))

        assert(!(evenBank.io.r.req.valid && evenWriteValid),
            "AccMatrixReg: even single-port SRAM read/write conflict")
        assert(!(oddBank.io.r.req.valid && oddWriteValid),
            "AccMatrixReg: odd single-port SRAM read/write conflict")

        (evenBank, oddBank)
    }
}
