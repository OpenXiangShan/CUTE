
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility.sram.SRAMTemplate

class CMatrixRegIO(implicit p: Parameters) extends CuteBundle {
    val FromDataController = new CDataControlMatrixRegIO
    val FromMemoryLoader = new CMemoryLoaderMatrixRegIO
}

class CMatrixReg(scp_id: Int)(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val MatrixRegIO = new CMatrixRegIO
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
        "CMatrixReg: data controller and memory loader reads cannot be active together"
    )
    assert(
        !(decodeRequest.IsWriteFromDataController && decodeRequest.IsWriteFromMemoryLoader),
        "CMatrixReg: data controller and memory loader writes cannot be active together"
    )

    val readRequestPerBankAddr = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U(log2Ceil(CMatrixRegBankNEntries).W))))
    val readRequestPerBankValid = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))
    val readRequestResponseValid = RegInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))

    for (bank <- 0 until CMatrixRegNBanks) {
        readRequestPerBankAddr(bank) := Mux(
            decodeRequest.IsReadFromDataController,
            io.MatrixRegIO.FromDataController.ReadBankAddr(bank).bits,
            io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(bank).bits
        )
        readRequestPerBankValid(bank) := Mux(
            decodeRequest.IsReadFromDataController,
            io.MatrixRegIO.FromDataController.ReadBankAddr(bank).valid,
            io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(bank).valid
        )
        readRequestResponseValid(bank) := readRequestPerBankValid(bank)
    }

    val sramBanks = (0 until CMatrixRegNBanks).map { bankIdx =>
        val bankDepthHalf = (CMatrixRegBankNEntries + 1) / 2
        val evenBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = CMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false
        )).suggestName("CUTE-C-MatrixReg-Even-SRAM")
        val oddBank = Module(new SRAMTemplate(
            gen = UInt(8.W),
            set = bankDepthHalf,
            way = CMatrixRegEntryByteSize,
            singlePort = true,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false
        )).suggestName("CUTE-C-MatrixReg-Odd-SRAM")

        val readAddr = readRequestPerBankAddr(bankIdx)
        val readValid = readRequestPerBankValid(bankIdx)
        val readParity = readAddr(0)
        val readIdx = readAddr >> 1
        val s1ReadParity = RegNext(readParity)

        io.MatrixRegIO.FromDataController.ReadResponseData(bankIdx).bits :=
            Mux(s1ReadParity, oddBank.io.r.resp.data.asUInt, evenBank.io.r.resp.data.asUInt)
        io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bankIdx).bits :=
            Mux(s1ReadParity, oddBank.io.r.resp.data.asUInt, evenBank.io.r.resp.data.asUInt)
        io.MatrixRegIO.FromDataController.ReadResponseData(bankIdx).valid :=
            decodePreRequest.IsReadFromDataController && readRequestResponseValid(bankIdx)
        io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(bankIdx).valid :=
            decodePreRequest.IsReadFromMemoryLoader && readRequestResponseValid(bankIdx)

        evenBank.io.r.req.valid := readValid && !readParity
        evenBank.io.r.req.bits.setIdx := readIdx
        oddBank.io.r.req.valid := readValid && readParity
        oddBank.io.r.req.bits.setIdx := readIdx

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
            assert(valids === 0.U || valids.andR, "CMatrixReg: loader lane valids must agree")
        }
        when(loaderLaneValid(1)) {
            assert(loaderLaneValid(0), "CMatrixReg: loader lane 1 requires lane 0")
            assert(
                loaderWrites(1).BankAddr.bits === loaderWrites(0).BankAddr.bits + 1.U,
                "CMatrixReg: loader pair addresses must be consecutive"
            )
            assert(
                loaderWrites(1).BankAddr.bits(0) =/= loaderWrites(0).BankAddr.bits(0),
                "CMatrixReg: loader pair must target opposite parity banks"
            )
        }

        val loaderEvenSelect = VecInit((0 until 2).map { lane =>
            loaderLaneValid(lane) && !loaderWrites(lane).BankAddr.bits(0)
        })
        val loaderOddSelect = VecInit((0 until 2).map { lane =>
            loaderLaneValid(lane) && loaderWrites(lane).BankAddr.bits(0)
        })
        assert(PopCount(loaderEvenSelect) <= 1.U, "CMatrixReg: multiple loader writes target even SRAM")
        assert(PopCount(loaderOddSelect) <= 1.U, "CMatrixReg: multiple loader writes target odd SRAM")

        val loaderEvenWrite = Mux1H(loaderEvenSelect, loaderWrites)
        val loaderOddWrite = Mux1H(loaderOddSelect, loaderWrites)
        val dcWritesEven = dcWriteValid && !dcWriteAddr(0)
        val dcWritesOdd = dcWriteValid && dcWriteAddr(0)

        val evenWriteValid = dcWritesEven || loaderEvenSelect.asUInt.orR
        val evenWriteAddr = Mux(dcWritesEven, dcWriteAddr, loaderEvenWrite.BankAddr.bits)
        val evenWriteData = Mux(dcWritesEven, dcWriteData, loaderEvenWrite.Data.bits)
        val evenWriteMask = Mux(
            dcWritesEven,
            Fill(CMatrixRegEntryByteSize, true.B),
            loaderEvenWrite.ByteMask.bits
        )
        val oddWriteValid = dcWritesOdd || loaderOddSelect.asUInt.orR
        val oddWriteAddr = Mux(dcWritesOdd, dcWriteAddr, loaderOddWrite.BankAddr.bits)
        val oddWriteData = Mux(dcWritesOdd, dcWriteData, loaderOddWrite.Data.bits)
        val oddWriteMask = Mux(
            dcWritesOdd,
            Fill(CMatrixRegEntryByteSize, true.B),
            loaderOddWrite.ByteMask.bits
        )

        evenBank.io.w.req.valid := evenWriteValid
        evenBank.io.w.req.bits.setIdx := evenWriteAddr >> 1
        evenBank.io.w.req.bits.waymask.get := evenWriteMask
        evenBank.io.w.req.bits.data := evenWriteData.asTypeOf(Vec(CMatrixRegEntryByteSize, UInt(8.W)))
        oddBank.io.w.req.valid := oddWriteValid
        oddBank.io.w.req.bits.setIdx := oddWriteAddr >> 1
        oddBank.io.w.req.bits.waymask.get := oddWriteMask
        oddBank.io.w.req.bits.data := oddWriteData.asTypeOf(Vec(CMatrixRegEntryByteSize, UInt(8.W)))

        assert(!(readValid && !readParity && evenWriteValid),
            "CMatrixReg: even single-port SRAM read/write conflict")
        assert(!(readValid && readParity && oddWriteValid),
            "CMatrixReg: odd single-port SRAM read/write conflict")

        (evenBank, oddBank)
    }
}
