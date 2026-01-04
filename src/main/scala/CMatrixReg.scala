
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import utility.sram.SRAMTemplate

// CMatrixReg，使用SRAMTemplate实现的C矩阵寄存器
// 数据在C MatrixReg中的编排
// 数据会先排N，再排M
//   N 0 1 2 3 4 5 6 7     CMatrixRegData里的排布
// M                               {bank    [0]         [1]}
// 0   0 1 2 3 4 5 6 7   |addr    0 |    0123,89ab   ghij,opgr 
// 1   8 9 a b c d e f   |        1 |    4567,cdef   klmn,stuv 
// 2   g h i j k l m n   |        2 |    wxyz,!...   @...,#... 
// 3   o p g r s t u v   |        3 |    ....,....   ....,....
// 4   w x y z .......   |        4 |    ....,....   ....,.... 
// 5   !..............   |        5 |    ....,....   ....,....
// 6   @..............   |        6 |    ....,....   ....,....
// 7   #..............   |        7 |    ....,....   ....,.... 
// 8   $..............   | ....................................

//TODO:这里就是有两个设计选项的
//矩阵乘结果出来后，如果有逐元素的DSP部件，那就是npu的形状              ---> SOC上的NPU！        ～  ultra --> 不足的L3总带宽+不足的热功耗
//如果矩阵乘结果出来后，如果没有逐元素的DSP部件，那就是矩阵乘部件的形状    ---> 通用多核/众核AI处理器 ～  dojo --> 充足的L3带宽+冗余的计算能力

//但是这里的reorder部件是一定要有的，方便后续的数据编排和处理，让输入和输出的数据排布一致。
//为什么在这里，因为我们的PE计算完后，在这里是第一次全逐个联线，所以这里是最合适的地方。

class CMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val FromDataController = new CDataControlMatrixRegIO
    val FromMemoryLoader = new CMemoryLoaderMatrixRegIO
}

class CMatrixReg(scp_id:Int)(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val MatrixRegIO = new CMatrixRegIO
    })
    

    //按照目前的设计，可以服务所有请求
    io.MatrixRegIO.FromDataController.ReadWriteResponse := io.MatrixRegIO.FromDataController.ReadWriteRequest
    io.MatrixRegIO.FromMemoryLoader.ReadWriteResponse := io.MatrixRegIO.FromMemoryLoader.ReadWriteRequest

    //记录当前拍回数应该返回给哪条数据线
    val request = io.MatrixRegIO.FromDataController.ReadWriteRequest | io.MatrixRegIO.FromMemoryLoader.ReadWriteRequest
    val PreRequest = RegNext(request)

    val decode_request = new MatrixRegTaskDecode(request)
    val decode_pre_request = new MatrixRegTaskDecode(PreRequest)
    
    assert(!(decode_request.IsReadFromDataController && decode_request.IsReadFromMemoryLoader), "CMatrixReg: ReadFromDataController and ReadFromMemoryLoader should not be both true at the same time")
    assert(!(decode_request.IsWriteFromDataController && decode_request.IsWriteFromMemoryLoader), "CMatrixReg: WriteFromDataController and WriteFromMemoryLoader should not be both true at the same time")
    
    val read_request_per_bank_addr = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U(CMatrixRegBankNEntrys.W))))
    val read_request_per_bank_valid = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))
    val read_request_response_valid = RegInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))

    val write_request_per_bank_addr = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U(CMatrixRegBankNEntrys.W))))
    val write_request_per_bank_data= WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(0.U((8*CMatrixRegEntryByteSize).W))))
    val write_request_per_bank_valid = WireInit(VecInit(Seq.fill(CMatrixRegNBanks)(false.B)))

    for( i <- 0 until CMatrixRegNBanks) {
        read_request_per_bank_addr(i) := Mux(decode_request.IsReadFromDataController, io.MatrixRegIO.FromDataController.ReadBankAddr(i).bits, io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(i).bits)
        read_request_per_bank_valid(i) := Mux(decode_request.IsReadFromDataController, io.MatrixRegIO.FromDataController.ReadBankAddr(i).valid, io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(i).valid)
        read_request_response_valid(i) := Mux(decode_request.IsReadFromDataController, io.MatrixRegIO.FromDataController.ReadBankAddr(i).valid, io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.BankAddr(i).valid)

        write_request_per_bank_addr(i) := Mux(decode_request.IsWriteFromDataController, io.MatrixRegIO.FromDataController.WriteBankAddr(i).bits, io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.BankAddr(i).bits)
        write_request_per_bank_data(i) := Mux(decode_request.IsWriteFromDataController, io.MatrixRegIO.FromDataController.WriteRequestData(i).bits, io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.Data(i).bits)
        write_request_per_bank_valid(i) := Mux(decode_request.IsWriteFromDataController, io.MatrixRegIO.FromDataController.WriteRequestData(i).valid, io.MatrixRegIO.FromMemoryLoader.WriteRequestToMatrixReg.Data(i).valid)
    }

    
    //实例化多个sram为多个bank
    val sram_banks = (0 until CMatrixRegNBanks) map { i =>

        //使用SRAMTemplate替代SyncReadMem
        //singlePort=false: 双端口SRAM，读写可以同时进行
        //latency=1: 读延迟为1拍
        val bank = Module(new SRAMTemplate(
            gen = UInt((8*CMatrixRegEntryByteSize).W),
            set = CMatrixRegBankNEntrys,
            way = 1,
            singlePort = false,
            latency = 1,
            hasMbist = false,
            hasSramCtl = false
        ))
        
        //第0周期的数据
        val s0_bank_read_addr = read_request_per_bank_addr(i)
        val s0_bank_read_valid = read_request_per_bank_valid(i)
        //第1周期的数据
        val s1_bank_read_data = WireInit(0.U((8*CMatrixRegEntryByteSize).W))
        val debug_s1_bank_addr = RegNext(s0_bank_read_addr)

        io.MatrixRegIO.FromDataController.ReadResponseData(i).bits := s1_bank_read_data
        io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(i).bits := s1_bank_read_data

        io.MatrixRegIO.FromDataController.ReadResponseData(i).valid := decode_pre_request.IsReadFromDataController && read_request_response_valid(i)
        io.MatrixRegIO.FromMemoryLoader.ReadRequestToMatrixReg.ReadResponseData(i).valid := decode_pre_request.IsReadFromMemoryLoader && read_request_response_valid(i)

        //连接SRAMTemplate的读接口
        bank.io.r.req.valid := s0_bank_read_valid
        bank.io.r.req.bits.setIdx := s0_bank_read_addr
        //读响应在下一拍返回（latency=1）
        s1_bank_read_data := bank.io.r.resp.data(0)
        
        //单独的写接口
        val s0_bank_write_addr = write_request_per_bank_addr(i)
        val s0_bank_write_data = write_request_per_bank_data(i)
        val s0_bank_write_valid = write_request_per_bank_valid(i)

        bank.io.w.req.valid := s0_bank_write_valid
        bank.io.w.req.bits.setIdx := s0_bank_write_addr
        bank.io.w.req.bits.data(0) := s0_bank_write_data

        bank
    }
}

