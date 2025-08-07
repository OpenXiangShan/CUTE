
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
import boom.v3.util._
    import os.write

    //数据在CScarachpad中的编排
    //数据会先排N，再排M
    //   N 0 1 2 3 4 5 6 7     CScaratchpadData里的排布
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



class CScarchPadIO(implicit p: Parameters) extends CuteBundle{
    val FromDataController = new CDataControlScaratchpadIO
    val FromMemoryLoader = new CMemoryLoaderScaratchpadIO
}

class CScratchpad(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ScarchPadIO = new CScarchPadIO
    })
    

    //按照目前的设计，可以服务所有请求
    io.ScarchPadIO.FromDataController.ReadWriteResponse := io.ScarchPadIO.FromDataController.ReadWriteRequest
    io.ScarchPadIO.FromMemoryLoader.ReadWriteResponse := io.ScarchPadIO.FromMemoryLoader.ReadWriteRequest

    //记录当前拍回数应该返回给哪条数据线
    val request = io.ScarchPadIO.FromDataController.ReadWriteRequest | io.ScarchPadIO.FromMemoryLoader.ReadWriteRequest
    val PreRequest = RegNext(request)

    val decode_request = new ScaratchpadTaskDecode(request)
    val decode_pre_request = new ScaratchpadTaskDecode(PreRequest)
    
    assert(!(decode_request.IsReadFromDataController && decode_request.IsReadFromMemoryLoader), "CScarchpad: ReadFromDataController and ReadFromMemoryLoader should not be both true at the same time")
    assert(!(decode_request.IsWriteFromDataController && decode_request.IsWriteFromMemoryLoader), "CScarchpad: WriteFromDataController and WriteFromMemoryLoader should not be both true at the same time")
    
    val read_request_per_bank_addr = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(0.U(CScratchpadBankNEntrys.W))))
    val read_request_per_bank_valid = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(false.B)))
    val read_request_response_valid = RegInit(VecInit(Seq.fill(CScratchpadNBanks)(false.B)))

    val write_request_per_bank_addr = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(0.U(CScratchpadBankNEntrys.W))))
    val write_request_per_bank_data= WireInit(VecInit(Seq.fill(CScratchpadNBanks)(0.U((8*CScratchpadEntryByteSize).W))))
    val write_request_per_bank_valid = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(false.B)))

    for( i <- 0 until CScratchpadNBanks) {
        read_request_per_bank_addr(i) := Mux(decode_request.IsReadFromDataController, io.ScarchPadIO.FromDataController.ReadBankAddr(i).bits, io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr(i).bits)
        read_request_per_bank_valid(i) := Mux(decode_request.IsReadFromDataController, io.ScarchPadIO.FromDataController.ReadBankAddr(i).valid, io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr(i).valid)
        read_request_response_valid(i) := Mux(decode_request.IsReadFromDataController, io.ScarchPadIO.FromDataController.ReadBankAddr(i).valid, io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr(i).valid)

        write_request_per_bank_addr(i) := Mux(decode_request.IsWriteFromDataController, io.ScarchPadIO.FromDataController.WriteBankAddr(i).bits, io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr(i).bits)
        write_request_per_bank_data(i) := Mux(decode_request.IsWriteFromDataController, io.ScarchPadIO.FromDataController.WriteRequestData(i).bits, io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data(i).bits)
        write_request_per_bank_valid(i) := Mux(decode_request.IsWriteFromDataController, io.ScarchPadIO.FromDataController.WriteRequestData(i).valid, io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data(i).valid)
    }

    
    //实例化多个sram为多个bank
    val sram_banks = (0 until CScratchpadNBanks) map { i =>

        //一个SeqMem就是一个SRAM，在一拍内完成读写，结果在下一拍输出，所以后头的代码里有s0，s1对不同阶段的流水数据进行分类，好区分每个周期的数据
        val bank = SyncReadMem(CScratchpadBankNEntrys, Bits(width = (8*CScratchpadEntryByteSize).W))
        bank.suggestName("CUTE-C-Scratchpad-SRAM")
        
        //第0周期的数据
        val s0_bank_read_addr = read_request_per_bank_addr(i)
        val s0_bank_read_valid = read_request_per_bank_valid(i)
        //第1周期的数据
        val s1_bank_read_data = WireInit(0.U((8*CScratchpadEntryByteSize).W))
        val debug_s1_bank_addr = RegNext(s0_bank_read_addr)


        io.ScarchPadIO.FromDataController.ReadResponseData(i).bits := s1_bank_read_data
        io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.ReadResponseData(i).bits := s1_bank_read_data

        io.ScarchPadIO.FromDataController.ReadResponseData(i).valid := decode_pre_request.IsReadFromDataController && read_request_response_valid(i)
        io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.ReadResponseData(i).valid := decode_pre_request.IsReadFromMemoryLoader && read_request_response_valid(i)

        //单独的读接口
        s1_bank_read_data := bank.read(s0_bank_read_addr, s0_bank_read_valid)
        
        ////单独的写接口
        val s0_bank_write_addr = write_request_per_bank_addr(i)
        val s0_bank_write_data = write_request_per_bank_data(i)
        val s0_bank_write_valid = write_request_per_bank_valid(i)

        when(s0_bank_write_valid) {
            bank.write(s0_bank_write_addr, s0_bank_write_data)
        }
    }


}

