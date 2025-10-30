
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

//AMatrixReg，用于暂存A矩阵的数据，供给TE模块使用
//A矩阵在DataController看来是一个只读的矩阵
//A矩阵需要支持滑动窗口，分Matrix_M个bank是合理的
//MatrixReg的功能是，根据输入的地址，输出数据

class AMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val FromDataController = new ADataControlMatrixRegIO
    val FromMemoryLoader = new AMemoryLoaderMatrixRegIO 
    // val DataControllerValid = Input(Bool())
    // val MemoryLoaderValid = Input(Bool())
}

class AMatrixReg(scp_id:Int)(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
        val MatrixRegIO = new AMatrixRegIO
    })

    
    //TODO:这里需要加读写优先级的逻辑，目前在Loader、DataController里面都加了FIFO，能保证一些堵的情况的发生

    //当前MatrixReg被选为工作MatrixReg
    // val DataControllerChosen = io.MatrixRegIO.FromDataController.Chosen
    //当前MatrixReg的各个bank的请求地址
    val DataControllerBankAddr = io.MatrixRegIO.FromDataController.BankAddr.bits
    //当前MatrixReg的返回的值
    val DataControllerData = io.MatrixRegIO.FromDataController.Data.bits

    //MatrixReg被MemoryLoader选中
    // val MemoryLoaderChosen = io.MatrixRegIO.FromMemoryLoader.Chosen
    //MemoryLoader的请求地址
    val MemoryLoaderBankAddr = io.MatrixRegIO.FromMemoryLoader.BankAddr
    val MemoryLoaderBankId = io.MatrixRegIO.FromMemoryLoader.BankId.bits
    //MemoryLoader的请求数据
    val MemoryLoaderData = io.MatrixRegIO.FromMemoryLoader.Data
    //每个bank是否要写零
    val MemoryLoaderZeroFill = io.MatrixRegIO.FromMemoryLoader.ZeroFill
    
    //TODO:fifoready?
    //TODO:我们要做成写优先的MatrixReg
    // val write_ready = io.MatrixRegIO.FromMemoryLoader.BankAddr.valid && !io.MatrixRegIO.FromDataController.BankAddr.valid && MemoryLoaderChosen && io.MatrixRegIO.FromMemoryLoader.Data.valid
    // val read_ready = io.MatrixRegIO.FromDataController.BankAddr.valid && !io.MatrixRegIO.FromMemoryLoader.BankAddr.valid && DataControllerChosen && !write_ready //写优先～
    val write_go = MemoryLoaderBankAddr.zip(MemoryLoaderData).map{case (a, b) => a.valid && b.valid}.reduce(_||_) || io.MatrixRegIO.FromMemoryLoader.ZeroFill.map(_.valid).reduce(_||_)
    val read_go = io.MatrixRegIO.FromDataController.BankAddr.valid && !MemoryLoaderBankAddr.map(_.valid).reduce(_||_) && !write_go //写优先～
    //为输入信号赋ready
    io.MatrixRegIO.FromDataController.BankAddr.ready := read_go
    // io.MatrixRegIO.FromMemoryLoader.BankAddr.ready := write_ready
    //SRAM下一拍的返回结果，所以使用上一拍的ready作为valid
    io.MatrixRegIO.FromDataController.Data.valid := RegNext(read_go)
    val debug_s1_bank_addr = RegNext(DataControllerBankAddr)
    //实例化多个sram为多个bank
    val sram_banks = (0 until AMatrixRegNBanks) map { i =>

        //一个SeqMem就是一个SRAM，在一拍内完成读写，结果在下一拍输出，所以后头的代码里有s0，s1对不同阶段的流水数据进行分类，好区分每个周期的数据
        val bank = SyncReadMem(AMatrixRegBankNEntrys, Bits(width = (AMatrixRegEntryByteSize*8).W))
        bank.suggestName("CUTE-A-MatrixReg-SRAM")
        
        //第0周期的数据
        val s0_bank_read_addr = DataControllerBankAddr(i)
        val s0_bank_read_valid = read_go
        //第1周期的数据
        val s1_bank_read_data = WireInit(0.U((AMatrixRegEntryByteSize*8).W))
        when(RegNext(read_go))
        {
            //输出读的信息
            if (YJPDebugEnable)
            {
                printf("[AMatrixReg_Read(%d)]Bank(%d): debug_s1_bank_addr = %d ,s1_bank_read_data = %x\n",scp_id.U, i.U, debug_s1_bank_addr(0), s1_bank_read_data)
            }
        }
        // val s1_bank_read_addr = RegEnable(s0_bank_read_addr, s0_bank_read_valid)
        // val s1_bank_read_valid = RegNext(s0_bank_read_valid)
        DataControllerData(i) := s1_bank_read_data
        //读取数据的fifo得在DataController里面自己实现，MatrixReg尽可能减少逻辑，符合SRAM的特性，所以上面的代码只有valid和data，没有ready
        
        //写数据
        val s0_bank_write_addr = MemoryLoaderBankAddr(i).bits
        val s0_bank_write_data = MemoryLoaderData(i).bits
        val s0_bank_write_valid = MemoryLoaderBankAddr(i).valid && MemoryLoaderData(i).valid
        val s0_bank_zerofill_valid = MemoryLoaderZeroFill(i).valid
        val s0_final_write_valid = (write_go && s0_bank_write_valid) || s0_bank_zerofill_valid
        val s0_final_write_addr = Mux((!(write_go && s0_bank_write_valid)) && s0_bank_zerofill_valid, MemoryLoaderZeroFill(i).bits, MemoryLoaderBankAddr(i).bits)
        val s0_final_write_data = Mux((!(write_go && s0_bank_write_valid)) && s0_bank_zerofill_valid, 0.U, MemoryLoaderData(i).bits)

        when(write_go && s0_bank_write_valid){
            // bank.write(s0_bank_write_addr, s0_bank_write_data)
            if (YJPDebugEnable)
            {
                printf("[AMatrixReg_Write(%d)]Bank(%d): s0_bank_write_addr = %d ,s0_bank_write_data = %x\n",scp_id.U, i.U, s0_bank_write_addr, s0_bank_write_data)
            }
        }.elsewhen(s0_bank_zerofill_valid){
            // bank.write(MemoryLoaderZeroFill(i).bits, 0.U)
            if (YJPDebugEnable)
            {
                printf("[AMatrixReg_ZeroFill(%d)]Bank(%d): s0_bank_write_addr = %d ,s0_bank_write_data = %x\n",scp_id.U, i.U, MemoryLoaderZeroFill(i).bits, 0.U)
            }
        }


        val Bank_Is_write = s0_final_write_valid
        val Bank_Enable = s0_final_write_valid || read_go
        val Bank_addr = Mux(read_go, DataControllerBankAddr(i), s0_final_write_addr)
        val Bank_wdata = s0_final_write_data
        s1_bank_read_data := bank.readWrite(Bank_addr, Bank_wdata, Bank_Enable, Bank_Is_write)


        bank
    }


}
