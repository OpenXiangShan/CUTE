
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

//BScratchpad，用于暂存B矩阵的数据，供给TE模块使用
//B矩阵在DataController看来是一个只读的矩阵
//B矩阵需要支持滑动窗口，分Matrix_M个bank是合理的
//Scarchpad的功能是，根据输入的地址，输出数据

class BScarchPadIO(implicit p: Parameters) extends CuteBundle{
    val FromDataController = new BDataControlScaratchpadIO
    val FromMemoryLoader = new BMemoryLoaderScaratchpadIO 
}

class BScratchpad(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
        val ScarchPadIO = new BScarchPadIO
    })

    
    //TODO:这里需要加读写优先级的逻辑，目前在Loader、DataController里面都加了FIFO，能保证一些堵的情况的发生

    //当前ScarchPad被选为工作ScarchPad
    // val DataControllerChosen = io.ScarchPadIO.FromDataController.Chosen
    //当前ScarchPad的各个bank的请求地址
    val DataControllerBankAddr = io.ScarchPadIO.FromDataController.BankAddr.bits
    //当前ScarchPad的返回的值
    val DataControllerData = io.ScarchPadIO.FromDataController.Data.bits

    //Scaratchpad的被MemoryLoader选中
    // val MemoryLoaderChosen = io.ScarchPadIO.FromMemoryLoader.Chosen
    //MemoryLoader的请求地址
    val MemoryLoaderBankAddr = io.ScarchPadIO.FromMemoryLoader.BankAddr
    val MemoryLoaderBankId = io.ScarchPadIO.FromMemoryLoader.BankId.bits
    //MemoryLoader的请求数据
    val MemoryLoaderData = io.ScarchPadIO.FromMemoryLoader.Data
    
    //TODO:fifoready?
    //TODO:我们要做成写优先的Scartchpad
    val write_go = io.ScarchPadIO.FromMemoryLoader.BankAddr.zip(io.ScarchPadIO.FromMemoryLoader.Data).map{case (a,b) => a.valid && b.valid}
    val read_go = io.ScarchPadIO.FromDataController.BankAddr.valid && !io.ScarchPadIO.FromMemoryLoader.BankAddr.map(_.valid).reduce(_||_)  && !write_go.reduce(_||_) //写优先～
    //为输入信号赋ready
    io.ScarchPadIO.FromDataController.BankAddr.ready := read_go
    // io.ScarchPadIO.FromMemoryLoader.BankAddr.ready := write_ready
    //SRAM下一拍的返回结果，所以使用上一拍的ready作为valid
    io.ScarchPadIO.FromDataController.Data.valid := RegNext(read_go)
    //实例化多个sram为多个bank
    val debug_s1_bank_addr = RegNext(DataControllerBankAddr)
    val sram_banks = (0 until BScratchpadNBanks) map { i =>

        //一个SeqMem就是一个SRAM，在一拍内完成读写，结果在下一拍输出，所以后头的代码里有s0，s1对不同阶段的流水数据进行分类，好区分每个周期的数据
        val bank = SyncReadMem(BScratchpadBankNEntrys, Bits(width = (BScratchpadEntryByteSize*8).W))
        bank.suggestName("CUTE-B-Scratchpad-SRAM")
        
        //第0周期的数据
        val s0_bank_read_addr = DataControllerBankAddr(i)
        val s0_bank_read_valid = read_go
        //第1周期的数据        
        val s1_bank_read_data = WireInit(0.U((BScratchpadEntryByteSize*8).W))
        // val s1_bank_read_addr = RegEnable(s0_bank_read_addr, s0_bank_read_valid)
        // val s1_bank_read_valid = RegNext(s0_bank_read_valid)
        DataControllerData(i) := s1_bank_read_data
        //读取数据的fifo得在DataController里面自己实现，ScarchPad尽可能减少逻辑，符合SRAM的特性，所以上面的代码只有valid和data，没有ready
        when(RegNext(read_go))
        {
            //输出读的信息
            if (YJPDebugEnable)
            {
                printf("[BSPD_Read]Bank(%d): debug_s1_bank_addr = %d ,s1_bank_read_data = %x\n", i.U, debug_s1_bank_addr(0), s1_bank_read_data)
            }
        }
        //写数据
        val s0_bank_write_addr = MemoryLoaderBankAddr(i).bits
        val s0_bank_write_data = MemoryLoaderData(i).bits
        val s0_bank_write_valid = MemoryLoaderBankAddr(i).valid && MemoryLoaderData(i).valid
        when(write_go(i) && s0_bank_write_valid){
            // bank.write(s0_bank_write_addr, s0_bank_write_data)
        }


        val Bank_Is_write = write_go(i) && s0_bank_write_valid
        val Bank_Enable = (write_go(i) && s0_bank_write_valid) || read_go
        val Bank_addr = Mux(read_go, DataControllerBankAddr(i), MemoryLoaderBankAddr(i).bits)
        val Bank_wdata = MemoryLoaderData(i).bits
        s1_bank_read_data := bank.readWrite(Bank_addr, Bank_wdata, Bank_Enable, Bank_Is_write)


        bank
    }


}
