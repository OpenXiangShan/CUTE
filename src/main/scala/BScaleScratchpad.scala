package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

class BScaleScratchpadIO(implicit p: Parameters) extends CuteBundle{
    val FromScaleController = new BScaleControlScaratchpadIO
    val FromScaleLoader = new BScaleLoaderScaratchpadIO 
}

class BScaleScratchpad(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        // val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
        val ScaleScratchpadIO = new BScaleScratchpadIO
    })

    
    //TODO:这里需要加读写优先级的逻辑，目前在Loader、DataController里面都加了FIFO，能保证一些堵的情况的发生

    //当前ScarchPad被选为工作ScarchPad
    // val DataControllerChosen = io.ScarchPadIO.FromDataController.Chosen
    //当前ScarchPad的各个bank的请求地址
    val ScaleControllerBankAddr = io.ScaleScratchpadIO.FromScaleController.BankAddr.bits
    //当前ScarchPad的返回的值
    val ScaleControllerData = io.ScaleScratchpadIO.FromScaleController.Data.bits

    //Scaratchpad的被MemoryLoader选中
    // val MemoryLoaderChosen = io.ScarchPadIO.FromMemoryLoader.Chosen
    //MemoryLoader的请求地址
    val ScaleLoaderBankAddr = io.ScaleScratchpadIO.FromScaleLoader.BankAddr
    //MemoryLoader的请求数据
    val ScaleLoaderData = io.ScaleScratchpadIO.FromScaleLoader.Data
    
    //TODO:fifoready?
    //TODO:我们要做成写优先的Scartchpad
    val write_go = io.ScaleScratchpadIO.FromScaleLoader.BankAddr.valid && io.ScaleScratchpadIO.FromScaleLoader.Data.valid
    val read_go = io.ScaleScratchpadIO.FromScaleController.BankAddr.valid && !io.ScaleScratchpadIO.FromScaleLoader.BankAddr.valid  && !write_go //写优先～
    //为输入信号赋ready
    io.ScaleScratchpadIO.FromScaleController.BankAddr.ready := read_go
    // io.ScarchPadIO.FromMemoryLoader.BankAddr.ready := write_ready
    //SRAM下一拍的返回结果，所以使用上一拍的ready作为valid
    io.ScaleScratchpadIO.FromScaleController.Data.valid := RegNext(read_go)
    //实例化多个sram为多个bank
    val debug_s1_bank_addr = RegNext(ScaleControllerBankAddr)

    val bank = SyncReadMem(BScaleBankNEntrys, Bits(width = (BScaleNSlices * ScaleWidth * ReduceGroupSize).W))
    bank.suggestName("CUTE-B-Scale-Scratchpad-SRAM")
    
    //第0周期的数据
    val s0_bank_read_addr = ScaleControllerBankAddr
    val s0_bank_read_valid = read_go
    //第1周期的数据        
    val s1_bank_read_data = WireInit(0.U((BScaleNSlices * ScaleWidth * ReduceGroupSize).W))
    // val s1_bank_read_addr = RegEnable(s0_bank_read_addr, s0_bank_read_valid)
    // val s1_bank_read_valid = RegNext(s0_bank_read_valid)
    for (i <- 0 until BScaleNSlices) {
        ScaleControllerData(i) := s1_bank_read_data((i + 1) * ScaleWidth * ReduceGroupSize - 1, i * ScaleWidth * ReduceGroupSize)
    }
    //读取数据的fifo得在DataController里面自己实现，ScarchPad尽可能减少逻辑，符合SRAM的特性，所以上面的代码只有valid和data，没有ready
    when(RegNext(read_go))
    {
        //输出读的信息
        if (YJPDebugEnable)
        {
            printf("[BSSPD_Read]Bank: debug_s1_bank_addr = %d ,s1_bank_read_data = %x\n", debug_s1_bank_addr, s1_bank_read_data)
        }
    }
    //写数据
    val s0_bank_write_valid = ScaleLoaderBankAddr.valid && ScaleLoaderData.valid

    val Bank_Is_write = write_go && s0_bank_write_valid
    val Bank_Enable = (write_go && s0_bank_write_valid) || read_go
    val Bank_addr = Mux(read_go, ScaleControllerBankAddr, ScaleLoaderBankAddr.bits)
    val Bank_wdata = ScaleLoaderData.bits
    s1_bank_read_data := bank.readWrite(Bank_addr, Bank_wdata.asUInt, Bank_Enable, Bank_Is_write)
    
}