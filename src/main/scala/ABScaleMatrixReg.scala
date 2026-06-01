package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

class ABScaleMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val FromScaleController = new ABScaleControlMatrixRegIO
    val FromScaleLoader = new ABScaleLoaderMatrixRegIO 
}

class ABScaleMatrixReg(implicit p: Parameters) extends CuteModule{
    val io = IO(new ABScaleMatrixRegIO)

    
    //TODO:这里需要加读写优先级的逻辑，目前在Loader、DataController里面都加了FIFO，能保证一些堵的情况的发生

    //当前MatrixReg被选为工作MatrixReg
    // val DataControllerChosen = io.MatrixRegIO.FromDataController.Chosen
    //当前MatrixReg的各个bank的请求地址
    val ScaleControllerBankAddr = io.FromScaleController.BankAddr.bits
    //当前MatrixReg的返回的值
    val ScaleControllerData = io.FromScaleController.Data.bits

    //MatrixReg的被MemoryLoader选中
    // val MemoryLoaderChosen = io.MatrixRegIO.FromMemoryLoader.Chosen
    //MemoryLoader的请求地址
    val ScaleLoaderBankAddr = io.FromScaleLoader.BankAddr
    //MemoryLoader的请求数据
    val ScaleLoaderData = io.FromScaleLoader.Data
    
    //TODO:fifoready?
    //TODO:我们要做成写优先的Scartchpad
    val write_go = io.FromScaleLoader.BankAddr.valid && io.FromScaleLoader.Data.valid
    val read_go = io.FromScaleController.BankAddr.valid && !io.FromScaleLoader.BankAddr.valid  && !write_go //写优先～
    //为输入信号赋ready
    io.FromScaleController.BankAddr.ready := read_go
    // io.MatrixRegIO.FromMemoryLoader.BankAddr.ready := write_ready
    //SRAM下一拍的返回结果，所以使用上一拍的ready作为valid
    io.FromScaleController.Data.valid := RegNext(read_go)
    //实例化多个sram为多个bank
    val debug_s1_bank_addr = RegNext(ScaleControllerBankAddr)

    val bank = SyncReadMem(ABScaleBankNEntries, Bits(width = (ABScaleNSlices * ScaleWidth * ReduceGroupSize).W))
    bank.suggestName("CUTE-AB-Scale-MatrixReg-SRAM")
    
    //第0周期的数据
    val s0_bank_read_addr = ScaleControllerBankAddr
    val s0_bank_read_valid = read_go
    //第1周期的数据        
    val s1_bank_read_data = WireInit(0.U((ABScaleNSlices * ScaleWidth * ReduceGroupSize).W))
    // val s1_bank_read_addr = RegEnable(s0_bank_read_addr, s0_bank_read_valid)
    // val s1_bank_read_valid = RegNext(s0_bank_read_valid)
    for (i <- 0 until ABScaleNSlices) {
        ScaleControllerData(i) := s1_bank_read_data((i + 1) * ScaleWidth * ReduceGroupSize - 1, i * ScaleWidth * ReduceGroupSize)
    }
    //读取数据的fifo得在DataController里面自己实现，MatrixReg尽可能减少逻辑，符合SRAM的特性，所以上面的代码只有valid和data，没有ready
    when(RegNext(read_go))
    {
        //输出读的信息
        if (YJPDebugEnable)
        {
            printf("[ABSMReg_Read]Bank: debug_s1_bank_addr = %d ,s1_bank_read_data = %x\n", debug_s1_bank_addr, s1_bank_read_data)
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
