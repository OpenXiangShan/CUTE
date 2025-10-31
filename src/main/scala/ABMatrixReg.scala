
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

// ABMatrixReg，统一的A/B矩阵寄存器
// 用于暂存A矩阵或B矩阵的数据，供给TE模块使用
// 在DataController看来是一个只读的矩阵
// 矩阵需要支持滑动窗口，分Matrix_M个bank是合理的
// MatrixReg的功能是，根据输入的地址，输出数据

class ABMatrixRegIO(implicit p: Parameters) extends CuteBundle{
    val FromDataController = new ABDataControlMatrixRegIO
    val FromMemoryLoader = new ABMemoryLoaderMatrixRegIO 
}

class ABMatrixReg(scp_id: Int)(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val MatrixRegIO = new ABMatrixRegIO
    })

    
    // 读写优先级逻辑：写优先于读
    // 目前在Loader、DataController里面都加了FIFO，能保证一些堵的情况的发生

    // DataController端的信号
    val DataControllerBankAddr = io.MatrixRegIO.FromDataController.BankAddr.bits
    val DataControllerData = io.MatrixRegIO.FromDataController.Data.bits

    // MemoryLoader端的信号
    val MemoryLoaderBankAddr = io.MatrixRegIO.FromMemoryLoader.BankAddr
    val MemoryLoaderBankId = io.MatrixRegIO.FromMemoryLoader.BankId.bits
    val MemoryLoaderData = io.MatrixRegIO.FromMemoryLoader.Data
    val MemoryLoaderZeroFill = io.MatrixRegIO.FromMemoryLoader.ZeroFill
    
    // 写优先的MatrixReg控制逻辑
    // write_go: 只要有写入请求（正常写或零填充）就为true
    val write_go = MemoryLoaderBankAddr.zip(MemoryLoaderData).map{case (a, b) => a.valid && b.valid}.reduce(_||_) || 
                   io.MatrixRegIO.FromMemoryLoader.ZeroFill.map(_.valid).reduce(_||_)
    
    // read_go: 只有在没有写请求时才允许读，实现写优先
    val read_go = io.MatrixRegIO.FromDataController.BankAddr.valid && 
                  !MemoryLoaderBankAddr.map(_.valid).reduce(_||_) && 
                  !write_go
    
    // 为输入信号赋ready
    io.MatrixRegIO.FromDataController.BankAddr.ready := read_go
    
    // SRAM下一拍返回结果，所以使用上一拍的ready作为valid
    io.MatrixRegIO.FromDataController.Data.valid := RegNext(read_go)
    val debug_s1_bank_addr = RegNext(DataControllerBankAddr)
    
    // 实例化多个SRAM作为多个bank
    val sram_banks = (0 until ABMatrixRegNBanks) map { i =>

        // 一个SeqMem就是一个SRAM，在一拍内完成读写，结果在下一拍输出
        // 代码里有s0，s1对不同阶段的流水数据进行分类
        val bank = SyncReadMem(ABMatrixRegBankNEntrys, Bits(width = (ABMatrixRegEntryByteSize*8).W))
        bank.suggestName(s"CUTE-AB-MatrixReg-SRAM-${scp_id}")
        
        // 第0周期的数据 - 读取
        val s0_bank_read_addr = DataControllerBankAddr(i)
        val s0_bank_read_valid = read_go
        
        // 第1周期的数据 - 读取结果
        val s1_bank_read_data = WireInit(0.U((ABMatrixRegEntryByteSize*8).W))
        
        when(RegNext(read_go))
        {
            // 输出读的信息
            if (YJPDebugEnable)
            {
                printf("[ABMatrixReg_Read(%d)]Bank(%d): debug_s1_bank_addr = %d, s1_bank_read_data = %x\n",
                       scp_id.U, i.U, debug_s1_bank_addr(0), s1_bank_read_data)
            }
        }
        
        DataControllerData(i) := s1_bank_read_data
        // 读取数据的fifo在DataController里面实现，MatrixReg尽可能减少逻辑
        
        // 写数据逻辑
        val s0_bank_write_addr = MemoryLoaderBankAddr(i).bits
        val s0_bank_write_data = MemoryLoaderData(i).bits
        val s0_bank_write_valid = MemoryLoaderBankAddr(i).valid && MemoryLoaderData(i).valid
        
        // 零填充逻辑
        val s0_bank_zerofill_valid = MemoryLoaderZeroFill(i).valid
        
        // 最终的写入控制
        val s0_final_write_valid = (write_go && s0_bank_write_valid) || s0_bank_zerofill_valid
        val s0_final_write_addr = Mux((!(write_go && s0_bank_write_valid)) && s0_bank_zerofill_valid, 
                                      MemoryLoaderZeroFill(i).bits, 
                                      MemoryLoaderBankAddr(i).bits)
        val s0_final_write_data = Mux((!(write_go && s0_bank_write_valid)) && s0_bank_zerofill_valid, 
                                      0.U, 
                                      MemoryLoaderData(i).bits)

        when(write_go && s0_bank_write_valid){
            if (YJPDebugEnable)
            {
                printf("[ABMatrixReg_Write(%d)]Bank(%d): s0_bank_write_addr = %d, s0_bank_write_data = %x\n",
                       scp_id.U, i.U, s0_bank_write_addr, s0_bank_write_data)
            }
        }.elsewhen(s0_bank_zerofill_valid){
            if (YJPDebugEnable)
            {
                printf("[ABMatrixReg_ZeroFill(%d)]Bank(%d): s0_bank_write_addr = %d, s0_bank_write_data = %x\n",
                       scp_id.U, i.U, MemoryLoaderZeroFill(i).bits, 0.U)
            }
        }

        // Bank的读写控制
        val Bank_Is_write = s0_final_write_valid
        val Bank_Enable = s0_final_write_valid || read_go
        val Bank_addr = Mux(read_go, DataControllerBankAddr(i), s0_final_write_addr)
        val Bank_wdata = s0_final_write_data
        s1_bank_read_data := bank.readWrite(Bank_addr, Bank_wdata, Bank_Enable, Bank_Is_write)

        bank
    }
}

