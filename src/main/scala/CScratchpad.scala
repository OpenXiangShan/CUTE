
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._
import boom.v3.util._

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



class CScarchPadIO extends Bundle with HWParameters{
    val FromDataController = new CDataControlScaratchpadIO
    val FromMemoryLoader = new CMemoryLoaderScaratchpadIO
}

class CScratchpad extends Module with HWParameters{
    val io = IO(new Bundle{
        // val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
        val ScarchPadIO = new CScarchPadIO
    })
    
    // //当前ScarchPad被选为工作ScarchPad
    // val DataControllerChosen = io.ScarchPadIO.FromDataController.Chosen
    // //当前ScarchPad的各个bank的请求地址
    // val DataControllerBankAddr = io.ScarchPadIO.FromDataController.ReadBankAddr.bits
    // //当前ScarchPad的返回的值
    // val DataControllerData = io.ScarchPadIO.FromDataController.ReadResponseData.bits

    // //Scaratchpad的被MemoryLoader选中
    // val MemoryLoaderChosen = io.ScarchPadIO.FromMemoryLoader.Chosen
    
    //根据读写请求的优先级，确定当前周期服务的是哪个请求
    val DataControllerReadWriteRequest = io.ScarchPadIO.FromDataController.ReadWriteRequest
    val MemoryControllerReadWriteRequest = io.ScarchPadIO.FromMemoryLoader.ReadWriteRequest
    val ReadWriteRequest = DataControllerReadWriteRequest | MemoryControllerReadWriteRequest //这里其实可以拼接
    //输出所有请求
    //只要非0就输出
    // when(ReadWriteRequest.orR){
    //     printf("DataControllerReadWriteRequest = %d\n", DataControllerReadWriteRequest)
    //     printf("MemoryControllerReadWriteRequest = %d\n", MemoryControllerReadWriteRequest)
    //     printf("ReadWriteRequest = %d\n", ReadWriteRequest)
    // }

    //只选择一个请求，进行服务,先来个时间片轮转?或者来个检查，看谁的fifo最深。
    //这么看一个时间片轮转就还挺不错的，如果k=4，则只需要4个周期处理到一次DataContrller来的一次读和一次写就行了
    val FirstRequestIndex = RegInit(0.U(log2Ceil(ScaratchpadTaskType.TaskTypeBitWidth).W))
    FirstRequestIndex := WrapInc(FirstRequestIndex, ScaratchpadTaskType.TaskTypeBitWidth)
    //选择一个离FirstRequestIndex最近的请求
    val FirstIndex = FirstRequestIndex
    val SecIndex = WrapInc(FirstRequestIndex, ScaratchpadTaskType.TaskTypeBitWidth)
    val ThirdIndex = WrapInc(SecIndex, ScaratchpadTaskType.TaskTypeBitWidth)
    val FourthIndex = WrapInc(ThirdIndex, ScaratchpadTaskType.TaskTypeBitWidth)

    val HasRequest = ReadWriteRequest.orR
    //根据请求的优先级，确定当前周期服务的是哪个请求
    val ChoseIndex_0 = Mux(ReadWriteRequest(FirstIndex), FirstIndex,
                    Mux(ReadWriteRequest(SecIndex), SecIndex,
                    Mux(ReadWriteRequest(ThirdIndex), ThirdIndex,
                    Mux(ReadWriteRequest(FourthIndex), FourthIndex, 0.U))))
    
    
    val ChoseOneHot_0 = UIntToOH(ChoseIndex_0)
    io.ScarchPadIO.FromDataController.ReadWriteResponse := Mux(HasRequest,ChoseOneHot_0,0.U)
    io.ScarchPadIO.FromMemoryLoader.ReadWriteResponse := Mux(HasRequest,ChoseOneHot_0,0.U)
    // when(HasRequest){
    //     printf("ChoseIndex_0 = %d\n", ChoseIndex_0)
    //     //输出io.ScarchPadIO.FromDataController.ReadWriteResponse
    //     printf("io.ScarchPadIO.FromDataController.ReadWriteResponse = %d\n", io.ScarchPadIO.FromDataController.ReadWriteResponse)
    //     //输出io.ScarchPadIO.FromMemoryLoader.ReadWriteResponse
    //     printf("io.ScarchPadIO.FromMemoryLoader.ReadWriteResponse = %d\n", io.ScarchPadIO.FromMemoryLoader.ReadWriteResponse)
    // }

    val SramAddr_0 = Wire(Vec(CScratchpadNBanks, Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W))))

    when(ChoseIndex_0 === ScaratchpadTaskType.ReadFromDataControllerIndex.U){
        SramAddr_0 := io.ScarchPadIO.FromDataController.ReadBankAddr
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.WriteFromDataControllerIndex.U){
        SramAddr_0 := io.ScarchPadIO.FromDataController.WriteBankAddr
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.ReadFromMemoryLoaderIndex.U){
        SramAddr_0 := io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.WriteFromMemoryLoaderIndex.U){
        SramAddr_0 := io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr
    }.otherwise{
        SramAddr_0 := 0.U.asTypeOf(SramAddr_0)
    }

    val SramIsWrite_0 = Mux((ChoseIndex_0 === ScaratchpadTaskType.WriteFromDataControllerIndex.U) || (ChoseIndex_0 === ScaratchpadTaskType.WriteFromMemoryLoaderIndex.U), true.B, false.B) && HasRequest
    val SramIsRead_0  = !SramIsWrite_0 && HasRequest

    //TODO:这里的参数还是有问题的，我们得想明白为什么要分bank，分bank核心是为了让Slidingwindows可以取数，如果数据我们在送入ScarchPad前组织好的，就不需要分bank了
    //TODO:TODO:TODO:目前的问题在于FromMemoryLoader.WriteRequestToScarchPad.Data.bits的宽度
    //TODO:需要修改这个Vec，让他每次回数都只占用一个周期，这样性能才能好，需要在MemoryLoader中完成拼接才可以，这样送进来的就是和数据带宽一致的数据，没有带宽的浪费
    //这个用MUX写

    val SramWriteData_0 = Wire(Vec(CScratchpadNBanks, Valid(UInt((CScratchpadEntryBitSize).W))))

    when(ChoseIndex_0 === ScaratchpadTaskType.ReadFromDataControllerIndex.U){
        SramWriteData_0 := 0.U.asTypeOf(SramWriteData_0)
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.WriteFromDataControllerIndex.U){
        SramWriteData_0 := io.ScarchPadIO.FromDataController.WriteRequestData
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.ReadFromMemoryLoaderIndex.U){
        SramWriteData_0 := 0.U.asTypeOf(SramWriteData_0)
    }.elsewhen(ChoseIndex_0 === ScaratchpadTaskType.WriteFromMemoryLoaderIndex.U){
        SramWriteData_0 := io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data
    }.otherwise{
        SramWriteData_0 := 0.U.asTypeOf(SramAddr_0)
    }
    
    
    
    //记录当前拍回数应该返回给哪条数据线
    val PreReadChosen_0 = RegNext(ChoseIndex_0)
    val PreIsRead_0 = RegNext(SramIsRead_0)

    val s1_bank_read_valid = RegInit(false.B)
    

    //实例化多个sram为多个bank
    val sram_banks = (0 until CScratchpadNBanks) map { i =>

        //一个SeqMem就是一个SRAM，在一拍内完成读写，结果在下一拍输出，所以后头的代码里有s0，s1对不同阶段的流水数据进行分类，好区分每个周期的数据
        val bank = SyncReadMem(CScratchpadBankNEntrys, Bits(width = (8*CScratchpadEntryByteSize).W))
        bank.suggestName("CUTE-C-Scratchpad-SRAM")
        
        //第0周期的数据
        val s0_bank_read_addr = SramAddr_0(i).bits
        val s0_bank_read_valid = SramIsRead_0 && HasRequest && SramAddr_0(i).valid
        //第1周期的数据
        s1_bank_read_valid := s0_bank_read_valid
        val s1_bank_read_data = WireInit(0.U((8*CScratchpadEntryByteSize).W))
        val debug_s1_bank_addr = RegNext(s0_bank_read_addr)
        // val s1_bank_read_addr = RegEnable(s0_bank_read_addr, s0_bank_read_valid)
        // val s1_bank_read_valid = RegNext(s0_bank_read_valid)
        // //输出所有回数请求
        // printf("s0_bank_read_addr[%d] = %d\n", i.U, s0_bank_read_addr)
        // //输出所有回数的值
        // printf("s1_bank_read_data[%d] = %d\n", i.U, s1_bank_read_data)
        // //输出io.ScarchPadIO.FromDataController.ReadResponseData
        // printf("io.ScarchPadIO.FromDataController.ReadResponseData[%d] = %d\n", i.U, io.ScarchPadIO.FromDataController.ReadResponseData.bits(i))
        io.ScarchPadIO.FromDataController.ReadResponseData(i).bits := s1_bank_read_data
        io.ScarchPadIO.FromDataController.ReadResponseData(i).valid := ((PreReadChosen_0 ===  ScaratchpadTaskType.ReadFromDataControllerIndex.U) && PreIsRead_0) && s1_bank_read_valid
        io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.ReadResponseData(i).bits := s1_bank_read_data
        io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.ReadResponseData(i).valid := (PreReadChosen_0 === ScaratchpadTaskType.ReadFromMemoryLoaderIndex.U && PreIsRead_0) && s1_bank_read_valid
        when(PreIsRead_0)
        {
            //输出读的信息
            if (YJPDebugEnable)
            {
                printf("[CSPD_Read]Bank(%d): debug_s1_bank_addr = %d ,s1_bank_read_data = %x, PreReadChosen_0 = %d \n", i.U, debug_s1_bank_addr, s1_bank_read_data,PreReadChosen_0)
            }
        }
        //读取数据的fifo得在DataController里面自己实现，ScarchPad尽可能减少逻辑，符合SRAM的特性，所以上面的代码只有valid和data，没有ready
        
        //写数据
        val s0_bank_write_addr = Mux(SramIsWrite_0 && SramAddr_0(i).valid, SramAddr_0(i).bits, 0.U)
        val s0_bank_write_data = Mux(SramIsWrite_0 && SramWriteData_0(i).valid, SramWriteData_0(i).bits, 0.U)
        val s0_bank_write_valid = SramIsWrite_0 && HasRequest && SramWriteData_0(i).valid && SramAddr_0(i).valid
        when(s0_bank_write_valid){
            // bank.write(s0_bank_write_addr, s0_bank_write_data)
            //输出写的信息
            if (YJPDebugEnable)
            {
                printf("[CSPD_Write]Bank(%d): s0_bank_write_addr = %d ,s0_bank_write_data = %x\n", i.U, s0_bank_write_addr, s0_bank_write_data)
            }
        }

        val Bank_Is_write = s0_bank_write_valid
        val Bank_Enable = HasRequest
        val Bank_addr = SramAddr_0(i).bits
        val Bank_wdata = s0_bank_write_data
        s1_bank_read_data := bank.readWrite(Bank_addr, Bank_wdata, Bank_Enable, Bank_Is_write)

        bank
    }


}

