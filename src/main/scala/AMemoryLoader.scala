
package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config._

// AMemoryLoader：加载 A 矩阵数据到 MatrixReg，仅支持矩阵加载（无卷积）
// 参考 CMemoryLoader 的 FullLoad 实现，按 M、K 维度顺序访存

class ASourceIdSearch(implicit p: Parameters) extends CuteBundle{
    val MatrixRegBankId = UInt(log2Ceil(ABMatrixRegNBanks).W)
    val MatrixRegAddr = UInt(log2Ceil(ABMatrixRegBankNEntrys).W)
}

class AMemoryLoader(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val ToMatrixRegIO = Flipped(new ABMemoryLoaderMatrixRegIO)
        val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
        val LocalMMUIO = Flipped(new LocalMMUIO)
        val DebugInfo = Input(new DebugInfoIO)
        val MatrixRegId = Output(UInt(ABMatrixRegIdWidth.W))
    })

    io.ToMatrixRegIO.BankAddr := 0.U.asTypeOf(io.ToMatrixRegIO.BankAddr)
    io.ToMatrixRegIO.Data := 0.U.asTypeOf(io.ToMatrixRegIO.Data)
    io.ToMatrixRegIO.ZeroFill := 0.U.asTypeOf(io.ToMatrixRegIO.ZeroFill)
    io.LocalMMUIO.Request.valid := false.B
    io.LocalMMUIO.Request.bits := 0.U.asTypeOf(io.LocalMMUIO.Request.bits)
    io.LocalMMUIO.Response.ready := false.B
    io.ConfigInfo.MicroTaskEndValid := false.B
    io.ConfigInfo.MicroTaskReady := false.B

    val CurrentMatrixRegId = RegInit(0.U(ABMatrixRegIdWidth.W))
    io.MatrixRegId := CurrentMatrixRegId

    dontTouch(io)

    if (EnableDifftest) {
        val pcReg = RegInit(0.U(64.W))
        when (io.ConfigInfo.MicroTaskValid) {
          pcReg := io.ConfigInfo.pc.get
        }
        val difftestAmuFinish = DifftestModule(new DiffAmuFinishEvent, delay = 0, dontCare = true)
        difftestAmuFinish.coreid := io.ConfigInfo.coreid.get
        difftestAmuFinish.index := 0.U
        difftestAmuFinish.valid := (io.ToMatrixRegIO.BankAddr.map(_.valid).reduce(_||_) ||
          (io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady))
        difftestAmuFinish.pc := pcReg
        for (i <- 0 until ABMatrixRegNBanks) {
          difftestAmuFinish.bankValid(i) := io.ToMatrixRegIO.BankAddr(i).valid
          difftestAmuFinish.bankAddr(i) := io.ToMatrixRegIO.BankAddr(i).bits
          difftestAmuFinish.data(i * 4 + 0) := io.ToMatrixRegIO.Data(i).bits(63,0)
          difftestAmuFinish.data(i * 4 + 1) := io.ToMatrixRegIO.Data(i).bits(127,64)
          difftestAmuFinish.data(i * 4 + 2) := io.ToMatrixRegIO.Data(i).bits(191,128)
          difftestAmuFinish.data(i * 4 + 3) := io.ToMatrixRegIO.Data(i).bits(255,192)
        }
        difftestAmuFinish.finish := io.ConfigInfo.MicroTaskEndValid && io.ConfigInfo.MicroTaskEndReady
    }

    val ConfigInfo = io.ConfigInfo
    val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W))
    val ApplicationTensor_A_Stride_M = RegInit(0.U(MMUAddrWidth.W))
    val dataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
    val MatrixRegTensor_M = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val MatrixRegTensor_K = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Conherent = RegInit(true.B)

    val Is_ZeroLoad = RegInit(false.B)
    val Is_FullLoad = RegInit(false.B)

    val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
    val state = RegInit(s_idle)
    val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
    val memoryload_state = RegInit(s_load_idle)

    val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize)+1).W))
    val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))
    val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val CurrentLoaded_BlockTensor_K_Iter = RegInit(0.U(MatrixRegMaxTensorDimBitSize.W))
    val Request_M_Iter_Time = RegInit(0.U(log2Ceil(Matrix_MN).W))

    val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new ASourceIdSearch).getWidth.W))))
    val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_MN*ReduceGroupSize*ReduceWidthByte)).W))

    val MReg_Fill_Table = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(outsideDataWidth.W)))))
    val MReg_Fill_Table_MReg_Addr = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(ABMatrixRegBankNEntrys).W)))))
    val MReg_Fill_Table_Time = RegInit((VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(outsideDataWidthByte/ABMatrixRegEntryByteSize)+1).W)))))
    val MReg_Fill_Table_Free = MReg_Fill_Table_Time.map(_ === 0.U)
    val MReg_Fill_Table_Insert_Index = PriorityEncoder(MReg_Fill_Table_Free)
    val MReg_Fill_Table_Not_Full = MReg_Fill_Table_Free.reduce(_ || _)
    val MAX_Fill_Times = outsideDataWidthByte/ABMatrixRegEntryByteSize

    val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(VecInit(Seq.fill(AMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))))
    val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(log2Ceil(AMemoryLoaderReadFromMemoryFIFODepth).W)))))
    val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(true.B)))
    val Bank_Fill_Valid = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(false.B)))
    val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)

    for(i <- 0 until ABMatrixRegNBanks){
        Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), AMemoryLoaderReadFromMemoryFIFODepth)
        Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)
        Bank_Fill_Valid(i) := Bank_Fill_Search_FIFO_Head(i) =/= Bank_Fill_Search_FIFO_Tail(i)
    }

    val Request = io.LocalMMUIO.Request
    Request.valid := false.B

    when(state === s_idle){
        ConfigInfo.MicroTaskReady := true.B
        when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
            state := s_mm_task
            memoryload_state := s_load_init
            MatrixRegTensor_M := ConfigInfo.MatrixRegTensor_M
            MatrixRegTensor_K := ConfigInfo.MatrixRegTensor_K
            CurrentMatrixRegId := ConfigInfo.MatrixRegId
            Tensor_Block_BaseAddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
            ApplicationTensor_A_Stride_M := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M
            dataType := ConfigInfo.ApplicationTensor_A.dataType
            Is_ZeroLoad := ConfigInfo.LoadTaskInfo.Is_ZeroLoad
            Is_FullLoad := ConfigInfo.LoadTaskInfo.Is_FullLoad
            Conherent := ConfigInfo.Conherent
            if(YJPAMLDebugEnable){
                printf("[AML<%d>]AMemoryLoader Task Start, MatrixRegTensor_M:%d, MatrixRegTensor_K:%d, BaseVaddr:%x, Stride_M:%x, dataType:%d, Is_ZeroLoad:%d, Is_FullLoad:%d\n",
                  io.DebugInfo.DebugTimeStampe, ConfigInfo.MatrixRegTensor_M, ConfigInfo.MatrixRegTensor_K,
                  ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr, ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M,
                  ConfigInfo.ApplicationTensor_A.dataType, ConfigInfo.LoadTaskInfo.Is_ZeroLoad.asUInt, ConfigInfo.LoadTaskInfo.Is_FullLoad.asUInt)
            }
        }
    }

    when(memoryload_state === s_load_init){
        memoryload_state := s_load_working
        TotalLoadSize := 0.U
        TotalRequestSize := 0.U
        CurrentLoaded_BlockTensor_M_Iter := 0.U
        CurrentLoaded_BlockTensor_K_Iter := 0.U
        Request_M_Iter_Time := 0.U
        MaxRequestIter := MatrixRegTensor_M * MatrixRegTensor_K * ReduceWidthByte.U / outsideDataWidthByte.U
        Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
        Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
        Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
        MReg_Fill_Table := 0.U.asTypeOf(MReg_Fill_Table)
        MReg_Fill_Table_MReg_Addr := 0.U.asTypeOf(MReg_Fill_Table_MReg_Addr)
        MReg_Fill_Table_Time := 0.U.asTypeOf(MReg_Fill_Table_Time)
    }.elsewhen(memoryload_state === s_load_working){
        assert(PopCount(Cat(Is_ZeroLoad, Is_FullLoad)) === 1.U,
               "Error! AML Load Task Type: Exactly one of Is_ZeroLoad, Is_FullLoad should be true!")

        when(Is_ZeroLoad){
            val Max_ZeroLoad_Write_Times = ABMatrixRegBankNEntrys
            for (i <- 0 until ABMatrixRegNBanks){
                io.ToMatrixRegIO.BankAddr(i).bits := TotalLoadSize
                io.ToMatrixRegIO.BankAddr(i).valid := true.B
                io.ToMatrixRegIO.Data(i).bits := 0.U
                io.ToMatrixRegIO.Data(i).valid := true.B
            }
            TotalLoadSize := TotalLoadSize + 1.U
            if (YJPAMLDebugEnable) printf("[AML<%d>]ZeroLoad, TotalLoadSize: %d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize)
            when(TotalLoadSize === (Max_ZeroLoad_Write_Times - 1).U){
                memoryload_state := s_load_end
                if (YJPAMLDebugEnable) printf("[AML<%d>]ZeroLoadEnd\n", io.DebugInfo.DebugTimeStampe)
            }
        }

        when(Is_FullLoad){
            // 矩阵访存顺序：按 M 分 bank 交织，再扫 K。地址 = BaseAddr + M*Stride_M + K*ReduceWidthByte
            val RequestMatrixRegBankId = (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) % ABMatrixRegNBanks.U
            val RequestMatrixRegAddr = (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) / ABMatrixRegNBanks.U * ReduceGroupSize.U + CurrentLoaded_BlockTensor_K_Iter

            Request.bits.RequestVirtualAddr := Tensor_Block_BaseAddr + (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) * ApplicationTensor_A_Stride_M + CurrentLoaded_BlockTensor_K_Iter * ReduceWidthByte.U
            val sourceId = Mux(Conherent, io.LocalMMUIO.ConherentRequsetSourceID, io.LocalMMUIO.nonConherentRequsetSourceID)
            Request.bits.RequestConherent := Conherent
            Request.bits.RequestSourceID := sourceId.bits
            Request.bits.RequestType_isWrite := false.B
            Request.valid := (TotalRequestSize < MaxRequestIter)

            when(Request.fire){
                val TableItem = Wire(new ASourceIdSearch)
                TableItem.MatrixRegBankId := RequestMatrixRegBankId
                TableItem.MatrixRegAddr := RequestMatrixRegAddr
                SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

                Request_M_Iter_Time := Request_M_Iter_Time + 1.U
                when(Request_M_Iter_Time === (Matrix_MN - 1).U || (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) === MatrixRegTensor_M - 1.U){
                    Request_M_Iter_Time := 0.U
                    CurrentLoaded_BlockTensor_K_Iter := CurrentLoaded_BlockTensor_K_Iter + (outsideDataWidthByte.U / ReduceWidthByte.U)
                    when(CurrentLoaded_BlockTensor_K_Iter + (outsideDataWidthByte.U / ReduceWidthByte.U) === MatrixRegTensor_K){
                        CurrentLoaded_BlockTensor_K_Iter := 0.U
                        CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_MN.U
                    }
                }
                when(TotalRequestSize =/= MaxRequestIter){
                    TotalRequestSize := TotalRequestSize + 1.U
                }
                if (YJPAMLDebugEnable){
                    printf("[AML<%d>]FullLoad Request, M_Iter:%d, K_Iter:%d, ReqTime:%d, Addr:%x, BankId:%d, RegAddr:%d\n",
                      io.DebugInfo.DebugTimeStampe, CurrentLoaded_BlockTensor_M_Iter, CurrentLoaded_BlockTensor_K_Iter,
                      Request_M_Iter_Time, Request.bits.RequestVirtualAddr, RequestMatrixRegBankId, RequestMatrixRegAddr)
                }
            }

            val current_fill_fifo_full = WireInit(false.B)
            when(io.LocalMMUIO.Response.valid){
                val respSourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
                val MatrixRegBankId = SoureceIdSearchTable(respSourceId).asTypeOf(new ASourceIdSearch).MatrixRegBankId
                current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(MatrixRegBankId)
            }
            io.LocalMMUIO.Response.ready := MReg_Fill_Table_Not_Full && !current_fill_fifo_full

            when(io.LocalMMUIO.Response.fire){
                val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
                val MatrixRegBankId = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).MatrixRegBankId
                val MatrixRegAddr = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).MatrixRegAddr
                val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
                val FIFOIndex = Bank_Fill_Search_FIFO_Head(MatrixRegBankId)

                MReg_Fill_Table(MReg_Fill_Table_Insert_Index) := ResponseData
                MReg_Fill_Table_MReg_Addr(MReg_Fill_Table_Insert_Index) := MatrixRegAddr
                MReg_Fill_Table_Time(MReg_Fill_Table_Insert_Index) := MAX_Fill_Times.U
                Bank_Fill_Search_FIFO(MatrixRegBankId)(FIFOIndex) := MReg_Fill_Table_Insert_Index
                Bank_Fill_Search_FIFO_Head(MatrixRegBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(MatrixRegBankId), AMemoryLoaderReadFromMemoryFIFODepth)
                if (YJPAMLDebugEnable){
                    printf("[AML<%d>]Response, Data:%x, BankId:%d, RegAddr:%d\n", io.DebugInfo.DebugTimeStampe, ResponseData, MatrixRegBankId, MatrixRegAddr)
                }
            }

            val Current_Fill_MReg_Time = WireInit(VecInit(Seq.fill(ABMatrixRegNBanks)(0.U(1.W))))
            for (i <- 0 until ABMatrixRegNBanks){
                when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
                    val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
                    Current_Fill_MReg_Time(i) := 1.U
                    val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U((8*ABMatrixRegEntryByteSize).W)))))
                    FIFOData := MReg_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
                    io.ToMatrixRegIO.BankAddr(i).bits := MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex) + (MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex))
                    io.ToMatrixRegIO.BankAddr(i).valid := true.B
                    io.ToMatrixRegIO.Data(i).bits := FIFOData(MAX_Fill_Times.U - MReg_Fill_Table_Time(CurrentFIFOIndex))
                    io.ToMatrixRegIO.Data(i).valid := true.B
                    MReg_Fill_Table_Time(CurrentFIFOIndex) := MReg_Fill_Table_Time(CurrentFIFOIndex) - 1.U
                    when(MReg_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
                        Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), AMemoryLoaderReadFromMemoryFIFODepth)
                    }
                    if (YJPAMLDebugEnable){
                        printf("[AML<%d>]Fill bank:%d, RegAddr:%x, Time:%d\n", io.DebugInfo.DebugTimeStampe, i.U, MReg_Fill_Table_MReg_Addr(CurrentFIFOIndex), MReg_Fill_Table_Time(CurrentFIFOIndex))
                    }
                }
            }
            val Current_Load_Fill_Size = PopCount(Current_Fill_MReg_Time.asUInt)
            TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size
            if(YJPAMLDebugEnable){
                when(Current_Load_Fill_Size =/= 0.U) {
                    printf("[AML<%d>]TotalLoadSize:%d, FillSize:%d, Max:%d\n", io.DebugInfo.DebugTimeStampe, TotalLoadSize, Current_Load_Fill_Size, MaxRequestIter * MAX_Fill_Times.U)
                }
            }
            when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
                memoryload_state := s_load_end
                if (YJPAMLDebugEnable) printf("[AML<%d>]FullLoadEnd\n", io.DebugInfo.DebugTimeStampe)
            }
        }
    }.elsewhen(memoryload_state === s_load_end){
        ConfigInfo.MicroTaskEndValid := true.B
        when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
            memoryload_state := s_load_idle
            state := s_idle
            if(YJPAMLDebugEnable) printf("[AML<%d>]AMemoryLoader Task End\n", io.DebugInfo.DebugTimeStampe)
        }
    }
}
