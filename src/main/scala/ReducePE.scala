
// package cute

// import chisel3._
// import chisel3.util._
// import org.chipsalliance.cde.config._
// // import boom.exu.ygjk._
// import boom.v3.util._

// class ReduceMACTree8(id:Int)(implicit p: Parameters) extends CuteModule{
//     val io = IO(new Bundle{
//         val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val CAdd    = Flipped(DecoupledIO(UInt(ResultWidth.W)))
//         val DResult = Valid(UInt(ResultWidth.W))
//         val Chosen = Input(Bool())
//         val FIFOReady = Input(Bool())
//         val working = Output(Bool())
//         val ExternalReduceSize = Input(UInt(ScaratchpadMaxTensorDimBitSize.W))
//     })
//     //FIFOReady置高，所有寄存器向下流一个流水级
//     //Chosen置高，该加法树工作被选择为工作加法树
//     //working置高，在加法树工作中
//     //完成加法树部分即可，ABC fire，且Ready置高，则DResult置valid
    
//     //累加ExternalReduceSize次，完成一次计算，置DResult为valid

//     //TODO:init
//     io.AVector.ready := DontCare
//     io.BVector.ready := DontCare
//     io.CAdd.ready := true.B
//     io.DResult.valid := true.B
//     io.DResult.bits := DontCare
//     io.working := false.B

//     //将A、B解释为8bit位宽的数据
//     val pure_AVectorList = Wire(Vec(ReduceWidth/8, UInt(8.W)))
//     val pure_BVectorList = Wire(Vec(ReduceWidth/8, UInt(8.W)))
//     val AVectorList = Wire(Vec(ReduceWidth/8, SInt(8.W)))
//     val BVectorList = Wire(Vec(ReduceWidth/8, SInt(8.W)))

//     pure_AVectorList := io.AVector.bits.asTypeOf(pure_AVectorList)
//     pure_BVectorList := io.BVector.bits.asTypeOf(pure_BVectorList)
//     for(i <- 0 until ReduceWidth/8){
//         AVectorList(i) := pure_AVectorList(i).asSInt
//         BVectorList(i) := pure_BVectorList(i).asSInt
//     }

//     //将A、B的数据分别乘，int8的肯定一拍能做完乘法
//     val MULResult = Wire(Vec(ReduceWidth/8, SInt(16.W)))
//     for(i <- 0 until ReduceWidth/8){
//         MULResult(i) := AVectorList(i) * BVectorList(i)
//     }
//     if(id == 0){
//         when(io.AVector.valid && io.BVector.valid){
//             if (YJPPEDebugEnable)
//             {
//                 printf("[YJPDebug]PE AVector: %x\n",io.AVector.bits)
//                 printf("[YJPDebug]PE BVector: %x\n",io.BVector.bits)
//                 printf("[YJPDebug]PE MULResult: %x\n",MULResult.asUInt)
//             }
//         }
        
//     }
    
//     //将MULResult的数据累加
//     //这里的累加树需要额外设计，以满足时钟频率
    
//     //reducetree的总层数
//     val ReduceTreeDepth = log2Ceil(ReduceWidth/8)
//     //每一层有多少个数据，第0层1个，第1层2个，第2层4个.....
//     val ReduceLevelElement = (0 until ReduceTreeDepth + 1).map(x => math.pow(2,x).toInt)
//     // val RegIndexList = (0 until ReduceTreeDepth + 1).map(x => x == )
//     val ReduceTreeWidth = 8+8+log2Ceil(ReduceWidth/8)

//     //所有层的数据，列成一个2维的vector,全都初始化为0
//     val ReduceTreeData = WireInit(VecInit(Seq.fill(ReduceTreeDepth +1)(VecInit(Seq.fill(ReduceWidth/8)(0.S(ReduceTreeWidth.W))))))
//     val ReduceTreeRegData = RegInit(VecInit(Seq.fill(ReduceTreeDepth +1)(VecInit(Seq.fill(ReduceWidth/8)(0.S(ReduceTreeWidth.W))))))
//     val ReduceTreeValid = WireInit(VecInit(Seq.fill(ReduceTreeDepth +1)(false.B)))
//     val ReduceTreeRegValid = RegInit(VecInit(Seq.fill(ReduceTreeDepth +1)(false.B)))
    
//     ReduceTreeData(ReduceTreeDepth) := MULResult
//     ReduceTreeValid(ReduceTreeDepth) := io.AVector.valid && io.BVector.valid

//     //写一个for来描述ReduceTreeData的数据、
//     for(i <- 0 until ReduceTreeDepth){
//         for(j <- 0 until ReduceLevelElement(i)){
//             ReduceTreeData(i)(j) := ReduceTreeData(i+1)(2*j) + ReduceTreeData(i+1)(2*j+1)
//             ReduceTreeValid(i) := ReduceTreeValid(i+1)
//             if(i==3)
//             {
//                 ReduceTreeRegData(i) := Mux(io.FIFOReady,ReduceTreeData(i),ReduceTreeRegData(i)) //这里的FIFOReady是外部的信号，用于控制ReduceTreeData的数据是否向下流
//                 ReduceTreeRegValid(i) := Mux(io.FIFOReady,ReduceTreeValid(i),ReduceTreeRegValid(i))
//             }
//             if(i==2)
//             {
//                 ReduceTreeData(i)(j) := ReduceTreeRegData(i+1)(2*j) + ReduceTreeRegData(i+1)(2*j+1)
//                 ReduceTreeValid(i) := ReduceTreeRegValid(i+1)
//             }
//         }
//         when(ReduceTreeValid(i)){
//             if(id == 0){
//                 //得到ReduceTreeWidth，4的整数倍的值
//                 val DebugReduceTreeWidth = (ReduceTreeWidth/4)*4
//                 val DebugReduceTreeData_i = Wire(Vec(ReduceWidth/8,SInt(DebugReduceTreeWidth.W)))
//                 DebugReduceTreeData_i := 0.S.asTypeOf(DebugReduceTreeData_i)
//                 for(j <- 0 until ReduceLevelElement(i))
//                 {
//                     DebugReduceTreeData_i(j) := ReduceTreeData(i)(j)
//                 }
//                 if (YJPPEDebugEnable)
//                 {
//                     printf("[YJPDebug]PE ReduceTreeData(%d): %x\n",i.U,DebugReduceTreeData_i.asUInt)
//                 }
//             }
//         }
//     }

    
//     //目前就在第3层切了一级流水，

//     //过ExternalReduceSize个周期，会有一个CAdd的数据，将这ExternalReduceSize个周期的结果累加并累加到CAdd上，完成一次计算
//     val ExternalReduceSize = ReduceGroupSize //(这里得改，需要加一个参数，考虑Tensor_K参与计算的次数)
//     //比如TensorMNK(128,128,128)
//     //MatrixMNK(16,16,8)
//     //那这里的ExternalReduceSize就是128/8
//     //目前写的是MatrixMNK(4,4,32)，所以ExternalReduceSize就是4
//     val PEPipeLineDepth = 2 //ReduceTreePipeline，这里的2是指ReduceTree的流水线深度
//     //如果CDelayFIFODepth值小于2则设置值为2
//     val CDelayFIFODepth =  if(PEPipeLineDepth > 2) PEPipeLineDepth else 2 //CAdd的FIFO深度，用于暂存C的数据，等待ReduceTree的数据全部计算完毕
//     val CExternalReduceAddChain = RegInit(VecInit(Seq.fill(ExternalReduceSize)(0.S(ResultWidth.W))))
//     val CExternalReduceAddChainValid = RegInit(VecInit(Seq.fill(ExternalReduceSize)(false.B)))

//     //CAdd的FIFO
//     val CDelayFIFO = RegInit(VecInit(Seq.fill(CDelayFIFODepth)(0.S(ResultWidth.W))))
//     val CDelayFIFOHead = RegInit(0.U(log2Ceil(CDelayFIFODepth).W))
//     val CDelayFIFOTail = RegInit(0.U(log2Ceil(CDelayFIFODepth).W))
//     val CDelayFIFOFull = CDelayFIFOTail === WrapInc(CDelayFIFOHead, CDelayFIFODepth)
//     val CDelayFIFOEmpty = CDelayFIFOHead === CDelayFIFOTail

//     when(io.CAdd.valid && io.FIFOReady){
//         when(!CDelayFIFOFull){
//             CDelayFIFO(CDelayFIFOHead) := io.CAdd.bits.asSInt
//             if(id == 0)
//             {
//                 if (YJPPEDebugEnable)
//                 {
//                     printf("[YJPDebug]PE CDelayFIFO: %x\n",io.CAdd.bits)
//                 }
//             }

//             when(CDelayFIFOHead+1.U===CDelayFIFODepth.U){
//                 CDelayFIFOHead := 0.U
//             }.otherwise{
//                 CDelayFIFOHead := CDelayFIFOHead + 1.U
//             }
//         }.otherwise{
//             assert(false.B, "[YJPDebug]PE FIFO CDelayFIFOFull")
//         }
//     }

//     CExternalReduceAddChainValid(0) := CExternalReduceAddChainValid(0)
//     val ExternalReduceConunter = RegInit(0.U(log2Ceil(ExternalReduceSize).W))
//     when(io.FIFOReady){
//         when(ReduceTreeValid(0))
//         {
//             CExternalReduceAddChainValid(0) := false.B
//             ExternalReduceConunter := ExternalReduceConunter + 1.U
//             when(ExternalReduceConunter === (ExternalReduceSize-1).U)
//             {
//                 ExternalReduceConunter := 0.U
//             }
//             when(!CDelayFIFOEmpty && ExternalReduceConunter === 0.U){
//                 CExternalReduceAddChain(0) := ReduceTreeData(0)(0) + CDelayFIFO(CDelayFIFOTail)
//                 CExternalReduceAddChainValid(0) := true.B
//                 if(id==0){
//                     if (YJPPEDebugEnable)
//                     {
//                         printf("[YJPDebug]PE CExternalReduceAddChain[0]: %x\n",ReduceTreeData(0)(0) + CDelayFIFO(CDelayFIFOTail))
//                     }
//                 }
                
//                 when(CDelayFIFOTail+1.U===CDelayFIFODepth.U){
//                     CDelayFIFOTail := 0.U
//                 }.otherwise{
//                     CDelayFIFOTail := CDelayFIFOTail + 1.U
//                 }
//             }.otherwise{
//                 when(ExternalReduceConunter === 0.U && CDelayFIFOEmpty)
//                 {
//                     assert(false.B, "[YJPDebug]PE FIFO CDelayFIFOEmpty")
//                 }
//             }
//         }
//     }

//     for(i <- 1 until ExternalReduceSize){
//         CExternalReduceAddChainValid(i) := Mux(io.FIFOReady,CExternalReduceAddChainValid(i-1),CExternalReduceAddChainValid(i))
//         when(CExternalReduceAddChainValid(i-1) && io.FIFOReady){
//             CExternalReduceAddChain(i) := CExternalReduceAddChain(i-1) + ReduceTreeData(0)(0)
//             if(id==0)
//             {
//                 if (YJPPEDebugEnable)
//                 {
//                     printf("[YJPDebug]PE CExternalReduceAddChain[%d]: %x\n",i.U,CExternalReduceAddChain(i-1)+ReduceTreeData(0)(0))
//                 }
//             }
//         }
//     }

//     when(CExternalReduceAddChainValid(ExternalReduceSize-1)){
//         io.DResult.bits := CExternalReduceAddChain(ExternalReduceSize-1).asUInt
//         io.DResult.valid := true.B
//         if(id==0)
//         {
//             if (YJPPEDebugEnable)
//             {
//                 printf("[YJPDebug]PE DResult: %x\n",io.DResult.bits)
//             }
//         }
//     }.otherwise{
//         io.DResult.valid := false.B
//     }
    
// }

// class ReduceMACTree16(implicit p: Parameters) extends CuteModule{
//     val io = IO(new Bundle{
//         val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val CAdd    = Flipped(DecoupledIO(UInt(ResultWidth.W)))
//         val DResult = Valid(UInt(ResultWidth.W))
//         val Chosen = Input(Bool())
//         val FIFOReady = Input(Bool())
//         val working = Output(Bool())
//         val ExternalReduceSize = Input(UInt(ScaratchpadMaxTensorDimBitSize.W))
//     })
//     io.AVector.ready := false.B
//     io.BVector.ready := false.B
//     io.CAdd.ready := false.B
//     io.DResult.valid := false.B
//     io.DResult.bits := DontCare
//     io.working := false.B
// }

// class ReduceMACTree32(implicit p: Parameters) extends CuteModule{
//     val io = IO(new Bundle{
//         val AVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val BVector = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val CAdd    = Flipped(DecoupledIO(UInt(ResultWidth.W)))
//         val DResult = Valid(UInt(ResultWidth.W))
//         val Chosen = Input(Bool())
//         val FIFOReady = Input(Bool())
//         val working = Output(Bool())
//         val ExternalReduceSize = Input(UInt(ScaratchpadMaxTensorDimBitSize.W))
//     })
//     io.AVector.ready := false.B
//     io.BVector.ready := false.B
//     io.CAdd.ready := false.B
//     io.DResult.valid := false.B
//     io.DResult.bits := DontCare
//     io.working := false.B
// }

// //单个ReducePE, 计算Reduce乘累加的结果
// class ReducePE(id:Int)(implicit p: Parameters) extends CuteModule{
//     val io = IO(new Bundle{
//         val ReduceA = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val ReduceB = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val AddC    = Flipped(DecoupledIO(UInt(ReduceWidth.W)))
//         val ResultD = DecoupledIO(UInt(ResultWidth.W))
//         val ConfigInfo = Flipped((new MTEMicroTaskConfigIO))
//         // val ExternalReduceSize = Flipped(DecoupledIO(UInt(ScaratchpadMaxTensorDimBitSize.W)))
//     })

//     //TODO:init
//     io.ReduceA.ready := false.B
//     io.ReduceB.ready := false.B
//     io.AddC.ready := false.B
//     io.ResultD.valid := false.B
//     io.ResultD.bits := 0.U
//     io.ConfigInfo.ready := false.B


//     //ReducePE和MatrixTE需要一个对于externalReduce的处理，以提高热效率，提供主频，减少对CScratchPad的访问
//     //ExternalReduce是指，我们的Scarchpad内的Tensor的K维大于1时，可以减少从CScratchPad的访问数据，让ReducePE使用自己暂存的累加结果后，再存至CScratchPad
//     //Trick：再来，这里的K越大，我们的CSratchPad的平均访问次数就越少，就可以使用更慢更大的SRAM
//     val ReduceMAC8 = Module(new ReduceMACTree8(id))
//     ReduceMAC8.io.AVector <> io.ReduceA
//     ReduceMAC8.io.BVector <> io.ReduceB
//     ReduceMAC8.io.CAdd    <> io.AddC
//     ReduceMAC8.io.Chosen  := false.B
//     ReduceMAC8.io.FIFOReady   := false.B
//     ReduceMAC8.io.ExternalReduceSize := ReduceGroupSize.U

//     val ReduceMAC16 = Module(new ReduceMACTree16)
//     ReduceMAC16.io.AVector <> io.ReduceA
//     ReduceMAC16.io.BVector <> io.ReduceB
//     ReduceMAC16.io.CAdd    <> io.AddC
//     ReduceMAC16.io.Chosen  := false.B
//     ReduceMAC16.io.FIFOReady   := false.B
//     ReduceMAC16.io.ExternalReduceSize := ReduceGroupSize.U

//     val ReduceMAC32 = Module(new ReduceMACTree32)
//     ReduceMAC32.io.AVector <> io.ReduceA
//     ReduceMAC32.io.BVector <> io.ReduceB
//     ReduceMAC32.io.CAdd    <> io.AddC
//     ReduceMAC32.io.Chosen  := false.B
//     ReduceMAC32.io.FIFOReady   := false.B
//     ReduceMAC32.io.ExternalReduceSize := ReduceGroupSize.U

//     //只有在数据类型匹配时才能进行计算
//     //在Reduce内完成数据的握手，及所有数据准备好后才能进行计算，并用一个fifo保存ResultD，等待ResultD被握手
//     val ResultFIFO = RegInit(VecInit(Seq.fill(ResultFIFODepth)(0.U(ResultWidth.W))))
//     val ResultFIFOHead = RegInit(0.U(log2Ceil(ResultFIFODepth).W))
//     val ResultFIFOTail = RegInit(0.U(log2Ceil(ResultFIFODepth).W))
//     val ResultFIFOFull = ResultFIFOTail === WrapInc(ResultFIFOHead, ResultFIFODepth)
//     val ResultFIFOEmpty = ResultFIFOHead === ResultFIFOTail
//     val ResultFIFOValid = WireInit(false.B)


//     //数据类型，整个计算过程中只有一个数据类型，ConfigInfo不会改变
//     val dataType = RegInit(ElementDataType.DataTypeUndef)
//     //PE不工作且FIFO为空时，才能接受新的配置信息
//     val PEWorking = ReduceMAC8.io.working || ReduceMAC16.io.working || ReduceMAC32.io.working
//     io.ConfigInfo.ready := !PEWorking && ResultFIFOEmpty
//     when(io.ConfigInfo.valid && io.ConfigInfo.ready){
//       dataType := io.ConfigInfo.dataType
//       dataType := ElementDataType.DataTypeSInt8 //默认改成SInt8
//       if(id == 0)
//       {
//           if (YJPPEDebugEnable)
//           {
//               printf("[YJPDebug]PE ConfigInfo: %x\n",io.ConfigInfo.dataType)
//               printf("[YJPDebug]PE Start\n")
//           }
//       }
//     }
    


//     //根据数据类型选择不同的ReduceMAC,作为CurrentResultD的数据源，由于configinfo不会改变，所以这里的DResult不用改变，并设置Valid信号
//     val CurrentResultD = Wire(Valid(UInt(ResultWidth.W)))
//     CurrentResultD := DontCare
//     when(dataType===ElementDataType.DataTypeSInt8){
//         ReduceMAC8.io.AVector <> io.ReduceA
//         ReduceMAC8.io.BVector <> io.ReduceB
//         ReduceMAC8.io.CAdd    <> io.AddC
//         CurrentResultD := ReduceMAC8.io.DResult
//         ReduceMAC8.io.Chosen := true.B
//     }.elsewhen(dataType===ElementDataType.DataTypeUInt16){
//         ReduceMAC16.io.AVector <> io.ReduceA
//         ReduceMAC16.io.BVector <> io.ReduceB
//         ReduceMAC16.io.CAdd    <> io.AddC
//         CurrentResultD := ReduceMAC16.io.DResult
//         ReduceMAC16.io.Chosen := true.B
//     }.elsewhen(dataType===ElementDataType.DataTypeUInt32){
//         ReduceMAC32.io.AVector <> io.ReduceA
//         ReduceMAC32.io.BVector <> io.ReduceB
//         ReduceMAC32.io.CAdd    <> io.AddC
//         CurrentResultD := ReduceMAC32.io.DResult
//         ReduceMAC32.io.Chosen := true.B
//     }.otherwise{
//         CurrentResultD.valid := false.B
//     }

    


//     when(CurrentResultD.valid){
//       when(!ResultFIFOFull){
//         ResultFIFO(ResultFIFOHead) := CurrentResultD.bits
//         when(ResultFIFOHead+1.U===ResultFIFODepth.U){
//           ResultFIFOHead := 0.U
//         }.otherwise{
//           ResultFIFOHead := ResultFIFOHead + 1.U
//         }
//         if(id == 0)
//         {
//             if (YJPPEDebugEnable)
//             {
//                 printf("[YJPDebug]PE ResultFIFO Insert: %x\n",CurrentResultD.bits)
//             }
//         }
//       }.otherwise{
//         // printf(p"ResultFIFOFull\n")
//         if(id == 0)
//         {
//             if (YJPPEDebugEnable)
//             {
//                 printf("[YJPDebug]PE FIFO ResultFIFOFull\n")
//             }
//         }
//       }
//     }


//     when(ResultFIFOEmpty){
//         ResultFIFOValid := false.B
//     }.otherwise{
//         io.ResultD.bits := ResultFIFO(ResultFIFOTail)
//         ResultFIFOValid := true.B
//         io.ResultD.valid := true.B
//         when(io.ResultD.fire){
//             when(ResultFIFOTail+1.U===ResultFIFODepth.U){
//                 ResultFIFOTail := 0.U
//             }.otherwise{
//                 ResultFIFOTail := ResultFIFOTail + 1.U
//             }
//             if(id == 0)
//             {
//                 if (YJPPEDebugEnable)
//                 {
//                     printf("[YJPDebug]PE ResultFIFO Pop: %x\n",io.ResultD.bits)
//                 }
//             }
//         }
//     }


//     //数据源ReduceA ReduceB AddC什么时候能置ready？
//     //全部valid的时候才可以，同时当前流水下的所有数据都能在fifo中存的下，才能置ready
//     //方案1:已知MACTree的流水线深度，已知ResultFIFO的深度，可以得出ResultFIFO存的数据达到某个深度时，可以安全的接受新的数据
//     //方案2：直接用FIFO满没满确定是否ready，整体流水线都受这个制约，好像有点粗暴？只要ready，所有数据往下流一个流水级，否则不动
//     val InputReady = ResultFIFOFull===false.B
//     io.ReduceA.ready := InputReady
//     io.ReduceB.ready := InputReady
//     io.AddC.ready    := InputReady

//     //什么时候能接让MacTree的数据输入到fifo？
//     //MacTree的数据输入到fifo的时候，fifo不满，且MacTree的数据有效
//     val MacTreeReady = ResultFIFOFull===false.B
//     ReduceMAC8.io.FIFOReady := MacTreeReady
//     ReduceMAC16.io.FIFOReady := MacTreeReady
//     ReduceMAC32.io.FIFOReady := MacTreeReady

//     //输出的ResultD什么时候能置valid？
//     //ResultFIFO不为空时，才能置valid
//     // io.ResultD.valid := ResultFIFOValid

// }

