
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

//MatrixTE
//该模块的设计目标是，外积模块，组织多个ReducePE，来复用矩阵乘的数据，MatrixTE接受ABMatrixReg的数据，交给broadcaster，生成数据馈送至至ReducePE，将Reduce的输出数据准备馈送至MatrixRegC。
//计算上来看，它的输入是两个向量，将两个向量广播成两个相同大小的矩阵后，将元素送入ReducuPE。向量的大小也很明显其一是Matrix_M，其二是Matrix_N，向量内的元素宽度是Reduce_Width
class MatrixTE(implicit p: Parameters) extends CuteModule{
    val io = IO(new Bundle{
        val VectorA = Flipped(DecoupledIO(UInt((ReduceWidth*Matrix_MN).W)))
        val VectorB = Flipped(DecoupledIO(UInt((ReduceWidth*Matrix_MN).W)))
        val ScaleA  = Option.when(cuteMatrixExtension.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ScaleWidth*Matrix_MN).W)))
        )
        val ScaleB  = Option.when(cuteMatrixExtension.enableScalingFactor)(
          Flipped(DecoupledIO(UInt((ScaleWidth*Matrix_MN).W)))
        )
        val ScaleALiveK = Option.when(cuteMatrixExtension.enableScalingFactor)(
            Input(Vec(Matrix_MN, UInt(log2Ceil(ReduceWidth / 4 + 1).W))))
        val ScaleBLiveK = Option.when(cuteMatrixExtension.enableScalingFactor)(
            Input(Vec(Matrix_MN, UInt(log2Ceil(ReduceWidth / 4 + 1).W))))
        val MatrixC = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
        val MatrixD = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
        // Atomic sideband of MatrixD; CDC owns the active output-element mask.
        val DFlags = Output(Vec(Matrix_MN*Matrix_MN, UInt(5.W)))
        val ConfigInfo = Flipped((new MTEMicroTaskConfigIO))
        val ComputeGo            = Output(Bool())
        val DebugInfo     = Input(new DebugInfoIO)
        val idle = Output(Bool())
    })
    dontTouch(io)

    //实例化ReducePE
    val Matrix = VecInit.tabulate(Matrix_MN, Matrix_MN){(x,y) => Module(new FReducePE()(cuteFpeParameters)).io}

    // 配置运算类型
    val ComputeType = RegInit(MteComputeType.ComputeTypeUndef)

    when (io.ConfigInfo.MicroTaskValid) {
        ComputeType := io.ConfigInfo.computeType
    }

    // Scaled operations must consume one aligned A/B/C/scale tuple, not five
    // independent ready-only handshakes. Keep an empty-input priming cycle:
    // the controllers need ComputeGo to initiate their synchronous SRAM reads.
    val scaledOp = ComputeType === MteComputeType.Mxfp4F32 ||
        ComputeType === MteComputeType.Nvfp4F32 ||
        ComputeType === MteComputeType.Mxfp8e4m3F32 ||
        ComputeType === MteComputeType.Mxfp8e5m2F32
    val scaleValids = io.ScaleA.zip(io.ScaleB).map { case (a, b) => a.valid && b.valid }.getOrElse(false.B)
    val anyScaleValid = io.ScaleA.zip(io.ScaleB).map { case (a, b) => a.valid || b.valid }.getOrElse(false.B)
    val tupleValid = io.VectorA.valid && io.VectorB.valid && io.MatrixC.valid && scaleValids
    val anyInputValid = io.VectorA.valid || io.VectorB.valid || io.MatrixC.valid || anyScaleValid

    //直接驱动ReducePE的输入
    //接下来给每个ReducePE的输入进行赋值
    //broadcaster的逻辑
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            Matrix(i)(j).AVector.bits       := io.VectorA.bits((i+1)*ReduceWidth-1,(i)*ReduceWidth)
            Matrix(i)(j).AVector.valid      := Mux(scaledOp, tupleValid, io.VectorA.valid)
            Matrix(i)(j).BVector.bits       := io.VectorB.bits((j+1)*ReduceWidth-1,(j)*ReduceWidth)
            Matrix(i)(j).BVector.valid      := Mux(scaledOp, tupleValid, io.VectorB.valid)
            io.ScaleA.zip(io.ScaleB).zip(Matrix(i)(j).AScale.zip(Matrix(i)(j).BScale)).foreach {
                case ((scaleA, scaleB), (peAScale, peBScale)) =>
                    peAScale.bits        := scaleA.bits((i+1)*ScaleWidth-1,(i)*ScaleWidth)
                    peAScale.valid       := Mux(scaledOp, tupleValid, scaleA.valid)
                    peBScale.bits        := scaleB.bits((j+1)*ScaleWidth-1,(j)*ScaleWidth)
                    peBScale.valid       := Mux(scaledOp, tupleValid, scaleB.valid)
            }
            Matrix(i)(j).CAdd.bits          := io.MatrixC.bits((i*Matrix_MN+j+1)*ResultWidth-1,(i*Matrix_MN+j)*ResultWidth)
            Matrix(i)(j).CAdd.valid         := Mux(scaledOp, tupleValid, io.MatrixC.valid)
            Matrix(i)(j).opcode             := ComputeType
            Matrix(i)(j).LiveK.foreach { live =>
                val a = io.ScaleALiveK.get(i)
                val b = io.ScaleBLiveK.get(j)
                // A padded output row/column has no live products. CDC retains
                // ownership of task20's architectural output/flags masking.
                live := Mux(a < b, a, b)
                when(scaledOp && tupleValid && a =/= 0.U && b =/= 0.U) {
                    assert(a === b, "MTE scale/data tuples must describe the same live K")
                }
            }
            when(io.VectorA.valid && io.VectorB.valid && io.MatrixC.valid){
                // printf("[MatrixTE]: Matrix(%d)(%d) ReduceA:%x ReduceB:%x AddC:%x\n",i.U,j.U,Matrix(i)(j).ReduceA.bits,Matrix(i)(j).ReduceB.bits,Matrix(i)(j).AddC.bits)
            }
        }
    }

    //将每个ReducePE的输出拼接成一个矩阵，然后送入MatrixD
    val CurrentMatrixD = Wire(Vec(Matrix_MN*Matrix_MN, UInt(ResultWidth.W)))
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            CurrentMatrixD(i*Matrix_MN+j) := Matrix(i)(j).DResult.bits
            io.DFlags(i*Matrix_MN+j) := Matrix(i)(j).DFlags
            assert(Matrix(i)(j).DResult.valid === Matrix(0)(0).DResult.valid,
                "MTE result and flags lanes must remain lockstep")
        }
    }
    //这里asUInt可以将Vec(UInt)转换成UInt
    io.MatrixD.bits := CurrentMatrixD.asUInt
    io.MatrixD.valid := Matrix(0)(0).DResult.valid
    //如果Matrix(i)(j)的每个vali都为true，那么MatrixD的valid才为true
    // val YJP_valid_count = RegInit(0.U(2.W))
    // val YJP_Count_number = RegInit(0.U(32.W))
    // when(io.VectorA.valid && io.VectorB.valid){
    //     YJP_valid_count := YJP_valid_count + 1.U
    //     YJP_Count_number := YJP_Count_number + 1.U
    //     when(YJP_valid_count === 3.U){
    //         io.MatrixD.bits := YJP_Count_number
    //         io.MatrixD.valid := true.B
    //     }.otherwise{
    //         io.MatrixD.valid := false.B
    //     }
    // }
    // io.MatrixD.valid := Matrix.map(_.map(_.ResultD.valid).reduce(_&&_)).reduce(_&&_)

    //TODO:注意，这里的ready可以不用这么判断，我们所有PE是锁步的，能保证所有PE的数据同时到达，所以只要考察一个PE即可
    //确定所有的ready信号
    //当所有的ReducePE的输入都ready的时候，VectorA和VectorB的ready才为true
    //注意这里如果是时序不足的点，很简单只用考察一个PE即可，因为所有PE是同步执行的，这里这样写是保证逻辑完整完备，代码可读性高
    val baseInputReady = Matrix(0)(0).AVector.ready && Matrix(0)(0).BVector.ready && Matrix(0)(0).CAdd.ready
    val scaleInputReady = Matrix(0)(0).AScale.zip(Matrix(0)(0).BScale).map { case (aScale, bScale) =>
        aScale.ready && bScale.ready
    }.getOrElse(true.B)
    val ReducePEInputAllReady = baseInputReady && scaleInputReady &&
        (!scaledOp || tupleValid || !anyInputValid)
    io.VectorA.ready := ReducePEInputAllReady
    io.VectorB.ready := ReducePEInputAllReady
    io.ScaleA.foreach(_.ready := ReducePEInputAllReady)
    io.ScaleB.foreach(_.ready := ReducePEInputAllReady)
    io.MatrixC.ready := ReducePEInputAllReady

    // Six valid registers in canonical FReducePE bound tuple occupancy to six.
    val pendingTokens = RegInit(0.U(3.W))
    val tupleFire = io.VectorA.fire && io.VectorB.fire && io.MatrixC.fire &&
        (!scaledOp || scaleValids)
    when(tupleFire =/= io.MatrixD.fire) {
        pendingTokens := Mux(tupleFire, pendingTokens + 1.U, pendingTokens - 1.U)
    }
    when(io.MatrixD.fire) {
        assert(pendingTokens =/= 0.U || tupleFire, "MTE returned an unowned D token")
    }
    when(tupleFire && !io.MatrixD.fire) {
        assert(pendingTokens < 6.U, "MTE exceeded canonical six-stage FPE capacity")
    }
    io.idle := pendingTokens === 0.U && !anyInputValid && !io.MatrixD.valid &&
        !io.ConfigInfo.MicroTaskValid
    
    assert(io.VectorA.fire === io.VectorB.fire, "VectorA and VectorB should be fired at the same time")
    when(scaledOp && (io.VectorA.fire || io.VectorB.fire || io.MatrixC.fire ||
        io.ScaleA.map(_.fire).getOrElse(false.B) || io.ScaleB.map(_.fire).getOrElse(false.B))) {
        assert(tupleValid, "scaled MTE must accept data, C and both scales atomically")
    }

    if(YJPMACDebugEnable)
    {
        when(io.VectorA.fire)
        {
            printf("[MatrixTE<%d>]: VectorA:%x\n",io.DebugInfo.DebugTimeStampe,io.VectorA.bits)
        }
        when(io.VectorB.fire)
        {
            printf("[MatrixTE<%d>]: VectorB:%x\n",io.DebugInfo.DebugTimeStampe,io.VectorB.bits)
        }
        when(io.MatrixC.fire)
        {
            printf("[MatrixTE<%d>]: MatrixC:%x\n",io.DebugInfo.DebugTimeStampe,io.MatrixC.bits)
        }
        when(io.MatrixD.fire)
        {
            printf("[MatrixTE<%d>]: MatrixD:%x\n",io.DebugInfo.DebugTimeStampe,io.MatrixD.bits)
        }
    }
    //全体PE对DC进行的完全同步的控制信号，保证每个PE的迭代器同步更新，保证每个Internal Reduce Dim的第一个次计算将C的数据送入PE，最后一次计算将PE的数据送回D
    io.ComputeGo := ReducePEInputAllReady

    //越浅的fifo，越少的能量消耗～
    //直接送到CMatrixReg的数据，是最香的
    //只要MatrixD的是ready的，就可以送数据
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            Matrix(i)(j).DResult.ready := io.MatrixD.ready
        }
    }

    //ReducePE和MatrixTE需要一个对于externalReduce的处理，以提高热效率，提供主频，减少对CMatrixReg的访问
    //ExternalReduce是指，我们的MatrixReg内的Tensor的K维大于1时，可以减少从CMatrixReg的访问数据，让ReducePE使用自己暂存的累加结果后，再存至CMatrixReg
    //Trick：再来，这里的K越大，我们的CSratchPad的平均访问次数就越少，就可以使用更慢更大的SRAM
    //256/8 = 32，在K大小为256时，我们的ExternalK可以是256/32 = 8,如果K=128，那么ExternalK=4。此时对CSratchPad进行一次读写的时间就只有4个周期，虽然好像也没有问题。
    //对于一个读+一个写端口的SRAM，我们确实可以在同一个周期内完成一次读一次写。但需要doublebuffer来阻挡从Memory到SRAM的数据。也就是K=32的情况，K=32的情况，用向量做Reduce就行了？

    //核心问题就是向量和矩阵算力的配比～～
    //向量算力和矩阵算力的配比，就是在roofline图上，找的那个说得通的点，证明我们的设计是合理的～

    //算的时候，reducePE可以对256的数据进行n种不同的计算，256/8 = 32。
    //这个(32,32)元素，送到PE里，可以得到(1,1)，(2,2)，(4,4)等的结果，影响ResultWidth的宽度，这会导致CMatrixReg的访存带宽相应的提升
    //ResultWidth比较宽，导致CMatrixReg宽度过宽，比如(4,4)的就需要(4*4)(4*4)*32bit,4096bit的读写，但是C有个好处就是不需要每周期都读写4096，可以花4个周期读，再画4个周期写。这样就是要求ExternalK必须大于4+4(单读写口的话)，也就是对于张量来说最小是(4+4)(8)。张量是16*16*64的张量。



}
