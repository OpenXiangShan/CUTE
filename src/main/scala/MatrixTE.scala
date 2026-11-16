
package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.exu.ygjk._

//MatrixTE
// Broadcast TR operands across the outer-product PE array and collect results for AccMatrixReg.
// Broadcast the two input vectors over Matrix_M rows and Matrix_N columns, with Reduce_Width-bit elements.
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
        val MatrixC = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W)))
        val MatrixD = DecoupledIO(UInt((ResultWidth*Matrix_MN*Matrix_MN).W))
        val ConfigInfo = Flipped((new MTEMicroTaskConfigIO))
        val ComputeGo            = Output(Bool())
        val DebugInfo     = Input(new DebugInfoIO)
    })
    dontTouch(io)

    // Instantiate reduction PEs.
    val Matrix = VecInit.tabulate(Matrix_MN, Matrix_MN){(x,y) => Module(new FReducePE()(cuteFpeParameters)).io}

    // Configure the operation type.
    val ComputeType = Reg(UInt(MteComputeType.ComputeTypeBitWidth.W))

    when (io.ConfigInfo.MicroTaskValid) {
        ComputeType := io.ConfigInfo.computeType
    }

    // Drive PE inputs directly.
    // Connect the input of each reduction PE.
    // Broadcast operand data.
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            Matrix(i)(j).AVector.bits       := io.VectorA.bits((i+1)*ReduceWidth-1,(i)*ReduceWidth)
            Matrix(i)(j).AVector.valid      := io.VectorA.valid
            Matrix(i)(j).BVector.bits       := io.VectorB.bits((j+1)*ReduceWidth-1,(j)*ReduceWidth)
            Matrix(i)(j).BVector.valid      := io.VectorB.valid
            io.ScaleA.zip(io.ScaleB).zip(Matrix(i)(j).AScale.zip(Matrix(i)(j).BScale)).foreach {
                case ((scaleA, scaleB), (peAScale, peBScale)) =>
                    peAScale.bits        := scaleA.bits((i+1)*ScaleWidth-1,(i)*ScaleWidth)
                    peAScale.valid       := scaleA.valid
                    peBScale.bits        := scaleB.bits((j+1)*ScaleWidth-1,(j)*ScaleWidth)
                    peBScale.valid       := scaleB.valid
            }
            Matrix(i)(j).CAdd.bits          := io.MatrixC.bits((i*Matrix_MN+j+1)*ResultWidth-1,(i*Matrix_MN+j)*ResultWidth)
            Matrix(i)(j).CAdd.valid         := io.MatrixC.valid
            Matrix(i)(j).opcode             := ComputeType
            when(io.VectorA.valid && io.VectorB.valid && io.MatrixC.valid){
                // printf("[MatrixTE]: Matrix(%d)(%d) ReduceA:%x ReduceB:%x AddC:%x\n",i.U,j.U,Matrix(i)(j).ReduceA.bits,Matrix(i)(j).ReduceB.bits,Matrix(i)(j).AddC.bits)
            }
        }
    }

    // Assemble PE outputs into MatrixD.
    val CurrentMatrixD = Wire(Vec(Matrix_MN*Matrix_MN, UInt(ResultWidth.W)))
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            CurrentMatrixD(i*Matrix_MN+j) := Matrix(i)(j).DResult.bits
        }
    }
    // asUInt packs Vec[UInt] into a UInt.
    io.MatrixD.bits := CurrentMatrixD.asUInt
    io.MatrixD.valid := Matrix(0)(0).DResult.valid
    // MatrixD is valid only when every PE output is valid.
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

    // TODO: lockstep PEs return together, so a single PE valid may be sufficient.
    // Combine readiness from all PEs.
    // VectorA and VectorB are ready only when every PE input is ready.
    // Lockstep execution may allow a single PE ready for timing; the full reduction makes the contract explicit.
    val baseInputReady = Matrix(0)(0).AVector.ready && Matrix(0)(0).BVector.ready && Matrix(0)(0).CAdd.ready
    val scaleInputReady = Matrix(0)(0).AScale.zip(Matrix(0)(0).BScale).map { case (aScale, bScale) =>
        aScale.ready && bScale.ready
    }.getOrElse(true.B)
    val ReducePEInputAllReady = baseInputReady && scaleInputReady
    io.VectorA.ready := ReducePEInputAllReady
    io.VectorB.ready := ReducePEInputAllReady
    io.ScaleA.foreach(_.ready := ReducePEInputAllReady)
    io.ScaleB.foreach(_.ready := ReducePEInputAllReady)
    io.MatrixC.ready := ReducePEInputAllReady
    
    assert(io.VectorA.fire === io.VectorB.fire, "VectorA and VectorB should be fired at the same time")

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
    // Keep PE and data-controller iterators in lockstep; read C at the first internal K step and write D at the last.
    io.ComputeGo := ReducePEInputAllReady

    // Shallower FIFOs consume less energy.
    // Prefer forwarding results directly to AccMatrixReg.
    // Forward data whenever MatrixD is ready.
    for (i <- 0 until Matrix_MN){
        for (j <- 0 until Matrix_MN){
            Matrix(i)(j).DResult.ready := io.MatrixD.ready
        }
    }

    // External reduction reuses PE accumulators to improve timing and reduce AccMatrixReg traffic.
    // For multiple K groups, retain partial sums in the PEs and write AccMatrixReg after the final group.
    // Larger K reduces average accumulator SRAM traffic and may permit slower, larger SRAMs.
    // Example: K=256 uses eight 32-byte groups; K=128 uses four, leaving four cycles per accumulator access.
    // A 1r1w SRAM can read and write together; double buffering separates memory fills from computation.

    // Balance vector and matrix compute throughput.
    // Use a roofline model to assess the vector-to-matrix throughput ratio.

    // A reduction PE can partition 256 input bits into several element shapes; 256 / 8 = 32.
    // Shapes such as (1,1), (2,2), and (4,4) change ResultWidth and accumulator bandwidth requirements.
    // A (4,4) shape needs 4096 result bits; spreading reads and writes over four cycles each requires ExternalK > 8.



}
