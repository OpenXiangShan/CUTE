package cute

import chisel3._
import chisel3.util._
import cute.CuteBundle
import org.chipsalliance.cde.config._

// TODO: use utilily here

/**
  * Produce named UInt(x.W)
  *
  * @example {{{
  *   object Fflags extends NamedUInt(5)
  *   val fflags = Fflags()
  * }}}
  */
abstract class NamedUInt(int : Int) {
  def apply(): UInt = UInt(width.W)

  def width: Int = int

  protected def checkInputWidth(uint: UInt): Unit = {
    require(
      uint.getWidth == this.width,
      s"the input UInt width(${uint.getWidth}) should be ${this.width}"
    )
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}

object Bundles {
  object MSew extends NamedUInt(3) {
    def e8  : UInt = "b000".U(width.W)
    def e16 : UInt = "b001".U(width.W)
    def e32 : UInt = "b010".U(width.W)
    def e64 : UInt = "b011".U(width.W)
    def e4  : UInt = "b111".U(width.W)

    def reserved = Seq(BitPat("b100"), BitPat("b101"), BitPat("b110"))

    def isReserved(sew: UInt) : Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        reserved.map(sew === _).reduce(_ || _)
      } else {
        false.B
      }
    }
  }

  object MSewOH extends NamedUInt(8) {
    def e8  : UInt = "b00000001".U(width.W)
    def e16 : UInt = "b00000010".U(width.W)
    def e32 : UInt = "b00000100".U(width.W)
    def e64 : UInt = "b00001000".U(width.W)
    def e4  : UInt = "b10000000".U(width.W)

    def convertFromMSew(msew: UInt): UInt = {
      require(msew.getWidth >= 2 && msew.getWidth <= 3)
      ZeroExt(UIntToOH(msew), this.width)
    }
  }

  object MtypeMSew extends NamedUInt(3)

  object Mtilex {
    def apply(): UInt = UInt(width.W)

    // TODO: use a correct width
    // The mlwidth is just a placeholder
    def width = 9
  }

  class AmuMmaIO extends Bundle {
    // rounding mode (xmfrm/xmxrm)
    val rm       = UInt(3.W) // 52 : 50
    // dest matrix register index
    val md       = UInt(4.W) // 49 : 46
    // whether saturate (xmsaten)
    val sat      = Bool()    // 45
    // src matrix register indices
    val ms1      = UInt(4.W) // 44 : 41
    val ms2      = UInt(4.W) // 40 : 37

    // the scale of mma operations, m/n/k
    val mtilem   = Mtilex()  // 36 : 28
    val mtilen   = Mtilex()  // 27 : 19
    val mtilek   = Mtilex()  // 18 : 10
    
    // the type of source matrices
    // - lower 2 bits stands for the element width:
    //   - 0: e8, 1: e16, 2: e32, 3: e4
    // - the highest bit determines the specific type:
    //   - 0 for unsigned and 1 for signed when isfp is false
    //   - 0 for e5m2 and 1 for e4m3 when the type is 8-bit fp
    //   - 0 for fp16 and 1 for bf16 when the type is 16-bit fp
    //   - 0 for fp32 and 1 for tf32 when the type is 32-bit fp
    val types2   = UInt(3.W) // 9 : 7
    val types1   = UInt(3.W) // 6 : 4
    // the same as types1/2, but for destination matrix
    val typed    = UInt(3.W) // 3 : 1
    // whether floating point mma
    val isfp     = Bool()    // 0
  }

  object AmuMmaIO {
    def apply(): AmuMmaIO = {
      new AmuMmaIO()
    }
  }

  class AmuLsuIO extends Bundle {
    // src/dest matrix register
    val ms        = UInt(4.W)         // 125 : 122
    // load(0)/store(1)
    val ls        = Bool()            // 121
    // whether transposed
    val transpose = Bool()            // 120
    // whether accumulation register
    val isacc     = Bool()            // 119
    // whether matrix A
    val isA       = Bool()            // 118
    // whether matrix B
    val isB       = Bool()            // 117

    // the address of the first element of the matrix
    val baseAddr  = UInt(48.W)        // 116 : 69
    // the stride of the matrix
    val stride    = UInt(48.W)        // 68 : 21
    
    // the number of rows of the matrix
    val row       = Mtilex()          // 20 : 12
    // the number of columns of the matrix
    val column    = Mtilex()          // 11 : 3
    // the width of elements in the matrix, see also MSew
    // 0: e8, 1: e16, 2: e32, 3: e64, 7: e4
    // other values are reserved
    val widths    = MtypeMSew()       // 2 : 0
  }

  object AmuLsuIO {
    def apply(): AmuLsuIO = {
      new AmuLsuIO()
    }
  }

  class AmuReleaseIO extends Bundle {
    val tokenRd = UInt(5.W)
  }

  object AmuReleaseIO {
    def apply(): AmuReleaseIO = {
      new AmuReleaseIO()
    }
  }

  class AmuArithIO extends Bundle {
    // Only support mzero1r currently
    
    // dest matrix register index
    val md     = UInt(4.W) // 12 : 9
    // operation type
    // see also package.scala
    val opType = UInt(9.W) // 8 : 0
  }

  object AmuArithIO {
    def apply(): AmuArithIO = {
      new AmuArithIO()
    }
  }

  class AmuCtrlIO(implicit p: Parameters) extends CuteBundle {
    // op: Determine the operation
    // 0: MMA
    // 1: Load/Store
    // 2: Release
    val op = UInt(2.W)
    
    def isMma()     : Bool = op === AmuCtrlIO.mmaOp()
    def isMls()     : Bool = op === AmuCtrlIO.mlsOp()
    def isRelease() : Bool = op === AmuCtrlIO.releaseOp()
    def isArith()   : Bool = op === AmuCtrlIO.arithOp()
    // data: The ctrl signal for op
    val data = UInt(150.W)

    val pc = Option.when(EnableDifftest) (UInt(64.W))
    val coreid = Option.when(EnableDifftest) (UInt(8.W))
  }

  object AmuCtrlIO {
    def apply()(implicit p: Parameters): AmuCtrlIO = {
      new AmuCtrlIO()
    }

    def mmaOp()     : UInt = "b00".U
    def mlsOp()     : UInt = "b01".U
    def releaseOp() : UInt = "b10".U
    def arithOp()   : UInt = "b11".U
  }
}
