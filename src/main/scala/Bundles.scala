package cute

import chisel3._
import chisel3.util._

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
    def width = 16
  }

  class AmuMmaIO extends Bundle {
    val md       = UInt(4.W) // 3 : 0
    val sat      = Bool()    // 4
    val ms1      = UInt(4.W) // 8 : 5
    val ms2      = UInt(4.W) // 12 : 9
    val mtilem   = Mtilex()  // 21 : 13
    val mtilen   = Mtilex()  // 30 : 22
    val mtilek   = Mtilex()  // 39 : 31
    val types    = UInt(3.W) // 42 : 40
    val typed    = UInt(3.W) // 45 : 43
    val isfp     = Bool()    // 46
    val issigned = Bool()  // 47
  }

  object AmuMmaIO {
    def apply(): AmuMmaIO = {
      new AmuMmaIO()
    }
  }

  class AmuLsuIO extends Bundle {
    // src/dest matrix register
    val ms        = UInt(4.W)         // 3 : 0
    // load(0)/store(1)
    val ls        = Bool()            // 4
    // whether transposed
    val transpose = Bool()            // 5
    // whether accumulation register
    val isacc     = Bool()            // 6
    val isA       = Bool()            // 7
    val isB       = Bool()            // 8

    val baseAddr  = UInt(48.W)        // 56 : 9
    val stride    = UInt(48.W)        // 104 : 57
    
    val row       = Mtilex()          // 113 : 105
    val column    = Mtilex()          // 122 : 114
    val widths    = MtypeMSew()       // 125 : 123
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
    val md     = UInt(4.W) // 3 : 0
    val opType = UInt(9.W) // 12 : 4
  }

  object AmuArithIO {
    def apply(): AmuArithIO = {
      new AmuArithIO()
    }
  }

  class AmuCtrlIO extends Bundle {
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
  }

  object AmuCtrlIO {
    def apply(): AmuCtrlIO = {
      new AmuCtrlIO()
    }

    def mmaOp()     : UInt = "b00".U
    def mlsOp()     : UInt = "b01".U
    def releaseOp() : UInt = "b10".U
    def arithOp()   : UInt = "b11".U
  }
}