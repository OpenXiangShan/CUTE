package cute

import chisel3._

/** Request traversal order; the selected row always retains its canonical identity. */
object ABBatchSchedule {
  val Natural: String = "Natural"
  val Rotate: String = "Rotate"
  val Permute: String = "Permute"
  val modes: Set[String] = Set(Natural, Rotate, Permute)

  def coprime(left: Int, right: Int): Boolean = BigInt(left).gcd(BigInt(right)) == 1

  def batch(ordinal: UInt, count: UInt, mode: String, offset: Int, multiplier: Int): UInt = {
    require(modes.contains(mode))
    require(offset >= 0 && multiplier > 0)
    val safeCount = Mux(count === 0.U, 1.U, count)
    mode match {
      case Natural => ordinal
      case Rotate => (ordinal +& offset.U) % safeCount
      case Permute => (ordinal * multiplier.U +& offset.U) % safeCount
    }
  }
}
