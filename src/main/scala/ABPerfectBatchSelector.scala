package cute

import chisel3._
import chisel3.util._

/**
  * Source-neutral entry point for AML/BML normal-load candidate selection.
  *
  * CMLPerfectBatchSelector contains the reviewed bounded greedy algorithm and
  * exact Omega survivor predicate. Keeping this forwarding API separate lets
  * AB loaders depend on the contract without duplicating the route model.
  */
object ABPerfectBatchSelector {
  def select(
    valid: Seq[Bool],
    address: Seq[UInt],
    destination: Seq[UInt],
    retained: Seq[Bool],
    seedPointer: UInt,
    l2BankCount: Int
  ): UInt = {
    require(valid.nonEmpty && isPow2(valid.size))
    require(address.size == valid.size)
    require(destination.size == valid.size)
    require(retained.size == valid.size)
    require(isPow2(l2BankCount))
    CMLPerfectBatchSelector.select(valid, address, destination, retained, seedPointer, l2BankCount)
  }
}
