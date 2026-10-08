package cute

/** Recorded zames C-load stimulus and the software-side exact route oracle. */
object CMLPerfectBatchTestUtils {
  val baseAddress: BigInt = BigInt("80049e00", 16)
  val rowStride: BigInt = BigInt("440", 16)
  val matrixM: Int = 128
  val matrixN: Int = 128
  val nBeatCount: Int = 8
  val expectedRequestCount: Int = matrixM * nBeatCount

  def l2Bank(address: BigInt, bankCount: Int): Int =
    ((address >> 6) & (bankCount - 1)).toInt

  def exactSurvivors(
    validByBank: Seq[Boolean],
    destinationByBank: Seq[Int],
    responseChannels: Int = 8,
    bankCount: Int = 8
  ): BigInt = LocalMMUSchedulerTestUtils.survivorMask(
    validByBank,
    destinationByBank,
    responseChannels,
    bankCount)

  def expectedRequests: Set[(Int, BigInt, Int)] =
    (0 until matrixM).flatMap { row =>
      (0 until nBeatCount).map { beat =>
        val channel = row % 8
        val regAddr = (row / 8) * 16 + beat * 2
        val address = baseAddress + row * rowStride + beat * 64
        (channel, address, regAddr)
      }
    }.toSet
}
