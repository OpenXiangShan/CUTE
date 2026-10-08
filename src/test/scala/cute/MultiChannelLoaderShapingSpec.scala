package cute

import java.io.File
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Using

/** Regression guard for the rejected loader-side shaper admission layer. */
class MultiChannelLoaderShapingSpec extends AnyFlatSpec {
  behavior of "multi-channel loader source tree"

  private def scalaFiles(root: File): Seq[File] = {
    if (!root.exists()) Seq.empty
    else if (root.isFile && root.getName.endsWith(".scala")) Seq(root)
    else Option(root.listFiles()).toSeq.flatMap(_.toSeq.flatMap(scalaFiles))
  }

  it should "contain no shaper symbols or valid-gating hooks" in {
    def ancestors(file: File): Seq[File] = {
      if (file == null) Seq.empty else file +: ancestors(file.getParentFile)
    }
    val classLocation = new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    val roots = Seq(new File(".").getCanonicalFile, classLocation)
      .flatMap(ancestors)
      .flatMap(dir => Seq(new File(dir, "CUTE/src/main/scala"), new File(dir, "src/main/scala")))
      .filter(_.exists())
      .distinct
    assert(roots.nonEmpty, "could not locate the CUTE Scala source tree")
    val source = roots.flatMap(scalaFiles).map { file =>
      Using.resource(scala.io.Source.fromFile(file))(_.mkString)
    }.mkString("\n")
    Seq("OmegaRoundShaper", "OmegaRoundSkewBound", "shapeValid", "shapeAddress", "shapeDestination", "shapeMask")
      .foreach(symbol => assert(!source.contains(symbol), s"obsolete shaper symbol remains: $symbol"))
  }
}
