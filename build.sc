import mill._
import scalalib._
import scalafmt._
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common
import $file.XSAICache.common

val projectRoot = os.Path(sys.env("MILL_WORKSPACE_ROOT"))
val xsaiRoot = sys.env.get("NOOP_HOME").map(root => os.Path(root))
val localDifftestRoot = projectRoot / "difftest"
val siblingDifftestRoot = projectRoot / os.up / "difftest"
val difftestRoot = xsaiRoot match {
  case Some(root) =>
    val path = root / "difftest"
    require(os.isDir(path / "src" / "main" / "scala"),
      s"NOOP_HOME points to $root; initialize its Difftest submodule before compiling CUTE")
    path
  case None =>
    Seq(localDifftestRoot, siblingDifftestRoot)
      .find(path => os.exists(path / "src" / "main" / "scala"))
      .getOrElse(localDifftestRoot)
}
val difftestSourceFiles =
  if (os.exists(difftestRoot / "src" / "main" / "scala")) {
    os.walk(difftestRoot / "src" / "main" / "scala")
      .filter(os.isFile)
      .filter(_.ext == "scala")
  } else Seq.empty
// Match the parent XSAI build's existing fudian dependency. NOOP_HOME is
// the canonical XSAI source-root override; otherwise use the sibling checkout.
val fudianRoot = xsaiRoot
  .map(root => root / "fudian")
  .getOrElse(projectRoot / os.up / "fudian")
val defaultScalaVersion = "2.13.15"
def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:6.7.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.7.0",
  "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:6.0.0"
)

trait HasChisel extends ScalaModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends `rocket-chip`.common.RocketChipModule with HasChisel {

  val rcPath = millOuterCtx.millSourcePath / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends `rocket-chip`.common.MacrosModule with HasChisel {
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  object cde extends `rocket-chip`.cde.common.CDEModule with HasChisel {
    override def millSourcePath = rcPath / "cde" / "cde"
  }

  object hardfloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

}

object utility extends SbtModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)

  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::sourcecode:0.4.2")
}

object openNCB extends SbtModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath / "XSAICache" / "OpenNCB"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
}

object zhujiangCompat extends SbtModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath / "XSAICache" / "ZhuJiang"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, utility)

  override def sources = T.sources {
    val sourceRoots = Seq(
      millSourcePath / "src" / "main" / "scala",
      millSourcePath / "xs-utils" / "src" / "main" / "scala"
    )
    sourceRoots.filter(os.isDir).flatMap(os.walk(_))
      .filter(path => os.isFile(path) && path.ext == "scala")
      .filterNot(_.last == "XsStage.scala")
      .map(PathRef(_))
  }
}

object XSAICache extends SbtModule with HasChisel with $file.XSAICache.common.XSCacheModule {
  override def millSourcePath = millOuterCtx.millSourcePath / "XSAICache"

  override def moduleDeps = super.moduleDeps ++ Seq(zhujiangCompat)

  def openNCBModule: ScalaModule = openNCB

  def rocketModule = rocketchip

  def utilityModule = utility

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }
}

object difftest extends SbtModule with HasChisel {
  override def millSourcePath = difftestRoot

  override def sources = T.sources {
    difftestSourceFiles.map(PathRef(_))
  }
}

object cuteFpe extends SbtModule with HasChisel {

  override def millSourcePath = millOuterCtx.millSourcePath / "cute-fpe" / "fpe"

  override def sources = T.sources {
    val sourceRoot = millSourcePath / "src" / "main" / "scala" / "top"
    Seq(
      "CuteFpeConfig.scala",
      "CuteFpeParameters.scala",
      "CmpTree.scala",
      "FReducePE.scala",
      "FReduceTypes.scala",
      "FVecDecoder.scala",
      "MACPipes.scala",
      "PipeBundles.scala"
    ).map(name => PathRef(sourceRoot / name))
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip.cde,
    rocketchip.hardfloat
  )
}

object fudian extends SbtModule with HasChisel {
  override def millSourcePath = fudianRoot
}

val cuteBaseSourceNames = Seq("CUTEParameters.scala", "ABBatchSchedule.scala")

object CUTEBase extends SbtModule with HasChisel {

  override def millSourcePath = millOuterCtx.millSourcePath

  override def sources = T.sources {
    val sourceRoot = millSourcePath / "src" / "main" / "scala"
    cuteBaseSourceNames.map(name => PathRef(sourceRoot / name))
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    cuteFpe
  )
}

object CUTE extends SbtModule with HasChisel with $file.common.CUTEModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  override def sources = T.sources {
    val sourceRoot = millSourcePath / "src" / "main" / "scala"
    os.walk(sourceRoot)
      .filter(p => os.isFile(p) && p.ext == "scala")
      .filterNot(p => cuteBaseSourceNames.contains(p.last))
      .map(PathRef(_))
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    CUTEBase,
    cuteFpe,
    fudian
  )

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def coupledL2Module: ScalaModule = XSAICache

  def difftestModule: ScalaModule = difftest

  object test extends SbtTests with TestModule.ScalaTest {
    def panelDiagnosticSources = T.input {
      val value = sys.env.getOrElse("CUTE_PANEL_DIAGNOSTIC", "0")
      require(value == "0" || value == "1", "CUTE_PANEL_DIAGNOSTIC must be 0 or 1")
      value == "1"
    }

    // A bounded observation-only run of the existing standalone probe is not
    // the full Scala regression. Keep the default complete source set intact.
    override def sources = T.sources {
      val all = super.sources()
      if (panelDiagnosticSources()) {
        val selected = all.map(ref => ref.path / "TestTop.scala")
          .filter(path => os.isFile(path)).map(PathRef(_))
        require(selected.size == 1, "diagnostic requires exactly the canonical TestTop.scala")
        selected
      } else all
    }

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
}
