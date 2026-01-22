import mill._
import scalalib._
import scalafmt._
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common
import $file.`coupledL2`.HuanCun.common
import $file.`coupledL2`.common

val projectRoot = os.pwd
val depsRoot = projectRoot / "coupledL2"
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
}

object huancun extends SbtModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath / "coupledL2" / "HuanCun"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object CoupledL2 extends SbtModule with HasChisel 
  with coupledL2.common.CoupledL2Module {

  override def millSourcePath = millOuterCtx.millSourcePath / "coupledL2"

  def rocketModule = rocketchip

  def utilityModule = utility

  def huancunModule = huancun

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }
}

object difftest extends SbtModule with HasChisel {
  override def millSourcePath = millOuterCtx.millSourcePath / "difftest"
}

object CUTE extends SbtModule with HasChisel with $file.common.CUTEModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def coupledL2Module: ScalaModule = CoupledL2

  def difftestModule: ScalaModule = difftest

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
}

