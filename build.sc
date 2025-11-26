import mill._
import scalalib._
import $file.^.`rocket-chip`.common
import $file.^.`rocket-chip`.cde.common
import $file.^.`rocket-chip`.hardfloat.common
import $file.^.huancun.common
import $file.^.coupledL2.common

val repoRoot = os.pwd / os.up
val defaultScalaVersion = "2.13.15"
def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:6.6.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.6.0"
)

trait HasChisel extends SbtModule {
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

object rocketchip
  extends $file.^.`rocket-chip`.common.RocketChipModule
    with HasChisel {

  override def millSourcePath = repoRoot / "rocket-chip"

  def chiselModule = None

  def chiselPluginJar = T(None)

  def chiselIvy = Some(defaultVersions("chisel"))

  def chiselPluginIvy = Some(defaultVersions("chisel-plugin"))

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  object macros extends Macros

  trait Macros
    extends $file.^.`rocket-chip`.common.MacrosModule
      with SbtModule {

    override def scalaVersion = defaultScalaVersion

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat
    extends $file.^.`rocket-chip`.hardfloat.common.HardfloatModule
      with HasChisel {

    override def millSourcePath = repoRoot / "rocket-chip" / "hardfloat" / "hardfloat"

    override def chiselModule = None

    override def chiselPluginJar = T(None)
  }

  object cde
    extends $file.^.`rocket-chip`.cde.common.CDEModule
      with ScalaModule {

    override def scalaVersion = defaultScalaVersion

    override def millSourcePath = repoRoot / "rocket-chip" / "cde" / "cde"
  }
}

object utility extends HasChisel {

  override def millSourcePath = repoRoot / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )
}

object huancun
  extends $file.^.huancun.common.HuanCunModule
    with HasChisel {

  override def millSourcePath = repoRoot / "huancun"

  def rocketModule = rocketchip

  def utilityModule = utility
}

object coupledL2
  extends $file.^.coupledL2.common.CoupledL2Module
    with HasChisel {

  override def millSourcePath = repoRoot / "coupledL2"

  def rocketModule = rocketchip

  def utilityModule = utility

  def huancunModule = huancun
}

object cute extends HasChisel {

  override def millSourcePath = repoRoot / "CUTE"

  def rocketModule = rocketchip

  def utilityModule = utility

  def coupledL2Module = coupledL2

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    utilityModule,
    coupledL2Module
  )
}

