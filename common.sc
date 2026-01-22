import mill._
import scalalib._

trait CUTEModule extends ScalaModule {

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  def coupledL2Module: ScalaModule
  
  def difftestModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    utilityModule,
    coupledL2Module,
    difftestModule
  )
}

