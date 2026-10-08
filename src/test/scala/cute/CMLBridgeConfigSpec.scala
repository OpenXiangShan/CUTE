package cute

import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

class CMLBridgeConfigSpec extends AnyFlatSpec {
  private class ParamView(implicit val p: Parameters) extends CUTEImplParameters

  private def paramsWith(cuteParams: CuteParams): Parameters = {
    new Config((_, _, _) => {
      case CuteParamsKey => cuteParams
    })
  }

  behavior of "CML bridge parameter derivation"

  it should "parse parameterized one-channel and legacy modes independently" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "A1BLCL1CS2"
    ))
    val view = new ParamView

    assert(!view.AMLUseLegacyLoader)
    assert(view.AMLResponseChannelCount == 1)
    assert(view.BMLUseLegacyLoader)
    assert(view.CLoadBridgeResponseChannelCount == 1)
    assert(view.CStoreBridgeResponseChannelCount == 2)
    assert(view.CStoreDiffTestLaneCount == 2)
    assert(view.CMLUseMultiChannelLoader)
  }

  it should "parse fully shared multi-channel C load/store modes" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCL4CS4"
    ))
    val view = new ParamView

    assert(view.CLoadBridgeResponseChannelCount == 4)
    assert(view.CStoreBridgeResponseChannelCount == 4)
    assert(view.CStoreDiffTestLaneCount == 4)
    assert(view.CMLUseMultiChannelLoader)
  }

  it should "preserve fully legacy operation when all modes are legacy" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCLLCSL"
    ))
    val view = new ParamView

    assert(view.AMLUseLegacyLoader)
    assert(view.BMLUseLegacyLoader)
    assert(view.CLoadUseLegacyLoader)
    assert(view.CStoreUseLegacyLoader)
    assert(view.CLoadBridgeResponseChannelCount == 1)
    assert(view.CStoreBridgeResponseChannelCount == 1)
    assert(view.CStoreDiffTestLaneCount == 1)
    assert(!view.CMLUseMultiChannelLoader)
    assert(view.EnableOmegaInputQueue)
  }

  it should "allow the Omega input queue to be disabled globally" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      EnableOmegaInputQueue = false
    ))
    assert(!(new ParamView).EnableOmegaInputQueue)
  }

  it should "derive route descriptors for every supported parameterized width" in {
    Seq(1, 2, 4, 8).foreach { width =>
      implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
        LoaderBridgeChannelConfig = s"A${width}B${width}CL${width}CS${width}"
      ))
      val view = new ParamView
      assert(view.AMLRouteDescriptor.responseChannelCount == width)
      assert(view.BMLRouteDescriptor.responseChannelCount == width)
      assert(view.CLoadRouteDescriptor.responseChannelCount == width)
      assert(view.CStoreRouteDescriptor.responseChannelCount == width)
      assert(view.AMLRouteDescriptor.collisionProne)
    }
  }

  it should "keep legacy descriptors outside collision prediction" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCLLCSL"
    ))
    val view = new ParamView
    assert(!view.AMLRouteDescriptor.collisionProne)
    assert(!view.BMLRouteDescriptor.collisionProne)
    assert(!view.CLoadRouteDescriptor.collisionProne)
    assert(!view.CStoreRouteDescriptor.collisionProne)
  }

  it should "derive every loader independently in representative mixed configurations" in {
    val expected = Seq(
      "ALBLCLLCSL" -> Seq(1, 1, 1, 1),
      "A8B8CL8CS8" -> Seq(8, 8, 8, 8),
      "A1BLCL2CS4" -> Seq(1, 1, 2, 4),
      "A1B2CL4CS8" -> Seq(1, 2, 4, 8)
    )
    expected.foreach { case (config, widths) =>
      implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
        LoaderBridgeChannelConfig = config
      ))
      val view = new ParamView
      assert(Seq(
        view.AMLRouteDescriptor.responseChannelCount,
        view.BMLRouteDescriptor.responseChannelCount,
        view.CLoadRouteDescriptor.responseChannelCount,
        view.CStoreRouteDescriptor.responseChannelCount
      ) == widths, config)
    }
  }

  it should "accept legal response widths 1/2/4/8 when the MatrixReg has eight banks" in {
    Seq(1, 2, 4, 8).foreach { width =>
      val params = CuteParams.baseParams.copy(
        Matrix_MN = 8,
        L2NBanks = 8,
        LoaderBridgeChannelConfig = s"A${width}B${width}CL${width}CS${width}"
      )
      assert(params.parsedLoaderBridgeChannelConfig.a.responseChannelCount == width)
    }
  }

  it should "reject invalid L2 bank counts and eight-bank lane/destination mismatches" in {
    assertThrows[IllegalArgumentException] {
      CuteParams.baseParams.copy(L2NBanks = 6)
    }
    assertThrows[IllegalArgumentException] {
      CuteParams.baseParams.copy(
        Matrix_MN = 4,
        L2NBanks = 8,
        LoaderBridgeChannelConfig = "A8B8CL8CS8"
      )
    }
  }

  it should "derive difftest lane count from the C-store channel mode" in {
    Seq(
      "ALBLCL1CS1" -> 1,
      "ALBLCL2CS2" -> 2,
      "ALBLCL4CS4" -> 4,
      "ALBLCL8CS8" -> 8
    ).foreach { case (loaderConfig, expectedLaneCount) =>
      implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
        LoaderBridgeChannelConfig = loaderConfig
      ))
      val view = new ParamView

      assert(view.CStoreDiffTestLaneCount == expectedLaneCount)
      assert(view.CStoreDiffTestLaneCount == view.CStoreBridgeResponseChannelCount)
    }
  }

  it should "reject unsupported loader config strings" in {
    assertThrows[IllegalArgumentException] {
      CuteParams.baseParams.copy(LoaderBridgeChannelConfig = "A3BLCL1CS2")
    }
  }
}
