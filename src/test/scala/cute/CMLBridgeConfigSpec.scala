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
    assert(view.CMLUseMultiChannelLoader)
  }

  it should "parse fully shared multi-channel C load/store modes" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCL4CS4"
    ))
    val view = new ParamView

    assert(view.CLoadBridgeResponseChannelCount == 4)
    assert(view.CStoreBridgeResponseChannelCount == 4)
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
    assert(!view.CMLUseMultiChannelLoader)
  }

  it should "reject unsupported loader config strings" in {
    assertThrows[IllegalArgumentException] {
      CuteParams.baseParams.copy(LoaderBridgeChannelConfig = "A3BLCL1CS2")
    }
  }
}
