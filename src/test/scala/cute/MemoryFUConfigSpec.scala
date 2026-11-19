package cute

import org.chipsalliance.cde.config.{Config, Parameters}
import org.scalatest.flatspec.AnyFlatSpec

class MemoryFUConfigSpec extends AnyFlatSpec {
  private class ParamView(implicit val p: Parameters) extends CUTEImplParameters

  private def paramsWith(cuteParams: CuteParams): Parameters = {
    new Config((_, _, _) => {
      case CuteParamsKey => cuteParams
    })
  }

  behavior of "accMemory bridge parameter derivation"

  it should "parse parameterized one-channel and legacy modes independently" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "A1BLCL1CS2"
    ))
    val view = new ParamView

    assert(!view.TrLoader0UseLegacyLoader)
    assert(view.TrLoader0ResponseChannelCount == 1)
    assert(view.TrLoader1UseLegacyLoader)
    assert(view.AccLoader0BridgeResponseChannelCount == 1)
    assert(view.AccStorer0BridgeResponseChannelCount == 2)
  }

  it should "parse fully shared multi-channel C load/store modes" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCL4CS4"
    ))
    val view = new ParamView

    assert(view.AccLoader0BridgeResponseChannelCount == 4)
    assert(view.AccStorer0BridgeResponseChannelCount == 4)
  }

  it should "preserve fully legacy operation when all modes are legacy" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCLLCSL"
    ))
    val view = new ParamView

    assert(view.TrLoader0UseLegacyLoader)
    assert(view.TrLoader1UseLegacyLoader)
    assert(view.AccLoader0UseLegacyLoader)
    assert(view.AccStorer0UseLegacyLoader)
    assert(view.AccLoader0BridgeResponseChannelCount == 1)
    assert(view.AccStorer0BridgeResponseChannelCount == 1)
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
      assert(view.TrLoader0RouteDescriptor.responseChannelCount == width)
      assert(view.TrLoader1RouteDescriptor.responseChannelCount == width)
      assert(view.AccLoader0RouteDescriptor.responseChannelCount == width)
      assert(view.AccStorer0RouteDescriptor.responseChannelCount == width)
      assert(view.TrLoader0RouteDescriptor.collisionProne)
    }
  }

  it should "keep legacy descriptors outside collision prediction" in {
    implicit val p: Parameters = paramsWith(CuteParams.baseParams.copy(
      LoaderBridgeChannelConfig = "ALBLCLLCSL"
    ))
    val view = new ParamView
    assert(!view.TrLoader0RouteDescriptor.collisionProne)
    assert(!view.TrLoader1RouteDescriptor.collisionProne)
    assert(!view.AccLoader0RouteDescriptor.collisionProne)
    assert(!view.AccStorer0RouteDescriptor.collisionProne)
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
        view.TrLoader0RouteDescriptor.responseChannelCount,
        view.TrLoader1RouteDescriptor.responseChannelCount,
        view.AccLoader0RouteDescriptor.responseChannelCount,
        view.AccStorer0RouteDescriptor.responseChannelCount
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
      assert(params.parsedLoaderBridgeChannelConfig.trLoader0.responseChannelCount == width)
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

  it should "reject unsupported loader config strings" in {
    assertThrows[IllegalArgumentException] {
      CuteParams.baseParams.copy(LoaderBridgeChannelConfig = "A3BLCL1CS2")
    }
  }
  behavior of "independent trStorer0 request configuration"

  it should "preserve old defaults and round-trip explicit trStorer0 widths independently of A" in {
    for (a <- Seq("L", "1", "2", "4", "8")) {
      val old = s"A${a}B4CL1CS2"
      val parsed = ResponseChannelHelper.parseLoaderBridgeChannelConfig(old)
      assert(ResponseChannelHelper.parseLoaderBridgeChannelConfig(parsed.render) == parsed)
      assert(parsed.trStoreRequestChannelCount == (if (a == "L") 1 else a.toInt))
      for (width <- Seq(1, 2, 4, 8)) {
        val config = s"${old}AS$width"
        val params = CuteParams.CUTE_8Tops_128SCP.copy(LoaderBridgeChannelConfig = config)
        assert(params.TrStorer0RequestChannelCount == width)
        assert(params.TrLoader0ChannelMode == parsed.trLoader0)
        assert(params.AccStorer0ChannelMode == parsed.accStorer0)
        assert(ResponseChannelHelper.parseLoaderBridgeChannelConfig(params.parsedLoaderBridgeChannelConfig.render) == params.parsedLoaderBridgeChannelConfig)
      }
    }
  }

  it should "reject unsupported trStorer0 widths and widths exceeding the register banks" in {
    for (suffix <- Seq("AS0", "AS3", "AS16", "ASL", "AS2AS4")) {
      assertThrows[IllegalArgumentException] {
        ResponseChannelHelper.parseLoaderBridgeChannelConfig(s"A1B1CL1CS1$suffix")
      }
    }
    assertThrows[IllegalArgumentException] {
      CuteParams.CUTE_2Tops.copy(LoaderBridgeChannelConfig = "A1B1CL1CS1AS8")
    }
  }

  it should "parse independent TS0 legacy and numeric modes and preserve old omitted AS" in {
    import ResponseChannelHelper._
    val presets = Seq("TL0LTL1LAL0LTS0LAS0L", "TL08TL18AL08TS08AS08", "TL0LTL1LAL02TS0LAS08")
    for (preset <- presets) {
      val params = CuteParams.CUTE_8Tops_128SCP.copy(LoaderBridgeChannelConfig = preset)
      assert(params.parsedLoaderBridgeChannelConfig.render == preset)
    }
    for (mode <- Seq("L", "1", "2", "4", "8")) {
      val text = s"TL04TL12AL01TS0${mode}AS08"
      val config = parseLoaderBridgeChannelConfig(text)
      assert(config.render == text)
      assert(config.trStorer0.isLegacy == (mode == "L"))
      assert(config.trStoreRequestChannelCount == (if (mode == "L") 1 else mode.toInt))
      assert(config.trLoader0 == ParameterizedLoaderMode(4))
    }
    assert(parseLoaderBridgeChannelConfig("ALBLCLLCSL").render == "TL0LTL1LAL0LTS01AS0L")
    assert(parseLoaderBridgeChannelConfig("ALBLCL2CS8").render == "TL0LTL1LAL02TS01AS08")
    assert(parseLoaderBridgeChannelConfig("A8B8CL8CS8").render == "TL08TL18AL08TS08AS08")
    for (bad <- Seq("TL01TL11AL01AS01", "TL01TL11AL01TS03AS01", "TL01TL11AL01TS01AS0L")) {
      assertThrows[IllegalArgumentException] {
        CuteParams.CUTE_8Tops_128SCP.copy(LoaderBridgeChannelConfig = bad)
      }
    }
  }

}
