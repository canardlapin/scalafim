package scalafim.atlas

class FslAtlasRequestSuite extends munit.FunSuite:
  test("named requests expose only pinned executable hard summaries"):
    FslAtlasFamily.values.foreach: family =>
      val request = FslAtlasRequest(family)
      assertEquals(request.ref.representation, AtlasRepresentation.Volume)
      assertEquals(request.ref.templateSpace, SpaceId.MNI152NLin6Asym)
      assertEquals(request.ref.resolution, Some("2mm"))
      Vector(request.xml, request.volume).foreach: asset =>
        assert(asset.url.contains(FslAtlasRequest.revision))
        assert(asset.sha256.matches("[0-9a-f]{64}"))
    assertEquals(FslAtlasRequest(FslAtlasFamily.Julich).volume.sha256,
      "d2209b9f70c6e273d44ef36a10b5ef4f96b1b6f81fed696dbe70320d252e700d")

  test("pinned asset rejects missing integrity, mutable branch and unsafe cache names"):
    val asset = FslAtlasRequest(FslAtlasFamily.Julich).xml
    intercept[IllegalArgumentException](asset.copy(sha256 = ""))
    intercept[IllegalArgumentException](asset.copy(url = "https://example.test/master/a"))
    intercept[IllegalArgumentException](asset.copy(fileName = "../a"))
