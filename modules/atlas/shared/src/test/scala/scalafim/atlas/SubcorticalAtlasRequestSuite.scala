package scalafim.atlas

class SubcorticalAtlasRequestSuite extends munit.FunSuite:
  test("native subcortical requests retain immutable sources and do not advertise derived CIT168 laterality"):
    val requests = SubcorticalAtlasFamily.values.map(SubcorticalAtlasRequest.apply)
    assertEquals(requests.map(_.id).distinct.size, SubcorticalAtlasFamily.values.size)
    requests.foreach: request =>
      assert(request.volume.url.startsWith("https://"))
      assert(request.labels.url.startsWith("https://"))
      assert(request.volume.sha256.matches("[0-9a-f]{64}"))
      assert(request.labels.sha256.matches("[0-9a-f]{64}"))
      assertEquals(request.ref.details.confidence, Confidence.Uncertain)
    val cit = SubcorticalAtlasRequest(SubcorticalAtlasFamily.Cit168)
    assert(cit.ref.notes.exists(_.contains("unsplit")))
    assert(!cit.ref.notes.exists(_.contains("LRSplit")))
