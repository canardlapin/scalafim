package scalafim.atlas

class OlsenMtlRequestSuite extends munit.FunSuite:
  test("Olsen mode selects only the audited hippocampal anatomical labels"):
    val request = OlsenMtlRequest(OlsenMtlMode.Hippocampus)
    assertEquals(request.ref.templateSpace, SpaceId.unknown("MNI152_custom"))
    assertEquals(request.selectedIds.map(_.value), Vector(1, 2, 3, 6, 8, 9, 10, 11, 14, 16))
    assertEquals(OlsenMtlRequest.regions.length, 16)
