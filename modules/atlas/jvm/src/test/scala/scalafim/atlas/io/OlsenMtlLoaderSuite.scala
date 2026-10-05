package scalafim.atlas.io

import scalafim.atlas.*

class OlsenMtlLoaderSuite extends munit.FunSuite:
  test("Olsen request retains exact bilateral hippocampal selection"):
    assertEquals(OlsenMtlRequest(OlsenMtlMode.Hippocampus).selectedIds.map(_.value), OlsenMtlRequest.hippocampalIds)
