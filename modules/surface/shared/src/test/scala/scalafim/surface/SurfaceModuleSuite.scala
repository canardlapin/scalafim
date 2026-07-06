package scalafim.surface

class SurfaceModuleSuite extends munit.FunSuite:

  test("module metadata is available"):
    assertEquals(SurfaceModule.name, "scalafim-surface")
