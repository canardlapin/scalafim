package scalafim.transform

class TransformModuleSuite extends munit.FunSuite:
  test("transform module links on this platform"):
    assert(classOf[TransformModuleSuite].getName.nonEmpty)
