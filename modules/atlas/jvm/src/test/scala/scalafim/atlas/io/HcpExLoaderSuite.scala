package scalafim.atlas.io

class HcpExLoaderSuite extends munit.FunSuite:
  test("HCPex parser rejects incomplete metadata"):
    assert(HcpExLoader.parse("1 L V1 1", "1 V1_L 1 2 3 0").isLeft)
