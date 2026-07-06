package scalafim.fmri.design

class NamesSuite extends munit.FunSuite:

  test("zeroPad matches R tests") {
    assertEquals(Names.zeroPad(3, 9), "03")
    assertEquals(Names.zeroPad(9, 9), "09")
    assertEquals(Names.zeroPad(3, 10), "03")
    assertEquals(Names.zeroPad(12, 99), "12")
    assertEquals(Names.zeroPad(12, 100), "012")
    assertEquals(Names.zeroPad(99, 100), "099")
    assertEquals(Names.zeroPad(Seq(1, 2, 3), 10), Vector("01", "02", "03"))
    assertEquals(Names.zeroPad(5, 0), "5")
  }

  test("sanitize matches R tests") {
    assertEquals(Names.sanitize("a.b c"), "a.b.c")
    assertEquals(Names.sanitize("a b.c", allowDot = false), "a_b_c")
    assertEquals(Names.sanitize("1var"), "X1var")
    assertEquals(Names.sanitizeAll(Seq("a", "a")), Vector("a", "a"))
    assertEquals(Names.sanitize("_start"), "X_start")
  }

  test("basisSuffix matches R tests") {
    assertEquals(Names.basisSuffix(1, 1), "_b1")
    assertEquals((1 to 3).map(j => Names.basisSuffix(j, 3)).toVector, Vector("_b01", "_b02", "_b03"))
    assertEquals(Names.basisSuffix(1, 5), "_b01")
  }

  test("featureSuffix basic") {
    assertEquals((1 to 3).map(j => Names.featureSuffix(j, 5)).toVector, Vector("f01", "f02", "f03"))
  }

  test("makeUniqueTags matches R tests") {
    assertEquals(Names.makeUniqueTags(Vector("a", "b")), Vector("a", "b"))
    assertEquals(Names.makeUniqueTags(Vector("a", "a")), Vector("a", "a#1"))
    assertEquals(Names.makeUniqueTags(Vector("a", "a", "a#1")), Vector("a", "a#2", "a#1"))
  }

  test("levelToken matches R tests") {
    assertEquals(Names.levelToken("cond", "A"), "cond.A")
    assertEquals(Names.levelToken("cond name", "Level 1"), "cond.name.Level.1")
    assertEquals(Names.levelToken("Input", "20"), "Input.20")
    assertEquals(Names.levelToken("Input", "1"), "Input.1")
  }

  test("addBasis matches R tests ordering") {
    assertEquals(
      Names.addBasis(Vector("cond.A"), 3),
      Vector("cond.A_b01", "cond.A_b02", "cond.A_b03")
    )
    assertEquals(
      Names.addBasis(Vector("t1", "t2"), 2),
      Vector("t1_b01", "t2_b01", "t1_b02", "t2_b02")
    )
  }

  test("makeColumnNames composes names") {
    assertEquals(
      Names.makeColumnNames(Some("term1"), Vector("cond.A"), 1),
      Vector("term1_cond.A")
    )
    assertEquals(
      Names.makeColumnNames(Some("term1"), Vector("cond.A"), 3),
      Vector("term1_cond.A_b01", "term1_cond.A_b02", "term1_cond.A_b03")
    )
    assertEquals(
      Names.makeColumnNames(Some("term1"), Vector("c1", "c2"), 2),
      Vector("term1_c1_b01", "term1_c2_b01", "term1_c1_b02", "term1_c2_b02")
    )
  }
