package scalafim.fmri.design

import scalafim.fmri.hrf.linalg.Mat

class ValidateSuite extends munit.FunSuite:

  test("Validate.validateContrasts: rank-deficient design marks non-estimable contrast") {
    val X = Mat.unsafe(3, 2, Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
    val colNames = Vector("a", "b")
    val res = Validate.validateContrasts(X, colNames, Vector(1.0, -1.0), name = "ab")
    assertEquals(res.length, 1)
    val row = res.head
    assertEquals(row.name, "ab")
    assertEquals(row.contrastType, Validate.ContrastType.T)
    assertEquals(row.estimable, false)
    assertEquals(row.sumToZero, true)
    assertEquals(row.orthogonalToIntercept, true)
    assertEquals(row.fullRank, None)
    assertEquals(row.nonzeroWeights, 2)
  }

  test("Validate.validateContrasts: detects intercept weights and sum-to-zero") {
    val X = Mat.unsafe(
      5,
      2,
      Array(
        1.0, 0.0,
        1.0, 1.0,
        1.0, 2.0,
        1.0, 3.0,
        1.0, 4.0
      )
    )
    val colNames = Vector("constant", "x")
    val res = Validate.validateContrasts(X, colNames, Vector(1.0, -1.0), name = "c")
    assertEquals(res.length, 1)
    val row = res.head
    assertEquals(row.estimable, true)
    assertEquals(row.sumToZero, true)
    assertEquals(row.orthogonalToIntercept, false)
    assertEquals(row.nonzeroWeights, 2)
  }

  test("Validate.validateContrasts: aligns weights by row names") {
    val X = Mat.eye(3)
    val colNames = Vector("a", "b", "c")
    val W = Mat.unsafe(2, 1, Array(1.0, -1.0))
    val res = Validate.validateContrasts(X, colNames, W, name = "b_vs_c", weightRowNames = Some(Vector("b", "c")))
    assertEquals(res.length, 1)
    val row = res.head
    assertEquals(row.name, "b_vs_c")
    assertEquals(row.estimable, true)
    assertEquals(row.sumToZero, true)
    assertEquals(row.nonzeroWeights, 2)
  }

  test("Validate.validateContrasts: F contrasts report fullRank") {
    val X = Mat.eye(2)
    val colNames = Vector("x1", "x2")
    val W = Mat.unsafe(2, 2, Array(1.0, 2.0, 0.0, 0.0))
    val res = Validate.validateContrasts(X, colNames, W, name = "fcon")
    assertEquals(res.map(_.name), Vector("fcon#1", "fcon#2"))
    assert(res.forall(_.contrastType == Validate.ContrastType.F))
    assert(res.forall(_.fullRank.contains(false)))
  }

  test("Validate.checkCollinearity: flags highly correlated columns") {
    val X = Mat.unsafe(
      5,
      4,
      Array(
        1.0, 1.0, 2.0, 0.0,
        1.0, 2.0, 4.0, 0.0,
        1.0, 3.0, 6.0, 0.0,
        1.0, 4.0, 8.0, 0.0,
        1.0, 5.0, 10.0, 0.0
      )
    )
    val colNames = Vector("constant", "a", "b", "zero")
    val res = Validate.checkCollinearity(X, colNames, threshold = 0.9)
    assertEquals(res.ok, false)
    assertEquals(res.pairs.map(p => (p.regressor1, p.regressor2)), Vector(("a", "b")))
    assert(clue(math.abs(res.pairs.head.r - 1.0)) < 1e-12)
  }
