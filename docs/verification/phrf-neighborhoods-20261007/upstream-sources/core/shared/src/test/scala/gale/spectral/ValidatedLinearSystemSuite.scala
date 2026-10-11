package gale.spectral

import gale.linalg.{DMat, DVec}

class ValidatedLinearSystemSuite extends munit.FunSuite:
  test("reference certificate encloses every vertex system without trusting inverse or solution"):
    val lower = DMat.tabulate(2, 2)((i, j) => if i == j then 1.9 else 0.08)
    val upper = DMat.tabulate(2, 2)((i, j) => if i == j then 2.1 else 0.12)
    val input = MatrixEnclosure.checked(lower, upper).toOption.get
    val rhs = MatrixEnclosure.checked(DMat.tabulate(2, 1)((i, _) => 0.9 + i),
      DMat.tabulate(2, 1)((i, _) => 1.1 + i)).toOption.get
    val inverse = DMat.tabulate(2, 2)((i, j) => if i == j then 0.5 else 0.0)
    val candidate = DVec.fromSeq(Vector(0.4, 0.9))
    val query = DVec.fromSeq(Vector(1.0, -1.0))
    val bound = ValidatedLinearSystem.bound(input, rhs, candidate, inverse, Some(query)).fold(e => fail(e.toString), identity)
    assert(bound.contractionInfinityUpper < 1.0)
    (0 until 64).foreach: mask =>
      val matrix = DMat.tabulate(2, 2)((i, j) => if (mask & (1 << (i * 2 + j))) == 0 then lower(i, j) else upper(i, j))
      val b = DVec.fromSeq(Vector.tabulate(2)(i => if (mask & (1 << (4 + i))) == 0 then rhs.lower(i, 0) else rhs.upper(i, 0)))
      val exact = matrix.solve(b).toOption.get
      (0 until 2).foreach(i => assert(math.abs(exact(i) - candidate(i)) <= bound.componentAbsoluteUpper(i)))
      assert(math.abs((exact(0) - candidate(0)) - (exact(1) - candidate(1))) <= bound.queryAbsoluteUpper.get)

  test("singular candidates refuse and accurate candidates yield tight bounds"):
    val matrix = DMat.tabulate(2, 2)((i, j) => if i == j then 2.0 else 0.0)
    val input = MatrixEnclosure.exact(matrix).toOption.get
    val rhs = MatrixEnclosure.exact(DMat.tabulate(2, 1)((i, _) => 1.0 + i)).toOption.get
    val a = DVec.fromSeq(Vector(0.5, 1.0))
    assert(ValidatedLinearSystem.bound(input, rhs, a, DMat.zeros(2, 2)).isLeft)
    val inverse = DMat.tabulate(2, 2)((i, j) => if i == j then 0.5 else 0.0)
    val result = ValidatedLinearSystem.bound(input, rhs, a, inverse).toOption.get
    assert(result.errorInfinityUpper < 1e-12)

  test("a signed query certificate retains cancellation instead of summing component errors"):
    val input = MatrixEnclosure.exact(DMat.eye(2)).toOption.get
    val rhs = MatrixEnclosure.exact(DMat.tabulate(2, 1)((_, _) => 10.0)).toOption.get
    val candidate = DVec.fromSeq(Vector(9.0, 9.0))
    val bound = ValidatedLinearSystem.bound(input, rhs, candidate, DMat.eye(2),
      Some(DVec.fromSeq(Vector(1.0, -1.0)))).toOption.get
    assert(bound.errorInfinityUpper >= 1.0)
    assert(bound.queryAbsoluteUpper.get < 1e-12)
    assert(ValidatedLinearSystem.bound(input, rhs, candidate, DMat.eye(2), Some(DVec.fromSeq(Vector(1.0)))).isLeft)
