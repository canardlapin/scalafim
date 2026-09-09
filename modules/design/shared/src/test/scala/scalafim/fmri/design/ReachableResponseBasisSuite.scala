package scalafim.fmri.design

import scalafim.fmri.hrf.*

class ReachableResponseBasisSuite extends munit.FunSuite:

  private def lwuFamily: Vector[Hrf] =
    for
      tau <- Vector(5.0, 6.0, 7.0)
      sigma <- Vector(2.0, 2.5, 3.0)
      rho <- Vector(0.2, 0.35)
    yield Hrfs.lwu(tau = tau, sigma = sigma, rho = rho)

  private lazy val basis: ReachableResponseBasis =
    ReachableResponseBasis
      .fromFamily(lwuFamily, samples = 97, maxRank = 18)
      .fold(error => fail(error.message), identity)

  test("basis columns are orthonormal on the sampling grid") {
    val curves = basis.curves
    var a = 0
    while a < basis.rank do
      var b = 0
      while b < basis.rank do
        var dot = 0.0
        var l = 0
        while l < basis.sampleCount do
          dot += curves(l, a) * curves(l, b)
          l += 1
        val expected = if a == b then 1.0 else 0.0
        assertEqualsDouble(dot, expected, 1e-8, s"columns $a and $b")
        b += 1
      a += 1
  }

  test("singular values descend and energy fractions rise to one") {
    var j = 1
    while j < basis.rank do
      assert(
        basis.singularValues(j) <= basis.singularValues(j - 1) + 1e-12,
        s"singular value $j out of order"
      )
      assert(
        basis.energyFractions(j) >= basis.energyFractions(j - 1) - 1e-12,
        s"energy fraction $j decreased"
      )
      j += 1
    assert(basis.energyFractions.last > 0.999999, s"final energy ${basis.energyFractions.last}")
  }

  test("family members are recovered at full retained rank") {
    lwuFamily.foreach { member =>
      val projection = basis.project(member).fold(error => fail(error.message), identity)
      assert(projection.residual <= 1e-5, s"${member.name}: residual ${projection.residual}")
    }
  }

  test("a held-out kernel has monotone, small span residuals") {
    val heldOut = Hrfs.lwu(tau = 5.5, sigma = 2.25, rho = 0.3)
    val projection = basis.project(heldOut).fold(error => fail(error.message), identity)
    var j = 1
    while j < projection.rank do
      assert(
        projection.residualByRank(j) <= projection.residualByRank(j - 1) + 1e-12,
        s"residual increased at rank ${j + 1}"
      )
      j += 1
    assert(
      projection.residualByRank(math.min(8, projection.rank) - 1) <= 1e-2,
      s"rank-8 residual ${projection.residualByRank(math.min(8, projection.rank) - 1)}"
    )
  }

  test("projecting a basis column returns a unit coefficient and no residual") {
    val column = Array.tabulate(basis.sampleCount)(l => basis.curves(l, 0))
    val projection = basis.projectCurve(column).fold(error => fail(error.message), identity)
    assertEqualsDouble(projection.coefficients(0), 1.0, 1e-10)
    var j = 1
    while j < projection.rank do
      assertEqualsDouble(projection.coefficients(j), 0.0, 1e-8, s"coefficient $j")
      j += 1
    assert(projection.residual <= 1e-8, s"residual ${projection.residual}")
  }

  test("truncation keeps the leading directions and matching receipts") {
    val truncated = basis.truncate(4).fold(error => fail(error.message), identity)
    assertEquals(truncated.rank, 4)
    val heldOut = Hrfs.lwu(tau = 5.5, sigma = 2.25, rho = 0.3)
    val full = basis.project(heldOut).fold(error => fail(error.message), identity)
    val short = truncated.project(heldOut).fold(error => fail(error.message), identity)
    var j = 0
    while j < 4 do
      assertEqualsDouble(short.coefficients(j), full.coefficients(j), 1e-12, s"coefficient $j")
      assertEqualsDouble(short.residualByRank(j), full.residualByRank(j), 1e-12, s"residual $j")
      j += 1
  }

  test("construction refuses empty, under-sampled, and multi-basis input") {
    assertEquals(
      ReachableResponseBasis.fromFamily(Vector.empty),
      Left(ReachableBasisError.EmptyFamily)
    )
    ReachableResponseBasis.fromFamily(lwuFamily, samples = 1) match
      case Left(ReachableBasisError.InvalidSampling(_)) => ()
      case other => fail(s"expected InvalidSampling, got $other")
    ReachableResponseBasis.fromFamily(Vector(Hrfs.SPMG2)) match
      case Left(ReachableBasisError.MultiBasisMember(_, nbasis)) => assertEquals(nbasis, 2)
      case other => fail(s"expected MultiBasisMember, got $other")
  }

  test("projection refuses mismatched, degenerate, and multi-basis curves") {
    basis.projectCurve(new Array[Double](3)) match
      case Left(ReachableBasisError.CurveLengthMismatch(expected, actual)) =>
        assertEquals(expected, basis.sampleCount)
        assertEquals(actual, 3)
      case other => fail(s"expected CurveLengthMismatch, got $other")
    basis.projectCurve(new Array[Double](basis.sampleCount)) match
      case Left(ReachableBasisError.DegenerateCurve) => ()
      case other => fail(s"expected DegenerateCurve, got $other")
    basis.project(Hrfs.SPMG2) match
      case Left(ReachableBasisError.MultiBasisMember(_, _)) => ()
      case other => fail(s"expected MultiBasisMember, got $other")
    basis.truncate(0) match
      case Left(ReachableBasisError.InvalidSampling(_)) => ()
      case other => fail(s"expected InvalidSampling, got $other")
  }
