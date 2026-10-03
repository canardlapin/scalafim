package scalafim.fmri.mvpa

class RsaScoreKernelsSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  test("Pearson is invariant to separate finite scales and reverses under a negative scale"):
    val a = Vector(1.0, 2.0, 3.0)
    val b = Vector(3.0, 1.0, 2.0)
    // Centered vectors (-1,0,1) and (1,-1,0) give -1/2.
    for leftScale <- Vector(1e200, 1e-200, 1.0); rightScale <- Vector(1e200, 1e-200, 1.0) do
      assertEqualsDouble(right(RsaScoreKernels.pearson(a.map(_ * leftScale), b.map(_ * rightScale))), -.5, 1e-12)
      assertEqualsDouble(right(RsaScoreKernels.pearson(a.map(_ * -leftScale), b.map(_ * rightScale))), .5, 1e-12)
    assertEqualsDouble(right(RsaScoreKernels.pearson(a.map(_ * 1e100), a.map(_ * 1e100))), 1.0, 1e-12)

  test("finite extremes and large offsets do not overflow sums or centered products"):
    val extremes = Vector(-1e308, 0.0, 1e308)
    assertEqualsDouble(right(RsaScoreKernels.pearson(extremes, extremes)), 1.0, 1e-12)
    val offset = Vector(1e300, 1e300 + 1e285, 1e300 + 2e285, 1e300 + 3e285)
    assertEqualsDouble(right(RsaScoreKernels.pearson(offset, offset.reverse)), -1.0, 1e-12)

  test("undefined and malformed inputs retain distinct typed failures"):
    assertEquals(RsaScoreKernels.pearson(Vector(2.0, 2.0), Vector(1.0, 2.0)), Left(RsaScoreFailure.ZeroVariance))
    assertEquals(RsaScoreKernels.pearson(Vector(1.0), Vector(2.0)), Left(RsaScoreFailure.InsufficientData(1)))
    assertEquals(RsaScoreKernels.pearson(Vector(1.0), Vector(2.0, 3.0)), Left(RsaScoreFailure.ShapeMismatch(1, 2)))
    assertEquals(RsaScoreKernels.pearson(Vector(1.0, Double.NaN), Vector(1.0, 2.0)), Left(RsaScoreFailure.NonFiniteInput(1)))
    assertEquals(RsaScoreKernels.spearman(Vector(1.0), Vector(2.0)), Left(RsaScoreFailure.InsufficientData(1)))
    assertEquals(RsaScoreKernels.spearman(Vector(1.0), Vector(2.0, 3.0)), Left(RsaScoreFailure.ShapeMismatch(1, 2)))

  test("average ties produce the independent five sixths Spearman expectation"):
    val a = Vector(1.0, 1.0, 2.0, 3.0)
    val b = Vector(1.0, 2.0, 2.0, 3.0)
    assertEquals(right(RsaScoreKernels.averageRanks(a)), Vector(1.5, 1.5, 3.0, 4.0))
    assertEqualsDouble(right(RsaScoreKernels.spearman(a, b)), 5.0 / 6.0, 1e-12)
