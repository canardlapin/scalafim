package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

class HrfNormalizationSuite extends munit.FunSuite:

  // Independent fmrihrf f42da39 receipt, evaluated by R on the package's
  // declared fixed grids. These pin the scale itself, not only normalized
  // output identities that an incorrect self-consistent implementation could
  // also satisfy.
  private val rSpmFactor = 491.16496534333595
  private val rCanonicalPeak = 1.7539456206329838
  private val rCanonicalIntegral = 9.835035504315636
  private val rPerBasisPeaks = Vector(1.7539456206329838, 0.6852932984245073, 0.454927494001017)

  private val spmGrid =
    Vector.tabulate(1600)(i => 32.0 * i.toDouble / 1599.0)

  private def normalized(hrf: Hrf, mode: HrfNormalization): Hrf =
    hrf.normalize(mode).fold(error => fail(error.message), identity)

  private def trapz(grid: Vector[Double], values: Array[Double]): Double =
    var total = 0.0
    var i = 1
    while i < grid.length do
      total += (grid(i) - grid(i - 1)) * (values(i - 1) + values(i)) / 2.0
      i += 1
    total

  test("SPM normalization uses the fixed 1,600-point Nilearn reference grid"):
    val transformed = Hrfs.SPMG1
      .normalizeWithTransform(HrfNormalization.Spm)
      .fold(error => fail(error.message), identity)
    val factor = transformed.transform match
      case BasisTransform.Diagonal(scales) => scales.head
      case other                           => fail(s"expected a diagonal normalization transform, got $other")
    assertEqualsDouble(factor, rSpmFactor, 1e-10)
    assertEqualsDouble(transformed.basis.evalDoubles(spmGrid).data.sum, 1.0, 1e-12)

  test("fixed normalization is independent of the later evaluation grid"):
    val scaled = normalized(Hrfs.SPMG1, HrfNormalization.Spm)
    val factorAtSix = Hrfs.SPMG1(Lag(6.0)).data(0) / scaled(Lag(6.0)).data(0)
    val factorAtEight = Hrfs.SPMG1(Lag(8.0)).data(0) / scaled(Lag(8.0)).data(0)
    assertEqualsDouble(factorAtSix, factorAtEight, 1e-12)

  test("canonical peak and integral modes apply one scalar to every basis"):
    val grid = Vector.tabulate(1201)(i => i.toDouble * 0.02)
    val raw = Hrfs.SPMG3.evalDoubles(grid)
    val peakTransform = Hrfs.SPMG3
      .normalizeWithTransform(HrfNormalization.UnitPeak)
      .fold(error => fail(error.message), identity)
    val integralTransform = Hrfs.SPMG3
      .normalizeWithTransform(HrfNormalization.UnitIntegral)
      .fold(error => fail(error.message), identity)
    val peak = peakTransform.basis.evalDoubles(grid)
    val integral = integralTransform.basis.evalDoubles(grid)

    peakTransform.transform match
      case BasisTransform.Diagonal(scales) => scales.foreach(scale => assertEqualsDouble(scale, rCanonicalPeak, 1e-12))
      case other                           => fail(s"expected a diagonal peak transform, got $other")
    integralTransform.transform match
      case BasisTransform.Diagonal(scales) => scales.foreach(scale => assertEqualsDouble(scale, rCanonicalIntegral, 1e-12))
      case other                           => fail(s"expected a diagonal integral transform, got $other")

    assertEqualsDouble(peak.col(0).data.map(math.abs).max, 1.0, 1e-12)
    assertEqualsDouble(trapz(grid, integral.col(0).data), 1.0, 1e-12)

    val factor = raw(300, 0) / peak(300, 0)
    var row = 0
    while row < grid.length do
      var column = 0
      while column < Hrfs.SPMG3.nbasis do
        assertEqualsDouble(peak(row, column), raw(row, column) / factor, 1e-12)
        column += 1
      row += 1

  test("per-basis peak mode uses independent fixed scales"):
    val grid = Vector.tabulate(1201)(i => i.toDouble * 0.02)
    val transformed = Hrfs.SPMG3
      .normalizeWithTransform(HrfNormalization.UnitPeakPerBasis)
      .fold(error => fail(error.message), identity)
    transformed.transform match
      case BasisTransform.Diagonal(scales) =>
        scales.zip(rPerBasisPeaks).foreach { case (actual, expected) =>
          assertEqualsDouble(actual, expected, 1e-12)
        }
      case other => fail(s"expected a diagonal per-basis transform, got $other")
    val scaled = transformed.basis.evalDoubles(grid)
    var column = 0
    while column < scaled.cols do
      assertEqualsDouble(scaled.col(column).data.map(math.abs).max, 1.0, 1e-12)
      column += 1

  test("normalization reports a transform that preserves reconstructed kernels"):
    val rawCoefficients = Vector(0.7, -1.1, 0.4)
    val transformed = Hrfs.SPMG3
      .normalizeWithTransform(HrfNormalization.UnitPeak)
      .fold(error => fail(error.message), identity)
    val originalBasis = ResponseBasis.of(Hrfs.SPMG3)
    val normalizedBasis = ResponseBasis.of(transformed.basis)
    val originalCoefficients = originalBasis.coefficients(rawCoefficients).fold(error => fail(error.message), identity)
    val normalizedCoefficients = transformed.transform
      .transportCoefficients(originalCoefficients)
      .fold(error => fail(error.message), identity)
    val original = originalBasis.reconstruct(originalCoefficients)
    val transported = normalizedBasis.reconstruct(BasisCoefficients.unsafe(normalizedCoefficients.values))

    Vector(0.0, 3.5, 6.0, 12.0, 24.0).foreach { lag =>
      assertEqualsDouble(transported(Lag(lag)).data(0), original(Lag(lag)).data(0), 1e-12)
    }

  test("construction entry points expose typed normalization without changing defaults"):
    assert(HrfCombinators.gen(Hrfs.SPMG1) eq Hrfs.SPMG1)
    val generated = HrfCombinators.gen(Hrfs.SPMG1, normalization = HrfNormalization.Spm)
    assertEqualsDouble(generated.evalDoubles(spmGrid).data.sum, 1.0, 1e-12)

    val registered = Registry
      .getEither("spmg1", normalization = HrfNormalization.UnitPeak)
      .fold(error => fail(error.message), identity)
    val grid = Vector.tabulate(1201)(i => i.toDouble * 0.02)
    assertEqualsDouble(registered.evalDoubles(grid).data.map(math.abs).max, 1.0, 1e-12)

    val conflict = HrfSpec(
      HrfKind.Spmg1,
      normalize = true,
      normalization = HrfNormalization.Spm
    )
    assertEquals(
      conflict,
      Left(HrfSpecError.InvalidNormalization(HrfNormalizationError.ConflictingModes))
    )

  test("unscaled mode preserves identity and unusable scales are typed errors"):
    val unchanged = Hrfs.SPMG1.normalize(HrfNormalization.None).fold(error => fail(error.message), identity)
    assert(unchanged.eq(Hrfs.SPMG1))

    val zero = Hrf.scalar("zero")(_ => 0.0)
    assertEquals(
      zero.normalize(HrfNormalization.Spm),
      Left(HrfNormalizationError.UnusableFactor("zero", HrfNormalization.Spm, 0, 0.0))
    )

    val subnormal = Hrf.scalar("subnormal")(_ => 1e-310)
    assertEquals(
      subnormal.normalize(HrfNormalization.UnitPeak),
      Left(HrfNormalizationError.UnusableFactor("subnormal", HrfNormalization.UnitPeak, 0, 1e-310))
    )
