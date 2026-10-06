package scalafim.fmri.design.hrf

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ParametricHrfFamily, ShapeChart, ShapePoint, ShapeSummary}

class KernelBasisBudgetSuite extends munit.FunSuite:
  private val step = PositiveSeconds(0.4).toOption.get

  private class Family(val chart: ShapeChart, refuseCallbacks: Boolean = true) extends ParametricHrfFamily:
    var evaluations: Int = 0
    def name: String = "budget-family"
    def kind: HrfKind = HrfKind.Gaussian
    def horizon: PositiveSeconds = PositiveSeconds(1.0).toOption.get
    def supports(rule: NormalizationRule): Boolean = true
    def libraryNormalization: NormalizationRule = NormalizationRule.Unnormalised
    private def count(): Unit =
      evaluations += 1
      if refuseCallbacks then fail("compiler admission must precede callbacks")
    def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      count()
      lags.indices.foreach(i => out(i) = math.exp(-lags(i)))
    def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      count()
      java.util.Arrays.fill(out, 0.0)
      lags.indices.foreach(i => out(i) = math.exp(-lags(i)))
    def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
      java.util.Arrays.fill(out, 0.0)
      out(0) = 1.0
    def summaries(point: ShapePoint): ShapeSummary = fail("compiler must not request shape summaries")
    def descriptor(point: ShapePoint): HrfDescriptor = HrfDescriptor.custom(name, 1, 1.0.s)
    def toHrf(point: ShapePoint): Hrf = Hrf.scalar(name, span = 1.0.s)(lag => math.exp(-lag.value))

  private def spec(family: ParametricHrfFamily): KernelBasisSpec =
    KernelBasisSpec(family, step, Vector.fill(family.dimension)(2), 0.01,
      maxRank = 2, heldOutPoints = 1)

  private def limit(result: Either[KernelBasisError, ?], quantity: String): Unit = result match
    case Left(KernelBasisError.Admission(KernelBasisAdmissionError.LimitExceeded(actual, _, _))) =>
      assertEquals(actual, quantity)
    case other => fail(s"expected $quantity admission refusal, got $other")

  test("portable-budget-exact-boundaries"):
    Vector(
      "fine samples" -> KernelBasisBudget.MaxFineSamples.toLong,
      "shape grid points" -> KernelBasisBudget.MaxGridPoints.toLong,
      "training columns" -> KernelBasisBudget.MaxTrainingColumns.toLong,
      "array cells" -> KernelBasisBudget.MaxArrayCells.toLong,
      "storage cells" -> KernelBasisBudget.MaxStorageCells,
      "training work" -> KernelBasisBudget.MaxTrainingWork,
      "spectral work" -> KernelBasisBudget.MaxSpectralWork,
      "certification work" -> KernelBasisBudget.MaxCertificationWork
    ).foreach: (quantity, maximum) =>
      assertEquals(KernelBasisBudget.checked(quantity, maximum.toDouble, maximum), Right(maximum.toDouble))
      limit(KernelBasisBudget.checked(quantity, maximum.toDouble + 1.0, maximum), quantity)
      limit(KernelBasisBudget.checked(quantity, Double.PositiveInfinity, maximum), quantity)

  test("explicit-capacity-is-positive-and-bounded-without-changing-defaults"):
    assertEquals(KernelBasisCapacity.Default.arrayCells, 2_000_000)
    assertEquals(KernelBasisCapacity.Default.spectralWork, 100_000_000_000L)
    Vector(0L, -1L, KernelBasisBudget.MaxStorageCells + 1L, Long.MaxValue).foreach: cells =>
      assert(KernelBasisCapacity(cells, 128_000_000_000L).isLeft)
    Vector(0L, -1L, KernelBasisCapacity.MaxExactSpectralWork + 1L, Long.MaxValue).foreach: work =>
      assert(KernelBasisCapacity(3_000_000L, work).isLeft)
    val maximum = KernelBasisCapacity(KernelBasisBudget.MaxStorageCells, KernelBasisCapacity.MaxExactSpectralWork).toOption.get
    assertEquals(maximum.arrayCells.toLong, KernelBasisBudget.MaxStorageCells)
    assertEquals(maximum.spectralWork, KernelBasisCapacity.MaxExactSpectralWork)
    limit(KernelBasisBudget.checked("spectral work", math.pow(2.0, 53.0), maximum.spectralWork), "spectral work")

  test("explicit-array-admission-retains-fixed-total-storage-and-callback-refusal"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0), ("b", 0.0, 1.0), ("c", 0.0, 1.0)))
    val input = spec(family).copy(nodesPerAxis = Vector(50, 50, 30))
    assertEquals(HrfKernelBasis.estimate(input.copy(capacity = null)), Left(KernelBasisError.InvalidSpec("capacity must be supplied")))
    limit(HrfKernelBasis.estimate(input), "training matrix cells")
    val capacity = KernelBasisCapacity(2_250_000L, KernelBasisBudget.MaxSpectralWork).toOption.get
    val admitted = HrfKernelBasis.estimate(input.copy(capacity = capacity)).toOption.get
    assertEquals(admitted.trainingCells, 2_250_000L)
    assertEquals(admitted.capacity, capacity)
    assert(admitted.storageCells <= KernelBasisBudget.MaxStorageCells)
    val below = KernelBasisCapacity(2_249_999L, KernelBasisBudget.MaxSpectralWork).toOption.get
    limit(HrfKernelBasis.compile(input.copy(capacity = below)), "training matrix cells")
    val enlarged = KernelBasisCapacity(3_000_000L, 128_000_000_000L).toOption.get
    limit(HrfKernelBasis.compile(input.copy(nodesPerAxis = Vector(100, 100, 10), capacity = enlarged)), "storage cells")
    assertEquals(family.evaluations, 0)

  test("frozen-LWU-capacity-admits-exact-inputs-and-keeps-spectral-default-refusal"):
    val family = scalafim.fmri.hrf.family.LwuFamily.Default
    val input = KernelBasisSpec(family, PositiveSeconds(0.1).toOption.get, Vector(14, 9, 7), 1e-3, maxRank = 48)
    limit(HrfKernelBasis.estimate(input), "training matrix cells")
    val arraysOnly = KernelBasisCapacity(3_000_000L, KernelBasisBudget.MaxSpectralWork).toOption.get
    limit(HrfKernelBasis.estimate(input.copy(capacity = arraysOnly)), "spectral work")
    val capacity = KernelBasisCapacity(3_000_000L, 128_000_000_000L).toOption.get
    val estimate = HrfKernelBasis.estimate(input.copy(capacity = capacity)).toOption.get
    assertEquals(estimate.fineCount, 321)
    assertEquals(estimate.gridPoints, 882)
    assertEquals(estimate.trainingCells, 2_831_220L)
    assertEquals(estimate.storageCells, 9_024_270L)
    assertEquals(estimate.trainingWork, 8_498_985L)
    assertEquals(estimate.spectralWork, 120_096_758_484L)
    assertEquals(estimate.certificationWork, 49_833_900L)
    assertEquals(estimate.capacity, capacity)

  test("nondefault-capacity-is-recorded-and-default-provenance-is-unchanged"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false)
    val input = spec(family)
    val baseline = HrfKernelBasis.compile(input).fold(e => fail(e.message), identity)
    val capacity = KernelBasisCapacity(3_000_000L, 128_000_000_000L).toOption.get
    val explicit = HrfKernelBasis.compile(input.copy(capacity = capacity)).fold(e => fail(e.message), identity)
    assertEquals(explicit.provenance.canonical, baseline.provenance.canonical + "|arrayCells=3000000|spectralWork=128000000000")
    assertEquals(baseline.provenance.copy(capacity = KernelBasisCapacity(2_000_000L, 100_000_000_000L).toOption.get).canonical,
      baseline.provenance.canonical)
    val equivalentDefault = KernelBasisCapacity(2_000_000L, 100_000_000_000L).toOption.get
    assertEquals(baseline.provenance.copy(capacity = equivalentDefault), baseline.provenance)
    assertEquals(equivalentDefault.hashCode(), KernelBasisCapacity.Default.hashCode())
    val workOnly = KernelBasisCapacity(2_000_000L, 128_000_000_000L).toOption.get
    assertEquals(baseline.provenance.copy(capacity = workOnly).canonical,
      baseline.provenance.canonical + "|arrayCells=2000000|spectralWork=128000000000")
    assertEquals(explicit.rank, baseline.rank)
    assertEquals(explicit.singularValues.map(java.lang.Double.doubleToLongBits),
      baseline.singularValues.map(java.lang.Double.doubleToLongBits))

  test("node-products-refuse-before-integer-overflow-or-callbacks"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0), ("b", 0.0, 1.0), ("c", 0.0, 1.0)))
    limit(HrfKernelBasis.compile(spec(family).copy(nodesPerAxis = Vector.fill(3)(Int.MaxValue))), "shape grid points")
    limit(HrfKernelBasis.compile(spec(family).copy(nodesPerAxis = Vector(100, 100, 11))), "shape grid points")
    assertEquals(family.evaluations, 0)

  test("training-matrix-refuses-before-builder-and-family-callbacks"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)))
    val fine = PositiveSeconds(0.001).toOption.get
    limit(HrfKernelBasis.compile(spec(family).copy(fineStep = fine, nodesPerAxis = Vector(1000))), "training matrix cells")
    assertEquals(family.evaluations, 0)

  test("full-SVD-shapes-are-independent-of-requested-rank"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0), ("b", 0.0, 1.0)))
    val input = spec(family).copy(nodesPerAxis = Vector(3, 2))
    val low = HrfKernelBasis.estimate(input.copy(maxRank = 1)).toOption.get
    val high = HrfKernelBasis.estimate(input.copy(maxRank = Int.MaxValue)).toOption.get
    assertEquals(low.fineCount, 3)
    assertEquals(low.gridPoints, 6)
    assertEquals(low.trainingColumns, 36)
    assertEquals(low.trainingCells, 108L)
    assertEquals(low.thinRank, 3)
    assertEquals(low.thinLeftCells, 9L)
    assertEquals(low.thinRightCells, 108L)
    assertEquals(low.svdScratchCells, 126L)
    assertEquals(low.svdCanonicalCells, 117L)
    assertEquals(low.svdGramCells, 18L)
    assertEquals(low.spectralWork, high.spectralWork)
    assertEquals(low.svdScratchCells, high.svdScratchCells)
    assertEquals(high.availableRank, 3)
    assertEquals(family.evaluations, 0)

  test("tall-SVD-shapes-use-thin-right-square-not-column-square"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)))
    val input = spec(family).copy(fineStep = PositiveSeconds(0.1).toOption.get, includeDerivatives = false)
    val estimate = HrfKernelBasis.estimate(input).toOption.get
    assertEquals(estimate.fineCount, 11)
    assertEquals(estimate.trainingColumns, 2)
    assertEquals(estimate.thinRank, 2)
    assertEquals(estimate.thinLeftCells, 22L)
    assertEquals(estimate.thinRightCells, 4L)
    assertEquals(estimate.svdScratchCells, 32L)
    assertEquals(estimate.svdGramCells, 8L)

  test("value-only-training-still-charges-full-certificate-jets"):
    val family = new Family(GaussianFamily.Default.chart)
    val input = spec(family).copy(fineStep = PositiveSeconds(0.01).toOption.get,
      includeDerivatives = false, maxRank = 1)
    val estimate = HrfKernelBasis.estimate(input).toOption.get
    assertEquals(estimate.trainingComponents, 1)
    assertEquals(estimate.jetComponents, 6)
    assertEquals(estimate.jetScratchCells, 6L * estimate.fineCount)
    val derivativeTraining = HrfKernelBasis.estimate(input.copy(includeDerivatives = true)).toOption.get
    assertEquals(estimate.certificationWork, derivativeTraining.certificationWork)
    val threshold = (KernelBasisBudget.MaxCertificationWork / estimate.certificationWork).toInt + 1
    limit(HrfKernelBasis.compile(input.copy(heldOutPoints = threshold)), "certification work")
    assertEquals(family.evaluations, 0)

  test("spectral-work-refuses-before-SVD-even-for-small-requested-rank"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)))
    // About one million training cells; the full 1,001-wide SVD dominates.
    val input = spec(family).copy(fineStep = PositiveSeconds(0.001).toOption.get,
      nodesPerAxis = Vector(334), maxRank = 1)
    limit(HrfKernelBasis.compile(input), "spectral work")
    assertEquals(family.evaluations, 0)

  test("finite-chart-bounds-with-overflowing-width-are-refused-before-callbacks"):
    val family = new Family(ShapeChart(("a", -Double.MaxValue, Double.MaxValue)))
    HrfKernelBasis.compile(spec(family)) match
      case Left(KernelBasisError.Admission(KernelBasisAdmissionError.InvalidChartWidth("a", width))) =>
        assert(!width.isFinite)
      case other => fail(s"expected chart width refusal, got $other")
    assertEquals(family.evaluations, 0)

  test("finite-chart-width-with-overflowing-node-intermediate-is-refused"):
    val family = new Family(ShapeChart(("a", 0.0, Double.MaxValue)))
    HrfKernelBasis.compile(spec(family).copy(nodesPerAxis = Vector(3))) match
      case Left(KernelBasisError.Admission(KernelBasisAdmissionError.InvalidChartPoint(error))) =>
        assert(error.message.contains("finite"))
      case other => fail(s"expected generated node refusal, got $other")
    assertEquals(family.evaluations, 0)

  test("admitted-compile-reports-actual-retained-and-certificate-shapes"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false)
    val basis = HrfKernelBasis.compile(spec(family)).fold(e => fail(e.message), identity)
    assertEquals(basis.fineCount, 3)
    assertEquals(basis.allocation.rank, basis.rank)
    assertEquals(basis.allocation.phiCells, basis.rank.toLong * basis.fineCount.toLong)
    assertEquals(basis.allocation.singularValues, basis.singularValues.length)
    assertEquals(basis.allocation.certificateRanks, basis.certificate.valueError.length)
    assertEquals(basis.allocation.certificateErrorValues,
      basis.certificate.valueError.length.toLong + basis.certificate.firstDerivativeError.length + basis.certificate.secondDerivativeError.length)
    assert(basis.certificate.valueError(basis.rank - 1) <= 0.01)
    assertEqualsDouble(basis.lags.last, 0.8, 1e-15)
    assert(basis.provenance.canonical.startsWith("kernel-basis/v2|family=13:budget-family"))

  test("callback-failures-remain-typed-after-admission"):
    val family = new Family(ShapeChart(("a", 0.0, 1.0)))
    HrfKernelBasis.compile(spec(family)) match
      case Left(KernelBasisError.Evaluation(detail)) => assert(detail.contains("precede callbacks"))
      case other => fail(s"expected typed callback failure, got $other")

  test("custom-tail-diagnostics-refuse-invalid-fractions-and-thrown-callbacks"):
    import scalafim.fmri.hrf.family.FamilySummaryError
    Vector(Double.NaN, Double.PositiveInfinity, -0.1, 1.1).foreach: value =>
      val family = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false):
        override def tailRelativeEnergyEither(point: ShapePoint, precision: PositiveSeconds, extent: Double): Either[FamilySummaryError, Double] = Right(value)
      HrfKernelBasis.compile(spec(family)) match
        case Left(KernelBasisError.Evaluation(detail)) => assert(detail.contains("tail energy"))
        case other => fail(s"expected invalid tail energy refusal, got $other")
    val throwing = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false):
      override def tailRelativeEnergyEither(point: ShapePoint, precision: PositiveSeconds, extent: Double): Either[FamilySummaryError, Double] =
        throw new IllegalArgumentException("tail callback failure")
    assertEquals(HrfKernelBasis.compile(spec(throwing)), Left(KernelBasisError.Evaluation("tail callback failure")))

  test("generated-Cascade-endpoints-preserve-original-IEEE-values"):
    val family = scalafim.fmri.hrf.family.Cascade34Family.Default
    val input = spec(family).copy(nodesPerAxis = Vector(9, 7, 5))
    val chart = family.chart
    val axis = 1
    val index = input.nodesPerAxis(axis) - 1
    val original = chart.lower(axis) + chart.width(axis) * index / index
    val generated = KernelBasisBudget.coordinate(input, axis, index)
    assertEquals(java.lang.Double.doubleToLongBits(generated), java.lang.Double.doubleToLongBits(original))
    assert(generated > chart.upper(axis), "fixture must exercise the legacy ULP overshoot")
    assert(HrfKernelBasis.estimate(input).isRight)
    val coordinates = chart.lower.updated(axis, generated)
    val point = KernelBasisBudget.generatedPoint(input, coordinates).toOption.get
    assertEquals(java.lang.Double.doubleToLongBits(point(axis)), java.lang.Double.doubleToLongBits(original))

  test("certificate-refuses-finite-projection-whose-square-overflows"):
    import scalafim.fmri.hrf.family.FamilySummaryError
    val x = math.sqrt(Double.MaxValue / 2.0)
    val family = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false):
      override def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
        java.util.Arrays.fill(out, 0.0)
        out(0) = x
        out(1) = x
      override def tailRelativeEnergyEither(point: ShapePoint, precision: PositiveSeconds, extent: Double): Either[FamilySummaryError, Double] = Right(0.0)
    val input = spec(family).copy(fineStep = PositiveSeconds(0.6).toOption.get, maxRank = 1)
    val admitted = HrfKernelBasis.estimate(input).toOption.get
    HrfKernelBasis.certify(family, Array(0.0, 0.6), Array(math.sqrt(0.5), math.sqrt(0.5)), 1, input, admitted) match
      case Left(KernelBasisError.Evaluation(detail)) => assert(detail.contains("squared certificate projection"))
      case other => fail(s"expected squared projection overflow refusal, got $other")

  test("training-refuses-finite-values-with-overflowing-column-norm"):
    val x = math.sqrt(Double.MaxValue / 2.0)
    val family = new Family(ShapeChart(("a", 0.0, 1.0)), refuseCallbacks = false):
      override def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
        java.util.Arrays.fill(out, 0, lags.length, x)
    HrfKernelBasis.compile(spec(family).copy(includeDerivatives = false)) match
      case Left(KernelBasisError.Evaluation(detail)) => assert(detail.contains("training norm"))
      case other => fail(s"expected training norm overflow refusal, got $other")
