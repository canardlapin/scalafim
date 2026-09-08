package scalafim.fmri.mvpa

import multivar.family.canonical.{CanonicalEffectSolution, ResidualRegularization, TraceRidgeFraction}

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.Matrix
import scalafim.fmri.fit.{
  DesignMatrix,
  PreparedContrastGeometry,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank
}
import scalafim.fmri.model.FitConfig

class CanonicalEffectAcceptanceSuite extends munit.FunSuite:
  private val R = CanonicalEffectReferenceFixtures

  test("the complete leave-one-run-out path agrees with the committed base-R oracle"):
    val result = analyze(
      responses = R.runs.map(_.response),
      design = R.design,
      contrastScale = 1.0,
      regularization = ridge(R.ridgeFraction)
    )
    val payload = onlySuccess(result)

    R.folds.foreach: expected =>
      val actual = foldFor(payload, expected.heldOutRun)
      assertEqualsDouble(actual.trainingFit.root.value, expected.trainingRoot, 1e-8)
      assertEqualsDouble(actual.trainingFit.regularization.ridgeAmount, expected.ridge, 1e-10)
      assertEqualsDouble(actual.heldOutRoot, expected.heldOutRoot, 1e-8)
      actual.trainingFit.solution match
        case CanonicalEffectSolution.Simple(direction, _) =>
          assertEquals(direction.length, expected.direction.length)
          var index = 0
          while index < direction.length do
            assertEqualsDouble(direction(index), expected.direction(index), 1e-8)
            index += 1
        case other => fail(s"expected an identifiable R-oracle direction, got $other")

    assertEqualsDouble(payload.meanHeldOutRoot, R.meanHeldOutRoot, 1e-8)
    assertEqualsDouble(payload.canonicalCorrelation, R.rootToCorrelationOfMean, 1e-10)

  test("run and row order, contrast scale, and nuisance basis do not change the estimand"):
    val baseline = onlySuccess(analyze(R.runs.map(_.response), R.design, 1.0, ridge(R.ridgeFraction)))

    val runOrder = Vector(2, 0, 1)
    val reorderedRuns = analyze(
      runOrder.map(R.runs(_).response),
      R.design,
      1.0,
      ridge(R.ridgeFraction),
      partitions = runOrder.map(index => PartitionId.unsafe(s"run-$index"))
    )
    assertFoldRootsEqual(baseline, onlySuccess(reorderedRuns), 1e-8)

    val rowOrder = Vector(5, 0, 7, 2, 1, 6, 3, 4)
    val rowReordered = onlySuccess(
      analyze(
        R.runs.map(run => permuteRows(run.response, rowOrder)),
        permuteRows(R.design, rowOrder),
        1.0,
        ridge(R.ridgeFraction)
      )
    )
    assertFoldRootsEqual(baseline, rowReordered, 1e-8)

    val rescaledContrast = onlySuccess(analyze(R.runs.map(_.response), R.design, -3.0, ridge(R.ridgeFraction)))
    assertFoldRootsEqual(baseline, rescaledContrast, 1e-8)

    val nuisanceReparameterized = onlySuccess(
      analyze(
        R.runs.map(_.response),
        reparameterizeNuisance(R.design),
        1.0,
        ridge(R.ridgeFraction)
      )
    )
    assertFoldRootsEqual(baseline, nuisanceReparameterized, 1e-8)

  test("feature rotations, scale, and semantic feature-axis permutations obey their covariance laws"):
    val responses = R.runs.map(_.response)
    val baseline = onlySuccess(analyze(responses, R.design, 1.0, ridge(R.ridgeFraction)))
    val rotation = fromRows(
      Vector(
        Vector(0.8, -0.6, 0.0),
        Vector(0.6, 0.8, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val rotated = onlySuccess(analyze(responses.map(_ * rotation), R.design, 1.0, ridge(R.ridgeFraction)))
    assertFoldRootsEqual(baseline, rotated, 1e-7)

    val scale = 7.0
    val scaled = onlySuccess(analyze(responses.map(mapValues(_, _ * scale)), R.design, 1.0, ridge(R.ridgeFraction)))
    assertFoldRootsEqual(baseline, scaled, 1e-7)
    baseline.folds.foreach: fold =>
      val scaledFold = foldFor(scaled, runNumber(fold.receipt.heldOut))
      assertEqualsDouble(
        scaledFold.trainingFit.regularization.ridgeAmount,
        fold.trainingFit.regularization.ridgeAmount * scale * scale,
        1e-8
      )

    val permutation = Vector(2, 0, 1)
    val permutedResponses = responses.map(response => permuteColumns(response, permutation))
    val semanticAxis = permutation.map(index => FeatureId.unsafe(s"feature-$index"))
    val permuted = onlySuccess(
      analyze(
        permutedResponses,
        R.design,
        1.0,
        ridge(R.ridgeFraction),
        neuralKeys = Some(semanticAxis)
      )
    )
    assertFoldRootsEqual(baseline, permuted, 1e-8)

  test("zero effects remain zero and perfect held-out effects report typed denominator degeneracy"):
    val residual = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val zeroResponses = Vector.tabulate(3): run =>
      fromRows(residual.map(value => Vector(value * (run + 1).toDouble)))
    val zero = analyze(zeroResponses, R.design, 1.0, ridge(0.02))
    val zeroPayload = onlySuccess(zero)
    assert(zeroPayload.meanHeldOutRoot <= 1e-24)

    val task = column(R.design, 1)
    val noisy = task.zip(residual).map((signal, noise) => Vector(signal + 0.2 * noise))
    val exact = task.map(value => Vector(value))
    val perfect = analyze(Vector(fromRows(exact), fromRows(noisy)), R.design, 1.0, ridge(0.02))
    perfect.values.head.outcome match
      case MeasurementOutcome.Failed(CanonicalTaskFailure.NonPositiveDenominator(partition, value), _) =>
        assertEquals(partition, PartitionId.unsafe("run-0"))
        assert(value.isFinite && Math.abs(value) <= 1e-12)
      case other => fail(s"expected typed perfect-effect denominator degeneracy, got $other")

  test("held-out signal recovery dominates deterministic null calibration without response leakage"):
    val nullScores = Vector.newBuilder[Double]
    val signalScores = Vector.newBuilder[Double]
    var seed = 0
    while seed < 12 do
      val noise = simulatedResponses(seed, signalAmplitude = 0.0)
      val signal = simulatedResponses(seed, signalAmplitude = 1.5)
      nullScores += onlySuccess(analyze(noise, R.design, 1.0, ridge(0.05))).meanHeldOutRoot
      signalScores += onlySuccess(analyze(signal, R.design, 1.0, ridge(0.05))).meanHeldOutRoot
      seed += 1
    val nullMean = nullScores.result().sum / 12.0
    val signalMean = signalScores.result().sum / 12.0

    assert(nullMean.isFinite && nullMean >= 0.0)
    assert(signalMean.isFinite)
    assert(signalMean > nullMean * 5.0 + 1.0)

  test("large time axes still produce only feature and design sufficient statistics"):
    val timepoints = 8192
    val design = Matrix.tabulate(timepoints, 3): (row, col) =>
      col match
        case 0 => 1.0
        case 1 => if (row / 8) % 2 == 0 then -1.0 else 1.0
        case _ => (2.0 * row.toDouble / (timepoints - 1).toDouble) - 1.0
    val response = Matrix.tabulate(timepoints, 3): (row, feature) =>
      Math.sin((row + 1).toDouble * (feature + 2).toDouble * 0.017) +
        0.1 * Math.cos((row + 3).toDouble * (feature + 1).toDouble * 0.031)
    val geometry = compileGeometry(design, 1.0)
    val moments = CanonicalMoments
      .accumulate(ResponseBlock.unsafe(response), geometry, Array(0, 1, 2))
      .toOption
      .get

    assertEquals((moments.total.rows, moments.total.cols), (3, 3))
    assertEquals((moments.effect.rows, moments.effect.cols), (3, 3))
    assertEquals((moments.residual.rows, moments.residual.cols), (3, 3))
    assertEquals((moments.responseDesign.rows, moments.responseDesign.cols), (3, 3))

  private def analyze(
      responses: Vector[DMat],
      design: DMat,
      contrastScale: Double,
      regularization: ResidualRegularization,
      partitions: Vector[PartitionId] = Vector.empty,
      neuralKeys: Option[Vector[FeatureId]] = None
  ): CanonicalAnalysisTestSupport.CanonicalResult =
    val geometry = compileGeometry(design, contrastScale)
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val ids =
      if partitions.isEmpty then responses.indices.map(index => PartitionId.unsafe(s"run-$index")).toVector
      else partitions
    val keys = neuralKeys.getOrElse:
      Vector.tabulate(responses.head.cols)(index => FeatureId.unsafe(s"feature-$index"))
    val revision = s"acceptance-${responses.length}-${responses.head.rows}-${responses.head.cols}"
    val neural = AxisRef
      .create(
        AxisId.unsafe(s"$revision-neural"),
        AxisPurpose.NeuralFeatures,
        keys,
        CoordinateBasis.unsafe("declared-feature-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("canonical-effect-r-oracle", revision)
      )
      .toOption
      .get
    val runs = responses.indices.map: index =>
      CanonicalRunInput(
        ids(index),
        ResponseBlock.unsafe(responses(index)),
        schedule,
        neural
      ).toOption.get
    val dataset = CanonicalEffectDataset(runs).toOption.get
    CanonicalAnalysisTestSupport.canonical(
      dataset,
      Vector.tabulate(responses.head.cols)(identity),
      regularization,
      revision = revision
    )

  private def compileGeometry(design: DMat, contrastScale: Double): PreparedContrastGeometry =
    val rows = design.rows
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = DesignMatrix.unsafe(design),
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> contrastScale)),
        selectedTimepoints = SelectedTimepointIndices.unsafe((0 until rows).toVector),
        partitions = Vector(RunPartition(0, (0 until rows).toVector, (0 until rows).toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get

  private def ridge(value: Double): ResidualRegularization =
    ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(value))

  private def onlySuccess(result: CanonicalAnalysisTestSupport.CanonicalResult): CanonicalEffectEstimate =
    CanonicalAnalysisTestSupport.success(result)

  private def foldFor(payload: CanonicalEffectEstimate, heldOut: Int): CanonicalEffectFoldEstimate =
    payload.folds
      .find(_.receipt.heldOut == PartitionId.unsafe(s"run-$heldOut"))
      .getOrElse(fail(s"missing held-out fold $heldOut"))

  private def runNumber(partition: PartitionId): Int =
    partition.value.stripPrefix("run-").toInt

  private def assertFoldRootsEqual(
      expected: CanonicalEffectEstimate,
      actual: CanonicalEffectEstimate,
      tolerance: Double
  ): Unit =
    expected.folds.foreach: expectedFold =>
      val actualFold = foldFor(actual, runNumber(expectedFold.receipt.heldOut))
      assertEqualsDouble(actualFold.trainingFit.root.value, expectedFold.trainingFit.root.value, tolerance)
      assertEqualsDouble(actualFold.heldOutRoot, expectedFold.heldOutRoot, tolerance)
    assertEqualsDouble(actual.meanHeldOutRoot, expected.meanHeldOutRoot, tolerance)

  private def reparameterizeNuisance(design: DMat): DMat =
    Matrix.tabulate(design.rows, design.cols): (row, col) =>
      col match
        case 0 => design(row, 0) + 0.4 * design(row, 2)
        case 1 => design(row, 1)
        case _ => -0.3 * design(row, 0) + 1.2 * design(row, 2)

  private def simulatedResponses(seed: Int, signalAmplitude: Double): Vector[DMat] =
    Vector.tabulate(3): run =>
      Matrix.tabulate(R.design.rows, 3): (time, feature) =>
        val phase = (seed + 1).toDouble * (run + 2).toDouble * (time + 1).toDouble * (feature + 3).toDouble
        val noise = Math.sin(phase * 0.173) + 0.5 * Math.cos(phase * 0.097 + feature.toDouble)
        val weight = feature match
          case 0 => 1.0
          case 1 => -0.6
          case _ => 0.35
        noise + signalAmplitude * R.design(time, 1) * weight

  private def permuteRows(matrix: DMat, order: Vector[Int]): DMat =
    Matrix.tabulate(order.length, matrix.cols)((row, col) => matrix(order(row), col))

  private def permuteColumns(matrix: DMat, order: Vector[Int]): DMat =
    Matrix.tabulate(matrix.rows, order.length)((row, col) => matrix(row, order(col)))

  private def mapValues(matrix: DMat, transform: Double => Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)((row, col) => transform(matrix(row, col)))

  private def column(matrix: DMat, index: Int): Vector[Double] =
    Vector.tabulate(matrix.rows)(row => matrix(row, index))

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))
