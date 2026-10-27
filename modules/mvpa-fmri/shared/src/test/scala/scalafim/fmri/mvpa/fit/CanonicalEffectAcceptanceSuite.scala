package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.family.canonical.{CanonicalEffectReferenceFixtures as R, CanonicalEffectSolution, ResidualRegularization, TraceRidgeFraction}
import scalafim.dataset.RunId
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitConfig

class CanonicalEffectAcceptanceSuite extends munit.FunSuite:
  private val budget = NativeCanonicalFixtures.budget

  test("the native leave-one-run-out artifact agrees with the committed base-R oracle"):
    val assessment = assess(R.runs.map(_.response))
    R.folds.foreach: expected =>
      val actual = foldFor(assessment, expected.heldOutRun)
      assertEqualsDouble(actual.training.fit.root.value, expected.trainingRoot, 1e-8)
      assertEqualsDouble(actual.training.fit.regularization.ridgeAmount, expected.ridge, 1e-10)
      assertEqualsDouble(actual.heldOutRoot, expected.heldOutRoot, 1e-8)
      actual.training.fit.solution match
        case CanonicalEffectSolution.Simple(direction, _) =>
          (0 until direction.length).foreach(index => assertEqualsDouble(direction(index), expected.direction(index), 1e-8))
        case other => fail(s"expected an identifiable R-oracle direction, got $other")
      assertEquals(actual.receipt.execution, CanonicalMomentExecution.RunwiseSufficientStatistics)
      assertEquals(actual.receipt.trainingRuns.length, 2)
      assertEquals(actual.receipt.temporalPreparation.length, 3)
    assertEqualsDouble(assessment.meanHeldOutRoot, R.meanHeldOutRoot, 1e-8)
    assertEqualsDouble(assessment.rootToCorrelation, R.rootToCorrelationOfMean, 1e-10)

  test("run and row order, contrast scale, and nuisance basis do not change the estimand"):
    val baseline = assess(R.runs.map(_.response))
    val order = Vector(2, 0, 1)
    assertFoldRootsEqual(baseline, assess(order.map(R.runs(_).response), runIds = order.map(i => RunId(s"run-$i"))), 1e-8)
    val rowOrder = Vector(5, 0, 7, 2, 1, 6, 3, 4)
    assertFoldRootsEqual(baseline, assess(R.runs.map(run => permuteRows(run.response, rowOrder)), permuteRows(R.design, rowOrder)), 1e-8)
    assertFoldRootsEqual(baseline, assess(R.runs.map(_.response), contrastScale = -3.0), 1e-8)
    assertFoldRootsEqual(baseline, assess(R.runs.map(_.response), reparameterizeNuisance(R.design)), 1e-8)

  test("feature rotations, scale, and semantic feature-key permutations obey covariance laws"):
    val responses = R.runs.map(_.response)
    val baseline = assess(responses)
    val rotation = fromRows(Vector(Vector(0.8, -0.6, 0.0), Vector(0.6, 0.8, 0.0), Vector(0.0, 0.0, 1.0)))
    assertFoldRootsEqual(baseline, assess(responses.map(_ * rotation)), 1e-7)
    val scale = 7.0
    val scaled = assess(responses.map(mapValues(_, _ * scale)))
    assertFoldRootsEqual(baseline, scaled, 1e-7)
    baseline.folds.foreach: fold =>
      val scaledFold = foldFor(scaled, fold.receipt.heldOutRun.value.stripPrefix("run-").toInt)
      assertEqualsDouble(scaledFold.training.fit.regularization.ridgeAmount, fold.training.fit.regularization.ridgeAmount * scale * scale, 1e-8)
    val permutation = Vector(2, 0, 1)
    assertFoldRootsEqual(baseline, assess(responses.map(permuteColumns(_, permutation)), featureKeys = permutation.map(i => s"neural-$i")), 1e-8)

  test("zero effects remain zero and perfect held-out effects return a native typed temporal error"):
    val residual = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val zero = assess(Vector.tabulate(3)(run => fromRows(residual.map(value => Vector(value * (run + 1).toDouble)))), regularization = ridge(0.02))
    assert(zero.meanHeldOutRoot <= 1e-24)
    val task = Vector.tabulate(R.design.rows)(row => R.design(row, 1))
    val noisy = fromRows(task.zip(residual).map((signal, noise) => Vector(signal + 0.2 * noise)))
    val exact = fromRows(task.map(value => Vector(value)))
    CanonicalGlobal.assess(native(Vector(exact, noisy), R.design).source, ridge(0.02), budget) match
      case Left(CanonicalArtifactError.Temporal(OneShotMvpaError.NonPositiveHeldOutDenominator(run, value))) =>
        assertEquals(run, RunId("run-0")); assert(value.isFinite && math.abs(value) <= 1e-12)
      case other => fail(s"expected typed perfect-effect denominator degeneracy, got $other")

  test("native typed artifacts retain global fit and fold assessment receipts"):
    val packed = native(R.runs.map(_.response), R.design)
    val fit = NativeCanonicalFixtures.right(CanonicalGlobal.fit(packed.source, NativeCanonicalFixtures.right(packed.source.selectRuns(Vector(RunId("run-0"), RunId("run-1"), RunId("run-2")))), ridge(R.ridgeFraction), budget))
    val assessment = NativeCanonicalFixtures.right(CanonicalGlobal.assess(packed.source, ridge(R.ridgeFraction), budget))
    assertEquals(fit.receipt.trainingRuns, Vector(RunId("run-0"), RunId("run-1"), RunId("run-2")))
    assertEquals(fit.neuralAxis, packed.source.neuralAxis)
    assert(assessment.folds.forall(_.receipt.trainingRuns.length == 2))

  test("held-out signal recovery dominates deterministic null calibration without response leakage"):
    val nullScores = Vector.newBuilder[Double]
    val signalScores = Vector.newBuilder[Double]
    var seed = 0
    while seed < 12 do
      nullScores += assess(simulatedResponses(seed, signalAmplitude = 0.0), regularization = ridge(0.05)).meanHeldOutRoot
      signalScores += assess(simulatedResponses(seed, signalAmplitude = 1.5), regularization = ridge(0.05)).meanHeldOutRoot
      seed += 1
    val nullMean = nullScores.result().sum / 12.0
    val signalMean = signalScores.result().sum / 12.0
    assert(nullMean.isFinite && nullMean >= 0.0)
    assert(signalMean.isFinite)
    assert(signalMean > nullMean * 5.0 + 1.0)

  test("large time axes still produce only feature and design sufficient statistics"):
    val timepoints = 8192
    val design = Matrix.tabulate(timepoints, 3): (row, column) =>
      column match
        case 0 => 1.0
        case 1 => if (row / 8) % 2 == 0 then -1.0 else 1.0
        case _ => (2.0 * row.toDouble / (timepoints - 1).toDouble) - 1.0
    val response = Matrix.tabulate(timepoints, 3): (row, feature) =>
      math.sin((row + 1).toDouble * (feature + 2).toDouble * 0.017) + 0.1 * math.cos((row + 3).toDouble * (feature + 1).toDouble * 0.031)
    val packed = native(Vector(response, response, response), design)
    NativeCanonicalFixtures.right(CanonicalGlobal.assess(packed.source, ridge(0.05), budget))
    val moments = CanonicalMoments.accumulate(ResponseBlock.unsafe(response), geometry(design, 1.0), Array(0, 1, 2)).toOption.get
    assertEquals((moments.total.rows, moments.total.cols), (3, 3))
    assertEquals((moments.effect.rows, moments.effect.cols), (3, 3))
    assertEquals((moments.residual.rows, moments.residual.cols), (3, 3))
    assertEquals((moments.responseDesign.rows, moments.responseDesign.cols), (3, 3))

  private def assess(responses: Vector[DMat], design: DMat = R.design, contrastScale: Double = 1.0,
      runIds: Vector[RunId] = Vector.empty, featureKeys: Vector[String] = Vector.empty,
      regularization: ResidualRegularization = ridge(R.ridgeFraction)): CanonicalAssessment[?] =
    NativeCanonicalFixtures.right(CanonicalGlobal.assess(native(responses, design, contrastScale, runIds, featureKeys).source, regularization, budget))

  private def native(responses: Vector[DMat], design: DMat, contrastScale: Double = 1.0,
      runIds: Vector[RunId] = Vector.empty, featureKeys: Vector[String] = Vector.empty) =
    val schedule = NativeCanonicalFixtures.right(CanonicalGeometrySchedule.stable(geometry(design, contrastScale)))
    NativeCanonicalFixtures.contrast(responses, Vector.fill(responses.length)(schedule), runIds, featureKeys)

  private def geometry(design: DMat, scale: Double): PreparedContrastGeometry =
    ResponsePreparationPlan.fromConfig(FitConfig()).prepareContrast(DesignMatrix.unsafe(design), Vector("intercept", "task", "drift"),
      TContrast("task", Map("task" -> scale)), SelectedTimepointIndices.unsafe((0 until design.rows).toVector),
      Vector(RunPartition(0, (0 until design.rows).toVector, (0 until design.rows).toVector)), TemporalNuisanceRank.unsafe(2)).toOption.get

  private def ridge(value: Double) = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(value))
  private def foldFor(assessment: CanonicalAssessment[?], held: Int) = assessment.folds.find(_.receipt.heldOutRun == RunId(s"run-$held")).getOrElse(fail(s"missing fold $held"))
  private def assertFoldRootsEqual(left: CanonicalAssessment[?], right: CanonicalAssessment[?], tolerance: Double): Unit =
    left.folds.foreach: fold =>
      val other = foldFor(right, fold.receipt.heldOutRun.value.stripPrefix("run-").toInt)
      assertEqualsDouble(fold.training.fit.root.value, other.training.fit.root.value, tolerance)
      assertEqualsDouble(fold.heldOutRoot, other.heldOutRoot, tolerance)
    assertEqualsDouble(left.meanHeldOutRoot, right.meanHeldOutRoot, tolerance)
  private def reparameterizeNuisance(d: DMat): DMat =
    Matrix.tabulate(d.rows, d.cols): (r, c) =>
      c match
        case 0 => d(r, 0) + 0.4 * d(r, 2)
        case 1 => d(r, 1)
        case _ => -0.3 * d(r, 0) + 1.2 * d(r, 2)
  private def permuteRows(m: DMat, order: Vector[Int]) = Matrix.tabulate(order.length, m.cols)((r, c) => m(order(r), c))
  private def permuteColumns(m: DMat, order: Vector[Int]) = Matrix.tabulate(m.rows, order.length)((r, c) => m(r, order(c)))
  private def mapValues(m: DMat, f: Double => Double) = Matrix.tabulate(m.rows, m.cols)((r, c) => f(m(r, c)))
  private def simulatedResponses(seed: Int, signalAmplitude: Double): Vector[DMat] =
    Vector.tabulate(3): run =>
      Matrix.tabulate(R.design.rows, 3): (time, feature) =>
        val phase = (seed + 1).toDouble * (run + 2).toDouble * (time + 1).toDouble * (feature + 3).toDouble
        val noise = math.sin(phase * 0.173) + 0.5 * math.cos(phase * 0.097 + feature.toDouble)
        val weight = feature match
          case 0 => 1.0
          case 1 => -0.6
          case _ => 0.35
        noise + signalAmplitude * R.design(time, 1) * weight
  private def fromRows(rows: Seq[Seq[Double]]) = Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))
