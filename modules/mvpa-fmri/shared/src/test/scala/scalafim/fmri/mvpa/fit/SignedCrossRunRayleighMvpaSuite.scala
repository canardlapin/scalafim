package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.family.canonical.{CanonicalEffectSolution, ResidualRegularization, TraceRidgeFraction}
import scalafim.dataset.RunId
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitConfig

class SignedCrossRunRayleighMvpaSuite extends munit.FunSuite:
  import SignedCrossRunRayleighReferenceFixtures as R
  private val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
  private val budget = NativeCanonicalFixtures.budget

  test("native signed assessments match the independent dense base-R cross-run fixture"):
    val assessment = assess(R.responses)
    assertEquals(assessment.folds.length, 3)
    assessment.folds.zipWithIndex.foreach: (fold, index) =>
      assertEqualsDouble(fold.numerator, R.numerators(index), 1e-8)
      assertEqualsDouble(fold.denominator, R.denominators(index), 1e-10)
      assertEqualsDouble(fold.statistic.value, R.statistics(index), 1e-8)
      assertEqualsDouble(fold.training.fit.regularization.ridgeAmount, R.ridgeAmounts(index), 1e-11)
      fold.training.fit.solution match
        case CanonicalEffectSolution.Simple(direction, _) => (0 until direction.length).foreach(i => assertEqualsDouble(direction(i), R.directions(index)(i), 1e-7))
        case other => fail(s"expected a simple direction, got $other")
      assertEquals(fold.receipt.estimator, SignedCrossRunEstimator.FrozenTrainingDirection)
      assertEquals(fold.receipt.orientation, SignedCrossRunOrientation.AgreementSign)
      assertEquals(fold.receipt.exchangeability, SignedCrossRunExchangeability.RunWiseContrastSignFlip)
      assertEquals(fold.receipt.canonical.trainingRuns.length, 2)
      assertEquals(fold.receipt.canonical.temporalPreparation.length, 3)
    assertEqualsDouble(assessment.meanStatistic.value, R.meanStatistic, 1e-8)

  test("a held-out run sign flip reverses its signed fold without changing its frozen native fit"):
    val ordinary = assess(R.responses)
    val flipped = assess(R.responses.updated(0, scaleRows(R.responses.head, -1.0)))
    val ordinaryFold = foldFor(ordinary, RunId("run-0"))
    val flippedFold = foldFor(flipped, RunId("run-0"))
    assertEqualsDouble(flippedFold.statistic.value, -ordinaryFold.statistic.value, 1e-9)
    assertEqualsDouble(flippedFold.numerator, -ordinaryFold.numerator, 1e-9)
    assertEqualsDouble(flippedFold.denominator, ordinaryFold.denominator, 1e-12)
    assertEquals(flippedFold.training.receipt.momentIdentity, ordinaryFold.training.receipt.momentIdentity)
    assertMatrixClose(NativeCanonicalFixtures.right(flippedFold.training.fit.functionalFrame.weights.toDense), NativeCanonicalFixtures.right(ordinaryFold.training.fit.functionalFrame.weights.toDense), 0.0)

  test("global response sign, common scale, contrast scale, and run order obey their invariance laws"):
    val reference = assess(R.responses)
    val globalSign = assess(R.responses.map(scaleRows(_, -1.0)))
    val commonScale = assess(R.responses.map(scaleRows(_, 7.5)))
    val contrastScale = assess(R.responses, contrastWeight = -4.0)
    val order = Vector(2, 0, 1)
    val reordered = assess(order.map(R.responses), runIds = order.map(i => RunId(s"run-$i")))
    assertEqualsDouble(globalSign.meanStatistic.value, reference.meanStatistic.value, 1e-9)
    assertEqualsDouble(commonScale.meanStatistic.value, reference.meanStatistic.value, 1e-8)
    assertEqualsDouble(contrastScale.meanStatistic.value, reference.meanStatistic.value, 1e-8)
    reference.folds.foreach: fold =>
      val id = fold.receipt.canonical.heldOutRun
      assertEqualsDouble(foldFor(globalSign, id).statistic.value, fold.statistic.value, 1e-9)
      assertEqualsDouble(foldFor(commonScale, id).statistic.value, fold.statistic.value, 1e-8)
      assertEqualsDouble(foldFor(contrastScale, id).statistic.value, fold.statistic.value, 1e-8)
      assertEqualsDouble(foldFor(reordered, id).statistic.value, fold.statistic.value, 1e-8)

  test("non-finite values and canonical degeneracy return typed native errors"):
    SignedCrossRunRayleigh(Double.NaN).left.toOption match
      case Some(OneShotMvpaError.InvalidSignedCrossRunValue(value)) => assert(value.isNaN)
      case other => fail(s"expected typed non-finite value, got $other")
    val exact = Vector.tabulate(3): run =>
      R.design.map(row => Vector(row(0) + (run + 1.0) * row(1), 2.0 * row(0) - row(1), row(2), row(0) + row(1) + row(2)))
    CanonicalGlobal.assessSigned(native(exact).source, regularization, budget) match
      case Left(CanonicalArtifactError.MultivarFailure(error)) => assert(error.message.nonEmpty)
      case other => fail(s"expected typed canonical degeneracy, got $other")

  private def assess(rows: Vector[Vector[Vector[Double]]], contrastWeight: Double = 1.0,
      runIds: Vector[RunId] = Vector(RunId("run-0"), RunId("run-1"), RunId("run-2"))): SignedCanonicalAssessment[?] =
    NativeCanonicalFixtures.right(CanonicalGlobal.assessSigned(native(rows, contrastWeight, runIds).source, regularization, budget))
  private def native(rows: Vector[Vector[Vector[Double]]], contrastWeight: Double = 1.0,
      ids: Vector[RunId] = Vector(RunId("run-0"), RunId("run-1"), RunId("run-2"))) =
    val schedule = NativeCanonicalFixtures.right(CanonicalGeometrySchedule.stable(geometry(contrastWeight)))
    NativeCanonicalFixtures.contrast(rows.map(fromRows), Vector.fill(rows.length)(schedule), ids)
  private def geometry(weight: Double): PreparedContrastGeometry =
    ResponsePreparationPlan.fromConfig(FitConfig()).prepareContrast(DesignMatrix.unsafe(fromRows(R.design)), Vector("intercept", "task", "drift"),
      TContrast("task", Map("task" -> weight)), SelectedTimepointIndices.unsafe(R.design.indices.toVector),
      Vector(RunPartition(0, R.design.indices.toVector, R.design.indices.toVector)), TemporalNuisanceRank.unsafe(2)).toOption.get
  private def foldFor(assessment: SignedCanonicalAssessment[?], id: RunId) = assessment.folds.find(_.receipt.canonical.heldOutRun == id).getOrElse(fail(s"missing fold for ${id.value}"))
  private def scaleRows(rows: Vector[Vector[Double]], scale: Double) = rows.map(_.map(_ * scale))
  private def fromRows(rows: Seq[Seq[Double]]) = Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))
  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    (0 until actual.rows).foreach(r => (0 until actual.cols).foreach(c => assertEqualsDouble(actual(r, c), expected(r, c), tolerance)))
