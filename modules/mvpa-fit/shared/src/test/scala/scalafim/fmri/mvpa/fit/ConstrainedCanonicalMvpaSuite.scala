package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.contract.RequestedOptimizationClaim
import multivar.family.canonical.{CanonicalEffectReferenceFixtures as R, ResidualRegularization, TraceRidgeFraction}
import multivar.optimization.FeasibleSetKind
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitConfig

class ConstrainedCanonicalMvpaSuite extends munit.FunSuite:
  private val model = NonnegativeCanonicalModelSpec(ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(R.ridgeFraction)))
  private val budget = NativeCanonicalFixtures.budget

  test("native nonnegative assessments agree with the independent base-R active-face oracle"):
    val assessment = assess(R.runs.map(_.response))
    assessment.folds.zip(ConstrainedCanonicalReferenceFixtures.folds).foreach: (actual, expected) =>
      assertEqualsDouble(actual.training.fit.root.value, expected.trainingRoot, 1e-7)
      assertEqualsDouble(actual.training.fit.regularization.ridgeAmount, expected.ridge, 1e-10)
      assertEqualsDouble(actual.heldOutRoot, expected.heldOutRoot, 1e-7)
      (0 until actual.training.fit.direction.length).foreach: coordinate =>
        assertEqualsDouble(actual.training.fit.direction(coordinate), expected.direction(coordinate), 1e-7)
        assert(actual.training.fit.direction(coordinate) >= -1e-12)
      assertEquals(actual.training.fit.programFit.program.constraints.map(_.feasibleSet), Vector(FeasibleSetKind.NonnegativeOrthant))
      assertEquals(actual.training.fit.programFit.program.resultSemantics.requestedClaim, RequestedOptimizationClaim.Stationary)
      assertEquals(actual.receipt.execution, CanonicalMomentExecution.RunwiseSufficientStatistics)
    assertEqualsDouble(assessment.meanHeldOutRoot, ConstrainedCanonicalReferenceFixtures.meanHeldOutRoot, 1e-7)
    assertEqualsDouble(assessment.rootToCorrelation, ConstrainedCanonicalReferenceFixtures.rootToCorrelationOfMean, 1e-9)

  test("held-out response perturbation cannot change its frozen native training fit"):
    val baseline = assess(R.runs.map(_.response)).folds.head
    val changed = assess(R.runs.map(_.response).updated(0, perturb(R.runs.head.response))).folds.head
    assertEqualsDouble(baseline.training.fit.root.value, changed.training.fit.root.value, 0.0)
    (0 until baseline.training.fit.direction.length).foreach(i => assertEqualsDouble(baseline.training.fit.direction(i), changed.training.fit.direction(i), 0.0))
    assertEquals(baseline.training.receipt.momentIdentity, changed.training.receipt.momentIdentity)
    assertNotEquals(baseline.heldOutRoot, changed.heldOutRoot)

  test("semantic feature-key permutations preserve the estimand while rotations need not"):
    val baseline = assess(R.runs.map(_.response))
    val permutation = Vector(2, 0, 1)
    val permuted = assess(R.runs.map(run => permuteColumns(run.response, permutation)), permutation.map(i => s"neural-$i"))
    val rotation = fromRows(Vector(Vector(0.8, -0.6, 0.0), Vector(0.6, 0.8, 0.0), Vector(0.0, 0.0, 1.0)))
    val rotated = assess(R.runs.map(run => run.response * rotation))
    baseline.folds.zip(permuted.folds).foreach: (left, right) =>
      assertEqualsDouble(left.training.fit.root.value, right.training.fit.root.value, 1e-7)
      assertEqualsDouble(left.heldOutRoot, right.heldOutRoot, 1e-7)
    assert(math.abs(baseline.meanHeldOutRoot - rotated.meanHeldOutRoot) > 1e-3)

  test("native typed artifacts expose model selection and fold receipts"):
    val assessment = assess(R.runs.map(_.response))
    assertEquals(model.selection, ConstrainedCanonicalSelection.FixedBeforeFolds)
    assert(assessment.folds.forall(_.receipt.trainingRuns.length == 2))
    assert(assessment.folds.forall(_.training.receipt.temporalPreparation.length == 2))

  private def assess(responses: Vector[DMat], featureKeys: Vector[String] = Vector.empty): NonnegativeCanonicalAssessment[?] =
    NativeCanonicalFixtures.right(CanonicalGlobal.assessNonnegative(native(responses, featureKeys).source, model, budget))
  private def native(responses: Vector[DMat], keys: Vector[String]) =
    val schedule = NativeCanonicalFixtures.right(CanonicalGeometrySchedule.stable(geometry))
    NativeCanonicalFixtures.contrast(responses, Vector.fill(responses.length)(schedule), featureKeys = keys)
  private def geometry: PreparedContrastGeometry =
    ResponsePreparationPlan.fromConfig(FitConfig()).prepareContrast(DesignMatrix.unsafe(R.design), Vector("intercept", "task", "drift"),
      TContrast("task", Map("task" -> 1.0)), SelectedTimepointIndices.unsafe((0 until R.design.rows).toVector),
      Vector(RunPartition(0, (0 until R.design.rows).toVector, (0 until R.design.rows).toVector)), TemporalNuisanceRank.unsafe(2)).toOption.get
  private def perturb(value: DMat) = Matrix.tabulate(value.rows, value.cols)((r, c) => value(r, c) + (if c == 0 then 0.4 * ((r % 3) - 1).toDouble else 0.0))
  private def permuteColumns(value: DMat, p: Vector[Int]) = Matrix.tabulate(value.rows, p.length)((r, c) => value(r, p(c)))
  private def fromRows(rows: Vector[Vector[Double]]) = Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))
