package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.family.canonical.ResidualRegularization
import scalafim.dataset.RunId
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitConfig

class ManovaMvpaSuite extends munit.FunSuite:
  private val budget = NativeCanonicalFixtures.budget
  private val ordinaryContrast = FContrast("a-and-b", Vector(Map("a" -> 1.0), Map("b" -> 1.0)))
  private val changedContrast = FContrast("changed-basis", Vector(Map("a" -> 1.0, "b" -> 1.0), Map("a" -> 2.0, "b" -> -1.0)))

  test("native MANOVA assessment matches the independent dense base-R fold fixture"):
    val assessment = assess(ordinaryContrast)
    assessment.folds.zip(ManovaReferenceFixtures.folds).foreach: (actual, expected) =>
      actual.heldOutRoots.values.zip(expected.roots).foreach((root, reference) => assertClose(root.value, reference, 1e-7, 1e-9))
      assertClose(actual.heldOutStatistics.royLargestRoot, expected.roy, 1e-7, 1e-9)
      assertClose(actual.heldOutStatistics.wilksLambda, expected.wilks, 1e-9, 1e-9)
      assertClose(actual.heldOutStatistics.pillaiTrace, expected.pillai, 1e-9, 1e-9)
      assertClose(actual.heldOutStatistics.hotellingLawleyTrace, expected.hotelling, 1e-7, 1e-9)
      assertEquals(actual.receipt.execution, ManovaMomentExecution.RunwiseSufficientStatistics)
      assertEquals(actual.training.fit.programFit.program.objective.label, "maximize-trace")
    val mean = assessment.meanStatistics
    assertClose(mean.royLargestRoot, ManovaReferenceFixtures.mean(0), 1e-7, 1e-9)
    assertClose(mean.wilksLambda, ManovaReferenceFixtures.mean(1), 1e-9, 1e-9)
    assertClose(mean.pillaiTrace, ManovaReferenceFixtures.mean(2), 1e-9, 1e-9)
    assertClose(mean.hotellingLawleyTrace, ManovaReferenceFixtures.mean(3), 1e-7, 1e-9)

  test("invertible contrast-basis changes preserve every native held-out estimand"):
    val ordinary = assess(ordinaryContrast)
    val changed = assess(changedContrast)
    ordinary.folds.zip(changed.folds).foreach: (left, right) =>
      left.heldOutRoots.values.zip(right.heldOutRoots.values).foreach((a, b) => assertClose(a.value, b.value, 1e-7, 1e-9))
      assertClose(left.heldOutStatistics.royLargestRoot, right.heldOutStatistics.royLargestRoot, 1e-7, 1e-9)
      assertClose(left.heldOutStatistics.wilksLambda, right.heldOutStatistics.wilksLambda, 1e-9, 1e-9)
      assertClose(left.heldOutStatistics.pillaiTrace, right.heldOutStatistics.pillaiTrace, 1e-9, 1e-9)
      assertClose(left.heldOutStatistics.hotellingLawleyTrace, right.heldOutStatistics.hotellingLawleyTrace, 1e-7, 1e-9)

  test("held-out perturbations cannot alter the frozen native training spectrum"):
    val before = assess(ordinaryContrast).folds.head
    val responses = ManovaReferenceFixtures.responses.updated(0, perturb(ManovaReferenceFixtures.responses.head))
    val after = assess(ordinaryContrast, responses).folds.head
    before.training.fit.roots.values.zip(after.training.fit.roots.values).foreach((left, right) => assertEqualsDouble(left.value, right.value, 0.0))
    assertEqualsDouble(before.training.fit.programFit.objectiveValue, after.training.fit.programFit.objectiveValue, 0.0)
    assertEquals(before.training.receipt.momentIdentity, after.training.receipt.momentIdentity)
    assertNotEquals(before.heldOutStatistics.hotellingLawleyTrace, after.heldOutStatistics.hotellingLawleyTrace)

  test("native MANOVA folds retain runwise receipts and named training domains"):
    val folds = assess(ordinaryContrast).folds
    assert(folds.forall(_.receipt.trainingRuns.length == 2))
    assert(folds.forall(_.receipt.temporalPreparation.length == 3))
    assertEquals(folds.map(_.receipt.heldOutRun).toSet, Set(RunId("run-1"), RunId("run-2"), RunId("run-3")))

  private def assess(contrast: FContrast, responses: Vector[DMat] = ManovaReferenceFixtures.responses): ManovaAssessment[?] =
    NativeCanonicalFixtures.right(CanonicalGlobal.assessManova(native(contrast, responses).source, ResidualRegularization.Unregularized, budget))
  private def native(contrast: FContrast, responses: Vector[DMat]) =
    val schedule = NativeCanonicalFixtures.right(ManovaGeometrySchedule.stable(geometry(contrast)))
    NativeCanonicalFixtures.manova(responses, Vector.fill(responses.length)(schedule), responses.indices.map(i => RunId(s"run-${i + 1}")).toVector)
  private def geometry(contrast: FContrast): PreparedManovaGeometry =
    ResponsePreparationPlan.fromConfig(FitConfig()).prepareManova(DesignMatrix.unsafe(ManovaReferenceFixtures.design), Vector("intercept", "a", "b", "drift"), contrast,
      SelectedTimepointIndices.unsafe((0 until 10).toVector), Vector(RunPartition(0, (0 until 10).toVector, (0 until 10).toVector)), TemporalNuisanceRank.unsafe(2)).toOption.get
  private def perturb(value: DMat) = Matrix.tabulate(value.rows, value.cols)((r, c) => value(r, c) + (if c == 0 then 0.35 * (r % 3 - 1).toDouble else 0.0))
  private def assertClose(actual: Double, expected: Double, absTol: Double, relTol: Double): Unit =
    assertEqualsDouble(actual, expected, math.max(absTol, relTol * math.max(math.abs(actual), math.abs(expected))))
