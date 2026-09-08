package scalafim.fmri.fit

import munit.FunSuite
import scalafim.fmri.fit.fixtures.MixedTrFixedEffectsRFixture as R
import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import scalafim.fmri.design.{FactorLevelRegistry, EmptyCellPolicy}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, FitPlan, FitStrategy}
import gale.linalg.Matrix

class CompactFixedEffectsSuite extends FunSuite:
  private def checked[E,A](value: Either[E,A]): A = value.fold(e => fail(e.toString), identity)
  private def runwise(): RunwiseFmriFitResult =
    val events = DatasetEvents(Vector(
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2")))
    val dataset = FmriDataset.unsafe(backend = InMemoryDatasetBackend(DatasetId("mni-crop-fixed-effects"),
      ImageDMat.fromRows(R.response), NeuroSpace(Vector(2, 1, 1),
        spacing = Some(Vector(2.0, 2.0, 2.0)), origin = Some(Vector(-16.5, -12.5, 1.5)))),
      samplingFrame = SamplingFrame(blockLens = Seq(8, 12), tr = Seq(2.0, 0.8), startTime = Seq(0.0, 0.0), precision = 0.1), events = events)
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = Intercept.Global,
      factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    checked(FitPlanExecutor.fit(FitPlan(model, FitStrategy.RunwiseLeastSquares()))).asInstanceOf[RunwiseFmriFitResult]

  test("public run combination retains compact statistics and agrees with independent correlated mixed-TR R oracle") {
    val source = runwise()
    val result = checked(FixedEffects.combine(source))
    assertEquals(result.residualDegreesOfFreedom.value, R.fixedResidualDf)
    assertEquals(result.voxelIndices, source.voxelIndices)
    for r <- R.fixedCoefficients.indices; v <- 0 until 2 do
      assertEqualsDouble(result.coefficients(r, v), R.fixedCoefficients(r)(v), 1e-7)
      assertEqualsDouble(result.standardErrors(r, v), R.fixedStandardErrors(r)(v), 1e-7)
      val covariance = checked(result.coefficientCovariance.matrixForVoxelPosition(v))
      for c <- R.fixedCoefficients.indices do
        assertEqualsDouble(covariance(r, c), R.fixedCovarianceByVoxel(v)(r)(c), 1e-7)
    assert(result.sufficientStatistics.contributions.forall(_.retainedPrecisionDoubleCount == 11L))
    assertEquals(result.coefficientCovariance.retainedDoubleCount, 22L)
    assert(result.coefficientCovariance.materialize(1).isLeft)
    assertEquals(checked(result.coefficientCovariance.materialize(2)).size, 2)
    val selected = checked(result.inference.selectVoxelPositions(Vector(1, 0)))
    assertEquals(selected.covariance.retainedDoubleCount, 22L)
    assertEqualsDouble(selected.standardErrors(0, 0), result.standardErrors(0, 1), 1e-12)
    val merged = checked(CoefficientCovariance.mergeByVoxel(Vector(selected.covariance, selected.covariance)))
    assertEquals(merged.matrixCount, 4)
    assertEquals(merged.retainedDoubleCount, 44L)
    assertEqualsDouble(merged.unsafeMatrixForVoxelPosition(3)(0, 1), result.coefficientCovariance.unsafeMatrixForVoxelPosition(0)(0, 1), 1e-12)
  }

  test("structured storage grows with voxel scales, preserves off-diagonal precision, and refuses malformed fields") {
    val base = Matrix.tabulate(2, 2)((r, c) => if r == c then 2.0 else 0.5)
    for n <- Vector(17, 4097) do
      val scales = Vector.tabulate(n)(i => 1.0 + i.toDouble / n)
      val precision = checked(CoefficientMatrixStorage.scaled(base, scales))
      val covariance = checked(CoefficientCovariance.fromPrecisionSum(Vector(precision, precision)))
      assertEquals(covariance.retainedDoubleCount, 2L * (4 + n))
      for v <- Vector(0, n / 2, n - 1) do
        val actual = checked(covariance.matrixForVoxelPosition(v))
        val divisor = 2 * scales(v) * (4.0 - 0.25)
        assertEqualsDouble(actual(0, 0), 2.0 / divisor, 1e-12)
        assertEqualsDouble(actual(0, 1), -0.5 / divisor, 1e-12)
      assert(covariance.materialize(n - 1).isLeft)
      assert(covariance.matrixForVoxelPosition(n).isLeft)
    assert(CoefficientMatrixStorage.scaled(base, Vector(0.0)).isLeft)
    assert(CoefficientMatrixStorage.scaled(base, Vector(Double.PositiveInfinity)).isLeft)
    assert(CoefficientMatrixStorage.scaled(Matrix.tabulate(2, 2)((_, _) => Double.MaxValue), Vector(2.0)).isLeft)
    assert(CoefficientCovariance.fromPrecisionSum(Vector(Vector(base), Vector(base, base))).isLeft)
    assert(CoefficientCovariance.fromPrecisionSum(Vector(Vector(Matrix.tabulate(2, 2)((_, _) => 0.0)))).isLeft)
  }

  test("unstructured legacy contributions, disjoint folding and availability rules remain usable") {
    val source = runwise()
    val expected = checked(FixedEffects.combine(source))
    val stats = expected.sufficientStatistics
    val legacy = stats.copy(contributions = stats.contributions.map(c => c.copy(precisionByVoxel = c.precisionByVoxel.toVector)))
    val combined = checked(FixedEffects.combineStatistics(legacy.copy(contributions = Vector(legacy.contributions.head)), legacy.copy(contributions = Vector(legacy.contributions.last)), expected.columnNames, expected.timepoints, expected.summary, expected.preparationProvenance))
    for r <- 0 until combined.coefficients.predictors; v <- 0 until 2 do
      assertEqualsDouble(combined.coefficients(r, v), expected.coefficients(r, v), 1e-12)
    val a = stats.copy(contributions = Vector(stats.contributions.head))
    val b = stats.copy(contributions = Vector(stats.contributions.last))
    assertEquals(checked(a.combine(b)).runIndices, stats.runIndices)
    assert(a.combine(a).isLeft)
    val badRun = source.runs.head.copy(residualVariance = gale.linalg.DVec.fromSeq(Vector(0.0, source.runs.head.residualVariance(1))))
    val omitted = checked(FixedEffects.combine(source.copy(runs = source.runs.updated(0, badRun))))
    assertEquals(omitted.voxelIndices, Vector(source.voxelIndices(1)))
  }


  test("compact concatenation refuses logical voxel count overflow without dense materialization") {
    var covariance = checked(CoefficientCovariance.voxelwise(Vector(Matrix.eye(1))))
    for _ <- 0 until 30 do
      covariance = checked(CoefficientCovariance.mergeByVoxel(Vector(covariance, covariance)))
    assertEquals(covariance.matrixCount, 1 << 30)
    assert(CoefficientCovariance.mergeByVoxel(Vector(covariance, covariance)).isLeft)
    assert(covariance.materialize(1024).isLeft)
    assertEqualsDouble(checked(covariance.matrixForVoxelPosition((1 << 30) - 1))(0, 0), 1.0, 0.0)
  }

/** Explicit stress probe, not part of the default unit-test run.
  * This exercises covariance storage at a full MNI common-mask cardinality;
  * it does not synthesize image data or establish whole-cohort fit memory.
  */
object CompactCovarianceScaleProbe:
  def main(args: Array[String]): Unit =
    require(args.isEmpty, "this probe has fixed, reviewable dimensions")
    val voxels = 206086
    val predictors = 17
    val runs = 3
    val units = 4
    val start = System.nanoTime()
    def checked[A](value: Either[FitError, A]): A =
      value.fold(e => throw IllegalStateException(e.message), identity)
    def scale(run: Int, voxel: Int, unit: Int): Double =
      1.0 + (voxel % 101).toDouble / 101.0 * (run + 1) + unit * 0.1
    val covariances = Vector.tabulate(units) { unit =>
      val fields = Vector.tabulate(runs) { run =>
        val a = 1.0 + run * 0.25
        val b = 0.1 * (run + 1)
        val base = Matrix.tabulate(predictors, predictors)((r, c) => (if r == c then a else 0.0) + b)
        checked(CoefficientMatrixStorage.scaled(base, Vector.tabulate(voxels)(v => scale(run, v, unit))))
      }
      checked(CoefficientCovariance.fromPrecisionSum(fields))
    }
    val expectedDoubles = units.toLong * runs * (predictors.toLong * predictors + voxels)
    val retainedDoubles = covariances.map(_.retainedDoubleCount).sum
    require(retainedDoubles == expectedDoubles, s"unexpected retained storage: $retainedDoubles")
    var maximumError = 0.0
    for unit <- 0 until units; v <- Vector(0, voxels / 2, voxels - 1) do
      val a = (0 until runs).map(run => (1.0 + run * 0.25) * scale(run, v, unit)).sum
      val b = (0 until runs).map(run => 0.1 * (run + 1) * scale(run, v, unit)).sum
      // Independent analytic inverse of a I + b 11': a^-1 I - b/[a(a + Pb)] 11'.
      val actual = checked(covariances(unit).matrixForVoxelPosition(v))
      for r <- 0 until predictors; c <- 0 until predictors do
        val expected = (if r == c then 1.0 / a else 0.0) - b / (a * (a + predictors * b))
        val error = math.abs(actual(r, c) - expected)
        require(error.isFinite && error < 1e-12, s"analytic inverse mismatch at $unit/$v/$r/$c: $error")
        maximumError = math.max(maximumError, error)
    require(covariances.forall(_.materialize(voxels - 1).isLeft), "materialization limit was bypassed")
    val seconds = (System.nanoTime() - start) / 1e9
    println(s"""{"probe":"compact-covariance-scale","units":$units,"runsPerUnit":$runs,"voxels":$voxels,"predictors":$predictors,"retainedDoubles":$retainedDoubles,"retainedNumericBytes":${retainedDoubles * 8},"maximumAnalyticAbsoluteError":$maximumError,"elapsedSeconds":$seconds,"scope":"covariance fields only; excludes full fit and group analysis"}""")
