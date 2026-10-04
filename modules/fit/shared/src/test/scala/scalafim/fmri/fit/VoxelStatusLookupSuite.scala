package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.fmri.model.{FitEngine, FitSummary}

private[fit] object VoxelStatusLookupFixture:
  def dense(voxels: Int): DenseFmriFitResult =
    val design = DesignMatrix.unsafe(Matrix.tabulate(4, 1)((_, _) => 1.0))
    val response = ResponseBlock.unsafe(Matrix.tabulate(4, voxels)((row, voxel) => row.toDouble + voxel))
    val fit = Ols.unsafeFit(design, response)
    DenseFmriFitResult(
      coefficients = fit.coefficients,
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All, fit.standardErrors, fit.coefficientCovariance,
        fit.residualVariance, fit.residualDegreesOfFreedom
      ),
      residualVariance = fit.residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      columnNames = Vector("intercept"),
      voxelIndices = Vector.tabulate(voxels)(i => 10 + 2 * i),
      timepoints = Vector(0, 1, 2, 3),
      engine = FitEngine.OrdinaryLeastSquares,
      summary = FitSummary(FitEngine.OrdinaryLeastSquares, 4, 1, voxels, false, false)
    )

class VoxelStatusLookupSuite extends munit.FunSuite:
  test("point lookup preserves default status, source identity, exclusions and unknown ids"):
    for count <- Vector(2, 4096) do
      val result = VoxelStatusLookupFixture.dense(count).copy(
        fitExclusions = Vector(VoxelInferenceExclusion(3, VoxelFitStatus.NonFinite))
      )
      for position <- Vector(0, count / 2, count - 1) do
        assertEquals(result.voxelStatus(result.voxelIndices(position)), Some(VoxelFitStatus.Estimable))
      assertEquals(result.voxelStatus(3), Some(VoxelFitStatus.NonFinite))
      for unknown <- Vector(-1, 0, 11, Int.MaxValue) do
        assertEquals(result.voxelStatus(unknown), None)
      assertEquals(result.resolvedVoxelStatuses, Vector.fill(count)(VoxelFitStatus.Estimable))

  test("explicit statuses are returned unchanged and malformed vectors remain rejected"):
    val result = VoxelStatusLookupFixture.dense(VoxelFitStatus.values.length)
    val statuses = VoxelFitStatus.values.toVector
    val explicit = result.copy(voxelStatuses = Some(statuses))
    result.voxelIndices.zip(statuses).foreach: (id, status) =>
      assertEquals(explicit.voxelStatus(id), Some(status))
    intercept[IllegalArgumentException]:
      result.copy(voxelStatuses = Some(Vector.empty))
