package scalafim.fmri.fit

import scalafim.fmri.fit.fixtures.ReducedRankGlsFmriregFixtures
import scalafim.fmri.model.{ArCoefficientSpec, AutocorrelationConfig, FitEngine, ReducedRankComponentSpec, ReducedRankGlsConfig, ReducedRankInferencePolicy}
import gale.linalg.DMat

class VoxelwiseReducedRankResultSuite extends munit.FunSuite:
  test("estimates-only voxelwise reduced-rank blocks merge and refuse nominal contrasts") {
    val design = DesignMatrix.unsafe(ReducedRankGlsFmriregFixtures.design)
    val response = ResponseBlock.unsafe(ReducedRankGlsFmriregFixtures.response)
    val rows = (0 until response.timepoints).toVector
    val partitions = Vector(RunPartition(0, rows, rows))
    val config = ReducedRankGlsConfig.unsafe(
      components = ReducedRankComponentSpec.unsafeFixed(1),
      autocorrelation = AutocorrelationConfig.unsafe(
        order = 1,
        iterations = 0,
        coefficients = ArCoefficientSpec.Rho(0.0)
      ),
      inference = ReducedRankInferencePolicy.EstimatesOnly
    )
    val prepared = ReducedRankGlsPrepared
      .prepare(design, response, partitions, config, selectedVoxelIndices = Vector(0, 1))
      .fold(error => fail(error.message), identity)
    val input = FitBlockInput(design, response, voxelIndices = Vector(0, 1), timepoints = rows, partitions = partitions)
    val block = prepared.fitBlock(input).fold(error => fail(error.message), identity) match
      case value: VoxelwiseReducedRankFitBlockResult => value
      case other => fail(s"expected voxelwise reduced-rank block, got ${other.getClass.getSimpleName}")

    val merged = VoxelwiseReducedRankFitBlockResult.merge(Vector(
      block.copy(estimate = block.estimate.selectVoxelPositions(Vector(0)).fold(error => fail(error.message), identity), voxelIndices = Vector(0), voxelStatuses = block.voxelStatuses.map(_.take(1))),
      block.copy(estimate = block.estimate.selectVoxelPositions(Vector(1)).fold(error => fail(error.message), identity), voxelIndices = Vector(1), voxelStatuses = block.voxelStatuses.map(_.drop(1)))
    )).fold(error => fail(error.message), identity)
    assertEquals(merged.voxelIndices, Vector(0, 1))
    assert(VoxelwiseReducedRankFitBlockResult.merge(Vector(block, block)).left.toOption.exists(_.message.contains("disjoint voxel identities")))
    assertEquals(merged.uncertainty, VoxelwiseReducedRankUncertainty.Unavailable)
    assertMatrixEquals(merged.coefficients.value, block.coefficients.value)

    val result = VoxelwiseReducedRankFmriFitResult(
      estimate = merged.estimate,
      columnNames = Vector("task_a", "task_b"),
      voxelIndices = merged.voxelIndices,
      timepoints = merged.timepoints,
      engine = FitEngine.ReducedRankGls,
      summary = scalafim.fmri.model.FitSummary(
        FitEngine.ReducedRankGls,
        rows.length,
        predictors = 2,
        voxels = 2,
        robust = false,
        autocorrelated = true
      )
    )
    assertEquals(result.coefficient("task_a", 0), Some(block.coefficients(0, 0)))
    assert(result.inferenceReady.left.toOption.exists(_.isInstanceOf[FitError.UnsupportedEngine]))
    assert(TContrast("task_a", Map("task_a" -> 1.0)).evaluate(result).isLeft)
    assert(FContrast("task_a", Vector(Map("task_a" -> 1.0))).evaluate(result).isLeft)
  }

  private def assertMatrixEquals(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), 1e-12)
        col += 1
      row += 1
