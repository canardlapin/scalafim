package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.FmriSeries
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.FmriModel
import scalafim.fmri.model.MissingDataPolicy

private[fit] final case class AdaptedResponseBlock(
    response: ResponseBlock,
    voxelIndices: Vector[Int],
    fitExclusions: Vector[VoxelInferenceExclusion]
):
  require(voxelIndices.length == response.voxels, "adapted response voxel indices must match response columns")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "adapted response")

object MatrixAdapters:
  def fromHrfMatrix(matrix: Mat): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var i = 0
    while i < matrix.data.length do
      out.writeLinear(i, matrix.data(i))
      i += 1
    out.result()

  def fromHrfMatrixRows(matrix: Mat, rows: IndexedSeq[Int]): DMat =
    require(rows.nonEmpty, "row selection must be non-empty")
    val out = Matrix.newBuilder(rows.length, matrix.cols)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      var col = 0
      while col < matrix.cols do
        out(outRow, col) = matrix.data(sourceRow * matrix.cols + col)
        col += 1
      outRow += 1
    out.result()

  def fromHrfMatrixRowsCols(matrix: Mat, rows: IndexedSeq[Int], cols: IndexedSeq[Int]): DMat =
    require(rows.nonEmpty, "row selection must be non-empty")
    cols.foreach { col =>
      require(col >= 0 && col < matrix.cols, s"column index $col out of bounds")
    }
    val out = Matrix.newBuilder(rows.length, cols.length)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      var outCol = 0
      while outCol < cols.length do
        out(outRow, outCol) = matrix.data(sourceRow * matrix.cols + cols(outCol))
        outCol += 1
      outRow += 1
    out.result()

  def bindColumns(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows, s"row mismatch: ${left.rows} vs ${right.rows}")
    if left.cols == 0 then Matrix.tabulate(right.rows, right.cols)(right.apply)
    else if right.cols == 0 then Matrix.tabulate(left.rows, left.cols)(left.apply)
    else
      val outCols = left.cols + right.cols
      val out = Matrix.newBuilder(left.rows, outCols)
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          out(row, col) = left(row, col)
          col += 1
        col = 0
        while col < right.cols do
          out(row, left.cols + col) = right(row, col)
          col += 1
        row += 1
      out.result()

  def designMatrix(model: FmriModel, timepoints: IndexedSeq[Int]): Either[FitError, DesignMatrix] =
    DesignMatrix.fromMatrix(fromHrfMatrixRows(model.designMatrix, timepoints))

  def responseBlock(series: FmriSeries): Either[FitError, ResponseBlock] =
    ResponseBlock.fromMatrix(series.data)

  private[fit] def responseBlock(
      series: FmriSeries,
      policy: MissingDataPolicy
  ): Either[FitError, AdaptedResponseBlock] =
    policy match
      case MissingDataPolicy.Error =>
        responseBlock(series).map(AdaptedResponseBlock(_, series.voxelIndices, Vector.empty))
      case MissingDataPolicy.ExcludeVoxel | MissingDataPolicy.Propagate =>
        excludeNonFiniteVoxels(series)
      case MissingDataPolicy.OmitRowsPerVoxel =>
        Left(FitError.UnsupportedMissingDataPolicy(
          "voxel-specific row omission must be executed through the observation-pattern planner"
        ))

  private def excludeNonFiniteVoxels(
      series: FmriSeries
  ): Either[FitError, AdaptedResponseBlock] =
    val retainedPositions = Vector.newBuilder[Int]
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    var voxel = 0
    while voxel < series.data.cols do
      var row = 0
      var finite = true
      while row < series.data.rows && finite do
        if !series.data(row, voxel).isFinite then finite = false
        row += 1
      if finite then retainedPositions += voxel
      else exclusions += VoxelInferenceExclusion(series.voxelIndices(voxel), VoxelFitStatus.NonFinite)
      voxel += 1

    val retained = retainedPositions.result()
    val omitted = exclusions.result()
    if retained.isEmpty then Left(FitError.AllVoxelsExcluded(omitted))
    else
      val matrix = Matrix.newBuilder(series.data.rows, retained.length)
      var row = 0
      while row < series.data.rows do
        var localVoxel = 0
        while localVoxel < retained.length do
          matrix(row, localVoxel) = series.data(row, retained(localVoxel))
          localVoxel += 1
        row += 1
      ResponseBlock.fromMatrix(matrix.result()).map { response =>
        AdaptedResponseBlock(
          response = response,
          voxelIndices = retained.map(series.voxelIndices),
          fitExclusions = omitted
        )
      }
