package scalafim.multivar

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.Matrix

/** Deliberately direct projector oracle for canonical-effect tests.
  *
  * This does not share the production moment path. It materializes the time-by-time
  * effect and residual projectors and uses a tiny Gauss-Jordan inverse so that a bug
  * in moment assembly or its Gale solve cannot certify itself.
  */
private[multivar] object CanonicalEffectDenseOracle:
  final case class Result(effect: DMat, residual: DMat)

  def evaluate(design: DMat, response: DMat, contrast: DMat): Result =
    require(design.rows == response.rows, "design and response must share timepoints")
    require(contrast.rows == 1, "version-one oracle requires one contrast row")
    require(contrast.cols == design.cols, "contrast columns must match the design")

    val inverseXtX = inverse(design.t * design)
    val contrastVariance = contrast * inverseXtX * contrast.t
    val inverseContrastVariance = inverse(contrastVariance)
    val fittedProjector = design * inverseXtX * design.t
    val effectProjector =
      design * inverseXtX * contrast.t * inverseContrastVariance * contrast * inverseXtX * design.t
    val residualProjector = subtract(identity(design.rows), fittedProjector)

    Result(
      response.t * effectProjector * response,
      response.t * residualProjector * response
    )

  private def identity(size: Int): DMat =
    val out = Matrix.newBuilder(size, size)
    var index = 0
    while index < size do
      out(index, index) = 1.0
      index += 1
    out.result()

  private def subtract(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols, "matrix shapes must match")
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

  private def inverse(matrix: DMat): DMat =
    require(matrix.rows == matrix.cols, "inverse requires a square matrix")
    val size = matrix.rows
    val width = size * 2
    val augmented = new Array[Double](size * width)
    var row = 0
    while row < size do
      var col = 0
      while col < size do
        augmented(row * width + col) = matrix(row, col)
        augmented(row * width + size + col) = if row == col then 1.0 else 0.0
        col += 1
      row += 1

    var pivot = 0
    while pivot < size do
      var best = pivot
      var candidate = pivot + 1
      while candidate < size do
        if Math.abs(augmented(candidate * width + pivot)) > Math.abs(augmented(best * width + pivot)) then
          best = candidate
        candidate += 1
      require(Math.abs(augmented(best * width + pivot)) > 1e-14, "oracle matrix must be nonsingular")
      if best != pivot then swapRows(augmented, width, pivot, best)

      val scale = augmented(pivot * width + pivot)
      var col = 0
      while col < width do
        augmented(pivot * width + col) /= scale
        col += 1

      row = 0
      while row < size do
        if row != pivot then
          val factor = augmented(row * width + pivot)
          col = 0
          while col < width do
            augmented(row * width + col) -= factor * augmented(pivot * width + col)
            col += 1
        row += 1
      pivot += 1

    val out = Matrix.newBuilder(size, size)
    row = 0
    while row < size do
      var col = 0
      while col < size do
        out(row, col) = augmented(row * width + size + col)
        col += 1
      row += 1
    out.result()

  private def swapRows(values: Array[Double], width: Int, left: Int, right: Int): Unit =
    var col = 0
    while col < width do
      val leftIndex = left * width + col
      val rightIndex = right * width + col
      val value = values(leftIndex)
      values(leftIndex) = values(rightIndex)
      values(rightIndex) = value
      col += 1
