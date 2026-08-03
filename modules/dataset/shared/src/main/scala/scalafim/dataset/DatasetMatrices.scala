package scalafim.dataset

import gale.linalg.DMat as GaleDMat
import scalafim.image.DMat

private[dataset] object DatasetMatrices:
  /** Materialize a Gale result at the image-layer storage boundary. */
  def fromGale(matrix: GaleDMat): DMat =
    val values = Array.ofDim[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        values(row * matrix.cols + col) = matrix(row, col)
        col += 1
      row += 1
    DMat.fromRowMajorOwned(matrix.rows, matrix.cols, values)
