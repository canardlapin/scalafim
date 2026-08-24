package scalafim.dataset.zarr

import gale.linalg.DMat

private[zarr] object GaleTestData:
  def toRows(matrix: DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols): column =>
        matrix(row, column)
