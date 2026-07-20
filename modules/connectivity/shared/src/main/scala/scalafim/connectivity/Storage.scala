package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

private[connectivity] object ConnectivityStorage:
  def copy(matrix: DMat): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out.result()
