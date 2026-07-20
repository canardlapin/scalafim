package scalafim.connectivity

import scalafim.linalg.DoubleMatrix

private[connectivity] object ConnectivityStorage:
  def copy(matrix: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, matrix.copyData)
