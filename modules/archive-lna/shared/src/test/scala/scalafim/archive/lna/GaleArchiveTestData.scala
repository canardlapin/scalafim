package scalafim.archive.lna

import gale.linalg.DMat

private[scalafim] object GaleArchiveTestData:
  def matrixFromRows(rows: Seq[Seq[Double]]): DMat =
    val rowCount = rows.length
    val columnCount = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == columnCount), "matrix rows must have equal lengths")
    DMat.tabulate(rowCount, columnCount)((row, column) => rows(row)(column))

  def toRows(matrix: DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols)(column => matrix(row, column))
