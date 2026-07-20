package scalafim.fmri.group

import gale.linalg.{DMat, Matrix}

private[group] object GroupTestMatrix:
  def fromRows(rows: Seq[Seq[Double]]): DMat =
    val rowCount = rows.length
    val colCount = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == colCount), "test matrix rows must have equal length")
    val out = Matrix.newBuilder(rowCount, colCount)
    var row = 0
    while row < rowCount do
      var col = 0
      while col < colCount do
        out(row, col) = rows(row)(col)
        col += 1
      row += 1
    out.result()
