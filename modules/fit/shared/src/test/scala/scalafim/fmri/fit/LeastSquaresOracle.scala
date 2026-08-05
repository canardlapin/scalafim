package scalafim.fmri.fit

import gale.linalg.DMat

private object LeastSquaresOracle:
  def coefficients(design: DMat, response: DMat, tolerance: Double = 1e-12): DMat =
    require(design.rows == response.rows, s"row mismatch: ${design.rows} vs ${response.rows}")
    require(design.cols > 0, "design must have at least one predictor")
    require(design.rows >= design.cols, s"underdetermined oracle solve: ${design.rows} rows, ${design.cols} predictors")
    requireAllFinite(design, "design")
    requireAllFinite(response, "response")
    val lhs = crossProduct(design)
    val rhs = transposeMultiply(design, response)
    solve(lhs, rhs, design.cols, response.cols, tolerance)

  private def requireAllFinite(matrix: DMat, label: String): Unit =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        require(matrix(row, col).isFinite, s"$label contains non-finite value at ($row, $col)")
        col += 1
      row += 1

  private def crossProduct(matrix: DMat): Array[Double] =
    val out = new Array[Double](matrix.cols * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var leftCol = 0
      while leftCol < matrix.cols do
        val left = matrix(row, leftCol)
        var rightCol = 0
        while rightCol < matrix.cols do
          out(leftCol * matrix.cols + rightCol) += left * matrix(row, rightCol)
          rightCol += 1
        leftCol += 1
      row += 1
    out

  private def transposeMultiply(left: DMat, right: DMat): Array[Double] =
    val out = new Array[Double](left.cols * right.cols)
    var row = 0
    while row < left.rows do
      var leftCol = 0
      while leftCol < left.cols do
        val value = left(row, leftCol)
        var rightCol = 0
        while rightCol < right.cols do
          out(leftCol * right.cols + rightCol) += value * right(row, rightCol)
          rightCol += 1
        leftCol += 1
      row += 1
    out

  private def solve(
      lhs: Array[Double],
      rhs: Array[Double],
      size: Int,
      rhsCols: Int,
      tolerance: Double
  ): DMat =
    val a = lhs.clone
    val b = rhs.clone
    var pivot = 0
    while pivot < size do
      val pivotRow = bestPivotRow(a, size, pivot)
      val pivotValue = math.abs(a(pivotRow * size + pivot))
      if pivotValue <= tolerance then
        throw new IllegalArgumentException(s"singular oracle system at pivot $pivot")
      if pivotRow != pivot then
        swapRows(a, size, pivot, pivotRow)
        swapRows(b, rhsCols, pivot, pivotRow)

      var row = pivot + 1
      while row < size do
        val factor = a(row * size + pivot) / a(pivot * size + pivot)
        a(row * size + pivot) = 0.0
        var col = pivot + 1
        while col < size do
          a(row * size + col) -= factor * a(pivot * size + col)
          col += 1
        var rhsCol = 0
        while rhsCol < rhsCols do
          b(row * rhsCols + rhsCol) -= factor * b(pivot * rhsCols + rhsCol)
          rhsCol += 1
        row += 1
      pivot += 1

    val out = new Array[Double](size * rhsCols)
    var rhsCol = 0
    while rhsCol < rhsCols do
      var row = size - 1
      while row >= 0 do
        var sum = b(row * rhsCols + rhsCol)
        var col = row + 1
        while col < size do
          sum -= a(row * size + col) * out(col * rhsCols + rhsCol)
          col += 1
        out(row * rhsCols + rhsCol) = sum / a(row * size + row)
        row -= 1
      rhsCol += 1
    scalafim.fmri.fit.GaleTestMatrix.fromArray(size, rhsCols, out)

  private def bestPivotRow(a: Array[Double], size: Int, pivot: Int): Int =
    var bestRow = pivot
    var bestValue = math.abs(a(pivot * size + pivot))
    var row = pivot + 1
    while row < size do
      val value = math.abs(a(row * size + pivot))
      if value > bestValue then
        bestValue = value
        bestRow = row
      row += 1
    bestRow

  private def swapRows(matrix: Array[Double], cols: Int, leftRow: Int, rightRow: Int): Unit =
    var col = 0
    while col < cols do
      val leftIndex = leftRow * cols + col
      val rightIndex = rightRow * cols + col
      val tmp = matrix(leftIndex)
      matrix(leftIndex) = matrix(rightIndex)
      matrix(rightIndex) = tmp
      col += 1
