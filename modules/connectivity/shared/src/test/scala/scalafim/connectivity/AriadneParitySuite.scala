package scalafim.connectivity

import gale.linalg.{DMat, Matrix}
import scalafim.connectivity.fixtures.AriadneCoreFixtures

class AriadneParitySuite extends munit.FunSuite:

  test("Ariadne compatibility order locks conn_set upper-triangle semantics") {
    val axis = NodeAxis.generated(4).toOption.get
    val matrix = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 2.0, 3.0, 4.0),
      Vector(5.0, 6.0, 7.0, 8.0),
      Vector(9.0, 10.0, 11.0, 12.0),
      Vector(13.0, 14.0, 15.0, 16.0)
    ))
    val ariadne = EdgeSpace.undirected(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val scalaNative = EdgeSpace.undirected(axis).toOption.get

    assertEquals(EdgeVectorizer.fromMatrix(matrix, ariadne).toOption.get.toVector, Vector(2.0, 3.0, 7.0, 4.0, 8.0, 12.0))
    assertEquals(EdgeVectorizer.fromMatrix(matrix, scalaNative).toOption.get.toVector, Vector(2.0, 3.0, 4.0, 7.0, 8.0, 12.0))
  }

  test("Ariadne compatibility order locks directed off-diagonal semantics") {
    val axis = NodeAxis.generated(3).toOption.get
    val matrix = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 2.0, 3.0),
      Vector(4.0, 5.0, 6.0),
      Vector(7.0, 8.0, 9.0)
    ))
    val ariadne = EdgeSpace.directed(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val scalaNative = EdgeSpace.directed(axis).toOption.get

    assertEquals(EdgeVectorizer.fromMatrix(matrix, ariadne).toOption.get.toVector, Vector(4.0, 7.0, 2.0, 8.0, 3.0, 6.0))
    assertEquals(EdgeVectorizer.fromMatrix(matrix, scalaNative).toOption.get.toVector, Vector(2.0, 3.0, 4.0, 6.0, 7.0, 8.0))
  }

  test("Ariadne compatibility order locks conn_rect_set as.vector semantics") {
    val seeds = NodeAxis.generated(2, "seed").toOption.get
    val rois = NodeAxis.generated(3, "roi").toOption.get
    val matrix = GaleTestMatrix.fromRows(Vector(
      Vector(1.0, 2.0, 3.0),
      Vector(4.0, 5.0, 6.0)
    ))
    val ariadne = EdgeSpace.rectangular(seeds, rois, VectorizationOrder.AriadneCompatible).toOption.get
    val scalaNative = EdgeSpace.rectangular(seeds, rois).toOption.get

    assertEquals(EdgeVectorizer.fromMatrix(matrix, ariadne).toOption.get.toVector, Vector(1.0, 4.0, 2.0, 5.0, 3.0, 6.0))
    assertEquals(EdgeVectorizer.fromMatrix(matrix, scalaNative).toOption.get.toVector, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("Ariadne vec_to_symm compatibility reconstructs the documented matrix") {
    val axis = NodeAxis.generated(4).toOption.get
    val space = EdgeSpace.undirected(axis, VectorizationOrder.AriadneCompatible).toOption.get
    val matrix = ConnectivityMatrix.fromEdgeVector(EdgeVector.from(space, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).toOption.get).toOption.get

    assertEquals(matrix.values.toRows, Vector(
      Vector(0.0, 1.0, 2.0, 4.0),
      Vector(1.0, 0.0, 3.0, 5.0),
      Vector(2.0, 3.0, 0.0, 6.0),
      Vector(4.0, 5.0, 6.0, 0.0)
    ))
  }

  test("Ariadne weighted_cor fixture locks normalized-weight correlation semantics") {
    val actual = weightedCorrelation(AriadneCoreFixtures.weightedInputRows, AriadneCoreFixtures.frameWeights)

    assertMatrixClose(actual, AriadneCoreFixtures.weightedCorrelation, 1e-12)
    assertEquals(actual.toRows.indices.forall(i => actual(i, i) == 1.0), true)
  }

  test("Ariadne cor_lw fixture locks diagonal-shrinkage semantics") {
    val actual = diagonalShrinkageCorrelation(AriadneCoreFixtures.weightedInputRows, AriadneCoreFixtures.frameWeights)

    assertMatrixClose(actual, AriadneCoreFixtures.diagonalShrinkageCorrelation, 1e-12)
    assert(AriadneCoreFixtures.notes.exists(_.contains("not ScalaFIM contracts")))
  }

  private def assertMatrixClose(actual: DMat, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1

  private def weightedCorrelation(rows: Vector[Vector[Double]], weights: Vector[Double]): DMat =
    val rowCount = rows.length
    val colCount = rows.head.length
    val weightSum = weights.sum
    val normalized = weights.map(_ / weightSum)
    val means = Array.fill(colCount)(0.0)
    var row = 0
    while row < rowCount do
      var col = 0
      while col < colCount do
        means(col) += rows(row)(col) * normalized(row)
        col += 1
      row += 1

    val covariance = Array.fill(colCount * colCount)(0.0)
    row = 0
    while row < rowCount do
      var left = 0
      while left < colCount do
        var right = 0
        while right < colCount do
          covariance(left * colCount + right) +=
            (rows(row)(left) - means(left)) * (rows(row)(right) - means(right)) * normalized(row)
          right += 1
        left += 1
      row += 1

    val out = new Array[Double](colCount * colCount)
    row = 0
    while row < colCount do
      var col = 0
      while col < colCount do
        val denom = Math.sqrt(Math.max(covariance(row * colCount + row), 1e-16)) *
          Math.sqrt(Math.max(covariance(col * colCount + col), 1e-16))
        out(row * colCount + col) = covariance(row * colCount + col) / denom
        col += 1
      out(row * colCount + row) = 1.0
      row += 1
    GaleTestMatrix.fromArray(colCount, colCount, out)

  private def diagonalShrinkageCorrelation(rows: Vector[Vector[Double]], weights: Vector[Double]): DMat =
    val correlation = weightedCorrelation(rows, weights)
    val off = Vector(correlation(0, 1), correlation(0, 2), correlation(1, 2))
    val mean = off.sum / off.length
    val variance = off.map(value => (value - mean) * (value - mean)).sum / (off.length - 1)
    val meanSquare = off.map(value => value * value).sum / off.length
    val denom = variance + meanSquare
    val alpha =
      if denom <= 0.0 || !denom.isFinite then 0.0
      else Math.max(0.0, Math.min(1.0, variance / denom))
    val out = correlation.copyData
    var row = 0
    while row < correlation.rows do
      var col = 0
      while col < correlation.cols do
        out(row * correlation.cols + col) =
          if row == col then 1.0
          else (1.0 - alpha) * correlation(row, col)
        col += 1
      row += 1
    GaleTestMatrix.fromArray(correlation.rows, correlation.cols, out)
