package scalafim.multivar
package capability

import scalafim.multivar.core.*

import gale.linalg.DMat

class ProjectionContractSuite extends munit.FunSuite:

  import ProjectionParityReferenceFixtures as R

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def multiply(left: DMat, right: DMat): DMat =
    GaleNumerics.multiply(left, right)

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def centerScale(input: DMat, center: Vector[Double], scale: Vector[Double]): DMat =
    require(input.cols == center.length)
    require(input.cols == scale.length)
    val values = Array.ofDim[Double](input.rows * input.cols)
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        values(row * input.cols + col) = (input(row, col) - center(col)) / scale(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def inverseCenterScale(input: DMat, center: Vector[Double], scale: Vector[Double]): DMat =
    require(input.cols == center.length)
    require(input.cols == scale.length)
    val values = Array.ofDim[Double](input.rows * input.cols)
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        values(row * input.cols + col) = input(row, col) * scale(col) + center(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def add(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows)
    require(left.cols == right.cols)
    val values = Array.ofDim[Double](left.rows * left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        values(row * left.cols + col) = left(row, col) + right(row, col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(left.rows, left.cols, values)

  private def identity(size: Int): DMat =
    matrix(Vector.tabulate(size)(row => Vector.tabulate(size)(col => if row == col then 1.0 else 0.0)))

  private def addDiagonal(input: DMat, amount: Double): DMat =
    require(input.rows == input.cols)
    val values = input.copyData
    var index = 0
    while index < input.rows do
      values(index * input.cols + index) += amount
      index += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def inverseTwoByTwo(input: DMat): Either[String, DMat] =
    require(input.rows == 2)
    require(input.cols == 2)
    val determinant = input(0, 0) * input(1, 1) - input(0, 1) * input(1, 0)
    if Math.abs(determinant) <= 1e-12 then Left("rank-deficient two-by-two system")
    else
      Right(
        matrix(
          Vector(
            Vector(input(1, 1) / determinant, -input(0, 1) / determinant),
            Vector(-input(1, 0) / determinant, input(0, 0) / determinant)
          )
        )
      )

  private def rightSolve(input: DMat, system: DMat): Either[String, DMat] =
    inverseTwoByTwo(system).map(inverse => multiply(input, inverse))

  private def recover(
      contribution: DMat,
      weights: DMat,
      metric: DMat,
      ridge: Double
  ): Either[String, DMat] =
    val metricWeights = multiply(metric, weights)
    val gram = addDiagonal(GaleNumerics.transposeMultiply(weights, metricWeights), ridge)
    rightSolve(contribution, gram)

  private def centerColumns(input: DMat): DMat =
    val means = Array.ofDim[Double](input.cols)
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        means(col) += input(row, col)
        col += 1
      row += 1
    var col = 0
    while col < input.cols do
      means(col) /= input.rows.toDouble
      col += 1
    val values = Array.ofDim[Double](input.rows * input.cols)
    row = 0
    while row < input.rows do
      col = 0
      while col < input.cols do
        values(row * input.cols + col) = input(row, col) - means(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def scaleRows(input: DMat, weights: Vector[Double]): DMat =
    require(input.rows == weights.length)
    val values = Array.ofDim[Double](input.rows * input.cols)
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        values(row * input.cols + col) = input(row, col) * weights(row)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def supplementaryCompatibility(
      variables: DMat,
      scores: DMat,
      singularValues: Vector[Double]
  ): Either[String, DMat] =
    if variables.rows != scores.rows then Left("supplementary rows must align with training scores")
    else if variables.rows <= 1 then Left("supplementary projection needs at least two rows")
    else if singularValues.length != scores.cols then Left("one singular value is required per component")
    else if singularValues.exists(value => value * value <= 1e-12) then Left("null component scale")
    else
      val cross = GaleNumerics.transposeMultiply(centerColumns(variables), scores)
      val values = cross.copyData
      var row = 0
      while row < cross.rows do
        var col = 0
        while col < cross.cols do
          values(row * cross.cols + col) /=
            singularValues(col) * singularValues(col) * (variables.rows - 1).toDouble
          col += 1
        row += 1
      Right(GaleNumerics.matrixFromRowMajor(cross.rows, cross.cols, values))

  private def supplementaryMetricLeastSquares(
      variables: DMat,
      scores: DMat,
      rowWeights: Vector[Double],
      ridge: Double
  ): Either[String, DMat] =
    if variables.rows != scores.rows || scores.rows != rowWeights.length then
      Left("row measure and identities must align")
    else
      val centered = centerColumns(variables)
      val weightedScores = scaleRows(scores, rowWeights)
      val cross = GaleNumerics.transposeMultiply(centered, weightedScores)
      val gram = addDiagonal(GaleNumerics.transposeMultiply(scores, weightedScores), ridge)
      rightSolve(cross, gram)

  private def restrictionIndices(
      fitted: Vector[String],
      supplied: Vector[String]
  ): Either[String, Vector[Int]] =
    if supplied.distinct.length != supplied.length then Left("duplicate feature identity")
    else
      supplied.foldLeft[Either[String, Vector[Int]]](Right(Vector.empty)): (result, feature) =>
        result.flatMap: indices =>
          val index = fitted.indexOf(feature)
          if index < 0 then Left(s"unknown feature identity: $feature")
          else Right(indices :+ index)

  private def alignmentPermutation(
      fittedRows: Vector[String],
      suppliedRows: Vector[String]
  ): Either[String, Vector[Int]] =
    if fittedRows.distinct.length != fittedRows.length || suppliedRows.distinct.length != suppliedRows.length then
      Left("row identities must be unique")
    else if fittedRows.toSet != suppliedRows.toSet then Left("row identity sets differ")
    else Right(fittedRows.map(suppliedRows.indexOf))

  private def accepted[A](result: Either[String, A]): A =
    result.fold(message => fail(message), value => value)

  test("base-R fixture fixes full, partial-contribution, and partial-recovery semantics"):
    val working = centerScale(R.newRaw, R.center, R.scale)
    assertMatrixClose(working, R.newWorking)
    assertMatrixClose(multiply(working, R.weights), R.fullScores)

    val partialCenter = R.subset.map(R.center)
    val partialScale = R.subset.map(R.scale)
    val partialWorking = centerScale(R.partialRaw, partialCenter, partialScale)
    val partialWeights = R.weights.selectColumns(Vector(0, 1)).selectRows(R.subset)
    val contribution = multiply(partialWorking, partialWeights)
    assertMatrixClose(partialWorking, R.partialWorking)
    assertMatrixClose(contribution, R.partialContribution)
    assertMatrixClose(accepted(recover(contribution, partialWeights, identity(2), R.ridge)), R.partialLeastSquares)
    assertMatrixClose(
      accepted(recover(contribution, partialWeights, R.partialMetric, R.ridge)),
      R.partialMetricLeastSquares
    )
    assert(Math.abs(contribution(0, 0) - R.partialLeastSquares(0, 0)) > 0.1)

  test("partial contributions are additive across a complete block partition"):
    val first = multiply(R.newWorking.selectColumns(Vector(0, 1)), R.weights.selectRows(Vector(0, 1)))
    val second = multiply(R.newWorking.selectColumns(Vector(2, 3)), R.weights.selectRows(Vector(2, 3)))
    assertMatrixClose(first, R.blockOneContribution)
    assertMatrixClose(second, R.blockTwoContribution)
    assertMatrixClose(add(first, second), R.fullScores)
    assertMatrixClose(multiply(R.newWorking, R.weights), R.fullScores)

  test("supplementary-variable conventions match independent R oracles"):
    assertMatrixClose(
      accepted(supplementaryCompatibility(R.supplementaryRaw, R.trainingScores, R.singularValues)),
      R.supplementaryCompatibility
    )
    assertMatrixClose(
      accepted(supplementaryMetricLeastSquares(R.supplementaryRaw, R.trainingScores, R.rowWeights, R.ridge)),
      R.supplementaryMetricLeastSquares
    )

  test("supplementary projection is invariant to a checked joint row permutation"):
    val permutation = Vector(3, 0, 4, 1, 2)
    val variables = R.supplementaryRaw.selectRows(permutation)
    val scores = R.trainingScores.selectRows(permutation)
    val rowWeights = permutation.map(R.rowWeights)
    assertMatrixClose(
      accepted(supplementaryCompatibility(variables, scores, R.singularValues)),
      R.supplementaryCompatibility
    )
    assertMatrixClose(
      accepted(supplementaryMetricLeastSquares(variables, scores, rowWeights, R.ridge)),
      R.supplementaryMetricLeastSquares
    )

  test("synthesis, original-coordinate reconstruction, and paired transfer are distinct compositions"):
    val reconstructionWorking = multiply(R.fullScores, R.decoder)
    assertMatrixClose(reconstructionWorking, R.reconstructionWorking)
    assertMatrixClose(inverseCenterScale(reconstructionWorking, R.center, R.scale), R.reconstructionRaw)

    val transferWorking = multiply(R.fullScores, R.pairedTargetDecoder)
    assertMatrixClose(transferWorking, R.transferWorking)
    assertMatrixClose(
      inverseCenterScale(transferWorking, R.pairedTargetCenter, R.pairedTargetScale),
      R.transferRaw
    )
    assert(R.decoder.rows == R.weights.cols)
    assert(R.decoder.cols == R.weights.rows)
    assert(R.pairedTargetDecoder.cols == R.pairedTargetWeights.rows)

  test("rank-deficient recovery rejects an exact solve and remains finite with an explicit ridge"):
    val rankDeficientWeights = matrix(Vector(Vector(1.0, 1.0), Vector(2.0, 2.0)))
    val observed = matrix(Vector(Vector(0.5, -1.0), Vector(2.0, 1.0)))
    val contribution = multiply(observed, rankDeficientWeights)
    val singularGram = GaleNumerics.transposeMultiply(rankDeficientWeights, rankDeficientWeights)
    assert(inverseTwoByTwo(singularGram).isLeft)
    val recovered = accepted(recover(contribution, rankDeficientWeights, identity(2), 0.2))
    var row = 0
    while row < recovered.rows do
      var col = 0
      while col < recovered.cols do
        assert(java.lang.Double.isFinite(recovered(row, col)))
        col += 1
      row += 1

  test("feature and row identities make restrictions and alignments explicit"):
    val fittedFeatures = Vector("motion", "age", "task", "site")
    assertEquals(
      restrictionIndices(fittedFeatures, Vector("task", "motion")),
      Right(Vector(2, 0))
    )
    assert(restrictionIndices(fittedFeatures, Vector("task", "task")).isLeft)
    assert(restrictionIndices(fittedFeatures, Vector("unknown")).isLeft)

    val fittedRows = Vector("s1", "s2", "s3")
    val suppliedRows = Vector("s3", "s1", "s2")
    assertEquals(alignmentPermutation(fittedRows, suppliedRows), Right(Vector(1, 2, 0)))
    assert(alignmentPermutation(fittedRows, Vector("s1", "s2", "s4")).isLeft)

  test("supplementary compatibility rejects null spectra and underspecified rows"):
    assert(supplementaryCompatibility(R.supplementaryRaw, R.trainingScores, Vector(1.0, 0.0)).isLeft)
    assert(
      supplementaryCompatibility(
        R.supplementaryRaw.selectRows(Vector(0)),
        R.trainingScores.selectRows(Vector(0)),
        R.singularValues
      ).isLeft
    )
