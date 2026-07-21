package scalafim.multivar

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class CpcaOperatorProblemSuite extends munit.FunSuite:

  test("typed CPCA feature covariance equals the independent X-star A X oracle"):
    val x = matrix(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(2.0, 4.0)
      )
    )
    val weights = Vector(2.0, 0.5, 1.5)
    val rowSpace = MvSpace.of("cpca.operator.rows", SpaceRole.Samples, 3).toOption.get
    val featureSpace = MvSpace.of("cpca.operator.features", SpaceRole.Observed, 2).toOption.get
    val rowMetric = MvMetric
      .diagonal(GaleNumerics.vectorFromArray(weights.toArray), Some(rowSpace))
      .toOption
      .get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        MatrixView.dense(x),
        Some(rowMetric),
        None,
        CpcaConstraint.Identity,
        CpcaConstraint.Identity,
        rowSpace,
        featureSpace
      )
      .toOption
      .get

    val actual = problem.value.featureCovariance.toDense.toOption.get
    val expected = weightedCrossProduct(x, weights)

    assertMatrixClose(actual, expected, 1e-12)
    assertEquals(problem.value.rowRelationship.role.value, OperatorRole.RowLink)
    assertEquals(problem.value.featureCovariance.role.value, OperatorRole.Covariance)

  test("CPCA block programs report the same complete block as a direct projector oracle"):
    val x = matrix(
      Vector(
        Vector(3.0, 9.0, 1.0),
        Vector(1.0, 7.0, 4.0),
        Vector(8.0, 2.0, 6.0),
        Vector(5.0, 3.0, 2.0)
      )
    )
    val rowDesign = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val featureDesign = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 1.0)
      )
    )
    val rowSpace = MvSpace.of("cpca.block.rows", SpaceRole.Samples, 4).toOption.get
    val featureSpace = MvSpace.of("cpca.block.features", SpaceRole.Observed, 3).toOption.get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        MatrixView.dense(x),
        None,
        None,
        CpcaConstraint.Basis(rowDesign),
        CpcaConstraint.Basis(featureDesign),
        rowSpace,
        featureSpace
      )
      .toOption
      .get
    val request = CpcaBlockRequest
      .from(Vector(CpcaBlock.GxH), defaultComponents = Some(ComponentCount.unsafe(2)))
      .toOption
      .get
    val fit = problem.fit(request).toOption.get
    val block = fit.block(CpcaBlock.GxH).get
    val operator = fit.operatorBlock(CpcaBlock.GxH).get
    val expected = matrix(
      Vector(
        Vector(3.0, 0.0, 1.0),
        Vector(1.0, 0.0, 4.0),
        Vector(0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 0.0)
      )
    )

    assertMatrixClose(block.reconstructWhitened().toOption.get, expected, 1e-9)
    assertEqualsDouble(fit.partition.inertia(CpcaBlock.GxH).get.ss, squaredNorm(expected), 1e-10)
    assertEquals(operator.programFit.program.objective.label, "maximize-trace")
    assertEquals(operator.programFit.frames.length, 1)
    assertEquals(operator.featureFrame.weights.role.value, OperatorRole.Frame)
    assertEquals(operator.blockTable.role.value, OperatorRole.Table)
    assertEquals(operator.featureOperator.role.value, OperatorRole.Covariance)
    assertEquals(operator.rowScores.role.value, OperatorRole.Score)
    assertMatrixClose(operator.rowScores.toDense.toOption.get, block.scores, 1e-9)
    assertEqualsDouble(operator.programFit.objectiveValue, squaredNorm(block.singularValues), 1e-12)
    assert(operator.diagnostics.crossResidual <= 1e-9)
    assert(operator.diagnostics.normalizationResidual <= 1e-9)

  test("constraint space orientation is static and shape mismatches are typed"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      import gale.linalg.DMat
      val rows = SpaceRef.of("cpca.static.rows", SpaceRole.Samples, 2).toOption.get
      val features = SpaceRef.of("cpca.static.features", SpaceRole.Observed, 2).toOption.get
      val wrong: OpConstraint[rows.Id, UncheckedEvidence] =
        Op.fromDense(
          DMat.eye(2),
          CoordinateEvidence.primal(features.evidence),
          CoordinateEvidence.primal(features.evidence),
          OperatorRoleWitness.constraint,
          ValueIdentity.source(ValueId.unsafe("wrong-cpca-projector"))
        ).toOption.get
    """)
    assert(errors.nonEmpty)

    val x = matrix(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val rowSpace = MvSpace.of("cpca.invalid.rows", SpaceRole.Samples, 2).toOption.get
    val featureSpace = MvSpace.of("cpca.invalid.features", SpaceRole.Observed, 2).toOption.get
    val wrongDesign = matrix(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))
    assert(
      CpcaOperatorProblem
        .fromMatrices(
          MatrixView.dense(x),
          None,
          None,
          CpcaConstraint.Identity,
          CpcaConstraint.Basis(wrongDesign),
          rowSpace,
          featureSpace
        )
        .isLeft
    )

  test("sparse CPCA input is not materialized without AllowDense"):
    val sparse = SparseMatrixView
      .fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
      .toOption
      .get
    val rowSpace = MvSpace.of("cpca.sparse.rows", SpaceRole.Samples, 3).toOption.get
    val featureSpace = MvSpace.of("cpca.sparse.features", SpaceRole.Observed, 2).toOption.get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        sparse,
        None,
        None,
        CpcaConstraint.Identity,
        CpcaConstraint.Identity,
        rowSpace,
        featureSpace,
        policy = StoragePolicy.PreserveSparse
      )
      .toOption
      .get
    val result = problem.fit(
      CpcaBlockRequest.default,
      policy = StoragePolicy.PreserveSparse
    )

    assert(result.swap.toOption.exists:
      case MultivarError.DensificationRejected("matrix", StorageKind.Sparse) => true
      case MultivarError.DensificationRejected(_, StorageKind.Sparse)        => true
      case _                                                                 => false
    )

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)

  private def weightedCrossProduct(x: DMat, weights: Vector[Double]): DMat =
    val out = new Array[Double](x.cols * x.cols)
    var left = 0
    while left < x.cols do
      var right = 0
      while right < x.cols do
        var value = 0.0
        var row = 0
        while row < x.rows do
          value += x(row, left) * weights(row) * x(row, right)
          row += 1
        out(left * x.cols + right) = value
        right += 1
      left += 1
    GaleNumerics.matrixFromRowMajor(x.cols, x.cols, out)

  private def squaredNorm(value: DMat): Double =
    val data = value.copyData
    var total = 0.0
    var index = 0
    while index < data.length do
      total += data(index) * data(index)
      index += 1
    total

  private def squaredNorm(values: gale.linalg.DVec): Double =
    var total = 0.0
    var index = 0
    while index < values.length do
      total += values(index) * values(index)
      index += 1
    total

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1
