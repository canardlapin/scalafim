package scalafim.multivar
package family.cpca

import scalafim.multivar.core.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.cpca.*

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
    val rowMetric = MetricSpec
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
      import scalafim.multivar.core.*
      import scalafim.multivar.contract.*
      import scalafim.multivar.optimization.*
      import scalafim.multivar.solver.*
      import scalafim.multivar.lifecycle.*
      import scalafim.multivar.capability.*
      import scalafim.multivar.family.spectral.*
      import scalafim.multivar.family.paired.*
      import scalafim.multivar.family.canonical.*
      import scalafim.multivar.family.cpca.*
      import scalafim.multivar.family.sparse.*
      import scalafim.multivar.family.glrm.*
      import scalafim.multivar.family.multiblock.*
      import scalafim.multivar.family.kernel.*
      import scalafim.multivar.workflow.*
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

  test("identity constraints reduce the complete block to ordinary SVD"):
    val x = matrix(
      Vector(
        Vector(1.0, 2.0, 0.5),
        Vector(0.0, -1.0, 2.0),
        Vector(3.0, 0.5, -0.5),
        Vector(2.0, -2.0, 1.0)
      )
    )
    val rows = MvSpace.of("cpca.identity.rows", SpaceRole.Samples, x.rows).toOption.get
    val features = MvSpace.of("cpca.identity.features", SpaceRole.Observed, x.cols).toOption.get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        MatrixView.dense(x),
        None,
        None,
        CpcaConstraint.Identity,
        CpcaConstraint.Identity,
        rows,
        features
      )
      .toOption
      .get
    val request = CpcaBlockRequest
      .from(CpcaBlock.all, rankByBlock = Map(CpcaBlock.GxH -> ComponentCount.unsafe(3)))
      .toOption
      .get
    val fit = problem.fit(request).toOption.get
    val svd = Svd.fit(MatrixView.dense(x), ComponentCount.unsafe(3)).toOption.get

    assertVectorClose(fit.block(CpcaBlock.GxH).get.singularValues, svd.result.singularValues, 1e-10)
    assertEqualsDouble(fit.partition.inertia(CpcaBlock.GxH).get.ss, squaredNorm(x), 1e-10)
    assertEquals(fit.block(CpcaBlock.G0xH).map(_.rank), Some(0))
    assertEquals(fit.block(CpcaBlock.GxH0).map(_.rank), Some(0))
    assertEquals(fit.block(CpcaBlock.G0xH0).map(_.rank), Some(0))

  test("four typed CPCA blocks reconstruct the whitened table and partition its inertia"):
    val x = matrix(
      Vector(
        Vector(1.0, 0.5, 2.0, -1.0),
        Vector(1.5, -0.5, 1.0, 0.0),
        Vector(-1.0, 2.0, 0.5, 1.0),
        Vector(0.0, 1.0, -1.5, 2.0),
        Vector(2.0, -1.0, 0.0, 1.5)
      )
    )
    val rowDesign = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val featureDesign = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val rows = MvSpace.of("cpca.partition.rows", SpaceRole.Samples, x.rows).toOption.get
    val features = MvSpace.of("cpca.partition.features", SpaceRole.Observed, x.cols).toOption.get
    val rowMetric = MetricSpec
      .diagonal(GaleNumerics.vectorFromArray(Array(1.0, 0.75, 1.5, 1.25, 0.5)), Some(rows))
      .toOption
      .get
    val featureMetric = MetricSpec
      .diagonal(GaleNumerics.vectorFromArray(Array(1.2, 0.8, 1.5, 0.6)), Some(features))
      .toOption
      .get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        MatrixView.dense(x),
        Some(rowMetric),
        Some(featureMetric),
        CpcaConstraint.Basis(rowDesign),
        CpcaConstraint.Basis(featureDesign),
        rows,
        features
      )
      .toOption
      .get
    val fit = problem.fit(CpcaBlockRequest.from(CpcaBlock.all).toOption.get).toOption.get
    val whitened = fit.value.rowMetricRoots.half.applyLeft(fit.value.featureMetricRoots.half.applyRight(x))
    val reconstructed = CpcaBlock.all
      .map(block => fit.block(block).get.reconstructWhitened().toOption.get)
      .foldLeft(DMat.zeros(x.rows, x.cols))(CpcaMath.add)
    val partitioned = fit.partition.blocks.map(_.ss).sum

    assertMatrixClose(reconstructed, whitened, 1e-8)
    assertEqualsDouble(partitioned, fit.partition.totalSS, 1e-9)
    assertEqualsDouble(fit.partition.totalSS, squaredNorm(whitened), 1e-9)

  test("zero row constraint assigns identity-column inertia to the residual row block"):
    val x = matrix(Vector(Vector(1.0, 2.0), Vector(-1.0, 0.5), Vector(0.0, 3.0)))
    val rows = MvSpace.of("cpca.zero.rows", SpaceRole.Samples, x.rows).toOption.get
    val features = MvSpace.of("cpca.zero.features", SpaceRole.Observed, x.cols).toOption.get
    val problem = CpcaOperatorProblem
      .fromMatrices(
        MatrixView.dense(x),
        None,
        None,
        CpcaConstraint.Zero,
        CpcaConstraint.Identity,
        rows,
        features
      )
      .toOption
      .get
    val fit = problem.fit(CpcaBlockRequest.from(CpcaBlock.all).toOption.get).toOption.get

    assertEquals(fit.block(CpcaBlock.GxH).map(_.rank), Some(0))
    assertEquals(fit.block(CpcaBlock.GxH0).map(_.rank), Some(0))
    assertEqualsDouble(fit.partition.inertia(CpcaBlock.G0xH).get.ss, squaredNorm(x), 1e-10)
    assertEqualsDouble(fit.partition.inertia(CpcaBlock.G0xH0).get.ss, 0.0, 1e-12)

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

  private def assertVectorClose(
      actual: gale.linalg.DVec,
      expected: gale.linalg.DVec,
      tolerance: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1

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
