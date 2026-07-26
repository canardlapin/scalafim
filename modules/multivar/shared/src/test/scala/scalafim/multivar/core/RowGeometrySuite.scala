package scalafim.multivar
package core

import scalafim.multivar.core.*
import scalafim.multivar.family.spectral.*

import gale.linalg.DMat
import gale.linalg.DVec

class RowGeometrySuite extends munit.FunSuite:

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

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertMatrixClose(actual, expected.toRows, tol)

  private def assertVectorClose(actual: DVec, expected: DVec, tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private def rightSpectralGram(vectors: DMat, values: DVec): DMat =
    val scaled = vectors.copyData
    var col = 0
    while col < vectors.cols do
      val scale = values(col) * values(col)
      var row = 0
      while row < vectors.rows do
        scaled(row * vectors.cols + col) *= scale
        row += 1
      col += 1
    GaleNumerics.multiply(GaleNumerics.matrixFromRowMajor(vectors.rows, vectors.cols, scaled), vectors.transpose)

  private def assertInvalidTolerance[A](result: Either[MultivarError, A], role: String): Unit =
    result match
      case Left(MultivarError.InvalidTolerance(actualRole, value)) =>
        assertEquals(actualRole, role)
        assert(!value.isFinite || value < 0.0, s"unexpected tolerance value $value")
      case other =>
        fail(s"expected InvalidTolerance($role), got $other")

  private def assertShapeMismatch[A](result: Either[MultivarError, A], expected: String): Unit =
    result match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains(expected), detail)
      case other =>
        fail(s"expected MatrixShapeMismatch containing '$expected', got $other")

  private def assertFiniteMatrix(matrix: DMat): Unit =
    val data = matrix.copyData
    var i = 0
    while i < data.length do
      assert(data(i).isFinite, s"entry $i is not finite: ${data(i)}")
      i += 1

  private def pass(cols: Int): FittedPreprocessor =
    FittedColumnAffine(cols, MatrixView.ones(cols), MatrixView.zeros(cols))

  private def cross(left: DMat, right: DMat): DMat =
    GaleNumerics.transposeMultiply(left, right)

  test("block Cholesky row whitening exposes whitening, unwhitening, and solve algebra") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 1), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(2), IndexAxis.Row).toOption.get
    )
    val metric = RowWhitening.blockCholesky(
      rows = 3,
      blocks = blocks,
      upperCholesky = Vector(
        GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(0.0, 3.0))),
        GaleNumerics.matrixFromRows(Vector(Vector(4.0)))
      )
    ).toOption.get
    val a = GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(3.0, 5.0), Vector(8.0, 4.0)))
    val b = GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))

    val whitened = metric.whiten(a).toOption.get
    val unwhitened = metric.unwhiten(whitened).toOption.get
    val solved = metric.solve(b).toOption.get

    assertMatrixClose(unwhitened, a, 1e-10)
    assertMatrixClose(cross(metric.whiten(a).toOption.get, metric.whiten(b).toOption.get), cross(a, solved), 1e-10)
  }

  test("a RowWhitening-induced operator metric makes GPCA equal whitening-then-PCA") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 1), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(2, 3), IndexAxis.Row).toOption.get
    )
    val whitening = RowWhitening.blockCholesky(
      rows = 4,
      blocks = blocks,
      upperCholesky = Vector(
        GaleNumerics.matrixFromRows(Vector(Vector(2.0, 0.5), Vector(0.0, 1.5))),
        GaleNumerics.matrixFromRows(Vector(Vector(1.25, -0.25), Vector(0.0, 2.0)))
      )
    ).toOption.get
    val x = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 2.0, -1.0),
        Vector(0.5, -1.0, 3.0),
        Vector(2.0, 0.25, 1.0),
        Vector(-1.0, 1.5, 0.5)
      )
    )
    val rowMetric = MetricSpec.fromRowWhitening(whitening).toOption.get
    val input = MatrixView.dense(x)
    val rowSpace = MvSpace.of("row-whitening-gpca.rows", SpaceRole.Samples, x.rows).toOption.get
    val featureSpace = MvSpace.of("row-whitening-gpca.features", SpaceRole.Observed, x.cols).toOption.get
    val featureMetric = MetricSpec.identity(x.cols, Some(featureSpace)).toOption.get
    val problem = DynamicGpcaProblem
      .from(
        input,
        rowSpace,
        featureSpace,
        rowMetric,
        featureMetric,
        ValueIdentity.source(ValueId.unsafe("row-whitening-gpca.table")),
        SemanticProvenance.source("row-whitening-gpca")
      )
      .toOption
      .get
    val conditioned = problem.fit(ComponentCount.unsafe(2)).toOption.get
    val whitened = Pca
      .fit(MatrixView.dense(whitening.whiten(x).toOption.get), ComponentCount.unsafe(2), PreprocessSpec.Pass)
      .toOption
      .get

    assertVectorClose(conditioned.singularValues, whitened.result.singularValues, 1e-9)
    assertMatrixClose(
      rightSpectralGram(conditioned.axes.get.toDense.toOption.get, conditioned.singularValues),
      rightSpectralGram(whitened.result.v, whitened.result.singularValues),
      1e-9
    )
  }

  test("row whitening freezes as a certified typed metric and row relation") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 2), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(1), IndexAxis.Row).toOption.get
    )
    val whitening = RowWhitening.blockCholesky(
      rows = 3,
      blocks = blocks,
      upperCholesky = Vector(
        GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(0.0, 3.0))),
        GaleNumerics.matrixFromRows(Vector(Vector(4.0)))
      )
    ).toOption.get
    val rows = MvSpace.of("operator-rows", SpaceRole.Samples, 3).toOption.get
    val geometry = whitening.toOperatorGeometry(rows, tolerance = 1e-9).toOption.get
    val expectedRoot = whitening.whiten(DMat.eye(3)).toOption.get
    val expected = GaleNumerics.crossProduct(expectedRoot)

    assertEquals(geometry.space.descriptor, rows)
    assertEquals(geometry.metric.domain.descriptor.variance, CoordinateVariance.Primal)
    assertEquals(geometry.metric.codomain.descriptor.variance, CoordinateVariance.Dual)
    assertEquals(geometry.relation.role.value, OperatorRole.RowLink)
    assertEquals(geometry.evidence.mode, RowWhiteningMode.BlockCholesky)
    assertEquals(geometry.evidence.certificate.claim.property, "spd")
    assertEqualsDouble(geometry.evidence.tolerance.absolute, 1e-9, 0.0)
    assertMatrixClose(geometry.metric.toDense.toOption.get, expected, 1e-12)
    assertMatrixClose(geometry.relation.toDense.toOption.get, expected, 1e-12)
  }

  test("row whitening operator bridge rejects nominal-space and tolerance mismatches") {
    val whitening = RowWhitening.identity(2).toOption.get
    val wrongRows = MvSpace.of("wrong-rows", SpaceRole.Samples, 3).toOption.get
    val rows = MvSpace.of("rows", SpaceRole.Samples, 2).toOption.get

    assert(whitening.toOperatorGeometry(wrongRows).swap.toOption.exists {
      case MultivarError.InvalidRowGeometry(detail) => detail.contains("has 3")
      case _                                        => false
    })
    assertInvalidTolerance(
      whitening.toOperatorGeometry(rows, tolerance = Double.NaN),
      "row operator geometry tolerance"
    )
  }

  test("grouped identity row whitening validates block coverage and acts as identity") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 2), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(1), IndexAxis.Row).toOption.get
    )
    val metric = RowWhitening.groupedIdentity(3, blocks).toOption.get
    val a = GaleNumerics.matrixFromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0), Vector(5.0, 6.0)))

    assertEquals(metric.mode, RowWhiteningMode.GroupedIdentity)
    assertEquals(metric.blocks, blocks)
    assertMatrixClose(metric.whiten(a).toOption.get, a, 0.0)
    assertMatrixClose(metric.unwhiten(a).toOption.get, a, 0.0)
    assertMatrixClose(metric.solve(a).toOption.get, a, 0.0)

    val uncovered = RowWhitening.groupedIdentity(3, Vector(IndexSet.from(Vector(0, 1), IndexAxis.Row).toOption.get))
    uncovered match
      case Left(MultivarError.InvalidRowGeometry(detail)) =>
        assert(detail.contains("row 2"), detail)
      case other =>
        fail(s"expected uncovered-row rejection, got $other")
  }

  test("block Cholesky whitening scatters interleaved row blocks correctly") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 2), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(1), IndexAxis.Row).toOption.get
    )
    val metric = RowWhitening.blockCholesky(
      rows = 3,
      blocks = blocks,
      upperCholesky = Vector(
        GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(0.0, 3.0))),
        GaleNumerics.matrixFromRows(Vector(Vector(4.0)))
      )
    ).toOption.get
    val a = GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(3.0, 5.0), Vector(8.0, 4.0)))

    // Block {0,2} solves U' y = [rows 0 and 2]; block {1} scales row 1 by 1/4.
    val whitened = metric.whiten(a).toOption.get
    assertMatrixClose(
      whitened,
      Vector(
        Vector(1.0, 0.5),
        Vector(0.75, 1.25),
        Vector(7.0 / 3.0, 7.0 / 6.0)
      ),
      1e-12
    )
    assertMatrixClose(metric.unwhiten(whitened).toOption.get, a, 1e-10)
  }

  test("block Cholesky whitening honors the construction tolerance in its triangular solves") {
    val blocks = Vector(IndexSet.from(Vector(0), IndexAxis.Row).toOption.get)
    val metric = RowWhitening.blockCholesky(
      rows = 1,
      blocks = blocks,
      upperCholesky = Vector(GaleNumerics.matrixFromRows(Vector(Vector(1e-13)))),
      tolerance = 1e-15
    ).toOption.get
    val b = GaleNumerics.matrixFromRows(Vector(Vector(2e-13)))

    // A factor accepted at construction tolerance must stay usable at whiten time.
    assertMatrixClose(metric.whiten(b).toOption.get, Vector(Vector(2.0)), 1e-9)
  }

  test("singular row whitening Cholesky factors return a typed error") {
    val blocks = Vector(IndexSet.from(Vector(0), IndexAxis.Row).toOption.get)
    val result = RowWhitening.blockCholesky(
      rows = 1,
      blocks = blocks,
      upperCholesky = Vector(GaleNumerics.matrixFromRows(Vector(Vector(0.0))))
    )

    assert(result.swap.toOption.exists {
      case MultivarError.SingularRowMetric(_) => true
      case _                                  => false
    })
  }

  test("public row geometry tolerances reject negative and non-finite values") {
    val block = IndexSet.from(Vector(0), IndexAxis.Row).toOption.get
    assertInvalidTolerance(
      RowWhitening.blockCholesky(
        rows = 1,
        blocks = Vector(block),
        upperCholesky = Vector(GaleNumerics.matrixFromRows(Vector(Vector(1.0)))),
        tolerance = -1.0
      ),
      "row whitening Cholesky tolerance"
    )

    val design = GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(0.0)))
    assertInvalidTolerance(
      RowProjector.orthogonal(design, tolerance = Double.NaN),
      "row projector eigen tolerance"
    )
    assertInvalidTolerance(
      RowProjector.fromMatrix(DMat.eye(2), tolerance = Double.PositiveInfinity),
      "row projector matrix tolerance"
    )

    val projector = RowProjector.orthogonal(design).toOption.get
    val zero = RowProjector.zero(2).toOption.get
    assertInvalidTolerance(
      projector.difference(zero, tolerance = Double.NegativeInfinity),
      "row projector difference tolerance"
    )

    val metric = RowWhitening.identity(2).toOption.get
    val termFit = EffectTermFit.fromDesign("x", design, Vector(0), metric).toOption.get
    val response = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0))))
    assertInvalidTolerance(
      EffectOperator.fit(termFit, response, pass(1), DMat.eye(1), tolerance = Double.NaN),
      "effect operator SVD tolerance"
    )
  }

  test("orthogonal row projectors are symmetric idempotent and expose rank") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, -1.0),
        Vector(1.0, 0.0),
        Vector(1.0, 1.0)
      )
    )

    val projector = RowProjector.orthogonal(design).toOption.get
    val squared = GaleNumerics.multiply(projector.matrix, projector.matrix)

    assertEquals(projector.rank, 2)
    assertMatrixClose(projector.matrix.transpose, projector.matrix, 1e-9)
    assertMatrixClose(squared, projector.matrix, 1e-9)
  }

  test("row projector complements are symmetric idempotent and orthogonal to the original projector") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, -1.0),
        Vector(1.0, 0.0),
        Vector(1.0, 1.0)
      )
    )
    val projector = RowProjector.orthogonal(design).toOption.get
    val complement = projector.complement
    val complementSquared = GaleNumerics.multiply(complement.matrix, complement.matrix)
    val cross = GaleNumerics.multiply(complement.matrix, projector.matrix)

    assertEquals(complement.rank, 1)
    assertMatrixClose(complement.matrix.transpose, complement.matrix, 1e-9)
    assertMatrixClose(complementSquared, complement.matrix, 1e-9)
    assertMatrixClose(cross, DMat.zeros(projector.rows, projector.rows), 1e-9)
  }

  test("row whitening and projectors reject wrong response row counts with typed errors") {
    val metric = RowWhitening.identity(3).toOption.get
    val projector = RowProjector.orthogonal(
      GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(0.0), Vector(-1.0)))
    ).toOption.get
    val wrongRows = GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0)))

    assertShapeMismatch(metric.whiten(wrongRows), "expected 3 rows")
    assertShapeMismatch(metric.solve(wrongRows), "expected 3 rows")
    assertShapeMismatch(projector.project(wrongRows), "expected 3 rows")
  }

  test("effect term fit reconstructs the fixed-effect projector algebra used by multivarious") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0, 1.0)
      )
    )
    val y = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 1.0),
        Vector(2.0, 2.0),
        Vector(3.0, 1.0),
        Vector(6.0, 2.0)
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val fit = EffectTermFit.fromDesign("group.level", design, Vector(3), metric).toOption.get
    val effectMatrix = fit.effectMatrix(y).toOption.get

    assertEquals(fit.term.df, 1)
    assertEquals(fit.termProjector.rank, 1)
    assertMatrixClose(
      effectMatrix,
      Vector(
        Vector(0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(0.5, 0.0)
      ),
      1e-9
    )
    assertMatrixClose(GaleNumerics.multiply(fit.fullProjector.matrix, fit.nuisanceProjector.matrix), fit.nuisanceProjector.matrix, 1e-9)
  }

  test("effect model fits compose a model projector and per-term fits over one whitening") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0, 1.0)
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val model = EffectModelFit.fromTerms(
      design,
      metric,
      Vector("group" -> Vector(1), "level" -> Vector(2), "group.level" -> Vector(3))
    ).toOption.get

    assertEquals(model.terms.map(_.term.label), Vector("group", "level", "group.level"))
    assertEquals(model.modelProjector.rank, 4)
    assertEquals(model.term("group.level").map(_.term.df), Some(1))
    assertEquals(model.term("missing"), None)
    assertMatrixClose(model.designWhitened, model.design, 0.0)

    // Every term projector is nested inside the model projector.
    model.terms.foreach { fit =>
      assertMatrixClose(
        GaleNumerics.multiply(model.modelProjector.matrix, fit.termProjector.matrix),
        fit.termProjector.matrix,
        1e-9
      )
    }
  }

  test("effect operators compose whiten, project, and unwhiten with a nontrivial row whitening") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 1), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(2, 3), IndexAxis.Row).toOption.get
    )
    val metric = RowWhitening.blockCholesky(
      rows = 4,
      blocks = blocks,
      upperCholesky = Vector(
        GaleNumerics.matrixFromRows(Vector(Vector(2.0, 1.0), Vector(0.0, 3.0))),
        GaleNumerics.matrixFromRows(Vector(Vector(1.0, 0.5), Vector(0.0, 2.0)))
      )
    ).toOption.get
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 1.0),
        Vector(1.0, 0.0),
        Vector(1.0, 1.0),
        Vector(1.0, 0.0)
      )
    )
    val yDense = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 4.0),
        Vector(0.5, 1.5)
      )
    )
    val termFit = EffectTermFit.fromDesign("whitened.term", design, Vector(1), metric).toOption.get
    val operator = EffectOperator.fit(termFit, MatrixView.dense(yDense), pass(2), DMat.eye(2)).toOption.get

    assertEquals(termFit.term.df, 1)
    assertEquals(operator.rank, 1)
    val whitenedEffect = termFit.termProjector.project(metric.whiten(yDense).toOption.get).toOption.get
    assertMatrixClose(operator.reconstruct(EffectScale.Whitened).toOption.get, whitenedEffect, 1e-9)
    assertMatrixClose(
      operator.reconstruct(EffectScale.Processed).toOption.get,
      metric.unwhiten(whitenedEffect).toOption.get,
      1e-9
    )
  }

  test("effect operators reconstruct original-scale contributions through the preprocessor inverse") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(-1.0, 0.0),
        Vector(0.0, -1.0)
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(2.0, 0.0),
          Vector(0.0, 3.0),
          Vector(-2.0, 0.0),
          Vector(0.0, -3.0)
        )
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("orig.scale", design, Vector(0, 1), metric).toOption.get
    val preprocessor = FittedColumnAffine(
      2,
      DVec.fromSeq(Vector(2.0, 0.5)),
      DVec.fromSeq(Vector(1.0, -3.0))
    )
    val operator = EffectOperator.fit(termFit, y, preprocessor, DMat.eye(2)).toOption.get
    val processed = operator.reconstruct(EffectScale.Processed).toOption.get
    val original = operator.reconstruct(EffectScale.Original).toOption.get

    // The affine shift cancels in the contribution difference; only the scale inverts.
    var row = 0
    while row < processed.rows do
      assertEqualsDouble(original(row, 0), processed(row, 0) / 2.0, 1e-10)
      assertEqualsDouble(original(row, 1), processed(row, 1) / 0.5, 1e-10)
      row += 1
  }

  test("effect operator reconstructs whitened and processed contributions") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0, 1.0)
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 1.0),
          Vector(6.0, 2.0)
        )
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("group.level", design, Vector(3), metric).toOption.get
    val basis = DMat.eye(2)
    val operator = EffectOperator.fit(termFit, y, pass(2), basis).toOption.get

    assertEquals(operator.rank, 1)
    assertEqualsDouble(operator.singularValues(0), 1.0, 1e-9)
    assertMatrixClose(
      operator.reconstruct(EffectScale.Whitened).toOption.get,
      Vector(
        Vector(0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(0.5, 0.0)
      ),
      1e-9
    )
    assertMatrixClose(
      operator.reconstruct(EffectScale.Processed).toOption.get,
      operator.reconstruct(EffectScale.Whitened).toOption.get,
      1e-9
    )
  }

  test("effect operator truncation preserves typed shapes and finite reconstructions") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(-1.0, 0.0),
        Vector(0.0, -1.0)
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(2.0, 0.0),
          Vector(0.0, 3.0),
          Vector(-2.0, 0.0),
          Vector(0.0, -3.0)
        )
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("two.component", design, Vector(0, 1), metric).toOption.get
    val operator = EffectOperator.fit(termFit, y, pass(2), DMat.eye(2)).toOption.get
    val one = operator.truncate(1).toOption.get
    val zero = operator.truncate(0).toOption.get
    val reconstructedOne = one.reconstruct(EffectScale.Processed).toOption.get
    val reconstructedZero = zero.reconstruct(EffectScale.Processed).toOption.get

    assertEquals(operator.rank, 2)
    assertEquals(one.rank, 1)
    assertEquals(one.scores.rows, 4)
    assertEquals(one.scores.cols, 1)
    assertEquals(one.loadings.rows, 2)
    assertEquals(one.loadings.cols, 1)
    assertEquals(reconstructedOne.rows, 4)
    assertEquals(reconstructedOne.cols, 2)
    assertFiniteMatrix(reconstructedOne)

    assertEquals(zero.rank, 0)
    assertEquals(zero.scores.cols, 0)
    assertEquals(zero.loadings.cols, 0)
    assertMatrixClose(reconstructedZero, DMat.zeros(4, 2), 1e-12)
  }

  test("effect operator fit rejects a non-orthonormal basis with a typed error") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(-1.0, 0.0),
        Vector(0.0, -1.0)
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(2.0, 0.0),
          Vector(0.0, 3.0),
          Vector(-2.0, 0.0),
          Vector(0.0, -3.0)
        )
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("scaled.basis", design, Vector(0, 1), metric).toOption.get
    val basis = GaleNumerics.matrixFromRows(Vector(Vector(2.0), Vector(0.0)))

    EffectOperator.fit(termFit, y, pass(2), basis) match
      case Left(MultivarError.NonOrthonormalBasis(context, row, col, value)) =>
        assertEquals(context, "effect basis")
        assertEquals((row, col), (0, 0))
        assertEqualsDouble(value, 4.0, 1e-12)
      case other =>
        fail(s"expected non-orthonormal basis rejection, got $other")
  }

  test("aliased effect terms produce empty valid effect operators") {
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0),
        Vector(1.0, 1.0, 1.0)
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 4.0),
          Vector(5.0, 6.0),
          Vector(7.0, 8.0)
        )
      )
    )
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("aliased", design, Vector(2), metric).toOption.get
    val operator = EffectOperator.fit(termFit, y, pass(2), DMat.eye(2)).toOption.get

    assertEquals(termFit.term.df, 0)
    assertEquals(operator.rank, 0)
    assertEquals(operator.scores.cols, 0)
    assertEquals(operator.loadings.cols, 0)
    assertMatrixClose(
      operator.reconstruct(EffectScale.Processed).toOption.get,
      Vector(
        Vector(0.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      ),
      1e-12
    )
  }

  test("an exactly zero response produces an empty valid effect operator") {
    // Regression: a term with positive df but an exactly zero projected effect used
    // to fail the whole fit with SolverFailed instead of yielding the empty operator.
    val design = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(1.0, 1.0),
        Vector(1.0, 1.0)
      )
    )
    val y = MatrixView.dense(DMat.zeros(4, 2))
    val metric = RowWhitening.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("zero.response", design, Vector(1), metric).toOption.get
    assert(termFit.term.df > 0)

    EffectOperator.fit(termFit, y, pass(2), DMat.eye(2)) match
      case Right(operator) =>
        assertEquals(operator.rank, 0)
        assertEquals(operator.scores.cols, 0)
        assertEquals(operator.loadings.cols, 0)
        assertMatrixClose(
          operator.reconstruct(EffectScale.Processed).toOption.get,
          Vector(
            Vector(0.0, 0.0),
            Vector(0.0, 0.0),
            Vector(0.0, 0.0),
            Vector(0.0, 0.0)
          ),
          1e-12
        )
      case Left(error) =>
        fail(s"expected an empty valid effect operator, got $error")
  }

  test("orthonormal-column validation fails closed on non-finite Gram entries") {
    val basis = GaleNumerics.matrixFromRows(Vector(Vector(Double.NaN), Vector(0.0)))
    assert(RowGeometryOps.requireOrthonormalColumns("nan basis", basis, 1e-8).isLeft)
  }
