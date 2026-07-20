package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class CpcaSuite extends munit.FunSuite:

  private def k(value: Int): ComponentCount =
    ComponentCount.unsafe(value)

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  private def assertVectorClose(actual: DoubleVector, expected: DoubleVector, tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private def assertMetricOrthonormal(factors: DoubleMatrix, metric: MvMetric, tol: Double): Unit =
    val weighted = metric.matvec(factors).toOption.get
    val gram = DoubleMatrix.transposeMultiply(factors, weighted)
    var row = 0
    while row < gram.rows do
      var col = 0
      while col < gram.cols do
        assertEqualsDouble(gram(row, col), if row == col then 1.0 else 0.0, tol)
        col += 1
      row += 1

  private def frobeniusInner(left: DoubleMatrix, right: DoubleMatrix): Double =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    val leftData = left.copyData
    val rightData = right.copyData
    var acc = 0.0
    var i = 0
    while i < leftData.length do
      acc += leftData(i) * rightData(i)
      i += 1
    acc

  private def partitionValue(fit: CpcaFit, block: CpcaBlock): Double =
    fit.partition.inertia(block).map(_.ss).getOrElse(fail(s"missing partition block ${block.label}"))

  private def rightProject(con: ResolvedCpcaConstraint, input: DoubleMatrix): DoubleMatrix =
    con.project(input.transpose).toOption.get.transpose

  test("constraint specs validate shape before resolving projector geometry") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0)
      )
    )
    val spec = CpcaConstraint.Basis(design)

    assertEquals(spec.basisRows, Some(3))
    assert(spec.validate(IndexAxis.Row, expectedRows = 3).isRight)
    assert(spec.validate(IndexAxis.Row, expectedRows = 4).isLeft)
    assert(CpcaConstraint.Identity.validate(IndexAxis.Column, expectedRows = 2).isRight)
    assert(CpcaConstraint.Zero.validate(IndexAxis.Column, expectedRows = 2).isRight)
  }

  test("constraint specs resolve only after a concrete space and metric are known") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val space = MvSpace.of("cpca.spec.rows", SpaceRole.Samples, 4).toOption.get
    val metric = MvMetric.identity(4, Some(space)).toOption.get
    val resolved = CpcaConstraint.Basis(design, keepDesign = false)
      .resolve(IndexAxis.Row, space, metric)
      .toOption
      .get

    assertEquals(resolved.constraint, CpcaConstraint.Basis(design, keepDesign = false))
    assertEquals(resolved.space, space)
    assertEquals(resolved.rank, 2)
    assertEquals(resolved.originalDesign, None)
  }

  test("constraints expose projector and coordinate maps with decoder-backed projection") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val input = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, 3.0),
        Vector(4.0, 5.0),
        Vector(7.0, 11.0),
        Vector(13.0, 17.0)
      )
    )
    val space = MvSpace.of("cpca.map.rows", SpaceRole.Samples, 4).toOption.get
    val metric = MvMetric.identity(4, Some(space)).toOption.get
    val basis = CpcaConstraint.Basis(design)
      .resolve(IndexAxis.Row, space, metric)
      .toOption
      .get
    val projected = basis.project(input).toOption.get
    val projectorProjected = basis.projector
      .forward(MatrixView.dense(input.transpose))
      .toOption
      .get
      .transpose
    val coordinateMap = basis.coordinateMap.getOrElse(fail("basis constraint should expose coordinate map"))
    val scores = coordinateMap.forward(MatrixView.dense(input.transpose)).toOption.get
    val decoded = coordinateMap.decoder.toOption.get.forward(MatrixView.dense(scores)).toOption.get.transpose

    assertEquals(basis.projector.domain, space)
    assertEquals(basis.projector.codomain, space)
    assertMatrixClose(projectorProjected, projected, 1e-12)
    assertMatrixClose(decoded, projected, 1e-12)
    assertMatrixClose(basis.coordinates(input).toOption.get, scores.transpose, 1e-12)

    val zero = CpcaConstraint.zero(IndexAxis.Row, space)
    assertEquals(zero.coordinateMap, None)
    assertMatrixClose(zero.project(input).toOption.get, DoubleMatrix.zeros(input.rows, input.cols), 1e-12)
  }

  test("estimator specs validate metric and constraint dimensions without resolving formulas") {
    val rowMetric = MvMetric.identity(4).toOption.get
    val colMetric = MvMetric.identity(3).toOption.get
    val rowDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val colDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0),
        Vector(0.0),
        Vector(1.0)
      )
    )
    val spec = CpcaEstimatorSpec(
      blocks = Vector(CpcaBlock.GxH, CpcaBlock.G0xH),
      rankByBlock = Map(CpcaBlock.GxH -> k(2)),
      defaultComponents = Some(k(1)),
      rowMetric = Some(rowMetric),
      columnMetric = Some(colMetric),
      rowConstraint = CpcaConstraint.Basis(rowDesign),
      columnConstraint = CpcaConstraint.Basis(colDesign)
    )

    assertEquals(spec.requestedComponents(CpcaBlock.GxH).map(_.value), Some(2))
    assertEquals(spec.requestedComponents(CpcaBlock.G0xH).map(_.value), Some(1))
    assertEquals(spec.requestedComponentUpperBound.map(_.value), Some(2))
    assertEquals(spec.requestedComponentSummary, "GxH:2,G0xH:1")
    assert(spec.validate(sampleCount = 4, featureCount = 3).isRight)

    val wrongColumnRows = spec.copy(columnConstraint = CpcaConstraint.Basis(rowDesign))
    assert(wrongColumnRows.validate(sampleCount = 4, featureCount = 3).isLeft)

    val unselectedRank = spec.copy(rankByBlock = Map(CpcaBlock.GxH0 -> k(1)))
    assert(unselectedRank.validate(sampleCount = 4, featureCount = 3).isLeft)

    val impossibleRank = spec.copy(defaultComponents = Some(k(4)))
    assert(impossibleRank.validate(sampleCount = 4, featureCount = 3).isLeft)
  }

  test("CPCA block requests are smart-constructed and validate selected rank keys") {
    val request = CpcaBlockRequest
      .from(
        blocks = Vector(CpcaBlock.GxH, CpcaBlock.G0xH),
        rankByBlock = Map(CpcaBlock.GxH -> k(2)),
        defaultComponents = Some(k(1))
      )
      .toOption
      .get

    assertEquals(request.requestedComponents(CpcaBlock.GxH).map(_.value), Some(2))
    assertEquals(request.requestedComponents(CpcaBlock.G0xH).map(_.value), Some(1))
    assertEquals(request.requestedComponentSummary, "GxH:2,G0xH:1")

    val empty = CpcaBlockRequest.from(Vector.empty[CpcaBlock])
    assert(empty.swap.toOption.exists {
      case MultivarError.InvalidBlockPartition(detail) =>
        detail.contains("at least one block")
      case _ =>
        false
    })

    val unselectedRank = CpcaBlockRequest.from(
      blocks = Vector(CpcaBlock.GxH),
      rankByBlock = Map(CpcaBlock.G0xH -> k(1))
    )
    assert(unselectedRank.swap.toOption.exists {
      case MultivarError.InvalidBlockPartition(detail) =>
        detail.contains("not selected")
      case _ =>
        false
    })
  }

  test("estimator specs reject positive ranks for statically zero identity or zero blocks") {
    val identityResidual = CpcaEstimatorSpec(
      blocks = Vector(CpcaBlock.G0xH),
      defaultComponents = Some(k(1))
    )
    assert(identityResidual.validate(sampleCount = 4, featureCount = 3).swap.toOption.contains(MultivarError.InvalidComponentRequest(1, 0)))

    val zeroProject = CpcaEstimatorSpec(
      blocks = Vector(CpcaBlock.GxH),
      defaultComponents = Some(k(1)),
      rowConstraint = CpcaConstraint.Zero
    )
    assert(zeroProject.validate(sampleCount = 4, featureCount = 3).swap.toOption.contains(MultivarError.InvalidComponentRequest(1, 0)))
  }

  test("identity constraints reduce GxH to ordinary SVD") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 0.5),
        Vector(0.0, -1.0, 2.0),
        Vector(3.0, 0.5, -0.5),
        Vector(2.0, -2.0, 1.0)
      )
    )
    val diagram = DualityDiagram.from(MatrixView.dense(x)).toOption.get
    val fit = Cpca
      .fit(
        diagram,
        blocks = CpcaBlock.all,
        rankByBlock = Map(CpcaBlock.GxH -> k(3))
      )
      .toOption
      .get
    val svd = Svd.fit(MatrixView.dense(x), k(3)).toOption.get
    val gxH = fit.block(CpcaBlock.GxH).get

    assertVectorClose(gxH.d, svd.result.singularValues, 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH), CpcaMath.frobeniusNorm2(x), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH), 0.0, 1e-12)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH0), 0.0, 1e-12)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH0), 0.0, 1e-12)
  }

  test("direct CPCA fit rejects requested rank for structural zero blocks") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(-1.0, 0.5),
        Vector(0.0, 3.0)
      )
    )
    val diagram = DualityDiagram.from(MatrixView.dense(x)).toOption.get
    val fit = Cpca.fit(
      diagram,
      blocks = Vector(CpcaBlock.G0xH),
      defaultComponents = Some(k(1))
    )

    assert(fit.swap.toOption.contains(MultivarError.InvalidComponentRequest(1, 0)))
  }

  test("diagonal-metric constraints produce the four orthogonal CPCA blocks") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.5, 2.0, -1.0),
        Vector(1.5, -0.5, 1.0, 0.0),
        Vector(-1.0, 2.0, 0.5, 1.0),
        Vector(0.0, 1.0, -1.5, 2.0),
        Vector(2.0, -1.0, 0.0, 1.5)
      )
    )
    val rowSpace = MvSpace.of("cpca.rows", SpaceRole.Samples, x.rows).toOption.get
    val colSpace = MvSpace.of("cpca.columns", SpaceRole.Observed, x.cols).toOption.get
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.75, 1.5, 1.25, 0.5)), Some(rowSpace)).toOption.get
    val colMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.2, 0.8, 1.5, 0.6)), Some(colSpace)).toOption.get
    val diagram = DualityDiagram
      .from(MatrixView.dense(x), rowMetric = Some(rowMetric), columnMetric = Some(colMetric), rowSpace = Some(rowSpace), columnSpace = Some(colSpace))
      .toOption
      .get

    val rowDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val colDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val rowCon = CpcaConstraint.basis(IndexAxis.Row, rowSpace, rowDesign, rowMetric).toOption.get
    val colCon = CpcaConstraint.basis(IndexAxis.Column, colSpace, colDesign, colMetric).toOption.get
    val fit = Cpca
      .fit(
        diagram,
        rowConstraint = Some(rowCon),
        columnConstraint = Some(colCon),
        blocks = CpcaBlock.all
      )
      .toOption
      .get

    val zStar = Cpca.whiten(diagram.table, fit.rowMetricRoots, fit.columnMetricRoots, StoragePolicy.AllowDense).toOption.get
    val zH = rightProject(colCon, zStar)
    val b11 = rowCon.project(zH).toOption.get
    val b01 = MatrixOps.subtract(zH, b11)
    val zG = rowCon.project(zStar).toOption.get
    val b10 = MatrixOps.subtract(zG, b11)
    val b00 = MatrixOps.subtract(zStar, CpcaMath.add(CpcaMath.add(b11, b01), b10))

    val blocks = Vector(b11, b01, b10, b00)
    var left = 0
    while left < blocks.length do
      var right = left + 1
      while right < blocks.length do
        assertEqualsDouble(frobeniusInner(blocks(left), blocks(right)), 0.0, 1e-8)
        right += 1
      left += 1

    assertEqualsDouble(fit.partition.totalSS, CpcaMath.frobeniusNorm2(zStar), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH), CpcaMath.frobeniusNorm2(b11), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH), CpcaMath.frobeniusNorm2(b01), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH0), CpcaMath.frobeniusNorm2(b10), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH0), CpcaMath.frobeniusNorm2(b00), 1e-10)

    assertMatrixClose(fit.block(CpcaBlock.GxH).get.reconstructWhitened().toOption.get, b11, 1e-8)
    assertMatrixClose(fit.block(CpcaBlock.G0xH).get.reconstructWhitened().toOption.get, b01, 1e-8)
    assertMatrixClose(fit.block(CpcaBlock.GxH0).get.reconstructWhitened().toOption.get, b10, 1e-8)
    assertMatrixClose(fit.block(CpcaBlock.G0xH0).get.reconstructWhitened().toOption.get, b00, 1e-8)

    fit.blocks.values.foreach { block =>
      if block.rank > 0 then
        assertMetricOrthonormal(block.u, rowMetric, 1e-8)
        assertMetricOrthonormal(block.v, colMetric, 1e-8)
    }
  }

  test("CPCA block fits expose scores, original-scale reconstructions, and constraint coordinates") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.5, 2.0, -1.0),
        Vector(1.5, -0.5, 1.0, 0.0),
        Vector(-1.0, 2.0, 0.5, 1.0),
        Vector(0.0, 1.0, -1.5, 2.0),
        Vector(2.0, -1.0, 0.0, 1.5)
      )
    )
    val rowSpace = MvSpace.of("cpca.fit.rows", SpaceRole.Samples, x.rows).toOption.get
    val colSpace = MvSpace.of("cpca.fit.columns", SpaceRole.Observed, x.cols).toOption.get
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.75, 1.5, 1.25, 0.5)), Some(rowSpace)).toOption.get
    val colMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.2, 0.8, 1.5, 0.6)), Some(colSpace)).toOption.get
    val diagram = DualityDiagram
      .from(MatrixView.dense(x), rowMetric = Some(rowMetric), columnMetric = Some(colMetric), rowSpace = Some(rowSpace), columnSpace = Some(colSpace))
      .toOption
      .get
    val rowDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val colDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val rowCon = CpcaConstraint.basis(IndexAxis.Row, rowSpace, rowDesign, rowMetric).toOption.get
    val colCon = CpcaConstraint.basis(IndexAxis.Column, colSpace, colDesign, colMetric).toOption.get
    val fit = Cpca
      .fit(diagram, rowConstraint = Some(rowCon), columnConstraint = Some(colCon), blocks = CpcaBlock.all)
      .toOption
      .get

    // Full-rank original-scale block reconstructions sum back to the data table.
    val total = CpcaBlock.all
      .map(block => fit.block(block).get.reconstructOriginal().toOption.get)
      .reduce(CpcaMath.add)
    assertMatrixClose(total, x, 1e-8)

    val gxH = fit.block(CpcaBlock.GxH).get
    assertMatrixClose(gxH.scores, MatrixOps.scaleColumns(gxH.u, gxH.d), 1e-12)
    assertMatrixClose(
      gxH.reconstructOriginal().toOption.get,
      DoubleMatrix.multiply(gxH.scores, gxH.v.transpose),
      1e-10
    )

    // Coordinates exist exactly on projected basis-constraint sides and regenerate
    // the whitened factors through the constraint basis.
    val rowCoords = gxH.rowCoordinates.getOrElse(fail("expected row coordinates for GxH"))
    val colCoords = gxH.columnCoordinates.getOrElse(fail("expected column coordinates for GxH"))
    val rowBasis = rowCon.basis.getOrElse(fail("expected row constraint basis"))
    val colBasis = colCon.basis.getOrElse(fail("expected column constraint basis"))
    assertMatrixClose(DoubleMatrix.multiply(rowBasis, rowCoords), gxH.uStar, 1e-8)
    assertMatrixClose(DoubleMatrix.multiply(colBasis, colCoords), gxH.vStar, 1e-8)
    assertEquals(fit.block(CpcaBlock.G0xH).get.rowCoordinates, None)
    assertEquals(fit.block(CpcaBlock.G0xH0).get.columnCoordinates, None)
  }

  test("CPCA block fits truncate to fewer components than the block rank") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 0.5),
        Vector(0.0, -1.0, 2.0),
        Vector(3.0, 0.5, -0.5),
        Vector(2.0, -2.0, 1.0)
      )
    )
    val diagram = DualityDiagram.from(MatrixView.dense(x)).toOption.get
    val fit = Cpca
      .fit(diagram, rankByBlock = Map(CpcaBlock.GxH -> k(1)))
      .toOption
      .get
    val svd = Svd.fit(MatrixView.dense(x), k(3)).toOption.get
    val gxH = fit.block(CpcaBlock.GxH).get

    assertEquals(gxH.rank, 1)
    assertEqualsDouble(gxH.d(0), svd.result.singularValues(0), 1e-10)
    assertEqualsDouble(gxH.ss, svd.result.singularValues(0) * svd.result.singularValues(0), 1e-10)
    // The partition still accounts for the full block inertia, independent of the request.
    assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH), CpcaMath.frobeniusNorm2(x), 1e-10)
  }

  test("rank-deficient constraint designs resolve to reduced bases and zero designs demote to Zero") {
    val space = MvSpace.of("cpca.deficient.rows", SpaceRole.Samples, 4).toOption.get
    val metric = MvMetric.identity(4, Some(space)).toOption.get
    val collinear = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(1.0, 2.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val reduced = CpcaConstraint.basis(IndexAxis.Row, space, collinear, metric).toOption.get
    assertEquals(reduced.constraint, CpcaConstraint.Basis(collinear))
    assertEquals(reduced.rank, 1)

    val demoted = CpcaConstraint.basis(IndexAxis.Row, space, DoubleMatrix.zeros(4, 2), metric).toOption.get
    assertEquals(demoted.constraint, CpcaConstraint.Zero)
    assertEquals(demoted.rank, 0)
  }

  test("CPCA problems reject constraints resolved against a different metric than the diagram") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(-1.0, 0.5),
        Vector(0.0, 3.0),
        Vector(2.0, -1.0)
      )
    )
    val rowSpace = MvSpace.of("cpca.metric.rows", SpaceRole.Samples, 4).toOption.get
    val diagMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 1.0, 1.0, 1.0)), Some(rowSpace)).toOption.get
    val identityMetric = MvMetric.identity(4, Some(rowSpace)).toOption.get
    val diagram = DualityDiagram
      .from(MatrixView.dense(x), rowMetric = Some(diagMetric), rowSpace = Some(rowSpace))
      .toOption
      .get
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val colIdentity = CpcaConstraint.identity(IndexAxis.Column, diagram.columnSpace)

    val mismatched = CpcaConstraint.basis(IndexAxis.Row, rowSpace, design, identityMetric).toOption.get
    CpcaProblem.from(diagram, mismatched, colIdentity) match
      case Left(MultivarError.MetricMismatch(detail)) =>
        assert(detail.contains("row"), detail)
      case other =>
        fail(s"expected metric mismatch, got $other")

    val matched = CpcaConstraint.basis(IndexAxis.Row, rowSpace, design, diagMetric).toOption.get
    assert(CpcaProblem.from(diagram, matched, colIdentity).isRight)
  }

  test("zero row constraint assigns all identity-column inertia to G0xH") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(-1.0, 0.5),
        Vector(0.0, 3.0)
      )
    )
    val diagram = DualityDiagram.from(MatrixView.dense(x)).toOption.get
    val rowZero = CpcaConstraint.zero(IndexAxis.Row, diagram.rowSpace)
    val fit = Cpca
      .fit(
        diagram,
        rowConstraint = Some(rowZero),
        blocks = CpcaBlock.all
      )
      .toOption
      .get

    assertEquals(fit.block(CpcaBlock.GxH).get.rank, 0)
    assertEquals(fit.block(CpcaBlock.GxH0).get.rank, 0)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH), CpcaMath.frobeniusNorm2(x), 1e-10)
    assertEqualsDouble(partitionValue(fit, CpcaBlock.G0xH0), 0.0, 1e-12)
  }

  test("a basis-constrained block that is exactly zero records a zero block") {
    // Regression: an all-zero ROI under a Basis column constraint used to abort the
    // whole CPCA fit with SolverFailed instead of recording the zero block.
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 0.0, 1.0, 2.0),
        Vector(0.0, 0.0, -1.5, 0.5),
        Vector(0.0, 0.0, 2.0, -1.0),
        Vector(0.0, 0.0, 0.5, 1.5)
      )
    )
    val colSpace = MvSpace.of("cpca.zero.roi", SpaceRole.Observed, 4).toOption.get
    val diagram = DualityDiagram.from(MatrixView.dense(x), columnSpace = Some(colSpace)).toOption.get
    val colDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val colCon = CpcaConstraint.basis(IndexAxis.Column, colSpace, colDesign, diagram.columnMetric).toOption.get

    Cpca.fit(diagram, columnConstraint = Some(colCon), blocks = CpcaBlock.all) match
      case Right(fit) =>
        val gxH = fit.block(CpcaBlock.GxH).get
        assertEquals(gxH.rank, 0)
        assertEqualsDouble(gxH.ss, 0.0, 1e-12)
        assert(fit.block(CpcaBlock.GxH0).get.rank > 0)
        assertEqualsDouble(partitionValue(fit, CpcaBlock.GxH), 0.0, 1e-12)
      case Left(error) =>
        fail(s"expected the fit to succeed with a zero block, got $error")
  }
