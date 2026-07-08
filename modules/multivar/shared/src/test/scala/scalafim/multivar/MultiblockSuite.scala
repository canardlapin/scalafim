package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class MultiblockSuite extends munit.FunSuite:

  private val leftId = BlockId("left").toOption.get
  private val rightId = BlockId("right").toOption.get

  private val partition: BlockPartition =
    BlockPartition.from(
      Dimension(3).toOption.get,
      Vector(
        BlockSpec(leftId, IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get),
        BlockSpec(rightId, IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get)
      )
    ).toOption.get

  private def data: MatrixView =
    MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 10.0, 2.0),
          Vector(3.0, 20.0, 4.0),
          Vector(5.0, 30.0, 6.0)
        )
      )
    )

  private def assertMatrixClose(actual: DoubleMatrix, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1

  private def pass(cols: Int): FittedPreprocessor =
    FittedColumnAffine(cols, MatrixView.ones(cols), MatrixView.zeros(cols))

  test("blockwise preprocessing composes into global column order") {
    val weights = PreprocessSpec.scale(Vector(2.0)).toOption.get
    val fitted = BlockwisePreprocessor.fit(
      data,
      partition,
      Vector(
        BlockPreprocessSpec(leftId, PreprocessSpec.Center),
        BlockPreprocessSpec(rightId, weights)
      )
    ).toOption.get

    val transformed = fitted.transform(data).toOption.get

    assert(fitted.preprocessorFor(leftId).isDefined)
    assert(fitted.preprocessorFor(rightId).isDefined)
    assertMatrixClose(
      transformed.toDense().toOption.get,
      Vector(
        Vector(-2.0, 20.0, -2.0),
        Vector(0.0, 40.0, 0.0),
        Vector(2.0, 60.0, 2.0)
      ),
      1e-12
    )
  }

  test("BlockMap sums block projections and projectBlock uses restriction semantics") {
    val domain = MvSpace.of("multi", SpaceRole.Observed, 3).toOption.get
    val latent = MvSpace.of("shared", SpaceRole.Latent, 1).toOption.get
    val leftBlock = partition.block(leftId).get
    val rightBlock = partition.block(rightId).get
    val leftMap = MatrixMap.from(
      MvSpace.of("left-space", SpaceRole.Block, 2).toOption.get,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(1.0))),
      pass(2)
    ).toOption.get
    val rightMap = MatrixMap.from(
      MvSpace.of("right-space", SpaceRole.Block, 1).toOption.get,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(0.5))),
      pass(1)
    ).toOption.get
    val blockMap = BlockMap.from(
      domain,
      latent,
      partition,
      Vector(BlockMapComponent(leftBlock, leftMap), BlockMapComponent(rightBlock, rightMap))
    ).toOption.get

    val projected = blockMap.forward(data).toOption.get
    val leftProjected = blockMap.projectBlock(data, leftId).toOption.get
    val localLeftSecond = blockMap.projectBlock(data, leftId, IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get).toOption.get
    val restricted = blockMap.restrictInput(IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get).toOption.get
    val selected = data.selectColumns(IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get).toOption.get

    assertMatrixClose(projected, Vector(Vector(8.0), Vector(17.0), Vector(26.0)), 1e-12)
    assertMatrixClose(leftProjected, Vector(Vector(3.0), Vector(7.0), Vector(11.0)), 1e-12)
    assertMatrixClose(localLeftSecond, Vector(Vector(2.0), Vector(4.0), Vector(6.0)), 1e-12)
    assertMatrixClose(restricted.forward(selected).toOption.get, leftProjected.toRows, 1e-12)
  }

  test("MultiblockProjection delegates global and block projection") {
    val domain = MvSpace.of("multi", SpaceRole.Observed, 3).toOption.get
    val latent = MvSpace.of("shared", SpaceRole.Latent, 1).toOption.get
    val leftBlock = partition.block(leftId).get
    val rightBlock = partition.block(rightId).get
    val leftMap = MatrixMap.from(
      MvSpace.of("left-space", SpaceRole.Block, 2).toOption.get,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0))),
      pass(2)
    ).toOption.get
    val rightMap = MatrixMap.from(
      MvSpace.of("right-space", SpaceRole.Block, 1).toOption.get,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(1.0))),
      pass(1)
    ).toOption.get
    val blockMap = BlockMap.from(
      domain,
      latent,
      partition,
      Vector(BlockMapComponent(leftBlock, leftMap), BlockMapComponent(rightBlock, rightMap))
    ).toOption.get
    val projection = MultiblockProjection(blockMap, blockMap.forward(data).toOption.get)

    assertMatrixClose(projection.project(data).toOption.get, Vector(Vector(11.0), Vector(23.0), Vector(35.0)), 1e-12)
    assertMatrixClose(projection.projectBlock(data, rightId).toOption.get, Vector(Vector(10.0), Vector(20.0), Vector(30.0)), 1e-12)
  }

  test("CrossProjection transfer is typed and decoder-capability explicit") {
    given PseudoInverseSolver = PseudoInverseSolver.orthonormalColumns()

    val xDomain = MvSpace.of("x", SpaceRole.Observed, 2).toOption.get
    val yDomain = MvSpace.of("y", SpaceRole.Observed, 2).toOption.get
    val latent = MvSpace.of("latent", SpaceRole.Latent, 1).toOption.get
    val xMap = MatrixMap.from(
      xDomain,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0))),
      pass(2)
    ).toOption.get
    val yMap = MatrixMap.from(
      yDomain,
      latent,
      DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(1.0))),
      pass(2)
    ).toOption.get
    val xInput = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(2.0, 99.0), Vector(3.0, 88.0))))
    val yInput = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(0.0, 2.0), Vector(0.0, 3.0))))
    val projection = CrossProjection(
      xMap,
      yMap,
      latent,
      xMap.forward(xInput).toOption.get,
      yMap.forward(yInput).toOption.get
    )

    val transferred = projection.transfer(DomainSide.X, DomainSide.Y, xInput).toOption.get

    assertMatrixClose(transferred, Vector(Vector(0.0, 2.0), Vector(0.0, 3.0)), 1e-12)
  }

