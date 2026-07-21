package scalafim.multivar

import gale.linalg.DMat

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
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 10.0, 2.0),
          Vector(3.0, 20.0, 4.0),
          Vector(5.0, 30.0, 6.0)
        )
      )
    )

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
  test("operator block partitions select typed tables and lift local frames") {
    val global = SpaceRef(MvSpace.of("typed-multi", SpaceRole.Observed, 3).toOption.get)
    val rows = SpaceRef(MvSpace.of("typed-rows", SpaceRole.Samples, 3).toOption.get)
    val components = SpaceRef(MvSpace.of("typed-components", SpaceRole.Latent, 1).toOption.get)
    val typed = OperatorBlockPartition.from(global.evidence, partition).toOption.get
    val table = Op.fromMatrixView(
      data,
      CoordinateEvidence.dual(global.evidence),
      CoordinateEvidence.primal(rows.evidence),
      OperatorRoleWitness.table,
      ValueIdentity.source(ValueId.unsafe("typed-multiblock-table"))
    ).toOption.get
    val left = typed.block(leftId).get
    val right = typed.block(rightId).get
    val leftWeights = Op.fromDense(
      GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(1.0))),
      CoordinateEvidence.primal(components.evidence),
      CoordinateEvidence.dual(left.space.evidence),
      OperatorRoleWitness.frame,
      ValueIdentity.source(ValueId.unsafe("typed-left-frame"))
    ).toOption.get
    val rightWeights = Op.fromDense(
      GaleNumerics.matrixFromRows(Vector(Vector(1.0))),
      CoordinateEvidence.primal(components.evidence),
      CoordinateEvidence.dual(right.space.evidence),
      OperatorRoleWitness.frame,
      ValueIdentity.source(ValueId.unsafe("typed-right-frame"))
    ).toOption.get
    val leftFrame = BlockFunctionalFrame
      .from(left, 1.0, FunctionalFrame(leftWeights))
      .toOption
      .get
    val rightFrame = BlockFunctionalFrame
      .from(right, 0.5, FunctionalFrame(rightWeights))
      .toOption
      .get
    val projection = OperatorBlockProjection
      .from(typed, components.evidence, Vector(leftFrame, rightFrame))
      .toOption
      .get
    val scores = projection.combinedScores(table).toOption.get

    assertEquals(left.table(table).domain.descriptor.space, left.space.descriptor)
    assertMatrixClose(left.table(table).toDense.toOption.get, Vector(Vector(1.0, 2.0), Vector(3.0, 4.0), Vector(5.0, 6.0)), 0.0)
    assertMatrixClose(scores.toDense.toOption.get, Vector(Vector(8.0), Vector(17.0), Vector(26.0)), 1e-12)
    assertMatrixClose(
      projection.liftedFrames.head.toDense.toOption.get,
      Vector(Vector(1.0), Vector(0.0), Vector(1.0)),
      0.0
    )
    assertEquals(scores.role.value, OperatorRole.Score)
  }

  test("operator block partition and projection fail closed on foreign structure") {
    val global = SpaceRef(MvSpace.of("typed-multi", SpaceRole.Observed, 3).toOption.get)
    val wrong = MvSpace.of("wrong", SpaceRole.Observed, 4).toOption.get
    assert(PreparedOperatorBlockPartition.from(wrong, partition).isLeft)

    val typed = OperatorBlockPartition.from(global.evidence, partition).toOption.get
    val components = SpaceRef(MvSpace.of("typed-components", SpaceRole.Latent, 1).toOption.get)
    assert(
      OperatorBlockProjection
        .from(typed, components.evidence, Vector.empty)
        .swap
        .toOption
        .exists(_.message.contains("one frame per block"))
    )
  }
  test("BlockwisePreprocessor.fit rejects unknown and duplicate spec ids") {
    val ghost = BlockId("ghost").toOption.get
    val unknown = BlockwisePreprocessor.fit(
      data,
      partition,
      Vector(BlockPreprocessSpec(ghost, PreprocessSpec.Center))
    )
    assert(unknown.swap.toOption.exists(_.message.contains("unknown block")))

    val duplicate = BlockwisePreprocessor.fit(
      data,
      partition,
      Vector(
        BlockPreprocessSpec(leftId, PreprocessSpec.Center),
        BlockPreprocessSpec(leftId, PreprocessSpec.Pass)
      )
    )
    assert(duplicate.swap.toOption.exists(_.message.contains("duplicate block id")))
  }

  test("BlockwisePreprocessor inverseTransform and restrict round-trip") {
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
    val roundTrip = fitted.inverseTransform(transformed).toOption.get
    assertMatrixClose(
      roundTrip.toDense().toOption.get,
      Vector(
        Vector(1.0, 10.0, 2.0),
        Vector(3.0, 20.0, 4.0),
        Vector(5.0, 30.0, 6.0)
      ),
      1e-12
    )

    val columns = IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get
    val restricted = fitted.restrict(columns).toOption.get
    val selected = data.selectColumns(columns).toOption.get
    val restrictedTransformed = restricted.transform(selected).toOption.get
    assertMatrixClose(
      restrictedTransformed.toDense().toOption.get,
      Vector(
        Vector(-2.0, -2.0),
        Vector(0.0, 0.0),
        Vector(2.0, 2.0)
      ),
      1e-12
    )

    val restrictedRoundTrip = restricted.inverseTransform(restrictedTransformed).toOption.get
    assertMatrixClose(
      restrictedRoundTrip.toDense().toOption.get,
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 4.0),
        Vector(5.0, 6.0)
      ),
      1e-12
    )
  }
