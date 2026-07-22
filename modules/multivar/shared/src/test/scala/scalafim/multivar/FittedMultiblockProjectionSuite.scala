package scalafim.multivar

import gale.linalg.DMat

class FittedMultiblockProjectionSuite extends munit.FunSuite:

  private val leftId = BlockId("left").toOption.get
  private val rightId = BlockId("right").toOption.get
  private val global = SpaceRef(MvSpace.of("fitted-multiblock", SpaceRole.Observed, 3).toOption.get)
  private val components = SpaceRef(MvSpace.of("fitted-multiblock-components", SpaceRole.Latent, 2).toOption.get)

  private val partition = BlockPartition
    .from(
      Dimension(3).toOption.get,
      Vector(
        BlockSpec(leftId, IndexSet.from(Vector(0, 2), IndexAxis.Feature).toOption.get),
        BlockSpec(rightId, IndexSet.from(Vector(1), IndexAxis.Feature).toOption.get)
      )
    )
    .toOption
    .get

  private val operatorPartition = OperatorBlockPartition
    .from(global.evidence, partition)
    .toOption
    .get

  private val training = MatrixView.dense(
    GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 10.0, 2.0),
        Vector(3.0, 20.0, 4.0),
        Vector(5.0, 30.0, 6.0)
      )
    )
  )

  private val fittedPreprocessor = BlockwisePreprocessor
    .fit(
      training,
      partition,
      Vector(
        BlockPreprocessSpec(leftId, PreprocessSpec.Center),
        BlockPreprocessSpec(
          rightId,
          PreprocessSpec.scale(Vector(2.0)).toOption.get
        )
      )
    )
    .toOption
    .get

  private val leftWeights = GaleNumerics.matrixFromRows(
    Vector(
      Vector(1.0, 0.5),
      Vector(-1.0, 2.0)
    )
  )
  private val rightWeights = GaleNumerics.matrixFromRows(
    Vector(Vector(0.25, -0.5))
  )

  private val blockFrames =
    val left = operatorPartition.block(leftId).get
    val right = operatorPartition.block(rightId).get
    val leftOperator = Op
      .fromDense(
        leftWeights,
        CoordinateEvidence.primal(components.evidence),
        CoordinateEvidence.dual(left.space.evidence),
        OperatorRoleWitness.frame,
        ValueIdentity.source(ValueId.unsafe("fitted-left-frame"))
      )
      .toOption
      .get
    val rightOperator = Op
      .fromDense(
        rightWeights,
        CoordinateEvidence.primal(components.evidence),
        CoordinateEvidence.dual(right.space.evidence),
        OperatorRoleWitness.frame,
        ValueIdentity.source(ValueId.unsafe("fitted-right-frame"))
      )
      .toOption
      .get
    Vector(
      BlockFunctionalFrame.from(left, 1.5, FunctionalFrame(leftOperator)).toOption.get,
      BlockFunctionalFrame.from(right, -2.0, FunctionalFrame(rightOperator)).toOption.get
    )

  private def fitted(method: String = "fitted-multiblock-projection") =
    FittedMultiblockProjection
      .from(
        training,
        operatorPartition,
        components.evidence,
        blockFrames,
        fittedPreprocessor,
        method,
        ComponentCount.unsafe(2),
        featureIds = Some(Vector("left-a", "right-a", "left-b").map(FeatureId.unsafe))
      )
      .toOption
      .get

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

  private def add(left: DMat, right: DMat): DMat =
    val values = left.copyData
    val rightValues = right.copyData
    var index = 0
    while index < values.length do
      values(index) += rightValues(index)
      index += 1
    GaleNumerics.matrixFromRowMajor(left.rows, left.cols, values)

  private def blockInput(
      projection: FittedMultiblockProjection[global.Id, components.Id],
      id: BlockId,
      values: MatrixView
  ): IdentifiedFeatureMatrix =
    projection
      .bindBlock(id, values, projection.blockSchema(id).toOption.get)
      .toOption
      .get

  test("weighted block contributions sum exactly to the global fitted projection"):
    val projection = fitted()
    val leftValues = training.selectColumns(partition.block(leftId).get.columns).toOption.get
    val rightValues = training.selectColumns(partition.block(rightId).get.columns).toOption.get
    val left = blockInput(projection, leftId, leftValues)
    val right = blockInput(projection, rightId, rightValues)
    val leftContribution = projection.blockContribution(leftId, left).toOption.get
    val rightContribution = projection.blockContribution(rightId, right).toOption.get
    val globalScores = projection.project(training).toOption.get

    assertMatrixClose(add(leftContribution.values, rightContribution.values), globalScores)
    assertEquals(leftContribution.projectionProvenance.combinationWeight, 1.5)
    assertEquals(rightContribution.projectionProvenance.combinationWeight, -2.0)

  test("unweighted block scores and weighted contributions are distinct result types and values"):
    val projection = fitted()
    val leftValues = training.selectColumns(partition.block(leftId).get.columns).toOption.get
    val input = blockInput(projection, leftId, leftValues)
    val scores: UnweightedBlockScores = projection.projectBlock(leftId, input).toOption.get
    val contribution: WeightedBlockContribution = projection.blockContribution(leftId, input).toOption.get

    assertMatrixClose(contribution.values, MatrixOps.scale(scores.values, 1.5))
    assert(Math.abs(scores.values(0, 1) - contribution.values(0, 1)) > 1e-12)

  test("block-local schemas reject global and foreign fitted feature identities"):
    val projection = fitted()
    val leftValues = training.selectColumns(partition.block(leftId).get.columns).toOption.get
    val globalInput = IdentifiedFeatureMatrix.from(training, projection.featureSchema).toOption.get
    assert(projection.projectBlock(leftId, globalInput).isLeft)

    val foreign = fitted("foreign-fitted-multiblock")
    val foreignInput = blockInput(foreign, leftId, leftValues)
    projection.projectBlock(leftId, foreignInput) match
      case Left(MultivarError.FeatureIdentityMismatch(detail)) => assert(detail.contains("foreign"))
      case other => fail(s"expected foreign fitted block schema failure, got $other")

    assert(projection.bindBlock(leftId, leftValues).isLeft)

  test("arbitrary cross-block partial projection delegates to FeatureRestriction"):
    val projection = fitted()
    val columns = IndexSet.from(Vector(2, 1), IndexAxis.Feature).toOption.get
    val restricted = projection.restrictFeatures(columns).toOption.get
    val partialValues = training.selectColumns(columns).toOption.get
    val input = restricted.restriction
      .bind(partialValues, restricted.restriction.restrictedSchema)
      .toOption
      .get
    val actual = restricted.contribution(input).toOption.get
    val processed = fittedPreprocessor.transform(training).toOption.get
    val processedPartial = processed.selectColumns(columns).toOption.get
    val selectedWeights = projection.analysis.frame.weights.toDense.toOption.get.selectRows(columns.indices)
    val expected = processedPartial.rightMultiply(selectedWeights).toOption.get

    assertMatrixClose(actual.values, expected)
    assertEquals(actual.projectionProvenance.selectedFeatures.map(_.value), Vector("left-b", "right-a"))

  test("fitted block preprocessors are reused and sparse block inputs preserve the projection law"):
    val projection = fitted()
    assert(projection.preprocessor eq fittedPreprocessor)
    assert(projection.preprocessorFor(leftId).toOption.get eq fittedPreprocessor.preprocessorFor(leftId).get)

    val denseLeft = training.selectColumns(partition.block(leftId).get.columns).toOption.get
    val sparseLeft = SparseMatrixView.fromRows(denseLeft.toDense().toOption.get.toRows).toOption.get
    val denseInput = blockInput(projection, leftId, denseLeft)
    val sparseInput = blockInput(projection, leftId, sparseLeft)

    assertMatrixClose(
      projection.projectBlock(leftId, sparseInput).toOption.get.values,
      projection.projectBlock(leftId, denseInput).toOption.get.values
    )

  test("foreign block partitions fail before fitted projection is constructed"):
    val reversed = BlockPartition
      .from(Dimension(3).toOption.get, partition.blocks.reverse)
      .toOption
      .get
    val foreignPreprocessor = BlockwisePreprocessor
      .fit(training, reversed, Vector.empty)
      .toOption
      .get
    val result = FittedMultiblockProjection.from(
      training,
      operatorPartition,
      components.evidence,
      blockFrames,
      foreignPreprocessor,
      "foreign-partition",
      ComponentCount.unsafe(2)
    )

    assert(result.swap.toOption.exists(_.message.contains("different block partition")))

  test("look-alike block frames remain bound to their exact operator partition"):
    val foreignPartition = OperatorBlockPartition.from(global.evidence, partition).toOption.get
    val foreignLeft = foreignPartition.block(leftId).get
    val foreignRight = foreignPartition.block(rightId).get
    val foreignLeftOperator = Op
      .fromDense(
        leftWeights,
        CoordinateEvidence.primal(components.evidence),
        CoordinateEvidence.dual(foreignLeft.space.evidence),
        OperatorRoleWitness.frame,
        ValueIdentity.source(ValueId.unsafe("foreign-left-frame"))
      )
      .toOption
      .get
    val foreignRightOperator = Op
      .fromDense(
        rightWeights,
        CoordinateEvidence.primal(components.evidence),
        CoordinateEvidence.dual(foreignRight.space.evidence),
        OperatorRoleWitness.frame,
        ValueIdentity.source(ValueId.unsafe("foreign-right-frame"))
      )
      .toOption
      .get
    val foreignFrames = Vector(
      BlockFunctionalFrame.from(foreignLeft, 1.5, FunctionalFrame(foreignLeftOperator)).toOption.get,
      BlockFunctionalFrame.from(foreignRight, -2.0, FunctionalFrame(foreignRightOperator)).toOption.get
    )
    val result = OperatorBlockProjection.from(operatorPartition, components.evidence, foreignFrames)

    assert(result.swap.toOption.exists(_.message.contains("different operator block partition")))
