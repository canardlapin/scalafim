package scalafim.multivar

import gale.linalg.DMat

final case class BlockProjectionProvenance(
    block: BlockId,
    globalFrame: ValueIdentity,
    blockFrame: ValueIdentity,
    blockSchema: ValueIdentity,
    combinationWeight: Double,
    semantic: SemanticProvenance
)

sealed trait BlockProjectionResult:
  def values: DMat
  def projectionProvenance: BlockProjectionProvenance

final case class UnweightedBlockScores private[multivar] (
    values: DMat,
    projectionProvenance: BlockProjectionProvenance
) extends BlockProjectionResult

final case class WeightedBlockContribution private[multivar] (
    values: DMat,
    projectionProvenance: BlockProjectionProvenance
) extends BlockProjectionResult

private final case class FittedBlockRuntime[
    GlobalFeature <: SemanticSpace,
    Component <: SemanticSpace
](
    local: BlockFunctionalFrame[GlobalFeature, Component],
    preprocessor: FittedPreprocessor,
    schema: FeatureSchema
)

/** Fitted, identity-bound facade over one operator block projection.
  *
  * The global transform uses the assembled weighted frame. Block projection uses
  * the original local frame, while block contribution applies its declared
  * combination weight. Arbitrary feature subsets delegate to the same fitted
  * restriction machinery as every other [[FittedFrameTransform]].
  */
final class FittedMultiblockProjection[
    GlobalFeature <: SemanticSpace,
    Component <: SemanticSpace
] private (
    val operatorProjection: OperatorBlockProjection[GlobalFeature, Component],
    val preprocessor: BlockwisePreprocessor,
    val analysis: FittedFrameTransform,
    val provenance: SemanticProvenance,
    private val fittedBlocks: Vector[FittedBlockRuntime[GlobalFeature, Component]]
):
  def partition: OperatorBlockPartition[GlobalFeature] =
    operatorProjection.partition

  def featureSchema: FeatureSchema =
    analysis.featureSchema

  def project(input: MatrixView): Either[MultivarError, DMat] =
    analysis.project(input)

  def restrictFeatures(
      columns: IndexSet
  ): Either[MultivarError, RestrictedFrameTransform[analysis.featureSpace.Id, analysis.componentSpace.Id]] =
    analysis.restrictFeatures(columns)

  def withExplicitSynthesis(
      decoder: DMat,
      identity: ValueIdentity
  ): Either[MultivarError, FittedBidirectionalTransform] =
    analysis.withExplicitSynthesis(decoder, identity)

  def withEuclideanSynthesis(
      ridge: Ridge
  ): Either[MultivarError, FittedBidirectionalTransform] =
    analysis.withEuclideanSynthesis(ridge)

  def blockSchema(id: BlockId): Either[MultivarError, FeatureSchema] =
    fittedBlock(id).map(_.schema)

  def preprocessorFor(id: BlockId): Either[MultivarError, FittedPreprocessor] =
    fittedBlock(id).map(_.preprocessor)

  def bindBlock(
      id: BlockId,
      values: MatrixView
  ): Either[MultivarError, IdentifiedFeatureMatrix] =
    fittedBlock(id).flatMap: fitted =>
      fitted.schema.mode match
        case FeatureIdentityMode.Positional => IdentifiedFeatureMatrix.from(values, fitted.schema)
        case FeatureIdentityMode.Named =>
          Left(
            MultivarError.FeatureIdentityMismatch(
              s"named input for block '${id.value}' must supply its fitted block schema explicitly"
            )
          )

  def bindBlock(
      id: BlockId,
      values: MatrixView,
      schema: FeatureSchema
  ): Either[MultivarError, IdentifiedFeatureMatrix] =
    fittedBlock(id).flatMap(_ => IdentifiedFeatureMatrix.from(values, schema))

  def projectBlock(
      id: BlockId,
      input: IdentifiedFeatureMatrix
  ): Either[MultivarError, UnweightedBlockScores] =
    for
      fitted <- fittedBlock(id)
      _ <- validateBlockInput(input, fitted)
      scores <- localScores(input.values, fitted)
    yield
      UnweightedBlockScores(
        scores,
        blockProvenance(fitted)
      )

  def blockContribution(
      id: BlockId,
      input: IdentifiedFeatureMatrix
  ): Either[MultivarError, WeightedBlockContribution] =
    projectBlock(id, input).flatMap: unweighted =>
      fittedBlock(id).map: fitted =>
        WeightedBlockContribution(
          MatrixOps.scale(unweighted.values, fitted.local.weight),
          blockProvenance(fitted)
        )

  private def fittedBlock(
      id: BlockId
  ): Either[MultivarError, FittedBlockRuntime[GlobalFeature, Component]] =
    fittedBlocks.find(_.local.block.spec.id == id).toRight(
      MultivarError.InvalidBlockPartition(s"unknown fitted block '${id.value}'")
    )

  private def validateBlockInput(
      input: IdentifiedFeatureMatrix,
      fitted: FittedBlockRuntime[GlobalFeature, Component]
  ): Either[MultivarError, Unit] =
    val expected = fitted.schema
    val actual = input.schema
    if actual.space != expected.space then
      Left(
        MultivarError.FeatureIdentityMismatch(
          s"input for block '${fitted.local.block.spec.id.value}' belongs to feature space " +
            s"'${actual.space.id.value}', expected '${expected.space.id.value}'"
        )
      )
    else if actual.valueIdentity != expected.valueIdentity then
      Left(MultivarError.FeatureIdentityMismatch("block input belongs to a foreign fitted feature schema"))
    else if actual.identities != expected.identities then
      Left(MultivarError.FeatureIdentityMismatch("block input feature identities or order do not match the fitted block"))
    else Right(())

  private def localScores(
      input: MatrixView,
      fitted: FittedBlockRuntime[GlobalFeature, Component]
  ): Either[MultivarError, DMat] =
    for
      processed <- fitted.preprocessor.transform(input)
      rows <- SpaceRef.of(
        s"${fitted.local.block.space.descriptor.id.value}.projection-rows",
        SpaceRole.Samples,
        input.rows
      )
      table <- fittedMultiblockSemantic(
        Op.fromMatrixView(
          processed,
          CoordinateEvidence.dual(fitted.local.block.space.evidence),
          CoordinateEvidence.primal(rows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.derived("fitted-block-input", fitted.local.frame.weights.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived(
              "project-fitted-block",
              Vector(fitted.local.frame.weights.valueIdentity)
            )
          )
        )
      )
      values <- fittedMultiblockSemantic(fitted.local.frame.scores(table).toDense)
    yield values

  private def blockProvenance(
      fitted: FittedBlockRuntime[GlobalFeature, Component]
  ): BlockProjectionProvenance =
    BlockProjectionProvenance(
      fitted.local.block.spec.id,
      analysis.frame.weights.valueIdentity,
      fitted.local.frame.weights.valueIdentity,
      fitted.schema.valueIdentity,
      fitted.local.weight,
      provenance
    )

object FittedMultiblockProjection:
  def from[
      GlobalFeature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      training: MatrixView,
      partition: OperatorBlockPartition[GlobalFeature],
      componentSpace: SpaceEvidence[Component],
      frames: Vector[BlockFunctionalFrame[GlobalFeature, Component]],
      preprocessor: BlockwisePreprocessor,
      method: String,
      requested: ComponentCount,
      featureIds: Option[Vector[FeatureId]] = None,
      rowIds: Option[Vector[RowId]] = None
  ): Either[MultivarError, FittedMultiblockProjection[GlobalFeature, Component]] =
    for
      projection <- OperatorBlockProjection.from(partition, componentSpace, frames, s"$method-operator-block-projection")
      _ <- validatePreprocessor(training, partition, preprocessor)
      weights <- assembleWeightedFrame(partition, componentSpace, frames)
      analysis <- FittedFrameTransform.fromTraining(
        training,
        weights,
        preprocessor,
        method,
        requested,
        featureIds = featureIds,
        rowIds = rowIds
      )
      fitted <- fittedRuntimes(frames, preprocessor, analysis)
      semantic = (projection.provenance ++ analysis.provenance).append(
        SemanticProvenanceEvent.Derived(
          "fit-multiblock-projection",
          frames.map(_.frame.weights.valueIdentity)
        )
      )
    yield new FittedMultiblockProjection(projection, preprocessor, analysis, semantic, fitted)

  private def validatePreprocessor[GlobalFeature <: SemanticSpace](
      training: MatrixView,
      partition: OperatorBlockPartition[GlobalFeature],
      preprocessor: BlockwisePreprocessor
  ): Either[MultivarError, Unit] =
    if training.cols != partition.globalSpace.dimension then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"multiblock training table has ${training.cols} columns, expected ${partition.globalSpace.dimension}"
        )
      )
    else if preprocessor.partition.blocks != partition.partition.blocks then
      Left(MultivarError.InvalidBlockPartition("fitted block preprocessor belongs to a different block partition"))
    else Right(())

  private def assembleWeightedFrame[
      GlobalFeature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      partition: OperatorBlockPartition[GlobalFeature],
      componentSpace: SpaceEvidence[Component],
      frames: Vector[BlockFunctionalFrame[GlobalFeature, Component]]
  ): Either[MultivarError, DMat] =
    val values = new Array[Double](partition.globalSpace.dimension * componentSpace.dimension)
    var blockIndex = 0
    var error = Option.empty[MultivarError]
    while blockIndex < frames.length && error.isEmpty do
      val fitted = frames(blockIndex)
      fittedMultiblockSemantic(fitted.frame.weights.toDense) match
        case Left(value) => error = Some(value)
        case Right(local) =>
          var localRow = 0
          while localRow < local.rows do
            val globalRow = fitted.block.spec.columns.indices(localRow)
            var component = 0
            while component < local.cols do
              values(globalRow * componentSpace.dimension + component) =
                fitted.weight * local(localRow, component)
              component += 1
            localRow += 1
      blockIndex += 1
    error match
      case Some(value) => Left(value)
      case None =>
        Right(
          GaleNumerics.matrixFromRowMajor(
            partition.globalSpace.dimension,
            componentSpace.dimension,
            values
          )
        )

  private def fittedRuntimes[
      GlobalFeature <: SemanticSpace,
      Component <: SemanticSpace
  ](
      frames: Vector[BlockFunctionalFrame[GlobalFeature, Component]],
      preprocessor: BlockwisePreprocessor,
      analysis: FittedFrameTransform
  ): Either[MultivarError, Vector[FittedBlockRuntime[GlobalFeature, Component]]] =
    MatrixOps.traverse(frames): local =>
      for
        fittedPreprocessor <- preprocessor.preprocessorFor(local.block.spec.id).toRight(
          MultivarError.InvalidBlockPartition(
            s"missing fitted preprocessor for block '${local.block.spec.id.value}'"
          )
        )
        schema <- FeatureSchema.restricted(
          analysis.featureSchema,
          local.block.space.descriptor,
          local.block.spec.columns.indices.map(analysis.featureSchema.identities),
          ValueIdentity.derived(
            s"fitted-block-schema-${local.block.spec.id.value}",
            analysis.featureSchema.valueIdentity,
            local.block.embedding.valueIdentity
          )
        )
      yield FittedBlockRuntime(local, fittedPreprocessor, schema)

private def fittedMultiblockSemantic[A](
    value: Either[SemanticError, A]
): Either[MultivarError, A] =
  value.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.InvalidMap(error.message)
