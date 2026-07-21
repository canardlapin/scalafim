package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

final case class BlockPreprocessSpec(id: BlockId, spec: PreprocessSpec)

/** Blockwise column preprocessing over a [[BlockPartition]].
  *
  * Construct via [[BlockwisePreprocessor.fit]], which guarantees that `global` is the
  * composition of `blockPreprocessors` laid out in global column order, so `transform`
  * (which uses `global`) and `preprocessorFor` (which uses the block list) always agree.
  */
final case class BlockwisePreprocessor private (
    partition: BlockPartition,
    blockPreprocessors: Vector[(BlockSpec, FittedPreprocessor)],
    global: FittedColumnAffine
) extends FittedPreprocessor:
  override def inputCols: Int =
    partition.totalSize.value

  override def transform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    global.transform(input, columns, policy)

  override def inverseTransform(
      input: MatrixView,
      columns: Option[IndexSet],
      policy: StoragePolicy
  ): Either[MultivarError, MatrixView] =
    global.inverseTransform(input, columns, policy)

  override def restrict(columns: IndexSet): Either[MultivarError, FittedPreprocessor] =
    global.restrict(columns)

  def preprocessorFor(blockId: BlockId): Option[FittedPreprocessor] =
    blockPreprocessors.find(_._1.id == blockId).map(_._2)

object BlockwisePreprocessor:
  def fit(
      input: MatrixView,
      partition: BlockPartition,
      specs: Vector[BlockPreprocessSpec],
      defaultSpec: PreprocessSpec = PreprocessSpec.Pass
  ): Either[MultivarError, BlockwisePreprocessor] =
    if input.cols != partition.totalSize.value then
      Left(MultivarError.MatrixShapeMismatch(s"input has ${input.cols} columns but partition covers ${partition.totalSize.value}"))
    else validateSpecIds(partition, specs).flatMap { _ =>
      val specById = specs.map(spec => spec.id -> spec.spec).toMap
      val globalScale = Array.fill(partition.totalSize.value)(1.0)
      val globalShift = new Array[Double](partition.totalSize.value)
      val fitted = Vector.newBuilder[(BlockSpec, FittedPreprocessor)]
      var blockIndex = 0
      var error = Option.empty[MultivarError]

      while blockIndex < partition.blocks.length && error.isEmpty do
        val block = partition.blocks(blockIndex)
        val spec = specById.getOrElse(block.id, defaultSpec)
        val fitResult =
          for
            blockInput <- input.selectColumns(block.columns)
            blockFit <- spec.fit(blockInput)
          yield blockFit
        fitResult match
          case Left(value) =>
            error = Some(value)
          case Right(blockFit: FittedColumnAffine) =>
            fitted += block -> blockFit
            var local = 0
            while local < block.columns.length do
              val global = block.columns.indices(local)
              globalScale(global) = blockFit.scale(local)
              globalShift(global) = blockFit.shift(local)
              local += 1
          case Right(_) =>
            error = Some(MultivarError.InvalidBlockPartition("blockwise preprocessing currently requires column-affine block preprocessors"))
        blockIndex += 1

      error match
        case Some(value) => Left(value)
        case None =>
          Right(
            BlockwisePreprocessor(
              partition,
              fitted.result(),
              FittedColumnAffine(
                partition.totalSize.value,
                GaleNumerics.vectorFromArray(globalScale),
                GaleNumerics.vectorFromArray(globalShift)
              )
            )
          )
    }

  private def validateSpecIds(
      partition: BlockPartition,
      specs: Vector[BlockPreprocessSpec]
  ): Either[MultivarError, Unit] =
    val knownIds = partition.blocks.map(_.id).toSet
    val seenIds = scala.collection.mutable.HashSet.empty[String]
    var i = 0
    var error = Option.empty[MultivarError]
    while i < specs.length && error.isEmpty do
      val id = specs(i).id
      if !knownIds.contains(id) then
        error = Some(MultivarError.InvalidBlockPartition(s"block preprocess spec references unknown block '${id.value}'"))
      else if seenIds.contains(id.value) then error = Some(MultivarError.DuplicateBlock(id))
      else seenIds += id.value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

/** One block of a feature-space partition represented by a typed embedding
  * `C_b* -> C*`. Selecting a block table and lifting a local functional frame
  * are therefore ordinary operator composition, not positional map magic.
  */
final class OperatorBlock[GlobalFeature <: SemanticSpace] private[multivar] (
    val spec: BlockSpec,
    val space: SpaceRef
)(
    val embedding: Op[
      Dual[space.Id],
      Dual[GlobalFeature],
      ConstraintOperatorRole,
      UncheckedEvidence
    ]
):
  def table[Rows <: SemanticSpace, E <: OperatorEvidence](
      source: OpTable[Rows, GlobalFeature, E]
  ): OpTable[Rows, space.Id, UncheckedEvidence] =
    embedding.andThen(source).retag(OperatorRoleWitness.table, "block-table")

  def liftFrame[Component <: SemanticSpace, E <: OperatorEvidence](
      frame: FunctionalFrame[space.Id, Component, E]
  ): OpFrame[GlobalFeature, Component, UncheckedEvidence] =
    frame.weights.andThen(embedding).retag(OperatorRoleWitness.frame, "lift-block-frame")

final class OperatorBlockPartition[GlobalFeature <: SemanticSpace] private[multivar] (
    val globalSpace: SpaceEvidence[GlobalFeature],
    val partition: BlockPartition,
    val blocks: Vector[OperatorBlock[GlobalFeature]],
    val provenance: SemanticProvenance
):
  require(blocks.map(_.spec) == partition.blocks, "operator blocks must preserve the declared partition order")

  def block(id: BlockId): Option[OperatorBlock[GlobalFeature]] =
    blocks.find(_.spec.id == id)

object OperatorBlockPartition:
  def from[GlobalFeature <: SemanticSpace](
      globalSpace: SpaceEvidence[GlobalFeature],
      partition: BlockPartition,
      provenanceLabel: String = "operator-block-partition"
  ): Either[MultivarError, OperatorBlockPartition[GlobalFeature]] =
    if partition.totalSize.value != globalSpace.dimension then
      Left(
        MultivarError.InvalidBlockPartition(
          s"partition covers ${partition.totalSize.value} features but '${globalSpace.id.value}' has ${globalSpace.dimension}"
        )
      )
    else if provenanceLabel.trim.isEmpty then
      Left(MultivarError.InvalidId("operator block provenance", provenanceLabel, "must be non-empty"))
    else
      val provenance = SemanticProvenance.source(provenanceLabel.trim)
      MatrixOps.traverse(partition.blocks) { spec =>
        val local = SpaceRef.of(
          s"${globalSpace.id.value}.${spec.id.value}",
          globalSpace.descriptor.role,
          spec.size
        )
        local.flatMap { blockSpace =>
          val values = new Array[Double](globalSpace.dimension * spec.size)
          var localColumn = 0
          while localColumn < spec.size do
            values(spec.columns.indices(localColumn) * spec.size + localColumn) = 1.0
            localColumn += 1
          val matrix = GaleNumerics.matrixFromRowMajor(globalSpace.dimension, spec.size, values)
          val identity = ValueIdentity.source(
            ValueId.unsafe(s"${globalSpace.id.value}.${spec.id.value}.block-embedding")
          )
          multiblockSemantic(
            Op.fromDense(
              matrix,
              CoordinateEvidence.dual(blockSpace.evidence),
              CoordinateEvidence.dual(globalSpace),
              OperatorRoleWitness.constraint,
              identity,
              provenance.append(SemanticProvenanceEvent.Derived("block-embedding", Vector.empty))
            )
          ).map(operator => new OperatorBlock(spec, blockSpace)(operator))
        }
      }.map(blocks => new OperatorBlockPartition(globalSpace, partition, blocks, provenance))

final class PreparedOperatorBlockPartition private[multivar] (
    val global: SpaceRef
)(
    val value: OperatorBlockPartition[global.Id]
)

object PreparedOperatorBlockPartition:
  def from(
      global: MvSpace,
      partition: BlockPartition,
      provenanceLabel: String = "operator-block-partition"
  ): Either[MultivarError, PreparedOperatorBlockPartition] =
    val ref = SpaceRef(global)
    OperatorBlockPartition
      .from(ref.evidence, partition, provenanceLabel)
      .map(value => new PreparedOperatorBlockPartition(ref)(value))

/** A local functional frame attached to exactly one typed block. */
final class BlockFunctionalFrame[
    GlobalFeature <: SemanticSpace,
    Component <: SemanticSpace
] private (
    val block: OperatorBlock[GlobalFeature],
    val weight: Double
)(
    val frame: FunctionalFrame[block.space.Id, Component, ? <: OperatorEvidence]
):
  require(weight.isFinite, "block functional-frame weight must be finite")

object BlockFunctionalFrame:
  def from[
      GlobalFeature <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      block: OperatorBlock[GlobalFeature],
      weight: Double,
      frame: FunctionalFrame[block.space.Id, Component, E]
  ): Either[MultivarError, BlockFunctionalFrame[GlobalFeature, Component]] =
    if !weight.isFinite then Left(MultivarError.NonFiniteValue("block functional-frame weight", 0, weight))
    else Right(new BlockFunctionalFrame(block, weight)(frame))

/** Combines typed block-local functional frames into one component score operator. Each local score is
  * derived from a block table and local functional frame; the combined score is
  * lifted as a `Score` operator on the shared component and row spaces.
  */
final class OperatorBlockProjection[
    GlobalFeature <: SemanticSpace,
    Component <: SemanticSpace
] private (
    val partition: OperatorBlockPartition[GlobalFeature],
    val componentSpace: SpaceEvidence[Component],
    val frames: Vector[BlockFunctionalFrame[GlobalFeature, Component]],
    val provenance: SemanticProvenance
):
  def liftedFrames: Vector[OpFrame[GlobalFeature, Component, UncheckedEvidence]] =
    frames.map(value => value.block.liftFrame(value.frame))

  def combinedScores[Rows <: SemanticSpace, E <: OperatorEvidence](
      table: OpTable[Rows, GlobalFeature, E]
  ): Either[MultivarError, Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence]] =
    if table.domain.descriptor.space != partition.globalSpace.descriptor then
      Left(
        MultivarError.InvalidMap(
          s"block projection expects feature space '${partition.globalSpace.id.value}', got '${table.domain.descriptor.space.id.value}'"
        )
      )
    else
      val out = new Array[Double](table.rows * componentSpace.dimension)
      var blockIndex = 0
      var error = Option.empty[MultivarError]
      while blockIndex < frames.length && error.isEmpty do
        val local = frames(blockIndex)
        multiblockSemantic(local.frame.scores(local.block.table(table)).toDense) match
          case Left(value) => error = Some(value)
          case Right(scores) =>
            val values = scores.copyData
            var index = 0
            while index < values.length do
              out(index) += local.weight * values(index)
              index += 1
        blockIndex += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val dense = GaleNumerics.matrixFromRowMajor(table.rows, componentSpace.dimension, out)
          multiblockSemantic(
            Op.fromDense(
              dense,
              CoordinateEvidence.primal(componentSpace),
              table.codomain,
              OperatorRoleWitness.score,
              ValueIdentity.derived("combined-block-scores", frames.map(_.frame.weights.valueIdentity)*),
              provenance.append(
                SemanticProvenanceEvent.Derived(
                  "combine-block-scores",
                  frames.map(_.frame.weights.valueIdentity)
                )
              )
            )
          )

object OperatorBlockProjection:
  def from[GlobalFeature <: SemanticSpace, Component <: SemanticSpace](
      partition: OperatorBlockPartition[GlobalFeature],
      componentSpace: SpaceEvidence[Component],
      frames: Vector[BlockFunctionalFrame[GlobalFeature, Component]],
      provenanceLabel: String = "operator-block-projection"
  ): Either[MultivarError, OperatorBlockProjection[GlobalFeature, Component]] =
    val expected = partition.blocks.map(_.spec.id)
    val actual = frames.map(_.block.spec.id)
    if expected != actual then
      Left(MultivarError.InvalidBlockPartition("block projection must provide one frame per block in partition order"))
    else if frames.exists(_.block.embedding.codomain.descriptor.space != partition.globalSpace.descriptor) then
      Left(MultivarError.InvalidBlockPartition("block projection frame belongs to a different global feature space"))
    else if provenanceLabel.trim.isEmpty then
      Left(MultivarError.InvalidId("operator block projection provenance", provenanceLabel, "must be non-empty"))
    else
      Right(
        new OperatorBlockProjection(
          partition,
          componentSpace,
          frames,
          SemanticProvenance.source(provenanceLabel.trim)
        )
      )

private def multiblockSemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map(error => MultivarError.InvalidMap(error.message))
