package scalafim.multivar

import gale.linalg.DMat
import scalafim.linalg.FirstOrderConfig

enum StructuredMultiblockGlrmError:
  case InvalidDefinition(detail: String)
  case UnknownBlock(id: BlockId)
  case Generalized(error: GeneralizedLowRankError)
  case Encoding(error: LatentEncodingError)
  case Semantic(error: SemanticError)
  case Multivar(error: MultivarError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case UnknownBlock(id) => s"unknown structured GLRM block '${id.value}'"
      case Generalized(error) => error.message
      case Encoding(error) => error.message
      case Semantic(error) => error.message
      case Multivar(error) => error.message

opaque type BlockImportance = Double

object BlockImportance:
  def apply(value: Double): Either[StructuredMultiblockGlrmError, BlockImportance] =
    if value.isFinite && value > 0.0 then Right(value)
    else
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"block importance must be finite and positive, got $value"
        )
      )

  def unsafe(value: Double): BlockImportance =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (importance: BlockImportance)
    inline def value: Double = importance

enum BlockLossScaling:
  /** Sum over observed cells. A larger block has more influence by definition. */
  case ObservedEntrySum(importance: BlockImportance)

  /** Average within a block before applying its scientific importance. */
  case MeanObservedLoss(importance: BlockImportance)

  def estimand: String =
    this match
      case ObservedEntrySum(_) => "importance-weighted sum over observed cells"
      case MeanObservedLoss(_) => "importance-weighted mean over observed cells"

  def effectiveCoefficient(observedCount: Int): Either[StructuredMultiblockGlrmError, Double] =
    if observedCount <= 0 then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          "a block loss coefficient requires at least one point-observed cell"
        )
      )
    else
      this match
        case ObservedEntrySum(importance) => Right(importance.value)
        case MeanObservedLoss(importance) => Right(importance.value / observedCount.toDouble)

  /** The declared duplication law: identical copies with split importance have
    * exactly the same combined contribution as the original block.
    */
  def splitImportance(copies: Int): Either[StructuredMultiblockGlrmError, Vector[BlockLossScaling]] =
    if copies <= 0 then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block duplication requires positive copies, got $copies"))
    else
      BlockImportance(importance.value / copies.toDouble).map: divided =>
        Vector.fill(copies):
          this match
            case ObservedEntrySum(_) => ObservedEntrySum(divided)
            case MeanObservedLoss(_) => MeanObservedLoss(divided)

  private def importance: BlockImportance =
    this match
      case ObservedEntrySum(value) => value
      case MeanObservedLoss(value) => value

enum BlockNaturalGeometryKind:
  case Euclidean
  case CertifiedSpd

sealed trait BlockNaturalGeometry[Natural <: SemanticSpace]:
  def space: SpaceEvidence[Natural]
  def kind: BlockNaturalGeometryKind
  def valueIdentity: ValueIdentity

object BlockNaturalGeometry:
  def euclidean[Natural <: SemanticSpace](
      spaceValue: SpaceEvidence[Natural],
      identityValue: ValueIdentity
  ): BlockNaturalGeometry[Natural] =
    new BlockNaturalGeometry[Natural]:
      override val kind: BlockNaturalGeometryKind = BlockNaturalGeometryKind.Euclidean
      override val valueIdentity: ValueIdentity = identityValue
      override val space: SpaceEvidence[Natural] = spaceValue

  def certifiedSpd[Natural <: SemanticSpace](
      metric: OpMetric[Natural, CertifiedSpd]
  ): BlockNaturalGeometry[Natural] =
    new BlockNaturalGeometry[Natural]:
      override val kind: BlockNaturalGeometryKind = BlockNaturalGeometryKind.CertifiedSpd
      override val valueIdentity: ValueIdentity = metric.valueIdentity
      override val space: SpaceEvidence[Natural] =
        SpaceEvidence.unsafe(metric.domain.descriptor.space)

enum BlockStructuredPenaltyKind:
  case GraphTotalVariation
  case GraphSmoothness
  case LinearTotalVariation
  case LinearSmoothness

  def isNonsmooth: Boolean =
    this match
      case GraphTotalVariation | LinearTotalVariation => true
      case GraphSmoothness | LinearSmoothness => false

/** A block-local operator on the decoder's natural-parameter coordinates.
  * Its adjoint is derived from the same semantic operator, never supplied as an
  * unrelated matrix.
  */
final class BlockDecoderStructure[
    Natural <: SemanticSpace,
    Target <: SemanticSpace
] private (
    val blockId: BlockId,
    val naturalSpace: SpaceEvidence[Natural],
    val targetSpace: SpaceEvidence[Target],
    val operator: Op[Primal[Natural], Primal[Target], PenaltyOperatorRole, UncheckedEvidence],
    val kind: BlockStructuredPenaltyKind,
    val weight: PenaltyWeight,
    val valueIdentity: ValueIdentity
):
  val adjointIdentity: ValueIdentity = operator.dual.valueIdentity

  private[multivar] def penalty(decoder: DMat): Either[StructuredMultiblockGlrmError, Double] =
    operator
      .apply(decoder.transpose)
      .left
      .map(StructuredMultiblockGlrmError.Semantic.apply)
      .flatMap: transformed =>
        var value = 0.0
        var row = 0
        while row < transformed.rows do
          var column = 0
          while column < transformed.cols do
            val current = transformed(row, column)
            if kind.isNonsmooth then value += Math.abs(current)
            else value += 0.5 * current * current
            column += 1
          row += 1
        val weighted = weight.value * value
        if weighted.isFinite then Right(weighted)
        else
          Left(
            StructuredMultiblockGlrmError.InvalidDefinition(
              s"structured decoder penalty for block '${blockId.value}' is not finite"
            )
          )

object BlockDecoderStructure:
  def from[Natural <: SemanticSpace, Target <: SemanticSpace](
      blockId: BlockId,
      naturalSpace: SpaceEvidence[Natural],
      targetSpace: SpaceEvidence[Target],
      operator: Op[Primal[Natural], Primal[Target], PenaltyOperatorRole, UncheckedEvidence],
      kind: BlockStructuredPenaltyKind,
      weight: PenaltyWeight
  ): Either[StructuredMultiblockGlrmError, BlockDecoderStructure[Natural, Target]] =
    if operator.domain.descriptor.space != naturalSpace.descriptor then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"structured operator for block '${blockId.value}' has a foreign natural-parameter domain"
        )
      )
    else if operator.codomain.descriptor.space != targetSpace.descriptor then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"structured operator for block '${blockId.value}' has a foreign target space"
        )
      )
    else
      Right(
        new BlockDecoderStructure(
          blockId,
          naturalSpace,
          targetSpace,
          operator,
          kind,
          weight,
          ValueIdentity.derived(
            s"${kind.toString.toLowerCase}-block-structure",
            operator.valueIdentity,
            operator.dual.valueIdentity
          )
        )
      )

final class SharedRowBinding[Rows <: SemanticSpace] private (
    val rowSpace: SpaceEvidence[Rows],
    val keySetIdentity: ValueIdentity,
    val origin: AlignmentOrigin,
    val valueIdentity: ValueIdentity
)

object SharedRowBinding:
  def verified[Rows <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      keySetIdentity: ValueIdentity,
      origin: AlignmentOrigin = AlignmentOrigin.ObservedKeys
  ): Either[StructuredMultiblockGlrmError, SharedRowBinding[Rows]] =
    if origin == AlignmentOrigin.UnsafeAssumption then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          "a verified shared-row binding cannot be created from an unsafe positional assumption"
        )
      )
    else
      Right(
        new SharedRowBinding(
          rowSpace,
          keySetIdentity,
          origin,
          ValueIdentity.derived("verified-shared-glrm-rows", keySetIdentity)
        )
      )

sealed trait AlignedGlrmBlock[
    Rows <: SemanticSpace,
    Latent <: SemanticSpace
]:
  type Feature <: SemanticSpace
  type Natural <: SemanticSpace

  def id: BlockId
  def program: GeneralizedLowRankProgram[Rows, Feature]
  def decoder: FeatureDecoder[Feature, Latent]
  def naturalSpace: SpaceEvidence[Natural]
  def rowBinding: SharedRowBinding[Rows]
  def lossScaling: BlockLossScaling
  def geometry: BlockNaturalGeometry[Natural]
  def structures: Vector[BlockDecoderStructure[Natural, ? <: SemanticSpace]]
  def blockIdentity: ValueIdentity

  private[multivar] def evaluate(
      rowCodes: GlrmRowCodes[Rows, Latent]
  ): Either[StructuredMultiblockGlrmError, AlignedGlrmBlockObjective]

object AlignedGlrmBlock:
  def from[
      Rows <: SemanticSpace,
      Feature0 <: SemanticSpace,
      Latent <: SemanticSpace,
      Natural0 <: SemanticSpace
  ](
      id: BlockId,
      program: GeneralizedLowRankProgram[Rows, Feature0],
      decoder: FeatureDecoder[Feature0, Latent],
      naturalSpace: SpaceEvidence[Natural0],
      rowBinding: SharedRowBinding[Rows],
      lossScaling: BlockLossScaling,
      geometry: BlockNaturalGeometry[Natural0],
      structures: Vector[BlockDecoderStructure[Natural0, ? <: SemanticSpace]] =
        Vector.empty[BlockDecoderStructure[Natural0, ? <: SemanticSpace]]
  ): Either[
    StructuredMultiblockGlrmError,
    AlignedGlrmBlock[Rows, Latent] { type Feature = Feature0; type Natural = Natural0 }
  ] =
    val rowPenalty = program.factorPenalties.find(_.target == GlrmFactorTarget.RowCodes)
    if program.observations.rowSpace.descriptor != rowBinding.rowSpace.descriptor then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"block '${id.value}' observations do not use its declared shared-row binding"
        )
      )
    else if !decoder.layout.sameStructure(program.layout) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block '${id.value}' decoder uses a foreign layout"))
    else if decoder.latentSpace.dimension <= 0 then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block '${id.value}' has an empty latent space"))
    else if naturalSpace.dimension != program.layout.naturalDimension then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"block '${id.value}' natural space has dimension ${naturalSpace.dimension}, " +
            s"expected ${program.layout.naturalDimension}"
        )
      )
    else if geometry.space.descriptor != naturalSpace.descriptor then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block '${id.value}' geometry uses a foreign space"))
    else if structures.exists(_.blockId != id) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block '${id.value}' contains foreign structured penalties"))
    else if structures.exists(_.naturalSpace.descriptor != naturalSpace.descriptor) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition(s"block '${id.value}' structure uses a foreign natural space"))
    else
      rowPenalty match
        case Some(_) =>
          Left(
            StructuredMultiblockGlrmError.InvalidDefinition(
              s"block '${id.value}' cannot own a row-code penalty; shared-row penalties belong to the multiblock program"
            )
          )
        case None =>
          val blockIdValue = id
          val programValue = program
          val decoderValue = decoder
          val naturalSpaceValue = naturalSpace
          val rowBindingValue = rowBinding
          val lossScalingValue = lossScaling
          val geometryValue = geometry
          val structuresValue = structures
          val inputs = Vector(
            program.programIdentity,
            decoder.valueIdentity,
            rowBinding.valueIdentity,
            geometry.valueIdentity
          ) ++ structures.map(_.valueIdentity)
          val identity = ValueIdentity.derived(
            s"aligned-glrm-block-${id.value}-${lossScaling.estimand}",
            inputs*
          )
          Right(
            new AlignedGlrmBlock[Rows, Latent]:
              type Feature = Feature0
              type Natural = Natural0

              override val id: BlockId = blockIdValue
              override val program: GeneralizedLowRankProgram[Rows, Feature0] = programValue
              override val decoder: FeatureDecoder[Feature0, Latent] = decoderValue
              override val naturalSpace: SpaceEvidence[Natural0] = naturalSpaceValue
              override val rowBinding: SharedRowBinding[Rows] = rowBindingValue
              override val lossScaling: BlockLossScaling = lossScalingValue
              override val geometry: BlockNaturalGeometry[Natural0] = geometryValue
              override val structures: Vector[BlockDecoderStructure[Natural0, ? <: SemanticSpace]] = structuresValue
              override val blockIdentity: ValueIdentity = identity

              override private[multivar] def evaluate(
                  rowCodes: GlrmRowCodes[Rows, Latent]
              ): Either[StructuredMultiblockGlrmError, AlignedGlrmBlockObjective] =
                for
                  factors <- GlrmFactors.from(rowCodes, decoder).left.map(StructuredMultiblockGlrmError.Generalized.apply)
                  raw <- program.evaluate(factors).left.map(StructuredMultiblockGlrmError.Generalized.apply)
                  coefficient <- lossScaling.effectiveCoefficient(program.observations.observedCount)
                  structured <- structuredPenalty(decoder.values, structures)
                yield
                  AlignedGlrmBlockObjective(
                    id,
                    program.observations.observedCount,
                    lossScaling,
                    coefficient,
                    raw.observedEntryLoss,
                    coefficient * raw.observedEntryLoss,
                    raw.decoderPenalty,
                    structured,
                    blockIdentity,
                    structures.map(structure => structure.valueIdentity -> structure.adjointIdentity)
                  )
          )

final case class AlignedGlrmBlockObjective(
    block: BlockId,
    observedCount: Int,
    scaling: BlockLossScaling,
    effectiveLossCoefficient: Double,
    rawObservedLoss: Double,
    weightedObservedLoss: Double,
    decoderPenalty: Double,
    structuredPenalty: Double,
    blockIdentity: ValueIdentity,
    structureAdjoints: Vector[(ValueIdentity, ValueIdentity)]
):
  require(observedCount > 0)
  require(effectiveLossCoefficient.isFinite && effectiveLossCoefficient > 0.0)
  require(rawObservedLoss.isFinite && rawObservedLoss >= 0.0)
  require(weightedObservedLoss.isFinite && weightedObservedLoss >= 0.0)
  require(decoderPenalty.isFinite && decoderPenalty >= 0.0)
  require(structuredPenalty.isFinite && structuredPenalty >= 0.0)

  def total: Double = weightedObservedLoss + decoderPenalty + structuredPenalty

final case class SharedGlrmScores(
    values: DMat,
    rowBinding: ValueIdentity,
    sourceCodes: ValueIdentity,
    programIdentity: ValueIdentity
)

final case class BlockGlrmScoreView(
    block: BlockId,
    values: DMat,
    blockIdentity: ValueIdentity,
    sharedCodes: ValueIdentity,
    lossScaling: BlockLossScaling
)

final case class AlignedGlrmObjective(
    blocks: Vector[AlignedGlrmBlockObjective],
    sharedRowPenalty: Double
):
  require(blocks.nonEmpty)
  require(sharedRowPenalty.isFinite && sharedRowPenalty >= 0.0)

  def total: Double = blocks.map(_.total).sum + sharedRowPenalty

final case class AlignedGlrmEvaluation(
    sharedScores: SharedGlrmScores,
    blockScores: Vector[BlockGlrmScoreView],
    objective: AlignedGlrmObjective,
    resultIdentity: ValueIdentity
)

/** A shared-score multiblock GLRM. Block objectives are heterogeneous, but the
  * row code is one named parameter and its penalty is applied exactly once.
  */
final class AlignedSharedScoreGlrm[
    Rows <: SemanticSpace,
    Latent <: SemanticSpace
] private (
    val rowBinding: SharedRowBinding[Rows],
    val latentSpace: SpaceEvidence[Latent],
    val blocks: Vector[AlignedGlrmBlock[Rows, Latent]],
    val sharedRowPenalties: Vector[GlrmFactorPenaltyTerm],
    val programIdentity: ValueIdentity
):
  def block(id: BlockId): Either[StructuredMultiblockGlrmError, AlignedGlrmBlock[Rows, Latent]] =
    blocks.find(_.id == id).toRight(StructuredMultiblockGlrmError.UnknownBlock(id))

  def evaluate(
      rowCodes: GlrmRowCodes[Rows, Latent]
  ): Either[StructuredMultiblockGlrmError, AlignedGlrmEvaluation] =
    if rowCodes.rowSpace.descriptor != rowBinding.rowSpace.descriptor then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("shared GLRM row codes use a foreign row space"))
    else if rowCodes.latentSpace.descriptor != latentSpace.descriptor then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("shared GLRM row codes use a foreign latent space"))
    else
      traverseStructured(blocks)(_.evaluate(rowCodes)).map: blockObjectives =>
        val sharedPenalty = factorPenaltyValue(sharedRowPenalties, rowCodes.values)
        val resultIdentity = ValueIdentity.derived(
          "aligned-shared-score-glrm-evaluation",
          programIdentity,
          rowCodes.valueIdentity
        )
        AlignedGlrmEvaluation(
          SharedGlrmScores(
            rowCodes.values,
            rowBinding.valueIdentity,
            rowCodes.valueIdentity,
            programIdentity
          ),
          blocks.map: current =>
            BlockGlrmScoreView(
              current.id,
              rowCodes.values,
              current.blockIdentity,
              rowCodes.valueIdentity,
              current.lossScaling
            ),
          AlignedGlrmObjective(blockObjectives, sharedPenalty),
          resultIdentity
        )

  def fittedEncoder(
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): FittedAlignedMultiblockEncoder[Rows, Latent] =
    new FittedAlignedMultiblockEncoder(this, config)

object AlignedSharedScoreGlrm:
  def from[Rows <: SemanticSpace, Latent <: SemanticSpace](
      rowBinding: SharedRowBinding[Rows],
      latentSpace: SpaceEvidence[Latent],
      blocks: Vector[AlignedGlrmBlock[Rows, Latent]],
      sharedRowPenalties: Vector[GlrmFactorPenaltyTerm] = Vector.empty
  ): Either[StructuredMultiblockGlrmError, AlignedSharedScoreGlrm[Rows, Latent]] =
    if blocks.isEmpty then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("an aligned shared-score GLRM requires at least one block"))
    else if blocks.map(_.id).distinct.length != blocks.length then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("aligned shared-score GLRM block ids must be distinct"))
    else if blocks.exists(_.rowBinding.valueIdentity != rowBinding.valueIdentity) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("every block must use the exact declared shared-row binding"))
    else if blocks.exists(_.decoder.latentSpace.descriptor != latentSpace.descriptor) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("every block decoder must use the shared latent space"))
    else if sharedRowPenalties.exists(_.target != GlrmFactorTarget.RowCodes) then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("shared multiblock penalties may target only row codes"))
    else if sharedRowPenalties.map(_.valueIdentity).distinct.length != sharedRowPenalties.length then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("shared row-penalty identities must be distinct"))
    else
      val identity = ValueIdentity.derived(
        "aligned-shared-score-glrm",
        (Vector(rowBinding.valueIdentity) ++ blocks.map(_.blockIdentity) ++ sharedRowPenalties.map(_.valueIdentity))*
      )
      Right(new AlignedSharedScoreGlrm(rowBinding, latentSpace, blocks, sharedRowPenalties, identity))

enum StructuredMultiblockFamily:
  case AlignedSharedScores
  case IndependentDirectSum
  case HubAlignedEntities

/** The three row semantics share no implicit conversion. In particular, a
  * direct sum or hub-aligned study cannot be passed to a shared-row solver.
  */
sealed trait StructuredMultiblockStudy:
  def family: StructuredMultiblockFamily

object StructuredMultiblockStudy:
  final case class Aligned[Rows <: SemanticSpace, Latent <: SemanticSpace](
      program: AlignedSharedScoreGlrm[Rows, Latent]
  ) extends StructuredMultiblockStudy:
    val family: StructuredMultiblockFamily = StructuredMultiblockFamily.AlignedSharedScores

  final case class DirectSum(study: DirectSumStudy) extends StructuredMultiblockStudy:
    val family: StructuredMultiblockFamily = StructuredMultiblockFamily.IndependentDirectSum

  final case class HubAligned[E <: SemanticSpace](study: EntityAlignedStudy[E]) extends StructuredMultiblockStudy:
    val family: StructuredMultiblockFamily = StructuredMultiblockFamily.HubAlignedEntities

sealed trait PartialAlignedBlockObservation[
    TrainingRows <: SemanticSpace,
    NewRows <: SemanticSpace,
    Latent <: SemanticSpace
]:
  type Feature <: SemanticSpace

  def blockId: BlockId
  def blockIdentity: ValueIdentity
  def observations: ObservationPattern[NewRows, Feature]
  def featureSpecs: Vector[GlrmFeatureSpec]
  def decoder: FeatureDecoder[Feature, Latent]
  def lossScaling: BlockLossScaling

  private[multivar] def contribution(
      code: FittedLatentCode[Latent]
  ): Either[StructuredMultiblockGlrmError, EncodedGlrmBlockContribution]

object PartialAlignedBlockObservation:
  def from[
      TrainingRows <: SemanticSpace,
      NewRows <: SemanticSpace,
      Feature0 <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      block: AlignedGlrmBlock[TrainingRows, Latent] { type Feature = Feature0 },
      observations: ObservationPattern[NewRows, Feature0]
  ): Either[
    StructuredMultiblockGlrmError,
    PartialAlignedBlockObservation[TrainingRows, NewRows, Latent] { type Feature = Feature0 }
  ] =
    if observations.rowSpace.dimension != 1 then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("multiblock partial encoding requires one new row"))
    else if observations.featureSpace.descriptor != block.program.layout.featureSpace.descriptor then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"partial observations for block '${block.id.value}' use a foreign feature space"
        )
      )
    else
      validatePartialValues(block.program.layout, observations).map: _ =>
        val blockIdValue = block.id
        val blockIdentityValue = block.blockIdentity
        val observationsValue = observations
        val featureSpecsValue = block.program.layout.features
        val decoderValue = block.decoder
        val lossScalingValue = block.lossScaling
        new PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]:
          type Feature = Feature0

          override val blockId: BlockId = blockIdValue
          override val blockIdentity: ValueIdentity = blockIdentityValue
          override val observations: ObservationPattern[NewRows, Feature0] = observationsValue
          override val featureSpecs: Vector[GlrmFeatureSpec] = featureSpecsValue
          override val decoder: FeatureDecoder[Feature0, Latent] = decoderValue
          override val lossScaling: BlockLossScaling = lossScalingValue

          override private[multivar] def contribution(
              code: FittedLatentCode[Latent]
          ): Either[StructuredMultiblockGlrmError, EncodedGlrmBlockContribution] =
            val rowValues = Array.tabulate(code.values.length)(code.values(_))
            for
              rowCodes <- GlrmRowCodes
                .from(
                  observations.rowSpace,
                  decoder.latentSpace,
                  GaleNumerics.matrixFromRowMajor(1, code.values.length, rowValues),
                  ValueIdentity.derived("multiblock-encoded-row", code.valueIdentity)
                )
                .left
                .map(StructuredMultiblockGlrmError.Generalized.apply)
              factors <- GlrmFactors.from(rowCodes, decoder).left.map(StructuredMultiblockGlrmError.Generalized.apply)
              rawLoss <- partialObservedLoss(observations, decoder.layout, factors)
              decoded <- traverseStructured(Vector.range(0, featureSpecs.length)): feature =>
                factors
                  .decoded(0, feature)
                  .left
                  .map(StructuredMultiblockGlrmError.Generalized.apply)
                  .map(prediction => DecodedLatentFeature(featureSpecs(feature).id, prediction))
              coefficient <-
                if observations.observedCount == 0 then Right(0.0)
                else lossScaling.effectiveCoefficient(observations.observedCount)
            yield
              EncodedGlrmBlockContribution(
                blockId,
                observations.observedCount,
                coefficient,
                rawLoss,
                coefficient * rawLoss,
                decoded,
                blockIdentity,
                observations.valueIdentity
              )

final case class EncodedGlrmBlockContribution(
    block: BlockId,
    observedCount: Int,
    effectiveLossCoefficient: Double,
    rawObservedLoss: Double,
    weightedObservedLoss: Double,
    decoded: Vector[DecodedLatentFeature],
    blockIdentity: ValueIdentity,
    observationIdentity: ValueIdentity
):
  require(observedCount >= 0)
  require(effectiveLossCoefficient.isFinite && effectiveLossCoefficient >= 0.0)
  require(rawObservedLoss.isFinite && rawObservedLoss >= 0.0)
  require(weightedObservedLoss.isFinite && weightedObservedLoss >= 0.0)

final case class FittedAlignedMultiblockEncoding[Latent <: SemanticSpace](
    global: FittedLatentEncoding[Latent],
    blocks: Vector[EncodedGlrmBlockContribution],
    weightedBlockLoss: Double,
    rowPenalty: Double,
    programIdentity: ValueIdentity,
    resultIdentity: ValueIdentity
):
  require(blocks.nonEmpty)
  require(weightedBlockLoss.isFinite && weightedBlockLoss >= 0.0)
  require(rowPenalty.isFinite && rowPenalty >= 0.0)

  def objective: Double = weightedBlockLoss + rowPenalty

final class FittedAlignedMultiblockEncoder[
    TrainingRows <: SemanticSpace,
    Latent <: SemanticSpace
] private[multivar] (
    val program: AlignedSharedScoreGlrm[TrainingRows, Latent],
    val config: FirstOrderConfig
):
  def encode[NewRows <: SemanticSpace](
      inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]
  ): Either[StructuredMultiblockGlrmError, FittedAlignedMultiblockEncoding[Latent]] =
    for
      ordered <- validateEncodingInputs(inputs)
      compiled <- compileEncoding(ordered)
      global <- compiled.encoder.encode(compiled.observations).left.map(StructuredMultiblockGlrmError.Encoding.apply)
      contributions <- traverseStructured(ordered)(_.contribution(global.code))
      blockLoss = contributions.map(_.weightedObservedLoss).sum
      _ <- requireObjectiveAgreement(blockLoss, global.objective.observedEntryLoss)
      resultIdentity = ValueIdentity.derived(
        "fitted-aligned-multiblock-encoding",
        program.programIdentity,
        global.resultIdentity
      )
    yield
      FittedAlignedMultiblockEncoding(
        global,
        contributions,
        blockLoss,
        global.objective.rowPenalty,
        program.programIdentity,
        resultIdentity
      )

  private def validateEncodingInputs[NewRows <: SemanticSpace](
      inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]
  ): Either[StructuredMultiblockGlrmError, Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]] =
    val ids = inputs.map(_.blockId)
    val expected = program.blocks.map(_.id)
    if ids.distinct.length != ids.length then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("partial multiblock observations contain duplicate block ids"))
    else if ids.toSet != expected.toSet then
      Left(
        StructuredMultiblockGlrmError.InvalidDefinition(
          s"partial multiblock observations must explicitly cover blocks ${expected.map(_.value).mkString(", ")}"
        )
      )
    else if inputs.map(_.observations.rowSpace.descriptor).distinct.length != 1 then
      Left(StructuredMultiblockGlrmError.InvalidDefinition("partial multiblock observations use misaligned new-row spaces"))
    else
      val byId = inputs.map(input => input.blockId -> input).toMap
      val ordered = expected.map(byId)
      val foreign = ordered.zip(program.blocks).find: (input, block) =>
        input.blockIdentity != block.blockIdentity
      foreign match
        case Some((input, _)) =>
          Left(
            StructuredMultiblockGlrmError.InvalidDefinition(
              s"partial observations for block '${input.blockId.value}' are bound to a foreign fitted block"
            )
          )
        case None => Right(ordered)

  private def compileEncoding[NewRows <: SemanticSpace](
      inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]
  ): Either[StructuredMultiblockGlrmError, CompiledMultiblockEncoding[NewRows, Latent]] =
    val featureCount = inputs.map(_.featureSpecs.length).sum
    for
      featureSpace <- SpaceRef
        .of(
          s"${program.rowBinding.rowSpace.id.value}.partial-multiblock-features",
          SpaceRole.Observed,
          featureCount
        )
        .left
        .map(StructuredMultiblockGlrmError.Multivar.apply)
      specifications <- prefixedFeatureSpecifications(inputs)
      layout <- GlrmFeatureLayout
        .from(
          featureSpace.evidence,
          specifications,
          ValueIdentity.derived("partial-multiblock-layout", inputs.map(_.blockIdentity)*)
        )
        .left
        .map(StructuredMultiblockGlrmError.Generalized.apply)
      decoder <- FeatureDecoder
        .from(
          layout,
          program.latentSpace,
          concatenateDecoders(inputs, layout.naturalDimension),
          ValueIdentity.derived("partial-multiblock-decoder", inputs.map(_.decoder.valueIdentity)*)
        )
        .left
        .map(StructuredMultiblockGlrmError.Generalized.apply)
      cells <- scaledObservationCells(inputs)
      compiledObservations <- ObservationPattern
        .from(
          inputs.head.observations.rowSpace,
          featureSpace.evidence,
          cells,
          ValueIdentity.derived("partial-multiblock-observations", inputs.map(_.observations.valueIdentity)*)
        )
        .left
        .map(StructuredMultiblockGlrmError.Generalized.apply)
      compiledEncoder <- FittedLatentEncoder
        .from(decoder, program.sharedRowPenalties, config)
        .left
        .map(StructuredMultiblockGlrmError.Encoding.apply)
    yield
      new CompiledMultiblockEncoding[NewRows, Latent]:
        type Feature = featureSpace.Id
        override val encoder: FittedLatentEncoder[featureSpace.Id, Latent] = compiledEncoder
        override val observations: ObservationPattern[NewRows, featureSpace.Id] = compiledObservations

private sealed trait CompiledMultiblockEncoding[
    Rows <: SemanticSpace,
    Latent <: SemanticSpace
]:
  type Feature <: SemanticSpace
  def encoder: FittedLatentEncoder[Feature, Latent]
  def observations: ObservationPattern[Rows, Feature]

private def structuredPenalty[Natural <: SemanticSpace](
    decoder: DMat,
    structures: Vector[BlockDecoderStructure[Natural, ? <: SemanticSpace]]
): Either[StructuredMultiblockGlrmError, Double] =
  traverseStructured(structures)(_.penalty(decoder)).map(_.sum)

private def factorPenaltyValue(terms: Vector[GlrmFactorPenaltyTerm], matrix: DMat): Double =
  terms.foldLeft(0.0): (total, term) =>
    total + term.weight.value * term.functional.value(matrix)

private def validatePartialValues[Rows <: SemanticSpace, Feature <: SemanticSpace](
    layout: GlrmFeatureLayout[Feature],
    observations: ObservationPattern[Rows, Feature]
): Either[StructuredMultiblockGlrmError, Unit] =
  var feature = 0
  var failure = Option.empty[StructuredMultiblockGlrmError]
  while feature < layout.features.length && failure.isEmpty do
    observations.cellUnsafe(0, feature) match
      case ObservationCell.Observed(value, _) =>
        val specification = layout.featureUnsafe(feature)
        specification.domain.validate(value).left.foreach: detail =>
          failure = Some(
            StructuredMultiblockGlrmError.Generalized(
              GeneralizedLowRankError.InvalidObservedValue(specification.id, 0, value, detail)
            )
          )
      case _ => ()
    feature += 1
  failure.toLeft(())

private def partialObservedLoss[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
](
    observations: ObservationPattern[Rows, Feature],
    layout: GlrmFeatureLayout[Feature],
    factors: GlrmFactors[Rows, Feature, Latent]
): Either[StructuredMultiblockGlrmError, Double] =
  if observations.observedCount == 0 then Right(0.0)
  else
    GeneralizedLowRankProgram
      .from(
        observations,
        layout,
        Vector.empty,
        MissingnessStatement.Unspecified,
        GlrmPredictionTarget.EncodeNewRows
      )
      .left
      .map(StructuredMultiblockGlrmError.Generalized.apply)
      .flatMap(_.evaluate(factors).left.map(StructuredMultiblockGlrmError.Generalized.apply))
      .map(_.observedEntryLoss)

private def prefixedFeatureSpecifications[
    TrainingRows <: SemanticSpace,
    NewRows <: SemanticSpace,
    Latent <: SemanticSpace
](
    inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]
): Either[StructuredMultiblockGlrmError, Vector[GlrmFeatureSpec]] =
  traverseStructured(inputs): input =>
    traverseStructured(input.featureSpecs): specification =>
      for
        featureId <- GlrmFeatureId
          .apply(s"${input.blockId.value}.${specification.id.value}")
          .left
          .map(StructuredMultiblockGlrmError.Generalized.apply)
        prefixed <- GlrmFeatureSpec
          .from(featureId, specification.domain, specification.loss)
          .left
          .map(StructuredMultiblockGlrmError.Generalized.apply)
      yield prefixed
  .map(_.flatten)

private def concatenateDecoders[
    TrainingRows <: SemanticSpace,
    NewRows <: SemanticSpace,
    Latent <: SemanticSpace
](
    inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]],
    totalColumns: Int
): DMat =
  val latentDimension = inputs.head.decoder.latentSpace.dimension
  val output = new Array[Double](latentDimension * totalColumns)
  var latent = 0
  while latent < latentDimension do
    var globalColumn = 0
    var block = 0
    while block < inputs.length do
      val decoder = inputs(block).decoder.values
      var column = 0
      while column < decoder.cols do
        output(latent * totalColumns + globalColumn) = decoder(latent, column)
        globalColumn += 1
        column += 1
      block += 1
    latent += 1
  GaleNumerics.matrixFromRowMajor(latentDimension, totalColumns, output)

private def scaledObservationCells[
    TrainingRows <: SemanticSpace,
    NewRows <: SemanticSpace,
    Latent <: SemanticSpace
](
    inputs: Vector[PartialAlignedBlockObservation[TrainingRows, NewRows, Latent]]
): Either[StructuredMultiblockGlrmError, Vector[ObservationCell]] =
  traverseStructured(inputs): input =>
    val count = input.observations.observedCount
    val coefficient =
      if count == 0 then Right(1.0)
      else input.lossScaling.effectiveCoefficient(count)
    coefficient.flatMap: scale =>
      traverseStructured(Vector.range(0, input.featureSpecs.length)): feature =>
        scaleObservationCell(input.observations.cellUnsafe(0, feature), scale)
  .map(_.flatten)

private def scaleObservationCell(
    cell: ObservationCell,
    scale: Double
): Either[StructuredMultiblockGlrmError, ObservationCell] =
  cell match
    case ObservationCell.Observed(value, weight) =>
      ObservationWeight(weight.value * scale)
        .left
        .map(StructuredMultiblockGlrmError.Generalized.apply)
        .map(ObservationCell.Observed(value, _))
    case ObservationCell.Censored(interval, weight) =>
      ObservationWeight(weight.value * scale)
        .left
        .map(StructuredMultiblockGlrmError.Generalized.apply)
        .map(ObservationCell.Censored(interval, _))
    case missing @ ObservationCell.Missing(_) => Right(missing)
    case structural @ ObservationCell.StructurallyInapplicable(_) => Right(structural)

private def requireObjectiveAgreement(
    blockLoss: Double,
    compiledLoss: Double
): Either[StructuredMultiblockGlrmError, Unit] =
  val tolerance = 1e-10 * Math.max(1.0, Math.max(Math.abs(blockLoss), Math.abs(compiledLoss)))
  if Math.abs(blockLoss - compiledLoss) <= tolerance then Right(())
  else
    Left(
      StructuredMultiblockGlrmError.InvalidDefinition(
        s"compiled multiblock loss $compiledLoss does not equal blockwise loss $blockLoss"
      )
    )

private def traverseStructured[A, B](
    values: Vector[A]
)(
    function: A => Either[StructuredMultiblockGlrmError, B]
): Either[StructuredMultiblockGlrmError, Vector[B]] =
  values.foldLeft[Either[StructuredMultiblockGlrmError, Vector[B]]](Right(Vector.empty)): (result, value) =>
    for
      accumulated <- result
      next <- function(value)
    yield accumulated :+ next
