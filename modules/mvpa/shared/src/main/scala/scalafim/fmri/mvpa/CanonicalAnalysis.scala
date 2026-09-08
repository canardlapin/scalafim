package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.LinAlgError
import gale.linalg.Matrix
import multivar.core.MultivarError
import multivar.core.SemanticSpace
import multivar.core.SpaceRef
import multivar.core.SpaceRole
import multivar.family.canonical.CanonicalEffectFit
import multivar.family.canonical.CanonicalEffectProblem
import multivar.family.canonical.CanonicalEffectSolution
import multivar.family.canonical.CanonicalFrameConstraint
import multivar.family.canonical.CanonicalRootSpectrum
import multivar.family.canonical.CanonicalSpectrumFit
import multivar.family.canonical.ConstrainedCanonicalFit
import multivar.family.canonical.ConstrainedCanonicalProblem
import multivar.family.canonical.ConstrainedCanonicalSolverSpec
import multivar.family.canonical.ManovaStatistics
import multivar.family.canonical.ResidualRegularization
import multivar.family.canonical.TraceRidgeFraction.*

enum CrossValidatedRelationError:
  case EmptySchedule
  case DuplicateHeldOut(partition: PartitionId)
  case UnknownHeldOut(partition: PartitionId)
  case MissingHeldOut(partition: PartitionId)
  case PartitionAxisMismatch(partition: PartitionId, expected: AxisFingerprint, actual: AxisFingerprint)
  case EffectAxisMismatch(partition: PartitionId, expected: AxisFingerprint, actual: AxisFingerprint)
  case NeuralAxisMismatch(partition: PartitionId, expected: AxisFingerprint, actual: AxisFingerprint)
  case NominalWitnessMismatch(partition: PartitionId, boundary: String)
  case Identity(error: ScientificIdentityError)

  def message: String =
    this match
      case EmptySchedule               => "cross-validated relations require at least one scheduled relation set"
      case DuplicateHeldOut(partition) =>
        s"partition '${partition.value}' has more than one scheduled relation set"
      case UnknownHeldOut(partition) =>
        s"scheduled relation set references unknown held-out partition '${partition.value}'"
      case MissingHeldOut(partition) =>
        s"held-out partition '${partition.value}' has no scheduled relation set"
      case PartitionAxisMismatch(partition, expected, actual) =>
        s"scheduled relations for '${partition.value}' use partition axis ${actual.value}, expected ${expected.value}"
      case EffectAxisMismatch(partition, expected, actual) =>
        s"scheduled relations for '${partition.value}' use effect axis ${actual.value}, expected ${expected.value}"
      case NeuralAxisMismatch(partition, expected, actual) =>
        s"scheduled relations for '${partition.value}' use neural axis ${actual.value}, expected ${expected.value}"
      case NominalWitnessMismatch(partition, boundary) =>
        s"scheduled relations for '${partition.value}' use a foreign nominal $boundary witness"
      case Identity(error) => error.message

/** Relation evidence may be fixed across validation folds or prepared under an exact training-fold scope. Both cases
  * have one source type: every held-out partition resolves to a complete, axis-identical relation set.
  */
final class CrossValidatedRelations[
    Partitions <: SemanticSpace,
    Effects <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    EffectKey,
    NeuralCoordinate,
    +Capabilities <: RelationCapabilities[NeuralSpace, NeuralCoordinate]
] private (
    val partitions: PartitionAxis[Partitions],
    val effects: AxisRef.Aux[EffectKey, Effects],
    val neuralAxis: AxisRef.Aux[NeuralCoordinate, NeuralSpace],
    private val scheduled: Map[
      PartitionId,
      PartitionedRelations[
        Partitions,
        Effects,
        NeuralSpace,
        EffectKey,
        NeuralCoordinate,
        Capabilities
      ]
    ],
    val partitionAxisName: ScientificAxisName,
    val effectAxisName: ScientificAxisName,
    val neuralAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = NeuralCoordinate

  def relationsFor(
      heldOut: PartitionId
  ): Either[
    CrossValidatedRelationError,
    PartitionedRelations[
      Partitions,
      Effects,
      NeuralSpace,
      EffectKey,
      NeuralCoordinate,
      Capabilities
    ]
  ] =
    scheduled.get(heldOut).toRight(CrossValidatedRelationError.MissingHeldOut(heldOut))

object CrossValidatedRelations:
  private val Protocol = "scalafim-mvpa-cross-validated-relations/v1"

  def stable[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: RelationCapabilities[N, NK]](
      source: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[CrossValidatedRelationError, CrossValidatedRelations[P, E, N, EK, NK, C]] =
    scheduled(
      source.partitions,
      source.effects,
      source.neuralAxis,
      source.partitionKeys.map(_ -> source),
      source.effectAxisName,
      source.neuralAxisName
    )

  def scheduled[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: RelationCapabilities[N, NK]](
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      input: Seq[(PartitionId, PartitionedRelations[P, E, N, EK, NK, C])],
      effectAxisName: ScientificAxisName = ScientificAxisName.unsafe("effects"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural")
  ): Either[CrossValidatedRelationError, CrossValidatedRelations[P, E, N, EK, NK, C]] =
    if input.isEmpty then Left(CrossValidatedRelationError.EmptySchedule)
    else
      val indexed = scala.collection.mutable.HashMap.empty[
        PartitionId,
        PartitionedRelations[P, E, N, EK, NK, C]
      ]
      val iterator = input.iterator
      while iterator.hasNext do
        val (heldOut, relations) = iterator.next()
        if partitions.axis.positionOf(heldOut).isEmpty then
          return Left(CrossValidatedRelationError.UnknownHeldOut(heldOut))
        if indexed.contains(heldOut) then return Left(CrossValidatedRelationError.DuplicateHeldOut(heldOut))
        validateAxes(heldOut, partitions, effects, neural, relations) match
          case Left(error) => return Left(error)
          case Right(_)    => indexed += heldOut -> relations

      var position = 0
      while position < partitions.axis.size do
        val heldOut = partitions.axis.keys(position)
        if !indexed.contains(heldOut) then return Left(CrossValidatedRelationError.MissingHeldOut(heldOut))
        position += 1

      val writer = CanonicalWriter()
      writer.string(Protocol)
      writer.int(partitions.axis.size)
      partitions.axis.keys.foreach: heldOut =>
        writer.string(heldOut.value)
        writer.string(indexed(heldOut).identity.fingerprint.value)
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("cross-validated-relations"),
        Vector(
          ScientificSourceAxis(partitions.name, partitions.axis.identity),
          ScientificSourceAxis(effectAxisName, effects.identity),
          ScientificSourceAxis(neuralAxisName, neural.identity)
        ),
        Vector(
          "protocol" -> Protocol,
          "schedule" -> AxisDigest.sha256Hex(writer.result())
        )
      ).left
        .map(CrossValidatedRelationError.Identity.apply)
        .map: identity =>
          new CrossValidatedRelations(
            partitions,
            effects,
            neural,
            indexed.toMap,
            partitions.name,
            effectAxisName,
            neuralAxisName,
            identity
          )

  private def validateAxes[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      heldOut: PartitionId,
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      relations: PartitionedRelations[P, E, N, EK, NK, C]
  ): Either[CrossValidatedRelationError, Unit] =
    if relations.partitions.axis.identity != partitions.axis.identity then
      Left(
        CrossValidatedRelationError.PartitionAxisMismatch(
          heldOut,
          partitions.axis.identity.fingerprint,
          relations.partitions.axis.identity.fingerprint
        )
      )
    else if !(relations.partitions.axis.evidence eq partitions.axis.evidence) then
      Left(CrossValidatedRelationError.NominalWitnessMismatch(heldOut, "partition"))
    else if relations.effects.identity != effects.identity then
      Left(
        CrossValidatedRelationError.EffectAxisMismatch(
          heldOut,
          effects.identity.fingerprint,
          relations.effects.identity.fingerprint
        )
      )
    else if !(relations.effects.evidence eq effects.evidence) then
      Left(CrossValidatedRelationError.NominalWitnessMismatch(heldOut, "effect"))
    else if relations.neuralAxis.identity != neural.identity then
      Left(
        CrossValidatedRelationError.NeuralAxisMismatch(
          heldOut,
          neural.identity.fingerprint,
          relations.neuralAxis.identity.fingerprint
        )
      )
    else if !(relations.neuralAxis.evidence eq neural.evidence) then
      Left(CrossValidatedRelationError.NominalWitnessMismatch(heldOut, "neural"))
    else Right(())

opaque type EffectNormalizationVariance = Double

object EffectNormalizationVariance:
  def apply(value: Double): Either[RelationError, EffectNormalizationVariance] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(RelationError.InvalidEffectNormalizationVariance(value))

  private[mvpa] def unsafe(value: Double): EffectNormalizationVariance =
    value

  extension (value: EffectNormalizationVariance) inline def toDouble: Double = value

/** Residual moments plus the exact scalar-effect normalization used to create a one-row relation estimate. The scale is
  * needed by signed cross-run statistics, while ordinary canonical and MANOVA estimands require only the statically
  * visible residual-moment capability.
  */
trait HasEffectNormalizationVariance[
    N <: SemanticSpace,
    K
] extends HasResidualMoments[N, K]
    with HasResidualDegreesOfFreedom[N, K]:
  def effectNormalizationVariance: EffectNormalizationVariance

final class ScalarEffectFitCapabilities[
    N <: SemanticSpace,
    K
] private (
    val residualMoments: CertifiedResidualMoments[N, K],
    val residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    val effectNormalizationVariance: EffectNormalizationVariance,
    val identity: ScientificComponentFingerprint
) extends HasEffectNormalizationVariance[N, K]:
  def neuralAxis: AxisRef.Aux[K, N] = residualMoments.neural

object ScalarEffectFitCapabilities:
  private val Kind = EstimandKind.unsafe("scalar-effect-fit-capabilities")

  def apply[N <: SemanticSpace, K](
      residualMoments: CertifiedResidualMoments[N, K],
      residualDegreesOfFreedom: ResidualDegreesOfFreedom,
      effectNormalizationVariance: EffectNormalizationVariance
  ): Either[RelationError, ScalarEffectFitCapabilities[N, K]] =
    ResidualFitCapabilities(
      residualMoments,
      residualDegreesOfFreedom
    ).flatMap: admitted =>
      EstimandIdentity(
        Kind,
        Vector(
          "degrees-of-freedom" -> residualDegreesOfFreedom.toDouble.toString,
          "effect-normalization-variance" -> effectNormalizationVariance.toDouble.toString,
          "neural" -> residualMoments.neural.identity.fingerprint.value,
          "residual-moment-certificate" -> residualMoments.certificate.identity.value,
          "residual-moments" -> residualMoments.table.table.valueIdentity.stableKey
        )
      ).left
        .map(RelationError.Identity.apply)
        .map: component =>
          new ScalarEffectFitCapabilities(
            admitted.residualMoments,
            admitted.residualDegreesOfFreedom,
            effectNormalizationVariance,
            component.fingerprint
          )

enum CanonicalBindRejection:
  case PartitionAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PartitionWitnessMismatch
  case RelationSchedule(error: CrossValidatedRelationError)
  case NonEstimableEffect(partition: PartitionId, effect: String)
  case ScalarEffectRequired(actual: Int)

  def message: String =
    this match
      case PartitionAxisMismatch(expected, actual) =>
        s"relation validation axis ${actual.value} does not match source partitions ${expected.value}"
      case PartitionWitnessMismatch =>
        "relation validation and source use different nominal partition witnesses"
      case RelationSchedule(error)               => error.message
      case NonEstimableEffect(partition, effect) =>
        s"partition '${partition.value}' cannot estimate canonical effect '$effect'"
      case ScalarEffectRequired(actual) =>
        s"the scalar canonical estimand requires exactly one effect coordinate, obtained $actual"

enum CanonicalTaskFailure:
  case Evidence(error: EvidenceTableError)
  case Relation(error: RelationError)
  case Linear(error: LinAlgError)
  case Multivar(error: MultivarError)
  case MissingRelation(partition: PartitionId)
  case NonIdentifiableDirection(partition: PartitionId, multiplicity: Int)
  case NonPositiveDenominator(partition: PartitionId, value: Double)
  case InvalidHeldOutRoot(partition: PartitionId, value: Double)
  case NonFiniteSignedNumerator(partition: PartitionId, value: Double)
  case InvalidSignedStatistic(partition: PartitionId, value: Double)
  case InvalidResult(detail: String)

  def message: String =
    this match
      case Evidence(error)            => error.message
      case Relation(error)            => error.message
      case Linear(error)              => error.getMessage
      case Multivar(error)            => error.message
      case MissingRelation(partition) =>
        s"canonical fold references missing partition '${partition.value}'"
      case NonIdentifiableDirection(partition, multiplicity) =>
        s"partition '${partition.value}' cannot be scored from a leading root of multiplicity $multiplicity"
      case NonPositiveDenominator(partition, value) =>
        s"partition '${partition.value}' has non-positive held-out denominator $value"
      case InvalidHeldOutRoot(partition, value) =>
        s"partition '${partition.value}' has invalid held-out canonical root $value"
      case NonFiniteSignedNumerator(partition, value) =>
        s"partition '${partition.value}' has non-finite signed cross-run numerator $value"
      case InvalidSignedStatistic(partition, value) =>
        s"partition '${partition.value}' has invalid signed cross-run statistic $value"
      case InvalidResult(detail) => detail

final case class CanonicalFoldReceipt(
    training: Vector[PartitionId],
    heldOut: PartitionId,
    relationFits: Vector[(PartitionId, ScientificComponentFingerprint)]
)

final class CanonicalComputationReceipt private[mvpa] (
    val source: ScientificComponentFingerprint,
    val design: ScientificComponentFingerprint,
    val measurement: MeasurementIdentity,
    val estimand: ScientificComponentFingerprint,
    val folds: Vector[CanonicalFoldReceipt],
    val operatorApplications: Long
)

final case class CanonicalEffectFoldEstimate(
    receipt: CanonicalFoldReceipt,
    trainingFit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoot: Double
)

final class CanonicalEffectEstimate private[mvpa] (
    val measurement: MeasurementIdentity,
    val folds: Vector[CanonicalEffectFoldEstimate],
    val meanHeldOutRoot: Double,
    val canonicalCorrelation: Double,
    val computation: CanonicalComputationReceipt
)

final case class ManovaFoldEstimate(
    receipt: CanonicalFoldReceipt,
    trainingFit: CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoots: CanonicalRootSpectrum,
    heldOutStatistics: ManovaStatistics
)

final class ManovaEstimate private[mvpa] (
    val measurement: MeasurementIdentity,
    val folds: Vector[ManovaFoldEstimate],
    val meanStatistics: ManovaStatistics,
    val computation: CanonicalComputationReceipt
)

final case class ConstrainedCanonicalFoldEstimate(
    receipt: CanonicalFoldReceipt,
    trainingFit: ConstrainedCanonicalFit[? <: SemanticSpace, ? <: SemanticSpace],
    heldOutRoot: Double
)

final class ConstrainedCanonicalEstimate private[mvpa] (
    val measurement: MeasurementIdentity,
    val folds: Vector[ConstrainedCanonicalFoldEstimate],
    val meanHeldOutRoot: Double,
    val canonicalCorrelation: Double,
    val computation: CanonicalComputationReceipt
)

opaque type SignedCrossRunRayleigh = Double

object SignedCrossRunRayleigh:
  def apply(value: Double): Either[CanonicalTaskFailure, SignedCrossRunRayleigh] =
    if value.isFinite then Right(value)
    else Left(CanonicalTaskFailure.InvalidResult(s"signed cross-run Rayleigh value must be finite, obtained $value"))

  private[mvpa] def unsafe(value: Double): SignedCrossRunRayleigh =
    require(value.isFinite, "signed cross-run Rayleigh statistic must be finite")
    value

  extension (value: SignedCrossRunRayleigh) inline def toDouble: Double = value

final case class SignedCrossRunFoldEstimate(
    receipt: CanonicalFoldReceipt,
    trainingFit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
    numerator: Double,
    denominator: Double,
    statistic: SignedCrossRunRayleigh
)

final class SignedCrossRunRayleighEstimate private[mvpa] (
    val measurement: MeasurementIdentity,
    val folds: Vector[SignedCrossRunFoldEstimate],
    val meanStatistic: SignedCrossRunRayleigh,
    val computation: CanonicalComputationReceipt
)

final class CanonicalPrepared private[mvpa] (
    val source: ScientificComponentFingerprint,
    val design: ScientificComponentFingerprint,
    val effectRank: Int
)

final class CanonicalEffectEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasResidualMoments[N, NK]
] private[mvpa] (
    val regularization: ResidualRegularization,
    val identity: EstimandIdentity
) extends Estimand[
      CrossValidatedRelations[P, E, N, EK, NK, C],
      RelationValidationDesign[P]
    ]:
  override type Result = CanonicalEffectEstimate
  override type Rejection = CanonicalBindRejection
  override type Failure = CanonicalTaskFailure
  override val defaultBoundaries: RequestedBoundaries = CanonicalAnalysis.CanonicalBoundaries
  override def rejectionMessage(value: CanonicalBindRejection): String = value.message
  override def failureMessage(value: CanonicalTaskFailure): String = value.message

final class ManovaEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasResidualMoments[N, NK]
] private[mvpa] (
    val regularization: ResidualRegularization,
    val identity: EstimandIdentity
) extends Estimand[
      CrossValidatedRelations[P, E, N, EK, NK, C],
      RelationValidationDesign[P]
    ]:
  override type Result = ManovaEstimate
  override type Rejection = CanonicalBindRejection
  override type Failure = CanonicalTaskFailure
  override val defaultBoundaries: RequestedBoundaries = CanonicalAnalysis.ManovaBoundaries
  override def rejectionMessage(value: CanonicalBindRejection): String = value.message
  override def failureMessage(value: CanonicalTaskFailure): String = value.message

final class NonnegativeCanonicalEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasResidualMoments[N, NK]
] private[mvpa] (
    val regularization: ResidualRegularization,
    val identity: EstimandIdentity
) extends Estimand[
      CrossValidatedRelations[P, E, N, EK, NK, C],
      RelationValidationDesign[P]
    ]:
  override type Result = ConstrainedCanonicalEstimate
  override type Rejection = CanonicalBindRejection
  override type Failure = CanonicalTaskFailure
  override val defaultBoundaries: RequestedBoundaries = CanonicalAnalysis.ConstrainedBoundaries
  override def rejectionMessage(value: CanonicalBindRejection): String = value.message
  override def failureMessage(value: CanonicalTaskFailure): String = value.message

final class SignedCrossRunRayleighEstimand[
    P <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    C <: HasEffectNormalizationVariance[N, NK]
] private[mvpa] (
    val regularization: ResidualRegularization,
    val identity: EstimandIdentity
) extends Estimand[
      CrossValidatedRelations[P, E, N, EK, NK, C],
      RelationValidationDesign[P]
    ]:
  override type Result = SignedCrossRunRayleighEstimate
  override type Rejection = CanonicalBindRejection
  override type Failure = CanonicalTaskFailure
  override val defaultBoundaries: RequestedBoundaries = CanonicalAnalysis.SignedBoundaries
  override def rejectionMessage(value: CanonicalBindRejection): String = value.message
  override def failureMessage(value: CanonicalTaskFailure): String = value.message

object CanonicalAnalysis:
  private val CanonicalKind = EstimandKind.unsafe("canonical-effect")
  private val ManovaKind = EstimandKind.unsafe("manova")
  private val ConstrainedKind = EstimandKind.unsafe("nonnegative-canonical-effect")
  private val SignedKind = EstimandKind.unsafe("signed-cross-run-rayleigh")

  private[mvpa] val CanonicalBoundaries = boundaries("canonical-effect-estimate")
  private[mvpa] val ManovaBoundaries = boundaries("manova-estimate")
  private[mvpa] val ConstrainedBoundaries = boundaries("nonnegative-canonical-estimate")
  private[mvpa] val SignedBoundaries = boundaries("signed-cross-run-rayleigh-estimate")

  def canonicalEffect[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ]](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      regularization: ResidualRegularization
  ): CanonicalEffectEstimand[P, E, N, EK, NK, C] =
    new CanonicalEffectEstimand(
      regularization,
      estimandIdentity(CanonicalKind, source, regularization, "leading-generalized-root")
    )

  def manova[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[N, NK]](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      regularization: ResidualRegularization
  ): ManovaEstimand[P, E, N, EK, NK, C] =
    new ManovaEstimand(
      regularization,
      estimandIdentity(ManovaKind, source, regularization, "canonical-root-spectrum")
    )

  def nonnegativeCanonical[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ]](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      regularization: ResidualRegularization
  ): NonnegativeCanonicalEstimand[P, E, N, EK, NK, C] =
    new NonnegativeCanonicalEstimand(
      regularization,
      estimandIdentity(ConstrainedKind, source, regularization, "nonnegative-coordinate-cone")
    )

  def signedCrossRunRayleigh[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: HasEffectNormalizationVariance[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      regularization: ResidualRegularization
  ): SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C] =
    new SignedCrossRunRayleighEstimand(
      regularization,
      estimandIdentity(SignedKind, source, regularization, "frozen-training-direction-agreement-sign")
    )

  given canonicalCompiler[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: Compile[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    CanonicalEffectEstimand[P, E, N, EK, NK, C],
    R
  ] with
    override type Prepared = CanonicalPrepared
    override def prepare(
        specification: ScientificSpecification[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          CanonicalEffectEstimand[P, E, N, EK, NK, C],
          R
        ]
    ): Either[specification.Rejection, CanonicalPrepared] =
      prepareBound(specification.source, specification.design, true)

  given manovaCompiler[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: Compile[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    ManovaEstimand[P, E, N, EK, NK, C],
    R
  ] with
    override type Prepared = CanonicalPrepared
    override def prepare(
        specification: ScientificSpecification[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          ManovaEstimand[P, E, N, EK, NK, C],
          R
        ]
    ): Either[specification.Rejection, CanonicalPrepared] =
      prepareBound(specification.source, specification.design, false)

  given constrainedCompiler[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: Compile[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    NonnegativeCanonicalEstimand[P, E, N, EK, NK, C],
    R
  ] with
    override type Prepared = CanonicalPrepared
    override def prepare(
        specification: ScientificSpecification[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          NonnegativeCanonicalEstimand[P, E, N, EK, NK, C],
          R
        ]
    ): Either[specification.Rejection, CanonicalPrepared] =
      prepareBound(specification.source, specification.design, true)

  given signedCompiler[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: HasEffectNormalizationVariance[N, NK],
      R
  ]: Compile[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C],
    R
  ] with
    override type Prepared = CanonicalPrepared
    override def prepare(
        specification: ScientificSpecification[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C],
          R
        ]
    ): Either[specification.Rejection, CanonicalPrepared] =
      prepareBound(specification.source, specification.design, true)

  given canonicalTask[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: MeasurementTask[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    CanonicalEffectEstimand[P, E, N, EK, NK, C],
    R,
    CanonicalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          CanonicalEffectEstimand[P, E, N, EK, NK, C],
          R,
          CanonicalPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluateCanonical(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement
      ) match
        case Left(error)  => TaskReport.failed(error, context.strategy.target)
        case Right(value) =>
          TaskReport.success(
            value,
            context.strategy.target,
            operatorApplications = value.computation.operatorApplications
          )

  given manovaTask[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: MeasurementTask[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    ManovaEstimand[P, E, N, EK, NK, C],
    R,
    CanonicalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          ManovaEstimand[P, E, N, EK, NK, C],
          R,
          CanonicalPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluateManova(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement
      ) match
        case Left(error)  => TaskReport.failed(error, context.strategy.target)
        case Right(value) =>
          TaskReport.success(
            value,
            context.strategy.target,
            operatorApplications = value.computation.operatorApplications
          )

  given constrainedTask[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ], R]: MeasurementTask[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    NonnegativeCanonicalEstimand[P, E, N, EK, NK, C],
    R,
    CanonicalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          NonnegativeCanonicalEstimand[P, E, N, EK, NK, C],
          R,
          CanonicalPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluateConstrained(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement
      ) match
        case Left(error)  => TaskReport.failed(error, context.strategy.target)
        case Right(value) =>
          TaskReport.success(
            value,
            context.strategy.target,
            operatorApplications = value.computation.operatorApplications
          )

  given signedTask[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: HasEffectNormalizationVariance[N, NK],
      R
  ]: MeasurementTask[
    CrossValidatedRelations[P, E, N, EK, NK, C],
    RelationValidationDesign[P],
    SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C],
    R,
    CanonicalPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
      validateStrategy(strategy)

    override def execute(
        plan: BoundScientificPlan[
          CrossValidatedRelations[P, E, N, EK, NK, C],
          RelationValidationDesign[P],
          SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C],
          R,
          CanonicalPrepared
        ]
    )(
        entry: MeasurementEntry[N, NK, ?, R],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      evaluateSigned(
        plan.specification.source,
        plan.specification.design,
        plan.specification.estimand,
        entry.measurement
      ) match
        case Left(error)  => TaskReport.failed(error, context.strategy.target)
        case Right(value) =>
          TaskReport.success(
            value,
            context.strategy.target,
            operatorApplications = value.computation.operatorApplications
          )

  private final case class LocalRelationMoments(
      partition: PartitionId,
      effects: DMat,
      effectMoment: DMat,
      residualMoment: DMat,
      fit: ScientificComponentFingerprint,
      effectNormalizationVariance: Option[EffectNormalizationVariance]
  )

  private final case class ProjectedMoments(
      values: Vector[LocalRelationMoments],
      byPartition: Map[PartitionId, LocalRelationMoments],
      operatorApplications: Long
  )

  private final case class ProjectedSchedule(
      byHeldOut: Map[PartitionId, ProjectedMoments],
      operatorApplications: Long
  ):
    def fold(partition: PartitionId): Either[CanonicalTaskFailure, ProjectedMoments] =
      byHeldOut.get(partition).toRight(CanonicalTaskFailure.MissingRelation(partition))

  private def prepareBound[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, C <: HasResidualMoments[
    N,
    NK
  ]](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      scalar: Boolean
  ): Either[CanonicalBindRejection, CanonicalPrepared] =
    if source.partitions.axis.identity != design.partitions.axis.identity then
      Left(
        CanonicalBindRejection.PartitionAxisMismatch(
          source.partitions.axis.identity.fingerprint,
          design.partitions.axis.identity.fingerprint
        )
      )
    else if !(source.partitions.axis.evidence eq design.partitions.axis.evidence) then
      Left(CanonicalBindRejection.PartitionWitnessMismatch)
    else if scalar && source.effects.size != 1 then
      Left(CanonicalBindRejection.ScalarEffectRequired(source.effects.size))
    else
      var rejection: Option[CanonicalBindRejection] = None
      var foldPosition = 0
      while foldPosition < design.folds.length && rejection.isEmpty do
        val heldOut = design.folds(foldPosition).heldOut
        source.relationsFor(heldOut) match
          case Left(error) =>
            rejection = Some(CanonicalBindRejection.RelationSchedule(error))
          case Right(relations) =>
            relations.foreachRelation: (partition, relation) =>
              if rejection.isEmpty then
                var effect = 0
                while effect < source.effects.size && rejection.isEmpty do
                  if !relation.receipt.estimability.flags.values(effect) then
                    rejection = Some(
                      CanonicalBindRejection.NonEstimableEffect(
                        partition,
                        source.effects.identity.orderedKeys(effect).value
                      )
                    )
                  effect += 1
        foldPosition += 1
      rejection match
        case Some(value) => Left(value)
        case None        =>
          Right(
            new CanonicalPrepared(
              source.identity.fingerprint,
              design.identity.fingerprint,
              source.effects.size
            )
          )

  private def evaluateCanonical[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasResidualMoments[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      estimand: CanonicalEffectEstimand[P, E, N, EK, NK, C],
      measurement: Measurement[N, NK, LK]
  ): Either[CanonicalTaskFailure, CanonicalEffectEstimate] =
    for
      projected <- project(source, design, measurement, _ => None)
      folds <- evaluateCanonicalFolds(
        design,
        measurement,
        projected,
        estimand.regularization
      )
      mean = folds.map(_.heldOutRoot).sum / folds.length.toDouble
      _ <- validateRoot(folds.head.receipt.heldOut, mean)
      receipt = computationReceipt(source, design, measurement, estimand.identity, folds.map(_.receipt), projected)
    yield new CanonicalEffectEstimate(
      measurement.identity,
      folds,
      mean,
      Math.sqrt(mean / (1.0 + mean)),
      receipt
    )

  private def evaluateCanonicalFolds[P <: SemanticSpace, N <: SemanticSpace, NK, LK](
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      projected: ProjectedSchedule,
      regularization: ResidualRegularization
  ): Either[CanonicalTaskFailure, Vector[CanonicalEffectFoldEstimate]] =
    val output = Vector.newBuilder[CanonicalEffectFoldEstimate]
    var foldPosition = 0
    while foldPosition < design.folds.length do
      val fold = design.folds(foldPosition)
      val foldMoments = projected.fold(fold.heldOut) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      trainingMoments(fold, foldMoments) match
        case Left(error)                        => return Left(error)
        case Right((effect, residual, receipt)) =>
          CanonicalEffectProblem
            .fromDense(measurement.local.evidence, effect, residual, regularization)
            .left
            .map(CanonicalTaskFailure.Multivar.apply)
            .flatMap(_.fit.left.map(CanonicalTaskFailure.Multivar.apply)) match
            case Left(error) => return Left(error)
            case Right(fit)  =>
              foldMoments.byPartition.get(fold.heldOut) match
                case None          => return Left(CanonicalTaskFailure.MissingRelation(fold.heldOut))
                case Some(heldOut) =>
                  heldOutRoot(fold.heldOut, fit, heldOut) match
                    case Left(error) => return Left(error)
                    case Right(root) =>
                      output += CanonicalEffectFoldEstimate(receipt, fit, root)
      foldPosition += 1
    Right(output.result())

  private def evaluateManova[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasResidualMoments[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      estimand: ManovaEstimand[P, E, N, EK, NK, C],
      measurement: Measurement[N, NK, LK]
  ): Either[CanonicalTaskFailure, ManovaEstimate] =
    for
      projected <- project(source, design, measurement, _ => None)
      folds <- evaluateManovaFolds(source, design, measurement, projected, estimand.regularization)
      statistics = meanStatistics(folds.map(_.heldOutStatistics))
      receipt = computationReceipt(source, design, measurement, estimand.identity, folds.map(_.receipt), projected)
    yield new ManovaEstimate(
      measurement.identity,
      folds,
      statistics,
      receipt
    )

  private def evaluateManovaFolds[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasResidualMoments[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      projected: ProjectedSchedule,
      regularization: ResidualRegularization
  ): Either[CanonicalTaskFailure, Vector[ManovaFoldEstimate]] =
    val output = Vector.newBuilder[ManovaFoldEstimate]
    var foldPosition = 0
    while foldPosition < design.folds.length do
      val fold = design.folds(foldPosition)
      val foldMoments = projected.fold(fold.heldOut) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      trainingMoments(fold, foldMoments) match
        case Left(error)                        => return Left(error)
        case Right((effect, residual, receipt)) =>
          CanonicalEffectProblem
            .fromDense(measurement.local.evidence, effect, residual, regularization)
            .left
            .map(CanonicalTaskFailure.Multivar.apply)
            .flatMap(_.fitSpectrum(source.effects.size).left.map(CanonicalTaskFailure.Multivar.apply)) match
            case Left(error) => return Left(error)
            case Right(fit)  =>
              foldMoments.byPartition.get(fold.heldOut) match
                case None          => return Left(CanonicalTaskFailure.MissingRelation(fold.heldOut))
                case Some(heldOut) =>
                  heldOutSpectrum(fit, heldOut, source.effects.size, regularization) match
                    case Left(error)       => return Left(error)
                    case Right(heldOutFit) =>
                      output += ManovaFoldEstimate(
                        receipt,
                        fit,
                        heldOutFit.roots,
                        heldOutFit.statistics
                      )
      foldPosition += 1
    Right(output.result())

  private def evaluateConstrained[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasResidualMoments[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      estimand: NonnegativeCanonicalEstimand[P, E, N, EK, NK, C],
      measurement: Measurement[N, NK, LK]
  ): Either[CanonicalTaskFailure, ConstrainedCanonicalEstimate] =
    for
      projected <- project(source, design, measurement, _ => None)
      folds <- evaluateConstrainedFolds(design, measurement, projected, estimand.regularization)
      mean = folds.map(_.heldOutRoot).sum / folds.length.toDouble
      _ <- validateRoot(folds.head.receipt.heldOut, mean)
      receipt = computationReceipt(source, design, measurement, estimand.identity, folds.map(_.receipt), projected)
    yield new ConstrainedCanonicalEstimate(
      measurement.identity,
      folds,
      mean,
      Math.sqrt(mean / (1.0 + mean)),
      receipt
    )

  private def evaluateConstrainedFolds[P <: SemanticSpace, N <: SemanticSpace, NK, LK](
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      projected: ProjectedSchedule,
      regularization: ResidualRegularization
  ): Either[CanonicalTaskFailure, Vector[ConstrainedCanonicalFoldEstimate]] =
    val output = Vector.newBuilder[ConstrainedCanonicalFoldEstimate]
    var foldPosition = 0
    while foldPosition < design.folds.length do
      val fold = design.folds(foldPosition)
      val foldMoments = projected.fold(fold.heldOut) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      trainingMoments(fold, foldMoments) match
        case Left(error)                        => return Left(error)
        case Right((effect, residual, receipt)) =>
          ConstrainedCanonicalProblem
            .fromDense(
              measurement.local.evidence,
              effect,
              residual,
              regularization,
              CanonicalFrameConstraint.Nonnegative,
              ConstrainedCanonicalSolverSpec.default
            )
            .left
            .map(CanonicalTaskFailure.Multivar.apply)
            .flatMap(_.fit.left.map(CanonicalTaskFailure.Multivar.apply)) match
            case Left(error) => return Left(error)
            case Right(fit)  =>
              foldMoments.byPartition.get(fold.heldOut) match
                case None          => return Left(CanonicalTaskFailure.MissingRelation(fold.heldOut))
                case Some(heldOut) =>
                  heldOutRoot(
                    fold.heldOut,
                    fit.direction,
                    fit.programFit.identifiability.context.tolerance,
                    heldOut
                  ) match
                    case Left(error) => return Left(error)
                    case Right(root) =>
                      output += ConstrainedCanonicalFoldEstimate(receipt, fit, root)
      foldPosition += 1
    Right(output.result())

  private def evaluateSigned[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasEffectNormalizationVariance[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      estimand: SignedCrossRunRayleighEstimand[P, E, N, EK, NK, C],
      measurement: Measurement[N, NK, LK]
  ): Either[CanonicalTaskFailure, SignedCrossRunRayleighEstimate] =
    for
      projected <- project(
        source,
        design,
        measurement,
        capabilities => Some(capabilities.effectNormalizationVariance)
      )
      folds <- evaluateSignedFolds(design, measurement, projected, estimand.regularization)
      mean = folds.map(_.statistic.toDouble).sum / folds.length.toDouble
      statistic <- signedStatistic(folds.head.receipt.heldOut, mean)
      receipt = computationReceipt(source, design, measurement, estimand.identity, folds.map(_.receipt), projected)
    yield new SignedCrossRunRayleighEstimate(
      measurement.identity,
      folds,
      statistic,
      receipt
    )

  private def evaluateSignedFolds[P <: SemanticSpace, N <: SemanticSpace, NK, LK](
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      projected: ProjectedSchedule,
      regularization: ResidualRegularization
  ): Either[CanonicalTaskFailure, Vector[SignedCrossRunFoldEstimate]] =
    val output = Vector.newBuilder[SignedCrossRunFoldEstimate]
    var foldPosition = 0
    while foldPosition < design.folds.length do
      val fold = design.folds(foldPosition)
      val foldMoments = projected.fold(fold.heldOut) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      trainingMoments(fold, foldMoments) match
        case Left(error)                        => return Left(error)
        case Right((effect, residual, receipt)) =>
          CanonicalEffectProblem
            .fromDense(measurement.local.evidence, effect, residual, regularization)
            .left
            .map(CanonicalTaskFailure.Multivar.apply)
            .flatMap(_.fit.left.map(CanonicalTaskFailure.Multivar.apply)) match
            case Left(error) => return Left(error)
            case Right(fit)  =>
              fit.solution match
                case CanonicalEffectSolution.LeadingSubspace(_, _, multiplicity) =>
                  return Left(CanonicalTaskFailure.NonIdentifiableDirection(fold.heldOut, multiplicity))
                case CanonicalEffectSolution.Simple(direction, _) =>
                  signedFold(fold, direction, fit, residual, foldMoments) match
                    case Left(error)                                => return Left(error)
                    case Right((numerator, denominator, statistic)) =>
                      output += SignedCrossRunFoldEstimate(
                        receipt,
                        fit,
                        numerator,
                        denominator,
                        statistic
                      )
      foldPosition += 1
    Right(output.result())

  private def signedFold(
      fold: RelationValidationFold,
      direction: DVec,
      fit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
      trainingResidual: DMat,
      projected: ProjectedMoments
  ): Either[CanonicalTaskFailure, (Double, Double, SignedCrossRunRayleigh)] =
    var trainingProjection = 0.0
    var trainingPosition = 0
    while trainingPosition < fold.training.length do
      projected.byPartition.get(fold.training(trainingPosition)) match
        case None        => return Left(CanonicalTaskFailure.MissingRelation(fold.training(trainingPosition)))
        case Some(value) =>
          val variance = value.effectNormalizationVariance match
            case None =>
              return Left(CanonicalTaskFailure.InvalidResult("signed relation has no effect normalization variance"))
            case Some(scale) => scale.toDouble
          trainingProjection += dot(direction, value.effects, 0) * Math.sqrt(variance)
      trainingPosition += 1
    projected.byPartition.get(fold.heldOut) match
      case None          => Left(CanonicalTaskFailure.MissingRelation(fold.heldOut))
      case Some(heldOut) =>
        heldOut.effectNormalizationVariance match
          case None =>
            Left(CanonicalTaskFailure.InvalidResult("signed held-out relation has no effect normalization variance"))
          case Some(scale) =>
            val variance = scale.toDouble
            val heldOutProjection = dot(direction, heldOut.effects, 0) * Math.sqrt(variance)
            val numerator = trainingProjection * heldOutProjection / variance
            val denominator = quadratic(direction, trainingResidual) +
              fit.regularization.ridgeAmount * squaredNorm(direction)
            val threshold = fit.programFit.identifiability.context.tolerance.threshold(
              matrixFrobenius(trainingResidual) +
                fit.regularization.ridgeAmount * Math.sqrt(direction.length.toDouble)
            )
            if !numerator.isFinite then Left(CanonicalTaskFailure.NonFiniteSignedNumerator(fold.heldOut, numerator))
            else if !denominator.isFinite || denominator <= threshold then
              Left(CanonicalTaskFailure.NonPositiveDenominator(fold.heldOut, denominator))
            else
              signedStatistic(fold.heldOut, numerator / denominator)
                .map(statistic => (numerator, denominator, statistic))

  private def project[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, LK, C <: HasResidualMoments[
    N,
    NK
  ]](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      normalizationVariance: C => Option[EffectNormalizationVariance]
  ): Either[CanonicalTaskFailure, ProjectedSchedule] =
    val cache = scala.collection.mutable.HashMap.empty[
      ScientificComponentFingerprint,
      ProjectedMoments
    ]
    val scheduled = Map.newBuilder[PartitionId, ProjectedMoments]
    var applications = 0L
    var foldPosition = 0
    while foldPosition < design.folds.length do
      val heldOut = design.folds(foldPosition).heldOut
      val relations = source.relationsFor(heldOut) match
        case Left(_)      => return Left(CanonicalTaskFailure.MissingRelation(heldOut))
        case Right(value) => value
      val projected = cache.get(relations.identity.fingerprint) match
        case Some(value) => value
        case None        =>
          projectRelations(relations, measurement, normalizationVariance) match
            case Left(error)  => return Left(error)
            case Right(value) =>
              cache += relations.identity.fingerprint -> value
              applications += value.operatorApplications
              value
      scheduled += heldOut -> projected
      foldPosition += 1
    Right(ProjectedSchedule(scheduled.result(), applications))

  private def projectRelations[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: HasResidualMoments[N, NK]
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      measurement: Measurement[N, NK, LK],
      normalizationVariance: C => Option[EffectNormalizationVariance]
  ): Either[CanonicalTaskFailure, ProjectedMoments] =
    val output = Vector.newBuilder[LocalRelationMoments]
    var applications = 0L
    var position = 0
    while position < source.partitionKeys.length do
      val partition = source.partitionKeys(position)
      val relation = source.relation(partition) match
        case Left(error)  => return Left(CanonicalTaskFailure.Relation(error))
        case Right(value) => value
      val measured = relation.estimate.measureColumns(measurement) match
        case Left(error)  => return Left(CanonicalTaskFailure.Evidence(error))
        case Right(value) => value
      val effects = measured.transposeMultiply(DMat.eye(source.effects.size)) match
        case Left(error)  => return Left(CanonicalTaskFailure.Evidence(error))
        case Right(value) => value.t
      val residual = projectResidual(relation.capabilities.residualMoments.table, measurement) match
        case Left(error)  => return Left(error)
        case Right(value) => value
      val scale = normalizationVariance(relation.capabilities)
      output += LocalRelationMoments(
        partition,
        effects,
        crossProduct(effects),
        symmetrize(residual),
        relation.receipt.identity,
        scale
      )
      applications += 4L
      position += 1
    val values = output.result()
    Right(ProjectedMoments(values, values.map(value => value.partition -> value).toMap, applications))

  private def projectResidual[N <: SemanticSpace, NK, LK](
      residual: EvidenceTable[N, N, NK, NK],
      measurement: Measurement[N, NK, LK]
  ): Either[CanonicalTaskFailure, DMat] =
    for
      weights <- measurement.operator.adjoint
        .applyTo(DMat.eye(measurement.local.size))
        .left
        .map(CanonicalTaskFailure.Linear.apply)
      right <- residual.rightMultiply(weights).left.map(CanonicalTaskFailure.Evidence.apply)
      local <- measurement.operator.applyTo(right).left.map(CanonicalTaskFailure.Linear.apply)
    yield local

  private def trainingMoments(
      fold: RelationValidationFold,
      projected: ProjectedMoments
  ): Either[CanonicalTaskFailure, (DMat, DMat, CanonicalFoldReceipt)] =
    val moments = Vector.newBuilder[LocalRelationMoments]
    var position = 0
    while position < fold.training.length do
      projected.byPartition.get(fold.training(position)) match
        case None        => return Left(CanonicalTaskFailure.MissingRelation(fold.training(position)))
        case Some(value) => moments += value
      position += 1
    val values = moments.result()
    Right(
      (
        sumMatrices(values.map(_.effectMoment)),
        sumMatrices(values.map(_.residualMoment)),
        CanonicalFoldReceipt(
          fold.training,
          fold.heldOut,
          (fold.training :+ fold.heldOut).map: partition =>
            partition -> projected.byPartition(partition).fit
        )
      )
    )

  private def heldOutRoot(
      partition: PartitionId,
      fit: CanonicalEffectFit[? <: SemanticSpace, ? <: SemanticSpace],
      heldOut: LocalRelationMoments
  ): Either[CanonicalTaskFailure, Double] =
    fit.solution match
      case CanonicalEffectSolution.LeadingSubspace(_, _, multiplicity) =>
        Left(CanonicalTaskFailure.NonIdentifiableDirection(partition, multiplicity))
      case CanonicalEffectSolution.Simple(direction, _) =>
        heldOutRoot(
          partition,
          direction,
          fit.programFit.identifiability.context.tolerance,
          heldOut
        )

  private def heldOutRoot(
      partition: PartitionId,
      direction: DVec,
      tolerance: multivar.core.CertificateTolerance,
      heldOut: LocalRelationMoments
  ): Either[CanonicalTaskFailure, Double] =
    val numerator = quadratic(direction, heldOut.effectMoment)
    val denominator = quadratic(direction, heldOut.residualMoment)
    val threshold = tolerance.threshold(matrixFrobenius(heldOut.residualMoment))
    if !denominator.isFinite || denominator <= threshold then
      Left(CanonicalTaskFailure.NonPositiveDenominator(partition, denominator))
    else validateRoot(partition, numerator / denominator)

  private def heldOutSpectrum(
      trainingFit: CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace],
      heldOut: LocalRelationMoments,
      rank: Int,
      regularization: ResidualRegularization
  ): Either[CanonicalTaskFailure, CanonicalSpectrumFit[? <: SemanticSpace, ? <: SemanticSpace]] =
    for
      frame <- trainingFit.denseFrame.left.map(CanonicalTaskFailure.Multivar.apply)
      effect = compress(frame, heldOut.effectMoment)
      residual = compress(frame, heldOut.residualMoment)
      space <- SpaceRef
        .of("manova-held-out", SpaceRole.Observed, effect.rows)
        .left
        .map(CanonicalTaskFailure.Multivar.apply)
      fit <- CanonicalEffectProblem
        .fromDense(space.evidence, effect, residual, regularization)
        .left
        .map(CanonicalTaskFailure.Multivar.apply)
        .flatMap(_.fitSpectrum(rank).left.map(CanonicalTaskFailure.Multivar.apply))
    yield fit

  private def computationReceipt[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      LK,
      C <: RelationCapabilities[N, NK]
  ](
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      design: RelationValidationDesign[P],
      measurement: Measurement[N, NK, LK],
      estimand: EstimandIdentity,
      folds: Vector[CanonicalFoldReceipt],
      projected: ProjectedSchedule
  ): CanonicalComputationReceipt =
    new CanonicalComputationReceipt(
      source.identity.fingerprint,
      design.identity.fingerprint,
      measurement.identity,
      estimand.fingerprint,
      folds,
      projected.operatorApplications
    )

  private def validateStrategy(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
    val supported = Vector(
      ExecutionRepresentation.Operator,
      ExecutionRepresentation.SufficientStatistics
    )
    if !supported.contains(strategy.representation) then
      Left(ExecutionPlanError.UnsupportedRepresentation(strategy.representation, supported))
    else if strategy.precision != NumericPrecision.Binary64 then
      Left(ExecutionPlanError.UnsupportedPrecision(strategy.precision, Vector(NumericPrecision.Binary64)))
    else if strategy.solver != SolverChoice.NotApplicable then
      Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
    else Right(())

  private def validateRoot(
      partition: PartitionId,
      value: Double
  ): Either[CanonicalTaskFailure, Double] =
    if value.isFinite && value >= 0.0 then Right(value)
    else if value.isFinite && value >= -1e-10 then Right(0.0)
    else Left(CanonicalTaskFailure.InvalidHeldOutRoot(partition, value))

  private def signedStatistic(
      partition: PartitionId,
      value: Double
  ): Either[CanonicalTaskFailure, SignedCrossRunRayleigh] =
    if value.isFinite then Right(SignedCrossRunRayleigh.unsafe(value))
    else Left(CanonicalTaskFailure.InvalidSignedStatistic(partition, value))

  private def estimandIdentity[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      kind: EstimandKind,
      source: CrossValidatedRelations[P, E, N, EK, NK, C],
      regularization: ResidualRegularization,
      operation: String
  ): EstimandIdentity =
    EstimandIdentity.trusted(
      kind,
      Vector(
        "operation" -> operation,
        "regularization" -> regularizationLabel(regularization),
        "source" -> source.identity.fingerprint.value
      )
    )

  private def regularizationLabel(value: ResidualRegularization): String =
    value match
      case ResidualRegularization.Unregularized         => "unregularized"
      case ResidualRegularization.TraceScaled(fraction) =>
        s"trace-scaled:${java.lang.Double.toHexString(fraction.value)}"

  private def boundaries(id: String): RequestedBoundaries =
    val boundary = OutputBoundaryIdentity.trusted(OutputBoundaryId.unsafe(id))
    RequestedBoundaries.trusted(Vector(boundary))

  private def crossProduct(value: DMat): DMat =
    val output = Matrix.newBuilder(value.cols, value.cols)
    var left = 0
    while left < value.cols do
      var right = 0
      while right < value.cols do
        var total = 0.0
        var effect = 0
        while effect < value.rows do
          total += value(effect, left) * value(effect, right)
          effect += 1
        output(left, right) = total
        right += 1
      left += 1
    output.result()

  private def sumMatrices(values: Vector[DMat]): DMat =
    require(values.nonEmpty, "canonical matrix sum must be non-empty")
    val output = Matrix.newBuilder(values.head.rows, values.head.cols)
    var row = 0
    while row < values.head.rows do
      var column = 0
      while column < values.head.cols do
        var total = 0.0
        var index = 0
        while index < values.length do
          total += values(index)(row, column)
          index += 1
        output(row, column) = total
        column += 1
      row += 1
    output.result()

  private def quadratic(vector: DVec, matrix: DMat): Double =
    var total = 0.0
    var row = 0
    while row < vector.length do
      var column = 0
      while column < vector.length do
        total += vector(row) * matrix(row, column) * vector(column)
        column += 1
      row += 1
    total

  private def dot(direction: DVec, effects: DMat, effect: Int): Double =
    var total = 0.0
    var feature = 0
    while feature < direction.length do
      total += direction(feature) * effects(effect, feature)
      feature += 1
    total

  private def squaredNorm(value: DVec): Double =
    var total = 0.0
    var position = 0
    while position < value.length do
      total += value(position) * value(position)
      position += 1
    total

  private def matrixFrobenius(value: DMat): Double =
    var total = 0.0
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        total += value(row, column) * value(row, column)
        column += 1
      row += 1
    Math.sqrt(total)

  private def symmetrize(value: DMat): DMat =
    val output = Matrix.newBuilder(value.rows, value.cols)
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        output(row, column) = 0.5 * (value(row, column) + value(column, row))
        column += 1
      row += 1
    output.result()

  private def compress(frame: DMat, value: DMat): DMat =
    val output = Matrix.newBuilder(frame.cols, frame.cols)
    var left = 0
    while left < frame.cols do
      var right = 0
      while right < frame.cols do
        var total = 0.0
        var row = 0
        while row < value.rows do
          var column = 0
          while column < value.cols do
            total += frame(row, left) * value(row, column) * frame(column, right)
            column += 1
          row += 1
        output(left, right) = total
        right += 1
      left += 1
    output.result()

  private def meanStatistics(values: Vector[ManovaStatistics]): ManovaStatistics =
    val count = values.length.toDouble
    ManovaStatistics(
      values.map(_.royLargestRoot).sum / count,
      values.map(_.wilksLambda).sum / count,
      values.map(_.pillaiTrace).sum / count,
      values.map(_.hotellingLawleyTrace).sum / count
    )
