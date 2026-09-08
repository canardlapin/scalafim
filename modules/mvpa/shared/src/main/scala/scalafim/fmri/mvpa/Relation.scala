package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.spectral.{Eigen, EigenSelection, EigenVectors}
import multivar.core.SemanticSpace
import multivar.core.ValueIdentity

enum RelationError:
  case Identity(error: ScientificIdentityError)
  case Column(error: ColumnError)
  case InvalidEffectPurpose(actual: AxisPurpose)
  case InvalidNeuralPurpose(actual: AxisPurpose)
  case InvalidTrainingPurpose(actual: AxisPurpose)
  case EmptyEstimableEffects
  case EffectAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case EffectWitnessMismatch
  case CapabilityAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case CapabilityWitnessMismatch
  case Evidence(error: EvidenceTableError)
  case InvalidResidualMomentTolerance(value: Double)
  case ResidualMomentShapeMismatch(expected: Int, actualRows: Int, actualColumns: Int)
  case ResidualMomentAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ResidualMomentWitnessMismatch
  case AsymmetricResidualMoments(row: Int, column: Int, left: Double, right: Double)
  case ResidualMomentSpectrumFailure(detail: String)
  case NonPositiveSemidefiniteResidualMoments(
      smallestEigenvalue: Double,
      allowedNegativeMagnitude: Double
  )
  case InvalidDegreesOfFreedom(value: Double)
  case InvalidEffectNormalizationVariance(value: Double)
  case EmptyRelations
  case DuplicatePartition(partition: PartitionId)
  case UnknownPartition(partition: PartitionId)
  case MissingPartition(partition: PartitionId)
  case RelationNotFound(partition: PartitionId)
  case PartitionPurposeMismatch(actual: AxisPurpose)
  case RelationEffectsMismatch(
      partition: PartitionId,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case RelationNeuralMismatch(
      partition: PartitionId,
      expected: AxisFingerprint,
      actual: AxisFingerprint
  )
  case RelationEffectWitnessMismatch(partition: PartitionId)
  case RelationNeuralWitnessMismatch(partition: PartitionId)

  def message: String =
    this match
      case Identity(error)              => error.message
      case Column(error)                => error.message
      case InvalidEffectPurpose(actual) =>
        s"relation effects require an '${AxisPurpose.Effects.value}' axis, obtained '${actual.value}'"
      case InvalidNeuralPurpose(actual) =>
        s"relation evidence requires a '${AxisPurpose.NeuralFeatures.value}' neural axis, obtained '${actual.value}'"
      case InvalidTrainingPurpose(actual) =>
        s"relation fit training evidence requires a '${AxisPurpose.Samples.value}' axis, obtained '${actual.value}'"
      case EmptyEstimableEffects =>
        "relation fit must identify at least one estimable effect"
      case EffectAxisMismatch(expected, actual) =>
        s"relation effect axis ${actual.value} does not match ${expected.value}"
      case EffectWitnessMismatch =>
        "relation effect values and estimability use different nominal witnesses"
      case CapabilityAxisMismatch(expected, actual) =>
        s"relation capability belongs to neural axis ${actual.value}, expected ${expected.value}"
      case CapabilityWitnessMismatch =>
        "relation capability and estimate use different nominal neural witnesses"
      case Evidence(error)                       => error.message
      case InvalidResidualMomentTolerance(value) =>
        s"residual-moment tolerance must be finite and positive, obtained $value"
      case ResidualMomentShapeMismatch(expected, actualRows, actualColumns) =>
        s"residual-moment certificate expected ${expected}x$expected matrix, obtained ${actualRows}x$actualColumns"
      case ResidualMomentAxisMismatch(expected, actual) =>
        s"residual-moment axis ${actual.value} does not match ${expected.value}"
      case ResidualMomentWitnessMismatch =>
        "residual-moment rows and columns use different nominal neural witnesses"
      case AsymmetricResidualMoments(row, column, left, right) =>
        s"residual moments are asymmetric at ($row,$column): $left versus $right"
      case ResidualMomentSpectrumFailure(detail) =>
        s"residual-moment PSD certification failed: $detail"
      case NonPositiveSemidefiniteResidualMoments(smallest, allowed) =>
        s"residual moments are not positive semidefinite: smallest eigenvalue $smallest is below -$allowed"
      case InvalidDegreesOfFreedom(value) =>
        s"residual degrees of freedom must be finite and positive, obtained $value"
      case InvalidEffectNormalizationVariance(value) =>
        s"effect normalization variance must be finite and positive, obtained $value"
      case EmptyRelations =>
        "partitioned relations require at least one keyed relation"
      case DuplicatePartition(partition) =>
        s"partition '${partition.value}' has more than one relation"
      case UnknownPartition(partition) =>
        s"relation references unknown partition '${partition.value}'"
      case MissingPartition(partition) =>
        s"partition '${partition.value}' has no relation"
      case RelationNotFound(partition) =>
        s"no relation is bound to partition '${partition.value}'"
      case PartitionPurposeMismatch(actual) =>
        s"partitioned relations require a '${AxisPurpose.Partitions.value}' axis, obtained '${actual.value}'"
      case RelationEffectsMismatch(partition, expected, actual) =>
        s"relation '${partition.value}' has effect axis ${actual.value}, expected ${expected.value}"
      case RelationNeuralMismatch(partition, expected, actual) =>
        s"relation '${partition.value}' has neural axis ${actual.value}, expected ${expected.value}"
      case RelationEffectWitnessMismatch(partition) =>
        s"relation '${partition.value}' uses a different nominal effect witness"
      case RelationNeuralWitnessMismatch(partition) =>
        s"relation '${partition.value}' uses a different nominal neural witness"

/** Owner-bound declaration of which effect estimates may be queried. */
final class Estimability[
    Effects <: SemanticSpace,
    EffectKey
] private (
    val effects: AxisRef.Aux[EffectKey, Effects],
    val flags: Column[Effects, Boolean],
    val rank: Int,
    val identity: ScientificComponentFingerprint
):
  def isEstimable(key: EffectKey): Boolean =
    effects.positionOf(key).exists(flags.values(_))

  def estimableKeys: Vector[EffectKey] =
    effects.keys
      .zip(flags.toVector)
      .collect:
        case (key, true) => key

object Estimability:
  private val Kind = EstimandKind.unsafe("relation-estimability")

  def apply[K](
      effects: AxisRef[K],
      estimable: Seq[Boolean]
  ): Either[RelationError, Estimability[effects.Id, K]] =
    if effects.identity.purpose != AxisPurpose.Effects then
      Left(RelationError.InvalidEffectPurpose(effects.identity.purpose))
    else
      Column(effects, estimable).left
        .map(RelationError.Column.apply)
        .flatMap: flags =>
          val rank = flags.toVector.count(identity)
          if rank == 0 then Left(RelationError.EmptyEstimableEffects)
          else
            val writer = CanonicalWriter()
            writer.string("scalafim-mvpa-estimability/v1")
            writer.string(effects.identity.fingerprint.value)
            writer.int(flags.size)
            flags.toVector.foreach(value => writer.int(if value then 1 else 0))
            EstimandIdentity(
              Kind,
              Vector(
                "effects" -> effects.identity.fingerprint.value,
                "flags" -> AxisDigest.sha256Hex(writer.result()),
                "rank" -> rank.toString
              )
            ).left
              .map(RelationError.Identity.apply)
              .map: component =>
                new Estimability(
                  effects,
                  flags,
                  rank,
                  component.fingerprint
                )

/** Complete scientific provenance for one fitted experimental-neural relation. This record contains no backend, solver,
  * chunking, or scheduling choices; those remain execution evidence.
  */
final class RelationFitReceipt[
    Effects <: SemanticSpace,
    EffectKey
] private (
    val sourceRevision: ValueIdentity,
    val design: DesignIdentity,
    val estimability: Estimability[Effects, EffectKey],
    val preparation: NormalizationIdentity,
    val trainingSamples: AxisIdentity,
    val identity: ScientificComponentFingerprint
)

object RelationFitReceipt:
  private val Kind = EstimandKind.unsafe("relation-fit-receipt")

  def apply[E <: SemanticSpace, K](
      sourceRevision: ValueIdentity,
      design: DesignIdentity,
      estimability: Estimability[E, K],
      preparation: NormalizationIdentity,
      trainingSamples: AxisRef[SampleId]
  ): Either[RelationError, RelationFitReceipt[E, K]] =
    if trainingSamples.identity.purpose != AxisPurpose.Samples then
      Left(RelationError.InvalidTrainingPurpose(trainingSamples.identity.purpose))
    else
      EstimandIdentity(
        Kind,
        Vector(
          "design" -> design.fingerprint.value,
          "estimability" -> estimability.identity.value,
          "preparation" -> preparation.fingerprint.value,
          "source-revision" -> sourceRevision.stableKey,
          "training-samples" -> trainingSamples.identity.fingerprint.value
        )
      ).left
        .map(RelationError.Identity.apply)
        .map: component =>
          new RelationFitReceipt(
            sourceRevision,
            design,
            estimability,
            preparation,
            trainingSamples.identity,
            component.fingerprint
          )

/** Open capability carried by a relation. Capability implementations are bound to one exact neural axis and provide
  * their own stable identity.
  */
trait RelationCapabilities[N <: SemanticSpace, K]:
  def neuralAxis: AxisRef.Aux[K, N]
  def identity: ScientificComponentFingerprint

trait HasResidualMoments[
    N <: SemanticSpace,
    K
] extends RelationCapabilities[N, K]:
  def residualMoments: CertifiedResidualMoments[N, K]

trait HasNoisePrecision[
    N <: SemanticSpace,
    K
] extends RelationCapabilities[N, K]:
  def noisePrecision: CertifiedNoisePrecision[N, K]

trait HasResidualDegreesOfFreedom[
    N <: SemanticSpace,
    K
] extends RelationCapabilities[N, K]:
  def residualDegreesOfFreedom: ResidualDegreesOfFreedom

opaque type ResidualMomentTolerance = Double

object ResidualMomentTolerance:
  def apply(value: Double): Either[RelationError, ResidualMomentTolerance] =
    if !value.isFinite || value <= 0.0 then Left(RelationError.InvalidResidualMomentTolerance(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): ResidualMomentTolerance =
    value

  extension (value: ResidualMomentTolerance) inline def toDouble: Double = value

/** Executable symmetry and positive-semidefiniteness attestation for one exact residual-moment value on one exact
  * neural axis.
  */
final class ResidualMomentCertificate[
    N <: SemanticSpace,
    K
] private (
    val neural: AxisRef.Aux[K, N],
    val valueIdentity: ValueIdentity,
    val contentDigest: String,
    val tolerance: ResidualMomentTolerance,
    val smallestEigenvalue: Double,
    val identity: ScientificComponentFingerprint
)

object ResidualMomentCertificate:
  private val Kind = EstimandKind.unsafe("residual-moment-certificate")

  private[mvpa] def verify[K](
      neural: AxisRef[K],
      moments: DMat,
      valueIdentity: ValueIdentity,
      tolerance: ResidualMomentTolerance
  ): Either[RelationError, ResidualMomentCertificate[neural.Id, K]] =
    for
      _ <- Either.cond(
        moments.rows == neural.size && moments.cols == neural.size,
        (),
        RelationError.ResidualMomentShapeMismatch(
          neural.size,
          moments.rows,
          moments.cols
        )
      )
      _ <- EvidenceTable
        .validateFinite(moments, "residual moments")
        .left
        .map(RelationError.Evidence.apply)
      _ <- validateSymmetry(moments, tolerance)
      spectrum <- Eigen
        .eigSymmetric(moments, EigenSelection.All, EigenVectors.ValuesOnly)
        .left
        .map(error => RelationError.ResidualMomentSpectrumFailure(error.getMessage))
      certified <- spectrum.requireConverged.left
        .map(error => RelationError.ResidualMomentSpectrumFailure(error.getMessage))
      smallest = certified.eigenvalues(0)
      spectralScale = eigenvalueScale(certified)
      allowedNegativeMagnitude = tolerance.toDouble * spectralScale
      _ <- Either.cond(
        smallest >= -allowedNegativeMagnitude,
        (),
        RelationError.NonPositiveSemidefiniteResidualMoments(
          smallest,
          allowedNegativeMagnitude
        )
      )
      digest = matrixDigest(moments)
      component <- EstimandIdentity(
        Kind,
        Vector(
          "contents" -> digest,
          "method" -> "symmetric-positive-semidefinite-eigenvalue",
          "neural" -> neural.identity.fingerprint.value,
          "smallest-eigenvalue" -> smallest.toString,
          "tolerance" -> tolerance.toDouble.toString,
          "value" -> valueIdentity.stableKey
        )
      ).left.map(RelationError.Identity.apply)
    yield new ResidualMomentCertificate(
      neural,
      valueIdentity,
      digest,
      tolerance,
      smallest,
      component.fingerprint
    )

  private def validateSymmetry(
      moments: DMat,
      tolerance: ResidualMomentTolerance
  ): Either[RelationError, Unit] =
    var row = 0
    while row < moments.rows do
      var column = row + 1
      while column < moments.cols do
        val left = moments(row, column)
        val right = moments(column, row)
        val scale = math.max(1.0, math.max(math.abs(left), math.abs(right)))
        if math.abs(left - right) > tolerance.toDouble * scale then
          return Left(
            RelationError.AsymmetricResidualMoments(row, column, left, right)
          )
        column += 1
      row += 1
    Right(())

  private def eigenvalueScale(
      spectrum: gale.spectral.EigenDecomposition
  ): Double =
    var scale = 1.0
    var index = 0
    while index < spectrum.eigenvalues.length do
      scale = math.max(scale, math.abs(spectrum.eigenvalues(index)))
      index += 1
    scale

  private def matrixDigest(moments: DMat): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-mvpa-residual-moments/v1")
    writer.int(moments.rows)
    writer.int(moments.cols)
    var row = 0
    while row < moments.rows do
      var column = 0
      while column < moments.cols do
        writer.double(moments(row, column))
        column += 1
      row += 1
    AxisDigest.sha256Hex(writer.result())

/** Residual-moment evidence paired with the certificate produced by checking that exact value. The private constructor
  * prevents swapping in equal-shape evidence from another neural axis or another value revision.
  */
final class CertifiedResidualMoments[
    N <: SemanticSpace,
    K
] private (
    val table: EvidenceTable[N, N, K, K],
    val certificate: ResidualMomentCertificate[N, K]
):
  def neural: AxisRef.Aux[K, N] = table.rows

object CertifiedResidualMoments:
  def apply[N <: SemanticSpace, K](
      table: EvidenceTable[N, N, K, K],
      tolerance: ResidualMomentTolerance = ResidualMomentTolerance.unsafe(1e-10)
  ): Either[RelationError, CertifiedResidualMoments[N, K]] =
    if table.rows.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(RelationError.InvalidNeuralPurpose(table.rows.identity.purpose))
    else if table.columns.identity != table.rows.identity then
      Left(
        RelationError.ResidualMomentAxisMismatch(
          table.rows.identity.fingerprint,
          table.columns.identity.fingerprint
        )
      )
    else if !(table.columns.evidence eq table.rows.evidence) then Left(RelationError.ResidualMomentWitnessMismatch)
    else
      for
        moments <- table
          .rightMultiply(DMat.eye(table.columnCount))
          .left
          .map(RelationError.Evidence.apply)
        certificate <- ResidualMomentCertificate.verify(
          table.rows,
          moments,
          table.table.valueIdentity,
          tolerance
        )
      yield new CertifiedResidualMoments(table, certificate)

opaque type ResidualDegreesOfFreedom = Double

object ResidualDegreesOfFreedom:
  def apply(value: Double): Either[RelationError, ResidualDegreesOfFreedom] =
    if !value.isFinite || value <= 0.0 then Left(RelationError.InvalidDegreesOfFreedom(value))
    else Right(value)

  private[mvpa] def unsafe(value: Double): ResidualDegreesOfFreedom =
    value

  extension (value: ResidualDegreesOfFreedom) inline def toDouble: Double = value

final class EstimateOnlyCapabilities[
    N <: SemanticSpace,
    K
] private (
    val neuralAxis: AxisRef.Aux[K, N],
    val identity: ScientificComponentFingerprint
) extends RelationCapabilities[N, K]

object EstimateOnlyCapabilities:
  private val Kind = EstimandKind.unsafe("estimate-only-capabilities")

  def apply[K](
      neural: AxisRef[K]
  ): Either[RelationError, EstimateOnlyCapabilities[neural.Id, K]] =
    capabilityIdentity(Kind, neural, Vector.empty).map: identity =>
      new EstimateOnlyCapabilities(neural, identity)

final class ResidualFitCapabilities[
    N <: SemanticSpace,
    K
] private (
    val residualMoments: CertifiedResidualMoments[N, K],
    val residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    val identity: ScientificComponentFingerprint
) extends HasResidualMoments[N, K]
    with HasResidualDegreesOfFreedom[N, K]:
  def neuralAxis: AxisRef.Aux[K, N] = residualMoments.neural

object ResidualFitCapabilities:
  private val Kind = EstimandKind.unsafe("residual-fit-capabilities")

  def apply[N <: SemanticSpace, K](
      residualMoments: CertifiedResidualMoments[N, K],
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  ): Either[RelationError, ResidualFitCapabilities[N, K]] =
    capabilityIdentity(
      Kind,
      residualMoments.neural,
      Vector(
        "degrees-of-freedom" -> residualDegreesOfFreedom.toDouble.toString,
        "residual-moment-certificate" -> residualMoments.certificate.identity.value,
        "residual-moments" -> residualMoments.table.table.valueIdentity.stableKey
      )
    ).map: identity =>
      new ResidualFitCapabilities(
        residualMoments,
        residualDegreesOfFreedom,
        identity
      )

final class PrecisionFitCapabilities[
    N <: SemanticSpace,
    K
] private (
    val residualMoments: CertifiedResidualMoments[N, K],
    val noisePrecision: CertifiedNoisePrecision[N, K],
    val residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    val identity: ScientificComponentFingerprint
) extends HasResidualMoments[N, K]
    with HasNoisePrecision[N, K]
    with HasResidualDegreesOfFreedom[N, K]:
  def neuralAxis: AxisRef.Aux[K, N] = residualMoments.neural

object PrecisionFitCapabilities:
  private val Kind = EstimandKind.unsafe("precision-fit-capabilities")

  def apply[N <: SemanticSpace, K](
      residualMoments: CertifiedResidualMoments[N, K],
      noisePrecision: CertifiedNoisePrecision[N, K],
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  ): Either[RelationError, PrecisionFitCapabilities[N, K]] =
    for
      _ <- validateCertifiedAxes(residualMoments.neural, noisePrecision.neural)
      identity <- capabilityIdentity(
        Kind,
        noisePrecision.neural,
        Vector(
          "degrees-of-freedom" -> residualDegreesOfFreedom.toDouble.toString,
          "noise-precision" -> noisePrecision.table.table.valueIdentity.stableKey,
          "precision-certificate" -> noisePrecision.certificate.identity.value,
          "residual-moment-certificate" -> residualMoments.certificate.identity.value,
          "residual-moments" -> residualMoments.table.table.valueIdentity.stableKey
        )
      )
    yield new PrecisionFitCapabilities(
      residualMoments,
      noisePrecision,
      residualDegreesOfFreedom,
      identity
    )

private def capabilityIdentity[K](
    kind: EstimandKind,
    neural: AxisRef[K],
    fields: Vector[(String, String)]
): Either[RelationError, ScientificComponentFingerprint] =
  if neural.identity.purpose != AxisPurpose.NeuralFeatures then
    Left(RelationError.InvalidNeuralPurpose(neural.identity.purpose))
  else
    EstimandIdentity(
      kind,
      ("neural" -> neural.identity.fingerprint.value) +: fields
    ).left.map(RelationError.Identity.apply).map(_.fingerprint)

private def validateCertifiedAxes[N <: SemanticSpace, K](
    expected: AxisRef.Aux[K, N],
    actual: AxisRef.Aux[K, N]
): Either[RelationError, Unit] =
  if actual.identity != expected.identity then
    Left(
      RelationError.CapabilityAxisMismatch(
        expected.identity.fingerprint,
        actual.identity.fingerprint
      )
    )
  else if !(actual.evidence eq expected.evidence) then Left(RelationError.CapabilityWitnessMismatch)
  else Right(())

/** One fitted experimental-neural relation, with mandatory estimability and an open statically visible capability
  * payload.
  */
final class Relation[
    Effects <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    EffectKey,
    NeuralCoordinate,
    +Capabilities <: RelationCapabilities[NeuralSpace, NeuralCoordinate]
] private (
    val estimate: EvidenceTable[
      Effects,
      NeuralSpace,
      EffectKey,
      NeuralCoordinate
    ],
    val receipt: RelationFitReceipt[Effects, EffectKey],
    val capabilities: Capabilities
)

object Relation:
  def apply[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      estimate: EvidenceTable[E, N, EK, NK],
      receipt: RelationFitReceipt[E, EK],
      capabilities: C
  ): Either[RelationError, Relation[E, N, EK, NK, C]] =
    if estimate.rows.identity != receipt.estimability.effects.identity then
      Left(
        RelationError.EffectAxisMismatch(
          receipt.estimability.effects.identity.fingerprint,
          estimate.rows.identity.fingerprint
        )
      )
    else if !(estimate.rows.evidence eq receipt.estimability.effects.evidence) then
      Left(RelationError.EffectWitnessMismatch)
    else if estimate.columns.identity != capabilities.neuralAxis.identity then
      Left(
        RelationError.CapabilityAxisMismatch(
          estimate.columns.identity.fingerprint,
          capabilities.neuralAxis.identity.fingerprint
        )
      )
    else if !(estimate.columns.evidence eq capabilities.neuralAxis.evidence) then
      Left(RelationError.CapabilityWitnessMismatch)
    else Right(new Relation(estimate, receipt, capabilities))

final case class PartitionRelation[
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK,
    +C <: RelationCapabilities[N, NK]
](
    partition: PartitionId,
    relation: Relation[E, N, EK, NK, C]
)

/** Exact key-to-relation evidence. The constructor accepts only keyed entries, validates total one-to-one coverage, and
  * stores the canonical partition order privately so a naked positional vector never becomes scientific evidence.
  */
final class PartitionedRelations[
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
    private val ordered: Vector[
      PartitionRelation[Effects, NeuralSpace, EffectKey, NeuralCoordinate, Capabilities]
    ],
    private val byPartition: Map[
      PartitionId,
      Relation[Effects, NeuralSpace, EffectKey, NeuralCoordinate, Capabilities]
    ],
    val partitionAxisName: ScientificAxisName,
    val effectAxisName: ScientificAxisName,
    val neuralAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = NeuralCoordinate

  def size: Int = ordered.size

  def partitionKeys: Vector[PartitionId] =
    partitions.axis.keys

  def relation(
      partition: PartitionId
  ): Either[
    RelationError,
    Relation[Effects, NeuralSpace, EffectKey, NeuralCoordinate, Capabilities]
  ] =
    byPartition.get(partition).toRight(RelationError.RelationNotFound(partition))

  def foreachRelation(
      function: (
          PartitionId,
          Relation[Effects, NeuralSpace, EffectKey, NeuralCoordinate, Capabilities]
      ) => Unit
  ): Unit =
    ordered.foreach(entry => function(entry.partition, entry.relation))

object PartitionedRelations:
  private val Protocol = "scalafim-mvpa-partitioned-relations/v1"

  def apply[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      partitions: PartitionAxis[P],
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      relations: Seq[PartitionRelation[E, N, EK, NK, C]],
      effectAxisName: ScientificAxisName = ScientificAxisName.unsafe("effects"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural")
  ): Either[RelationError, PartitionedRelations[P, E, N, EK, NK, C]] =
    if partitions.axis.identity.purpose != AxisPurpose.Partitions then
      Left(RelationError.PartitionPurposeMismatch(partitions.axis.identity.purpose))
    else if effects.identity.purpose != AxisPurpose.Effects then
      Left(RelationError.InvalidEffectPurpose(effects.identity.purpose))
    else if neural.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(RelationError.InvalidNeuralPurpose(neural.identity.purpose))
    else if relations.isEmpty then Left(RelationError.EmptyRelations)
    else
      val input = relations.toVector
      val indexed = scala.collection.mutable.HashMap.empty[
        PartitionId,
        Relation[E, N, EK, NK, C]
      ]
      val iterator = input.iterator
      while iterator.hasNext do
        val entry = iterator.next()
        if partitions.axis.positionOf(entry.partition).isEmpty then
          return Left(RelationError.UnknownPartition(entry.partition))
        if indexed.contains(entry.partition) then return Left(RelationError.DuplicatePartition(entry.partition))
        validateRelation(entry.partition, effects, neural, entry.relation) match
          case Left(error) => return Left(error)
          case Right(_)    => indexed += entry.partition -> entry.relation

      val canonical = Vector.newBuilder[PartitionRelation[E, N, EK, NK, C]]
      var partitionPosition = 0
      while partitionPosition < partitions.axis.size do
        val partition = partitions.axis.keys(partitionPosition)
        indexed.get(partition) match
          case None           => return Left(RelationError.MissingPartition(partition))
          case Some(relation) => canonical += PartitionRelation(partition, relation)
        partitionPosition += 1
      val ordered = canonical.result()
      val digest = relationDigest(ordered)
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("partitioned-relations"),
        Vector(
          ScientificSourceAxis(partitions.name, partitions.axis.identity),
          ScientificSourceAxis(effectAxisName, effects.identity),
          ScientificSourceAxis(neuralAxisName, neural.identity)
        ),
        Vector(
          "protocol" -> Protocol,
          "relations" -> digest
        )
      ).left
        .map(RelationError.Identity.apply)
        .map: identity =>
          new PartitionedRelations(
            partitions,
            effects,
            neural,
            ordered,
            indexed.toMap,
            partitions.name,
            effectAxisName,
            neuralAxisName,
            identity
          )

  private def validateRelation[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      partition: PartitionId,
      effects: AxisRef.Aux[EK, E],
      neural: AxisRef.Aux[NK, N],
      relation: Relation[E, N, EK, NK, C]
  ): Either[RelationError, Unit] =
    if relation.estimate.rows.identity != effects.identity then
      Left(
        RelationError.RelationEffectsMismatch(
          partition,
          effects.identity.fingerprint,
          relation.estimate.rows.identity.fingerprint
        )
      )
    else if !(relation.estimate.rows.evidence eq effects.evidence) then
      Left(RelationError.RelationEffectWitnessMismatch(partition))
    else if relation.estimate.columns.identity != neural.identity then
      Left(
        RelationError.RelationNeuralMismatch(
          partition,
          neural.identity.fingerprint,
          relation.estimate.columns.identity.fingerprint
        )
      )
    else if !(relation.estimate.columns.evidence eq neural.evidence) then
      Left(RelationError.RelationNeuralWitnessMismatch(partition))
    else Right(())

  private def relationDigest[
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK]
  ](
      relations: Vector[PartitionRelation[E, N, EK, NK, C]]
  ): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.int(relations.length)
    relations.foreach: entry =>
      writer.string(entry.partition.value)
      writer.string(entry.relation.estimate.table.valueIdentity.stableKey)
      writer.string(entry.relation.receipt.identity.value)
      writer.string(entry.relation.capabilities.identity.value)
    AxisDigest.sha256Hex(writer.result())
