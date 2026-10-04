package scalafim.fmri.mvpa.artifacts

import scalafim.estimates.FileReference
import scalafim.fmri.mvpa.{AxisRecord, AxisRef}
import scalafim.fmri.mvpa.pattern.*

/** A declaration of an attempted fit. It deliberately makes no validity claim
  * beyond complete declared unit coverage. */
final case class PatternScientificDeclaration(
    planIdentity: String,
    measurementIdentity: String,
    valueIdentity: String,
    sourceRevisions: Vector[(String, String)],
    seed: Long,
    reducerIdentity: String,
    expectedUnitIds: Vector[String],
    committedUnitIds: Vector[String]
):
  require(planIdentity.nonEmpty && measurementIdentity.nonEmpty && valueIdentity.nonEmpty && reducerIdentity.nonEmpty)
  require(sourceRevisions.nonEmpty && sourceRevisions.forall { case (name, revision) => name.nonEmpty && revision.nonEmpty })
  require(expectedUnitIds.nonEmpty && expectedUnitIds.distinct == expectedUnitIds)
  require(committedUnitIds == expectedUnitIds)

object PatternScientificDeclaration:
  def coverage(expected: Vector[String], committed: Vector[String]): Either[PatternArchiveError, Unit] =
    if expected.isEmpty || expected.distinct != expected then Left(PatternArchiveError.Invalid("expected unit ids must be nonempty and unique"))
    else if committed != expected then Left(PatternArchiveError.Invalid("committed unit ids must exactly match expected unit ids"))
    else Right(())

enum PatternArchiveError:
  case Invalid(detail: String)
  case Unsupported(detail: String)
  case Integrity(detail: String)
  case Io(detail: String)
  def message: String = this match
    case Invalid(detail) => detail
    case Unsupported(detail) => detail
    case Integrity(detail) => detail
    case Io(detail) => detail

final case class PatternArchiveLimits(maximumMetadataBytes: Int = 1024 * 1024, maximumCells: Long = 64L * 1024L * 1024L):
  require(maximumMetadataBytes > 0 && maximumCells > 0 && maximumCells <= Int.MaxValue.toLong)

enum PatternProfileState:
  case Completed
  case Unsupported(detail: String)
  case InMemoryOnly(detail: String)

final case class ProfileAxis(record: AxisRecord)

final case class ProfilePayload(name: String, rows: Int, columns: Int, reference: FileReference):
  require(name.nonEmpty && rows > 0 && columns > 0)
  require(ProfilePayload.bytes(rows, columns).contains(reference.bytes))
object ProfilePayload:
  def cells(rows: Int, columns: Int): Option[Long] =
    try Some(Math.multiplyExact(rows.toLong, columns.toLong))
    catch case _: ArithmeticException => None
  def bytes(rows: Int, columns: Int): Option[Long] =
    cells(rows, columns).flatMap: value =>
      try Some(Math.multiplyExact(value, 8L))
      catch case _: ArithmeticException => None

/** Complete v1 profile. Numerical values are hash-pinned external leaves. */
final case class CompletedPatternProfile(
    version: Int,
    declaration: PatternScientificDeclaration,
    neural: ProfileAxis,
    target: ProfileAxis,
    components: ProfileAxis,
    training: ProfileAxis,
    targetGeometry: TargetGeometry,
    centering: CenteringPolicy,
    degenerateTarget: DegenerateTargetPolicy,
    residualCovariance: ResidualCovarianceCapability,
    trainingBinding: TrainingBinding,
    trainingLineage: Vector[String],
    diagnostics: PatternFitDiagnostics,
    interpretation: InterpretationStatus,
    gauge: GaugeEvidence,
    coordinateGauge: CoordinateGauge,
    payloads: Vector[ProfilePayload],
    metadata: FileReference
):
  require(version == CompletedPatternProfile.CurrentVersion)
  require(payloads.map(_.name).distinct == payloads.map(_.name))

object CompletedPatternProfile:
  val CurrentVersion = 1
  def compatible(left: CompletedPatternProfile, right: CompletedPatternProfile): Either[String, Unit] =
    if left.version != right.version then Left("profile version differs")
    else if left.declaration != right.declaration then Left("scientific declaration, seed, reducer, source revisions, or unit coverage differs")
    else if Vector(left.neural, left.target, left.components, left.training) != Vector(right.neural, right.target, right.components, right.training) then Left("ordered axis records differ")
    else if targetKey(left.targetGeometry) != targetKey(right.targetGeometry) || centeringKey(left.centering) != centeringKey(right.centering) || left.degenerateTarget != right.degenerateTarget || covarianceKey(left.residualCovariance) != covarianceKey(right.residualCovariance) then Left("artifact policy differs")
    else if bindingKey(left.trainingBinding) != bindingKey(right.trainingBinding) || left.trainingLineage != right.trainingLineage || diagnosticsKey(left.diagnostics) != diagnosticsKey(right.diagnostics) || left.interpretation != right.interpretation || left.gauge != right.gauge || left.coordinateGauge != right.coordinateGauge then Left("scoped fit declaration differs")
    else if left.payloads != right.payloads then Left("payload identities or factor content references differ")
    else Right(())

  private def valuesKey(value: AxisValues[?]): (AxisRecord, Vector[Long]) =
    value.axis.toRecord -> value.values.map(java.lang.Double.doubleToRawLongBits)
  private def targetKey(value: TargetGeometry): Any = value match
    case TargetGeometry.Categorical(x) => ("categorical", x.conditions.toRecord, x.targetAxis.toRecord, valuesKey(x.priors), Vector.tabulate(x.contrast.rows * x.contrast.cols)(i => java.lang.Double.doubleToRawLongBits(x.contrast(i / x.contrast.cols, i % x.contrast.cols))) )
    case TargetGeometry.Continuous(x) => ("continuous", x.targetAxis.toRecord, valuesKey(x.priorScale), valuesKey(x.metricDiagonal), x.blockWeights.map((name, value) => name -> valuesKey(value)))
  private def centeringKey(value: CenteringPolicy): Any = value match
    case CenteringPolicy.CenteredBeforeFit(neural, target) => ("centered", neural, target)
    case CenteringPolicy.ExplicitIntercept(values, receipt) => ("intercept", valuesKey(values), receipt)
  private def covarianceKey(value: ResidualCovarianceCapability): Any = value match
    case ResidualCovarianceCapability.NotFitted => "none"
    case ResidualCovarianceCapability.DiagonalPlusLowRank(axis, rank) => ("lowrank", axis.stableKey, rank)
    case ResidualCovarianceCapability.ProviderBacked(axis, name) => ("provider", axis.stableKey, name)
  private def bindingKey(value: TrainingBinding): Any = (value.declaredSampleAxis.stableKey, value.source, value.fingerprintDigest)
  private def diagnosticsKey(value: PatternFitDiagnostics): Any = (value.objective.map(java.lang.Double.doubleToRawLongBits), value.solver, value.notes)

/** The training axis must be supplied by the reader and checked against the
  * binding descriptor; persisted stable keys alone do not prove its semantics. */
final case class RestoredPatternArtifact(profile: CompletedPatternProfile, artifact: PatternArtifact, trainingAxis: AxisRef[?])

object PatternArchiveProfile:
  /** M3.11 persists completed declarations only; it never advertises a resumable
    * fitter checkpoint through the durable profile surface. */
  def resumeCheckpoint(): Either[PatternArchiveError, Nothing] =
    Left(PatternArchiveError.Unsupported("durable fitter checkpoint resumption is not supported by pattern profile v1"))
