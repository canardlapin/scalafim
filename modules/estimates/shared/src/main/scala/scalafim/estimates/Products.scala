package scalafim.estimates

enum NumericPrecision:
  case Float32, Float64

enum StatisticKind:
  case T, Z, P, F

enum ProductKind:
  case Effect, StandardError, Variance, ResidualVariance, Covariance
  case Statistic(kind: StatisticKind)

  def accepts(value: Double): Boolean = value.isFinite && (this match
    case StandardError | Variance | ResidualVariance => value >= 0.0
    case Statistic(StatisticKind.P) => value >= 0.0 && value <= 1.0
    case Statistic(StatisticKind.F) => value >= 0.0
    case _ => true)

enum ProductOutcome:
  case Available(product: ProductId)
  case NotRequested
  case Unsupported(reason: String)
  case Failed(reason: String)

  private[estimates] def valid: Boolean = this match
    case Unsupported(reason) => Invariants.text(reason)
    case Failed(reason) => Invariants.text(reason)
    case _ => true

/** Covariance cells have their own indexed pair axis, never an implicit t axis. */
enum ProductTargets:
  case Scalar(ids: Vector[EstimandId])
  case UpperTriangle(ids: Vector[EstimandId])

  def estimands: Vector[EstimandId] = this match
    case Scalar(ids) => ids
    case UpperTriangle(ids) => ids

  def width: Long = this match
    case Scalar(ids) => ids.size.toLong
    case UpperTriangle(ids) => ids.size.toLong * (ids.size.toLong + 1L) / 2L

  def pairs: Vector[(EstimandId, EstimandId)] = this match
    case Scalar(_) => Vector.empty
    case UpperTriangle(ids) => ids.indices.toVector.flatMap(i => (i until ids.size).map(j => ids(i) -> ids(j)))

final case class ProductDescriptor(
    id: ProductId,
    kind: ProductKind,
    precision: NumericPrecision,
    observations: Vector[ObservationId],
    targets: ProductTargets,
    pooling: PoolingScope,
    units: String
):
  require(Invariants.unique(observations) && Invariants.unique(targets.estimands))
  require(targets.width <= Int.MaxValue)
  require(Invariants.text(units))
  require((kind == ProductKind.Covariance) == targets.isInstanceOf[ProductTargets.UpperTriangle],
    "covariance uses an explicit pair axis; scalar products use an estimand axis")

/** A distribution is supplied only when its parameters and interpretation are known. */
enum ReferenceDistribution:
  case Unknown(reason: String)
  case Normal
  case StudentT(df: DegreesOfFreedom)
  case FisherF(numerator: DegreesOfFreedom, denominator: DegreesOfFreedom)

enum DfRole:
  case Residual, Effective, Reference

enum DfValue:
  case Unknown(reason: String)
  case NotApplicable
  case Scalar(value: Double)
  case Product(id: ProductId)

final case class DegreesOfFreedom(role: DfRole, value: DfValue, method: String, approximate: Boolean):
  require(Invariants.text(method))
  value match
    case DfValue.Scalar(df) => require(df.isFinite && (if role == DfRole.Residual then df >= 0 else df > 0))
    case DfValue.Unknown(reason) => require(Invariants.text(reason))
    case _ => ()

enum TestTail:
  case Lower, Upper, TwoSided

final case class StatisticSemantics(
    product: ProductId,
    distribution: ReferenceDistribution,
    tail: Option[TestTail],
    nullValue: Option[Double],
    effect: Option[ProductId],
    standardError: Option[ProductId]
):
  require(nullValue.forall(_.isFinite))

/** Scaling is always a variance multiplier; never silently square an SD. */
enum CovarianceEquation:
  case Absolute
  case Normalized(varianceScale: ProductId)

final case class CovarianceDescriptor(
    product: ProductId,
    effects: ProductId,
    equation: CovarianceEquation,
    invariantObservations: Boolean,
    invariantSamples: Boolean
)

/** A full-rank assertion includes its fitted axis and numerical evidence.
  * A deficient design must carry a verifiable subspace/design reference.
  */
enum EstimabilityEvidence:
  case Unknown(reason: String)
  case FullRank(columns: Vector[ColumnId], observations: Int, tolerance: Double, method: String)
  case Design(columns: Vector[ColumnId], matrix: FileReference)
  case Subspace(columns: Vector[ColumnId], basis: FileReference, rank: Int, tolerance: Double, method: String)

  private[estimates] def valid: Boolean = this match
    case Unknown(reason) => Invariants.text(reason)
    case FullRank(columns, observations, tolerance, method) =>
      Invariants.unique(columns) && observations >= columns.size && tolerance.isFinite && tolerance >= 0 && Invariants.text(method)
    case Design(columns, _) => Invariants.unique(columns)
    case Subspace(columns, _, rank, tolerance, method) =>
      Invariants.unique(columns) && rank >= 0 && rank <= columns.size && tolerance.isFinite && tolerance >= 0 && Invariants.text(method)
