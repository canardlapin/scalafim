package scalafim.fmri.mvpa

enum ExecutionPlanError:
  case InvalidIdentity(error: ScientificIdentityError)
  case InvalidParallelism(value: Int)
  case InvalidChunkSize(value: Int)
  case EmptyFallbackChain
  case DuplicateFallbackTarget(target: ExecutionTarget)
  case FallbackTargetMatchesRequested(target: ExecutionTarget)
  case DuplicateRandomStream(id: RandomStreamId)
  case UnsupportedRepresentation(
      requested: ExecutionRepresentation,
      supported: Vector[ExecutionRepresentation]
  )
  case UnsupportedDelivery(
      requested: ResultDelivery,
      supported: Vector[ResultDelivery]
  )
  case UnsupportedPrecision(
      requested: NumericPrecision,
      supported: Vector[NumericPrecision]
  )
  case UnsupportedSolver(requested: SolverChoice)
  case MaterializationRequired(reason: String)
  case InvalidExecutionFingerprint(value: String)

  def message: String =
    this match
      case InvalidIdentity(error)    => error.message
      case InvalidParallelism(value) =>
        s"execution parallelism must be positive, obtained $value"
      case InvalidChunkSize(value) =>
        s"execution chunk size must be positive, obtained $value"
      case EmptyFallbackChain =>
        "an explicit fallback policy must name at least one permitted target"
      case DuplicateFallbackTarget(target) =>
        s"execution fallback target ${target.label} is duplicated"
      case FallbackTargetMatchesRequested(target) =>
        s"execution fallback target ${target.label} is already the requested target"
      case DuplicateRandomStream(id) =>
        s"random stream '${id.value}' is duplicated"
      case UnsupportedRepresentation(requested, supported) =>
        s"execution representation '${requested.label}' is unsupported; expected one of ${supported.map(_.label).mkString(", ")}"
      case UnsupportedDelivery(requested, supported) =>
        s"result delivery '${requested.label}' is unsupported; expected one of ${supported.map(_.label).mkString(", ")}"
      case UnsupportedPrecision(requested, supported) =>
        s"numeric precision '${requested.label}' is unsupported; expected one of ${supported.map(_.label).mkString(", ")}"
      case UnsupportedSolver(requested) =>
        s"execution solver '${requested.canonicalLabel}' is unsupported for this task"
      case MaterializationRequired(reason) =>
        s"execution requires explicit materialization permission: $reason"
      case InvalidExecutionFingerprint(value) =>
        s"invalid execution fingerprint '$value'"

opaque type BackendId = String

object BackendId:
  def apply(value: String): Either[ExecutionPlanError, BackendId] =
    ScientificIdentityText
      .lowerIdentifier("execution backend id", value)
      .left
      .map(ExecutionPlanError.InvalidIdentity.apply)

  private[mvpa] def unsafe(value: String): BackendId =
    value

  extension (id: BackendId) inline def value: String = id

opaque type SolverId = String

object SolverId:
  def apply(value: String): Either[ExecutionPlanError, SolverId] =
    ScientificIdentityText
      .lowerIdentifier("solver id", value)
      .left
      .map(ExecutionPlanError.InvalidIdentity.apply)

  private[mvpa] def unsafe(value: String): SolverId =
    value

  extension (id: SolverId) inline def value: String = id

opaque type RandomStreamId = String

object RandomStreamId:
  def apply(value: String): Either[ExecutionPlanError, RandomStreamId] =
    ScientificIdentityText
      .lowerIdentifier("random stream id", value)
      .left
      .map(ExecutionPlanError.InvalidIdentity.apply)

  private[mvpa] def unsafe(value: String): RandomStreamId =
    value

  extension (id: RandomStreamId) inline def value: String = id

enum ExecutionRepresentation:
  case Dense
  case Operator
  case Fused
  case SufficientStatistics

  def label: String =
    this match
      case Dense                => "dense"
      case Operator             => "operator"
      case Fused                => "fused"
      case SufficientStatistics => "sufficient-statistics"

enum ResultDelivery:
  case Streaming
  case Collected

  def label: String =
    this match
      case Streaming => "streaming"
      case Collected => "collected"

enum NumericPrecision:
  case Binary32
  case Binary64

  def label: String =
    this match
      case Binary32 => "binary32"
      case Binary64 => "binary64"

enum CompletionOrder:
  case MeasurementOrder
  case AsCompleted

  def label: String =
    this match
      case MeasurementOrder => "measurement-order"
      case AsCompleted      => "completion-order"

final class Scheduling private (
    val parallelism: Int,
    val chunkSize: Int,
    val completionOrder: CompletionOrder
):
  override def equals(other: Any): Boolean =
    other match
      case that: Scheduling =>
        parallelism == that.parallelism &&
        chunkSize == that.chunkSize &&
        completionOrder == that.completionOrder
      case _ => false

  override def hashCode(): Int =
    (parallelism, chunkSize, completionOrder).hashCode

object Scheduling:
  val serial: Scheduling =
    new Scheduling(1, 1, CompletionOrder.MeasurementOrder)

  def parallel(
      parallelism: Int,
      chunkSize: Int,
      completionOrder: CompletionOrder
  ): Either[ExecutionPlanError, Scheduling] =
    if parallelism <= 0 then Left(ExecutionPlanError.InvalidParallelism(parallelism))
    else if chunkSize <= 0 then Left(ExecutionPlanError.InvalidChunkSize(chunkSize))
    else Right(new Scheduling(parallelism, chunkSize, completionOrder))

final class SolverIdentity private (
    val id: SolverId,
    val fields: Vector[AxisDescriptorField]
):
  override def equals(other: Any): Boolean =
    other match
      case that: SolverIdentity => id == that.id && fields == that.fields
      case _                    => false

  override def hashCode(): Int =
    (id, fields).hashCode

object SolverIdentity:
  def apply(
      id: SolverId,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ExecutionPlanError, SolverIdentity] =
    ScientificIdentityComponents
      .fields(fields)
      .left
      .map(ExecutionPlanError.InvalidIdentity.apply)
      .map(new SolverIdentity(id, _))

  private[mvpa] def trusted(
      id: SolverId,
      fields: Seq[(String, String)] = Vector.empty
  ): SolverIdentity =
    new SolverIdentity(id, ScientificIdentityComponents.trustedFields(fields))

enum SolverChoice:
  case NotApplicable
  case Selected(identity: SolverIdentity)

  private[mvpa] def canonicalLabel: String =
    this match
      case SolverChoice.NotApplicable      => "not-applicable"
      case SolverChoice.Selected(identity) =>
        val writer = CanonicalWriter()
        writer.string(identity.id.value)
        writer.fields(identity.fields)
        AxisDigest.hex(writer.result())

final case class RandomStream(id: RandomStreamId, seed: Long)

final case class ExecutionTarget(
    backend: BackendId,
    representation: ExecutionRepresentation
):
  def label: String =
    s"${backend.value}/${representation.label}"

final class FallbackPolicy private (
    val permittedTargets: Vector[ExecutionTarget]
):
  def permits(target: ExecutionTarget): Boolean =
    permittedTargets.contains(target)

  def isForbidden: Boolean =
    permittedTargets.isEmpty

  override def equals(other: Any): Boolean =
    other match
      case that: FallbackPolicy => permittedTargets == that.permittedTargets
      case _                    => false

  override def hashCode(): Int =
    permittedTargets.hashCode

object FallbackPolicy:
  val forbidden: FallbackPolicy =
    new FallbackPolicy(Vector.empty)

  def explicit(
      targets: Seq[ExecutionTarget]
  ): Either[ExecutionPlanError, FallbackPolicy] =
    val values = targets.toVector
    if values.isEmpty then Left(ExecutionPlanError.EmptyFallbackChain)
    else
      var index = 0
      while index < values.length do
        val target = values(index)
        if values.take(index).contains(target) then return Left(ExecutionPlanError.DuplicateFallbackTarget(target))
        index += 1
      Right(new FallbackPolicy(values))

opaque type ExecutionStrategyFingerprint = String

object ExecutionStrategyFingerprint:
  private val Prefix = "scalafim-mvpa-strategy-v1-"

  def apply(value: String): Either[ExecutionPlanError, ExecutionStrategyFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(ExecutionPlanError.InvalidExecutionFingerprint(value))

  private[mvpa] def fromDigest(digest: String): ExecutionStrategyFingerprint =
    Prefix + digest

  extension (fingerprint: ExecutionStrategyFingerprint) inline def value: String = fingerprint

final class ExecutionStrategy private (
    val backend: BackendId,
    val representation: ExecutionRepresentation,
    val precision: NumericPrecision,
    val solver: SolverChoice,
    val randomStreams: Vector[RandomStream],
    val scheduling: Scheduling,
    val materialization: MaterializationPolicy,
    val fallback: FallbackPolicy,
    val delivery: ResultDelivery,
    val fields: Vector[AxisDescriptorField],
    val fingerprint: ExecutionStrategyFingerprint
):
  def target: ExecutionTarget =
    ExecutionTarget(backend, representation)

  override def equals(other: Any): Boolean =
    other match
      case that: ExecutionStrategy => fingerprint == that.fingerprint
      case _                       => false

  override def hashCode(): Int =
    fingerprint.hashCode

object ExecutionStrategy:
  val Protocol = "scalafim-mvpa-execution-strategy/v1"

  def apply(
      backend: BackendId,
      representation: ExecutionRepresentation,
      precision: NumericPrecision,
      solver: SolverChoice,
      randomStreams: Seq[RandomStream],
      scheduling: Scheduling,
      materialization: MaterializationPolicy,
      fallback: FallbackPolicy,
      delivery: ResultDelivery,
      fields: Seq[(String, String)] = Vector.empty
  ): Either[ExecutionPlanError, ExecutionStrategy] =
    val streams = randomStreams.toVector.sortBy(_.id.value)
    streams
      .sliding(2)
      .collectFirst:
        case Vector(left, right) if left.id == right.id => left.id
    match
      case Some(duplicate) => Left(ExecutionPlanError.DuplicateRandomStream(duplicate))
      case None            =>
        val requested = ExecutionTarget(backend, representation)
        if fallback.permittedTargets.contains(requested) then
          Left(ExecutionPlanError.FallbackTargetMatchesRequested(requested))
        else
          ScientificIdentityComponents
            .fields(fields)
            .left
            .map(ExecutionPlanError.InvalidIdentity.apply)
            .map: canonicalFields =>
              val bytes = canonicalBytes(
                backend,
                representation,
                precision,
                solver,
                streams,
                scheduling,
                materialization,
                fallback,
                delivery,
                canonicalFields
              )
              new ExecutionStrategy(
                backend,
                representation,
                precision,
                solver,
                streams,
                scheduling,
                materialization,
                fallback,
                delivery,
                canonicalFields,
                ExecutionStrategyFingerprint.fromDigest(AxisDigest.sha256Hex(bytes))
              )

  private def canonicalBytes(
      backend: BackendId,
      representation: ExecutionRepresentation,
      precision: NumericPrecision,
      solver: SolverChoice,
      randomStreams: Vector[RandomStream],
      scheduling: Scheduling,
      materialization: MaterializationPolicy,
      fallback: FallbackPolicy,
      delivery: ResultDelivery,
      fields: Vector[AxisDescriptorField]
  ): Array[Byte] =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(backend.value)
    writer.string(representation.label)
    writer.string(precision.label)
    writer.string(solver.canonicalLabel)
    writer.int(randomStreams.length)
    randomStreams.foreach: stream =>
      writer.string(stream.id.value)
      writer.string(stream.seed.toString)
    writer.int(scheduling.parallelism)
    writer.int(scheduling.chunkSize)
    writer.string(scheduling.completionOrder.label)
    materialization match
      case MaterializationPolicy.Reject =>
        writer.string("reject")
        writer.string("0")
      case MaterializationPolicy.Allow(budget) =>
        writer.string("allow")
        writer.string(budget.maxElements.toString)
    writer.int(fallback.permittedTargets.length)
    fallback.permittedTargets.foreach: target =>
      writer.string(target.backend.value)
      writer.string(target.representation.label)
    writer.string(delivery.label)
    writer.fields(fields)
    writer.result()

opaque type ExecutionPlanFingerprint = String

object ExecutionPlanFingerprint:
  private val Prefix = "scalafim-mvpa-execution-v1-"

  def apply(value: String): Either[ExecutionPlanError, ExecutionPlanFingerprint] =
    val digest = value.stripPrefix(Prefix)
    if value.startsWith(Prefix) && digest.length == 64 && digest.forall(AxisText.isLowerHexDigit) then Right(value)
    else Left(ExecutionPlanError.InvalidExecutionFingerprint(value))

  private[mvpa] def fromDigest(digest: String): ExecutionPlanFingerprint =
    Prefix + digest

  extension (fingerprint: ExecutionPlanFingerprint) inline def value: String = fingerprint

final class ExecutionPlanIdentity private (
    val scientific: ScientificPlanFingerprint,
    val strategy: ExecutionStrategyFingerprint,
    val fingerprint: ExecutionPlanFingerprint
):
  override def equals(other: Any): Boolean =
    other match
      case that: ExecutionPlanIdentity =>
        scientific == that.scientific &&
        strategy == that.strategy &&
        fingerprint == that.fingerprint
      case _ => false

  override def hashCode(): Int =
    fingerprint.hashCode

  override def toString: String =
    s"ExecutionPlanIdentity(${fingerprint.value})"

object ExecutionPlanIdentity:
  val Protocol = "scalafim-mvpa-execution-plan/v1"

  /** Canonically identify one scientific plan and one validated execution strategy. This is identity data, not
    * execution-plan admission.
    */
  def apply(
      scientific: ScientificPlanFingerprint,
      strategy: ExecutionStrategyFingerprint
  ): ExecutionPlanIdentity =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(scientific.value)
    writer.string(strategy.value)
    new ExecutionPlanIdentity(
      scientific,
      strategy,
      ExecutionPlanFingerprint.fromDigest(AxisDigest.sha256Hex(writer.result()))
    )

/** One task contract spans every numerical representation and result-delivery mode. P2.5 supplies the execution
  * operation; this capability already owns strategy admission so unsupported paths fail before numerical work.
  */
trait MeasurementTask[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition,
    Prepared
]:
  def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit]

  def execute(
      plan: BoundScientificPlan[Source, Design, E, Rendition, Prepared]
  )(
      measurement: MeasurementEntry[
        plan.specification.source.Neural,
        plan.specification.source.NeuralKey,
        ?,
        Rendition
      ],
      context: TaskContext
  ): Either[
    TaskReportError,
    TaskReport[plan.Result, plan.Rejection, plan.Failure]
  ]

final class ExecutionPlan[
    Source <: ScientificSource,
    Design <: EvidenceDesign,
    E <: Estimand[Source, Design],
    Rendition,
    Prepared
] private (
    val scientific: BoundScientificPlan[Source, Design, E, Rendition, Prepared],
    val strategy: ExecutionStrategy,
    val identity: ExecutionPlanIdentity,
    private[mvpa] val task: MeasurementTask[Source, Design, E, Rendition, Prepared]
):
  def measurementCount: Int =
    scientific.specification.frame.size

object ExecutionPlan:
  def apply[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition,
      Prepared
  ](
      scientific: BoundScientificPlan[Source, Design, E, Rendition, Prepared],
      strategy: ExecutionStrategy
  )(using
      task: MeasurementTask[Source, Design, E, Rendition, Prepared]
  ): Either[
    ExecutionPlanError,
    ExecutionPlan[Source, Design, E, Rendition, Prepared]
  ] =
    task
      .validate(strategy)
      .map: _ =>
        new ExecutionPlan(
          scientific,
          strategy,
          ExecutionPlanIdentity(scientific.identity.fingerprint, strategy.fingerprint),
          task
        )
