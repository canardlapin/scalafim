package scalafim.spatial

enum FieldExplanationPhase:
  case Descriptive, Planned, Evaluated

final case class FieldShape(samples: Int, observations: Int):
  require(samples >= 0, "field explanation sample count must be non-negative")
  require(observations >= 0, "field explanation observation count must be non-negative")

final case class FieldSourceExplanation(
  label: String,
  materialized: Boolean,
  revision: Option[FieldSourceRevision]
)

final case class ExplainedRouteStep(
  morphism: MorphismId,
  source: DomainId,
  target: DomainId,
  kind: MorphismKind,
  routeTag: RouteTag,
  cost: Double,
  inverse: Inverse,
  isInverted: Boolean,
  compiler: MorphismCompilerId
):
  require(cost.isFinite && cost >= 0.0, "explained route cost must be finite and non-negative")

enum ExplainedProgramStage:
  case Coordinate(
    morphismId: MorphismId,
    compiler: MorphismCompilerId,
    fingerprint: String
  )
  case Value(
    morphismId: MorphismId,
    compiler: MorphismCompilerId,
    stageSemantics: StageSemantics,
    plugin: Option[MorphismPluginId],
    fingerprint: String
  )

  def semantics: StageSemantics =
    this match
      case Coordinate(_, _, _) => StageSemantics.CoordinatePullback
      case Value(_, _, stageSemantics, _, _) => stageSemantics

  def morphism: MorphismId =
    this match
      case Coordinate(morphismId, _, _) => morphismId
      case Value(morphismId, _, _, _, _) => morphismId

object ExplainedProgramStage:
  private[spatial] def from(stage: ProgramStage): ExplainedProgramStage =
    stage match
      case ProgramStage.Coordinate(step) =>
        ExplainedProgramStage.Coordinate(
          step.morphism.id,
          step.compiler,
          step.coordinateMap.fingerprint
        )
      case ProgramStage.Value(value) =>
        ExplainedProgramStage.Value(
          value.morphism.id,
          value.compiler,
          value.semantics,
          value.morphism.plugin.map(_.id),
          value.fingerprint
        )

final case class FusionExplanation(
  coordinateStages: Int,
  barriers: Vector[MorphismId],
  valueResamplingPasses: Int
):
  require(coordinateStages >= 0, "coordinate-stage count must be non-negative")
  require(valueResamplingPasses >= 0 && valueResamplingPasses <= 1, "spatial resampling passes must be zero or one")

enum FieldQcPolicyError:
  case InvalidMinimumCoverage(value: Double)
  case InvalidMinimumPathQuality(value: Double)

  def message: String =
    this match
      case InvalidMinimumCoverage(value) =>
        s"minimum coverage must be finite and in [0, 1], got $value"
      case InvalidMinimumPathQuality(value) =>
        s"minimum path quality must be finite and in [0, 1], got $value"

final case class FieldQcPolicy private (
  minimumCoverage: Double,
  minimumPathQuality: Double,
  requireEveryTargetCovered: Boolean
)

object FieldQcPolicy:
  val permissive: FieldQcPolicy =
    new FieldQcPolicy(0.0, 0.0, requireEveryTargetCovered = false)

  def build(
    minimumCoverage: Double = 0.0,
    minimumPathQuality: Double = 0.0,
    requireEveryTargetCovered: Boolean = false
  ): Either[FieldQcPolicyError, FieldQcPolicy] =
    if !minimumCoverage.isFinite || minimumCoverage < 0.0 || minimumCoverage > 1.0 then
      Left(FieldQcPolicyError.InvalidMinimumCoverage(minimumCoverage))
    else if !minimumPathQuality.isFinite || minimumPathQuality < 0.0 || minimumPathQuality > 1.0 then
      Left(FieldQcPolicyError.InvalidMinimumPathQuality(minimumPathQuality))
    else
      Right(new FieldQcPolicy(minimumCoverage, minimumPathQuality, requireEveryTargetCovered))

enum FieldQcFailure:
  case CoverageBelowMinimum(observed: Double, minimum: Double)
  case PathQualityBelowMinimum(observed: Double, minimum: Double)
  case UncoveredTargetRows(rows: Vector[Int])

  def message: String =
    this match
      case CoverageBelowMinimum(observed, minimum) =>
        s"coverage $observed is below required minimum $minimum"
      case PathQualityBelowMinimum(observed, minimum) =>
        s"path quality $observed is below required minimum $minimum"
      case UncoveredTargetRows(rows) =>
        s"target rows have no source coverage: ${rows.mkString(",")}"

final case class EvaluationQc(
  coverage: CoverageReport,
  pathQuality: Double
):
  require(pathQuality.isFinite && pathQuality >= 0.0 && pathQuality <= 1.0, "path quality must be in [0, 1]")

  def validate(policy: FieldQcPolicy): Either[FieldQcFailure, Unit] =
    if coverage.fraction < policy.minimumCoverage then
      Left(FieldQcFailure.CoverageBelowMinimum(coverage.fraction, policy.minimumCoverage))
    else if pathQuality < policy.minimumPathQuality then
      Left(FieldQcFailure.PathQualityBelowMinimum(pathQuality, policy.minimumPathQuality))
    else if policy.requireEveryTargetCovered && coverage.uncoveredTargetRows.nonEmpty then
      Left(FieldQcFailure.UncoveredTargetRows(coverage.uncoveredTargetRows))
    else Right(())

final case class ExecutionExplanation(
  phase: FieldExplanationPhase,
  cacheKey: EvaluationCacheKey,
  normalizedRoute: Vector[ExplainedRouteStep],
  stages: Vector[ExplainedProgramStage],
  compilerTrace: Vector[MorphismCompilerId],
  pullback: PullbackFingerprint,
  backend: PullbackBackendId,
  operator: OperatorSignature,
  sourceSupport: PullbackSupport,
  fusion: FusionExplanation,
  qc: EvaluationQc,
  planCache: CacheDisposition,
  resultCache: CacheDisposition
):
  require(phase != FieldExplanationPhase.Descriptive, "execution explanation must be planned or evaluated")

object ExecutionExplanation:
  private[spatial] def from(trace: EvaluationTrace): ExecutionExplanation =
    ExecutionExplanation(
      phase = trace.phase,
      cacheKey = trace.key,
      normalizedRoute = trace.normalizedRoute,
      stages = trace.stages,
      compilerTrace = trace.compilerTrace,
      pullback = trace.pullback,
      backend = trace.key.backend,
      operator = trace.operator,
      sourceSupport = trace.support,
      fusion = FusionExplanation(
        coordinateStages = trace.stages.count(_.semantics == StageSemantics.CoordinatePullback),
        barriers = trace.stages.collect {
          case stage if stage.semantics == StageSemantics.FusionBarrier => stage.morphism
        },
        valueResamplingPasses = trace.valueResamplingPasses
      ),
      qc = trace.qc,
      planCache = trace.planCache,
      resultCache = trace.resultCache
    )

final case class FieldMaterializationLineage(
  sourceRootId: FieldRootId,
  sourceRootRevision: FieldRevision,
  sourceDomain: DomainId,
  targetDomain: DomainId,
  demand: ResolvedDemand,
  steps: Vector[ViewPlanStep],
  execution: Option[ExecutionExplanation]
)

object FieldMaterializationLineage:
  private[spatial] def from(
    field: Field,
    execution: Option[ExecutionExplanation]
  ): FieldMaterializationLineage =
    FieldMaterializationLineage(
      sourceRootId = field.rootId,
      sourceRootRevision = field.rootRevision,
      sourceDomain = field.root,
      targetDomain = field.domain,
      demand = field.plan.intent.demand,
      steps = field.plan.steps,
      execution = execution
    )

final case class FieldExplanation(
  phase: FieldExplanationPhase,
  rootId: FieldRootId,
  rootRevision: FieldRevision,
  rootDomain: DomainId,
  currentDomain: DomainId,
  rootShape: FieldShape,
  currentShape: FieldShape,
  source: FieldSourceExplanation,
  demand: ResolvedDemand,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  allowInverses: Boolean,
  steps: Vector[ViewPlanStep],
  lineage: Vector[FieldMaterializationLineage],
  execution: Option[ExecutionExplanation]
)

object FieldExplanation:
  private[spatial] def from(
    field: Field,
    trace: Option[EvaluationTrace] = None
  ): FieldExplanation =
    val execution = trace.map(ExecutionExplanation.from)
    FieldExplanation(
      phase = execution.map(_.phase).getOrElse(FieldExplanationPhase.Descriptive),
      rootId = field.rootId,
      rootRevision = field.rootRevision,
      rootDomain = field.root,
      currentDomain = field.domain,
      rootShape = FieldShape(field.plan.rootSampleCount, field.plan.rootObservations),
      currentShape = FieldShape(field.sampleCount, field.observations),
      source = FieldSourceExplanation(
        label = field.data.label,
        materialized = field.data.isMaterialized,
        revision = field.data.fieldSource.map(_.descriptor.revision)
      ),
      demand = field.plan.intent.demand,
      routing = field.plan.intent.routing,
      sampling = field.plan.intent.sampling,
      allowInverses = field.plan.intent.allowInverses,
      steps = field.plan.steps,
      lineage = field.provenance.materializations,
      execution = execution
    )
