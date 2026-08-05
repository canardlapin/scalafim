package scalafim.spatial

import gale.linalg.{DMat, LinAlgError}

import scala.collection.mutable

opaque type PullbackBackendId = String

object PullbackBackendId:
  def apply(value: String): Either[SpatialError, PullbackBackendId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SpatialError.EmptyIdentifier("pullback backend"))
    else Right(normalized)

  private[spatial] def unsafe(value: String): PullbackBackendId =
    value

  extension (id: PullbackBackendId)
    def value: String =
      id

trait PullbackProgramCompiler:
  def id: PullbackBackendId
  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator]

object PullbackProgramCompiler:
  val volume: PullbackProgramCompiler =
    VolumePullbackProgramCompiler

  val volumeAffine: PullbackProgramCompiler =
    VolumeAffineProgramCompiler

private object VolumePullbackProgramCompiler extends PullbackProgramCompiler:
  override val id: PullbackBackendId =
    PullbackBackendId.unsafe("volume-pullback-program-v1")

  override def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    VolumePullbackOperatorCompiler.compile(program)

private object VolumeAffineProgramCompiler extends PullbackProgramCompiler:
  override val id: PullbackBackendId =
    PullbackBackendId.unsafe("volume-affine-program-v1")

  override def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    VolumeAffineOperatorCompiler.compile(program)

final case class EvaluationCacheKey private (
  rootId: FieldRootId,
  rootRevision: FieldRevision,
  sourceRevision: Option[FieldSourceRevision],
  graph: SpatialGraph,
  root: DomainId,
  target: DomainId,
  rootSampleCount: Int,
  rootObservations: Int,
  demand: DemandFingerprint,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  allowInverses: Boolean,
  morphismCompilers: Vector[MorphismCompilerId],
  backend: PullbackBackendId
):
  def label: String =
    val compilers = morphismCompilers.map(_.value).mkString(",")
    val source = sourceRevision.map(value => s"|source=${value.value}").getOrElse("")
    s"root=${rootId.value}@${rootRevision.value}$source|${root.value}->${target.value}|${demand.value}|routing=$routing|sampling=$sampling|inverse=$allowInverses|morphisms=$compilers|backend=${backend.value}"

object EvaluationCacheKey:
  def from(
    field: Field,
    graph: SpatialGraph,
    registry: MorphismCompilerRegistry,
    backend: PullbackBackendId
  ): EvaluationCacheKey =
    val intent = field.plan.intent
    EvaluationCacheKey(
      rootId = field.rootId,
      rootRevision = field.rootRevision,
      sourceRevision = field.data.fieldSource.map(_.descriptor.revision),
      graph = graph,
      root = field.root,
      target = field.domain,
      rootSampleCount = field.plan.rootSampleCount,
      rootObservations = field.plan.rootObservations,
      demand = intent.demand.fingerprint,
      routing = intent.routing,
      sampling = intent.sampling,
      allowInverses = intent.allowInverses,
      morphismCompilers = registry.ids,
      backend = backend
    )

final case class CompiledEvaluation(
  program: PullbackProgram,
  operator: SpatialOperator,
  support: PullbackSupport
)

trait EvaluationPlanCache:
  def get(key: EvaluationCacheKey): Option[CompiledEvaluation]
  def put(key: EvaluationCacheKey, evaluation: CompiledEvaluation): Unit
  def size: Int

final class InMemoryEvaluationPlanCache private (
  private val store: mutable.LinkedHashMap[EvaluationCacheKey, CompiledEvaluation]
) extends EvaluationPlanCache:
  override def get(key: EvaluationCacheKey): Option[CompiledEvaluation] =
    store.get(key)

  override def put(key: EvaluationCacheKey, evaluation: CompiledEvaluation): Unit =
    store.update(key, evaluation)

  override def size: Int =
    store.size

  def keys: Vector[EvaluationCacheKey] =
    store.keys.toVector

object InMemoryEvaluationPlanCache:
  def empty: InMemoryEvaluationPlanCache =
    new InMemoryEvaluationPlanCache(mutable.LinkedHashMap.empty)

final case class CachedFieldResult(
  data: DMat,
  evaluation: CompiledEvaluation
)

trait FieldResultCache:
  def get(key: EvaluationCacheKey): Option[CachedFieldResult]
  def put(key: EvaluationCacheKey, result: CachedFieldResult): Unit
  def size: Int

final class InMemoryFieldResultCache private (
  private val store: mutable.LinkedHashMap[EvaluationCacheKey, CachedFieldResult]
) extends FieldResultCache:
  override def get(key: EvaluationCacheKey): Option[CachedFieldResult] =
    store.get(key)

  override def put(key: EvaluationCacheKey, result: CachedFieldResult): Unit =
    store.update(key, result)

  override def size: Int =
    store.size

  def keys: Vector[EvaluationCacheKey] =
    store.keys.toVector

object InMemoryFieldResultCache:
  def empty: InMemoryFieldResultCache =
    new InMemoryFieldResultCache(mutable.LinkedHashMap.empty)

enum CacheDisposition:
  case Hit, Miss, NotConsulted

final case class EvaluationTrace(
  phase: FieldExplanationPhase,
  key: EvaluationCacheKey,
  normalizedRoute: Vector[ExplainedRouteStep],
  stages: Vector[ExplainedProgramStage],
  compilerTrace: Vector[MorphismCompilerId],
  pullback: PullbackFingerprint,
  operator: OperatorSignature,
  support: PullbackSupport,
  qc: EvaluationQc,
  planCache: CacheDisposition,
  resultCache: CacheDisposition,
  valueResamplingPasses: Int
)

final case class LazyRuntimeStats(
  planCacheHits: Long,
  resultCacheHits: Long,
  compilations: Long,
  executions: Long,
  materializations: Long
)

final class LazyFieldRuntime private (
  graph: SpatialGraph,
  backend: PullbackProgramCompiler,
  registry: MorphismCompilerRegistry,
  planCache: EvaluationPlanCache,
  resultCache: FieldResultCache,
  legacyRuntime: FieldRuntime
) extends FieldRuntime:
  private var planHitCount = 0L
  private var resultHitCount = 0L
  private var compilationCount = 0L
  private var executionCount = 0L
  private var materializationCount = 0L
  private var latestTrace = Option.empty[EvaluationTrace]
  private val traces = mutable.LinkedHashMap.empty[EvaluationCacheKey, EvaluationTrace]

  override def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field] =
    legacyRuntime.view(field, operator)

  override def data(field: Field): Either[SpatialError, DMat] =
    val key = EvaluationCacheKey.from(field, graph, registry, backend.id)
    validateSource(field).flatMap { _ =>
      resultCache.get(key) match
        case Some(cached) =>
          resultHitCount += 1L
          recordTrace(
            trace(
              FieldExplanationPhase.Evaluated,
              key,
              cached.evaluation,
              CacheDisposition.NotConsulted,
              CacheDisposition.Hit
            )
          )
          Right(cached.data)
        case None =>
          for
            compiledAndDisposition <- compiled(key, field)
            (evaluation, planDisposition) = compiledAndDisposition
            rootData <- rootData(field, evaluation)
            operatorResult <- evaluation.operator.forward(rootData).left.map(linearError)
            stagedResult <- ValueStageExecutor.execute(evaluation.program, operatorResult)
            result <- finalizeObservationDemand(field, evaluation.program, stagedResult)
            _ <- validateResult(field, result)
          yield
            executionCount += 1L
            val cached = CachedFieldResult(result, evaluation)
            resultCache.put(key, cached)
            recordTrace(
              trace(
                FieldExplanationPhase.Evaluated,
                key,
                evaluation,
                planDisposition,
                CacheDisposition.Miss
              )
            )
            result
    }

  override def materialize(field: Field): Either[SpatialError, Field] =
    data(field).flatMap { result =>
      val key = EvaluationCacheKey.from(field, graph, registry, backend.id)
      val execution = traces.get(key).map(ExecutionExplanation.from)
      Field.materializedFrom(field, result, execution).map { root =>
        materializationCount += 1L
        root
      }
    }

  def stats: LazyRuntimeStats =
    LazyRuntimeStats(
      planCacheHits = planHitCount,
      resultCacheHits = resultHitCount,
      compilations = compilationCount,
      executions = executionCount,
      materializations = materializationCount
    )

  def lastTrace: Option[EvaluationTrace] =
    latestTrace

  def explain(field: Field): FieldExplanation =
    val key = EvaluationCacheKey.from(field, graph, registry, backend.id)
    FieldExplanation.from(field, traces.get(key))

  def plan(field: Field): Either[SpatialError, FieldExplanation] =
    val key = EvaluationCacheKey.from(field, graph, registry, backend.id)
    compiled(key, field).map { case (evaluation, planDisposition) =>
      val planned =
        trace(
          FieldExplanationPhase.Planned,
          key,
          evaluation,
          planDisposition,
          CacheDisposition.NotConsulted
        )
      recordTrace(planned)
      FieldExplanation.from(field, Some(planned))
    }

  private def compiled(
    key: EvaluationCacheKey,
    field: Field
  ): Either[SpatialError, (CompiledEvaluation, CacheDisposition)] =
    planCache.get(key) match
      case Some(evaluation) =>
        planHitCount += 1L
        Right((evaluation, CacheDisposition.Hit))
      case None =>
        val intent = field.plan.intent
        val request =
          CompileRequest.forRows(
            source = field.root,
            target = field.domain,
            rowSelection = intent.rowSelection,
            routing = intent.routing,
            sampling = intent.sampling,
            allowInverses = intent.allowInverses
          )
        for
          program <- PullbackProgram.compile(graph, request, registry)
          operator <- backend.compile(program)
          support <- PullbackSupportPlanner.standard.plan(program, operator)
        yield
          compilationCount += 1L
          val evaluation = CompiledEvaluation(program, operator, support)
          planCache.put(key, evaluation)
          (evaluation, CacheDisposition.Miss)

  private def validateSource(field: Field): Either[SpatialError, Unit] =
    field.data.fieldSource match
      case Some(source) => source.validate()
      case None => Right(())

  private def rootData(
    field: Field,
    evaluation: CompiledEvaluation
  ): Either[SpatialError, DMat] =
    val observations = rootObservationDemand(field, evaluation.program)
    field.data.fieldSource match
      case Some(source) =>
        for
          request <- FieldSourceRequest.make(source.descriptor, evaluation.support.sourceRows, observations)
          block <- source.read(request)
          _ <-
            if block.request == request then Right(())
            else Left(SpatialError.FieldSourceRequestMismatch(source.descriptor.id))
          _ <- FieldSourceBlock.make(source.descriptor.id, request, block.data)
          expanded <- expandSourceBlock(field, request, block.data)
        yield expanded
      case None =>
        field.data.materialized match
          case None =>
            Left(SpatialError.FieldDataUnavailable(field.data.label))
          case Some(data) if data.rows != field.plan.rootSampleCount =>
            Left(SpatialError.FieldShapeMismatch(field.plan.rootSampleCount, data.rows))
          case Some(data) if data.cols != field.plan.rootObservations =>
            Left(SpatialError.FieldObservationMismatch(field.plan.rootObservations, data.cols))
          case Some(data) =>
            selectObservations(data, observations)

  private def rootObservationDemand(field: Field, program: PullbackProgram): Vector[Int] =
    if hasObservationTransform(program) then Vector.tabulate(field.plan.rootObservations)(identity)
    else field.plan.intent.demand.observationIndices

  private def finalizeObservationDemand(
    field: Field,
    program: PullbackProgram,
    data: DMat
  ): Either[SpatialError, DMat] =
    if hasObservationTransform(program) then
      selectObservations(data, field.plan.intent.demand.observationIndices)
    else Right(data)

  private def hasObservationTransform(program: PullbackProgram): Boolean =
    program.valueStages.exists {
      _.transform match
        case ValueTransform.ObservationLinear(_) => true
        case _ => false
    }

  private def expandSourceBlock(
    field: Field,
    request: FieldSourceRequest,
    block: DMat
  ): Either[SpatialError, DMat] =
    val valueCount = field.plan.rootSampleCount.toLong * request.columns.toLong
    if valueCount > Int.MaxValue.toLong then
      Left(SpatialError.FieldSourceReadFailed(field.data.fieldSource.get.descriptor.id, s"expanded block has $valueCount values"))
    else
      val compact = block.copyData
      val expanded = new Array[Double](valueCount.toInt)
      var compactRow = 0
      while compactRow < request.rows do
        val sourceRow = request.sourceRows(compactRow)
        var column = 0
        while column < request.columns do
          expanded(sourceRow * request.columns + column) = compact(compactRow * request.columns + column)
          column += 1
        compactRow += 1
      Right(GaleSpatialSupport.unsafeOwnedMatrix(field.plan.rootSampleCount, request.columns, expanded))

  private def selectObservations(
    data: DMat,
    observations: Vector[Int]
  ): Either[SpatialError, DMat] =
    var i = 0
    while i < observations.length do
      val observation = observations(i)
      if observation < 0 || observation >= data.cols then
        return Left(SpatialError.FieldObservationMismatch(data.cols, observation))
      i += 1

    if observations.length == data.cols && observations.indices.forall(i => observations(i) == i) then
      Right(data)
    else
      val source = data.copyData
      val selected = new Array[Double](data.rows * observations.length)
      var row = 0
      while row < data.rows do
        var outCol = 0
        while outCol < observations.length do
          selected(row * observations.length + outCol) = source(row * data.cols + observations(outCol))
          outCol += 1
        row += 1
      Right(GaleSpatialSupport.unsafeOwnedMatrix(data.rows, observations.length, selected))

  private def validateResult(field: Field, result: DMat): Either[SpatialError, Unit] =
    if result.rows != field.sampleCount then
      Left(SpatialError.FieldShapeMismatch(field.sampleCount, result.rows))
    else if result.cols != field.observations then
      Left(SpatialError.FieldObservationMismatch(field.observations, result.cols))
    else Right(())

  private def trace(
    phase: FieldExplanationPhase,
    key: EvaluationCacheKey,
    evaluation: CompiledEvaluation,
    planDisposition: CacheDisposition,
    resultDisposition: CacheDisposition
  ): EvaluationTrace =
    val route = evaluation.program.route.path.morphisms.zip(evaluation.program.compilerTrace).map {
      case (morphism, compiler) =>
        ExplainedRouteStep(
          morphism = morphism.id,
          source = morphism.source,
          target = morphism.target,
          kind = morphism.kind,
          routeTag = morphism.routeTag,
          cost = morphism.cost,
          inverse = morphism.inverse,
          isInverted = morphism.isInverted,
          compiler = compiler
        )
    }
    EvaluationTrace(
      phase = phase,
      key = key,
      normalizedRoute = route,
      stages = evaluation.program.stageTrace.map(ExplainedProgramStage.from),
      compilerTrace = evaluation.program.compilerTrace,
      pullback = evaluation.program.fingerprint,
      operator = evaluation.operator.signature,
      support = evaluation.support,
      qc = EvaluationQc(evaluation.operator.qc.coverage, evaluation.operator.qc.pathQuality),
      planCache = planDisposition,
      resultCache = resultDisposition,
      valueResamplingPasses = if evaluation.program.requiresSpatialSampling then 1 else 0
    )

  private def recordTrace(trace: EvaluationTrace): Unit =
    traces.update(trace.key, trace)
    latestTrace = Some(trace)

  private def linearError(error: LinAlgError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.getMessage)

object LazyFieldRuntime:
  def apply(
    graph: SpatialGraph,
    backend: PullbackProgramCompiler = PullbackProgramCompiler.volume,
    registry: MorphismCompilerRegistry = MorphismCompilerRegistry.standard,
    planCache: EvaluationPlanCache = InMemoryEvaluationPlanCache.empty,
    resultCache: FieldResultCache = InMemoryFieldResultCache.empty,
    legacyOperatorCache: OperatorCache = InMemoryOperatorCache.empty
  ): LazyFieldRuntime =
    new LazyFieldRuntime(
      graph,
      backend,
      registry,
      planCache,
      resultCache,
      CachedFieldRuntime(legacyOperatorCache)
    )
