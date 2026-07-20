package scalafim.spatial

import scalafim.image.SpatialPoint
import scalafim.linalg.{CsrMatrix, DoubleMatrix}

opaque type MorphismCompilerId = String

object MorphismCompilerId:
  def apply(value: String): Either[SpatialError, MorphismCompilerId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SpatialError.EmptyIdentifier("morphism compiler"))
    else Right(normalized)

  private[spatial] def unsafe(value: String): MorphismCompilerId =
    value

  extension (id: MorphismCompilerId)
    def value: String =
      id

opaque type PullbackFingerprint = String

object PullbackFingerprint:
  private[spatial] def unsafe(value: String): PullbackFingerprint =
    value

  extension (fingerprint: PullbackFingerprint)
    def value: String =
      fingerprint

final case class ResolvedPullbackRoute private (
  source: Domain,
  target: Domain,
  path: MorphismPath,
  targetRows: TargetRows,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  allowInverses: Boolean
)

object ResolvedPullbackRoute:
  def resolve(
    graph: SpatialGraph,
    request: CompileRequest
  ): Either[SpatialError, ResolvedPullbackRoute] =
    for
      source <- graph.domain(request.source)
      target <- graph.domain(request.target)
      path <- graph.path(request.source, request.target, request.routing, request.allowInverses)
      targetRows <- TargetRows.fromSelection(request.rowSelection, target.nElements)
    yield
      ResolvedPullbackRoute(
        source = source,
        target = target,
        path = path,
        targetRows = targetRows,
        routing = request.routing,
        sampling = request.sampling,
        allowInverses = request.allowInverses
      )

final case class PullbackStep private (
  morphism: Morphism,
  compiler: MorphismCompilerId,
  coordinateMap: CoordinateMap
):
  def source: DomainId =
    morphism.source

  def target: DomainId =
    morphism.target

object PullbackStep:
  def build(
    morphism: Morphism,
    compiler: MorphismCompilerId,
    coordinateMap: CoordinateMap
  ): Either[SpatialError, PullbackStep] =
    coordinateMap match
      case CoordinateMap.Unspecified =>
        Left(SpatialError.MissingCoordinateMap(morphism.id))
      case _ =>
        Right(new PullbackStep(morphism, compiler, coordinateMap))

final case class ValueStage private (
  morphism: Morphism,
  compiler: MorphismCompilerId,
  semantics: StageSemantics,
  transform: ValueTransform
):
  require(semantics != StageSemantics.CoordinatePullback, "value stage cannot be a coordinate pullback")

  def source: DomainId =
    morphism.source

  def target: DomainId =
    morphism.target

  def fingerprint: String =
    val plugin = morphism.plugin.map(_.fingerprint).getOrElse(transform.fingerprint)
    s"${compiler.value}:$semantics:$plugin"

object ValueStage:
  def fromPlugin(
    morphism: Morphism,
    compiler: MorphismCompilerId
  ): Either[SpatialError, ValueStage] =
    morphism.plugin match
      case Some(plugin) =>
        Right(new ValueStage(morphism, compiler, plugin.semantics, plugin.transform))
      case None =>
        Left(SpatialError.InvalidMorphismPlugin(morphism.id, "no executable plugin payload was supplied"))

enum ProgramStage:
  case Coordinate(step: PullbackStep)
  case Value(stage: ValueStage)

  def morphism: Morphism =
    this match
      case Coordinate(step) => step.morphism
      case Value(stage) => stage.morphism

  def compiler: MorphismCompilerId =
    this match
      case Coordinate(step) => step.compiler
      case Value(stage) => stage.compiler

  def semantics: StageSemantics =
    this match
      case Coordinate(_) => StageSemantics.CoordinatePullback
      case Value(stage) => stage.semantics

  def fingerprint: String =
    this match
      case Coordinate(step) => s"${step.compiler.value}:coordinate:${step.coordinateMap.fingerprint}"
      case Value(stage) => stage.fingerprint

final case class MorphismLowering(stages: Vector[ProgramStage])

object MorphismLowering:
  def coordinates(steps: Vector[PullbackStep]): MorphismLowering =
    MorphismLowering(steps.map(ProgramStage.Coordinate.apply))

trait MorphismPullbackCompiler:
  def id: MorphismCompilerId
  def kind: MorphismKind

  def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]]

  def lowerProgram(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, MorphismLowering] =
    lower(morphism, source, target).map(MorphismLowering.coordinates)

object MorphismPullbackCompiler:
  val identity: MorphismPullbackCompiler =
    IdentityPullbackCompiler

  val affine3D: MorphismPullbackCompiler =
    AffinePullbackCompiler

  val warp3D: MorphismPullbackCompiler =
    WarpPullbackCompiler

  val volumeToSurface: MorphismPullbackCompiler =
    VolumeToSurfacePullbackCompiler

  val surfaceToSurface: MorphismPullbackCompiler =
    SurfaceToSurfacePullbackCompiler

  val functional: MorphismPullbackCompiler =
    FunctionalPluginCompiler

  val filter: MorphismPullbackCompiler =
    FilterPluginCompiler

  val hybrid: MorphismPullbackCompiler =
    HybridPluginCompiler

private object IdentityPullbackCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("identity-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.Identity

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    validateCompilerInput(this, morphism, source, target).map(_ => Vector.empty)

private object AffinePullbackCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("affine-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.Affine3D

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    for
      _ <- validateCompilerInput(this, morphism, source, target)
      step <- PullbackStep.build(morphism, id, morphism.coordinateMap)
    yield Vector(step)

private object WarpPullbackCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("dense-warp-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.Warp3D

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    for
      _ <- validateCompilerInput(this, morphism, source, target)
      step <- PullbackStep.build(morphism, id, morphism.coordinateMap)
    yield Vector(step)

private object VolumeToSurfacePullbackCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("volume-to-surface-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.VolumeToSurface

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    for
      _ <- validateCompilerInput(this, morphism, source, target)
      step <- PullbackStep.build(morphism, id, morphism.coordinateMap)
    yield Vector(step)

private object SurfaceToSurfacePullbackCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("surface-to-surface-pullback-v1")

  override val kind: MorphismKind =
    MorphismKind.SurfaceToSurface

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    for
      _ <- validateCompilerInput(this, morphism, source, target)
      step <- PullbackStep.build(morphism, id, morphism.coordinateMap)
    yield Vector(step)

private abstract class ValuePluginCompiler(
  override val id: MorphismCompilerId,
  override val kind: MorphismKind
) extends MorphismPullbackCompiler:
  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    lowerProgram(morphism, source, target).map(_ => Vector.empty)

  override def lowerProgram(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, MorphismLowering] =
    for
      _ <- validateCompilerInput(this, morphism, source, target)
      stage <- ValueStage.fromPlugin(morphism, id)
    yield MorphismLowering(Vector(ProgramStage.Value(stage)))

private object FunctionalPluginCompiler
    extends ValuePluginCompiler(
      MorphismCompilerId.unsafe("functional-value-plugin-v1"),
      MorphismKind.Functional
    )

private object FilterPluginCompiler
    extends ValuePluginCompiler(
      MorphismCompilerId.unsafe("filter-value-plugin-v1"),
      MorphismKind.Filter
    )

private object HybridPluginCompiler
    extends ValuePluginCompiler(
      MorphismCompilerId.unsafe("hybrid-value-plugin-v1"),
      MorphismKind.Hybrid
    )

private def validateCompilerInput(
  compiler: MorphismPullbackCompiler,
  morphism: Morphism,
  source: Domain,
  target: Domain
): Either[SpatialError, Unit] =
  if morphism.kind != compiler.kind then
    Left(
      SpatialError.MorphismCompilerKindMismatch(
        compiler.id.value,
        compiler.kind,
        morphism.kind
      )
    )
  else Morphism.validateDomains(morphism, source, target)

final case class MorphismCompilerRegistry private (
  private val compilers: Map[MorphismKind, MorphismPullbackCompiler]
):
  def compilerFor(kind: MorphismKind): Either[SpatialError, MorphismPullbackCompiler] =
    compilers.get(kind).toRight(SpatialError.MorphismCompilerNotFound(kind))

  def register(
    compiler: MorphismPullbackCompiler
  ): Either[SpatialError, MorphismCompilerRegistry] =
    if compilers.contains(compiler.kind) then
      Left(SpatialError.DuplicateMorphismCompiler(compiler.kind))
    else Right(copy(compilers = compilers.updated(compiler.kind, compiler)))

  def ids: Vector[MorphismCompilerId] =
    compilers.values.toVector.sortBy(_.kind.ordinal).map(_.id)

object MorphismCompilerRegistry:
  val empty: MorphismCompilerRegistry =
    MorphismCompilerRegistry(Map.empty)

  val standard: MorphismCompilerRegistry =
    MorphismCompilerRegistry(
      Map(
        MorphismKind.Identity -> MorphismPullbackCompiler.identity,
        MorphismKind.Affine3D -> MorphismPullbackCompiler.affine3D,
        MorphismKind.Warp3D -> MorphismPullbackCompiler.warp3D,
        MorphismKind.VolumeToSurface -> MorphismPullbackCompiler.volumeToSurface,
        MorphismKind.SurfaceToSurface -> MorphismPullbackCompiler.surfaceToSurface,
        MorphismKind.Functional -> MorphismPullbackCompiler.functional,
        MorphismKind.Filter -> MorphismPullbackCompiler.filter,
        MorphismKind.Hybrid -> MorphismPullbackCompiler.hybrid
      )
    )

  def build(
    compilers: Vector[MorphismPullbackCompiler]
  ): Either[SpatialError, MorphismCompilerRegistry] =
    compilers.foldLeft[Either[SpatialError, MorphismCompilerRegistry]](Right(empty)) {
      (registry, compiler) => registry.flatMap(_.register(compiler))
    }

final case class PullbackProgram private (
  route: ResolvedPullbackRoute,
  stageTrace: Vector[ProgramStage],
  compilerTrace: Vector[MorphismCompilerId],
  fingerprint: PullbackFingerprint
):
  require(
    stageTrace.forall(stage => route.path.morphisms.contains(stage.morphism)),
    "program stages must originate in the resolved route"
  )

  def steps: Vector[PullbackStep] =
    stageTrace.collect { case ProgramStage.Coordinate(step) => step }

  def valueStages: Vector[ValueStage] =
    stageTrace.collect { case ProgramStage.Value(stage) => stage }

  def root: DomainId =
    route.source.id

  def target: DomainId =
    route.target.id

  def targetRows: Vector[Int] =
    route.targetRows.indices

  def isIdentity: Boolean =
    route.source.id == route.target.id && stageTrace.isEmpty

  def requiresSpatialSampling: Boolean =
    steps.nonEmpty

  def pullback(point: SpatialPoint): Either[SpatialError, SpatialPoint] =
    var current = point
    var i = steps.length - 1
    var error = Option.empty[SpatialError]
    while i >= 0 && error.isEmpty do
      steps(i).coordinateMap.transform(current) match
        case Left(err) =>
          error = Some(err)
        case Right(next) =>
          current = next
      i -= 1
    error.toLeft(current)

object PullbackProgram:
  def compile(
    graph: SpatialGraph,
    request: CompileRequest,
    registry: MorphismCompilerRegistry = MorphismCompilerRegistry.standard
  ): Either[SpatialError, PullbackProgram] =
    ResolvedPullbackRoute.resolve(graph, request).flatMap(lower(_, graph, registry))

  def lower(
    route: ResolvedPullbackRoute,
    graph: SpatialGraph,
    registry: MorphismCompilerRegistry
  ): Either[SpatialError, PullbackProgram] =
    val stages = Vector.newBuilder[ProgramStage]
    val compilerTrace = Vector.newBuilder[MorphismCompilerId]
    var i = 0
    var error = Option.empty[SpatialError]

    while i < route.path.morphisms.length && error.isEmpty do
      val morphism = route.path.morphisms(i)
      val lowered =
        for
          source <- graph.domain(morphism.source)
          target <- graph.domain(morphism.target)
          compiler <- registry.compilerFor(morphism.kind)
          lowered <- compiler.lowerProgram(morphism, source, target)
        yield (compiler.id, lowered)

      lowered match
        case Left(err) =>
          error = Some(err)
        case Right((compilerId, loweredSteps)) =>
          compilerTrace += compilerId
          stages ++= loweredSteps.stages
      i += 1

    error match
      case Some(err) => Left(err)
      case None =>
        val trace = compilerTrace.result()
        val programStages = stages.result()
        Right(
          PullbackProgram(
            route = route,
            stageTrace = programStages,
            compilerTrace = trace,
            fingerprint = fingerprint(route, programStages, trace)
          )
        )

  private def fingerprint(
    route: ResolvedPullbackRoute,
    stages: Vector[ProgramStage],
    compilerTrace: Vector[MorphismCompilerId]
  ): PullbackFingerprint =
    val executableStages = stages.map(stage => stage.morphism.id -> stage.fingerprint).toMap
    val path = route.path.morphisms.map { morphism =>
      val executable = executableStages.getOrElse(
        morphism.id,
        morphism.plugin.map(_.fingerprint).getOrElse(morphism.coordinateMap.fingerprint)
      )
      s"${morphism.id.value}:${morphism.kind}:${morphism.isInverted}:$executable"
    }.mkString(",")
    val rows = route.targetRows.indices.mkString(",")
    val compilers = compilerTrace.map(_.value).mkString(",")
    val stageOrder = stages.map(stage => s"${stage.morphism.id.value}:${stage.semantics}:${stage.fingerprint}").mkString(",")
    PullbackFingerprint.unsafe(
      s"pullback-v1|root=${route.source.id.value}|target=${route.target.id.value}|path=$path|routing=${route.routing}|sampling=${route.sampling}|rows=$rows|inverse=${route.allowInverses}|compilers=$compilers|stages=$stageOrder"
    )

enum SupportPrecision:
  case Exact, Conservative

final case class PullbackSupport(
  program: PullbackFingerprint,
  sourceRows: Vector[Int],
  precision: SupportPrecision
)

trait PullbackSupportPlanner:
  def plan(
    program: PullbackProgram,
    operator: SpatialOperator
  ): Either[SpatialError, PullbackSupport]

object PullbackSupportPlanner:
  val standard: PullbackSupportPlanner =
    SparseOperatorSupportPlanner

private object SparseOperatorSupportPlanner extends PullbackSupportPlanner:
  override def plan(
    program: PullbackProgram,
    operator: SpatialOperator
  ): Either[SpatialError, PullbackSupport] =
    if operator.source != program.root || operator.target != program.target then
      Left(SpatialError.OperatorAssemblyFailed("support operator domains do not match pullback program"))
    else if operator.rows != program.targetRows.length then
      Left(SpatialError.OperatorAssemblyFailed("support operator rows do not match pullback demand"))
    else
      operator.map match
        case csr: CsrMatrix =>
          val rows = csr.toTriplets.colIndices.distinct.sorted.toVector
          Right(PullbackSupport(program.fingerprint, rows, SupportPrecision.Exact))
        case _ =>
          Right(
            PullbackSupport(
              program.fingerprint,
              Vector.tabulate(operator.cols)(identity),
              SupportPrecision.Conservative
            )
          )

trait PullbackOperatorAssembler:
  def assemble(
    program: PullbackProgram,
    support: PullbackSupport
  ): Either[SpatialError, SpatialOperator]

trait PullbackExecutor:
  def execute(
    operator: SpatialOperator,
    rootData: DoubleMatrix
  ): Either[SpatialError, DoubleMatrix]
