package scalafim.spatial

import neurosurf4s.routing.*

/** Functional stage wiring and execution around the upstream anatomical program. */
object FunctionalRouting:
  val registry: MorphismCompilerRegistry =
    val registered = Vector(FunctionalPluginCompiler, FilterPluginCompiler, HybridPluginCompiler)
      .foldLeft[Either[SpatialError, MorphismCompilerRegistry]](Right(MorphismCompilerRegistry.standard)):
        (prior, compiler) => prior.flatMap(_.register(compiler))
    registered.fold(error => throw new IllegalStateException(error.message), identity)

  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator] =
    PullbackProgram.compile(graph, request, registry).flatMap(compile)

  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    if program.valueStages.nonEmpty then StagedOperatorCompiler.compile(program)
    else VolumePullbackOperatorCompiler.compile(program)

private def checkedCompilerId(value: String): MorphismCompilerId =
  MorphismCompilerId(value).fold(error => throw new IllegalStateException(error.message), identity)

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
      checkedCompilerId("functional-value-plugin-v1"),
      MorphismKind.Functional
    )

private object FilterPluginCompiler
    extends ValuePluginCompiler(
      checkedCompilerId("filter-value-plugin-v1"),
      MorphismKind.Filter
    )

private object HybridPluginCompiler
    extends ValuePluginCompiler(
      checkedCompilerId("hybrid-value-plugin-v1"),
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

