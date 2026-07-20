package scalafim.spatial

import scalafim.linalg.{CsrMatrix, DoubleMatrix, GaleLinearMap, GaleMatrixBridge, LinearMap, LinearMapError}

object StagedOperatorCompiler:
  private val CompilerName = "staged-gale-pullback-v1"

  def compile(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    for
      _ <- validateStageOrder(program)
      operator <-
        if program.steps.nonEmpty then
          if rowStages(program).nonEmpty then
            Left(
              SpatialError.UnsupportedPluginComposition(
                "row-linear value stages interleaved with coordinate pullbacks require an explicit fusion-barrier backend"
              )
            )
          else VolumePullbackOperatorCompiler.compileCoordinateOnly(program)
        else compileAlgebraic(program)
    yield operator

  private def compileAlgebraic(program: PullbackProgram): Either[SpatialError, SpatialOperator] =
    val route = program.route
    val sourceSize = route.source.nElements
    val targetSize = route.target.nElements
    val rows = route.targetRows
    for
      base <- CsrMatrix.identity(sourceSize).left.map(linearError)
      composed <- composeRows(base, rowStages(program))
      _ <-
        if composed.rows == targetSize then Right(())
        else
          Left(
            SpatialError.UnsupportedPluginComposition(
              s"staged row pipeline ends with ${composed.rows} rows, expected $targetSize"
            )
          )
      selected <-
        if rows.length == targetSize && rows.indices.indices.forall(index => rows.indices(index) == index) then
          Right(composed)
        else
          LinearMap.restrict(composed, targetRows = Some(rows.indices)).left.map(linearError)
      coverage <- CoverageReport.build(rows, Vector.fill(rows.length)(1.0))
      recipe <- OperatorRecipe.build(
        route.path.ids,
        route.routing,
        route.sampling,
        rows.selection,
        route.allowInverses,
        CompilerName
      )
      operator <- SpatialOperator.build(
        route.source.id,
        route.target.id,
        selected,
        route.path,
        OperatorQc(coverage, route.path.pathQuality),
        OperatorProvenance.fromRecipe(recipe)
      )
    yield operator

  private def composeRows(
    initial: LinearMap,
    stages: Vector[ValueStage]
  ): Either[SpatialError, LinearMap] =
    var current = initial
    var index = 0
    var error = Option.empty[SpatialError]
    while index < stages.length && error.isEmpty do
      stages(index).transform match
        case ValueTransform.RowLinear(matrix) =>
          LinearMap.compose(current, GaleLinearMap(matrix)) match
            case Left(err) => error = Some(linearError(err))
            case Right(next) => current = next
        case _ => ()
      index += 1
    error.toLeft(current)

  private def validateStageOrder(program: PullbackProgram): Either[SpatialError, Unit] =
    val pathIndex = program.route.path.morphisms.zipWithIndex.map { case (morphism, index) => morphism.id -> index }.toMap
    val structural =
      program.stageTrace.collect {
        case ProgramStage.Coordinate(step) => pathIndex(step.morphism.id)
        case ProgramStage.Value(stage) if stage.transform.isInstanceOf[ValueTransform.RowLinear] =>
          pathIndex(stage.morphism.id)
      }
    val lastStructural = structural.maxOption.getOrElse(-1)
    program.valueStages.find { stage =>
      stage.semantics == StageSemantics.FusionBarrier && pathIndex(stage.morphism.id) < lastStructural
    } match
      case Some(stage) =>
        Left(
          SpatialError.UnsupportedPluginComposition(
            s"fusion barrier ${stage.morphism.id.value} precedes a later coordinate or row stage"
          )
        )
      case None => Right(())

  private def rowStages(program: PullbackProgram): Vector[ValueStage] =
    program.valueStages.filter {
      _.transform match
        case ValueTransform.RowLinear(_) => true
        case _ => false
    }

  private def linearError(error: LinearMapError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

object ValueStageExecutor:
  def execute(program: PullbackProgram, input: DoubleMatrix): Either[SpatialError, DoubleMatrix] =
    var current = input
    var index = 0
    var error = Option.empty[SpatialError]
    while index < program.valueStages.length && error.isEmpty do
      program.valueStages(index).transform match
        case ValueTransform.RowLinear(_) =>
          ()
        case ValueTransform.ObservationLinear(matrix) =>
          GaleMatrixBridge.rightMultiply(current, matrix.t) match
            case Left(err) => error = Some(SpatialError.OperatorAssemblyFailed(err.message))
            case Right(next) => current = next
        case ValueTransform.PointwiseAffine(scale, offset) =>
          val data = current.copyData
          var valueIndex = 0
          while valueIndex < data.length do
            data(valueIndex) = scale * data(valueIndex) + offset
            valueIndex += 1
          current = DoubleMatrix.unsafe(current.rows, current.cols, data)
      index += 1
    error.toLeft(current)
