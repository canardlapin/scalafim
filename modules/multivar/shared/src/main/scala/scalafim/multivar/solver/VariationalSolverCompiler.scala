package scalafim.multivar
package solver

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat
import scalafim.linalg.BoundedLinearMap
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.FirstOrderCapabilities
import scalafim.linalg.FirstOrderConfig
import scalafim.linalg.FirstOrderError
import scalafim.linalg.FirstOrderMethod
import scalafim.linalg.FirstOrderSolvers
import scalafim.linalg.FirstOrderStoppingStatus
import scalafim.linalg.FirstOrderTolerance
import scalafim.linalg.GaleMatrixBridge
import scalafim.linalg.LinearCompositeFunctional
import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapError
import scalafim.linalg.ProjectionSet
import scalafim.linalg.ProximalObjective
import scalafim.linalg.ProximalTerm
import scalafim.linalg.SmoothObjective
import scalafim.linalg.SolverMethodRequest
import scala.util.control.NonFatal

enum VariationalExecutionForm:
  case SmoothSeparableProximal
  case SmoothProjection
  case SmoothLinearComposite
  case LinearComposite
  case ExactNullSpace

final case class VariationalSolverSelection(
    form: VariationalExecutionForm,
    method: FirstOrderMethod
)

object VariationalSolverCompiler:
  def select(
      form: VariationalExecutionForm,
      request: SolverMethodRequest,
      capabilities: FirstOrderCapabilities
  ): Either[CompositeLoweringError, VariationalSolverSelection] =
    val compatible = form match
      case VariationalExecutionForm.SmoothSeparableProximal => Vector(FirstOrderMethod.ProximalGradient)
      case VariationalExecutionForm.SmoothProjection => Vector(FirstOrderMethod.ProjectedGradient)
      case VariationalExecutionForm.SmoothLinearComposite => Vector(FirstOrderMethod.SmoothCompositePrimalDual)
      case VariationalExecutionForm.LinearComposite => Vector(FirstOrderMethod.LinearCompositePrimalDual)
      case VariationalExecutionForm.ExactNullSpace => Vector(FirstOrderMethod.ExactLinearReduction)
    capabilities
      .select(compatible, request)
      .left
      .map(CompositeLoweringError.SolverBoundary.apply)
      .map(method => VariationalSolverSelection(form, method))

  def compileL1[Source <: SemanticSpace, Target <: SemanticSpace](
      plan: CompositePenaltyPlan[Source, Target],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledLinearCompositePenalty[Source, Target]] =
    if plan.functional != CompositeFunctional.ElementwiseL1 then
      Left(CompositeLoweringError.FunctionalUnsupported(plan.original.functional))
    else compileCompositePenalty(plan, observation, request, capabilities)

  def compileCompositePenalty[Source <: SemanticSpace, Target <: SemanticSpace](
      plan: CompositePenaltyPlan[Source, Target],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledLinearCompositePenalty[Source, Target]] =
    if plan.method != SplitMethod.PrimalDual then
      Left(CompositeLoweringError.ReferenceMethodUnsupported(plan.method))
    else if plan.functional.isInstanceOf[CompositeFunctional.LatentOverlappingGroups] then
      Left(CompositeLoweringError.InvalidDefinition("overlapping groups require the exact lifted-variable compiler"))
    else if observation.rows != plan.targetOperator.cols then
      Left(CompositeLoweringError.InvalidDefinition("observation rows do not match the source parameter"))
    else
      for
        selection <- select(VariationalExecutionForm.LinearComposite, request, capabilities)
        representation <- plan.targetOperator(DMat.eye(plan.targetOperator.cols)).left.map(CompositeLoweringError.Semantic.apply)
        normBound = frobenius(representation)
        _ <-
          if normBound.isFinite then Right(())
          else Left(CompositeLoweringError.NumericalFailure("target norm bound is non-finite"))
        numericalMap <- BoundedLinearMap
          .from(new OperatorLinearMap(plan.targetOperator), normBound)
          .left
          .map(CompositeLoweringError.SolverBoundary.apply)
        provenance = plan.provenance.append(
          SemanticProvenanceEvent.Derived(
            "compile-generic-linear-composite",
            Vector(plan.targetOperator.valueIdentity)
          )
        )
      yield
        new CompiledLinearCompositePenalty(
          plan,
          observation,
          numericalMap,
          selection,
          provenance
        )

  def compileDirectPenalty[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      plan: DirectProximalPlan[Feature, Coordinates],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledDirectPenalty[Feature, Coordinates]] =
    if observation.rows != plan.chart.featureSpace.dimension then
      Left(CompositeLoweringError.InvalidDefinition("observation rows do not match the feature chart"))
    else
      select(VariationalExecutionForm.SmoothSeparableProximal, request, capabilities).map: selection =>
        new CompiledDirectPenalty(plan, observation, selection)

  def compilePenalty[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      original: PenaltyTerm,
      chart: FeatureChart[Feature, Coordinates],
      kind: DirectProximalKind,
      observation: DMat,
      request: SolverMethodRequest,
      capabilities: FirstOrderCapabilities
  ): Either[CompositeLoweringError, CompiledDirectPenalty[Feature, Coordinates]] =
    DirectProximalPlan
      .from(original, chart, kind)
      .left
      .map(CompositeLoweringError.Chart.apply)
      .flatMap(plan => compileDirectPenalty(plan, observation, request, capabilities))

  def compileDirectConstraint[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      plan: DirectProjectionPlan[Feature, Coordinates],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledDirectConstraint[Feature, Coordinates]] =
    if observation.rows != plan.chart.featureSpace.dimension then
      Left(CompositeLoweringError.InvalidDefinition("observation rows do not match the feature chart"))
    else
      select(VariationalExecutionForm.SmoothProjection, request, capabilities).map: selection =>
        new CompiledDirectConstraint(plan, observation, selection)

  def compileConstraint[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      original: ConstraintTerm,
      chart: FeatureChart[Feature, Coordinates],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledDirectConstraint[Feature, Coordinates]] =
    DirectProjectionPlan
      .from(original, chart)
      .left
      .map(CompositeLoweringError.Chart.apply)
      .flatMap(plan => compileDirectConstraint(plan, observation, request, capabilities))

  def compileOverlappingGroups[Source <: SemanticSpace, Coordinates <: SemanticSpace](
      plan: CompositePenaltyPlan[Source, Coordinates],
      chart: FeatureChart[Source, Coordinates],
      observation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledOverlappingGroups[Source, Coordinates]] =
    plan.functional match
      case CompositeFunctional.LatentOverlappingGroups(structure) =>
        if plan.method != SplitMethod.PrimalDual then
          Left(CompositeLoweringError.ReferenceMethodUnsupported(plan.method))
        else if plan.targetOperator.valueIdentity != chart.forward.valueIdentity then
          Left(CompositeLoweringError.InvalidDefinition("overlapping group plan must target the supplied feature chart"))
        else if chart.kind != ChartKind.Identity && !chart.kind.isInstanceOf[ChartKind.Orthogonal] then
          Left(CompositeLoweringError.InvalidDefinition("exact lifted overlapping groups require an identity or certified orthogonal chart"))
        else if observation.rows != chart.featureSpace.dimension then
          Left(CompositeLoweringError.InvalidDefinition("observation rows do not match the overlapping group feature chart"))
        else
          for
            selection <- select(VariationalExecutionForm.LinearComposite, request, capabilities)
            lift <- OverlappingGroupLift.from(structure)
            coordinateObservation <- chart.forward(observation).left.map(CompositeLoweringError.Semantic.apply)
            numericalMap <- BoundedLinearMap
              .from(new OverlappingGroupAggregateMap(lift), lift.normUpperBound)
              .left
              .map(CompositeLoweringError.SolverBoundary.apply)
          yield new CompiledOverlappingGroups(plan, coordinateObservation, lift, numericalMap, selection)
      case _ => Left(CompositeLoweringError.FunctionalUnsupported(plan.original.functional))

  def compileAlignedScorePenalty(
      target: AlignedScoreTarget,
      original: PenaltyTerm,
      sourceObservation: DMat,
      targetObservation: DMat,
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositeLoweringError, CompiledAlignedScorePenalty] =
    if original.target != target.erased then
      Left(CompositeLoweringError.InvalidDefinition("aligned-score penalty does not bind the supplied typed target"))
    else if sourceObservation.cols != targetObservation.cols then
      Left(CompositeLoweringError.InvalidDefinition("aligned-score frames must have the same component count"))
    else
      val functional = original.functional match
        case FunctionalKind.L1 => Right(CompositeFunctional.ElementwiseL1)
        case FunctionalKind.GroupL21 => Right(CompositeFunctional.RowGroupL21)
        case FunctionalKind.Huber(delta) => Right(CompositeFunctional.Huber(delta.value))
        case other => Left(CompositeLoweringError.FunctionalUnsupported(other))
      for
        selectedFunctional <- functional
        selection <- select(VariationalExecutionForm.LinearComposite, request, capabilities)
        probe <- evaluateAlignedTarget(
          target,
          DMat.zeros(sourceObservation.rows, 1),
          DMat.zeros(targetObservation.rows, 1)
        )
        map = new AlignedScoreLinearMap(
          target,
          sourceObservation.rows,
          targetObservation.rows,
          probe.rows
        )
        representation <- map.forward(DoubleMatrix.eye(map.cols)).left.map: error =>
          CompositeLoweringError.NumericalFailure(error.message)
        normBound = Math.sqrt(representation.copyData.foldLeft(0.0)((sum, current) => sum + current * current))
        bounded <- BoundedLinearMap.from(map, normBound).left.map(CompositeLoweringError.SolverBoundary.apply)
      yield
        new CompiledAlignedScorePenalty(
          target,
          original,
          selectedFunctional,
          sourceObservation,
          targetObservation,
          bounded,
          selection
        )

final class CompiledDirectPenalty[
    Feature <: SemanticSpace,
    Coordinates <: SemanticSpace
] private[multivar] (
    val semanticPlan: DirectProximalPlan[Feature, Coordinates],
    val observation: DMat,
    val selection: VariationalSolverSelection
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, scalafim.linalg.FirstOrderSolution] =
    for
      numericalConfig <- firstOrderConfig(config)
      solution <- FirstOrderSolvers
        .proximalGradient(
          quadraticSmooth(observation),
          new DirectPenaltyTerm(semanticPlan),
          GaleMatrixBridge.fromGale(observation),
          numericalConfig
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
    yield solution

final class CompiledDirectConstraint[
    Feature <: SemanticSpace,
    Coordinates <: SemanticSpace
] private[multivar] (
    val semanticPlan: DirectProjectionPlan[Feature, Coordinates],
    val observation: DMat,
    val selection: VariationalSolverSelection
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, scalafim.linalg.FirstOrderSolution] =
    for
      numericalConfig <- firstOrderConfig(config)
      initial <- semanticPlan(observation).left.map(CompositeLoweringError.Chart.apply)
      solution <- FirstOrderSolvers
        .projectedGradient(
          quadraticSmooth(observation),
          new DirectConstraintSet(semanticPlan),
          GaleMatrixBridge.fromGale(initial),
          numericalConfig
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
    yield solution

final case class LiftedGroupSolution(
    parameter: DMat,
    auxiliary: DMat,
    dual: DMat,
    objective: Double,
    status: SplitStoppingStatus,
    certificate: scalafim.linalg.FirstOrderCertificate
)

final case class AlignedScoreSolution(
    sourceParameter: DMat,
    targetParameter: DMat,
    auxiliary: DMat,
    dual: DMat,
    objective: Double,
    status: SplitStoppingStatus,
    certificate: scalafim.linalg.FirstOrderCertificate
)

final class CompiledAlignedScorePenalty private[multivar] (
    val target: AlignedScoreTarget,
    val original: PenaltyTerm,
    val functional: CompositeFunctional,
    val sourceObservation: DMat,
    val targetObservation: DMat,
    val numericalMap: BoundedLinearMap,
    val selection: VariationalSolverSelection
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, AlignedScoreSolution] =
    val observation = stackRows(sourceObservation, targetObservation)
    for
      numericalConfig <- firstOrderConfig(config)
      solution <- FirstOrderSolvers
        .linearCompositePrimalDual(
          quadraticProximal(observation),
          linearCompositeFunctional(functional, numericalMap.map.rows, original.weight.value),
          numericalMap,
          GaleMatrixBridge.fromGale(observation),
          numericalConfig
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
      dualValue <- solution.dual.toRight(
        CompositeLoweringError.NumericalFailure("aligned-score solver returned no dual value")
      )
      parameter = GaleMatrixBridge.toGale(solution.primal)
      sourceParameter = takeRows(parameter, 0, sourceObservation.rows)
      targetParameter = takeRows(parameter, sourceObservation.rows, targetObservation.rows)
      auxiliary <- evaluateAlignedTarget(target, sourceParameter, targetParameter)
      dual = GaleMatrixBridge.toGale(dualValue)
      transposeValue <- numericalMap.map.adjoint
        .forward(dualValue)
        .left
        .map(error => CompositeLoweringError.NumericalFailure(error.message))
      transpose = GaleMatrixBridge.toGale(transposeValue)
      certificate = alignedCertificate(
        parameter,
        observation,
        auxiliary,
        dual,
        transpose,
        functional,
        original.weight.value,
        solution,
        config.tolerance
      )
      status = solution.status match
        case FirstOrderStoppingStatus.Converged => SplitStoppingStatus.Converged(certificate)
        case FirstOrderStoppingStatus.IterationLimit => SplitStoppingStatus.IterationLimit(certificate)
    yield
      AlignedScoreSolution(
        sourceParameter,
        targetParameter,
        auxiliary,
        dual,
        solution.objective,
        status,
        solution.certificate
      )

final class CompiledOverlappingGroups[
    Source <: SemanticSpace,
    Target <: SemanticSpace
] private[multivar] (
    val semanticPlan: CompositePenaltyPlan[Source, Target],
    val observation: DMat,
    val lift: OverlappingGroupLift,
    val numericalMap: BoundedLinearMap,
    val selection: VariationalSolverSelection
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, LiftedGroupSolution] =
    for
      numericalConfig <- firstOrderConfig(config)
      initial <- lift.feasibleLift(observation)
      solution <- FirstOrderSolvers
        .linearCompositePrimalDual(
          new LiftedGroupObjective(lift, semanticPlan.original.weight.value),
          quadraticComposite(observation),
          numericalMap,
          GaleMatrixBridge.fromGale(initial),
          numericalConfig
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
      dualValue <- solution.dual.toRight(
        CompositeLoweringError.NumericalFailure("lifted group solver returned no dual value")
      )
      auxiliary = GaleMatrixBridge.toGale(solution.primal)
      parameter <- lift.aggregate(auxiliary)
      dual = GaleMatrixBridge.toGale(dualValue)
      certificate <- liftedGroupCertificate(parameter, auxiliary, dual, solution, config.tolerance)
      status = solution.status match
        case FirstOrderStoppingStatus.Converged => SplitStoppingStatus.Converged(certificate)
        case FirstOrderStoppingStatus.IterationLimit => SplitStoppingStatus.IterationLimit(certificate)
    yield LiftedGroupSolution(parameter, auxiliary, dual, solution.objective, status, solution.certificate)

  private def liftedGroupCertificate(
      parameter: DMat,
      auxiliary: DMat,
      dual: DMat,
      solution: scalafim.linalg.FirstOrderSolution,
      tolerance: CertificateTolerance
  ): Either[CompositeLoweringError, SplitResidualCertificate] =
    for
      adjoint <- lift.adjoint(dual)
      penalty <- lift.value(auxiliary)
    yield
      val weight = semanticPlan.original.weight.value
      val dualFeasibility = liftedDualViolation(adjoint, lift.structure, weight)
      val primalObjective = 0.5 * squaredNorm(MatrixOps.subtract(parameter, observation)) + weight * penalty
      val dualObjective =
        if dualFeasibility <= tolerance.threshold(1.0) then
          -0.5 * squaredNorm(dual) - inner(dual, observation)
        else Double.NegativeInfinity
      SplitResidualCertificate(
        solution.certificate.primalResidual,
        dualFeasibility,
        solution.certificate.dualResidual,
        if dualObjective.isFinite then Math.max(0.0, primalObjective - dualObjective) else Double.MaxValue,
        solution.certificate.iterations,
        tolerance
      )

final class CompiledLinearCompositePenalty[
    Source <: SemanticSpace,
    Target <: SemanticSpace
] private[multivar] (
    val semanticPlan: CompositePenaltyPlan[Source, Target],
    val observation: DMat,
    val numericalMap: BoundedLinearMap,
    val selection: VariationalSolverSelection,
    val provenance: SemanticProvenance
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, PrimalDualSolution] =
    for
      tolerance <- FirstOrderTolerance
        .from(config.tolerance.absoluteValue, config.tolerance.relativeValue)
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
      numericalConfig <- FirstOrderConfig
        .from(
          config.iterations.intValue,
          tolerance,
          extrapolation = config.extrapolation.value
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
      solution <- FirstOrderSolvers
        .linearCompositePrimalDual(
          quadraticProximal(observation),
          linearCompositeFunctional(
            semanticPlan.functional,
            semanticPlan.targetOperator.rows,
            semanticPlan.original.weight.value
          ),
          numericalMap,
          GaleMatrixBridge.fromGale(observation),
          numericalConfig
        )
        .left
        .map(CompositeLoweringError.SolverBoundary.apply)
      dualValue <- solution.dual.toRight(
        CompositeLoweringError.NumericalFailure("linear-composite solver returned no dual value")
      )
      parameter = GaleMatrixBridge.toGale(solution.primal)
      dual = GaleMatrixBridge.toGale(dualValue)
      auxiliary <- semanticPlan.targetOperator(parameter).left.map(CompositeLoweringError.Semantic.apply)
      certificate <- validateCertificate(
        parameter,
        dual,
        auxiliary,
        config.tolerance,
        solution.certificate
      )
      status = solution.status match
        case FirstOrderStoppingStatus.Converged => SplitStoppingStatus.Converged(certificate)
        case FirstOrderStoppingStatus.IterationLimit => SplitStoppingStatus.IterationLimit(certificate)
    yield
      PrimalDualSolution(
        parameter,
        auxiliary,
        dual,
        solution.objective,
        status,
        solution.certificate
      )

  private def validateCertificate(
      parameter: DMat,
      dual: DMat,
      auxiliary: DMat,
      tolerance: CertificateTolerance,
      numerical: scalafim.linalg.FirstOrderCertificate
  ): Either[CompositeLoweringError, SplitResidualCertificate] =
    semanticPlan.targetOperator.dual(dual).left.map(CompositeLoweringError.Semantic.apply).map: transpose =>
      val lambda = semanticPlan.original.weight.value
      val stationarity = matrixMaxAbs(add(MatrixOps.subtract(parameter, observation), transpose))
      val (dualFeasibility, complementarity) =
        compositeResiduals(semanticPlan.functional, auxiliary, dual, lambda, tolerance)
      val primalObjective =
        0.5 * squaredNorm(MatrixOps.subtract(parameter, observation)) +
          compositeValue(semanticPlan.functional, auxiliary, lambda)
      val conjugate = compositeConjugate(semanticPlan.functional, dual, lambda, tolerance)
      val dualObjective =
        if conjugate.isFinite then -0.5 * squaredNorm(transpose) + inner(transpose, observation) - conjugate
        else Double.NegativeInfinity
      SplitResidualCertificate(
        stationarity,
        dualFeasibility,
        complementarity,
        if dualObjective.isFinite then Math.max(0.0, primalObjective - dualObjective) else Double.MaxValue,
        numerical.iterations,
        tolerance
      )

private final class AlignedScoreLinearMap(
    target: AlignedScoreTarget,
    sourceRows: Int,
    targetRows: Int,
    outputRows: Int
) extends LinearMap:
  val rows: Int = outputRows
  val cols: Int = sourceRows + targetRows

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else
      try
        val value = GaleMatrixBridge.toGale(input)
        val source = takeRows(value, 0, sourceRows)
        val targetParameter = takeRows(value, sourceRows, targetRows)
        val result = target.map((source, targetParameter))
        if result.rows != rows then Left(LinearMapError.DimensionMismatch(rows, result.rows))
        else if result.cols != input.cols then
          Left(LinearMapError.OperatorApplicationFailed("aligned-score map changed the component count"))
        else Right(GaleMatrixBridge.fromGale(result))
      catch
        case NonFatal(error) => Left(LinearMapError.OperatorApplicationFailed(error.getMessage))

  def adjoint: LinearMap =
    new LinearMap:
      val rows: Int = AlignedScoreLinearMap.this.cols
      val cols: Int = AlignedScoreLinearMap.this.rows

      def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
        if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
        else
          try
            val (source, targetParameter) = target.map.dual(GaleMatrixBridge.toGale(input))
            if source.rows != sourceRows then Left(LinearMapError.DimensionMismatch(sourceRows, source.rows))
            else if targetParameter.rows != targetRows then
              Left(LinearMapError.DimensionMismatch(targetRows, targetParameter.rows))
            else if source.cols != input.cols || targetParameter.cols != input.cols then
              Left(LinearMapError.OperatorApplicationFailed("aligned-score adjoint changed the component count"))
            else Right(GaleMatrixBridge.fromGale(stackRows(source, targetParameter)))
          catch
            case NonFatal(error) => Left(LinearMapError.OperatorApplicationFailed(error.getMessage))

      def adjoint: LinearMap = AlignedScoreLinearMap.this

private final class OverlappingGroupAggregateMap(lift: OverlappingGroupLift) extends LinearMap:
  val rows: Int = lift.structure.coordinateDimension
  val cols: Int = lift.auxiliaryRows

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    lift
      .aggregate(GaleMatrixBridge.toGale(input))
      .left
      .map(error => LinearMapError.OperatorApplicationFailed(error.message))
      .map(GaleMatrixBridge.fromGale)

  def adjoint: LinearMap =
    new LinearMap:
      val rows: Int = lift.auxiliaryRows
      val cols: Int = lift.structure.coordinateDimension
      def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
        lift
          .adjoint(GaleMatrixBridge.toGale(input))
          .left
          .map(error => LinearMapError.OperatorApplicationFailed(error.message))
          .map(GaleMatrixBridge.fromGale)
      def adjoint: LinearMap = OverlappingGroupAggregateMap.this

private final class DirectPenaltyTerm[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
    plan: DirectProximalPlan[Feature, Coordinates]
) extends ProximalTerm:
  val variableRows: Int = plan.chart.featureSpace.dimension

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    plan.chart
      .forward(GaleMatrixBridge.toGale(at))
      .left
      .map(error => FirstOrderError.OracleFailure("direct penalty chart", error.message))
      .map(coordinates => directPenaltyValue(plan.kind, coordinates, plan.original.weight.value))

  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
    plan(GaleMatrixBridge.toGale(at), PenaltyWeight.unsafe(step))
      .left
      .map(error => FirstOrderError.OracleFailure("direct penalty proximal", error.message))
      .map(GaleMatrixBridge.fromGale)

private final class DirectConstraintSet[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
    plan: DirectProjectionPlan[Feature, Coordinates]
) extends ProjectionSet:
  val variableRows: Int = plan.chart.featureSpace.dimension

  def project(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
    plan(GaleMatrixBridge.toGale(at))
      .left
      .map(error => FirstOrderError.OracleFailure("direct constraint projection", error.message))
      .map(GaleMatrixBridge.fromGale)

private final class LiftedGroupObjective(
    lift: OverlappingGroupLift,
    weight: Double
) extends ProximalObjective:
  val variableRows: Int = lift.auxiliaryRows

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    lift
      .value(GaleMatrixBridge.toGale(at))
      .left
      .map(error => FirstOrderError.OracleFailure("lifted group objective", error.message))
      .map(_ * weight)

  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
    lift
      .proximal(GaleMatrixBridge.toGale(at), step * weight)
      .left
      .map(error => FirstOrderError.OracleFailure("lifted group proximal", error.message))
      .map(GaleMatrixBridge.fromGale)

private def firstOrderConfig(config: PrimalDualConfig): Either[CompositeLoweringError, FirstOrderConfig] =
  for
    tolerance <- FirstOrderTolerance
      .from(config.tolerance.absoluteValue, config.tolerance.relativeValue)
      .left
      .map(CompositeLoweringError.SolverBoundary.apply)
    numerical <- FirstOrderConfig
      .from(
        config.iterations.intValue,
        tolerance,
        extrapolation = config.extrapolation.value
      )
      .left
      .map(CompositeLoweringError.SolverBoundary.apply)
  yield numerical

private def quadraticSmooth(center: DMat): SmoothObjective =
  val numericalCenter = GaleMatrixBridge.fromGale(center)
  new SmoothObjective:
    val variableRows: Int = center.rows
    val lipschitz: Double = 1.0
    def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
      Right(0.5 * numericalSquaredNorm(numericalSubtract(at, numericalCenter)))
    def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
      Right(numericalSubtract(at, numericalCenter))

private def quadraticProximal(center: DMat): ProximalObjective =
  val numericalCenter = GaleMatrixBridge.fromGale(center)
  new ProximalObjective:
    val variableRows: Int = center.rows
    def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
      Right(0.5 * numericalSquaredNorm(numericalSubtract(at, numericalCenter)))
    def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
      Right(
        numericalScale(
          numericalAdd(at, numericalScale(numericalCenter, step)),
          1.0 / (1.0 + step)
        )
      )

private def linearCompositeFunctional(
    functional: CompositeFunctional,
    rows: Int,
    weight: Double
): LinearCompositeFunctional =
  new LinearCompositeFunctional:
    val targetRows: Int = rows
    def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
      Right(compositeValue(functional, GaleMatrixBridge.toGale(at), weight))
    def proximalConjugate(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
      val value = GaleMatrixBridge.toGale(at)
      val result = functional match
        case CompositeFunctional.ElementwiseL1 => clipElements(value, weight)
        case CompositeFunctional.RowGroupL21 => clipRows(value, weight)
        case CompositeFunctional.Huber(delta) =>
          clipElements(MatrixOps.scale(value, 1.0 / (1.0 + step * delta / weight)), weight)
        case CompositeFunctional.LatentOverlappingGroups(_) => value
      Right(GaleMatrixBridge.fromGale(result))

private def quadraticComposite(center: DMat): LinearCompositeFunctional =
  val numericalCenter = GaleMatrixBridge.fromGale(center)
  new LinearCompositeFunctional:
    val targetRows: Int = center.rows
    def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
      Right(0.5 * numericalSquaredNorm(numericalSubtract(at, numericalCenter)))
    def proximalConjugate(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
      Right(numericalScale(numericalSubtract(at, numericalScale(numericalCenter, step)), 1.0 / (1.0 + step)))

private def directPenaltyValue(kind: DirectProximalKind, value: DMat, weight: Double): Double =
  val raw = kind match
    case DirectProximalKind.ElementwiseL1 => l1(value)
    case DirectProximalKind.FeatureRowsL21 => rowL21(value)
    case DirectProximalKind.DisjointGroups(groups) => groupL2(value, groups)
    case DirectProximalKind.SparseGroup(fraction, groups) =>
      fraction.value * l1(value) + (1.0 - fraction.value) * groupL2(value, groups)
    case DirectProximalKind.ElasticNet(fraction) =>
      fraction.value * l1(value) + 0.5 * (1.0 - fraction.value) * squaredNorm(value)
  weight * raw

private def compositeValue(functional: CompositeFunctional, value: DMat, weight: Double): Double =
  functional match
    case CompositeFunctional.ElementwiseL1 => weight * l1(value)
    case CompositeFunctional.RowGroupL21 => weight * rowL21(value)
    case CompositeFunctional.Huber(delta) => weight * huber(value, delta)
    case CompositeFunctional.LatentOverlappingGroups(groups) => weight * groupL2(value, groups)

private def compositeConjugate(
    functional: CompositeFunctional,
    dual: DMat,
    weight: Double,
    tolerance: CertificateTolerance
): Double =
  functional match
    case CompositeFunctional.ElementwiseL1 =>
      if matrixMaxAbs(dual) <= weight + tolerance.threshold(1.0) then 0.0 else Double.PositiveInfinity
    case CompositeFunctional.RowGroupL21 =>
      if maxRowNorm(dual) <= weight + tolerance.threshold(1.0) then 0.0 else Double.PositiveInfinity
    case CompositeFunctional.Huber(delta) =>
      if matrixMaxAbs(dual) <= weight + tolerance.threshold(1.0) then
        delta * squaredNorm(dual) / (2.0 * weight)
      else Double.PositiveInfinity
    case CompositeFunctional.LatentOverlappingGroups(_) => Double.PositiveInfinity

private def compositeResiduals(
    functional: CompositeFunctional,
    value: DMat,
    dual: DMat,
    weight: Double,
    tolerance: CertificateTolerance
): (Double, Double) =
  functional match
    case CompositeFunctional.ElementwiseL1 =>
      var feasibility = 0.0
      var subgradient = 0.0
      var row = 0
      while row < value.rows do
        var column = 0
        while column < value.cols do
          val current = value(row, column)
          val multiplier = dual(row, column)
          feasibility = Math.max(feasibility, Math.max(0.0, Math.abs(multiplier) - weight))
          val residual =
            if Math.abs(current) > tolerance.threshold(1.0) then
              Math.abs(multiplier - weight * Math.signum(current))
            else Math.max(0.0, Math.abs(multiplier) - weight)
          subgradient = Math.max(subgradient, residual)
          column += 1
        row += 1
      feasibility -> subgradient
    case CompositeFunctional.RowGroupL21 =>
      val feasibility = Math.max(0.0, maxRowNorm(dual) - weight)
      var subgradient = 0.0
      var row = 0
      while row < value.rows do
        val norm = rowNorm(value, row)
        val dualNorm = rowNorm(dual, row)
        if norm > tolerance.threshold(1.0) then
          var column = 0
          while column < value.cols do
            subgradient = Math.max(subgradient, Math.abs(dual(row, column) - weight * value(row, column) / norm))
            column += 1
        else subgradient = Math.max(subgradient, Math.max(0.0, dualNorm - weight))
        row += 1
      feasibility -> subgradient
    case CompositeFunctional.Huber(delta) =>
      val feasibility = Math.max(0.0, matrixMaxAbs(dual) - weight)
      var residual = 0.0
      var row = 0
      while row < value.rows do
        var column = 0
        while column < value.cols do
          val current = value(row, column)
          val gradient = weight * Math.max(-1.0, Math.min(1.0, current / delta))
          residual = Math.max(residual, Math.abs(dual(row, column) - gradient))
          column += 1
        row += 1
      feasibility -> residual
    case CompositeFunctional.LatentOverlappingGroups(_) => Double.MaxValue -> Double.MaxValue

private def alignedCertificate(
    parameter: DMat,
    observation: DMat,
    auxiliary: DMat,
    dual: DMat,
    transpose: DMat,
    functional: CompositeFunctional,
    weight: Double,
    solution: scalafim.linalg.FirstOrderSolution,
    tolerance: CertificateTolerance
): SplitResidualCertificate =
  val stationarity = matrixMaxAbs(add(MatrixOps.subtract(parameter, observation), transpose))
  val (dualFeasibility, complementarity) =
    compositeResiduals(functional, auxiliary, dual, weight, tolerance)
  val primalObjective =
    0.5 * squaredNorm(MatrixOps.subtract(parameter, observation)) +
      compositeValue(functional, auxiliary, weight)
  val conjugate = compositeConjugate(functional, dual, weight, tolerance)
  val dualObjective =
    if conjugate.isFinite then -0.5 * squaredNorm(transpose) + inner(transpose, observation) - conjugate
    else Double.NegativeInfinity
  SplitResidualCertificate(
    stationarity,
    dualFeasibility,
    complementarity,
    if dualObjective.isFinite then Math.max(0.0, primalObjective - dualObjective) else Double.MaxValue,
    solution.certificate.iterations,
    tolerance
  )

private def evaluateAlignedTarget(
    target: AlignedScoreTarget,
    source: DMat,
    targetParameter: DMat
): Either[CompositeLoweringError, DMat] =
  try Right(target.map((source, targetParameter)))
  catch
    case NonFatal(error) =>
      Left(CompositeLoweringError.NumericalFailure(s"aligned-score target failed: ${error.getMessage}"))

private def takeRows(value: DMat, start: Int, count: Int): DMat =
  val output = new Array[Double](count * value.cols)
  var row = 0
  while row < count do
    var column = 0
    while column < value.cols do
      output(row * value.cols + column) = value(start + row, column)
      column += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(count, value.cols, output)

private def stackRows(top: DMat, bottom: DMat): DMat =
  require(top.cols == bottom.cols, "stacked matrices must have equal column counts")
  val output = new Array[Double]((top.rows + bottom.rows) * top.cols)
  var row = 0
  while row < top.rows do
    var column = 0
    while column < top.cols do
      output(row * top.cols + column) = top(row, column)
      column += 1
    row += 1
  row = 0
  while row < bottom.rows do
    var column = 0
    while column < bottom.cols do
      output((top.rows + row) * top.cols + column) = bottom(row, column)
      column += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(top.rows + bottom.rows, top.cols, output)

private def liftedDualViolation(
    adjoint: DMat,
    structure: GroupStructure,
    weight: Double
): Double =
  var result = 0.0
  var offset = 0
  structure.groups.foreach: group =>
    var squared = 0.0
    var local = 0
    while local < group.indices.length do
      var column = 0
      while column < adjoint.cols do
        val current = adjoint(offset + local, column)
        squared += current * current
        column += 1
      local += 1
    result = Math.max(result, Math.max(0.0, Math.sqrt(squared) - weight))
    offset += group.indices.length
  result

private def clipElements(value: DMat, bound: Double): DMat =
  val output = matrixData(value)
  var index = 0
  while index < output.length do
    output(index) = Math.max(-bound, Math.min(bound, output(index)))
    index += 1
  GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

private def clipRows(value: DMat, bound: Double): DMat =
  val output = matrixData(value)
  var row = 0
  while row < value.rows do
    val norm = rowNorm(value, row)
    val scale = if norm <= bound || norm == 0.0 then 1.0 else bound / norm
    var column = 0
    while column < value.cols do
      output(row * value.cols + column) *= scale
      column += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(value.rows, value.cols, output)

private def numericalAdd(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
  val output = left.copyData
  val rightValues = right.copyData
  var index = 0
  while index < output.length do
    output(index) += rightValues(index)
    index += 1
  DoubleMatrix.fromRowMajor(left.shape, output).toOption.get

private def numericalSubtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
  numericalAdd(left, numericalScale(right, -1.0))

private def numericalScale(value: DoubleMatrix, factor: Double): DoubleMatrix =
  numericalMapValues(value)(_ * factor)

private def numericalMapValues(value: DoubleMatrix)(function: Double => Double): DoubleMatrix =
  val output = value.copyData
  var index = 0
  while index < output.length do
    output(index) = function(output(index))
    index += 1
  DoubleMatrix.fromRowMajor(value.shape, output).toOption.get

private def numericalSquaredNorm(value: DoubleMatrix): Double =
  value.copyData.foldLeft(0.0)((result, current) => result + current * current)

private def numericalL1(value: DoubleMatrix): Double =
  value.copyData.foldLeft(0.0)((result, current) => result + Math.abs(current))

private def add(left: DMat, right: DMat): DMat =
  MatrixOps.subtract(left, MatrixOps.scale(right, -1.0))

private def inner(left: DMat, right: DMat): Double =
  var result = 0.0
  var row = 0
  while row < left.rows do
    var column = 0
    while column < left.cols do
      result += left(row, column) * right(row, column)
      column += 1
    row += 1
  result

private def squaredNorm(value: DMat): Double =
  inner(value, value)

private def frobenius(value: DMat): Double =
  Math.sqrt(squaredNorm(value))

private def l1(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      result += Math.abs(value(row, column))
      column += 1
    row += 1
  result

private def rowL21(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    result += rowNorm(value, row)
    row += 1
  result

private def groupL2(value: DMat, groups: GroupStructure): Double =
  var result = 0.0
  groups.groups.foreach: group =>
    var squared = 0.0
    group.indices.indices.foreach: row =>
      var column = 0
      while column < value.cols do
        val current = value(row, column)
        squared += current * current
        column += 1
    result += Math.sqrt(squared)
  result

private def huber(value: DMat, delta: Double): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      val current = Math.abs(value(row, column))
      val contribution =
        if current <= delta then current * current / (2.0 * delta)
        else current - delta / 2.0
      result += contribution
      column += 1
    row += 1
  result

private def rowNorm(value: DMat, row: Int): Double =
  var squared = 0.0
  var column = 0
  while column < value.cols do
    val current = value(row, column)
    squared += current * current
    column += 1
  Math.sqrt(squared)

private def maxRowNorm(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    result = Math.max(result, rowNorm(value, row))
    row += 1
  result

private def matrixData(value: DMat): Array[Double] =
  val output = new Array[Double](value.rows * value.cols)
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      output(row * value.cols + column) = value(row, column)
      column += 1
    row += 1
  output

private def matrixMaxAbs(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      result = Math.max(result, Math.abs(value(row, column)))
      column += 1
    row += 1
  result
