package scalafim.multivar

import gale.linalg.DMat
import scalafim.linalg.FirstOrderCertificate
import scalafim.linalg.FirstOrderSolution
import scalafim.linalg.FirstOrderStoppingStatus
import scalafim.linalg.GaleMatrixBridge

enum VariationalFrameTerm:
  case Penalty(value: PenaltyTerm)
  case Constraint(value: ConstraintTerm)

enum VariationalFrameStopping:
  case FirstOrder(value: FirstOrderStoppingStatus)
  case Split(value: SplitStoppingStatus)

final case class VariationalFrameCertificate(
    numerical: FirstOrderCertificate,
    stationarityResidual: Double,
    feasibilityResidual: Double,
    primalDualGap: Option[Double]
):
  require(stationarityResidual.isFinite && stationarityResidual >= 0.0)
  require(feasibilityResidual.isFinite && feasibilityResidual >= 0.0)
  require(primalDualGap.forall(value => value.isFinite && value >= 0.0))

final case class VariationalLoweringContract(
    parameterization: ParameterizationKind,
    chart: Option[ChartKind],
    chartIdentity: Option[ValueIdentity],
    chartLaw: Option[ChartLawCertificate],
    termSymmetry: FrameSymmetry,
    gauge: ParameterizationGauge,
    resultEquivalence: ResultEquivalence
)

/** Convex coefficient-space refinement of a typed functional frame.
  *
  * The fitted problem is explicit: minimize Euclidean squared distance from
  * `anchor` plus one declared penalty, or over one declared feasible set. This
  * is not a claim about the nonconvex normalized spectral program that produced
  * the anchor.
  */
final class ConvexFunctionalFrameProblem[
    Feature <: SemanticSpace,
    Component <: SemanticSpace,
    E <: OperatorEvidence
] private (
    val variable: FrameVariable[Feature, Component],
    val anchor: FunctionalFrame[Feature, Component, E]
)

object ConvexFunctionalFrameProblem:
  def from[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      anchor: FunctionalFrame[Feature, Component, E]
  ): Either[CompositeLoweringError, ConvexFunctionalFrameProblem[Feature, Component, E]] =
    if anchor.weights.codomain.descriptor.space != variable.featureSpace.descriptor then
      Left(CompositeLoweringError.InvalidDefinition("anchor feature space does not match the frame variable"))
    else if anchor.weights.domain.descriptor.space != variable.componentSpace.descriptor then
      Left(CompositeLoweringError.InvalidDefinition("anchor component space does not match the frame variable"))
    else Right(new ConvexFunctionalFrameProblem(variable, anchor))

final case class VariationalFunctionalFrameFit[
    Feature <: SemanticSpace,
    Component <: SemanticSpace
](
    frame: FunctionalFrame[Feature, Component, UncheckedEvidence],
    term: VariationalFrameTerm,
    selection: VariationalSolverSelection,
    attainedGuarantee: Option[SolverGuarantee],
    stopping: VariationalFrameStopping,
    certificate: VariationalFrameCertificate,
    lowering: VariationalLoweringContract,
    provenance: SemanticProvenance
)

final case class AlignedVariationalFunctionalFrameFit[
    Source <: SemanticSpace,
    Target <: SemanticSpace,
    Component <: SemanticSpace
](
    sourceFrame: FunctionalFrame[Source, Component, UncheckedEvidence],
    targetFrame: FunctionalFrame[Target, Component, UncheckedEvidence],
    term: PenaltyTerm,
    selection: VariationalSolverSelection,
    attainedGuarantee: Option[SolverGuarantee],
    stopping: SplitStoppingStatus,
    certificate: VariationalFrameCertificate,
    lowering: VariationalLoweringContract,
    provenance: SemanticProvenance
)

object VariationalFunctionalFrames:
  def alignedScorePenalty[
      Source <: SemanticSpace,
      Target <: SemanticSpace,
      Component <: SemanticSpace,
      ES <: OperatorEvidence,
      ET <: OperatorEvidence
  ](
      sourceProblem: ConvexFunctionalFrameProblem[Source, Component, ES],
      targetProblem: ConvexFunctionalFrameProblem[Target, Component, ET],
      alignedTarget: AlignedScoreTarget,
      term: PenaltyTerm,
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, AlignedVariationalFunctionalFrameFit[Source, Target, Component]] =
    if sourceProblem.variable.componentSpace.descriptor != targetProblem.variable.componentSpace.descriptor then
      Left(CompositeLoweringError.InvalidDefinition("aligned-score frames must share one typed component space"))
    else
      for
        _ <- validateAlignedTarget(
          sourceProblem.variable.id,
          targetProblem.variable.id,
          alignedTarget,
          term
        )
        sourceObservation <- sourceProblem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
        targetObservation <- targetProblem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
        compiled <- VariationalSolverCompiler.compileAlignedScorePenalty(
          alignedTarget,
          term,
          sourceObservation,
          targetObservation
        )
        solution <- compiled.solve(config)
        residual <- splitResidual(solution.status)
        provenance = (sourceProblem.anchor.weights.provenance ++ targetProblem.anchor.weights.provenance).append(
          SemanticProvenanceEvent.Derived(
            "aligned-score-primal-dual-functional-frames",
            Vector(
              sourceProblem.anchor.weights.valueIdentity,
              targetProblem.anchor.weights.valueIdentity
            ) ++ alignedTarget.map.descriptor.operatorIdentities
          )
        )
        sourceOperator <- Op
          .fromDense(
            solution.sourceParameter,
            sourceProblem.anchor.weights.domain,
            sourceProblem.anchor.weights.codomain,
            OperatorRoleWitness.frame,
            ValueIdentity.derived(
              "aligned-score-source-frame",
              sourceProblem.anchor.weights.valueIdentity,
              targetProblem.anchor.weights.valueIdentity
            ),
            provenance
          )
          .left
          .map(CompositeLoweringError.Semantic.apply)
        targetOperator <- Op
          .fromDense(
            solution.targetParameter,
            targetProblem.anchor.weights.domain,
            targetProblem.anchor.weights.codomain,
            OperatorRoleWitness.frame,
            ValueIdentity.derived(
              "aligned-score-target-frame",
              targetProblem.anchor.weights.valueIdentity,
              sourceProblem.anchor.weights.valueIdentity
            ),
            provenance
          )
          .left
          .map(CompositeLoweringError.Semantic.apply)
      yield
        AlignedVariationalFunctionalFrameFit(
          FunctionalFrame(sourceOperator, sourceProblem.anchor.cometric),
          FunctionalFrame(targetOperator, targetProblem.anchor.cometric),
          term,
          compiled.selection,
          solution.status match
            case SplitStoppingStatus.Converged(_) => Some(SolverGuarantee.GlobalConvexOptimum)
            case _ => None,
          solution.status,
          VariationalFrameCertificate(
            solution.certificate,
            residual.stationarity,
            residual.dualFeasibility,
            Some(residual.primalDualGap)
          ),
          loweringContract(term.symmetry, None),
          provenance
        )

  def directPenalty[
      Feature <: SemanticSpace,
      Coordinates <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      plan: DirectProximalPlan[Feature, Coordinates],
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    for
      _ <- validateTarget(problem.variable.id, plan.original.target)
      observation <- problem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
      compiled <- VariationalSolverCompiler.compileDirectPenalty(plan, observation)
      solution <- compiled.solve(config)
      fit <- firstOrderFit(
        problem,
        GaleMatrixBridge.toGale(solution.primal),
        VariationalFrameTerm.Penalty(plan.original),
        compiled.selection,
        solution,
        feasibility = 0.0,
        Some(plan.chart),
        "direct-proximal-functional-frame"
      )
    yield fit

  def directConstraint[
      Feature <: SemanticSpace,
      Coordinates <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      plan: DirectProjectionPlan[Feature, Coordinates],
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    for
      _ <- validateTarget(problem.variable.id, plan.original.target)
      observation <- problem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
      compiled <- VariationalSolverCompiler.compileDirectConstraint(plan, observation)
      solution <- compiled.solve(config)
      weights = GaleMatrixBridge.toGale(solution.primal)
      projected <- plan(weights).left.map(CompositeLoweringError.Chart.apply)
      fit <- firstOrderFit(
        problem,
        weights,
        VariationalFrameTerm.Constraint(plan.original),
        compiled.selection,
        solution,
        feasibility = maxAbs(MatrixOps.subtract(weights, projected)),
        Some(plan.chart),
        "direct-projected-functional-frame"
      )
    yield fit

  def compositePenalty[
      Feature <: SemanticSpace,
      Target <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      plan: CompositePenaltyPlan[Feature, Target],
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    for
      _ <- validateTarget(problem.variable.id, plan.original.target)
      observation <- problem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
      compiled <- VariationalSolverCompiler.compileCompositePenalty(plan, observation)
      solution <- compiled.solve(config)
      fit <- splitFit(
        problem,
        solution.parameter,
        VariationalFrameTerm.Penalty(plan.original),
        compiled.selection,
        solution.status,
        solution.numericalCertificate,
        None,
        "composite-primal-dual-functional-frame"
      )
    yield fit

  def overlappingGroups[
      Feature <: SemanticSpace,
      Coordinates <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      plan: CompositePenaltyPlan[Feature, Coordinates],
      chart: FeatureChart[Feature, Coordinates],
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    for
      _ <- validateTarget(problem.variable.id, plan.original.target)
      observation <- problem.anchor.weights.toDense.left.map(CompositeLoweringError.Semantic.apply)
      compiled <- VariationalSolverCompiler.compileOverlappingGroups(plan, chart, observation)
      solution <- compiled.solve(config)
      weights <- chart.synthesis(solution.parameter).left.map(CompositeLoweringError.Semantic.apply)
      fit <- splitFit(
        problem,
        weights,
        VariationalFrameTerm.Penalty(plan.original),
        compiled.selection,
        solution.status,
        solution.certificate,
        Some(chart),
        "lifted-overlapping-group-functional-frame"
      )
    yield fit

  private def firstOrderFit[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      weights: DMat,
      term: VariationalFrameTerm,
      selection: VariationalSolverSelection,
      solution: FirstOrderSolution,
      feasibility: Double,
      chart: Option[FeatureChart[? <: SemanticSpace, ? <: SemanticSpace]],
      method: String
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    val guarantee = solution.status match
      case FirstOrderStoppingStatus.Converged => Some(SolverGuarantee.GlobalConvexOptimum)
      case FirstOrderStoppingStatus.IterationLimit if term.isInstanceOf[VariationalFrameTerm.Constraint] && feasibility == 0.0 =>
        Some(SolverGuarantee.FeasiblePoint)
      case FirstOrderStoppingStatus.IterationLimit => None
    assemble(
      problem,
      weights,
      term,
      selection,
      guarantee,
      VariationalFrameStopping.FirstOrder(solution.status),
      VariationalFrameCertificate(
        solution.certificate,
        solution.certificate.primalResidual,
        feasibility,
        None
      ),
      loweringContract(termSymmetry(term), chart),
      method
    )

  private def splitFit[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      weights: DMat,
      term: VariationalFrameTerm,
      selection: VariationalSolverSelection,
      status: SplitStoppingStatus,
      numerical: FirstOrderCertificate,
      chart: Option[FeatureChart[? <: SemanticSpace, ? <: SemanticSpace]],
      method: String
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    val residual = status match
      case SplitStoppingStatus.Converged(value) => value
      case SplitStoppingStatus.IterationLimit(value) => value
      case SplitStoppingStatus.Infeasible(value) =>
        return Left(CompositeLoweringError.NumericalFailure(value.reason))
      case SplitStoppingStatus.NumericalFailure(reason) =>
        return Left(CompositeLoweringError.NumericalFailure(reason))
    val guarantee = status match
      case SplitStoppingStatus.Converged(_) => Some(SolverGuarantee.GlobalConvexOptimum)
      case _ => None
    assemble(
      problem,
      weights,
      term,
      selection,
      guarantee,
      VariationalFrameStopping.Split(status),
      VariationalFrameCertificate(
        numerical,
        residual.stationarity,
        residual.dualFeasibility,
        Some(residual.primalDualGap)
      ),
      loweringContract(termSymmetry(term), chart),
      method
    )

  private def assemble[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      E <: OperatorEvidence
  ](
      problem: ConvexFunctionalFrameProblem[Feature, Component, E],
      weights: DMat,
      term: VariationalFrameTerm,
      selection: VariationalSolverSelection,
      guarantee: Option[SolverGuarantee],
      stopping: VariationalFrameStopping,
      certificate: VariationalFrameCertificate,
      lowering: VariationalLoweringContract,
      method: String
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    val provenance = problem.anchor.weights.provenance.append(
      SemanticProvenanceEvent.Derived(method, Vector(problem.anchor.weights.valueIdentity))
    )
    Op
      .fromDense(
        weights,
        problem.anchor.weights.domain,
        problem.anchor.weights.codomain,
        OperatorRoleWitness.frame,
        ValueIdentity.derived(method, problem.anchor.weights.valueIdentity),
        provenance
      )
      .left
      .map(CompositeLoweringError.Semantic.apply)
      .map: operator =>
        VariationalFunctionalFrameFit(
          FunctionalFrame(operator, problem.anchor.cometric),
          term,
          selection,
          guarantee,
          stopping,
          certificate,
          lowering,
          provenance
        )

  private def validateTarget(
      parameter: ParameterId,
      target: TargetExpression
  ): Either[CompositeLoweringError, Unit] =
    if target.parameters == Vector(parameter) then Right(())
    else Left(CompositeLoweringError.InvalidDefinition("functional-frame term must bind exactly the supplied frame variable"))

  private def validateAlignedTarget(
      source: ParameterId,
      target: ParameterId,
      alignedTarget: AlignedScoreTarget,
      term: PenaltyTerm
  ): Either[CompositeLoweringError, Unit] =
    val parameters = Vector(source, target)
    if alignedTarget.erased.parameters != parameters then
      Left(CompositeLoweringError.InvalidDefinition("aligned-score target must bind the supplied source and target frames in order"))
    else if term.target != alignedTarget.erased then
      Left(CompositeLoweringError.InvalidDefinition("aligned-score penalty does not use the supplied typed score target"))
    else Right(())

  private def splitResidual(
      status: SplitStoppingStatus
  ): Either[CompositeLoweringError, SplitResidualCertificate] =
    status match
      case SplitStoppingStatus.Converged(value) => Right(value)
      case SplitStoppingStatus.IterationLimit(value) => Right(value)
      case SplitStoppingStatus.Infeasible(value) =>
        Left(CompositeLoweringError.NumericalFailure(value.reason))
      case SplitStoppingStatus.NumericalFailure(reason) =>
        Left(CompositeLoweringError.NumericalFailure(reason))

  private def termSymmetry(term: VariationalFrameTerm): FrameSymmetry =
    term match
      case VariationalFrameTerm.Penalty(value) => value.symmetry
      case VariationalFrameTerm.Constraint(value) => value.symmetry

  private def loweringContract(
      symmetry: FrameSymmetry,
      chart: Option[FeatureChart[? <: SemanticSpace, ? <: SemanticSpace]]
  ): VariationalLoweringContract =
    VariationalLoweringContract(
      ParameterizationKind.Identity,
      chart.map(_.kind),
      chart.map(_.valueIdentity),
      chart.flatMap(_.lawCertificate),
      symmetry,
      ParameterizationGauge.Unique,
      ResultEquivalence.ValueEquivalent(CertificateTolerance.strict)
    )

  private def maxAbs(value: DMat): Double =
    var result = 0.0
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        result = Math.max(result, Math.abs(value(row, column)))
        column += 1
      row += 1
    result
