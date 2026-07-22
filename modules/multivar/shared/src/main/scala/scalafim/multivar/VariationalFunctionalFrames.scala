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
    primalDualGap: Option[Double],
    distanceToMinimizer: Option[Double]
):
  require(stationarityResidual.isFinite && stationarityResidual >= 0.0)
  require(feasibilityResidual.isFinite && feasibilityResidual >= 0.0)
  require(primalDualGap.forall(value => value.isFinite && value >= 0.0))
  require(distanceToMinimizer.forall(value => value.isFinite && value >= 0.0))

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
    achievement: AchievedOptimizationGuarantee,
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
    achievement: AchievedOptimizationGuarantee,
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
        certificate = VariationalFrameCertificate(
          solution.certificate,
          residual.stationarity,
          residual.dualFeasibility,
          Some(residual.primalDualGap),
          None
        )
        achievement <- admitAchievement(
          Vector(sourceProblem.anchor.weights.valueIdentity, targetProblem.anchor.weights.valueIdentity),
          Vector(sourceProblem.variable.id, targetProblem.variable.id),
          Vector(sourceOperator.valueIdentity, targetOperator.valueIdentity),
          VariationalFrameTerm.Penalty(term),
          VariationalFrameStopping.Split(solution.status),
          certificate,
          "aligned-score-primal-dual-functional-frames"
        )
      yield
        AlignedVariationalFunctionalFrameFit(
          FunctionalFrame(sourceOperator, sourceProblem.anchor.cometric),
          FunctionalFrame(targetOperator, targetProblem.anchor.cometric),
          term,
          compiled.selection,
          achievement,
          solution.status,
          certificate,
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
      exact <- plan(observation, PenaltyWeight.unsafe(1.0)).left.map(CompositeLoweringError.Chart.apply)
      weights = GaleMatrixBridge.toGale(solution.primal)
      fit <- firstOrderFit(
        problem,
        weights,
        VariationalFrameTerm.Penalty(plan.original),
        compiled.selection,
        solution,
        feasibility = 0.0,
        distanceToMinimizer = maxAbs(MatrixOps.subtract(weights, exact)),
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
      exact <- plan(observation).left.map(CompositeLoweringError.Chart.apply)
      fit <- firstOrderFit(
        problem,
        weights,
        VariationalFrameTerm.Constraint(plan.original),
        compiled.selection,
        solution,
        feasibility = maxAbs(MatrixOps.subtract(weights, projected)),
        distanceToMinimizer = maxAbs(MatrixOps.subtract(weights, exact)),
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
      distanceToMinimizer: Double,
      chart: Option[FeatureChart[? <: SemanticSpace, ? <: SemanticSpace]],
      method: String
  ): Either[CompositeLoweringError, VariationalFunctionalFrameFit[Feature, Component]] =
    assemble(
      problem,
      weights,
      term,
      selection,
      VariationalFrameStopping.FirstOrder(solution.status),
      VariationalFrameCertificate(
        solution.certificate,
        solution.certificate.primalResidual,
        feasibility,
        None,
        Some(distanceToMinimizer)
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
    assemble(
      problem,
      weights,
      term,
      selection,
      VariationalFrameStopping.Split(status),
      VariationalFrameCertificate(
        numerical,
        residual.stationarity,
        residual.dualFeasibility,
        Some(residual.primalDualGap),
        None
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
      .flatMap: operator =>
        admitAchievement(
          Vector(problem.anchor.weights.valueIdentity),
          Vector(problem.variable.id),
          Vector(operator.valueIdentity),
          term,
          stopping,
          certificate,
          method
        ).map: achievement =>
          VariationalFunctionalFrameFit(
            FunctionalFrame(operator, problem.anchor.cometric),
            term,
            selection,
            achievement,
            stopping,
            certificate,
            lowering,
            provenance
          )

  private def admitAchievement(
      anchors: Vector[ValueIdentity],
      parameters: Vector[ParameterId],
      results: Vector[ValueIdentity],
      term: VariationalFrameTerm,
      stopping: VariationalFrameStopping,
      certificate: VariationalFrameCertificate,
      method: String
  ): Either[CompositeLoweringError, AchievedOptimizationGuarantee] =
    val contract = MathematicalContractCatalog.anchorRegularizedFrame
    val target = term match
      case VariationalFrameTerm.Penalty(value) => value.target
      case VariationalFrameTerm.Constraint(value) => value.target
    val programIdentity = ValueIdentity.Derived(s"$method-program", anchors ++ target.operators)
    val oracleKind = term match
      case VariationalFrameTerm.Penalty(_) => ExactOracleKind.Proximal
      case VariationalFrameTerm.Constraint(_) => ExactOracleKind.Projection
    val oracleIdentity = ValueIdentity.derived(s"exact-${oracleKind.toString.toLowerCase}-law", programIdentity)
    val operatorIdentities = (target.operators :+ oracleIdentity).distinct
    val resultIdentity = ValueIdentity.Derived(s"$method-result", results)
    val termination = stopping match
      case VariationalFrameStopping.FirstOrder(FirstOrderStoppingStatus.Converged) => NumericalTermination.Converged
      case VariationalFrameStopping.FirstOrder(FirstOrderStoppingStatus.IterationLimit) => NumericalTermination.IterationLimit
      case VariationalFrameStopping.Split(SplitStoppingStatus.Converged(_)) => NumericalTermination.Converged
      case VariationalFrameStopping.Split(SplitStoppingStatus.IterationLimit(_)) => NumericalTermination.IterationLimit
      case VariationalFrameStopping.Split(SplitStoppingStatus.Infeasible(_)) => NumericalTermination.Infeasible
      case VariationalFrameStopping.Split(SplitStoppingStatus.NumericalFailure(_)) => NumericalTermination.NumericalFailure
    val claim = stopping match
      case VariationalFrameStopping.FirstOrder(FirstOrderStoppingStatus.Converged) =>
        OptimizationClaimClass.UniqueMinimizerWithinBound
      case VariationalFrameStopping.FirstOrder(FirstOrderStoppingStatus.IterationLimit)
          if term.isInstanceOf[VariationalFrameTerm.Constraint] &&
            certificate.feasibilityResidual <= certificate.numerical.settings.tolerance.threshold(1.0) =>
        OptimizationClaimClass.Feasible
      case VariationalFrameStopping.Split(SplitStoppingStatus.Converged(_)) =>
        OptimizationClaimClass.EpsilonGlobal
      case _ => OptimizationClaimClass.Unresolved
    val global = claim.isGlobal
    for
      bindings <- OptimizationIdentityBindings
        .from(
          contract.id,
          programIdentity,
          ValueIdentity.Derived("anchor-observations", anchors),
          ObservationMaskIdentity.Complete,
          operatorIdentities,
          parameters,
          resultIdentity
        )
        .left
        .map(proofError)
      convex <- ProperClosedConvexWitness
        .from(bindings, programIdentity, proofAssumption("proper-closed-convex-penalty"))
        .left
        .map(proofError)
      smooth <- SmoothnessWitness
        .from(
          bindings,
          programIdentity,
          PositiveProofConstant.unsafeSmoothness(1.0),
          proofAssumption("strongly-convex-anchor")
        )
        .left
        .map(proofError)
      strong <- StrongConvexityWitness
        .from(
          bindings,
          programIdentity,
          PositiveProofConstant.unsafeStrongConvexity(1.0),
          proofAssumption("strongly-convex-anchor")
        )
        .left
        .map(proofError)
      exact <- ExactOracleLawWitness
        .from(bindings, oracleIdentity, oracleKind, proofAssumption("certified-prox-or-splitting"))
        .left
        .map(proofError)
      assumptions <- OptimizationAssumptions
        .from(
          bindings,
          properClosedConvex = Vector(convex),
          smoothness = Vector(smooth),
          strongConvexity = Vector(strong),
          exactOracleLaws = Vector(exact)
        )
        .left
        .map(proofError)
      stationarity <- NonNegativeProofBound.residual(certificate.stationarityResidual).left.map(proofError)
      feasibility <- NonNegativeProofBound.residual(certificate.feasibilityResidual).left.map(proofError)
      gap <- certificate.primalDualGap match
        case Some(value) => NonNegativeProofBound.objectiveGap(value).map(Some(_)).left.map(proofError)
        case None => Right(None)
      distance <- certificate.distanceToMinimizer match
        case Some(value) => NonNegativeProofBound.distance(value).map(Some(_)).left.map(proofError)
        case None => Right(None)
      evidence <- SemanticOptimizationEvidence
        .from(
          bindings,
          termination,
          stationarity = Some(stationarity),
          feasibility = Some(feasibility),
          objectiveGap = gap,
          distanceToMinimizer = distance
        )
        .left
        .map(proofError)
      witness <-
        if global then
          GlobalOptimalityWitness
            .from(
              bindings,
              proofTheorem("strongly-convex-anchor-composite"),
              assumptions.assumptionReferences,
              OracleFamily.Analytic
            )
            .map(Some(_))
            .left
            .map(proofError)
        else Right(None)
      achievement <- OptimizationGuaranteeAdmission
        .admit(
          contract,
          claim,
          assumptions,
          if global then
            Set(
              OptimizationProofObligation.Smooth(programIdentity),
              oracleKind match
                case ExactOracleKind.Proximal => OptimizationProofObligation.ExactProximal(oracleIdentity)
                case ExactOracleKind.Projection => OptimizationProofObligation.ExactProjection(oracleIdentity)
            )
          else Set.empty,
          evidence,
          witness
        )
        .left
        .map(proofError)
    yield achievement

  private def proofAssumption(value: String): ContractReference[AssumptionReference] =
    ContractReference.unsafeAssumption(value)

  private def proofTheorem(value: String): ContractReference[TheoremReference] =
    ContractReference.unsafeTheorem(value)

  private def proofError(error: OptimizationGuaranteeError): CompositeLoweringError =
    CompositeLoweringError.InvalidDefinition(s"optimization proof rejected: ${error.message}")

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
