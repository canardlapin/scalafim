package scalafim.multivar

import gale.linalg.DMat
import scalafim.linalg.BoundedLinearMap
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.FirstOrderCapabilities
import scalafim.linalg.FirstOrderCertificate
import scalafim.linalg.FirstOrderConfig
import scalafim.linalg.FirstOrderError
import scalafim.linalg.FirstOrderSolvers
import scalafim.linalg.FirstOrderStoppingStatus
import scalafim.linalg.FirstOrderTolerance
import scalafim.linalg.GaleMatrixBridge
import scalafim.linalg.LinearCompositeFunctional
import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapError
import scalafim.linalg.ProximalTerm
import scalafim.linalg.SmoothObjective
import scalafim.linalg.SolverMethodRequest

enum CompositePenaltyCompileError:
  case InvalidDefinition(detail: String)
  case UnsupportedProximalSum(count: Int)
  case UnsupportedSplitFunctional(actual: FunctionalKind)
  case NonObjectiveQuadratic(actual: QuadraticPlacement)
  case NonFiniteInput(row: Int, column: Int, value: Double)
  case Semantic(error: SemanticError)
  case Chart(error: ChartError)
  case Solver(error: FirstOrderError)
  case Guarantee(error: OptimizationGuaranteeError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case UnsupportedProximalSum(count) =>
        s"$count direct proximal terms require an exact combined-prox proof; at most one is currently admitted"
      case UnsupportedSplitFunctional(actual) =>
        s"split composite compilation currently requires an l1/TV functional, got $actual"
      case NonObjectiveQuadratic(actual) =>
        s"sparse-smooth fitting requires objective smoothness, not $actual"
      case NonFiniteInput(row, column, value) => s"anchor value ($row,$column) is not finite: $value"
      case Semantic(error) => error.message
      case Chart(error) => error.message
      case Solver(error) => error.message
      case Guarantee(error) => error.message

enum NormBoundDerivation:
  case ColumnwiseFrobenius
  case StackedRootSumSquares

final case class DerivedOperatorNormBound private (
    operator: ValueIdentity,
    upperBound: CertifiedOperatorNormBound,
    derivation: NormBoundDerivation,
    evaluatedColumns: Int
)

object DerivedOperatorNormBound:
  private[multivar] def fromOperator[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      operator: Op[From, To, R, E]
  ): Either[CompositePenaltyCompileError, DerivedOperatorNormBound] =
    var squared = 0.0
    var column = 0
    var failure = Option.empty[CompositePenaltyCompileError]
    while column < operator.cols && failure.isEmpty do
      val basis = new Array[Double](operator.cols)
      basis(column) = 1.0
      val vector = GaleNumerics.matrixFromRowMajor(operator.cols, 1, basis)
      operator(vector) match
        case Left(error) => failure = Some(CompositePenaltyCompileError.Semantic(error))
        case Right(image) =>
          var row = 0
          while row < image.rows do
            val value = image(row, 0)
            if !value.isFinite then
              failure = Some(CompositePenaltyCompileError.InvalidDefinition("operator norm derivation produced a non-finite value"))
            else squared += value * value
            row += 1
      column += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        NonNegativeProofBound
          .operatorNorm(Math.sqrt(squared))
          .left
          .map(CompositePenaltyCompileError.Guarantee.apply)
          .map: bound =>
            DerivedOperatorNormBound(
              operator.valueIdentity,
              bound,
              NormBoundDerivation.ColumnwiseFrobenius,
              operator.cols
            )

final case class StackedOperatorNormBound private (
    operators: Vector[ValueIdentity],
    upperBound: CertifiedOperatorNormBound,
    derivation: NormBoundDerivation
)

object StackedOperatorNormBound:
  private[multivar] def from(
      terms: Vector[DerivedOperatorNormBound]
  ): Either[CompositePenaltyCompileError, StackedOperatorNormBound] =
    val squared = terms.foldLeft(0.0): (total, term) =>
      val bound = term.upperBound.doubleValue
      total + bound * bound
    NonNegativeProofBound
      .operatorNorm(Math.sqrt(squared))
      .left
      .map(CompositePenaltyCompileError.Guarantee.apply)
      .map: bound =>
        StackedOperatorNormBound(
          terms.map(_.operator),
          bound,
          NormBoundDerivation.StackedRootSumSquares
        )

/** Strongly convex quadratic anchor for one convex block problem.
  *
  * The Euclidean constructor keeps the ordinary refinement objective. The
  * metric constructor represents `0.5 * (x-a)' Q (x-a)` for a certified SPD
  * cometric `Q`; both the smoothness upper bound and strong-convexity modulus
  * are taken from proof-bearing operator evidence.
  */
final class StronglyConvexAnchor[Feature <: SemanticSpace] private (
    val point: DMat,
    val valueIdentity: ValueIdentity,
    val geometryIdentity: Option[ValueIdentity],
    val lipschitzBound: CertifiedOperatorNormBound,
    val strongConvexity: StrongConvexityModulus,
    private val applyGeometry: DMat => Either[CompositePenaltyCompileError, DMat]
):
  private[multivar] def value(at: DMat): Either[CompositePenaltyCompileError, Double] =
    val residual = MatrixOps.subtract(at, point)
    applyGeometry(residual).map(image => 0.5 * inner(residual, image))

  private[multivar] def gradient(at: DMat): Either[CompositePenaltyCompileError, DMat] =
    applyGeometry(MatrixOps.subtract(at, point))

object StronglyConvexAnchor:
  def euclidean[Feature <: SemanticSpace](
      point: DMat,
      valueIdentity: ValueIdentity
  ): StronglyConvexAnchor[Feature] =
    new StronglyConvexAnchor(
      point,
      valueIdentity,
      None,
      PositiveProofConstant.unsafeOperatorNorm(1.0),
      PositiveProofConstant.unsafeStrongConvexity(1.0),
      Right.apply
    )

  def metric[Feature <: SemanticSpace](
      point: DMat,
      valueIdentity: ValueIdentity,
      geometry: OpMetric[Feature, CertifiedSpd]
  ): Either[CompositePenaltyCompileError, StronglyConvexAnchor[Feature]] =
    if geometry.rows != point.rows || geometry.cols != point.rows then
      Left(
        CompositePenaltyCompileError.InvalidDefinition(
          s"anchor geometry dimension ${geometry.rows} does not match anchor rows ${point.rows}"
        )
      )
    else
      for
        norm <- DerivedOperatorNormBound.fromOperator(geometry)
        minimumOption = geometry.certificate.claims.collectFirst:
          case NumericalCertificate(_, CertificateClaim.PositiveDefinite(value, _, _), _) if value > 0.0 => value
        minimum <- minimumOption.toRight(
          CompositePenaltyCompileError.InvalidDefinition(
            "metric anchor requires a positive-definite certificate with a positive minimum eigenvalue"
          )
        )
        modulus <- PositiveProofConstant
          .strongConvexity(minimum)
          .left
          .map(CompositePenaltyCompileError.Guarantee.apply)
      yield
        new StronglyConvexAnchor(
          point,
          valueIdentity,
          Some(geometry.valueIdentity),
          norm.upperBound,
          modulus,
          at => geometry(at).left.map(CompositePenaltyCompileError.Semantic.apply)
        )

final class SmoothQuadraticPenalty[Feature <: SemanticSpace] private (
    val lowering: QuadraticLowering[Feature],
    val normBound: DerivedOperatorNormBound
):
  def identity: ValueIdentity = lowering.pulledBack.valueIdentity
  def weight: Double = lowering.original.weight.value

object SmoothQuadraticPenalty:
  def from[Feature <: SemanticSpace](
      lowering: QuadraticLowering[Feature]
  ): Either[CompositePenaltyCompileError, SmoothQuadraticPenalty[Feature]] =
    if lowering.placement != QuadraticPlacement.ObjectiveRidge then
      Left(CompositePenaltyCompileError.NonObjectiveQuadratic(lowering.placement))
    else
      DerivedOperatorNormBound
        .fromOperator(lowering.pulledBack)
        .map(new SmoothQuadraticPenalty(lowering, _))

final class ExactDirectPenalty[Feature <: SemanticSpace] private (
    val original: PenaltyTerm,
    val featureDimension: Int,
    val identity: ValueIdentity,
    private val valueAt: DMat => Either[CompositePenaltyCompileError, Double],
    private val proximalAt: (DMat, Double) => Either[CompositePenaltyCompileError, DMat]
):
  private[multivar] def value(at: DMat): Either[CompositePenaltyCompileError, Double] = valueAt(at)
  private[multivar] def proximal(at: DMat, step: Double): Either[CompositePenaltyCompileError, DMat] =
    proximalAt(at, step)

object ExactDirectPenalty:
  def from[Feature <: SemanticSpace, Coordinates <: SemanticSpace](
      plan: DirectProximalPlan[Feature, Coordinates]
  ): ExactDirectPenalty[Feature] =
    new ExactDirectPenalty(
      plan.original,
      plan.chart.featureSpace.dimension,
      ValueIdentity.derived("exact-direct-proximal", plan.chart.valueIdentity),
      at =>
        plan.chart
          .forward(at)
          .left
          .map(CompositePenaltyCompileError.Semantic.apply)
          .map(coordinates => directPenaltyValue(plan.kind, coordinates, plan.original.weight.value)),
      (at, step) =>
        if !step.isFinite || step <= 0.0 then
          Left(CompositePenaltyCompileError.InvalidDefinition(s"proximal step must be finite and positive, got $step"))
        else
          plan(at, PenaltyWeight.unsafe(step))
            .left
            .map(CompositePenaltyCompileError.Chart.apply)
    )

final class SplitL1Penalty[Feature <: SemanticSpace] private (
    val original: PenaltyTerm,
    val rows: Int,
    val columns: Int,
    val identity: ValueIdentity,
    val normBound: DerivedOperatorNormBound,
    private val forwardAt: DMat => Either[CompositePenaltyCompileError, DMat],
    private val adjointAt: DMat => Either[CompositePenaltyCompileError, DMat]
):
  def weight: Double = original.weight.value
  private[multivar] def forward(at: DMat): Either[CompositePenaltyCompileError, DMat] = forwardAt(at)
  private[multivar] def adjoint(at: DMat): Either[CompositePenaltyCompileError, DMat] = adjointAt(at)

object SplitL1Penalty:
  def from[Feature <: SemanticSpace, Target <: SemanticSpace](
      plan: CompositePenaltyPlan[Feature, Target]
  ): Either[CompositePenaltyCompileError, SplitL1Penalty[Feature]] =
    if plan.functional != CompositeFunctional.ElementwiseL1 then
      Left(CompositePenaltyCompileError.UnsupportedSplitFunctional(plan.original.functional))
    else
      DerivedOperatorNormBound.fromOperator(plan.targetOperator).map: bound =>
        new SplitL1Penalty(
          plan.original,
          plan.targetOperator.rows,
          plan.targetOperator.cols,
          plan.targetOperator.valueIdentity,
          bound,
          at => plan.targetOperator(at).left.map(CompositePenaltyCompileError.Semantic.apply),
          at => plan.targetOperator.dual(at).left.map(CompositePenaltyCompileError.Semantic.apply)
        )

final class CompositeSparseSmoothProgram[Feature <: SemanticSpace] private (
    val parameter: ParameterId,
    val anchorQuadratic: StronglyConvexAnchor[Feature],
    val smooth: Vector[SmoothQuadraticPenalty[Feature]],
    val direct: Option[ExactDirectPenalty[Feature]],
    val split: Vector[SplitL1Penalty[Feature]],
    val provenance: SemanticProvenance
):
  def anchor: DMat = anchorQuadratic.point
  def anchorIdentity: ValueIdentity = anchorQuadratic.valueIdentity

  def compile(
      request: SolverMethodRequest = SolverMethodRequest.Automatic,
      capabilities: FirstOrderCapabilities = FirstOrderCapabilities.portable
  ): Either[CompositePenaltyCompileError, CompiledCompositeSparseSmooth[Feature]] =
    val form =
      if split.nonEmpty then VariationalExecutionForm.SmoothLinearComposite
      else VariationalExecutionForm.SmoothSeparableProximal
    for
      selection <- VariationalSolverCompiler
        .select(form, request, capabilities)
        .left
        .map:
          case CompositeLoweringError.SolverBoundary(error) => CompositePenaltyCompileError.Solver(error)
          case other => CompositePenaltyCompileError.InvalidDefinition(other.message)
      stacked <- StackedSplitOperator.from(anchor.rows, split)
    yield
      new CompiledCompositeSparseSmooth(
        this,
        selection,
        new CompositeSmoothObjective(anchorQuadratic, smooth),
        direct.fold[ProximalTerm](new ZeroDirectTerm(anchor.rows))(new DirectTermAdapter(_)),
        stacked
      )

object CompositeSparseSmoothProgram:
  def from[Feature <: SemanticSpace](
      parameter: ParameterId,
      anchor: DMat,
      anchorIdentity: ValueIdentity,
      smooth: Vector[SmoothQuadraticPenalty[Feature]],
      direct: Vector[ExactDirectPenalty[Feature]],
      split: Vector[SplitL1Penalty[Feature]],
      provenance: SemanticProvenance = SemanticProvenance.source("composite-sparse-smooth-program")
  ): Either[CompositePenaltyCompileError, CompositeSparseSmoothProgram[Feature]] =
    fromAnchor(
      parameter,
      StronglyConvexAnchor.euclidean(anchor, anchorIdentity),
      smooth,
      direct,
      split,
      provenance
    )

  def fromAnchor[Feature <: SemanticSpace](
      parameter: ParameterId,
      anchor: StronglyConvexAnchor[Feature],
      smooth: Vector[SmoothQuadraticPenalty[Feature]],
      direct: Vector[ExactDirectPenalty[Feature]],
      split: Vector[SplitL1Penalty[Feature]],
      provenance: SemanticProvenance = SemanticProvenance.source("composite-sparse-smooth-program")
  ): Either[CompositePenaltyCompileError, CompositeSparseSmoothProgram[Feature]] =
    val point = anchor.point
    if point.rows <= 0 || point.cols <= 0 then
      Left(CompositePenaltyCompileError.InvalidDefinition("anchor must have positive rows and columns"))
    else if direct.length > 1 then Left(CompositePenaltyCompileError.UnsupportedProximalSum(direct.length))
    else
      firstNonFinite(point) match
        case Some((row, column, value)) => Left(CompositePenaltyCompileError.NonFiniteInput(row, column, value))
        case None =>
          val terms = smooth.map(_.lowering.original) ++ direct.map(_.original) ++ split.map(_.original)
          val wrongParameter = terms.find(_.target.parameters != Vector(parameter))
          val wrongDimension = direct.exists(_.featureDimension != point.rows) || split.exists(_.columns != point.rows)
          val identities = smooth.map(_.identity) ++ direct.map(_.identity) ++ split.map(_.identity)
          if wrongParameter.nonEmpty then
            Left(CompositePenaltyCompileError.InvalidDefinition("every penalty must bind exactly the declared frame parameter"))
          else if wrongDimension then
            Left(CompositePenaltyCompileError.InvalidDefinition("every penalty operator must share the anchor feature dimension"))
          else if identities.distinct.length != identities.length then
            Left(CompositePenaltyCompileError.InvalidDefinition("composite penalty identities must be distinct"))
          else
            Right(
              new CompositeSparseSmoothProgram(
                parameter,
                anchor,
                smooth,
                direct.headOption,
                split,
                provenance
              )
            )

final case class CompositeObjectiveBreakdown(
    anchor: Double,
    smooth: Vector[(ValueIdentity, Double)],
    direct: Option[(ValueIdentity, Double)],
    split: Vector[(ValueIdentity, Double)]
):
  def total: Double =
    anchor + smooth.map(_._2).sum + direct.fold(0.0)(_._2) + split.map(_._2).sum

final case class CompositeSparseSmoothCertificate(
    numerical: FirstOrderCertificate,
    objective: CompositeObjectiveBreakdown,
    objectiveAgreement: Double,
    stationarityResidual: Double,
    dualFeasibilityResidual: Double,
    primalDualGap: Double,
    smoothLipschitzBound: Double,
    splitOperatorNorm: StackedOperatorNormBound
):
  require(objectiveAgreement.isFinite && objectiveAgreement >= 0.0)
  require(stationarityResidual.isFinite && stationarityResidual >= 0.0)
  require(dualFeasibilityResidual.isFinite && dualFeasibilityResidual >= 0.0)
  require(primalDualGap.isFinite && primalDualGap >= 0.0)
  require(smoothLipschitzBound.isFinite && smoothLipschitzBound > 0.0)

  def splitNormBound: Double = splitOperatorNorm.upperBound.doubleValue

final case class CompositeSparseSmoothFit(
    parameter: DMat,
    dual: Option[DMat],
    selection: VariationalSolverSelection,
    certificate: CompositeSparseSmoothCertificate,
    achievement: AchievedOptimizationGuarantee,
    resultIdentity: ValueIdentity,
    provenance: SemanticProvenance
)

final class CompiledCompositeSparseSmooth[Feature <: SemanticSpace] private[multivar] (
    val program: CompositeSparseSmoothProgram[Feature],
    val selection: VariationalSolverSelection,
    private val smoothObjective: CompositeSmoothObjective[Feature],
    private val directTerm: ProximalTerm,
    private val stacked: StackedSplitOperator[Feature]
):
  def solve(
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositePenaltyCompileError, CompositeSparseSmoothFit] =
    for
      numericalConfig <- compositeFirstOrderConfig(config)
      solution <-
        val attempted =
          if program.split.isEmpty then
            FirstOrderSolvers.proximalGradient(
              smoothObjective,
              directTerm,
              GaleMatrixBridge.fromGale(program.anchor),
              numericalConfig
            )
          else
            FirstOrderSolvers.smoothCompositePrimalDual(
              smoothObjective,
              directTerm,
              stacked.functional,
              stacked.bounded,
              GaleMatrixBridge.fromGale(program.anchor),
              numericalConfig
            )
        attempted.left.map(CompositePenaltyCompileError.Solver.apply)
      parameter = GaleMatrixBridge.toGale(solution.primal)
      dual = solution.dual.map(GaleMatrixBridge.toGale)
      certificate <- recomputeCertificate(parameter, dual, solution, numericalConfig)
      resultIdentity = ValueIdentity.derived(
        "composite-sparse-smooth-fit",
        (program.anchorIdentity +: termIdentities)*
      )
      achievement <- admitAchievement(resultIdentity, certificate, solution.status)
      provenance = program.provenance.append(
        SemanticProvenanceEvent.Derived("solve-composite-sparse-smooth", termIdentities)
      )
    yield CompositeSparseSmoothFit(parameter, dual, selection, certificate, achievement, resultIdentity, provenance)

  private def recomputeCertificate(
      parameter: DMat,
      dual: Option[DMat],
      solution: scalafim.linalg.FirstOrderSolution,
      config: FirstOrderConfig
  ): Either[CompositePenaltyCompileError, CompositeSparseSmoothCertificate] =
    for
      breakdown <- objectiveBreakdown(parameter)
      residuals <- fixedPointResiduals(parameter, dual, config)
      gap <- certifiedGap(parameter, dual, breakdown.total, config)
    yield
      CompositeSparseSmoothCertificate(
        solution.certificate,
        breakdown,
        Math.abs(breakdown.total - solution.objective),
        residuals._1,
        residuals._2,
        gap,
        smoothObjective.lipschitz,
        stacked.normBound
      )

  private def objectiveBreakdown(
      parameter: DMat
  ): Either[CompositePenaltyCompileError, CompositeObjectiveBreakdown] =
    for
      anchorValue <- program.anchorQuadratic.value(parameter)
      smoothValues <- traverse(program.smooth): term =>
        term.lowering.pulledBack(parameter)
          .left
          .map(CompositePenaltyCompileError.Semantic.apply)
          .map(image => term.identity -> (term.weight * inner(parameter, image)))
      directValue <- program.direct match
        case Some(term) => term.value(parameter).map(value => Some(term.identity -> value))
        case None => Right(None)
      splitValues <- traverse(program.split): term =>
        term.forward(parameter).map(value => term.identity -> (term.weight * l1(value)))
    yield
      CompositeObjectiveBreakdown(
        anchorValue,
        smoothValues,
        directValue,
        splitValues
      )

  private def fixedPointResiduals(
      parameter: DMat,
      dual: Option[DMat],
      config: FirstOrderConfig
  ): Either[CompositePenaltyCompileError, (Double, Double)] =
    val primalStep =
      if program.split.isEmpty then config.stepSafety / smoothObjective.lipschitz
      else config.stepSafety / (smoothObjective.lipschitz + stacked.normBound.upperBound.doubleValue)
    for
      gradient <- smoothObjective.gradient(GaleMatrixBridge.fromGale(parameter))
        .left
        .map(CompositePenaltyCompileError.Solver.apply)
      adjoint <- dual match
        case Some(value) => stacked.adjoint(value)
        case None => Right(DMat.zeros(parameter.rows, parameter.cols))
      candidate = MatrixOps.subtract(
        parameter,
        MatrixOps.scale(MatrixOps.subtract(GaleMatrixBridge.toGale(gradient), MatrixOps.scale(adjoint, -1.0)), primalStep)
      )
      fixed <- program.direct match
        case Some(term) => term.proximal(candidate, primalStep)
        case None => Right(candidate)
      dualResidual <- dual match
        case Some(value) => stacked.dualResidual(parameter, value, config.stepSafety)
        case None => Right(0.0)
    yield compositeMaxAbs(MatrixOps.subtract(parameter, fixed)) / primalStep -> dualResidual

  private def certifiedGap(
      parameter: DMat,
      dual: Option[DMat],
      primalObjective: Double,
      config: FirstOrderConfig
  ): Either[CompositePenaltyCompileError, Double] =
    val dualValue = dual.getOrElse(DMat.zeros(stacked.rows, parameter.cols))
    for
      adjoint <- stacked.adjoint(dualValue)
      conjugateUpper <- qConjugateUpper(MatrixOps.scale(adjoint, -1.0), parameter, config)
      dualFeasibility = stacked.dualFeasibility(dualValue)
      _ <-
        if dualFeasibility.isFinite then Right(())
        else Left(CompositePenaltyCompileError.InvalidDefinition("dual feasibility is non-finite"))
      dualLower = if dualFeasibility == 0.0 then -conjugateUpper else Double.NegativeInfinity
    yield if dualLower.isFinite then Math.max(0.0, primalObjective - dualLower) else Double.MaxValue

  private def qConjugateUpper(
      argument: DMat,
      initial: DMat,
      config: FirstOrderConfig
  ): Either[CompositePenaltyCompileError, Double] =
    val tilted = new TiltedSmoothObjective(smoothObjective, argument)
    for
      solution <- FirstOrderSolvers
        .proximalGradient(tilted, directTerm, GaleMatrixBridge.fromGale(initial), config)
        .left
        .map(CompositePenaltyCompileError.Solver.apply)
      candidate = GaleMatrixBridge.toGale(solution.primal)
      step = config.stepSafety / smoothObjective.lipschitz
      baseGradient <- smoothObjective.gradient(solution.primal).left.map(CompositePenaltyCompileError.Solver.apply)
      shifted = MatrixOps.subtract(GaleMatrixBridge.toGale(baseGradient), argument)
      proximalInput = MatrixOps.subtract(candidate, MatrixOps.scale(shifted, step))
      point <- program.direct match
        case Some(term) => term.proximal(proximalInput, step)
        case None => Right(proximalInput)
      pointGradient <- smoothObjective
        .gradient(GaleMatrixBridge.fromGale(point))
        .left
        .map(CompositePenaltyCompileError.Solver.apply)
      directSubgradient = MatrixOps.scale(MatrixOps.subtract(proximalInput, point), 1.0 / step)
      residual = MatrixOps.subtract(
        MatrixOps.subtract(GaleMatrixBridge.toGale(pointGradient), argument),
        MatrixOps.scale(directSubgradient, -1.0)
      )
      qValue <- qValue(point)
      lower = inner(argument, point) - qValue
    yield
      lower + squaredNorm(residual) /
        (2.0 * program.anchorQuadratic.strongConvexity.doubleValue)

  private def qValue(parameter: DMat): Either[CompositePenaltyCompileError, Double] =
    for
      smooth <- smoothObjective.value(GaleMatrixBridge.fromGale(parameter)).left.map(CompositePenaltyCompileError.Solver.apply)
      direct <- program.direct match
        case Some(term) => term.value(parameter)
        case None => Right(0.0)
    yield smooth + direct

  private def admitAchievement(
      resultIdentity: ValueIdentity,
      certificate: CompositeSparseSmoothCertificate,
      status: FirstOrderStoppingStatus
  ): Either[CompositePenaltyCompileError, AchievedOptimizationGuarantee] =
    val contract = MathematicalContractCatalog.anchorRegularizedFrame
    val programIdentity = ValueIdentity.Derived("composite-sparse-smooth-program", program.anchorIdentity +: termIdentities)
    val oracleIdentity = ValueIdentity.derived("composite-sparse-smooth-exact-oracles", programIdentity)
    val operators = (program.anchorQuadratic.geometryIdentity.toVector ++ termIdentities :+ oracleIdentity).distinct
    for
      bindings <- OptimizationIdentityBindings
        .from(
          contract.id,
          programIdentity,
          program.anchorIdentity,
          ObservationMaskIdentity.Complete,
          operators,
          Vector(program.parameter),
          resultIdentity
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      convex <- ProperClosedConvexWitness
        .from(bindings, programIdentity, ContractReference.unsafeAssumption("proper-closed-convex-penalty"))
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      smooth <- SmoothnessWitness
        .from(
          bindings,
          programIdentity,
          PositiveProofConstant.unsafeSmoothness(certificate.smoothLipschitzBound),
          ContractReference.unsafeAssumption("strongly-convex-anchor")
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      strong <- StrongConvexityWitness
        .from(
          bindings,
          programIdentity,
          program.anchorQuadratic.strongConvexity,
          ContractReference.unsafeAssumption("strongly-convex-anchor")
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      exact <- ExactOracleLawWitness
        .from(
          bindings,
          oracleIdentity,
          ExactOracleKind.Proximal,
          ContractReference.unsafeAssumption("certified-prox-or-splitting")
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      normWitnesses <- traverse(
        program.anchorQuadratic.geometryIdentity.toVector.map(
          identity => identity -> program.anchorQuadratic.lipschitzBound
        ) ++ program.smooth.map(term => term.identity -> term.normBound.upperBound) ++
          program.split.map(term => term.identity -> term.normBound.upperBound)
      ): (identity, bound) =>
        OperatorNormWitness
          .from(
            bindings,
            identity,
            bound,
            ContractReference.unsafeAssumption("certified-prox-or-splitting")
          )
          .left
          .map(CompositePenaltyCompileError.Guarantee.apply)
      assumptions <- OptimizationAssumptions
        .from(
          bindings,
          properClosedConvex = Vector(convex),
          smoothness = Vector(smooth),
          strongConvexity = Vector(strong),
          normBounds = normWitnesses,
          exactOracleLaws = Vector(exact)
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      stationarity <- NonNegativeProofBound
        .residual(certificate.stationarityResidual)
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      feasibility <- NonNegativeProofBound
        .residual(certificate.dualFeasibilityResidual)
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      gap <- NonNegativeProofBound
        .objectiveGap(certificate.primalDualGap)
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      evidence <- SemanticOptimizationEvidence
        .from(
          bindings,
          status match
            case FirstOrderStoppingStatus.Converged => NumericalTermination.Converged
            case FirstOrderStoppingStatus.IterationLimit => NumericalTermination.IterationLimit,
          stationarity = Some(stationarity),
          feasibility = Some(feasibility),
          objectiveGap = Some(gap)
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      witness <- GlobalOptimalityWitness
        .from(
          bindings,
          ContractReference.unsafeTheorem("strongly-convex-anchor-composite"),
          assumptions.assumptionReferences,
          OracleFamily.Analytic
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
      achievement <- OptimizationGuaranteeAdmission
        .admit(
          contract,
          OptimizationClaimClass.EpsilonGlobal,
          assumptions,
          Set(
            OptimizationProofObligation.Smooth(programIdentity),
            OptimizationProofObligation.ExactProximal(oracleIdentity)
          ) ++ normWitnesses.map(witness => OptimizationProofObligation.NormBounded(witness.operator)),
          evidence,
          Some(witness)
        )
        .left
        .map(CompositePenaltyCompileError.Guarantee.apply)
    yield achievement

  private def termIdentities: Vector[ValueIdentity] =
    program.smooth.map(_.identity) ++ program.direct.toVector.map(_.identity) ++ program.split.map(_.identity)

private final class CompositeSmoothObjective[Feature <: SemanticSpace](
    anchor: StronglyConvexAnchor[Feature],
    terms: Vector[SmoothQuadraticPenalty[Feature]]
) extends SmoothObjective:
  val variableRows: Int = anchor.point.rows
  val lipschitz: Double =
    anchor.lipschitzBound.doubleValue +
      terms.map(term => 2.0 * term.weight * term.normBound.upperBound.doubleValue).sum

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    val parameter = GaleMatrixBridge.toGale(at)
    anchor
      .value(parameter)
      .left
      .map(error => FirstOrderError.OracleFailure("quadratic anchor value", error.message))
      .flatMap: initial =>
        var result = initial
        var index = 0
        var failure = Option.empty[FirstOrderError]
        while index < terms.length && failure.isEmpty do
          val term = terms(index)
          term.lowering.pulledBack(parameter) match
            case Left(error) => failure = Some(FirstOrderError.OracleFailure("smooth quadratic value", error.message))
            case Right(image) => result += term.weight * inner(parameter, image)
          index += 1
        failure.toLeft(result)

  def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
    val parameter = GaleMatrixBridge.toGale(at)
    anchor
      .gradient(parameter)
      .left
      .map(error => FirstOrderError.OracleFailure("quadratic anchor gradient", error.message))
      .flatMap: initial =>
        var result = initial
        var index = 0
        var failure = Option.empty[FirstOrderError]
        while index < terms.length && failure.isEmpty do
          val term = terms(index)
          term.lowering.pulledBack(parameter) match
            case Left(error) => failure = Some(FirstOrderError.OracleFailure("smooth quadratic gradient", error.message))
            case Right(image) =>
              result = MatrixOps.subtract(result, MatrixOps.scale(image, -2.0 * term.weight))
          index += 1
        failure.toLeft(GaleMatrixBridge.fromGale(result))

private final class TiltedSmoothObjective(
    base: SmoothObjective,
    argument: DMat
) extends SmoothObjective:
  val variableRows: Int = base.variableRows
  val lipschitz: Double = base.lipschitz

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    base.value(at).map(_ - inner(argument, GaleMatrixBridge.toGale(at)))

  def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix] =
    base.gradient(at).map: value =>
      GaleMatrixBridge.fromGale(MatrixOps.subtract(GaleMatrixBridge.toGale(value), argument))

private final class ZeroDirectTerm(val variableRows: Int) extends ProximalTerm:
  def value(at: DoubleMatrix): Either[FirstOrderError, Double] = Right(0.0)
  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] = Right(at)

private final class DirectTermAdapter[Feature <: SemanticSpace](term: ExactDirectPenalty[Feature]) extends ProximalTerm:
  val variableRows: Int = term.featureDimension
  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    term.value(GaleMatrixBridge.toGale(at)).left.map(error => FirstOrderError.OracleFailure("direct penalty value", error.message))
  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
    term
      .proximal(GaleMatrixBridge.toGale(at), step)
      .left
      .map(error => FirstOrderError.OracleFailure("direct penalty proximal", error.message))
      .map(GaleMatrixBridge.fromGale)

private final class StackedSplitOperator[Feature <: SemanticSpace] private (
    sourceRows: Int,
    terms: Vector[SplitL1Penalty[Feature]],
    val normBound: StackedOperatorNormBound,
    val bounded: BoundedLinearMap,
    val functional: LinearCompositeFunctional
):
  val rows: Int = terms.map(_.rows).sum

  def adjoint(value: DMat): Either[CompositePenaltyCompileError, DMat] =
    if terms.isEmpty then Right(DMat.zeros(sourceRows, value.cols))
    else
      var result = DMat.zeros(sourceRows, value.cols)
      var offset = 0
      var index = 0
      var failure = Option.empty[CompositePenaltyCompileError]
      while index < terms.length && failure.isEmpty do
        val term = terms(index)
        val block = sliceRows(value, offset, term.rows)
        term.adjoint(block) match
          case Left(error) => failure = Some(error)
          case Right(image) => result = MatrixOps.subtract(result, MatrixOps.scale(image, -1.0))
        offset += term.rows
        index += 1
      failure.toLeft(result)

  def dualFeasibility(value: DMat): Double =
    var result = 0.0
    var offset = 0
    var index = 0
    while index < terms.length do
      val term = terms(index)
      var row = 0
      while row < term.rows do
        var column = 0
        while column < value.cols do
          result = Math.max(result, Math.max(0.0, Math.abs(value(offset + row, column)) - term.weight))
          column += 1
        row += 1
      offset += term.rows
      index += 1
    result

  def dualResidual(parameter: DMat, dual: DMat, stepSafety: Double): Either[CompositePenaltyCompileError, Double] =
    val bound = normBound.upperBound.doubleValue
    val dualStep = if bound == 0.0 then 1.0 else stepSafety / bound
    for
      mapped <- bounded.map
        .forward(GaleMatrixBridge.fromGale(parameter))
        .left
        .map(error => CompositePenaltyCompileError.InvalidDefinition(error.message))
      candidate = MatrixOps.subtract(
        dual,
        MatrixOps.scale(GaleMatrixBridge.toGale(mapped), -dualStep)
      )
      fixed <- functional
        .proximalConjugate(GaleMatrixBridge.fromGale(candidate), dualStep)
        .left
        .map(CompositePenaltyCompileError.Solver.apply)
    yield compositeMaxAbs(MatrixOps.subtract(dual, GaleMatrixBridge.toGale(fixed))) / dualStep

object StackedSplitOperator:
  def from[Feature <: SemanticSpace](
      sourceRows: Int,
      terms: Vector[SplitL1Penalty[Feature]]
  ): Either[CompositePenaltyCompileError, StackedSplitOperator[Feature]] =
    val map = new StackedLinearMap(sourceRows, terms)
    for
      norm <- StackedOperatorNormBound.from(terms.map(_.normBound))
      bounded <- BoundedLinearMap
        .from(map, norm.upperBound.doubleValue)
        .left
        .map(CompositePenaltyCompileError.Solver.apply)
    yield new StackedSplitOperator(sourceRows, terms, norm, bounded, new StackedL1Functional(terms))

private final class StackedLinearMap[Feature <: SemanticSpace](
    sourceRows: Int,
    terms: Vector[SplitL1Penalty[Feature]]
) extends LinearMap:
  val rows: Int = terms.map(_.rows).sum
  val cols: Int = sourceRows

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    val source = GaleMatrixBridge.toGale(input)
    val output = new Array[Double](rows * input.cols)
    var offset = 0
    var index = 0
    var failure = Option.empty[LinearMapError]
    while index < terms.length && failure.isEmpty do
      val term = terms(index)
      term.forward(source) match
        case Left(error) => failure = Some(LinearMapError.OperatorApplicationFailed(error.message))
        case Right(block) => copyRows(block, output, offset)
      offset += term.rows
      index += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          DoubleMatrix
            .fromRowMajor(scalafim.linalg.MatrixShape.unsafe(rows, input.cols), output)
            .toOption
            .get
        )

  def adjoint: LinearMap = new StackedAdjointMap(sourceRows, terms)

private final class StackedAdjointMap[Feature <: SemanticSpace](
    targetRows: Int,
    terms: Vector[SplitL1Penalty[Feature]]
) extends LinearMap:
  val rows: Int = targetRows
  val cols: Int = terms.map(_.rows).sum

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    val value = GaleMatrixBridge.toGale(input)
    var result = DMat.zeros(rows, input.cols)
    var offset = 0
    var index = 0
    var failure = Option.empty[LinearMapError]
    while index < terms.length && failure.isEmpty do
      val term = terms(index)
      term.adjoint(sliceRows(value, offset, term.rows)) match
        case Left(error) => failure = Some(LinearMapError.OperatorApplicationFailed(error.message))
        case Right(block) => result = MatrixOps.subtract(result, MatrixOps.scale(block, -1.0))
      offset += term.rows
      index += 1
    failure.toLeft(GaleMatrixBridge.fromGale(result))

  def adjoint: LinearMap = new StackedLinearMap(targetRows, terms)

private final class StackedL1Functional[Feature <: SemanticSpace](
    terms: Vector[SplitL1Penalty[Feature]]
) extends LinearCompositeFunctional:
  val targetRows: Int = terms.map(_.rows).sum

  def value(at: DoubleMatrix): Either[FirstOrderError, Double] =
    var result = 0.0
    var offset = 0
    var index = 0
    while index < terms.length do
      val term = terms(index)
      var row = 0
      while row < term.rows do
        var column = 0
        while column < at.cols do
          result += term.weight * Math.abs(at(offset + row, column))
          column += 1
        row += 1
      offset += term.rows
      index += 1
    Right(result)

  def proximalConjugate(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix] =
    val output = at.copyData
    var offset = 0
    var index = 0
    while index < terms.length do
      val term = terms(index)
      var row = 0
      while row < term.rows do
        var column = 0
        while column < at.cols do
          val position = (offset + row) * at.cols + column
          output(position) = Math.max(-term.weight, Math.min(term.weight, output(position)))
          column += 1
        row += 1
      offset += term.rows
      index += 1
    DoubleMatrix
      .fromRowMajor(at.shape, output)
      .left
      .map(error => FirstOrderError.InvalidConfiguration(error.message))

private def compositeFirstOrderConfig(
    config: PrimalDualConfig
): Either[CompositePenaltyCompileError, FirstOrderConfig] =
  for
    tolerance <- FirstOrderTolerance
      .from(config.tolerance.absoluteValue, config.tolerance.relativeValue)
      .left
      .map(CompositePenaltyCompileError.Solver.apply)
    result <- FirstOrderConfig
      .from(config.iterations.intValue, tolerance, extrapolation = config.extrapolation.value)
      .left
      .map(CompositePenaltyCompileError.Solver.apply)
  yield result

private def directPenaltyValue(kind: DirectProximalKind, value: DMat, weight: Double): Double =
  kind match
    case DirectProximalKind.ElementwiseL1 => weight * l1(value)
    case DirectProximalKind.FeatureRowsL21 => weight * rowL21(value)
    case DirectProximalKind.DisjointGroups(groups) => weight * groupL2(value, groups)
    case DirectProximalKind.SparseGroup(fraction, groups) =>
      weight * (fraction.value * l1(value) + (1.0 - fraction.value) * groupL2(value, groups))
    case DirectProximalKind.ElasticNet(fraction) =>
      weight * (fraction.value * l1(value) + 0.5 * (1.0 - fraction.value) * squaredNorm(value))

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
    var squared = 0.0
    var column = 0
    while column < value.cols do
      val current = value(row, column)
      squared += current * current
      column += 1
    result += Math.sqrt(squared)
    row += 1
  result

private def groupL2(value: DMat, groups: GroupStructure): Double =
  groups.groups.foldLeft(0.0): (result, group) =>
    var squared = 0.0
    var index = 0
    while index < group.indices.length do
      val row = group.indices.indices(index)
      var column = 0
      while column < value.cols do
        val current = value(row, column)
        squared += current * current
        column += 1
      index += 1
    result + Math.sqrt(squared)

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

private def squaredNorm(value: DMat): Double = inner(value, value)

private def compositeMaxAbs(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      result = Math.max(result, Math.abs(value(row, column)))
      column += 1
    row += 1
  result

private def firstNonFinite(value: DMat): Option[(Int, Int, Double)] =
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      if !value(row, column).isFinite then return Some((row, column, value(row, column)))
      column += 1
    row += 1
  None

private def sliceRows(value: DMat, start: Int, count: Int): DMat =
  val output = new Array[Double](count * value.cols)
  var row = 0
  while row < count do
    var column = 0
    while column < value.cols do
      output(row * value.cols + column) = value(start + row, column)
      column += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(count, value.cols, output)

private def copyRows(value: DMat, output: Array[Double], offset: Int): Unit =
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      output((offset + row) * value.cols + column) = value(row, column)
      column += 1
    row += 1

private def traverse[A, B](values: Vector[A])(
    function: A => Either[CompositePenaltyCompileError, B]
): Either[CompositePenaltyCompileError, Vector[B]] =
  values.foldLeft[Either[CompositePenaltyCompileError, Vector[B]]](Right(Vector.empty)): (result, value) =>
    for
      completed <- result
      next <- function(value)
    yield completed :+ next
