package scalafim.multivar

import gale.linalg.DMat

enum PalmConvergenceError:
  case InvalidDefinition(detail: String)
  case UnknownBlock(parameter: ParameterId)
  case ObjectiveFailure(context: String, detail: String)
  case BlockFailure(parameter: ParameterId, detail: String)
  case DescentViolation(
      iteration: Int,
      parameter: ParameterId,
      before: Double,
      after: Double,
      upperBound: Double
  )
  case InexactnessViolation(iteration: Int, parameter: ParameterId, actual: Double, allowed: Double)
  case Guarantee(error: OptimizationGuaranteeError)
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case UnknownBlock(parameter) => s"unknown PALM block '${parameter.value}'"
      case ObjectiveFailure(context, detail) => s"PALM objective $context failed: $detail"
      case BlockFailure(parameter, detail) => s"PALM block '${parameter.value}' failed: $detail"
      case DescentViolation(iteration, parameter, before, after, upperBound) =>
        s"PALM iteration $iteration block '${parameter.value}' increased the objective from $before to $after; " +
          s"the admitted upper bound was $upperBound"
      case InexactnessViolation(iteration, parameter, actual, allowed) =>
        s"PALM iteration $iteration block '${parameter.value}' reported inexactness $actual above $allowed"
      case Guarantee(error) => error.message
      case Semantic(error) => error.message

final class PalmBlockValue private (
    val parameter: ParameterId,
    val values: DMat,
    val valueIdentity: ValueIdentity
)

object PalmBlockValue:
  def from(
      parameter: ParameterId,
      values: DMat,
      valueIdentity: ValueIdentity
  ): Either[PalmConvergenceError, PalmBlockValue] =
    if values.rows <= 0 || values.cols <= 0 then
      Left(
        PalmConvergenceError.InvalidDefinition(
          s"PALM block '${parameter.value}' must be non-empty, got ${values.rows} x ${values.cols}"
        )
      )
    else
      firstNonFinite(values) match
        case Some((row, column, value)) =>
          Left(
            PalmConvergenceError.InvalidDefinition(
              s"PALM block '${parameter.value}' value ($row,$column) is not finite: $value"
            )
          )
        case None => Right(new PalmBlockValue(parameter, values, valueIdentity))

final class PalmState private (
    val blocks: Vector[PalmBlockValue],
    val valueIdentity: ValueIdentity
):
  def block(parameter: ParameterId): Either[PalmConvergenceError, PalmBlockValue] =
    blocks.find(_.parameter == parameter).toRight(PalmConvergenceError.UnknownBlock(parameter))

  private[multivar] def replace(next: PalmBlockValue): PalmState =
    val replaced = blocks.map(current => if current.parameter == next.parameter then next else current)
    new PalmState(
      replaced,
      ValueIdentity.derived("palm-state", replaced.map(_.valueIdentity)*)
    )

object PalmState:
  def from(blocks: Vector[PalmBlockValue]): Either[PalmConvergenceError, PalmState] =
    if blocks.isEmpty then Left(PalmConvergenceError.InvalidDefinition("a PALM state requires at least one block"))
    else if blocks.map(_.parameter).distinct.length != blocks.length then
      Left(PalmConvergenceError.InvalidDefinition("PALM state block parameters must be distinct"))
    else
      Right(
        new PalmState(
          blocks,
          ValueIdentity.derived("palm-state", blocks.map(_.valueIdentity)*)
        )
      )

final class PalmObjective private (
    val valueIdentity: ValueIdentity,
    val description: String,
    private val evaluateFunction: PalmState => Either[String, Double]
):
  def evaluate(state: PalmState): Either[PalmConvergenceError, Double] =
    evaluateFunction(state)
      .left
      .map(PalmConvergenceError.ObjectiveFailure(description, _))
      .flatMap: value =>
        if value.isFinite then Right(value)
        else Left(PalmConvergenceError.ObjectiveFailure(description, s"non-finite value $value"))

object PalmObjective:
  def from(
      valueIdentity: ValueIdentity,
      description: String
  )(
      evaluate: PalmState => Either[String, Double]
  ): Either[PalmConvergenceError, PalmObjective] =
    val clean = description.trim
    if clean.isEmpty then Left(PalmConvergenceError.InvalidDefinition("PALM objective description must be non-empty"))
    else Right(new PalmObjective(valueIdentity, clean, evaluate))

final case class PalmBlockAssumptions(
    parameter: ParameterId,
    functional: ValueIdentity,
    properClosedConvex: ContractReference[AssumptionReference],
    partialGradientLipschitz: SmoothnessConstant,
    lipschitzAssumption: ContractReference[AssumptionReference]
)

enum PalmBlockSolveKind:
  case Exact
  case Inexact

final class PalmBlockUpdate private (
    val value: DMat,
    val valueIdentity: ValueIdentity,
    val solveKind: PalmBlockSolveKind,
    val subproblemResidual: Double,
    val normalizationResidual: Double,
    val inexactness: Double
)

object PalmBlockUpdate:
  def from(
      value: DMat,
      valueIdentity: ValueIdentity,
      solveKind: PalmBlockSolveKind,
      subproblemResidual: Double,
      normalizationResidual: Double,
      inexactness: Double
  ): Either[PalmConvergenceError, PalmBlockUpdate] =
    firstNonFinite(value) match
      case Some((row, column, actual)) =>
        Left(PalmConvergenceError.InvalidDefinition(s"PALM update value ($row,$column) is not finite: $actual"))
      case None if !subproblemResidual.isFinite || subproblemResidual < 0.0 =>
        Left(PalmConvergenceError.InvalidDefinition(s"subproblem residual must be finite and non-negative, got $subproblemResidual"))
      case None if !normalizationResidual.isFinite || normalizationResidual < 0.0 =>
        Left(PalmConvergenceError.InvalidDefinition(s"normalization residual must be finite and non-negative, got $normalizationResidual"))
      case None if !inexactness.isFinite || inexactness < 0.0 =>
        Left(PalmConvergenceError.InvalidDefinition(s"inexactness must be finite and non-negative, got $inexactness"))
      case None if solveKind == PalmBlockSolveKind.Exact && inexactness != 0.0 =>
        Left(PalmConvergenceError.InvalidDefinition("an exact PALM block update must report zero inexactness"))
      case None =>
        Right(
          new PalmBlockUpdate(
            value,
            valueIdentity,
            solveKind,
            subproblemResidual,
            normalizationResidual,
            inexactness
          )
        )

final class PalmBlockOracle private (
    val assumptions: PalmBlockAssumptions,
    private val updateFunction: (PalmState, Int) => Either[String, PalmBlockUpdate],
    private val stationarityFunction: PalmState => Either[String, Double],
    private val normalizationFunction: PalmState => Either[String, Double]
):
  def parameter: ParameterId = assumptions.parameter

  def update(state: PalmState, iteration: Int): Either[PalmConvergenceError, PalmBlockUpdate] =
    updateFunction(state, iteration).left.map(PalmConvergenceError.BlockFailure(parameter, _))

  def stationarity(state: PalmState): Either[PalmConvergenceError, Double] =
    checkedResidual("stationarity", stationarityFunction(state))

  def normalization(state: PalmState): Either[PalmConvergenceError, Double] =
    checkedResidual("normalization", normalizationFunction(state))

  private def checkedResidual(
      label: String,
      result: Either[String, Double]
  ): Either[PalmConvergenceError, Double] =
    result
      .left
      .map(PalmConvergenceError.BlockFailure(parameter, _))
      .flatMap: value =>
        if value.isFinite && value >= 0.0 then Right(value)
        else Left(PalmConvergenceError.BlockFailure(parameter, s"$label residual is invalid: $value"))

object PalmBlockOracle:
  def from(
      assumptions: PalmBlockAssumptions
  )(
      update: (PalmState, Int) => Either[String, PalmBlockUpdate],
      stationarity: PalmState => Either[String, Double],
      normalization: PalmState => Either[String, Double]
  ): PalmBlockOracle =
    new PalmBlockOracle(assumptions, update, stationarity, normalization)

enum PalmSingularGeometryPolicy:
  case Reject
  case RestrictToSupport(support: ValueIdentity)
  case QuotientNullspace(quotient: ValueIdentity)

enum PalmLevelSetKind:
  case Coercive(modulus: NullspaceCoercivityModulus)
  case CompactNormalization(
      geometry: ValueIdentity,
      radius: Double,
      ambientDimension: Int,
      effectiveRank: Int,
      singularPolicy: PalmSingularGeometryPolicy
  )

final class PalmLevelSetWitness private (
    val program: ValueIdentity,
    val kind: PalmLevelSetKind,
    val assumption: ContractReference[AssumptionReference],
    val valueIdentity: ValueIdentity
)

object PalmLevelSetWitness:
  def coercive(
      program: ValueIdentity,
      modulus: NullspaceCoercivityModulus,
      assumption: ContractReference[AssumptionReference]
  ): PalmLevelSetWitness =
    new PalmLevelSetWitness(
      program,
      PalmLevelSetKind.Coercive(modulus),
      assumption,
      ValueIdentity.derived("palm-coercive-level-set", program)
    )

  def compactNormalization(
      program: ValueIdentity,
      geometry: ValueIdentity,
      radius: Double,
      ambientDimension: Int,
      effectiveRank: Int,
      singularPolicy: PalmSingularGeometryPolicy,
      assumption: ContractReference[AssumptionReference]
  ): Either[PalmConvergenceError, PalmLevelSetWitness] =
    if !radius.isFinite || radius <= 0.0 then
      Left(PalmConvergenceError.InvalidDefinition(s"normalization radius must be finite and positive, got $radius"))
    else if ambientDimension <= 0 || effectiveRank <= 0 || effectiveRank > ambientDimension then
      Left(
        PalmConvergenceError.InvalidDefinition(
          s"normalization geometry rank $effectiveRank must lie in 1..$ambientDimension"
        )
      )
    else if effectiveRank < ambientDimension && singularPolicy == PalmSingularGeometryPolicy.Reject then
      Left(
        PalmConvergenceError.InvalidDefinition(
          "singular normalization geometry requires support restriction or quotient-nullspace semantics"
        )
      )
    else
      Right(
        new PalmLevelSetWitness(
          program,
          PalmLevelSetKind.CompactNormalization(
            geometry,
            radius,
            ambientDimension,
            effectiveRank,
            singularPolicy
          ),
          assumption,
          ValueIdentity.derived("palm-compact-normalization", program, geometry)
        )
      )

final case class GeometricInexactnessSchedule private (
    initialBound: Double,
    contraction: Double
):
  def bound(iteration: Int): Double = initialBound * Math.pow(contraction, iteration.toDouble)
  def infiniteSumBound: Double = initialBound / (1.0 - contraction)

object GeometricInexactnessSchedule:
  def from(initialBound: Double, contraction: Double): Either[PalmConvergenceError, GeometricInexactnessSchedule] =
    if !initialBound.isFinite || initialBound < 0.0 then
      Left(PalmConvergenceError.InvalidDefinition(s"initial inexactness bound must be finite and non-negative, got $initialBound"))
    else if !contraction.isFinite || contraction < 0.0 || contraction >= 1.0 then
      Left(PalmConvergenceError.InvalidDefinition(s"inexactness contraction must lie in [0,1), got $contraction"))
    else Right(GeometricInexactnessSchedule(initialBound, contraction))

enum PalmSubproblemPolicy:
  case Exact
  case SummablyInexact(
      schedule: GeometricInexactnessSchedule,
      assumption: ContractReference[AssumptionReference]
  )

enum PalmKlEvidence:
  case NotClaimed
  case SemiAlgebraic(
      objective: ValueIdentity,
      assumption: ContractReference[AssumptionReference],
      proof: String
  )
  case LogExpDefinable(
      objective: ValueIdentity,
      assumption: ContractReference[AssumptionReference],
      proof: String
  )

enum PalmConvergenceTarget:
  case CriticalPoint
  case CoordinatewiseStationary

final class PalmProblem private (
    val contract: MathematicalModelContract,
    val programIdentity: ValueIdentity,
    val dataIdentity: ValueIdentity,
    val observationMask: ObservationMaskIdentity,
    val operatorIdentities: Vector[ValueIdentity],
    val objective: PalmObjective,
    val blocks: Vector[PalmBlockOracle]
):
  def parameters: Vector[ParameterId] = blocks.map(_.parameter)

object PalmProblem:
  def from(
      contract: MathematicalModelContract,
      programIdentity: ValueIdentity,
      dataIdentity: ValueIdentity,
      observationMask: ObservationMaskIdentity,
      operatorIdentities: Vector[ValueIdentity],
      objective: PalmObjective,
      blocks: Vector[PalmBlockOracle]
  ): Either[PalmConvergenceError, PalmProblem] =
    if blocks.isEmpty then Left(PalmConvergenceError.InvalidDefinition("a PALM problem requires at least one block"))
    else if blocks.map(_.parameter).distinct.length != blocks.length then
      Left(PalmConvergenceError.InvalidDefinition("PALM problem block parameters must be distinct"))
    else if operatorIdentities.isEmpty then
      Left(PalmConvergenceError.InvalidDefinition("a PALM problem requires bound operator identities"))
    else if operatorIdentities.distinct.length != operatorIdentities.length then
      Left(PalmConvergenceError.InvalidDefinition("PALM problem operator identities must be distinct"))
    else if !operatorIdentities.contains(objective.valueIdentity) then
      Left(PalmConvergenceError.InvalidDefinition("PALM objective identity must be among the bound operators"))
    else if blocks.exists(block => !operatorIdentities.contains(block.assumptions.functional)) then
      Left(PalmConvergenceError.InvalidDefinition("every PALM block functional must be among the bound operators"))
    else Right(new PalmProblem(contract, programIdentity, dataIdentity, observationMask, operatorIdentities, objective, blocks))

final class PalmAdmission private (
    val problem: PalmProblem,
    val levelSet: PalmLevelSetWitness,
    val subproblems: PalmSubproblemPolicy,
    val klEvidence: PalmKlEvidence,
    val target: PalmConvergenceTarget,
    val admissionIdentity: ValueIdentity
)

object PalmAdmission:
  def from(
      problem: PalmProblem,
      levelSet: PalmLevelSetWitness,
      subproblems: PalmSubproblemPolicy,
      klEvidence: PalmKlEvidence,
      target: PalmConvergenceTarget
  ): Either[PalmConvergenceError, PalmAdmission] =
    if levelSet.program != problem.programIdentity then
      Left(PalmConvergenceError.InvalidDefinition("PALM level-set witness is bound to a foreign program"))
    else if problem.blocks.exists(_.assumptions.partialGradientLipschitz.doubleValue <= 0.0) then
      Left(PalmConvergenceError.InvalidDefinition("every PALM block requires a positive partial-gradient Lipschitz bound"))
    else if target == PalmConvergenceTarget.CriticalPoint && klEvidence == PalmKlEvidence.NotClaimed then
      Left(PalmConvergenceError.InvalidDefinition("critical-point convergence requires explicit KL applicability evidence"))
    else
      klEvidence match
        case PalmKlEvidence.SemiAlgebraic(objective, _, proof)
            if objective != problem.objective.valueIdentity || proof.trim.isEmpty =>
          Left(PalmConvergenceError.InvalidDefinition("semi-algebraic KL evidence must bind the objective and state a proof"))
        case PalmKlEvidence.LogExpDefinable(objective, _, proof)
            if objective != problem.objective.valueIdentity || proof.trim.isEmpty =>
          Left(PalmConvergenceError.InvalidDefinition("log-exp definable KL evidence must bind the objective and state a proof"))
        case _ =>
          val identity = ValueIdentity.derived(
            s"palm-admission-${target.toString.toLowerCase}",
            problem.programIdentity,
            problem.objective.valueIdentity,
            levelSet.valueIdentity
          )
          Right(new PalmAdmission(problem, levelSet, subproblems, klEvidence, target, identity))

opaque type PalmDecreaseCoefficient = Double

object PalmDecreaseCoefficient:
  def apply(value: Double): Either[PalmConvergenceError, PalmDecreaseCoefficient] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(PalmConvergenceError.InvalidDefinition(s"sufficient-decrease coefficient must be finite and positive, got $value"))

  def unsafe(value: Double): PalmDecreaseCoefficient =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: PalmDecreaseCoefficient)
    inline def doubleValue: Double = value

enum PalmDescentPolicy:
  case Monotone
  case SufficientDecrease(coefficient: PalmDecreaseCoefficient)

final case class PalmConfig(
    iterations: IterationBudget,
    tolerance: CertificateTolerance,
    descent: PalmDescentPolicy
)

object PalmConfig:
  val portable: PalmConfig =
    PalmConfig(
      IterationBudget.unsafe(1000),
      CertificateTolerance.strict,
      PalmDescentPolicy.Monotone
    )

enum PalmTermination:
  case Converged
  case IterationLimit

final case class PalmBlockTrace(
    parameter: ParameterId,
    objectiveBefore: Double,
    objectiveAfter: Double,
    stepNorm: Double,
    subproblemResidual: Double,
    normalizationResidual: Double,
    inexactness: Double,
    solveKind: PalmBlockSolveKind,
    valueIdentity: ValueIdentity
)

final case class PalmIterationTrace(
    iteration: Int,
    objectiveBefore: Double,
    objectiveAfter: Double,
    blocks: Vector[PalmBlockTrace],
    stationarity: Vector[(ParameterId, Double)],
    normalizationResidual: Double,
    totalStepNorm: Double
):
  require(iteration >= 0)
  require(blocks.nonEmpty)

  def maximumStationarity: Double = stationarity.map(_._2).max

final case class PalmConvergenceReceipt(
    initialObjective: Double,
    traces: Vector[PalmIterationTrace],
    termination: PalmTermination,
    descent: PalmDescentPolicy,
    subproblems: PalmSubproblemPolicy,
    levelSet: PalmLevelSetWitness,
    klEvidence: PalmKlEvidence,
    tolerance: CertificateTolerance
):
  require(initialObjective.isFinite)
  require(traces.nonEmpty)

  def objectiveHistory: Vector[Double] = initialObjective +: traces.map(_.objectiveAfter)
  def finalStationarity: Vector[(ParameterId, Double)] = traces.last.stationarity
  def finalNormalizationResidual: Double = traces.last.normalizationResidual

final case class PalmFit(
    state: PalmState,
    objective: Double,
    achievement: AchievedOptimizationGuarantee,
    receipt: PalmConvergenceReceipt,
    certificate: NumericalCertificate,
    resultIdentity: ValueIdentity
)

final class PalmSolver private (val admission: PalmAdmission):
  def solve(
      initialization: PalmInitialization,
      config: PalmConfig = PalmConfig.portable
  ): Either[PalmConvergenceError, PalmFit] =
    val problem = admission.problem
    for
      _ <- validateInitialization(problem, initialization)
      initialObjective <- problem.objective.evaluate(initialization.state)
      run <- iterate(problem, initialization.state, initialObjective, config)
      (state, objective, traces, termination) = run
      resultIdentity = ValueIdentity.derived(
        "palm-result",
        problem.programIdentity,
        initialization.valueIdentity,
        state.valueIdentity
      )
      admitted <- admitAchievement(resultIdentity, traces.last, termination, config.tolerance)
      (achievement, certificate) = admitted
    yield
      PalmFit(
        state,
        objective,
        achievement,
        PalmConvergenceReceipt(
          initialObjective,
          traces,
          termination,
          config.descent,
          admission.subproblems,
          admission.levelSet,
          admission.klEvidence,
          config.tolerance
        ),
        certificate,
        resultIdentity
      )

  private def iterate(
      problem: PalmProblem,
      initial: PalmState,
      initialObjective: Double,
      config: PalmConfig
  ): Either[PalmConvergenceError, (PalmState, Double, Vector[PalmIterationTrace], PalmTermination)] =
    var state = initial
    var objective = initialObjective
    var traces = Vector.empty[PalmIterationTrace]
    var iteration = 0
    var converged = false
    var failure = Option.empty[PalmConvergenceError]
    while iteration < config.iterations.intValue && !converged && failure.isEmpty do
      val beforeSweep = objective
      var blockTraces = Vector.empty[PalmBlockTrace]
      var blockIndex = 0
      while blockIndex < problem.blocks.length && failure.isEmpty do
        val oracle = problem.blocks(blockIndex)
        oracle.update(state, iteration) match
          case Left(error) => failure = Some(error)
          case Right(update) =>
            state.block(oracle.parameter) match
              case Left(error) => failure = Some(error)
              case Right(previous) =>
                validateUpdate(previous, update) match
                  case Left(error) => failure = Some(error)
                  case Right(_) =>
                    allowedInexactness(iteration, oracle.parameter, update) match
                      case Left(error) => failure = Some(error)
                      case Right(allowed) =>
                        PalmBlockValue.from(oracle.parameter, update.value, update.valueIdentity) match
                          case Left(error) => failure = Some(error)
                          case Right(nextValue) =>
                            val nextState = state.replace(nextValue)
                            problem.objective.evaluate(nextState) match
                              case Left(error) => failure = Some(error)
                              case Right(nextObjective) =>
                                val step = matrixDistance(previous.values, update.value)
                                descentUpperBound(objective, step, update.inexactness, config) match
                                  case upper if nextObjective > upper =>
                                    failure = Some(
                                      PalmConvergenceError.DescentViolation(
                                        iteration,
                                        oracle.parameter,
                                        objective,
                                        nextObjective,
                                        upper
                                      )
                                    )
                                  case _ =>
                                    blockTraces :+= PalmBlockTrace(
                                      oracle.parameter,
                                      objective,
                                      nextObjective,
                                      step,
                                      update.subproblemResidual,
                                      update.normalizationResidual,
                                      update.inexactness,
                                      update.solveKind,
                                      update.valueIdentity
                                    )
                                    state = nextState
                                    objective = nextObjective
        blockIndex += 1
      if failure.isEmpty then
        stationarity(problem, state) match
          case Left(error) => failure = Some(error)
          case Right(residuals) =>
            normalization(problem, state) match
              case Left(error) => failure = Some(error)
              case Right(normalizationResidual) =>
                val totalStep = Math.sqrt(blockTraces.map(trace => trace.stepNorm * trace.stepNorm).sum)
                val trace = PalmIterationTrace(
                  iteration,
                  beforeSweep,
                  objective,
                  blockTraces,
                  residuals,
                  normalizationResidual,
                  totalStep
                )
                traces :+= trace
                val threshold = config.tolerance.threshold(Math.max(1.0, Math.abs(objective)))
                converged = trace.maximumStationarity <= threshold && normalizationResidual <= threshold
      iteration += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          (
            state,
            objective,
            traces,
            if converged then PalmTermination.Converged else PalmTermination.IterationLimit
          )
        )

  private def allowedInexactness(
      iteration: Int,
      parameter: ParameterId,
      update: PalmBlockUpdate
  ): Either[PalmConvergenceError, Double] =
    admission.subproblems match
      case PalmSubproblemPolicy.Exact =>
        val actual = Math.max(update.inexactness, update.subproblemResidual)
        if update.solveKind == PalmBlockSolveKind.Exact && actual == 0.0 then Right(0.0)
        else Left(PalmConvergenceError.InexactnessViolation(iteration, parameter, actual, 0.0))
      case PalmSubproblemPolicy.SummablyInexact(schedule, _) =>
        val allowed = schedule.bound(iteration)
        val actual = Math.max(update.inexactness, update.subproblemResidual)
        if update.solveKind == PalmBlockSolveKind.Inexact && actual <= allowed then Right(allowed)
        else Left(PalmConvergenceError.InexactnessViolation(iteration, parameter, actual, allowed))

  private def descentUpperBound(
      before: Double,
      stepNorm: Double,
      inexactness: Double,
      config: PalmConfig
  ): Double =
    val slack = config.tolerance.threshold(Math.max(1.0, Math.abs(before))) + inexactness
    config.descent match
      case PalmDescentPolicy.Monotone => before + slack
      case PalmDescentPolicy.SufficientDecrease(coefficient) =>
        before - coefficient.doubleValue * stepNorm * stepNorm + slack

  private def stationarity(
      problem: PalmProblem,
      state: PalmState
  ): Either[PalmConvergenceError, Vector[(ParameterId, Double)]] =
    traversePalm(problem.blocks)(oracle => oracle.stationarity(state).map(oracle.parameter -> _))

  private def normalization(problem: PalmProblem, state: PalmState): Either[PalmConvergenceError, Double] =
    traversePalm(problem.blocks)(_.normalization(state)).map(_.max)

  private def admitAchievement(
      resultIdentity: ValueIdentity,
      finalTrace: PalmIterationTrace,
      termination: PalmTermination,
      tolerance: CertificateTolerance
  ): Either[PalmConvergenceError, (AchievedOptimizationGuarantee, NumericalCertificate)] =
    val problem = admission.problem
    for
      bindings <- OptimizationIdentityBindings
        .from(
          problem.contract.id,
          problem.programIdentity,
          problem.dataIdentity,
          problem.observationMask,
          problem.operatorIdentities,
          problem.parameters,
          resultIdentity
        )
        .left
        .map(PalmConvergenceError.Guarantee.apply)
      blockResiduals <- traversePalm(finalTrace.stationarity): (parameter, residual) =>
        NonNegativeProofBound
          .residual(residual)
          .left
          .map(PalmConvergenceError.Guarantee.apply)
          .map(parameter -> _)
      maximum <- NonNegativeProofBound
        .residual(finalTrace.maximumStationarity)
        .left
        .map(PalmConvergenceError.Guarantee.apply)
      feasibility <- NonNegativeProofBound
        .residual(finalTrace.normalizationResidual)
        .left
        .map(PalmConvergenceError.Guarantee.apply)
      context <- CertificateContext
        .from(
          tolerance,
          CertificateNorm.Euclidean,
          "portable-palm",
          "gale",
          NumericalPrecision.Float64
        )
        .left
        .map(PalmConvergenceError.Semantic.apply)
      certificate <- Certificate
        .solverTrace(
          resultIdentity,
          finalTrace.iteration + 1,
          finalTrace.maximumStationarity,
          Math.max(1.0, Math.abs(finalTrace.objectiveAfter)),
          termination == PalmTermination.Converged,
          context
        )
        .left
        .map(PalmConvergenceError.Semantic.apply)
      evidence <- SemanticOptimizationEvidence
        .from(
          bindings,
          if termination == PalmTermination.Converged then NumericalTermination.Converged
          else NumericalTermination.IterationLimit,
          stationarity = Some(maximum),
          blockStationarity = blockResiduals,
          feasibility = Some(feasibility),
          numericalCertificates = Vector(certificate.runtime)
        )
        .left
        .map(PalmConvergenceError.Guarantee.apply)
      requested =
        if termination != PalmTermination.Converged then OptimizationClaimClass.Unresolved
        else
          admission.target match
            case PalmConvergenceTarget.CriticalPoint => OptimizationClaimClass.Stationary
            case PalmConvergenceTarget.CoordinatewiseStationary => OptimizationClaimClass.CoordinatewiseStationary
      achievement <- OptimizationGuaranteeAdmission
        .admit(
          problem.contract,
          requested,
          OptimizationAssumptions.empty(bindings),
          Set.empty,
          evidence,
          None
        )
        .left
        .map(PalmConvergenceError.Guarantee.apply)
    yield achievement -> certificate.runtime

object PalmSolver:
  def from(admission: PalmAdmission): PalmSolver = new PalmSolver(admission)

enum PalmInitializationKind:
  case SvdDerived(source: ValueIdentity)
  case Deterministic(label: String)

final class PalmInitialization private (
    val id: ParameterId,
    val kind: PalmInitializationKind,
    val state: PalmState,
    val valueIdentity: ValueIdentity
)

object PalmInitialization:
  def from(
      id: ParameterId,
      kind: PalmInitializationKind,
      state: PalmState
  ): Either[PalmConvergenceError, PalmInitialization] =
    kind match
      case PalmInitializationKind.Deterministic(label) if label.trim.isEmpty =>
        Left(PalmConvergenceError.InvalidDefinition("deterministic initialization label must be non-empty"))
      case _ =>
        Right(
          new PalmInitialization(
            id,
            kind,
            state,
            ValueIdentity.derived(s"palm-initialization-${id.value}", state.valueIdentity)
          )
        )

enum PalmStartOutcome:
  case Succeeded(fit: PalmFit)
  case Failed(error: PalmConvergenceError)

final case class PalmStartResult(
    initialization: PalmInitialization,
    outcome: PalmStartOutcome
)

enum PalmMultiStartSelection:
  case MinimumObjectiveThenStationarityThenId

final case class PalmMultiStartFit(
    starts: Vector[PalmStartResult],
    selected: Int,
    selection: PalmMultiStartSelection
):
  require(starts.nonEmpty)
  require(selected >= 0 && selected < starts.length)

  def fit: PalmFit =
    starts(selected).outcome match
      case PalmStartOutcome.Succeeded(value) => value
      case PalmStartOutcome.Failed(_) => throw new IllegalStateException("selected PALM start must have succeeded")

object PalmMultiStart:
  def solve(
      solver: PalmSolver,
      initializations: Vector[PalmInitialization],
      config: PalmConfig = PalmConfig.portable,
      selection: PalmMultiStartSelection = PalmMultiStartSelection.MinimumObjectiveThenStationarityThenId
  ): Either[PalmConvergenceError, PalmMultiStartFit] =
    if initializations.isEmpty then Left(PalmConvergenceError.InvalidDefinition("PALM multi-start requires at least one initialization"))
    else if initializations.map(_.id).distinct.length != initializations.length then
      Left(PalmConvergenceError.InvalidDefinition("PALM multi-start initialization ids must be distinct"))
    else
      val starts = initializations.map: initialization =>
        PalmStartResult(
          initialization,
          solver.solve(initialization, config) match
            case Right(fit) => PalmStartOutcome.Succeeded(fit)
            case Left(error) => PalmStartOutcome.Failed(error)
        )
      val successful = starts.zipWithIndex.collect:
        case (PalmStartResult(initialization, PalmStartOutcome.Succeeded(fit)), index) =>
          (index, initialization.id.value, fit)
      if successful.isEmpty then
        starts.collectFirst { case PalmStartResult(_, PalmStartOutcome.Failed(error)) => error } match
          case Some(error) => Left(error)
          case None => Left(PalmConvergenceError.InvalidDefinition("PALM multi-start produced no result"))
      else
        val selected = successful.minBy: (_, id, fit) =>
          (fit.objective, fit.receipt.finalStationarity.map(_._2).max, id)
        Right(PalmMultiStartFit(starts, selected._1, selection))

final case class ConvexLowRankCertificate(
    bindings: OptimizationIdentityBindings,
    loss: ProperClosedConvexWitness,
    theoremAssumptions: Vector[TheoremAssumptionWitness],
    globalWitness: GlobalOptimalityWitness,
    evidence: SemanticOptimizationEvidence,
    subgradientResidual: CertifiedResidualBound
)

/** Global admission is deliberately outside [[PalmSolver]]. It accepts only
  * the convexified low-rank matrix contract and its nuclear-subgradient theorem.
  */
object ConvexLowRankGlobalAdmission:
  def admitExact(
      certificate: ConvexLowRankCertificate
  ): Either[PalmConvergenceError, AchievedOptimizationGuarantee] =
    val contract = MathematicalContractCatalog.convexifiedLowRankMatrix
    if certificate.bindings.contract != contract.id then
      Left(
        PalmConvergenceError.InvalidDefinition(
          "a nuclear-norm global certificate must bind the convexified low-rank matrix contract"
        )
      )
    else if certificate.evidence.bindings ne certificate.bindings then
      Left(PalmConvergenceError.InvalidDefinition("convex global evidence uses foreign optimization bindings"))
    else if certificate.subgradientResidual.doubleValue > 0.0 then
      Left(
        PalmConvergenceError.InvalidDefinition(
          s"exact nuclear-subgradient admission requires zero residual, got ${certificate.subgradientResidual.doubleValue}"
        )
      )
    else
      OptimizationAssumptions
        .from(
          certificate.bindings,
          properClosedConvex = Vector(certificate.loss),
          theoremAssumptions = certificate.theoremAssumptions
        )
        .left
        .map(PalmConvergenceError.Guarantee.apply)
        .flatMap: assumptions =>
          OptimizationGuaranteeAdmission
            .admit(
              contract,
              OptimizationClaimClass.ExactGlobal,
              assumptions,
              Set.empty,
              certificate.evidence,
              Some(certificate.globalWitness)
            )
            .left
            .map(PalmConvergenceError.Guarantee.apply)

private def validateInitialization(
    problem: PalmProblem,
    initialization: PalmInitialization
): Either[PalmConvergenceError, Unit] =
  val actual = initialization.state.blocks.map(_.parameter)
  if actual != problem.parameters then
    Left(
      PalmConvergenceError.InvalidDefinition(
        s"PALM initialization blocks ${actual.map(_.value).mkString(", ")} do not match problem order " +
          problem.parameters.map(_.value).mkString(", ")
      )
    )
  else Right(())

private def validateUpdate(
    previous: PalmBlockValue,
    update: PalmBlockUpdate
): Either[PalmConvergenceError, Unit] =
  if previous.values.rows != update.value.rows || previous.values.cols != update.value.cols then
    Left(
      PalmConvergenceError.BlockFailure(
        previous.parameter,
        s"update shape ${update.value.rows} x ${update.value.cols} does not match " +
          s"${previous.values.rows} x ${previous.values.cols}"
      )
    )
  else Right(())

private def matrixDistance(left: DMat, right: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < left.rows do
    var column = 0
    while column < left.cols do
      val difference = left(row, column) - right(row, column)
      squared += difference * difference
      column += 1
    row += 1
  Math.sqrt(squared)

private def firstNonFinite(matrix: DMat): Option[(Int, Int, Double)] =
  var row = 0
  var failure = Option.empty[(Int, Int, Double)]
  while row < matrix.rows && failure.isEmpty do
    var column = 0
    while column < matrix.cols && failure.isEmpty do
      val value = matrix(row, column)
      if !value.isFinite then failure = Some((row, column, value))
      column += 1
    row += 1
  failure

private def traversePalm[A, B](
    values: Vector[A]
)(
    function: A => Either[PalmConvergenceError, B]
): Either[PalmConvergenceError, Vector[B]] =
  values.foldLeft[Either[PalmConvergenceError, Vector[B]]](Right(Vector.empty)): (result, value) =>
    for
      accumulated <- result
      next <- function(value)
    yield accumulated :+ next
