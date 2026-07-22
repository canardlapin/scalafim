package scalafim.linalg

enum FirstOrderMethod:
  case ProximalGradient
  case ProjectedGradient
  case SmoothCompositePrimalDual
  case LinearCompositePrimalDual
  case ExactLinearReduction

enum SolverMethodRequest:
  case Automatic
  case Require(method: FirstOrderMethod)

enum FirstOrderError:
  case InvalidConfiguration(detail: String)
  case ShapeMismatch(context: String, expectedRows: Int, actualRows: Int)
  case NonFiniteValue(context: String, index: Int, value: Double)
  case OracleFailure(context: String, detail: String)
  case MissingCapability(
      requested: Option[FirstOrderMethod],
      compatible: Vector[FirstOrderMethod],
      available: Set[FirstOrderMethod]
  )
  case InvalidReduction(detail: String)

  def message: String =
    this match
      case InvalidConfiguration(detail) => detail
      case ShapeMismatch(context, expectedRows, actualRows) =>
        s"$context expected $expectedRows rows, got $actualRows"
      case NonFiniteValue(context, index, value) =>
        s"$context value at linear index $index is not finite: $value"
      case OracleFailure(context, detail) => s"$context failed: $detail"
      case MissingCapability(requested, compatible, available) =>
        val requestedText = requested.fold("automatic selection")(_.toString)
        s"$requestedText is unavailable; compatible methods are ${compatible.mkString(", ")}; " +
          s"available methods are ${available.toVector.sortBy(_.ordinal).mkString(", ")}"
      case InvalidReduction(detail) => detail

final case class FirstOrderCapabilities private (methods: Set[FirstOrderMethod]):
  def supports(method: FirstOrderMethod): Boolean =
    methods.contains(method)

  def select(
      compatible: Vector[FirstOrderMethod],
      request: SolverMethodRequest
  ): Either[FirstOrderError, FirstOrderMethod] =
    val distinct = compatible.distinct
    request match
      case SolverMethodRequest.Require(method) =>
        if distinct.contains(method) && supports(method) then Right(method)
        else Left(FirstOrderError.MissingCapability(Some(method), distinct, methods))
      case SolverMethodRequest.Automatic =>
        FirstOrderCapabilities.preference
          .find(method => distinct.contains(method) && supports(method))
          .toRight(FirstOrderError.MissingCapability(None, distinct, methods))

object FirstOrderCapabilities:
  private val preference = Vector(
    FirstOrderMethod.ExactLinearReduction,
    FirstOrderMethod.ProximalGradient,
    FirstOrderMethod.ProjectedGradient,
    FirstOrderMethod.SmoothCompositePrimalDual,
    FirstOrderMethod.LinearCompositePrimalDual
  )

  def from(methods: Set[FirstOrderMethod]): Either[FirstOrderError, FirstOrderCapabilities] =
    if methods.nonEmpty then Right(FirstOrderCapabilities(methods))
    else Left(FirstOrderError.InvalidConfiguration("solver capabilities must not be empty"))

  val portable: FirstOrderCapabilities =
    FirstOrderCapabilities(FirstOrderMethod.values.toSet)

final case class FirstOrderTolerance private (
    absolute: Double,
    relative: Double
):
  def threshold(scale: Double): Double =
    absolute + relative * Math.abs(scale)

object FirstOrderTolerance:
  def from(absolute: Double, relative: Double): Either[FirstOrderError, FirstOrderTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"absolute tolerance must be finite and non-negative, got $absolute"))
    else if !relative.isFinite || relative < 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"relative tolerance must be finite and non-negative, got $relative"))
    else if absolute == 0.0 && relative == 0.0 then
      Left(FirstOrderError.InvalidConfiguration("at least one stopping tolerance must be positive"))
    else Right(FirstOrderTolerance(absolute, relative))

  val strict: FirstOrderTolerance =
    FirstOrderTolerance(1e-8, 1e-8)

final case class FirstOrderConfig private (
    maxIterations: Int,
    tolerance: FirstOrderTolerance,
    stepSafety: Double,
    extrapolation: Double
)

object FirstOrderConfig:
  def from(
      maxIterations: Int,
      tolerance: FirstOrderTolerance,
      stepSafety: Double = 0.99,
      extrapolation: Double = 1.0
  ): Either[FirstOrderError, FirstOrderConfig] =
    if maxIterations <= 0 then
      Left(FirstOrderError.InvalidConfiguration(s"iteration budget must be positive, got $maxIterations"))
    else if !stepSafety.isFinite || stepSafety <= 0.0 || stepSafety >= 1.0 then
      Left(FirstOrderError.InvalidConfiguration(s"step safety must be finite and in (0, 1), got $stepSafety"))
    else if !extrapolation.isFinite || extrapolation < 0.0 || extrapolation > 1.0 then
      Left(FirstOrderError.InvalidConfiguration(s"extrapolation must be finite and in [0, 1], got $extrapolation"))
    else Right(FirstOrderConfig(maxIterations, tolerance, stepSafety, extrapolation))

  val portable: FirstOrderConfig =
    FirstOrderConfig(10000, FirstOrderTolerance.strict, 0.99, 1.0)

final case class ValueSummary(
    rows: Int,
    columns: Int,
    sum: Double,
    squaredNorm: Double,
    maxAbs: Double
)

object ValueSummary:
  def from(value: DoubleMatrix): ValueSummary =
    var sum = 0.0
    var squaredNorm = 0.0
    var maxAbs = 0.0
    var index = 0
    val data = value.dataArray
    while index < data.length do
      val current = data(index)
      sum += current
      squaredNorm += current * current
      maxAbs = Math.max(maxAbs, Math.abs(current))
      index += 1
    ValueSummary(value.rows, value.cols, sum, squaredNorm, maxAbs)

final case class FirstOrderSettings(
    method: FirstOrderMethod,
    maxIterations: Int,
    tolerance: FirstOrderTolerance,
    stepSafety: Double,
    extrapolation: Double
)

final case class FirstOrderCertificate(
    primal: ValueSummary,
    dual: Option[ValueSummary],
    objective: Double,
    primalResidual: Double,
    dualResidual: Double,
    objectiveChange: Double,
    iterations: Int,
    settings: FirstOrderSettings
):
  require(objective.isFinite, "certified objective must be finite")
  require(primalResidual.isFinite && primalResidual >= 0.0, "primal residual must be finite and non-negative")
  require(dualResidual.isFinite && dualResidual >= 0.0, "dual residual must be finite and non-negative")
  require(objectiveChange.isFinite && objectiveChange >= 0.0, "objective change must be finite and non-negative")
  require(iterations >= 0, "iteration count must be non-negative")

  def binds(primalValue: DoubleMatrix, dualValue: Option[DoubleMatrix]): Boolean =
    primal == ValueSummary.from(primalValue) && dual == dualValue.map(ValueSummary.from)

enum FirstOrderStoppingStatus:
  case Converged
  case IterationLimit

final case class FirstOrderSolution(
    primal: DoubleMatrix,
    dual: Option[DoubleMatrix],
    objective: Double,
    status: FirstOrderStoppingStatus,
    certificate: FirstOrderCertificate
):
  require(certificate.binds(primal, dual), "solver certificate must bind the returned values")

trait SmoothObjective:
  def variableRows: Int
  def lipschitz: Double
  def value(at: DoubleMatrix): Either[FirstOrderError, Double]
  def gradient(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix]

trait ProximalTerm:
  def variableRows: Int
  def value(at: DoubleMatrix): Either[FirstOrderError, Double]
  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix]

trait ProjectionSet:
  def variableRows: Int
  def project(at: DoubleMatrix): Either[FirstOrderError, DoubleMatrix]

trait ProximalObjective:
  def variableRows: Int
  def value(at: DoubleMatrix): Either[FirstOrderError, Double]
  def proximal(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix]

trait LinearCompositeFunctional:
  def targetRows: Int
  def value(at: DoubleMatrix): Either[FirstOrderError, Double]
  def proximalConjugate(at: DoubleMatrix, step: Double): Either[FirstOrderError, DoubleMatrix]

final case class BoundedLinearMap private (
    map: LinearMap,
    normUpperBound: Double
)

object BoundedLinearMap:
  def from(map: LinearMap, normUpperBound: Double): Either[FirstOrderError, BoundedLinearMap] =
    if !normUpperBound.isFinite || normUpperBound < 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"operator norm bound must be finite and non-negative, got $normUpperBound"))
    else Right(BoundedLinearMap(map, normUpperBound))

object FirstOrderSolvers:
  def proximalGradient(
      smooth: SmoothObjective,
      term: ProximalTerm,
      initial: DoubleMatrix,
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): Either[FirstOrderError, FirstOrderSolution] =
    if smooth.variableRows != term.variableRows then
      Left(FirstOrderError.ShapeMismatch("proximal term", smooth.variableRows, term.variableRows))
    else if !smooth.lipschitz.isFinite || smooth.lipschitz <= 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"smooth Lipschitz bound must be finite and positive, got ${smooth.lipschitz}"))
    else
      for
        _ <- validateMatrix("initial value", initial, smooth.variableRows)
        initialObjective <- combinedValue(smooth, term, initial)
        result <- iterateSingle(
          FirstOrderMethod.ProximalGradient,
          initial,
          initialObjective,
          config,
          config.stepSafety / smooth.lipschitz,
          value =>
            for
              gradient <- smooth.gradient(value)
              _ <- validateLike("smooth gradient", gradient, value)
              next <- term.proximal(subtract(value, scale(gradient, config.stepSafety / smooth.lipschitz)), config.stepSafety / smooth.lipschitz)
              _ <- validateLike("proximal result", next, value)
              objective <- combinedValue(smooth, term, next)
            yield next -> objective
        )
      yield result

  def projectedGradient(
      smooth: SmoothObjective,
      feasible: ProjectionSet,
      initial: DoubleMatrix,
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): Either[FirstOrderError, FirstOrderSolution] =
    if smooth.variableRows != feasible.variableRows then
      Left(FirstOrderError.ShapeMismatch("projection set", smooth.variableRows, feasible.variableRows))
    else if !smooth.lipschitz.isFinite || smooth.lipschitz <= 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"smooth Lipschitz bound must be finite and positive, got ${smooth.lipschitz}"))
    else
      val step = config.stepSafety / smooth.lipschitz
      for
        _ <- validateMatrix("initial value", initial, smooth.variableRows)
        initialObjective <- smooth.value(initial).flatMap(validateScalar("smooth objective", _))
        result <- iterateSingle(
          FirstOrderMethod.ProjectedGradient,
          initial,
          initialObjective,
          config,
          step,
          value =>
            for
              gradient <- smooth.gradient(value)
              _ <- validateLike("smooth gradient", gradient, value)
              next <- feasible.project(subtract(value, scale(gradient, step)))
              _ <- validateLike("projection result", next, value)
              objective <- smooth.value(next).flatMap(validateScalar("smooth objective", _))
            yield next -> objective
        )
      yield result

  def linearCompositePrimalDual(
      primalObjective: ProximalObjective,
      functional: LinearCompositeFunctional,
      operator: BoundedLinearMap,
      initial: DoubleMatrix,
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): Either[FirstOrderError, FirstOrderSolution] =
    if operator.map.cols != primalObjective.variableRows then
      Left(FirstOrderError.ShapeMismatch("linear-composite source", primalObjective.variableRows, operator.map.cols))
    else if operator.map.rows != functional.targetRows then
      Left(FirstOrderError.ShapeMismatch("linear-composite target", functional.targetRows, operator.map.rows))
    else
      for
        _ <- validateMatrix("initial value", initial, primalObjective.variableRows)
        mappedInitial <- forward(operator.map, initial, "linear-composite forward")
        initialObjective <- compositeValue(primalObjective, functional, initial, mappedInitial)
        result <- iteratePrimalDual(
          primalObjective,
          functional,
          operator,
          initial,
          initialObjective,
          config
        )
      yield result

  /** Condat--Vu splitting for `f(x) + h(x) + g(Kx)`.
    *
    * `f` is differentiable with a certified Lipschitz bound, `h` has an exact
    * proximal map, and `g` has an exact conjugate proximal map. The chosen
    * steps satisfy `tau < 2 / L` and `tau * sigma * ||K||^2 < 1` from the
    * supplied upper bound, including the zero-operator case.
    */
  def smoothCompositePrimalDual(
      smooth: SmoothObjective,
      direct: ProximalTerm,
      functional: LinearCompositeFunctional,
      operator: BoundedLinearMap,
      initial: DoubleMatrix,
      config: FirstOrderConfig = FirstOrderConfig.portable
  ): Either[FirstOrderError, FirstOrderSolution] =
    if smooth.variableRows != direct.variableRows then
      Left(FirstOrderError.ShapeMismatch("direct proximal term", smooth.variableRows, direct.variableRows))
    else if operator.map.cols != smooth.variableRows then
      Left(FirstOrderError.ShapeMismatch("smooth-composite source", smooth.variableRows, operator.map.cols))
    else if operator.map.rows != functional.targetRows then
      Left(FirstOrderError.ShapeMismatch("smooth-composite target", functional.targetRows, operator.map.rows))
    else if !smooth.lipschitz.isFinite || smooth.lipschitz <= 0.0 then
      Left(FirstOrderError.InvalidConfiguration(s"smooth Lipschitz bound must be finite and positive, got ${smooth.lipschitz}"))
    else
      for
        _ <- validateMatrix("initial value", initial, smooth.variableRows)
        mappedInitial <- forward(operator.map, initial, "smooth-composite forward")
        initialObjective <- smoothCompositeValue(smooth, direct, functional, initial, mappedInitial)
        result <- iterateSmoothComposite(
          smooth,
          direct,
          functional,
          operator,
          initial,
          initialObjective,
          config
        )
      yield result

  private def iterateSingle(
      method: FirstOrderMethod,
      initial: DoubleMatrix,
      initialObjective: Double,
      config: FirstOrderConfig,
      step: Double,
      update: DoubleMatrix => Either[FirstOrderError, (DoubleMatrix, Double)]
  ): Either[FirstOrderError, FirstOrderSolution] =
    var current = initial
    var objective = initialObjective
    var residual = Double.MaxValue
    var objectiveChange = Double.MaxValue
    var iteration = 0
    var converged = false
    var error = Option.empty[FirstOrderError]
    while iteration < config.maxIterations && !converged && error.isEmpty do
      update(current) match
        case Left(value) => error = Some(value)
        case Right((next, nextObjective)) =>
          residual = maxAbs(subtract(next, current)) / step
          objectiveChange = Math.abs(nextObjective - objective)
          current = next
          objective = nextObjective
          iteration += 1
          val scaleValue = Math.max(1.0, ValueSummary.from(current).maxAbs)
          converged = residual <= config.tolerance.threshold(scaleValue) &&
            objectiveChange <= config.tolerance.threshold(Math.max(1.0, Math.abs(objective)))
    error match
      case Some(value) => Left(value)
      case None =>
        val status = if converged then FirstOrderStoppingStatus.Converged else FirstOrderStoppingStatus.IterationLimit
        val currentCertificate = certificate(
          method,
          current,
          None,
          objective,
          residual,
          0.0,
          objectiveChange,
          iteration,
          config
        )
        Right(FirstOrderSolution(current, None, objective, status, currentCertificate))

  private def iteratePrimalDual(
      primalObjective: ProximalObjective,
      functional: LinearCompositeFunctional,
      operator: BoundedLinearMap,
      initial: DoubleMatrix,
      initialObjective: Double,
      config: FirstOrderConfig
  ): Either[FirstOrderError, FirstOrderSolution] =
    val denominator = Math.max(1.0, operator.normUpperBound)
    val primalStep = config.stepSafety / denominator
    val dualStep = config.stepSafety / denominator
    var primal = initial
    var extrapolated = initial
    var dual = DoubleMatrix.zeros(operator.map.rows, initial.cols)
    var objective = initialObjective
    var primalResidual = Double.MaxValue
    var dualResidual = Double.MaxValue
    var objectiveChange = Double.MaxValue
    var iteration = 0
    var converged = false
    var error = Option.empty[FirstOrderError]
    while iteration < config.maxIterations && !converged && error.isEmpty do
      val updated =
        for
          mapped <- forward(operator.map, extrapolated, "linear-composite forward")
          nextDual <- functional.proximalConjugate(add(dual, scale(mapped, dualStep)), dualStep)
          _ <- validateLike("dual proximal result", nextDual, dual)
          adjoint <- forward(operator.map.adjoint, nextDual, "linear-composite adjoint")
          nextPrimal <- primalObjective.proximal(subtract(primal, scale(adjoint, primalStep)), primalStep)
          _ <- validateLike("primal proximal result", nextPrimal, primal)
          mappedNext <- forward(operator.map, nextPrimal, "linear-composite forward")
          nextObjective <- compositeValue(primalObjective, functional, nextPrimal, mappedNext)
          residuals <- fixedPointResiduals(
            primalObjective,
            functional,
            operator.map,
            nextPrimal,
            nextDual,
            primalStep,
            dualStep
          )
        yield (nextPrimal, nextDual, nextObjective, residuals._1, residuals._2)
      updated match
        case Left(value) => error = Some(value)
        case Right((nextPrimal, nextDual, nextObjective, nextPrimalResidual, nextDualResidual)) =>
          extrapolated = add(
            nextPrimal,
            scale(subtract(nextPrimal, primal), config.extrapolation)
          )
          primal = nextPrimal
          dual = nextDual
          primalResidual = nextPrimalResidual
          dualResidual = nextDualResidual
          objectiveChange = Math.abs(nextObjective - objective)
          objective = nextObjective
          iteration += 1
          val primalScale = Math.max(1.0, ValueSummary.from(primal).maxAbs)
          val dualScale = Math.max(1.0, ValueSummary.from(dual).maxAbs)
          converged = primalResidual <= config.tolerance.threshold(primalScale) &&
            dualResidual <= config.tolerance.threshold(dualScale) &&
            objectiveChange <= config.tolerance.threshold(Math.max(1.0, Math.abs(objective)))
    error match
      case Some(value) => Left(value)
      case None =>
        val status = if converged then FirstOrderStoppingStatus.Converged else FirstOrderStoppingStatus.IterationLimit
        val currentCertificate = certificate(
          FirstOrderMethod.LinearCompositePrimalDual,
          primal,
          Some(dual),
          objective,
          primalResidual,
          dualResidual,
          objectiveChange,
          iteration,
          config
        )
        Right(FirstOrderSolution(primal, Some(dual), objective, status, currentCertificate))

  private def iterateSmoothComposite(
      smooth: SmoothObjective,
      direct: ProximalTerm,
      functional: LinearCompositeFunctional,
      operator: BoundedLinearMap,
      initial: DoubleMatrix,
      initialObjective: Double,
      config: FirstOrderConfig
  ): Either[FirstOrderError, FirstOrderSolution] =
    val normBound = operator.normUpperBound
    val primalStep = config.stepSafety / (smooth.lipschitz + normBound)
    val dualStep = if normBound == 0.0 then 1.0 else config.stepSafety / normBound
    var primal = initial
    var extrapolated = initial
    var dual = DoubleMatrix.zeros(operator.map.rows, initial.cols)
    var objective = initialObjective
    var primalResidual = Double.MaxValue
    var dualResidual = Double.MaxValue
    var objectiveChange = Double.MaxValue
    var iteration = 0
    var converged = false
    var error = Option.empty[FirstOrderError]
    while iteration < config.maxIterations && !converged && error.isEmpty do
      val updated =
        for
          mapped <- forward(operator.map, extrapolated, "smooth-composite forward")
          nextDual <- functional.proximalConjugate(add(dual, scale(mapped, dualStep)), dualStep)
          _ <- validateLike("smooth-composite dual proximal result", nextDual, dual)
          gradient <- smooth.gradient(primal)
          _ <- validateLike("smooth-composite gradient", gradient, primal)
          adjoint <- forward(operator.map.adjoint, nextDual, "smooth-composite adjoint")
          nextPrimal <- direct.proximal(
            subtract(primal, scale(add(gradient, adjoint), primalStep)),
            primalStep
          )
          _ <- validateLike("smooth-composite primal proximal result", nextPrimal, primal)
          mappedNext <- forward(operator.map, nextPrimal, "smooth-composite forward")
          nextObjective <- smoothCompositeValue(smooth, direct, functional, nextPrimal, mappedNext)
          residuals <- smoothCompositeResiduals(
            smooth,
            direct,
            functional,
            operator.map,
            nextPrimal,
            nextDual,
            primalStep,
            dualStep
          )
        yield (nextPrimal, nextDual, nextObjective, residuals._1, residuals._2)
      updated match
        case Left(value) => error = Some(value)
        case Right((nextPrimal, nextDual, nextObjective, nextPrimalResidual, nextDualResidual)) =>
          extrapolated = add(nextPrimal, scale(subtract(nextPrimal, primal), config.extrapolation))
          primal = nextPrimal
          dual = nextDual
          primalResidual = nextPrimalResidual
          dualResidual = nextDualResidual
          objectiveChange = Math.abs(nextObjective - objective)
          objective = nextObjective
          iteration += 1
          val primalScale = Math.max(1.0, ValueSummary.from(primal).maxAbs)
          val dualScale = Math.max(1.0, ValueSummary.from(dual).maxAbs)
          converged = primalResidual <= config.tolerance.threshold(primalScale) &&
            dualResidual <= config.tolerance.threshold(dualScale) &&
            objectiveChange <= config.tolerance.threshold(Math.max(1.0, Math.abs(objective)))
    error match
      case Some(value) => Left(value)
      case None =>
        val status = if converged then FirstOrderStoppingStatus.Converged else FirstOrderStoppingStatus.IterationLimit
        val currentCertificate = certificate(
          FirstOrderMethod.SmoothCompositePrimalDual,
          primal,
          Some(dual),
          objective,
          primalResidual,
          dualResidual,
          objectiveChange,
          iteration,
          config
        )
        Right(FirstOrderSolution(primal, Some(dual), objective, status, currentCertificate))

  private def smoothCompositeResiduals(
      smooth: SmoothObjective,
      direct: ProximalTerm,
      functional: LinearCompositeFunctional,
      operator: LinearMap,
      primal: DoubleMatrix,
      dual: DoubleMatrix,
      primalStep: Double,
      dualStep: Double
  ): Either[FirstOrderError, (Double, Double)] =
    for
      gradient <- smooth.gradient(primal)
      _ <- validateLike("smooth-composite certificate gradient", gradient, primal)
      adjoint <- forward(operator.adjoint, dual, "smooth-composite certificate adjoint")
      primalFixed <- direct.proximal(
        subtract(primal, scale(add(gradient, adjoint), primalStep)),
        primalStep
      )
      _ <- validateLike("smooth-composite certificate primal proximal", primalFixed, primal)
      mapped <- forward(operator, primal, "smooth-composite certificate forward")
      dualFixed <- functional.proximalConjugate(add(dual, scale(mapped, dualStep)), dualStep)
      _ <- validateLike("smooth-composite certificate dual proximal", dualFixed, dual)
    yield (
      maxAbs(subtract(primal, primalFixed)) / primalStep,
      maxAbs(subtract(dual, dualFixed)) / dualStep
    )

  private def fixedPointResiduals(
      primalObjective: ProximalObjective,
      functional: LinearCompositeFunctional,
      operator: LinearMap,
      primal: DoubleMatrix,
      dual: DoubleMatrix,
      primalStep: Double,
      dualStep: Double
  ): Either[FirstOrderError, (Double, Double)] =
    for
      adjoint <- forward(operator.adjoint, dual, "certificate adjoint")
      primalFixed <- primalObjective.proximal(subtract(primal, scale(adjoint, primalStep)), primalStep)
      _ <- validateLike("certificate primal proximal", primalFixed, primal)
      mapped <- forward(operator, primal, "certificate forward")
      dualFixed <- functional.proximalConjugate(add(dual, scale(mapped, dualStep)), dualStep)
      _ <- validateLike("certificate dual proximal", dualFixed, dual)
    yield (
      maxAbs(subtract(primal, primalFixed)) / primalStep,
      maxAbs(subtract(dual, dualFixed)) / dualStep
    )

  private def certificate(
      method: FirstOrderMethod,
      primal: DoubleMatrix,
      dual: Option[DoubleMatrix],
      objective: Double,
      primalResidual: Double,
      dualResidual: Double,
      objectiveChange: Double,
      iterations: Int,
      config: FirstOrderConfig
  ): FirstOrderCertificate =
    FirstOrderCertificate(
      ValueSummary.from(primal),
      dual.map(ValueSummary.from),
      objective,
      primalResidual,
      dualResidual,
      objectiveChange,
      iterations,
      FirstOrderSettings(
        method,
        config.maxIterations,
        config.tolerance,
        config.stepSafety,
        config.extrapolation
      )
    )

  private def combinedValue(
      smooth: SmoothObjective,
      term: ProximalTerm,
      at: DoubleMatrix
  ): Either[FirstOrderError, Double] =
    for
      left <- smooth.value(at).flatMap(validateScalar("smooth objective", _))
      right <- term.value(at).flatMap(validateScalar("proximal term", _))
      result <- validateScalar("combined objective", left + right)
    yield result

  private def compositeValue(
      primal: ProximalObjective,
      functional: LinearCompositeFunctional,
      at: DoubleMatrix,
      mapped: DoubleMatrix
  ): Either[FirstOrderError, Double] =
    for
      left <- primal.value(at).flatMap(validateScalar("primal objective", _))
      right <- functional.value(mapped).flatMap(validateScalar("composite functional", _))
      result <- validateScalar("composite objective", left + right)
    yield result

  private def smoothCompositeValue(
      smooth: SmoothObjective,
      direct: ProximalTerm,
      functional: LinearCompositeFunctional,
      at: DoubleMatrix,
      mapped: DoubleMatrix
  ): Either[FirstOrderError, Double] =
    for
      smoothValue <- smooth.value(at).flatMap(validateScalar("smooth objective", _))
      directValue <- direct.value(at).flatMap(validateScalar("direct proximal term", _))
      compositeValue <- functional.value(mapped).flatMap(validateScalar("linear-composite functional", _))
      result <- validateScalar("smooth-composite objective", smoothValue + directValue + compositeValue)
    yield result

  private def forward(
      map: LinearMap,
      value: DoubleMatrix,
      context: String
  ): Either[FirstOrderError, DoubleMatrix] =
    map.forward(value)
      .left
      .map(error => FirstOrderError.OracleFailure(context, error.message))
      .flatMap: result =>
        if result.cols != value.cols then
          Left(
            FirstOrderError.InvalidConfiguration(
              s"$context expected ${value.cols} columns, got ${result.cols}"
            )
          )
        else validateMatrix(context, result, map.rows).map(_ => result)

object ExactLinearReduction:
  def verify(
      basis: LinearMap,
      constraint: LinearMap,
      tolerance: FirstOrderTolerance
  ): Either[FirstOrderError, LinearReductionCertificate] =
    if basis.rows != constraint.cols then
      Left(
        FirstOrderError.InvalidReduction(
          s"basis target dimension ${basis.rows} does not match constraint source dimension ${constraint.cols}"
        )
      )
    else
      for
        image <- basis.forward(DoubleMatrix.eye(basis.cols)).left.map: error =>
          FirstOrderError.OracleFailure("null-space basis", error.message)
        residualValue <- constraint.forward(image).left.map: error =>
          FirstOrderError.OracleFailure("null-space constraint", error.message)
        _ <-
          if residualValue.cols != basis.cols then
            Left(
              FirstOrderError.InvalidReduction(
                s"null-space residual expected ${basis.cols} columns, got ${residualValue.cols}"
              )
            )
          else validateMatrix("null-space residual", residualValue, constraint.rows)
        residual = maxAbs(residualValue)
        threshold = tolerance.threshold(Math.max(1.0, ValueSummary.from(image).maxAbs))
        _ <-
          if residual <= threshold then Right(())
          else Left(FirstOrderError.InvalidReduction(s"null-space residual $residual exceeds threshold $threshold"))
      yield
        LinearReductionCertificate(
          basis.cols,
          basis.rows,
          constraint.rows,
          ValueSummary.from(image),
          ValueSummary.from(residualValue),
          residual,
          threshold,
          tolerance
        )

final case class LinearReductionCertificate(
    freeRows: Int,
    semanticRows: Int,
    constraintRows: Int,
    basisImage: ValueSummary,
    constraintImage: ValueSummary,
    residual: Double,
    threshold: Double,
    tolerance: FirstOrderTolerance
):
  require(residual.isFinite && residual >= 0.0, "reduction residual must be finite and non-negative")
  require(threshold.isFinite && threshold >= 0.0, "reduction threshold must be finite and non-negative")

private def validateMatrix(
    context: String,
    value: DoubleMatrix,
    expectedRows: Int
): Either[FirstOrderError, Unit] =
  if value.rows != expectedRows then
    Left(FirstOrderError.ShapeMismatch(context, expectedRows, value.rows))
  else
    var index = 0
    var error = Option.empty[FirstOrderError]
    while index < value.dataArray.length && error.isEmpty do
      val current = value.dataArray(index)
      if !current.isFinite then error = Some(FirstOrderError.NonFiniteValue(context, index, current))
      index += 1
    error.toLeft(())

private def validateLike(
    context: String,
    actual: DoubleMatrix,
    expected: DoubleMatrix
): Either[FirstOrderError, Unit] =
  if actual.cols != expected.cols then
    Left(FirstOrderError.InvalidConfiguration(s"$context expected ${expected.cols} columns, got ${actual.cols}"))
  else validateMatrix(context, actual, expected.rows)

private def validateScalar(context: String, value: Double): Either[FirstOrderError, Double] =
  if value.isFinite then Right(value)
  else Left(FirstOrderError.NonFiniteValue(context, 0, value))

private def add(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
  val output = left.copyData
  val rightData = right.dataArray
  var index = 0
  while index < output.length do
    output(index) += rightData(index)
    index += 1
  DoubleMatrix.unsafe(left.rows, left.cols, output)

private def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
  val output = left.copyData
  val rightData = right.dataArray
  var index = 0
  while index < output.length do
    output(index) -= rightData(index)
    index += 1
  DoubleMatrix.unsafe(left.rows, left.cols, output)

private def scale(value: DoubleMatrix, factor: Double): DoubleMatrix =
  val output = value.copyData
  var index = 0
  while index < output.length do
    output(index) *= factor
    index += 1
  DoubleMatrix.unsafe(value.rows, value.cols, output)

private def maxAbs(value: DoubleMatrix): Double =
  var result = 0.0
  var index = 0
  while index < value.dataArray.length do
    result = Math.max(result, Math.abs(value.dataArray(index)))
    index += 1
  result
