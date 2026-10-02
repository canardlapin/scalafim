package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.optim.*
import gale.spectral.{Eigen, EigenSelection, SingularSelection, Svds}
import scalafim.fmri.mvpa.{AxisRef, MultiResponse, Observations}

/** Replay is a caller declaration bound to the supplied evidence. This fitter
  * never grants repeated access to a one-shot source implicitly.
  */
enum PatternReplay:
  case Repeatable(receipt: String)
  case SinglePass

enum StructuredPatternError:
  case Invalid(detail: String)
  case AxisMismatch(field: String)
  case Budget(resource: String, required: BigInt, allowed: Long)
  case Evidence(stage: String, detail: String)
  case Covariance(error: ResidualCovarianceError)
  case Solver(error: FirstOrderError)
  case Artifact(error: PatternArtifactError)
  case ObjectiveIncreased(previous: Double, current: Double)

final class StructuredPatternConfig private (
    val penalty: SupportPenalty, val ridge: Double,
    val maximumOuterIterations: Int, val inner: FirstOrderConfig,
    val stationarityTolerance: Double, val objectiveTolerance: Double,
    val maximumWorkspaceCells: Long, val maximumOperatorColumns: Long,
    val maximumTargetDimension: Int
)
object StructuredPatternConfig:
  def apply(penalty: SupportPenalty, ridge: Double = 0.0,
      maximumOuterIterations: Int = 100, inner: FirstOrderConfig = FirstOrderConfig.portable,
      stationarityTolerance: Double = 1e-6, objectiveTolerance: Double = 1e-8,
      maximumWorkspaceCells: Long = 10000000L, maximumOperatorColumns: Long = 100000L,
      maximumTargetDimension: Int = 64): Either[StructuredPatternError, StructuredPatternConfig] =
    if !ridge.isFinite || ridge < 0.0 || maximumOuterIterations < 1 ||
        !stationarityTolerance.isFinite || stationarityTolerance <= 0.0 ||
        !objectiveTolerance.isFinite || objectiveTolerance <= 0.0 ||
        maximumWorkspaceCells < 0 || maximumOperatorColumns < 0 || maximumTargetDimension < 1 then
      Left(StructuredPatternError.Invalid("penalties, tolerances and resource/iteration limits must be finite and valid"))
    else Right(new StructuredPatternConfig(penalty, ridge, maximumOuterIterations, inner,
      stationarityTolerance, objectiveTolerance, maximumWorkspaceCells, maximumOperatorColumns, maximumTargetDimension))

enum StructuredPatternStopping:
  case Converged
  case IterationLimit

/** Every residual is evaluated at the same returned A/g/C, after the C step.
  * The shifted objective omits the fixed X-Psi-inverse-X term and may be negative.
  */
final case class StructuredPatternIteration(
    iteration: Int, shiftedObjective: Double, objectiveChange: Double,
    aStationarity: Double, dualStationarity: Double, cStationarity: Double,
    coneViolation: Double, orthogonalityViolation: Double,
    aIterations: Int, cIterations: Int,
    aStopping: FirstOrderStoppingStatus, cStopping: FirstOrderStoppingStatus
)
final case class StructuredPatternWork(
    plannedWorkspaceCells: Long, plannedOperatorColumns: Long,
    forwardColumns: Long, adjointColumns: Long, targetColumns: Long,
    precisionApplications: Long, capacitanceConditionLowerBound: Double, diagonalVarianceRatio: Double,
    initialization: String
)
/** Deterministic fitter admission. This is metadata-only: it does not apply
  * either evidence operator or fit/refit a covariance.
  */
final case class StructuredPatternAdmission(
    sampleCount: Int, neuralCount: Int, targetCount: Int, componentCount: Int,
    workspaceCells: Long, maximumOperatorColumns: Long,
    targetOperatorColumns: Long, precisionPeakCells: Long
)
final class StructuredPatternResult[N, Q, R] private[pattern] (
    val factors: PatternFactors[N, Q, R], val envelope: Vector[Double],
    val covariance: ResidualCovariance[N], val iterations: Vector[StructuredPatternIteration],
    val stopping: StructuredPatternStopping, val work: StructuredPatternWork,
    val artifact: Option[PatternArtifact]
)

/** Fixed-Psi structured reduced-rank regression. No covariance refitting or
  * population interpretation is asserted. Main X products have width r; neither
  * the neural effect matrix nor a p-by-p covariance is formed. Target coordinates
  * are explicitly bounded: Y and q-by-q target Grams are retained. Initialization
  * uses the right Gram of X^T Y/n, accumulated one column at a time; squaring the
  * singular spectrum can refuse ill-conditioned starts. Gale owns all generic
  * decompositions and numerical iterations. Workspace bounds cover this adapter's
  * blocks, retained numeric history and covariance products, excluding provider-internal scratch/object
  * overhead and cumulative allocation; they are not process-memory measurements.
  */
object StructuredPatternOptimizer:
  private type Result[A] = Either[StructuredPatternError, A]

  def admit[SK, NK, QK, RK](samples: AxisRef[SK], neural: AxisRef[NK], targetAxis: AxisRef[QK], components: AxisRef[RK])(
      observations: Observations[samples.Id, neural.Id], targets: MultiResponse[samples.Id, targetAxis.Id],
      covariance: ResidualCovariance[NK], graph: SupportGraph, geometry: TargetGeometry,
      centering: CenteringPolicy, config: StructuredPatternConfig, binding: TrainingBinding,
      lineage: Vector[String], replay: PatternReplay,
      initial: Option[PatternFactors[NK, QK, ?]] = None
  ): Result[StructuredPatternAdmission] =
    val n = samples.size
    val p = neural.size
    val q = targetAxis.size
    val r = components.size
    val targetDescriptor = geometry match
      case TargetGeometry.Categorical(value) => value.targetAxis.descriptor
      case TargetGeometry.Continuous(value) => value.targetAxis.descriptor
    val coordinatesAdmitted = geometry match
      case TargetGeometry.Categorical(_) => true
      case TargetGeometry.Continuous(value) => value.metricDiagonal.values.forall(_ == 1.0) &&
        value.blockWeights.size == 1 && value.blockWeights.head._2.values.forall(_ == 1.0)
    val centered = centering match
      case CenteringPolicy.CenteredBeforeFit(x, y) => x.trim.nonEmpty && y.trim.nonEmpty
      case _ => false
    val repeatable = replay match
      case PatternReplay.Repeatable(receipt) => receipt.trim.nonEmpty
      case _ => false
    val precisionPlan = covariance.precisionWork(math.max(1, r))
    val precisionCells = precisionPlan.map(_.peakCellsUpperBound)
    val workspace = BigInt(128) * p * (r + 1L) + BigInt(64) * n * math.max(q, r) +
      BigInt(64) * q * q + BigInt(64) * graph.edges.size +
      BigInt(16) * config.maximumOuterIterations + precisionCells.getOrElse(Long.MaxValue)
    val columns = BigInt(1) + 2L * q + BigInt(r) * (1L + 3L * config.maximumOuterIterations)
    if observations.sampleAxis != samples.descriptor || targets.sampleAxis != samples.descriptor ||
        binding.declaredSampleAxis != samples.descriptor then Left(StructuredPatternError.AxisMismatch("training samples"))
    else if observations.neuralAxis != neural.descriptor || graph.axis != neural.descriptor ||
        covariance.neuralAxis.descriptor != neural.descriptor then Left(StructuredPatternError.AxisMismatch("neural coordinates"))
    else if targets.featureAxis != targetAxis.descriptor || targetDescriptor != targetAxis.descriptor then
      Left(StructuredPatternError.AxisMismatch("target coordinates"))
    else if n < 2 || r < 1 || r > math.min(p, q) then Left(StructuredPatternError.Invalid("invalid sample count or requested rank"))
    else if !coordinatesAdmitted then Left(StructuredPatternError.Invalid("this objective admits unit continuous metric and one unit-weight block; transform other coordinates explicitly"))
    else if !centered then Left(StructuredPatternError.Invalid("centered neural and target coordinates with receipts are required"))
    else if !repeatable then Left(StructuredPatternError.Invalid("a repeatable evidence receipt is required"))
    else if lineage.isEmpty || lineage.exists(_.trim.isEmpty) then Left(StructuredPatternError.Invalid("training lineage is required"))
    else if q > config.maximumTargetDimension then Left(StructuredPatternError.Budget("target dimension", BigInt(q), config.maximumTargetDimension.toLong))
    else if workspace > config.maximumWorkspaceCells || BigInt(p) * (r + 1L) > Int.MaxValue || BigInt(n) * q > Int.MaxValue || BigInt(q) * q > Int.MaxValue ||
        precisionPlan.exists(work => BigInt(work.largestDenseRows) * work.largestDenseColumns > Int.MaxValue) then
      Left(StructuredPatternError.Budget("workspace cells", workspace, config.maximumWorkspaceCells))
    else if columns + q > config.maximumOperatorColumns then Left(StructuredPatternError.Budget("source columns", columns + q, config.maximumOperatorColumns))
    else if initial.exists(value => value.neuralAxis.descriptor != neural.descriptor || value.targetAxis.descriptor != targetAxis.descriptor ||
        value.componentAxis.size > r ||
        (value.componentAxis.size == r && value.componentAxis.descriptor != components.descriptor) ||
        (0 until value.componentAxis.size).exists(index => value.componentAxis.index.stableKeyAt(index) != components.index.stableKeyAt(index))) then
      Left(StructuredPatternError.AxisMismatch("warm start"))
    else if initial.exists(value => orthogonalityViolation(value.targetByComponent) > config.stationarityTolerance) then
      Left(StructuredPatternError.Invalid("warm-start target factors must already be orthonormal; silently rotating C would change the fitted mean"))
    else Right(StructuredPatternAdmission(n, p, q, r, workspace.toLong, (columns + q).toLong, q.toLong, precisionCells.getOrElse(Long.MaxValue)))

  def fit[SK, NK, QK, RK](samples: AxisRef[SK], neural: AxisRef[NK], targetAxis: AxisRef[QK], components: AxisRef[RK])(
      observations: Observations[samples.Id, neural.Id], targets: MultiResponse[samples.Id, targetAxis.Id],
      covariance: ResidualCovariance[NK], graph: SupportGraph, geometry: TargetGeometry,
      centering: CenteringPolicy, config: StructuredPatternConfig, binding: TrainingBinding,
      lineage: Vector[String], replay: PatternReplay,
      degenerate: DegenerateTargetPolicy = DegenerateTargetPolicy.Refuse,
      initial: Option[PatternFactors[NK, QK, ?]] = None
  ): Result[StructuredPatternResult[NK, QK, RK]] =
    admit(samples, neural, targetAxis, components)(observations, targets, covariance, graph, geometry, centering,
      config, binding, lineage, replay, initial).flatMap: admitted =>
      val n = admitted.sampleCount
      val p = admitted.neuralCount
      val q = admitted.targetCount
      val r = admitted.componentCount
      val workspace = admitted.workspaceCells
      var forwardColumns = 0L
      var adjointColumns = 0L
      var precisionCalls = 0L
      def xApply(value: DMat, transpose: Boolean): Result[DMat] =
        if transpose then adjointColumns += value.cols else forwardColumns += value.cols
        val result = if transpose then observations.patterns.star(value) else observations.patterns(value)
        result.left.map(error => StructuredPatternError.Evidence("neural operator", error.toString)).flatMap: matrix =>
          if ResidualCovariance.finite(matrix) then Right(matrix)
          else Left(StructuredPatternError.Invalid("nonfinite neural operator output"))
      def precision(value: DMat): Result[DMat] =
        precisionCalls += 1L
        covariance.applyPrecision(value).left.map(StructuredPatternError.Covariance.apply)
      def spectralStart(y: DMat): Result[DMat] =
        val gram = new Array[Double](q * q)
        var column = 0
        var error: Option[StructuredPatternError] = None
        while column < q && error.isEmpty do
          val yColumn = DMat.tabulate(n, 1)((row, _) => y(row, column) / n)
          val product = for
            k <- xApply(yColumn, true)
            xk <- xApply(k, false)
          yield scale(y.t * xk, 1.0 / n)
          product match
            case Left(value) => error = Some(value)
            case Right(value) =>
              var row = 0
              while row < q do
                gram(row * q + column) = value(row, 0)
                row += 1
          column += 1
        error match
          case Some(value) => Left(value)
          case None =>
            val symmetric = DMat.tabulate(q, q)((row, col) => 0.5 * (gram(row * q + col) + gram(col * q + row)))
            Eigen.eigSymmetric(symmetric, EigenSelection.All).left.map(error => StructuredPatternError.Evidence("supervised target Gram", error.toString))
              .flatMap(_.requireConverged.left.map(error => StructuredPatternError.Evidence("supervised target Gram convergence", error.toString)))
              .flatMap: decomposition =>
                val largest = decomposition.eigenvalues(q - 1)
                if !largest.isFinite || largest <= 0.0 || decomposition.eigenvalues(q - r) <= 1e-12 * largest then
                  Left(StructuredPatternError.Invalid("supervised initialization has insufficient numerical rank at relative Gram tolerance 1e-12"))
                else Right(DMat.tabulate(q, r)((row, column) => decomposition.eigenvectors(row, q - 1 - column)))
      val result = for
        y <- targets.targets(DMat.tabulate(q, q)((row, column) => if row == column then 1.0 else 0.0))
          .left.map(error => StructuredPatternError.Evidence("target operator", error.toString))
        _ <- if ResidualCovariance.finite(y) && isCentered(y) then Right(()) else Left(StructuredPatternError.Invalid("target values must be finite and centered"))
        _ <- if degenerate == DegenerateTargetPolicy.Refuse && !TargetGeometry.fullColumnRank(y, 1e-12) then
          Left(StructuredPatternError.Invalid("rank-deficient target coordinates")) else Right(())
        means <- xApply(DMat.tabulate(n, 1)((_, _) => 1.0 / n), true)
        _ <- if maxAbs(means) <= 1e-10 then Right(()) else Left(StructuredPatternError.Invalid("neural values must be centered (absolute mean tolerance 1e-10)"))
        cold <- if initial.exists(_.componentAxis.size == r) then Right(initial.get.targetByComponent) else spectralStart(y)
        startC <- initial match
          case Some(value) if value.componentAxis.size < r => appendComplement(value.targetByComponent, cold)
          case Some(value) => Right(value.targetByComponent)
          case None => Right(cold)
        startA = initial.fold(DMat.zeros(p, r))(value => DMat.tabulate(p, r)((row, col) => if col < value.componentAxis.size then value.neuralByComponent(row, col) else 0.0))
        fitted <- alternate(y, startA, startC, covariance.diagonalValues.min, graph, config, xApply, precision)
        (a, g, c, trace, stopping) = fitted
        factors <- PatternFactors(neural, targetAxis, components, a, c, GaugeEvidence.PendingNumericalCheck).left.map(StructuredPatternError.Artifact.apply)
        diagnostics <- PatternFitDiagnostics(trace.map(_.shiftedObjective), "Gale structured alternating fixed-Psi fit",
          Vector("objective excludes the fixed data-only constant", "nonconvex stationary fit; no global optimum claim",
            "target Gram initialization squares conditioning", s"stopping=$stopping"))
          .left.map(StructuredPatternError.Artifact.apply)
        artifact <- if stopping == StructuredPatternStopping.Converged then
          PatternArtifact(factors, geometry, centering, degenerate,
            ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, covariance.rank), binding, lineage, diagnostics)
            .left.map(StructuredPatternError.Artifact.apply).map(Some(_))
          else Right(None)
      yield new StructuredPatternResult(factors, g, covariance, trace, stopping,
        StructuredPatternWork(workspace, admitted.maximumOperatorColumns, forwardColumns, adjointColumns, q,
          precisionCalls, covariance.capacitanceConditionEstimate, covariance.diagonalValues.max / covariance.diagonalValues.min,
          if initial.isEmpty then "supervised streamed right Gram" else if initial.get.componentAxis.size < r then "rank-expanded warm start" else "provided warm start"), artifact)
      result

  private def alternate(y: DMat, startA: DMat, startC: DMat, minimumDiagonal: Double,
      graph: SupportGraph, config: StructuredPatternConfig,
      xApply: (DMat, Boolean) => Result[DMat], precision: DMat => Result[DMat]
  ): Result[(DMat, Vector[Double], DMat, Vector[StructuredPatternIteration], StructuredPatternStopping)] =
    val n = y.rows
    val p = startA.rows
    val r = startA.cols
    val s = scale(y.t * y, 1.0 / n)
    var a = startA
    var c = startC
    var g = Vector.tabulate(p)(row => rowNorm(a, row))
    var previous = 0.0
    var trace = Vector.empty[StructuredPatternIteration]
    var iteration = 0
    var converged = false
    var failure: Option[StructuredPatternError] = None
    def aProblem(atC: DMat): Result[LoadingProblem] =
      val t = y * atC
      xApply(scale(t, 1.0 / n), true).map(b => new LoadingProblem(scale(t.t * t, 1.0 / n), b,
        minimumDiagonal, graph, config, precision))
    aProblem(c).flatMap(_.objective(a, g)) match
      case Left(error) => failure = Some(error)
      case Right(value) => previous = value
    while iteration < config.maximumOuterIterations && !converged && failure.isEmpty do
      val update = for
        conditional <- aProblem(c)
        solvedA <- conditional.solve(a, g)
        nextA = conditional.loadings(solvedA.primal)
        nextG = conditional.envelope(solvedA.primal)
        pa <- precision(nextA)
        xpa <- xApply(pa, false)
        cProblem = new TargetProblem(s, nextA.t * pa, scale(y.t * xpa, 1.0 / n), config)
        solvedC <- cProblem.solve(c)
        finalC = solvedC.primal
        finalProblem <- aProblem(finalC)
        objective <- finalProblem.objective(nextA, nextG)
        residuals <- finalProblem.residuals(solvedA.primal, solvedA.dual)
        cResidual <- cProblem.residual(finalC)
      yield (nextA, nextG, finalC, objective, residuals, cResidual, solvedA, solvedC)
      update match
        case Left(error) => failure = Some(error)
        case Right((nextA, nextG, finalC, objective, residuals, cResidual, solvedA, solvedC)) =>
          val change = math.abs(objective - previous)
          if objective > previous + config.objectiveTolerance * (1.0 + math.abs(previous)) then
            failure = Some(StructuredPatternError.ObjectiveIncreased(previous, objective))
          else
            a = nextA
            g = nextG
            c = finalC
            iteration += 1
            val violation = coneViolation(a, g)
            val orthogonality = orthogonalityViolation(c)
            val evidence = StructuredPatternIteration(iteration, objective, change, residuals._1, residuals._2,
              cResidual, violation, orthogonality, solvedA.certificate.iterations, solvedC.certificate.iterations,
              solvedA.status, solvedC.status)
            trace :+= evidence
            val tolerance = config.stationarityTolerance * (1.0 + math.max(maxAbs(a), g.max))
            converged = residuals._1 <= tolerance && residuals._2 <= config.stationarityTolerance * (1.0 + config.penalty.supportTv * graph.edges.map(_.weight).maxOption.getOrElse(0.0)) &&
              cResidual <= config.stationarityTolerance * (1.0 + maxAbs(c)) &&
              violation <= config.stationarityTolerance && orthogonality <= config.stationarityTolerance &&
              change <= config.objectiveTolerance * (1.0 + math.abs(objective))
            previous = objective
    failure.toLeft((a, g, c, trace, if converged then StructuredPatternStopping.Converged else StructuredPatternStopping.IterationLimit))

  /** Scientific C-block: empirical S is distinct from a declared target prior. */
  private[pattern] final class TargetProblem(s: DMat, gram: DMat, cross: DMat, config: StructuredPatternConfig):
    val smooth: SmoothObjective = new SmoothObjective:
      val variableRows = s.rows
      val lipschitz = math.max(1e-30, diagonalSum(s) * diagonalSum(gram))
      def value(at: DMat): Either[FirstOrderError, Double] =
        Right(0.5 * inner(at, s * at * gram) - inner(at, cross))
      def gradient(at: DMat): Either[FirstOrderError, DMat] = Right(s * at * gram - cross)
    private val projection = new ProjectionSet:
      val variableRows = s.rows
      def project(at: DMat): Either[FirstOrderError, DMat] = polar(at).left.map(error => FirstOrderError.OracleFailure("target polar", error.toString))
    def solve(initial: DMat): Result[FirstOrderSolution] =
      FirstOrderSolvers.projectedGradient(smooth, projection, initial, config.inner).left.map(StructuredPatternError.Solver.apply)
    def residual(at: DMat): Result[Double] =
      val step = config.inner.stepSafety / smooth.lipschitz
      for
        gradient <- smooth.gradient(at).left.map(StructuredPatternError.Solver.apply)
        projected <- polar(at - scale(gradient, step))
      yield maxAbs(at - projected) / step

  private[pattern] final class LoadingProblem(h: DMat, b: DMat, minimumDiagonal: Double,
      graph: SupportGraph, config: StructuredPatternConfig, precision: DMat => Result[DMat]):
    private val p = b.rows
    private val r = b.cols
    private val width = r + 1
    private val cells = p * width
    private val penalty = config.penalty
    private val tvActive = graph.edges.nonEmpty && penalty.supportTv > 0.0
    private val bound = if tvActive then math.sqrt(2.0 * graph.degree.max) else 0.0
    private def flatten(a: DMat, g: Vector[Double]): DMat = DMat.tabulate(cells, 1): (index, _) =>
      if index % width < r then a(index / width, index % width) else g(index / width)
    def loadings(at: DMat): DMat = DMat.tabulate(p, r)((row, col) => at(row * width + col, 0))
    def envelope(at: DMat): Vector[Double] = Vector.tabulate(p)(row => at(row * width + r, 0))
    private def oracle[A](value: Result[A]): Either[FirstOrderError, A] = value.left.map(error => FirstOrderError.OracleFailure("fixed-Psi loading block", error.toString))
    val smooth: SmoothObjective = new SmoothObjective:
      val variableRows = cells
      val lipschitz = math.max(1e-30, diagonalSum(h) / minimumDiagonal + config.ridge +
        2.0 * penalty.signedSmoothness * graph.weightedDegree.maxOption.getOrElse(0.0))
      def value(at: DMat): Either[FirstOrderError, Double] =
        val a = loadings(at)
        oracle(precision(a)).map: pa =>
          0.5 * inner(a, pa * h) - inner(pa, b) + 0.5 * config.ridge * inner(a, a) + smoothPenalty(a)
      def gradient(at: DMat): Either[FirstOrderError, DMat] =
        val a = loadings(at)
        oracle(precision(a * h - b)).map: weighted =>
          val values = Array.tabulate(cells)(index => if index % width < r then weighted(index / width, index % width) + config.ridge * a(index / width, index % width) else 0.0)
          graph.edges.foreach: edge =>
            var col = 0
            while col < r do
              val delta = penalty.signedSmoothness * edge.weight * (a(edge.left, col) - a(edge.right, col))
              values(edge.left * width + col) += delta
              values(edge.right * width + col) -= delta
              col += 1
          DMat.dense(cells, 1, values.toVector)
    private def smoothPenalty(a: DMat): Double =
      var sum = 0.0
      graph.edges.foreach: edge =>
        var col = 0
        while col < r do
          val delta = a(edge.left, col) - a(edge.right, col)
          sum += 0.5 * penalty.signedSmoothness * edge.weight * delta * delta
          col += 1
      sum
    private val direct = new ProximalTerm:
      val variableRows = cells
      def value(at: DMat): Either[FirstOrderError, Double] =
        if coneViolation(loadings(at), envelope(at)) > config.stationarityTolerance then Left(FirstOrderError.OracleFailure("support cone", "infeasible loading envelope"))
        else Right(penalty.sparsity * envelope(at).sum)
      def proximal(at: DMat, step: Double): Either[FirstOrderError, DMat] =
        val values = new Array[Double](cells)
        var row = 0
        while row < p do
          var length = 0.0
          var col = 0
          while col < r do
            length = math.hypot(length, at(row * width + col, 0))
            col += 1
          val shifted = at(row * width + r, 0) - step * penalty.sparsity
          val multiplier = if length <= shifted then 1.0 else if length <= -shifted then 0.0 else 0.5 * (1.0 + shifted / length)
          col = 0
          while col < r do
            values(row * width + col) = at(row * width + col, 0) * multiplier
            col += 1
          values(row * width + r) = if length <= shifted then shifted else if length <= -shifted then 0.0 else 0.5 * (length + shifted)
          row += 1
        Right(DMat.dense(cells, 1, values.toVector))
    private val incidence = new DoubleLinearOperator:
      val rows = graph.edges.size
      val cols = cells
      def applyTo(x: DVec, into: MutableDVec): Unit =
        var edge = 0
        while edge < rows do
          val e = graph.edges(edge)
          into(edge) = x(e.left * width + r) - x(e.right * width + r)
          edge += 1
      override def transposeApplyTo(x: DVec, into: MutableDVec): Unit =
        var index = 0
        while index < cols do
          into(index) = 0.0
          index += 1
        index = 0
        while index < rows do
          val e = graph.edges(index)
          into(e.left * width + r) += x(index)
          into(e.right * width + r) -= x(index)
          index += 1
    private val functional = new LinearCompositeFunctional:
      val targetRows = graph.edges.size
      def value(at: DMat): Either[FirstOrderError, Double] =
        Right(graph.edges.indices.map(index => penalty.supportTv * graph.edges(index).weight * math.abs(at(index, 0))).sum)
      def proximalConjugate(at: DMat, step: Double): Either[FirstOrderError, DMat] =
        Right(DMat.tabulate(targetRows, at.cols): (index, col) =>
          val limit = penalty.supportTv * graph.edges(index).weight
          math.max(-limit, math.min(limit, at(index, col))))
    def solve(a: DMat, g: Vector[Double]): Result[FirstOrderSolution] =
      val start = flatten(a, g)
      val solved = if tvActive then BoundedLinearOperator.from(incidence, bound)
        .flatMap(FirstOrderSolvers.smoothCompositePrimalDual(smooth, direct, functional, _, start, config.inner))
      else FirstOrderSolvers.proximalGradient(smooth, direct, start, config.inner)
      solved.left.map(StructuredPatternError.Solver.apply)
    def objective(a: DMat, g: Vector[Double]): Result[Double] =
      val at = flatten(a, g)
      for
        smoothValue <- smooth.value(at).left.map(StructuredPatternError.Solver.apply)
        directValue <- direct.value(at).left.map(StructuredPatternError.Solver.apply)
        mapped <- incidence.applyTo(at).left.map(error => StructuredPatternError.Evidence("support incidence", error.toString))
        tvValue <- functional.value(mapped).left.map(StructuredPatternError.Solver.apply)
        value = smoothValue + directValue + tvValue
        _ <- if value.isFinite then Right(()) else Left(StructuredPatternError.Invalid("nonfinite shifted objective"))
      yield value
    def residuals(at: DMat, dual: Option[DMat]): Result[(Double, Double)] =
      val tau = config.inner.stepSafety / (smooth.lipschitz + bound)
      val sigma = if bound == 0.0 then 1.0 else config.inner.stepSafety / bound
      val multipliers = dual.getOrElse(DMat.zeros(graph.edges.size, 1))
      for
        gradient <- smooth.gradient(at).left.map(StructuredPatternError.Solver.apply)
        adjoint <- incidence.adjoint.applyTo(multipliers).left.map(error => StructuredPatternError.Evidence("support adjoint", error.toString))
        mapped <- incidence.applyTo(at).left.map(error => StructuredPatternError.Evidence("support incidence", error.toString))
        next <- direct.proximal(at - scale(gradient + adjoint, tau), tau).left.map(StructuredPatternError.Solver.apply)
        nextDual <- functional.proximalConjugate(multipliers + scale(mapped, sigma), sigma).left.map(StructuredPatternError.Solver.apply)
      yield (maxAbs(at - next) / tau, if tvActive then maxAbs(multipliers - nextDual) / sigma else 0.0)

  /** Preserve old C/A coordinates exactly; append orthonormal directions
    * from the supervised span after removing its projection on the old C.
    */
  private[pattern] def appendComplement(old: DMat, supervised: DMat): Result[DMat] =
    val needed = supervised.cols - old.cols
    val projected = supervised - old * (old.t * supervised)
    Svds.svd(projected, SingularSelection.All)
      .left.map(error => StructuredPatternError.Evidence("warm-start complement", error.toString))
      .flatMap(_.requireConverged.left.map(error => StructuredPatternError.Evidence("warm-start complement convergence", error.toString)))
      .flatMap: decomposition =>
        if decomposition.rank < needed then Left(StructuredPatternError.Invalid("insufficient independent warm-start complement"))
        else Right(DMat.tabulate(old.rows, supervised.cols): (row, column) =>
          if column < old.cols then old(row, column) else decomposition.u(row, column - old.cols))

  private def polar(at: DMat): Result[DMat] =
    Svds.svd(at, SingularSelection.All).left.map(error => StructuredPatternError.Evidence("target polar", error.toString))
      .flatMap(_.requireConverged.left.map(error => StructuredPatternError.Evidence("target polar convergence", error.toString)))
      .flatMap: decomposition =>
        // Full economy U/V supply a valid (possibly nonunique) nearest
        // Stiefel point even when the polar argument is rank deficient.
        val projected = decomposition.u * decomposition.vt
        if !ResidualCovariance.finite(projected) || orthogonalityViolation(projected) > 1e-8 then
          Left(StructuredPatternError.Invalid("Gale polar projection failed finite orthogonality admission"))
        else Right(projected)
  private def scale(at: DMat, scalar: Double): DMat = DMat.tabulate(at.rows, at.cols)((row, col) => at(row, col) * scalar)
  private def inner(left: DMat, right: DMat): Double =
    var sum = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        sum += left(row, col) * right(row, col)
        col += 1
      row += 1
    sum
  private def diagonalSum(at: DMat): Double = (0 until at.rows).map(row => at(row, row)).sum
  private def maxAbs(at: DMat): Double =
    var value = 0.0
    var row = 0
    while row < at.rows do
      var col = 0
      while col < at.cols do
        value = math.max(value, math.abs(at(row, col)))
        col += 1
      row += 1
    value
  private def rowNorm(at: DMat, row: Int): Double =
    var norm = 0.0
    var col = 0
    while col < at.cols do
      norm = math.hypot(norm, at(row, col))
      col += 1
    norm
  private def coneViolation(a: DMat, g: Vector[Double]): Double =
    (0 until a.rows).map(row => math.max(-g(row), rowNorm(a, row) - g(row))).maxOption.getOrElse(0.0)
  private def orthogonalityViolation(c: DMat): Double =
    val gram = c.t * c
    maxAbs(gram - DMat.tabulate(c.cols, c.cols)((row, col) => if row == col then 1.0 else 0.0))
  private def isCentered(at: DMat): Boolean =
    (0 until at.cols).forall: col =>
      val values = (0 until at.rows).map(row => at(row, col))
      math.abs(values.sum) <= 1e-12 * math.max(1.0, values.map(math.abs).sum)
