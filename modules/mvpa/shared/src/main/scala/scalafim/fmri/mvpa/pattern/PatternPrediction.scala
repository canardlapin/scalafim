package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{Cholesky, CholeskyOptions, DMat, QROptions}
import multivar.core.ValueIdentity
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, EvidenceIdentity}

enum PatternPredictionError:
  case Invalid(detail: String)
  case AxisMismatch(field: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Factorization(stage: String, detail: String)
  case Covariance(error: ResidualCovarianceError)
  case Artifact(error: PatternArtifactError)
  case UnsupportedHead(name: String)

final case class PatternPredictionPolicy(relativePivotTolerance: Double, maximumWorkspaceCells: Long):
  require(relativePivotTolerance.isFinite && relativePivotTolerance >= 0.0 && relativePivotTolerance < 1.0, "relative pivot tolerance must lie in [0,1)")
  require(maximumWorkspaceCells >= 0L, "workspace budget must be nonnegative")
object PatternPredictionPolicy:
  val strict: PatternPredictionPolicy = PatternPredictionPolicy(1e-12, 10000000L)

/** The covariance is explicit in the declared target coordinates, including
  * their metric/block transformations. No ambiguous prior scale is squared or
  * interpreted as a variance implicitly. Singular priors are refused in v1.
  */
final class TargetPriorCovariance[Q] private (
    val axis: AxisRef[Q], val matrix: DMat, val valueIdentity: ValueIdentity,
    val coordinateReceipt: String, val pivotTolerance: Double
)
object TargetPriorCovariance:
  def apply[Q](axis: AxisRef[Q], matrix: DMat, identity: ValueIdentity, coordinateReceipt: String,
      policy: PatternPredictionPolicy = PatternPredictionPolicy.strict): Either[PatternPredictionError, TargetPriorCovariance[Q]] =
    val cells = BigInt(16) * axis.size * axis.size
    if matrix.rows != axis.size || matrix.cols != axis.size then Left(PatternPredictionError.AxisMismatch("target covariance shape"))
    else if cells > policy.maximumWorkspaceCells || BigInt(axis.size) * axis.size > Int.MaxValue then
      Left(PatternPredictionError.Budget(cells, policy.maximumWorkspaceCells))
    else if coordinateReceipt.trim.isEmpty || !ResidualCovariance.finite(matrix) then Left(PatternPredictionError.Invalid("finite target covariance and coordinate receipt are required"))
    else if (0 until matrix.rows).exists(row => (0 until row).exists(col => matrix(row, col) != matrix(col, row))) then
      Left(PatternPredictionError.Invalid("target covariance must be explicitly symmetric"))
    else PatternPrediction.factor(matrix, "target prior covariance", policy).map(factor =>
      new TargetPriorCovariance(axis, matrix, identity, coordinateReceipt, factor.options.pivotTolerance))

final case class RawComponentScores[R](values: AxisValues[R])
final case class CalibratedComponentScores[R](values: AxisValues[R])
final case class PosteriorComponentScores[R](values: AxisValues[R])
final case class PosteriorTargetMean[Q](values: AxisValues[Q], priorIdentity: ValueIdentity, coordinateReceipt: String)
final case class ClassPosterior(keys: Vector[String], logScores: Vector[Double], probabilities: Vector[Double])
final class RawNeuralFilters[N, R] private[pattern] (
    val neuralAxis: AxisRef[N], val componentAxis: AxisRef[R], val neuralByComponent: DMat
)
final case class PatternPredictionWork(plannedWorkspaceCells: Long, retainedFilterCells: Long, solveDimension: Int,
    relativePivotTolerance: Double, stabilization: String)

/** Heads of one admitted experimental fitted relationship. Construction also
  * accepts a restored artifact: exact axis descriptors, class order, coding,
  * priors, metric/block metadata and centering are validated/preserved. All
  * online solves have component dimension r; no neural-by-neural inverse or
  * neural-by-target fitted effect is formed. Resource counts are adapter plans,
  * excluding provider factorization scratch and object overhead.
  */
final class PatternPrediction[N, Q, R] private[pattern] (
    val artifact: PatternArtifact, val factors: PatternFactors[N, Q, R],
    val covariance: ResidualCovariance[N], val rawFilters: RawNeuralFilters[N, R],
    val gram: DMat, val work: PatternPredictionWork,
    private val calibrated: Either[PatternPredictionError, Cholesky],
    private val prior: Option[TargetPriorCovariance[Q]],
    private val priorComponent: Option[Cholesky], private val posteriorComponent: Option[Cholesky],
    private val whitenedTarget: Option[DMat]
):
  /** Content identity for attached predictive heads, independent of mutable
    * display labels and including the explicit prior and numerical policy.
    */
  val numericalIdentity: String = AxisDigest.sha256Hex: writer =>
    writer.string("scalafim.pattern-prediction.v1")
    writer.string(artifact.trainingBinding.fingerprintDigest)
    Vector(factors.neuralAxis.descriptor, factors.targetAxis.descriptor, factors.componentAxis.descriptor).foreach: axis =>
      writer.string(axis.stableKey)
      writer.string(axis.coordinateSignature.value)
    def matrix(value: DMat): Unit =
      writer.intLE(value.rows)
      writer.intLE(value.cols)
      var row = 0
      while row < value.rows do
        var col = 0
        while col < value.cols do
          writer.string(java.lang.Double.toHexString(value(row, col)))
          col += 1
        row += 1
    def values(value: Vector[Double]): Unit =
      writer.intLE(value.length)
      value.foreach(number => writer.string(java.lang.Double.toHexString(number)))
    matrix(factors.neuralByComponent)
    matrix(factors.targetByComponent)
    values(covariance.diagonalValues)
    matrix(covariance.loadingsMatrix)
    artifact.centering match
      case CenteringPolicy.CenteredBeforeFit(x, y) => writer.string("centered"); writer.string(x); writer.string(y)
      case CenteringPolicy.ExplicitIntercept(offset, receipt) => writer.string("intercept"); values(offset.values); writer.string(receipt)
    artifact.target match
      case TargetGeometry.Categorical(value) =>
        writer.string("categorical")
        writer.string(value.conditions.descriptor.coordinateSignature.value)
        matrix(value.contrast)
        values(value.priors.values)
      case TargetGeometry.Continuous(value) =>
        writer.string("continuous")
        values(value.priorScale.values)
        values(value.metricDiagonal.values)
        writer.intLE(value.blockWeights.size)
        value.blockWeights.foreach: (name, weights) =>
          writer.string(name)
          values(weights.values)
    prior match
      case None => writer.string("no Gaussian prior")
      case Some(value) =>
        writer.string(value.coordinateReceipt)
        EvidenceIdentity.writeValues(writer, value.valueIdentity)
        matrix(value.matrix)
    writer.string(java.lang.Double.toHexString(work.relativePivotTolerance))
    writer.string(work.stabilization)

  private def input(values: AxisValues[?]): Either[PatternPredictionError, DMat] =
    if values.axis.descriptor != factors.neuralAxis.descriptor then Left(PatternPredictionError.AxisMismatch("neural input"))
    else
      val centered = artifact.centering match
        case CenteringPolicy.CenteredBeforeFit(_, _) => values.values
        case CenteringPolicy.ExplicitIntercept(offset, _) => values.values.zip(offset.values).map((x, mean) => x - mean)
      if centered.exists(!_.isFinite) then Left(PatternPredictionError.Invalid("centered input is nonfinite"))
      else Right(DMat.dense(centered.size, 1, centered))
  private def raw(values: AxisValues[?]): Either[PatternPredictionError, DMat] = input(values).flatMap: x =>
    val u = rawFilters.neuralByComponent.t * x
    if !ResidualCovariance.finite(u) then Left(PatternPredictionError.Invalid("nonfinite raw component scores")) else Right(u)
  private def components(matrix: DMat): Either[PatternPredictionError, AxisValues[R]] =
    AxisValues(factors.componentAxis, Vector.tabulate(matrix.rows)(row => matrix(row, 0))).left.map(PatternPredictionError.Artifact.apply)
  def rawScores(values: AxisValues[?]): Either[PatternPredictionError, RawComponentScores[R]] =
    raw(values).flatMap(components).map(RawComponentScores.apply)
  /** G^-1 u is an unshrunk calibrated estimate; deficient G refuses this head
    * independently of the still-defined raw and posterior heads.
    */
  def calibratedScores(values: AxisValues[?]): Either[PatternPredictionError, CalibratedComponentScores[R]] =
    for
      u <- raw(values)
      factor <- calibrated
      estimate <- factor.solve(u).left.map(error => PatternPredictionError.Factorization("calibrated component solve", error.toString))
      result <- components(estimate)
    yield CalibratedComponentScores(result)
  def admitClassification: Either[PatternPredictionError, Unit] = artifact.target match
    case TargetGeometry.Categorical(_) => Right(())
    case _ => Left(PatternPredictionError.UnsupportedHead("classification requires categorical coding"))
  def admitDecoding: Either[PatternPredictionError, Unit] =
    if prior.nonEmpty then Right(()) else Left(PatternPredictionError.UnsupportedHead("Gaussian decoding requires an explicit target covariance"))
  private def whitenedPosterior(values: AxisValues[?]): Either[PatternPredictionError, DMat] =
    for
      u <- raw(values)
      phi <- priorComponent.toRight(PatternPredictionError.UnsupportedHead("Gaussian posterior requires an explicit continuous target covariance"))
      conditional <- posteriorComponent.toRight(PatternPredictionError.UnsupportedHead("Gaussian posterior"))
      solved <- conditional.solve(phi.lower.t * u).left.map(error => PatternPredictionError.Factorization("posterior component solve", error.toString))
    yield solved
  private def posterior(values: AxisValues[?]): Either[PatternPredictionError, DMat] =
    whitenedPosterior(values).map(value => priorComponent.get.lower * value)
  def posteriorScores(values: AxisValues[?]): Either[PatternPredictionError, PosteriorComponentScores[R]] =
    posterior(values).flatMap(components).map(PosteriorComponentScores.apply)
  def decode(values: AxisValues[?]): Either[PatternPredictionError, PosteriorTargetMean[Q]] =
    for
      white <- whitenedPosterior(values)
      targetPrior <- prior.toRight(PatternPredictionError.UnsupportedHead("Gaussian decoding"))
      decoder <- whitenedTarget.toRight(PatternPredictionError.UnsupportedHead("Gaussian decoding"))
      estimate = decoder * white
      result <- AxisValues(factors.targetAxis, Vector.tabulate(estimate.rows)(row => estimate(row, 0))).left.map(PatternPredictionError.Artifact.apply)
    yield PosteriorTargetMean(result, targetPrior.valueIdentity, targetPrior.coordinateReceipt)
  def encode(values: AxisValues[?]): Either[PatternPredictionError, AxisValues[N]] =
    artifact.forwardMean(values).left.map(PatternPredictionError.Artifact.apply).flatMap(result =>
      AxisValues(factors.neuralAxis, result.values).left.map(PatternPredictionError.Artifact.apply))
  def classify(values: AxisValues[?]): Either[PatternPredictionError, ClassPosterior] = artifact.target match
    case TargetGeometry.Continuous(_) => Left(PatternPredictionError.UnsupportedHead("classification requires categorical coding"))
    case TargetGeometry.Categorical(target) =>
      raw(values).flatMap: u =>
        val means = target.contrast * factors.targetByComponent
        val quadratic = means * gram
        val scores = Vector.tabulate(target.conditions.size): row =>
          var score = math.log(target.priors.values(row))
          var col = 0
          while col < factors.componentAxis.size do
            score += means(row, col) * u(col, 0) - 0.5 * means(row, col) * quadratic(row, col)
            col += 1
          score
        if scores.exists(!_.isFinite) then Left(PatternPredictionError.Invalid("class scores are nonfinite"))
        else
          val maximum = scores.max
          val weights = scores.map(score => math.exp(score - maximum))
          val total = weights.sum
          val keys = Vector.newBuilder[String]
          var error: Option[PatternPredictionError] = None
          var row = 0
          while row < target.conditions.size && error.isEmpty do
            target.conditions.index.stableKeyAt(row) match
              case Left(value) => error = Some(PatternPredictionError.Invalid(value.toString))
              case Right(value) => keys += value
            row += 1
          error.toLeft(ClassPosterior(keys.result(), scores, weights.map(_ / total)))

object PatternPrediction:
  private[pattern] def factor(matrix: DMat, stage: String, policy: PatternPredictionPolicy): Either[PatternPredictionError, Cholesky] =
    val scale = (0 until matrix.rows).map(row => math.abs(matrix(row, row))).maxOption.getOrElse(0.0)
    val tolerance = policy.relativePivotTolerance * scale
    if !ResidualCovariance.finite(matrix) || !tolerance.isFinite then
      Left(PatternPredictionError.Factorization(stage, "derived matrix or pivot tolerance is nonfinite"))
    else matrix.cholesky(CholeskyOptions(tolerance))
      .left.map(error => PatternPredictionError.Factorization(stage, error.toString))

  def fromArtifact[N, Q, R](neural: AxisRef[N], target: AxisRef[Q], components: AxisRef[R], artifact: PatternArtifact,
      covariance: ResidualCovariance[N], targetPrior: Option[TargetPriorCovariance[Q]] = None,
      policy: PatternPredictionPolicy = PatternPredictionPolicy.strict): Either[PatternPredictionError, PatternPrediction[N, Q, R]] =
    val stored = artifact.factors
    val p = neural.size
    val q = target.size
    val r = components.size
    val covariancePlan = covariance.precisionWork(math.max(1, r))
    val conditionCount = artifact.target match
      case TargetGeometry.Categorical(value) => value.conditions.size
      case _ => 0
    val covarianceAdmitted = artifact.residualCovariance match
      case ResidualCovarianceCapability.DiagonalPlusLowRank(axis, rank) => axis == neural.descriptor && rank == covariance.rank
      case _ => false
    val cells = BigInt(64) * p * r + BigInt(64) * q * r + BigInt(64) * r * r +
      BigInt(16) * q * q + BigInt(p) * covariance.rank + BigInt(64) * conditionCount * (r + 1L) + covariancePlan.map(_.peakCellsUpperBound).getOrElse(Long.MaxValue)
    val categorical = artifact.target match
      case TargetGeometry.Categorical(_) => true
      case _ => false
    if stored.neuralAxis.descriptor != neural.descriptor || covariance.neuralAxis.descriptor != neural.descriptor then
      Left(PatternPredictionError.AxisMismatch("neural artifact coordinates"))
    else if stored.targetAxis.descriptor != target.descriptor || stored.componentAxis.descriptor != components.descriptor ||
        targetPrior.exists(_.axis.descriptor != target.descriptor) then Left(PatternPredictionError.AxisMismatch("target/component artifact coordinates"))
    else if !covarianceAdmitted then Left(PatternPredictionError.Invalid("artifact must declare the supplied diagonal-plus-low-rank covariance capability"))
    else if categorical && targetPrior.nonEmpty then Left(PatternPredictionError.Invalid("categorical priors are class probabilities; do not substitute a Gaussian target covariance"))
    else if cells > policy.maximumWorkspaceCells || BigInt(p) * r > Int.MaxValue || BigInt(q) * q > Int.MaxValue || BigInt(conditionCount) * r > Int.MaxValue ||
        covariancePlan.exists(plan => BigInt(plan.largestDenseRows) * plan.largestDenseColumns > Int.MaxValue) then
      Left(PatternPredictionError.Budget(cells, policy.maximumWorkspaceCells))
    else
      for
        factors <- PatternFactors(neural, target, components, stored.neuralByComponent, stored.targetByComponent, stored.gauge, stored.coordinateGauge)
          .left.map(PatternPredictionError.Artifact.apply)
        filters <- covariance.applyPrecision(factors.neuralByComponent).left.map(PatternPredictionError.Covariance.apply)
        unsymmetric = factors.neuralByComponent.t * filters
        gram = DMat.tabulate(r, r)((row, col) => 0.5 * (unsymmetric(row, col) + unsymmetric(col, row)))
        _ <- if ResidualCovariance.finite(gram) then Right(()) else Left(PatternPredictionError.Invalid("nonfinite precision Gram"))
        posterior <- targetPrior match
          case None => Right(None)
          case Some(value) =>
            val phi = factors.targetByComponent.t * value.matrix * factors.targetByComponent
            for
              priorFactor <- factor(phi, "component target covariance", policy)
              conditional = DMat.eye(r) + priorFactor.lower.t * gram * priorFactor.lower
              posteriorFactor <- factor(conditional, "posterior component precision", policy)
              decoderTranspose <- priorFactor.lower.leastSquares(factors.targetByComponent.t * value.matrix,
                QROptions(rankTolerance = Some(0.0))).left.map(error => PatternPredictionError.Factorization("whitened target decoder", error.toString))
              _ <- if ResidualCovariance.finite(decoderTranspose) then Right(()) else Left(PatternPredictionError.Invalid("nonfinite whitened target decoder"))
            yield Some((priorFactor, posteriorFactor, decoderTranspose.t))
      yield new PatternPrediction(artifact, factors, covariance, new RawNeuralFilters(neural, components, filters), gram,
        PatternPredictionWork(cells.toLong, p.toLong * r, r, policy.relativePivotTolerance, "no added jitter or ridge"),
        factor(gram, "calibrated component precision (singular filters are unsupported)", policy), targetPrior,
        posterior.map(_._1), posterior.map(_._2), posterior.map(_._3))
