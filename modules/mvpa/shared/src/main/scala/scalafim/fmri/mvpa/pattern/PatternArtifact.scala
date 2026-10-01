package scalafim.fmri.mvpa.pattern

import gale.linalg.{CholeskyOptions, DMat}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef}

enum PatternArtifactError:
  case AxisLength(field: String, expected: Int, actual: Int)
  case AxisMismatch(field: String, expected: String, actual: String)
  case MatrixShape(field: String, expectedRows: Int, expectedColumns: Int, actualRows: Int, actualColumns: Int)
  case NonFinite(field: String)
  case InvalidRank(rank: Int, maximum: Int)
  case InvalidTarget(detail: String)
  case InvalidPolicy(field: String)
  case InvalidLineage(detail: String)

final class AxisValues[K] private (val axis: AxisRef[K], val values: Vector[Double])
object AxisValues:
  def apply[K](axis: AxisRef[K], values: Vector[Double]): Either[PatternArtifactError, AxisValues[K]] =
    if values.length != axis.size then Left(PatternArtifactError.AxisLength("values", axis.size, values.length))
    else if values.exists(value => !value.isFinite) then Left(PatternArtifactError.NonFinite("values"))
    else Right(new AxisValues(axis, values))

enum CenteringPolicy:
  /** Callers supply both neural and target coordinates already centered. */
  case CenteredBeforeFit(neuralReceipt: String, targetReceipt: String)
  /** Stores the neural mean; supplied target coordinates are already centered
    * under the named training receipt. No target centering is performed here.
    */
  case ExplicitIntercept(values: AxisValues[?], targetCenteringReceipt: String)
/** These describe the fitter's policy; this artifact does not inspect training outcomes. */
enum DegenerateTargetPolicy:
  case Refuse
  case RecordExperimental(reason: String)
/** A numerical rank admission at a recorded tolerance, not an exact rank proof.
  * Pending and declared diagnostics allow rank-deficient experimental factors.
  */
enum GaugeEvidence:
  case PendingNumericalCheck
  case DeclaredSolverDiagnostic(solver: String, reportedRank: Int)
  case VerifiedByGaleGramCholesky(relativePivotTolerance: Double)
/** Rank evidence does not identify component scale, sign, or rotation. */
enum CoordinateGauge:
  case UnfixedBasis
/** Declarations for later covariance consumers. Neither diagonal Psi values nor
  * low-rank loadings are stored; no precision solve or decoding is provided here.
  */
enum ResidualCovarianceCapability:
  case NotFitted
  case DiagonalPlusLowRank(neuralAxis: AxisDescriptor, latentRank: Int)
  case ProviderBacked(neuralAxis: AxisDescriptor, name: String)

final class CategoricalTarget[K, Q] private[pattern] (
    val conditions: AxisRef[K], val targetAxis: AxisRef[Q], val contrast: DMat, val priors: AxisValues[K]
)
final class ContinuousTarget[Q] private[pattern] (
    val targetAxis: AxisRef[Q], val priorScale: AxisValues[Q], val metricDiagonal: AxisValues[Q], val blockWeights: Vector[(String, AxisValues[Q])]
)
enum TargetGeometry:
  case Categorical(value: CategoricalTarget[?, ?])
  case Continuous(value: ContinuousTarget[?])

object TargetGeometry:
  /** Contrast columns are an unweighted zero-sum basis. Priors are separately
    * declared probabilities; this does not assert prior-weighted centering.
    */
  def categorical[K, Q](conditions: AxisRef[K], targetAxis: AxisRef[Q], contrast: DMat, priors: AxisValues[K]): Either[PatternArtifactError, TargetGeometry] =
    if priors.axis.descriptor != conditions.descriptor then Left(PatternArtifactError.AxisMismatch("categorical prior", conditions.descriptor.stableKey, priors.axis.descriptor.stableKey))
    else if contrast.rows != conditions.size || contrast.cols != targetAxis.size then Left(PatternArtifactError.MatrixShape("contrast", conditions.size, targetAxis.size, contrast.rows, contrast.cols))
    else if !finite(contrast) || priors.values.exists(_ <= 0.0) || math.abs(priors.values.sum - 1.0) > 1e-12 then Left(PatternArtifactError.InvalidTarget("categorical contrast/prior is invalid"))
    else if targetAxis.size > conditions.size - 1 || !centered(contrast) || !fullColumnRank(contrast, 1e-12) then Left(PatternArtifactError.InvalidTarget("categorical contrast must be centered, numerically full-column-rank at relative pivot tolerance 1e-12, and at most K - 1"))
    else Right(TargetGeometry.Categorical(new CategoricalTarget(conditions, targetAxis, contrast, priors)))
  /** Diagonal prior/metric only. Named full-axis weight vectors do not assert a
    * partition and do not represent an arbitrary correlated target prior.
    */
  def continuous[Q](targetAxis: AxisRef[Q], prior: AxisValues[Q], metric: AxisValues[Q], blocks: Vector[(String, AxisValues[Q])]): Either[PatternArtifactError, TargetGeometry] =
    if prior.axis.descriptor != targetAxis.descriptor then Left(PatternArtifactError.AxisMismatch("continuous prior", targetAxis.descriptor.stableKey, prior.axis.descriptor.stableKey))
    else if metric.axis.descriptor != targetAxis.descriptor then Left(PatternArtifactError.AxisMismatch("continuous metric", targetAxis.descriptor.stableKey, metric.axis.descriptor.stableKey))
    else if prior.values.exists(_ <= 0.0) || metric.values.exists(_ <= 0.0) || blocks.isEmpty || blocks.map(_._1).distinct.size != blocks.size || blocks.exists { case (name, values) => name.trim.isEmpty || values.axis.descriptor != targetAxis.descriptor || values.values.exists(_ <= 0.0) } then Left(PatternArtifactError.InvalidTarget("continuous prior, metric, and uniquely named block weights must be positive and target-bound"))
    else Right(TargetGeometry.Continuous(new ContinuousTarget(targetAxis, prior, metric, blocks)))
  /** Normalize each column before the Gale Gram factorization so unfixed
    * component scales do not change admission.
    * Gram formation squares the condition number; this conservative admission
    * can refuse ill-conditioned full-rank inputs and is not an SVD rank oracle.
    */
  private[pattern] def fullColumnRank(matrix: DMat, relativeTolerance: Double): Boolean =
    val scales = Array.ofDim[Double](matrix.cols)
    val norms = Array.ofDim[Double](matrix.cols)
    var column = 0
    while column < matrix.cols do
      var row = 0
      while row < matrix.rows do
        scales(column) = math.max(scales(column), math.abs(matrix(row, column)))
        row += 1
      if scales(column) == 0.0 || !scales(column).isFinite then return false
      row = 0
      while row < matrix.rows do
        val scaled = matrix(row, column) / scales(column)
        norms(column) += scaled * scaled
        row += 1
      norms(column) = math.sqrt(norms(column))
      column += 1
    if matrix.cols == 0 then false
    else
      val normalized = DMat.tabulate(matrix.rows, matrix.cols): (row, column) =>
        (matrix(row, column) / scales(column)) / norms(column)
      val gram = normalized.t * normalized
      var maximumDiagonal = 0.0
      column = 0
      while column < gram.cols do
        maximumDiagonal = math.max(maximumDiagonal, gram(column, column))
        column += 1
      maximumDiagonal > 0.0 && maximumDiagonal.isFinite &&
        gram.cholesky(CholeskyOptions(relativeTolerance * maximumDiagonal)).isRight
  private def finite(matrix: DMat): Boolean =
    (0 until matrix.rows).forall(row => (0 until matrix.cols).forall(column => matrix(row, column).isFinite))
  private def centered(matrix: DMat): Boolean =
    var column = 0
    while column < matrix.cols do
      var scale = 0.0
      var row = 0
      while row < matrix.rows do
        scale = math.max(scale, math.abs(matrix(row, column)))
        row += 1
      if scale > 0.0 then
        var sum = 0.0
        var absoluteSum = 0.0
        row = 0
        while row < matrix.rows do
          val value = matrix(row, column) / scale
          sum += value
          absoluteSum += math.abs(value)
          row += 1
        if math.abs(sum) > 1e-12 * absoluteSum then return false
      column += 1
    true

final class PatternFactors[P, Q, R] private (
    val neuralAxis: AxisRef[P], val targetAxis: AxisRef[Q], val componentAxis: AxisRef[R],
    val neuralByComponent: DMat, val targetByComponent: DMat, val gauge: GaugeEvidence,
    val coordinateGauge: CoordinateGauge
):
  /** Factor contribution A C-transpose y, excluding any artifact intercept.
    * Runtime descriptors also permit use through an existential artifact.
    */
  def forwardMean(target: AxisValues[?]): Either[PatternArtifactError, AxisValues[P]] =
    if target.axis.descriptor != targetAxis.descriptor then
      Left(PatternArtifactError.AxisMismatch("forward target", targetAxis.descriptor.stableKey, target.axis.descriptor.stableKey))
    else
      val y = DMat.dense(target.values.length, 1, target.values)
      val result = neuralByComponent * (targetByComponent.t * y)
      AxisValues(neuralAxis, Vector.tabulate(result.rows)(row => result(row, 0)))
object PatternFactors:
  def apply[P, Q, R](neural: AxisRef[P], target: AxisRef[Q], components: AxisRef[R], a: DMat, c: DMat, gauge: GaugeEvidence, coordinateGauge: CoordinateGauge = CoordinateGauge.UnfixedBasis): Either[PatternArtifactError, PatternFactors[P, Q, R]] =
    val maximum = math.min(neural.size, target.size)
    if components.size <= 0 || components.size > maximum then Left(PatternArtifactError.InvalidRank(components.size, maximum))
    else if a.rows != neural.size || a.cols != components.size then Left(PatternArtifactError.MatrixShape("A", neural.size, components.size, a.rows, a.cols))
    else if c.rows != target.size || c.cols != components.size then Left(PatternArtifactError.MatrixShape("C", target.size, components.size, c.rows, c.cols))
    else if !finite(a) || !finite(c) then Left(PatternArtifactError.NonFinite("A/C"))
    else gauge match
      case GaugeEvidence.DeclaredSolverDiagnostic(name, rank) if name.trim.isEmpty || rank < 0 || rank > components.size => Left(PatternArtifactError.InvalidPolicy("declared rank diagnostic"))
      case GaugeEvidence.VerifiedByGaleGramCholesky(tolerance) if !tolerance.isFinite || tolerance <= 0.0 || tolerance >= 1.0 => Left(PatternArtifactError.InvalidPolicy("relative rank tolerance"))
      case GaugeEvidence.VerifiedByGaleGramCholesky(tolerance) if !TargetGeometry.fullColumnRank(a, tolerance) || !TargetGeometry.fullColumnRank(c, tolerance) => Left(PatternArtifactError.InvalidPolicy("Gale numerical rank admission"))
      case _ => Right(new PatternFactors(neural, target, components, a, c, gauge, coordinateGauge))
  private def finite(matrix: DMat): Boolean =
    (0 until matrix.rows).forall(row => (0 until matrix.cols).forall(column => matrix(row, column).isFinite))

final class PatternFitDiagnostics private (val objective: Vector[Double], val solver: String, val notes: Vector[String])
object PatternFitDiagnostics:
  def apply(objective: Vector[Double], solver: String, notes: Vector[String]): Either[PatternArtifactError, PatternFitDiagnostics] =
    if solver.trim.isEmpty || objective.exists(value => !value.isFinite) then Left(PatternArtifactError.InvalidPolicy("diagnostics")) else Right(new PatternFitDiagnostics(objective, solver, notes))
enum InterpretationStatus:
  case ExperimentalFitOnly
/** Declared training provenance. The axis is retained as a declaration; an
  * Alder attachment validates the digest, not this axis, unless an adapter
  * explicitly supplies an axis-mapping check.
  */
final class TrainingBinding private (
    val declaredSampleAxis: AxisDescriptor,
    val source: String,
    val fingerprintDigest: String
)
object TrainingBinding:
  def apply(declaredSampleAxis: AxisDescriptor, source: String, fingerprintDigest: String): Either[PatternArtifactError, TrainingBinding] =
    if source.trim.isEmpty || fingerprintDigest.trim.isEmpty then Left(PatternArtifactError.InvalidPolicy("training binding"))
    else Right(new TrainingBinding(declaredSampleAxis, source, fingerprintDigest))
final class PatternArtifact private (
    val factors: PatternFactors[?, ?, ?], val target: TargetGeometry, val centering: CenteringPolicy,
    val degenerateTarget: DegenerateTargetPolicy, val residualCovariance: ResidualCovarianceCapability,
    val trainingBinding: TrainingBinding, val trainingLineage: Vector[String], val diagnostics: PatternFitDiagnostics, val interpretation: InterpretationStatus
):
  /** Forward mean in the declared target coordinates. Centered artifacts return
    * centered neural coordinates; intercept artifacts add the stored neural mean.
    */
  def forwardMean(targetValues: AxisValues[?]): Either[PatternArtifactError, AxisValues[?]] =
    factors.forwardMean(targetValues).flatMap: contribution =>
      centering match
        case CenteringPolicy.CenteredBeforeFit(_, _) => Right(contribution)
        case CenteringPolicy.ExplicitIntercept(intercept, _) =>
          AxisValues(contribution.axis, contribution.values.zip(intercept.values).map((value, offset) => value + offset))
object PatternArtifact:
  def apply(factors: PatternFactors[?, ?, ?], target: TargetGeometry, centering: CenteringPolicy, degenerate: DegenerateTargetPolicy, covariance: ResidualCovarianceCapability, binding: TrainingBinding, lineage: Vector[String], diagnostics: PatternFitDiagnostics): Either[PatternArtifactError, PatternArtifact] =
    val targetAxis = target match
      case TargetGeometry.Categorical(value) => value.targetAxis.descriptor
      case TargetGeometry.Continuous(value) => value.targetAxis.descriptor
    val categoricalTooHigh = target match
      case TargetGeometry.Categorical(value) => factors.componentAxis.size > value.conditions.size - 1
      case _ => false
    if factors.targetAxis.descriptor != targetAxis then Left(PatternArtifactError.AxisMismatch("C rows / target", factors.targetAxis.descriptor.stableKey, targetAxis.stableKey))
    else if categoricalTooHigh then
      val maximum = target match
        case TargetGeometry.Categorical(value) => value.conditions.size - 1
        case _ => targetAxis.size
      Left(PatternArtifactError.InvalidRank(factors.componentAxis.size, maximum))
    else centering match
      case CenteringPolicy.CenteredBeforeFit(neural, target) if neural.trim.isEmpty || target.trim.isEmpty => Left(PatternArtifactError.InvalidPolicy("centering receipts"))
      case CenteringPolicy.ExplicitIntercept(values, _) if values.axis.descriptor != factors.neuralAxis.descriptor => Left(PatternArtifactError.AxisMismatch("intercept", factors.neuralAxis.descriptor.stableKey, values.axis.descriptor.stableKey))
      case CenteringPolicy.ExplicitIntercept(_, receipt) if receipt.trim.isEmpty => Left(PatternArtifactError.InvalidPolicy("target centering receipt"))
      case _ => covariance match
        case ResidualCovarianceCapability.DiagonalPlusLowRank(axis, rank) if axis != factors.neuralAxis.descriptor || rank < 0 || rank > factors.neuralAxis.size => Left(PatternArtifactError.InvalidPolicy("residual covariance"))
        case ResidualCovarianceCapability.ProviderBacked(axis, name) if axis != factors.neuralAxis.descriptor || name.trim.isEmpty => Left(PatternArtifactError.InvalidPolicy("residual covariance provider"))
        case _ => degenerate match
          case DegenerateTargetPolicy.RecordExperimental(reason) if reason.trim.isEmpty => Left(PatternArtifactError.InvalidPolicy("degenerate target"))
          case _ if lineage.isEmpty || lineage.exists(_.trim.isEmpty) => Left(PatternArtifactError.InvalidLineage("training lineage must be non-empty and named"))
          case _ => Right(new PatternArtifact(factors, target, centering, degenerate, covariance, binding, lineage, diagnostics, InterpretationStatus.ExperimentalFitOnly))
