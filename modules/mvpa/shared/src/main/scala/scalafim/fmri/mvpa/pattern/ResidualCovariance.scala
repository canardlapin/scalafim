package scalafim.fmri.mvpa.pattern

import gale.linalg.{Cholesky, DMat, QR}
import gale.spectral.{SingularOrder, SingularSelection, Svds}
import resample4s.core.Injection
import scalafim.fmri.mvpa.{AxisMember, AxisRef, EvidenceError, ReindexingLeg}

enum ResidualCovarianceError:
  case Shape(field: String, expectedRows: Int, expectedColumns: Int, actualRows: Int, actualColumns: Int)
  case NonFinite(field: String)
  case NonPositiveDiagonal(index: Int)
  case InvalidRank(rank: Int, maximum: Int)
  case InvalidPolicy(field: String)
  case InvalidReceipt
  case TrainingRowsMismatch(declared: Int, actual: Int)
  case NotCentered(feature: Int, standardizedMean: Double)
  case ZeroVarianceFeature(index: Int)
  case FactorizationFailed(stage: String, detail: String)
  case NotConverged(iterations: Int, lastRelativeChange: Double, diagonalMomentResidual: Double)
  case UnknownRoiKey(key: String)
  case IncompatibleRoi(field: String, expected: String, actual: String)
  case EmptyRoi
  case UnsupportedMeasurement(detail: String)
  case Evidence(error: EvidenceError)
  case WorkOverflow

  def message: String = this match
    case Shape(field, er, ec, ar, ac) => s"$field must be ${er}x$ec, got ${ar}x$ac"
    case NonFinite(field) => s"$field is not finite"
    case NonPositiveDiagonal(index) => s"residual diagonal at feature $index must be positive and finite"
    case InvalidRank(rank, maximum) => s"noise rank $rank must lie in [1, $maximum] (identifiable and below min(n, p))"
    case InvalidPolicy(field) => s"invalid residual covariance policy field $field"
    case InvalidReceipt => "residual receipt must name what was removed from the training data"
    case TrainingRowsMismatch(declared, actual) => s"training binding declares $declared samples but $actual residual rows were supplied"
    case NotCentered(feature, mean) => s"residual feature $feature has standardized mean $mean; residuals must be centered within the training scope"
    case ZeroVarianceFeature(index) => s"feature $index has zero residual variance"
    case FactorizationFailed(stage, detail) => s"Gale factorization failed at $stage: $detail"
    case NotConverged(iterations, change, residual) =>
      s"factor fit did not converge in $iterations iterations (relative likelihood change $change, diagonal moment residual $residual)"
    case UnknownRoiKey(key) => s"ROI feature $key is not on the fitted neural axis"
    case IncompatibleRoi(field, expected, actual) => s"ROI axis $field $actual does not match the fitted axis $expected"
    case EmptyRoi => "ROI must contain at least one feature"
    case UnsupportedMeasurement(detail) => s"unsupported measurement: $detail"
    case Evidence(error) => s"evidence: ${error.message}"
    case WorkOverflow => "work accounting overflows a 64-bit count"

/** Planned dense cells from this module's own allocation sites (arrays and
  * `DMat`s it creates; not runtime overhead and not workspace internal to
  * Gale's SVD or Cholesky kernels). `peakCellsUpperBound` assumes every
  * retained result and the largest transient coexist; it is a plan bound, not
  * an observed measurement. `largestDense*` is the largest single dense array:
  * nothing here allocates a features-by-features array.
  */
final case class ResidualCovarianceWork(
    storedCells: Long,
    peakCellsUpperBound: Long,
    cumulativeCells: Long,
    largestDenseRows: Int,
    largestDenseColumns: Int
)

/** Residual covariance `Psi = D + U U^T` on a neural axis, with positive
  * diagonal `D` and noise rank `h`. With `W = D^-1/2 U`,
  * `Psi^-1 = D^-1/2 (I + W W^T)^-1 D^-1/2`, and `(I + W W^T)^-1` is the top-left
  * `p x p` block of the projector onto the orthogonal complement of the
  * columns of the augmented `A = [W; I_h]` (`(p + h) x h`, full column rank).
  * A compact Gale Householder QR of `A` applies that projector through its
  * reflectors: embed `D^-1/2 x` as `[y; 0]`, apply `Q^T`, zero the first `h`
  * coordinates, apply `Q`, keep the first `p` rows and scale by `D^-1/2`. No
  * step subtracts nearly equal large terms; quadratic forms are sums of
  * squares; `log|Psi| = sum log d + 2 sum log|R_kk|`. The orthogonal factor is
  * never materialised, no features-by-features matrix is formed and no
  * pseudoinverse is used.
  */
final class ResidualCovariance[N] private (
    val neuralAxis: AxisRef[N],
    diagonal: Array[Double],
    loadings: Array[Double],
    val rank: Int,
    factor: QR
):
  private val features = diagonal.length
  private val augmented = features + rank
  private val rootDiagonal = diagonal.map(math.sqrt)

  /** `U` is identified only up to an orthogonal rotation (and sign) of its
    * columns: `U Q` gives the same `Psi`. Only `Psi` and its products are
    * meaningful; loadings are not interpretable components.
    */
  val coordinateGauge: CoordinateGauge = CoordinateGauge.UnfixedBasis

  def diagonalValues: Vector[Double] = diagonal.toVector

  /** Loadings `U` as a `p x h` matrix, in the unfixed gauge above. */
  def loadingsMatrix: DMat = DMat.tabulate(features, rank)((row, column) => loadings(row * rank + column))

  /** `log|Psi| = sum_j log d_j + log|A^T A| = sum_j log d_j + 2 sum_k log|R_kk|`. */
  val logDeterminant: Double =
    var sum = 0.0
    var feature = 0
    while feature < features do
      sum += math.log(diagonal(feature))
      feature += 1
    var component = 0
    while component < rank do
      sum += 2.0 * math.log(math.abs(factor.r(component, component)))
      component += 1
    sum

  /** `(max_k |R_kk| / min_k |R_kk|)^2`, a lower bound on the condition number
    * of the capacitance `A^T A = I + W^T W`.
    */
  def capacitanceConditionEstimate: Double =
    val pivots = (0 until rank).map(component => math.abs(factor.r(component, component)))
    math.pow(pivots.max / pivots.min, 2)

  /** Stored model cells: diagonal and its square root (`2 p`), loadings
    * (`p h`) and the compact Gale QR of the `(p + h) x h` augmented matrix,
    * whose reflectors and `R` are each `(p + h) x h`, plus `h` scalars:
    * `2 p + p h + 2 (p + h) h + h`.
    */
  def storedCells: Long = 2L * features + features.toLong * rank + 2L * augmented * rank + rank

  /** Work of one `applyPrecision` on `c` columns. Transient dense
    * allocations: the embedded block, its `Q^T` transform, the projected
    * block, its `Q` back-transform (each `(p + h) x c`) and the `p x c`
    * result; the peak adds the stored model.
    */
  def precisionWork(columns: Int): Either[ResidualCovarianceError, ResidualCovarianceWork] =
    workFor(columns, 4L * augmented * columns + features.toLong * columns, augmented, columns)

  /** Work of one square whitening application: embedded and transformed
    * augmented blocks and the retained tail, without a dense covariance. */
  def whiteningWork(columns: Int): Either[ResidualCovarianceError, ResidualCovarianceWork] =
    workFor(columns, 2L * augmented * columns + features.toLong * columns, augmented, columns)

  /** Square whitening W from the retained Gale augmented QR: W^T W = Psi^-1.
    * The trailing `features` transformed coordinates have identity covariance
    * under the declared Psi. They are numeric coordinates, not original rows. */
  def whiten(values: DMat): Either[ResidualCovarianceError, DMat] =
    for
      _ <- checkBlock("whitening operand", values)
      transformed <- transform(DMat.tabulate(augmented, values.cols)((row, column) => if row < features then values(row, column) / rootDiagonal(row) else 0.0))
      result <- finiteResult("whitening result", DMat.tabulate(features, values.cols)((row, column) => transformed(row + rank, column)))
    yield result

  /** Work of one `applyCovariance` on `c` columns: `h x c` projections and the
    * `p x c` result; the peak adds the stored model.
    */
  def covarianceWork(columns: Int): Either[ResidualCovarianceError, ResidualCovarianceWork] =
    workFor(columns, rank.toLong * columns + features.toLong * columns, math.max(features, rank), columns)

  /** Pre-execution bounds for `precisionDiagonal`. Transient allocations: the
    * `(p + h) x h` leading identity block and its `Q` image, the `p` result,
    * and for every fallback row an embedded and transformed `(p + h)` column.
    * The cumulative bound assumes all `p` rows fall back; the peak holds the
    * stored model, the leading blocks, the result and one chunk of at most
    * `ResidualCovariance.fallbackChunk` rows.
    */
  def precisionDiagonalWork: ResidualCovarianceWork =
    val chunk = math.min(features, ResidualCovariance.fallbackChunk)
    val fixed = 2L * augmented * rank + features
    ResidualCovarianceWork(storedCells, storedCells + fixed + 2L * augmented * chunk, fixed + 2L * augmented * features,
      augmented, math.max(rank, chunk))

  private def workFor(columns: Int, cells: Long, rows: Int, width: Int): Either[ResidualCovarianceError, ResidualCovarianceWork] =
    if columns < 1 then Left(ResidualCovarianceError.Shape("work columns", features, 1, features, columns))
    else Right(ResidualCovarianceWork(storedCells, storedCells + cells, cells, rows, width))

  /** `Psi x` for a `p x c` block, without forming `Psi`. */
  def applyCovariance(values: DMat): Either[ResidualCovarianceError, DMat] =
    checkBlock("covariance operand", values).flatMap: _ =>
      val k = values.cols
      val projected = new Array[Double](rank * k)
      ResidualCovariance.multiplyTransposeInto(loadings, features, rank, values, projected)
      val result = DMat.tabulate(features, k): (row, column) =>
        var sum = diagonal(row) * values(row, column)
        var component = 0
        while component < rank do
          sum += loadings(row * rank + component) * projected(component * k + column)
          component += 1
        sum
      finiteResult("covariance result", result)

  /** `Psi^-1 x` for a `p x c` block through the augmented projector, which
    * avoids subtracting nearly equal large terms. Accuracy is conditioning
    * dependent and no componentwise error bound is claimed: where `Psi` is
    * extremely ill conditioned along the loading span, components that are
    * tiny relative to `|x|` can be dominated by rounding. The suite records
    * observed accuracy for specific dominant-loading fixtures.
    */
  def applyPrecision(values: DMat): Either[ResidualCovarianceError, DMat] =
    for
      _ <- checkBlock("precision operand", values)
      transformed <- transform(DMat.tabulate(augmented, values.cols)((row, column) => if row < features then values(row, column) / rootDiagonal(row) else 0.0))
      projected = DMat.tabulate(augmented, values.cols)((row, column) => if row < rank then 0.0 else transformed(row, column))
      back <- factor.applyQ(projected).left.map(error => ResidualCovarianceError.FactorizationFailed("apply Q", error.toString))
      result <- finiteResult("precision result", DMat.tabulate(features, values.cols)((row, column) => back(row, column) / rootDiagonal(row)))
    yield result

  /** `x^T Psi^-1 x = |tail_h(Q^T [D^-1/2 x; 0])|^2`, evaluated directly as a
    * sum of squares of transformed coordinates (never a difference). Tiny tail
    * coordinates can be dominated by rounding, and their squares can
    * underflow; non-finite results are refused.
    */
  def precisionQuadratic(values: Vector[Double]): Either[ResidualCovarianceError, Double] =
    if values.length != features then Left(ResidualCovarianceError.Shape("quadratic operand", features, 1, values.length, 1))
    else if values.exists(value => !value.isFinite) then Left(ResidualCovarianceError.NonFinite("quadratic operand"))
    else
      transform(DMat.tabulate(augmented, 1)((row, _) => if row < features then values(row) / rootDiagonal(row) else 0.0)).flatMap: transformed =>
        val value = tailSquares(transformed, 0)
        if value.isFinite then Right(value) else Left(ResidualCovarianceError.NonFinite("quadratic result"))

  /** Diagonal of `Psi^-1`, `[P_perp]_ii / d_i`. Rows whose complement
    * `1 - |(Q_1)_i|^2` (`Q_1` the leading `h` columns of `Q`) is at least
    * `ResidualCovariance.fastDiagonalFloor` use that form; the floor is a
    * heuristic guard against cancellation, not an accuracy certificate. Other
    * rows evaluate the transformed tail squares `|tail_h(Q^T [e_i; 0])|^2`
    * directly, in chunks of at most `ResidualCovariance.fallbackChunk` rows.
    * Squares can underflow at extreme scales; every returned entry is checked
    * positive and finite, otherwise the call is refused.
    */
  def precisionDiagonal: Either[ResidualCovarianceError, Vector[Double]] =
    val leading = DMat.tabulate(augmented, rank)((row, column) => if row == column then 1.0 else 0.0)
    factor.applyQ(leading).left.map(error => ResidualCovarianceError.FactorizationFailed("apply Q", error.toString)).flatMap: columns =>
      val values = new Array[Double](features)
      val fallback = Vector.newBuilder[Int]
      var feature = 0
      while feature < features do
        var norm = 0.0
        var component = 0
        while component < rank do
          norm += columns(feature, component) * columns(feature, component)
          component += 1
        val complement = 1.0 - norm
        if complement >= ResidualCovariance.fastDiagonalFloor then values(feature) = complement / diagonal(feature)
        else fallback += feature
        feature += 1
      val exact = fallback.result().grouped(ResidualCovariance.fallbackChunk).toVector
      val filled = ResidualCovariance.traverse(exact)(chunk =>
        transform(DMat.tabulate(augmented, chunk.length)((row, column) => if row == chunk(column) then 1.0 else 0.0))
          .map(transformed => chunk.indices.foreach(column => values(chunk(column)) = tailSquares(transformed, column) / diagonal(chunk(column)))))
      filled.flatMap(_ =>
        if values.forall(value => value > 0.0 && value.isFinite) then Right(values.toVector)
        else Left(ResidualCovarianceError.NonFinite("precision diagonal")))

  /** Exact hard-ROI marginal `Psi_RR = D_R + U_R U_R^T`. The ROI axis must
    * share the fitted axis's role, basis, units and scale, and its stable keys
    * select fitted features; all `h` factors are kept even when the ROI has
    * fewer than `h` features. Its precision is the inverse of this marginal,
    * not a crop of the whole-axis precision.
    */
  def restrict[M](roi: AxisRef[M]): Either[ResidualCovarianceError, ResidualCovariance[M]] =
    val parentAxis = neuralAxis.descriptor
    val child = roi.descriptor
    if roi.size == 0 then Left(ResidualCovarianceError.EmptyRoi)
    else if child.role != parentAxis.role then Left(ResidualCovarianceError.IncompatibleRoi("role", parentAxis.role.toString, child.role.toString))
    else if child.basis != parentAxis.basis then Left(ResidualCovarianceError.IncompatibleRoi("basis", parentAxis.basis, child.basis))
    else if child.units != parentAxis.units then Left(ResidualCovarianceError.IncompatibleRoi("units", parentAxis.units, child.units))
    else if child.scale != parentAxis.scale then Left(ResidualCovarianceError.IncompatibleRoi("scale", parentAxis.scale, child.scale))
    else
      for
        parent <- ResidualCovariance.keyIndex(neuralAxis)
        roiKeys <- ResidualCovariance.keys(roi)
        ordinals <- ResidualCovariance.traverse(roiKeys)(key => parent.get(key).toRight(ResidualCovarianceError.UnknownRoiKey(key)))
        restricted <- ResidualCovariance.fromArrays(roi,
          ordinals.map(diagonal(_)).toArray,
          ordinals.flatMap(ordinal => (0 until rank).map(component => loadings(ordinal * rank + component))).toArray,
          rank)
      yield restricted

  /** Exact ordinal hard restriction avoids matching the member-hash keys of a
    * reindexed child axis against this fitted parent axis. */
  def restrict[P <: multivar.core.SemanticSpace, K](by: ReindexingLeg[P, K, Injection]): Either[ResidualCovarianceError, ResidualCovariance[AxisMember[K]]] =
    if by.parentAxis != neuralAxis.descriptor then Left(ResidualCovarianceError.IncompatibleRoi("parent", neuralAxis.descriptor.stableKey, by.parentAxis.stableKey))
    else if by.child.size == 0 then Left(ResidualCovarianceError.EmptyRoi)
    else
      val ordinals = by.ordinals.toVector
      if ordinals.exists(value => value < 0 || value >= features) then Left(ResidualCovarianceError.IncompatibleRoi("ordinal", neuralAxis.descriptor.stableKey, by.child.descriptor.stableKey))
      else ResidualCovariance.fromArrays(by.child, ordinals.map(diagonal(_)).toArray,
        ordinals.flatMap(ordinal => (0 until rank).map(component => loadings(ordinal * rank + component))).toArray, rank)

  /** A general measurement `L` maps `Psi` to `L D L^T + (L U)(L U)^T`, whose
    * first term is not diagonal in general, so the diagonal-plus-low-rank
    * structure and its solves do not carry over. Only exact hard-ROI
    * restriction is supported; this always refuses.
    */
  def measured(measurement: DMat): Either[ResidualCovarianceError, Nothing] =
    Left(ResidualCovarianceError.UnsupportedMeasurement(
      s"general ${measurement.rows}x${measurement.cols} measurement does not preserve diagonal-plus-low-rank structure; use restrict for a hard ROI"))

  /** `sum_i r_i^T Psi^-1 r_i` over row-major `n x p` rows, in chunks of at most
    * `ResidualCovariance.fallbackChunk` rows through the augmented projector.
    */
  private[pattern] def quadraticSum(data: Array[Double], rows: Int): Either[ResidualCovarianceError, Double] =
    val chunks = (0 until rows).toVector.grouped(ResidualCovariance.fallbackChunk).toVector
    val sums = ResidualCovariance.traverse(chunks)(chunk =>
      transform(DMat.tabulate(augmented, chunk.length)((row, column) =>
        if row < features then data(chunk(column) * features + row) / rootDiagonal(row) else 0.0
      )).map(transformed => chunk.indices.foldLeft(0.0)((sum, column) => sum + tailSquares(transformed, column))))
    sums.map(_.sum)

  private def transform(embedded: DMat): Either[ResidualCovarianceError, DMat] =
    factor.applyQT(embedded).left.map(error => ResidualCovarianceError.FactorizationFailed("apply Q^T", error.toString))

  private def tailSquares(transformed: DMat, column: Int): Double =
    var sum = 0.0
    var row = rank
    while row < augmented do
      sum += transformed(row, column) * transformed(row, column)
      row += 1
    sum

  private def checkBlock(field: String, values: DMat): Either[ResidualCovarianceError, Unit] =
    if values.rows != features || values.cols < 1 then Left(ResidualCovarianceError.Shape(field, features, math.max(values.cols, 1), values.rows, values.cols))
    else if !ResidualCovariance.finite(values) then Left(ResidualCovarianceError.NonFinite(field))
    else Right(())

  private def finiteResult(field: String, result: DMat): Either[ResidualCovarianceError, DMat] =
    if ResidualCovariance.finite(result) then Right(result) else Left(ResidualCovarianceError.NonFinite(field))

enum ConvergencePolicy:
  case Refuse
  case Record

/** Factor-analytic fit policy. `relativeFloor` bounds each unique variance
  * below by that fraction of the feature's residual second moment; it is the
  * only regularization (no loading or covariance shrinkage). Convergence needs
  * both a relative log-likelihood change at most `tolerance` and the diagonal
  * moment residual `max_j |d_j + |u_j|^2 - S_jj| / S_jj` over unfloored
  * features at most `diagonalMomentTolerance`. That residual is a necessary
  * condition of a factor-analysis likelihood optimum only; it is not a full
  * gradient or KKT certificate, so convergence here is not a certificate of
  * an optimum. `centeringTolerance`
  * bounds `|mean_j| / sqrt(S_jj)`. `sensitivityRanks` are refitted and
  * reported, never substituted for `rank`.
  */
final class ResidualCovarianceFitPolicy private (
    val rank: Int,
    val relativeFloor: Double,
    val maximumIterations: Int,
    val tolerance: Double,
    val diagonalMomentTolerance: Double,
    val centeringTolerance: Double,
    val convergence: ConvergencePolicy,
    val sensitivityRanks: Vector[Int]
)
object ResidualCovarianceFitPolicy:
  def apply(
      rank: Int,
      relativeFloor: Double = 1e-3,
      maximumIterations: Int = 1000,
      tolerance: Double = 1e-9,
      diagonalMomentTolerance: Double = 1e-6,
      centeringTolerance: Double = 1e-8,
      convergence: ConvergencePolicy = ConvergencePolicy.Refuse,
      sensitivityRanks: Vector[Int] = Vector.empty
  ): Either[ResidualCovarianceError, ResidualCovarianceFitPolicy] =
    def positive(value: Double): Boolean = value > 0.0 && value.isFinite
    if rank < 1 then Left(ResidualCovarianceError.InvalidPolicy("rank"))
    else if !(relativeFloor > 0.0 && relativeFloor < 1.0) then Left(ResidualCovarianceError.InvalidPolicy("relativeFloor"))
    else if maximumIterations < 1 then Left(ResidualCovarianceError.InvalidPolicy("maximumIterations"))
    else if !positive(tolerance) then Left(ResidualCovarianceError.InvalidPolicy("tolerance"))
    else if !positive(diagonalMomentTolerance) then Left(ResidualCovarianceError.InvalidPolicy("diagonalMomentTolerance"))
    else if !positive(centeringTolerance) then Left(ResidualCovarianceError.InvalidPolicy("centeringTolerance"))
    else if sensitivityRanks.exists(_ < 1) || sensitivityRanks.contains(rank) || sensitivityRanks.distinct.length != sensitivityRanks.length then
      Left(ResidualCovarianceError.InvalidPolicy("sensitivityRanks"))
    else Right(new ResidualCovarianceFitPolicy(rank, relativeFloor, maximumIterations, tolerance, diagonalMomentTolerance, centeringTolerance, convergence, sensitivityRanks))

/** `parameterCount = p h + p - h (h - 1) / 2` (loadings modulo rotation plus
  * unique variances); `bic = -2 logLikelihood + parameterCount log n`.
  */
final case class ResidualCovarianceSensitivity(rank: Int, logLikelihood: Double, parameterCount: Long, bic: Double, converged: Boolean, iterations: Int)

/** What was estimated and how. Centering is necessary, not sufficient: that
  * the residuals hold unexplained variation (task model removed within the
  * training scope) is the caller's declared provenance in `residualReceipt`
  * and `training`, which this primitive records but cannot verify.
  * `logLikelihood` is evaluated through the stable precision form;
  * `logLikelihoodTrace` holds the EM iterates. `explainedFraction` is
  * `tr(U U^T) / tr(S)`; `minimumUniqueRatio` is `min_j d_j / S_jj`;
  * `centeringResidual` is the largest `|mean_j| / sqrt(S_jj)`.
  */
final case class ResidualCovarianceFitReceipt(
    residualReceipt: String,
    training: TrainingBinding,
    samples: Int,
    features: Int,
    rank: Int,
    regularization: String,
    relativeFloor: Double,
    initialization: String,
    centeringResidual: Double,
    flooredFeatures: Vector[Int],
    minimumUniqueRatio: Double,
    iterations: Int,
    logLikelihoodTrace: Vector[Double],
    logLikelihood: Double,
    converged: Boolean,
    diagonalMomentResidual: Double,
    parameterCount: Long,
    bic: Double,
    explainedFraction: Double,
    capacitanceConditionEstimate: Double,
    coordinateGauge: CoordinateGauge,
    sensitivity: Vector[ResidualCovarianceSensitivity],
    work: ResidualCovarianceWork
)

final case class ResidualCovarianceFit[N](covariance: ResidualCovariance[N], receipt: ResidualCovarianceFitReceipt)

object ResidualCovariance:
  /** Validated construction from declared factors. */
  def fromFactors[N](neuralAxis: AxisRef[N], diagonal: Vector[Double], loadings: DMat): Either[ResidualCovarianceError, ResidualCovariance[N]] =
    if loadings.rows != neuralAxis.size || loadings.cols < 1 then
      Left(ResidualCovarianceError.Shape("loadings", neuralAxis.size, math.max(loadings.cols, 1), loadings.rows, loadings.cols))
    else if diagonal.length != neuralAxis.size then Left(ResidualCovarianceError.Shape("diagonal", neuralAxis.size, 1, diagonal.length, 1))
    else if !finite(loadings) then Left(ResidualCovarianceError.NonFinite("loadings"))
    else
      val rank = loadings.cols
      fromArrays(neuralAxis, diagonal.toArray, Array.tabulate(loadings.rows * rank)(index => loadings(index / rank, index % rank)), rank)

  /** Largest noise rank `h < min(n, p)` whose factor model has non-negative
    * degrees of freedom, `(p - h)^2 >= p + h` (the Ledermann bound).
    */
  def maximumIdentifiableRank(samples: Int, features: Int): Int =
    (0 until math.min(samples, features)).reverseIterator
      .find(h => (features - h).toLong * (features - h) >= features.toLong + h)
      .getOrElse(0)

  /** `p h + p - h (h - 1) / 2`. */
  def parameterCount(features: Int, rank: Int): Long =
    features.toLong * rank + features - rank.toLong * (rank - 1) / 2

  /** Maximum-likelihood factor analysis of training residuals `R` (`n x p`,
    * samples by features), estimating `Psi = D + U U^T` with `S = R^T R / n`.
    * The rows must be exactly the training binding's declared samples, with the
    * task model and mean already removed within that scope (named by
    * `residualReceipt`); columns whose standardized mean exceeds the policy's
    * centering tolerance are refused rather than recentred. Initialization is
    * Gale's partial (Lanczos) SVD of `R` for the largest requested rank; each
    * EM step costs `O(n p h)` and never forms `S`.
    */
  def fit[N](
      neuralAxis: AxisRef[N],
      residuals: DMat,
      residualReceipt: String,
      training: TrainingBinding,
      policy: ResidualCovarianceFitPolicy
  ): Either[ResidualCovarianceError, ResidualCovarianceFit[N]] =
    val n = residuals.rows
    val p = residuals.cols
    val maximum = maximumIdentifiableRank(n, p)
    val ranks = policy.rank +: policy.sensitivityRanks
    if residualReceipt.trim.isEmpty then Left(ResidualCovarianceError.InvalidReceipt)
    else if p != neuralAxis.size || n < 2 then Left(ResidualCovarianceError.Shape("residuals", math.max(n, 2), neuralAxis.size, n, p))
    else if training.declaredSampleAxis.size != n then Left(ResidualCovarianceError.TrainingRowsMismatch(training.declaredSampleAxis.size, n))
    else if !finite(residuals) then Left(ResidualCovarianceError.NonFinite("residuals"))
    else ranks.find(_ > maximum) match
      case Some(rank) => Left(ResidualCovarianceError.InvalidRank(rank, maximum))
      case None =>
        val data = Array.tabulate(n * p)(index => residuals(index / p, index % p))
        val second = new Array[Double](p)
        val mean = new Array[Double](p)
        var index = 0
        while index < n * p do
          second(index % p) += data(index) * data(index)
          mean(index % p) += data(index)
          index += 1
        var feature = 0
        while feature < p do
          second(feature) /= n.toDouble
          mean(feature) /= n.toDouble
          feature += 1
        if second.exists(value => !value.isFinite) || mean.exists(value => !value.isFinite) then Left(ResidualCovarianceError.NonFinite("residual second moments"))
        else
          val standardized = Array.tabulate(p)(j => if second(j) > 0.0 then math.abs(mean(j)) / math.sqrt(second(j)) else 0.0)
          val centering = standardized.max
          second.indices.find(feature => !(second(feature) > 0.0)) match
            case Some(zero) => Left(ResidualCovarianceError.ZeroVarianceFeature(zero))
            case None if centering > policy.centeringTolerance =>
              Left(ResidualCovarianceError.NotCentered(standardized.indexOf(centering), centering))
            case None =>
              val largest = ranks.max
              for
                spectrum <- Svds.svd(residuals, SingularSelection.Count(largest, SingularOrder.Largest))
                  .flatMap(_.requireConverged)
                  .left.map(error => ResidualCovarianceError.FactorizationFailed("initial partial SVD", error.toString))
                primary <- emFit(neuralAxis, data, n, p, second, spectrum, policy.rank, policy)
                alternatives <- traverse(policy.sensitivityRanks)(rank => emFit(neuralAxis, data, n, p, second, spectrum, rank, policy).map(rank -> _))
                work <- fitWork(n, p, largest, (primary.iterations +: alternatives.map(_._2.iterations)).zip(ranks))
                _ <- if primary.converged || policy.convergence == ConvergencePolicy.Record then Right(())
                  else Left(ResidualCovarianceError.NotConverged(primary.iterations, primary.lastRelativeChange, primary.diagonalMomentResidual))
              yield
                val parameters = parameterCount(p, policy.rank)
                val minimumUnique = primary.covariance.diagonalValues.zipWithIndex.map((d, j) => d / second(j)).min
                val sensitivity = alternatives.map: (rank, alternative) =>
                  val count = parameterCount(p, rank)
                  ResidualCovarianceSensitivity(rank, alternative.logLikelihood, count, -2.0 * alternative.logLikelihood + count * math.log(n.toDouble),
                    alternative.converged, alternative.iterations)
                ResidualCovarianceFit(primary.covariance, ResidualCovarianceFitReceipt(residualReceipt, training, n, p, policy.rank,
                  "relative unique-variance floor only; no loading or covariance shrinkage", policy.relativeFloor,
                  s"gale-partial-svd(k=$largest)", centering, primary.floored, minimumUnique, primary.iterations, primary.logLikelihoodTrace,
                  primary.logLikelihood, primary.converged, primary.diagonalMomentResidual, parameters,
                  -2.0 * primary.logLikelihood + parameters * math.log(n.toDouble), primary.explained / second.sum,
                  primary.covariance.capacitanceConditionEstimate, CoordinateGauge.UnfixedBasis,
                  sensitivity, work))

  private final case class EmResult[N](
      covariance: ResidualCovariance[N],
      explained: Double,
      floored: Vector[Int],
      iterations: Int,
      logLikelihoodTrace: Vector[Double],
      logLikelihood: Double,
      converged: Boolean,
      lastRelativeChange: Double,
      diagonalMomentResidual: Double
  )

  /** Rubin-Thayer EM for `Psi = D + U U^T`. With `C = I + U^T D^-1 U`,
    * `beta = U^T Psi^-1 = C^-1 U^T D^-1` and `Z = R beta^T`:
    * `E[zz^T] = C^-1 + Z^T Z / n`, `S beta^T = R^T Z / n`,
    * `U' = S beta^T E[zz^T]^-1`, `d'_j = max(S_jj - (U' beta S)_jj, floor S_jj)`.
    * The floored diagonal update is the exact constrained maximizer of its
    * unimodal term, so the likelihood stays non-decreasing. The final model's
    * likelihood is re-evaluated through its stable precision form.
    */
  private def emFit[N](
      neuralAxis: AxisRef[N],
      data: Array[Double],
      n: Int,
      p: Int,
      second: Array[Double],
      spectrum: gale.spectral.SVD,
      h: Int,
      policy: ResidualCovarianceFitPolicy
  ): Either[ResidualCovarianceError, EmResult[N]] =
    val u = new Array[Double](p * h)
    val d = new Array[Double](p)
    val scale = 1.0 / math.sqrt(n.toDouble)
    var feature = 0
    while feature < p do
      var component = 0
      var explained = 0.0
      while component < h do
        val value = spectrum.vt(component, feature) * spectrum.singularValues(component) * scale
        u(feature * h + component) = value
        explained += value * value
        component += 1
      d(feature) = math.max(second(feature) - explained, policy.relativeFloor * second(feature))
      feature += 1
    val trace = Vector.newBuilder[Double]
    var previous = Double.NaN
    var change = Double.PositiveInfinity
    var momentResidual = Double.PositiveInfinity
    var iteration = 0
    var converged = false
    var failure: Option[ResidualCovarianceError] = None
    while failure.isEmpty && !converged && iteration < policy.maximumIterations do
      emStep(data, n, p, h, second, u, d, policy.relativeFloor) match
        case Left(error) => failure = Some(error)
        case Right(likelihood) if !likelihood.isFinite => failure = Some(ResidualCovarianceError.NonFinite("log-likelihood"))
        case Right(likelihood) =>
          trace += likelihood
          momentResidual = diagonalMomentResidual(p, h, second, u, d, policy.relativeFloor)
          if !previous.isNaN then
            change = math.abs(likelihood - previous) / (1.0 + math.abs(previous))
            converged = change <= policy.tolerance && momentResidual <= policy.diagonalMomentTolerance
          previous = likelihood
          iteration += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        for
          iterate <- latentState(data, n, p, h, u, d).map(_._3)
          _ <- if iterate.isFinite then Right(()) else Left(ResidualCovarianceError.NonFinite("log-likelihood"))
          covariance <- fromArrays(neuralAxis, d.clone(), u.clone(), h)
          quadratic <- covariance.quadraticSum(data, n)
          stable = -0.5 * n.toDouble * (p.toDouble * math.log(2.0 * math.Pi) + covariance.logDeterminant) - 0.5 * quadratic
          _ <- if stable.isFinite then Right(()) else Left(ResidualCovarianceError.NonFinite("log-likelihood"))
        yield
          val floored = (0 until p).filter(feature => d(feature) <= policy.relativeFloor * second(feature)).toVector
          EmResult(covariance, u.iterator.map(value => value * value).sum, floored, iteration, (trace += iterate).result(), stable,
            converged, change, momentResidual)

  /** Diagonal moment residual `max_j |d_j + |u_j|^2 - S_jj| / S_jj` over
    * features above their floor (a necessary optimality condition only).
    */
  private def diagonalMomentResidual(p: Int, h: Int, second: Array[Double], u: Array[Double], d: Array[Double], floor: Double): Double =
    var worst = 0.0
    var feature = 0
    while feature < p do
      if d(feature) > floor * second(feature) then
        var sum = d(feature)
        var component = 0
        while component < h do
          sum += u(feature * h + component) * u(feature * h + component)
          component += 1
        worst = math.max(worst, math.abs(sum - second(feature)) / second(feature))
      feature += 1
    worst

  /** One EM update in place; returns the log-likelihood at the incoming parameters. */
  private def emStep(data: Array[Double], n: Int, p: Int, h: Int, second: Array[Double], u: Array[Double], d: Array[Double], floor: Double): Either[ResidualCovarianceError, Double] =
    for
      state <- latentState(data, n, p, h, u, d)
      (chol, scores, likelihood) = state
      inverse <- chol.solve(DMat.eye(h)).left.map(error => ResidualCovarianceError.FactorizationFailed("capacitance inverse", error.toString))
      sb = Array.tabulate(p * h): index =>
        val feature = index / h
        val component = index % h
        var sum = 0.0
        var row = 0
        while row < n do
          sum += data(row * p + feature) * scores(row * h + component)
          row += 1
        sum / n.toDouble
      moment = DMat.tabulate(h, h): (a, b) =>
        var sum = 0.0
        var row = 0
        while row < n do
          sum += scores(row * h + a) * scores(row * h + b)
          row += 1
        inverse(a, b) + sum / n.toDouble
      momentFactor <- moment.cholesky.left.map(error => ResidualCovarianceError.FactorizationFailed("latent second moment", error.toString))
      updated <- momentFactor.solve(DMat.tabulate(h, p)((component, feature) => sb(feature * h + component)))
        .left.map(error => ResidualCovarianceError.FactorizationFailed("loading update", error.toString))
    yield
      var feature = 0
      while feature < p do
        var reduction = 0.0
        var component = 0
        while component < h do
          val loading = updated(component, feature)
          u(feature * h + component) = loading
          reduction += loading * sb(feature * h + component)
          component += 1
        d(feature) = math.max(second(feature) - reduction, floor * second(feature))
        feature += 1
      likelihood

  /** Capacitance factor, latent scores `Z = R D^-1 U C^-1` (row-major `n x h`)
    * and the EM-route Gaussian log-likelihood
    * `-n/2 (p log 2 pi + log|Psi| + tr(Psi^-1 S))`, with
    * `n tr(Psi^-1 S) = sum r^2/d - sum_i a_i^T C^-1 a_i`, `a_i = (R D^-1 U)_i`.
    * Used for EM progress only; reported likelihoods use the stable form.
    */
  private def latentState(data: Array[Double], n: Int, p: Int, h: Int, u: Array[Double], d: Array[Double]): Either[ResidualCovarianceError, (Cholesky, Array[Double], Double)] =
    val capacitance = DMat.tabulate(h, h): (a, b) =>
      var sum = if a == b then 1.0 else 0.0
      var feature = 0
      while feature < p do
        sum += u(feature * h + a) * u(feature * h + b) / d(feature)
        feature += 1
      sum
    capacitance.cholesky.left.map(error => ResidualCovarianceError.FactorizationFailed("capacitance", error.toString)).flatMap: chol =>
      val projected = DMat.tabulate(h, n): (component, row) =>
        var sum = 0.0
        var feature = 0
        while feature < p do
          sum += data(row * p + feature) / d(feature) * u(feature * h + component)
          feature += 1
        sum
      chol.solve(projected).left.map(error => ResidualCovarianceError.FactorizationFailed("latent scores", error.toString)).map: solved =>
        val scores = new Array[Double](n * h)
        var weighted = 0.0
        var explained = 0.0
        var row = 0
        while row < n do
          var feature = 0
          while feature < p do
            val value = data(row * p + feature)
            weighted += value * value / d(feature)
            feature += 1
          var component = 0
          while component < h do
            val score = solved(component, row)
            scores(row * h + component) = score
            explained += projected(component, row) * score
            component += 1
          row += 1
        var logDeterminant = 0.0
        var feature = 0
        while feature < p do
          logDeterminant += math.log(d(feature))
          feature += 1
        var component = 0
        while component < h do
          logDeterminant += 2.0 * math.log(chol.lower(component, component))
          component += 1
        (chol, scores, -0.5 * n.toDouble * (p.toDouble * math.log(2.0 * math.Pi) + logDeterminant) - 0.5 * (weighted - explained))

  /** Planned own-code dense allocations of a fit, including matrices that
    * Gale returns (see [[ResidualCovarianceWork]]); integer arrays and object
    * overhead are not counted.
    * Fixed: residual copy `n p`; second moments, means and standardized means
    * `3 p`; partial SVD `n k + k + k p` (`k` the largest requested rank).
    * Per rank `h`, live state `u`, `d` (`p h + p`).
    * Per EM step: capacitance and its factor `2 h^2`, projections, solved
    * projections and scores `3 n h`; identity right-hand side, inverse, latent
    * moment and its factor `4 h^2`; `S beta^T`, the loading right-hand side
    * and update `3 p h` (`3 n h + 3 p h + 6 h^2`).
    * Finish: one more latent pass (`3 n h + 2 h^2`), the `d`/`u` clones
    * (`p + p h`), the augmented input `(p + h) h`, and the stable likelihood's
    * embedded and transformed row batches, `2 (p + h)` per row (all `n` rows
    * cumulatively, one chunk of at most `fallbackChunk` rows at peak).
    * Retained per rank: the model's stored cells
    * `2 p + p h + 2 (p + h) h + h`.
    * Peak bound: fixed, every retained result, and the largest single-rank
    * live state plus transient together. Cumulative: every allocation of every
    * rank. Largest dense shape: the biggest of these arrays by cell count.
    */
  private def fitWork(n: Int, p: Int, largest: Int, iterationsByRank: Vector[(Int, Int)]): Either[ResidualCovarianceError, ResidualCovarianceWork] =
    def add(values: Long*): Long = values.foldLeft(0L)((total, value) => Math.addExact(total, value))
    def mul(a: Long, b: Long): Long = Math.multiplyExact(a, b)
    val rows = n.toLong
    val cols = p.toLong
    val chunk = math.min(n, fallbackChunk).toLong
    def augmented(h: Long): Long = add(cols, h)
    def step(h: Long): Long = add(mul(3L * rows, h), mul(3L * cols, h), mul(6L, mul(h, h)))
    def state(h: Long): Long = add(mul(cols, h), cols)
    def finishBase(h: Long): Long = add(mul(3L * rows, h), mul(2L, mul(h, h)), cols, mul(cols, h), mul(augmented(h), h))
    def finishPeak(h: Long): Long = add(finishBase(h), mul(2L * augmented(h), chunk))
    def finishCumulative(h: Long): Long = add(finishBase(h), mul(2L * augmented(h), rows))
    def retained(h: Long): Long = add(2L * cols, mul(cols, h), mul(2L * augmented(h), h), h)
    try
      val k = largest.toLong
      val fixed = add(mul(rows, cols), 3L * cols, mul(rows, k), k, mul(k, cols))
      val retainedAll = iterationsByRank.foldLeft(0L)((total, entry) => add(total, retained(entry._2.toLong)))
      val transient = iterationsByRank.map((_, h) => add(state(h.toLong), math.max(step(h.toLong), finishPeak(h.toLong)))).max
      val cumulative = iterationsByRank.foldLeft(fixed): (total, entry) =>
        val (iterations, h) = entry
        add(total, state(h.toLong), mul(iterations.toLong, step(h.toLong)), finishCumulative(h.toLong), retained(h.toLong))
      val maximumRank = iterationsByRank.map(_._2).max
      val shapes = Vector((n, p), (n, largest), (largest, p), (p + maximumRank, maximumRank), (p + maximumRank, chunk.toInt), (maximumRank, n), (maximumRank, p))
      val (largestRows, largestColumns) = shapes.maxBy((r, c) => r.toLong * c)
      val primary = iterationsByRank.head._2.toLong
      Right(ResidualCovarianceWork(retained(primary), add(fixed, retainedAll, transient), cumulative, largestRows, largestColumns))
    catch case _: ArithmeticException => Left(ResidualCovarianceError.WorkOverflow)

  /** Complement below which the precision diagonal evaluates tail squares
    * directly instead of `1 - |(Q_1)_i|^2`: a heuristic cancellation guard,
    * not an accuracy certificate.
    */
  val fastDiagonalFloor: Double = 1e-6

  /** Column chunk for exact precision-diagonal rows and likelihood rows. */
  val fallbackChunk: Int = 64

  private[pattern] def fromArrays[N](neuralAxis: AxisRef[N], diagonal: Array[Double], loadings: Array[Double], rank: Int): Either[ResidualCovarianceError, ResidualCovariance[N]] =
    val p = diagonal.length
    if rank < 1 then Left(ResidualCovarianceError.InvalidRank(rank, math.max(rank, 1)))
    else if p != neuralAxis.size then Left(ResidualCovarianceError.Shape("diagonal", neuralAxis.size, 1, p, 1))
    else if loadings.length != p * rank then Left(ResidualCovarianceError.Shape("loadings", p, rank, loadings.length / rank, rank))
    else if loadings.exists(value => !value.isFinite) then Left(ResidualCovarianceError.NonFinite("loadings"))
    else diagonal.indices.find(index => !(diagonal(index) > 0.0 && diagonal(index).isFinite)) match
      case Some(index) => Left(ResidualCovarianceError.NonPositiveDiagonal(index))
      case None =>
        val augmentedLoadings = DMat.tabulate(p + rank, rank): (row, column) =>
          if row < p then loadings(row * rank + column) / math.sqrt(diagonal(row)) else if row - p == column then 1.0 else 0.0
        if !finite(augmentedLoadings) then Left(ResidualCovarianceError.NonFinite("whitened loadings"))
        else
          val factor = augmentedLoadings.qr
          val pivots = (0 until rank).map(component => math.abs(factor.r(component, component)))
          if pivots.exists(pivot => !(pivot > 0.0 && pivot.isFinite)) || !finite(factor.r) then
            Left(ResidualCovarianceError.FactorizationFailed("augmented loading QR", s"non-positive or non-finite R diagonal ${pivots.mkString(",")}"))
          else Right(new ResidualCovariance(neuralAxis, diagonal, loadings, rank, factor))

  /** `out = U^T x` for row-major `U` (`p x h`) and a `p x k` block; `out` is `h x k` row-major. */
  private def multiplyTransposeInto(loadings: Array[Double], p: Int, h: Int, values: DMat, out: Array[Double]): Unit =
    val k = values.cols
    var feature = 0
    while feature < p do
      var component = 0
      while component < h do
        val loading = loadings(feature * h + component)
        var column = 0
        while column < k do
          out(component * k + column) += loading * values(feature, column)
          column += 1
        component += 1
      feature += 1

  private def keyIndex[N](axis: AxisRef[N]): Either[ResidualCovarianceError, Map[String, Int]] =
    keys(axis).map(_.zipWithIndex.toMap)

  private def keys[N](axis: AxisRef[N]): Either[ResidualCovarianceError, Vector[String]] =
    traverse((0 until axis.size).toVector)(ordinal => axis.index.stableKeyAt(ordinal).left.map(ResidualCovarianceError.Evidence.apply))

  private[pattern] def finite(matrix: DMat): Boolean =
    var row = 0
    var ok = true
    while ok && row < matrix.rows do
      var column = 0
      while ok && column < matrix.cols do
        ok = matrix(row, column).isFinite
        column += 1
      row += 1
    ok

  private def traverse[A, B](values: Vector[A])(step: A => Either[ResidualCovarianceError, B]): Either[ResidualCovarianceError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var index = 0
    var failure: Option[ResidualCovarianceError] = None
    while index < values.length && failure.isEmpty do
      step(values(index)) match
        case Left(error) => failure = Some(error)
        case Right(value) => out += value
      index += 1
    failure.toLeft(out.result())
