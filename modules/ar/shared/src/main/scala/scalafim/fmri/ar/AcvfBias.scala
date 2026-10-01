package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix, QROptions, QRPivoting, Vec}

/** The linear map `A` with `E[gamma_raw] = A gamma_true` induced by projecting a design out of the data.
  *
  * Rows are indexed by the lag of the raw estimate and columns by the lag of the truth. One matrix is kept per
  * run because each run is estimated separately and the residual operator restricted to a run still involves the
  * whole design.
  *
  * @param requestedLag the lag budget asked for (after clamping to the number of rows)
  * @param lag the lag budget actually used; smaller than `requestedLag` when the design leaves too few residual
  *            degrees of freedom to recover that many lags
  * @param residualDf rows minus the numerical rank of the design
  */
final case class AcvfBiasMatrices private[ar] (
    requestedLag: Int,
    lag: Int,
    residualDf: Int,
    byRun: Vector[DMat]
):
  def budgetCapped: Boolean = lag < requestedLag

/** Bias matrices together with the per-run outcome of the conditioning gate. */
final case class PreparedCorrection private[ar] (
    matrices: AcvfBiasMatrices,
    runs: Vector[RunCorrection]
):
  /** The matrix to solve against for `run`, or `None` when the run is left uncorrected. */
  def usable(run: Int): Option[DMat] =
    runs(run) match
      case RunCorrection.Applied(_) => Some(matrices.byRun(run))
      case _                        => None

/** Design-aware residual-bias correction of the raw autocovariance, mirroring fmriAR's `acvf_bias_matrix()`.
  *
  * With residuals `e = M y` and `M = I - QQ'`, `E[e e'] = M Sigma M`, so every lag product mixes in structure from
  * the whole design. `Sigma = sum_k gamma_k S_k`, with `S_k` the lag-`k` indicator that does not cross a run
  * boundary, and expectation is linear in `gamma`:
  *
  * {{{
  * A[h,k] = (1 / pairs_h) * sum_{(a,b) in P_h} (R S_k R')[a,b]
  * }}}
  *
  * where `R` is the operator carrying the data to the residuals the estimator actually accumulates (projection,
  * then per-run centering over the surviving rows) and `P_h` is the set of lag-`h` pairs the estimator actually
  * uses. Both are read off the same estimation layout the estimator uses, so censoring and run boundaries are
  * handled by construction. The bias is undone by solving `A gamma = gamma_raw`.
  *
  * The correction is exact for noise whose autocovariance dies within the lag budget; under long memory it is
  * partial because truncating the system aliases in structure beyond the budget.
  */
object AcvfBias:

  /** A bias matrix whose reciprocal 1-norm condition number falls below this is not solved against. */
  val ReciprocalConditionFloor: Double = 1e-6

  /** Largest accepted `||Q'e|| / ||e||` for residuals claimed to be least-squares residuals of the design. */
  val OrthogonalityTolerance: Double = 1e-6

  /** Orthonormal basis of the design column space: `q` is `rows x rank`. */
  private final class DesignBasis(val rows: Int, val rank: Int, val q: DMat)

  private def designBasis(design: DMat): Either[ArError, DesignBasis] =
    var row = 0
    while row < design.rows do
      var col = 0
      while col < design.cols do
        val value = design(row, col)
        if !value.isFinite then return Left(ArError.NonFiniteDesign(row, col, value))
        col += 1
      row += 1
    if design.cols == 0 then Right(new DesignBasis(design.rows, 0, design))
    else
      val factor = design.qr(QROptions(pivoting = QRPivoting.Column))
      val rank = factor.diagnostics.rank.getOrElse(0)
      if rank == 0 then Right(new DesignBasis(design.rows, 0, design))
      else Right(new DesignBasis(design.rows, rank, factor.q.slice(0, design.rows, 0, rank)))

  /** Check that `residuals` are numerically orthogonal to `design`, the necessary condition for them to be its
    * ordinary least-squares residuals. Returns the relative projection `||Q'e||_F / ||e||_F`.
    *
    * Orthogonality cannot prove provenance (residuals of a nested design are also orthogonal to it), so the
    * caller remains responsible for supplying the matching design.
    */
  def validateResiduals(
      residuals: DMat,
      design: DMat,
      tolerance: Double = OrthogonalityTolerance
  ): Either[ArError, Double] =
    if design.rows != residuals.rows then Left(ArError.DesignRowMismatch(design.rows, residuals.rows))
    else designBasis(design).flatMap(validateResiduals(residuals, _, tolerance))

  private def validateResiduals(
      residuals: DMat,
      basis: DesignBasis,
      tolerance: Double
  ): Either[ArError, Double] =
    if basis.rank == 0 || residuals.cols == 0 then Right(0.0)
    else
      val norm = frobenius(residuals)
      if !norm.isFinite || norm == 0.0 then Right(0.0)
      else
        val relative = frobenius(basis.q.t * residuals) / norm
        if !relative.isFinite || relative > tolerance then
          Left(ArError.DesignResidualMismatch(relative, tolerance))
        else Right(relative)

  private def frobenius(matrix: DMat): Double =
    var sum = 0.0
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val value = matrix(row, col)
        sum += value * value
        col += 1
      row += 1
    math.sqrt(sum)

  /** Reciprocal 1-norm condition number; zero when the matrix is singular or the estimate is not finite. */
  def reciprocalCondition(matrix: DMat): Double =
    matrix.conditionEstimate match
      case Right(cond) if cond.isFinite && cond > 0.0 => 1.0 / cond
      case _                                          => 0.0

  /** One bias matrix per run, mirroring `acvf_bias_matrix()`. The lag budget is clamped to the row count and then
    * reduced to `max(1, df - 1)` when the design leaves fewer than `maxLag + 1` residual degrees of freedom.
    */
  def matrices(design: DMat, layout: NoiseEstimationLayout, maxLag: Int): Either[ArError, AcvfBiasMatrices] =
    if design.rows != layout.rows then Left(ArError.DesignRowMismatch(design.rows, layout.rows))
    else if maxLag < 1 then Left(ArError.InvalidCorrectionLag(maxLag))
    else designBasis(design).flatMap(buildMatrices(_, layout, math.min(maxLag, layout.rows)))

  /** Validate the residuals against the design, resolve the lag budget, build the per-run matrices, and gate each
    * on conditioning.
    *
    * @param targetOrder the AR order being fitted; only used by [[CorrectionBudget.Adaptive]]
    */
  def prepare(
      design: DMat,
      residuals: DMat,
      layout: NoiseEstimationLayout,
      budget: CorrectionBudget,
      targetOrder: Int
  ): Either[ArError, PreparedCorrection] =
    if design.rows != residuals.rows then Left(ArError.DesignRowMismatch(design.rows, residuals.rows))
    else
      for
        basis <- designBasis(design)
        _ <- validateResiduals(residuals, basis, OrthogonalityTolerance)
        lag <- resolveLag(budget, design, layout, targetOrder)
        built <- buildMatrices(basis, layout, lag)
      yield PreparedCorrection(
        built,
        built.byRun.map { matrix =>
          val rcond = reciprocalCondition(matrix)
          if rcond.isFinite && rcond >= ReciprocalConditionFloor then RunCorrection.Applied(rcond)
          else RunCorrection.IllConditioned(rcond)
        }
      )

  private def resolveLag(
      budget: CorrectionBudget,
      design: DMat,
      layout: NoiseEstimationLayout,
      targetOrder: Int
  ): Either[ArError, Int] =
    budget match
      case CorrectionBudget.Fixed(maxLag) =>
        if maxLag < 1 then Left(ArError.InvalidCorrectionLag(maxLag))
        else Right(math.min(maxLag, layout.rows))
      case CorrectionBudget.Adaptive(ceiling) =>
        if ceiling < 1 then Left(ArError.InvalidCorrectionLag(ceiling))
        else
          val order = math.max(1, targetOrder)
          val minRdf = (0 until layout.runCount).map(run => runResidualDf(design, layout, run)).min
          val desired = math.max(5, 2 * order + 1)
          val supported = math.max(0, minRdf / 3)
          Right(math.min(math.max(order, math.min(ceiling, math.min(desired, supported))), layout.rows))

  private def runResidualDf(design: DMat, layout: NoiseEstimationLayout, run: Int): Int =
    val keep = keptRows(layout, run)
    if keep.isEmpty then 0
    else
      val sub = Matrix.tabulate(keep.length, design.cols)((row, col) => design(keep(row), col))
      keep.length - sub.rankEstimate

  /** Rows of `run` that survive censoring, in time order. */
  private def keptRows(layout: NoiseEstimationLayout, run: Int): Array[Int] =
    val out = Array.newBuilder[Int]
    layout.segmentsForRun(run).foreach { segment =>
      var row = segment.start
      while row < segment.endExclusive do
        out += row
        row += 1
    }
    out.result()

  private def buildMatrices(
      basis: DesignBasis,
      layout: NoiseEstimationLayout,
      requestedLag: Int
  ): Either[ArError, AcvfBiasMatrices] =
    val rows = layout.rows
    val df = rows - basis.rank
    if df <= 0 then Left(ArError.NoResidualDegreesOfFreedom(rows, basis.rank))
    else
      // Recovering lag + 1 autocovariances needs at least that many residual dimensions to recover them from.
      // Below the boundary the correction degrades and individual fits pin phi at the stationarity clamp.
      val lag = if df < requestedLag + 1 then math.max(1, df - 1) else requestedLag
      val runOf = new Array[Int](rows)
      layout.whiteningSegments.foreach { segment =>
        var row = segment.start
        while row < segment.endExclusive do
          runOf(row) = segment.runIndex
          row += 1
      }
      val byRun = Vector.tabulate(layout.runCount) { run =>
        val keep = keptRows(layout, run)
        if keep.length < 2 then identityMatrix(lag + 1)
        else
          val segmentOf = new Array[Int](keep.length)
          var position = 0
          var ordinal = 0
          layout.segmentsForRun(run).foreach { segment =>
            var row = segment.start
            while row < segment.endExclusive do
              segmentOf(position) = ordinal
              position += 1
              row += 1
            ordinal += 1
          }
          runMatrix(basis, runOf, keep, segmentOf, lag)
      }
      Right(AcvfBiasMatrices(requestedLag, lag, df, byRun))

  private def identityMatrix(size: Int): DMat =
    Matrix.tabulate(size, size)((row, col) => if row == col then 1.0 else 0.0)

  /** Bias matrix for one run. `keep` are the run's surviving rows in time order and `segmentOf` their segment
    * ordinals; lag products only pair rows of the same segment.
    */
  private def runMatrix(
      basis: DesignBasis,
      runOf: Array[Int],
      keep: Array[Int],
      segmentOf: Array[Int],
      maxLag: Int
  ): DMat =
    val n = basis.rows
    val nv = keep.length
    val dim = maxLag + 1

    // R = C (I - QQ')[keep, :], with C centering the surviving rows (one group per run).
    val resid = new Array[Double](nv * n)
    if basis.rank > 0 then
      val qKeep = Matrix.tabulate(nv, basis.rank)((row, col) => basis.q(keep(row), col))
      val projection = qKeep * basis.q.t
      var a = 0
      while a < nv do
        var j = 0
        while j < n do
          resid(a * n + j) = -projection(a, j)
          j += 1
        a += 1
    var a = 0
    while a < nv do
      resid(a * n + keep(a)) += 1.0
      a += 1
    var j = 0
    while j < n do
      var sum = 0.0
      a = 0
      while a < nv do
        sum += resid(a * n + j)
        a += 1
      val mean = sum / nv.toDouble
      a = 0
      while a < nv do
        resid(a * n + j) -= mean
        a += 1
      j += 1

    // Lag-h pairs actually used by the estimator: (hi, lo) positions within the surviving rows.
    val pairHi = new Array[Array[Int]](dim)
    val pairLo = new Array[Array[Int]](dim)
    pairHi(0) = Array.tabulate(nv)(i => i)
    pairLo(0) = pairHi(0)
    var lag = 1
    while lag <= maxLag do
      if nv <= lag then
        pairHi(lag) = Array.emptyIntArray
        pairLo(lag) = Array.emptyIntArray
      else
        val hi = Array.newBuilder[Int]
        val lo = Array.newBuilder[Int]
        var i = lag
        while i < nv do
          if segmentOf(i) == segmentOf(i - lag) then
            hi += i
            lo += i - lag
          i += 1
        pairHi(lag) = hi.result()
        pairLo(lag) = lo.result()
      lag += 1

    val out = new Array[Double](dim * dim)
    var d = 0
    while d < dim do
      out(d * dim + d) = 1.0
      d += 1

    val shifted = new Array[Double](nv * n)
    var k = 0
    while k <= maxLag do
      // R S_k: (S_k v)[j] = v[j - k] + v[j + k], keeping only neighbours inside the same run.
      if k == 0 then System.arraycopy(resid, 0, shifted, 0, nv * n)
      else
        java.util.Arrays.fill(shifted, 0.0)
        if k < n then
          var b = 0
          while b < nv do
            val base = b * n
            var col = k
            while col < n do
              if runOf(col) == runOf(col - k) then
                shifted(base + col) += resid(base + col - k)
                shifted(base + col - k) += resid(base + col)
              col += 1
            b += 1
      var h = 0
      while h <= maxLag do
        val hi = pairHi(h)
        val lo = pairLo(h)
        if hi.length > 0 then
          var total = 0.0
          var p = 0
          while p < hi.length do
            val left = hi(p) * n
            val right = lo(p) * n
            var col = 0
            while col < n do
              total += resid(left + col) * shifted(right + col)
              col += 1
            p += 1
          out(h * dim + k) = total / hi.length.toDouble
        h += 1
      k += 1

    Matrix.tabulate(dim, dim)((row, col) => out(row * dim + col))

  /** Solve `A gamma_true = gamma_raw`, mirroring fmriAR's `.apply_acvf_correction_result()`.
    *
    * The leading `min(length, A.rows)` block of `A` is gated on its own reciprocal condition number and solved;
    * lags beyond the matrix are carried through unchanged. When the block is ill-conditioned, singular, yields a
    * non-finite solution or a non-positive variance, the raw vector comes back with `applied = false`.
    */
  private[ar] def correct(gamma: Vector[Double], matrix: DMat): (Vector[Double], Boolean) =
    val size = math.min(gamma.length, matrix.rows)
    if size < 1 then (gamma, false)
    else
      val block = matrix.slice(0, size, 0, size)
      val rcond = reciprocalCondition(block)
      if !rcond.isFinite || rcond < ReciprocalConditionFloor then (gamma, false)
      else
        block.solve(Vec.tabulate(size)(i => gamma(i))) match
          case Left(_) => (gamma, false)
          case Right(solution) =>
            val solved = Vector.tabulate(size)(i => solution(i))
            if !solved.forall(_.isFinite) || solved.head <= 0.0 then (gamma, false)
            else (solved ++ gamma.drop(size), true)
