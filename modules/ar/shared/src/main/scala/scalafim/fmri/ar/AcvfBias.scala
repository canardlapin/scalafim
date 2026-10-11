package scalafim.fmri.ar

import gale.linalg.{DMat, DVec, LinAlgError, Matrix, QROptions, QRPivoting, Vec}

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
/** Exact identity of a design: dimensions plus a 64-bit FNV-1a hash over `doubleToLongBits` of every entry
  * (row-major), and the numerical rank it was prepared with. Two designs with equal fingerprints are the same
  * matrix up to hash collision; a nested design (same columns plus or minus some) never matches.
  */
final case class DesignFingerprint private[ar] (rows: Int, cols: Int, hash: Long, rank: Int)

/** What a prepared correction is bound to besides the layout. */
final case class PreparedBinding private[ar] (
    fingerprint: DesignFingerprint,
    budget: CorrectionBudget,
    targetOrder: Int,
    private[ar] basis: AcvfBias.DesignBasis,
    solvePolicy: AcvfCorrectionSolve = AcvfCorrectionSolve.Exact
)

final case class PreparedCorrection private[ar] (
    matrices: AcvfBiasMatrices,
    runs: Vector[RunCorrection],
    layout: NoiseEstimationLayout,
    binding: Option[PreparedBinding],
    private[ar] operators: Vector[Option[AcvfCorrection]] = Vector.empty
):
  /** The matrix to solve against for `run`, or `None` when the run is left uncorrected. */
  def usable(run: Int): Option[DMat] = correctionFor(run).map(_.matrix)

  private[ar] def correctionFor(run: Int): Option[AcvfCorrection] =
    runs(run) match
      case RunCorrection.Applied(_) | RunCorrection.AppliedWithTailAnchor(_, _) =>
        if operators.isEmpty then Some(AcvfCorrection(matrices.byRun(run))) else operators(run)
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
  private[ar] final class DesignBasis(val rows: Int, val rank: Int, val q: DMat)

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
      val factor = pivotedQr(design)
      val rank = factor.diagnostics.rank.getOrElse(0)
      if rank == 0 then Right(new DesignBasis(design.rows, 0, design))
      else Right(new DesignBasis(design.rows, rank, factor.q.slice(0, design.rows, 0, rank)))

  private def pivotedQr(matrix: DMat) = matrix.qr(QROptions(pivoting = QRPivoting.Column))

  /** The one rank source: Gale's column-pivoted QR with its default tolerance. Used for the design basis, the
    * residual degrees of freedom, and the adaptive budget's per-run rdf, so the three can never disagree. R's
    * `qr()` (LINPACK `dqrdc2`, tol 1e-7 on column norms) can differ on numerically near-deficient designs.
    */
  private[ar] def numericalRank(matrix: DMat): Int =
    if matrix.rows == 0 || matrix.cols == 0 then 0 else pivotedQr(matrix).diagnostics.rank.getOrElse(0)

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

  /** Build the per-run bias matrices once for a design and layout, gate each on conditioning, and keep the
    * design basis so every later residual set is still validated for orthogonality.
    *
    * The result is bound to this design and layout: it can only be used with residuals of the same row count
    * and an equal layout ([[bind]] checks both). Callers fitting many residual sets (voxels, records) against
    * one design should prepare once and pass the result to `ArEstimation.fitNoise`, `NoiseFit.estimate` or
    * `NoiseAcvf.estimate`.
    *
    * @param targetOrder the AR order being fitted; only used by [[CorrectionBudget.Adaptive]]
    */
  def prepare(
      design: DMat,
      layout: NoiseEstimationLayout,
      budget: CorrectionBudget,
      targetOrder: Int,
      solvePolicy: AcvfCorrectionSolve = AcvfCorrectionSolve.Exact
  ): Either[ArError, PreparedCorrection] =
    prepareChecked(design, layout, budget, targetOrder, None, solvePolicy)

  /** The policy path: the basis is computed and the residuals validated before any bias matrix is built, so bad
    * residuals fail fast.
    */
  private[ar] def prepareChecked(
      design: DMat,
      layout: NoiseEstimationLayout,
      budget: CorrectionBudget,
      targetOrder: Int,
      residuals: Option[DMat],
      solvePolicy: AcvfCorrectionSolve = AcvfCorrectionSolve.Exact
  ): Either[ArError, PreparedCorrection] =
    if design.rows != layout.rows then Left(ArError.DesignRowMismatch(design.rows, layout.rows))
    else
      for
        basis <- designBasis(design)
        _ <- residuals.fold[Either[ArError, Double]](Right(0.0))(validateResiduals(_, basis, OrthogonalityTolerance))
        lag <- resolveLag(budget, design, layout, targetOrder)
        built <- buildMatrices(basis, layout, lag)
        operators <- AcvfCorrection.prepare(built, layout, solvePolicy)
      yield PreparedCorrection(
        built,
        built.byRun.zipWithIndex.map { (matrix, run) =>
          val rcond = reciprocalCondition(matrix)
          val directions = operators(run).fold(0)(_.anchoredDirections)
          if rcond.isFinite && rcond >= ReciprocalConditionFloor && directions > 0 then RunCorrection.AppliedWithTailAnchor(rcond, directions)
          else if rcond.isFinite && rcond >= ReciprocalConditionFloor then RunCorrection.Applied(rcond)
          else RunCorrection.IllConditioned(rcond)
        },
        layout,
        Some(PreparedBinding(fingerprint(design, basis.rank), budget, targetOrder, basis, solvePolicy)),
        operators
      )

  private[ar] def fingerprint(design: DMat, rank: Int): DesignFingerprint =
    var hash = 0xcbf29ce484222325L
    var row = 0
    while row < design.rows do
      var col = 0
      while col < design.cols do
        hash = (hash ^ java.lang.Double.doubleToLongBits(design(row, col))) * 0x100000001b3L
        col += 1
      row += 1
    DesignFingerprint(design.rows, design.cols, hash, rank)

  /** Check a prepared correction against one residual set. The caller supplies the design again and it must be
    * exactly the prepared one (dimensions and a hash of every entry's bits), so a nested design, which residuals
    * are also orthogonal to, cannot be substituted. Also checked: row count, equal layout, the AR order the
    * correction was prepared for, and numerical orthogonality of these residuals to the design.
    *
    * The order is checked for every budget, not only [[CorrectionBudget.Adaptive]] (where it changes the lag):
    * a correction is prepared for one fit configuration.
    */
  def bind(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      design: DMat,
      targetOrder: Int,
      prepared: PreparedCorrection
  ): Either[ArError, PreparedCorrection] =
    if prepared.layout.rows != residuals.rows then
      Left(ArError.DesignRowMismatch(prepared.layout.rows, residuals.rows))
    else if design.rows != residuals.rows then Left(ArError.DesignRowMismatch(design.rows, residuals.rows))
    else if prepared.layout != layout then Left(ArError.PreparedCorrectionLayoutMismatch)
    else
      prepared.binding match
        case None => Right(prepared)
        case Some(binding) =>
          if fingerprint(design, binding.fingerprint.rank) != binding.fingerprint then
            Left(ArError.PreparedCorrectionDesignMismatch)
          else if binding.targetOrder != targetOrder then
            Left(ArError.PreparedCorrectionOrderMismatch(binding.targetOrder, targetOrder))
          else validateResiduals(residuals, binding.basis, OrthogonalityTolerance).map(_ => prepared)

  private[ar] def resolveLag(
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
      keep.length - numericalRank(sub)

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
    val rank = basis.rank
    val dim = maxLag + 1
    val qKeep = Matrix.tabulate(nv, rank)((row, col) => basis.q(keep(row), col))
    val qMeans = Array.tabulate(rank): col =>
      var total = 0.0
      var row = 0
      while row < nv do
        total += qKeep(row, col)
        row += 1
      total / nv.toDouble
    val u = Matrix.tabulate(nv, rank)((row, col) => qKeep(row, col) - qMeans(col))
    val selected = new Array[Double](n)
    keep.foreach(row => selected(row) = 1.0)

    val pairHi = new Array[Array[Int]](dim)
    val pairLo = new Array[Array[Int]](dim)
    var h = 0
    while h < dim do
      val hi = Array.newBuilder[Int]
      val lo = Array.newBuilder[Int]
      var row = h
      while row < nv do
        if segmentOf(row) == segmentOf(row - h) then
          hi += row
          lo += row - h
        row += 1
      pairHi(h) = hi.result()
      pairLo(h) = lo.result()
      h += 1
    val out = Matrix.newBuilder(dim, dim)
    var diagonal = 0
    while diagonal < dim do
      out(diagonal, diagonal) = 1.0
      diagonal += 1

    // R = B - U Q', where B selects and centers valid rows. Expand R S_k R'
    // through the rank-sized design basis, avoiding both n_valid-by-n buffers.
    var k = 0
    while k < dim do
      val skq = Matrix.tabulate(n, rank): (row, col) =>
        if k == 0 then basis.q(row, col)
        else
          var total = 0.0
          if row >= k && runOf(row) == runOf(row - k) then total += basis.q(row - k, col)
          if row + k < n && runOf(row) == runOf(row + k) then total += basis.q(row + k, col)
          total
      val e = Array.tabulate(nv): position =>
        val row = keep(position)
        if k == 0 then selected(row)
        else
          var total = 0.0
          if row >= k && runOf(row) == runOf(row - k) then total += selected(row - k)
          if row + k < n && runOf(row) == runOf(row + k) then total += selected(row + k)
          total
      val means = Array.tabulate(rank): col =>
        var total = 0.0
        var row = 0
        while row < nv do
          total += skq(keep(row), col)
          row += 1
        total / nv.toDouble
      val x = Matrix.tabulate(nv, rank)((row, col) => skq(keep(row), col) - means(col))
      val g = basis.q.t * skq
      val ug = u * g // Hoist this rank-squared product outside the observed-lag loop.
      val msm = e.sum
      h = 0
      while h < dim do
        val hi = pairHi(h)
        val lo = pairLo(h)
        if hi.nonEmpty then
          var centering = 0.0
          var t2 = 0.0
          var t3 = 0.0
          var t4 = 0.0
          var pair = 0
          while pair < hi.length do
            val a = hi(pair)
            val b = lo(pair)
            centering += e(a) + e(b)
            var col = 0
            while col < rank do
              t2 += x(a, col) * u(b, col)
              t3 += x(b, col) * u(a, col)
              t4 += ug(a, col) * u(b, col)
              col += 1
            pair += 1
          val count = hi.length.toDouble
          val t1 = (if h == k then count else 0.0) - centering / nv.toDouble + count * msm / (nv.toDouble * nv.toDouble)
          out(h, k) = (t1 - t2 - t3 + t4) / count
        h += 1
      k += 1
    out.result()

  /** Solve `A gamma_true = gamma_raw`, mirroring fmriAR's `.apply_acvf_correction_result()`.
    *
    * The leading `min(length, A.rows)` block of `A` is gated on its own reciprocal condition number and solved;
    * lags beyond the matrix are carried through unchanged. When the block is ill-conditioned, singular, yields a
    * non-finite solution or a non-positive variance, the caller keeps the raw vector (as fmriAR does) and the
    * typed reason is reported. `solve` is a test seam for forcing solver failures.
    */
  private[ar] def correct(gamma: Vector[Double], correction: AcvfCorrection): Either[CorrectionFallback, Vector[Double]] =
    correct(gamma, correction.matrix, correction.solve)

  private[ar] def correct(
      gamma: Vector[Double],
      matrix: DMat,
      solve: (DMat, DVec) => Either[LinAlgError, DVec] = (block, rhs) => block.solve(rhs)
  ): Either[CorrectionFallback, Vector[Double]] =
    if gamma.isEmpty || !gamma.head.isFinite || gamma.head <= 0.0 then
      Left(CorrectionFallback.NonPositiveRawVariance(gamma.headOption.getOrElse(Double.NaN)))
    else
      val size = math.min(gamma.length, matrix.rows)
      val block = matrix.slice(0, size, 0, size)
      val rcond = reciprocalCondition(block)
      if !rcond.isFinite || rcond < ReciprocalConditionFloor then Left(CorrectionFallback.IllConditionedBlock(rcond))
      else
        solve(block, Vec.tabulate(size)(i => gamma(i))) match
          case Left(_) => Left(CorrectionFallback.SingularSystem)
          case Right(solution) =>
            val solved = Vector.tabulate(size)(i => solution(i))
            if !solved.forall(_.isFinite) then Left(CorrectionFallback.NonFiniteSolution)
            else if solved.head <= 0.0 then Left(CorrectionFallback.NonPositiveVariance(solved.head))
            else Right(solved ++ gamma.drop(size))
