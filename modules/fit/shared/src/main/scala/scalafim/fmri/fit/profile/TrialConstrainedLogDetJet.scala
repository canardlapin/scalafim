package scalafim.fmri.fit.profile

import gale.linalg.{BandedCholesky, BandedDerivatives, DMat, DMatBuilder}
import scalafim.fmri.hrf.family.JetLayout

/** Fixed engineering admission for every solved equation: the max-column
  * normalized backward residual `‖A uⱼ - rⱼ‖∞ / (‖A‖∞ ‖uⱼ‖∞ + ‖rⱼ‖∞)`, with
  * 0/0 = 0, against the original `A` bands including lambda. It is an empirical
  * solve-residual check, not a forward-error or scientific certificate.
  */
private[profile] final case class TrialDeterminantNumerics private (residualTolerance: Double)

private[profile] object TrialDeterminantNumerics:
  val Default: TrialDeterminantNumerics = new TrialDeterminantNumerics(1e-10)

  def apply(residualTolerance: Double): Either[TrialDeterminantError, TrialDeterminantNumerics] =
    if residualTolerance > 0.0 && residualTolerance.isFinite then Right(new TrialDeterminantNumerics(residualTolerance))
    else Left(TrialDeterminantError.InvalidTolerance(residualTolerance))

private[profile] enum TrialDeterminantStage:
  case TrialFactor
  case MembershipSolve
  case FirstSolve(axis: Int)
  case SecondSolve(p: Int, q: Int)
  case TrialLogDetJet
  case ConditionFactor
  case ConditionLogDetJet
  case Result

private[profile] enum TrialDeterminantError:
  case InvalidDimension(dimension: Int, familyDimension: Int)
  case NonFiniteCoordinate(axis: Int, value: Double)
  case InvalidLambda(value: Double)
  case InvalidTolerance(value: Double)
  case DerivativeCount(expectedFirst: Int, first: Int, expectedSecond: Int, second: Int)
  case BandShape(expectedRows: Int, expectedCols: Int, rows: Int, cols: Int)
  case NonFiniteBand(band: String, row: Int, delta: Int, value: Double)
  case Solve(stage: TrialDeterminantStage, detail: String)
  case Residual(stage: TrialDeterminantStage, residual: Double, tolerance: Double)
  case Factor(stage: TrialDeterminantStage, detail: String)
  case LogDetJet(stage: TrialDeterminantStage, detail: String)
  case NonFinite(stage: TrialDeterminantStage)

  def message: String =
    this match
      case InvalidDimension(d, family) => s"determinant jet dimension $d must be 1..3 and match family dimension $family"
      case NonFiniteCoordinate(axis, value) => s"shape coordinate $axis must be finite, got $value"
      case InvalidLambda(value) => s"intrinsic lambda must be finite and > 0, got $value"
      case InvalidTolerance(value) => s"residual tolerance must be finite and > 0, got $value"
      case DerivativeCount(ef, f, es, s) => s"expected $ef first and $es second derivative bands, got $f and $s"
      case BandShape(er, ec, r, c) => s"trial lower band must be $er-by-$ec, got $r-by-$c"
      case NonFiniteBand(band, row, delta, value) => s"nonfinite active $band band entry at ($row,$delta): $value"
      case Solve(stage, detail) => s"$stage banded solve failed: $detail"
      case Residual(stage, r, tol) => s"$stage normalized solve residual $r exceeds admitted $tol"
      case Factor(stage, detail) => s"$stage factorisation refused: $detail"
      case LogDetJet(stage, detail) => s"$stage log-determinant jet refused: $detail"
      case NonFinite(stage) => s"$stage produced a nonfinite value"

/** Work for one attempted accepted-band construction, separate from evaluation.
  * `constructionRefusals` are validation refusals before any factorisation;
  * `factorAttempts`/`factorFailures` count the single N-sized Gale factorisation.
  * `retainedBandDoubles` is the frozen canonical A band kept by the bundle.
  */
private[profile] final case class TrialBandFactorWork(
    constructionAttempts: Long,
    constructionRefusals: Long,
    factorAttempts: Long,
    factorFailures: Long,
    retainedBandDoubles: Long)

private[profile] final case class TrialBandFactorAttempt(
    outcome: Either[TrialDeterminantError, TrialAcceptedTrialBand],
    work: TrialBandFactorWork)

/** A stamped reference bundle: the lower band of `A = X'X + lambda I` at ONE
  * shape point, copied into canonical owned storage (active entries only,
  * padding zeroed) and frozen, together with the Gale factor of exactly that
  * frozen copy, made in the same call. The constructor is private, so the
  * factor cannot be paired with any other band, and later mutation of the
  * caller's source matrix cannot reach it. Evaluation reuses this factor and
  * performs no further N-sized factorisation.
  */
private[profile] final class TrialAcceptedTrialBand private (
    val preparation: TrialBandedPreparation,
    val coordinates: Vector[Double],
    val lambda: Double,
    val band: DMat,
    val factor: BandedCholesky)

private[profile] object TrialAcceptedTrialBand:
  /** Intrinsic convention: lambda is the preparation's frozen lambda. */
  def atPreparationLambda(
      preparation: TrialBandedPreparation,
      coordinates: Vector[Double],
      value: DMat
  ): TrialBandFactorAttempt =
    factorize(preparation, coordinates, preparation.lambda, value)

  /** Explicit lambda, for callers that rescale the whole design consistently
    * (`X* = sX` with `lambda* = s² lambda`); `value` must already include it.
    * All validation precedes the factorisation.
    */
  def factorize(
      preparation: TrialBandedPreparation,
      coordinates: Vector[Double],
      lambda: Double,
      value: DMat
  ): TrialBandFactorAttempt =
    val n = preparation.trials
    val width = preparation.bandWidth
    def refused(error: TrialDeterminantError): TrialBandFactorAttempt =
      TrialBandFactorAttempt(Left(error), TrialBandFactorWork(1L, 1L, 0L, 0L, 0L))
    val d = coordinates.length
    val family = preparation.basis.family.dimension
    if d < 1 || d > BandedDerivatives.MaxDimension || d != family then
      return refused(TrialDeterminantError.InvalidDimension(d, family))
    val badAxis = coordinates.indexWhere(!_.isFinite)
    if badAxis >= 0 then return refused(TrialDeterminantError.NonFiniteCoordinate(badAxis, coordinates(badAxis)))
    if !(lambda > 0.0 && lambda.isFinite) then return refused(TrialDeterminantError.InvalidLambda(lambda))
    if value.rows != n || value.cols != width then
      return refused(TrialDeterminantError.BandShape(n, width, value.rows, value.cols))
    val canonical = DMatBuilder.zeros(n, width)
    var row = 0
    while row < n do
      var delta = 0
      while delta <= math.min(row, width - 1) do
        val entry = value(row, delta)
        if !entry.isFinite then return refused(TrialDeterminantError.NonFiniteBand("value", row, delta, entry))
        canonical(row, delta) = entry
        delta += 1
      row += 1
    val frozen = canonical.result()
    val retained = n.toLong * width
    BandedCholesky.factorLower(frozen) match
      case Left(error) =>
        TrialBandFactorAttempt(Left(TrialDeterminantError.Factor(TrialDeterminantStage.TrialFactor, error.getMessage)),
          TrialBandFactorWork(1L, 0L, 1L, 1L, 0L))
      case Right(factor) =>
        TrialBandFactorAttempt(Right(new TrialAcceptedTrialBand(preparation, coordinates, lambda, frozen, factor)),
          TrialBandFactorWork(1L, 0L, 1L, 0L, retained))

/** An accepted reference bundle plus its shape jets. Bands use the lower layout
  * `A(row, row-delta)`, shape `N x (b+1)`; padding at `delta > row` is ignored.
  * `second` is in ScalaFIM `JetLayout.second` row-packed order (00, 01, 02, 11,
  * 12, 22 in 3D) and holds actual second derivatives. Derivative views are
  * call-scoped; nothing but identity references is retained in the result.
  */
private[profile] final class TrialDeterminantInput private (
    val accepted: TrialAcceptedTrialBand,
    val first: Vector[DMat],
    val second: Vector[DMat]):
  def preparation: TrialBandedPreparation = accepted.preparation
  def coordinates: Vector[Double] = accepted.coordinates
  def lambda: Double = accepted.lambda
  def factor: BandedCholesky = accepted.factor
  def value: DMat = accepted.band
  def dimension: Int = coordinates.length

private[profile] object TrialDeterminantInput:
  /** Checks derivative count, shape and finite active entries before numerical work. */
  def apply(
      accepted: TrialAcceptedTrialBand,
      first: Vector[DMat],
      second: Vector[DMat]
  ): Either[TrialDeterminantError, TrialDeterminantInput] =
    val d = accepted.coordinates.length
    val n = accepted.preparation.trials
    val width = accepted.preparation.bandWidth
    if first.length != d || second.length != d * (d + 1) / 2 then
      return Left(TrialDeterminantError.DerivativeCount(d, first.length, d * (d + 1) / 2, second.length))
    val named = first.zipWithIndex.map((m, p) => s"first $p" -> m) ++
      second.zipWithIndex.map((m, r) => s"second $r" -> m)
    var index = 0
    while index < named.length do
      val (name, band) = named(index)
      if band.rows != n || band.cols != width then
        return Left(TrialDeterminantError.BandShape(n, width, band.rows, band.cols))
      var row = 0
      while row < n do
        var delta = 0
        while delta <= math.min(row, width - 1) do
          val entry = band(row, delta)
          if !entry.isFinite then return Left(TrialDeterminantError.NonFiniteBand(name, row, delta, entry))
          delta += 1
        row += 1
      index += 1
    Right(new TrialDeterminantInput(accepted, first, second))

/** Explicit work, charged when attempted. `bandedSolve*` count calls on the
  * accepted trial factor (one call solves `C` columns); `bandProductColumns`
  * counts length-N banded products for RHS assembly and residual checks.
  * `scratchDoubles` is the call-scoped N-by-C working storage.
  */
private[profile] final case class TrialDeterminantWork(
    bandedSolveAttempts: Long,
    bandedSolveFailures: Long,
    rightHandSideAttempts: Long,
    rightHandSideFailures: Long,
    residualCheckAttempts: Long,
    residualCheckFailures: Long,
    bandProductColumns: Long,
    conditionFactorAttempts: Long,
    conditionFactorFailures: Long,
    logDetJetAttempts: Long,
    logDetJetFailures: Long,
    scratchDoubles: Long)

/** Constrained determinant `D = log|A| - (N-C) log lambda + log|M'A⁻¹M| - log|M'M|`
  * with gradient and full Hessian (actual second derivatives). `factor` and
  * `preparation` are the same instances as the input.
  */
private[profile] final case class TrialDeterminantJet(
    preparation: TrialBandedPreparation,
    coordinates: Vector[Double],
    factor: BandedCholesky,
    value: Double,
    gradient: Vector[Double],
    hessian: Vector[Double],
    maxNormalizedResidual: Double):
  def dimension: Int = gradient.length
  def hessianAt(p: Int, q: Int): Double = hessian(p * dimension + q)

private[profile] final case class TrialDeterminantAttempt(
    outcome: Either[TrialDeterminantError, TrialDeterminantJet],
    work: TrialDeterminantWork)

private[profile] object TrialConstrainedLogDetJet:
  /** Uses the accepted trial factor for `U = A⁻¹M`, `U_p = -A⁻¹A_pU` and
    * `U_pq = -A⁻¹(A_pqU + A_pU_q + A_qU_p)`, and Gale's banded log-determinant
    * jet for `A` and for the small `B = M'U` (lower triangle packed as a full
    * band). No N-sized factorisation, dense N x N matrix or domain inverse.
    */
  def evaluate(input: TrialDeterminantInput, policy: TrialDeterminantNumerics): TrialDeterminantAttempt =
    val prep = input.preparation
    val n = prep.trials
    val c = prep.conditions
    val d = input.dimension
    val pairs = d * (d + 1) / 2
    val membership = prep.membership
    val factor = input.factor

    var solveAttempts = 0L
    var solveFailures = 0L
    var rhsAttempts = 0L
    var rhsFailures = 0L
    var residualAttempts = 0L
    var residualFailures = 0L
    var productColumns = 0L
    var conditionFactorAttempts = 0L
    var conditionFactorFailures = 0L
    var logDetAttempts = 0L
    var logDetFailures = 0L
    // U, d first derivatives, one reused pair buffer, RHS and residual.
    val scratchDoubles = (d + 4).toLong * n * c
    var maxResidual = 0.0

    def work: TrialDeterminantWork =
      TrialDeterminantWork(solveAttempts, solveFailures, rhsAttempts, rhsFailures, residualAttempts,
        residualFailures, productColumns, conditionFactorAttempts, conditionFactorFailures, logDetAttempts,
        logDetFailures, scratchDoubles)
    def refuse(error: TrialDeterminantError): TrialDeterminantAttempt = TrialDeterminantAttempt(Left(error), work)

    val b = factor.bandwidth
    val matrixNorm =
      val rowSums = new Array[Double](n)
      var row = 0
      while row < n do
        var delta = 0
        while delta <= math.min(row, b) do
          val a = math.abs(input.value(row, delta))
          rowSums(row) += a
          if delta > 0 then rowSums(row - delta) += a
          delta += 1
        row += 1
      rowSums.foldLeft(0.0)(math.max)

    /** out(:, j) += sign * band * vector(:, j), symmetric lower band, all C columns. */
    def addBandProduct(band: DMat, vector: DMatBuilder, out: DMatBuilder, sign: Double): Unit =
      var row = 0
      while row < n do
        var delta = 0
        while delta <= math.min(row, b) do
          val a = sign * band(row, delta)
          if a != 0.0 then
            val other = row - delta
            var col = 0
            while col < c do
              out(row, col) = out(row, col) + a * vector(other, col)
              if delta > 0 then out(other, col) = out(other, col) + a * vector(row, col)
              col += 1
          delta += 1
        row += 1
      productColumns += c

    val rhs = DMatBuilder.zeros(n, c)
    val residual = DMatBuilder.zeros(n, c)

    /** Solves `A x = rhs` into `target`, then admits it by normalized residual. */
    def solveAdmitted(stage: TrialDeterminantStage, target: DMatBuilder): Option[TrialDeterminantError] =
      var i = 0
      while i < n do
        var j = 0
        while j < c do
          target(i, j) = rhs(i, j)
          j += 1
        i += 1
      solveAttempts += 1L
      rhsAttempts += c
      factor.solveInPlace(target) match
        case Left(error) =>
          solveFailures += 1L
          rhsFailures += c
          Some(TrialDeterminantError.Solve(stage, error.getMessage))
        case Right(_) =>
          residualAttempts += 1L
          i = 0
          while i < n do
            var j = 0
            while j < c do
              residual(i, j) = -rhs(i, j)
              j += 1
            i += 1
          addBandProduct(input.value, target, residual, 1.0)
          var worst = 0.0
          var j = 0
          while j < c do
            var rNorm = 0.0
            var xNorm = 0.0
            var bNorm = 0.0
            i = 0
            while i < n do
              rNorm = math.max(rNorm, math.abs(residual(i, j)))
              xNorm = math.max(xNorm, math.abs(target(i, j)))
              bNorm = math.max(bNorm, math.abs(rhs(i, j)))
              i += 1
            val scale = matrixNorm * xNorm + bNorm
            val normalized = if rNorm == 0.0 then 0.0 else rNorm / scale
            worst = if normalized.isNaN then Double.NaN else math.max(worst, normalized)
            j += 1
          if worst.isNaN || !(worst <= policy.residualTolerance) then
            residualFailures += 1L
            Some(TrialDeterminantError.Residual(stage, worst, policy.residualTolerance))
          else
            maxResidual = math.max(maxResidual, worst)
            None

    /** B-sized contraction `M' x` into row-major C x C. */
    def contract(x: DMatBuilder): Array[Double] =
      val out = new Array[Double](c * c)
      var i = 0
      while i < n do
        val row = membership.conditionOfTrial(i)
        var j = 0
        while j < c do
          out(row * c + j) += x(i, j)
          j += 1
        i += 1
      out

    def fill(target: DMatBuilder, value: Double): Unit =
      var i = 0
      while i < n do
        var j = 0
        while j < c do
          target(i, j) = value
          j += 1
        i += 1

    // U = A⁻¹ M.
    val u = DMatBuilder.zeros(n, c)
    fill(rhs, 0.0)
    var trial = 0
    while trial < n do
      rhs(trial, membership.conditionOfTrial(trial)) = 1.0
      trial += 1
    solveAdmitted(TrialDeterminantStage.MembershipSolve, u) match
      case Some(error) => return refuse(error)
      case None => ()
    val bValue = contract(u)

    // U_p = -A⁻¹ A_p U.
    val up = Vector.fill(d)(DMatBuilder.zeros(n, c))
    val bFirst = new Array[Array[Double]](d)
    var p = 0
    while p < d do
      fill(rhs, 0.0)
      addBandProduct(input.first(p), u, rhs, -1.0)
      solveAdmitted(TrialDeterminantStage.FirstSolve(p), up(p)) match
        case Some(error) => return refuse(error)
        case None => ()
      bFirst(p) = contract(up(p))
      p += 1

    // U_pq = -A⁻¹ (A_pq U + A_p U_q + A_q U_p); the same-axis case has two A_p U_p terms.
    // Gale's secondUpper is column-packed (00, 01, 11, 02, 12, 22): index by (p, q) explicitly.
    val upq = DMatBuilder.zeros(n, c)
    val bSecond = new Array[Array[Double]](pairs)
    var q = 0
    while q < d do
      p = 0
      while p <= q do
        fill(rhs, 0.0)
        addBandProduct(input.second(JetLayout.second(d, p, q) - 1 - d), u, rhs, -1.0)
        addBandProduct(input.first(p), up(q), rhs, -1.0)
        addBandProduct(input.first(q), up(p), rhs, -1.0)
        solveAdmitted(TrialDeterminantStage.SecondSolve(p, q), upq) match
          case Some(error) => return refuse(error)
          case None => ()
        bSecond(q * (q + 1) / 2 + p) = contract(upq)
        p += 1
      q += 1

    // log|A| jet on the accepted factor, remapping ScalaFIM row-packed seconds to Gale column packing.
    val galeSecond = Vector.tabulate(pairs): index =>
      val qq = (0 until d).find(k => (k + 1) * (k + 2) / 2 > index).get
      val pp = index - qq * (qq + 1) / 2
      input.second(JetLayout.second(d, pp, qq) - 1 - d)
    logDetAttempts += 1L
    val trialJet = BandedDerivatives(input.first, galeSecond).flatMap(factor.logDetJet) match
      case Left(error) =>
        logDetFailures += 1L
        return refuse(TrialDeterminantError.LogDetJet(TrialDeterminantStage.TrialLogDetJet, error.getMessage))
      case Right(jet) => jet

    // Small B: lower triangle packed as a C x C full band.
    def smallBand(values: Array[Double]): DMat =
      val out = DMatBuilder.zeros(c, c)
      var i = 0
      while i < c do
        var delta = 0
        while delta <= i do
          out(i, delta) = values(i * c + i - delta)
          delta += 1
        i += 1
      out.result()
    conditionFactorAttempts += 1L
    val conditionFactor = BandedCholesky.factorLower(smallBand(bValue)) match
      case Left(error) =>
        conditionFactorFailures += 1L
        return refuse(TrialDeterminantError.Factor(TrialDeterminantStage.ConditionFactor, error.getMessage))
      case Right(f) => f
    logDetAttempts += 1L
    val conditionJet =
      BandedDerivatives(bFirst.toVector.map(smallBand), bSecond.toVector.map(smallBand))
        .flatMap(conditionFactor.logDetJet) match
        case Left(error) =>
          logDetFailures += 1L
          return refuse(TrialDeterminantError.LogDetJet(TrialDeterminantStage.ConditionLogDetJet, error.getMessage))
        case Right(jet) => jet

    val membershipLogDet = (0 until c).map(cond => math.log(membership.trialsOf(cond).length.toDouble)).sum
    val value = factor.logDet - (n - c) * math.log(input.lambda) + conditionFactor.logDet - membershipLogDet
    val gradient = Vector.tabulate(d)(i => trialJet.gradient(i) + conditionJet.gradient(i))
    val hessian = Vector.tabulate(d * d)(k => trialJet.hessian(k / d, k % d) + conditionJet.hessian(k / d, k % d))
    if !value.isFinite || gradient.exists(!_.isFinite) || hessian.exists(!_.isFinite) then
      return refuse(TrialDeterminantError.NonFinite(TrialDeterminantStage.Result))
    TrialDeterminantAttempt(
      Right(TrialDeterminantJet(prep, input.coordinates, factor, value, gradient, hessian, maxResidual)),
      work)
