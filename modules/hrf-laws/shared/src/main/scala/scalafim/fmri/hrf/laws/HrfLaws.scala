package scalafim.fmri.hrf.laws

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.regressor.{HrfAssignment, Regressor, StimulusEvent}

/** A law that failed, with enough detail to act on. */
final case class LawFailure(law: String, detail: String, worstError: Double):
  def message: String = s"$law violated: $detail (max error ${worstError})"

/** Executable versions of the algebraic laws the HRF core is supposed to obey.
  *
  * These are ordinary functions returning `Vector[LawFailure]` rather than
  * assertions, so any module can run them against its own kernels — the way
  * `design` can check the HRFs its generators produce — and so a caller can
  * decide what a violation means. `HrfLawsSuite` in this module wires them to
  * munit for the stock library.
  *
  * The laws are worth stating because they are exactly what licenses the fast
  * paths: additivity is why events can be accumulated in parallel, permutation
  * invariance is why an event train needs no canonical order, and
  * reconstruction-commutes-with-rendering is why a basis-valued design block
  * can be convolved once and contracted per voxel rather than convolved per
  * voxel.
  */
object HrfLaws:

  private val defaultTol = 1e-9

  private def maxAbsDiff(a: Mat, b: Mat): Double =
    require(a.rows == b.rows && a.cols == b.cols, s"shape mismatch: ${a.rows}x${a.cols} vs ${b.rows}x${b.cols}")
    var m = 0.0
    var i = 0
    while i < a.data.length do
      val d = math.abs(a.data(i) - b.data(i))
      if d > m then m = d
      i += 1
    m

  private def check(law: String, detail: String, error: Double, tol: Double): Option[LawFailure] =
    if error <= tol then None else Some(LawFailure(law, detail, error))

  private def regressorOf(
      onsets: Seq[Double],
      hrf: Hrf,
      durations: Seq[Double],
      amplitudes: Seq[Double],
      summate: Boolean = true
  ): Regressor =
    Regressor(onsets, hrf, duration = durations, amplitude = amplitudes, summate = summate)

  // --- kernel laws --------------------------------------------------------

  /** `h(lag) = 0` for every `lag < 0`. */
  def causality(hrf: Hrf, negativeLags: Seq[Double] = Seq(-100.0, -10.0, -1.0, -1e-9)): Vector[LawFailure] =
    negativeLags.flatMap { lag =>
      val v = hrf(Lag(lag)).data
      val worst = v.map(math.abs).maxOption.getOrElse(0.0)
      check("causality", s"${hrf.name} responded at lag $lag", worst, 0.0)
    }.toVector

  /** `h(lag) = 0` beyond a declared compact horizon. */
  def support(hrf: Hrf): Vector[LawFailure] =
    hrf.support match
      case Support.Unbounded => Vector.empty
      case Support.Compact(horizon) =>
        Seq(1e-9, 0.5, 50.0).flatMap { delta =>
          val lag = horizon.value + delta
          val worst = hrf(Lag(lag)).data.map(math.abs).maxOption.getOrElse(0.0)
          check("support", s"${hrf.name} non-zero at lag $lag past horizon ${horizon.value}", worst, 0.0)
        }.toVector

  // --- rendering laws -----------------------------------------------------

  /** `respond(0, h) = 0`. */
  def emptyDrive(hrf: Hrf, grid: Seq[Double]): Vector[LawFailure] =
    val out = regressorOf(Seq.empty, hrf, Seq.empty, Seq.empty).evaluate(grid)
    val worst = out.data.map(math.abs).maxOption.getOrElse(0.0)
    check("emptyDrive", s"${hrf.name} produced a response with no events", worst, 0.0).toVector

  /** `respond(E1 + E2, h) = respond(E1, h) + respond(E2, h)`. */
  def eventAdditivity(
      hrf: Hrf,
      left: Seq[Double],
      right: Seq[Double],
      grid: Seq[Double],
      durations: Double = 0.0,
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    def run(onsets: Seq[Double]) =
      regressorOf(onsets, hrf, Seq.fill(onsets.length)(durations), Seq.fill(onsets.length)(1.0)).evaluate(grid)
    val a = run(left)
    val b = run(right)
    val both = run(left ++ right)
    val sum = Mat.unsafe(a.rows, a.cols, a.data.zip(b.data).map(_ + _))
    check("eventAdditivity", s"${hrf.name}", maxAbsDiff(both, sum), tol).toVector

  /** `respond(aE, h) = a * respond(E, h)`. */
  def homogeneity(
      hrf: Hrf,
      onsets: Seq[Double],
      grid: Seq[Double],
      scale: Double = 2.5,
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    val unit = regressorOf(onsets, hrf, Seq.fill(onsets.length)(0.0), Seq.fill(onsets.length)(1.0)).evaluate(grid)
    val scaled =
      regressorOf(onsets, hrf, Seq.fill(onsets.length)(0.0), Seq.fill(onsets.length)(scale)).evaluate(grid)
    val expected = Mat.unsafe(unit.rows, unit.cols, unit.data.map(_ * scale))
    check("homogeneity", s"${hrf.name} at scale $scale", maxAbsDiff(scaled, expected), tol).toVector

  /** `respond(pi E, h) = respond(E, h)` — event order carries no meaning. */
  def permutationInvariance(
      hrf: Hrf,
      onsets: Seq[Double],
      amplitudes: Seq[Double],
      grid: Seq[Double],
      tol: Double = 0.0
  ): Vector[LawFailure] =
    val durations = Seq.fill(onsets.length)(0.0)
    val forward = regressorOf(onsets, hrf, durations, amplitudes).evaluate(grid)
    val idx = onsets.indices.reverse
    val reversed =
      regressorOf(idx.map(onsets), hrf, durations, idx.map(amplitudes)).evaluate(grid)
    check("permutationInvariance", s"${hrf.name}", maxAbsDiff(forward, reversed), tol).toVector

  /** `respond(tau_a E, h)(t + a) = respond(E, h)(t)`. */
  def translationEquivariance(
      hrf: Hrf,
      onsets: Seq[Double],
      grid: Seq[Double],
      shift: Double = 6.0,
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    val durations = Seq.fill(onsets.length)(0.0)
    val amps = Seq.fill(onsets.length)(1.0)
    val base = regressorOf(onsets, hrf, durations, amps).evaluate(grid)
    val shifted =
      regressorOf(onsets.map(_ + shift), hrf, durations, amps).evaluate(grid.map(_ + shift))
    check("translationEquivariance", s"${hrf.name} by $shift", maxAbsDiff(base, shifted), tol).toVector

  // --- pulse laws ---------------------------------------------------------

  /** A zero-duration box is the impulse: `q_0 * h = h`. */
  def impulseIdentity(hrf: Hrf, grid: Seq[Double], tol: Double = 0.0): Vector[LawFailure] =
    val impulse = Evaluate.doubles(hrf, grid, duration = 0.0)
    val direct = hrf.evalDoubles(grid)
    check("impulseIdentity", s"${hrf.name}", maxAbsDiff(impulse, direct), tol).toVector

  /** `q_d^mass = q_d^height / d`. */
  def unitMassRelation(
      hrf: Hrf,
      grid: Seq[Double],
      duration: Double = 4.0,
      precision: Double = 0.05,
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    val height = Evaluate.doubles(hrf, grid, duration = duration, precision = precision, summate = true)
    val mass = Evaluate.doubles(hrf, grid, duration = duration, precision = precision, summate = false)
    val expected = Mat.unsafe(height.rows, height.cols, height.data.map(_ / duration))
    check("unitMassRelation", s"${hrf.name} at duration $duration", maxAbsDiff(mass, expected), tol).toVector

  /** The box response converges under refinement.
    *
    * Stated as a *rate* rather than an absolute gap, because the rate depends
    * on the kernel's regularity: the trapezoid rule is O(h^2) for a smooth
    * kernel like SPMG1, but only O(h) for one with jump discontinuities — FIR
    * bins, a boxcar edge, a Fourier basis truncated at its horizon. A single
    * tolerance would either be vacuous for the smooth kernels or spuriously
    * fail the discontinuous ones. What both must do is keep improving.
    *
    * The old unweighted summation fails this outright: refining `precision`
    * made the answer diverge rather than settle.
    */
  def quadratureConvergence(
      hrf: Hrf,
      grid: Seq[Double],
      duration: Double = 4.0,
      steps: Seq[Double] = Seq(0.2, 0.1, 0.05, 0.025, 0.0125)
  ): Vector[LawFailure] =
    val evaluated = steps.map(p => Evaluate.doubles(hrf, grid, duration = duration, precision = p))
    val gaps = evaluated.zip(evaluated.tail).map((a, b) => maxAbsDiff(a, b))
    // Each successive refinement must move the answer less than the previous
    // one did. A small absolute slack keeps round-off from tripping kernels
    // that have already converged to machine precision.
    val slack = 1e-12
    gaps
      .zip(gaps.tail)
      .zipWithIndex
      .flatMap { case ((previous, next), i) =>
        check(
          "quadratureConvergence",
          s"${hrf.name} at duration $duration: refining from ${steps(i + 1)} to ${steps(i + 2)} " +
            s"moved the answer by $next, more than the previous refinement's $previous",
          next - previous - slack,
          0.0
        )
      }
      .toVector

  // --- basis laws ---------------------------------------------------------

  /** `respond(E, beta . Phi) = beta . respond(E, Phi)`.
    *
    * Reconstruction commutes with rendering, so a basis-valued design block may
    * be convolved once and contracted afterwards. This is what makes
    * voxel-wise fitted HRFs cheap.
    */
  def reconstructionCommutes(
      basis: Hrf,
      coefficients: Seq[Double],
      onsets: Seq[Double],
      grid: Seq[Double],
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    if coefficients.length != basis.nbasis then
      Vector(LawFailure("reconstructionCommutes", s"${basis.nbasis} columns but ${coefficients.length} coefficients", 0.0))
    else
      val durations = Seq.fill(onsets.length)(0.0)
      val amps = Seq.fill(onsets.length)(1.0)
      // Contract first, then render.
      val fitted = HrfCombinators.withCoefficients(basis)(coefficients.toArray, None)
      val renderContract = regressorOf(onsets, fitted, durations, amps).evaluate(grid)
      // Render first, then contract.
      val block = regressorOf(onsets, basis, durations, amps).evaluate(grid)
      val contracted = new Array[Double](block.rows)
      var i = 0
      while i < block.rows do
        var acc = 0.0
        var j = 0
        while j < block.cols do
          acc += block(i, j) * coefficients(j)
          j += 1
        contracted(i) = acc
        i += 1
      val contractRender = Mat.unsafe(block.rows, 1, contracted)
      check("reconstructionCommutes", s"${basis.name}", maxAbsDiff(renderContract, contractRender), tol).toVector

  /** `beta'(Phi') = beta(Phi)` when `Phi' = A Phi` and `beta' = beta A^-1`. */
  def gaugeInvariance(
      basis: Hrf,
      coefficients: Seq[Double],
      lags: Seq[Double],
      tol: Double = defaultTol
  ): Vector[LawFailure] =
    val TransformedBasis(normalized, transform) = HrfCombinators.normalizeWithTransform(basis)(Seconds(0.1))
    val original = HrfCombinators.withCoefficients(basis)(coefficients.toArray, None)
    transform.transportCoefficients(BasisCoefficients.unsafe[Any](coefficients.toVector)) match
      case Left(err) => Vector(LawFailure("gaugeInvariance", err.message, 0.0))
      case Right(betaPrime) =>
        val moved = HrfCombinators.withCoefficients(normalized)(betaPrime.toArray, None)
        val worst = lags.map(t => math.abs(original(Lag(t)).data(0) - moved(Lag(t)).data(0))).maxOption.getOrElse(0.0)
        check("gaugeInvariance", s"${basis.name}", worst, tol).toVector

  // --- plan laws ----------------------------------------------------------

  /** Different evaluation plans must agree to a stated tolerance.
    *
    * They are not bit-identical: `Conv`/`FFT` quantize onsets onto the
    * microtime grid and interpolate the result, while `Loop` evaluates the
    * kernel exactly at `t - onset`. For onsets that do not land on the grid the
    * gap is real and is inherited from R, which shows the same discrepancy to
    * six digits. Stating the tolerance is the honest option; asserting equality
    * would only hide it.
    */
  def planEquivalence(
      hrf: Hrf,
      onsets: Seq[Double],
      grid: Seq[Double],
      precision: Double = 0.1,
      tol: Double = 1e-2
  ): Vector[LawFailure] =
    val durations = Seq.fill(onsets.length)(0.0)
    val amps = Seq.fill(onsets.length)(1.0)
    val reg = regressorOf(onsets, hrf, durations, amps)
    val conv = reg.evaluate(grid, precision, Regressor.EvalMethod.Conv)
    val fft = reg.evaluate(grid, precision, Regressor.EvalMethod.FFT)
    val loop = reg.evaluate(grid, precision, Regressor.EvalMethod.Loop)
    Vector(
      // Conv and FFT are the same algorithm and must agree to round-off.
      check("planEquivalence(conv/fft)", s"${hrf.name}", maxAbsDiff(conv, fft), 1e-9),
      check("planEquivalence(conv/loop)", s"${hrf.name}", maxAbsDiff(conv, loop), tol)
    ).flatten

  /** Every law that applies to a bare scalar kernel. */
  def allScalar(hrf: Hrf, grid: Seq[Double]): Vector[LawFailure] =
    causality(hrf) ++
      support(hrf) ++
      emptyDrive(hrf, grid) ++
      eventAdditivity(hrf, Seq(5.0, 25.0), Seq(40.0), grid) ++
      homogeneity(hrf, Seq(10.0, 30.0), grid) ++
      permutationInvariance(hrf, Seq(10.0, 30.0, 50.0), Seq(1.0, -0.5, 2.0), grid) ++
      translationEquivariance(hrf, Seq(10.0, 30.0), grid) ++
      impulseIdentity(hrf, grid) ++
      unitMassRelation(hrf, grid) ++
      quadratureConvergence(hrf, grid) ++
      planEquivalence(hrf, Seq(10.0, 30.0), grid)
