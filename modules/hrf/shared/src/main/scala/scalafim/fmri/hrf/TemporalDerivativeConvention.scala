package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Vec

/** The convention used to form the temporal column paired with a scalar SPMG
  * canonical response. The finite difference is deliberately a response
  * convention, rather than a numerical approximation used elsewhere. */
enum TemporalDerivativeConvention:
  /** The continuous derivative `dh/dt` of the canonical response. */
  case AnalyticSpmg

  /** SPM12's informed-basis convention (`spm_get_bf.m`): the one-second
    * backward difference of two sum-normalized canonical kernels, then
    * `spm_orth` serial orthogonalization against the canonical column on SPM's
    * kernel sampling grid (see [[SpmKernelGrid]]). Realizing it therefore
    * requires the grid, which depends on the repetition time. The
    * un-orthogonalized difference is available, under an explicitly raw name,
    * as [[TemporalDerivativeConvention.rawOneSecondDifference]]. */
  case SpmOneSecondBackwardDifference

enum TemporalDerivativeConventionError:
  case RequiresScalarSpmgCanonical(name: String, basis: Int)
  case RequiresKernelGrid
  case InvalidKernelGrid(detail: String)
  case UnsupportedInformedColumns(columns: Int)
  case DegenerateColumn(column: Int, relativeNorm: Double)
  case BasisIdentity(detail: String)

  def message: String = this match
    case RequiresScalarSpmgCanonical(name, basis) =>
      s"temporal derivative convention requires a scalar SPMG canonical response, got '$name' with $basis basis columns"
    case RequiresKernelGrid =>
      "the SPM temporal derivative convention requires an SPM kernel sampling grid (repetition time and microtime resolution)"
    case InvalidKernelGrid(detail) => s"invalid SPM kernel sampling grid: $detail"
    case UnsupportedInformedColumns(columns) => s"SPM informed basis supports 2 or 3 columns, got $columns"
    case DegenerateColumn(column, relativeNorm) =>
      s"SPM informed basis column ${column + 1} vanishes after serial orthogonalization (relative norm $relativeNorm)"
    case BasisIdentity(detail) => s"SPM informed basis identity is invalid: $detail"

/** SPM's kernel sampling grid. `spm_get_bf` builds basis functions at
  * `dt = TR / T` (`T` = `defaults.stats.fmri.t`, 16 by default) over
  * `[0, L]` with `L = 32 s` (`spm_hrf` parameter `p(7)`), i.e. at
  * `k * dt` for `k = 0 .. floor(L / dt)`. Inner products used by the
  * SPM-named convention are sums over exactly these samples. */
final case class SpmKernelGrid private (
    repetitionTime: PositiveSeconds,
    microtimeResolution: Int,
    length: PositiveSeconds
):
  def step: Double = repetitionTime.value / microtimeResolution.toDouble

  def sampleCount: Int = math.floor(length.value / step).toInt + 1

  def times: Array[Double] = Array.tabulate(sampleCount)(k => k.toDouble * step)

  def canonical: String =
    s"tr=${java.lang.Double.doubleToLongBits(repetitionTime.value)};T=$microtimeResolution;length=${java.lang.Double.doubleToLongBits(length.value)}"

object SpmKernelGrid:
  val DefaultMicrotimeResolution: Int = 16
  val DefaultLength: Double = 32.0
  private val MaximumSamples = 10000000

  def apply(
      repetitionTime: Seconds,
      microtimeResolution: Int = DefaultMicrotimeResolution,
      length: Seconds = Seconds(DefaultLength)
  ): Either[TemporalDerivativeConventionError, SpmKernelGrid] =
    for
      tr <- PositiveSeconds.fromSeconds(repetitionTime, "repetition time").left.map(error => TemporalDerivativeConventionError.InvalidKernelGrid(error.toString))
      window <- PositiveSeconds.fromSeconds(length, "kernel length").left.map(error => TemporalDerivativeConventionError.InvalidKernelGrid(error.toString))
      _ <-
        if microtimeResolution >= 1 then Right(())
        else Left(TemporalDerivativeConventionError.InvalidKernelGrid(s"microtime resolution must be >= 1, got $microtimeResolution"))
      samples = window.value / (tr.value / microtimeResolution.toDouble)
      _ <-
        if samples.isFinite && samples < MaximumSamples.toDouble then Right(())
        else Left(TemporalDerivativeConventionError.InvalidKernelGrid(s"grid needs $samples samples, exceeding $MaximumSamples"))
    yield new SpmKernelGrid(tr, microtimeResolution, window)

object TemporalDerivativeConvention:
  private val Shift = 1.0
  private val DispersionStep = 0.01

  /** A column whose serial residual has relative Euclidean norm (against the
    * column before projection) below this bound is reported as degenerate.
    * `spm_orth` silently drops columns with `norm(D, 1) <= exp(-32)`; a typed
    * error is returned here instead. */
  val DegenerateRelativeNorm: Double = 1e-10

  /** Derive a temporal response from a scalar SPMG canonical kernel.
    *
    * `AnalyticSpmg` is the continuous derivative. `SpmOneSecondBackwardDifference`
    * is SPM12's derivative column (see [[spmInformedBasis]]) and requires `grid`. */
  def derive(
      canonical: Hrf,
      convention: TemporalDerivativeConvention,
      grid: Option[SpmKernelGrid] = None
  ): Either[TemporalDerivativeConventionError, Hrf] =
    spmgParams(canonical).flatMap { params =>
      convention match
        case TemporalDerivativeConvention.AnalyticSpmg =>
          Right(Hrfs.spmg1TemporalDeriv(params.p1, params.p2, params.a1, canonical.span))
        case TemporalDerivativeConvention.SpmOneSecondBackwardDifference =>
          grid match
            case None => Left(TemporalDerivativeConventionError.RequiresKernelGrid)
            case Some(value) =>
              spmInformedBasis(canonical, 2, value).flatMap { basis =>
                val temporal = Hrf.of(
                  name = s"${basis.name}_temporal",
                  nbasis = 1,
                  span = basis.span,
                  descriptor = Some(basis.descriptor.derived(s"spm12-orthogonalized-temporal-difference;${value.canonical}", span = basis.span).copy(basis = BasisCount.One)),
                  support = basis.support
                )(lag => Vec.unsafe(Array(basis(lag).data(1))))
                val element = BasisElement(
                  BasisElementId.unsafe(s"${temporal.descriptor.canonicalId}|${BasisRole.TemporalDerivative.stableLabel}|1"),
                  index = 1,
                  role = BasisRole.TemporalDerivative,
                  label = temporal.name
                )
                Hrf.withBasisElements(temporal, Vector(element)).left.map(error => TemporalDerivativeConventionError.BasisIdentity(error.message))
              }
    }

  /** The raw backward difference `h(t) - h(t - 1 s)`, without SPM's sum
    * normalization or orthogonalization. This is not SPM's design column. */
  def rawOneSecondDifference(canonical: Hrf): Either[TemporalDerivativeConventionError, Hrf] =
    spmgParams(canonical).flatMap { _ =>
      val step = Seconds(Shift)
      val descriptor = canonical.descriptor.derived("raw-1-second-temporal-difference", span = canonical.span + step)
      val difference = Hrf.of(
        name = s"${canonical.name}_raw_temporal_difference",
        nbasis = 1,
        span = canonical.span + step,
        descriptor = Some(descriptor),
        support = canonical.support.widened(step)
      ) { lag =>
        Vec.unsafe(Array(canonical(lag).data(0) - canonical(Lag.unsafe(lag.value - step.value)).data(0)))
      }
      val element = BasisElement(
        BasisElementId.unsafe(s"${difference.descriptor.canonicalId}|${BasisRole.TemporalDerivative.stableLabel}|1"),
        index = 1,
        role = BasisRole.TemporalDerivative,
        label = difference.name
      )
      Hrf.withBasisElements(difference, Vector(element)).left.map(error => TemporalDerivativeConventionError.BasisIdentity(error.message))
    }

  /** SPM12's informed basis (`spm_get_bf.m`, 'hrf (with time derivative)' and
    * 'hrf (with time and dispersion derivatives)') as continuous kernels in
    * this module's canonical units.
    *
    * SPM forms `bf = [h/S0, h/S0 - h(. - 1)/S1, (h/S0 - h_d/Sd) / 0.01]`, where
    * every `spm_hrf` call is divided by its own sample sum over `grid` and
    * `h_d` raises the response dispersion `p(3)` by 0.01, then applies
    * `spm_orth` (serial projection against all earlier columns; no mean
    * removal). Each column here is the same linear combination of `h(t)`,
    * `h(t - 1)` and the continuous dispersion difference, multiplied by `S0`:
    * the canonical column is exactly `h`, and every column equals SPM's column
    * times the one common factor `S0`. Orthogonality therefore holds for the
    * sampled kernels on the SPM grid, not over continuous time.
    *
    * The basis has compact support and span `grid.length` (32 s by default):
    * it is zero past SPM's kernel window, and convolution keeps the whole
    * window rather than truncating at the canonical response's span.
    */
  def spmInformedBasis(canonical: Hrf, columns: Int, grid: SpmKernelGrid): Either[TemporalDerivativeConventionError, Hrf] =
    if columns != 2 && columns != 3 then Left(TemporalDerivativeConventionError.UnsupportedInformedColumns(columns))
    else
      spmgParams(canonical).flatMap { params =>
        val times = grid.times
        val n = times.length
        // Primitive kernels: h(t), h(t - 1), and the continuous dispersion difference.
        val primitives = Array.ofDim[Double](3, n)
        var k = 0
        while k < n do
          primitives(0)(k) = canonical(Lag(times(k))).data(0)
          primitives(1)(k) = canonical(Lag.unsafe(times(k) - Shift)).data(0)
          primitives(2)(k) = dispersion(params, times(k))
          k += 1
        val s0 = sum(primitives(0))
        val s1 = sum(primitives(1))
        // Sample sum of h_d = h - 0.01 * (dispersion difference).
        var sd = 0.0
        k = 0
        while k < n do
          sd += primitives(0)(k) - DispersionStep * primitives(2)(k)
          k += 1
        val raw = Vector(
          Array(1.0, 0.0, 0.0),
          Array(1.0, -s0 / s1, 0.0),
          Array((1.0 - s0 / sd) / DispersionStep, 0.0, s0 / sd)
        ).take(columns)
        orthogonalize(raw, primitives).flatMap { coefficients =>
          // SPM's kernels exist only on the grid's `[0, L]` window, and the
          // serial orthogonalization holds over exactly those samples. The
          // realized basis is therefore compactly supported on `[0, L]` and its
          // span (the convolution horizon) is `L`, so design columns keep the
          // full SPM kernel, including the tail past the canonical's span.
          val span = grid.length.seconds
          val descriptor = canonical.descriptor
            .derived(s"spm12-informed-basis-$columns;${grid.canonical}", span = span)
            .copy(basis = BasisCount(columns), components = Vector(canonical.descriptor))
          val support = Support.Compact(span)
          val basis = Hrf.of(name = s"SPMG${columns}_spm12", nbasis = columns, span = span, descriptor = Some(descriptor), support = support) { lag =>
            val h = canonical(lag).data(0)
            val shifted = canonical(Lag.unsafe(lag.value - Shift)).data(0)
            val disp = if columns == 3 then dispersion(params, lag.value) else 0.0
            val out = new Array[Double](columns)
            var c = 0
            while c < columns do
              val w = coefficients(c)
              out(c) = w(0) * h + w(1) * shifted + w(2) * disp
              c += 1
            Vec.unsafe(out)
          }
          val roles = Vector(BasisRole.Canonical, BasisRole.TemporalDerivative, BasisRole.DispersionDerivative).take(columns)
          val elements = roles.zipWithIndex.map { (role, index) =>
            BasisElement(BasisElementId.unsafe(s"${basis.descriptor.canonicalId}|${role.stableLabel}|${index + 1}"), index + 1, role, role.stableLabel)
          }
          Hrf.withBasisElements(basis, elements).left.map(error => TemporalDerivativeConventionError.BasisIdentity(error.message))
        }
      }

  private def spmgParams(canonical: Hrf): Either[TemporalDerivativeConventionError, SpmgParams] =
    (canonical.descriptor.family, canonical.descriptor.params, canonical.nbasis) match
      case (HrfFamily.Known(HrfKind.Spmg1), HrfParams.Spmg(params), 1) => Right(params)
      case _ => Left(TemporalDerivativeConventionError.RequiresScalarSpmgCanonical(canonical.name, canonical.nbasis))

  private def dispersion(params: SpmgParams, time: Double): Double =
    if time < 0.0 then 0.0 else HrfFunctions.spmg1DispersionDeriv(Lag(time), params.p1, params.a1)

  private def sum(values: Array[Double]): Double =
    var total = 0.0
    var i = 0
    while i < values.length do
      total += values(i)
      i += 1
    total

  /** `spm_orth` on coefficient vectors over the sampled primitives: each
    * column loses its least-squares projection onto every earlier
    * (already orthogonalized) column, using grid inner products. */
  private def orthogonalize(
      raw: Vector[Array[Double]],
      primitives: Array[Array[Double]]
  ): Either[TemporalDerivativeConventionError, Vector[Array[Double]]] =
    val n = primitives(0).length
    def sample(coefficients: Array[Double]): Array[Double] =
      val out = new Array[Double](n)
      var k = 0
      while k < n do
        out(k) = coefficients(0) * primitives(0)(k) + coefficients(1) * primitives(1)(k) + coefficients(2) * primitives(2)(k)
        k += 1
      out
    def dot(left: Array[Double], right: Array[Double]): Double =
      var total = 0.0
      var k = 0
      while k < n do
        total += left(k) * right(k)
        k += 1
      total
    var accepted = Vector.empty[(Array[Double], Array[Double])]
    var failure = Option.empty[TemporalDerivativeConventionError]
    var column = 0
    while column < raw.length && failure.isEmpty do
      val coefficients = raw(column).clone()
      val original = sample(coefficients)
      var values = original
      var previous = 0
      while previous < accepted.length do
        val (previousCoefficients, previousValues) = accepted(previous)
        val beta = dot(previousValues, values) / dot(previousValues, previousValues)
        var i = 0
        while i < coefficients.length do
          coefficients(i) -= beta * previousCoefficients(i)
          i += 1
        values = sample(coefficients)
        previous += 1
      val relative = math.sqrt(dot(values, values) / dot(original, original))
      if !relative.isFinite || relative < DegenerateRelativeNorm then
        failure = Some(TemporalDerivativeConventionError.DegenerateColumn(column, relative))
      else
        accepted = accepted :+ (coefficients -> values)
      column += 1
    failure.toLeft(accepted.map(_._1))
