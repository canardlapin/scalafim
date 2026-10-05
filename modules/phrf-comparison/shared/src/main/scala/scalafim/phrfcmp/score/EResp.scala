package scalafim.phrfcmp.score

import scalafim.fmri.hrf.{HrfFunctions, Hrfs, Lag}
import scalafim.fmri.hrf.family.{JetLayout, NormalizationRule, ParametricHrfFamily, ShapePoint}

/** Why an E-resp reconstruction was refused. No exception crosses this boundary. */
enum ERespError:
  case EmptyGrid
  case InvalidGrid(detail: String)
  case CoefficientCount(expected: Int, actual: Int)
  case ConditionCount(expected: Int, actual: Int)
  case UnsupportedNormalization(family: String, rule: String)
  case WrongDimension(expected: Int, actual: Int)
  case NonFinite(what: String)

  def message: String = this match
    case EmptyGrid                    => "E-resp grid is empty"
    case InvalidGrid(d)               => s"E-resp grid: $d"
    case CoefficientCount(e, a)       => s"expected $e coefficients, got $a"
    case ConditionCount(e, a)         => s"expected $e conditions, got $a"
    case UnsupportedNormalization(f, r) => s"family $f does not support $r normalisation"
    case WrongDimension(e, a)         => s"expected a shape point of dimension $e, got $a"
    case NonFinite(w)                 => s"non-finite $w"

/**
  * The lags at which a fitted condition response is evaluated. The protocol grid (section 3) is 0.1 s steps over
  * `[0, H]`; [[ResponseGrid.standard]] builds it with lag `k` equal to the double nearest `k / 10`, so the lags are
  * identical on the JVM and in JS and agree with the decimal text of the generator.
  */
final class ResponseGrid private (private val values: Array[Double]):
  def size: Int = values.length
  def lag(i: Int): Double = values(i)
  def lags: Array[Double] = values.clone()
  def horizon: Double = values(values.length - 1)

object ResponseGrid:

  /** The protocol grid: lags `k / 10` for `k = 0 .. 10 * horizonSeconds` (H = 32 s gives 321 points). */
  def standard(horizonSeconds: Int = 32): Either[ERespError, ResponseGrid] =
    if horizonSeconds <= 0 then Left(ERespError.InvalidGrid(s"horizon must be positive, got $horizonSeconds"))
    else Right(new ResponseGrid(Array.tabulate(10 * horizonSeconds + 1)(_ / 10.0)))

  /** An arbitrary strictly increasing, finite, non-negative lag set (used for the R parity fixtures, which are on 0.25 s). */
  def of(lags: Seq[Double]): Either[ERespError, ResponseGrid] =
    if lags.isEmpty then Left(ERespError.EmptyGrid)
    else if lags.exists(l => !l.isFinite || l < 0.0) then Left(ERespError.InvalidGrid("lags must be finite and non-negative"))
    else if lags.zip(lags.tail).exists((a, b) => !(b > a)) then Left(ERespError.InvalidGrid("lags must be strictly increasing"))
    else Right(new ResponseGrid(lags.toArray))

/**
  * A fixed linear kernel basis whose condition response is a coefficient-weighted sum of its columns.
  *
  * Each arm is scored on exactly what its fitted design represents (owner decision 2026-10-02): the SPM kernels (CAN,
  * INF3) are truncated at the HRF span of their design, 24 s (lag in [0, 24] inclusive, as in the convolution), and the
  * E-resp is exactly zero after 24 s. `fmrireg::fitted_hrf` evaluates the raw kernel out to 32 s, so from 24 s to 32 s
  * this deliberately differs from fmrireg; parity is asserted on 0-24 s.
  */
enum KernelBasis:
  /** Raw SPM canonical double gamma (peak 0.1754..., not peak-normalised: E-resp is scale invariant). */
  case Canonical
  /** Raw canonical, analytic temporal derivative and (fmrihrf) dispersion difference, in that column order. */
  case InformedThree
  /**
    * `bins` boxes of width `binWidth`: the fitted model's own response, constant on each bin `[k w, (k+1) w)` and zero from
    * the last bin edge on. This is the only FIR E-resp rule (owner decision 2026-10-01); it equals fmrireg `fitted_hrf`.
    */
  case Fir(bins: Int, binWidth: Double)

  def size: Int = this match
    case Canonical     => 1
    case InformedThree => 3
    case Fir(bins, _)  => bins

  /** Basis values at `lag` (zero for negative lags). */
  def valuesAt(lag: Double, out: Array[Double]): Unit = this match
    case Canonical =>
      out(0) = if lag > KernelBasis.SpmgSpanSeconds then 0.0 else HrfFunctions.spmg1(Lag(lag))
    case InformedThree =>
      if lag > KernelBasis.SpmgSpanSeconds then java.util.Arrays.fill(out, 0, 3, 0.0)
      else
        val l = Lag(lag)
        out(0) = HrfFunctions.spmg1(l)
        out(1) = HrfFunctions.spmg1Deriv(l)
        out(2) = HrfFunctions.spmg1DispersionDeriv(l)
    case Fir(bins, w) =>
      java.util.Arrays.fill(out, 0, bins, 0.0)
      if lag >= 0.0 && lag < bins * w then out(math.min(math.floor(lag / w).toInt, bins - 1)) = 1.0

object KernelBasis:
  /** Span of the SPM design kernels (`Hrfs.SPMG1`, `Hrfs.SPMG3`): their convolution is zero beyond it. */
  val SpmgSpanSeconds: Double = Hrfs.SPMG3.span.value

/** Fitted condition responses on a grid, in data units per unit drive: `curves(c)(i)` is condition `c` at `grid.lag(i)`. */
final case class ConditionResponse(grid: ResponseGrid, curves: Vector[Array[Double]]):
  def conditions: Int = curves.length

object EResp:

  /**
    * `r_c(tau) = sum_k beta(c, k) h_k(tau)` for a fixed basis. `coefficients` is condition-major (`c * basis.size + k`),
    * which is the column order of the event design.
    */
  def fromBasis(grid: ResponseGrid, basis: KernelBasis, coefficients: Array[Double], conditions: Int): Either[ERespError, ConditionResponse] =
    val nb = basis.size
    if conditions <= 0 then Left(ERespError.ConditionCount(1, conditions))
    else if coefficients.length != conditions * nb then Left(ERespError.CoefficientCount(conditions * nb, coefficients.length))
    else if coefficients.exists(c => !c.isFinite) then Left(ERespError.NonFinite("coefficient"))
    else
      val curves = Array.fill(conditions)(new Array[Double](grid.size))
      val h = new Array[Double](nb)
      var i = 0
      while i < grid.size do
        basis.valuesAt(grid.lag(i), h)
        var c = 0
        while c < conditions do
          var acc = 0.0
          var k = 0
          while k < nb do
            acc += coefficients(c * nb + k) * h(k)
            k += 1
          curves(c)(i) = acc
          c += 1
        i += 1
      Right(ConditionResponse(grid, curves.toVector))

  /**
    * PHRF: `r_c(tau) = a_c s(theta) k(tau; theta)`, where `a_c` is the condition amplitude in the family's library
    * normalisation (`ProfileVoxelResult.conditionMeans`), `s` the normalisation scale at the decoded point and `k` the
    * family's raw kernel. For the Gaussian family under `Density` this is `a_c` times the library `Hrfs.gaussian` kernel.
    */
  def fromParametric(
      grid: ResponseGrid,
      family: ParametricHrfFamily,
      point: ShapePoint,
      rule: NormalizationRule,
      amplitudes: Vector[Double]
  ): Either[ERespError, ConditionResponse] =
    if amplitudes.isEmpty then Left(ERespError.ConditionCount(1, 0))
    else if point.coordinates.length != family.dimension then Left(ERespError.WrongDimension(family.dimension, point.coordinates.length))
    else if !family.supports(rule) then Left(ERespError.UnsupportedNormalization(family.name, rule.label))
    else if amplitudes.exists(a => !a.isFinite) || point.coordinates.exists(c => !c.isFinite) then Left(ERespError.NonFinite("amplitude or coordinate"))
    else
      val scale = new Array[Double](family.jetComponents)
      family.scaleJetInto(rule, point, scale)
      val s = scale(JetLayout.Value)
      val k = new Array[Double](grid.size)
      family.evalInto(grid.lags, point, k)
      Right(ConditionResponse(grid, amplitudes.map(a => Array.tabulate(grid.size)(i => a * s * k(i)))))
