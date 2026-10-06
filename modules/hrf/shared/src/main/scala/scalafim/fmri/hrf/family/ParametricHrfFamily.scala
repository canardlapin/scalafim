package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind, PositiveSeconds, Seconds}

/** A normalisation rule declared over a whole family. The rule is fixed; its
  * numerical scale may vary with shape and then carries derivatives.
  */
enum NormalizationRule(val label: String):
  /** The family's raw kernel; scale 1 everywhere. */
  case Unnormalised extends NormalizationRule("unnormalised")
  /** Maximum value 1 (analytic, not a grid maximum). */
  case UnitPeak extends NormalizationRule("unit_peak")
  /** Signed integral 1 over the causal support. */
  case UnitIntegral extends NormalizationRule("unit_integral")
  /** The probability-density convention of the existing library kernel. */
  case Density extends NormalizationRule("density")
  /** Unit integral of the positive component, before subtracting the undershoot. */
  case PositiveComponentArea extends NormalizationRule("positive_component_area")

/** Whether an exact finite-state realisation of the family exists. */
enum RealizationSupport:
  case Exact(states: Int)
  case Approximate(states: Int, detail: String)
  case Unavailable

final case class ShapeSummary(peakLatency: Seconds, fwhm: Seconds, undershootRatio: Option[Double])

/** A constrained scalar HRF family with a parameter chart and analytic jets.
  *
  * This is the shape contract a profile fitter consumes: values and first and
  * mixed second parameter derivatives on a lag grid, written into
  * caller-owned arrays, plus the normalisation scale and its derivatives under
  * a declared rule. It is deliberately distinct from [[Hrf]] (a kernel at one
  * fixed shape), from a `ResponseBasis` (a fixed linear basis) and from
  * `Deriv` (which differentiates lag, not parameters).
  *
  * Jets are laid out component-major: `out(component * n + i)` for lag `i`,
  * with components as in [[JetLayout]]. Values are of the unnormalised kernel;
  * apply [[scaleJetInto]] to obtain a normalised convention.
  */
trait ParametricHrfFamily:
  def name: String
  def kind: HrfKind
  def chart: ShapeChart

  /** Declared evaluation horizon `T_h`; the tail beyond it is bounded, not zero. */
  def horizon: PositiveSeconds

  def realization: RealizationSupport = RealizationSupport.Unavailable

  /** Whether second parameter derivatives exist everywhere in the chart. */
  def secondOrderSmooth: Boolean = true

  final def dimension: Int = chart.dimension
  final def jetComponents: Int = JetLayout.components(dimension)

  def supports(rule: NormalizationRule): Boolean

  /** The rule under which [[toHrf]] equals the family kernel times its scale. */
  def libraryNormalization: NormalizationRule

  /** Unnormalised kernel values at `lags` into `out(0 until lags.length)`; zero for negative lags. */
  def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit

  /** Unnormalised value and parameter derivatives at `lags`, component-major,
    * into `out` of length `jetComponents * lags.length`.
    */
  def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit

  /** Scale `s(theta)` of `rule` and its parameter derivatives, `jetComponents` values. */
  def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit

  def summaries(point: ShapePoint): ShapeSummary

  /** Structurally unidentified chart coordinates at this shape, independently
    * of any experiment's information or a fitter's conditional uncertainty.
    */
  def unidentifiedCoordinates(@annotation.unused point: ShapePoint): Vector[String] = Vector.empty

  /** Provenance for the kernel realised at `point`. */
  def descriptor(point: ShapePoint): HrfDescriptor

  /** The existing library kernel at `point`, in the [[libraryNormalization]] convention. */
  def toHrf(point: ShapePoint): Hrf

  /** Allocation-free admission for the tail diagnostic's lag/value arrays. */
  final def validateTailRelativeEnergyGrid(precision: PositiveSeconds, extent: Double = 4.0): Either[FamilySummaryError, Unit] =
    FamilySummaryGrid.tail(horizon, precision, extent).map(_ => ())

  /** Relative squared-norm mass beyond the horizon. The requested extent must
    * be finite and >= 1. The trapezoid grid rounds up to the next precision step
    * (and has at least two samples), preserving the legacy endpoint semantics.
    * Extent 1 is accepted; a ceil overshoot can still contribute tail mass.
    * Requests above [[FamilySummaryGrid.MaxSamples]] are refused, never coarsened.
    * This is a receipt diagnostic, not a truncation guarantee.
    */
  def tailRelativeEnergyEither(point: ShapePoint, precision: PositiveSeconds, extent: Double = 4.0): Either[FamilySummaryError, Double] =
    for
      checkedPoint <- chart.point(point.coordinates).left.map(FamilySummaryError.Chart.apply)
      grid <- FamilySummaryGrid.tail(horizon, precision, extent)
      evaluated <- grid.evaluate(this, checkedPoint)
      energy <-
        val (lags, values) = evaluated
        var total = 0.0
        var tail = 0.0
        var i = 0
        while i < values.length do
          val w = if i == 0 || i == values.length - 1 then 0.5 else 1.0
          val v = w * values(i) * values(i)
          total += v
          if lags(i) > horizon.value then tail += v
          i += 1
        if !total.isFinite || !tail.isFinite then Left(FamilySummaryError.NonFiniteEnergy)
        else Right(if total > 0.0 then tail / total else 0.0)
    yield energy

  /** Throwing compatibility facade for [[tailRelativeEnergyEither]]. */
  def tailRelativeEnergy(point: ShapePoint, precision: PositiveSeconds, extent: Double = 4.0): Double =
    tailRelativeEnergyEither(point, precision, extent).fold(error => throw new IllegalArgumentException(error.message), identity)
