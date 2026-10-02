package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Vec

/** The convention used to form the temporal column paired with a scalar SPMG
  * canonical response. The finite difference is deliberately a response
  * convention, rather than a numerical approximation used elsewhere. */
enum TemporalDerivativeConvention:
  case AnalyticSpmg
  case SpmOneSecondBackwardDifference

enum TemporalDerivativeConventionError:
  case RequiresScalarSpmgCanonical(name: String, basis: Int)

  def message: String = this match
    case RequiresScalarSpmgCanonical(name, basis) =>
      s"temporal derivative convention requires a scalar SPMG canonical response, got '$name' with $basis basis columns"

object TemporalDerivativeConvention:
  /** Derive a temporal response from a scalar SPMG canonical kernel.
    *
    * `AnalyticSpmg` is the continuous derivative. `SpmOneSecondBackwardDifference`
    * is exactly `h(t) - h(t - 1 second)`, the discrete SPM convention. */
  def derive(canonical: Hrf, convention: TemporalDerivativeConvention): Either[TemporalDerivativeConventionError, Hrf] =
    (canonical.descriptor.family, canonical.descriptor.params, canonical.nbasis) match
      case (HrfFamily.Known(HrfKind.Spmg1), HrfParams.Spmg(params), 1) =>
        convention match
          case TemporalDerivativeConvention.AnalyticSpmg =>
            Right(Hrfs.spmg1TemporalDeriv(params.p1, params.p2, params.a1, canonical.span))
          case TemporalDerivativeConvention.SpmOneSecondBackwardDifference =>
            val step = Seconds(1.0)
            val descriptor = canonical.descriptor.derived("spm-1-second-temporal-difference", span = canonical.span + step)
            val difference = Hrf.of(
              name = s"${canonical.name}_spm_temporal_difference",
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
            Right(Hrf.withBasisElements(difference, Vector(element)).fold(error => throw new IllegalStateException(error.message), identity))
      case _ => Left(TemporalDerivativeConventionError.RequiresScalarSpmgCanonical(canonical.name, canonical.nbasis))
