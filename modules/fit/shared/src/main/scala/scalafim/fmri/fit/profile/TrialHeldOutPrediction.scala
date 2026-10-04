package scalafim.fmri.fit.profile

import scalafim.fmri.design.hrf.ExpandedTrialDesign

/** Typed reason a held-out prediction was refused; no exception crosses this boundary. */
enum TrialHeldOutError:
  case AmplitudeCount(expected: Int, actual: Int)
  case OutputLength(expected: Int, actual: Int)
  case NonFiniteAmplitude(index: Int)
  case InvalidShape(detail: String)

  def message: String = this match
    case AmplitudeCount(e, a)   => s"$a trial amplitudes for a design with $e trials"
    case OutputLength(e, a)     => s"output has $a rows; the design has $e"
    case NonFiniteAmplitude(i)  => s"trial amplitude ${i + 1} is not finite"
    case InvalidShape(d)        => d

/**
  * Held-out prediction for the trial-banded PHRF model: the signal `sum_i a_i x_i(theta)` that a fixed shape `theta` and
  * a vector of trial amplitudes `a` imply on the rows of an expanded trial design lowered on ANY frame (typically the
  * single held-out run), where `x_i(theta) = sum_j c_j(theta) X_(i,j)` is the trial's regressor and `c(theta)` the
  * kernel-basis coefficients at the shape.
  *
  * Nothing here reads data: a held-out response enters only the score that compares it with this prediction, so the
  * prediction is a function of the training fit (shape, amplitudes) and the held-out design alone. Amplitudes are the raw
  * coefficients of the unnormalised family kernel (`ProfileVoxelResult.readout` / `TrialBandedReadout.trialAmplitudes`),
  * i.e. the units in which `x_i(theta)` is built; the public normalised amplitudes times the normalisation scale are the
  * same numbers.
  */
object TrialHeldOutPrediction:

  /** `c(theta)` (length = basis rank) of the design's kernel basis at `coordinates`. */
  def basisCoefficients(expanded: ExpandedTrialDesign, coordinates: Vector[Double]): Either[TrialHeldOutError, Array[Double]] =
    expanded.basis.family.chart.point(coordinates).left.map(e => TrialHeldOutError.InvalidShape(e.message)).map { point =>
      val out = new Array[Double](expanded.rank)
      expanded.basis.coefficientsInto(point, new Array[Double](expanded.basis.fineCount), out)
      out
    }

  /** Predicted (unwhitened) signal rows into `out(0 until expanded.rows)`. */
  def signalInto(
      expanded: ExpandedTrialDesign,
      coordinates: Vector[Double],
      trialAmplitudes: Array[Double],
      out: Array[Double]
  ): Either[TrialHeldOutError, Unit] =
    val n = expanded.trials
    if trialAmplitudes.length != n then Left(TrialHeldOutError.AmplitudeCount(n, trialAmplitudes.length))
    else if out.length != expanded.rows then Left(TrialHeldOutError.OutputLength(expanded.rows, out.length))
    else
      val bad = trialAmplitudes.indexWhere(a => !a.isFinite)
      if bad >= 0 then Left(TrialHeldOutError.NonFiniteAmplitude(bad))
      else
        basisCoefficients(expanded, coordinates).map { c =>
          val m = expanded.rank
          val cols = expanded.columns
          val data = expanded.term.data.data
          var r = 0
          while r < expanded.rows do
            var acc = 0.0
            var j = 0
            while j < m do
              val base = r * cols + j * n
              var s = 0.0
              var i = 0
              while i < n do
                s += data(base + i) * trialAmplitudes(i)
                i += 1
              acc += c(j) * s
              j += 1
            out(r) = acc
            r += 1
        }
