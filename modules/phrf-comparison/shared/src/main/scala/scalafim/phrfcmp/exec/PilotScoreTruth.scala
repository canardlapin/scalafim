package scalafim.phrfcmp.exec

import scala.util.control.NonFatal
import scalafim.phrfcmp.ingest.{BoundDataset, CellKind}
import scalafim.phrfcmp.score.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** Scoring choices are explicit and named in the assembler schema; no change can reinterpret an old aggregate. */
enum GaussianCoverageTarget(val code: String):
  case Tau extends GaussianCoverageTarget("tau")
  case BothShapeCoordinates extends GaussianCoverageTarget("tau-and-log-sigma")

/** Immutable scorer-only snapshot. It must never be handed to a fitter or a dataset source. */
final class PilotScoreTruth private (
    val cell: ScoreCell, val dataset: Int, private[phrfcmp] val inputFingerprint: String, private[phrfcmp] val sourceFileSha256: String,
    private[phrfcmp] val responses: Vector[ConditionEstimate], private[phrfcmp] val trial: Option[TrialTruth],
    private[phrfcmp] val gaussian: Vector[(Double, Double, Vector[Double])]):
  private[phrfcmp] def voxels: Int = if cell.isCondition then responses.length else trial.get.voxels
  override def toString: String = "PilotScoreTruth(<redacted>)"

object PilotScoreTruth:
  /** Generator v1 has marginal noise variance 1, 0.1-second truth samples and H=32. The condition response is
    * cond_coef times the unit-peak truth kernel, never the drive-convolved cond_peak_amp. Trial betas are already
    * in single-trial peak units and remain in original event order. TG parameters are (tau, sigma), whereas the
    * fitted chart is (tau, log sigma). Copies isolate the scorer from mutable generator arrays.
    */
  def fromBound(b: BoundDataset): Either[String, PilotScoreTruth] =
    try
      val cell = ScoreCell.values.find(_.id == b.manifest.cell.cellId).get
      require(b.manifest.cell.kind == (if cell.isCondition then CellKind.Condition else CellKind.Trial))
      require(b.manifest.dataset >= 0 && b.inputSha256.matches("[0-9a-f]{64}"))
      val t = b.truth; val v = b.fit.y.rows
      require(v > 0 && t.condCoef.rows == v && t.condCoef.cols == 3)
      val fp = ScoringInputFingerprint.of(b.fit)
      if cell.isCondition then
        val lags = Vector.tabulate(321)(_ / 10.0)
        require(t.truthT.length >= 321 && lags.indices.forall(i => t.truthT(i) == lags(i)))
        require(t.truthKernel.rows == v && t.truthKernel.cols == t.truthT.length)
        val responses = Vector.tabulate(v) { i =>
          val curves = Vector.tabulate(3)(c => lags.indices.map(k => t.condCoef(i, c) * t.truthKernel(i, k)).toVector)
          require(curves.flatten.forall(_.isFinite))
          new ConditionEstimate(lags, curves, None)
        }
        val g = if cell != ScoreCell.CTG5 then Vector.empty else
          require(t.truthParams.rows == v && t.truthParams.cols == 2)
          Vector.tabulate(v) { i =>
            val tau = t.truthParams(i, 0); val sigma = t.truthParams(i, 1)
            require(tau.isFinite && sigma.isFinite && sigma > 0.0)
            // Generator normalizes the Gaussian at its maximum on the fixed 0.01 s grid, not at continuous tau.
            val node = math.round(tau * 100.0) / 100.0
            val finePeak = math.exp(-0.5 * math.pow((node - tau) / sigma, 2))
            val peaks = Vector.tabulate(3)(c => t.condCoef(i, c) / finePeak)
            require(peaks.forall(x => x.isFinite && x != 0.0))
            (tau, math.log(sigma), peaks)
          }
        Right(new PilotScoreTruth(cell, b.manifest.dataset, fp, b.inputSha256, responses, None, g))
      else
        val beta = t.trialBeta.get
        require(beta.rows == v && beta.cols == b.fit.evCond.length)
        val truth = TrialTruth.of(b.fit.evCond.toVector, Vector.tabulate(v)(i => Vector.tabulate(beta.cols)(beta(i, _)))).toOption.get
        Right(new PilotScoreTruth(cell, b.manifest.dataset, fp, b.inputSha256, Vector.empty, Some(truth), Vector.empty))
    catch case NonFatal(_) => Left("invalid_scoring_truth")

/** The declared per-voxel TG summaries. Peak error is the arithmetic mean across the three conditions of
  * (estimated signed peak - true signed peak) / abs(true signed peak). Thus negative amplitudes retain their sign.
  * Coverage is conditional on Accepted and uses the existing data-curvature conditional SD, without a new fit.
  */
private[phrfcmp] object PilotCoverage:
  def observation(e: ConditionEstimate, truth: (Double, Double, Vector[Double]), target: GaussianCoverageTarget): Either[String, CoverageObs] =
    e.gaussian.toRight("missing_gaussian_readout").flatMap { g =>
      val (tau, logSigma, peaks) = truth
      if g.peaks.length != peaks.length then Left("peak_layout")
      else
        val peakError = g.peaks.zip(peaks).map((a, b) => (a - b) / math.abs(b)).sum / peaks.length
        val tauError = g.tau - tau
        val tauCovered = math.abs(tauError) <= g.sd(0)
        val covered = target match
          case GaussianCoverageTarget.Tau => tauCovered
          case GaussianCoverageTarget.BothShapeCoordinates => tauCovered && math.abs(g.logSigma - logSigma) <= g.sd(1)
        CoverageObs.of(peakError, tauError, covered).left.map(_ => "coverage_observation")
    }
