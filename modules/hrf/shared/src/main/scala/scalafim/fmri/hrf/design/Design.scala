package scalafim.fmri.hrf.design

import scalafim.fmri.hrf.Hrf
import scalafim.fmri.hrf.s
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.regressor.{Regressor, RegressorSet}
import scalafim.fmri.hrf.linalg.Mat

object Design:

  def regressorDesign(
      onsets: Seq[Double],
      fac: Seq[String],
      block: Seq[Int],
      sframe: SamplingFrame,
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Double = 40.0,
      precision: Double = 0.33,
      method: Regressor.EvalMethod = Regressor.EvalMethod.Conv,
      summate: Boolean = true
  ): Mat =
    require(onsets.length == fac.length && onsets.length == block.length, "`onsets`, `fac`, and `block` must match length")
    val gOnsets = sframe.globalOnsets(onsets.map(_.s), block)
    val rs = RegressorSet(gOnsets.map(_.value), fac, hrf, duration, amplitude, Some(span), summate)
    val grid = sframe.samples(global = true).map(_.value)
    rs.evaluate(grid, precision, method)
