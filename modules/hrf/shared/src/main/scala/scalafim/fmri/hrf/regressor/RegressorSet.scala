package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.Hrf
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.linalg.Mat

final case class RegressorSet(regs: Vector[Regressor], levels: Vector[String]):
  def nbasis: Int = regs.map(_.hrf.nbasis).sum

  def evaluate(
      grid: Seq[Double],
      precision: Double = 0.33,
      method: Regressor.EvalMethod = Regressor.EvalMethod.Conv
  ): Mat =
    val mats = regs.map(r => Regressor.evaluate(r, grid, precision, method))
    mats.reduceOption(_ ++ _).getOrElse(Mat.zeros(grid.length, 0))

object RegressorSet:

  def apply(
      onsets: Seq[Double],
      fac: Seq[String],
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): RegressorSet =
    require(onsets.length == fac.length, "`fac` must match `onsets` length")
    val n = onsets.length
    require(duration.length == 1 || duration.length == n, "`duration` must have length 1 or match `onsets`")
    require(amplitude.length == 1 || amplitude.length == n, "`amplitude` must have length 1 or match `onsets`")
    def durAt(i: Int): Double = if duration.length == 1 then duration.head else duration(i)
    def ampAt(i: Int): Double = if amplitude.length == 1 then amplitude.head else amplitude(i)
    val levs = fac.foldLeft(Vector.empty[String]) { (acc, f) =>
      if acc.contains(f) then acc else acc :+ f
    }
    val regs = levs.map { lv =>
      val idx = fac.indices.filter(i => fac(i) == lv)
      val onsLv = idx.map(onsets)
      val durLv = idx.map(durAt)
      val ampLv = idx.map(ampAt)
      Regressor(onsLv, hrf, durLv, ampLv, span, summate)
    }.toVector
    RegressorSet(regs, levs)
