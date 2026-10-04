package scalafim.phrfcmp.score

/** ICC(1) of one-way random effects, clusters = datasets, with the F-based 80 % upper limit.
  *
  * `(MSB - MSW) / (MSB + (k0 - 1) MSW)`, where `k0 = (N - sum n_i^2 / N) / (a - 1)` is the usual unbalanced
  * cluster size (equal to `k` when every cluster has `k` members, which is the design's formula with k = 40).
  * Both the point estimate and the limit are truncated to [0, 1]. The grand mean is computed and dropped inside
  * `of`; no function returns a mean.
  */
object Icc:
  /** ICC and its one-sided 80 % upper limit. */
  final case class Estimate(icc: Double, upper80: Double)

  /** `None` when the ICC is undefined: fewer than two non-empty clusters, no within-cluster df, or MSB = MSW = 0. */
  def of(clusters: Vector[IndexedSeq[Double]]): Either[ScoreError, Option[Estimate]] =
    val cs = clusters.filter(_.nonEmpty)
    if cs.exists(_.exists(v => v.isNaN || v.isInfinite)) then Left(ScoreError.NonFiniteValue(ScoreError.Site.IccCluster))
    else
      val a = cs.length
      val nTot = cs.map(_.length).sum
      if a < 2 || nTot <= a then Right(None)
      else
        val grand = cs.map(_.sum).sum / nTot
        var ssb = 0.0
        var ssw = 0.0
        var sumN2 = 0.0
        cs.foreach { c =>
          val ni = c.length.toDouble
          val mi = c.sum / ni
          ssb += ni * (mi - grand) * (mi - grand)
          c.foreach(x => ssw += (x - mi) * (x - mi))
          sumN2 += ni * ni
        }
        val dfb = (a - 1).toDouble
        val dfw = (nTot - a).toDouble
        val msb = ssb / dfb
        val msw = ssw / dfw
        val k0 = (nTot.toDouble - sumN2 / nTot) / dfb
        if msb == 0.0 && msw == 0.0 then Right(None)
        else
          val icc = clamp01((msb - msw) / (msb + (k0 - 1.0) * msw))
          val upper =
            if msw == 0.0 then 1.0
            else
              val fq = Dist.fQuantile(0.80, dfw, dfb)
              val fu = msb / msw * fq
              clamp01((fu - 1.0) / (fu + k0 - 1.0))
          Right(Some(Estimate(icc, upper)))

  private def clamp01(x: Double): Double = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x
