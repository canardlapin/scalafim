package scalafim.phrfcmp.prep

import scalafim.phrfcmp.ingest.Matrix

/**
  * PHRF's frozen noise variance (design 2.0.7): whitened-refit RSS / (n - rank), pooled over voxels, on the whitened
  * pre-fit design. Whitening is per run, so selecting the rows of the training runs equals whitening only those runs.
  */
final case class Sigma2Result(sigma2: Double, rss: Double, rows: Int, rank: Int, voxels: Int)

object Sigma2:

  /** `training = None` uses every run (the final fit); `Some(runs)` uses only those runs (a LOROCV fold). */
  def pooled(w: WhitenedArrays, runId: Array[Int], training: Option[Set[Int]]): Either[PrepRefusal, Sigma2Result] =
    if w.prefitDesign.rows != runId.length || w.y.cols != runId.length then
      Left(PrepRefusal.Sigma2("whitened arrays and run_id disagree on the time axis"))
    else
      val rows = runId.indices.filter(t => training.forall(_.contains(runId(t)))).toArray
      if rows.isEmpty then Left(PrepRefusal.Sigma2("no training rows"))
      else
        val x = Linear.selectRows(w.prefitDesign, rows)
        val yT = Matrix.of(w.y.cols, w.y.rows, Array.tabulate(w.y.cols * w.y.rows)(k => w.y(k % w.y.rows, k / w.y.rows)))
        yT.left.map(e => PrepRefusal.Sigma2(e.message)).flatMap { y =>
          val (res, rank) = Linear.residuals(Linear.toDMat(x), Linear.toDMat(Linear.selectRows(y, rows)))
          val dof = rows.length - rank
          if dof <= 0 then Left(PrepRefusal.Sigma2(s"rank $rank leaves no degrees of freedom on ${rows.length} rows"))
          else
            var rss = 0.0
            var i = 0
            while i < res.rows do
              var v = 0
              while v < res.cols do
                rss += res(i, v) * res(i, v)
                v += 1
              i += 1
            Right(Sigma2Result(rss / (res.cols.toDouble * dof), rss, rows.length, rank, res.cols))
        }
