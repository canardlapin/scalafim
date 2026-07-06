package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrf
import scalafim.fmri.hrf.linalg.Mat

object Reconstruction:
  def matrix(hrf: Hrf, times: Seq[Double]): Mat =
    hrf.evalDoubles(times)
