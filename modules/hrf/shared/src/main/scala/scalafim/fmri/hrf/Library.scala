package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrf

object Library:

  def hrfLibrary[A](params: Seq[A], name: String = "hrf_library")(f: A => Hrf): Hrf =
    HrfCombinators.bindBasis(params.map(f), name = Some(name))
