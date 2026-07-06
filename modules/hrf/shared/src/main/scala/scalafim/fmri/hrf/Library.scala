package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrf

object Library:

  def hrfSet(hrfs: Seq[Hrf], name: String = "hrf_set"): Hrf =
    HrfCombinators.bindBasis(hrfs, name = Some(name))

  def hrfLibrary[A](params: Seq[A], name: String = "hrf_library")(f: A => Hrf): Hrf =
    HrfCombinators.bindBasis(params.map(f), name = Some(name))
