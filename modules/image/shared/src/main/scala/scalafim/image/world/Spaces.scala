package scalafim.image.world

import image4s.geometry.{D3, Frame}

/** Stable frames for standard template spaces, so they can appear in types:
  * {{{
  * val peak: Point[Spaces.Mni2009c, D3] = ...
  * }}}
  * Frames decoded from files are other runtime owners with the same persistent keys; reach these static frames through
  * [[Placed.bindTo]].
  */
object Spaces:
  val MNI152NLin2009cAsym: Frame[D3] = template("MNI152NLin2009cAsym")
  val MNI152NLin6Asym: Frame[D3] = template("MNI152NLin6Asym")
  val MNI305: Frame[D3] = template("MNI305")
  val fsaverage: Frame[D3] = template("fsaverage")
  val fsLR: Frame[D3] = template("fsLR")

  type Mni2009c = MNI152NLin2009cAsym.type
  type Mni6 = MNI152NLin6Asym.type
  type Mni305 = MNI305.type
  type FsAverage = fsaverage.type
  type FsLR = fsLR.type

  private def template(name: String): Frame[D3] =
    FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe(name)))
