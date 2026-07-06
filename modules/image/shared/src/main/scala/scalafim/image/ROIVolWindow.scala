package scalafim.image

import narr.NArray

final case class ROIVolWindow[A](
  space: NeuroSpace,
  coords: ROICoords,
  data: NArray[A],
  centerIndex: Int,
  parentIndex: Int,
  label: String = ""
):
  require(coords.size == data.length, "data length must match coords")
  require(space.ndim >= 3, "space must be at least 3D")

  def toROIVol: ROIVol[A] =
    ROIVol(space, coords, data)
