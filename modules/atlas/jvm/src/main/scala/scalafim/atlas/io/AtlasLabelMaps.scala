package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*
import scalafim.image.*
import scalafim.image.io.Nifti

object AtlasLabelMaps:
  def readIntVolume(path: Path, label: String = ""): NeuroVol[Int] =
    fromDouble(Nifti.readVol(path), label)

  def fromDouble(vol: NeuroVol[Double], label: String = ""): NeuroVol[Int] =
    val out = Array.ofDim[Int](vol.values.size)
    var i = 0
    while i < out.length do
      val value = vol.valueAtCanonicalOrdinal(i)
      if !value.isFinite then
        throw new IllegalArgumentException(s"label volume contains non-finite value at linear index $i")
      val rounded = math.round(value).toInt
      if math.abs(value - rounded.toDouble) > 1e-6 then
        throw new IllegalArgumentException(s"label volume contains non-integer value $value at linear index $i")
      if rounded < 0 then
        throw new IllegalArgumentException(s"label volume contains negative region id $rounded at linear index $i")
      out(i) = rounded
      i += 1
    val outLabel = if label.nonEmpty then label else vol.label
    NeuroVol.copyFromCanonicalArray[Int](out, vol.space, outLabel)

  def buildAtlas(ref: AtlasRef, regions: RegionIndex, labels: NeuroVol[Int], label: String = ""): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, labels, label)
