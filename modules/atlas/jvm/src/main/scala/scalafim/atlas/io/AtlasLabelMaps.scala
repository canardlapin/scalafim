package scalafim.atlas.io

import java.nio.file.Path
import image4s.ImageMetadata
import scalafim.atlas.*
import scalafim.image.*
import scalafim.image.io.Nifti

object AtlasLabelMaps:
  def readIntVolume(
      path: Path,
      label: String = ""
  ): SomeLabelVolume[Int] =
    fromDouble(Nifti.readVol(path), label)

  def fromDouble(
      vol: NeuroVol[Double],
      label: String = ""
  ): SomeLabelVolume[Int] =
    val out = Array.ofDim[Int](vol.values.size)
    var i = 0
    while i < out.length do
      val voxel = vol.space.indexToVoxel3D(i)
      val value = vol(voxel)
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
    NeuroVolume
      .copyCategoricalFromCanonicalArray[Int](
        vol.sampled.sampleSpace,
        out,
        ImageMetadata.named(outLabel)
      )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        SomeNeuroVolume.eraseSpace
      )

  def presentRegionIds(labels: SomeLabelVolume[Int]): Set[RegionId] =
    val shape = labels.grid.shape
    val out = scala.collection.mutable.Set.empty[RegionId]
    var x = 0
    while x < shape(0) do
      var y = 0
      while y < shape(1) do
        var z = 0
        while z < shape(2) do
          val id = labels(x, y, z)
          if id > 0 then out += RegionId(id)
          z += 1
        y += 1
      x += 1
    out.toSet

  def buildAtlas(
      ref: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int]
  ): VolumeAtlas =
    buildAtlas(
      ref,
      regions,
      labels,
      AtlasProvenance.fromRef(ref, regions)
    )

  def buildAtlas(
      ref: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int],
      provenance: AtlasProvenance
  ): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, labels, provenance)
