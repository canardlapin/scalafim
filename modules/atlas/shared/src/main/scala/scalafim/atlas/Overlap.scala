package scalafim.atlas

import scalafim.image.*

final case class RegionOverlap(
  region1: Region,
  region2: Region,
  dice: Double,
  jaccard: Double,
  nOverlap: Int,
  nRegion1: Int,
  nRegion2: Int
)

object AtlasOverlap:
  def compute(atlas1: VolumeAtlas, atlas2: VolumeAtlas, resample: Boolean = true): Vector[RegionOverlap] =
    val vol1 = atlas1.labelVolume
    val vol2 =
      if atlas1.space.spatialDims == atlas2.space.spatialDims then atlas2.labelVolume
      else if resample then Resample.nearest(atlas2.labelVolume, atlas1.space, fill = 0)
      else throw new IllegalArgumentException(AtlasError.SpaceMismatch(atlas1.space.spatialDims, atlas2.space.spatialDims).message)

    val n1 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val n2 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val both = scala.collection.mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)

    var i = 0
    while i < vol1.values.data.length do
      val a = vol1.linear(i)
      val b = vol2.linear(i)
      if a != 0 then n1.update(a, n1(a) + 1)
      if b != 0 then n2.update(b, n2(b) + 1)
      if a != 0 && b != 0 then both.update((a, b), both((a, b)) + 1)
      i += 1

    both.toVector.flatMap { case ((id1, id2), nOverlap) =>
      for
        r1 <- atlas1.region(RegionId(id1))
        r2 <- atlas2.region(RegionId(id2))
      yield
        val a = n1(id1)
        val b = n2(id2)
        RegionOverlap(
          r1,
          r2,
          dice = 2.0 * nOverlap.toDouble / (a.toDouble + b.toDouble),
          jaccard = nOverlap.toDouble / (a.toDouble + b.toDouble - nOverlap.toDouble),
          nOverlap = nOverlap,
          nRegion1 = a,
          nRegion2 = b
        )
    }.sortBy(o => (-o.dice, o.region1.id.value, o.region2.id.value))
