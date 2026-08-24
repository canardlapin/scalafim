package scalafim.atlas

import scalafim.image.SampleSpaces.*

import image4s.geometry.Grid
import scalafim.image.*

final case class RegionOverlap(
  region1: AtlasRegionMetadata,
  region2: AtlasRegionMetadata,
  dice: Double,
  jaccard: Double,
  nOverlap: Int,
  nRegion1: Int,
  nRegion2: Int
)

enum AtlasAlignment:
  /** Compare equal voxel ordinals only after exact grid validation. */
  case Exact

  /** Explicitly resample the second atlas into the first atlas grid with
    * nearest-neighbor label interpolation.
    */
  case NearestNeighborToFirst

object AtlasOverlap:
  def computeEither(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas
  ): Either[AtlasError, Vector[RegionOverlap]] =
    computeEither(atlas1, atlas2, AtlasAlignment.Exact)

  def computeEither(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    alignment: AtlasAlignment
  ): Either[AtlasError, Vector[RegionOverlap]] =
    alignment match
      case AtlasAlignment.Exact =>
        requireExactGrid(atlas1, atlas2)
          .map(_ => computeFromAssignments(atlas1, atlas2))
      case AtlasAlignment.NearestNeighborToFirst =>
        val first = atlas1.labelVolume
        NativeResampling
          .nearestLike(atlas2.labelVolume, first, fill = 0)
          .left
          .map(error => AtlasError.InvalidAlignment(error.message))
          .map(second =>
            computeWithAlignedVolumes(atlas1, atlas2, first, second)
          )

  private def requireExactGrid(
      atlas1: VolumeAtlas,
      atlas2: VolumeAtlas
  ): Either[AtlasError, Unit] =
    Grid
      .exactCongruence(atlas1.labelVolume.grid, atlas2.labelVolume.grid)
      .left
      .map(AtlasError.Geometry.apply)
      .map(_ => ())

  private def computeFromAssignments(
      atlas1: VolumeAtlas,
      atlas2: VolumeAtlas
  ): Vector[RegionOverlap] =
    val first = atlas1.realization
    val second = atlas2.realization
    val n1 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val n2 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val both = scala.collection.mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)

    var ordinal = 0
    while ordinal < first.domain.space.size do
      val firstVoxel = first.domain.space.indexAtValidatedOrdinal(ordinal)
      val secondVoxel = second.domain.space.indexAtValidatedOrdinal(ordinal)
      val a =
        first.parcelAssignment(firstVoxel)
          .fold(0)(parcel => first.metadata(parcel).id.value)
      val b =
        second.parcelAssignment(secondVoxel)
          .fold(0)(parcel => second.metadata(parcel).id.value)
      accumulate(a, b, n1, n2, both)
      ordinal += 1

    buildOverlaps(atlas1, atlas2, n1, n2, both)

  private def computeWithAlignedVolumes(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    vol1: SomeLabelVolume[Int],
    vol2: SomeLabelVolume[Int]
  ): Vector[RegionOverlap] =
    val n1 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val n2 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val both = scala.collection.mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)

    var i = 0
    while i < vol1.values.size do
      val voxel = atlas1.space.indexToVoxel3D(i)
      val a = vol1(voxel)
      val b = vol2(voxel)
      accumulate(a, b, n1, n2, both)
      i += 1

    buildOverlaps(atlas1, atlas2, n1, n2, both)

  private def accumulate(
      a: Int,
      b: Int,
      n1: scala.collection.mutable.Map[Int, Int],
      n2: scala.collection.mutable.Map[Int, Int],
      both: scala.collection.mutable.Map[(Int, Int), Int]
  ): Unit =
    if a != 0 then n1.update(a, n1(a) + 1)
    if b != 0 then n2.update(b, n2(b) + 1)
    if a != 0 && b != 0 then
      both.update((a, b), both((a, b)) + 1)

  private def buildOverlaps(
      atlas1: VolumeAtlas,
      atlas2: VolumeAtlas,
      n1: scala.collection.mutable.Map[Int, Int],
      n2: scala.collection.mutable.Map[Int, Int],
      both: scala.collection.mutable.Map[(Int, Int), Int]
  ): Vector[RegionOverlap] =
    val overlaps = both.toVector.flatMap { case ((id1, id2), nOverlap) =>
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
    overlaps

  def compute(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas
  ): Vector[RegionOverlap] =
    compute(atlas1, atlas2, AtlasAlignment.Exact)

  def compute(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    alignment: AtlasAlignment
  ): Vector[RegionOverlap] =
    computeEither(atlas1, atlas2, alignment)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
