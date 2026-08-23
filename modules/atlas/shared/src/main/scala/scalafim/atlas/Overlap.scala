package scalafim.atlas

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
    val vol1 = atlas1.labelVolume
    val vol2Either: Either[AtlasError, NeuroVol[Int]] =
      alignment match
        case AtlasAlignment.Exact =>
          GridCompatibility.spatial(atlas1.space, atlas2.space) match
            case Right(_) =>
              Right(atlas2.labelVolume)
            case Left(_) if atlas1.space.spatialDims != atlas2.space.spatialDims =>
              Left(
                AtlasError.SpaceMismatch(
                  atlas1.space.spatialDims,
                  atlas2.space.spatialDims
                )
              )
            case Left(_) =>
              Left(
                AtlasError.ExactGridRequired(
                  atlas1.space.spatialSpace.toString,
                  atlas2.space.spatialSpace.toString
                )
              )
        case AtlasAlignment.NearestNeighborToFirst =>
          Right(
            Resample.nearest(
              atlas2.labelVolume,
              atlas1.space,
              fill = 0
            )
          )

    vol2Either.flatMap(vol2 => computeWithAlignedVolumes(atlas1, atlas2, vol1, vol2))

  @deprecated(
    "Use computeEither(atlas1, atlas2, AtlasAlignment); alignment must be explicit.",
    "0.2.0"
  )
  def computeEither(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    resample: Boolean
  ): Either[AtlasError, Vector[RegionOverlap]] =
    computeEither(
      atlas1,
      atlas2,
      if resample then AtlasAlignment.NearestNeighborToFirst
      else AtlasAlignment.Exact
    )

  private def computeWithAlignedVolumes(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    vol1: NeuroVol[Int],
    vol2: NeuroVol[Int]
  ): Either[AtlasError, Vector[RegionOverlap]] =

    val n1 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val n2 = scala.collection.mutable.Map.empty[Int, Int].withDefaultValue(0)
    val both = scala.collection.mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)

    var i = 0
    while i < vol1.values.size do
      val a = vol1.valueAtCanonicalOrdinal(i)
      val b = vol2.valueAtCanonicalOrdinal(i)
      if a != 0 then n1.update(a, n1(a) + 1)
      if b != 0 then n2.update(b, n2(b) + 1)
      if a != 0 && b != 0 then both.update((a, b), both((a, b)) + 1)
      i += 1

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
    Right(overlaps)

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

  @deprecated(
    "Use compute(atlas1, atlas2, AtlasAlignment); alignment must be explicit.",
    "0.2.0"
  )
  def compute(
    atlas1: VolumeAtlas,
    atlas2: VolumeAtlas,
    resample: Boolean
  ): Vector[RegionOverlap] =
    compute(
      atlas1,
      atlas2,
      if resample then AtlasAlignment.NearestNeighborToFirst
      else AtlasAlignment.Exact
    )
