package scalafim.atlas

import scalafim.image.Indexing

final case class QueryHit(
  pointIndex: Int,
  input: Point3D,
  atlasPoint: Point3D,
  atlasName: String,
  region: Option[AtlasRegionMetadata],
  distanceMm: Option[Double]
):
  def id: Option[RegionId] =
    region.map(_.id)

  def label: Option[String] =
    region.map(_.label)

object AtlasQuery:
  def exactEither(
    atlas: VolumeAtlas,
    point: Point3D,
    fromSpace: AnySpaceId = SpaceId.MNI152
  ): Either[AtlasError, QueryHit] =
    queryEither(atlas, Vector(point), radiusMm = 0.0, fromSpace).flatMap {
      case head +: _ => Right(head)
      case _ => Left(AtlasError.InvalidQuery("exact atlas query requires one point"))
    }

  def exact(
    atlas: VolumeAtlas,
    point: Point3D,
    fromSpace: AnySpaceId = SpaceId.MNI152
  ): QueryHit =
    exactEither(atlas, point, fromSpace).fold(err => throw new IllegalArgumentException(err.message), identity)

  def queryEither(
    atlas: VolumeAtlas,
    points: Vector[Point3D],
    radiusMm: Double = 0.0,
    fromSpace: AnySpaceId = SpaceId.MNI152
  ): Either[AtlasError, Vector[QueryHit]] =
    if radiusMm < 0.0 || !radiusMm.isFinite then
      Left(AtlasError.InvalidQuery("radiusMm must be finite and non-negative"))
    else
      queryValidated(atlas, points, radiusMm, fromSpace)

  def query(
    atlas: VolumeAtlas,
    points: Vector[Point3D],
    radiusMm: Double = 0.0,
    fromSpace: AnySpaceId = SpaceId.MNI152
  ): Vector[QueryHit] =
    queryEither(atlas, points, radiusMm, fromSpace).fold(err => throw new IllegalArgumentException(err.message), identity)

  private def queryValidated(
    atlas: VolumeAtlas,
    points: Vector[Point3D],
    radiusMm: Double,
    fromSpace: AnySpaceId
  ): Either[AtlasError, Vector[QueryHit]] =
    val atlasPointsEither =
      val fromNorm = SpaceId.normalize(fromSpace)
      val toNorm = SpaceId.normalize(atlas.ref.coordSpace)
      if fromNorm == toNorm then Right(points)
      else
        SpaceTransforms.transformCoords(points, fromNorm, toNorm)

    atlasPointsEither.map { atlasPoints =>
      if radiusMm == 0.0 then
        atlasPoints.zip(points).zipWithIndex.map { case ((atlasPoint, input), idx) =>
          exactOne(atlas, input, atlasPoint, idx)
        }
      else
        atlasPoints.zip(points).zipWithIndex.flatMap { case ((atlasPoint, input), idx) =>
          radiusOne(atlas, input, atlasPoint, idx, radiusMm)
        }
    }

  private def exactOne(atlas: VolumeAtlas, input: Point3D, atlasPoint: Point3D, pointIndex: Int): QueryHit =
    val grid = atlas.space.coordToIndex(atlasPoint.toVector).map(v => math.round(v).toInt)
    val id = labelAtGrid(atlas, grid)
    QueryHit(pointIndex, input, atlasPoint, atlas.name, id.flatMap(atlas.region), Some(0.0))

  private def radiusOne(
    atlas: VolumeAtlas,
    input: Point3D,
    atlasPoint: Point3D,
    pointIndex: Int,
    radiusMm: Double
  ): Vector[QueryHit] =
    val center = atlas.space.coordToIndex(atlasPoint.toVector).map(v => math.round(v).toInt)
    val offsets = radiusOffsets(atlas, radiusMm)
    var hits = Map.empty[RegionId, Double]
    offsets.foreach { off =>
      val grid = Vector(center(0) + off(0), center(1) + off(1), center(2) + off(2))
      if inBounds(atlas, grid) then
        val world = Point3D.fromVector(atlas.space.indexToCoord(grid.map(_.toDouble)))
        val dist = distance(world, atlasPoint)
        if dist <= radiusMm + 1e-12 then
          labelAtGrid(atlas, grid).foreach { id =>
            val old = hits.get(id)
            if old.isEmpty || dist < old.get then hits = hits.updated(id, dist)
          }
    }

    if hits.isEmpty then
      Vector(QueryHit(pointIndex, input, atlasPoint, atlas.name, None, None))
    else
      hits.toVector
        .sortBy { case (id, dist) => (dist, id.value) }
        .map { case (id, dist) =>
          QueryHit(pointIndex, input, atlasPoint, atlas.name, atlas.region(id), Some(dist))
        }

  private def labelAtGrid(atlas: VolumeAtlas, grid: Vector[Int]): Option[RegionId] =
    if !inBounds(atlas, grid) then None
    else
      val lin = Indexing.gridToIndex3D(atlas.space.spatialDims, grid(0), grid(1), grid(2))
      val id = atlas.labelVolume.valueAtCanonicalOrdinal(lin)
      if id == 0 then None else Some(RegionId(id))

  private def inBounds(atlas: VolumeAtlas, grid: Vector[Int]): Boolean =
    val dims = atlas.space.spatialDims
    grid.length == 3 &&
      grid(0) >= 0 && grid(0) < dims(0) &&
      grid(1) >= 0 && grid(1) < dims(1) &&
      grid(2) >= 0 && grid(2) < dims(2)

  private def radiusOffsets(atlas: VolumeAtlas, radiusMm: Double): Vector[Vector[Int]] =
    val spacing = atlas.space.spacing
    val rx = math.ceil(radiusMm / spacing(0)).toInt + 1
    val ry = math.ceil(radiusMm / spacing(1)).toInt + 1
    val rz = math.ceil(radiusMm / spacing(2)).toInt + 1
    Vector.tabulate((2 * rx + 1) * (2 * ry + 1) * (2 * rz + 1)) { idx =>
      val nx = 2 * rx + 1
      val ny = 2 * ry + 1
      val x = idx % nx
      val y = (idx / nx) % ny
      val z = idx / (nx * ny)
      Vector(x - rx, y - ry, z - rz)
    }

  private def distance(a: Point3D, b: Point3D): Double =
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    math.sqrt(dx * dx + dy * dy + dz * dz)
