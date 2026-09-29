package scalafim.atlas

import scalafim.image.Indexing
import scalafim.image.SampleSpaces.*

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
  private val maxRadiusVisitedVoxels = 1000000L
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

    atlasPointsEither.flatMap { atlasPoints =>
      if radiusMm == 0.0 then
        atlasPoints.zip(points).zipWithIndex.foldLeft(Right(Vector.empty): Either[AtlasError, Vector[QueryHit]]) {
          case (result, ((atlasPoint, input), idx)) =>
            result.flatMap(hits => exactOne(atlas, input, atlasPoint, idx).map(hits :+ _))
        }
      else
        atlasPoints.zip(points).zipWithIndex.foldLeft(Right(Vector.empty): Either[AtlasError, Vector[QueryHit]]) {
          case (result, ((atlasPoint, input), idx)) =>
            result.flatMap(hits => radiusOne(atlas, input, atlasPoint, idx, radiusMm).map(hits ++ _))
        }
    }

  private def exactOne(atlas: VolumeAtlas, input: Point3D, atlasPoint: Point3D, pointIndex: Int): Either[AtlasError, QueryHit] =
    gridCenter(atlas, atlasPoint).map { grid =>
      val id = labelAtGrid(atlas, grid)
      QueryHit(pointIndex, input, atlasPoint, atlas.name, id.flatMap(atlas.region), Some(0.0))
    }

  private def radiusOne(
    atlas: VolumeAtlas,
    input: Point3D,
    atlasPoint: Point3D,
    pointIndex: Int,
    radiusMm: Double
  ): Either[AtlasError, Vector[QueryHit]] =
    gridCenter(atlas, atlasPoint).flatMap { center =>
      radiusBounds(atlas, center, radiusMm).flatMap { case (lower, upper) =>
        var hits = Map.empty[RegionId, Double]
        var validWorldCoordinates = true
        var z = lower(2)
        while z <= upper(2) do
          var y = lower(1)
          while y <= upper(1) do
            var x = lower(0)
            while x <= upper(0) do
              val grid = Vector(x, y, z)
              pointFromFiniteCoordinates(atlas.space.indexToCoord(grid.map(_.toDouble))) match
                case Left(_) => validWorldCoordinates = false
                case Right(world) =>
                  val dist = distance(world, atlasPoint)
                  if dist <= radiusMm + 1e-12 then
                    labelAtGrid(atlas, grid).foreach { id =>
                      val old = hits.get(id)
                      if old.isEmpty || dist < old.get then hits = hits.updated(id, dist)
                    }
              x += 1
            y += 1
          z += 1

        if !validWorldCoordinates then
          Left(AtlasError.InvalidQuery("atlas query voxel coordinates do not map to finite world coordinates"))
        else if hits.isEmpty then
          Right(Vector(QueryHit(pointIndex, input, atlasPoint, atlas.name, None, None)))
        else
          Right(hits.toVector
            .sortBy { case (id, dist) => (dist, id.value) }
            .map { case (id, dist) =>
              QueryHit(pointIndex, input, atlasPoint, atlas.name, atlas.region(id), Some(dist))
            })
      }
    }

  private def gridCenter(atlas: VolumeAtlas, atlasPoint: Point3D): Either[AtlasError, Vector[Int]] =
    val coordinates = atlas.space.coordToIndex(atlasPoint.toVector)
    if coordinates.length != 3 || !coordinates.forall(_.isFinite) then
      Left(AtlasError.InvalidQuery("atlas query coordinates must transform to three finite voxel coordinates"))
    else
      val rounded = coordinates.map(math.round)
      if rounded.exists(value => value < Int.MinValue || value > Int.MaxValue) then
        Left(AtlasError.InvalidQuery("atlas query voxel coordinates exceed Int range"))
      else Right(rounded.map(_.toInt))

  private[atlas] def pointFromFiniteCoordinates(coordinates: Vector[Double]): Either[AtlasError, Point3D] =
    if coordinates.length != 3 || !coordinates.forall(_.isFinite) then
      Left(AtlasError.InvalidQuery("atlas query voxel coordinates do not map to finite world coordinates"))
    else Right(Point3D.fromVector(coordinates))

  private def radiusBounds(atlas: VolumeAtlas, center: Vector[Int], radiusMm: Double): Either[AtlasError, (Vector[Int], Vector[Int])] =
    val spacing = atlas.space.spacing
    if spacing.length != 3 || !spacing.forall(value => value.isFinite && value > 0.0) then
      Left(AtlasError.InvalidQuery("atlas query requires three finite positive voxel spacings"))
    else
      val extents = spacing.map(value => math.ceil(radiusMm / value) + 1.0)
      if !extents.forall(_.isFinite) then
        Left(AtlasError.InvalidQuery("atlas query radius extent overflow"))
      else
        val dims = atlas.space.spatialDims
        val lower = Vector.tabulate(3)(axis =>
          val c = center(axis).toDouble
          val extent = extents(axis)
          math.max(0.0, math.min(dims(axis).toDouble, c - extent)).toInt
        )
        val upper = Vector.tabulate(3)(axis =>
          val c = center(axis).toDouble
          val last = (dims(axis) - 1).toDouble
          val extent = extents(axis)
          math.max(-1.0, math.min(last, c + extent)).toInt
        )
        var candidates = 1L
        var axis = 0
        while axis < 3 && candidates > 0 && candidates <= maxRadiusVisitedVoxels do
          val width = math.max(0L, upper(axis).toLong - lower(axis).toLong + 1L)
          if width == 0 then candidates = 0
          else if width > maxRadiusVisitedVoxels / candidates then
            candidates = maxRadiusVisitedVoxels + 1
          else candidates *= width
          axis += 1
        if candidates > maxRadiusVisitedVoxels then
          Left(AtlasError.InvalidQuery("atlas query radius traversal exceeds the voxel budget"))
        else Right((lower, upper))

  private def labelAtGrid(atlas: VolumeAtlas, grid: Vector[Int]): Option[RegionId] =
    if !inBounds(atlas, grid) then None
    else
      val lin = Indexing.gridToIndex3D(atlas.space.spatialDims, grid(0), grid(1), grid(2))
      val realization = atlas.realization
      val voxel = realization.domain.space.indexAtValidatedOrdinal(lin)
      realization.parcelAssignment(voxel).map: parcel =>
        realization.metadata(parcel).id

  private def inBounds(atlas: VolumeAtlas, grid: Vector[Int]): Boolean =
    val dims = atlas.space.spatialDims
    grid.length == 3 &&
      grid(0) >= 0 && grid(0) < dims(0) &&
      grid(1) >= 0 && grid(1) < dims(1) &&
      grid(2) >= 0 && grid(2) < dims(2)

  private def distance(a: Point3D, b: Point3D): Double =
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    math.hypot(math.hypot(dx, dy), dz)
