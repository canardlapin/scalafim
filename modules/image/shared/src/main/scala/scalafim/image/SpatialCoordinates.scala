package scalafim.image

import SampleSpaces.*

import image4s.SomeSampleSpace
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.ContinuousIndex
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameAlignment
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Point
import scala.annotation.targetName
import scalafim.image.world.{Rebind, SpaceError}

object SpatialCoordinates:

  @deprecated("Use lpsToRas(SpatialPoint) or lpsToRas(WorldPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def lpsToRas(point: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    Vector(-point(0), -point(1), point(2))

  def lpsToRas(point: SpatialPoint): SpatialPoint =
    SpatialPoint(-point.x, -point.y, point.z)

  def lpsToRas(point: WorldPoint): WorldPoint =
    WorldPoint(-point.x, -point.y, point.z)

  @deprecated("Map lpsToRas(WorldPoint) over typed points.", "0.2.0")
  def lpsToRasPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    points.map(lpsToRas)

  @deprecated("Use rasToLps(SpatialPoint) or rasToLps(WorldPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def rasToLps(point: Vector[Double]): Vector[Double] =
    lpsToRas(point)

  def rasToLps(point: SpatialPoint): SpatialPoint =
    lpsToRas(point)

  def rasToLps(point: WorldPoint): WorldPoint =
    lpsToRas(point)

  @deprecated("Map rasToLps(WorldPoint) over typed points.", "0.2.0")
  def rasToLpsPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    lpsToRasPoints(points)

  @deprecated("Use tkrasToRas(WorldPoint, WorldPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def tkrasToRas(point: Vector[Double], cRas: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    validatePoint(cRas, "cRas")
    Vector.tabulate(3)(i => point(i) + cRas(i))

  def tkrasToRas(point: SpatialPoint, cRas: SpatialPoint): SpatialPoint =
    SpatialPoint(point.x + cRas.x, point.y + cRas.y, point.z + cRas.z)

  def tkrasToRas(point: WorldPoint, cRas: WorldPoint): WorldPoint =
    WorldPoint(point.x + cRas.x, point.y + cRas.y, point.z + cRas.z)

  @deprecated("Map tkrasToRas(WorldPoint, WorldPoint) over typed points.", "0.2.0")
  def tkrasToRasPoints(points: Vector[Vector[Double]], cRas: Vector[Double]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    validatePoint(cRas, "cRas")
    points.map(point => tkrasToRas(point, cRas))

  @deprecated("Use rasToTkras(WorldPoint, WorldPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def rasToTkras(point: Vector[Double], cRas: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    validatePoint(cRas, "cRas")
    Vector.tabulate(3)(i => point(i) - cRas(i))

  def rasToTkras(point: SpatialPoint, cRas: SpatialPoint): SpatialPoint =
    SpatialPoint(point.x - cRas.x, point.y - cRas.y, point.z - cRas.z)

  def rasToTkras(point: WorldPoint, cRas: WorldPoint): WorldPoint =
    WorldPoint(point.x - cRas.x, point.y - cRas.y, point.z - cRas.z)

  @deprecated("Map rasToTkras(WorldPoint, WorldPoint) over typed points.", "0.2.0")
  def rasToTkrasPoints(points: Vector[Vector[Double]], cRas: Vector[Double]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    validatePoint(cRas, "cRas")
    points.map(point => rasToTkras(point, cRas))

  @deprecated("Use voxelToWorld(VoxelPoint, affine), which returns Either instead of throwing, or GridSpec.pointAt for a frame-owned point.", "0.2.0")
  def voxelToWorld(
      voxel: Vector[Double],
      affine: Affine[D3]
  ): Vector[Double] =
    validatePoint(voxel, "voxel")
    affine(voxel).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

  def voxelToWorld(
      voxel: SpatialPoint,
      affine: Affine[D3]
  ): SpatialPoint =
    SpatialPoint.unsafeFromVector(
      affine(voxel.toVector).fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      ),
      "world coordinate"
    )

  def voxelToWorld(
      voxel: VoxelPoint,
      affine: Affine[D3]
  ): Either[GeometryError, WorldPoint] =
    affine(voxel.toVector)
      .map(value => WorldPoint.unsafeFromVector(value, "world point"))

  @deprecated("Use voxelPointsToWorld(Vector[VoxelPoint], affine), which returns Either instead of throwing.", "0.2.0")
  def voxelsToWorld(
      voxels: Vector[Vector[Double]],
      affine: Affine[D3]
  ): Vector[Vector[Double]] =
    validatePoints(voxels, "voxels")
    voxels.map(voxel => voxelToWorld(voxel, affine))

  def voxelPointsToWorld(
      voxels: Vector[SpatialPoint],
      affine: Affine[D3]
  ): Vector[SpatialPoint] =
    voxels.map(voxel => voxelToWorld(voxel, affine))

  def voxelPointsToWorld(
      voxels: Vector[VoxelPoint],
      affine: Affine[D3]
  ): Either[GeometryError, Vector[WorldPoint]] =
    traverse(voxels)(voxel => voxelToWorld(voxel, affine))

  @deprecated("Use worldToVoxel(WorldPoint, affine) or GridSpec.voxelAt for a frame-owned point.", "0.2.0")
  def worldToVoxel(
      world: Vector[Double],
      affine: Affine[D3]
  ): Either[GeometryError, Vector[Double]] =
    validatePoint(world, "world")
    affine.inverse(world)

  def worldToVoxel(
      world: SpatialPoint,
      affine: Affine[D3]
  ): Either[GeometryError, SpatialPoint] =
    affine.inverse(world.toVector)
      .map(value =>
        SpatialPoint.unsafeFromVector(value, "voxel coordinate")
      )

  def worldToVoxel(
      world: WorldPoint,
      affine: Affine[D3]
  ): Either[GeometryError, VoxelPoint] =
    affine.inverse(world.toVector)
      .map(value => VoxelPoint.unsafeFromVector(value, "voxel point"))

  @deprecated("Use worldPointsToVoxel(Vector[WorldPoint], affine).", "0.2.0")
  def worldsToVoxel(
      worlds: Vector[Vector[Double]],
      affine: Affine[D3]
  ): Either[GeometryError, Vector[Vector[Double]]] =
    validatePoints(worlds, "worlds")
    traverse(worlds)(world => affine.inverse(world))

  def worldPointsToVoxel(
      worlds: Vector[SpatialPoint],
      affine: Affine[D3]
  ): Either[GeometryError, Vector[SpatialPoint]] =
    traverse(worlds)(world => worldToVoxel(world, affine))

  @targetName("worldTypedPointsToVoxel")
  def worldPointsToVoxel(
      worlds: Vector[WorldPoint],
      affine: Affine[D3]
  ): Either[GeometryError, Vector[VoxelPoint]] =
    traverse(worlds)(world => worldToVoxel(world, affine))

  @deprecated("Use GridSpec.typedWorldPoints or GridSpec.worldPoints.", "0.2.0")
  def gridCoords(grid: GridSpec[?]): Vector[Vector[Double]] =
    grid.worldCoords

  @deprecated("Use GridSpec.fromSpace(space).typedWorldPoints.", "0.2.0")
  @targetName("gridCoordsFromSampleSpace")
  def gridCoords(space: SomeSampleSpace): Vector[Vector[Double]] =
    GridSpec.fromSpace(space).worldCoords

  private[image] def validatePoint(point: Vector[Double], label: String): Unit =
    require(point.length == 3, s"$label must contain exactly 3 coordinates")
    require(point.forall(_.isFinite), s"$label coordinates must be finite")

  private[image] def validatePoints(points: Vector[Vector[Double]], label: String): Unit =
    points.foreach(point => validatePoint(point, label))

  private def traverse[A, B](
      values: Vector[A]
  )(f: A => Either[GeometryError, B]): Either[GeometryError, Vector[B]] =
    values.foldLeft[Either[GeometryError, Vector[B]]](Right(Vector.empty)):
      case (acc, value) =>
        for
          built <- acc
          next <- f(value)
        yield built :+ next

/** A spatial-only D3 sampling grid whose frame owner is part of its type.
  *
  * The value is the image4s `SampleSpace[F, D3]` itself; `F` is the provider frame owner, so a grid in one frame
  * cannot be passed where a grid in another is required. Grids decoded at runtime have an unknown frame and are typed
  * `GridSpec[?]`; code that relates two such grids does so through a provider-checked alignment, never a cast.
  *
  * Equality is the provider's: two `GridSpec`s are equal when they wrap the same live sample space.
  */
final class GridSpec[F <: Frame[D3]] private (private val space: SampleSpace[F, D3]):
  /** The exact image4s grid retained by this spatial-only refinement. */
  val grid: Grid[F, D3] = space.grid

  /** The provider frame owner the grid's index-to-frame affine maps into. */
  def frame: F = grid.frame

  /** The frame as a dynamic provider endpoint, for routing code that stores heterogeneous maps. */
  private[scalafim] def providerFrame: Frame[D3] = grid.frame

  def shape: SpatialDims =
    SpatialDims.unsafeFromVector(grid.shape, "GridSpec dims")

  def affine: Affine[D3] =
    grid.indexToFrame

  def dims: Vector[Int] =
    grid.shape

  def nVoxels: Int =
    grid.shape(0) * grid.shape(1) * grid.shape(2)

  /** The world point at a continuous voxel coordinate, owned by this grid's frame. */
  def pointAt(voxel: VoxelPoint): Either[GeometryError, Point[F, D3]] =
    ContinuousIndex.fromVector[D3](voxel.toVector).flatMap(grid.pointAt)

  /** The continuous voxel coordinate of a point in this grid's frame; points of other owners are rejected. */
  def voxelAt(point: Point[F, D3]): Either[GeometryError, VoxelPoint] =
    grid.continuousIndexOf(point).map(index => VoxelPoint(index.values(0), index.values(1), index.values(2)))

  /** Claim an unowned world coordinate for this grid's frame. */
  def bind(world: WorldPoint): Either[GeometryError, Point[F, D3]] =
    GridSpec.pointIn(frame, world.toVector)

  /** The world box enclosing this grid's voxel centres, in the grid's frame. */
  def bounds: WorldBox[F] =
    val extents = grid.shape.map(extent => (extent - 1).toDouble)
    val corners =
      Vector.tabulate(8): mask =>
        pointAt(
          VoxelPoint(
            if (mask & 1) == 0 then 0.0 else extents(0),
            if (mask & 2) == 0 then 0.0 else extents(1),
            if (mask & 4) == 0 then 0.0 else extents(2)
          )
        ).fold(error => throw new IllegalStateException(error.message), identity)
    WorldBox.enclosing(corners).getOrElse(throw new IllegalStateException("a grid has eight corners"))

  @deprecated("Use voxelToWorld(VoxelPoint) or pointAt(VoxelPoint); a bare Vector[Double] carries neither arity nor role.", "0.2.0")
  def voxelToWorld(voxel: Vector[Double]): Vector[Double] =
    SpatialCoordinates.voxelToWorld(voxel, affine)

  def voxelToWorld(voxel: SpatialPoint): SpatialPoint =
    SpatialCoordinates.voxelToWorld(voxel, affine)

  def voxelToWorld(voxel: VoxelPoint): Either[GeometryError, WorldPoint] =
    SpatialCoordinates.voxelToWorld(voxel, affine)

  @deprecated("Use voxelPointsToWorld(Vector[VoxelPoint]).", "0.2.0")
  def voxelsToWorld(
      voxels: Vector[Vector[Double]]
  ): Vector[Vector[Double]] =
    SpatialCoordinates.voxelsToWorld(voxels, affine)

  def voxelPointsToWorld(
      voxels: Vector[SpatialPoint]
  ): Vector[SpatialPoint] =
    SpatialCoordinates.voxelPointsToWorld(voxels, affine)

  @targetName("voxelTypedPointsToWorld")
  def voxelPointsToWorld(
      voxels: Vector[VoxelPoint]
  ): Either[GeometryError, Vector[WorldPoint]] =
    SpatialCoordinates.voxelPointsToWorld(voxels, affine)

  @deprecated("Use worldToVoxel(WorldPoint) or voxelAt(Point[F, D3]).", "0.2.0")
  def worldToVoxel(
      world: Vector[Double]
  ): Either[GeometryError, Vector[Double]] =
    SpatialCoordinates.worldToVoxel(world, affine)

  def worldToVoxel(
      world: SpatialPoint
  ): Either[GeometryError, SpatialPoint] =
    SpatialCoordinates.worldToVoxel(world, affine)

  def worldToVoxel(
      world: WorldPoint
  ): Either[GeometryError, VoxelPoint] =
    SpatialCoordinates.worldToVoxel(world, affine)

  @deprecated("Use worldPointsToVoxel(Vector[WorldPoint]).", "0.2.0")
  def worldsToVoxel(
      worlds: Vector[Vector[Double]]
  ): Either[GeometryError, Vector[Vector[Double]]] =
    SpatialCoordinates.worldsToVoxel(worlds, affine)

  def worldPointsToVoxel(
      worlds: Vector[SpatialPoint]
  ): Either[GeometryError, Vector[SpatialPoint]] =
    SpatialCoordinates.worldPointsToVoxel(worlds, affine)

  @targetName("worldTypedPointsToVoxel")
  def worldPointsToVoxel(
      worlds: Vector[WorldPoint]
  ): Either[GeometryError, Vector[VoxelPoint]] =
    SpatialCoordinates.worldPointsToVoxel(worlds, affine)

  @deprecated("Use typedWorldPoints or worldPoints.", "0.2.0")
  def worldCoords: Vector[Vector[Double]] =
    worldPoints.map(_.toVector)

  def worldPoints: Vector[SpatialPoint] =
    val gridShape = shape
    val tx = affine
    val out = Vector.newBuilder[SpatialPoint]
    out.sizeHint(nVoxels)
    var x = 0
    while x < gridShape.x do
      var y = 0
      while y < gridShape.y do
        var z = 0
        while z < gridShape.z do
          out += SpatialCoordinates.voxelToWorld(
            SpatialPoint(x.toDouble, y.toDouble, z.toDouble),
            tx
          )
          z += 1
        y += 1
      x += 1
    out.result()

  def typedWorldPoints: Either[GeometryError, Vector[WorldPoint]] =
    val tx = affine
    val gridShape = shape
    val voxels = Vector.newBuilder[VoxelPoint]
    voxels.sizeHint(nVoxels)
    var x = 0
    while x < gridShape.x do
      var y = 0
      while y < gridShape.y do
        var z = 0
        while z < gridShape.z do
          voxels += VoxelPoint(x.toDouble, y.toDouble, z.toDouble)
          z += 1
        y += 1
      x += 1
    SpatialCoordinates.voxelPointsToWorld(voxels.result(), tx)

  /** The provider sample space, with its frame type retained. */
  def sampleSpace: SampleSpace[F, D3] =
    space

  def toSampleSpace: SomeSampleSpace =
    SampleSpaces.fromCanonical(space)

  override def equals(other: Any): Boolean =
    other match
      case that: GridSpec[?] => space.sameRuntimeSpaceAs(that.space)
      case _                 => false

  override def hashCode(): Int =
    space.hashCode()

  override def toString: String =
    s"GridSpec(dims=$dims, frame=$frame)"

object GridSpec:
  /** Retain a provider grid and its frame type. */
  def fromGrid[F <: Frame[D3]](grid: Grid[F, D3]): GridSpec[F] =
    new GridSpec(SampleSpace.create(grid, NonSpatialAxes.empty))

  /** The spatial part of a typed provider sample space. */
  def fromSampleSpace[F <: Frame[D3]](space: SampleSpace[F, D3]): GridSpec[F] =
    new GridSpec(space.spatialOnly)

  /** An ephemeral grid in exactly `frame`, so the result's type names that frame. */
  def in(frame: Frame[D3])(
      shape: SpatialDims,
      affine: Affine[D3]
  ): Either[GeometryError, GridSpec[frame.type]] =
    Grid.in[D3](frame)(shape.toVector, affine).map(grid => fromGrid(grid))

  /** Admit dynamic D3 geometry; the frame is known only at runtime. */
  def fromSpaceEither(space: SomeSampleSpace): Either[SampleSpaceError, GridSpec[?]] =
    SampleSpaces.requireSpatialD3(space).map(typed => fromSampleSpace(typed))

  def fromSpace(space: SomeSampleSpace): GridSpec[?] =
    require(space.spatialDims.length == 3, "GridSpec requires 3D geometry")
    fromSpaceEither(space).fold(error => throw new IllegalArgumentException(error.message), grid => grid)

  def apply(dims: Vector[Int], affine: Affine[D3]): GridSpec[?] =
    fromVector(dims, affine)
      .fold(err => throw new IllegalArgumentException(err.message), grid => grid)

  def fromVector(
      dims: Vector[Int],
      affine: Affine[D3]
  ): Either[SampleSpaceError, GridSpec[?]] =
    if dims.length != 3 then
      Left(
        SampleSpaceError.ExpectedDimensionality(
          "GridSpec dims",
          3,
          dims.length
        )
      )
    else
      SampleSpaces
        .make(dims, affine = Some(affine))
        .flatMap(fromSpaceEither)

  def fromSpatialDims(dims: SpatialDims, affine: Affine[D3]): GridSpec[?] =
    fromVector(dims.toVector, affine)
      .fold(error => throw new IllegalArgumentException(error.message), grid => grid)

  def identity(dims: Vector[Int]): GridSpec[?] =
    GridSpec(dims, Affine.identity[D3])

  def identity(dims: SpatialDims): GridSpec[?] =
    fromSpatialDims(dims, Affine.identity[D3])

  /** A point in exactly the static frame `F`, reached through a provider alignment of `frame` with itself. */
  private[image] def pointIn[F <: Frame[D3]](
      frame: F,
      coordinates: Vector[Double]
  ): Either[GeometryError, Point[F, D3]] =
    for
      raw <- Point.fromVector[D3](frame, coordinates)
      self <- Frame.alignOwners[D3, frame.type, F](frame, frame)
      owned <- self.pointToRight(raw)
    yield owned

  /** Move a grid across a checked frame alignment, keeping its shape, affine and persistent grid id. */
  given rebind: Rebind[GridSpec] with
    def toRight[A <: Frame[D3], B <: Frame[D3]](
        value: GridSpec[A],
        alignment: FrameAlignment[D3, A, B]
    ): Either[SpaceError, GridSpec[B]] =
      if !value.frame.sameRuntimeOwnerAs(alignment.left) then
        Left(SpaceError.FrameBinding("grid frame is not the left frame of the alignment"))
      else
        val shape = value.grid.shape
        val affine = value.grid.indexToFrame
        val rebound: Either[GeometryError, Grid[B, D3]] =
          value.grid.persistentId match
            case Some(id) if alignment.right.persistentKey.nonEmpty =>
              Grid.createPersistent[D3, B](id, alignment.right)(shape, affine)
            case _ =>
              Grid.forFrame[D3, B](alignment.right)(shape, affine)
        rebound
          .left
          .map(error => SpaceError.FrameBinding(error.message))
          .map(grid => fromGrid(grid))
