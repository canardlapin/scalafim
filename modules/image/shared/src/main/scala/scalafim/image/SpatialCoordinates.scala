package scalafim.image

import image4s.SomeSampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import scala.annotation.targetName

enum CoordinateError:
  case SingularAffine(reason: String)

  def message: String =
    this match
      case SingularAffine(reason) => s"affine is singular: $reason"

object SpatialCoordinates:

  def lpsToRas(point: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    Vector(-point(0), -point(1), point(2))

  def lpsToRas(point: SpatialPoint): SpatialPoint =
    SpatialPoint(-point.x, -point.y, point.z)

  def lpsToRas(point: WorldPoint): WorldPoint =
    WorldPoint(-point.x, -point.y, point.z)

  def lpsToRasPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    points.map(lpsToRas)

  def rasToLps(point: Vector[Double]): Vector[Double] =
    lpsToRas(point)

  def rasToLps(point: SpatialPoint): SpatialPoint =
    lpsToRas(point)

  def rasToLps(point: WorldPoint): WorldPoint =
    lpsToRas(point)

  def rasToLpsPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    lpsToRasPoints(points)

  def tkrasToRas(point: Vector[Double], cRas: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    validatePoint(cRas, "cRas")
    Vector.tabulate(3)(i => point(i) + cRas(i))

  def tkrasToRas(point: SpatialPoint, cRas: SpatialPoint): SpatialPoint =
    SpatialPoint(point.x + cRas.x, point.y + cRas.y, point.z + cRas.z)

  def tkrasToRas(point: WorldPoint, cRas: WorldPoint): WorldPoint =
    WorldPoint(point.x + cRas.x, point.y + cRas.y, point.z + cRas.z)

  def tkrasToRasPoints(points: Vector[Vector[Double]], cRas: Vector[Double]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    validatePoint(cRas, "cRas")
    points.map(point => tkrasToRas(point, cRas))

  def rasToTkras(point: Vector[Double], cRas: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    validatePoint(cRas, "cRas")
    Vector.tabulate(3)(i => point(i) - cRas(i))

  def rasToTkras(point: SpatialPoint, cRas: SpatialPoint): SpatialPoint =
    SpatialPoint(point.x - cRas.x, point.y - cRas.y, point.z - cRas.z)

  def rasToTkras(point: WorldPoint, cRas: WorldPoint): WorldPoint =
    WorldPoint(point.x - cRas.x, point.y - cRas.y, point.z - cRas.z)

  def rasToTkrasPoints(points: Vector[Vector[Double]], cRas: Vector[Double]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    validatePoint(cRas, "cRas")
    points.map(point => rasToTkras(point, cRas))

  def voxelToWorld(voxel: Vector[Double], affine: DMat): Vector[Double] =
    validatePoint(voxel, "voxel")
    Affine3DMorphism.requireAffine3D(affine)
    Affine.applyAffine(affine, voxel)

  def voxelToWorld(voxel: SpatialPoint, affine: DMat): SpatialPoint =
    Affine3DMorphism.requireAffine3D(affine)
    SpatialPoint.unsafeFromVector(Affine.applyAffine(affine, voxel.toVector), "world coordinate")

  def voxelToWorld(voxel: VoxelPoint, affine: Affine3D): WorldPoint =
    affine.voxelToWorld(voxel)

  def voxelsToWorld(voxels: Vector[Vector[Double]], affine: DMat): Vector[Vector[Double]] =
    validatePoints(voxels, "voxels")
    Affine3DMorphism.requireAffine3D(affine)
    voxels.map(voxel => Affine.applyAffine(affine, voxel))

  def voxelPointsToWorld(voxels: Vector[SpatialPoint], affine: DMat): Vector[SpatialPoint] =
    Affine3DMorphism.requireAffine3D(affine)
    voxels.map { voxel =>
      SpatialPoint.unsafeFromVector(Affine.applyAffine(affine, voxel.toVector), "world coordinate")
    }

  def voxelPointsToWorld(voxels: Vector[VoxelPoint], affine: Affine3D): Vector[WorldPoint] =
    affine.voxelsToWorld(voxels)

  def worldToVoxel(world: Vector[Double], affine: DMat): Either[CoordinateError, Vector[Double]] =
    validatePoint(world, "world")
    Affine3DMorphism.requireAffine3D(affine)
    DMat.invert(affine) match
      case Left(reason) => Left(CoordinateError.SingularAffine(reason))
      case Right(inverse) => Right(Affine.applyAffine(inverse, world))

  def worldToVoxel(world: SpatialPoint, affine: DMat): Either[CoordinateError, SpatialPoint] =
    Affine3DMorphism.requireAffine3D(affine)
    DMat.invert(affine) match
      case Left(reason) => Left(CoordinateError.SingularAffine(reason))
      case Right(inverse) =>
        Right(SpatialPoint.unsafeFromVector(Affine.applyAffine(inverse, world.toVector), "voxel coordinate"))

  def worldToVoxel(world: WorldPoint, affine: Affine3D): VoxelPoint =
    affine.worldToVoxel(world)

  def worldsToVoxel(worlds: Vector[Vector[Double]], affine: DMat): Either[CoordinateError, Vector[Vector[Double]]] =
    validatePoints(worlds, "worlds")
    Affine3DMorphism.requireAffine3D(affine)
    DMat.invert(affine) match
      case Left(reason) => Left(CoordinateError.SingularAffine(reason))
      case Right(inverse) => Right(worlds.map(world => Affine.applyAffine(inverse, world)))

  def worldPointsToVoxel(worlds: Vector[SpatialPoint], affine: DMat): Either[CoordinateError, Vector[SpatialPoint]] =
    Affine3DMorphism.requireAffine3D(affine)
    DMat.invert(affine) match
      case Left(reason) => Left(CoordinateError.SingularAffine(reason))
      case Right(inverse) =>
        Right(
          worlds.map { world =>
            SpatialPoint.unsafeFromVector(Affine.applyAffine(inverse, world.toVector), "voxel coordinate")
          }
        )

  def worldPointsToVoxel(worlds: Vector[WorldPoint], affine: Affine3D): Vector[VoxelPoint] =
    affine.worldsToVoxel(worlds)

  def gridCoords(grid: GridSpec): Vector[Vector[Double]] =
    grid.worldCoords

  @targetName("gridCoordsFromNeuroSpace")
  def gridCoords(space: NeuroSpace): Vector[Vector[Double]] =
    GridSpec.fromSpace(space).worldCoords

  private[image] def validatePoint(point: Vector[Double], label: String): Unit =
    require(point.length == 3, s"$label must contain exactly 3 coordinates")
    require(point.forall(_.isFinite), s"$label coordinates must be finite")

  private[image] def validatePoints(points: Vector[Vector[Double]], label: String): Unit =
    points.foreach(point => validatePoint(point, label))

/** Zero-wrapper 3D grid view over image4s' canonical sampling geometry.
  *
  * The opaque name preserves ScalaFIM's domain vocabulary without retaining a
  * parallel `(shape, affine)` value.
  */
opaque type GridSpec = SomeSampleSpace

object GridSpec:
  extension (gridSpec: GridSpec)
    /** Exact image4s grid retained by this checked D3 refinement. */
    private[image] inline def nativeGrid: Grid[Frame[D3], D3] =
      gridSpec.grid.asInstanceOf[Grid[Frame[D3], D3]]

    def shape: SpatialDims =
      SpatialDims.unsafeFromVector(gridSpec.grid.shape, "GridSpec dims")

    def affine: DMat =
      gridSpec.toNeuroSpace.trans

    def affine3D: Either[Affine3DError, Affine3D] =
      Affine3D.make(affine)

    def dims: Vector[Int] =
      gridSpec.grid.shape

    private[scalafim] inline def extentX: Int =
      gridSpec.grid.shape(0)

    private[scalafim] inline def extentY: Int =
      gridSpec.grid.shape(1)

    private[scalafim] inline def extentZ: Int =
      gridSpec.grid.shape(2)

    private[scalafim] inline def affineElement(row: Int, column: Int): Double =
      gridSpec.grid.indexToFrame.matrix(row, column)

    inline def nVoxels: Int =
      extentX * extentY * extentZ

    def voxelToWorld(voxel: Vector[Double]): Vector[Double] =
      SpatialCoordinates.voxelToWorld(voxel, affine)

    def voxelToWorld(voxel: SpatialPoint): SpatialPoint =
      SpatialCoordinates.voxelToWorld(voxel, affine)

    def voxelToWorld(voxel: VoxelPoint): Either[Affine3DError, WorldPoint] =
      affine3D.map(_.voxelToWorld(voxel))

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
    ): Either[Affine3DError, Vector[WorldPoint]] =
      affine3D.map(_.voxelsToWorld(voxels))

    def worldToVoxel(
        world: Vector[Double]
    ): Either[CoordinateError, Vector[Double]] =
      SpatialCoordinates.worldToVoxel(world, affine)

    def worldToVoxel(
        world: SpatialPoint
    ): Either[CoordinateError, SpatialPoint] =
      SpatialCoordinates.worldToVoxel(world, affine)

    def worldToVoxel(
        world: WorldPoint
    ): Either[Affine3DError, VoxelPoint] =
      affine3D.map(_.worldToVoxel(world))

    def worldsToVoxel(
        worlds: Vector[Vector[Double]]
    ): Either[CoordinateError, Vector[Vector[Double]]] =
      SpatialCoordinates.worldsToVoxel(worlds, affine)

    def worldPointsToVoxel(
        worlds: Vector[SpatialPoint]
    ): Either[CoordinateError, Vector[SpatialPoint]] =
      SpatialCoordinates.worldPointsToVoxel(worlds, affine)

    @targetName("worldTypedPointsToVoxel")
    def worldPointsToVoxel(
        worlds: Vector[WorldPoint]
    ): Either[Affine3DError, Vector[VoxelPoint]] =
      affine3D.map(_.worldsToVoxel(worlds))

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

    def typedWorldPoints: Either[Affine3DError, Vector[WorldPoint]] =
      affine3D.map { tx =>
        val gridShape = shape
        val out = Vector.newBuilder[WorldPoint]
        out.sizeHint(nVoxels)
        var x = 0
        while x < gridShape.x do
          var y = 0
          while y < gridShape.y do
            var z = 0
            while z < gridShape.z do
              out += tx.voxelToWorld(
                VoxelPoint(x.toDouble, y.toDouble, z.toDouble)
              )
              z += 1
            y += 1
          x += 1
        out.result()
      }

    def toNeuroSpace: NeuroSpace =
      NeuroSpace.fromCanonical(gridSpec)

  def apply(dims: Vector[Int], affine: DMat): GridSpec =
    fromVector(dims, affine)
      .fold(err => throw new IllegalArgumentException(err.message), grid => grid)

  def fromVector(dims: Vector[Int], affine: DMat): Either[GeometryError, GridSpec] =
    for
      shape <- SpatialDims.fromVector(dims, "GridSpec dims")
      space <- NeuroSpace
        .make(shape.toVector, trans = Some(affine))
        .left
        .map(error => GeometryError.InvalidGridGeometry(error.message))
    yield NeuroSpace.canonical(space)

  def fromSpatialDims(dims: SpatialDims, affine: DMat): GridSpec =
    fromVector(dims.toVector, affine)
      .fold(error => throw new IllegalArgumentException(error.message), grid => grid)

  def identity(dims: Vector[Int]): GridSpec =
    GridSpec(dims, DMat.eye(4))

  def identity(dims: SpatialDims): GridSpec =
    fromSpatialDims(dims, DMat.eye(4))

  def fromSpace(space: NeuroSpace): GridSpec =
    require(space.spatialDims.length == 3, "GridSpec requires 3D geometry")
    NeuroSpace.canonical(space.spatialSpace)

  def fromVolumeSpace(space: VolumeSpace): GridSpec =
    NeuroSpace.canonical(space.toNeuroSpace)
