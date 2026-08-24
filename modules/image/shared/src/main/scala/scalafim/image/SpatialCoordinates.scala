package scalafim.image

import SampleSpaces.*

import image4s.SomeSampleSpace
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import scala.annotation.targetName

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
      voxelToWorld(voxel.toVector, affine),
      "world coordinate"
    )

  def voxelToWorld(
      voxel: VoxelPoint,
      affine: Affine[D3]
  ): Either[GeometryError, WorldPoint] =
    affine(voxel.toVector)
      .map(value => WorldPoint.unsafeFromVector(value, "world point"))

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

  def gridCoords(grid: GridSpec): Vector[Vector[Double]] =
    grid.worldCoords

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

    /** Checked provider endpoint retained by this admitted D3 grid. */
    private[scalafim] inline def providerFrame: Frame[D3] =
      nativeGrid.frame

    def shape: SpatialDims =
      SpatialDims.unsafeFromVector(gridSpec.grid.shape, "GridSpec dims")

    def affine: Affine[D3] =
      nativeGrid.indexToFrame

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

    def voxelToWorld(voxel: VoxelPoint): Either[GeometryError, WorldPoint] =
      SpatialCoordinates.voxelToWorld(voxel, affine)

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

    def toSampleSpace: SomeSampleSpace =
      SampleSpaces.fromCanonical(gridSpec)

  def apply(dims: Vector[Int], affine: Affine[D3]): GridSpec =
    fromVector(dims, affine)
      .fold(err => throw new IllegalArgumentException(err.message), grid => grid)

  def fromVector(
      dims: Vector[Int],
      affine: Affine[D3]
  ): Either[SampleSpaceError, GridSpec] =
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
        .map(SampleSpaces.canonical)

  def fromSpatialDims(dims: SpatialDims, affine: Affine[D3]): GridSpec =
    fromVector(dims.toVector, affine)
      .fold(error => throw new IllegalArgumentException(error.message), grid => grid)

  def identity(dims: Vector[Int]): GridSpec =
    GridSpec(dims, Affine.identity[D3])

  def identity(dims: SpatialDims): GridSpec =
    fromSpatialDims(dims, Affine.identity[D3])

  def fromSpace(space: SomeSampleSpace): GridSpec =
    require(space.spatialDims.length == 3, "GridSpec requires 3D geometry")
    SampleSpaces.canonical(space.spatialSpace)

  def fromGrid[F <: Frame[D3]](grid: Grid[F, D3]): GridSpec =
    SampleSpace.create(grid, NonSpatialAxes.empty)
