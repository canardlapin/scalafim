package scalafim.image

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

  def lpsToRasPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    validatePoints(points, "points")
    points.map(lpsToRas)

  def rasToLps(point: Vector[Double]): Vector[Double] =
    lpsToRas(point)

  def rasToLps(point: SpatialPoint): SpatialPoint =
    lpsToRas(point)

  def rasToLpsPoints(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    lpsToRasPoints(points)

  def tkrasToRas(point: Vector[Double], cRas: Vector[Double]): Vector[Double] =
    validatePoint(point, "point")
    validatePoint(cRas, "cRas")
    Vector.tabulate(3)(i => point(i) + cRas(i))

  def tkrasToRas(point: SpatialPoint, cRas: SpatialPoint): SpatialPoint =
    SpatialPoint(point.x + cRas.x, point.y + cRas.y, point.z + cRas.z)

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

  def voxelsToWorld(voxels: Vector[Vector[Double]], affine: DMat): Vector[Vector[Double]] =
    validatePoints(voxels, "voxels")
    Affine3DMorphism.requireAffine3D(affine)
    voxels.map(voxel => Affine.applyAffine(affine, voxel))

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

  def worldsToVoxel(worlds: Vector[Vector[Double]], affine: DMat): Either[CoordinateError, Vector[Vector[Double]]] =
    validatePoints(worlds, "worlds")
    Affine3DMorphism.requireAffine3D(affine)
    DMat.invert(affine) match
      case Left(reason) => Left(CoordinateError.SingularAffine(reason))
      case Right(inverse) => Right(worlds.map(world => Affine.applyAffine(inverse, world)))

  def gridCoords(grid: GridSpec): Vector[Vector[Double]] =
    grid.worldCoords

  def gridCoords(space: NeuroSpace): Vector[Vector[Double]] =
    GridSpec.fromSpace(space).worldCoords

  private[image] def validatePoint(point: Vector[Double], label: String): Unit =
    require(point.length == 3, s"$label must contain exactly 3 coordinates")
    require(point.forall(_.isFinite), s"$label coordinates must be finite")

  private[image] def validatePoints(points: Vector[Vector[Double]], label: String): Unit =
    points.foreach(point => validatePoint(point, label))

final case class GridSpec private (shape: SpatialDims, affine: DMat):
  Affine3DMorphism.requireAffine3D(affine)

  def dims: Vector[Int] =
    shape.toVector

  def nVoxels: Int =
    shape.product

  def voxelToWorld(voxel: Vector[Double]): Vector[Double] =
    SpatialCoordinates.voxelToWorld(voxel, affine)

  def voxelToWorld(voxel: SpatialPoint): SpatialPoint =
    SpatialCoordinates.voxelToWorld(voxel, affine)

  def voxelsToWorld(voxels: Vector[Vector[Double]]): Vector[Vector[Double]] =
    SpatialCoordinates.voxelsToWorld(voxels, affine)

  def worldToVoxel(world: Vector[Double]): Either[CoordinateError, Vector[Double]] =
    SpatialCoordinates.worldToVoxel(world, affine)

  def worldToVoxel(world: SpatialPoint): Either[CoordinateError, SpatialPoint] =
    SpatialCoordinates.worldToVoxel(world, affine)

  def worldsToVoxel(worlds: Vector[Vector[Double]]): Either[CoordinateError, Vector[Vector[Double]]] =
    SpatialCoordinates.worldsToVoxel(worlds, affine)

  def worldCoords: Vector[Vector[Double]] =
    val out = Vector.newBuilder[Vector[Double]]
    out.sizeHint(nVoxels)
    var z = 0
    while z < shape.z do
      var y = 0
      while y < shape.y do
        var x = 0
        while x < shape.x do
          out += voxelToWorld(Vector(x.toDouble, y.toDouble, z.toDouble))
          x += 1
        y += 1
      z += 1
    out.result()

  def worldPoints: Vector[SpatialPoint] =
    worldCoords.map(coord => SpatialPoint.unsafeFromVector(coord, "world coordinate"))

  def toNeuroSpace: NeuroSpace =
    val spacing = Affine.voxelSizes(affine)
    val origin = Vector.tabulate(3)(axis => affine(axis, 3))
    NeuroSpace(
      dims = dims,
      spacing = Some(spacing),
      origin = Some(origin),
      trans = Some(affine)
    )

object GridSpec:
  def apply(dims: Vector[Int], affine: DMat): GridSpec =
    fromVector(dims, affine).fold(err => throw new IllegalArgumentException(err.message), grid => grid)

  def fromVector(dims: Vector[Int], affine: DMat): Either[GeometryError, GridSpec] =
    SpatialDims.fromVector(dims, "GridSpec dims").map(shape => new GridSpec(shape, affine))

  def fromSpatialDims(dims: SpatialDims, affine: DMat): GridSpec =
    new GridSpec(dims, affine)

  def identity(dims: Vector[Int]): GridSpec =
    GridSpec(dims, DMat.eye(4))

  def identity(dims: SpatialDims): GridSpec =
    fromSpatialDims(dims, DMat.eye(4))

  def fromSpace(space: NeuroSpace): GridSpec =
    fromSpatialDims(space.spatialShape, space.trans)
