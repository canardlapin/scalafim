package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.Continuous
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError as ImageGeometryError
import image4s.geometry.Grid
import image4s.geometry.Point
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import scala.reflect.ClassTag

enum SlicePlanError:
  case Geometry(error: ImageGeometryError)
  case Map(error: reframe4s.core.MapError)

  def message: String =
    this match
      case Geometry(error) =>
        error.message
      case Map(error) =>
        error.message

private[image] trait SliceSampleCursor[A]:
  def sample(x: Double, y: Double, z: Double): A

/** Sampling policy whose result type is tied to the voxel value type.
  *
  * Nearest-neighbour sampling works for any voxel semantics. Linear and cubic
  * interpolation are `SliceSampling[Double, Continuous]`, so they cannot accidentally be
  * requested for label or Boolean volumes.
  */
sealed trait SliceSampling[A, Sem]:
  private[image] def cursor(
      volume: SomeNeuroVolume[A, Sem],
      dims: SpatialDims
  ): SliceSampleCursor[A]

object SliceSampling:
  final case class Nearest[A, Sem](outside: A) extends SliceSampling[A, Sem]:
    private[image] def cursor(
        volume: SomeNeuroVolume[A, Sem],
        dims: SpatialDims
    ): SliceSampleCursor[A] =
      new SliceSampleCursor[A]:
        def sample(x: Double, y: Double, z: Double): A =
          VoxelSamplingKernel.nearest(volume, dims, x, y, z, outside)

  final case class Linear(outside: Double = 0.0) extends SliceSampling[Double, Continuous]:
    private[image] def cursor(
        volume: SomeScalarVolume[Double],
        dims: SpatialDims
    ): SliceSampleCursor[Double] =
      new SliceSampleCursor[Double]:
        def sample(x: Double, y: Double, z: Double): Double =
          VoxelSamplingKernel.linear(volume, dims, x, y, z, outside)

  final case class Cubic(outside: Double = 0.0) extends SliceSampling[Double, Continuous]:
    private[image] def cursor(
        volume: SomeScalarVolume[Double],
        dims: SpatialDims
    ): SliceSampleCursor[Double] =
      val workspace = new CubicWorkspace
      new SliceSampleCursor[Double]:
        def sample(x: Double, y: Double, z: Double): Double =
          VoxelSamplingKernel.cubic(volume, dims, x, y, z, outside, workspace)

/** Row-major, top-to-bottom slice values paired with their world-space grid. */
final case class SliceImage[A] private (
  grid: SliceGrid,
  values: NDArray[A, Rank[2]]
):
  require(
    values.shape(0) == grid.dimensions.height &&
      values.shape(1) == grid.dimensions.width,
    "slice shape must be (row, column)"
  )

  def dimensions: SliceDimensions =
    grid.dimensions

  inline def apply(column: Int, row: Int): A =
    require(column >= 0 && column < dimensions.width, "slice column out of bounds")
    require(row >= 0 && row < dimensions.height, "slice row out of bounds")
    values(row, column)

  inline def valueAtCanonicalOrdinal(index: Int): A =
    require(index >= 0 && index < dimensions.pixelCount, "slice ordinal out of bounds")
    values(index / dimensions.width, index % dimensions.width)

object SliceImage:
  private[image] def unsafe[A](grid: SliceGrid, values: NDArray[A, Rank[2]]): SliceImage[A] =
    new SliceImage(grid, values)

/** Reusable affine stepping plan from a finite slice grid into one source volume.
  *
  * Only the first source voxel and two voxel-space increments are retained.
  * Sampling advances primitive coordinates in the inner loop and allocates no
  * point objects per pixel.
  */
final case class SlicePlan private (
  source: Grid[? <: Frame[D3], D3],
  grid: SliceGrid,
  firstVoxel: VoxelPoint,
  private val columnStep: VoxelStep,
  private val rowStep: VoxelStep
):
  def sourceVoxelAt(pixel: PixelCoord): Either[SliceGeometryError, VoxelPoint] =
    grid.worldAt(pixel).map(worldToSourceVoxel)

  private def worldToSourceVoxel(world: WorldPoint): VoxelPoint =
    source.worldToVoxel(world).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )

  def sample[A: ClassTag, Sem](
    volume: SomeNeuroVolume[A, Sem],
    sampling: SliceSampling[A, Sem]
  ): Either[SlicePlanError, SliceImage[A]] =
    Grid
      .exactCongruence(source, volume.grid)
      .left
      .map(SlicePlanError.Geometry.apply)
      .map: _ =>
        val dimensions = grid.dimensions
        given DType[A] = volume.values.dtype
        val sampleCursor = sampling.cursor(volume, source.spatialShape)
        val out =
          NDArray.build[A, Rank[2]](
            Shape(dimensions.height, dimensions.width)
          ): output =>
            var row = 0
            var rowX = firstVoxel.x
            var rowY = firstVoxel.y
            var rowZ = firstVoxel.z
            while row < dimensions.height do
              var column = 0
              var x = rowX
              var y = rowY
              var z = rowZ
              val rowOffset = row * dimensions.width
              while column < dimensions.width do
                output.writeLinear(
                  rowOffset + column,
                  sampleCursor.sample(x, y, z)
                )
                x += columnStep.x
                y += columnStep.y
                z += columnStep.z
                column += 1
              rowX += rowStep.x
              rowY += rowStep.y
              rowZ += rowStep.z
              row += 1
        SliceImage.unsafe(grid, out)

object SlicePlan:
  def make(source: Grid[? <: Frame[D3], D3], grid: SliceGrid): SlicePlan =
    def voxelAt(world: WorldPoint): VoxelPoint =
      source.worldToVoxel(world).fold(
        error => throw new IllegalStateException(error.message),
        identity
      )

    val first = voxelAt(grid.topLeftCenter)
    val nextColumnWorld = grid.topLeftCenter + grid.plane.screenRight.scaled(grid.spacing.horizontal)
    val nextRowWorld = grid.topLeftCenter + grid.plane.screenUp.scaled(-grid.spacing.vertical)
    val nextColumn = voxelAt(nextColumnWorld)
    val nextRow = voxelAt(nextRowWorld)
    new SlicePlan(
      source,
      grid,
      first,
      VoxelStep(nextColumn.x - first.x, nextColumn.y - first.y, nextColumn.z - first.z),
      VoxelStep(nextRow.x - first.x, nextRow.y - first.y, nextRow.z - first.z)
    )

/** Reusable pullback plan for nonlinear world mappings.
  *
  * Mapping is paid once at plan construction. Execution stores and advances
  * only primitive source-voxel coordinates, just like [[SlicePlan]].
  */
final class MappedSlicePlan private (
  val source: Grid[? <: Frame[D3], D3],
  val grid: SliceGrid,
  val mappingBatchCount: Int,
  private val sourceX: Array[Double],
  private val sourceY: Array[Double],
  private val sourceZ: Array[Double]
):
  def sourceVoxelAt(pixel: PixelCoord): Either[SliceGeometryError, VoxelPoint] =
    grid.worldAt(pixel).map { _ =>
      val index = pixel.row * grid.dimensions.width + pixel.column
      VoxelPoint(sourceX(index), sourceY(index), sourceZ(index))
    }

  def sample[A: ClassTag, Sem](
    volume: SomeNeuroVolume[A, Sem],
    sampling: SliceSampling[A, Sem]
  ): Either[SlicePlanError, SliceImage[A]] =
    Grid
      .exactCongruence(source, volume.grid)
      .left
      .map(SlicePlanError.Geometry.apply)
      .map: _ =>
        given DType[A] = volume.values.dtype
        val sampleCursor = sampling.cursor(volume, source.spatialShape)
        val dimensions = grid.dimensions
        val out =
          NDArray.build[A, Rank[2]](
            Shape(dimensions.height, dimensions.width)
          ): output =>
            var index = 0
            while index < dimensions.pixelCount do
              output.writeLinear(
                index,
                sampleCursor.sample(
                  sourceX(index),
                  sourceY(index),
                  sourceZ(index)
                )
              )
              index += 1
        SliceImage.unsafe(grid, out)

object MappedSlicePlan:
  def make(
    source: Grid[? <: Frame[D3], D3],
    grid: SliceGrid,
    referenceToSource: SpatialPullback
  ): Either[SlicePlanError, MappedSlicePlan] =
    val count = grid.dimensions.pixelCount
    val width = grid.dimensions.width
    val xs = new Array[Double](count)
    val ys = new Array[Double](count)
    val zs = new Array[Double](count)
    val sourceInverse = source.indexToFrame.inverse.matrix
    val referenceFrame: Frame[D3] = referenceToSource.source
    var failure =
      reframe4s.core.SpatialMap
        .validateResultFrame(source.frame, referenceToSource.target)
        .left
        .toOption
    var row = 0
    while row < grid.dimensions.height && failure.isEmpty do
      val upDistance = -row.toDouble * grid.spacing.vertical
      var column = 0
      while column < width && failure.isEmpty do
        val rightDistance = column.toDouble * grid.spacing.horizontal
        val referenceX =
          grid.topLeftCenter.x + grid.plane.screenRight.x * rightDistance +
            grid.plane.screenUp.x * upDistance
        val referenceY =
          grid.topLeftCenter.y + grid.plane.screenRight.y * rightDistance +
            grid.plane.screenUp.y * upDistance
        val referenceZ =
          grid.topLeftCenter.z + grid.plane.screenRight.z * rightDistance +
            grid.plane.screenUp.z * upDistance
        val mapped =
          for
            raw <- Point
              .in[D3](referenceFrame)(referenceX, referenceY, referenceZ)
              .left
              .map(reframe4s.core.MapError.Geometry.apply)
            alignment <- Frame
              .alignOwners[D3, referenceFrame.type, Frame[D3]](
                referenceFrame,
                referenceFrame
              )
              .left
              .map(reframe4s.core.MapError.Geometry.apply)
            point <- alignment
              .pointToRight(raw)
              .left
              .map(reframe4s.core.MapError.Geometry.apply)
            result <- referenceToSource(point)
          yield result
        mapped match
          case Left(error) => failure = Some(error)
          case Right(point) =>
            val index = row * width + column
            xs(index) = affineCoordinate(
              sourceInverse,
              0,
              point.coordinates(0),
              point.coordinates(1),
              point.coordinates(2)
            )
            ys(index) = affineCoordinate(
              sourceInverse,
              1,
              point.coordinates(0),
              point.coordinates(1),
              point.coordinates(2)
            )
            zs(index) = affineCoordinate(
              sourceInverse,
              2,
              point.coordinates(0),
              point.coordinates(1),
              point.coordinates(2)
            )
        column += 1
      row += 1
    failure match
      case Some(error) => Left(SlicePlanError.Map(error))
      case None =>
        Right(
          new MappedSlicePlan(
            source,
            grid,
            grid.dimensions.height,
            xs,
            ys,
            zs
          )
        )

  private inline def affineCoordinate(
      matrix: DMat,
      row: Int,
      x: Double,
      y: Double,
      z: Double
  ): Double =
    var sum = matrix(row, 3)
    sum += matrix(row, 0) * x
    sum += matrix(row, 1) * y
    sum += matrix(row, 2) * z
    sum

private[image] final case class VoxelStep(x: Double, y: Double, z: Double)

private[image] final class CubicWorkspace:
  val alongY: Array[Double] = Array.ofDim[Double](4)
  val alongZ: Array[Double] = Array.ofDim[Double](4)

private[image] object VoxelSamplingKernel:
  inline def inBounds(dims: SpatialDims, x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < dims.x &&
      y >= 0 && y < dims.y &&
      z >= 0 && z < dims.z

  inline def nearest[A, Sem](
    volume: SomeNeuroVolume[A, Sem],
    dims: SpatialDims,
    x: Double,
    y: Double,
    z: Double,
    outside: A
  ): A =
    val xi = math.round(x).toInt
    val yi = math.round(y).toInt
    val zi = math.round(z).toInt
    if inBounds(dims, xi, yi, zi) then
      volume(xi, yi, zi)
    else outside

  inline def valueOrOutside(
    volume: SomeScalarVolume[Double],
    dims: SpatialDims,
    x: Int,
    y: Int,
    z: Int,
    outside: Double
  ): Double =
    if inBounds(dims, x, y, z) then
      volume(x, y, z)
    else outside

  def linear(
    volume: SomeScalarVolume[Double],
    dims: SpatialDims,
    x: Double,
    y: Double,
    z: Double,
    outside: Double
  ): Double =
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val z0 = math.floor(z).toInt
    val x1 = x0 + 1
    val y1 = y0 + 1
    val z1 = z0 + 1
    val xd = x - x0
    val yd = y - y0
    val zd = z - z0

    val c000 = valueOrOutside(volume, dims, x0, y0, z0, outside)
    val c100 = valueOrOutside(volume, dims, x1, y0, z0, outside)
    val c010 = valueOrOutside(volume, dims, x0, y1, z0, outside)
    val c110 = valueOrOutside(volume, dims, x1, y1, z0, outside)
    val c001 = valueOrOutside(volume, dims, x0, y0, z1, outside)
    val c101 = valueOrOutside(volume, dims, x1, y0, z1, outside)
    val c011 = valueOrOutside(volume, dims, x0, y1, z1, outside)
    val c111 = valueOrOutside(volume, dims, x1, y1, z1, outside)

    val c00 = c000 * (1.0 - xd) + c100 * xd
    val c10 = c010 * (1.0 - xd) + c110 * xd
    val c01 = c001 * (1.0 - xd) + c101 * xd
    val c11 = c011 * (1.0 - xd) + c111 * xd
    val c0 = c00 * (1.0 - yd) + c10 * yd
    val c1 = c01 * (1.0 - yd) + c11 * yd
    c0 * (1.0 - zd) + c1 * zd

  def cubic(
    volume: SomeScalarVolume[Double],
    dims: SpatialDims,
    x: Double,
    y: Double,
    z: Double,
    outside: Double,
    workspace: CubicWorkspace
  ): Double =
    val x1 = math.floor(x).toInt
    val y1 = math.floor(y).toInt
    val z1 = math.floor(z).toInt
    val tx = x - x1.toDouble
    val ty = y - y1.toDouble
    val tz = z - z1.toDouble

    var kk = 0
    while kk < 4 do
      val zk = z1 + kk - 1
      var jj = 0
      while jj < 4 do
        val yj = y1 + jj - 1
        workspace.alongY(jj) = catmullRom(
          valueOrOutside(volume, dims, x1 - 1, yj, zk, outside),
          valueOrOutside(volume, dims, x1, yj, zk, outside),
          valueOrOutside(volume, dims, x1 + 1, yj, zk, outside),
          valueOrOutside(volume, dims, x1 + 2, yj, zk, outside),
          tx
        )
        jj += 1
      workspace.alongZ(kk) = catmullRom(
        workspace.alongY(0),
        workspace.alongY(1),
        workspace.alongY(2),
        workspace.alongY(3),
        ty
      )
      kk += 1

    catmullRom(
      workspace.alongZ(0),
      workspace.alongZ(1),
      workspace.alongZ(2),
      workspace.alongZ(3),
      tz
    )

  private inline def catmullRom(
    p0: Double,
    p1: Double,
    p2: Double,
    p3: Double,
    t: Double
  ): Double =
    val t2 = t * t
    val t3 = t2 * t
    0.5 * (
      2.0 * p1 +
        (-p0 + p2) * t +
        (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
        (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3
    )
