package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

enum SlicePlanError:
  case SourceSpaceMismatch(expected: VolumeSpace, actual: VolumeSpace)

  def message: String =
    this match
      case SourceSpaceMismatch(expected, actual) =>
        s"slice plan source space does not match volume: expected ${expected.dims}, got ${actual.dims}"

private[image] trait SliceSampleCursor[A]:
  def sample(x: Double, y: Double, z: Double): A

/** Sampling policy whose result type is tied to the voxel value type.
  *
  * Nearest-neighbour sampling works for any voxel type. Linear and cubic
  * interpolation are `SliceSampling[Double]`, so they cannot accidentally be
  * requested for label or Boolean volumes.
  */
sealed trait SliceSampling[A]:
  private[image] def cursor(volume: NeuroVol[A], dims: SpatialDims): SliceSampleCursor[A]

object SliceSampling:
  final case class Nearest[A](outside: A) extends SliceSampling[A]:
    private[image] def cursor(volume: NeuroVol[A], dims: SpatialDims): SliceSampleCursor[A] =
      new SliceSampleCursor[A]:
        def sample(x: Double, y: Double, z: Double): A =
          VoxelSamplingKernel.nearest(volume, dims, x, y, z, outside)

  final case class Linear(outside: Double = 0.0) extends SliceSampling[Double]:
    private[image] def cursor(volume: NeuroVol[Double], dims: SpatialDims): SliceSampleCursor[Double] =
      new SliceSampleCursor[Double]:
        def sample(x: Double, y: Double, z: Double): Double =
          VoxelSamplingKernel.linear(volume, dims, x, y, z, outside)

  final case class Cubic(outside: Double = 0.0) extends SliceSampling[Double]:
    private[image] def cursor(volume: NeuroVol[Double], dims: SpatialDims): SliceSampleCursor[Double] =
      val workspace = new CubicWorkspace
      new SliceSampleCursor[Double]:
        def sample(x: Double, y: Double, z: Double): Double =
          VoxelSamplingKernel.cubic(volume, dims, x, y, z, outside, workspace)

/** Row-major, top-to-bottom slice values paired with their world-space grid. */
final case class SliceImage[A] private (
  grid: SliceGrid,
  values: NArray[A]
):
  require(values.length == grid.dimensions.pixelCount, "slice value count must match grid dimensions")

  def dimensions: SliceDimensions =
    grid.dimensions

  inline def apply(column: Int, row: Int): A =
    require(column >= 0 && column < dimensions.width, "slice column out of bounds")
    require(row >= 0 && row < dimensions.height, "slice row out of bounds")
    values(row * dimensions.width + column)

object SliceImage:
  private[image] def unsafe[A](grid: SliceGrid, values: NArray[A]): SliceImage[A] =
    new SliceImage(grid, values)

/** Reusable affine stepping plan from a finite slice grid into one source volume.
  *
  * Only the first source voxel and two voxel-space increments are retained.
  * Sampling advances primitive coordinates in the inner loop and allocates no
  * point objects per pixel.
  */
final case class SlicePlan private (
  source: VolumeSpace,
  grid: SliceGrid,
  firstVoxel: VoxelPoint,
  private val columnStep: VoxelStep,
  private val rowStep: VoxelStep
):
  def sourceVoxelAt(pixel: PixelCoord): Either[SliceGeometryError, VoxelPoint] =
    grid.worldAt(pixel).map(source.worldToVoxel)

  def sample[A: ClassTag](
    volume: NeuroVol[A],
    sampling: SliceSampling[A]
  ): Either[SlicePlanError, SliceImage[A]] =
    if volume.volumeSpace != source then
      Left(SlicePlanError.SourceSpaceMismatch(source, volume.volumeSpace))
    else
      val dimensions = grid.dimensions
      val out = NArrayUtil.ofSize[A](dimensions.pixelCount)
      val sampleCursor = sampling.cursor(volume, source.shape)
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
          out(rowOffset + column) = sampleCursor.sample(x, y, z)
          x += columnStep.x
          y += columnStep.y
          z += columnStep.z
          column += 1
        rowX += rowStep.x
        rowY += rowStep.y
        rowZ += rowStep.z
        row += 1
      Right(SliceImage.unsafe(grid, out))

object SlicePlan:
  def make(source: VolumeSpace, grid: SliceGrid): SlicePlan =
    val first = source.worldToVoxel(grid.topLeftCenter)
    val nextColumnWorld = grid.topLeftCenter + grid.plane.screenRight.scaled(grid.spacing.horizontal)
    val nextRowWorld = grid.topLeftCenter + grid.plane.screenUp.scaled(-grid.spacing.vertical)
    val nextColumn = source.worldToVoxel(nextColumnWorld)
    val nextRow = source.worldToVoxel(nextRowWorld)
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
  val source: VolumeSpace,
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

  def sample[A: ClassTag](
    volume: NeuroVol[A],
    sampling: SliceSampling[A]
  ): Either[SlicePlanError, SliceImage[A]] =
    if volume.volumeSpace != source then
      Left(SlicePlanError.SourceSpaceMismatch(source, volume.volumeSpace))
    else
      val out = NArrayUtil.ofSize[A](grid.dimensions.pixelCount)
      val sampleCursor = sampling.cursor(volume, source.shape)
      var index = 0
      while index < out.length do
        out(index) = sampleCursor.sample(sourceX(index), sourceY(index), sourceZ(index))
        index += 1
      Right(SliceImage.unsafe(grid, out))

object MappedSlicePlan:
  def make(
    source: VolumeSpace,
    grid: SliceGrid,
    referenceToSource: SpatialMorphism
  ): MappedSlicePlan =
    val count = grid.dimensions.pixelCount
    val width = grid.dimensions.width
    val xs = new Array[Double](count)
    val ys = new Array[Double](count)
    val zs = new Array[Double](count)
    val referenceX = new Array[Double](width)
    val referenceY = new Array[Double](width)
    val referenceZ = new Array[Double](width)
    val mappedX = new Array[Double](width)
    val mappedY = new Array[Double](width)
    val mappedZ = new Array[Double](width)
    val sourceInverse = source.affine.inverse
    var row = 0
    while row < grid.dimensions.height do
      val upDistance = -row.toDouble * grid.spacing.vertical
      var column = 0
      while column < width do
        val rightDistance = column.toDouble * grid.spacing.horizontal
        referenceX(column) =
          grid.topLeftCenter.x + grid.plane.screenRight.x * rightDistance +
            grid.plane.screenUp.x * upDistance
        referenceY(column) =
          grid.topLeftCenter.y + grid.plane.screenRight.y * rightDistance +
            grid.plane.screenUp.y * upDistance
        referenceZ(column) =
          grid.topLeftCenter.z + grid.plane.screenRight.z * rightDistance +
            grid.plane.screenUp.z * upDistance
        column += 1
      referenceToSource.transformWorldCoordinatesInto(
        referenceX,
        referenceY,
        referenceZ,
        mappedX,
        mappedY,
        mappedZ
      )
      val rowOffset = row * width
      column = 0
      while column < width do
        val index = rowOffset + column
        xs(index) = affineCoordinate(sourceInverse, 0, mappedX(column), mappedY(column), mappedZ(column))
        ys(index) = affineCoordinate(sourceInverse, 1, mappedX(column), mappedY(column), mappedZ(column))
        zs(index) = affineCoordinate(sourceInverse, 2, mappedX(column), mappedY(column), mappedZ(column))
        column += 1
      row += 1
    new MappedSlicePlan(source, grid, grid.dimensions.height, xs, ys, zs)

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

  inline def nearest[A](
    volume: NeuroVol[A],
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
      volume.linear(xi + yi * dims.x + zi * dims.x * dims.y)
    else outside

  inline def valueOrOutside(
    volume: NeuroVol[Double],
    dims: SpatialDims,
    x: Int,
    y: Int,
    z: Int,
    outside: Double
  ): Double =
    if inBounds(dims, x, y, z) then
      volume.linear(x + y * dims.x + z * dims.x * dims.y)
    else outside

  def linear(
    volume: NeuroVol[Double],
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
    volume: NeuroVol[Double],
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
