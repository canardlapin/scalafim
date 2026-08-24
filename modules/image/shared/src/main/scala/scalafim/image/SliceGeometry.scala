package scalafim.image

import SampleSpaces.*

import scalafim.image.NeuroAffineSyntax.*

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid

enum SliceGeometryError:
  case NonFiniteVector(x: Double, y: Double, z: Double)
  case ZeroDirection
  case NonOrthogonalAxes(dotProduct: Double)
  case InvalidPixelDimensions(width: Int, height: Int)
  case InvalidPixelSpacing(horizontal: Double, vertical: Double)
  case NegativePixel(column: Int, row: Int)
  case NonFinitePixel(column: Double, row: Double)
  case PixelOutOfBounds(column: Int, row: Int, dimensions: SliceDimensions)

  def message: String =
    this match
      case NonFiniteVector(x, y, z) =>
        s"world vector components must be finite; got ($x, $y, $z)"
      case ZeroDirection =>
        "a world direction must have non-zero length"
      case NonOrthogonalAxes(dotProduct) =>
        s"slice screen axes must be orthogonal; dot product was $dotProduct"
      case InvalidPixelDimensions(width, height) =>
        s"slice dimensions must be positive; got ${width}x$height"
      case InvalidPixelSpacing(horizontal, vertical) =>
        s"pixel spacing must be finite and positive; got ($horizontal, $vertical)"
      case NegativePixel(column, row) =>
        s"pixel coordinates must be non-negative; got ($column, $row)"
      case NonFinitePixel(column, row) =>
        s"pixel coordinates must be finite; got ($column, $row)"
      case PixelOutOfBounds(column, row, dimensions) =>
        s"pixel ($column, $row) is outside ${dimensions.width}x${dimensions.height}"

/** A finite displacement in canonical NIfTI RAS+ world coordinates. */
final case class WorldVector private (x: Double, y: Double, z: Double):
  def +(other: WorldVector): WorldVector =
    WorldVector.unsafe(x + other.x, y + other.y, z + other.z)

  def -(other: WorldVector): WorldVector =
    WorldVector.unsafe(x - other.x, y - other.y, z - other.z)

  def unary_- : WorldVector =
    WorldVector.unsafe(-x, -y, -z)

  def *(scale: Double): WorldVector =
    WorldVector.unsafe(x * scale, y * scale, z * scale)

  def /(scale: Double): WorldVector =
    WorldVector.unsafe(x / scale, y / scale, z / scale)

  def dot(other: WorldVector): Double =
    x * other.x + y * other.y + z * other.z

  def cross(other: WorldVector): WorldVector =
    WorldVector.unsafe(
      y * other.z - z * other.y,
      z * other.x - x * other.z,
      x * other.y - y * other.x
    )

  def squaredLength: Double =
    dot(this)

  def length: Double =
    math.sqrt(squaredLength)

  def unit: Either[SliceGeometryError, UnitWorldVector] =
    UnitWorldVector.from(this)

  def toVector: Vector[Double] =
    Vector(x, y, z)

object WorldVector:
  val Zero: WorldVector =
    unsafe(0.0, 0.0, 0.0)

  def make(x: Double, y: Double, z: Double): Either[SliceGeometryError, WorldVector] =
    if x.isFinite && y.isFinite && z.isFinite then Right(new WorldVector(x, y, z))
    else Left(SliceGeometryError.NonFiniteVector(x, y, z))

  def between(from: WorldPoint, to: WorldPoint): WorldVector =
    unsafe(to.x - from.x, to.y - from.y, to.z - from.z)

  def unsafe(x: Double, y: Double, z: Double): WorldVector =
    make(x, y, z).fold(err => throw new IllegalArgumentException(err.message), identity)

/** A normalized direction in world space. Construction always normalizes. */
final case class UnitWorldVector private (vector: WorldVector):
  def x: Double = vector.x
  def y: Double = vector.y
  def z: Double = vector.z

  def dot(other: UnitWorldVector): Double =
    vector.dot(other.vector)

  def cross(other: UnitWorldVector): WorldVector =
    vector.cross(other.vector)

  def scaled(length: Double): WorldVector =
    vector * length

  def opposite: UnitWorldVector =
    UnitWorldVector.unsafe(-vector)

object UnitWorldVector:
  def make(x: Double, y: Double, z: Double): Either[SliceGeometryError, UnitWorldVector] =
    WorldVector.make(x, y, z).flatMap(from)

  def from(vector: WorldVector): Either[SliceGeometryError, UnitWorldVector] =
    val length = vector.length
    if length == 0.0 then Left(SliceGeometryError.ZeroDirection)
    else if !length.isFinite then Left(SliceGeometryError.NonFiniteVector(vector.x, vector.y, vector.z))
    else Right(new UnitWorldVector(vector / length))

  def unsafe(x: Double, y: Double, z: Double): UnitWorldVector =
    make(x, y, z).fold(err => throw new IllegalArgumentException(err.message), identity)

  private[image] def unsafe(vector: WorldVector): UnitWorldVector =
    from(vector).fold(err => throw new IllegalArgumentException(err.message), identity)

/** Named RAS+ directions: +x right, +y anterior, +z superior. */
enum AnatomicalDirection:
  case Left, Right, Posterior, Anterior, Inferior, Superior

  def unit: UnitWorldVector =
    this match
      case Left => UnitWorldVector.unsafe(-1.0, 0.0, 0.0)
      case Right => UnitWorldVector.unsafe(1.0, 0.0, 0.0)
      case Posterior => UnitWorldVector.unsafe(0.0, -1.0, 0.0)
      case Anterior => UnitWorldVector.unsafe(0.0, 1.0, 0.0)
      case Inferior => UnitWorldVector.unsafe(0.0, 0.0, -1.0)
      case Superior => UnitWorldVector.unsafe(0.0, 0.0, 1.0)

  def opposite: AnatomicalDirection =
    this match
      case Left => Right
      case Right => Left
      case Posterior => Anterior
      case Anterior => Posterior
      case Inferior => Superior
      case Superior => Inferior

enum AnatomicalPlane:
  case Sagittal, Coronal, Axial

  /** Increasing anatomical direction, independent of screen mirroring. */
  def positiveNormal: AnatomicalDirection =
    this match
      case Sagittal => AnatomicalDirection.Right
      case Coronal => AnatomicalDirection.Anterior
      case Axial => AnatomicalDirection.Superior

/** Explicit names avoid the historically ambiguous radiological/neurological labels. */
enum LeftRightConvention:
  case PatientLeftOnLeft, PatientRightOnLeft

final case class SlicePlane private (
  anatomicalPlane: AnatomicalPlane,
  through: WorldPoint,
  screenRight: UnitWorldVector,
  screenUp: UnitWorldVector,
  normal: UnitWorldVector
):
  def movedTo(point: WorldPoint): SlicePlane =
    new SlicePlane(anatomicalPlane, point, screenRight, screenUp, normal)

  def signedDistance(point: WorldPoint): Double =
    (point - through).dot(normal.vector)

object SlicePlane:
  private val OrthogonalTolerance = 1e-10

  def make(
    anatomicalPlane: AnatomicalPlane,
    through: WorldPoint,
    screenRight: UnitWorldVector,
    screenUp: UnitWorldVector
  ): Either[SliceGeometryError, SlicePlane] =
    val dot = screenRight.dot(screenUp)
    if math.abs(dot) > OrthogonalTolerance then Left(SliceGeometryError.NonOrthogonalAxes(dot))
    else
      screenRight.cross(screenUp).unit.map { normal =>
        new SlicePlane(anatomicalPlane, through, screenRight, screenUp, normal)
      }

  def canonical(
    anatomicalPlane: AnatomicalPlane,
    through: WorldPoint,
    convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft
  ): SlicePlane =
    val leftToRight =
      convention match
        case LeftRightConvention.PatientLeftOnLeft => AnatomicalDirection.Right.unit
        case LeftRightConvention.PatientRightOnLeft => AnatomicalDirection.Left.unit
    val (screenRight, screenUp) =
      anatomicalPlane match
        case AnatomicalPlane.Axial => leftToRight -> AnatomicalDirection.Anterior.unit
        case AnatomicalPlane.Coronal => leftToRight -> AnatomicalDirection.Superior.unit
        case AnatomicalPlane.Sagittal => AnatomicalDirection.Posterior.unit -> AnatomicalDirection.Superior.unit
    make(anatomicalPlane, through, screenRight, screenUp)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

final case class SliceDimensions private (width: Int, height: Int):
  def pixelCount: Int =
    width * height

object SliceDimensions:
  def make(width: Int, height: Int): Either[SliceGeometryError, SliceDimensions] =
    if width > 0 && height > 0 then Right(new SliceDimensions(width, height))
    else Left(SliceGeometryError.InvalidPixelDimensions(width, height))

  def apply(width: Int, height: Int): SliceDimensions =
    make(width, height).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class PixelSpacing private (horizontal: Double, vertical: Double)

object PixelSpacing:
  def make(horizontal: Double, vertical: Double): Either[SliceGeometryError, PixelSpacing] =
    if horizontal.isFinite && vertical.isFinite && horizontal > 0.0 && vertical > 0.0 then
      Right(new PixelSpacing(horizontal, vertical))
    else Left(SliceGeometryError.InvalidPixelSpacing(horizontal, vertical))

  def isotropic(size: Double): Either[SliceGeometryError, PixelSpacing] =
    make(size, size)

  def apply(horizontal: Double, vertical: Double): PixelSpacing =
    make(horizontal, vertical).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class PixelCoord private (column: Int, row: Int)

object PixelCoord:
  def make(column: Int, row: Int): Either[SliceGeometryError, PixelCoord] =
    if column >= 0 && row >= 0 then Right(new PixelCoord(column, row))
    else Left(SliceGeometryError.NegativePixel(column, row))

  def apply(column: Int, row: Int): PixelCoord =
    make(column, row).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class ContinuousPixel private (column: Double, row: Double)

object ContinuousPixel:
  def make(column: Double, row: Double): Either[SliceGeometryError, ContinuousPixel] =
    if column.isFinite && row.isFinite then Right(new ContinuousPixel(column, row))
    else Left(SliceGeometryError.NonFinitePixel(column, row))

  def apply(column: Double, row: Double): ContinuousPixel =
    make(column, row).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class SliceProjection(pixel: ContinuousPixel, signedDistance: Double)

/** A finite pixel lattice embedded in a world-space plane.
  *
  * Rows are stored top-to-bottom. `topLeftCenter` is the center of pixel (0, 0),
  * so row increments move opposite `screenUp`.
  */
final case class SliceGrid private (
  plane: SlicePlane,
  dimensions: SliceDimensions,
  spacing: PixelSpacing,
  topLeftCenter: WorldPoint
):
  def worldAt(pixel: PixelCoord): Either[SliceGeometryError, WorldPoint] =
    if pixel.column >= dimensions.width || pixel.row >= dimensions.height then
      Left(SliceGeometryError.PixelOutOfBounds(pixel.column, pixel.row, dimensions))
    else Right(unsafeWorldAt(pixel.column, pixel.row))

  def project(point: WorldPoint): SliceProjection =
    val offset = point - topLeftCenter
    val column = offset.dot(plane.screenRight.vector) / spacing.horizontal
    val row = -offset.dot(plane.screenUp.vector) / spacing.vertical
    SliceProjection(ContinuousPixel(column, row), plane.signedDistance(point))

  def containsProjection(point: WorldPoint, tolerance: Double = 1e-10): Boolean =
    val projected = project(point)
    val pixel = projected.pixel
    math.abs(projected.signedDistance) <= tolerance &&
      pixel.column >= -0.5 - tolerance &&
      pixel.column <= dimensions.width - 0.5 + tolerance &&
      pixel.row >= -0.5 - tolerance &&
      pixel.row <= dimensions.height - 0.5 + tolerance

  private[image] def unsafeWorldAt(column: Int, row: Int): WorldPoint =
    topLeftCenter +
      plane.screenRight.scaled(column.toDouble * spacing.horizontal) +
      plane.screenUp.scaled(-row.toDouble * spacing.vertical)

object SliceGrid:
  def covering(
    space: Grid[? <: Frame[D3], D3],
    plane: SlicePlane,
    spacing: PixelSpacing
  ): SliceGrid =
    val corners = boundaryCorners(space)
    var minHorizontal = Double.PositiveInfinity
    var maxHorizontal = Double.NegativeInfinity
    var minVertical = Double.PositiveInfinity
    var maxVertical = Double.NegativeInfinity
    var i = 0
    while i < corners.length do
      val offset = corners(i) - plane.through
      val horizontal = offset.dot(plane.screenRight.vector)
      val vertical = offset.dot(plane.screenUp.vector)
      minHorizontal = math.min(minHorizontal, horizontal)
      maxHorizontal = math.max(maxHorizontal, horizontal)
      minVertical = math.min(minVertical, vertical)
      maxVertical = math.max(maxVertical, vertical)
      i += 1

    val width = math.max(1, math.ceil((maxHorizontal - minHorizontal) / spacing.horizontal).toInt)
    val height = math.max(1, math.ceil((maxVertical - minVertical) / spacing.vertical).toInt)
    val topLeft = plane.through +
      plane.screenRight.scaled(minHorizontal + 0.5 * spacing.horizontal) +
      plane.screenUp.scaled(maxVertical - 0.5 * spacing.vertical)
    new SliceGrid(plane, SliceDimensions(width, height), spacing, topLeft)

  def native(space: Grid[? <: Frame[D3], D3], plane: SlicePlane): SliceGrid =
    val size = space.indexToFrame.neuroVoxelSizes.min
    covering(space, plane, PixelSpacing(size, size))

  private[image] def boundaryCorners(space: Grid[? <: Frame[D3], D3]): Vector[WorldPoint] =
    val shape = space.spatialShape
    val xs = Vector(-0.5, shape.x.toDouble - 0.5)
    val ys = Vector(-0.5, shape.y.toDouble - 0.5)
    val zs = Vector(-0.5, shape.z.toDouble - 0.5)
    for
      z <- zs
      y <- ys
      x <- xs
    yield space
      .voxelToWorld(VoxelPoint(x, y, z))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class OrthogonalSliceGrids private (
  cursor: WorldPoint,
  sagittal: SliceGrid,
  coronal: SliceGrid,
  axial: SliceGrid
):
  def apply(plane: AnatomicalPlane): SliceGrid =
    plane match
      case AnatomicalPlane.Sagittal => sagittal
      case AnatomicalPlane.Coronal => coronal
      case AnatomicalPlane.Axial => axial

  def all: Vector[SliceGrid] =
    Vector(sagittal, coronal, axial)

object OrthogonalSliceGrids:
  def covering(
    space: Grid[? <: Frame[D3], D3],
    cursor: WorldPoint,
    spacing: PixelSpacing,
    convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft
  ): OrthogonalSliceGrids =
    def grid(anatomicalPlane: AnatomicalPlane): SliceGrid =
      SliceGrid.covering(space, SlicePlane.canonical(anatomicalPlane, cursor, convention), spacing)

    new OrthogonalSliceGrids(
      cursor,
      grid(AnatomicalPlane.Sagittal),
      grid(AnatomicalPlane.Coronal),
      grid(AnatomicalPlane.Axial)
    )

  def native(
    space: Grid[? <: Frame[D3], D3],
    cursor: WorldPoint,
    convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft
  ): OrthogonalSliceGrids =
    val size = space.indexToFrame.neuroVoxelSizes.min
    covering(space, cursor, PixelSpacing(size, size), convention)
