package scalafim.image

import ravel.Array1
import ravel.DType
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scala.annotation.targetName

enum VoxelRoiError:
  case InvalidCoordinate(position: Int, error: GeometryError)
  case InvalidIndexSet(error: VoxelIndexSetError)
  case InvalidSpace(error: NeuroSpaceError)

  def message: String =
    this match
      case InvalidCoordinate(position, error) =>
        s"ROI coordinate at position $position is invalid: ${error.message}"
      case InvalidIndexSet(error) =>
        error.message
      case InvalidSpace(error) =>
        error.message

enum ROIVolError:
  case InvalidRoi(error: VoxelRoiError)
  case InvalidSpace(error: NeuroSpaceError)
  case Grid(error: GridMismatch)
  case DataLengthMismatch(expected: Int, actual: Int)

  def message: String =
    this match
      case InvalidRoi(error) =>
        error.message
      case InvalidSpace(error) =>
        error.message
      case Grid(error) =>
        error.message
      case DataLengthMismatch(expected, actual) =>
        s"ROI data length mismatch: expected $expected, got $actual"

/** Coordinate transport that must be bound to an explicit grid before typed use. */
final case class ROICoords(coords: Vector[Vector[Int]]):
  require(coords.forall(_.length == 3), "coords must be Nx3")
  def size: Int = coords.length

  def asRegionIn(space: VolumeSpace): Either[VoxelRoiError, VoxelRegion] =
    VoxelRegion.fromROICoords(space, this)

  def asSelectionIn(space: VolumeSpace): Either[VoxelRoiError, VoxelSelection] =
    VoxelSelection.fromROICoords(space, this)

  def linearIndices(space: NeuroSpace): Array1[Int] =
    NDArray.tabulate[Int](coords.length): i =>
      Indexing.gridToIndex3D(
        space.spatialDims,
        coords(i)(0),
        coords(i)(1),
        coords(i)(2)
      )

final class VoxelRoi private (
    val space: VolumeSpace,
    private val values: Vector[VoxelCoord],
    private val indexSet: VoxelIndexSet
):
  def size: Int =
    values.length

  def coords: Vector[VoxelCoord] =
    values

  def rawCoords: Vector[Vector[Int]] =
    values.map(_.toVector)

  def linearIndices: Array1[Int] =
    indexSet.indices

  def linearIndexSet: VoxelIndexSet =
    indexSet

  def toROICoords: ROICoords =
    ROICoords(rawCoords)

  def toRegion: VoxelRegion =
    VoxelRegion.fromRoi(this)

  def toSelection: VoxelSelection =
    VoxelSelection.fromRoi(this)

  override def equals(other: Any): Boolean =
    other match
      case that: VoxelRoi =>
        this.space == that.space && this.values == that.values
      case _ => false

  override def hashCode(): Int =
    31 * space.hashCode() + values.hashCode()

  override def toString: String =
    s"VoxelRoi(size=$size, dims=${space.dims})"

object VoxelRoi:
  def make(space: VolumeSpace, coords: Vector[VoxelCoord]): Either[VoxelRoiError, VoxelRoi] =
    val linear = Array.ofDim[Int](coords.length)
    val shape = space.shape
    var i = 0
    var error = Option.empty[VoxelRoiError]
    while i < coords.length && error.isEmpty do
      Indexing.gridToIndexChecked(shape, coords(i)) match
        case Left(err) => error = Some(VoxelRoiError.InvalidCoordinate(i, err))
        case Right(idx) => linear(i) = idx
      i += 1

    error match
      case Some(err) => Left(err)
      case None =>
        VoxelIndexSet.makeUnique(
          space,
          NDArray.fromSeq(Shape(linear.length), linear)
        )
          .left.map(VoxelRoiError.InvalidIndexSet.apply)
          .map(indexSet => new VoxelRoi(space, coords, indexSet))

  @targetName("makeFromNeuroSpace")
  def make(space: NeuroSpace, coords: Vector[VoxelCoord]): Either[VoxelRoiError, VoxelRoi] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelRoiError.InvalidSpace.apply).flatMap(make(_, coords))

  def fromRaw(space: VolumeSpace, coords: Vector[Vector[Int]]): Either[VoxelRoiError, VoxelRoi] =
    parseCoords(coords).flatMap(make(space, _))

  @targetName("fromRawNeuroSpace")
  def fromRaw(space: NeuroSpace, coords: Vector[Vector[Int]]): Either[VoxelRoiError, VoxelRoi] =
    VolumeSpace.fromSpatialPart(space).left.map(VoxelRoiError.InvalidSpace.apply).flatMap(fromRaw(_, coords))

  def apply(space: VolumeSpace, coords: Vector[VoxelCoord]): VoxelRoi =
    make(space, coords).fold(err => throw new IllegalArgumentException(err.message), roi => roi)

  @targetName("applyNeuroSpace")
  def apply(space: NeuroSpace, coords: Vector[VoxelCoord]): VoxelRoi =
    make(space, coords).fold(err => throw new IllegalArgumentException(err.message), roi => roi)

  def fromRawUnsafe(space: VolumeSpace, coords: Vector[Vector[Int]]): VoxelRoi =
    fromRaw(space, coords).fold(err => throw new IllegalArgumentException(err.message), roi => roi)

  @targetName("fromRawUnsafeNeuroSpace")
  def fromRawUnsafe(space: NeuroSpace, coords: Vector[Vector[Int]]): VoxelRoi =
    fromRaw(space, coords).fold(err => throw new IllegalArgumentException(err.message), roi => roi)

  private[image] def fromSelection(selection: VoxelSelection): VoxelRoi =
    new VoxelRoi(selection.space, selection.voxelCoords, selection.indexSet)

  private def parseCoords(coords: Vector[Vector[Int]]): Either[VoxelRoiError, Vector[VoxelCoord]] =
    val out = Vector.newBuilder[VoxelCoord]
    out.sizeHint(coords.length)
    var i = 0
    var error = Option.empty[VoxelRoiError]
    while i < coords.length && error.isEmpty do
      VoxelCoord.fromVector(coords(i), "ROI coord") match
        case Left(err) => error = Some(VoxelRoiError.InvalidCoordinate(i, err))
        case Right(coord) => out += coord
      i += 1

    error match
      case Some(err) => Left(err)
      case None => Right(out.result())

final class ROIVol[A] private[scalafim] (
    val space: NeuroSpace,
    private val checkedRoi: VoxelRoi,
    private[scalafim] val data: Array1[A]
):
  def size: Int = checkedRoi.size

  def apply(index: Int): A =
    data(index)

  def coords: ROICoords =
    checkedRoi.toROICoords

  def roi: VoxelRoi =
    checkedRoi

  /** Immutable Ravel storage in ROI order. */
  def values: Array1[A] =
    data

  def linearIndices: Array1[Int] =
    checkedRoi.linearIndices

  def realCoords: Vector[Vector[Double]] =
    checkedRoi.coords.map { coord =>
      val voxel = VoxelPoint(coord.x.toDouble, coord.y.toDouble, coord.z.toDouble)
      checkedRoi.space.voxelToWorld(voxel).toVector
    }

  def toSparse: SparseNeuroVol[A] =
    SparseNeuroVol.fromIndexSet(data, checkedRoi.linearIndexSet, space, label = "")

object ROICoords:
  def apply(coords: Array1[Int]): ROICoords =
    require(coords.size % 3 == 0, "flat coords must be multiple of 3")
    val n = coords.size / 3
    val vec = Vector.tabulate(n) { i =>
      Vector(coords(3 * i), coords(3 * i + 1), coords(3 * i + 2))
    }
    ROICoords(vec)

  def apply(coords: Array[Int]): ROICoords =
    require(coords.length % 3 == 0, "flat coords must be multiple of 3")
    val n = coords.length / 3
    ROICoords(
      Vector.tabulate(n): i =>
        Vector(coords(3 * i), coords(3 * i + 1), coords(3 * i + 2))
    )

  def fromRoi(roi: VoxelRoi): ROICoords =
    roi.toROICoords

  def make(space: VolumeSpace, coords: Vector[Vector[Int]]): Either[VoxelRoiError, ROICoords] =
    VoxelRoi.fromRaw(space, coords).map(_.toROICoords)

  @targetName("makeFromNeuroSpace")
  def make(space: NeuroSpace, coords: Vector[Vector[Int]]): Either[VoxelRoiError, ROICoords] =
    VoxelRoi.fromRaw(space, coords).map(_.toROICoords)

object ROIVol:
  private def ravelValues[A](
      data: Array[A]
  )(using DType[A]): Array1[A] =
    NDArray.fromSeq(Shape(data.length), data)

  def make[A](
      space: NeuroSpace,
      coords: Vector[Vector[Int]],
      data: Array1[A]
  ): Either[ROIVolError, ROIVol[A]] =
    VoxelRoi
      .fromRaw(space, coords)
      .left
      .map(ROIVolError.InvalidRoi.apply)
      .flatMap(make(space, _, data))

  def make[A](
      space: NeuroSpace,
      coords: Vector[Vector[Int]],
      data: Array[A]
  )(using DType[A]): Either[ROIVolError, ROIVol[A]] =
    make(space, coords, ravelValues(data))

  def make[A](
      space: NeuroSpace,
      roi: VoxelRoi,
      data: Array1[A]
  ): Either[ROIVolError, ROIVol[A]] =
    for
      volumeSpace <- VolumeSpace
        .fromSpatialPart(space)
        .left
        .map(ROIVolError.InvalidSpace.apply)
      _ <- GridCompatibility
        .volume(volumeSpace, roi.space)
        .left
        .map(ROIVolError.Grid.apply)
      _ <-
        if data.size == roi.size then Right(())
        else Left(ROIVolError.DataLengthMismatch(roi.size, data.size))
    yield new ROIVol(space, roi, data)

  def make[A](
      space: NeuroSpace,
      roi: VoxelRoi,
      data: Array[A]
  )(using DType[A]): Either[ROIVolError, ROIVol[A]] =
    make(space, roi, ravelValues(data))

  def apply[A](
      space: NeuroSpace,
      coords: Vector[Vector[Int]],
      data: Array1[A]
  ): ROIVol[A] =
    make(space, coords, data)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def apply[A](
      space: NeuroSpace,
      coords: Vector[Vector[Int]],
      data: Array[A]
  )(using DType[A]): ROIVol[A] =
    apply(space, coords, ravelValues(data))

  def apply[A](
      space: NeuroSpace,
      roi: VoxelRoi,
      data: Array1[A]
  ): ROIVol[A] =
    make(space, roi, data)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def apply[A](
      space: NeuroSpace,
      roi: VoxelRoi,
      data: Array[A]
  )(using DType[A]): ROIVol[A] =
    apply(space, roi, ravelValues(data))

  private[scalafim] def unsafeOwned[A](
      space: NeuroSpace,
      roi: VoxelRoi,
      data: Array1[A]
  ): ROIVol[A] =
    new ROIVol(space, roi, data)
