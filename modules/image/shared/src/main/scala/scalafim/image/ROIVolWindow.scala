package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

enum ROIVolWindowError:
  case DataLengthMismatch(expected: Int, actual: Int)
  case InvalidSpace(error: NeuroSpaceError)
  case InvalidSelection(error: VoxelRoiError)
  case CenterIndexOutOfBounds(index: Int, size: Int)
  case ParentIndexOutOfBounds(index: Int, size: Int)
  case CenterMismatch(centerIndex: Int, expectedParent: Int, actualParent: Int)

  def message: String =
    this match
      case DataLengthMismatch(expected, actual) =>
        s"ROI window data length mismatch: expected $expected, got $actual"
      case InvalidSpace(error) =>
        error.message
      case InvalidSelection(error) =>
        error.message
      case CenterIndexOutOfBounds(index, size) =>
        s"ROI window center index $index out of bounds for $size selected voxels"
      case ParentIndexOutOfBounds(index, size) =>
        s"ROI window parent index $index out of bounds for volume size $size"
      case CenterMismatch(centerIndex, expectedParent, actualParent) =>
        s"ROI window center row $centerIndex points to voxel $actualParent, not parent voxel $expectedParent"

final class ROIVolWindow[A] private (
    val space: NeuroSpace,
    val coords: ROICoords,
    private[scalafim] val data: NArray[A],
    val centerIndex: Int,
    val parentIndex: Int,
    val label: String,
    val selection: VoxelSelection
):
  def centerVoxel: VoxelCoord =
    selection.voxelCoords(centerIndex)

  def apply(index: Int): A =
    data(index)

  def toNArray(using ClassTag[A]): NArray[A] =
    NArray.copy(data)

  def region: VoxelRegion =
    selection.region

  def toROIVol: ROIVol[A] =
    ROIVol.unsafeOwned(space, selection.toVoxelRoi, data)

object ROIVolWindow:
  def make[A](
      space: NeuroSpace,
      coords: ROICoords,
      data: NArray[A],
      centerIndex: Int,
      parentIndex: Int,
      label: String = ""
  )(using ClassTag[A]): Either[ROIVolWindowError, ROIVolWindow[A]] =
    fromOwned(space, coords, NArray.copy(data), centerIndex, parentIndex, label)

  private[scalafim] def fromOwned[A](
      space: NeuroSpace,
      coords: ROICoords,
      data: NArray[A],
      centerIndex: Int,
      parentIndex: Int,
      label: String = ""
  ): Either[ROIVolWindowError, ROIVolWindow[A]] =
    for
      volumeSpace <- VolumeSpace
        .fromSpatialPart(space)
        .left
        .map(ROIVolWindowError.InvalidSpace.apply)
      selection <- coords
        .asSelectionIn(volumeSpace)
        .left
        .map(ROIVolWindowError.InvalidSelection.apply)
      _ <-
        if data.length == selection.size then Right(())
        else Left(ROIVolWindowError.DataLengthMismatch(selection.size, data.length))
      _ <-
        if centerIndex >= 0 && centerIndex < selection.size then Right(())
        else Left(ROIVolWindowError.CenterIndexOutOfBounds(centerIndex, selection.size))
      _ <-
        if parentIndex >= 0 && parentIndex < volumeSpace.nVoxels then Right(())
        else Left(ROIVolWindowError.ParentIndexOutOfBounds(parentIndex, volumeSpace.nVoxels))
      actualParent = selection.indexSet(centerIndex)
      _ <-
        if actualParent == parentIndex then Right(())
        else Left(ROIVolWindowError.CenterMismatch(centerIndex, parentIndex, actualParent))
    yield new ROIVolWindow(space, coords, data, centerIndex, parentIndex, label, selection)

  def apply[A](
      space: NeuroSpace,
      coords: ROICoords,
      data: NArray[A],
      centerIndex: Int,
      parentIndex: Int,
      label: String = ""
  )(using ClassTag[A]): ROIVolWindow[A] =
    make(space, coords, data, centerIndex, parentIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private[scalafim] def unsafe[A](
      space: NeuroSpace,
      coords: ROICoords,
      data: NArray[A],
      centerIndex: Int,
      parentIndex: Int,
      label: String = ""
  ): ROIVolWindow[A] =
    val selection =
      VoxelSelection
        .fromROICoords(space, coords)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    new ROIVolWindow(space, coords, data, centerIndex, parentIndex, label, selection)
