package scalafim.dataset

import scalafim.archive.ArchiveError
import scalafim.image.NeuroSpaceError
import scalafim.latent.LatentError

enum DatasetAxis(val label: String):
  case Timepoint extends DatasetAxis("timepoint")
  case Voxel extends DatasetAxis("voxel")

enum DatasetError:
  case InvalidSpace(error: NeuroSpaceError)
  case NonPositiveTimepoints(value: Int)
  case NonPositiveAxisSize(axis: DatasetAxis, value: Int)
  case NegativeIndex(axis: DatasetAxis, index: Int)
  case IndexOutOfBounds(axis: DatasetAxis, index: Int, size: Int)
  case EmptySelection(axis: DatasetAxis)
  case DuplicateSelection(axis: DatasetAxis, index: Int)
  case ShapeMismatch(detail: String)
  case MatrixShapeMismatch(label: String, expectedRows: Int, expectedCols: Int, actualRows: Int, actualCols: Int)
  case VoxelOutsideMask(voxel: Int)
  case ArchiveFailure(error: ArchiveError)
  case LatentFailure(error: LatentError)
  case StorageFailure(detail: String)
  case InvalidLabel(label: String, value: String, detail: String)

  def message: String =
    this match
      case InvalidSpace(error) =>
        error.message
      case NonPositiveTimepoints(value) =>
        s"timepoints must be positive; got $value"
      case NonPositiveAxisSize(axis, value) =>
        s"${axis.label} axis size must be positive; got $value"
      case NegativeIndex(axis, index) =>
        s"${axis.label} index must be non-negative; got $index"
      case IndexOutOfBounds(axis, index, size) =>
        s"${axis.label} index $index out of bounds for size $size"
      case EmptySelection(axis) =>
        s"${axis.label} selection must be non-empty"
      case DuplicateSelection(axis, index) =>
        s"${axis.label} selection contains duplicate index $index"
      case ShapeMismatch(detail) =>
        s"dataset shape mismatch: $detail"
      case MatrixShapeMismatch(label, expectedRows, expectedCols, actualRows, actualCols) =>
        s"$label expected ${expectedRows}x${expectedCols} but got ${actualRows}x${actualCols}"
      case VoxelOutsideMask(voxel) =>
        s"voxel $voxel is outside the latent mask"
      case ArchiveFailure(error) =>
        error.message
      case LatentFailure(error) =>
        error.message
      case StorageFailure(detail) =>
        detail
      case InvalidLabel(label, value, detail) =>
        s"invalid $label label '$value': $detail"
