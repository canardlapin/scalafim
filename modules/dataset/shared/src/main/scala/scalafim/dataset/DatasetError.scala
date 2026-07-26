package scalafim.dataset

import scalafim.archive.ArchiveError
import scalafim.image.{NeuroSpaceError, VoxelCoord}
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
  case InvalidVoxelCoordinate(coordinate: VoxelCoord, detail: String)
  case ArchiveFailure(error: ArchiveError)
  case LatentFailure(error: LatentError)
  case StorageFailure(detail: String)
  case InvalidLabel(label: String, value: String, detail: String)
  case EmptyDatasetIndex
  case DuplicateDatasetRun(key: String)
  case DatasetRunNotFound(query: String)
  case AmbiguousDatasetRun(query: String, matches: Int)
  case InvalidTimeAxis(detail: String)
  case InvalidDatasetValue(field: String, value: String, detail: String)
  case InvalidEventRow(row: Int, detail: String)
  case DatasetColumnNotFound(field: String)

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
      case InvalidVoxelCoordinate(coordinate, detail) =>
        s"invalid voxel coordinate $coordinate: $detail"
      case ArchiveFailure(error) =>
        error.message
      case LatentFailure(error) =>
        error.message
      case StorageFailure(detail) =>
        detail
      case InvalidLabel(label, value, detail) =>
        s"invalid $label label '$value': $detail"
      case EmptyDatasetIndex =>
        "dataset index must contain at least one run"
      case DuplicateDatasetRun(key) =>
        s"dataset index contains duplicate run key '$key'"
      case DatasetRunNotFound(query) =>
        s"dataset query matched no runs: $query"
      case AmbiguousDatasetRun(query, matches) =>
        s"dataset query matched $matches runs: $query"
      case InvalidTimeAxis(detail) =>
        s"invalid dataset time axis: $detail"
      case InvalidDatasetValue(field, value, detail) =>
        s"invalid dataset value for '$field'='$value': $detail"
      case InvalidEventRow(row, detail) =>
        s"invalid dataset event row $row: $detail"
      case DatasetColumnNotFound(field) =>
        s"dataset event column '$field' is not present"
