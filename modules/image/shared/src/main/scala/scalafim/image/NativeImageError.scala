package scalafim.image

import image4s.AxisKind
import image4s.ImageError

enum NativeImageError derives CanEqual:
  case Image(error: ImageError)
  case ExpectedSingleTimeAxis(actual: Vector[AxisKind])
  case SpatialAxisOutOfBounds(axis: Int)
  case SpatialIndexOutOfBounds(axis: Int, index: Int, extent: Int)

  def message: String =
    this match
      case Image(error) =>
        error.message
      case ExpectedSingleTimeAxis(actual) =>
        s"NeuroSeries requires exactly one Time axis; found $actual"
      case SpatialAxisOutOfBounds(axis) =>
        s"spatial axis $axis is outside [0, 3)"
      case SpatialIndexOutOfBounds(axis, index, extent) =>
        s"spatial index $index on axis $axis is outside [0, $extent)"
