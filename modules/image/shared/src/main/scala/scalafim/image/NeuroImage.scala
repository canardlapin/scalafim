package scalafim.image

import image4s.AxisKind
import image4s.ImageError
import image4s.ImageMetadata
import image4s.geometry.GeometryError
import image4s.geometry.GridId

enum NeuroImageError derives CanEqual:
  case InvalidRank(label: String, expected: Int, actual: Int)
  case ShapeMismatch(label: String, expected: Vector[Int], actual: Vector[Int])
  case LinearSizeMismatch(label: String, expected: Int, actual: Int)
  case Space(error: SampleSpaceError)
  case Geometry(error: GeometryError)
  case Image(error: ImageError)
  case GridOwnerMismatch(
      expected: Option[GridId],
      actual: Option[GridId]
  )
  case ConcatenationMetadataMismatch(
      left: ImageMetadata,
      right: ImageMetadata
  )
  case ExpectedSingleTimeAxis(actual: Vector[AxisKind])
  case CanonicalArraySizeMismatch(expected: Int, actual: Int)
  case SpatialAxisOutOfBounds(axis: Int)
  case SpatialIndexOutOfBounds(axis: Int, index: Int, extent: Int)

  def message: String =
    this match
      case InvalidRank(label, expected, actual) =>
        s"$label requires $expected-dimensional data; got $actual-dimensional data"
      case ShapeMismatch(label, expected, actual) =>
        s"$label data shape mismatch: expected $expected, got $actual"
      case LinearSizeMismatch(label, expected, actual) =>
        s"$label linear data length mismatch: expected $expected, got $actual"
      case Space(error) =>
        error.message
      case Geometry(error) =>
        error.message
      case Image(error) =>
        error.message
      case GridOwnerMismatch(expected, actual) =>
        s"grid runtime owner mismatch: expected persistent id $expected, got $actual"
      case ConcatenationMetadataMismatch(left, right) =>
        s"series concatenation metadata mismatch: left=$left, right=$right"
      case ExpectedSingleTimeAxis(actual) =>
        s"NeuroSeries requires exactly one Time axis; found $actual"
      case CanonicalArraySizeMismatch(expected, actual) =>
        s"canonical input requires $expected values; found $actual"
      case SpatialAxisOutOfBounds(axis) =>
        s"spatial axis $axis is outside [0, 3)"
      case SpatialIndexOutOfBounds(axis, index, extent) =>
        s"spatial index $index on axis $axis is outside [0, $extent)"
