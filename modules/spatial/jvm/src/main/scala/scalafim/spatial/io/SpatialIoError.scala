package scalafim.spatial.io

import java.nio.file.Path

enum SpatialIoError:
  case IoFailure(path: Path, reason: String)
  case InvalidCacheHeader(path: Path, reason: String)
  case InvalidCachePayload(path: Path, reason: String)
  case UnsupportedLinearMap(path: Path, className: String)
  case InvalidTransformDescriptor(label: String, reason: String)
  case MissingInverseQuality(asset: String)

  def message: String =
    this match
      case IoFailure(path, reason) =>
        s"spatial IO failed for $path: $reason"
      case InvalidCacheHeader(path, reason) =>
        s"invalid spatial triplet cache header in $path: $reason"
      case InvalidCachePayload(path, reason) =>
        s"invalid spatial triplet cache payload in $path: $reason"
      case UnsupportedLinearMap(path, className) =>
        s"spatial triplet cache can only persist CSR operators, got $className for $path"
      case InvalidTransformDescriptor(label, reason) =>
        s"invalid transform descriptor $label: $reason"
      case MissingInverseQuality(asset) =>
        s"transform descriptor $asset has no declared inverse quality"
