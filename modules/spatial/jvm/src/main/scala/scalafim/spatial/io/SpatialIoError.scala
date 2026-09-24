package scalafim.spatial.io

import scalafim.transform.{TransformError, TransformFormat, TransformIoError}

import java.nio.file.Path

enum SpatialIoReason:
  case Io
  case CacheHeader
  case CachePayload
  case UnsupportedLinearMap
  case TransformDescriptor
  case TransformAsset
  case TransformInterpretation
  case MissingInverseQuality

enum SpatialIoError:
  case IoFailure(path: Path, reason: String)
  case InvalidCacheHeader(path: Path, reason: String)
  case InvalidCachePayload(path: Path, reason: String)
  case UnsupportedLinearMap(path: Path, className: String)
  case InvalidTransformDescriptor(label: String, reason: String)

  /** The file could not be read, detected or decoded by the transform codecs. */
  case TransformRead(path: Path, cause: TransformIoError)

  /** The decoded file has no world-transform meaning between the descriptor's domains (or cannot be inverted). */
  case TransformInterpretation(path: Path, cause: TransformError)

  /** A decodable format the graph adapter does not ingest as a single route (e.g. a multi-volume series). */
  case UnsupportedTransformAsset(path: Path, format: TransformFormat, reason: String)
  case MissingInverseQuality(asset: String)

  def reasonKind: SpatialIoReason =
    this match
      case IoFailure(_, _) => SpatialIoReason.Io
      case InvalidCacheHeader(_, _) => SpatialIoReason.CacheHeader
      case InvalidCachePayload(_, _) => SpatialIoReason.CachePayload
      case UnsupportedLinearMap(_, _) => SpatialIoReason.UnsupportedLinearMap
      case InvalidTransformDescriptor(_, _) => SpatialIoReason.TransformDescriptor
      case TransformRead(_, _) | UnsupportedTransformAsset(_, _, _) => SpatialIoReason.TransformAsset
      case TransformInterpretation(_, _) => SpatialIoReason.TransformInterpretation
      case MissingInverseQuality(_) => SpatialIoReason.MissingInverseQuality

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
      case TransformRead(path, cause) =>
        s"cannot read transform asset $path: ${cause.message}"
      case TransformInterpretation(path, cause) =>
        s"cannot interpret transform asset $path: ${cause.message}"
      case UnsupportedTransformAsset(path, format, reason) =>
        s"unsupported $format transform asset $path: $reason"
      case MissingInverseQuality(asset) =>
        s"transform descriptor $asset has no declared inverse quality"
