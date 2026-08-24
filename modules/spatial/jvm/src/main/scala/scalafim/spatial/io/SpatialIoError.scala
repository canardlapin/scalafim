package scalafim.spatial.io

import image4s.geometry.GeometryError
import java.nio.file.Path

enum SpatialIoReason:
  case Io
  case CacheHeader
  case CachePayload
  case UnsupportedLinearMap
  case TransformDescriptor
  case TransformAsset
  case TransformConvention
  case TransformGeometry
  case MissingInverseQuality

enum SpatialIoError:
  case IoFailure(path: Path, reason: String)
  case InvalidCacheHeader(path: Path, reason: String)
  case InvalidCachePayload(path: Path, reason: String)
  case UnsupportedLinearMap(path: Path, className: String)
  case InvalidTransformDescriptor(label: String, reason: String)
  case UnsupportedTransformAsset(path: Path, format: TransformFileFormat, reason: String)
  case UnsupportedItkTransformType(path: Path, componentIndex: Int, transformType: String)
  case MalformedTransformAsset(path: Path, reason: String)
  case TransformConventionMismatch(path: Path, reason: String)
  case Geometry(path: Path, cause: GeometryError)
  case MissingInverseQuality(asset: String)

  def reasonKind: SpatialIoReason =
    this match
      case IoFailure(_, _) => SpatialIoReason.Io
      case InvalidCacheHeader(_, _) => SpatialIoReason.CacheHeader
      case InvalidCachePayload(_, _) => SpatialIoReason.CachePayload
      case UnsupportedLinearMap(_, _) => SpatialIoReason.UnsupportedLinearMap
      case InvalidTransformDescriptor(_, _) => SpatialIoReason.TransformDescriptor
      case UnsupportedTransformAsset(_, _, _) | UnsupportedItkTransformType(_, _, _) | MalformedTransformAsset(_, _) => SpatialIoReason.TransformAsset
      case TransformConventionMismatch(_, _) => SpatialIoReason.TransformConvention
      case Geometry(_, _) => SpatialIoReason.TransformGeometry
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
      case UnsupportedTransformAsset(path, format, reason) =>
        s"unsupported $format transform asset $path: $reason"
      case UnsupportedItkTransformType(path, componentIndex, transformType) =>
        s"unsupported ITK transform type '$transformType' at component $componentIndex in $path"
      case MalformedTransformAsset(path, reason) =>
        s"malformed transform asset $path: $reason"
      case TransformConventionMismatch(path, reason) =>
        s"transform convention mismatch for $path: $reason"
      case Geometry(path, cause) =>
        s"invalid transform geometry for $path: ${cause.message}"
      case MissingInverseQuality(asset) =>
        s"transform descriptor $asset has no declared inverse quality"
