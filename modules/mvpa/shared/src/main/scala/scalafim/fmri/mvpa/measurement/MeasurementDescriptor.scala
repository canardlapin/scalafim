package scalafim.fmri.mvpa.measurement

import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest}

opaque type MeasurementId = String

object MeasurementId:
  def apply(value: String): Either[MeasurementError, MeasurementId] =
    if value.isEmpty || value != value.trim || value.length > 1024 || value.exists(_.isControl) then
      Left(MeasurementError.InvalidMeasurementId(value))
    else Right(value)

  def unsafe(value: String): MeasurementId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: MeasurementId) def value: String = value

enum MeasurementKind:
  case Identity, HardSelection, WeightedRegion, BasisMap

enum MetricCapability:
  case CoordinateIsometry
  case DeclaredMetricRequired(reason: String)

enum MaterializationCost:
  case None
  case LinearOperatorApplications(perOutputColumn: Int, reason: String)

final case class MeasurementDescriptor(
    id: MeasurementId,
    kind: MeasurementKind,
    source: AxisDescriptor,
    local: AxisDescriptor,
    mapFingerprint: String,
    metric: MetricCapability,
    materialization: MaterializationCost,
    rendition: Vector[(String, String)] = Vector.empty
):
  val semanticId: String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim-measurement/v1")
      writer.string(id.value)
      writer.string(kind.toString)
      writer.string(source.stableKey)
      writer.string(local.stableKey)
      writer.string(mapFingerprint)
      metric match
        case MetricCapability.CoordinateIsometry => writer.string("coordinate-isometry")
        case MetricCapability.DeclaredMetricRequired(reason) =>
          writer.string("declared-metric-required")
          writer.string(reason)

enum MeasurementError:
  case InvalidMeasurementId(value: String)
  case EmptySupport
  case DuplicateSupport(position: Int)
  case UnknownSupport
  case InvalidWeight(position: Int, value: Double)
  case ShapeMismatch(expectedRows: Int, expectedColumns: Int, actualRows: Int, actualColumns: Int)
  case SourceMismatch(expected: AxisDescriptor, actual: AxisDescriptor)
  case DuplicateMeasurement(id: MeasurementId)
  case EmptyFrame
  case InvalidBudget(maxOpenResources: Int)
  case UnverifiedMetric
  case Semantic(detail: String)
  case Axis(detail: String)

  def message: String =
    this match
      case InvalidMeasurementId(value) => s"invalid measurement id '$value'"
      case EmptySupport => "measurement support must be non-empty"
      case DuplicateSupport(position) => s"measurement support repeats source ordinal $position"
      case UnknownSupport => "measurement support contains a coordinate absent from the source axis"
      case InvalidWeight(position, value) => s"measurement weight at source ordinal $position must be finite and non-zero, got $value"
      case ShapeMismatch(rows, columns, actualRows, actualColumns) => s"measurement map must be ${rows}x${columns}, got ${actualRows}x${actualColumns}"
      case SourceMismatch(expected, actual) => s"measurement source ${actual.stableKey} does not match ${expected.stableKey}"
      case DuplicateMeasurement(id) => s"measurement frame repeats id '${id.value}'"
      case EmptyFrame => "measurement frame must contain at least one measurement"
      case InvalidBudget(value) => s"measurement traversal requires at least one open resource, got $value"
      case UnverifiedMetric => "arbitrary basis maps require an explicit metric; coordinate isometry has not been verified"
      case Semantic(detail) => detail
      case Axis(detail) => detail
