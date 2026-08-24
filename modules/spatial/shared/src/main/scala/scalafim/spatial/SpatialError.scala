package scalafim.spatial

import image4s.geometry.GeometryError
import scalafim.image.SampleSpaceError
import reframe4s.core.MapError

enum SpatialErrorReason:
  case Identifier
  case Dimension
  case Geometry
  case DomainKind
  case MorphismCompatibility
  case Graph
  case Route
  case CoordinateMap
  case Operator
  case RowSelection
  case Field
  case Source
  case Cache

enum SpatialError:
  case EmptyIdentifier(label: String)
  case NonPositiveDimension(label: String, value: Int)
  case EmptyHybrid
  case DuplicatePartName(name: PartName)
  case NegativeOffset(part: PartName, offset: Int)
  case MaskSpaceMismatch(label: String)
  case SampleSpaceAdmission(cause: SampleSpaceError)
  case Geometry(cause: GeometryError)
  case DomainKindMismatch(id: DomainId, expected: DomainKind, actual: DomainKind)
  case LatentDimensionMismatch(id: DomainId, spaceDim: Int, geometryDim: Int)
  case UnsupportedGeometry(label: String)
  case InvalidCost(value: Double)
  case InvalidQuality(label: String, value: Double)
  case IdentityMorphismDomainMismatch(source: DomainId, target: DomainId)
  case IncompatibleMorphismKind(kind: MorphismKind, source: DomainId, sourceKind: DomainKind, target: DomainId, targetKind: DomainKind)
  case DuplicateDomain(id: DomainId)
  case DuplicateMorphism(id: MorphismId)
  case DomainNotFound(id: DomainId)
  case MorphismDomainMissing(id: MorphismId, domain: DomainId)
  case NoPath(source: DomainId, target: DomainId)
  case EmptyPath
  case DisconnectedPath(previous: DomainId, next: DomainId)
  case NonInvertibleMorphism(id: MorphismId)
  case InvalidProviderCoordinateMap(reason: String)
  case CoordinateMapDomainMismatch(id: MorphismId, source: DomainId, target: DomainId, mapSource: String, mapTarget: String)
  case ProviderMap(cause: MapError)
  case MissingCoordinateMap(id: MorphismId)
  case NonVolumeDomain(id: DomainId)
  case NonSurfaceDomain(id: DomainId)
  case UnsupportedMorphismForCompilation(id: MorphismId, kind: MorphismKind)
  case MorphismCompilerNotFound(kind: MorphismKind)
  case DuplicateMorphismCompiler(kind: MorphismKind)
  case MorphismCompilerKindMismatch(compiler: String, expected: MorphismKind, actual: MorphismKind)
  case InvalidMorphismPlugin(id: MorphismId, detail: String)
  case UnsupportedPluginComposition(detail: String)
  case UnsupportedSurfaceSampling(label: String)
  case SurfacePairMismatch(domain: DomainId)
  case SurfaceSamplingGeometryMismatch(id: MorphismId)
  case SurfaceMappingGeometryMismatch(id: MorphismId)
  case InvalidMixedPullback(detail: String)
  case FieldDataUnavailable(label: String)
  case FieldDomainMismatch(field: DomainId, operator: DomainId)
  case FieldShapeMismatch(expectedRows: Int, actualRows: Int)
  case FieldObservationMismatch(expected: Int, actual: Int)
  case FieldMaterializedShapeMismatch(expectedRows: Int, actualRows: Int, expectedObservations: Int, actualObservations: Int)
  case FieldSourceUnavailable(source: FieldSourceId, detail: String)
  case FieldSourceStale(source: FieldSourceId, expected: String, actual: String)
  case FieldSourceDomainMismatch(source: FieldSourceId, expected: DomainId, actual: DomainId)
  case FieldSourceGeometryMismatch(source: FieldSourceId)
  case FieldSourceSampleSpaceAdmission(source: FieldSourceId, cause: SampleSpaceError)
  case FieldSourceGridMismatch(source: FieldSourceId, cause: GeometryError)
  case FieldSourceShapeMismatch(source: FieldSourceId, expectedRows: Int, actualRows: Int)
  case FieldSourceIndexOutOfBounds(axis: String, index: Int, limit: Int)
  case DuplicateFieldSourceIndex(axis: String, index: Int)
  case FieldSourceRequestMismatch(source: FieldSourceId)
  case FieldSourceBlockShapeMismatch(source: FieldSourceId, expectedRows: Int, actualRows: Int, expectedObservations: Int, actualObservations: Int)
  case FieldSourceReadFailed(source: FieldSourceId, detail: String)
  case OperatorCacheMiss(key: String)
  case InvalidRoiRow(index: Int, limit: Int)
  case DuplicateRoiRow(index: Int)
  case EmptyRoi
  case CoordinateTransformFailed(reason: String)
  case OperatorAssemblyFailed(reason: String)

  def reasonKind: SpatialErrorReason =
    this match
      case EmptyIdentifier(_) =>
        SpatialErrorReason.Identifier
      case NonPositiveDimension(_, _) | LatentDimensionMismatch(_, _, _) | NegativeOffset(_, _) =>
        SpatialErrorReason.Dimension
      case EmptyHybrid | DuplicatePartName(_) | MaskSpaceMismatch(_) | SampleSpaceAdmission(_) | Geometry(_) | UnsupportedGeometry(_) =>
        SpatialErrorReason.Geometry
      case DomainKindMismatch(_, _, _) =>
        SpatialErrorReason.DomainKind
      case InvalidCost(_) | InvalidQuality(_, _) | IdentityMorphismDomainMismatch(_, _) | IncompatibleMorphismKind(_, _, _, _, _) =>
        SpatialErrorReason.MorphismCompatibility
      case DuplicateDomain(_) | DuplicateMorphism(_) | DomainNotFound(_) | MorphismDomainMissing(_, _) =>
        SpatialErrorReason.Graph
      case NoPath(_, _) | EmptyPath | DisconnectedPath(_, _) | NonInvertibleMorphism(_) =>
        SpatialErrorReason.Route
      case InvalidProviderCoordinateMap(_) | CoordinateMapDomainMismatch(_, _, _, _, _) | ProviderMap(_) | MissingCoordinateMap(_) | CoordinateTransformFailed(_) =>
        SpatialErrorReason.CoordinateMap
      case NonVolumeDomain(_) | NonSurfaceDomain(_) | UnsupportedMorphismForCompilation(_, _) | MorphismCompilerNotFound(_) | DuplicateMorphismCompiler(_) | MorphismCompilerKindMismatch(_, _, _) | InvalidMorphismPlugin(_, _) | UnsupportedPluginComposition(_) | UnsupportedSurfaceSampling(_) | SurfacePairMismatch(_) | SurfaceSamplingGeometryMismatch(_) | SurfaceMappingGeometryMismatch(_) | InvalidMixedPullback(_) | OperatorAssemblyFailed(_) =>
        SpatialErrorReason.Operator
      case InvalidRoiRow(_, _) | DuplicateRoiRow(_) | EmptyRoi =>
        SpatialErrorReason.RowSelection
      case FieldDataUnavailable(_) | FieldDomainMismatch(_, _) | FieldShapeMismatch(_, _) | FieldObservationMismatch(_, _) | FieldMaterializedShapeMismatch(_, _, _, _) =>
        SpatialErrorReason.Field
      case FieldSourceUnavailable(_, _) | FieldSourceStale(_, _, _) | FieldSourceDomainMismatch(_, _, _) | FieldSourceGeometryMismatch(_) | FieldSourceSampleSpaceAdmission(_, _) | FieldSourceGridMismatch(_, _) | FieldSourceShapeMismatch(_, _, _) | FieldSourceIndexOutOfBounds(_, _, _) | DuplicateFieldSourceIndex(_, _) | FieldSourceRequestMismatch(_) | FieldSourceBlockShapeMismatch(_, _, _, _, _) | FieldSourceReadFailed(_, _) =>
        SpatialErrorReason.Source
      case OperatorCacheMiss(_) =>
        SpatialErrorReason.Cache

  def message: String =
    this match
      case EmptyIdentifier(label) =>
        s"$label identifier must be non-empty"
      case NonPositiveDimension(label, value) =>
        s"$label dimension must be positive, got $value"
      case EmptyHybrid =>
        "hybrid geometry must contain at least one part"
      case DuplicatePartName(name) =>
        s"hybrid part name is duplicated: ${name.value}"
      case NegativeOffset(part, offset) =>
        s"hybrid part ${part.value} has negative offset $offset"
      case MaskSpaceMismatch(label) =>
        s"$label mask geometry does not match sampled geometry"
      case SampleSpaceAdmission(cause) =>
        cause.message
      case Geometry(cause) =>
        cause.message
      case DomainKindMismatch(id, expected, actual) =>
        s"domain ${id.value} declares $expected space but uses $actual sampling geometry"
      case LatentDimensionMismatch(id, spaceDim, geometryDim) =>
        s"latent domain ${id.value} declares $spaceDim dimensions but geometry has $geometryDim"
      case UnsupportedGeometry(label) =>
        s"unsupported geometry: $label"
      case InvalidCost(value) =>
        s"morphism cost must be finite and non-negative, got $value"
      case InvalidQuality(label, value) =>
        s"$label quality must be finite and in [0, 1], got $value"
      case IdentityMorphismDomainMismatch(source, target) =>
        s"identity morphism must stay within one domain, got ${source.value} to ${target.value}"
      case IncompatibleMorphismKind(kind, source, sourceKind, target, targetKind) =>
        s"morphism kind $kind is incompatible with ${source.value}:$sourceKind to ${target.value}:$targetKind"
      case DuplicateDomain(id) =>
        s"domain already exists in graph: ${id.value}"
      case DuplicateMorphism(id) =>
        s"morphism already exists in graph: ${id.value}"
      case DomainNotFound(id) =>
        s"domain not found: ${id.value}"
      case MorphismDomainMissing(id, domain) =>
        s"morphism ${id.value} references missing domain ${domain.value}"
      case NoPath(source, target) =>
        s"no path from ${source.value} to ${target.value}"
      case EmptyPath =>
        "morphism path must contain at least one morphism"
      case DisconnectedPath(previous, next) =>
        s"morphism path is disconnected between ${previous.value} and ${next.value}"
      case NonInvertibleMorphism(id) =>
        s"morphism ${id.value} does not have a geometric inverse"
      case InvalidProviderCoordinateMap(reason) =>
        s"invalid provider coordinate map: $reason"
      case CoordinateMapDomainMismatch(id, source, target, mapSource, mapTarget) =>
        s"morphism ${id.value} is ${source.value}->${target.value}, but its coordinate map is $mapSource->$mapTarget"
      case ProviderMap(cause) =>
        cause.message
      case MissingCoordinateMap(id) =>
        s"morphism ${id.value} does not carry an executable coordinate map"
      case NonVolumeDomain(id) =>
        s"domain ${id.value} is not a volume domain"
      case NonSurfaceDomain(id) =>
        s"domain ${id.value} is not a surface domain"
      case UnsupportedMorphismForCompilation(id, kind) =>
        s"morphism ${id.value} with kind $kind cannot be compiled by this operator compiler"
      case MorphismCompilerNotFound(kind) =>
        s"no pullback compiler is registered for morphism kind $kind"
      case DuplicateMorphismCompiler(kind) =>
        s"a pullback compiler is already registered for morphism kind $kind"
      case MorphismCompilerKindMismatch(compiler, expected, actual) =>
        s"pullback compiler $compiler handles $expected but received $actual"
      case InvalidMorphismPlugin(id, detail) =>
        s"invalid morphism plugin ${id.value}: $detail"
      case UnsupportedPluginComposition(detail) =>
        s"unsupported staged plugin composition: $detail"
      case UnsupportedSurfaceSampling(label) =>
        s"unsupported surface sampling policy: $label"
      case SurfacePairMismatch(domain) =>
        s"surface pair does not match target domain ${domain.value}"
      case SurfaceSamplingGeometryMismatch(id) =>
        s"volume-to-surface morphism ${id.value} sampling geometry does not match its target domain"
      case SurfaceMappingGeometryMismatch(id) =>
        s"surface morphism ${id.value} vertex mapping does not match its source and target domains"
      case InvalidMixedPullback(detail) =>
        s"invalid mixed-domain pullback: $detail"
      case FieldDataUnavailable(label) =>
        s"field data is not materialized: $label"
      case FieldDomainMismatch(field, operator) =>
        s"field domain ${field.value} does not match operator source ${operator.value}"
      case FieldShapeMismatch(expectedRows, actualRows) =>
        s"field expected $expectedRows rows, got $actualRows"
      case FieldObservationMismatch(expected, actual) =>
        s"field expected $expected observations, got $actual"
      case FieldMaterializedShapeMismatch(expectedRows, actualRows, expectedObservations, actualObservations) =>
        s"materialized field expected ${expectedRows}x$expectedObservations values, got ${actualRows}x$actualObservations"
      case FieldSourceUnavailable(source, detail) =>
        s"field source ${source.value} is unavailable: $detail"
      case FieldSourceStale(source, expected, actual) =>
        s"field source ${source.value} is stale: expected $expected, got $actual"
      case FieldSourceDomainMismatch(source, expected, actual) =>
        s"field source ${source.value} targets ${actual.value}, expected ${expected.value}"
      case FieldSourceGeometryMismatch(source) =>
        s"field source ${source.value} geometry does not match its root domain"
      case FieldSourceSampleSpaceAdmission(source, cause) =>
        s"field source ${source.value} has an invalid sample space: ${cause.message}"
      case FieldSourceGridMismatch(source, cause) =>
        s"field source ${source.value} grid mismatch: ${cause.message}"
      case FieldSourceShapeMismatch(source, expectedRows, actualRows) =>
        s"field source ${source.value} expected $expectedRows rows, got $actualRows"
      case FieldSourceIndexOutOfBounds(axis, index, limit) =>
        s"field source $axis $index is out of bounds for size $limit"
      case DuplicateFieldSourceIndex(axis, index) =>
        s"field source request contains duplicate $axis $index"
      case FieldSourceRequestMismatch(source) =>
        s"field source ${source.value} returned a block for a different request"
      case FieldSourceBlockShapeMismatch(source, expectedRows, actualRows, expectedObservations, actualObservations) =>
        s"field source ${source.value} returned ${actualRows}x$actualObservations values, expected ${expectedRows}x$expectedObservations"
      case FieldSourceReadFailed(source, detail) =>
        s"field source ${source.value} read failed: $detail"
      case OperatorCacheMiss(key) =>
        s"operator cache miss: $key"
      case InvalidRoiRow(index, limit) =>
        s"ROI row $index out of bounds for target domain with $limit samples"
      case DuplicateRoiRow(index) =>
        s"ROI row selection contains duplicate row $index"
      case EmptyRoi =>
        "ROI row selection must be non-empty when provided"
      case CoordinateTransformFailed(reason) =>
        s"coordinate transform failed: $reason"
      case OperatorAssemblyFailed(reason) =>
        s"operator assembly failed: $reason"
