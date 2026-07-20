package scalafim.spatial

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
  case Cache

enum SpatialError:
  case EmptyIdentifier(label: String)
  case NonPositiveDimension(label: String, value: Int)
  case EmptyHybrid
  case DuplicatePartName(name: PartName)
  case NegativeOffset(part: PartName, offset: Int)
  case MaskSpaceMismatch(label: String)
  case DomainKindMismatch(id: DomainId, expected: DomainKind, actual: DomainKind)
  case LatentDimensionMismatch(id: DomainId, spaceDim: Int, geometryDim: Int)
  case UnsupportedGeometry(label: String)
  case InvalidCost(value: Double)
  case InvalidQuality(label: String, value: Double)
  case IdentityMorphismDomainMismatch(source: DomainId, target: DomainId)
  case IncompatibleMorphismKind(kind: MorphismKind, source: DomainId, sourceKind: DomainKind, target: DomainId, targetKind: DomainKind)
  case DuplicateDomain(id: DomainId)
  case DuplicateMorphism(id: MorphismId)
  case GraphAssemblyFailed(reason: String)
  case DomainNotFound(id: DomainId)
  case MorphismDomainMissing(id: MorphismId, domain: DomainId)
  case NoPath(source: DomainId, target: DomainId)
  case EmptyPath
  case DisconnectedPath(previous: DomainId, next: DomainId)
  case NonInvertibleMorphism(id: MorphismId)
  case InvalidAffineCoordinateMap(label: String)
  case MissingCoordinateMap(id: MorphismId)
  case NonVolumeDomain(id: DomainId)
  case NonSurfaceDomain(id: DomainId)
  case UnsupportedMorphismForCompilation(id: MorphismId, kind: MorphismKind)
  case UnsupportedSurfaceSampling(label: String)
  case SurfacePairMismatch(domain: DomainId)
  case FieldDataUnavailable(label: String)
  case FieldDomainMismatch(field: DomainId, operator: DomainId)
  case FieldShapeMismatch(expectedRows: Int, actualRows: Int)
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
      case EmptyHybrid | DuplicatePartName(_) | MaskSpaceMismatch(_) | UnsupportedGeometry(_) =>
        SpatialErrorReason.Geometry
      case DomainKindMismatch(_, _, _) =>
        SpatialErrorReason.DomainKind
      case InvalidCost(_) | InvalidQuality(_, _) | IdentityMorphismDomainMismatch(_, _) | IncompatibleMorphismKind(_, _, _, _, _) =>
        SpatialErrorReason.MorphismCompatibility
      case DuplicateDomain(_) | DuplicateMorphism(_) | GraphAssemblyFailed(_) | DomainNotFound(_) | MorphismDomainMissing(_, _) =>
        SpatialErrorReason.Graph
      case NoPath(_, _) | EmptyPath | DisconnectedPath(_, _) | NonInvertibleMorphism(_) =>
        SpatialErrorReason.Route
      case InvalidAffineCoordinateMap(_) | MissingCoordinateMap(_) | CoordinateTransformFailed(_) =>
        SpatialErrorReason.CoordinateMap
      case NonVolumeDomain(_) | NonSurfaceDomain(_) | UnsupportedMorphismForCompilation(_, _) | UnsupportedSurfaceSampling(_) | SurfacePairMismatch(_) | OperatorAssemblyFailed(_) =>
        SpatialErrorReason.Operator
      case InvalidRoiRow(_, _) | DuplicateRoiRow(_) | EmptyRoi =>
        SpatialErrorReason.RowSelection
      case FieldDataUnavailable(_) | FieldDomainMismatch(_, _) | FieldShapeMismatch(_, _) =>
        SpatialErrorReason.Field
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
      case GraphAssemblyFailed(reason) =>
        s"spatial route graph assembly failed: $reason"
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
      case InvalidAffineCoordinateMap(label) =>
        s"$label coordinate map must be a finite 4x4 affine matrix"
      case MissingCoordinateMap(id) =>
        s"morphism ${id.value} does not carry an executable coordinate map"
      case NonVolumeDomain(id) =>
        s"domain ${id.value} is not a volume domain"
      case NonSurfaceDomain(id) =>
        s"domain ${id.value} is not a surface domain"
      case UnsupportedMorphismForCompilation(id, kind) =>
        s"morphism ${id.value} with kind $kind cannot be compiled by this operator compiler"
      case UnsupportedSurfaceSampling(label) =>
        s"unsupported surface sampling policy: $label"
      case SurfacePairMismatch(domain) =>
        s"surface pair does not match target domain ${domain.value}"
      case FieldDataUnavailable(label) =>
        s"field data is not materialized: $label"
      case FieldDomainMismatch(field, operator) =>
        s"field domain ${field.value} does not match operator source ${operator.value}"
      case FieldShapeMismatch(expectedRows, actualRows) =>
        s"field expected $expectedRows rows, got $actualRows"
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
