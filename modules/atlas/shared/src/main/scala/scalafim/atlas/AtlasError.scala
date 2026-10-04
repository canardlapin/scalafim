package scalafim.atlas

import image4s.geometry.GeometryError
import scalafim.spatial.SpatialError
import scalafim.transform.TransformError

enum AtlasError:
  case InvalidSpaceId(detail: String)
  case EmptyAtlas
  case DuplicateRegionIds(ids: Vector[RegionId])
  case MissingRegionId(id: RegionId)
  case MissingPayloadRegionIds(ids: Vector[RegionId])
  case UnknownAtlas(name: String, available: Vector[String])
  case UnknownSpace(space: SpaceId)
  case SpaceKindMismatch(space: SpaceId, expected: SpaceKindTag, actual: SpaceKindTag)
  case NoTransformRoute(from: SpaceId, to: SpaceId)
  case TransformNotExecutable(from: SpaceId, to: SpaceId, reason: String)
  case TransformGraph(cause: SpatialError)
  case Transform(cause: TransformError)

  /** A template asset was read but is not admitted: an uninspected file, or one whose direction contradicts its name. */
  case TemplateAssetRefused(asset: String, reason: String)

  /** A template asset is in none of the searched caches; nothing is downloaded or substituted. */
  case TemplateAssetMissing(asset: String, searched: Vector[String])
  case GridWorldMismatch(role: String, space: SpaceId, detail: String)
  case SpaceMismatch(expected: Vector[Int], actual: Vector[Int])
  case ExactGridRequired(expected: String, actual: String)
  case Geometry(cause: GeometryError)
  case InvalidQuery(detail: String)
  case InvalidCoordinate(detail: String)
  case InvalidRegionMetadata(detail: String)
  case InvalidAlignment(detail: String)
  case Reduction(cause: AtlasReductionError)

  def message: String =
    this match
      case EmptyAtlas =>
        "atlas must contain at least one region"
      case InvalidSpaceId(detail) => detail
      case DuplicateRegionIds(ids) =>
        s"atlas region ids must be unique: ${ids.map(_.value).mkString(", ")}"
      case MissingRegionId(id) =>
        s"atlas payload is missing region id ${id.value}"
      case MissingPayloadRegionIds(ids) =>
        s"label volume is missing region ids: ${ids.map(_.value).mkString(", ")}"
      case UnknownAtlas(name, available) =>
        s"unknown atlas '$name'; available atlases: ${available.mkString(", ")}"
      case UnknownSpace(space) =>
        s"unknown space '${space.value}'"
      case SpaceKindMismatch(space, expected, actual) =>
        s"space '${space.value}' has kind $actual; expected $expected"
      case NoTransformRoute(from, to) =>
        s"no transform route found from '${from.value}' to '${to.value}'"
      case TransformNotExecutable(from, to, reason) =>
        s"transform route from '${from.value}' to '${to.value}' is not executable: $reason"
      case TransformGraph(cause) =>
        s"transform manifest does not form a valid spatial graph: ${cause.message}"
      case Transform(cause) =>
        cause.message
      case TemplateAssetRefused(asset, reason) =>
        s"template asset $asset is refused: $reason"
      case TemplateAssetMissing(asset, searched) =>
        s"template asset $asset is not cached under ${searched.mkString(", ")}"
      case GridWorldMismatch(role, space, detail) =>
        s"the $role grid is not in the world space of '${space.value}': $detail"
      case SpaceMismatch(expected, actual) =>
        s"expected spatial dimensions ${expected.mkString("x")} but got ${actual.mkString("x")}"
      case ExactGridRequired(expected, actual) =>
        s"exact atlas grid required; expected $expected but got $actual"
      case Geometry(cause) =>
        cause.message
      case InvalidQuery(detail) =>
        detail
      case InvalidCoordinate(detail) =>
        detail
      case InvalidRegionMetadata(detail) =>
        detail
      case InvalidAlignment(detail) =>
        detail
      case Reduction(cause) =>
        cause.message
