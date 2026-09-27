package scalafim.atlas

import image4s.geometry.GeometryError
import scalafim.spatial.SpatialError

enum AtlasError:
  case EmptyAtlas
  case DuplicateRegionIds(ids: Vector[RegionId])
  case MissingRegionId(id: RegionId)
  case MissingPayloadRegionIds(ids: Vector[RegionId])
  case UnknownAtlas(name: String, available: Vector[String])
  case UnknownSpace(space: AnySpaceId)
  case SpaceKindMismatch(space: AnySpaceId, expected: SpaceKindTag, actual: SpaceKindTag)
  case NoTransformRoute(from: AnySpaceId, to: AnySpaceId)
  case TransformNotExecutable(from: AnySpaceId, to: AnySpaceId, reason: String)
  case TransformGraph(cause: SpatialError)
  case GridWorldMismatch(role: String, space: AnySpaceId, detail: String)
  case SpaceMismatch(expected: Vector[Int], actual: Vector[Int])
  case ExactGridRequired(expected: String, actual: String)
  case Geometry(cause: GeometryError)
  case InvalidQuery(detail: String)
  case InvalidCoordinate(detail: String)
  case InvalidRegionMetadata(detail: String)
  case InvalidAlignment(detail: String)
  case InvalidReduction(detail: String)

  def message: String =
    this match
      case EmptyAtlas =>
        "atlas must contain at least one region"
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
      case InvalidReduction(detail) =>
        detail
