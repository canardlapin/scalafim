package scalafim.atlas

enum AtlasError:
  case EmptyAtlas
  case DuplicateRegionIds(ids: Vector[RegionId])
  case MissingRegionId(id: RegionId)
  case UnknownAtlas(name: String, available: Vector[String])
  case UnknownSpace(space: SpaceId)
  case NoTransformRoute(from: SpaceId, to: SpaceId)
  case SpaceMismatch(expected: Vector[Int], actual: Vector[Int])
  case InvalidCoordinate(detail: String)
  case InvalidRegionMetadata(detail: String)

  def message: String =
    this match
      case EmptyAtlas =>
        "atlas must contain at least one region"
      case DuplicateRegionIds(ids) =>
        s"atlas region ids must be unique: ${ids.map(_.value).mkString(", ")}"
      case MissingRegionId(id) =>
        s"atlas payload is missing region id ${id.value}"
      case UnknownAtlas(name, available) =>
        s"unknown atlas '$name'; available atlases: ${available.mkString(", ")}"
      case UnknownSpace(space) =>
        s"unknown space '${space.value}'"
      case NoTransformRoute(from, to) =>
        s"no transform route found from '${from.value}' to '${to.value}'"
      case SpaceMismatch(expected, actual) =>
        s"expected spatial dimensions ${expected.mkString("x")} but got ${actual.mkString("x")}"
      case InvalidCoordinate(detail) =>
        detail
      case InvalidRegionMetadata(detail) =>
        detail
