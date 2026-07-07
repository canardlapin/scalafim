package scalafim.surface

enum SurfaceError:
  case InvalidHemisphereTag(tag: Hemisphere)
  case MissingSurfaceDomain(owner: String)
  case DomainMismatch(left: SurfaceDomain, right: SurfaceDomain)
  case InvalidGeometry(reason: String)
  case InvalidTopology(reason: String)
  case InvalidField(reason: String)
  case InvalidLabels(reason: String)
  case InvalidParcel(reason: String)
  case ReadFailure(source: String, reason: String)

  def message: String =
    this match
      case InvalidHemisphereTag(tag) =>
        s"surface hemisphere ${tag.code} is an IO tag, not a usable cortical hemisphere"
      case MissingSurfaceDomain(owner) =>
        s"$owner does not carry a usable surface domain"
      case DomainMismatch(left, right) =>
        s"surface domains do not match: ${left.display} vs ${right.display}"
      case InvalidGeometry(reason) =>
        s"invalid surface geometry: $reason"
      case InvalidTopology(reason) =>
        s"invalid mesh topology: $reason"
      case InvalidField(reason) =>
        s"invalid surface field: $reason"
      case InvalidLabels(reason) =>
        s"invalid labeled surface: $reason"
      case InvalidParcel(reason) =>
        s"invalid surface parcel: $reason"
      case ReadFailure(source, reason) =>
        s"surface read failed for $source: $reason"

object SurfaceError:
  def reason(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
