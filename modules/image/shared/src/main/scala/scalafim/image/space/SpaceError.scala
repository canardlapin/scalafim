package scalafim.image.space

/** Failures of world-space identity, resolution, frame binding, and coordinate conventions. */
enum SpaceError derives CanEqual:
  case EmptyIdentifier(label: String)

  def message: String =
    this match
      case EmptyIdentifier(label) => s"$label identifier must be non-empty"
