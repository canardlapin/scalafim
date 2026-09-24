package scalafim.image.world

/** Failures of world-space identity, resolution, frame binding, and coordinate conventions. */
enum SpaceError derives CanEqual:
  case EmptyIdentifier(label: String)
  case InvalidGeometry(reason: String)
  case UnrecognisedWorldSpaceId(text: String)
  case NotAWorldFrame(reason: String)
  case FrameBinding(reason: String)

  def message: String =
    this match
      case EmptyIdentifier(label)         => s"$label identifier must be non-empty"
      case InvalidGeometry(reason)        => s"invalid acquisition geometry: $reason"
      case UnrecognisedWorldSpaceId(text) => s"not a ScalaFIM world-space identifier: $text"
      case NotAWorldFrame(reason)         => s"frame is not a RAS-millimetre D3 world frame: $reason"
      case FrameBinding(reason)           => s"frame binding failed: $reason"
