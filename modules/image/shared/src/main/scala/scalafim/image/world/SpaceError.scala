package scalafim.image.world

/** Failures of world-space identity, resolution, frame binding, and coordinate conventions. */
enum SpaceError derives CanEqual:
  case EmptyIdentifier(label: String)
  case InvalidGeometry(reason: String)
  case FslHandednessConflict(qformCode: Int, qformDeterminant: Double, sformCode: Int, sformDeterminant: Double)
  case UnrecognisedWorldSpaceId(text: String)
  case NotAWorldFrame(reason: String)
  case FrameBinding(reason: String)
  case UnknownXformCode(code: String)
  case AmbiguousTemplate(reason: String)
  case ConflictingEvidence(first: String, second: String)
  case MissingNativeContext(reason: String)
  case NoWorldSpace(reason: String)

  def message: String =
    this match
      case EmptyIdentifier(label)         => s"$label identifier must be non-empty"
      case InvalidGeometry(reason)        => s"invalid acquisition geometry: $reason"
      case FslHandednessConflict(qcode, qdet, scode, sdet) =>
        s"FSL qform (code $qcode, determinant $qdet) and sform (code $scode, determinant $sdet) disagree in handedness"
      case UnrecognisedWorldSpaceId(text) => s"not a ScalaFIM world-space identifier: $text"
      case NotAWorldFrame(reason)         => s"frame is not a RAS-millimetre D3 world frame: $reason"
      case FrameBinding(reason)           => s"frame binding failed: $reason"
      case UnknownXformCode(code)         => s"unknown NIfTI/GIFTI xform code: $code"
      case AmbiguousTemplate(reason)      => s"ambiguous template space: $reason"
      case ConflictingEvidence(a, b)      => s"conflicting world-space evidence: $a vs $b"
      case MissingNativeContext(reason)   => s"$reason, but no dataset/subject/reference context was supplied"
      case NoWorldSpace(reason)           => s"no world-space identity: $reason"
