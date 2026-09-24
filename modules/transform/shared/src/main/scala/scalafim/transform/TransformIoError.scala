package scalafim.transform

/** Failures decoding or encoding a toolkit transform file. */
enum TransformIoError derives CanEqual:
  case Malformed(format: String, reason: String)
  case Unsupported(format: UnsupportedFormat, reason: String)
  case UnsupportedItkTransform(transformType: String)
  case UnsupportedNifti(reason: String)
  case Undetectable(reason: String)
  case Ambiguous(candidates: Vector[TransformFormat], reason: String)
  case WrongSource(format: TransformFormat, expected: String)

  def message: String =
    this match
      case Malformed(format, reason)        => s"malformed $format: $reason"
      case Unsupported(format, reason)      => s"$format is not supported: $reason"
      case UnsupportedItkTransform(kind)    => s"ITK transform type $kind is not supported"
      case UnsupportedNifti(reason)         => s"unsupported NIfTI transform container: $reason"
      case Undetectable(reason)             => s"cannot identify the transform format: $reason"
      case Ambiguous(candidates, reason)    => s"transform format is ambiguous between ${candidates.mkString(", ")}: $reason"
      case WrongSource(format, expected)    => s"$format codec expects $expected"
