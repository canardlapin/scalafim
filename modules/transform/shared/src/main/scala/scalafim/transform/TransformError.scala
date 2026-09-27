package scalafim.transform

import image4s.geometry.GeometryError
import reframe4s.core.MapError
import scalafim.image.world.SpaceError

/** Failures of interpreting, composing, inverting or applying a world transform. */
enum TransformError derives CanEqual:
  /** The forward (source -> target) direction is not available: a dense warp without an inverse asset or estimate. */
  case NoForwardMap(transform: String)
  case Map(cause: MapError)
  case Geometry(cause: GeometryError)
  case Space(cause: SpaceError)
  case Io(cause: TransformIoError)
  case ContextFrameMismatch(role: String, expected: String, actual: String)
  case MissingContext(format: TransformFormat, needed: String)
  /** The supplied context contradicts what the file records (e.g. a reference volume of another shape). */
  case ContextMismatch(format: TransformFormat, reason: String)
  case UnsupportedConversion(from: String, to: TransformFormat, reason: String)
  case AmbiguousFnirtDefinition(reason: String)
  case Invalid(reason: String)

  def message: String =
    this match
      case NoForwardMap(t)                        => s"$t has no forward (source -> target) map; supply its inverse asset or a numerical inverse"
      case Map(cause)                             => cause.toString
      case Geometry(cause)                        => cause.message
      case Space(cause)                           => cause.message
      case Io(cause)                              => cause.message
      case ContextFrameMismatch(role, expected, actual) => s"$role grid lives in $actual, expected $expected"
      case MissingContext(format, needed)         => s"$format needs $needed"
      case ContextMismatch(format, reason)        => s"$format context does not match the file: $reason"
      case UnsupportedConversion(from, to, reason) => s"cannot express $from as $to: $reason"
      case AmbiguousFnirtDefinition(reason)       => s"cannot tell whether the FNIRT field is relative or absolute: $reason; state it explicitly"
      case Invalid(reason)                        => reason
