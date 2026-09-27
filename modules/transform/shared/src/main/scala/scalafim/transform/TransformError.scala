package scalafim.transform

import image4s.geometry.GeometryError
import reframe4s.core.MapError
import reframe4s.field.{CompositionError, InversionError, TopologyAssessmentError}
import reframe4s.resample.ResamplingError
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

  /** A well-formed file whose meaning under the toolkit's convention has no oracle yet: refused rather than guessed. */
  case UnqualifiedConvention(format: TransformFormat, reason: String)
  case Invalid(reason: String)

  /** `CoordinateBoundaryPolicy.HoldBorderDisplacement` (ITK's half-voxel border hold) asked of a format whose own tool
    * does not extend its field that way: FNIRT fields and coefficients, AFNI 3dQwarp, X5.
    */
  case ItkBorderHoldUnsupported(format: TransformFormat)

  /** Materializing a pullback on a lattice failed; `RejectedPoints` carries the full coverage report. */
  case Composition(cause: CompositionError)

  /** Numerical inversion failed; `GatesFailed` carries the complete residual evidence. It is never a degraded success. */
  case Inversion(cause: InversionError)

  /** A Jacobian-determinant field could not be evaluated. */
  case Determinant(cause: TopologyAssessmentError)

  /** Compiling or running a (modulated) resampling plan failed. */
  case Resampling(cause: ResamplingError)

  /** The operation needs a dense pullback; materialize the transform on a lattice first. */
  case NeedsMaterialization(transform: String)

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
      case UnqualifiedConvention(format, reason)  => s"$format: $reason; this reading is not yet qualified against the native tool"
      case Invalid(reason)                        => reason
      case ItkBorderHoldUnsupported(format)       =>
        s"$format: HoldBorderDisplacement reproduces ITK's half-voxel border hold, which this format's own tool does not use; choose Reject, Constant or PreserveSource"
      case Composition(cause)                     => cause.message
      case Inversion(cause)                       => cause.message
      case Determinant(cause)                     => cause.message
      case Resampling(cause)                      => cause.message
      case NeedsMaterialization(t)                => s"$t has no dense pullback; materialize it on a target lattice first"
