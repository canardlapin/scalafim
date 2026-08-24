package scalafim.fmri.threshold

import image4s.geometry.GeometryError

enum ThresholdError:
  case InvalidAlpha(value: Double)
  case InvalidQValue(value: Double)
  case InvalidKappa(value: Double)
  case InvalidDegreesOfFreedom(value: Double)
  case InvalidAdjustedPValue(value: Double)
  case InvalidPermutationCount(value: Int)
  case EmptyMask
  case EmptyRegion
  case DuplicateRegionIndex(index: Int)
  case ShapeMismatch(what: String, expected: String, actual: String)
  case Geometry(cause: GeometryError)
  case NonFiniteData(what: String)
  case NegativeUnsignedEvidence(index: Int, value: Double)
  case IncompatibleAlternative(alternative: ThresholdAlternative, orientation: EvidenceOrientation)
  case NegativePrior(index: Int, value: Double)
  case ZeroPriorMass
  case IndexOutOfBounds(index: Int, length: Int)
  case NullMatrixShape(rows: Int, cols: Int, expectedCols: Int)
  case InvalidArgument(name: String, reason: String)

  def message: String =
    this match
      case InvalidAlpha(value) =>
        s"alpha must be finite and in (0, 1), got $value"
      case InvalidQValue(value) =>
        s"q must be finite and in (0, 1), got $value"
      case InvalidKappa(value) =>
        s"kappa must be finite and positive, got $value"
      case InvalidDegreesOfFreedom(value) =>
        s"degrees of freedom must be finite and positive, got $value"
      case InvalidAdjustedPValue(value) =>
        s"adjusted p-value must be finite and in [0, 1], got $value"
      case InvalidPermutationCount(value) =>
        s"permutation count must be positive, got $value"
      case EmptyMask =>
        "analysis mask is empty"
      case EmptyRegion =>
        "region is empty"
      case DuplicateRegionIndex(index) =>
        s"region contains duplicate compact index $index"
      case ShapeMismatch(what, expected, actual) =>
        s"$what shape mismatch: expected $expected, got $actual"
      case Geometry(cause) =>
        cause.message
      case NonFiniteData(what) =>
        s"$what contains non-finite values"
      case NegativeUnsignedEvidence(index, value) =>
        s"unsigned evidence at index $index is negative: $value"
      case IncompatibleAlternative(alternative, orientation) =>
        s"alternative $alternative is incompatible with $orientation evidence"
      case NegativePrior(index, value) =>
        s"prior weight at index $index is negative: $value"
      case ZeroPriorMass =>
        "prior weights have zero mass"
      case IndexOutOfBounds(index, length) =>
        s"index $index out of bounds for length $length"
      case NullMatrixShape(rows, cols, expectedCols) =>
        s"null matrix has shape ${rows}x$cols, expected *x$expectedCols with at least one row"
      case InvalidArgument(name, reason) =>
        s"$name is invalid: $reason"
