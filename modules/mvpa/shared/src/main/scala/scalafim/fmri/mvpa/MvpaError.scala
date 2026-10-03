package scalafim.fmri.mvpa

import gale.linalg.LinAlgError

/** Numerical/storage failures; scientific execution and evidence errors live
  * in their owning typed methods, not a generic ROI engine.
  */
enum MvpaError:
  case EmptyResponse
  case ResponseLengthMismatch(expected: Int, actual: Int)
  case SingleClassResponse
  case MissingFeature(index: FeatureIndex)
  case PredictionIndexOutOfBounds(index: Int, samples: Int)
  case PatternCopyBudgetExceeded(requiredCells: BigInt, allowedCells: Long)
  case MatrixShapeMismatch(detail: String)
  case InvalidClassMembership(detail: String)
  case InvalidPatternOperatorInput(detail: String)
  case PatternOperatorFailed(cause: LinAlgError)
  case OperatorRidgeFailed(cause: OperatorRidgeError)
  case InvalidRdmInput(detail: String)
  case InvalidClassifierInput(detail: String)
  case InvalidFeatureModelInput(detail: String)
  case ClassifierFitFailed(classifier: String, detail: String)

  def message: String = this match
    case EmptyResponse => "response must contain at least one sample"
    case ResponseLengthMismatch(expected, actual) => s"response length mismatch: expected $expected, got $actual"
    case SingleClassResponse => "categorical response must contain at least two classes"
    case MissingFeature(index) => s"storage references missing feature ${index.value}"
    case PredictionIndexOutOfBounds(index, samples) => s"prediction index $index out of bounds for $samples samples"
    case PatternCopyBudgetExceeded(required, allowed) => s"dense copy needs $required cells; ceiling is $allowed"
    case MatrixShapeMismatch(detail) => detail
    case InvalidClassMembership(detail) => detail
    case InvalidPatternOperatorInput(detail) => detail
    case PatternOperatorFailed(cause) => s"pattern operator failed: ${cause.getMessage}"
    case OperatorRidgeFailed(cause) => s"operator ridge failed: ${cause.message}"
    case InvalidRdmInput(detail) => detail
    case InvalidClassifierInput(detail) => detail
    case InvalidFeatureModelInput(detail) => detail
    case ClassifierFitFailed(classifier, detail) => s"$classifier fit failed: $detail"
