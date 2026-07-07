package scalafim.fmri.mvpa

enum MvpaError:
  case EmptyResponse
  case ResponseLengthMismatch(expected: Int, actual: Int)
  case SingleClassResponse
  case EmptyFoldPlan
  case EmptyFold(id: String)
  case FoldIndexOutOfBounds(id: String, index: Int, samples: Int)
  case FoldTrainTestOverlap(id: String)
  case InvalidSampleAxis(detail: String)
  case MissingFoldPlan(analysis: String)
  case EmptyFeatureSet(id: RoiId)
  case DuplicateFeatureIndices(id: RoiId)
  case InvalidFeatureSetPlan(detail: String)
  case FeatureIndexOutOfBounds(id: RoiId, index: FeatureIndex)
  case MissingFeature(id: RoiId, index: FeatureIndex)
  case TooFewFeatures(id: RoiId, count: Int, required: Int)
  case MatrixShapeMismatch(detail: String)
  case InvalidRdmInput(detail: String)
  case InvalidClassifierInput(detail: String)
  case InvalidFeatureModelInput(detail: String)
  case ClassifierFitFailed(classifier: String, detail: String)
  case AnalysisFailed(id: RoiId, reason: String)

  def message: String =
    this match
      case EmptyResponse =>
        "response must contain at least one sample"
      case ResponseLengthMismatch(expected, actual) =>
        s"response length mismatch: expected $expected, got $actual"
      case SingleClassResponse =>
        "categorical response must contain at least two classes"
      case EmptyFoldPlan =>
        "fold plan must contain at least one fold"
      case EmptyFold(id) =>
        s"fold '$id' must have non-empty train and test sets"
      case FoldIndexOutOfBounds(id, index, samples) =>
        s"fold '$id' index $index out of bounds for $samples samples"
      case FoldTrainTestOverlap(id) =>
        s"fold '$id' has overlapping train and test samples"
      case InvalidSampleAxis(detail) =>
        detail
      case MissingFoldPlan(analysis) =>
        s"$analysis requires a fold plan"
      case EmptyFeatureSet(id) =>
        s"feature set ${id.value} must contain at least one feature"
      case DuplicateFeatureIndices(id) =>
        s"feature set ${id.value} contains duplicate feature indices"
      case InvalidFeatureSetPlan(detail) =>
        detail
      case FeatureIndexOutOfBounds(id, index) =>
        s"feature set ${id.value} contains out-of-bounds feature index ${index.value}"
      case MissingFeature(id, index) =>
        s"feature set ${id.value} references missing feature ${index.value}"
      case TooFewFeatures(id, count, required) =>
        s"feature set ${id.value} has $count feature(s), required $required"
      case MatrixShapeMismatch(detail) =>
        detail
      case InvalidRdmInput(detail) =>
        detail
      case InvalidClassifierInput(detail) =>
        detail
      case InvalidFeatureModelInput(detail) =>
        detail
      case ClassifierFitFailed(classifier, detail) =>
        s"$classifier fit failed: $detail"
      case AnalysisFailed(id, reason) =>
        s"analysis failed for ROI ${id.value}: $reason"
