package scalafim.fmri.fit

import scalafim.linalg.LinearAlgebraError

enum FitError:
  case EmptyDesign
  case EmptyResponse
  case RowMismatch(designRows: Int, responseRows: Int)
  case NonFiniteInput(component: String)
  case SingularDesign(cause: LinearAlgebraError)
  case UnsupportedLeastSquaresPolicy(detail: String)
  case UnsupportedEngine(engine: String)
  case InvalidFitAxis(axis: String, detail: String)
  case NonPositiveResidualDegreesOfFreedom(value: Int)
  case EmptyRunPartition(runIndex: Int)
  case RunwiseFitFailed(runIndex: Int, cause: FitError)
  case UnsupportedAutocorrelation(detail: String)
  case MissingLssTrialTerm
  case UnknownLssTrialTerm(term: String)
  case AmbiguousLssTrialTerms(candidates: Vector[String])
  case UnsupportedLssDesign(detail: String)
  case NonEstimableLssTrials(trials: Vector[String])
  case UnknownContrastColumn(columnName: String)
  case EmptyContrast(name: String)
  case NonEstimableContrast(name: String, detail: String)
  case IncompatibleFitBlocks(detail: String)

  def message: String =
    this match
      case EmptyDesign =>
        "design matrix must have at least one row and one column"
      case EmptyResponse =>
        "response block must have at least one row and one column"
      case RowMismatch(designRows, responseRows) =>
        s"design rows $designRows do not match response rows $responseRows"
      case NonFiniteInput(component) =>
        s"$component contains non-finite values"
      case SingularDesign(cause) =>
        s"design matrix is singular or ill-conditioned: ${cause.message}"
      case UnsupportedLeastSquaresPolicy(detail) =>
        s"unsupported least-squares policy: $detail"
      case UnsupportedEngine(engine) =>
        s"fit engine is not executable in scalafim-fmri-fit yet: $engine"
      case InvalidFitAxis(axis, detail) =>
        s"invalid $axis: $detail"
      case NonPositiveResidualDegreesOfFreedom(value) =>
        s"residual degrees of freedom must be positive for inference-ready fit results; got $value"
      case EmptyRunPartition(runIndex) =>
        s"run $runIndex has no selected rows"
      case RunwiseFitFailed(runIndex, cause) =>
        s"run $runIndex fit failed: ${cause.message}"
      case UnsupportedAutocorrelation(detail) =>
        s"unsupported autocorrelation configuration: $detail"
      case MissingLssTrialTerm =>
        "LeastSquaresSeparate requires a trialwise event term"
      case UnknownLssTrialTerm(term) =>
        s"LeastSquaresSeparate references unknown trialwise event term: $term"
      case AmbiguousLssTrialTerms(candidates) =>
        s"LeastSquaresSeparate found multiple trialwise event terms; configure lss.trialTerm as one of: ${candidates.mkString(", ")}"
      case UnsupportedLssDesign(detail) =>
        s"unsupported LSS design: $detail"
      case NonEstimableLssTrials(trials) =>
        s"LSS trial regressors are not estimable after fixed-effect residualization: ${trials.mkString(", ")}"
      case UnknownContrastColumn(columnName) =>
        s"contrast references unknown design column: $columnName"
      case EmptyContrast(name) =>
        s"contrast '$name' has no non-zero weights"
      case NonEstimableContrast(name, detail) =>
        s"contrast '$name' is not estimable: $detail"
      case IncompatibleFitBlocks(detail) =>
        s"fit blocks cannot be merged: $detail"
