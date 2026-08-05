package scalafim.fmri.fit

import gale.linalg.LinAlgError
import scalafim.fmri.design.RankPreviewUnavailableReason
import scalafim.fmri.model.FitEngine

enum FitError:
  case EmptyDesign
  case EmptyResponse
  case RowMismatch(designRows: Int, responseRows: Int)
  case NonFiniteInput(component: String)
  case SingularDesign(cause: LinAlgError)
  case RankDeficientDesign(report: RankDiagnostics)
  case StructuralRankDeficientDesign(report: StructuralRankReport)
  case DesignRankPreviewUnavailable(reason: RankPreviewUnavailableReason)
  case UnsupportedLeastSquaresPolicy(detail: String)
  case UnsupportedVolumeWeighting(detail: String)
  case InvalidVolumeWeights(detail: String)
  case UnsupportedEngine(engine: String)
  case NonDenseFitResult(engine: FitEngine)
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
  case TrialReadoutFailed(cause: LinAlgError)
  case UnknownContrastColumn(columnName: String)
  case EmptyContrast(name: String)
  case NonEstimableContrast(name: String, detail: String)
  case StructuralHypothesisFailure(name: String, kind: StructuralHypothesisErrorKind, detail: String)
  case MissingStructuralIdentity(component: String)
  case HypothesisDesignMismatch(name: String, expectedFingerprint: String, actualFingerprint: String)
  case HypothesisCoefficientAxisMismatch(
      name: String,
      expectedFingerprint: String,
      actualFingerprint: String,
      expectedColumns: Vector[String],
      actualColumns: Vector[String]
  )
  case IncompatibleFitBlocks(detail: String)
  case ChunkFailed(chunkOrdinal: Int, cause: FitError)
  case UnsupportedRobust(detail: String)
  case FixedEffectsIncompatible(detail: String)
  case FixedEffectsContributionFailure(runIndex: Int, voxelIndex: Int, detail: String)

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
        s"design matrix is singular or ill-conditioned: ${cause.getMessage}"
      case RankDeficientDesign(report) =>
        val aliases = report.aliasedPredictors.mkString(", ")
        s"design rank ${report.numericalRank}/${report.predictorCount} under tolerance ${report.tolerance}; aliased predictors: [$aliases]"
      case StructuralRankDeficientDesign(report) =>
        val aliases = report.aliasedColumnIds.map(_.value).mkString(", ")
        s"design rank ${report.numericalRank}/${report.predictorCount} under ${report.toleranceConvention} tolerance ${report.tolerance}; aliased structural columns: [$aliases]"
      case DesignRankPreviewUnavailable(reason) =>
        s"compiled design rank preview is unavailable: $reason"
      case UnsupportedLeastSquaresPolicy(detail) =>
        s"unsupported least-squares policy: $detail"
      case UnsupportedVolumeWeighting(detail) =>
        s"unsupported volume-weighting policy: $detail"
      case InvalidVolumeWeights(detail) =>
        s"invalid volume weights: $detail"
      case UnsupportedEngine(engine) =>
        s"fit engine is not executable in scalafim-fmri-fit yet: $engine"
      case NonDenseFitResult(engine) =>
        s"fit engine $engine does not produce a dense fit result"
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
      case TrialReadoutFailed(cause) =>
        s"trial readout application failed: ${cause.getMessage}"
      case UnknownContrastColumn(columnName) =>
        s"contrast references unknown design column: $columnName"
      case EmptyContrast(name) =>
        s"contrast '$name' has no non-zero weights"
      case NonEstimableContrast(name, detail) =>
        s"contrast '$name' is not estimable: $detail"
      case StructuralHypothesisFailure(name, kind, detail) =>
        s"structural hypothesis '$name' failed (${kind.toString}): $detail"
      case MissingStructuralIdentity(component) =>
        s"structural identity is required for $component"
      case HypothesisDesignMismatch(name, expectedFingerprint, actualFingerprint) =>
        s"hypothesis '$name' was compiled for design $expectedFingerprint but the result carries $actualFingerprint"
      case HypothesisCoefficientAxisMismatch(name, expectedFingerprint, actualFingerprint, expectedColumns, actualColumns) =>
        s"hypothesis '$name' has coefficient axis ${expectedColumns.mkString("[", ", ", "]")} but the result carries ${actualColumns.mkString("[", ", ", "]")} for design $actualFingerprint (expected $expectedFingerprint)"
      case IncompatibleFitBlocks(detail) =>
        s"fit blocks cannot be merged: $detail"
      case ChunkFailed(chunkOrdinal, cause) =>
        s"fit chunk $chunkOrdinal failed: ${cause.message}"
      case UnsupportedRobust(detail) =>
        s"unsupported robust least-squares configuration: $detail"
      case FixedEffectsIncompatible(detail) =>
        s"fixed-effects contributions are incompatible: $detail"
      case FixedEffectsContributionFailure(runIndex, voxelIndex, detail) =>
        s"fixed-effects contribution failed for run $runIndex voxel $voxelIndex: $detail"
