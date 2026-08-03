package scalafim.fmri.mvpa.fit

import multivar.core.MultivarError

import scalafim.dataset.RunId
import scalafim.fmri.fit.{FitError, TrainingRunScope}
import scalafim.fmri.mvpa.{FeatureIndex, MvpaError}
import gale.linalg.LinAlgError

enum OneShotMvpaError:
  case EmptyRuns
  case DuplicateRuns(runIds: Vector[RunId])
  case TimepointMismatch(runId: RunId, readoutTimepoints: Int, responseTimepoints: Int)
  case FeatureAxisLengthMismatch(runId: RunId, axisLength: Int, responseFeatures: Int)
  case FeatureAxisMismatch(
      runId: RunId,
      expected: Vector[FeatureIndex],
      actual: Vector[FeatureIndex]
  )
  case InsufficientRunsForCrossValidation(actual: Int)
  case ReadoutFailure(runId: RunId, cause: FitError)
  case MvpaFailure(cause: MvpaError)
  case CompositionFailure(cause: LinAlgError)
  case InvalidGeometrySchedule(detail: String)
  case MissingFoldGeometry(runId: RunId, training: TrainingRunScope)
  case TemporalPreparationFailure(runId: RunId, cause: FitError)
  case CanonicalEffectFailure(cause: MultivarError)
  case NonIdentifiableHeldOutDirection(runId: RunId, multiplicity: Int)
  case NonPositiveHeldOutDenominator(runId: RunId, value: Double)
  case InvalidHeldOutRoot(runId: RunId, value: Double)
  case NonFiniteCrossRunNumerator(runId: RunId, value: Double)
  case NonPositiveCrossRunDenominator(runId: RunId, value: Double)
  case InvalidSignedCrossRunValue(value: Double)
  case InvalidSignedCrossRunStatistic(runId: RunId, value: Double)

  def message: String =
    this match
      case EmptyRuns =>
        "one-shot dataset must contain at least one run"
      case DuplicateRuns(runIds) =>
        s"one-shot dataset run ids must be unique; duplicates: ${runIds.map(_.value).mkString(", ")}"
      case TimepointMismatch(runId, readoutTimepoints, responseTimepoints) =>
        s"run ${runId.value} readout timepoints $readoutTimepoints do not match response timepoints $responseTimepoints"
      case FeatureAxisLengthMismatch(runId, axisLength, responseFeatures) =>
        s"run ${runId.value} feature-axis length $axisLength does not match response features $responseFeatures"
      case FeatureAxisMismatch(runId, expected, actual) =>
        val expectedIds = expected.map(_.value).mkString("[", ", ", "]")
        val actualIds = actual.map(_.value).mkString("[", ", ", "]")
        s"run ${runId.value} feature axis $actualIds does not match expected axis $expectedIds"
      case InsufficientRunsForCrossValidation(actual) =>
        s"leave-one-run-out cross-validation requires at least two runs; got $actual"
      case ReadoutFailure(runId, cause) =>
        s"run ${runId.value} trial readout failed: ${cause.message}"
      case MvpaFailure(cause) =>
        s"one-shot MVPA failed: ${cause.message}"
      case CompositionFailure(cause) =>
        s"one-shot operator composition failed: ${cause.getMessage}"
      case InvalidGeometrySchedule(detail) =>
        s"invalid canonical geometry schedule: $detail"
      case MissingFoldGeometry(runId, training) =>
        s"run ${runId.value} has no prepared contrast geometry for training runs ${training.runs.map(_.value).mkString("[", ", ", "]")}"
      case TemporalPreparationFailure(runId, cause) =>
        s"run ${runId.value} temporal preparation failed: ${cause.message}"
      case CanonicalEffectFailure(cause) =>
        s"canonical effect fit failed: ${cause.message}"
      case NonIdentifiableHeldOutDirection(runId, multiplicity) =>
        s"run ${runId.value} cannot be scored from a non-identifiable training root of multiplicity $multiplicity"
      case NonPositiveHeldOutDenominator(runId, value) =>
        s"run ${runId.value} held-out residual denominator must be positive and finite, got $value"
      case InvalidHeldOutRoot(runId, value) =>
        s"run ${runId.value} held-out canonical root must be finite and non-negative, got $value"
      case NonFiniteCrossRunNumerator(runId, value) =>
        s"run ${runId.value} signed cross-run numerator must be finite, got $value"
      case NonPositiveCrossRunDenominator(runId, value) =>
        s"run ${runId.value} signed cross-run denominator must be positive and finite, got $value"
      case InvalidSignedCrossRunValue(value) =>
        s"signed cross-run Rayleigh value must be finite, got $value"
      case InvalidSignedCrossRunStatistic(runId, value) =>
        s"run ${runId.value} signed cross-run Rayleigh statistic must be finite, got $value"
