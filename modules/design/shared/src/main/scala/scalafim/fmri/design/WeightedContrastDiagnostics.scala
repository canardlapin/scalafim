package scalafim.fmri.design

import scalafim.fmri.hrf.linalg.Mat

/** Combination is a scientific choice: fixed effects combines estimable
  * run-level contrasts; concatenation sums nuisance-residualized task information.
  */
enum RunContrastCombination:
  case FixedEffects
  case Concatenated(taskColumns: Vector[ColumnId])

final case class WeightedTDiagnostic(
    contrast: ColumnContrast,
    variance: Option[Double],
    contributingRuns: Vector[Int],
    excludedRuns: Vector[Int],
    combination: RunContrastCombination,
    residualSigmas: Vector[Double]
)

final case class WeightedFDiagnostic(
    contrast: FColumnContrast,
    worstDirectionSe: Option[Double],
    effectiveDf: Int,
    contributingRuns: Vector[Int],
    excludedRuns: Vector[Int],
    combination: RunContrastCombination,
    residualSigmas: Vector[Double]
)

/** Uncertainty conditional on caller-supplied, positive run-specific residual
  * standard deviations. These methods do not estimate sigma or noise correlation.
  * Run indices in receipts are zero-based positions in the supplied vector.
  */
object WeightedContrastDiagnostics:
  def t(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: ColumnContrast,
      residualSigmas: Vector[Double],
      combination: RunContrastCombination
  ): Either[DesignDiagnosticsError, WeightedTDiagnostic] =
    scaled(runs, residualSigmas).flatMap: weighted =>
      combination match
        case RunContrastCombination.FixedEffects =>
          val result = ContrastDiagnostics.fixedEffectsT(weighted, contrast)
          checked(result.varianceOverSigmaSquared).map: variance =>
            WeightedTDiagnostic(contrast, variance, result.contributingRuns, result.excludedRuns.map(_._1), combination, residualSigmas)
        case RunContrastCombination.Concatenated(taskColumns) =>
          ContrastDiagnostics.concatenatedTaskT(weighted, taskColumns, contrast).flatMap: result =>
            checked(result.varianceOverSigmaSquared).map: variance =>
              WeightedTDiagnostic(contrast, variance, result.contributingRuns, result.excludedRuns, combination, residualSigmas)

  def f(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: FColumnContrast,
      residualSigmas: Vector[Double],
      combination: RunContrastCombination
  ): Either[DesignDiagnosticsError, WeightedFDiagnostic] =
    scaled(runs, residualSigmas).flatMap: weighted =>
      combination match
        case RunContrastCombination.FixedEffects =>
          ContrastDiagnostics.fixedEffectsF(weighted, contrast).flatMap: result =>
            checked(result.worstDirectionSeOverSigma).map: se =>
              WeightedFDiagnostic(contrast, se, result.effectiveDf, result.contributingRuns, result.excludedRuns.map(_._1), combination, residualSigmas)
        case RunContrastCombination.Concatenated(taskColumns) =>
          ContrastDiagnostics.concatenatedTaskF(weighted, taskColumns, contrast).flatMap: result =>
            checked(result.worstDirectionSeOverSigma).map: se =>
              WeightedFDiagnostic(contrast, se, result.effectiveDf, result.contributingRuns, result.excludedRuns, combination, residualSigmas)

  private def checked(value: Option[Double]): Either[DesignDiagnosticsError, Option[Double]] =
    if value.exists(number => !number.isFinite || number <= 0.0) then Left(DesignDiagnosticsError.NumericalFailure("weighted contrast uncertainty is outside the finite positive range"))
    else Right(value)

  private def scaled(runs: Vector[ContrastDiagnosticsResult], sigmas: Vector[Double]): Either[DesignDiagnosticsError, Vector[ContrastDiagnosticsResult]] =
    if sigmas.length != runs.length then Left(DesignDiagnosticsError.NumericalFailure("provide exactly one residual sigma per run"))
    else if sigmas.exists(value => !value.isFinite || value <= 0.0) then Left(DesignDiagnosticsError.NumericalFailure("residual sigmas must be finite and strictly positive"))
    else if runs.map(_.rankPolicy).distinct.length > 1 then Left(DesignDiagnosticsError.NumericalFailure("combined runs must use the same rank policy"))
    else
      runs.zip(sigmas).foldLeft[Either[DesignDiagnosticsError, Vector[ContrastDiagnosticsResult]]](Right(Vector.empty)): (acc, pair) =>
        val (run, sigma) = pair
        acc.flatMap: previous =>
          val geometry = run.geometry
          val values = geometry.matrix.data.map(_ / sigma)
          val singular = geometry.singular.map(_ / sigma)
          val variances = run.t.map: diagnostic =>
            diagnostic.copy(varianceOverSigmaSquared = diagnostic.varianceOverSigmaSquared.map(value => (value * sigma) * sigma))
          if values.exists(value => !value.isFinite) || singular.exists(value => !value.isFinite) ||
              geometry.singular.zip(singular).exists((original, scaled) => original > 0.0 && scaled == 0.0) ||
              variances.exists(_.varianceOverSigmaSquared.exists(value => !value.isFinite || value <= 0.0)) then
            Left(DesignDiagnosticsError.NumericalFailure("residual sigma scaling exceeds numeric range"))
          else
            val changed = geometry.copy(matrix = Mat.unsafe(geometry.matrix.rows, geometry.matrix.cols, values), singular = singular, cutoff = geometry.cutoff / sigma)
            Right(previous :+ run.copy(t = variances, geometry = changed))
