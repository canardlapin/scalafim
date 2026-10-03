package scalafim.fmri.design

/** Combination is a scientific choice: fixed effects combines estimable
  * run-level contrasts; concatenation sums nuisance-residualized task information.
  */
enum RunContrastCombination:
  case FixedEffects
  case Concatenated(taskColumns: Vector[ColumnId])

/** `variance` is the absolute contrast variance under the supplied sigmas. */
final case class WeightedTDiagnostic(
    contrast: ColumnContrast,
    variance: Option[Double],
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[RunIndex],
    combination: RunContrastCombination,
    residualSigmas: Vector[Double]
)

/** `worstDirectionSe` is the absolute worst-direction standard error under
  * the supplied sigmas.
  */
final case class WeightedFDiagnostic(
    contrast: FColumnContrast,
    worstDirectionSe: Option[Double],
    effectiveDf: Int,
    contributingRuns: Vector[RunIndex],
    excludedRuns: Vector[RunIndex],
    combination: RunContrastCombination,
    residualSigmas: Vector[Double]
)

/** Uncertainty conditional on caller-supplied, positive run-specific residual
  * standard deviations. These methods do not estimate sigma or noise
  * correlation. Run receipts are [[RunIndex]] values: the one-based position
  * of the run in the supplied vector. All runs must share one [[RankPolicy]].
  */
object WeightedContrastDiagnostics:
  def t(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: ColumnContrast,
      residualSigmas: Vector[Double],
      combination: RunContrastCombination
  ): Either[DesignDiagnosticsError, WeightedTDiagnostic] =
    validated(runs, residualSigmas).flatMap: _ =>
      combination match
        case RunContrastCombination.FixedEffects =>
          for
            result <- ContrastDiagnostics.combineFixedEffectsT(runs, contrast, residualSigmas)
            variance <- checked(result.variance)
          yield WeightedTDiagnostic(contrast, variance, result.contributing, result.excluded.map(_._1), combination, residualSigmas)
        case RunContrastCombination.Concatenated(taskColumns) =>
          for
            result <- ContrastDiagnostics.combineConcatenatedT(runs, taskColumns, contrast, residualSigmas)
            variance <- checked(result.variance)
          yield WeightedTDiagnostic(contrast, variance, result.contributing, result.excluded, combination, residualSigmas)

  def f(
      runs: Vector[ContrastDiagnosticsResult],
      contrast: FColumnContrast,
      residualSigmas: Vector[Double],
      combination: RunContrastCombination
  ): Either[DesignDiagnosticsError, WeightedFDiagnostic] =
    validated(runs, residualSigmas).flatMap: _ =>
      combination match
        case RunContrastCombination.FixedEffects =>
          for
            result <- ContrastDiagnostics.combineFixedEffectsF(runs, contrast, residualSigmas)
            se <- checked(result.worstSe)
          yield WeightedFDiagnostic(contrast, se, result.effectiveDf, result.contributing, result.excluded.map(_._1), combination, residualSigmas)
        case RunContrastCombination.Concatenated(taskColumns) =>
          for
            result <- ContrastDiagnostics.combineConcatenatedF(runs, taskColumns, contrast, residualSigmas)
            se <- checked(result.worstSe)
          yield WeightedFDiagnostic(contrast, se, result.effectiveDf, result.contributing, result.excluded, combination, residualSigmas)

  private def checked(value: Option[Double]): Either[DesignDiagnosticsError, Option[Double]] =
    if value.exists(number => !number.isFinite || number <= 0.0) then
      Left(DesignDiagnosticsError.NumericalFailure("weighted contrast uncertainty is outside the finite positive range"))
    else Right(value)

  private def validated(runs: Vector[ContrastDiagnosticsResult], sigmas: Vector[Double]): Either[DesignDiagnosticsError, Unit] =
    if sigmas.length != runs.length then Left(DesignDiagnosticsError.ResidualSigmaCountMismatch(runs.length, sigmas.length))
    else
      sigmas.indices.find(index => !sigmas(index).isFinite || sigmas(index) <= 0.0) match
        case Some(index) => Left(DesignDiagnosticsError.InvalidResidualSigma(RunIndex.unsafeOneBased(index + 1), sigmas(index)))
        case None => ContrastDiagnostics.commonPolicy(runs).map(_ => ())
