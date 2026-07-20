package scalafim.fmri.fit

import gale.linalg.{DMat, DoubleLinearOperator, Matrix}

opaque type TrialCoefficientId = String

object TrialCoefficientId:
  def apply(value: String): Either[FitError, TrialCoefficientId] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(FitError.InvalidFitAxis("trial coefficient id", "must be non-empty"))

  def unsafe(value: String): TrialCoefficientId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: TrialCoefficientId)
    inline def value: String = id

enum TrialEstimability:
  case Estimable
  case ZeroRegressor

final case class TrialCoefficientAxis private (
    ids: Vector[TrialCoefficientId],
    estimability: Vector[TrialEstimability]
):
  require(ids.nonEmpty, "trial coefficient axis must be non-empty")
  require(ids.forall(_.value.nonEmpty), "trial coefficient ids must be non-empty")
  require(ids.distinct.length == ids.length, "trial coefficient ids must be unique")
  require(estimability.length == ids.length, "trial estimability must match coefficient ids")

  def size: Int = ids.length
  def names: Vector[String] = ids.map(_.value)

object TrialCoefficientAxis:
  def fromNames(
      names: Vector[String],
      estimability: Vector[TrialEstimability]
  ): Either[FitError, TrialCoefficientAxis] =
    if names.isEmpty then
      Left(FitError.InvalidFitAxis("trial coefficient axis", "must contain at least one trial"))
    else if estimability.length != names.length then
      Left(
        FitError.InvalidFitAxis(
          "trial coefficient axis",
          s"estimability length ${estimability.length} does not match id length ${names.length}"
        )
      )
    else
      val ids = Vector.newBuilder[TrialCoefficientId]
      var index = 0
      while index < names.length do
        TrialCoefficientId(names(index)) match
          case Left(error) => return Left(error)
          case Right(id)   => ids += id
        index += 1

      val typedIds = ids.result()
      val duplicateIds = typedIds.groupBy(_.value).collect { case (id, occurrences) if occurrences.length > 1 => id }.toVector.sorted
      if duplicateIds.nonEmpty then
        Left(
          FitError.InvalidFitAxis(
            "trial coefficient axis",
            s"ids must be unique; duplicates: ${duplicateIds.mkString(", ")}"
          )
        )
      else Right(new TrialCoefficientAxis(typedIds, estimability))

enum TrialReadoutMethod:
  case LeastSquaresSeparate

final case class TrialReadoutReceipt(
    method: TrialReadoutMethod,
    fixedRank: Int
):
  require(fixedRank >= 0, "fixed rank must be non-negative")

/** A response-independent linear map from timepoints to named trial coefficients.
  *
  * The operator includes every design-only step required by the estimator. Its
  * adjoint therefore maps trial-space scores back to the original time axis,
  * including fixed-nuisance projection when present.
  */
final class TrialReadout private[fit] (
    val operator: DoubleLinearOperator,
    val axis: TrialCoefficientAxis,
    val receipt: TrialReadoutReceipt
):
  require(operator.rows == axis.size, "readout rows must match trial coefficient axis")
  require(operator.cols > 0, "readout must contain at least one timepoint")
  require(receipt.fixedRank <= operator.cols, "fixed rank cannot exceed readout timepoints")

  def timepoints: Int = operator.cols
  def trials: Int = operator.rows
  def trialNames: Vector[String] = axis.names

  def forward(response: ResponseBlock): Either[FitError, CoefficientBlock] =
    if response.timepoints != timepoints then
      Left(FitError.RowMismatch(timepoints, response.timepoints))
    else
      operator
        .applyTo(response.value)
        .left
        .map(FitError.TrialReadoutFailed.apply)
        .map(CoefficientBlock.apply)

  def adjoint(trialScores: CoefficientBlock): Either[FitError, ResponseBlock] =
    if trialScores.predictors != trials then
      Left(
        FitError.InvalidFitAxis(
          "trial score block",
          s"rows ${trialScores.predictors} do not match readout trials $trials"
        )
      )
    else if trialScores.voxels == 0 then
      Left(FitError.InvalidFitAxis("trial score block", "must contain at least one column"))
    else if containsNonFinite(trialScores.value) then
      Left(FitError.NonFiniteInput("trial score block"))
    else
      operator
        .transposeApplyTo(trialScores.value)
        .left
        .map(FitError.TrialReadoutFailed.apply)
        .flatMap(ResponseBlock.fromMatrix)

  private def containsNonFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return true
        col += 1
      row += 1
    false

private[fit] object LssTrialReadout:
  def fromPrepared(prepared: LssPreparedDesign): Either[FitError, TrialReadout] =
    val base = Matrix.newBuilder(prepared.trials, prepared.timepoints)
    var trial = 0
    while trial < prepared.trials do
      val workspace = prepared.workspaces(trial)
      workspace.status match
        case LssTrialStatus.ZeroTrial =>
          ()
        case LssTrialStatus.Active | LssTrialStatus.TargetOnly =>
          var timepoint = 0
          while timepoint < prepared.timepoints do
            val target = prepared.residualizedTrials(timepoint, trial)
            val total = prepared.totalTrialSignal(timepoint)
            val numerator = (1.0 + workspace.alpha) * target - workspace.alpha * total
            base(trial, timepoint) = numerator / workspace.denominator
            timepoint += 1
      trial += 1

    val estimability = prepared.workspaces.map: workspace =>
      workspace.status match
        case LssTrialStatus.ZeroTrial                          => TrialEstimability.ZeroRegressor
        case LssTrialStatus.Active | LssTrialStatus.TargetOnly => TrialEstimability.Estimable

    for
      axis <- TrialCoefficientAxis.fromNames(prepared.trialNames, estimability)
      projectedTranspose <- prepared.fixedProjection.residualize(base.result().t)
    yield
      new TrialReadout(
        operator = projectedTranspose.t,
        axis = axis,
        receipt = TrialReadoutReceipt(
          method = TrialReadoutMethod.LeastSquaresSeparate,
          fixedRank = prepared.fixedRank
        )
      )
