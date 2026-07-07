package scalafim.fmri.group

import scalafim.linalg.DoubleMatrix

/** One first-level contrast, ready to reduce over subjects: the per-subject
  * effect estimates and, when available, their variances.
  *
  * Both matrices are `[subjects × samples]` — subjects are the reduction axis
  * (the group analog of first-level timepoints), samples are voxels/parcels/etc.
  * This is the second-level analog of `scalafim.fmri.fit.ResponseBlock`.
  */
sealed trait GroupResponse[+V <: VarianceCapability]:
  def effects: DoubleMatrix
  def variances: Option[DoubleMatrix]

  def nSubjects: Int = effects.rows
  def nSamples: Int = effects.cols
  def hasVariances: Boolean = variances.isDefined

  def requireVariances(weighting: GroupWeighting): Either[GroupError, GroupResponse.WithVariances] =
    this match
      case response: GroupResponse.WithVariances => Right(response)
      case _: GroupResponse.EffectsOnly          => Left(GroupError.MissingVariances(weighting.label))

object GroupResponse:
  final case class EffectsOnly private[GroupResponse] (effects: DoubleMatrix)
      extends GroupResponse[VarianceCapability.EffectsOnly]:
    def variances: Option[DoubleMatrix] = None

  final case class WithVariances private[GroupResponse] (
      effects: DoubleMatrix,
      varianceMatrix: DoubleMatrix
  ) extends GroupResponse[VarianceCapability.WithVariances]:
    def variances: Option[DoubleMatrix] = Some(varianceMatrix)

  /** Effects only, for OLS-style estimators. */
  def fromEffects(effects: DoubleMatrix): Either[GroupError, EffectsOnly] =
    for
      _ <- requireNonEmpty(effects)
      _ <- requireFinite(effects, "effect estimates")
    yield EffectsOnly(effects)

  /** Effects with per-subject variances, for meta-analytic estimators. */
  def weighted(effects: DoubleMatrix, variances: DoubleMatrix): Either[GroupError, WithVariances] =
    for
      _ <- requireNonEmpty(effects)
      _ <- requireFinite(effects, "effect estimates")
      _ <- requireSameShape(effects, variances)
      _ <- requireFinite(variances, "variances")
      _ <- requirePositive(variances)
    yield WithVariances(effects, variances)

  def unsafeFromEffects(effects: DoubleMatrix): EffectsOnly =
    fromEffects(effects).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeWeighted(effects: DoubleMatrix, variances: DoubleMatrix): WithVariances =
    weighted(effects, variances).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def requireNonEmpty(m: DoubleMatrix): Either[GroupError, Unit] =
    if m.rows == 0 || m.cols == 0 then Left(GroupError.EmptyResponse) else Right(())

  private def requireSameShape(effects: DoubleMatrix, variances: DoubleMatrix): Either[GroupError, Unit] =
    if variances.rows != effects.rows then Left(GroupError.responseSubjectMismatch(effects.rows, variances.rows))
    else if variances.cols != effects.cols then Left(GroupError.sampleMismatch(effects.cols, variances.cols))
    else Right(())

  private def requireFinite(m: DoubleMatrix, what: String): Either[GroupError, Unit] =
    if allFinite(m) then Right(()) else Left(GroupError.NonFiniteData(what))

  private def requirePositive(m: DoubleMatrix): Either[GroupError, Unit] =
    var ok = true
    var i = 0
    val data = m.copyData
    while ok && i < data.length do
      if data(i) <= 0.0 then ok = false
      i += 1
    if ok then Right(()) else Left(GroupError.NonPositiveVariance)

  private def allFinite(m: DoubleMatrix): Boolean =
    val data = m.copyData
    var i = 0
    var ok = true
    while ok && i < data.length do
      if !data(i).isFinite then ok = false
      i += 1
    ok
