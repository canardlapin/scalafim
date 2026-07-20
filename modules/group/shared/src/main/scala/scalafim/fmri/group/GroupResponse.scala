package scalafim.fmri.group

import gale.linalg.DMat

/** One first-level contrast, ready to reduce over subjects: the per-subject
  * effect estimates and, when available, their variances.
  *
  * Both matrices are `[subjects × samples]` — subjects are the reduction axis
  * (the group analog of first-level timepoints), samples are voxels/parcels/etc.
  * This is the second-level analog of `scalafim.fmri.fit.ResponseBlock`.
  */
sealed trait GroupResponse[+V <: VarianceCapability]:
  def effects: DMat
  def variances: Option[DMat]

  def nSubjects: Int = effects.rows
  def nSamples: Int = effects.cols
  def hasVariances: Boolean = variances.isDefined

  def requireVariances(weighting: GroupWeighting): Either[GroupError, GroupResponse.WithVariances] =
    this match
      case response: GroupResponse.WithVariances => Right(response)
      case _: GroupResponse.EffectsOnly          => Left(GroupError.MissingVariances(weighting.label))

object GroupResponse:
  final case class EffectsOnly private[GroupResponse] (effects: DMat)
      extends GroupResponse[VarianceCapability.EffectsOnly]:
    def variances: Option[DMat] = None

  final case class WithVariances private[GroupResponse] (
      effects: DMat,
      varianceMatrix: DMat
  ) extends GroupResponse[VarianceCapability.WithVariances]:
    def variances: Option[DMat] = Some(varianceMatrix)

  /** Effects only, for OLS-style estimators. */
  def fromEffects(effects: DMat): Either[GroupError, EffectsOnly] =
    for
      _ <- requireNonEmpty(effects)
      _ <- requireFinite(effects, "effect estimates")
    yield EffectsOnly(effects)

  /** Effects with per-subject variances, for meta-analytic estimators. */
  def weighted(effects: DMat, variances: DMat): Either[GroupError, WithVariances] =
    for
      _ <- requireNonEmpty(effects)
      _ <- requireFinite(effects, "effect estimates")
      _ <- requireSameShape(effects, variances)
      _ <- requireFinite(variances, "variances")
      _ <- requirePositive(variances)
    yield WithVariances(effects, variances)

  def unsafeFromEffects(effects: DMat): EffectsOnly =
    fromEffects(effects).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeWeighted(effects: DMat, variances: DMat): WithVariances =
    weighted(effects, variances).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def requireNonEmpty(m: DMat): Either[GroupError, Unit] =
    if m.rows == 0 || m.cols == 0 then Left(GroupError.EmptyResponse) else Right(())

  private def requireSameShape(effects: DMat, variances: DMat): Either[GroupError, Unit] =
    if variances.rows != effects.rows then Left(GroupError.responseSubjectMismatch(effects.rows, variances.rows))
    else if variances.cols != effects.cols then Left(GroupError.sampleMismatch(effects.cols, variances.cols))
    else Right(())

  private def requireFinite(m: DMat, what: String): Either[GroupError, Unit] =
    if allFinite(m) then Right(()) else Left(GroupError.NonFiniteData(what))

  private def requirePositive(m: DMat): Either[GroupError, Unit] =
    var ok = true
    var row = 0
    while ok && row < m.rows do
      var col = 0
      while ok && col < m.cols do
        if m(row, col) <= 0.0 then ok = false
        col += 1
      row += 1
    if ok then Right(()) else Left(GroupError.NonPositiveVariance)

  private def allFinite(m: DMat): Boolean =
    var row = 0
    var ok = true
    while ok && row < m.rows do
      var col = 0
      while ok && col < m.cols do
        if !m(row, col).isFinite then ok = false
        col += 1
      row += 1
    ok
