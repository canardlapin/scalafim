package scalafim.fmri.group

/** A pure, inspectable description of a group analysis: the data cube, the
  * second-level design, and the weighting (estimator) choice — the same "inspect
  * before it runs" split that `FitPlan` gives the first level.
  *
  * The case class enforces only the structural invariant (design and data agree
  * on subject count). `build` is the validating entry point: it additionally
  * checks residual degrees of freedom and that a variance-weighted estimator has
  * variances. Whichever path constructs the model, `GroupEngine.fit` re-validates
  * totally, so no runnable-looking but ill-posed model reaches the numerics.
  */
final case class GroupModel[+V <: VarianceCapability](
    data: GroupData[V],
    design: GroupDesign,
    weighting: GroupWeighting = GroupWeighting.Unweighted
):
  require(design.subjects == data.nSubjects, "design subjects must match data subjects")

  def requiresVariance: Boolean = weighting.requiresVariance

  /** Combinator: same model under a different estimator. The new estimator's
    * variance requirement is checked at `fit`, not here.
    */
  def reduceWith(newWeighting: GroupWeighting): GroupModel[V] = copy(weighting = newWeighting)

  def summary: GroupSummary =
    GroupSummary(
      subjects = data.nSubjects,
      samples = data.nSamples,
      contrasts = data.nContrasts,
      terms = design.terms,
      termNames = design.termNames,
      weighting = weighting,
      residualDf = data.nSubjects - design.terms
    )

object GroupModel:

  /** Build a model, reporting composition errors (subject mismatch, or a
    * variance-weighted estimator applied to variance-free data) as values.
    */
  def build[V <: VarianceCapability](
      data: GroupData[V],
      design: GroupDesign,
      weighting: GroupWeighting = GroupWeighting.Unweighted
  ): Either[GroupError, GroupModel[V]] =
    if design.subjects != data.nSubjects then
      Left(GroupError.subjectMismatch(data.nSubjects, design.subjects))
    else if data.nSubjects <= design.terms then
      Left(GroupError.InsufficientSubjects(data.nSubjects, design.terms))
    else if weighting.requiresVariance && !data.hasVariances then
      Left(GroupError.MissingVariances(weighting.label))
    else Right(GroupModel(data, design, weighting))

  def unweighted[V <: VarianceCapability](
      data: GroupData[V],
      design: GroupDesign
  ): Either[GroupError, GroupModel[V]] =
    validateShape(data, design).map(_ => GroupModel(data, design, GroupWeighting.Unweighted))

  def inverseVariance(
      data: GroupData[VarianceCapability.WithVariances],
      design: GroupDesign
  ): Either[GroupError, GroupModel[VarianceCapability.WithVariances]] =
    validateShape(data, design).map(_ => GroupModel(data, design, GroupWeighting.InverseVariance))

  /** Compatibility constructor: DL with a plug-in normal reference by default.
    * This is not a calibrated small-sample inference default.
    */
  def randomEffects(
      data: GroupData[VarianceCapability.WithVariances],
      design: GroupDesign,
      tau: TauEstimator = TauEstimator.DerSimonianLaird
  ): Either[GroupError, GroupModel[VarianceCapability.WithVariances]] =
    validateShape(data, design).map(_ => GroupModel(data, design, GroupWeighting.RandomEffects(tau)))

  /** Explicit mixed-effects policy, with no inferred default. The older
    * randomEffects constructor retains DL/z compatibility. Neither plug-in z
    * nor modified Knapp-Hartung is uniformly calibrated for few subjects and
    * unequal or estimated sampling variances; select and qualify the policy
    * for the admitted scientific design. See docs/verification/group-repair-2026-09-08.md.
    */
  def mixedEffects(
      data: GroupData[VarianceCapability.WithVariances],
      design: GroupDesign,
      tau: TauEstimator,
      inference: MetaInference
  ): Either[GroupError, GroupModel[VarianceCapability.WithVariances]] =
    validateShape(data, design).map(_ => GroupModel(data, design, GroupWeighting.RandomEffects(tau, inference)))

  private def validateShape(
      data: GroupData[? <: VarianceCapability],
      design: GroupDesign
  ): Either[GroupError, Unit] =
    if design.subjects != data.nSubjects then
      Left(GroupError.subjectMismatch(data.nSubjects, design.subjects))
    else if data.nSubjects <= design.terms then
      Left(GroupError.InsufficientSubjects(data.nSubjects, design.terms))
    else Right(())

final case class GroupSummary(
    subjects: Int,
    samples: Int,
    contrasts: Int,
    terms: Int,
    termNames: Vector[String],
    weighting: GroupWeighting,
    residualDf: Int
)
