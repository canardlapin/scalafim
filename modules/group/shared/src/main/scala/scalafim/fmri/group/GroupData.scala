package scalafim.fmri.group

import scalafim.dataset.SubjectId
import gale.linalg.DMat

import scala.collection.immutable.VectorMap

/** The canonical group cube: subjects × samples × (first-level) contrasts.
  *
  * Stored as one `GroupResponse` per first-level contrast, all sharing the same
  * subjects and sample space. This is the value that flows into a `GroupModel`.
  */
final case class GroupData[+V <: VarianceCapability] private (
    subjectAxis: SubjectAxis,
    space: GroupSpace,
    responses: VectorMap[String, GroupResponse[V]]
):
  require(responses.nonEmpty, "group data must have at least one contrast")
  responses.foreach { case (name, r) =>
    require(r.nSubjects == subjectAxis.length, s"contrast '$name' has ${r.nSubjects} subjects, expected ${subjectAxis.length}")
    require(r.nSamples == space.nSamples, s"contrast '$name' has ${r.nSamples} samples, expected ${space.nSamples}")
  }

  def subjects: Vector[SubjectId] = subjectAxis.subjects
  def nSubjects: Int = subjectAxis.length
  def nSamples: Int = space.nSamples
  def contrasts: Vector[String] = responses.keys.toVector
  def nContrasts: Int = responses.size

  /** True when every contrast carries per-subject variances (required by the
    * meta-analytic estimators).
    */
  def hasVariances: Boolean = responses.values.forall(_.hasVariances)

  def response(contrast: String): Option[GroupResponse[V]] = responses.get(contrast)

object GroupData:

  /** Build from named responses, validating subject/sample alignment. */
  def apply(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      responses: Vector[(String, GroupResponse[VarianceCapability])]
  ): Either[GroupError, GroupData[VarianceCapability]] =
    build(subjects, space, responses)

  def build[V <: VarianceCapability](
      subjects: Vector[SubjectId],
      space: GroupSpace,
      responses: Vector[(String, GroupResponse[V])]
  ): Either[GroupError, GroupData[V]] =
    for
      axis <- SubjectAxis.from(subjects)
      names <- parseContrastNames(responses.map(_._1))
      _ <- requireResponseNames(names)
      _ <- requireResponses(space, axis, responses)
    yield new GroupData[V](axis, space, VectorMap.from(responses))

  /** Convenience for a single first-level contrast given raw effect (and
    * optional variance) matrices `[subjects × samples]`.
    */
  def single(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      contrast: String,
      effects: DMat,
      variances: Option[DMat] = None
  ): Either[GroupError, GroupData[VarianceCapability]] =
    variances match
      case Some(v) => withVariances(subjects, space, contrast, effects, v).map(data => data: GroupData[VarianceCapability])
      case None    => effectsOnly(subjects, space, contrast, effects).map(data => data: GroupData[VarianceCapability])

  def effectsOnly(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      contrast: String,
      effects: DMat
  ): Either[GroupError, GroupData[VarianceCapability.EffectsOnly]] =
    for
      name <- FirstLevelContrastName(contrast)
      response <- GroupResponse.fromEffects(effects)
      data <- build(subjects, space, Vector(name.value -> response))
    yield data

  def withVariances(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      contrast: String,
      effects: DMat,
      variances: DMat
  ): Either[GroupError, GroupData[VarianceCapability.WithVariances]] =
    for
      name <- FirstLevelContrastName(contrast)
      response <- GroupResponse.weighted(effects, variances)
      data <- build(subjects, space, Vector(name.value -> response))
    yield data

  def unsafe(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      responses: Vector[(String, GroupResponse[VarianceCapability])]
  ): GroupData[VarianceCapability] =
    apply(subjects, space, responses).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def parseContrastNames(names: Vector[String]): Either[GroupError, Vector[FirstLevelContrastName]] =
    names.foldLeft[Either[GroupError, Vector[FirstLevelContrastName]]](Right(Vector.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), name) => FirstLevelContrastName(name).map(acc :+ _)
    }

  private def requireResponseNames(names: Vector[FirstLevelContrastName]): Either[GroupError, Unit] =
    if names.isEmpty then Left(GroupError.contrastMismatch(1, 0))
    else
      val values = names.map(_.value)
      val duplicates = values.diff(values.distinct).distinct
      if duplicates.nonEmpty then Left(GroupError.DuplicateContrasts(duplicates))
      else Right(())

  private def requireResponses[V <: VarianceCapability](
      space: GroupSpace,
      axis: SubjectAxis,
      responses: Vector[(String, GroupResponse[V])]
  ): Either[GroupError, Unit] =
    responses.collectFirst {
      case (_, r) if r.nSubjects != axis.length =>
        GroupError.subjectMismatch(axis.length, r.nSubjects)
      case (_, r) if r.nSamples != space.nSamples =>
        GroupError.sampleMismatch(space.nSamples, r.nSamples)
    } match
      case Some(err) => Left(err)
      case None      => Right(())
