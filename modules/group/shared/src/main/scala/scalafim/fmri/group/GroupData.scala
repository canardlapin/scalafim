package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.linalg.DoubleMatrix

import scala.collection.immutable.VectorMap

/** The canonical group cube: subjects × samples × (first-level) contrasts.
  *
  * Stored as one `GroupResponse` per first-level contrast, all sharing the same
  * subjects and sample space. This is the value that flows into a `GroupModel`.
  */
final case class GroupData private (
    subjects: Vector[SubjectId],
    space: GroupSpace,
    responses: VectorMap[String, GroupResponse]
):
  require(subjects.nonEmpty, "group data must have at least one subject")
  require(responses.nonEmpty, "group data must have at least one contrast")
  responses.foreach { case (name, r) =>
    require(r.nSubjects == subjects.length, s"contrast '$name' has ${r.nSubjects} subjects, expected ${subjects.length}")
    require(r.nSamples == space.nSamples, s"contrast '$name' has ${r.nSamples} samples, expected ${space.nSamples}")
  }

  def nSubjects: Int = subjects.length
  def nSamples: Int = space.nSamples
  def contrasts: Vector[String] = responses.keys.toVector
  def nContrasts: Int = responses.size

  /** True when every contrast carries per-subject variances (required by the
    * meta-analytic estimators).
    */
  def hasVariances: Boolean = responses.values.forall(_.hasVariances)

  def response(contrast: String): Option[GroupResponse] = responses.get(contrast)

object GroupData:

  /** Build from named responses, validating subject/sample alignment. */
  def apply(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      responses: Vector[(String, GroupResponse)]
  ): Either[GroupError, GroupData] =
    if subjects.isEmpty then Left(GroupError.EmptyResponse)
    else if responses.isEmpty then Left(GroupError.ContrastMismatch(1, 0))
    else
      val names = responses.map(_._1)
      val duplicates = names.diff(names.distinct).distinct
      if duplicates.nonEmpty then Left(GroupError.DuplicateContrasts(duplicates))
      else
        val misaligned = responses.collectFirst {
          case (_, r) if r.nSubjects != subjects.length =>
            GroupError.SubjectMismatch(r.nSubjects, subjects.length)
          case (_, r) if r.nSamples != space.nSamples =>
            GroupError.SampleMismatch(space.nSamples, r.nSamples)
        }
        misaligned match
          case Some(err) => Left(err)
          case None      => Right(new GroupData(subjects, space, VectorMap.from(responses)))

  /** Convenience for a single first-level contrast given raw effect (and
    * optional variance) matrices `[subjects × samples]`.
    */
  def single(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      contrast: String,
      effects: DoubleMatrix,
      variances: Option[DoubleMatrix] = None
  ): Either[GroupError, GroupData] =
    val response: Either[GroupError, GroupResponse] = variances match
      case Some(v) => GroupResponse.weighted(effects, v).map(r => r: GroupResponse)
      case None    => GroupResponse.fromEffects(effects).map(r => r: GroupResponse)
    response.flatMap(r => apply(subjects, space, Vector(contrast -> r)))

  def unsafe(
      subjects: Vector[SubjectId],
      space: GroupSpace,
      responses: Vector[(String, GroupResponse)]
  ): GroupData =
    apply(subjects, space, responses).fold(error => throw new IllegalArgumentException(error.message), identity)
