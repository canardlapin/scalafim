package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.estimates.{DfRole, FileReference, PoolingScope, ProductId, ScientificFact}

enum GroupDfValues:
  case Scalar(value: Double)
  case BySample(product: ProductId, samples: Vector[Int], values: Vector[Double])

  private[group] def valid: Boolean = this match
    case Scalar(value) => value.isFinite && value > 0.0
    case BySample(_, samples, values) =>
      samples.nonEmpty && samples.distinct.size == samples.size && samples.forall(_ >= 0) &&
        values.size == samples.size && values.forall(value => value.isFinite && value > 0.0)

final case class GroupDegreesOfFreedom(
    role: DfRole,
    values: GroupDfValues,
    method: String,
    approximate: Boolean
):
  require(values.valid, "group degrees of freedom must be finite, positive and sample-aligned")
  require(method.trim.nonEmpty, "group degrees-of-freedom method must be non-empty")
  require(role != DfRole.Reference, "first-level variance uncertainty requires residual or effective df")
  require(!approximate || role == DfRole.Effective, "approximate degrees of freedom must be identified as effective")

enum GroupVarianceOrigin:
  case Known(method: String)
  case Estimated(degreesOfFreedom: GroupDegreesOfFreedom)
  case Unknown(reason: String)

  private[group] def valid: Boolean = this match
    case Known(method) => method.trim.nonEmpty
    case Estimated(_) => true
    case Unknown(reason) => reason.trim.nonEmpty

final case class GroupFitProvenance(
    estimator: ScientificFact,
    serialCorrelation: ScientificFact,
    nuisance: ScientificFact,
    runCombination: ScientificFact
):
  require(Vector(estimator, serialCorrelation, nuisance, runCombination).forall(GroupFitProvenance.validFact))

object GroupFitProvenance:
  private def validFact(fact: ScientificFact): Boolean = fact match
    case ScientificFact.Known(description) => description.trim.nonEmpty
    case ScientificFact.Unknown(reason) => reason.trim.nonEmpty

enum GroupGeometryEvidence:
  case Verified(worldFrame: String, method: String, references: Vector[FileReference])
  case Unknown(reason: String)

  private[group] def valid: Boolean = this match
    case Verified(worldFrame, method, references) =>
      worldFrame.trim.nonEmpty && method.trim.nonEmpty &&
        references.map(_.path).distinct.size == references.size
    case Unknown(reason) => reason.trim.nonEmpty

final case class GroupUncertaintySource(
    subject: SubjectId,
    contrast: String,
    samples: Vector[Int],
    varianceProduct: Option[ProductId],
    origin: GroupVarianceOrigin,
    fit: GroupFitProvenance,
    pooling: Option[PoolingScope]
):
  require(contrast.trim.nonEmpty, "uncertainty contrast must be non-empty")
  require(samples.nonEmpty && samples.distinct.size == samples.size && samples.forall(_ >= 0),
    "uncertainty samples must be nonempty, unique and nonnegative")
  require(origin.valid)

/** Subject-, contrast- and sample-bound first-level uncertainty provenance.
  * Numerical group estimators consume the attached variances; this receipt
  * records what those variances mean without promoting provenance to a
  * calibration claim.
  */
final class GroupUncertaintyReceipt private (
    val sources: Vector[GroupUncertaintySource],
    val geometry: GroupGeometryEvidence
)

object GroupUncertaintyReceipt:
  def make(
      subjects: Vector[SubjectId],
      contrasts: Vector[String],
      samples: Vector[Int],
      sources: Vector[GroupUncertaintySource],
      geometry: GroupGeometryEvidence
  ): Either[GroupError, GroupUncertaintyReceipt] =
    val expected = subjects.flatMap(subject => contrasts.map(contrast => subject -> contrast))
    val actual = sources.map(source => source.subject -> source.contrast)
    if subjects.isEmpty || subjects.distinct.size != subjects.size || contrasts.isEmpty || contrasts.distinct.size != contrasts.size then
      Left(GroupError.InvalidUncertaintyReceipt("subject and contrast axes must be nonempty and unique"))
    else if !geometry.valid then Left(GroupError.InvalidUncertaintyReceipt("geometry evidence is invalid"))
    else if expected.distinct.size != expected.size || actual != expected then
      Left(GroupError.InvalidUncertaintyReceipt("uncertainty rows must exactly follow the subject and contrast axes"))
    else if samples.isEmpty || samples.distinct.size != samples.size || samples.exists(_ < 0) ||
        sources.exists(_.samples != samples) then
      Left(GroupError.InvalidUncertaintyReceipt("uncertainty samples must exactly follow the group sample axis"))
    else Right(new GroupUncertaintyReceipt(sources, geometry))

  private[group] def unknown(
      subjects: Vector[SubjectId],
      contrasts: Vector[String],
      samples: Vector[Int],
      reason: String
  ): Either[GroupError, GroupUncertaintyReceipt] =
    val unknown = ScientificFact.Unknown(reason)
    val fit = GroupFitProvenance(unknown, unknown, unknown, unknown)
    val sources = subjects.flatMap: subject =>
      contrasts.map: contrast =>
        GroupUncertaintySource(subject, contrast, samples, None, GroupVarianceOrigin.Unknown(reason), fit, None)
    make(subjects, contrasts, samples, sources, GroupGeometryEvidence.Unknown(reason))
