package scalafim.fmri.group

import scalafim.dataset.SubjectId

/** Compile-time marker for whether a group data cube carries per-subject
  * variances. Effects-only data can run OLS; variance-carrying data can also
  * run fixed- and random-effects meta-analysis.
  */
sealed trait VarianceCapability

object VarianceCapability:
  sealed trait EffectsOnly extends VarianceCapability
  sealed trait WithVariances extends VarianceCapability

/** Subject axis shared by every first-level contrast in a group data cube. */
final case class SubjectAxis private (subjects: Vector[SubjectId]):
  require(subjects.nonEmpty, "subject axis must have at least one subject")
  require(subjects.distinct.length == subjects.length, "subject ids must be unique")

  def length: Int = subjects.length

object SubjectAxis:
  def from(subjects: Vector[SubjectId]): Either[GroupError, SubjectAxis] =
    if subjects.isEmpty then Left(GroupError.EmptyResponse)
    else
      val duplicates = subjects.diff(subjects.distinct).distinct
      if duplicates.nonEmpty then Left(GroupError.DuplicateSubjects(duplicates.map(_.value)))
      else Right(new SubjectAxis(subjects))

  def unsafe(subjects: Vector[SubjectId]): SubjectAxis =
    from(subjects).fold(error => throw new IllegalArgumentException(error.message), identity)

/** Whether a design builder should include an intercept column. */
enum InterceptPolicy:
  case Include
  case Omit

  def include: Boolean =
    this match
      case Include => true
      case Omit    => false

object InterceptPolicy:
  def fromBoolean(include: Boolean): InterceptPolicy =
    if include then Include else Omit

opaque type FirstLevelContrastName = String

object FirstLevelContrastName:
  def apply(value: String): Either[GroupError, FirstLevelContrastName] =
    GroupLabel.validate("first-level contrast", value).map(valid => valid: FirstLevelContrastName)

  def unsafe(value: String): FirstLevelContrastName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: FirstLevelContrastName)
    def value: String = name

opaque type DesignTermName = String

object DesignTermName:
  val Intercept: DesignTermName = unsafe("(Intercept)")

  def apply(value: String): Either[GroupError, DesignTermName] =
    GroupLabel.validate("design term", value).map(valid => valid: DesignTermName)

  def unsafe(value: String): DesignTermName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: DesignTermName)
    def value: String = name

opaque type CovariateName = String

object CovariateName:
  def apply(value: String): Either[GroupError, CovariateName] =
    GroupLabel.validate("covariate", value).map(valid => valid: CovariateName)

  def unsafe(value: String): CovariateName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: CovariateName)
    def value: String = name

opaque type GroupContrastName = String

object GroupContrastName:
  def apply(value: String): Either[GroupError, GroupContrastName] =
    GroupLabel.validate("group contrast", value).map(valid => valid: GroupContrastName)

  def unsafe(value: String): GroupContrastName =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (name: GroupContrastName)
    def value: String = name

opaque type SampleLabel = String

object SampleLabel:
  def apply(value: String): Either[GroupError, SampleLabel] =
    GroupLabel.validate("sample label", value).map(valid => valid: SampleLabel)

  def unsafe(value: String): SampleLabel =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (label: SampleLabel)
    def value: String = label

opaque type DegreesOfFreedom = Int

object DegreesOfFreedom:
  def apply(value: Int): Either[GroupError, DegreesOfFreedom] =
    if value > 0 then Right(value: DegreesOfFreedom)
    else Left(GroupError.InvalidDegreesOfFreedom(value))

  def unsafe(value: Int): DegreesOfFreedom =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (df: DegreesOfFreedom)
    def value: Int = df

opaque type PValue = Double

object PValue:
  def apply(value: Double): Either[GroupError, PValue] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value: PValue)
    else Left(GroupError.InvalidPValue(value))

  def unsafe(value: Double): PValue =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (p: PValue)
    def value: Double = p

private object GroupLabel:
  def validate(kind: String, value: String): Either[GroupError, String] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(GroupError.InvalidLabel(kind, value))
