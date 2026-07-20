package scalafim.connectivity

enum CovariateType:
  case Categorical
  case Continuous
  case Integer
  case Binary

  def label: String =
    this match
      case Categorical => "categorical"
      case Continuous  => "continuous"
      case Integer     => "integer"
      case Binary      => "binary"

sealed trait CovariateValue:
  def valueType: CovariateType
  def description: String

object CovariateValue:
  private final case class CategoricalValue(label: String) extends CovariateValue:
    def valueType: CovariateType =
      CovariateType.Categorical

    def description: String =
      label

  private final case class ContinuousValue(value: Double) extends CovariateValue:
    def valueType: CovariateType =
      CovariateType.Continuous

    def description: String =
      ConnectivityText.double(value)

  private final case class IntegerValue(value: Int) extends CovariateValue:
    def valueType: CovariateType =
      CovariateType.Integer

    def description: String =
      value.toString

  private final case class BinaryValue(value: Boolean) extends CovariateValue:
    def valueType: CovariateType =
      CovariateType.Binary

    def description: String =
      value.toString

  def categorical(label: String): Either[ConnectivityError, CovariateValue] =
    val trimmed = label.trim
    if trimmed.isEmpty then Left(ConnectivityError.InvalidId("covariate value", label, "must be non-empty"))
    else Right(CategoricalValue(trimmed))

  def continuous(value: Double): Either[ConnectivityError, CovariateValue] =
    if value.isFinite then Right(ContinuousValue(value))
    else Left(ConnectivityError.InvalidScalar("covariate value", value, "must be finite"))

  def integer(value: Int): CovariateValue =
    IntegerValue(value)

  def binary(value: Boolean): CovariateValue =
    BinaryValue(value)

final class SubjectCovariates private (
    val subjectId: SubjectId,
    val values: Map[CovariateName, CovariateValue]
):
  def isEmpty: Boolean =
    values.isEmpty

  def value(name: CovariateName): Option[CovariateValue] =
    values.get(name)

  def columns: Vector[CovariateName] =
    values.keys.toVector.sortBy(_.value)

object SubjectCovariates:
  def from(
      subjectId: SubjectId,
      values: Map[CovariateName, CovariateValue]
  ): Either[ConnectivityError, SubjectCovariates] =
    if values.isEmpty then Left(ConnectivityError.InvalidPlan("subject covariates must contain at least one typed value"))
    else Right(new SubjectCovariates(subjectId, values))

  def unsafe(subjectId: SubjectId, values: Map[CovariateName, CovariateValue]): SubjectCovariates =
    from(subjectId, values).fold(error => throw new IllegalArgumentException(error.message), identity)
