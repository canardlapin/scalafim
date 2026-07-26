package scalafim.bids

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all.*

enum BidsIssueSeverity:
  case Warning
  case Error

enum BidsIssueCode:
  case InvalidPath
  case InvalidName
  case InvalidEntity
  case UnsupportedFileRole
  case UnrecognizedFile
  case InvalidQuery
  case InvalidTable
  case InvalidSidecar
  case MissingRequiredField
  case InconsistentMetadata
  case InvalidConfound

final case class BidsIssue private (
    code: BidsIssueCode,
    severity: BidsIssueSeverity,
    path: Option[BidsPath],
    field: Option[String],
    message: String
)

object BidsIssue:
  private[bids] def error(
      code: BidsIssueCode,
      path: Option[BidsPath],
      field: Option[String],
      message: String
  ): BidsIssue =
    make(code, BidsIssueSeverity.Error, path, field, message)

  private[bids] def warning(
      code: BidsIssueCode,
      path: Option[BidsPath],
      field: Option[String],
      message: String
  ): BidsIssue =
    make(code, BidsIssueSeverity.Warning, path, field, message)

  private def make(
      code: BidsIssueCode,
      severity: BidsIssueSeverity,
      path: Option[BidsPath],
      field: Option[String],
      message: String
  ): BidsIssue =
    val cleanMessage = message.trim
    require(cleanMessage.nonEmpty, "BIDS issue message must be non-empty")
    BidsIssue(code, severity, path, field.map(_.trim).filter(_.nonEmpty), cleanMessage)

  private[bids] def sortKey(issue: BidsIssue): (String, Int, String, String) =
    (
      issue.path.map(_.value).getOrElse(""),
      issue.code.ordinal,
      issue.field.getOrElse(""),
      issue.message
    )

final case class BidsIssueReport private (issues: Vector[BidsIssue]):
  def errors: Vector[BidsIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Error)

  def warnings: Vector[BidsIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Warning)

  def primary: BidsIssue =
    errors.headOption.getOrElse(issues.head)

object BidsIssueReport:
  def from(issues: IterableOnce[BidsIssue]): Option[BidsIssueReport] =
    val ordered = issues.iterator.toVector.sortBy(BidsIssue.sortKey)
    Option.when(ordered.nonEmpty)(BidsIssueReport(ordered))

  private[bids] def unsafe(issues: Vector[BidsIssue]): BidsIssueReport =
    require(issues.nonEmpty, "BidsIssueReport requires at least one issue")
    BidsIssueReport(issues.sortBy(BidsIssue.sortKey))

enum BidsValidationPolicy:
  case Collect
  case Strict

final case class BidsValidationReport[+A] private (value: A, issues: Vector[BidsIssue]):
  def errors: Vector[BidsIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Error)

  def warnings: Vector[BidsIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Warning)

  def hasErrors: Boolean =
    errors.nonEmpty

  def map[B](f: A => B): BidsValidationReport[B] =
    BidsValidationReport.from(f(value), issues)

  def enforce(policy: BidsValidationPolicy): Either[BidsIssueReport, BidsValidationReport[A]] =
    policy match
      case BidsValidationPolicy.Collect => Right(this)
      case BidsValidationPolicy.Strict =>
        if hasErrors then Left(BidsIssueReport.unsafe(issues))
        else Right(this)

object BidsValidationReport:
  def clean[A](value: A): BidsValidationReport[A] =
    BidsValidationReport(value, Vector.empty)

  private[bids] def from[A](value: A, issues: IterableOnce[BidsIssue]): BidsValidationReport[A] =
    BidsValidationReport(value, issues.iterator.toVector.sortBy(BidsIssue.sortKey))

object BidsMetadataValidation:
  def datasetDescription(path: BidsPath, json: JsonValue.Obj): Vector[BidsIssue] =
    val issues = Vector.newBuilder[BidsIssue]
    requiredNonEmptyString(path, json, "Name").foreach(issues += _)
    requiredNonEmptyString(path, json, "BIDSVersion").foreach(issues += _)
    json.fields.get("DatasetType").foreach {
      case JsonValue.Str(value) if value == "raw" || value == "derivative" => ()
      case JsonValue.Str(value) =>
        issues += invalidMetadata(path, "DatasetType", s"expected 'raw' or 'derivative', got '$value'")
      case _ =>
        issues += invalidSidecar(path, "DatasetType", "must be a string")
    }
    json.fields.get("DatasetLinks").foreach {
      case JsonValue.Obj(links) =>
        links.toVector.sortBy(_._1).foreach { case (name, value) =>
          value match
            case JsonValue.Str(link) if link.trim.nonEmpty => ()
            case JsonValue.Str(_) =>
              issues += invalidMetadata(path, s"DatasetLinks.$name", "link must be non-empty")
            case _ =>
              issues += invalidSidecar(path, s"DatasetLinks.$name", "link must be a string")
        }
      case _ =>
        issues += invalidSidecar(path, "DatasetLinks", "must be an object")
    }
    issues.result()

  def resolvedSidecar(path: BidsPath, json: JsonValue.Obj): Vector[BidsIssue] =
    val issues = Vector.newBuilder[BidsIssue]
    json.fields.get("TaskName").foreach {
      case JsonValue.Str(value) if value.trim.nonEmpty => ()
      case JsonValue.Str(_) =>
        issues += invalidMetadata(path, "TaskName", "must be non-empty")
      case _ =>
        issues += invalidSidecar(path, "TaskName", "must be a string")
    }
    json.fields.get("RepetitionTime").foreach {
      case JsonValue.Num(value) if value.isFinite && value > 0.0 => ()
      case JsonValue.Num(value) =>
        issues += invalidMetadata(path, "RepetitionTime", s"must be finite and positive, got $value")
      case _ =>
        issues += invalidSidecar(path, "RepetitionTime", "must be a number")
    }
    json.fields.get("VolumeTiming").foreach {
      case JsonValue.Arr(values) =>
        val timings = values.map(_.asNumber)
        if timings.lengthCompare(2) < 0 then
          issues += invalidMetadata(path, "VolumeTiming", "must contain at least two values")
        if timings.exists(_.forall(value => !value.isFinite)) then
          issues += invalidSidecar(path, "VolumeTiming", "must contain only finite numbers")
        else
          val numbers = timings.flatten
          if numbers.sliding(2).exists {
              case Vector(left, right) => right <= left
              case _                   => false
            }
          then issues += invalidMetadata(path, "VolumeTiming", "values must be strictly increasing")
      case _ =>
        issues += invalidSidecar(path, "VolumeTiming", "must be an array")
    }
    json.fields.get("SliceTiming").foreach {
      case JsonValue.Arr(values) =>
        val timings = values.map(_.asNumber)
        if timings.isEmpty then
          issues += invalidMetadata(path, "SliceTiming", "must contain at least one value")
        if timings.exists(_.forall(value => !value.isFinite || value < 0.0)) then
          issues += invalidSidecar(path, "SliceTiming", "must contain only finite non-negative numbers")
      case _ =>
        issues += invalidSidecar(path, "SliceTiming", "must be an array")
    }
    if json.fields.contains("RepetitionTime") && json.fields.contains("VolumeTiming") then
      issues += invalidMetadata(path, "RepetitionTime", "RepetitionTime and VolumeTiming are mutually exclusive")
    issues.result()

  private def requiredNonEmptyString(
      path: BidsPath,
      json: JsonValue.Obj,
      field: String
  ): Option[BidsIssue] =
    json.fields.get(field) match
      case None =>
        Some(BidsIssue.error(
          BidsIssueCode.MissingRequiredField,
          Some(path),
          Some(field),
          s"required field '$field' is missing"
        ))
      case Some(JsonValue.Str(value)) if value.trim.nonEmpty => None
      case Some(JsonValue.Str(_)) => Some(invalidMetadata(path, field, "must be non-empty"))
      case Some(_) => Some(invalidSidecar(path, field, "must be a string"))

  private def invalidSidecar(path: BidsPath, field: String, message: String): BidsIssue =
    BidsIssue.error(BidsIssueCode.InvalidSidecar, Some(path), Some(field), message)

  private def invalidMetadata(path: BidsPath, field: String, message: String): BidsIssue =
    BidsIssue.error(BidsIssueCode.InconsistentMetadata, Some(path), Some(field), message)

private[bids] object BidsValidation:
  type Check[A] = ValidatedNec[BidsIssue, A]

  def valid[A](value: A): Check[A] =
    Validated.Valid(value)

  def invalid[A](issue: BidsIssue): Check[A] =
    Validated.Invalid(NonEmptyChain.one(issue))

  def all(checks: IterableOnce[Check[Unit]]): Check[Unit] =
    checks.iterator.foldLeft(valid(())) { (combined, next) =>
      (combined, next).mapN((_, _) => ())
    }

  def toEither[A](check: Check[A]): Either[BidsIssueReport, A] =
    check.toEither.left.map(issues => BidsIssueReport.unsafe(issues.toChain.toVector))
