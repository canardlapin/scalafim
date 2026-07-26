package scalafim.bids

import cats.syntax.all.*
import scala.util.control.NonFatal

enum BidsScope:
  case All
  case Raw
  case Derivatives

enum MatchMode:
  case Regex
  case Exact
  case Glob

enum QueryPattern:
  case Regex(pattern: String)
  case Exact(value: String)
  case Glob(pattern: String)

  def asFilenameRegex: String =
    this match
      case QueryPattern.Regex(pattern) => pattern
      case QueryPattern.Exact(value) => "^" + Matching.quoteRegex(value) + "$"
      case QueryPattern.Glob(pattern) => Matching.globToRegex(pattern)

object QueryPattern:
  def regex(pattern: String): Either[BidsError, QueryPattern] =
    Matching.validateRegex(pattern).map(QueryPattern.Regex(_))

  def exact(value: String): Either[BidsError, QueryPattern] =
    checkedPatternValue(value, "exact query value").map(QueryPattern.Exact(_))

  def glob(pattern: String): Either[BidsError, QueryPattern] =
    checkedPatternValue(pattern, "glob query pattern").map(QueryPattern.Glob(_))

  private def checkedPatternValue(value: String, label: String): Either[BidsError, String] =
    val clean = value.trim
    if clean.isEmpty then Left(BidsError.InvalidQuery(s"$label must be non-empty"))
    else Right(clean)

final class EntityFilter private (val key: EntityKey, val values: Vector[String]):
  def nonEmpty: Boolean = values.nonEmpty

  override def equals(other: Any): Boolean =
    other match
      case that: EntityFilter => key == that.key && values == that.values
      case _                  => false

  override def hashCode(): Int =
    (key, values).##

  override def toString: String =
    s"EntityFilter($key,$values)"

object EntityFilter:
  def from(key: EntityKey, value: String): Either[BidsError, EntityFilter] =
    from(key, Vector(value))

  def from(key: EntityKey, values: Vector[String]): Either[BidsError, EntityFilter] =
    if values.isEmpty then Left(BidsError.InvalidQuery("EntityFilter requires at least one value"))
    else if values.exists(_.trim.isEmpty) then Left(BidsError.InvalidQuery("EntityFilter values must be non-empty"))
    else Right(new EntityFilter(key, values.map(_.trim)))

  private[bids] def unsafe(key: EntityKey, value: String): EntityFilter =
    unsafe(key, Vector(value))

  private[bids] def unsafe(key: EntityKey, values: Vector[String]): EntityFilter =
    require(values.nonEmpty, "EntityFilter requires at least one value")
    require(values.forall(_.trim.nonEmpty), "EntityFilter values must be non-empty")
    new EntityFilter(key, values.map(_.trim))

final class BidsQuery private (
    val filename: Vector[String],
    val filters: Vector[EntityFilter],
    val matchMode: MatchMode,
    val requireEntity: Boolean,
    val scope: BidsScope,
    val pipeline: Option[PipelineName],
    val strict: Boolean
):
  override def equals(other: Any): Boolean =
    other match
      case that: BidsQuery =>
        filename == that.filename &&
          filters == that.filters &&
          matchMode == that.matchMode &&
          requireEntity == that.requireEntity &&
          scope == that.scope &&
          pipeline == that.pipeline &&
          strict == that.strict
      case _ => false

  override def hashCode(): Int =
    (filename, filters, matchMode, requireEntity, scope, pipeline, strict).##

  override def toString: String =
    s"BidsQuery($filename,$filters,$matchMode,$requireEntity,$scope,$pipeline,$strict)"

object BidsQuery:
  val All: BidsQuery = unsafe()

  def from(
      filename: Vector[String] = Vector(".*"),
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): Either[BidsError, BidsQuery] =
    fromChecked(filename, filters, matchMode, requireEntity, scope, pipeline, strict)
      .left
      .map(report => BidsError.InvalidQuery(report.primary.message))

  def fromChecked(
      filename: Vector[String] = Vector(".*"),
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): Either[BidsIssueReport, BidsQuery] =
    val filenamePresence =
      if filename.nonEmpty then BidsValidation.valid(())
      else
        BidsValidation.invalid(
          BidsIssue.error(
            BidsIssueCode.InvalidQuery,
            None,
            Some("filename"),
            "BidsQuery.filename requires at least one pattern"
          )
        )
    val filenameChecks =
      BidsValidation.all(
        filename.zipWithIndex.flatMap { case (pattern, index) =>
          Matching.validateRegex(pattern).left.toOption.map { error =>
            BidsValidation.invalid(
              BidsIssue.error(
                BidsIssueCode.InvalidQuery,
                None,
                Some(s"filename[$index]"),
                error.message
              )
            )
          }
        }
      )
    val filterPresenceChecks =
      BidsValidation.all(
        filters.zipWithIndex.collect { case (filter, index) if !filter.nonEmpty =>
          BidsValidation.invalid(
            BidsIssue.error(
              BidsIssueCode.InvalidQuery,
              None,
              Some(s"filters[$index]"),
              s"EntityFilter '${filter.key.short}' requires at least one value"
            )
          )
        }
      )
    val filterPatternChecks =
      if matchMode != MatchMode.Regex then BidsValidation.valid(())
      else
        BidsValidation.all(
          filters.zipWithIndex.flatMap { case (filter, filterIndex) =>
            filter.values.zipWithIndex.flatMap { case (pattern, valueIndex) =>
              Matching.validateRegex(pattern).left.toOption.map { error =>
                BidsValidation.invalid(
                  BidsIssue.error(
                    BidsIssueCode.InvalidQuery,
                    None,
                    Some(s"filters[$filterIndex].values[$valueIndex]"),
                    error.message
                  )
                )
              }
            }
          }
        )

    BidsValidation.toEither(
      (filenamePresence, filenameChecks, filterPresenceChecks, filterPatternChecks).mapN { (_, _, _, _) =>
        new BidsQuery(
          filename = filename,
          filters = filters,
          matchMode = matchMode,
          requireEntity = requireEntity,
          scope = scope,
          pipeline = pipeline,
          strict = strict
        )
      }
    )

  private[bids] def unsafe(
      filename: Vector[String] = Vector(".*"),
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): BidsQuery =
    require(filename.nonEmpty, "BidsQuery.filename requires at least one pattern")
    require(filters.forall(_.nonEmpty), "EntityFilter requires at least one value")
    new BidsQuery(filename, filters, matchMode, requireEntity, scope, pipeline, strict)

  def fromPatterns(
      filename: Vector[QueryPattern],
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): Either[BidsError, BidsQuery] =
    if filename.isEmpty then Left(BidsError.InvalidQuery("BidsQuery.filename requires at least one pattern"))
    else
      from(
        filename = filename.map(_.asFilenameRegex),
        filters = filters,
        matchMode = matchMode,
        requireEntity = requireEntity,
        scope = scope,
        pipeline = pipeline,
        strict = strict
      )

private[bids] object Matching:
  def validateRegexPatterns(patterns: Vector[String]): Either[BidsError, Vector[String]] =
    BidsEither.traverse(patterns)(validateRegex)

  def validateRegex(pattern: String): Either[BidsError, String] =
    val clean = pattern.trim
    if clean.isEmpty then Left(BidsError.InvalidQuery("regex pattern must be non-empty"))
    else
      try
        clean.r
        Right(clean)
      catch
        case NonFatal(ex) => Left(BidsError.InvalidQuery(s"invalid regex '$pattern': ${ex.getMessage}"))

  def filenameMatches(value: String, patterns: Vector[String]): Boolean =
    patterns.exists(regexFind(value, _))

  def entityMatches(value: String, filter: EntityFilter, mode: MatchMode): Boolean =
    mode match
      case MatchMode.Regex =>
        filter.values.exists(regexFind(value, _))
      case MatchMode.Exact =>
        filter.values.contains(value)
      case MatchMode.Glob =>
        filter.values.exists(pattern => value.matches(globToRegex(pattern)))

  def isWildcard(filter: EntityFilter, mode: MatchMode): Boolean =
    mode == MatchMode.Regex && filter.values.length == 1 && filter.values.head == ".*"

  def globToRegex(glob: String): String =
    val out = new StringBuilder("^")
    glob.foreach {
      case '*' => out.append(".*")
      case '?' => out.append(".")
      case c if "\\.[]{}()+-^$|".contains(c) => out.append('\\').append(c)
      case c => out.append(c)
    }
    out.append("$").toString

  def quoteRegex(value: String): String =
    val out = new StringBuilder
    value.foreach {
      case c if "\\.[]{}()+-^$|*?".contains(c) => out.append('\\').append(c)
      case c => out.append(c)
    }
    out.toString

  private def regexFind(value: String, pattern: String): Boolean =
    try pattern.r.findFirstIn(value).isDefined
    catch case NonFatal(_) => false
