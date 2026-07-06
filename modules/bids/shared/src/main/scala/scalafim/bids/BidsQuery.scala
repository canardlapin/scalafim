package scalafim.bids

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

final case class EntityFilter(key: EntityKey, values: Vector[String]):
  def nonEmpty: Boolean = values.nonEmpty

object EntityFilter:
  def apply(key: EntityKey, value: String): EntityFilter =
    EntityFilter(key, Vector(value))

  def from(key: EntityKey, values: Vector[String]): Either[BidsError, EntityFilter] =
    if values.isEmpty then Left(BidsError.InvalidQuery("EntityFilter requires at least one value"))
    else Right(EntityFilter(key, values))

final case class BidsQuery(
    filename: Vector[String] = Vector(".*"),
    filters: Vector[EntityFilter] = Vector.empty,
    matchMode: MatchMode = MatchMode.Regex,
    requireEntity: Boolean = false,
    scope: BidsScope = BidsScope.All,
    pipeline: Option[PipelineName] = None,
    strict: Boolean = true
)

object BidsQuery:
  val All: BidsQuery = BidsQuery()

  def from(
      filename: Vector[String] = Vector(".*"),
      filters: Vector[EntityFilter] = Vector.empty,
      matchMode: MatchMode = MatchMode.Regex,
      requireEntity: Boolean = false,
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None,
      strict: Boolean = true
  ): Either[BidsError, BidsQuery] =
    if filename.isEmpty then Left(BidsError.InvalidQuery("BidsQuery.filename requires at least one pattern"))
    else if filters.exists(!_.nonEmpty) then Left(BidsError.InvalidQuery("EntityFilter requires at least one value"))
    else
      Matching.validateRegexPatterns(filename).map { filename =>
        BidsQuery(
          filename = filename,
          filters = filters,
          matchMode = matchMode,
          requireEntity = requireEntity,
          scope = scope,
          pipeline = pipeline,
          strict = strict
        )
      }

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
