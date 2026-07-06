package scalafim.bids

enum BidsScope:
  case All
  case Raw
  case Derivatives

enum MatchMode:
  case Regex
  case Exact
  case Glob

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
      Right(
        BidsQuery(
          filename = filename,
          filters = filters,
          matchMode = matchMode,
          requireEntity = requireEntity,
          scope = scope,
          pipeline = pipeline,
          strict = strict
        )
      )

private[bids] object Matching:
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

  private def regexFind(value: String, pattern: String): Boolean =
    pattern.r.findFirstIn(value).isDefined
