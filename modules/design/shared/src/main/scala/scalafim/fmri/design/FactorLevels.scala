package scalafim.fmri.design

import scalafim.fmri.design.contrast.LevelId

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** A declared, stable level set for one categorical factor.
  *
  * The order is part of the factor's scientific identity.  It is therefore
  * supplied by the caller rather than inferred from the first subject/run in
  * which a factor happens to be observed.
  */
final case class FactorLevelSet private (factor: FactorId, levels: Vector[LevelId]):
  require(levels.nonEmpty, "a factor level set must contain at least one level")
  require(levels.distinct.length == levels.length, "factor levels must be unique")

  def values: Vector[String] = levels.map(_.value)

  def indexOf(value: String): Option[Int] =
    levels.indexWhere(_.value == value) match
      case -1 => None
      case ix => Some(ix)

  def canonical: String =
    s"${factor.value}=[${levels.map(_.value).mkString(",")}]"

object FactorLevelSet:
  /** Derive the declared order from a Scala enum's source-level case order.
    *
    * This is an ergonomic entrance to [[from]], not a second schema model: the
    * compiler supplies the case labels and the ordinary validated constructor
    * remains authoritative.
    */
  inline def fromEnum[A](factor: FactorId)(using mirror: Mirror.SumOf[A]): Either[DesignError, FactorLevelSet] =
    val labels = constValueTuple[mirror.MirroredElemLabels].productIterator.map(_.toString).toVector
    from(factor, labels)

  def from(factor: FactorId, levels: Seq[String]): Either[DesignError, FactorLevelSet] =
    if levels.isEmpty then
      Left(DesignError.InvalidSchema(s"factor '${factor.value}' must declare at least one level"))
    else
      val parsed = levels.toVector.map { level =>
        LevelId(level).left.map(error => DesignError.InvalidSchema(error.message))
      }
      parsed.collectFirst { case Left(error) => error } match
        case Some(error) => Left(error)
        case None =>
          val ids = parsed.collect { case Right(level) => level }
          if ids.distinct.length != ids.length then
            Left(DesignError.InvalidSchema(s"factor '${factor.value}' declares duplicate levels"))
          else Right(FactorLevelSet(factor, ids))

  def unsafe(factor: FactorId, levels: Seq[String]): FactorLevelSet =
    from(factor, levels).fold(error => throw new IllegalArgumentException(error.message), identity)

/** An ordered collection of declared factor level sets. */
final case class FactorLevelRegistry private (sets: Vector[FactorLevelSet]):
  require(sets.map(_.factor.value).distinct.length == sets.length, "factor registry entries must have unique factors")

  def get(factor: FactorId): Option[FactorLevelSet] =
    sets.find(_.factor.value == factor.value)

  def factorIds: Vector[FactorId] = sets.map(_.factor)

  def isEmpty: Boolean = sets.isEmpty

  def canonical: String = sets.sortBy(_.factor.value).map(_.canonical).mkString("registry{") + "}"

object FactorLevelRegistry:
  val empty: FactorLevelRegistry = FactorLevelRegistry(Vector.empty)

  def from(sets: Seq[FactorLevelSet]): Either[DesignError, FactorLevelRegistry] =
    val xs = sets.toVector.sortBy(_.factor.value)
    if xs.map(_.factor.value).distinct.length != xs.length then
      Left(DesignError.InvalidSchema("factor registry contains duplicate factor identities"))
    else Right(FactorLevelRegistry(xs))

  def of(entries: (String, Seq[String])*): Either[DesignError, FactorLevelRegistry] =
    val parsed = entries.toVector.map { case (factor, levels) =>
      FactorId(factor).flatMap(FactorLevelSet.from(_, levels))
    }
    parsed.collectFirst { case Left(error) => error } match
      case Some(error) => Left(error)
      case None       => from(parsed.collect { case Right(set) => set })

/** The boundary at which independently loaded factor schemas must agree.
  *
  * A single compiled design normally uses one global registry.  These scopes
  * describe the common upstream cases where several run- or subject-local
  * registries must first be bound to that one stable registry.
  */
enum FactorSchemaScope:
  case PerRun
  case PerSubject

  def label: String =
    this match
      case PerRun     => "run"
      case PerSubject => "subject"

/** A validated binding of independently loaded factor schemas.
  *
  * The first schema supplies the canonical factor and level order.  Every
  * subsequent source must declare exactly the same factor identities and
  * ordered levels; a missing factor or a reordered level is a typed error.
  */
final case class FactorSchemaBinding private (
    scope: FactorSchemaScope,
    registry: FactorLevelRegistry,
    sources: Vector[String]
):
  require(sources.nonEmpty, "a factor schema binding must have at least one source")
  require(sources.distinct.length == sources.length, "factor schema binding sources must be unique")

  def canonical: String =
    s"factor-schema(${scope.label};sources=${sources.sorted.mkString(",")};${registry.canonical})"

object FactorSchemaBinding:
  def perRun(entries: (String, FactorLevelRegistry)*): Either[DesignError, FactorSchemaBinding] =
    bind(FactorSchemaScope.PerRun, entries.toVector)

  def perSubject(entries: (String, FactorLevelRegistry)*): Either[DesignError, FactorSchemaBinding] =
    bind(FactorSchemaScope.PerSubject, entries.toVector)

  def bind(
      scope: FactorSchemaScope,
      entries: Seq[(String, FactorLevelRegistry)]
  ): Either[DesignError, FactorSchemaBinding] =
    val values = entries.toVector
    if values.isEmpty then
      Left(DesignError.InvalidSchema(s"${scope.label} factor schema binding requires at least one source"))
    else if values.exists(_._1.trim.isEmpty) then
      Left(DesignError.InvalidSchema(s"${scope.label} factor schema binding source labels must be non-empty"))
    else if values.map(_._1).distinct.length != values.length then
      Left(DesignError.InvalidSchema(s"${scope.label} factor schema binding source labels must be unique"))
    else
      val expected = values.head._2
      values.tail.iterator
        .map { case (source, observed) => compare(scope, source, expected, observed) }
        .collectFirst { case Left(error) => error } match
        case Some(error) => Left(error)
        case None       => Right(FactorSchemaBinding(scope, expected, values.map(_._1)))

  private def compare(
      scope: FactorSchemaScope,
      source: String,
      expected: FactorLevelRegistry,
      observed: FactorLevelRegistry
  ): Either[DesignError, Unit] =
    val factors = (expected.factorIds ++ observed.factorIds).distinct.sortBy(_.value)
    factors.iterator
      .map { factor =>
        val expectedLevels = expected.get(factor).map(_.values).getOrElse(Vector.empty)
        val observedLevels = observed.get(factor).map(_.values).getOrElse(Vector.empty)
        if expectedLevels == observedLevels then Right(())
        else
          Left(
            DesignError.IncompatibleFactorSchema(
              scope = scope.label,
              source = source,
              factor = factor.value,
              expected = expectedLevels,
              observed = observedLevels
            )
          )
      }
      .collectFirst { case Left(error) => error }
      .fold[Either[DesignError, Unit]](Right(()))(Left(_))

/** Evidence for a factor's observations within one stable run partition. */
final case class FactorPartitionAudit(
    partition: String,
    observed: Vector[LevelId]
):
  require(partition.trim.nonEmpty, "factor audit partition must be non-empty")
  require(observed.distinct.length == observed.length, "partition factor levels must be unique")

/** How a compiled term handles cells whose declared factor combination has no
  * event rows.  [[UseDropEmptyFlag]] is the compatibility adapter for the
  * legacy boolean `dropEmpty` option; the other cases make the scientific
  * choice explicit.
  */
enum EmptyCellPolicy:
  case UseDropEmptyFlag
  case Omit
  case RetainZero
  case Reject

  def label: String =
    this match
      case UseDropEmptyFlag => "use-drop-empty-flag"
      case Omit             => "omit"
      case RetainZero       => "retain-zero"
      case Reject           => "reject"

/** The realized treatment of a declared cell with no event rows. */
enum EmptyCellDisposition:
  case Omitted
  case RetainedZero

/** Scope in which a declared factor cell was unobserved. */
enum EmptyCellScope:
  case Global
  case Run(index: RunIndex)

  def run: Option[RunIndex] =
    this match
      case Global     => None
      case Run(index) => Some(index)

  def label: String =
    this match
      case Global     => "global"
      case Run(index) => s"run-${index.oneBased}"

/** Structured evidence for one declared-but-unobserved factorial cell. */
final case class EmptyCellAudit(
    term: Option[TermId],
    cell: CellKey,
    policy: EmptyCellPolicy,
    disposition: EmptyCellDisposition,
    scope: EmptyCellScope = EmptyCellScope.Global
):
  require(policy != EmptyCellPolicy.Reject, "rejected empty cells do not produce a compiled audit")
  require(
    policy != EmptyCellPolicy.Omit || disposition == EmptyCellDisposition.Omitted,
    "Omit empty-cell policy must produce an omitted disposition"
  )
  require(
    policy != EmptyCellPolicy.RetainZero || disposition == EmptyCellDisposition.RetainedZero,
    "RetainZero empty-cell policy must produce a retained-zero disposition"
  )

  def run: Option[RunIndex] = scope.run

  def canonical: String =
    s"term=${term.fold("")(_.value)};cell=${cell.canonical};scope=${scope.label};policy=${policy.label};disposition=${disposition.toString.toLowerCase}"

/** Evidence about the declared and actually observed levels of one factor. */
final case class FactorLevelAudit(
    factor: FactorId,
    declared: Option[Vector[LevelId]],
    observed: Vector[LevelId],
    partitions: Vector[FactorPartitionAudit] = Vector.empty
):
  require(observed.distinct.length == observed.length, "observed factor levels must be unique")
  require(declared.forall(levels => levels.nonEmpty && levels.distinct.length == levels.length), "declared factor levels must be non-empty and unique")
  require(
    declared.forall(levels => observed.forall(level => levels.contains(level))),
    "observed factor levels must belong to the declared level set"
  )
  require(
    partitions.map(_.partition).distinct.length == partitions.length,
    "factor audit partitions must be unique"
  )
  require(
    declared.forall(levels => partitions.forall(partition => partition.observed.forall(levels.contains))),
    "partition factor levels must belong to the declared level set"
  )

  def canonical: String =
    val declared0 = declared.fold("inferred")(_.map(_.value).mkString("[", ",", "]"))
    val observed0 = observed.map(_.value).mkString("[", ",", "]")
    val partitions0 = partitions
      .sortBy(_.partition)
      .map(partition => s"${partition.partition}=[${partition.observed.map(_.value).mkString(",")}]")
      .mkString("{") + "}"
    s"${factor.value}:declared=$declared0;observed=$observed0;partitions=$partitions0"
