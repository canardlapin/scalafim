package scalafim.graphics

/** Values created by a statistical transformation rather than read directly
  * from an input row. The type parameter keeps each computed field honest.
  */
enum ComputedAesthetic[A](val label: String):
  case Count extends ComputedAesthetic[Double]("count")
  case Proportion extends ComputedAesthetic[Double]("proportion")
  case Density extends ComputedAesthetic[Double]("density")
  case BinMidpoint extends ComputedAesthetic[Double]("bin_midpoint")

/** A finite typed record of computed aesthetics. Future statistics can add
  * fields without turning their output into a string-keyed map.
  */
final case class ComputedValues private (
    count: Option[Double] = None,
    proportion: Option[Double] = None,
    density: Option[Double] = None,
    binMidpoint: Option[Double] = None
):
  def get[A](aesthetic: ComputedAesthetic[A]): Option[A] =
    aesthetic match
      case ComputedAesthetic.Count       => count
      case ComputedAesthetic.Proportion  => proportion
      case ComputedAesthetic.Density     => density
      case ComputedAesthetic.BinMidpoint => binMidpoint

object ComputedValues:
  val empty: ComputedValues =
    ComputedValues()

  private[graphics] def counted(count: Int, total: Int): ComputedValues =
    ComputedValues(
      count = Some(count.toDouble),
      proportion = Some(count.toDouble / total.toDouble)
    )

/** One output row from a statistic. `members` makes aggregation inspectable;
  * `source` is the stable representative used by source-oriented diagnostics.
  */
final case class StatRow[Row] private[graphics] (
    source: Row,
    members: Vector[Row],
    category: Option[String],
    computed: ComputedValues
):
  require(members.nonEmpty, "`members` must be non-empty")

/** Immutable, typed output of a statistical layer transformation. */
final case class StatFrame[Row] private[graphics] (
    rows: Vector[StatRow[Row]],
    computedAesthetics: Set[ComputedAesthetic[?]]
)

enum CountOrder:
  /** Preserve the first occurrence of every category. */
  case Encountered

  /** Use platform-stable Unicode lexicographic order. */
  case Lexicographic

  /** Follow declared levels, then append undeclared observed levels in first
    * occurrence order.
    */
  case Declared(domain: DiscreteDomain)

  private[graphics] def arrange(observed: Vector[String]): Vector[String] =
    val distinct = observed.distinct
    this match
      case Encountered => distinct
      case Lexicographic => distinct.sorted
      case Declared(domain) =>
        val levels = domain.levels
        levels.filter(distinct.contains) ++ distinct.filterNot(levels.contains)

object CountOrder:
  def declared(levels: Vector[String]): Either[GraphicsError, CountOrder] =
    DiscreteDomain.ordered(levels).map(CountOrder.Declared(_))

  def declaredUnsafe(levels: Vector[String]): CountOrder =
    declared(levels).orThrow

sealed trait Stat[-Row]:
  def label: String

object Stat:
  case object Identity extends Stat[Any]:
    override val label: String = "identity"

  final case class Count[Row](
      x: Row => String,
      order: CountOrder = CountOrder.Encountered,
      scaleName: GraphicsName = GraphicsName.unsafe("x")
  ) extends Stat[Row]:
    override val label: String = "count"
