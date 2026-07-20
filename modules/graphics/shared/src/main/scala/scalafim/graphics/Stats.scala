package scalafim.graphics

/** Values created by a statistical transformation rather than read directly
  * from an input row. The type parameter keeps each computed field honest.
  */
enum ComputedAesthetic[A](val label: String):
  case Count extends ComputedAesthetic[Double]("count")
  case Proportion extends ComputedAesthetic[Double]("proportion")
  case Density extends ComputedAesthetic[Double]("density")
  case Position extends ComputedAesthetic[Double]("position")
  case Mean extends ComputedAesthetic[Double]("mean")
  case Lower extends ComputedAesthetic[Double]("lower")
  case Upper extends ComputedAesthetic[Double]("upper")
  case BinLower extends ComputedAesthetic[Double]("bin_lower")
  case BinUpper extends ComputedAesthetic[Double]("bin_upper")
  case BinWidth extends ComputedAesthetic[Double]("bin_width")
  case BinMidpoint extends ComputedAesthetic[Double]("bin_midpoint")

/** A finite typed record of computed aesthetics. Future statistics can add
  * fields without turning their output into a string-keyed map.
  */
final case class ComputedValues private (
    count: Option[Double] = None,
    proportion: Option[Double] = None,
    density: Option[Double] = None,
    position: Option[Double] = None,
    mean: Option[Double] = None,
    lower: Option[Double] = None,
    upper: Option[Double] = None,
    binLower: Option[Double] = None,
    binUpper: Option[Double] = None,
    binWidth: Option[Double] = None,
    binMidpoint: Option[Double] = None
):
  def get[A](aesthetic: ComputedAesthetic[A]): Option[A] =
    aesthetic match
      case ComputedAesthetic.Count       => count
      case ComputedAesthetic.Proportion  => proportion
      case ComputedAesthetic.Density     => density
      case ComputedAesthetic.Position    => position
      case ComputedAesthetic.Mean        => mean
      case ComputedAesthetic.Lower       => lower
      case ComputedAesthetic.Upper       => upper
      case ComputedAesthetic.BinLower    => binLower
      case ComputedAesthetic.BinUpper    => binUpper
      case ComputedAesthetic.BinWidth    => binWidth
      case ComputedAesthetic.BinMidpoint => binMidpoint

object ComputedValues:
  val empty: ComputedValues =
    ComputedValues()

  private[graphics] def counted(count: Int, total: Int): ComputedValues =
    ComputedValues(
      count = Some(count.toDouble),
      proportion = Some(count.toDouble / total.toDouble)
    )

  private[graphics] def binned(
      count: Int,
      total: Int,
      binLower: Double,
      binUpper: Double
  ): ComputedValues =
    val width = binUpper - binLower
    val frequency = count.toDouble
    ComputedValues(
      count = Some(frequency),
      proportion = Some(frequency / total.toDouble),
      density = Some(frequency / (total.toDouble * width)),
      binLower = Some(binLower),
      binUpper = Some(binUpper),
      binWidth = Some(width),
      binMidpoint = Some(binLower + width / 2.0)
    )

  private[graphics] def summarized(
      position: Double,
      mean: Double,
      lower: Double,
      upper: Double,
      count: Int
  ): ComputedValues =
    ComputedValues(
      count = Some(count.toDouble),
      position = Some(position),
      mean = Some(mean),
      lower = Some(lower),
      upper = Some(upper)
    )

  private[graphics] def densityAt(position: Double, density: Double, count: Int): ComputedValues =
    ComputedValues(
      count = Some(density * count.toDouble),
      density = Some(density),
      position = Some(position)
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

opaque type BinCount = Int

object BinCount:
  def apply(value: Int): Either[GraphicsError, BinCount] =
    if value >= 1 then Right(value)
    else Left(GraphicsError.InvalidStatParameter("bin", "bin count >= 1", value.toString))

  def unsafe(value: Int): BinCount =
    apply(value).orThrow

  extension (value: BinCount) def toInt: Int = value

opaque type BinWidth = Double

object BinWidth:
  def apply(value: Double): Either[GraphicsError, BinWidth] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(GraphicsError.InvalidStatParameter("bin", "finite bin width > 0", value.toString))

  def unsafe(value: Double): BinWidth =
    apply(value).orThrow

  extension (value: BinWidth) def toDouble: Double = value

/** A histogram partition chosen by count, width, or explicit break points.
  * Construction is validated so the statistical kernel never sees an empty
  * or incoherent partition.
  */
sealed trait HistogramBins

object HistogramBins:
  private final case class ByCount(value: BinCount) extends HistogramBins
  private final case class ByWidth(value: BinWidth) extends HistogramBins
  private final case class AtBreaks(values: Vector[Double]) extends HistogramBins

  val default: HistogramBins =
    ByCount(BinCount.unsafe(30))

  def count(value: Int): Either[GraphicsError, HistogramBins] =
    BinCount(value).map(ByCount(_))

  def countUnsafe(value: Int): HistogramBins =
    count(value).orThrow

  def width(value: Double): Either[GraphicsError, HistogramBins] =
    BinWidth(value).map(ByWidth(_))

  def widthUnsafe(value: Double): HistogramBins =
    width(value).orThrow

  def breaks(values: Vector[Double]): Either[GraphicsError, HistogramBins] =
    if values.length < 2 then
      Left(GraphicsError.InvalidStatParameter("bin", "at least two breaks", values.mkString(", ")))
    else if values.exists(value => !value.isFinite) then
      Left(GraphicsError.InvalidStatParameter("bin", "finite breaks", values.mkString(", ")))
    else if values.sliding(2).exists(pair => pair(0) >= pair(1)) then
      Left(GraphicsError.InvalidStatParameter("bin", "strictly increasing breaks", values.mkString(", ")))
    else Right(AtBreaks(values))

  def breaksUnsafe(values: Vector[Double]): HistogramBins =
    breaks(values).orThrow

  private[graphics] def partition(spec: HistogramBins, minimum: Double, maximum: Double): Vector[Double] =
    spec match
      case ByCount(value) =>
        val count = value.toInt
        if minimum == maximum then Vector(minimum - 0.5, maximum + 0.5)
        else Vector.tabulate(count + 1)(idx => minimum + (maximum - minimum) * idx.toDouble / count.toDouble)
      case ByWidth(value) =>
        val width = value.toDouble
        val lower = math.floor(minimum / width) * width
        val upper = math.ceil(maximum / width) * width
        val bins = math.max(1, math.ceil((upper - lower) / width).toInt)
        Vector.tabulate(bins + 1)(idx => lower + idx.toDouble * width)
      case AtBreaks(values) => values

  private[graphics] def isExplicit(spec: HistogramBins): Boolean =
    spec.isInstanceOf[AtBreaks]

enum SummaryInterval:
  /** Arithmetic mean plus or minus one sample standard error. */
  case StandardError

  /** Arithmetic mean with the observed minimum and maximum. */
  case Range

opaque type DensityBandwidth = Double

object DensityBandwidth:
  def apply(value: Double): Either[GraphicsError, DensityBandwidth] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(GraphicsError.InvalidStatParameter("density", "finite bandwidth > 0", value.toString))

  def unsafe(value: Double): DensityBandwidth =
    apply(value).orThrow

  extension (value: DensityBandwidth) def toDouble: Double = value

opaque type DensityPoints = Int

object DensityPoints:
  def apply(value: Int): Either[GraphicsError, DensityPoints] =
    if value >= 2 then Right(value)
    else Left(GraphicsError.InvalidStatParameter("density", "grid points >= 2", value.toString))

  def unsafe(value: Int): DensityPoints =
    apply(value).orThrow

  extension (value: DensityPoints) def toInt: Int = value

/** Validated Gaussian kernel-density configuration. When bandwidth is absent,
  * the transform uses R's `bw.nrd0` rule. An absent domain spans the observed
  * data exactly.
  */
final case class DensityConfig private (
    bandwidth: Option[DensityBandwidth],
    points: DensityPoints,
    domain: Option[Interval]
)

object DensityConfig:
  val default: DensityConfig =
    DensityConfig(None, DensityPoints.unsafe(512), None)

  def automatic(points: Int = 512, domain: Option[Interval] = None): Either[GraphicsError, DensityConfig] =
    DensityPoints(points).map(DensityConfig(None, _, domain))

  def fixed(
      bandwidth: Double,
      points: Int = 512,
      domain: Option[Interval] = None
  ): Either[GraphicsError, DensityConfig] =
    for
      resolvedBandwidth <- DensityBandwidth(bandwidth)
      resolvedPoints <- DensityPoints(points)
    yield DensityConfig(Some(resolvedBandwidth), resolvedPoints, domain)

  def fixedUnsafe(
      bandwidth: Double,
      points: Int = 512,
      domain: Option[Interval] = None
  ): DensityConfig =
    fixed(bandwidth, points, domain).orThrow

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

  final case class Bin[Row](x: Row => Double, bins: HistogramBins = HistogramBins.default) extends Stat[Row]:
    override val label: String = "bin"

  final case class Summary[Row](
      x: Row => Double,
      y: Row => Double,
      interval: SummaryInterval = SummaryInterval.StandardError
  ) extends Stat[Row]:
    override val label: String = "summary"

  final case class Density[Row](x: Row => Double, config: DensityConfig = DensityConfig.default) extends Stat[Row]:
    override val label: String = "density"
