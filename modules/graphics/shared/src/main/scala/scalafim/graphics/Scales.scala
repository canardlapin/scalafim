package scalafim.graphics

final case class Interval private (lower: Double, upper: Double):
  require(lower.isFinite, "`lower` must be finite")
  require(upper.isFinite, "`upper` must be finite")
  require(lower <= upper, "`lower` must be <= `upper`")

  def width: Double =
    upper - lower

  def contains(value: Double): Boolean =
    value.isFinite && value >= lower && value <= upper

  def union(that: Interval): Interval =
    Interval.unsafe(math.min(lower, that.lower), math.max(upper, that.upper))

  def rescale(value: Double): Double =
    if width == 0.0 then 0.5
    else (value - lower) / width

object Interval:
  def apply(lower: Double, upper: Double): Either[GraphicsError, Interval] =
    if lower.isFinite && upper.isFinite && lower <= upper then Right(new Interval(lower, upper))
    else Left(GraphicsError.InvalidInterval(lower, upper))

  def unsafe(lower: Double, upper: Double): Interval =
    apply(lower, upper).orThrow

  def train(values: IterableOnce[Double]): Either[GraphicsError, Interval] =
    trainOption(values).toRight(GraphicsError.EmptyContinuousRange)

  def trainOption(values: IterableOnce[Double]): Option[Interval] =
    val finite = values.iterator.filter(_.isFinite)
    if !finite.hasNext then None
    else
      var lo = finite.next()
      var hi = lo
      while finite.hasNext do
        val x = finite.next()
        if x < lo then lo = x
        if x > hi then hi = x
      Some(unsafe(lo, hi))

final case class ContinuousRange private (interval: Option[Interval]):
  def isEmpty: Boolean =
    interval.isEmpty

  def train(values: IterableOnce[Double]): ContinuousRange =
    Interval.trainOption(values) match
      case None       => this
      case Some(next) => ContinuousRange(Some(interval.fold(next)(_.union(next))))

  def requireTrained: Either[GraphicsError, Interval] =
    interval.toRight(GraphicsError.EmptyContinuousRange)

object ContinuousRange:
  val empty: ContinuousRange =
    ContinuousRange(None)

  def from(values: IterableOnce[Double]): ContinuousRange =
    empty.train(values)

final case class TransformDomain private (lower: Double, upper: Double):
  require(!lower.isNaN, "`lower` must not be NaN")
  require(!upper.isNaN, "`upper` must not be NaN")
  require(lower < upper, "`lower` must be < `upper`")

  def contains(value: Double): Boolean =
    value.isFinite && value >= lower && value <= upper

object TransformDomain:
  val all: TransformDomain =
    TransformDomain(Double.NegativeInfinity, Double.PositiveInfinity)

  def apply(name: String, lower: Double, upper: Double): Either[GraphicsError, TransformDomain] =
    if !lower.isNaN && !upper.isNaN && lower < upper then Right(new TransformDomain(lower, upper))
    else Left(GraphicsError.InvalidTransformDomain(name, lower, upper))

  def unsafe(name: String, lower: Double, upper: Double): TransformDomain =
    apply(name, lower, upper).orThrow

trait Breaks:
  def apply(range: Interval): Vector[Double]

object Breaks:
  def count(n: Int): Either[GraphicsError, Breaks] =
    if n < 1 then Left(GraphicsError.InvalidBreakCount(n))
    else
      Right(new Breaks:
        override def apply(range: Interval): Vector[Double] =
          if n == 1 then Vector((range.lower + range.upper) / 2.0)
          else
            val step = range.width / (n - 1).toDouble
            Vector.tabulate(n)(i => range.lower + step * i)
      )

  def countUnsafe(n: Int): Breaks =
    count(n).orThrow

  def width(width: Double, offset: Double = 0.0): Either[GraphicsError, Breaks] =
    if !width.isFinite || width <= 0.0 then Left(GraphicsError.InvalidBreakWidth(width))
    else
      Right(new Breaks:
        override def apply(range: Interval): Vector[Double] =
          val first = math.ceil((range.lower - offset) / width) * width + offset
          val buf = Vector.newBuilder[Double]
          var x = first
          while x <= range.upper + width * 1e-12 do
            buf += x
            x += width
          buf.result()
      )

  val log10: Breaks =
    new Breaks:
      override def apply(range: Interval): Vector[Double] =
        if range.upper <= 0.0 then Vector.empty
        else
          val lo = math.ceil(math.log10(math.max(range.lower, Double.MinPositiveValue))).toInt
          val hi = math.floor(math.log10(range.upper)).toInt
          if hi < lo then Vector.empty
          else Vector.tabulate(hi - lo + 1)(i => math.pow(10.0, lo + i))

  val default: Breaks =
    countUnsafe(5)

trait Labeler:
  def apply(values: Vector[Double]): Vector[String]

object Labeler:
  val default: Labeler =
    values => values.map { value =>
      val rounded = math.rint(value)
      if math.abs(value - rounded) < 1e-10 then rounded.toLong.toString
      else value.toString
    }

final case class Transform private (
    name: GraphicsName,
    forward: Double => Double,
    backward: Double => Double,
    domain: TransformDomain,
    breaks: Breaks,
    labeler: Labeler
):
  def transform(value: Double): Either[GraphicsError, Double] =
    if !domain.contains(value) then Left(GraphicsError.TransformOutsideDomain(name.value, value))
    else
      val out = forward(value)
      if out.isFinite then Right(out)
      else Left(GraphicsError.TransformOutsideDomain(name.value, value))

  def inverse(value: Double): Either[GraphicsError, Double] =
    val out = backward(value)
    if out.isFinite then Right(out)
    else Left(GraphicsError.TransformOutsideDomain(name.value, value))

  def roundTrips(value: Double, tolerance: Double): Boolean =
    transform(value).flatMap(inverse).exists(restored => math.abs(restored - value) <= tolerance)

object Transform:
  def apply(
      name: String,
      forward: Double => Double,
      backward: Double => Double,
      domain: TransformDomain = TransformDomain.all,
      breaks: Breaks = Breaks.default,
      labeler: Labeler = Labeler.default
  ): Either[GraphicsError, Transform] =
    GraphicsName(name, "transform").map(Transform(_, forward, backward, domain, breaks, labeler))

  val identity: Transform =
    apply("identity", value => value, value => value).orThrow

  val reverse: Transform =
    apply("reverse", value => -value, value => -value).orThrow

  val log10: Transform =
    apply(
      "log10",
      value => math.log10(value),
      value => math.pow(10.0, value),
      TransformDomain.unsafe("log10", 0.0, Double.PositiveInfinity),
      breaks = Breaks.log10
    ).orThrow

  val sqrt: Transform =
    apply(
      "sqrt",
      value => math.sqrt(value),
      value => value * value,
      TransformDomain.unsafe("sqrt", 0.0, Double.PositiveInfinity)
    ).orThrow

enum OobPolicy:
  case Censor
  case Squish
  case Keep

  def apply(value: Double): Option[Double] =
    this match
      case Censor =>
        if value >= 0.0 && value <= 1.0 then Some(value) else None
      case Squish =>
        Some(math.max(0.0, math.min(1.0, value)))
      case Keep =>
        Some(value)

trait Palette[+A]:
  def apply(value: Double): A

object Palette:
  def constant[A](value: A): Palette[A] =
    _ => value

  val numeric: Palette[Double] =
    value => value

  def gradient(from: Rgba, to: Rgba): Palette[Rgba] =
    value =>
      val t = math.max(0.0, math.min(1.0, value))
      def channel(a: Int, b: Int): Int =
        math.rint(a + (b - a) * t).toInt
      Rgba.unsafe(
        channel(from.red, to.red),
        channel(from.green, to.green),
        channel(from.blue, to.blue),
        from.alpha + (to.alpha - from.alpha) * t
      )

trait DiscretePalette[+A]:
  def apply(index: Int, count: Int): A

object DiscretePalette:
  def values[A](values: Vector[A]): Either[GraphicsError, DiscretePalette[A]] =
    if values.isEmpty then Left(GraphicsError.EmptyPalette)
    else
      Right(new DiscretePalette[A]:
        override def apply(index: Int, count: Int): A =
          values(index % values.length)
      )

  def valuesUnsafe[A](values: Vector[A]): DiscretePalette[A] =
    DiscretePalette.values(values).orThrow

final case class ContinuousScale[A] private (
    name: GraphicsName,
    domain: Interval,
    transformedDomain: Interval,
    transform: Transform,
    palette: Palette[A],
    oob: OobPolicy
) extends Scale[Double, A]:
  override def mapValue(value: Double): Option[A] =
    transform.transform(value).toOption.flatMap { transformed =>
      oob(transformedDomain.rescale(transformed)).map(palette(_))
    }

  def mapValues(values: IterableOnce[Double]): Vector[Option[A]] =
    values.iterator.map(mapValue).toVector

  def breaks: Vector[Double] =
    transform.breaks(domain).filter(domain.contains)

  def labels: Vector[String] =
    transform.labeler(breaks)

object ContinuousScale:
  def train[A](
      name: String,
      values: IterableOnce[Double],
      palette: Palette[A],
      transform: Transform = Transform.identity,
      oob: OobPolicy = OobPolicy.Censor
  ): Either[GraphicsError, ContinuousScale[A]] =
    val domains = trainDomains(values, transform)
    for
      scaleName <- GraphicsName(name, "continuous scale")
      (domain, transformedDomain) <- domains
    yield ContinuousScale(scaleName, domain, transformedDomain, transform, palette, oob)

  private def trainDomains(
      values: IterableOnce[Double],
      transform: Transform
  ): Either[GraphicsError, (Interval, Interval)] =
    var seen = false
    var rawLo = 0.0
    var rawHi = 0.0
    var transformedLo = 0.0
    var transformedHi = 0.0
    val it = values.iterator
    while it.hasNext do
      val value = it.next()
      transform.transform(value).toOption.foreach { transformed =>
        if !seen then
          rawLo = value
          rawHi = value
          transformedLo = transformed
          transformedHi = transformed
          seen = true
        else
          if value < rawLo then rawLo = value
          if value > rawHi then rawHi = value
          if transformed < transformedLo then transformedLo = transformed
          if transformed > transformedHi then transformedHi = transformed
      }
    if seen then
      Right((Interval.unsafe(rawLo, rawHi), Interval.unsafe(transformedLo, transformedHi)))
    else Left(GraphicsError.EmptyContinuousRange)

final case class DiscreteDomain private (levels: Vector[String], ordered: Boolean):
  require(levels.distinct.length == levels.length, "`levels` must be distinct")

  def contains(value: String): Boolean =
    levels.contains(value)

  def train(values: IterableOnce[String]): Either[GraphicsError, DiscreteDomain] =
    val additions = values.iterator.filterNot(levels.contains).toVector.distinct
    if ordered then DiscreteDomain.ordered(levels ++ additions)
    else DiscreteDomain.unordered(levels ++ additions)

object DiscreteDomain:
  val empty: DiscreteDomain =
    DiscreteDomain(Vector.empty, ordered = true)

  def ordered(levels: Vector[String]): Either[GraphicsError, DiscreteDomain] =
    firstDuplicate(levels) match
      case Some(level) => Left(GraphicsError.DuplicateLevel(level))
      case None        => Right(DiscreteDomain(levels, ordered = true))

  def unordered(levels: Vector[String]): Either[GraphicsError, DiscreteDomain] =
    firstDuplicate(levels) match
      case Some(level) => Left(GraphicsError.DuplicateLevel(level))
      case None        => Right(DiscreteDomain(levels.sorted, ordered = false))

  private def firstDuplicate(levels: Vector[String]): Option[String] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    levels.find(level => !seen.add(level))

final case class DiscreteScale[A] private (
    name: GraphicsName,
    domain: DiscreteDomain,
    palette: DiscretePalette[A]
) extends Scale[String, A]:
  override def mapValue(value: String): Option[A] =
    val idx = domain.levels.indexOf(value)
    if idx < 0 then None else Some(palette(idx, domain.levels.length))

  def mapLevels(values: IterableOnce[String]): Vector[Option[A]] =
    values.iterator.map(mapValue).toVector

object DiscreteScale:
  def apply[A](
      name: String,
      domain: DiscreteDomain,
      palette: DiscretePalette[A]
  ): Either[GraphicsError, DiscreteScale[A]] =
    GraphicsName(name, "discrete scale").map(DiscreteScale(_, domain, palette))

trait Scale[-In, +Out]:
  def name: GraphicsName
  def mapValue(value: In): Option[Out]

final case class ScaleBinding[Row, In, Out](
    aesthetic: Aesthetic[Out],
    value: Row => In,
    scale: Scale[In, Out]
):
  def map(row: Row): Option[Out] =
    scale.mapValue(value(row))
