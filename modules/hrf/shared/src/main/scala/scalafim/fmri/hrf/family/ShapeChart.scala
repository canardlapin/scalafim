package scalafim.fmri.hrf.family

enum ShapeChartError:
  case EmptyChart
  case TooManyDimensions(dimension: Int, maximum: Int)
  case DuplicateName(name: String)
  case InvalidBound(name: String, lower: Double, upper: Double)
  case DimensionMismatch(expected: Int, actual: Int)
  case NonFinite(name: String, value: Double)
  case OutOfChart(name: String, value: Double, lower: Double, upper: Double)

  def message: String =
    this match
      case EmptyChart => "a shape chart needs at least one coordinate"
      case TooManyDimensions(dimension, maximum) => s"a shape chart supports at most $maximum coordinates, got $dimension"
      case DuplicateName(name) => s"coordinate name '$name' is repeated"
      case InvalidBound(name, lower, upper) => s"coordinate '$name' needs finite bounds with lower < upper, got [$lower, $upper]"
      case DimensionMismatch(expected, actual) => s"expected $expected coordinates, got $actual"
      case NonFinite(name, value) => s"coordinate '$name' must be finite, got $value"
      case OutOfChart(name, value, lower, upper) => s"coordinate '$name' = $value lies outside [$lower, $upper]"

/** A bounded box of fitted shape coordinates, `d <= 3`.
  *
  * The chart is the coordinate system a decoder moves in (for example
  * `(tau, log sigma)`), distinct from a family's scientific parameters. Bounds
  * are part of the model: a point outside them is not a valid shape.
  */
final case class ShapeChart private (names: Vector[String], lower: Vector[Double], upper: Vector[Double]):
  def dimension: Int = names.length

  def point(coordinates: Vector[Double]): Either[ShapeChartError, ShapePoint] =
    if coordinates.length != dimension then Left(ShapeChartError.DimensionMismatch(dimension, coordinates.length))
    else
      var i = 0
      while i < dimension do
        val v = coordinates(i)
        if !v.isFinite then return Left(ShapeChartError.NonFinite(names(i), v))
        if v < lower(i) || v > upper(i) then return Left(ShapeChartError.OutOfChart(names(i), v, lower(i), upper(i)))
        i += 1
      Right(ShapePoint(coordinates))

  def point(coordinates: Double*): Either[ShapeChartError, ShapePoint] =
    point(coordinates.toVector)

  /** Clamp one coordinate into its bounds. */
  def clamp(index: Int, value: Double): Double =
    math.min(upper(index), math.max(lower(index), value))

  def onBoundary(point: ShapePoint, tolerance: Double = 1e-9): Boolean =
    var i = 0
    while i < dimension do
      val v = point.coordinates(i)
      if v <= lower(i) + tolerance || v >= upper(i) - tolerance then return true
      i += 1
    false

  def width(index: Int): Double = upper(index) - lower(index)

object ShapeChart:
  val MaxDimension: Int = 3

  def make(axes: (String, Double, Double)*): Either[ShapeChartError, ShapeChart] =
    if axes.isEmpty then Left(ShapeChartError.EmptyChart)
    else if axes.length > MaxDimension then Left(ShapeChartError.TooManyDimensions(axes.length, MaxDimension))
    else
      val names = axes.map(_._1).toVector
      names.groupBy(identity).collectFirst { case (n, group) if group.length > 1 => n } match
        case Some(name) => Left(ShapeChartError.DuplicateName(name))
        case None =>
          axes.collectFirst { case (n, lo, hi) if !(lo.isFinite && hi.isFinite && lo < hi) => (n, lo, hi) } match
            case Some((n, lo, hi)) => Left(ShapeChartError.InvalidBound(n, lo, hi))
            case None => Right(new ShapeChart(names, axes.map(_._2).toVector, axes.map(_._3).toVector))

  def apply(axes: (String, Double, Double)*): ShapeChart =
    make(axes*).fold(err => throw new IllegalArgumentException(err.message), identity)

/** A validated point in a [[ShapeChart]]; construct through `chart.point`. */
final case class ShapePoint private[family] (coordinates: Vector[Double]):
  def dimension: Int = coordinates.length
  def apply(index: Int): Double = coordinates(index)

object ShapePoint:
  /** Bypass chart validation; for finite-difference probes and hot paths that already validated. */
  def unsafe(coordinates: Vector[Double]): ShapePoint = new ShapePoint(coordinates)

/** Component layout of a second-order jet: value, `d` first derivatives, then
  * the upper triangle of second derivatives in row order.
  */
object JetLayout:
  def components(dimension: Int): Int = 1 + dimension + dimension * (dimension + 1) / 2
  val Value: Int = 0
  def first(p: Int): Int = 1 + p
  def second(dimension: Int, p: Int, q: Int): Int =
    val (a, b) = if p <= q then (p, q) else (q, p)
    1 + dimension + a * dimension - a * (a - 1) / 2 + (b - a)
