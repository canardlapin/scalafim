package scalafim.fmri.ar

final case class ArOrderValue private (value: Int):
  require(value >= 0, "AR order must be non-negative")

  def toInt: Int = value

object ArOrderValue:
  val Zero: ArOrderValue = unsafe(0)

  def apply(value: Int): Either[ArError, ArOrderValue] =
    if value < 0 then Left(ArError.InvalidArOrder(value))
    else Right(new ArOrderValue(value))

  def unsafe(value: Int): ArOrderValue =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ArLag private (value: Int):
  require(value >= 0, "AR lag must be non-negative")

  def toInt: Int = value

object ArLag:
  val Zero: ArLag = unsafe(0)

  def apply(value: Int): Either[ArError, ArLag] =
    if value < 0 then Left(ArError.InvalidArLag(value))
    else Right(new ArLag(value))

  def unsafe(value: Int): ArLag =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class StationarityBound private (value: Double):
  require(value > 0.0 && value < 1.0 && value.isFinite, "stationarity bound must be finite and in (0, 1)")

object StationarityBound:
  val Default: StationarityBound = unsafe(0.99)

  def apply(value: Double): Either[ArError, StationarityBound] =
    if value > 0.0 && value < 1.0 && value.isFinite then Right(new StationarityBound(value))
    else Left(ArError.InvalidStationarityBound(value))

  def unsafe(value: Double): StationarityBound =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class PartialAutocorrelations private (values: Vector[Double]):
  require(values.forall(_.isFinite), "partial autocorrelations must be finite")

  def length: Int = values.length
  def isEmpty: Boolean = values.isEmpty
  def toVector: Vector[Double] = values

  def clipped(bound: StationarityBound): PartialAutocorrelations =
    PartialAutocorrelations.unsafe(values.map(k => math.max(-bound.value, math.min(bound.value, k))))

object PartialAutocorrelations:
  val Empty: PartialAutocorrelations = unsafe(Vector.empty)

  def apply(values: Vector[Double]): Either[ArError, PartialAutocorrelations] =
    firstNonFinite(values) match
      case Some((index, value)) => Left(ArError.NonFinitePartialAutocorrelation(index, value))
      case None                 => Right(new PartialAutocorrelations(values))

  def unsafe(values: Vector[Double]): PartialAutocorrelations =
    apply(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def firstNonFinite(values: Vector[Double]): Option[(Int, Double)] =
    var index = 0
    while index < values.length do
      val value = values(index)
      if !value.isFinite then return Some(index -> value)
      index += 1
    None

final case class Autocovariances private (values: Vector[Double]):
  require(values.nonEmpty, "autocovariances must include lag zero")
  require(values.forall(_.isFinite), "autocovariances must be finite")

  def length: Int = values.length
  def lagZero: Double = values.head
  def maxLag: ArLag = ArLag.unsafe(values.length - 1)
  def toVector: Vector[Double] = values

  def at(lag: ArLag): Double =
    values(lag.value)

  def takeThrough(order: ArOrderValue): Either[ArError, Autocovariances] =
    val required = order.value + 1
    if values.length < required then Left(ArError.InsufficientAutocovariances(required, values.length))
    else Right(Autocovariances.unsafe(values.take(required)))

object Autocovariances:
  def apply(values: Vector[Double]): Either[ArError, Autocovariances] =
    if values.isEmpty then Left(ArError.EmptyAutocovariances)
    else
      firstNonFinite(values) match
        case Some((lag, value)) => Left(ArError.NonFiniteAutocovariance(ArLag.unsafe(lag), value))
        case None               => Right(new Autocovariances(values))

  def unsafe(values: Vector[Double]): Autocovariances =
    apply(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def firstNonFinite(values: Vector[Double]): Option[(Int, Double)] =
    var lag = 0
    while lag < values.length do
      val value = values(lag)
      if !value.isFinite then return Some(lag -> value)
      lag += 1
    None
