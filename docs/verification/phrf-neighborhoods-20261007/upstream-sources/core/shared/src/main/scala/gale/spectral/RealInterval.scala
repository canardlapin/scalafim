package gale.spectral

import gale.linalg.{LinAlgError, OutwardInterval, OutwardIntervalError}

/** Finite outward-rounded real interval. Construction is checked. */
final class RealInterval private (val lower: Double, val upper: Double):
  // Constructor checks also apply to JVM callers bypassing Scala privacy.
  private val interval = OutwardInterval(lower, upper).fold(
    error => throw new IllegalArgumentException(error.toString),
    identity
  )
  def add(that: RealInterval): Either[LinAlgError, RealInterval] =
    RealInterval.from(interval + that.interval)
  def subtract(that: RealInterval): Either[LinAlgError, RealInterval] =
    RealInterval.from(interval - that.interval)
  def multiply(that: RealInterval): Either[LinAlgError, RealInterval] =
    RealInterval.from(interval * that.interval)
  def divide(that: RealInterval): Either[LinAlgError, RealInterval] =
    RealInterval.from(interval / that.interval)
  def square: Either[LinAlgError, RealInterval] =
    RealInterval.from(interval.square).flatMap(x => RealInterval.checked(math.max(0.0, x.lower), x.upper))
  def sqrt: Either[LinAlgError, RealInterval] =
    if lower < 0.0 then Left(LinAlgError.InvalidArgument("interval square root requires nonnegative lower bound"))
    else for lo <- RealInterval.sqrtDown(lower); hi <- RealInterval.sqrtUp(upper); out <- RealInterval.checked(lo, hi) yield out
  /** Monotone exponential enclosure using a positive Taylor series with a
    * geometric remainder, followed by interval squaring. No platform exp/log
    * accuracy assumption enters this bound. Nonfinite results are refused.
    */
  def exp: Either[LinAlgError, RealInterval] =
    for lo <- RealInterval.expPoint(lower); hi <- RealInterval.expPoint(upper); out <- RealInterval.checked(lo.lower, hi.upper) yield out
object RealInterval:
  private def from(value: Either[OutwardIntervalError, OutwardInterval]): Either[LinAlgError, RealInterval] =
    value.left.map(error => LinAlgError.InvalidArgument(error.toString)).flatMap(x => checked(x.lower, x.upper))
  def exact(x: Double): Either[LinAlgError, RealInterval] = checked(x, x)
  def checked(lower: Double, upper: Double): Either[LinAlgError, RealInterval] =
    if lower.isFinite && upper.isFinite && lower <= upper then Right(new RealInterval(lower, upper)) else fail
  private[spectral] def fail = Left(LinAlgError.InvalidArgument("interval endpoint is nonfinite or reversed"))
  private[spectral] def down(x: Double): Double = java.lang.Math.nextDown(x)
  private[spectral] def up(x: Double): Double = java.lang.Math.nextUp(x)
  private def expPoint(x: Double): Either[LinAlgError, RealInterval] =
    if x == 0.0 then exact(1.0)
    else
      var reductions = 0
      var reduced = math.abs(x)
      while reduced > 0.5 do
        reduced *= 0.5
        reductions += 1
      // Halving a normal number by a power of two is exact. The loop stops
      // before entering the subnormal range; an initially tiny x is unchanged.
      val one = exact(1.0).toOption.get
      val t = exact(reduced).toOption.get
      var term: Either[LinAlgError, RealInterval] = Right(one)
      var total: Either[LinAlgError, RealInterval] = Right(one)
      var n = 1
      while n <= 24 do
        term = for a <- term; b <- a.multiply(t); d <- exact(n.toDouble); c <- b.divide(d) yield c
        total = for a <- total; b <- term; c <- a.add(b) yield c
        n += 1
      // For terms after t^24/24!, successive ratios are <= t/26.
      val result = for
        a <- term
        next <- a.multiply(t).flatMap(_.divide(exact(25.0).toOption.get))
        ratio <- t.divide(exact(26.0).toOption.get)
        denominator <- one.subtract(ratio)
        tail <- next.divide(denominator)
        sum <- total
        extra <- checked(0.0, tail.upper)
        enclosed <- sum.add(extra)
        signed <- if x < 0.0 then one.divide(enclosed) else Right(enclosed)
      yield signed
      var scaled = result
      var i = 0
      while i < reductions && scaled.isRight do
        scaled = scaled.flatMap(_.square)
        i += 1
      scaled
  private def sqrtDown(x: Double): Either[LinAlgError, Double] =
    if x == 0.0 then Right(0.0) else
      def seek(y: Double, n: Int): Either[LinAlgError, Double] =
        if !y.isFinite || y < 0.0 then fail
        else if up(y * y) <= x then checked(down(y), down(y)).map(_.lower)
        else if n == 8 then fail else seek(down(y), n + 1)
      seek(math.sqrt(x), 0)
  private def sqrtUp(x: Double): Either[LinAlgError, Double] =
    if x == 0.0 then Right(0.0) else
      def seek(y: Double, n: Int): Either[LinAlgError, Double] =
        if !y.isFinite || y < 0.0 then fail
        else if down(y * y) >= x then checked(up(y), up(y)).map(_.upper)
        else if n == 8 then fail else seek(up(y), n + 1)
      seek(math.sqrt(x), 0)
