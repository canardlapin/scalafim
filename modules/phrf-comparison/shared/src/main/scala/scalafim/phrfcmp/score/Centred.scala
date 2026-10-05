package scalafim.phrfcmp.score

/** A sample reduced to deviations from its own mean; the mean is discarded at construction.
  *
  * This is the only carrier of comparative endpoints (`lambda_d`, `delta z_d`). It exposes the size, the degrees of
  * freedom and the standard deviation. There is no accessor for the mean, the median, the signed deviations or any
  * single value, so a location-bearing quantity cannot be read back out (design section 3.2, item 2).
  */
final class Centred private (private val deviations: Array[Double]):
  def n: Int = deviations.length
  def df: Int = deviations.length - 1

  /** `sqrt(sum (e_d - e_bar)^2 / (D - 1))`. */
  def sd: Double =
    var s = 0.0
    var i = 0
    while i < deviations.length do
      s += deviations(i) * deviations(i)
      i += 1
    math.sqrt(s / (deviations.length - 1).toDouble)

  override def toString: String = "Centred(<redacted>)"

object Centred:
  /** Needs at least two finite values. The mean is removed with a corrected two-pass step so that adding a
    * constant to every value leaves the deviations unchanged to rounding.
    */
  def of(values: IndexedSeq[Double]): Either[ScoreError, Centred] =
    if values.length < 2 then Left(ScoreError.TooFewValues(ScoreError.Site.Centring, values.length, 2))
    else if !values.forall(v => !v.isNaN && !v.isInfinite) then Left(ScoreError.NonFiniteValue(ScoreError.Site.Centring))
    else
      val n = values.length
      var s = 0.0
      var i = 0
      while i < n do
        s += values(i)
        i += 1
      val m = s / n
      val dev = new Array[Double](n)
      var r = 0.0
      i = 0
      while i < n do
        dev(i) = values(i) - m
        r += dev(i)
        i += 1
      val corr = r / n
      i = 0
      while i < n do
        dev(i) -= corr
        i += 1
      Right(new Centred(dev))
