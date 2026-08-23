package scalafim.fmri.threshold

import scalafim.image.NeuroVol

final class PriorWeights private (private[threshold] val data: Array[Double]):
  def length: Int = data.length

  def apply(index: Int): Double = data(index)

  def totalMass: Double =
    var s = 0.0
    var i = 0
    while i < data.length do
      s += data(i)
      i += 1
    s

  def copyData: Array[Double] =
    data.clone

  def shrinkToUniform(eta: Double): Either[ThresholdError, PriorWeights] =
    if !eta.isFinite then return Left(ThresholdError.InvalidArgument("eta", "must be finite"))
    val e = math.max(0.0, math.min(1.0, eta))
    val n = data.length
    val uniform = 1.0 / n.toDouble
    val out = new Array[Double](n)
    var sum = 0.0
    var i = 0
    while i < n do
      val v = (1.0 - e) * uniform + e * data(i)
      out(i) = v
      sum += v
      i += 1
    i = 0
    while i < n do
      out(i) = out(i) / sum
      i += 1
    Right(new PriorWeights(out))

object PriorWeights:

  def uniform(length: Int): Either[ThresholdError, PriorWeights] =
    if length <= 0 then Left(ThresholdError.EmptyMask)
    else
      val w = 1.0 / length.toDouble
      Right(new PriorWeights(Array.fill(length)(w)))

  def fromArray(weights: Array[Double]): Either[ThresholdError, PriorWeights] =
    if weights.isEmpty then return Left(ThresholdError.EmptyMask)
    val out = new Array[Double](weights.length)
    var sum = 0.0
    var i = 0
    while i < weights.length do
      val w = weights(i)
      if !w.isFinite then return Left(ThresholdError.NonFiniteData("prior weights"))
      if w < 0.0 then return Left(ThresholdError.NegativePrior(i, w))
      out(i) = w
      sum += w
      i += 1
    if sum <= 0.0 then Left(ThresholdError.ZeroPriorMass)
    else
      i = 0
      while i < out.length do
        out(i) = out(i) / sum
        i += 1
      Right(new PriorWeights(out))

  def fromVolume(prior: NeuroVol[Double], field: MaskedField): Either[ThresholdError, PriorWeights] =
    if prior.space.spatialDims != field.space.spatialDims then
      return Left(
        ThresholdError.ShapeMismatch(
          "prior/field",
          field.space.spatialDims.mkString("x"),
          prior.space.spatialDims.mkString("x")
        )
      )
    val out = new Array[Double](field.size)
    var i = 0
    while i < field.size do
      out(i) = prior.valueAtCanonicalOrdinal(field.originalIndex(i))
      i += 1
    fromArray(out)
