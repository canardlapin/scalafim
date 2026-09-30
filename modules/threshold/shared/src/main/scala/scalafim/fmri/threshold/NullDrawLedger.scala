package scalafim.fmri.threshold

/** Fetches null draws for one procedure run and accounts for every one.
  *
  * A draw that fails, has the wrong mask-space length, or contains non-finite
  * values is reported as [[ThresholdError.NullDrawFailed]] with its index;
  * nothing is dropped or replaced, because silently shrinking the null family
  * would change the reference distribution. Each draw is fingerprinted on first
  * fetch, and a later fetch of the same index that differs is refused as
  * [[ThresholdError.NondeterministicNullDraw]]: hierarchical procedures revisit
  * draws and rely on one common null action per index. The fingerprint is
  * over raw bits, so a draw that alternates `-0.0` and `0.0` counts as changed.
  * Memory is one fingerprint per draw.
  */
private[threshold] final class NullDrawLedger(nullDraw: NullDraw, fieldSize: Int):
  private val draws = nullDraw.nPermutations.value
  private val fingerprints = new Array[Long](draws)
  private val seen = new Array[Boolean](draws)

  def size: Int =
    draws

  def reference: NullReference =
    nullDraw.reference

  def fetch(index: Int): Either[ThresholdError, Array[Double]] =
    if index < 0 || index >= draws then return Left(ThresholdError.IndexOutOfBounds(index, draws))
    nullDraw.draw(index) match
      case Left(cause) =>
        Left(ThresholdError.NullDrawFailed(index, cause))
      case Right(raw) =>
        if raw.length != fieldSize then
          return Left(
            ThresholdError.NullDrawFailed(
              index,
              ThresholdError.ShapeMismatch("null draw", fieldSize.toString, raw.length.toString)
            )
          )
        var hash = 0xcbf29ce484222325L
        var i = 0
        while i < raw.length do
          val value = raw(i)
          if !value.isFinite then return Left(ThresholdError.NullDrawFailed(index, ThresholdError.NonFiniteData("null draw")))
          hash = (hash ^ java.lang.Double.doubleToLongBits(value)) * 0x100000001b3L
          i += 1
        if !seen(index) then
          seen(index) = true
          fingerprints(index) = hash
          Right(raw)
        else if fingerprints(index) == hash then Right(raw)
        else Left(ThresholdError.NondeterministicNullDraw(index))
