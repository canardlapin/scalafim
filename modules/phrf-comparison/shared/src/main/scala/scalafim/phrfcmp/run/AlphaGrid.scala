package scalafim.phrfcmp.run

import scalafim.fmri.model.PositiveAlpha

/**
  * The LOROCV tuning grid (protocol section 7, design 2.2): nine strictly increasing positive alphas, `2^-6 .. 2^2`
  * for the pilot, in PHRF's own units (`alpha = 1 / lambda`, the trial-deviation variance ratio). The same grid, folds,
  * score and tie rule serve PHRF, PHRF-can and rLSS; rLSS reaches it through the df mapping ([[DfMapping]]).
  *
  * Powers of two are exact in binary floating point, so `lambda = 1 / alpha` is exact for the pilot grid.
  */
final class AlphaGrid private (private val values: Vector[PositiveAlpha]):
  def size: Int = values.length

  /** Alpha of grid point `k` (ascending in `k`). */
  def alpha(k: Int): Double = PositiveAlpha.value(values(k))

  /** The typed alpha, as `AmplitudeStructure.ConditionCenteredTrials` takes it. */
  def typed(k: Int): PositiveAlpha = values(k)

  /** PHRF's penalty `lambda = 1 / alpha` at grid point `k`. */
  def lambda(k: Int): Double = values(k).lambda

  def alphas: Vector[Double] = values.map(a => PositiveAlpha.value(a))

  def lambdas: Vector[Double] = values.map(_.lambda)

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case g: AlphaGrid => g.alphas == alphas
    case _            => false

  override def hashCode: Int = alphas.hashCode

  override def toString: String = s"AlphaGrid(${alphas.mkString(", ")})"

object AlphaGrid:

  /** `2^-6 .. 2^2`, nine points. */
  val Pilot: AlphaGrid = powersOfTwo(-6 to 2)

  private def powersOfTwo(exponents: Range): AlphaGrid =
    from(exponents.toVector.map(e => math.pow(2.0, e.toDouble))).fold(m => throw new IllegalStateException(m), identity)

  /** Strictly increasing, finite, positive alphas; at least one. */
  def from(alphas: Vector[Double]): Either[String, AlphaGrid] =
    if alphas.isEmpty then Left("alpha grid must not be empty")
    else
      val typed = alphas.map(a => PositiveAlpha(a).left.map(_.message))
      typed.collectFirst { case Left(m) => m } match
        case Some(m) => Left(m)
        case None =>
          if alphas.zip(alphas.tail).exists((a, b) => !(a < b)) then Left("alpha grid must be strictly increasing")
          else Right(new AlphaGrid(typed.collect { case Right(a) => a }))
