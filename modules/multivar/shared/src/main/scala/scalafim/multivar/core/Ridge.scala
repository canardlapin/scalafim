package scalafim.multivar
package core

/** Non-negative ridge coefficient shared by paired, projection, and
  * multiblock models.
  */
opaque type Ridge = Double

object Ridge:
  def apply(value: Double): Either[MultivarError, Ridge] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("ridge", value))

  private[multivar] def unsafe(value: Double): Ridge =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (ridge: Ridge)
    inline def value: Double = ridge
