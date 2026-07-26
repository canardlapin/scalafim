package scalafim.multivar
package core

private[multivar] object Identifier:
  def validate(kind: String, value: String): Either[MultivarError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(MultivarError.InvalidId(kind, value, "must be non-empty"))
    else if !trimmed.forall(isAllowed) then
      Left(MultivarError.InvalidId(kind, value, "may contain only letters, digits, '.', '_', and '-'"))
    else Right(trimmed)

  private def isAllowed(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '.' || ch == '_' || ch == '-'

opaque type SpaceId = String

object SpaceId:
  def apply(value: String): Either[MultivarError, SpaceId] =
    Identifier.validate("space id", value)

  def unsafe(value: String): SpaceId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SpaceId)
    inline def value: String = id

opaque type BlockId = String

object BlockId:
  def apply(value: String): Either[MultivarError, BlockId] =
    Identifier.validate("block id", value)

  def unsafe(value: String): BlockId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: BlockId)
    inline def value: String = id

opaque type Dimension = Int

object Dimension:
  def apply(value: Int): Either[MultivarError, Dimension] =
    from(value, "dimension")

  def from(value: Int, kind: String): Either[MultivarError, Dimension] =
    if value > 0 then Right(value)
    else Left(MultivarError.InvalidDimension(kind, value))

  def unsafe(value: Int): Dimension =
    require(value > 0, "dimension must be positive")
    value

  extension (dimension: Dimension)
    inline def value: Int = dimension

opaque type ComponentCount = Int

object ComponentCount:
  def apply(value: Int): Either[MultivarError, ComponentCount] =
    if value > 0 then Right(value)
    else Left(MultivarError.InvalidDimension("component count", value))

  def unsafe(value: Int): ComponentCount =
    require(value > 0, "component count must be positive")
    value

  extension (count: ComponentCount)
    inline def value: Int = count

