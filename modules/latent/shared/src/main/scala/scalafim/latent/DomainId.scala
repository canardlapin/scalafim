package scalafim.latent

opaque type DomainId = String

object DomainId:
  def apply(value: String): Either[LatentError, DomainId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(LatentError.EmptyIdentifier("domain"))
    else Right(normalized)

  private[scalafim] def unsafe(value: String): DomainId =
    value

  extension (id: DomainId)
    inline def value: String = id
