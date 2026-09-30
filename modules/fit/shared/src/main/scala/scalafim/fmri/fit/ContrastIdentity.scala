package scalafim.fmri.fit

opaque type ContrastId = String

object ContrastId:
  def apply(value: String): Either[FitError, ContrastId] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(FitError.InvalidFitAxis("contrast id", "must be non-empty"))

  def unsafe(value: String): ContrastId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ContrastId)
    inline def value: String = id
