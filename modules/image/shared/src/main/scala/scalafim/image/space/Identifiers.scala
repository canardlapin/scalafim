package scalafim.image.space

private object SpaceIdentifier:
  def normalize(label: String, value: String): Either[SpaceError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(SpaceError.EmptyIdentifier(label))
    else Right(trimmed)

/** A participant label, scoped by a dataset namespace wherever it defines identity. */
opaque type SubjectId = String

object SubjectId:
  def apply(value: String): Either[SpaceError, SubjectId] =
    SpaceIdentifier.normalize("subject", value)

  private[scalafim] def unsafe(value: String): SubjectId =
    value

  extension (id: SubjectId)
    inline def value: String = id

opaque type SessionId = String

object SessionId:
  def apply(value: String): Either[SpaceError, SessionId] =
    SpaceIdentifier.normalize("session", value)

  private[scalafim] def unsafe(value: String): SessionId =
    value

  extension (id: SessionId)
    inline def value: String = id

/** A standard template name, e.g. `MNI152NLin2009cAsym`, `fsaverage`, `fsLR`. Globally unique. */
opaque type TemplateName = String

object TemplateName:
  def apply(value: String): Either[SpaceError, TemplateName] =
    SpaceIdentifier.normalize("template", value)

  private[scalafim] def unsafe(value: String): TemplateName =
    value

  extension (id: TemplateName)
    inline def value: String = id
