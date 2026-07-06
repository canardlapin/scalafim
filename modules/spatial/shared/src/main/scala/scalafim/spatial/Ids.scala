package scalafim.spatial

private object SpatialId:
  def normalize(label: String, value: String): Either[SpatialError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(SpatialError.EmptyIdentifier(label))
    else Right(trimmed)

opaque type DomainId = String

object DomainId:
  def apply(value: String): Either[SpatialError, DomainId] =
    SpatialId.normalize("domain", value)

  private[scalafim] def unsafe(value: String): DomainId =
    value

  extension (id: DomainId)
    inline def value: String = id

opaque type MorphismId = String

object MorphismId:
  def apply(value: String): Either[SpatialError, MorphismId] =
    SpatialId.normalize("morphism", value)

  private[scalafim] def unsafe(value: String): MorphismId =
    value

  extension (id: MorphismId)
    inline def value: String = id

opaque type OperatorId = String

object OperatorId:
  def apply(value: String): Either[SpatialError, OperatorId] =
    SpatialId.normalize("operator", value)

  private[scalafim] def unsafe(value: String): OperatorId =
    value

  extension (id: OperatorId)
    inline def value: String = id

opaque type SubjectId = String

object SubjectId:
  def apply(value: String): Either[SpatialError, SubjectId] =
    SpatialId.normalize("subject", value)

  private[scalafim] def unsafe(value: String): SubjectId =
    value

  extension (id: SubjectId)
    inline def value: String = id

opaque type SessionId = String

object SessionId:
  def apply(value: String): Either[SpatialError, SessionId] =
    SpatialId.normalize("session", value)

  private[scalafim] def unsafe(value: String): SessionId =
    value

  extension (id: SessionId)
    inline def value: String = id

opaque type Modality = String

object Modality:
  def apply(value: String): Either[SpatialError, Modality] =
    SpatialId.normalize("modality", value)

  private[scalafim] def unsafe(value: String): Modality =
    value

  extension (id: Modality)
    inline def value: String = id

opaque type TemplateName = String

object TemplateName:
  def apply(value: String): Either[SpatialError, TemplateName] =
    SpatialId.normalize("template", value)

  private[scalafim] def unsafe(value: String): TemplateName =
    value

  extension (id: TemplateName)
    inline def value: String = id

opaque type Resolution = String

object Resolution:
  def apply(value: String): Either[SpatialError, Resolution] =
    SpatialId.normalize("resolution", value)

  private[scalafim] def unsafe(value: String): Resolution =
    value

  extension (id: Resolution)
    inline def value: String = id

opaque type BasisName = String

object BasisName:
  def apply(value: String): Either[SpatialError, BasisName] =
    SpatialId.normalize("basis", value)

  private[scalafim] def unsafe(value: String): BasisName =
    value

  extension (id: BasisName)
    inline def value: String = id

opaque type PartName = String

object PartName:
  def apply(value: String): Either[SpatialError, PartName] =
    SpatialId.normalize("part", value)

  private[scalafim] def unsafe(value: String): PartName =
    value

  extension (id: PartName)
    inline def value: String = id
