package scalafim.connectivity

sealed trait NodeAxisProvenance:
  def description: String

  def compatibleWith(other: NodeAxisProvenance): Boolean

object NodeAxisProvenance:
  case object Unspecified extends NodeAxisProvenance:
    def description: String =
      "unspecified"

    def compatibleWith(other: NodeAxisProvenance): Boolean =
      other == Unspecified

  final class Declared private[connectivity] (
      val basisId: String,
      val version: Option[String]
  ) extends NodeAxisProvenance:
    def description: String =
      version.fold(basisId)(value => s"$basisId@$value")

    def compatibleWith(other: NodeAxisProvenance): Boolean =
      this == other

    override def equals(other: Any): Boolean =
      other match
        case that: Declared => basisId == that.basisId && version == that.version
        case _              => false

    override def hashCode(): Int =
      31 * basisId.hashCode + version.hashCode

    override def toString: String =
      s"NodeAxisProvenance.Declared(${description})"

  def declared(
      basisId: String,
      version: Option[String] = None
  ): Either[ConnectivityError, NodeAxisProvenance] =
    for
      validId <- ConnectivityIdentifier.validate("node-axis basis", basisId)
      validVersion <- version match
        case None        => Right(None)
        case Some(value) => ConnectivityIdentifier.validate("node-axis version", value).map(Some(_))
    yield new Declared(validId, validVersion)

  def unsafeDeclared(basisId: String, version: Option[String] = None): NodeAxisProvenance =
    declared(basisId, version).fold(error => throw new IllegalArgumentException(error.message), identity)
