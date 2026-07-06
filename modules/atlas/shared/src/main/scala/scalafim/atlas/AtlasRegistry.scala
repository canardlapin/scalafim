package scalafim.atlas

import scala.collection.immutable.VectorMap

final case class AtlasSpec(
  id: String,
  label: String,
  family: String,
  defaultSpace: SpaceId,
  representation: AtlasRepresentation,
  aliases: Vector[String] = Vector.empty,
  description: Option[String] = None
):
  require(id.trim.nonEmpty, "atlas spec id must be non-empty")
  require(label.trim.nonEmpty, "atlas spec label must be non-empty")
  require(family.trim.nonEmpty, "atlas spec family must be non-empty")

  def matches(name: String): Boolean =
    val needle = AtlasRegistry.normalize(name)
    (id +: aliases).exists(a => AtlasRegistry.normalize(a) == needle)

final case class AtlasRegistry(entries: VectorMap[String, AtlasSpec]):
  def register(spec: AtlasSpec): AtlasRegistry =
    copy(entries = entries.updated(spec.id, spec))

  def list: Vector[AtlasSpec] =
    entries.values.toVector

  def ids: Vector[String] =
    entries.keys.toVector

  def find(name: String): Either[AtlasError, AtlasSpec] =
    entries.valuesIterator.find(_.matches(name)) match
      case Some(spec) => Right(spec)
      case None => Left(AtlasError.UnknownAtlas(name, ids))

  def apply(name: String): AtlasSpec =
    find(name) match
      case Right(spec) => spec
      case Left(err) => throw new NoSuchElementException(err.message)

object AtlasRegistry:
  def empty: AtlasRegistry =
    AtlasRegistry(VectorMap.empty)

  def normalize(s: String): String =
    s.trim.toLowerCase.replaceAll("[^a-z0-9]+", "")

  val default: AtlasRegistry =
    StandardAtlases.registerBuiltins(empty)
