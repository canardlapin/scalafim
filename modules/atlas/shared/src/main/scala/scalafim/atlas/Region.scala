package scalafim.atlas

final case class RegionId(value: Int):
  require(value > 0, "region id must be positive")

  override def toString: String = value.toString

object RegionId:
  given Ordering[RegionId] = Ordering.by(_.value)

enum Hemisphere:
  case Left, Right, Bilateral, Midline

final case class NetworkId(value: String):
  require(value.trim.nonEmpty, "network id must be non-empty")

  override def toString: String = value

final case class Rgb(red: Int, green: Int, blue: Int):
  require(red >= 0 && red <= 255, "red must be in [0,255]")
  require(green >= 0 && green <= 255, "green must be in [0,255]")
  require(blue >= 0 && blue <= 255, "blue must be in [0,255]")

final case class RegionLabel private (value: String):
  override def toString: String = value

object RegionLabel:
  def from(value: String): Either[AtlasError, RegionLabel] =
    if value.trim.nonEmpty then Right(RegionLabel(value))
    else Left(AtlasError.InvalidRegionMetadata("region label must be non-empty"))

  def unsafe(value: String): RegionLabel =
    from(value).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class RegionAttributeKey private (value: String):
  override def toString: String = value

object RegionAttributeKey:
  def from(value: String): Either[AtlasError, RegionAttributeKey] =
    if value.trim.nonEmpty then Right(RegionAttributeKey(value))
    else Left(AtlasError.InvalidRegionMetadata("region attribute keys must be non-empty"))

  def unsafe(value: String): RegionAttributeKey =
    from(value).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class RegionAttributeValue(value: String):
  override def toString: String = value

final case class RegionAttributes private (values: Map[RegionAttributeKey, RegionAttributeValue]):
  def toMap: Map[String, String] =
    values.map { case (key, value) => key.value -> value.value }

  def get(key: RegionAttributeKey): Option[RegionAttributeValue] =
    values.get(key)

object RegionAttributes:
  val empty: RegionAttributes =
    RegionAttributes(Map.empty)

  def from(values: Map[String, String]): Either[AtlasError, RegionAttributes] =
    values.keys.find(_.trim.isEmpty) match
      case Some(_) => Left(AtlasError.InvalidRegionMetadata("region attribute keys must be non-empty"))
      case None =>
        Right(
          RegionAttributes(
            values.map { case (key, value) =>
              RegionAttributeKey.unsafe(key) -> RegionAttributeValue(value)
            }
          )
        )

  def unsafe(values: Map[String, String]): RegionAttributes =
    from(values).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class Region(
  id: RegionId,
  label: String,
  labelFull: Option[String] = None,
  hemisphere: Option[Hemisphere] = None,
  network: Option[NetworkId] = None,
  color: Option[Rgb] = None,
  attributes: Map[String, String] = Map.empty
):
  require(label.trim.nonEmpty, "region label must be non-empty")
  attributes.keys.foreach(k => require(k.trim.nonEmpty, "region attribute keys must be non-empty"))

  def fullLabel: String = labelFull.getOrElse(label)

  def typedLabel: RegionLabel =
    RegionLabel.unsafe(label)

  def typedFullLabel: RegionLabel =
    RegionLabel.unsafe(fullLabel)

  def typedAttributes: RegionAttributes =
    RegionAttributes.unsafe(attributes)

object Region:
  def checked(
    id: RegionId,
    label: String,
    labelFull: Option[String] = None,
    hemisphere: Option[Hemisphere] = None,
    network: Option[NetworkId] = None,
    color: Option[Rgb] = None,
    attributes: Map[String, String] = Map.empty
  ): Either[AtlasError, Region] =
    for
      _ <- RegionLabel.from(label)
      _ <- labelFull.map(RegionLabel.from).getOrElse(Right(RegionLabel.unsafe(label)))
      _ <- RegionAttributes.from(attributes)
    yield Region(id, label, labelFull, hemisphere, network, color, attributes)

final case class RegionIndex(regions: Vector[Region]):
  require(regions.nonEmpty, AtlasError.EmptyAtlas.message)
  private val duplicateIds =
    regions
      .groupBy(_.id)
      .collect { case (id, rs) if rs.length > 1 => id }
      .toVector
      .sorted
  require(duplicateIds.isEmpty, AtlasError.DuplicateRegionIds(duplicateIds).message)

  lazy val byId: Map[RegionId, Region] =
    regions.map(r => r.id -> r).toMap

  lazy val byLabel: Map[String, Vector[Region]] =
    regions.groupBy(r => RegionIndex.normalize(r.label))

  lazy val byFullLabel: Map[String, Vector[Region]] =
    regions.groupBy(r => RegionIndex.normalize(r.fullLabel))

  def ids: Vector[RegionId] =
    regions.map(_.id)

  def size: Int =
    regions.length

  def labels: Vector[String] =
    regions.map(_.label)

  def get(id: RegionId): Option[Region] =
    byId.get(id)

  def requireRegion(id: RegionId): Region =
    get(id).getOrElse(throw new NoSuchElementException(AtlasError.MissingRegionId(id).message))

  def find(label: String, hemisphere: Option[Hemisphere] = None): Vector[Region] =
    val key = RegionIndex.normalize(label)
    val matches = byLabel.getOrElse(key, Vector.empty) ++ byFullLabel.getOrElse(key, Vector.empty)
    val distinct = matches.distinct
    hemisphere match
      case None => distinct
      case Some(h) => distinct.filter(_.hemisphere.contains(h))

  def filter(p: Region => Boolean): RegionIndex =
    RegionIndex(regions.filter(p))

  def labelMap: Map[Int, String] =
    regions.map(r => r.id.value -> r.label).toMap

object RegionIndex:
  private[atlas] def normalize(label: String): String =
    label.trim.toLowerCase.replaceAll("[^a-z0-9]+", "")
