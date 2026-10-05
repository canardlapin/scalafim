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
  require(value.trim.nonEmpty, "region label must be non-empty")
  override def toString: String = value

object RegionLabel:
  def from(value: String): Either[AtlasError, RegionLabel] =
    if value.trim.nonEmpty then Right(RegionLabel(value))
    else Left(AtlasError.InvalidRegionMetadata("region label must be non-empty"))

  def unsafe(value: String): RegionLabel =
    from(value).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class RegionAttributeKey private (value: String):
  require(value.trim.nonEmpty, "region attribute keys must be non-empty")
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

final case class AtlasRegionMetadata(
  id: RegionId,
  label: RegionLabel,
  labelFull: Option[RegionLabel] = None,
  hemisphere: Option[Hemisphere] = None,
  network: Option[NetworkId] = None,
  color: Option[Rgb] = None,
  attributes: RegionAttributes = RegionAttributes.empty
):
  def fullLabel: RegionLabel = labelFull.getOrElse(label)

object AtlasRegionMetadata:
  def checked(
    id: RegionId,
    label: String,
    labelFull: Option[String] = None,
    hemisphere: Option[Hemisphere] = None,
    network: Option[NetworkId] = None,
    color: Option[Rgb] = None,
    attributes: Map[String, String] = Map.empty
  ): Either[AtlasError, AtlasRegionMetadata] =
    for
      short <- RegionLabel.from(label)
      full <- labelFull match
        case None => Right(None)
        case Some(value) => RegionLabel.from(value).map(Some(_))
      attrs <- RegionAttributes.from(attributes)
    yield AtlasRegionMetadata(id, short, full, hemisphere, network, color, attrs)

  def fromStrings(
    id: RegionId,
    label: String,
    labelFull: Option[String] = None,
    hemisphere: Option[Hemisphere] = None,
    network: Option[NetworkId] = None,
    color: Option[Rgb] = None,
    attributes: Map[String, String] = Map.empty
  ): AtlasRegionMetadata =
    checked(id, label, labelFull, hemisphere, network, color, attributes)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class RegionIndex(regions: Vector[AtlasRegionMetadata]):
  require(regions.nonEmpty, AtlasError.EmptyAtlas.message)
  private val duplicateIds =
    regions
      .groupBy(_.id)
      .collect { case (id, rs) if rs.length > 1 => id }
      .toVector
      .sorted
  require(duplicateIds.isEmpty, AtlasError.DuplicateRegionIds(duplicateIds).message)

  lazy val byId: Map[RegionId, AtlasRegionMetadata] =
    regions.map(r => r.id -> r).toMap

  lazy val byLabel: Map[String, Vector[AtlasRegionMetadata]] =
    regions.groupBy(r => RegionIndex.normalize(r.label.value))

  lazy val byFullLabel: Map[String, Vector[AtlasRegionMetadata]] =
    regions.groupBy(r => RegionIndex.normalize(r.fullLabel.value))

  def ids: Vector[RegionId] =
    regions.map(_.id)

  def size: Int =
    regions.length

  def labels: Vector[String] =
    regions.map(_.label.value)

  def get(id: RegionId): Option[AtlasRegionMetadata] =
    byId.get(id)

  def requireRegion(id: RegionId): AtlasRegionMetadata =
    get(id).getOrElse(throw new NoSuchElementException(AtlasError.MissingRegionId(id).message))

  def find(label: String, hemisphere: Option[Hemisphere] = None): Vector[AtlasRegionMetadata] =
    val key = RegionIndex.normalize(label)
    val matches = byLabel.getOrElse(key, Vector.empty) ++ byFullLabel.getOrElse(key, Vector.empty)
    val distinct = matches.distinct
    hemisphere match
      case None => distinct
      case Some(h) => distinct.filter(_.hemisphere.contains(h))

  def filter(p: AtlasRegionMetadata => Boolean): RegionIndex =
    RegionIndex(regions.filter(p))

  def labelMap: Map[Int, String] =
    regions.map(r => r.id.value -> r.label.value).toMap

object RegionIndex:
  private[atlas] def normalize(label: String): String =
    label.trim.toLowerCase.replaceAll("[^a-z0-9]+", "")
