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
