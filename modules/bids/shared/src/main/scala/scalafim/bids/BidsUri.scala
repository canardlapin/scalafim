package scalafim.bids

final case class DatasetDescription(
    name: Option[String],
    bidsVersion: Option[String],
    datasetType: String = "raw",
    parentDirectory: Option[BidsPath] = None,
    datasetLinks: Map[DatasetName, String] = Map.empty,
    raw: JsonValue.Obj = JsonValue.EmptyObject
):
  def resolve(uri: BidsUri): Either[BidsError, String] =
    uri.resolve(this)

object DatasetDescription:
  def fromJson(json: JsonValue.Obj, parentDirectory: Option[BidsPath] = None): DatasetDescription =
    val fields = json.fields
    val links =
      fields
        .get("DatasetLinks")
        .flatMap(_.asObject)
        .toVector
        .flatMap(_.toVector)
        .flatMap { case (name, value) => value.asString.map(DatasetName(name) -> _) }
        .toMap

    DatasetDescription(
      name = fields.get("Name").flatMap(_.asString),
      bidsVersion = fields.get("BIDSVersion").flatMap(_.asString),
      datasetType = fields.get("DatasetType").flatMap(_.asString).getOrElse("raw"),
      parentDirectory = parentDirectory,
      datasetLinks = links,
      raw = json
    )

final case class BidsUri(datasetName: DatasetName, relativePath: BidsPath):
  def value: String = s"bids:${datasetName.value}:${relativePath.value}"

  def resolve(description: DatasetDescription): Either[BidsError, String] =
    if datasetName.isCurrent then
      val base = description.parentDirectory.map(_.value).getOrElse("")
      Right(joinPath(base, relativePath.value))
    else
      description.datasetLinks.get(datasetName) match
        case None =>
          Left(BidsError.UnknownDatasetLink(datasetName.value, description.datasetLinks.keys.map(_.value).toVector.sorted))
        case Some(link) if link.startsWith("http://") || link.startsWith("https://") || link.startsWith("s3://") =>
          Right(joinPath(link.stripSuffix("/"), relativePath.value))
        case Some(link) if link.startsWith("file://") =>
          Right(joinPath(link.stripPrefix("file://").stripSuffix("/"), relativePath.value))
        case Some(link) =>
          Right(joinPath(link.stripSuffix("/"), relativePath.value))

  private def joinPath(base: String, rel: String): String =
    if base.isEmpty then rel else s"${base.stripSuffix("/")}/$rel"

object BidsUri:
  def parse(uri: String): Either[BidsError, BidsUri] =
    if !uri.startsWith("bids:") then
      Left(BidsError.InvalidBidsUri(uri, "must start with 'bids:'"))
    else
      val rest = uri.stripPrefix("bids:")
      val colon = rest.indexOf(':')
      if colon < 0 then
        Left(BidsError.InvalidBidsUri(uri, "must contain dataset name and relative path separated by ':'"))
      else
        val dataset = rest.substring(0, colon)
        val rel = rest.substring(colon + 1)
        if rel.isEmpty then Left(BidsError.InvalidBidsUri(uri, "relative path must be non-empty"))
        else
          BidsPath.relative(rel) match
            case Left(err) => Left(BidsError.InvalidBidsUri(uri, err.message))
            case Right(path) => Right(BidsUri(DatasetName(dataset), path))
