package scalafim.atlas

final case class SpaceId(value: String):
  require(value.trim.nonEmpty, "space id must be non-empty")

  override def toString: String = value

object SpaceId:
  val MNI152: SpaceId = SpaceId("MNI152")
  val MNI305: SpaceId = SpaceId("MNI305")
  val MNI152NLin6Asym: SpaceId = SpaceId("MNI152NLin6Asym")
  val MNI152NLin2009cAsym: SpaceId = SpaceId("MNI152NLin2009cAsym")
  val FsAverage: SpaceId = SpaceId("fsaverage")
  val FsAverage5: SpaceId = SpaceId("fsaverage5")
  val FsAverage6: SpaceId = SpaceId("fsaverage6")
  val FsLR32k: SpaceId = SpaceId("fsLR_32k")
  val Custom: SpaceId = SpaceId("custom")
  val Unknown: SpaceId = SpaceId("unknown")

  def normalize(space: SpaceId): SpaceId =
    normalize(space.value)

  def normalize(space: String): SpaceId =
    val key = space.trim.toLowerCase.replace("-", "").replace(" ", "")
    key match
      case "mni152" => MNI152
      case "mni305" => MNI305
      case "mni152nlin6asym" => MNI152NLin6Asym
      case "mni152nlin2009casym" => MNI152NLin2009cAsym
      case "fsaverage" => FsAverage
      case "fsaverage5" => FsAverage5
      case "fsaverage6" => FsAverage6
      case "fslr" | "fslr32k" | "fslr_32k" => FsLR32k
      case "" => Unknown
      case other => SpaceId(space.trim)

enum AtlasRepresentation:
  case Volume, Surface, Derived

enum Confidence:
  case Exact, High, Approximate, Uncertain

final case class AtlasArtifact(
  role: String,
  sourceName: String,
  sourceRef: String,
  sourceUrl: Option[String] = None,
  citationDoi: Option[String] = None,
  license: Option[String] = None,
  sha256: Option[String] = None,
  notes: Option[String] = None
):
  require(role.trim.nonEmpty, "artifact role must be non-empty")
  require(sourceName.trim.nonEmpty, "artifact sourceName must be non-empty")
  require(sourceRef.trim.nonEmpty, "artifact sourceRef must be non-empty")

final case class AtlasHistoryStep(
  action: String,
  fromTemplateSpace: SpaceId,
  toTemplateSpace: SpaceId,
  fromCoordSpace: SpaceId,
  toCoordSpace: SpaceId,
  status: TransformStatus,
  confidence: Confidence,
  details: String
):
  require(action.trim.nonEmpty, "history action must be non-empty")

final case class AtlasRef(
  family: String,
  model: String,
  representation: AtlasRepresentation,
  templateSpace: SpaceId,
  coordSpace: SpaceId,
  resolution: Option[String] = None,
  density: Option[String] = None,
  provenance: Option[String] = None,
  source: Option[String] = None,
  lineage: Option[String] = None,
  confidence: Confidence = Confidence.Uncertain,
  notes: Option[String] = None,
  artifacts: Vector[AtlasArtifact] = Vector.empty,
  history: Vector[AtlasHistoryStep] = Vector.empty
):
  require(family.trim.nonEmpty, "atlas family must be non-empty")
  require(model.trim.nonEmpty, "atlas model must be non-empty")

  def name: String = s"$family:$model"

  def toProvenance(regions: RegionIndex): AtlasProvenance =
    AtlasProvenance.fromRef(this, regions)
