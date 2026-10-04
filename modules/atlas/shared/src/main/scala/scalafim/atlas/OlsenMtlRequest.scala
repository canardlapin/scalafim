package scalafim.atlas

enum OlsenMtlMode:
  case Mtl, Hippocampus

/** Olsen's source grid is explicitly an unknown MNI152_custom volume. Its
  * dimensions do not authorize a mapping to a standard MNI template. */
final case class OlsenMtlRequest(mode: OlsenMtlMode = OlsenMtlMode.Mtl):
  def id: String = mode match
    case OlsenMtlMode.Mtl => "olsen-mtl"
    case OlsenMtlMode.Hippocampus => "olsen-hippocampus"

  def ref: VolumeAtlasRef = AtlasRef.volume(
    family = "olsen", model = mode match
      case OlsenMtlMode.Mtl => "OlsenMTL"
      case OlsenMtlMode.Hippocampus => "OlsenHippocampus",
    templateSpace = SpaceId.unknown("MNI152_custom"), coordSpace = SpaceId.MNI152,
    resolution = Some("1mm"), source = Some("neuroatlas_extdata"),
    provenance = Some(OlsenMtlRequest.volume.url),
    lineage = Some(s"Immutable neuroatlas revision ${OlsenMtlRequest.revision}; custom-cropped MNI152-like grid."),
    confidence = Confidence.Uncertain,
    notes = Some("No standard-space transform is declared from geometry alone."))

  def volume: PinnedAtlasAsset = OlsenMtlRequest.volume
  def selectedIds: Vector[RegionId] = mode match
    case OlsenMtlMode.Mtl => OlsenMtlRequest.regions.map(_.id)
    case OlsenMtlMode.Hippocampus => OlsenMtlRequest.hippocampalIds.map(RegionId.apply)

object OlsenMtlRequest:
  val revision = "653974510d2357b55d411361f6ed3a62e872a586"
  val volume = PinnedAtlasAsset("olsen-mtl-volume", "Olsen_MNI_MTL_prob33.nii.gz",
    s"https://raw.githubusercontent.com/bbuchsbaum/neuroatlas/$revision/inst/extdata/Olsen_MNI_MTL_prob33.nii.gz", revision,
    "5fdd9bad8e3ef42acd046e03edd3e819fe439e74100c779a39cfd3dec6251372", 38765L)
  val hippocampalIds = Vector(1, 2, 3, 6, 8, 9, 10, 11, 14, 16)
  private val names = Vector("L_Ant_Hipp", "L_CA1", "L_CA3_DG", "L_ERC", "L_PHC", "L_Post_Hipp", "L_PRC", "L_Sub",
    "R_Ant_Hipp", "R_CA1", "R_CA3_DG", "R_ERC", "R_PHC", "R_Post_Hipp", "R_PRC", "R_Sub")
  val regions: Vector[AtlasRegionMetadata] = names.zipWithIndex.map: (name, index) =>
    val left = name.startsWith("L_")
    AtlasRegionMetadata.fromStrings(RegionId(index + 1), name.drop(2), Some(name),
      Some(if left then Hemisphere.Left else Hemisphere.Right), attributes = Map("atlas" -> "OlsenMTL"))
