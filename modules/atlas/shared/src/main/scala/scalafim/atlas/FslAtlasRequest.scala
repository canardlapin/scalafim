package scalafim.atlas

enum FslAtlasFamily(val key: String, val model: String, val xmlName: String, val imagePath: String):
  case HarvardOxfordCortical extends FslAtlasFamily("harvard-oxford-cortical", "HarvardOxfordCortical",
    "HarvardOxford-Cortical.xml", "HarvardOxford/HarvardOxford-cort-maxprob-thr25-2mm.nii.gz")
  case HarvardOxfordSubcortical extends FslAtlasFamily("harvard-oxford-subcortical", "HarvardOxfordSubcortical",
    "HarvardOxford-Subcortical.xml", "HarvardOxford/HarvardOxford-sub-maxprob-thr25-2mm.nii.gz")
  case Julich extends FslAtlasFamily("julich", "JulichHistological",
    "Juelich.xml", "Juelich/Juelich-maxprob-thr25-2mm.nii.gz")

/** Pinned FSL hard-summary release. Only the verified 2mm/25% summaries are
  * advertised by these named requests; generic FSL XML parsing is independent.
  */
final case class FslAtlasRequest(family: FslAtlasFamily):
  def ref: VolumeAtlasRef = AtlasRef.volume(family.key, family.model,
    SpaceId.MNI152NLin6Asym, SpaceId.MNI152, resolution = Some("2mm"),
    source = Some("fsl-data-atlases"), parcelVariant = Some("maxprob-thr25"),
    lineage = Some(s"FSL data_atlases revision ${FslAtlasRequest.revision}; hard maximum-probability summary"))

  def xml: PinnedAtlasAsset = PinnedAtlasAsset(family.key + "-xml", family.xmlName,
    FslAtlasRequest.base + family.xmlName, FslAtlasRequest.revision,
    family match
      case FslAtlasFamily.HarvardOxfordCortical => "b8eb5ba5e787a4d3b442f75041f533f31b278a83e467bd2ab22438c1cff36581"
      case FslAtlasFamily.HarvardOxfordSubcortical => FslAtlasRequest.subXmlSha
      case FslAtlasFamily.Julich => "3cdfa38ce10066df784b0e9083f450e1322ac00a315ff91c93fc416e4ce194ba")

  def volume: PinnedAtlasAsset = PinnedAtlasAsset(family.key + "-volume", family.imagePath.split('/').last,
    FslAtlasRequest.base + family.imagePath, FslAtlasRequest.revision,
    family match
      case FslAtlasFamily.HarvardOxfordCortical => "7397ffdbae7559e0f0aa6237f998dc0f6aca3f9db9009f6ffa637c0c62b686ca"
      case FslAtlasFamily.HarvardOxfordSubcortical => FslAtlasRequest.subVolumeSha
      case FslAtlasFamily.Julich => "d2209b9f70c6e273d44ef36a10b5ef4f96b1b6f81fed696dbe70320d252e700d")

object FslAtlasRequest:
  val revision = "b3ad6133f723052d8295c48c68bbc8ab05961874"
  private val base = s"https://git.fmrib.ox.ac.uk/fsl/data_atlases/-/raw/$revision/"
  private[atlas] val subXmlSha = "4aa369944d514843a07b5e6febac0c844e76fc1980ab203a0021b083de699650"
  private[atlas] val subVolumeSha = "72140df8117250d915b753ca2937e078c917525d206e6185e2c1b4ab703fbfcc"
