package scalafim.atlas

/** Native upstream subcortical segmentations. These requests intentionally do
  * not reproduce AtlasPack's template harmonization or voxel-grid splitting.
  */
enum SubcorticalAtlasFamily:
  case Cit168, HcpThalamic, Mdtb10, HcpHippocampusAmygdala

final case class SubcorticalAtlasRequest(family: SubcorticalAtlasFamily):
  def id: String = family match
    case SubcorticalAtlasFamily.Cit168 => "cit168-native"
    case SubcorticalAtlasFamily.HcpThalamic => "hcp-thalamic-native"
    case SubcorticalAtlasFamily.Mdtb10 => "mdtb10-native"
    case SubcorticalAtlasFamily.HcpHippocampusAmygdala => "hcp-hippocampus-amygdala-native"

  def ref: VolumeAtlasRef =
    val (model, template, resolution, source, note): (String, VolumeOrUnknownSpaceId, String, String, String) = family match
      case SubcorticalAtlasFamily.Cit168 =>
        ("CIT168", SpaceId.MNI152NLin6Asym, "1mm", "osf.io/2qswg",
          "Native, unsplit CIT168 source labels; AtlasPack's derived L/R split is not represented.")
      case SubcorticalAtlasFamily.HcpThalamic =>
        ("HCPThalamic", SpaceId.unknown("MNI152NLin2009aSym"), "1mm", "zenodo:1405484",
          "Native HCP thalamic maximum-probability labels; no template conversion is implied.")
      case SubcorticalAtlasFamily.Mdtb10 =>
        ("MDTB10", SpaceId.MNI152NLin6Asym, "1mm", "DiedrichsenLab/cerebellar_atlases",
          "Native King_2019 MNI-labelled segmentation; no TemplateFlow resampling is implied.")
      case SubcorticalAtlasFamily.HcpHippocampusAmygdala =>
        ("HPandAMYG", SpaceId.MNI152NLin6Asym, "1.6mm", "HCPpipelines Atlas_ROIs.1.60",
          "Exact hippocampus/amygdala selection from the source HCP ROI segmentation; no resampling is implied.")
    AtlasRef.volume(
      family = "subcortical", model = model, templateSpace = template, coordSpace = SpaceId.MNI152,
      resolution = Some(resolution), source = Some(source), provenance = Some(volume.url),
      lineage = Some(s"Immutable native source revision ${volume.revision}."), confidence = Confidence.Uncertain,
      parcelVariant = Some("native-source-labels"), notes = Some(note))

  def volume: PinnedAtlasAsset = family match
    case SubcorticalAtlasFamily.Cit168 => SubcorticalAtlasRequest.cit168Volume
    case SubcorticalAtlasFamily.HcpThalamic => SubcorticalAtlasRequest.hcpThalamicVolume
    case SubcorticalAtlasFamily.Mdtb10 => SubcorticalAtlasRequest.mdtb10Volume
    case SubcorticalAtlasFamily.HcpHippocampusAmygdala => SubcorticalAtlasRequest.hcpRoisVolume

  def labels: PinnedAtlasAsset = family match
    case SubcorticalAtlasFamily.Cit168 => SubcorticalAtlasRequest.cit168Labels
    case SubcorticalAtlasFamily.HcpThalamic => SubcorticalAtlasRequest.hcpThalamicLabels
    case SubcorticalAtlasFamily.Mdtb10 => SubcorticalAtlasRequest.mdtb10Labels
    case SubcorticalAtlasFamily.HcpHippocampusAmygdala => SubcorticalAtlasRequest.hcpRoisLabels

object SubcorticalAtlasRequest:
  private val atlasPackRevision = "2727a5fb14a42fb5995bdfc309112af5e4342cab"
  private val mdtbRevision = "6857dfa732de6dd60c94da2f84d0b15ccf316618"
  private val hcpRevision = "fb25ef4e9be44402d1d746834d18050842bb4042"
  private val xcpRevision = "8a07b53b8a1bb921cd8ca3dc2d23fd09da7982fd"

  val cit168Volume = PinnedAtlasAsset("cit168-native-volume", "CIT168.nii.gz", "https://osf.io/download/2qswg",
    "osf:2qswg", "8b43e911c6849c9a57c9153499a324d2e3894531bb31446b6905b4c357693ae0", 44739L)
  val cit168Labels = PinnedAtlasAsset("cit168-labels", "CIT168-labels.txt", "https://osf.io/download/6qrcb",
    "osf:6qrcb", "5e00265a7bbbdf242356345e0f6615d84791ada54aa9739539096aab3c1e1b41", 123L)
  val hcpThalamicVolume = PinnedAtlasAsset("hcp-thalamic-native-volume", "Thalamus_Nuclei-HCP-MaxProb.nii.gz",
    "https://zenodo.org/records/1405484/files/Thalamus_Nuclei-HCP-MaxProb.nii.gz", "zenodo:1405484",
    "68f0229de14a5375e0b438784787e73180483cff243a932d0d6f598c42400174", 14805L)
  val hcpThalamicLabels = PinnedAtlasAsset("hcp-thalamic-lut", "Thalamic_Nuclei-ColorLUT.txt",
    "https://zenodo.org/records/1405484/files/Thalamic_Nuclei-ColorLUT.txt", "zenodo:1405484",
    "141df5af64bb33fb51671d2244b38764828e54398ad4bd7488f01daf1e7f39f8", 1336L)
  val mdtb10Volume = PinnedAtlasAsset("mdtb10-native-volume", "atl-MDTB10_space-MNI_dseg.nii",
    s"https://raw.githubusercontent.com/DiedrichsenLab/cerebellar_atlases/$mdtbRevision/King_2019/atl-MDTB10_space-MNI_dseg.nii", mdtbRevision,
    "48de27c43ddff528442bfb7256eb1f237b533ff432b885ef781db74196038322", 1165717L)
  val mdtb10Labels = PinnedAtlasAsset("mdtb10-labels", "atl-MDTB10.tsv",
    s"https://raw.githubusercontent.com/DiedrichsenLab/cerebellar_atlases/$mdtbRevision/King_2019/atl-MDTB10.tsv", mdtbRevision,
    "c51b7795f1993c27aa0de8de6c09d0837d61d71afae37abc5aaa78b91214a404", 199L)
  val hcpRoisVolume = PinnedAtlasAsset("hcp-rois-1.60-volume", "Atlas_ROIs.1.60.nii.gz",
    s"https://raw.githubusercontent.com/Washington-University/HCPpipelines/$hcpRevision/global/templates/170494_Greyordinates/Atlas_ROIs.1.60.nii.gz", hcpRevision,
    "75b295274c31311b4e6c42e7a1804c8a0bc31a12199dcb561f8628afb2ed340b", 25355L)
  val hcpRoisLabels = PinnedAtlasAsset("hcp-rois-labels", "atlas-HCP_dseg.tsv",
    s"https://raw.githubusercontent.com/PennLINC/xcp_d/$xcpRevision/xcp_d/data/atlases/atlas-HCP_dseg.tsv", xcpRevision,
    "a3826e3324d62c8ca04dac5031912ad658250e3782f56526039daa748357b354", 1027L)
