package scalafim.atlas

/** Pinned HCPex v1.1 volume request. The two native grids are distinct source
  * representations and are never inferred from their geometry. */
final case class HcpExRequest(resolution: VoxelResolution = VoxelResolution.OneMm):
  def id: String = s"hcpex-426-${resolution.mm}mm"

  def ref: VolumeAtlasRef =
    AtlasRef.volume(
      family = "hcpex", model = "HCPex", templateSpace = SpaceId.MNI152NLin2009cAsym,
      coordSpace = SpaceId.MNI152, resolution = Some(s"${resolution.mm}mm"),
      provenance = Some("https://github.com/wayalan/HCPex"), source = Some("HCPex_v1.1"),
      lineage = Some(s"HCPex v1.1 immutable revision ${HcpExRequest.revision}; native HCPex ordering."),
      confidence = Confidence.High, parcelVariant = Some("426-regions"),
      notes = Some("360 cortical and 66 subcortical labels; no resampling or registration is implied."))

  def volume: PinnedAtlasAsset =
    if resolution == VoxelResolution.OneMm then HcpExRequest.oneMmVolume else HcpExRequest.twoMmVolume

  def labels: PinnedAtlasAsset = HcpExRequest.labelsAsset
  def lut: PinnedAtlasAsset = HcpExRequest.lutAsset

object HcpExRequest:
  val revision = "6d4082fcbfdb6814fc21dff82ff90b6c4ae33f30"
  private val base = s"https://raw.githubusercontent.com/wayalan/HCPex/$revision/HCPex_v1.1/"
  val oneMmVolume = PinnedAtlasAsset("hcpex-1mm-volume", "HCPex.nii.gz", base + "HCPex.nii.gz", revision,
    "f1cb3134812de0537e0e728a0946f111be0667ab371e80e775b9f8117b800063", 751874L)
  val twoMmVolume = PinnedAtlasAsset("hcpex-2mm-volume", "HCPex_2mm.nii", base + "HCPex_2mm.nii", revision,
    "728c43bad80eec611d7cf9c62a332b48015c2d4c787182a79386f092fe809fdf", 7221384L)
  val labelsAsset = PinnedAtlasAsset("hcpex-labels", "HCPex.nii.txt", base + "HCPex.nii.txt", revision,
    "2caae7c54e5d556a5a8fd45814567ef839cf43321d61c76d218e93add69e3b1d", 5851L)
  val lutAsset = PinnedAtlasAsset("hcpex-lut", "HCPex_LookUpTable.txt", base + "HCPex_LookUpTable.txt", revision,
    "58bb93b922735369b77ce4288ffddb66e825eb123b35e357f9d0c6c1bdff8745", 15838L)
