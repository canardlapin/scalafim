package scalafim.atlas

/** Anatomical V1-V5 subset of the pinned Julich hard summary. V3V retains its
  * ventral anatomical name; no functional localization or rendering is done.
  */
case object JulichVisualRequest:
  val source: FslAtlasRequest = FslAtlasRequest(FslAtlasFamily.Julich)

  def area(region: AtlasRegionMetadata): Option[String] =
    val pattern = "GM Visual cortex (V1|V2|V3V|V4|V5)(?: BA[0-9]+)? [LR]".r
    region.fullLabel.value match
      case pattern(value) => Some(value)
      case _ => None

  def select(parent: VolumeAtlas): Either[AtlasAcquisitionError, VolumeAtlas] =
    if parent.ref.family != source.ref.family || parent.ref.model != source.ref.model ||
      parent.ref.parcelVariant != source.ref.parcelVariant then
      Left(AtlasAcquisitionError.Parse("visual subset requires the pinned Julich summary identity"))
    else parent.subsetEither(region => area(region).nonEmpty).left.map(AtlasAcquisitionError.Realization.apply)
