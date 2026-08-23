package scalafim.atlas.io

import java.net.URI
import java.nio.file.Path
import scalafim.atlas.*

object AsegLoader:
  final case class Assets(volume: AtlasAsset)

  final case class LabelRow(
    id: RegionId,
    label: String,
    hemisphere: Option[Hemisphere],
    color: Rgb,
    freesurferLabel: String
  )

  def assets(spec: FreeSurferAseg = FreeSurferAseg.default): Assets =
    Assets(
      volume = AtlasAsset(
        key = spec.id + "-volume",
        fileName = "atlas_aparc_aseg_prob33.nii.gz",
        uri = URI.create(spec.source.sourceUrl),
        minBytes = 50000L
      )
    )

  def load(
    spec: FreeSurferAseg = FreeSurferAseg.default,
    store: AtlasStore = FileAtlasStore.default,
    policy: AssetPolicy = AssetPolicy.CacheOrDownload
  ): VolumeAtlas =
    val volumePath = store.resolve(assets(spec).volume, policy)
    loadFromPaths(spec, volumePath)

  def loadFromPaths(spec: FreeSurferAseg, volumePath: Path): VolumeAtlas =
    val labelVol = AtlasLabelMaps.readIntVolume(volumePath, spec.id)
    val presentIds = AtlasLabelMaps.presentRegionIds(labelVol)
    val allRegions = regionsFor(spec)
    val regions = RegionIndex(allRegions.filter(r => presentIds.contains(r.id)))
    val ref = refFor(spec)
    val provenance =
      AtlasProvenanceFiles.withLocalFiles(
        AtlasProvenance.loaded(ref, regions, allRegions.map(_.id)),
        ArtifactRole.ParcellationVolume -> volumePath
      )
    AtlasLabelMaps.buildAtlas(ref, regions, labelVol, provenance)

  def refFor(spec: FreeSurferAseg = FreeSurferAseg.default): VolumeAtlasRef =
    val a = assets(spec)
    spec.atlasRef().copy(
      artifacts = Vector(
        AtlasArtifact(
          role = ArtifactRole.ParcellationVolume,
          sourceName = "neuroatlas",
          sourceRef = a.volume.fileName,
          sourceUrl = Some(a.volume.uri.toString),
          citationDoi = Some("10.1016/S0896-6273(02)00569-X"),
          license = Some("Unspecified: consult neuroatlas and FreeSurfer source terms"),
          notes = Some("Bundled neuroatlas volume derived from FreeSurfer ASEG labels.")
        )
      ),
      history = Vector(
        AtlasHistoryStep(
          action = "load",
          fromTemplateSpace = SpaceId.MNI152NLin6Asym,
          toTemplateSpace = SpaceId.MNI152NLin6Asym,
          fromCoordSpace = SpaceId.MNI152,
          toCoordSpace = SpaceId.MNI152,
          status = TransformStatus.Available,
          confidence = Confidence.High,
          details = "Loaded FreeSurfer ASEG atlas."
        )
      )
    )

  def labelRows: Vector[LabelRow] =
    Vector(
      LabelRow(RegionId(10), "Thalamus", Some(Hemisphere.Left), Rgb(0, 118, 14), "Left-Thalamus"),
      LabelRow(RegionId(11), "Caudate", Some(Hemisphere.Left), Rgb(122, 186, 220), "Left-Caudate"),
      LabelRow(RegionId(12), "Putamen", Some(Hemisphere.Left), Rgb(236, 13, 176), "Left-Putamen"),
      LabelRow(RegionId(13), "Pallidum", Some(Hemisphere.Left), Rgb(12, 48, 255), "Left-Pallidum"),
      LabelRow(RegionId(16), "Brainstem", None, Rgb(119, 159, 176), "Brain-Stem"),
      LabelRow(RegionId(17), "Hippocampus", Some(Hemisphere.Left), Rgb(220, 216, 20), "Left-Hippocampus"),
      LabelRow(RegionId(18), "Amygdala", Some(Hemisphere.Left), Rgb(220, 216, 20), "Left-Amygdala"),
      LabelRow(RegionId(26), "Accumbens", Some(Hemisphere.Left), Rgb(255, 165, 0), "Left-Accumbens-area"),
      LabelRow(RegionId(28), "VentralDC", None, Rgb(165, 42, 42), "Left-VentralDC"),
      LabelRow(RegionId(49), "Thalamus", Some(Hemisphere.Right), Rgb(0, 118, 14), "Right-Thalamus"),
      LabelRow(RegionId(50), "Caudate", Some(Hemisphere.Right), Rgb(122, 186, 220), "Right-Caudate"),
      LabelRow(RegionId(51), "Putamen", Some(Hemisphere.Right), Rgb(236, 13, 176), "Right-Putamen"),
      LabelRow(RegionId(52), "Pallidum", Some(Hemisphere.Right), Rgb(13, 48, 255), "Right-Pallidum"),
      LabelRow(RegionId(53), "Hippocampus", Some(Hemisphere.Right), Rgb(220, 216, 20), "Right-Hippocampus"),
      LabelRow(RegionId(54), "Amygdala", Some(Hemisphere.Right), Rgb(103, 255, 255), "Right-Amygdala"),
      LabelRow(RegionId(58), "Accumbens", Some(Hemisphere.Right), Rgb(255, 165, 0), "Right-Accumbens-area"),
      LabelRow(RegionId(60), "VentralDC", Some(Hemisphere.Right), Rgb(165, 42, 42), "Right-VentralDC")
    )

  def regionsFor(spec: FreeSurferAseg = FreeSurferAseg.default): Vector[AtlasRegionMetadata] =
    labelRows.map { row =>
      Region(
        id = row.id,
        label = row.label,
        labelFull = Some(row.label),
        hemisphere = row.hemisphere,
        color = Some(row.color),
        attributes = Map(
          "atlas" -> spec.model,
          "freesurfer_label" -> row.freesurferLabel
        )
      )
    }

  def parseColorLut(text: String): Vector[AtlasRegionMetadata] =
    text.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap { line =>
        val parts = line.split("\\s+").toVector
        if parts.length < 5 then None
        else
          val id = parts(0).toInt
          if id <= 0 then None
          else
            val fsLabel = parts(1)
            val (hemi, label) = normalizeFreeSurferLabel(fsLabel)
            Some(
              Region(
                id = RegionId(id),
                label = label,
                labelFull = Some(label),
                hemisphere = hemi,
                color = Some(Rgb(parts(2).toInt, parts(3).toInt, parts(4).toInt)),
                attributes = Map(
                  "atlas" -> "FreeSurferASEG",
                  "freesurfer_label" -> fsLabel
                )
              )
            )
      }
      .toVector
      .sortBy(_.id.value)

  private def normalizeFreeSurferLabel(label: String): (Option[Hemisphere], String) =
    val (hemi, stripped) =
      if label.startsWith("Left-") then (Some(Hemisphere.Left), label.stripPrefix("Left-"))
      else if label.startsWith("Right-") then (Some(Hemisphere.Right), label.stripPrefix("Right-"))
      else (None, label)
    val clean =
      stripped
        .stripSuffix("-Proper")
        .stripSuffix("-area")
        .replace("-", "")
    val neuroatlasLabel =
      if clean == "BrainStem" then "Brainstem" else clean
    (hemi, neuroatlasLabel)
