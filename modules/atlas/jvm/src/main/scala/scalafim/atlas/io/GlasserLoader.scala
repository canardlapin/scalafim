package scalafim.atlas.io

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scalafim.atlas.*
import scalafim.image.world.SpaceEvidence

object GlasserLoader:
  final case class Assets(volume: AtlasAsset, labels: AtlasAsset)

  def assets(spec: GlasserHcpMmp1): Assets =
    val labelUri = URI.create(
      "https://raw.githubusercontent.com/PennLINC/xcpEngine/master/atlas/glasser360/glasser360NodeNames.txt"
    )
    spec.source match
      case GlasserSource.Mni2009c =>
        Assets(
          volume = AtlasAsset(
            key = spec.id + "-volume",
            fileName = "MMP_in_MNI_corr.nii.gz",
            uri = URI.create(
              "https://raw.githubusercontent.com/Raj-Lab-UCSF/Human_Brain_Atlases-glasser/master/MMP_in_MNI_corr.nii.gz"
            ),
            minBytes = 1024L
          ),
          labels = AtlasAsset(spec.id + "-labels", "glasser360NodeNames.txt", labelUri, minBytes = 16L)
        )
      case GlasserSource.XcpEngine =>
        Assets(
          volume = AtlasAsset(
            key = spec.id + "-volume",
            fileName = "glasser360MNI.nii.gz",
            uri = URI.create(
              "https://raw.githubusercontent.com/PennLINC/xcpEngine/master/atlas/glasser360/glasser360MNI.nii.gz"
            ),
            minBytes = 1024L
          ),
          labels = AtlasAsset(spec.id + "-labels", "glasser360NodeNames.txt", labelUri, minBytes = 16L)
        )

  def load(
    spec: GlasserHcpMmp1 = GlasserHcpMmp1(),
    store: AtlasStore = FileAtlasStore.default,
    policy: AssetPolicy = AssetPolicy.CacheOrDownload
  ): VolumeAtlas =
    val a = assets(spec)
    val volumePath = store.resolve(a.volume, policy)
    val labelPath = store.resolve(a.labels, policy)
    loadFromPaths(spec, volumePath, labelPath)

  def loadFromPaths(spec: GlasserHcpMmp1, volumePath: Path, labelPath: Path): VolumeAtlas =
    val labelVol = AtlasLabelMaps.readIntVolume(volumePath, SpaceEvidence(bidsSpace = Some(spec.atlasRef().templateSpace.value)), spec.id)
    val presentIds = AtlasLabelMaps.presentRegionIds(labelVol)
    val allRegions = parseLabels(Files.readString(labelPath, StandardCharsets.UTF_8))
    val regions = RegionIndex(allRegions.filter(r => presentIds.contains(r.id)))
    val ref = refFor(spec)
    val provenance =
      AtlasProvenanceFiles.withLocalFiles(
        AtlasProvenance.loaded(ref, regions, allRegions.map(_.id)),
        ArtifactRole.ParcellationVolume -> volumePath,
        ArtifactRole.LabelTable -> labelPath
      )
    AtlasLabelMaps.buildAtlas(ref, regions, labelVol, provenance)

  def refFor(spec: GlasserHcpMmp1): VolumeAtlasRef =
    val a = assets(spec)
    spec.atlasRef().withDetails(details => details.copy(
      artifacts = Vector(
        AtlasArtifact(
          role = ArtifactRole.ParcellationVolume,
          sourceName = spec.source.key,
          sourceRef = a.volume.fileName,
          sourceUrl = Some(a.volume.uri.toString),
          citationDoi = Some("10.1038/nature18933"),
          license = Some("Unspecified: consult HCP-MMP1.0 and xcpEngine source terms"),
          notes = spec.source.notes
        ),
        AtlasArtifact(
          role = ArtifactRole.LabelTable,
          sourceName = "xcpEngine",
          sourceRef = a.labels.fileName,
          sourceUrl = Some(a.labels.uri.toString),
          citationDoi = Some("10.1038/nature18933"),
          license = Some("Unspecified: consult HCP-MMP1.0 and xcpEngine source terms"),
          notes = Some("xcpEngine Glasser360 node-name table")
        )
      ),
      history = Vector(
        AtlasHistoryStep(
          action = "load",
          fromTemplateSpace = spec.source.defaultTemplateSpace,
          toTemplateSpace = spec.source.defaultTemplateSpace,
          fromCoordSpace = SpaceId.MNI152,
          toCoordSpace = SpaceId.MNI152,
          status = TransformStatus.Available,
          confidence = spec.source.defaultConfidence,
          details = s"Loaded Glasser HCP-MMP1.0 volume from source '${spec.source.key}'."
        )
      )
    ))

  def parseLabels(text: String): Vector[AtlasRegionMetadata] =
    parseLabelsEither(text).fold(error => throw new IllegalArgumentException(error.message), identity)

  def parseLabelsEither(text: String): Either[ParcelIdentityError, Vector[AtlasRegionMetadata]] =
    text.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .zipWithIndex
      .foldLeft[Either[ParcelIdentityError, Vector[AtlasRegionMetadata]]](Right(Vector.empty)):
        (result, entry) =>
          val (line, idx) = entry
          val name = line.split("\\s+").head
          for
            regions <- result
            key <- GlasserParcelKey.fromSourceLabel(name)
          yield regions :+ AtlasRegionMetadata.fromStrings(
            id = RegionId(idx + 1),
            label = key.area,
            labelFull = Some(key.value),
            hemisphere = Some(key.hemisphere),
            attributes = Map("atlas" -> "HCP-MMP1.0", "source_label" -> name)
          )
