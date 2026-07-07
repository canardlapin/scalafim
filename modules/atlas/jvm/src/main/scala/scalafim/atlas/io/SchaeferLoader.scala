package scalafim.atlas.io

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scalafim.atlas.*

object SchaeferLoader:
  private val baseUri =
    "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI"

  final case class Assets(volume: AtlasAsset, labels: AtlasAsset)

  def assets(spec: Schaefer2018): Assets =
    val volumeName =
      s"Schaefer2018_${spec.parcels.value}Parcels_${spec.networks.value}Networks_order_FSLMNI152_${spec.resolution.mm}mm.nii.gz"
    val labelName =
      s"Schaefer2018_${spec.parcels.value}Parcels_${spec.networks.value}Networks_order.txt"
    Assets(
      volume = AtlasAsset(
        key = spec.id + "-volume",
        fileName = volumeName,
        uri = URI.create(s"$baseUri/$volumeName"),
        minBytes = 1024L
      ),
      labels = AtlasAsset(
        key = spec.id + "-labels",
        fileName = labelName,
        uri = URI.create(s"$baseUri/freeview_lut/$labelName"),
        minBytes = 16L
      )
    )

  def load(
    spec: Schaefer2018,
    store: AtlasStore = FileAtlasStore.default,
    policy: AssetPolicy = AssetPolicy.CacheOrDownload
  ): VolumeAtlas =
    val a = assets(spec)
    val volumePath = store.resolve(a.volume, policy)
    val labelPath = store.resolve(a.labels, policy)
    loadFromPaths(spec, volumePath, labelPath)

  def loadFromPaths(spec: Schaefer2018, volumePath: Path, labelPath: Path): VolumeAtlas =
    val labelVol = AtlasLabelMaps.readIntVolume(volumePath, spec.id)
    val presentIds = presentRegionIds(labelVol)
    val allRegions = parseLut(Files.readString(labelPath, StandardCharsets.UTF_8), spec)
    val regions = RegionIndex(allRegions.filter(r => presentIds.contains(r.id)))
    val ref = refFor(spec)
    val provenance =
      AtlasProvenanceFiles.withLocalFiles(
        AtlasProvenance.loaded(ref, regions, allRegions.map(_.id)),
        ArtifactRole.ParcellationVolume -> volumePath,
        ArtifactRole.LabelTable -> labelPath
      )
    AtlasLabelMaps.buildAtlas(ref, regions, labelVol, spec.id).copy(provenance = provenance)

  def refFor(spec: Schaefer2018): VolumeAtlasRef =
    val a = assets(spec)
    spec.atlasRef().copy(
      artifacts = Vector(
        AtlasArtifact(
          role = ArtifactRole.ParcellationVolume,
          sourceName = "CBIG",
          sourceRef = a.volume.fileName,
          sourceUrl = Some(a.volume.uri.toString),
          citationDoi = Some("10.1093/cercor/bhx179"),
          license = Some("Unspecified: consult CBIG upstream repository terms"),
          notes = Some(s"Schaefer2018 ${spec.parcels.value}-parcel ${spec.networks.value}-network volumetric labelmap")
        ),
        AtlasArtifact(
          role = ArtifactRole.LabelTable,
          sourceName = "CBIG",
          sourceRef = a.labels.fileName,
          sourceUrl = Some(a.labels.uri.toString),
          citationDoi = Some("10.1093/cercor/bhx179"),
          license = Some("Unspecified: consult CBIG upstream repository terms"),
          notes = Some("CBIG Freeview LUT label table")
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
          details = s"Loaded Schaefer2018 ${spec.parcels.value}-parcel ${spec.networks.value}-network atlas at ${spec.resolution.mm}mm."
        )
      )
    )

  def parseLut(text: String, spec: Schaefer2018): Vector[Region] =
    val prefix = s"${spec.networks.value}Networks_"
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
            val full = parts(1)
            val canonical = full.stripPrefix(prefix)
            val tokens = canonical.split("_").toVector
            val hemi =
              tokens.headOption match
                case Some("LH") => Some(Hemisphere.Left)
                case Some("RH") => Some(Hemisphere.Right)
                case _ => None
            val network = tokens.lift(1).map(NetworkId.apply)
            val label =
              if tokens.length >= 2 then tokens.takeRight(2).mkString("_")
              else canonical
            Some(
              Region(
                id = RegionId(id),
                label = label,
                labelFull = Some(full),
                hemisphere = hemi,
                network = network,
                color = Some(Rgb(parts(2).toInt, parts(3).toInt, parts(4).toInt)),
                attributes = Map(
                  "atlas" -> "Schaefer2018",
                  "parcels" -> spec.parcels.value.toString,
                  "networks" -> spec.networks.value.toString
                )
              )
            )
      }
      .toVector
      .sortBy(_.id.value)

  private def presentRegionIds(vol: scalafim.image.NeuroVol[Int]): Set[RegionId] =
    val out = scala.collection.mutable.Set.empty[RegionId]
    var i = 0
    while i < vol.values.data.length do
      val id = vol.linear(i)
      if id > 0 then out += RegionId(id)
      i += 1
    out.toSet
