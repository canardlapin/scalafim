package scalafim.atlas.io

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scalafim.atlas.*

object BrainnetomeLoader:
  private val downloadBase =
    "https://pan.cstcloud.cn/s/api/shareDownload"

  final case class Assets(volume: AtlasAsset, lut: AtlasAsset, networks: AtlasAsset)

  def assets(spec: Brainnetome246 = Brainnetome246.default): Assets =
    def asset(key: String, fileName: String, shareId: String, fid: String, minBytes: Long): AtlasAsset =
      AtlasAsset(
        key = s"${spec.id}-$key",
        fileName = fileName,
        uri = URI.create(s"$downloadBase?shareId=$shareId&fid=$fid"),
        minBytes = minBytes
      )

    Assets(
      volume = asset("volume", "BN_Atlas_246_1mm.nii.gz", "gfGflpp3Q0E", "86217173499925", 100000L),
      lut = asset("lut", "BN_Atlas_246_LUT.txt", "Edvop4rRtU", "86217173499928", 1000L),
      networks = asset("networks", "subregion_func_network_Yeo_updated.csv", "EYTnTX5SS5c", "86217173500082", 1000L)
    )

  def load(
    spec: Brainnetome246 = Brainnetome246.default,
    store: AtlasStore = FileAtlasStore.default,
    policy: AssetPolicy = AssetPolicy.CacheOrDownload
  ): VolumeAtlas =
    val a = assets(spec)
    val volumePath = store.resolve(a.volume, policy)
    val lutPath = store.resolve(a.lut, policy)
    val networkPath = store.resolve(a.networks, policy)
    loadFromPaths(spec, volumePath, lutPath, Some(networkPath))

  def loadFromPaths(
    spec: Brainnetome246,
    volumePath: Path,
    lutPath: Path,
    networkPath: Option[Path] = None
  ): VolumeAtlas =
    val labelVol = AtlasLabelMaps.readIntVolume(volumePath, spec.id)
    val presentIds = AtlasLabelMaps.presentRegionIds(labelVol)
    val networkText = networkPath.map(path => Files.readString(path, StandardCharsets.UTF_8))
    val allRegions = parseLut(Files.readString(lutPath, StandardCharsets.UTF_8), networkText, spec)
    val regions = RegionIndex(allRegions.filter(r => presentIds.contains(r.id)))
    val ref = refFor(spec)
    val localFiles =
      Vector(
        ArtifactRole.ParcellationVolume -> volumePath,
        ArtifactRole.LabelTable -> lutPath
      ) ++ networkPath.map(path => ArtifactRole.NetworkTable -> path).toVector
    val provenance =
      AtlasProvenanceFiles.withLocalFiles(
        AtlasProvenance.loaded(ref, regions, allRegions.map(_.id)),
        localFiles*
      )
    AtlasLabelMaps.buildAtlas(ref, regions, labelVol, provenance)

  def refFor(spec: Brainnetome246 = Brainnetome246.default): VolumeAtlasRef =
    val a = assets(spec)
    spec.atlasRef().copy(
      artifacts = Vector(
        AtlasArtifact(
          role = ArtifactRole.ParcellationVolume,
          sourceName = "Brainnetome Center",
          sourceRef = a.volume.fileName,
          sourceUrl = Some(spec.source.pageUrl),
          citationDoi = Some("10.1093/cercor/bhw157"),
          license = Some("Restricted: Brainnetome website legal agreement"),
          notes = Some("Brainnetome Center MNI152 1mm labelmap; downloaded on demand.")
        ),
        AtlasArtifact(
          role = ArtifactRole.LabelTable,
          sourceName = "Brainnetome Center",
          sourceRef = a.lut.fileName,
          sourceUrl = Some(spec.source.pageUrl),
          citationDoi = Some("10.1093/cercor/bhw157"),
          license = Some("Restricted: Brainnetome website legal agreement"),
          notes = Some("Brainnetome Freeview-style LUT label table.")
        ),
        AtlasArtifact(
          role = ArtifactRole.NetworkTable,
          sourceName = "Brainnetome Center",
          sourceRef = a.networks.fileName,
          sourceUrl = Some(spec.source.pageUrl),
          citationDoi = Some("10.1093/cercor/bhw157"),
          license = Some("Restricted: Brainnetome website legal agreement"),
          notes = Some("Brainnetome to Yeo 7/17-network membership table.")
        )
      ),
      history = Vector(
        AtlasHistoryStep(
          action = "load",
          fromTemplateSpace = SpaceId.MNI152,
          toTemplateSpace = SpaceId.MNI152,
          fromCoordSpace = SpaceId.MNI152,
          toCoordSpace = SpaceId.MNI152,
          status = TransformStatus.Available,
          confidence = Confidence.High,
          details = "Loaded Brainnetome 246-region MNI152 1mm atlas."
        )
      )
    )

  def parseLut(
    lutText: String,
    networkText: Option[String] = None,
    spec: Brainnetome246 = Brainnetome246.default
  ): Vector[AtlasRegionMetadata] =
    val networkById = networkText.map(parseNetworks).getOrElse(Map.empty)
    lutText.linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap { line =>
        val parts = line.split("\\s+").toVector
        if parts.length < 5 then None
        else
          val id = parts(0).toInt
          if id <= 0 then None
          else
            val label = parts(1)
            val net = networkById.get(id)
            val hemi =
              if label.endsWith("_L") then Some(Hemisphere.Left)
              else if label.endsWith("_R") then Some(Hemisphere.Right)
              else None
            val attrs =
              Map(
                "atlas" -> "BrainnetomeAtlas246",
                "parcels" -> "246"
              ) ++ net.toVector.flatMap(_.attributes)
            Some(
              Region(
                id = RegionId(id),
                label = label,
                labelFull = net.flatMap(_.region).map(region => s"$region: $label").orElse(Some(label)),
                hemisphere = hemi,
                network = net.flatMap(_.yeo17Name).map(NetworkId.apply),
                color = Some(Rgb(parts(2).toInt, parts(3).toInt, parts(4).toInt)),
                attributes = attrs
              )
            )
      }
      .toVector
      .sortBy(_.id.value)

  private final case class NetworkRow(
    id: Int,
    subregionName: Option[String],
    region: Option[String],
    yeo7: Option[String],
    yeo17: Option[String]
  ):
    def yeo7Name: Option[String] =
      yeo7.flatMap(BrainnetomeLoader.yeo7Name)

    def yeo17Name: Option[String] =
      yeo17.flatMap(BrainnetomeLoader.yeo17Name)

    def attributes: Map[String, String] =
      Vector(
        "subregion_name" -> subregionName,
        "region" -> region,
        "yeo_7network" -> yeo7,
        "yeo_7network_name" -> yeo7Name,
        "yeo_17network" -> yeo17,
        "yeo_17network_name" -> yeo17Name
      ).collect { case (key, Some(value)) if value.trim.nonEmpty => key -> value }.toMap

  private def parseNetworks(text: String): Map[Int, NetworkRow] =
    val rows = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val headerIndex = rows.indexWhere(line => csvRow(line).exists(_.trim == "Label"))
    if headerIndex < 0 then Map.empty
    else
      val headers = csvRow(rows(headerIndex)).map(_.trim)
      val index = headers.zipWithIndex.toMap
      def value(row: Vector[String], name: String): Option[String] =
        index.get(name).flatMap(i => row.lift(i)).map(_.trim).filter(_.nonEmpty)

      rows.drop(headerIndex + 1).flatMap { line =>
        val row = csvRow(line)
        value(row, "Label").flatMap(label => label.toIntOption).filter(_ > 0).map { id =>
          id -> NetworkRow(
            id = id,
            subregionName = value(row, "subregion_name"),
            region = value(row, "region"),
            yeo7 = value(row, "Yeo_7network"),
            yeo17 = value(row, "Yeo_17network")
          )
        }
      }.toMap

  private def csvRow(line: String): Vector[String] =
    val fields = Vector.newBuilder[String]
    val current = new StringBuilder
    var i = 0
    var quoted = false
    while i < line.length do
      val c = line.charAt(i)
      if c == '"' then
        if quoted && i + 1 < line.length && line.charAt(i + 1) == '"' then
          current.append('"')
          i += 1
        else quoted = !quoted
      else if c == ',' && !quoted then
        fields += current.result()
        current.clear()
      else current.append(c)
      i += 1
    fields += current.result()
    fields.result()

  private def yeo7Name(value: String): Option[String] =
    value.trim match
      case "1" => Some("Visual")
      case "2" => Some("Somatomotor")
      case "3" => Some("Dorsal Attention")
      case "4" => Some("Ventral Attention")
      case "5" => Some("Limbic")
      case "6" => Some("Frontoparietal")
      case "7" => Some("Default")
      case _ => None

  private def yeo17Name(value: String): Option[String] =
    value.trim match
      case "1"  => Some("Visual peripheral")
      case "2"  => Some("Visual central")
      case "3"  => Some("Somato-motor A")
      case "4"  => Some("Somato-motor B")
      case "5"  => Some("Dorsal attention A")
      case "6"  => Some("Dorsal attention B")
      case "7"  => Some("Ventral attention")
      case "8"  => Some("Salience")
      case "9"  => Some("Limbic-1")
      case "10" => Some("Limbic-2")
      case "11" => Some("Control C")
      case "12" => Some("Control A")
      case "13" => Some("Control B")
      case "14" => Some("Default D (Auditory)")
      case "15" => Some("Default C")
      case "16" => Some("Default A")
      case "17" => Some("Default B")
      case _ => None
