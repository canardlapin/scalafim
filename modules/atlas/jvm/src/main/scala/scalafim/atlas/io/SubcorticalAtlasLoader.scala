package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.atlas.*
import scalafim.image.world.XformCode

object SubcorticalAtlasLoader:
  def load(request: SubcorticalAtlasRequest, store: AtlasStore = FileAtlasStore.default,
      policy: AssetPolicy = AssetPolicy.CacheOrDownload): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      volume <- PinnedVolumeLoader.resolve(request.volume, store, policy)
      labels <- PinnedVolumeLoader.resolve(request.labels, store, policy)
      atlas <- loadFromPaths(request, volume, labels)
    yield atlas

  def loadFromPaths(request: SubcorticalAtlasRequest, volume: Path, labels: Path): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      _ <- PinnedVolumeLoader.verify(request.labels, labels)
      text <- PinnedVolumeLoader.attempt(Files.readString(labels, StandardCharsets.UTF_8))
      regions <- parse(request.family, text)
      loaded <- PinnedVolumeLoader.load(request.ref, regions, volume, Vector(
        (request.volume, ArtifactRole.ParcellationVolume, volume),
        (request.labels, ArtifactRole.LabelTable, labels)), coordinateAdmission(request.family))
      atlas <- if request.family == SubcorticalAtlasFamily.HcpHippocampusAmygdala then
        loaded.subsetEither(region => SubcorticalAtlasLoader.hippocampusAmygdalaIds.contains(region.id.value)).left.map(AtlasAcquisitionError.Realization.apply)
      else Right(loaded)
    yield atlas

  private def coordinateAdmission(family: SubcorticalAtlasFamily): AtlasCoordinateAdmission = family match
    case SubcorticalAtlasFamily.Cit168 => AtlasCoordinateAdmission.DeclaredArtifactCoordinates(
      XformCode.ScannerAnatomical, "Native CIT168 header is scanner-coded while the source describes an NLin6 grid.")
    case SubcorticalAtlasFamily.HcpThalamic | SubcorticalAtlasFamily.Mdtb10 => AtlasCoordinateAdmission.DeclaredArtifactCoordinates(
      XformCode.AlignedAnatomical, "Native source header is aligned-anatomical and does not identify a specific standard-template transform.")
    case SubcorticalAtlasFamily.HcpHippocampusAmygdala => AtlasCoordinateAdmission.RequireTemplateHeader

  def parse(family: SubcorticalAtlasFamily, text: String): Either[AtlasAcquisitionError, Vector[AtlasRegionMetadata]] =
    try Right(family match
      case SubcorticalAtlasFamily.Cit168 =>
        val rows = text.linesIterator.map(_.trim).filter(_.nonEmpty).map(_.split("\\s+").toVector).toVector
        require(rows.length == 16 && rows.forall(_.length == 2), "CIT168 requires 16 source label rows")
        rows.map: row =>
          val sourceId = row.head.toInt
          AtlasRegionMetadata.fromStrings(RegionId(sourceId + 1), row(1), Some(row(1)),
            attributes = Map("atlas" -> "CIT168", "source_label" -> sourceId.toString, "hemisphere" -> "unsplit"))
      case SubcorticalAtlasFamily.HcpThalamic =>
        val rows = text.linesIterator.map(_.trim).filter(line => line.headOption.exists(_.isDigit)).map(_.split("\\s+").toVector)
          .filter(_.head != "0").toVector
        rows.map: row =>
            require(row.length == 6, "invalid HCP thalamic LUT row")
            val hemisphere = if row(1).startsWith("LH-") then Some(Hemisphere.Left) else if row(1).startsWith("RH-") then Some(Hemisphere.Right) else None
            require(hemisphere.nonEmpty, "HCP thalamic label requires LH or RH prefix")
            AtlasRegionMetadata.fromStrings(RegionId(row.head.toInt), row(1).drop(3), Some(row(1)), hemisphere,
              attributes = Map("atlas" -> "HCPThalamic"))
      case SubcorticalAtlasFamily.Mdtb10 =>
        val rows = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
        require(rows.headOption.contains("index\tname\tcolor"), "invalid MDTB10 TSV header")
        rows.tail.map(_.split("\\t", -1).toVector).map: row =>
          require(row.length == 3 && row.head.matches("[1-9][0-9]*"), "invalid MDTB10 TSV row")
          AtlasRegionMetadata.fromStrings(RegionId(row.head.toInt), row(1), Some(row(1)), attributes = Map("atlas" -> "MDTB10"))
      case SubcorticalAtlasFamily.HcpHippocampusAmygdala =>
        val rows = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
        require(rows.headOption.contains("index\tlabel\tcifti_label\tcolor_red\tcolor_green\tcolor_blue\topacity"), "invalid HCP labels TSV header")
        val all = rows.tail.map(_.split("\\t", -1).toVector)
        require(all.forall(_.length == 7) && all.map(_.head).distinct.length == all.length, "invalid HCP labels rows")
        require(all.exists(row => hippocampusAmygdalaIds.contains(row.head.toInt)), "HCP labels lack hippocampus/amygdala IDs")
        all.map: row =>
          val hemisphere = if row(1).endsWith("_LEFT") then Hemisphere.Left else Hemisphere.Right
          val side = if row(1).endsWith("_LEFT") then Some(Hemisphere.Left) else if row(1).endsWith("_RIGHT") then Some(Hemisphere.Right) else None
          AtlasRegionMetadata.fromStrings(RegionId(row.head.toInt), row(1), Some(row(1)), side, attributes = Map("atlas" -> "HCPROIs"))
    ) catch case NonFatal(error) => Left(AtlasAcquisitionError.Parse(Option(error.getMessage).getOrElse("invalid subcortical metadata")))

  private val hippocampusAmygdalaIds = Set(17, 18, 53, 54)
