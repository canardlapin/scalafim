package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.atlas.*
import scalafim.image.world.XformCode

object HcpExLoader:
  def load(request: HcpExRequest = HcpExRequest(), store: AtlasStore = FileAtlasStore.default,
      policy: AssetPolicy = AssetPolicy.CacheOrDownload): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      volume <- PinnedVolumeLoader.resolve(request.volume, store, policy)
      labels <- PinnedVolumeLoader.resolve(request.labels, store, policy)
      lut <- PinnedVolumeLoader.resolve(request.lut, store, policy)
      atlas <- loadFromPaths(request, volume, labels, lut)
    yield atlas

  def loadFromPaths(request: HcpExRequest, volume: Path, labels: Path, lut: Path): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      _ <- PinnedVolumeLoader.verify(request.labels, labels)
      _ <- PinnedVolumeLoader.verify(request.lut, lut)
      regions <- parse(Files.readString(labels, StandardCharsets.UTF_8), Files.readString(lut, StandardCharsets.UTF_8))
      atlas <- PinnedVolumeLoader.load(request.ref, regions, volume, Vector(
        (request.volume, ArtifactRole.ParcellationVolume, volume),
        (request.labels, ArtifactRole.LabelTable, labels),
        (request.lut, ArtifactRole.LabelTable, lut)),
        AtlasCoordinateAdmission.DeclaredArtifactCoordinates(
          XformCode.ScannerAnatomical,
          "Pinned HCPex headers identify scanner anatomical coordinates while upstream v1.1 declares MNI152NLin2009cAsym."
        ))
    yield atlas

  def parse(labels: String, lut: String): Either[AtlasAcquisitionError, Vector[AtlasRegionMetadata]] =
    try
      val short = labels.linesIterator.map(_.trim).filter(_.nonEmpty).map(_.split("\\s+").toVector).toVector
      val colors = lut.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#"))
        .map(_.split("\\s+").toVector).filter(_.headOption.exists(_ != "0")).toVector
      require(short.length == 426 && colors.length == 426, "expected exactly 426 HCPex label rows")
      val shortById = short.map { row =>
        require(row.length == 4 && row(0) == row(3), "invalid HCPex short label row")
        row(0).toInt -> (row(1), row(2))
      }.toMap
      require(shortById.keySet == (1 to 426).toSet, "HCPex short labels must cover IDs 1 through 426")
      val colorById = colors.map { row =>
        require(row.length == 6, "invalid HCPex LUT row")
        row(0).toInt -> (row(1), Rgb(row(2).toInt, row(3).toInt, row(4).toInt))
      }.toMap
      require(colorById.keySet == shortById.keySet, "HCPex LUT IDs do not match short labels")
      Right((1 to 426).toVector.map: id =>
        val (hemiText, shortName) = shortById(id)
        val (full, color) = colorById(id)
        val hemisphere = hemiText match
          case "L" => Hemisphere.Left
          case "R" => Hemisphere.Right
          case _ => throw new IllegalArgumentException("HCPex hemisphere must be L or R")
        require(full.endsWith(s"_$hemiText"), "HCPex LUT hemisphere does not match short label")
        AtlasRegionMetadata.fromStrings(
          RegionId(id), s"${shortName}_$hemiText", labelFull = Some(full), hemisphere = Some(hemisphere),
          color = Some(color), attributes = Map("atlas" -> "HCPex", "division" -> (if id <= 360 then "cortical" else "subcortical")))
      )
    catch case NonFatal(error) => Left(AtlasAcquisitionError.Parse(Option(error.getMessage).getOrElse("invalid HCPex labels")))
