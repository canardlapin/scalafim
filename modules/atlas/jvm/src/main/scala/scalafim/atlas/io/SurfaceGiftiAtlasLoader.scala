package scalafim.atlas.io

import java.nio.file.Path
import scala.util.control.NonFatal
import scalafim.atlas.*
import scalafim.surface.{Hemisphere as SurfaceHemisphere, LabelInfo, LabeledSurface, SurfaceGeometry}
import scalafim.surface.gifti.*
import scalafim.surface.io.{GiftiReader, GiftiSurfaceReader}

object SurfaceGiftiAtlasLoader:
  final case class Paths(left: Path, right: Path)

  final case class Geometry(left: SurfaceGeometry, right: SurfaceGeometry):
    require(left.hemisphere == SurfaceHemisphere.Left, "left GIFTI atlas geometry must use left hemisphere")
    require(right.hemisphere == SurfaceHemisphere.Right, "right GIFTI atlas geometry must use right hemisphere")

  final case class Options(backgroundLabel: Int = 0, label: String = ""):
    require(backgroundLabel >= 0, "GIFTI atlas background label must be non-negative")

  def loadFromPaths(
    ref: SurfaceAtlasRef,
    geometry: Geometry,
    paths: Paths,
    options: Options = Options()
  ): SurfaceAtlas =
    loadFromPathsEither(ref, geometry, paths, options).fold(error => throw new IllegalArgumentException(error.message), identity)

  def loadFromPathsEither(
    ref: SurfaceAtlasRef,
    geometry: Geometry,
    paths: Paths,
    options: Options = Options()
  ): Either[AtlasError, SurfaceAtlas] =
    for
      leftDoc <- GiftiReader.read(paths.left).left.map(toAtlasError)
      rightDoc <- GiftiReader.read(paths.right).left.map(toAtlasError)
      leftLabels <- GiftiSurfaceReader.labeledSurface(leftDoc, geometry.left, options.label).left.map(toAtlasError)
      rightLabels <- GiftiSurfaceReader.labeledSurface(rightDoc, geometry.right, options.label).left.map(toAtlasError)
      regions <- regionsFromGifti(leftDoc, rightDoc, leftLabels, rightLabels, options.backgroundLabel)
      atlas <- buildAtlas(ref, regions, leftLabels, rightLabels, leftDoc, rightDoc, paths, options)
    yield atlas

  private final case class LabelDef(id: RegionId, name: String, color: Option[Rgb])

  private def regionsFromGifti(
    leftDoc: GiftiDocument,
    rightDoc: GiftiDocument,
    leftLabels: LabeledSurface,
    rightLabels: LabeledSurface,
    background: Int
  ): Either[AtlasError, RegionIndex] =
    for
      _ <- rejectNegativeLabels(leftLabels, rightLabels)
      present = presentIds(leftLabels, background) ++ presentIds(rightLabels, background)
      _ <- requirePositivePresentLabels(present)
      table <- labelDefinitions(leftDoc, rightDoc, background)
      _ <- requireKnownLabels(present, table.keySet)
      regions <- buildRegionIndex(present, table, leftLabels, rightLabels, leftDoc, rightDoc, background)
    yield regions

  private def rejectNegativeLabels(left: LabeledSurface, right: LabeledSurface): Either[AtlasError, Unit] =
    val negative = (labelValues(left) ++ labelValues(right)).filter(_ < 0).distinct.sorted
    if negative.isEmpty then Right(())
    else Left(AtlasError.InvalidRegionMetadata(s"GIFTI surface atlas labels must be non-negative: ${negative.mkString(",")}"))

  private def requirePositivePresentLabels(present: Set[Int]): Either[AtlasError, Unit] =
    val nonPositive = present.filter(_ <= 0).toVector.sorted
    if nonPositive.isEmpty then Right(())
    else Left(AtlasError.InvalidRegionMetadata(s"GIFTI non-background label ids must be positive: ${nonPositive.mkString(",")}"))

  private def labelDefinitions(leftDoc: GiftiDocument, rightDoc: GiftiDocument, background: Int): Either[AtlasError, Map[Int, LabelDef]] =
    val entries =
      (leftDoc.labelTable ++ rightDoc.labelTable)
        .filter(label => label.key != background && label.key > 0)
        .groupBy(_.key)
    val conflicts =
      entries.collect {
        case (id, labels) if labels.map(label => label.name -> label.colorHex).distinct.length > 1 => id
      }.toVector.sorted
    if conflicts.nonEmpty then
      Left(AtlasError.InvalidRegionMetadata(s"GIFTI label tables disagree for ids: ${conflicts.mkString(",")}"))
    else
      Right(
        entries.map { case (id, labels) =>
          val label = labels.head
          id -> LabelDef(RegionId(id), label.name, rgb(label))
        }
      )

  private def requireKnownLabels(present: Set[Int], tableIds: Set[Int]): Either[AtlasError, Unit] =
    val missing = present.diff(tableIds).toVector.sorted
    if missing.isEmpty then Right(())
    else Left(AtlasError.InvalidRegionMetadata(s"GIFTI label payload contains ids not present in LabelTable: ${missing.mkString(",")}"))

  private def buildRegionIndex(
    present: Set[Int],
    table: Map[Int, LabelDef],
    leftLabels: LabeledSurface,
    rightLabels: LabeledSurface,
    leftDoc: GiftiDocument,
    rightDoc: GiftiDocument,
    background: Int
  ): Either[AtlasError, RegionIndex] =
    val leftPresent = presentIds(leftLabels, background)
    val rightPresent = presentIds(rightLabels, background)
    val regions =
      present.toVector.sorted.map { id =>
        val info = table(id)
        Region(
          id = info.id,
          label = info.name,
          labelFull = Some(info.name),
          hemisphere = atlasHemisphere(id, leftPresent, rightPresent),
          color = info.color,
          attributes = regionAttributes(id, leftDoc, rightDoc)
        )
      }
    try Right(RegionIndex(regions))
    catch case NonFatal(error) => Left(AtlasError.InvalidRegionMetadata(cleanRequirement(error.getMessage)))

  private def atlasHemisphere(id: Int, leftPresent: Set[Int], rightPresent: Set[Int]): Option[Hemisphere] =
    (leftPresent.contains(id), rightPresent.contains(id)) match
      case (true, false) => Some(Hemisphere.Left)
      case (false, true) => Some(Hemisphere.Right)
      case (true, true) => Some(Hemisphere.Bilateral)
      case _ => None

  private def regionAttributes(id: Int, leftDoc: GiftiDocument, rightDoc: GiftiDocument): Map[String, String] =
    val source =
      (leftDoc.metadata.get("Name"), rightDoc.metadata.get("Name")) match
        case (Some(left), Some(right)) if left == right => Some(left)
        case (Some(left), Some(right)) => Some(s"$left;$right")
        case (Some(left), None) => Some(left)
        case (None, Some(right)) => Some(right)
        case _ => None
    Map("gifti_label_key" -> id.toString) ++ source.map("gifti_name" -> _)

  private def buildAtlas(
    ref: SurfaceAtlasRef,
    regions: RegionIndex,
    leftLabels: LabeledSurface,
    rightLabels: LabeledSurface,
    leftDoc: GiftiDocument,
    rightDoc: GiftiDocument,
    paths: Paths,
    options: Options
  ): Either[AtlasError, SurfaceAtlas] =
    val refWithFiles = withLocalArtifacts(ref, paths)
    val declaredIds =
      (leftDoc.labelTable ++ rightDoc.labelTable)
        .map(_.key)
        .filter(_ != options.backgroundLabel)
        .distinct
        .sorted
        .map(RegionId(_))
    val provenance =
      resolveLocalArtifacts(
        AtlasProvenance.loaded(refWithFiles, regions, declaredIds)
          .withLabels(LabelSchema.fromRegions(refWithFiles, regions).copy(background = Some(options.backgroundLabel)))
          .withDerivationStep(
            DerivationStep.ParsedLabels(
              artifactId = sourceArtifactId(ArtifactRole.SurfaceAnnotation, sourceRef("left", paths.left)),
              schema = LabelTableSchema("GIFTI LabelTable", Vector("Key", "Label", "Red", "Green", "Blue", "Alpha"))
            )
          ),
        paths
      )
    try Right(SurfaceAtlas.fromLabeledSurfaces(refWithFiles, regions, leftLabels, rightLabels, provenance))
    catch case NonFatal(error) => Left(AtlasError.InvalidRegionMetadata(cleanRequirement(error.getMessage)))

  private def withLocalArtifacts(ref: SurfaceAtlasRef, paths: Paths): SurfaceAtlasRef =
    ref.copy(artifacts = ref.artifacts ++ Vector(localArtifact("left", paths.left), localArtifact("right", paths.right)))

  private def localArtifact(side: String, path: Path): AtlasArtifact =
    AtlasArtifact(
      role = ArtifactRole.SurfaceAnnotation,
      sourceName = s"GIFTI surface labels ($side)",
      sourceRef = sourceRef(side, path),
      notes = Some("Local GIFTI label payload loaded explicitly; no download performed.")
    )

  private def resolveLocalArtifacts(provenance: AtlasProvenance, paths: Paths): AtlasProvenance =
    provenance.mapSourceArtifacts { artifact =>
      artifact.sourceRef match
        case ref if ref == sourceRef("left", paths.left) => artifact.withResolvedFile(paths.left.toString, AtlasAsset.digest(paths.left))
        case ref if ref == sourceRef("right", paths.right) => artifact.withResolvedFile(paths.right.toString, AtlasAsset.digest(paths.right))
        case _ => artifact
    }

  private def sourceRef(side: String, path: Path): String =
    s"$side:${path.getFileName.toString}"

  private def sourceArtifactId(role: ArtifactRole, sourceRef: String): String =
    val raw = s"${role.legacy}:$sourceRef"
    val normalized = raw.trim.toLowerCase.replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_")
    if normalized.nonEmpty then normalized else "artifact"

  private def presentIds(surface: LabeledSurface, background: Int): Set[Int] =
    labelValues(surface).filter(_ != background).toSet

  private def labelValues(surface: LabeledSurface): Vector[Int] =
    Vector.tabulate(surface.labels.length)(surface.labels(_))

  private def rgb(label: GiftiLabel): Option[Rgb] =
    for
      red <- label.red
      green <- label.green
      blue <- label.blue
    yield Rgb(toByte(red), toByte(green), toByte(blue))

  private def toByte(value: Double): Int =
    math.max(0, math.min(255, math.round(value * 255.0).toInt))

  private def toAtlasError(error: GiftiError): AtlasError =
    AtlasError.InvalidRegionMetadata(error.message)

  private def cleanRequirement(message: String): String =
    Option(message)
      .getOrElse("requirement failed")
      .stripPrefix("requirement failed: ")
