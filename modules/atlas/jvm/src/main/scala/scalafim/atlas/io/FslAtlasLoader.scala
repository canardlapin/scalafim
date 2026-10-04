package scalafim.atlas.io

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path}
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Element
import scala.util.control.NonFatal
import scalafim.atlas.*

enum FslAtlasType:
  case Probabilistic, Label

final case class FslAtlasXml(name: String, shortName: Option[String], kind: FslAtlasType,
    summaries: Vector[String], regions: Vector[AtlasRegionMetadata])

object FslAtlasLoader:
  /** Secure XML admission. Probabilistic XML indices are zero-based channel
    * positions; the hard summary uses index+1. Label atlases use literal ids.
    * XML image paths are metadata only and are never resolved as local paths.
    */
  def parse(bytes: Array[Byte]): Either[AtlasAcquisitionError, FslAtlasXml] =
    try
      val factory = DocumentBuilderFactory.newInstance()
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
      factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
      factory.setXIncludeAware(false)
      factory.setExpandEntityReferences(false)
      val builder = factory.newDocumentBuilder()
      builder.setErrorHandler(new org.xml.sax.helpers.DefaultHandler:
        override def error(e: org.xml.sax.SAXParseException): Unit = throw e
        override def fatalError(e: org.xml.sax.SAXParseException): Unit = throw e
      )
      val doc = builder.parse(new ByteArrayInputStream(bytes))
      require(doc.getDocumentElement.getTagName == "atlas", "expected atlas root")
      def text(name: String): String =
        val elements = doc.getElementsByTagName(name)
        require(elements.getLength == 1, s"expected exactly one $name")
        val value = elements.item(0).getTextContent.trim
        require(value.nonEmpty, s"empty $name")
        value
      val kind = text("type").toLowerCase match
        case "probabilistic" => FslAtlasType.Probabilistic
        case "label" => FslAtlasType.Label
        case other => throw new IllegalArgumentException(s"unsupported FSL atlas type $other")
      val labels = doc.getElementsByTagName("label")
      val rows = Vector.tabulate(labels.getLength): i =>
        val label = labels.item(i).asInstanceOf[Element]
        val index = label.getAttribute("index").toInt
        require(index >= 0 && (kind != FslAtlasType.Probabilistic || index < Int.MaxValue), "invalid label index")
        val id = if kind == FslAtlasType.Probabilistic then index + 1 else index
        val name = label.getTextContent.trim
        require(name.nonEmpty, "empty FSL label")
        (id, name, index)
      require(rows.map(_._1).distinct.length == rows.length, "duplicate FSL label index")
      val regions = rows.filter(_._1 != 0).map: (id, name, index) =>
        AtlasRegionMetadata.fromStrings(RegionId(id), name, Some(name), hemisphere = hemisphere(name),
          attributes = Map("fsl_xml_index" -> index.toString, "fsl_atlas_type" -> kind.toString))
      require(regions.nonEmpty, "FSL XML has no positive parcel labels")
      val summaries = doc.getElementsByTagName("summaryimagefile")
      val paths = Vector.tabulate(summaries.getLength)(i => summaries.item(i).getTextContent.trim)
      require(paths.nonEmpty && paths.forall(_.nonEmpty), "missing FSL summary image")
      val shortNames = doc.getElementsByTagName("shortname")
      require(shortNames.getLength <= 1, "expected at most one shortname")
      val shortName = Option.when(shortNames.getLength == 1)(text("shortname"))
      Right(FslAtlasXml(text("name"), shortName, kind, paths, regions))
    catch case NonFatal(error) => Left(AtlasAcquisitionError.Parse(Option(error.getMessage).getOrElse("invalid XML")))

  def parse(text: String): Either[AtlasAcquisitionError, FslAtlasXml] = parse(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  def load(request: FslAtlasRequest, store: AtlasStore = FileAtlasStore.default,
      policy: AssetPolicy = AssetPolicy.CacheOrDownload): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      xml <- PinnedVolumeLoader.resolve(request.xml, store, policy)
      volume <- PinnedVolumeLoader.resolve(request.volume, store, policy)
      atlas <- loadFromPaths(request, xml, volume)
    yield atlas

  def loadFromPaths(request: FslAtlasRequest, xml: Path, volume: Path): Either[AtlasAcquisitionError, VolumeAtlas] =
    for
      _ <- PinnedVolumeLoader.verify(request.xml, xml)
      bytes <- PinnedVolumeLoader.attempt(Files.readAllBytes(xml))
      metadata <- parse(bytes)
      _ <- Either.cond(metadata.summaries.exists(path => request.family.imagePath.stripSuffix(".nii.gz").endsWith(path.stripPrefix("/"))),
        (), AtlasAcquisitionError.Parse("pinned summary image is absent from XML"))
      atlas <- PinnedVolumeLoader.load(request.ref, metadata.regions, volume,
        Vector((request.xml, ArtifactRole.LabelTable, xml), (request.volume, ArtifactRole.ParcellationVolume, volume)))
    yield atlas

  private def hemisphere(name: String): Option[Hemisphere] =
    val n = name.toLowerCase
    if n.startsWith("left ") || n.endsWith(" l") then Some(Hemisphere.Left)
    else if n.startsWith("right ") || n.endsWith(" r") then Some(Hemisphere.Right)
    else None
