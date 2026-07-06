package scalafim.examples.atlas

import scalafim.atlas.*

final case class AtlasDescriptorRow(
  id: String,
  family: String,
  model: String,
  representation: AtlasRepresentation,
  templateSpace: SpaceId,
  coordSpace: SpaceId,
  resolution: Option[String],
  density: Option[String],
  source: Option[String],
  confidence: Confidence
):
  def tabSeparated: String =
    Vector(
      id,
      family,
      model,
      representation.toString,
      templateSpace.value,
      coordSpace.value,
      resolution.getOrElse(""),
      density.getOrElse(""),
      source.getOrElse(""),
      confidence.toString
    ).mkString("\t")

object StandardAtlasDescriptions:
  def rows: Vector[AtlasDescriptorRow] =
    Vector(
      Schaefer2018.default.id -> Schaefer2018.default.atlasRef(),
      GlasserHcpMmp1().id -> GlasserHcpMmp1().atlasRef(),
      Brainnetome246.default.id -> Brainnetome246.default.atlasRef(),
      FreeSurferAseg.default.id -> FreeSurferAseg.default.atlasRef(),
      Schaefer2018Surface.default.id -> Schaefer2018Surface.default.atlasRef(),
      GlasserHcpMmp1Surface.default.id -> GlasserHcpMmp1Surface.default.atlasRef()
    ).map(fromRef)

  private def fromRef(idAndRef: (String, AtlasRef)): AtlasDescriptorRow =
    val (id, ref) = idAndRef
    AtlasDescriptorRow(
      id = id,
      family = ref.family,
      model = ref.model,
      representation = ref.representation,
      templateSpace = ref.templateSpace,
      coordSpace = ref.coordSpace,
      resolution = ref.resolution,
      density = ref.density,
      source = ref.source,
      confidence = ref.confidence
    )

@main def describeStandardAtlases(): Unit =
  println("id\tfamily\tmodel\trepresentation\ttemplateSpace\tcoordSpace\tresolution\tdensity\tsource\tconfidence")
  StandardAtlasDescriptions.rows.foreach(row => println(row.tabSeparated))
