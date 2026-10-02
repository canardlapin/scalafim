package scalafim.examples.atlas

import scalafim.atlas.*

final case class RegionFeatureRow(regionId: Int, label: Option[String], nFeatures: Int)

object AtlasToMvpaRegionPlans:
  /** Atlas-realization rows are typed by the realization's exact support; no legacy feature plan is synthesized. */
  def toyRegionRows(): Vector[RegionFeatureRow] =
    val atlas = AtlasExampleData.atlas()
    atlas.regions.regions.map: metadata =>
      val support = atlas.realization.region(metadata.id).getOrElse(
        throw new IllegalStateException(s"atlas realization is missing ${metadata.id.value}")
      )
      RegionFeatureRow(metadata.id.value, Some(metadata.label), support.ordinalsInDomainOrder.length)

@main def atlasToMvpaRegions(): Unit =
  AtlasToMvpaRegionPlans.toyRegionRows().foreach: row =>
    println(s"${row.regionId}\t${row.label.getOrElse("")}\t${row.nFeatures}")
