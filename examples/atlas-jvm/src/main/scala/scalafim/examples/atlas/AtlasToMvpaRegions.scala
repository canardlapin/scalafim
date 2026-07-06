package scalafim.examples.atlas

import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.spatial.SpatialFeatureSetPlans

final case class RegionFeatureRow(regionId: Int, label: Option[String], nFeatures: Int)

object AtlasToMvpaRegionPlans:
  def toyRegionPlan(): FeatureSetPlan =
    SpatialFeatureSetPlans
      .fromVolumeAtlas("toy-atlas-regions", AtlasExampleData.atlas())
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def toyRegionRows(): Vector[RegionFeatureRow] =
    toyRegionPlan().featureSets.map { featureSet =>
      RegionFeatureRow(featureSet.id.value, featureSet.label, featureSet.size)
    }

@main def atlasToMvpaRegions(): Unit =
  AtlasToMvpaRegionPlans.toyRegionRows().foreach { row =>
    println(s"${row.regionId}\t${row.label.getOrElse("")}\t${row.nFeatures}")
  }
