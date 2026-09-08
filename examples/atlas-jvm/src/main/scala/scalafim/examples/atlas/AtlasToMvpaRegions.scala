package scalafim.examples.atlas

import scalafim.fmri.mvpa.*
import scalafim.image.VolumeDomain
import scalafim.image.VolumeSpace
import scalafim.locus.SpaceKey

final case class RegionFeatureRow(regionId: Int, label: Option[String], nFeatures: Int)

object AtlasToMvpaMeasurements:
  private val packedDomain =
    VolumeDomain.semantic(
      SpaceKey.unsafe("toy-atlas-mvpa-volume"),
      VolumeSpace(AtlasExampleData.space)
    )
  private type Voxel = packedDomain.S
  private val domain: VolumeDomain[Voxel] = packedDomain.value
  private val neural =
    SpatialAxes
      .volume(domain)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def toyRegionFrame =
    VolumeFrames
      .atlas(neural, domain, AtlasExampleData.atlas())
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def toyRegionRows(): Vector[RegionFeatureRow] =
    toyRegionFrame.entries.map: entry =>
      RegionFeatureRow(
        entry.rendition.region.id.value,
        Some(entry.rendition.region.label),
        entry.measurement.local.size
      )

@main def atlasToMvpaRegions(): Unit =
  AtlasToMvpaMeasurements.toyRegionRows().foreach { row =>
    println(s"${row.regionId}\t${row.label.getOrElse("")}\t${row.nFeatures}")
  }
