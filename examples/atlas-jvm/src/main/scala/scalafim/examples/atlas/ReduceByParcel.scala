package scalafim.examples.atlas

import scalafim.atlas.*

final case class ParcelMeanRow(regionId: RegionId, label: String, value: Double)

object ReduceByParcel:
  def toyParcelMeans(): Vector[ParcelMeanRow] =
    val atlas = AtlasExampleData.atlas()
    val values = AtlasReduce.reduceVolume(atlas, AtlasExampleData.statMap())
    values.values.map(v => ParcelMeanRow(v.region.id, v.region.label, v.value))

@main def reduceToyAtlas(): Unit =
  ReduceByParcel.toyParcelMeans().foreach { row =>
    println(s"${row.regionId.value}\t${row.label}\t${row.value}")
  }
