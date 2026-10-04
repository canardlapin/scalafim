package scalafim.examples.atlas

import scalafim.atlas.*
import scalafim.image.NeuroVolume

final case class ParcelMeanRow(regionId: RegionId, label: String, value: Double)

object ReduceByParcel:
  def toyMetricRoundTrip(): Vector[Double] =
    val atlas = AtlasExampleData.atlas()
    val r: atlas.realization.type = atlas.realization
    val values = AtlasReduce.summarizeVolume(atlas, AtlasExampleData.statMap())
    val restored = for
      schema <- ParcelMetricSchema.from("parcel mean", Some("signal"))
      document <- ParcelMetricJson.encode(r)(values, schema)
      metric <- ParcelMetricJson.decode(r)(document)
    yield metric
    val metric = restored.fold(error => throw new IllegalArgumentException(error.message), identity)
    AtlasExpand.volume(atlas)(metric.values, Double.NaN)
      .fold(error => throw new IllegalArgumentException(error.message), volume => NeuroVolume.sampled(volume).data.iterator.toVector)

  def toyParcelMeans(): Vector[ParcelMeanRow] =
    val atlas = AtlasExampleData.atlas()
    val values = AtlasReduce.summarizeVolume(atlas, AtlasExampleData.statMap())
    ParcelFields.records(atlas.realization)(values)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .map(v => ParcelMeanRow(v.region.id, v.region.label.value, v.value))

@main def reduceToyAtlas(): Unit =
  ReduceByParcel.toyParcelMeans().foreach { row =>
    println(s"${row.regionId.value}\t${row.label}\t${row.value}")
  }
