package scalafim.atlas

import cats.kernel.CommutativeMonoid
import ravel.NDArray as RavelArray
import ravel.Shape
import scalafim.image.*
import locus4s.data.Aggregation
import spire.std.double.given

final case class ParcelValue(
    region: AtlasRegionMetadata,
    value: Double
)

final case class ParcelValues(atlas: VolumeAtlas, values: Vector[ParcelValue]):
  require(values.map(_.region.id).toSet == atlas.regions.ids.toSet, "parcel values must cover atlas regions exactly")

  def value(regionId: RegionId): Option[Double] =
    values.find(_.region.id == regionId).map(_.value)

object Reducers:
  val mean: Array[Double] => Double =
    values =>
      if values.length == 0 then Double.NaN
      else
        var i = 0
        var sum = 0.0
        var n = 0
        while i < values.length do
          val v = values(i)
          if !v.isNaN then
            sum += v
            n += 1
          i += 1
        if n == 0 then Double.NaN else sum / n.toDouble

  val sum: Array[Double] => Double =
    values =>
      var i = 0
      var sum = 0.0
      while i < values.length do
        val v = values(i)
        if !v.isNaN then sum += v
        i += 1
      sum

object AtlasReduce:
  def reduceVolumeEither(
    atlas: VolumeAtlas,
    data: NeuroVol[Double],
    reducer: Array[Double] => Double = Reducers.mean
  ): Either[AtlasError, ParcelValues] =
    if sameReducer(reducer, Reducers.mean) then
      onePassVolume(atlas, data, _.mean)
    else if sameReducer(reducer, Reducers.sum) then
      onePassVolume(atlas, data, _.sum)
    else
      requireExactVolumeGrid(atlas, data).map: _ =>
        val vals =
          atlas.regions.regions.map: region =>
            val idx = atlas.volume.clusterMap(region.id.value)
            val tmp = Array.ofDim[Double](idx.size)
            var i = 0
            while i < idx.size do
              tmp(i) = data.valueAtCanonicalOrdinal(idx(i))
              i += 1
            ParcelValue(region, reducer(tmp))
        ParcelValues(atlas, vals)

  def reduceVolume(
    atlas: VolumeAtlas,
    data: NeuroVol[Double],
    reducer: Array[Double] => Double = Reducers.mean
  ): ParcelValues =
    reduceVolumeEither(atlas, data, reducer).fold(err => throw new IllegalArgumentException(err.message), identity)

  def reduceVecEither(
    atlas: VolumeAtlas,
    data: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    reducer: Array[Double] => Double = Reducers.mean
  ): Either[AtlasError, ClusteredNeuroVec[Double]] =
    for
      _ <- requireExactSeriesGrid(atlas, data)
      _ <- mask.map(m => requireExactVolumeGrid(atlas, m)).getOrElse(Right(()))
    yield
      if sameReducer(reducer, Reducers.mean) || sameReducer(reducer, Reducers.sum) then
        onePassVec(atlas, data, mask, reducer)
      else
        val clusterIds = atlas.volume.clusterIds
        val tLen = data.nVolumes
        val indices = clusterIds.map: id =>
          val rawIdx = atlas.volume.clusterMap(id)
          mask match
            case None => rawIdx
            case Some(m) =>
              val kept = Array.newBuilder[Int]
              var p = 0
              while p < rawIdx.size do
                if m.valueAtCanonicalOrdinal(rawIdx(p)) then kept += rawIdx(p)
                p += 1
              val retained = kept.result()
              RavelArray.fromSeq(Shape(retained.length), retained)
        val scratch = indices.map(index => Array.ofDim[Double](index.size))
        val out =
          RavelArray.tabulate[Double](tLen, clusterIds.length) {
            (time, cluster) =>
              val index = indices(cluster)
              if index.size == 0 then Double.NaN
              else
                val tmp = scratch(cluster)
                var position = 0
                while position < index.size do
                  val voxel = data.space.indexToVoxel3D(index(position))
                  tmp(position) = data(voxel.x, voxel.y, voxel.z, time)
                  position += 1
                reducer(tmp)
          }

        ClusteredNeuroVec.fromMatrix(
          out,
          atlas.volume,
          data.label
        )

  def reduceVec(
    atlas: VolumeAtlas,
    data: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    reducer: Array[Double] => Double = Reducers.mean
  ): ClusteredNeuroVec[Double] =
    reduceVecEither(atlas, data, mask, reducer).fold(err => throw new IllegalArgumentException(err.message), identity)

  private def requireExactVolumeGrid[A](
      atlas: VolumeAtlas,
      data: NeuroVol[A]
  ): Either[AtlasError, Unit] =
    atlas.quotient.domain
      .spatialField(data.sampled)
      .map(_ => ())
      .left
      .map(_ => exactGridError(atlas, data.space))

  private def requireExactSeriesGrid[A](
      atlas: VolumeAtlas,
      data: NeuroVec[A]
  ): Either[AtlasError, Unit] =
    atlas.quotient.domain
      .seriesField(data.sampled)
      .map(_ => ())
      .left
      .map(_ => exactGridError(atlas, data.space))

  private def exactGridError(
      atlas: VolumeAtlas,
      actual: NeuroSpace
  ): AtlasError =
    if atlas.space.spatialDims != actual.spatialDims then
      AtlasError.SpaceMismatch(atlas.space.spatialDims, actual.spatialDims)
    else
      AtlasError.ExactGridRequired(
        atlas.space.spatialSpace.toString,
        actual.spatialSpace.toString
      )

  private final case class SumCount(sum: Double, count: Long):
    def mean: Double =
      if count == 0L then Double.NaN else sum / count.toDouble

  private object SumCount:
    val empty: SumCount =
      SumCount(0.0, 0L)

    def from(value: Double): SumCount =
      if value.isNaN then empty else SumCount(value, 1L)

    given CommutativeMonoid[SumCount] with
      def empty: SumCount =
        SumCount.empty

      def combine(left: SumCount, right: SumCount): SumCount =
        SumCount(left.sum + right.sum, left.count + right.count)

  private def onePassVolume(
      atlas: VolumeAtlas,
      data: NeuroVol[Double],
      finish: SumCount => Double
  ): Either[AtlasError, ParcelValues] =
    val quotient = atlas.quotient
    quotient.domain
      .spatialField(data.sampled)
      .left
      .map(_ => exactGridError(atlas, data.space))
      .map: field =>
        val summaries =
          Aggregation
            .foldMapBy(quotient.parcellation.quotientRelation, field)(SumCount.empty)(SumCount.from)(
              summon[CommutativeMonoid[SumCount]].combine
            )
        val values =
          quotient.displayOrder.indices.map: parcel =>
            ParcelValue(
              quotient.metadata(parcel),
              finish(summaries(parcel))
            )
          .toVector
        ParcelValues(atlas, values)

  private def onePassVec(
      atlas: VolumeAtlas,
      data: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]],
      reducer: Array[Double] => Double
  ): ClusteredNeuroVec[Double] =
    val quotient = atlas.quotient
    val parcelCount = quotient.parcellation.parcels.size
    val timeCount = data.nVolumes
    val spatialCount = atlas.space.spatialDims.product
    val sums = Array.fill(parcelCount * timeCount)(0.0)
    val counts = Array.fill(parcelCount * timeCount)(0L)

    var voxel = 0
    while voxel < spatialCount do
      val included = mask.forall(_.valueAtCanonicalOrdinal(voxel))
      if included then
        val coordinate = data.space.indexToVoxel3D(voxel)
        val point = quotient.parcellation.ambient.indexOption(voxel).get
        quotient.parcellation.parcelAt(point).foreach: parcel =>
          var time = 0
          while time < timeCount do
            val value = data(coordinate.x, coordinate.y, coordinate.z, time)
            if !value.isNaN then
              val index = parcel.value * timeCount + time
              sums(index) += value
              counts(index) += 1L
            time += 1
      voxel += 1

    val clusterIds = atlas.volume.clusterIds
    val parcelOrdinals =
      clusterIds.map { clusterId =>
        quotient
          .parcelPoint(RegionId(clusterId))
          .fold(
            throw new IllegalStateException(
              s"validated atlas quotient is missing cluster id $clusterId"
            )
          )(_.value)
      }
    val out =
      RavelArray.tabulate[Double](timeCount, clusterIds.length) {
        (time, column) =>
          val index = parcelOrdinals(column) * timeCount + time
          if counts(index) == 0L then
            Double.NaN
          else if sameReducer(reducer, Reducers.sum) then
            sums(index)
          else
            sums(index) / counts(index).toDouble
      }

    ClusteredNeuroVec.fromMatrix(
      out,
      atlas.volume,
      data.label
    )

  private def sameReducer(
      left: Array[Double] => Double,
      right: Array[Double] => Double
  ): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
