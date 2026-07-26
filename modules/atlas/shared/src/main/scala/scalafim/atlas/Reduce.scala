package scalafim.atlas

import narr.NArray
import cats.kernel.CommutativeMonoid
import scalafim.image.*
import scalafim.locus.Aggregation
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
  val mean: NArray[Double] => Double =
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

  val sum: NArray[Double] => Double =
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
    reducer: NArray[Double] => Double = Reducers.mean
  ): Either[AtlasError, ParcelValues] =
    requireSameSpatialEither(atlas, data.space).flatMap: _ =>
      if sameReducer(reducer, Reducers.mean) then
        onePassVolume(atlas, data, _.mean)
      else if sameReducer(reducer, Reducers.sum) then
        onePassVolume(atlas, data, _.sum)
      else
        val vals =
          atlas.regions.regions.map: region =>
            val idx = atlas.volume.clusterMap(region.id.value)
            val tmp = NArray.ofSize[Double](idx.length)
            var i = 0
            while i < idx.length do
              tmp(i) = data.linear(idx(i))
              i += 1
            ParcelValue(region, reducer(tmp))
        Right(ParcelValues(atlas, vals))

  def reduceVolume(
    atlas: VolumeAtlas,
    data: NeuroVol[Double],
    reducer: NArray[Double] => Double = Reducers.mean
  ): ParcelValues =
    reduceVolumeEither(atlas, data, reducer).fold(err => throw new IllegalArgumentException(err.message), identity)

  def reduceVecEither(
    atlas: VolumeAtlas,
    data: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    reducer: NArray[Double] => Double = Reducers.mean
  ): Either[AtlasError, ClusteredNeuroVec[Double]] =
    for
      _ <- requireSameSpatialEither(atlas, data.space)
      _ <- mask.map(m => requireSameSpatialEither(atlas, m.space)).getOrElse(Right(()))
    yield
      if sameReducer(reducer, Reducers.mean) || sameReducer(reducer, Reducers.sum) then
        onePassVec(atlas, data, mask, reducer)
      else
        val clusterIds = atlas.volume.clusterIds
        val tLen = data.nVolumes
        val out = NArray.ofSize[Double](tLen * clusterIds.length)
        var k = 0
        while k < clusterIds.length do
          val id = clusterIds(k)
          val rawIdx = atlas.volume.clusterMap(id)
          val idx =
            mask match
              case None => rawIdx
              case Some(m) =>
                val kept = Array.newBuilder[Int]
                var p = 0
                while p < rawIdx.length do
                  if m.linear(rawIdx(p)) then kept += rawIdx(p)
                  p += 1
                NArrayUtil.fromArray(kept.result())

          var t = 0
          while t < tLen do
            if idx.length == 0 then out(t + k * tLen) = Double.NaN
            else
              val tmp = NArray.ofSize[Double](idx.length)
              var p = 0
              while p < idx.length do
                tmp(p) = data.values.data(idx(p) + t * atlas.space.spatialDims.product)
                p += 1
              out(t + k * tLen) = reducer(tmp)
            t += 1
          k += 1

        ClusteredNeuroVec.fromMatrix(
          NDArray(out, Vector(tLen, clusterIds.length)),
          atlas.volume,
          data.label
        )

  def reduceVec(
    atlas: VolumeAtlas,
    data: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    reducer: NArray[Double] => Double = Reducers.mean
  ): ClusteredNeuroVec[Double] =
    reduceVecEither(atlas, data, mask, reducer).fold(err => throw new IllegalArgumentException(err.message), identity)

  private def requireSameSpatialEither(atlas: VolumeAtlas, space: NeuroSpace): Either[AtlasError, Unit] =
    GridCompatibility.spatial(atlas.space, space) match
      case Right(_) =>
        Right(())
      case Left(_) if atlas.space.spatialDims != space.spatialDims =>
        Left(AtlasError.SpaceMismatch(atlas.space.spatialDims, space.spatialDims))
      case Left(_) =>
        Left(
          AtlasError.ExactGridRequired(
            atlas.space.spatialSpace.toString,
            space.spatialSpace.toString
          )
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
    val field =
      quotient.domain.indexedField(data).toOption.get
    val summaries =
      Aggregation
        .foldMapBy(quotient.parcellation, field)(SumCount.from)
        .toOption
        .get
    val values =
      quotient.displayOrder.points.map: parcel =>
        ParcelValue(quotient.metadata(parcel), finish(summaries(parcel)))
      .toVector
    Right(ParcelValues(atlas, values))

  private def onePassVec(
      atlas: VolumeAtlas,
      data: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]],
      reducer: NArray[Double] => Double
  ): ClusteredNeuroVec[Double] =
    val quotient = atlas.quotient
    val parcelCount = quotient.parcellation.parcels.size
    val timeCount = data.nVolumes
    val spatialCount = atlas.space.spatialDims.product
    val sums = Array.fill(parcelCount * timeCount)(0.0)
    val counts = Array.fill(parcelCount * timeCount)(0L)

    var voxel = 0
    while voxel < spatialCount do
      val included = mask.forall(_.linear(voxel))
      if included then
        val point = quotient.parcellation.ambient.point(voxel).get
        quotient.parcellation.parcelAt(point).foreach: parcel =>
          var time = 0
          while time < timeCount do
            val value = data.values.data(voxel + time * spatialCount)
            if !value.isNaN then
              val index = parcel.ordinal * timeCount + time
              sums(index) += value
              counts(index) += 1L
            time += 1
      voxel += 1

    val clusterIds = atlas.volume.clusterIds
    val out = NArray.ofSize[Double](timeCount * clusterIds.length)
    var column = 0
    while column < clusterIds.length do
      val parcel =
        quotient.parcelPoint(RegionId(clusterIds(column))).get
      var time = 0
      while time < timeCount do
        val index = parcel.ordinal * timeCount + time
        if counts(index) == 0L then
          out(time + column * timeCount) = Double.NaN
        else if sameReducer(reducer, Reducers.sum) then
          out(time + column * timeCount) = sums(index)
        else
          out(time + column * timeCount) =
            sums(index) / counts(index).toDouble
        time += 1
      column += 1

    ClusteredNeuroVec.fromMatrix(
      NDArray(out, Vector(timeCount, clusterIds.length)),
      atlas.volume,
      data.label
    )

  private def sameReducer(
      left: NArray[Double] => Double,
      right: NArray[Double] => Double
  ): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
