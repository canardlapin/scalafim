package scalafim.atlas

import narr.NArray
import scalafim.image.*
import spire.std.double.given

final case class ParcelValue(region: Region, value: Double)

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
    requireSameSpatialEither(atlas, data.space).map { _ =>
      val vals =
        atlas.regions.regions.map { region =>
          val idx = atlas.volume.clusterMap(region.id.value)
          val tmp = NArray.ofSize[Double](idx.length)
          var i = 0
          while i < idx.length do
            tmp(i) = data.linear(idx(i))
            i += 1
          ParcelValue(region, reducer(tmp))
        }
      ParcelValues(atlas, vals)
    }

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

      ClusteredNeuroVec.fromMatrix(NDArray(out, Vector(tLen, clusterIds.length)), atlas.volume, data.label)

  def reduceVec(
    atlas: VolumeAtlas,
    data: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    reducer: NArray[Double] => Double = Reducers.mean
  ): ClusteredNeuroVec[Double] =
    reduceVecEither(atlas, data, mask, reducer).fold(err => throw new IllegalArgumentException(err.message), identity)

  private def requireSameSpatialEither(atlas: VolumeAtlas, space: NeuroSpace): Either[AtlasError, Unit] =
    if atlas.space.spatialDims == space.spatialDims then Right(())
    else Left(AtlasError.SpaceMismatch(atlas.space.spatialDims, space.spatialDims))
