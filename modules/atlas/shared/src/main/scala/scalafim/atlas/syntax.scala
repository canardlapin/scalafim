package scalafim.atlas

import narr.NArray
import scalafim.image.*

object syntax:
  extension (atlas: VolumeAtlas)
    def queryEither(point: Point3D, radiusMm: Double = 0.0, fromSpace: AnySpaceId = atlas.ref.coordSpace): Either[AtlasError, Vector[QueryHit]] =
      AtlasQuery.queryEither(atlas, Vector(point), radiusMm, fromSpace)

    def query(point: Point3D, radiusMm: Double = 0.0, fromSpace: AnySpaceId = atlas.ref.coordSpace): Vector[QueryHit] =
      AtlasQuery.query(atlas, Vector(point), radiusMm, fromSpace)

    def queryEither(points: Vector[Point3D], radiusMm: Double, fromSpace: AnySpaceId): Either[AtlasError, Vector[QueryHit]] =
      AtlasQuery.queryEither(atlas, points, radiusMm, fromSpace)

    def query(points: Vector[Point3D], radiusMm: Double, fromSpace: AnySpaceId): Vector[QueryHit] =
      AtlasQuery.query(atlas, points, radiusMm, fromSpace)

    def reduceEither(data: NeuroVol[Double]): Either[AtlasError, ParcelValues] =
      AtlasReduce.reduceVolumeEither(atlas, data, Reducers.mean)

    def reduce(data: NeuroVol[Double]): ParcelValues =
      AtlasReduce.reduceVolume(atlas, data, Reducers.mean)

    def reduceEither(data: NeuroVol[Double], reducer: NArray[Double] => Double): Either[AtlasError, ParcelValues] =
      AtlasReduce.reduceVolumeEither(atlas, data, reducer)

    def reduce(data: NeuroVol[Double], reducer: NArray[Double] => Double): ParcelValues =
      AtlasReduce.reduceVolume(atlas, data, reducer)

    def reduceEither(data: NeuroVec[Double]): Either[AtlasError, ClusteredNeuroVec[Double]] =
      AtlasReduce.reduceVecEither(atlas, data, reducer = Reducers.mean)

    def reduce(data: NeuroVec[Double]): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, reducer = Reducers.mean)

    def reduceEither(data: NeuroVec[Double], reducer: NArray[Double] => Double): Either[AtlasError, ClusteredNeuroVec[Double]] =
      AtlasReduce.reduceVecEither(atlas, data, reducer = reducer)

    def reduce(data: NeuroVec[Double], reducer: NArray[Double] => Double): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, reducer = reducer)

    def reduceEither(
      data: NeuroVec[Double],
      mask: NeuroVol[Boolean],
      reducer: NArray[Double] => Double
    ): Either[AtlasError, ClusteredNeuroVec[Double]] =
      AtlasReduce.reduceVecEither(atlas, data, Some(mask), reducer)

    def reduce(
      data: NeuroVec[Double],
      mask: NeuroVol[Boolean],
      reducer: NArray[Double] => Double
    ): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, Some(mask), reducer)

    def overlapEither(other: VolumeAtlas): Either[AtlasError, Vector[RegionOverlap]] =
      AtlasOverlap.computeEither(atlas, other)

    def overlapEither(
      other: VolumeAtlas,
      alignment: AtlasAlignment
    ): Either[AtlasError, Vector[RegionOverlap]] =
      AtlasOverlap.computeEither(atlas, other, alignment)

    @deprecated(
      "Use overlapEither(other, AtlasAlignment); alignment must be explicit.",
      "0.2.0"
    )
    def overlapEither(
      other: VolumeAtlas,
      resample: Boolean
    ): Either[AtlasError, Vector[RegionOverlap]] =
      AtlasOverlap.computeEither(atlas, other, resample)

    def overlap(other: VolumeAtlas): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other)

    def overlap(
      other: VolumeAtlas,
      alignment: AtlasAlignment
    ): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other, alignment)

    @deprecated(
      "Use overlap(other, AtlasAlignment); alignment must be explicit.",
      "0.2.0"
    )
    def overlap(
      other: VolumeAtlas,
      resample: Boolean
    ): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other, resample)

    def adjacency(connectivity: VoxelConnectivity = VoxelConnectivity.Connect6): Vector[RegionEdge] =
      RegionGraph.adjacency(atlas, connectivity)
