package scalafim.atlas

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

    def reduceEither(
        data: SomeScalarVolume[Double]
    ): Either[AtlasError, ParcelValues] =
      AtlasReduce.summarizeVolumeEither(atlas, data, Reducers.mean)

    def reduce(data: SomeScalarVolume[Double]): ParcelValues =
      AtlasReduce.summarizeVolume(atlas, data, Reducers.mean)

    def reduceEither(
        data: SomeScalarVolume[Double],
        reducer: Array[Double] => Double
    ): Either[AtlasError, ParcelValues] =
      AtlasReduce.summarizeVolumeEither(atlas, data, reducer)

    def reduce(
        data: SomeScalarVolume[Double],
        reducer: Array[Double] => Double
    ): ParcelValues =
      AtlasReduce.summarizeVolume(atlas, data, reducer)

    @scala.annotation.targetName("reduceNeuroSeriesEither")
    def reduceEither(
        data: SomeScalarSeries[Double]
    ): Either[AtlasError, SomeScalarParcelSeries[Double]] =
      AtlasReduce.reduceSeriesEither(atlas, data, reducer = Reducers.mean)

    @scala.annotation.targetName("reduceNeuroSeries")
    def reduce(
        data: SomeScalarSeries[Double]
    ): SomeScalarParcelSeries[Double] =
      AtlasReduce.reduceSeries(atlas, data, reducer = Reducers.mean)

    @scala.annotation.targetName("reduceNeuroSeriesWithEither")
    def reduceEither(
        data: SomeScalarSeries[Double],
        reducer: Array[Double] => Double
    ): Either[AtlasError, SomeScalarParcelSeries[Double]] =
      AtlasReduce.reduceSeriesEither(atlas, data, reducer = reducer)

    @scala.annotation.targetName("reduceNeuroSeriesWith")
    def reduce(
        data: SomeScalarSeries[Double],
        reducer: Array[Double] => Double
    ): SomeScalarParcelSeries[Double] =
      AtlasReduce.reduceSeries(atlas, data, reducer = reducer)

    def reduceEither(
        data: SomeScalarSeries[Double],
        mask: SomeMaskVolume,
        reducer: Array[Double] => Double
    ): Either[AtlasError, SomeScalarParcelSeries[Double]] =
      AtlasReduce.reduceSeriesEither(atlas, data, Some(mask), reducer)

    def reduce(
        data: SomeScalarSeries[Double],
        mask: SomeMaskVolume,
        reducer: Array[Double] => Double
    ): SomeScalarParcelSeries[Double] =
      AtlasReduce.reduceSeries(atlas, data, Some(mask), reducer)

    def overlapEither(other: VolumeAtlas): Either[AtlasError, Vector[RegionOverlap]] =
      AtlasOverlap.computeEither(atlas, other)

    def overlapEither(
      other: VolumeAtlas,
      alignment: AtlasAlignment
    ): Either[AtlasError, Vector[RegionOverlap]] =
      AtlasOverlap.computeEither(atlas, other, alignment)

    def overlap(other: VolumeAtlas): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other)

    def overlap(
      other: VolumeAtlas,
      alignment: AtlasAlignment
    ): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other, alignment)

    def adjacency(connectivity: VoxelConnectivity = VoxelConnectivity.Connect6): Vector[RegionEdge] =
      RegionGraph.adjacency(atlas, connectivity)
