package scalafim.atlas

import scalafim.image.*
import image4s.Continuous
import locus4s.data.Field

object syntax:
  extension (atlas: VolumeAtlas)
    def expand(
        values: Field[atlas.realization.P, Double],
        background: Double
    ): Either[VolumeParcellationError, ScalarVolume[atlas.realization.parcellation.sampleSpace.type, Double]] =
      AtlasExpand.volume(atlas)(values, background)

    def queryEither(point: Point3D, radiusMm: Double = 0.0, fromSpace: SpaceId = atlas.ref.coordSpace): Either[AtlasError, Vector[QueryHit]] =
      AtlasQuery.queryEither(atlas, Vector(point), radiusMm, fromSpace)

    def query(point: Point3D, radiusMm: Double = 0.0, fromSpace: SpaceId = atlas.ref.coordSpace): Vector[QueryHit] =
      AtlasQuery.query(atlas, Vector(point), radiusMm, fromSpace)

    def queryEither(points: Vector[Point3D], radiusMm: Double, fromSpace: SpaceId): Either[AtlasError, Vector[QueryHit]] =
      AtlasQuery.queryEither(atlas, points, radiusMm, fromSpace)

    def query(points: Vector[Point3D], radiusMm: Double, fromSpace: SpaceId): Vector[QueryHit] =
      AtlasQuery.query(atlas, points, radiusMm, fromSpace)

    def reduceEither(
        data: SomeScalarVolume[Double],
        reducer: ParcelReducer = ParcelReducer.Mean,
        mask: Option[SomeMaskVolume] = None,
        policy: ParcelReductionPolicy = ParcelReductionPolicy()
    ): Either[AtlasError, Field[atlas.realization.P, Double]] =
      AtlasReduce.summarizeVolumeEither(atlas, data, reducer, mask, policy)

    def reduce(
        data: SomeScalarVolume[Double],
        reducer: ParcelReducer = ParcelReducer.Mean,
        mask: Option[SomeMaskVolume] = None,
        policy: ParcelReductionPolicy = ParcelReductionPolicy()
    ): Field[atlas.realization.P, Double] =
      AtlasReduce.summarizeVolume(atlas, data, reducer, mask, policy)

    def reduceSeriesEither(
        data: SomeScalarSeries[Double],
        reducer: ParcelReducer = ParcelReducer.Mean,
        mask: Option[SomeMaskVolume] = None,
        policy: ParcelReductionPolicy = ParcelReductionPolicy()
    ): Either[AtlasError, ParcelSeries[atlas.realization.F, atlas.realization.X, atlas.realization.P, Double, Continuous]] =
      AtlasReduce.reduceSeriesEither(atlas, data, mask, reducer, policy)

    def reduceSeries(
        data: SomeScalarSeries[Double],
        reducer: ParcelReducer = ParcelReducer.Mean,
        mask: Option[SomeMaskVolume] = None,
        policy: ParcelReductionPolicy = ParcelReductionPolicy()
    ): ParcelSeries[atlas.realization.F, atlas.realization.X, atlas.realization.P, Double, Continuous] =
      AtlasReduce.reduceSeries(atlas, data, mask, reducer, policy)

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
