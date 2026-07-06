package scalafim.atlas

import narr.NArray
import scalafim.image.*

object syntax:
  extension (atlas: VolumeAtlas)
    def query(point: Point3D, radiusMm: Double = 0.0, fromSpace: SpaceId = atlas.ref.coordSpace): Vector[QueryHit] =
      AtlasQuery.query(atlas, Vector(point), radiusMm, fromSpace)

    def query(points: Vector[Point3D], radiusMm: Double, fromSpace: SpaceId): Vector[QueryHit] =
      AtlasQuery.query(atlas, points, radiusMm, fromSpace)

    def reduce(data: NeuroVol[Double]): ParcelValues =
      AtlasReduce.reduceVolume(atlas, data, Reducers.mean)

    def reduce(data: NeuroVol[Double], reducer: NArray[Double] => Double): ParcelValues =
      AtlasReduce.reduceVolume(atlas, data, reducer)

    def reduce(data: NeuroVec[Double]): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, reducer = Reducers.mean)

    def reduce(data: NeuroVec[Double], reducer: NArray[Double] => Double): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, reducer = reducer)

    def reduce(
      data: NeuroVec[Double],
      mask: NeuroVol[Boolean],
      reducer: NArray[Double] => Double
    ): ClusteredNeuroVec[Double] =
      AtlasReduce.reduceVec(atlas, data, Some(mask), reducer)

    def overlap(other: VolumeAtlas, resample: Boolean = true): Vector[RegionOverlap] =
      AtlasOverlap.compute(atlas, other, resample)

    def adjacency(connectivity: VoxelConnectivity = VoxelConnectivity.Connect6): Vector[RegionEdge] =
      RegionGraph.adjacency(atlas, connectivity)
