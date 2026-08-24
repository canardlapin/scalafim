package scalafim.image

import SampleSpaces.*

import image4s.ValueSemantics
import image4s.geometry.D3
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape

/** Image-semantic resampling that retains native image4s/Ravel values.
  *
  * The target volume contributes only its exact grid. The result has the
  * source value semantics and metadata, and owns one canonical Ravel array.
  */
object NativeResampling:
  def nearestLike[A, Sem, B, TargetSem](
      source: SomeNeuroVolume[A, Sem],
      target: SomeNeuroVolume[B, TargetSem],
      fill: A
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroVolume[A, Sem]] =
    val sourceSampled = SomeNeuroVolume.sampled(source)
    val targetSampled = SomeNeuroVolume.sampled(target)
    val sourceSpace = SampleSpaces.fromCanonical(sourceSampled.sampleSpace)
    val targetSampleSpace = targetSampled.sampleSpace
    val targetSpace = SampleSpaces.fromCanonical(targetSampleSpace)
    val targetShape = targetSampled.grid.shape
    val sourceShape = sourceSampled.grid.shape
    val data =
      NDArray.build[A, Rank[3]](
        Shape(targetShape(0), targetShape(1), targetShape(2))
      ): output =>
        var ordinal = 0
        while ordinal < targetShape.product do
          val targetVoxel = targetSpace.indexToVoxel3D(ordinal)
          val world =
            targetSpace.indexToCoord(
              Vector(
                targetVoxel.x.toDouble,
                targetVoxel.y.toDouble,
                targetVoxel.z.toDouble
              )
            )
          val sourceVoxel = sourceSpace.coordToIndex(world)
          val x = math.floor(sourceVoxel(0) + 0.5).toInt
          val y = math.floor(sourceVoxel(1) + 0.5).toInt
          val z = math.floor(sourceVoxel(2) + 0.5).toInt
          val value =
            if x >= 0 && x < sourceShape(0) &&
                y >= 0 && y < sourceShape(1) &&
                z >= 0 && z < sourceShape(2)
            then sourceSampled.data(x, y, z)
            else fill
          output.writeLinear(ordinal, value)
          ordinal += 1

    NeuroVolume
      .fromRavel[A, Sem](targetSampleSpace, data, sourceSampled.metadata)
      .left
      .map(NeuroImageError.Image.apply)
      .map(SomeNeuroVolume.eraseSpace)
