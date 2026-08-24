package scalafim.atlas

import image4s.Axis
import image4s.AxisKind
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scalafim.image.NeuroSeries
import scalafim.image.SomeSampleSpace
import scalafim.image.NeuroVolume
import scalafim.image.Indexing
import scalafim.image.SomeLabelVolume
import scalafim.image.SomeMaskVolume
import scalafim.image.SomeNeuroSeries
import scalafim.image.SomeNeuroVolume
import scalafim.image.SomeScalarSeries
import scalafim.image.SomeScalarVolume
import scalafim.image.SampleSpaces
import scalafim.image.SampleSpaces.*

private[atlas] object AtlasTestImages:
  def labelVolume(
      space: SomeSampleSpace,
      values: Array[Int],
      label: String = ""
  ): SomeLabelVolume[Int] =
    val sampleSpace =
      SampleSpaces
        .requireVolumeD3(space)
        .fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )
    NeuroVolume
      .copyCategoricalFromCanonicalArray(
        sampleSpace,
        values,
        ImageMetadata.named(label)
      )
      .map(SomeNeuroVolume.eraseSpace)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def labelAtCanonicalOrdinal(
      volume: SomeLabelVolume[Int],
      ordinal: Int
  ): Int =
    val coordinate = Indexing.indexToGrid3D(volume.grid.shape, ordinal)
    volume.data(coordinate(0), coordinate(1), coordinate(2))

  def scalarVolume(
      atlas: VolumeAtlas,
      values: Array[Double],
      label: String = ""
  ): SomeScalarVolume[Double] =
    val sampleSpace =
      SampleSpace.create(
        atlas.realization.domain.grid,
        NonSpatialAxes.empty
      )
    NeuroVolume
      .copyContinuousFromCanonicalArray(
        sampleSpace,
        values,
        ImageMetadata.named(label)
      )
      .map(SomeNeuroVolume.eraseSpace)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def scalarSeries(
      atlas: VolumeAtlas,
      values: Array[Double],
      timeCount: Int,
      label: String = ""
  ): SomeScalarSeries[Double] =
    val time =
      Axis
        .create("time", timeCount, AxisKind.Time)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val axes =
      NonSpatialAxes
        .from(Vector(time))
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val sampleSpace =
      SampleSpace.create(atlas.realization.domain.grid, axes)
    val shape = atlas.realization.domain.grid.shape
    val data =
      NDArray.fromSeq(
        Shape(shape(0), shape(1), shape(2), timeCount),
        values
      )
    NeuroSeries
      .continuous(
        sampleSpace,
        data,
        ImageMetadata.named(label)
      )
      .map(SomeNeuroSeries.eraseSpace)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def maskVolume(
      atlas: VolumeAtlas,
      values: Array[Boolean],
      label: String = ""
  ): SomeMaskVolume =
    val sampleSpace =
      SampleSpace.create(
        atlas.realization.domain.grid,
        NonSpatialAxes.empty
      )
    NeuroVolume
      .copyMaskFromCanonicalArray(
        sampleSpace,
        values,
        ImageMetadata.named(label)
      )
      .map(SomeNeuroVolume.eraseSpace)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
