package scalafim.atlas

import image4s.Axis
import image4s.AxisKind
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import neuroim4s.image.NeuroSeries
import image4s.SomeSampleSpace
import neuroim4s.image.NeuroVolume
import neuroim4s.image.Indexing
import neuroim4s.image.SomeLabelVolume
import neuroim4s.image.SomeMaskVolume
import neuroim4s.image.SomeNeuroSeries
import neuroim4s.image.SomeNeuroVolume
import neuroim4s.image.SomeScalarSeries
import neuroim4s.image.SomeScalarVolume
import neuroim4s.image.SampleSpaces
import neuroim4s.image.SampleSpaces.*

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
      .fold(error => throw new IllegalArgumentException(error.message), value => value)

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
      .fold(error => throw new IllegalArgumentException(error.message), value => value)

  def scalarSeries(
      atlas: VolumeAtlas,
      values: Array[Double],
      timeCount: Int,
      label: String = ""
  ): SomeScalarSeries[Double] =
    val time =
      Axis
        .create("time", timeCount, AxisKind.Time)
        .fold(error => throw new IllegalArgumentException(error.message), value => value)
    val axes =
      NonSpatialAxes
        .from(Vector(time))
        .fold(error => throw new IllegalArgumentException(error.message), value => value)
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
      .fold(error => throw new IllegalArgumentException(error.message), value => value)

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
      .fold(error => throw new IllegalArgumentException(error.message), value => value)
