package scalafim.image

import image4s.Categorical
import image4s.Continuous
import image4s.ImageMetadata
import image4s.Mask as MaskSemantics
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.geometry.D3
import ravel.DType
import ravel.NDArray

/** Temporary, package-internal migration boundary for historical producers.
  *
  * Every method names the source layout and copies exactly once into the final
  * canonical Ravel owner. New code must use the canonical-array or Ravel
  * constructors on NeuroVolume and NeuroSeries. This object is deleted when
  * the algorithm and I/O migration no longer produces first-axis-fastest data.
  */
private[scalafim] object NativeImageIngress:
  def copyContinuousVolumeFromFirstAxisFastest[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[NativeImageError, SomeScalarVolume[A]] =
    copyVolume[A, Continuous](data, space, label)

  def copyCategoricalVolumeFromFirstAxisFastest[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Categorical]
  ): Either[NativeImageError, SomeLabelVolume[A]] =
    copyVolume[A, Categorical](data, space, label)

  def copyMaskVolumeFromFirstAxisFastest(
      data: Array[Boolean],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[NativeImageError, SomeMaskVolume] =
    copyVolume[Boolean, MaskSemantics](data, space, label)

  def copyContinuousSeriesFromFirstAxisFastest[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[NativeImageError, SomeScalarSeries[A]] =
    copySeries[A, Continuous](data, space, label)

  def copyCategoricalSeriesFromFirstAxisFastest[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Categorical]
  ): Either[NativeImageError, SomeLabelSeries[A]] =
    copySeries[A, Categorical](data, space, label)

  def copyMaskSeriesFromFirstAxisFastest(
      data: Array[Boolean],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[NativeImageError, SomeMaskSeries] =
    copySeries[Boolean, MaskSemantics](data, space, label)

  private def copyVolume[A, Sem](
      data: Array[A],
      dynamicSpace: NeuroSpace,
      label: String
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[NativeImageError, SomeNeuroVolume[A, Sem]] =
    for
      space <- NeuroSpace
        .requireSpatialD3(dynamicSpace)
        .left
        .map(NativeImageError.Space.apply)
      shape = space.grid.shape
      expected = shape.product
      _ <-
        Either.cond(
          data.length == expected,
          (),
          NativeImageError.FirstAxisFastestSizeMismatch(
            expected,
            data.length
          )
        )
      nx = shape(0)
      ny = shape(1)
      canonical =
        NDArray.tabulate[A](nx, ny, shape(2)): (x, y, z) =>
          data(x + nx * (y + ny * z))
      volume <- NeuroVolume
        .fromRavel[A, Sem](
          space,
          canonical,
          ImageMetadata.named(label)
        )
        .left
        .map(NativeImageError.Image.apply)
    yield SomeNeuroVolume.eraseSpace(volume)

  private def copySeries[A, Sem](
      data: Array[A],
      dynamicSpace: NeuroSpace,
      label: String
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
    for
      space <- NeuroSpace
        .requireD3(dynamicSpace)
        .left
        .map(NativeImageError.Space.apply)
      axes = space.nonSpatialAxes.values
      _ <-
        Either.cond(
          axes.size == 1 && axes.head.kind == image4s.AxisKind.Time,
          (),
          NativeImageError.ExpectedSingleTimeAxis(axes.map(_.kind))
        )
      shape = space.logicalShape
      expected = shape.product
      _ <-
        Either.cond(
          data.length == expected,
          (),
          NativeImageError.FirstAxisFastestSizeMismatch(
            expected,
            data.length
          )
        )
      nx = shape(0)
      ny = shape(1)
      nz = shape(2)
      canonical =
        NDArray.tabulate[A](nx, ny, nz, shape(3)): (x, y, z, time) =>
          data(x + nx * (y + ny * (z + nz * time)))
      series <- NeuroSeries.fromRavel[A, Sem](
        space,
        canonical,
        ImageMetadata.named(label)
      )
    yield SomeNeuroSeries.eraseSpace(series)
