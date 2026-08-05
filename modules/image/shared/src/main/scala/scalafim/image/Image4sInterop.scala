package scalafim.image

import image4s.ImageError
import image4s.ImageMetadata
import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.Continuous
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.SomeSampleSpace
import image4s.ValueSemantics
import ravel.DType
import ravel.DType.given
import ravel.NDArray as RavelArray
import ravel.Rank
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

enum Image4sInteropError derives CanEqual:
  case Geometry(error: GeometryError)
  case Image(error: ImageError)

  def message: String =
    this match
      case Geometry(error) => error.message
      case Image(error)    => error.message

enum Image4sStorageTransfer derives CanEqual:
  case CanonicalizedLegacy

sealed trait ScalaFimValues

object ScalaFimValues:
  given [A]: ValueSemantics[A, ScalaFimValues] with {}

final case class ImportedScalarVolume(
    sampled: Sampled[
      ? <: SampleSpace[?, ?],
      Double,
      Continuous,
      Rank[3]
    ],
    transfer: Image4sStorageTransfer
)

final case class DenseImageImport[+I](
    image: I,
    transfer: Image4sStorageTransfer
)

/** Checked, explicitly materializing admission from the legacy image stack.
  *
  * ScalaFIM's current dense buffers are first-axis-fastest while canonical
  * Ravel storage is last-axis-fastest. This boundary therefore copies logical
  * `(i,j,k)` values exactly once; it never adopts the legacy flat buffer.
  */
object Image4sInterop:
  private[image] type PackedSlice[A] =
    Sampled[
      ? <: SampleSpace[?, ?],
      A,
      ScalaFimValues,
      Rank[2]
    ]

  private[image] type PackedVolume[A] =
    Sampled[
      ? <: SampleSpace[?, ?],
      A,
      ScalaFimValues,
      Rank[3]
    ]

  private[image] type PackedSeries[A] =
    Sampled[
      ? <: SampleSpace[?, ?],
      A,
      ScalaFimValues,
      Rank[4]
    ]

  private[image] type PackedComponents =
    Sampled[
      ? <: SampleSpace[?, ?],
      Double,
      Continuous,
      Rank[4]
    ]

  def canonicalizeScalarVolume(
      volume: NeuroVol[Double]
  ): Either[Image4sInteropError, ImportedScalarVolume] =
    for
      space <- canonicalD3(volume.space)
        .left
        .map(_ =>
          Image4sInteropError.Image(
            ImageError.StorageRankMismatch(
              3,
              volume.space.spatialDims.length
            )
          )
        )
      sampled <- Sampled
        .continuous[Double, Rank[3]](space.typed, volume.values)
        .left
        .map(Image4sInteropError.Image.apply)
    yield ImportedScalarVolume(
      sampled,
      Image4sStorageTransfer.CanonicalizedLegacy
    )

  private[image] def volumeFromLegacyLinear[A](
      values: Array[A],
      space: NeuroSpace,
      label: String
  )(using DType[A]): Either[NeuroImageError, PackedVolume[A]] =
    val shape = space.spatialDims
    val expected = shape.product
    if values.length != expected then
      Left(
        NeuroImageError.LinearSizeMismatch(
          "NeuroVol",
          expected,
          values.length
        )
      )
    else
      val nx = shape(0)
      val ny = shape(1)
      val data =
        RavelArray.tabulate[A](nx, ny, shape(2)) { (i, j, k) =>
          values(i + nx * (j + ny * k))
        }
      volumeFromRavel(data, space, label)

  private[image] def seriesFromLegacyLinear[A](
      values: Array[A],
      space: NeuroSpace,
      label: String
  )(using DType[A]): Either[NeuroImageError, PackedSeries[A]] =
    val shape = space.dims.take(4)
    val expected = shape.product
    if values.length != expected then
      Left(
        NeuroImageError.LinearSizeMismatch(
          "NeuroVec",
          expected,
          values.length
        )
      )
    else
      val nx = shape(0)
      val ny = shape(1)
      val nz = shape(2)
      val data =
        RavelArray.tabulate[A](nx, ny, nz, shape(3)) { (i, j, k, t) =>
          values(i + nx * (j + ny * (k + nz * t)))
        }
      seriesFromRavel(data, space, label)

  private[image] def volumeFromRavel[A](
      data: RavelArray[A, Rank[3]],
      space: NeuroSpace,
      label: String
  ): Either[NeuroImageError, PackedVolume[A]] =
    for
      canonical <- canonicalD3(space.spatialSpace)
      sampled <- Sampled
        .create[A, ScalaFimValues, Rank[3]](
          canonical.typed,
          data,
          ImageMetadata.named(label)
        )
        .left
        .map(NeuroImageError.Image.apply)
    yield sampled

  private[image] def sliceFromRavel[A](
      data: RavelArray[A, Rank[2]],
      space: NeuroSpace,
      label: String
  ): Either[NeuroImageError, PackedSlice[A]] =
    if space.ndim != 2 then
      Left(NeuroImageError.InvalidRank("NeuroSlice", 2, space.ndim))
    else
      for
        canonical <- canonicalD2(space)
        sampled <- Sampled
          .create[A, ScalaFimValues, Rank[2]](
            canonical.typed,
            data,
            ImageMetadata.named(label)
          )
          .left
          .map(NeuroImageError.Image.apply)
      yield sampled

  private[image] def seriesFromRavel[A](
      data: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String
  ): Either[NeuroImageError, PackedSeries[A]] =
    for
      canonical <- canonicalD3(space)
      sampled <- Sampled
        .create[A, ScalaFimValues, Rank[4]](
          canonical.typed,
          data,
          ImageMetadata.named(label)
        )
        .left
        .map(NeuroImageError.Image.apply)
    yield sampled

  private[image] def componentsFromRavel(
      data: RavelArray[Double, Rank[4]],
      gridSpec: GridSpec,
      label: String
  ): Either[NeuroImageError, PackedComponents] =
    for
      canonical <- canonicalD3(gridSpec.toNeuroSpace)
      direction <- ImageAxis
        .create("direction", 3, AxisKind.Direction)
        .left
        .map(NeuroImageError.Image.apply)
      axes <- NonSpatialAxes
        .from(Vector(direction))
        .left
        .map(NeuroImageError.Image.apply)
      componentSpace = SampleSpace.create(canonical.grid, axes)
      sampled <- Sampled
        .continuous(
          componentSpace,
          data,
          ImageMetadata.named(label)
        )
        .left
        .map(NeuroImageError.Image.apply)
    yield sampled

  private[image] def volumeFromSeriesView[A](
      series: PackedSeries[A],
      index: Int
  ): Either[NeuroImageError, PackedVolume[A]] =
    series
      .selectTime(index)
      .left
      .map(NeuroImageError.Image.apply)

  private def canonicalD2(
      space: NeuroSpace
  ): Either[
    NeuroImageError,
    SomeSampleSpace
  ] =
    val canonical = NeuroSpace.canonical(space)
    Either.cond(
      canonical.spatialRank == 2,
      canonical,
      NeuroImageError.Image(
        ImageError.SpatialDimensionMismatch(2, canonical.spatialRank)
      )
    )

  private def canonicalD3(
      space: NeuroSpace
  ): Either[
    NeuroImageError,
    SomeSampleSpace
  ] =
    val canonical = NeuroSpace.canonical(space)
    Either.cond(
      canonical.spatialRank == 3,
      canonical,
      NeuroImageError.Image(
        ImageError.SpatialDimensionMismatch(3, canonical.spatialRank)
      )
    )
