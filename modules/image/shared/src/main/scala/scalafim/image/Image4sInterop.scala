package scalafim.image

import image4s.ImageError
import image4s.ImageMetadata
import image4s.Continuous
import image4s.SampleSpace
import image4s.Sampled
import image4s.SomeSampleSpace
import ravel.DType
import ravel.DType.given
import ravel.NDArray as RavelArray
import ravel.Rank
import image4s.geometry.Affine
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
  private[image] type PackedVolume[A] =
    AnyNeuroVolume[A]

  private[image] type PackedSeries[A] =
    AnyNeuroSeries[A]

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
  )(using
      DType[A],
      MigrationValueSemantics[A]
  ): Either[NeuroImageError, PackedVolume[A]] =
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
  )(using
      DType[A],
      MigrationValueSemantics[A]
  ): Either[NeuroImageError, PackedSeries[A]] =
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
  )(using
      policy: MigrationValueSemantics[A]
  ): Either[NeuroImageError, PackedVolume[A]] =
    given image4s.ValueSemantics[A, policy.Sem] = policy.evidence
    for
      canonical <- NeuroSpace
        .requireSpatialD3(space)
        .left
        .map(NeuroImageError.Space.apply)
      sampled <- NeuroVolume
        .fromRavel[A, policy.Sem](
          canonical,
          data,
          ImageMetadata.named(label)
        )
        .left
        .map(NeuroImageError.Image.apply)
    yield AnyNeuroVolume.eraseSemantics(
      SomeNeuroVolume.eraseSpace(sampled)
    )

  private[image] def seriesFromRavel[A](
      data: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String
  )(using
      policy: MigrationValueSemantics[A]
  ): Either[NeuroImageError, PackedSeries[A]] =
    given image4s.ValueSemantics[A, policy.Sem] = policy.evidence
    for
      canonical <- NeuroSpace
        .requireD3(space)
        .left
        .map(NeuroImageError.Space.apply)
      sampled <- NeuroSeries
        .fromRavel[A, policy.Sem](
          canonical,
          data,
          ImageMetadata.named(label)
        )
        .left.map:
          case NativeImageError.Image(error) => NeuroImageError.Image(error)
          case NativeImageError.Space(error) => NeuroImageError.Space(error)
          case error =>
            NeuroImageError.InvalidRank(
              error.message,
              4,
              data.rank
            )
    yield AnyNeuroSeries.eraseSemantics(
      SomeNeuroSeries.eraseSpace(sampled)
    )

  private[image] def volumeFromSeriesView[A](
      series: PackedSeries[A],
      index: Int
  ): Either[NeuroImageError, PackedVolume[A]] =
    series
      .selectTime(index)
      .left
      .map(NeuroImageError.Image.apply)
      .map(AnyNeuroVolume.unsafeFromSampled)

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
