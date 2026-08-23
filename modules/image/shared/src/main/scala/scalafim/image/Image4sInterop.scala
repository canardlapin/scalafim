package scalafim.image

import image4s.ImageMetadata
import ravel.NDArray as RavelArray
import ravel.Rank

/** Temporary old-name construction shell over native image values. */
object Image4sInterop:
  private[image] type PackedVolume[A] =
    AnyNeuroVolume[A]

  private[image] type PackedSeries[A] =
    AnyNeuroSeries[A]

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
