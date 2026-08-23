package scalafim.image

import image4s.AxisKind
import image4s.Categorical
import image4s.Continuous
import image4s.ImageMetadata
import image4s.Mask as MaskSemantics
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.D3
import ravel.CanonicalArray
import ravel.CanonicalLayoutError
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape

/** A zero-wrapper D3 sampled series with exactly one non-spatial Time axis. */
opaque type AnyNeuroSeries[A] <:
    Sampled[
      ? <: SampleSpace[?, D3],
      A,
      ?,
      Rank[4]
    ] =
  Sampled[? <: SampleSpace[?, D3], A, ?, Rank[4]]

opaque type SomeNeuroSeries[A, Sem] <:
    AnyNeuroSeries[A] & Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Sem,
      Rank[4]
    ] =
  Sampled[? <: SampleSpace[?, D3], A, Sem, Rank[4]]

opaque type NeuroSeries[
    S <: SampleSpace[?, D3],
    A,
    Sem
] <: SomeNeuroSeries[A, Sem] & Sampled[S, A, Sem, Rank[4]] =
  Sampled[S, A, Sem, Rank[4]]

type ScalarSeries[S <: SampleSpace[?, D3], A] =
  NeuroSeries[S, A, Continuous]

type SomeScalarSeries[A] =
  SomeNeuroSeries[A, Continuous]

type LabelSeries[S <: SampleSpace[?, D3], A] =
  NeuroSeries[S, A, Categorical]

type SomeLabelSeries[A] =
  SomeNeuroSeries[A, Categorical]

type MaskSeries[S <: SampleSpace[?, D3]] =
  NeuroSeries[S, Boolean, MaskSemantics]

type SomeMaskSeries =
  SomeNeuroSeries[Boolean, MaskSemantics]

object AnyNeuroSeries:
  private[image] inline def unsafeFromSampled[A](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        ?,
        Rank[4]
      ]
  ): AnyNeuroSeries[A] =
    sampled

  inline def eraseSemantics[A, Sem](
      series: SomeNeuroSeries[A, Sem]
  ): AnyNeuroSeries[A] =
    series

  extension [A](series: AnyNeuroSeries[A])
    inline def apply(x: Int, y: Int, z: Int, time: Int): A =
      series.data(x, y, z, time)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[4]]
    ] =
      CanonicalArray.from(series.data)

    def materializedCanonical: AnyNeuroSeries[A] =
      series.materializedCopy

object SomeNeuroSeries:
  inline def eraseSpace[S <: SampleSpace[?, D3], A, Sem](
      series: NeuroSeries[S, A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    series

  private[image] def fromSampled[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ]
  ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
    val axes = sampled.nonSpatialAxes.values
    if axes.size == 1 && axes.head.kind == AxisKind.Time then Right(sampled)
    else Left(NativeImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))

  extension [A, Sem](series: SomeNeuroSeries[A, Sem])
    inline def apply(x: Int, y: Int, z: Int, time: Int): A =
      series.data(x, y, z, time)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[4]]
    ] =
      CanonicalArray.from(series.data)

    def voxelTimeMatrix: Either[
      CanonicalLayoutError,
      NDArray[A, Rank[2]]
    ] =
      wholeCanonical.map: canonical =>
        canonical.reshapeView(
          Shape(series.grid.shape.product, series.nonSpatialAxes.values.head.extent)
        )

    def materializedCanonical: SomeNeuroSeries[A, Sem] =
      unsafeFromSampled(series.materializedCopy)

  private inline def unsafeFromSampled[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ]
  ): SomeNeuroSeries[A, Sem] =
    sampled

object NeuroSeries:
  private def validateSome[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ]
  ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
    SomeNeuroSeries.fromSampled(sampled)

  def fromSampled[
      S <: SampleSpace[?, D3],
      A,
      Sem
  ](
      sampled: Sampled[S, A, Sem, Rank[4]]
  ): Either[NativeImageError, NeuroSeries[S, A, Sem]] =
    val axes = sampled.nonSpatialAxes.values
    if axes.size == 1 && axes.head.kind == AxisKind.Time then Right(sampled)
    else Left(NativeImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))

  private[image] inline def unsafeFromSampled[
      S <: SampleSpace[?, D3],
      A,
      Sem
  ](
      sampled: Sampled[S, A, Sem, Rank[4]]
  ): NeuroSeries[S, A, Sem] =
    sampled

  def fromRavel[A, Sem](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[4]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    NativeImageError,
    NeuroSeries[sampleSpace.type, A, Sem]
  ] =
    Sampled
      .create[A, Sem, Rank[4]](sampleSpace, data, metadata)
      .left
      .map(NativeImageError.Image.apply)
      .flatMap(fromSampled)

  def continuous[A](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[4]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    NativeImageError,
    ScalarSeries[sampleSpace.type, A]
  ] =
    fromRavel[A, Continuous](sampleSpace, data, metadata)

  def categorical[A](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[4]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    NativeImageError,
    LabelSeries[sampleSpace.type, A]
  ] =
    fromRavel[A, Categorical](sampleSpace, data, metadata)

  def mask(
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[Boolean, Rank[4]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[
    NativeImageError,
    MaskSeries[sampleSpace.type]
  ] =
    fromRavel[Boolean, MaskSemantics](sampleSpace, data, metadata)

  def copyContinuousFromCanonicalArray[A](
      sampleSpace: SampleSpace[?, D3],
      data: Array[A],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      DType[A],
      ValueSemantics[A, Continuous]
  ): Either[
    NativeImageError,
    ScalarSeries[sampleSpace.type, A]
  ] =
    copyFromCanonicalArray[A, Continuous](sampleSpace, data, metadata)

  def copyCategoricalFromCanonicalArray[A](
      sampleSpace: SampleSpace[?, D3],
      data: Array[A],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      DType[A],
      ValueSemantics[A, Categorical]
  ): Either[
    NativeImageError,
    LabelSeries[sampleSpace.type, A]
  ] =
    copyFromCanonicalArray[A, Categorical](sampleSpace, data, metadata)

  def copyMaskFromCanonicalArray(
      sampleSpace: SampleSpace[?, D3],
      data: Array[Boolean],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[
    NativeImageError,
    MaskSeries[sampleSpace.type]
  ] =
    copyFromCanonicalArray[Boolean, MaskSemantics](
      sampleSpace,
      data,
      metadata
    )

  private def copyFromCanonicalArray[A, Sem](
      sampleSpace: SampleSpace[?, D3],
      data: Array[A],
      metadata: ImageMetadata
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[
    NativeImageError,
    NeuroSeries[sampleSpace.type, A, Sem]
  ] =
    val axes = sampleSpace.nonSpatialAxes.values
    if axes.size != 1 || axes.head.kind != AxisKind.Time then
      Left(NativeImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))
    else
      val shape = sampleSpace.logicalShape
      val expected = shape.product
      if data.length != expected then
        Left(
          NativeImageError.CanonicalArraySizeMismatch(
            expected,
            data.length
          )
        )
      else
        val copied =
          NDArray.fromSeq(
            Shape(shape(0), shape(1), shape(2), shape(3)),
            data
          )
        fromRavel[A, Sem](sampleSpace, copied, metadata)

  extension [S <: SampleSpace[?, D3], A, Sem](
      series: NeuroSeries[S, A, Sem]
  )
    inline def sampled: Sampled[S, A, Sem, Rank[4]] =
      series

    inline def apply(x: Int, y: Int, z: Int, time: Int): A =
      series.data(x, y, z, time)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[4]]
    ] =
      CanonicalArray.from(series.data)

    /** Zero-copy `(voxel, time)` reshape for a whole canonical series. */
    def voxelTimeMatrix: Either[
      CanonicalLayoutError,
      NDArray[A, Rank[2]]
    ] =
      wholeCanonical.map: canonical =>
        canonical.reshapeView(
          Shape(series.grid.shape.product, series.nonSpatialAxes.values.head.extent)
        )

    def volumeAt(
        time: Int
    ): Either[NativeImageError, SomeNeuroVolume[A, Sem]] =
      series
        .selectTime(time)
        .left
        .map(NativeImageError.Image.apply)
        .map(SomeNeuroVolume.unsafeFromSampled)

    /** Explicitly copy logical values into a whole canonical Ravel owner. */
    def materializedCanonical: NeuroSeries[S, A, Sem] =
      unsafeFromSampled(series.materializedCopy)

    def withImageMetadata(
        metadata: ImageMetadata
    ): NeuroSeries[S, A, Sem] =
      unsafeFromSampled(series.withMetadata(metadata))

    def cropSeries(
        origin: Vector[Int],
        shape: Vector[Int]
    ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
      series
        .crop(origin, shape)
        .left
        .map(NativeImageError.Image.apply)
        .flatMap(validateSome)

    def flipSeries(
        axis: Int
    ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
      series
        .flipSpatial(axis)
        .left
        .map(NativeImageError.Image.apply)
        .flatMap(validateSome)

    def permuteSeries(
        sourceAxisForTarget: IterableOnce[Int]
    ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
      series
        .permuteSpatial(sourceAxisForTarget)
        .left
        .map(NativeImageError.Image.apply)
        .flatMap(validateSome)

    def strideSeries(
        steps: IterableOnce[Int]
    ): Either[NativeImageError, SomeNeuroSeries[A, Sem]] =
      series
        .strideSpatial(steps)
        .left
        .map(NativeImageError.Image.apply)
        .flatMap(validateSome)
