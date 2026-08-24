package scalafim.image

import SampleSpaces.*

import image4s.AxisKind
import image4s.AxisConcatenationPolicy
import image4s.Categorical
import image4s.Continuous
import image4s.ImageMetadata
import image4s.Mask as MaskSemantics
import image4s.SampleSpace
import image4s.Sampled
import image4s.SamplingAlignment
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.CanonicalArray
import ravel.CanonicalLayoutError
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import ravel.select
import scala.reflect.ClassTag
import scala.annotation.targetName

/** A zero-wrapper D3 sampled series with exactly one non-spatial Time axis. */
opaque type SomeNeuroSeries[A, Sem] =
  Sampled[? <: SampleSpace[?, D3], A, Sem, Rank[4]]

opaque type NeuroSeries[
    S <: SampleSpace[?, D3],
    A,
    Sem
] =
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

/** Policy for combining metadata when concatenating admitted series. */
enum SeriesMetadataPolicy derives CanEqual:
  /** Require every input to carry the same metadata. */
  case RequireEqual

  /** Retain the first input's metadata after an explicit caller choice. */
  case UseLeft

object SomeScalarSeries:
  def fromRavel[A](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using ValueSemantics[A, Continuous]): Either[NeuroImageError, SomeScalarSeries[A]] =
    SomeNeuroSeries.fromRavel[A, Continuous](data, space, metadata)

  def unsafeFromRavel[A](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
    SomeNeuroSeries.unsafeFromRavel[A, Continuous](data, space, label)

  def unsafeCopyFromCanonicalArray[A](
      data: Array[A],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[A], ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
    SomeNeuroSeries.unsafeCopyFromCanonicalArray[A, Continuous](data, space, label)

object SomeLabelSeries:
  def fromRavel[A](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using ValueSemantics[A, Categorical]): Either[NeuroImageError, SomeLabelSeries[A]] =
    SomeNeuroSeries.fromRavel[A, Categorical](data, space, metadata)

  def unsafeFromRavel[A](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[A, Categorical]): SomeLabelSeries[A] =
    SomeNeuroSeries.unsafeFromRavel[A, Categorical](data, space, label)

  def unsafeCopyFromCanonicalArray[A](
      data: Array[A],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[A], ValueSemantics[A, Categorical]): SomeLabelSeries[A] =
    SomeNeuroSeries.unsafeCopyFromCanonicalArray[A, Categorical](data, space, label)

object SomeMaskSeries:
  def unsafeFromRavel(
      data: NDArray[Boolean, Rank[4]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[Boolean, MaskSemantics]): SomeMaskSeries =
    SomeNeuroSeries.unsafeFromRavel[Boolean, MaskSemantics](data, space, label)

  def unsafeCopyFromCanonicalArray(
      data: Array[Boolean],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[Boolean], ValueSemantics[Boolean, MaskSemantics]): SomeMaskSeries =
    SomeNeuroSeries.unsafeCopyFromCanonicalArray[Boolean, MaskSemantics](data, space, label)

object SomeNeuroSeries:
  inline def eraseSpace[S <: SampleSpace[?, D3], A, Sem](
      series: NeuroSeries[S, A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    unsafeFromSampled(NeuroSeries.sampled(series))

  private[image] def fromSampled[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    val axes = sampled.nonSpatialAxes.values
    if axes.size == 1 && axes.head.kind == AxisKind.Time then
      Right(sampled)
    else Left(NeuroImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))

  extension [A, Sem](series: SomeNeuroSeries[A, Sem])
    inline def apply(x: Int, y: Int, z: Int, time: Int): A =
      series.data(x, y, z, time)

    @targetName("seriesWholeCanonical")
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

    @targetName("seriesMaterializedCanonical")
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

  def fromRavel[A, Sem](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    for
      sampleSpace <- SampleSpaces
        .requireD3(space)
        .left
        .map(NeuroImageError.Space.apply)
      sampled <- NeuroSeries
        .fromRavel[A, Sem](sampleSpace, data, metadata)
    yield eraseSpace(sampled)

  def unsafeFromRavel[A, Sem](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    fromRavel[A, Sem](data, space, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeFromRavel[A, Sem](
      data: NDArray[A, Rank[4]],
      space: SomeSampleSpace,
      label: String
  )(using
      ValueSemantics[A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    unsafeFromRavel[A, Sem](data, space, ImageMetadata.named(label))

  def copyFromCanonicalArray[A, Sem](
      data: Array[A],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    val shape = space.dims.take(4)
    val expected = shape.product
    if shape.length != 4 then
      Left(NeuroImageError.InvalidRank("NeuroSeries space", 4, shape.length))
    else if data.length != expected then
      Left(
        NeuroImageError.LinearSizeMismatch(
          "NeuroSeries canonical array",
          expected,
          data.length
        )
      )
    else
      fromRavel[A, Sem](
        NDArray.fromSeq(Shape(shape(0), shape(1), shape(2), shape(3)), data),
        space,
        metadata
      )

  def unsafeCopyFromCanonicalArray[A, Sem](
      data: Array[A],
      space: SomeSampleSpace,
      label: String = ""
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    copyFromCanonicalArray[A, Sem](data, space, ImageMetadata.named(label))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def mapDynamic[A, Sem, B, OutSem](
      series: SomeNeuroSeries[A, Sem]
  )(
      f: A => B
  )(using
      DType[B],
      ValueSemantics[B, OutSem]
  ): SomeNeuroSeries[B, OutSem] =
    unsafeFromSampled(series.mapValuesAs[B, OutSem](f))

  /** Reattach data constructed directly from this series' complete logical shape. */
  private def replaceValuesSameShape[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
      series: NeuroSeries[S, A, Sem],
      data: NDArray[B, Rank[4]]
  )(using ValueSemantics[B, OutSem]): NeuroSeries[S, B, OutSem] =
    NeuroSeries.unsafeFromSampled(
      NeuroSeries
        .sampled(series)
        .replaceDataChecked[B, OutSem, Rank[4]](data)
        .fold(
          error =>
            throw new IllegalStateException(
              s"internally constructed series shape was invalid: ${error.message}"
            ),
          identity
        )
    )

  private def mapSamplesExact[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
      series: NeuroSeries[S, A, Sem]
  )(
      f: (VoxelCoord, Int, A) => B
  )(using
      DType[B],
      ValueSemantics[B, OutSem]
  ): NeuroSeries[S, B, OutSem] =
    val sampled = NeuroSeries.sampled(series)
    val shape = sampled.grid.shape
    val nVolumes = sampled.nonSpatialAxes.values.head.extent
    val mapped =
      NDArray.tabulate[B](shape(0), shape(1), shape(2), nVolumes):
        (x, y, z, time) =>
          f(VoxelCoord(x, y, z), time, sampled.data(x, y, z, time))
    replaceValuesSameShape(series, mapped)

  private def selectTimesChecked[A, Sem](
      series: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ],
      times: IterableOnce[Int]
  )(using
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    val copied = times.iterator.toVector
    val sourceAxis = series.nonSpatialAxes.values.head
    given DType[A] = series.data.dtype
    for
      selectedAxis <- sourceAxis
        .select(copied)
        .left
        .map(NeuroImageError.Image.apply)
      selectedAxes <- series.nonSpatialAxes
        .updated(0, selectedAxis)
        .left
        .map(NeuroImageError.Image.apply)
      targetSpace = SampleSpace.create(series.grid, selectedAxes)
      shape = series.grid.shape
      selectedData =
        NDArray.tabulate[A](shape(0), shape(1), shape(2), copied.size):
          (x, y, z, position) =>
            series.data(x, y, z, copied(position))
      selected <- Sampled
        .create[A, Sem, Rank[4]](
          targetSpace,
          selectedData,
          series.metadata
        )
        .left
        .map(NeuroImageError.Image.apply)
      admitted <- fromSampled(selected)
    yield admitted

  extension [A, Sem](series: SomeNeuroSeries[A, Sem])
    @targetName("seriesSampled")
    def sampled: Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Sem,
      Rank[4]
    ] =
      series

    /** Provider-shaped read access without weakening the opaque admission boundary. */
    @targetName("seriesData")
    inline def data: NDArray[A, Rank[4]] =
      series.data

    @targetName("seriesGrid")
    inline def grid: Grid[? <: Frame[D3], D3] =
      series.grid

    @targetName("seriesProviderSampleSpace")
    inline def sampleSpace: SomeSampleSpace =
      SampleSpaces.fromCanonical(series.sampleSpace)

    @targetName("seriesMetadata")
    inline def metadata: ImageMetadata =
      series.metadata

    @targetName("seriesLabel")
    inline def label: String =
      series.metadata.label

    @targetName("seriesValues")
    inline def values: NDArray[A, Rank[4]] =
      series.data

    @targetName("seriesSampleSpace")
    inline def space: SomeSampleSpace =
      SampleSpaces.fromCanonical(series.sampleSpace)

    @targetName("seriesNdim")
    def ndim: Int =
      series.data.rank

    def nVolumes: Int =
      series.nonSpatialAxes.values.head.extent

    def volume(time: Int): SomeNeuroVolume[A, Sem] =
      series
        .selectTime(time)
        .left
        .map(NeuroImageError.Image.apply)
        .map(SomeNeuroVolume.unsafeFromSampled)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

    inline def apply(time: Int): SomeNeuroVolume[A, Sem] =
      volume(time)

    def volumeAt(time: Int): Either[NeuroImageError, SomeNeuroVolume[A, Sem]] =
      series
        .selectTime(time)
        .left
        .map(NeuroImageError.Image.apply)
        .map(SomeNeuroVolume.unsafeFromSampled)

    @targetName("seriesValueAtCanonicalOrdinal")
    private[scalafim] def valueAtCanonicalOrdinal(index: Int): A =
      val logical = Indexing.indexToGrid(space.dims.take(4), index)
      series.data(logical(0), logical(1), logical(2), logical(3))

    private[scalafim] def valueAtVoxelOrdinal(
        voxelOrdinal: Int,
        time: Int
    ): A =
      val voxel = space.indexToVoxel3D(voxelOrdinal)
      series.data(voxel.x, voxel.y, voxel.z, time)

    @targetName("seriesCopyToCanonicalArray")
    def copyToCanonicalArray(using ClassTag[A]): Array[A] =
      val out = PrimitiveBuffers.ofSize[A](series.data.size)
      var index = 0
      while index < out.length do
        out(index) = valueAtCanonicalOrdinal(index)
        index += 1
      out

    def gridToIndex(x: Int, y: Int, z: Int, time: Int): Int =
      Indexing.gridToIndex(space.dims.take(4), Vector(x, y, z, time))

    @targetName("seriesIndexToGrid")
    def indexToGrid(index: Int): Vector[Int] =
      Indexing.indexToGrid(space.dims.take(4), index)

    @targetName("seriesAsMatrix")
    def asMatrix: NDArray[A, Rank[2]] =
      given DType[A] = series.data.dtype
      val spatialSize = space.spatialDims.product
      NDArray.tabulate[A](spatialSize, nVolumes):
        (voxelOrdinal, time) => valueAtVoxelOrdinal(voxelOrdinal, time)

    def subArray(
        xs: Seq[Int],
        ys: Seq[Int],
        zs: Seq[Int],
        times: Seq[Int]
    ): NDArray[A, Rank[4]] =
      val dims = space.dims.take(4)
      require(xs.forall(index => index >= 0 && index < dims(0)), "x index out of bounds")
      require(ys.forall(index => index >= 0 && index < dims(1)), "y index out of bounds")
      require(zs.forall(index => index >= 0 && index < dims(2)), "z index out of bounds")
      require(times.forall(index => index >= 0 && index < dims(3)), "time index out of bounds")
      given DType[A] = series.data.dtype
      NDArray.tabulate[A](xs.length, ys.length, zs.length, times.length):
        (x, y, z, time) => series.data(xs(x), ys(y), zs(z), times(time))

    def timeSeries(voxelOrdinal: Int): ravel.Array1[A] =
      val spatialSize = space.spatialDims.product
      require(voxelOrdinal >= 0 && voxelOrdinal < spatialSize, "spatial index out of bounds")
      val voxel = space.indexToVoxel3D(voxelOrdinal)
      series.data
        .select(0, voxel.x)
        .select(0, voxel.y)
        .select(0, voxel.z)

    def timeSeries(x: Int, y: Int, z: Int): ravel.Array1[A] =
      timeSeries(Indexing.gridToIndex3D(space.spatialDims, x, y, z))

    def timeSeries(voxelOrdinals: ravel.Array1[Int]): NDArray[A, Rank[2]] =
      val spatialSize = space.spatialDims.product
      given DType[A] = series.data.dtype
      NDArray.tabulate[A](voxelOrdinals.size, nVolumes):
        (position, time) =>
          val ordinal = voxelOrdinals(position)
          require(ordinal >= 0 && ordinal < spatialSize, "spatial index out of bounds")
          valueAtVoxelOrdinal(ordinal, time)

    def timeSeries(voxelOrdinals: Array[Int]): NDArray[A, Rank[2]] =
      timeSeries(NDArray.fromSeq(Shape(voxelOrdinals.length), voxelOrdinals))

    def timeSeries(
        mask: SomeMaskVolume
    ): Either[NeuroImageError, NDArray[A, Rank[2]]] =
      Grid
        .exactCongruence(series.grid, mask.grid)
        .left
        .map(NeuroImageError.Geometry.apply)
        .flatMap: _ =>
          if series.grid.sameRuntimeOwnerAs(mask.grid) then
            Right(timeSeries(Mask.indices(mask)))
          else
            Left(
              NeuroImageError.GridOwnerMismatch(
                series.grid.persistentId,
                mask.grid.persistentId
              )
            )

    def selectTimes(times: Seq[Int])(using
        ValueSemantics[A, Sem]
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      selectTimesChecked(series, times)

    @targetName("concatenateSeries")
    def concatenate(
        that: SomeNeuroSeries[A, Sem],
        rest: SomeNeuroSeries[A, Sem]*
    )(
        axisPolicy: AxisConcatenationPolicy,
        metadataPolicy: SeriesMetadataPolicy
    )(using
        ValueSemantics[A, Sem]
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      given DType[A] = series.data.dtype
      val all = Vector(series, that) ++ rest.toVector
      val compatibleGrids =
        all.tail.foldLeft[Either[NeuroImageError, Unit]](Right(())):
          (result, next) =>
            result.flatMap: _ =>
              Grid
                .exactCongruence(series.grid, next.grid)
                .left
                .map(NeuroImageError.Geometry.apply)
                .map(_ => ())
      val selectedMetadata =
        metadataPolicy match
          case SeriesMetadataPolicy.UseLeft =>
            Right(series.metadata)
          case SeriesMetadataPolicy.RequireEqual =>
            all.tail
              .find(_.metadata != series.metadata)
              .map(next =>
                NeuroImageError.ConcatenationMetadataMismatch(
                  series.metadata,
                  next.metadata
                )
              )
              .toLeft(series.metadata)
      val concatenatedAxis =
        all.tail.foldLeft[Either[NeuroImageError, image4s.Axis]](
          Right(all.head.nonSpatialAxes.values.head)
        ): (result, next) =>
          result.flatMap: axis =>
            axis
              .concatenate(next.nonSpatialAxes.values.head, axisPolicy)
              .left
              .map(NeuroImageError.Image.apply)
      for
        _ <- compatibleGrids
        metadata <- selectedMetadata
        axis <- concatenatedAxis
        axes <- series.nonSpatialAxes
          .updated(0, axis)
          .left
          .map(NeuroImageError.Image.apply)
        targetSpace = SampleSpace.create(series.grid, axes)
        boundaries =
          all.scanLeft(0)((offset, value) => offset + value.nVolumes)
        totalTimes = boundaries.last
        shape = space.spatialDims
        data =
          NDArray.tabulate[A](shape(0), shape(1), shape(2), totalTimes):
            (x, y, z, time) =>
              var block = 0
              while boundaries(block + 1) <= time do block += 1
              all(block).data(x, y, z, time - boundaries(block))
        sampled <- Sampled
          .create[A, Sem, Rank[4]](targetSpace, data, metadata)
          .left
          .map(NeuroImageError.Image.apply)
        admitted <- fromSampled(sampled)
      yield admitted

    @targetName("mapSeriesValues")
    def mapValues[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroSeries[B, OutSem] =
      mapDynamic[A, Sem, B, OutSem](series)(f)

    @targetName("mapSeries")
    def map[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroSeries[B, OutSem] =
      mapDynamic[A, Sem, B, OutSem](series)(f)

    @targetName("mapSeriesVoxels")
    def mapVoxels[B, OutSem](
        f: (VoxelCoord, A) => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroSeries[B, OutSem] =
      val shape = space.spatialDims
      val mapped =
        NDArray.tabulate[B](shape(0), shape(1), shape(2), nVolumes):
          (x, y, z, time) =>
            f(VoxelCoord(x, y, z), series.data(x, y, z, time))
      unsafeFromRavel[B, OutSem](mapped, space, series.metadata)

    def mapSamples[B, OutSem](
        f: (VoxelCoord, Int, A) => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroSeries[B, OutSem] =
      val shape = space.spatialDims
      val mapped =
        NDArray.tabulate[B](shape(0), shape(1), shape(2), nVolumes):
          (x, y, z, time) =>
            f(VoxelCoord(x, y, z), time, series.data(x, y, z, time))
      unsafeFromRavel[B, OutSem](mapped, space, series.metadata)

    @targetName("zipSeriesExact")
    def zipExact[B, BSem, C, OutSem](
        that: SomeNeuroSeries[B, BSem]
    )(
        f: (A, B) => C
    )(using
        DType[C],
        ValueSemantics[C, OutSem]
    ): Either[NeuroImageError, SomeNeuroSeries[C, OutSem]] =
      for
        left <- SampleSpaces
          .requireD3(series.space)
          .left
          .map(NeuroImageError.Space.apply)
        right <- SampleSpaces
          .requireD3(that.space)
          .left
          .map(NeuroImageError.Space.apply)
        _ <- SamplingAlignment
          .exact(left, right)
          .left
          .map(NeuroImageError.Image.apply)
      yield
        val shape = space.spatialDims
        val data =
          NDArray.tabulate[C](shape(0), shape(1), shape(2), nVolumes):
            (x, y, z, time) =>
              f(series.data(x, y, z, time), that.data(x, y, z, time))
        unsafeFromRavel[C, OutSem](data, space, series.metadata)

  extension [S <: SampleSpace[?, D3], A, Sem](
      series: NeuroSeries[S, A, Sem]
  )
    @targetName("materializedSeriesOwned")
    def materializedCanonical: NeuroSeries[S, A, Sem] =
      NeuroSeries.unsafeFromSampled(
        NeuroSeries.sampled(series).materializedCopy
      )

    @targetName("metadataSeriesOwned")
    def withImageMetadata(
        metadata: ImageMetadata
    ): NeuroSeries[S, A, Sem] =
      NeuroSeries.unsafeFromSampled(
        NeuroSeries.sampled(series).withMetadata(metadata)
      )

    @targetName("mapSeriesValuesOwned")
    def mapValues[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): NeuroSeries[S, B, OutSem] =
      NeuroSeries.unsafeFromSampled(
        NeuroSeries.sampled(series).mapValuesAs[B, OutSem](f)
      )

    @targetName("mapSeriesOwned")
    def map[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): NeuroSeries[S, B, OutSem] =
      NeuroSeries.unsafeFromSampled(
        NeuroSeries.sampled(series).mapValuesAs[B, OutSem](f)
      )

    @targetName("mapSeriesVoxelsOwned")
    def mapVoxels[B, OutSem](
        f: (VoxelCoord, A) => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): NeuroSeries[S, B, OutSem] =
      mapSamplesExact(series)((voxel, _, value) => f(voxel, value))

    @targetName("mapSeriesSamplesOwned")
    def mapSamples[B, OutSem](
        f: (VoxelCoord, Int, A) => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): NeuroSeries[S, B, OutSem] =
      mapSamplesExact(series)(f)

    @targetName("selectSeriesTimesOwned")
    def selectTimes(times: Seq[Int])(using
        ValueSemantics[A, Sem]
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      selectTimesChecked(NeuroSeries.sampled(series), times)

object NeuroSeries:
  private def validateSome[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[4]
      ]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    SomeNeuroSeries.fromSampled(sampled)

  def fromSampled[
      S <: SampleSpace[?, D3],
      A,
      Sem
  ](
      sampled: Sampled[S, A, Sem, Rank[4]]
  ): Either[NeuroImageError, NeuroSeries[S, A, Sem]] =
    val axes = sampled.nonSpatialAxes.values
    if axes.size == 1 && axes.head.kind == AxisKind.Time then Right(sampled)
    else Left(NeuroImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))

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
    NeuroImageError,
    NeuroSeries[sampleSpace.type, A, Sem]
  ] =
    Sampled
      .create[A, Sem, Rank[4]](sampleSpace, data, metadata)
      .left
      .map(NeuroImageError.Image.apply)
      .flatMap(fromSampled)

  def continuous[A](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[4]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    NeuroImageError,
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
    NeuroImageError,
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
    NeuroImageError,
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
    NeuroImageError,
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
    NeuroImageError,
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
    NeuroImageError,
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
    NeuroImageError,
    NeuroSeries[sampleSpace.type, A, Sem]
  ] =
    val axes = sampleSpace.nonSpatialAxes.values
    if axes.size != 1 || axes.head.kind != AxisKind.Time then
      Left(NeuroImageError.ExpectedSingleTimeAxis(axes.map(_.kind)))
    else
      val shape = sampleSpace.logicalShape
      val expected = shape.product
      if data.length != expected then
        Left(
          NeuroImageError.CanonicalArraySizeMismatch(
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
    ): Either[NeuroImageError, SomeNeuroVolume[A, Sem]] =
      series
        .selectTime(time)
        .left
        .map(NeuroImageError.Image.apply)
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
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      series
        .crop(origin, shape)
        .left
        .map(NeuroImageError.Image.apply)
        .flatMap(validateSome)

    def flipSeries(
        axis: Int
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      series
        .flipSpatial(axis)
        .left
        .map(NeuroImageError.Image.apply)
        .flatMap(validateSome)

    def permuteSeries(
        sourceAxisForTarget: IterableOnce[Int]
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      series
        .permuteSpatial(sourceAxisForTarget)
        .left
        .map(NeuroImageError.Image.apply)
        .flatMap(validateSome)

    def strideSeries(
        steps: IterableOnce[Int]
    ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
      series
        .strideSpatial(steps)
        .left
        .map(NeuroImageError.Image.apply)
        .flatMap(validateSome)
