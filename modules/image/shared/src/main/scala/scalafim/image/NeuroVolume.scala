package scalafim.image

import cats.Applicative
import cats.syntax.all.*
import image4s.Categorical
import image4s.Continuous
import image4s.ImageMetadata
import image4s.Mask as MaskSemantics
import image4s.SampleSpace
import image4s.Sampled
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
import scala.reflect.ClassTag
import spire.algebra.{Order, Ring}

/** A zero-wrapper three-dimensional neuroimaging refinement.
  *
  * The value is the exact image4s `Sampled` object supplied at construction.
  * Its only dense owner is the retained Ravel array. The static D3 plus rank-3
  * contract implies that the sample space has no non-spatial axes.
  */
opaque type SomeNeuroVolume[A, Sem] =
  Sampled[? <: SampleSpace[?, D3], A, Sem, Rank[3]]

opaque type NeuroVolume[
    S <: SampleSpace[?, D3],
    A,
    Sem
] <: Sampled[S, A, Sem, Rank[3]] & SomeNeuroVolume[A, Sem] =
  Sampled[S, A, Sem, Rank[3]]

type ScalarVolume[S <: SampleSpace[?, D3], A] =
  NeuroVolume[S, A, Continuous]

type SomeScalarVolume[A] =
  SomeNeuroVolume[A, Continuous]

type LabelVolume[S <: SampleSpace[?, D3], A] =
  NeuroVolume[S, A, Categorical]

type SomeLabelVolume[A] =
  SomeNeuroVolume[A, Categorical]

type MaskVolume[S <: SampleSpace[?, D3]] =
  NeuroVolume[S, Boolean, MaskSemantics]

type SomeMaskVolume =
  SomeNeuroVolume[Boolean, MaskSemantics]

object SomeScalarVolume:
  def fromRavel[A](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using ValueSemantics[A, Continuous]): Either[NeuroImageError, SomeScalarVolume[A]] =
    SomeNeuroVolume.fromRavel[A, Continuous](data, space, metadata)

  def unsafeFromRavel[A](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
    SomeNeuroVolume.unsafeFromRavel[A, Continuous](data, space, label)

  def unsafeCopyFromCanonicalArray[A](
      data: Array[A],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[A], ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
    SomeNeuroVolume.unsafeCopyFromCanonicalArray[A, Continuous](data, space, label)

object SomeLabelVolume:
  def fromRavel[A](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using ValueSemantics[A, Categorical]): Either[NeuroImageError, SomeLabelVolume[A]] =
    SomeNeuroVolume.fromRavel[A, Categorical](data, space, metadata)

  def unsafeFromRavel[A](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[A, Categorical]): SomeLabelVolume[A] =
    SomeNeuroVolume.unsafeFromRavel[A, Categorical](data, space, label)

  def unsafeCopyFromCanonicalArray[A](
      data: Array[A],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[A], ValueSemantics[A, Categorical]): SomeLabelVolume[A] =
    SomeNeuroVolume.unsafeCopyFromCanonicalArray[A, Categorical](data, space, label)

object SomeMaskVolume:
  def fromRavel(
      data: NDArray[Boolean, Rank[3]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using ValueSemantics[Boolean, MaskSemantics]): Either[NeuroImageError, SomeMaskVolume] =
    SomeNeuroVolume.fromRavel[Boolean, MaskSemantics](data, space, metadata)

  def unsafeFromRavel(
      data: NDArray[Boolean, Rank[3]],
      space: SomeSampleSpace,
      label: String = ""
  )(using ValueSemantics[Boolean, MaskSemantics]): SomeMaskVolume =
    SomeNeuroVolume.unsafeFromRavel[Boolean, MaskSemantics](data, space, label)

  def unsafeCopyFromCanonicalArray(
      data: Array[Boolean],
      space: SomeSampleSpace,
      label: String = ""
  )(using DType[Boolean], ValueSemantics[Boolean, MaskSemantics]): SomeMaskVolume =
    SomeNeuroVolume.unsafeCopyFromCanonicalArray[Boolean, MaskSemantics](data, space, label)

object SomeNeuroVolume:
  inline def eraseSpace[S <: SampleSpace[?, D3], A, Sem](
      volume: NeuroVolume[S, A, Sem]
  ): SomeNeuroVolume[A, Sem] =
    volume

  private[image] inline def unsafeFromSampled[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[3]
      ]
  ): SomeNeuroVolume[A, Sem] =
    sampled

  def fromRavel[A, Sem](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroVolume[A, Sem]] =
    for
      sampleSpace <- SampleSpaces
        .requireSpatialD3(space)
        .left
        .map(NeuroImageError.Space.apply)
      sampled <- NeuroVolume
        .fromRavel[A, Sem](sampleSpace, data, metadata)
        .left
        .map(NeuroImageError.Image.apply)
    yield eraseSpace(sampled)

  def unsafeFromRavel[A, Sem](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): SomeNeuroVolume[A, Sem] =
    fromRavel[A, Sem](data, space, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeFromRavel[A, Sem](
      data: NDArray[A, Rank[3]],
      space: SomeSampleSpace,
      label: String
  )(using
      ValueSemantics[A, Sem]
  ): SomeNeuroVolume[A, Sem] =
    unsafeFromRavel[A, Sem](data, space, ImageMetadata.named(label))

  def copyFromCanonicalArray[A, Sem](
      data: Array[A],
      space: SomeSampleSpace,
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroVolume[A, Sem]] =
    val shape = space.spatialDims
    val expected = shape.product
    if data.length != expected then
      Left(
        NeuroImageError.LinearSizeMismatch(
          "NeuroVolume canonical array",
          expected,
          data.length
        )
      )
    else
      fromRavel[A, Sem](
        NDArray.fromSeq(Shape(shape(0), shape(1), shape(2)), data),
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
  ): SomeNeuroVolume[A, Sem] =
    copyFromCanonicalArray[A, Sem](data, space, ImageMetadata.named(label))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension [A, Sem](volume: SomeNeuroVolume[A, Sem])
    def sampled: Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Sem,
      Rank[3]
    ] =
      volume

    /** Provider-shaped read access without weakening the opaque admission boundary. */
    inline def data: NDArray[A, Rank[3]] =
      volume.data

    inline def grid: Grid[? <: Frame[D3], D3] =
      volume.grid

    inline def sampleSpace: SomeSampleSpace =
      SampleSpaces.fromCanonical(volume.sampleSpace)

    inline def metadata: ImageMetadata =
      volume.metadata

    inline def label: String =
      volume.metadata.label

    inline def values: NDArray[A, Rank[3]] =
      volume.data

    inline def space: SomeSampleSpace =
      SampleSpaces.fromCanonical(volume.sampleSpace)

    def ndim: Int =
      volume.data.rank

    def typedSpace: ImageSpace[Volume3D] =
      ImageSpace
        .make[Volume3D](space)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

    /** Exact spatial sample space retained by this semantic volume. */
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(SampleSpaces.fromCanonical(volume.sampleSpace))

    inline def apply(x: Int, y: Int, z: Int): A =
      volume.data(x, y, z)

    inline def apply(voxel: VoxelCoord): A =
      volume.data(voxel.x, voxel.y, voxel.z)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[3]]
    ] =
      CanonicalArray.from(volume.data)

    def materializedCanonical: SomeNeuroVolume[A, Sem] =
      unsafeFromSampled(volume.materializedCopy)

    def plane(
        axis: SpatialAxis,
        index: Int
    ): Either[NativeImageError, SomeNeuroVolume[A, Sem]] =
      val axisIndex = axis.index
      val extent = volume.grid.shape(axisIndex)
      if index < 0 || index >= extent then
        Left(NativeImageError.SpatialIndexOutOfBounds(axisIndex, index, extent))
      else
        val origin = Vector.tabulate(3)(i => if i == axisIndex then index else 0)
        val shape = Vector.tabulate(3)(i => if i == axisIndex then 1 else volume.grid.shape(i))
        volume
          .crop(origin, shape)
          .left
          .map(NativeImageError.Image.apply)
          .map(unsafeFromSampled)

    def gridToIndex(i: Int, j: Int, k: Int): Int =
      space.gridToIndex3D(i, j, k)

    def gridToIndex(voxel: VoxelCoord): Int =
      space.gridToIndex3D(voxel)

    def indexToGrid(index: Int): Vector[Int] =
      space.indexToGrid3D(index)

    def indexToVoxel(index: Int): VoxelCoord =
      space.indexToVoxel3D(index)

    private[scalafim] def valueAtCanonicalOrdinal(index: Int): A =
      val voxel = indexToVoxel(index)
      volume.data(voxel.x, voxel.y, voxel.z)

    def copyToCanonicalArray(using ClassTag[A]): Array[A] =
      val out = PrimitiveBuffers.ofSize[A](volume.data.size)
      var index = 0
      while index < out.length do
        out(index) = valueAtCanonicalOrdinal(index)
        index += 1
      out

    def asMatrix: NDArray[A, Rank[2]] =
      given DType[A] = volume.data.dtype
      val spatialSize = space.spatialDims.product
      NDArray.tabulate[A](spatialSize, 1)((index, _) => valueAtCanonicalOrdinal(index))

    def asLogical(using Ring[A]): SomeMaskVolume =
      val zero = summon[Ring[A]].zero
      val mapped =
        ravel.map(volume.data): value =>
          value match
            case d: Double => !d.isNaN && d != 0.0
            case f: Float => !f.isNaN && f != 0.0f
            case _ => value != zero
      unsafeFromRavel[Boolean, MaskSemantics](mapped, space, volume.metadata)

    def asMask(using Ring[A], Order[A]): SomeMaskVolume =
      val zero = summon[Ring[A]].zero
      val order = summon[Order[A]]
      val mapped =
        ravel.map(volume.data): value =>
          value match
            case d: Double => d.isFinite && d > 0.0
            case f: Float => f.isFinite && f > 0.0f
            case _ => order.gt(value, zero)
      unsafeFromRavel[Boolean, MaskSemantics](mapped, space, volume.metadata)

    def asMask(indices: ravel.Array1[Int], label: String): SomeMaskVolume =
      Mask.fromIndices(space, indices, label)

    def asMask(indices: Array[Int], label: String): SomeMaskVolume =
      Mask.fromIndices(space, indices, label)

    def asMask(indices: ravel.Array1[Int]): SomeMaskVolume =
      asMask(indices, volume.metadata.label)

    def asMask(indices: Array[Int]): SomeMaskVolume =
      asMask(indices, volume.metadata.label)

    def toSeries(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
      val shape = space.spatialDims
      val seriesSpace = space.spatialSpace.addDim(1, Some(Axis.Time))
      SomeNeuroSeries.unsafeFromRavel[A, Sem](
        volume.data.reshape(Shape(shape(0), shape(1), shape(2), 1)),
        seriesSpace,
        volume.metadata
      )

    def concatenate(
        that: SomeNeuroVolume[A, Sem],
        rest: SomeNeuroVolume[A, Sem]*
    )(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
      given DType[A] = volume.data.dtype
      val volumes = Vector(volume, that) ++ rest.toVector
      val base = space.spatialSpace
      volumes.foreach(value => GridCompatibility.requireSpatial(base, value.space))
      val shape = base.spatialDims
      val data =
        NDArray.tabulate[A](shape(0), shape(1), shape(2), volumes.length):
          (x, y, z, time) => volumes(time).data(x, y, z)
      SomeNeuroSeries.unsafeFromRavel[A, Sem](
        data,
        base.addDim(volumes.length, Some(Axis.Time)),
        volume.metadata
      )

    def mapValues[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroVolume[B, OutSem] =
      val mapped = ravel.map(volume.data)(f)
      Sampled
        .create[B, OutSem, Rank[3]](
          volume.sampleSpace,
          mapped,
          volume.metadata
        )
        .map(unsafeFromSampled)
        .fold(error => throw new IllegalStateException(error.message), identity)

    def map[B, OutSem](
        f: A => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroVolume[B, OutSem] =
      mapValues(f)

    def mapVoxels[B, OutSem](
        f: (VoxelCoord, A) => B
    )(using
        DType[B],
        ValueSemantics[B, OutSem]
    ): SomeNeuroVolume[B, OutSem] =
      val shape = space.spatialDims
      val mapped =
        NDArray.tabulate[B](shape(0), shape(1), shape(2)):
          (x, y, z) => f(VoxelCoord(x, y, z), volume.data(x, y, z))
      unsafeFromRavel[B, OutSem](mapped, space, volume.metadata)

    def zipExact[B, BSem, C, OutSem](
        that: SomeNeuroVolume[B, BSem]
    )(
        f: (A, B) => C
    )(using
        DType[C],
        ValueSemantics[C, OutSem]
    ): Either[GridMismatch, SomeNeuroVolume[C, OutSem]] =
      GridCompatibility.exact(space, that.space).map: _ =>
        val shape = space.spatialDims
        val data =
          NDArray.tabulate[C](shape(0), shape(1), shape(2)):
            (x, y, z) => f(volume.data(x, y, z), that.data(x, y, z))
        unsafeFromRavel[C, OutSem](data, space, volume.metadata)

    def traverseValues[F[_], B, OutSem](
        f: A => F[B]
    )(using
        Applicative[F],
        DType[B],
        ValueSemantics[B, OutSem]
    ): F[SomeNeuroVolume[B, OutSem]] =
      Vector
        .tabulate(volume.data.size)(valueAtCanonicalOrdinal)
        .traverse(f)
        .map: values =>
          val shape = space.spatialDims
          unsafeFromRavel[B, OutSem](
            NDArray.fromSeq(Shape(shape(0), shape(1), shape(2)), values),
            space,
            volume.metadata
          )

object NeuroVolume:
  /** Retain an already checked image4s value without allocating a wrapper. */
  inline def fromSampled[
      S <: SampleSpace[?, D3],
      A,
      Sem
  ](
      sampled: Sampled[S, A, Sem, Rank[3]]
  ): NeuroVolume[S, A, Sem] =
    sampled

  /** Build one image4s header while retaining the exact immutable Ravel owner. */
  def fromRavel[A, Sem](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[3]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[
    image4s.ImageError,
    NeuroVolume[sampleSpace.type, A, Sem]
  ] =
    Sampled
      .create[A, Sem, Rank[3]](sampleSpace, data, metadata)
      .map(fromSampled)

  def continuous[A](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[3]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[
    image4s.ImageError,
    ScalarVolume[sampleSpace.type, A]
  ] =
    fromRavel[A, Continuous](sampleSpace, data, metadata)

  def categorical[A](
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[A, Rank[3]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[
    image4s.ImageError,
    LabelVolume[sampleSpace.type, A]
  ] =
    fromRavel[A, Categorical](sampleSpace, data, metadata)

  def mask(
      sampleSpace: SampleSpace[?, D3],
      data: NDArray[Boolean, Rank[3]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[
    image4s.ImageError,
    MaskVolume[sampleSpace.type]
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
    ScalarVolume[sampleSpace.type, A]
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
    LabelVolume[sampleSpace.type, A]
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
    MaskVolume[sampleSpace.type]
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
    NeuroVolume[sampleSpace.type, A, Sem]
  ] =
    val shape = sampleSpace.grid.shape
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
          Shape(shape(0), shape(1), shape(2)),
          data
        )
      fromRavel[A, Sem](sampleSpace, copied, metadata)
        .left
        .map(NativeImageError.Image.apply)

  extension [S <: SampleSpace[?, D3], A, Sem](
      volume: NeuroVolume[S, A, Sem]
  )
    inline def sampled: Sampled[S, A, Sem, Rank[3]] =
      volume

    inline def apply(x: Int, y: Int, z: Int): A =
      volume.data(x, y, z)

    inline def apply(voxel: VoxelCoord): A =
      volume.data(voxel.x, voxel.y, voxel.z)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[3]]
    ] =
      CanonicalArray.from(volume.data)

    /** Explicitly copy logical values into a whole canonical Ravel owner. */
    def materializedCanonical: NeuroVolume[S, A, Sem] =
      fromSampled(volume.materializedCopy)

    def withImageMetadata(
        metadata: ImageMetadata
    ): NeuroVolume[S, A, Sem] =
      fromSampled(volume.withMetadata(metadata))

    def cropVolume(
        origin: Vector[Int],
        shape: Vector[Int]
    ): Either[image4s.ImageError, SomeNeuroVolume[A, Sem]] =
      volume.crop(origin, shape).map(SomeNeuroVolume.unsafeFromSampled)

    def flipVolume(
        axis: Int
    ): Either[image4s.ImageError, SomeNeuroVolume[A, Sem]] =
      volume.flipSpatial(axis).map(SomeNeuroVolume.unsafeFromSampled)

    def permuteVolume(
        sourceAxisForTarget: IterableOnce[Int]
    ): Either[image4s.ImageError, SomeNeuroVolume[A, Sem]] =
      volume
        .permuteSpatial(sourceAxisForTarget)
        .map(SomeNeuroVolume.unsafeFromSampled)

    def strideVolume(
        steps: IterableOnce[Int]
    ): Either[image4s.ImageError, SomeNeuroVolume[A, Sem]] =
      volume.strideSpatial(steps).map(SomeNeuroVolume.unsafeFromSampled)

    /** Select an affine-honest plane as a zero-copy singleton-D3 view. */
    def plane(
        axis: Int,
        index: Int
    ): Either[NativeImageError, SomeNeuroVolume[A, Sem]] =
      if axis < 0 || axis >= 3 then
        Left(NativeImageError.SpatialAxisOutOfBounds(axis))
      else
        val extent = volume.grid.shape(axis)
        if index < 0 || index >= extent then
          Left(
            NativeImageError.SpatialIndexOutOfBounds(
              axis,
              index,
              extent
            )
          )
        else
          val origin = Vector.tabulate(3)(i => if i == axis then index else 0)
          val shape = Vector.tabulate(3)(i => if i == axis then 1 else volume.grid.shape(i))
          cropVolume(origin, shape).left.map(NativeImageError.Image.apply)
