package scalafim.image

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

/** A zero-wrapper three-dimensional neuroimaging refinement.
  *
  * The value is the exact image4s `Sampled` object supplied at construction.
  * Its only dense owner is the retained Ravel array. The static D3 plus rank-3
  * contract implies that the sample space has no non-spatial axes.
  */
opaque type AnyNeuroVolume[A] <:
    Sampled[
      ? <: SampleSpace[?, D3],
      A,
      ?,
      Rank[3]
    ] =
  Sampled[? <: SampleSpace[?, D3], A, ?, Rank[3]]

opaque type SomeNeuroVolume[A, Sem] <:
    AnyNeuroVolume[A] & Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Sem,
      Rank[3]
    ] =
  Sampled[? <: SampleSpace[?, D3], A, Sem, Rank[3]]

opaque type NeuroVolume[
    S <: SampleSpace[?, D3],
    A,
    Sem
] <: SomeNeuroVolume[A, Sem] & Sampled[S, A, Sem, Rank[3]] =
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

object AnyNeuroVolume:
  private[image] inline def unsafeFromSampled[A](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        ?,
        Rank[3]
      ]
  ): AnyNeuroVolume[A] =
    sampled

  inline def eraseSemantics[A, Sem](
      volume: SomeNeuroVolume[A, Sem]
  ): AnyNeuroVolume[A] =
    volume

  extension [A](volume: AnyNeuroVolume[A])
    /** Exact spatial sample space retained by this native volume. */
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(NeuroSpace.fromCanonical(volume.sampleSpace))

    inline def apply(x: Int, y: Int, z: Int): A =
      volume.data(x, y, z)

    inline def apply(voxel: VoxelCoord): A =
      volume.data(voxel.x, voxel.y, voxel.z)

    def wholeCanonical: Either[
      CanonicalLayoutError,
      CanonicalArray[A, Rank[3]]
    ] =
      CanonicalArray.from(volume.data)

    def materializedCanonical: AnyNeuroVolume[A] =
      volume.materializedCopy

    /** Select an affine-honest plane as a zero-copy singleton-D3 view. */
    def plane(
        axis: Int,
        index: Int
    ): Either[NativeImageError, AnyNeuroVolume[A]] =
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
          volume
            .crop(origin, shape)
            .left
            .map(NativeImageError.Image.apply)
            .map(unsafeFromSampled)

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

  extension [A, Sem](volume: SomeNeuroVolume[A, Sem])
    /** Exact spatial sample space retained by this semantic volume. */
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(NeuroSpace.fromCanonical(volume.sampleSpace))

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
