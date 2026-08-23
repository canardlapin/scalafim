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
import ravel.NDArray
import ravel.NonContiguousLayout
import ravel.Rank

/** A zero-wrapper three-dimensional neuroimaging refinement.
  *
  * The value is the exact image4s `Sampled` object supplied at construction.
  * Its only dense owner is the retained Ravel array. The static D3 plus rank-3
  * contract implies that the sample space has no non-spatial axes.
  */
opaque type NeuroVolume[
    S <: SampleSpace[?, D3],
    A,
    Sem
] <: Sampled[S, A, Sem, Rank[3]] = Sampled[S, A, Sem, Rank[3]]

opaque type SomeNeuroVolume[A, Sem] <:
    Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Sem,
      Rank[3]
    ] =
  Sampled[? <: SampleSpace[?, D3], A, Sem, Rank[3]]

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

object SomeNeuroVolume:
  private[image] inline def unsafeFromSampled[A, Sem](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Sem,
        Rank[3]
      ]
  ): SomeNeuroVolume[A, Sem] =
    sampled

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
      NonContiguousLayout,
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
