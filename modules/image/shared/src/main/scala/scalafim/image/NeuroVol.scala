package scalafim.image

import cats.Applicative
import cats.syntax.all.*
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import ravel.map
import scala.reflect.ClassTag
import spire.algebra.{Order, Ring}

opaque type NeuroVol[A] = AnyNeuroVolume[A]

object NeuroVol:
  private[image] def fromPacked[A](
      volume: Image4sInterop.PackedVolume[A]
  ): NeuroVol[A] =
    volume

  extension [A](volume: NeuroVol[A])
    /** Zero-wrapper escape from the temporary compatibility name. */
    inline def toNative: AnyNeuroVolume[A] =
      volume

    inline def label: String =
      volume.metadata.label

    /** Canonical dense storage. This is the same Ravel value retained by
      * `sampled`; no second dense buffer is cached alongside it.
      */
    inline def values: RavelArray[A, Rank[3]] =
      volume.data

    /** The image4s representation underlying volume compatibility view. */
    inline def sampled: image4s.Sampled[
      ? <: image4s.SampleSpace[?, image4s.geometry.D3],
      A,
      ?,
      Rank[3]
    ] =
      volume

    /** Zero-wrapper ScalaFIM name for the geometry retained by `sampled`. */
    inline def space: NeuroSpace =
      NeuroSpace.fromCanonical(volume.sampleSpace)

    def ndim: Int =
      values.rank

    def typedSpace: ImageSpace[Volume3D] =
      ImageSpace
        .make[Volume3D](space)
        .fold(err => throw new IllegalArgumentException(err.message), identity)

    def volumeSpace: VolumeSpace =
      VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)

    inline def apply(i: Int, j: Int, k: Int): A =
      values(i, j, k)

    inline def apply(coord: VoxelCoord): A =
      apply(coord.x, coord.y, coord.z)

    /** Temporary old-name bridge to the native singleton-D3 plane view. */
    def plane(
        axis: SpatialAxis,
        index: Int
    ): Either[NativeImageError, AnyNeuroVolume[A]] =
      AnyNeuroVolume.plane(volume)(axis.index, index)

    def toVec(using
        ClassTag[A],
        MigrationValueSemantics[A]
    ): NeuroVec[A] =
      val shape = space.spatialDims
      val newSpace = space.spatialSpace.addDim(1, Some(Axis.Time))
      NeuroVec.fromRavel(
        values.reshape(Shape(shape(0), shape(1), shape(2), 1)),
        newSpace,
        label
      )

    def concat(that: NeuroVol[A], rest: NeuroVol[A]*)(using
        ClassTag[A],
        MigrationValueSemantics[A]
    ): NeuroVec[A] =
      given DType[A] = values.dtype
      val all = Vector(volume, that) ++ rest.toVector
      val base = all.head.space.spatialSpace
      all.foreach(v => GridCompatibility.requireSpatial(base, v.space))

      val totalT = all.length
      val shape = base.spatialDims
      val out =
        RavelArray.tabulate[A](
          shape(0),
          shape(1),
          shape(2),
          totalT
        )((i, j, k, t) => all(t)(i, j, k))
      val newSpace = base.addDim(totalT, Some(Axis.Time))
      NeuroVec.fromRavel(out, newSpace, label)

    def asMatrix: RavelArray[A, Rank[2]] =
      given DType[A] = values.dtype
      val spatialNels = space.spatialDims.product
      RavelArray.tabulate[A](spatialNels, 1)((index, _) => valueAtCanonicalOrdinal(index))

    def asLogical(using Ring[A], ClassTag[Boolean]): NeuroVol[Boolean] =
      val zero = summon[Ring[A]].zero
      map { a =>
        a match
          case d: Double => !d.isNaN && d != 0.0
          case f: Float => !f.isNaN && f != 0.0f
          case _ => a != zero
      }

    def asMask(using Ring[A], Order[A], ClassTag[Boolean]): NeuroVol[Boolean] =
      val zero = summon[Ring[A]].zero
      val ord = summon[Order[A]]
      map { a =>
        a match
          case d: Double => d.isFinite && d > 0.0
          case f: Float => f.isFinite && f > 0.0f
          case _ => ord.gt(a, zero)
      }

    def asMask(indices: Array1[Int], label: String): NeuroVol[Boolean] =
      Mask.fromIndices(space, indices, label)

    def asMask(indices: Array[Int], label: String): NeuroVol[Boolean] =
      Mask.fromIndices(space, indices, label)

    def asMask(indices: Array1[Int]): NeuroVol[Boolean] =
      asMask(indices, volume.label)

    def asMask(indices: Array[Int]): NeuroVol[Boolean] =
      asMask(indices, volume.label)

    def gridToIndex(i: Int, j: Int, k: Int): Int =
      space.gridToIndex3D(i, j, k)

    def gridToIndex(coord: VoxelCoord): Int =
      space.gridToIndex3D(coord)

    def indexToGrid(idx: Int): Vector[Int] =
      space.indexToGrid3D(idx)

    def indexToVoxel(idx: Int): VoxelCoord =
      space.indexToVoxel3D(idx)

    private[scalafim] def valueAtCanonicalOrdinal(i: Int): A =
      val voxel = space.indexToVoxel3D(i)
      apply(voxel)

    /** Explicitly copy logical values in canonical last-axis-fastest order. */
    def copyToCanonicalArray(using ClassTag[A]): Array[A] =
      val out = PrimitiveBuffers.ofSize[A](values.size)
      var index = 0
      while index < out.length do
        out(index) = valueAtCanonicalOrdinal(index)
        index += 1
      out

    def mapValues[B](f: A => B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVol[B] =
      NeuroVol.fromRavel(ravel.map(values)(f), space, label)

    def mapVoxels[B](f: (VoxelCoord, A) => B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVol[B] =
      val shape = space.spatialDims
      val out =
        RavelArray.tabulate[B](shape(0), shape(1), shape(2)) {
          (i, j, k) =>
            val voxel = VoxelCoord(i, j, k)
            f(voxel, apply(voxel))
        }
      NeuroVol.fromRavel(out, space, label)

    def zipWith[B, C](
        that: NeuroVol[B]
    )(
        f: (A, B) => C
    )(using
        ClassTag[C],
        DType[C],
        MigrationValueSemantics[C]
    ): Either[GridMismatch, NeuroVol[C]] =
      GridCompatibility.exact(space, that.space).map: _ =>
        val shape = space.spatialDims
        val out =
          RavelArray.tabulate[C](shape(0), shape(1), shape(2)) {
            (i, j, k) => f(apply(i, j, k), that(i, j, k))
          }
        NeuroVol.fromRavel(out, space, label)

    def traverseValues[F[_], B](
        f: A => F[B]
    )(using
        Applicative[F],
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): F[NeuroVol[B]] =
      Vector
        .tabulate(values.size)(valueAtCanonicalOrdinal)
        .traverse(f)
        .map: traversed =>
          val shape = space.spatialDims
          NeuroVol.fromRavel(
            RavelArray.fromSeq(
              Shape(shape(0), shape(1), shape(2)),
              traversed
            ),
            space,
            label
          )

    def map[B](f: A => B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVol[B] =
      mapValues(f)

    def reconstruct(
        values: RavelArray[A, Rank[3]] = volume.values,
        space: NeuroSpace = volume.space,
        label: String = volume.label
    )(using MigrationValueSemantics[A]): Either[NeuroImageError, NeuroVol[A]] =
      NeuroVol.makeRavel(values, space, label)

    private[scalafim] def copy(
        values: RavelArray[A, Rank[3]] = volume.values,
        space: NeuroSpace = volume.space,
        label: String = volume.label
    )(using MigrationValueSemantics[A]): NeuroVol[A] =
      NeuroVol.fromRavel(values, space, label)

  def makeRavel[A](
      values: RavelArray[A, Rank[3]],
      space: NeuroSpace,
      label: String = ""
  )(using MigrationValueSemantics[A]): Either[NeuroImageError, NeuroVol[A]] =
    Image4sInterop
      .volumeFromRavel(values, space, label)
      .map(value => value)

  def fromRavel[A](
      values: RavelArray[A, Rank[3]],
      space: NeuroSpace,
      label: String = ""
  )(using MigrationValueSemantics[A]): NeuroVol[A] =
    makeRavel(values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private[scalafim] def copyFromCanonicalArrayChecked[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      MigrationValueSemantics[A]
  ): Either[NeuroImageError, NeuroVol[A]] =
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
      makeRavel(
        RavelArray.fromSeq(
          Shape(shape(0), shape(1), shape(2)),
          data
        ),
        space,
        label
      )

  private[scalafim] def copyFromCanonicalArray[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    copyFromCanonicalArrayChecked(data, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
