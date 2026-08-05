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

opaque type NeuroVol[A] = Image4sInterop.PackedVolume[A]

object NeuroVol:
  private[image] def fromPacked[A](
      volume: Image4sInterop.PackedVolume[A]
  ): NeuroVol[A] =
    volume

  extension [A](volume: NeuroVol[A])
    inline def label: String =
      volume.metadata.label

    /** Canonical dense storage. This is the same Ravel value retained by
      * `sampled`; no legacy dense buffer is cached alongside it.
      */
    inline def values: RavelArray[A, Rank[3]] =
      volume.data

    /** The image4s representation underlying volume compatibility view. */
    inline def sampled: image4s.Sampled[
      ? <: image4s.SampleSpace[?, ?],
      A,
      ScalaFimValues,
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

    def apply(coords: ROICoords): Array1[A] =
      val dims = space.spatialDims
      given DType[A] = values.dtype
      RavelArray.tabulate[A](coords.size): p =>
        val c = coords.coords(p)
        require(c(0) >= 0 && c(0) < dims(0), "roi coord out of bounds")
        require(c(1) >= 0 && c(1) < dims(1), "roi coord out of bounds")
        require(c(2) >= 0 && c(2) < dims(2), "roi coord out of bounds")
        apply(c(0), c(1), c(2))

    def apply(roi: VoxelRoi): Array1[A] =
      GridCompatibility.requireVolume(volumeSpace, roi.space)
      given DType[A] = values.dtype
      val coords = roi.coords
      RavelArray.tabulate[A](coords.length)(p => apply(coords(p)))

    def apply(roi: ROIVol[?]): Array1[A] =
      apply(roi.roi)

    def select(selection: VoxelSelection): Either[GridMismatch, RoiValues[A]] =
      GridCompatibility.volume(volumeSpace, selection.space).map: _ =>
        val indices = selection.indexSet.unsafeArray
        given DType[A] = values.dtype
        val out =
          RavelArray.tabulate[A](indices.size)(i => linear(indices(i)))
        RoiValues.unsafe(selection, out, label)

    def select(region: VoxelRegion): Either[GridMismatch, RoiValues[A]] =
      select(region.toSelection)

    def select(roi: VoxelRoi): Either[GridMismatch, RoiValues[A]] =
      select(VoxelSelection.fromRoi(roi))

    def slices(axis: Int = 2)(using ClassTag[A]): Vector[NeuroSlice[A]] =
      slices(SpatialAxis.unsafe(axis))

    def slices(axis: SpatialAxis)(using ClassTag[A]): Vector[NeuroSlice[A]] =
      val dims = space.spatialShape
      Vector.tabulate(dims(axis))(i => slice(axis, i))

    def toVec(using ClassTag[A]): NeuroVec[A] =
      val shape = space.spatialDims
      val newSpace = space.spatialSpace.addDim(1, Some(Axis.Time))
      NeuroVec.fromRavel(
        values.reshape(Shape(shape(0), shape(1), shape(2), 1)),
        newSpace,
        label
      )

    def concat(that: NeuroVol[A], rest: NeuroVol[A]*)(using
        ClassTag[A]
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
      RavelArray.tabulate[A](spatialNels, 1)((index, _) => linear(index))

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
      Mask.fromIndexSet(VoxelIndexSet(volumeSpace, indices), label)

    def asMask(indices: Array[Int], label: String): NeuroVol[Boolean] =
      Mask.fromIndexSet(VoxelIndexSet(volumeSpace, indices), label)

    def asMask(indexSet: VoxelIndexSet, label: String): NeuroVol[Boolean] =
      GridCompatibility.requireVolume(volumeSpace, indexSet.space)
      Mask.fromIndexSet(indexSet, label)

    def asMask(indexSet: VoxelIndexSet): NeuroVol[Boolean] =
      asMask(indexSet, volume.label)

    def asMask(indices: Array1[Int]): NeuroVol[Boolean] =
      asMask(indices, volume.label)

    def asMask(indices: Array[Int]): NeuroVol[Boolean] =
      asMask(indices, volume.label)

    def asSparse(mask: NeuroVol[Boolean], label: String = volume.label)(using ClassTag[A]): SparseNeuroVol[A] =
      GridCompatibility.requireSpatial(space, mask.space)
      val idx = Mask.indices(mask)
      asSparse(idx, label)

    def asSparse(indices: Array1[Int]): SparseNeuroVol[A] =
      asSparse(indices, volume.label)

    def asSparse(indices: Array[Int]): SparseNeuroVol[A] =
      asSparse(
        RavelArray.fromSeq(ravel.Shape(indices.length), indices),
        volume.label
      )

    def asSparse(indices: Array1[Int], label: String): SparseNeuroVol[A] =
      val indexSet = VoxelIndexSet.unique(volumeSpace, indices)
      asSparse(indexSet, label)

    def asSparse(indices: Array[Int], label: String): SparseNeuroVol[A] =
      asSparse(
        RavelArray.fromSeq(ravel.Shape(indices.length), indices),
        label
      )

    def asSparse(indexSet: VoxelIndexSet): SparseNeuroVol[A] =
      asSparse(indexSet, volume.label)

    def asSparse(indexSet: VoxelIndexSet, label: String): SparseNeuroVol[A] =
      GridCompatibility.requireVolume(volumeSpace, indexSet.space)
      given DType[A] = values.dtype
      val out =
        RavelArray.tabulate[A](indexSet.size)(p => linear(indexSet(p)))
      SparseNeuroVol.fromIndexSet(out, indexSet, space, label)

    def gridToIndex(i: Int, j: Int, k: Int): Int =
      space.gridToIndex3D(i, j, k)

    def gridToIndex(coord: VoxelCoord): Int =
      space.gridToIndex3D(coord)

    def indexToGrid(idx: Int): Vector[Int] =
      space.indexToGrid3D(idx)

    def indexToVoxel(idx: Int): VoxelCoord =
      space.indexToVoxel3D(idx)

    def linear(i: Int): A =
      val voxel = space.indexToVoxel3D(i)
      apply(voxel)

    /** Explicit compatibility export in ScalaFIM's historical
      * first-axis-fastest order. This always materializes a fresh buffer.
      */
    def copyLegacyLinear(using ClassTag[A]): Array[A] =
      val out = PrimitiveBuffers.ofSize[A](values.size)
      var index = 0
      while index < out.length do
        out(index) = linear(index)
        index += 1
      out

    def slice(axis: Int, index: Int)(using ClassTag[A]): NeuroSlice[A] =
      slice(SpatialAxis.unsafe(axis), index)

    def slice(axis: SpatialAxis, index: Int)(using ClassTag[A]): NeuroSlice[A] =
      val dims = space.spatialShape
      require(index >= 0 && index < dims(axis), "index out of bounds")
      val outDims =
        axis match
          case SpatialAxis.X => Vector(dims.y, dims.z)
          case SpatialAxis.Y => Vector(dims.x, dims.z)
          case SpatialAxis.Z => Vector(dims.x, dims.y)

      val out = Array.ofDim[A](outDims.product)
      var idx = 0
      axis match
        case SpatialAxis.X =>
          var y = 0
          while y < dims.y do
            var z = 0
            while z < dims.z do
              out(idx) = values(index, y, z)
              idx += 1
              z += 1
            y += 1
        case SpatialAxis.Y =>
          var x = 0
          while x < dims.x do
            var z = 0
            while z < dims.z do
              out(idx) = values(x, index, z)
              idx += 1
              z += 1
            x += 1
        case SpatialAxis.Z =>
          var x = 0
          while x < dims.x do
            var y = 0
            while y < dims.y do
              out(idx) = values(x, y, index)
              idx += 1
              y += 1
            x += 1

      val sliceSpace = space.dropDim(axis.index)
      given DType[A] = values.dtype
      NeuroSlice.fromLinear(out, sliceSpace, label)

    def mapValues[B](f: A => B)(using
        ClassTag[B],
        DType[B]
    ): NeuroVol[B] =
      NeuroVol.fromRavel(ravel.map(values)(f), space, label)

    def mapVoxels[B](f: (VoxelCoord, A) => B)(using
        ClassTag[B],
        DType[B]
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
    )(using ClassTag[C], DType[C]): Either[GridMismatch, NeuroVol[C]] =
      GridCompatibility.exact(space, that.space).map: _ =>
        val shape = space.spatialDims
        val out =
          RavelArray.tabulate[C](shape(0), shape(1), shape(2)) {
            (i, j, k) => f(apply(i, j, k), that(i, j, k))
          }
        NeuroVol.fromRavel(out, space, label)

    def traverseValues[F[_], B](
        f: A => F[B]
    )(using Applicative[F], ClassTag[B], DType[B]): F[NeuroVol[B]] =
      Vector
        .tabulate(values.size)(linear)
        .traverse(f)
        .map(values => NeuroVol.fromLinear(PrimitiveBuffers.fromArray(values.toArray), space, label))

    def map[B](f: A => B)(using ClassTag[B], DType[B]): NeuroVol[B] =
      mapValues(f)

    def reconstruct(
        values: RavelArray[A, Rank[3]] = volume.values,
        space: NeuroSpace = volume.space,
        label: String = volume.label
    ): Either[NeuroImageError, NeuroVol[A]] =
      NeuroVol.makeRavel(values, space, label)

    private[scalafim] def copy(
        values: RavelArray[A, Rank[3]] = volume.values,
        space: NeuroSpace = volume.space,
        label: String = volume.label
    ): NeuroVol[A] =
      NeuroVol.fromRavel(values, space, label)

  def makeRavel[A](
      values: RavelArray[A, Rank[3]],
      space: NeuroSpace,
      label: String = ""
  ): Either[NeuroImageError, NeuroVol[A]] =
    Image4sInterop
      .volumeFromRavel(values, space, label)
      .map(value => value)

  def fromRavel[A](
      values: RavelArray[A, Rank[3]],
      space: NeuroSpace,
      label: String = ""
  ): NeuroVol[A] =
    makeRavel(values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def fromLinearChecked[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): Either[NeuroImageError, NeuroVol[A]] =
    importLegacyLinear(data, space, label).map(_.image)

  /** Checked legacy-linear ingress with an explicit materialization receipt. */
  def importLegacyLinear[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): Either[
    NeuroImageError,
    DenseImageImport[NeuroVol[A]]
  ] =
    Image4sInterop
      .volumeFromLegacyLinear(data, space, label)
      .map(volume =>
        DenseImageImport(
          volume,
          Image4sStorageTransfer.CanonicalizedLegacy
        )
      )

  private[scalafim] def fromLinear[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): NeuroVol[A] =
    fromLinearChecked(data, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
