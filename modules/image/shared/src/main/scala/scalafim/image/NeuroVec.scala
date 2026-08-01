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

opaque type NeuroVec[A] = Image4sInterop.PackedSeries[A]

object NeuroVec:
  extension [A](vector: NeuroVec[A])
    inline def label: String =
      vector.metadata.label

    /** Canonical dense storage. This is the same Ravel value retained by
      * `sampled`; no legacy dense buffer is cached alongside it.
      */
    inline def values: RavelArray[A, Rank[4]] =
      vector.data

    /** The image4s representation underlying vector compatibility view. */
    inline def sampled: image4s.Sampled[
      ? <: image4s.geometry.Frame[image4s.geometry.D3],
      image4s.geometry.D3,
      A,
      image4s.FieldRole,
      Rank[4]
    ] =
      vector

    /** Zero-wrapper ScalaFIM name for the geometry retained by `sampled`. */
    inline def space: NeuroSpace =
      NeuroSpace.fromCanonical(vector.sampleSpace)

    def ndim: Int =
      values.rank

    def typedSpace: ImageSpace[Series4D] =
      ImageSpace
        .make[Series4D](space)
        .fold(err => throw new IllegalArgumentException(err.message), identity)

    def seriesSpace: SeriesSpace =
      SeriesSpace.make(space).fold(err => throw new IllegalArgumentException(err.message), series => series)

    def nVolumes: Int = values.shape(3)

    inline def apply(i: Int, j: Int, k: Int, t: Int): A =
      values(i, j, k, t)

    def apply(t: Int)(using ClassTag[A]): NeuroVol[A] =
      volume(t)

    def apply(ts: Seq[Int])(using ClassTag[A]): NeuroVec[A] =
      subVector(ts)

    def gridToIndex(i: Int, j: Int, k: Int, t: Int): Int =
      Indexing.gridToIndex(space.dims.take(4), Vector(i, j, k, t))

    def indexToGrid(idx: Int): Vector[Int] =
      Indexing.indexToGrid(space.dims.take(4), idx)

    def linear(i: Int): A =
      val index = indexToGrid(i)
      apply(index(0), index(1), index(2), index(3))

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

    def asMatrix: RavelArray[A, Rank[2]] =
      given DType[A] = values.dtype
      val spatialNels = space.spatialDims.product
      RavelArray.tabulate[A](spatialNels, nVolumes) { (index, t) =>
        val voxel = space.indexToVoxel3D(index)
        apply(voxel.x, voxel.y, voxel.z, t)
      }

    def subArray(
      i: Seq[Int],
      j: Seq[Int],
      k: Seq[Int],
      t: Seq[Int]
    )(using ClassTag[A]): RavelArray[A, Rank[4]] =
      val dims = space.dims.take(4)
      require(i.forall(ii => ii >= 0 && ii < dims(0)), "i index out of bounds")
      require(j.forall(jj => jj >= 0 && jj < dims(1)), "j index out of bounds")
      require(k.forall(kk => kk >= 0 && kk < dims(2)), "k index out of bounds")
      require(t.forall(tt => tt >= 0 && tt < dims(3)), "t index out of bounds")

      given DType[A] = values.dtype
      RavelArray.tabulate[A](i.length, j.length, k.length, t.length) {
        (ii, jj, kk, tt) =>
          apply(i(ii), j(jj), k(kk), t(tt))
      }

    def volume(t: Int)(using ClassTag[A]): NeuroVol[A] =
      require(t >= 0 && t < nVolumes, "t out of bounds")
      val selected =
        Image4sInterop
          .volumeFromSeriesView(vector, t)
          .fold(err => throw new IllegalArgumentException(err.message), identity)
      NeuroVol.fromPacked(selected)

    def series(linearSpatial: Int)(using ClassTag[A]): Array[A] =
      val spatialNels = space.spatialDims.product
      require(linearSpatial >= 0 && linearSpatial < spatialNels, "spatial index out of bounds")
      val tLen = nVolumes
      val out = Array.ofDim[A](tLen)
      val voxel = space.indexToVoxel3D(linearSpatial)
      var t = 0
      while t < tLen do
        out(t) = apply(voxel.x, voxel.y, voxel.z, t)
        t += 1
      out

    def series(i: Int, j: Int, k: Int)(using ClassTag[A]): Array[A] =
      val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
      series(lin)

    def series(linearSpatial: Array1[Int]): RavelArray[A, Rank[2]] =
      val spatialNels = space.spatialDims.product
      val tLen = nVolumes
      val nVox = linearSpatial.size
      given DType[A] = values.dtype
      RavelArray.tabulate[A](tLen, nVox) { (time, position) =>
        val lin = linearSpatial(position)
        require(lin >= 0 && lin < spatialNels, "spatial index out of bounds")
        val voxel = space.indexToVoxel3D(lin)
        apply(voxel.x, voxel.y, voxel.z, time)
      }

    def series(linearSpatial: Array[Int]): RavelArray[A, Rank[2]] =
      series(RavelArray.fromSeq(Shape(linearSpatial.length), linearSpatial))

    def series(indexSet: VoxelIndexSet): RavelArray[A, Rank[2]] =
      GridCompatibility.requireVolume(seriesSpace.volumeSpace, indexSet.space)
      series(indexSet.unsafeArray)

    def series(roi: VoxelRoi): RavelArray[A, Rank[2]] =
      GridCompatibility.requireVolume(seriesSpace.volumeSpace, roi.space)
      series(roi.linearIndexSet.unsafeArray)

    def series(roi: ROICoords): RavelArray[A, Rank[2]] =
      series(roi.linearIndices(space.spatialSpace))

    def series(coords: Vector[Vector[Int]]): RavelArray[A, Rank[2]] =
      series(ROICoords(coords))

    def series(mask: NeuroVol[Boolean]): RavelArray[A, Rank[2]] =
      series(Mask.indexSet(mask))

    def seriesRoi(roi: ROICoords): ROIVec[A] =
      val lin = roi.linearIndices(space.spatialSpace)
      ROIVec(space, roi, series(lin))

    def select(selection: VoxelSelection): Either[GridMismatch, RoiSeries[A]] =
      GridCompatibility.volume(seriesSpace.volumeSpace, selection.space).map: _ =>
        val selected = series(selection.indexSet.unsafeArray)
        RoiSeries.unsafe(seriesSpace, selection, selected, label)

    def select(region: VoxelRegion): Either[GridMismatch, RoiSeries[A]] =
      select(region.toSelection)

    def select(roi: VoxelRoi): Either[GridMismatch, RoiSeries[A]] =
      select(VoxelSelection.fromRoi(roi))

    def asSparse(mask: NeuroVol[Boolean], label: String = vector.label)(using ClassTag[A]): SparseNeuroVec[A] =
      SparseNeuroVec.fromDense(vector, mask, label)

    def asSparse(indices: Array1[Int])(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(indices, vector.label)

    def asSparse(indices: Array1[Int], label: String)(using ClassTag[A]): SparseNeuroVec[A] =
      val indexSet = VoxelIndexSet.unique(seriesSpace.volumeSpace, indices)
      asSparse(indexSet, label)

    def asSparse(indices: Array[Int])(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(
        RavelArray.fromSeq(Shape(indices.length), indices),
        vector.label
      )

    def asSparse(indices: Array[Int], label: String)(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(RavelArray.fromSeq(Shape(indices.length), indices), label)

    def asSparse(indexSet: VoxelIndexSet)(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(indexSet, vector.label)

    def asSparse(indexSet: VoxelIndexSet, label: String)(using ClassTag[A]): SparseNeuroVec[A] =
      GridCompatibility.requireVolume(seriesSpace.volumeSpace, indexSet.space)
      SparseNeuroVec.fromDense(
        vector,
        SparseSupport.fromIndexSet(indexSet),
        label
      )

    def asSparse(roi: ROICoords)(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(roi, vector.label)

    def asSparse(roi: ROICoords, label: String)(using ClassTag[A]): SparseNeuroVec[A] =
      asSparse(roi.linearIndices(space.spatialSpace), label)

    def splitClusters(clusters: ClusteredNeuroVol)(using ClassTag[A]): Vector[ROIVec[A]] =
      GridCompatibility.requireSpatial(space, clusters.space)
      clusters.clusterMap.toVector.sortBy(_._1).map { case (_, idx) =>
        val coords = Vector.tabulate(idx.size)(i => Indexing.indexToGrid3D(space.spatialDims, idx(i)))
        ROIVec(space, ROICoords(coords), series(idx))
      }

    def subVector(ts: Seq[Int])(using ClassTag[A]): NeuroVec[A] =
      given DType[A] = values.dtype
      val tLen = nVolumes
      require(ts.nonEmpty, "ts must be non-empty")
      require(ts.forall(t => t >= 0 && t < tLen), "time index out of bounds")
      val shape = space.spatialDims
      val out =
        RavelArray.tabulate[A](
          shape(0),
          shape(1),
          shape(2),
          ts.length
        )((i, j, k, p) => apply(i, j, k, ts(p)))
      val newSpace = space.spatialSpace.addDim(ts.length, Some(Axis.Time))
      NeuroVec.fromRavel(out, newSpace, label)

    def concat(that: NeuroVec[A], rest: NeuroVec[A]*)(using
        ClassTag[A]
    ): NeuroVec[A] =
      given DType[A] = values.dtype
      val all = Vector(vector, that) ++ rest.toVector
      all.foreach(v => GridCompatibility.requireSpatial(space, v.space))

      val totalT = all.map(_.nVolumes).sum
      val boundaries =
        all.scanLeft(0)((offset, vector) => offset + vector.nVolumes)
      val shape = space.spatialDims
      val out =
        RavelArray.tabulate[A](
          shape(0),
          shape(1),
          shape(2),
          totalT
        ) { (i, j, k, t) =>
          var block = 0
          while boundaries(block + 1) <= t do
            block += 1
          all(block)(i, j, k, t - boundaries(block))
        }
      val newSpace = space.spatialSpace.addDim(totalT, Some(Axis.Time))
      NeuroVec.fromRavel(out, newSpace, label)

    def mapValues[B](f: A => B)(using
        ClassTag[B],
        DType[B]
    ): NeuroVec[B] =
      NeuroVec.fromRavel(ravel.map(values)(f), space, label)

    def mapVoxels[B](f: (VoxelCoord, A) => B)(using
        ClassTag[B],
        DType[B]
    ): NeuroVec[B] =
      val shape = space.spatialDims
      val out =
        RavelArray.tabulate[B](
          shape(0),
          shape(1),
          shape(2),
          nVolumes
        ) { (i, j, k, t) =>
          val voxel = VoxelCoord(i, j, k)
          f(voxel, apply(i, j, k, t))
        }
      NeuroVec.fromRavel(out, space, label)

    def mapSamples[B](
        f: (VoxelCoord, Int, A) => B
    )(using ClassTag[B], DType[B]): NeuroVec[B] =
      val shape = space.spatialDims
      val out =
        RavelArray.tabulate[B](
          shape(0),
          shape(1),
          shape(2),
          nVolumes
        ) { (i, j, k, t) =>
          f(VoxelCoord(i, j, k), t, apply(i, j, k, t))
        }
      NeuroVec.fromRavel(out, space, label)

    def zipWith[B, C](
        that: NeuroVec[B]
    )(
        f: (A, B) => C
    )(using ClassTag[C], DType[C]): Either[GridMismatch, NeuroVec[C]] =
      GridCompatibility.exact(space, that.space).map: _ =>
        val shape = space.spatialDims
        val out =
          RavelArray.tabulate[C](
            shape(0),
            shape(1),
            shape(2),
            nVolumes
          )((i, j, k, t) => f(apply(i, j, k, t), that(i, j, k, t)))
        NeuroVec.fromRavel(out, space, label)

    def traverseValues[F[_], B](
        f: A => F[B]
    )(using Applicative[F], ClassTag[B], DType[B]): F[NeuroVec[B]] =
      Vector
        .tabulate(values.size)(linear)
        .traverse(f)
        .map(values => NeuroVec.fromLinear(PrimitiveBuffers.fromArray(values.toArray), space, label))

    def map[B](f: A => B)(using ClassTag[B], DType[B]): NeuroVec[B] =
      mapValues(f)

    def reconstruct(
        values: RavelArray[A, Rank[4]] = vector.values,
        space: NeuroSpace = vector.space,
        label: String = vector.label
    ): Either[NeuroImageError, NeuroVec[A]] =
      NeuroVec.makeRavel(values, space, label)

    private[scalafim] def copy(
        values: RavelArray[A, Rank[4]] = vector.values,
        space: NeuroSpace = vector.space,
        label: String = vector.label
    ): NeuroVec[A] =
      NeuroVec.fromRavel(values, space, label)

  def makeRavel[A](
      values: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String = ""
  ): Either[NeuroImageError, NeuroVec[A]] =
    Image4sInterop
      .seriesFromRavel(values, space, label)
      .map(value => value)

  def fromRavel[A](
      values: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String = ""
  ): NeuroVec[A] =
    makeRavel(values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def fromLinearChecked[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): Either[NeuroImageError, NeuroVec[A]] =
    importLegacyLinear(data, space, label).map(_.image)

  /** Checked legacy-linear ingress with an explicit materialization receipt. */
  def importLegacyLinear[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): Either[
    NeuroImageError,
    DenseImageImport[NeuroVec[A]]
  ] =
    Image4sInterop
      .seriesFromLegacyLinear(data, space, label)
      .map(vector =>
        DenseImageImport(
          vector,
          Image4sStorageTransfer.CanonicalizedLegacy
        )
      )

  private[scalafim] def fromLinear[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): NeuroVec[A] =
    fromLinearChecked(data, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
