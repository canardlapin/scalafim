package scalafim.image

import cats.Applicative
import cats.syntax.all.*
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import ravel.map
import ravel.select
import scala.reflect.ClassTag

opaque type NeuroVec[A] = AnyNeuroSeries[A]

object NeuroVec:
  /** Zero-copy compatibility admission from a semantics-preserving native
    * series. Kept package-scoped while downstream modules migrate their
    * public signatures.
    */
  private[scalafim] inline def fromNative[A, Sem](
      series: SomeNeuroSeries[A, Sem]
  ): NeuroVec[A] =
    series

  extension [A](vector: NeuroVec[A])
    inline def label: String =
      vector.metadata.label

    /** Canonical dense storage. This is the same Ravel value retained by
      * `sampled`; no second dense buffer is cached alongside it.
      */
    inline def values: RavelArray[A, Rank[4]] =
      vector.data

    /** The image4s representation underlying vector compatibility view. */
    inline def sampled: image4s.Sampled[
      ? <: image4s.SampleSpace[?, image4s.geometry.D3],
      A,
      ?,
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

    def apply(ts: Seq[Int])(using
        ClassTag[A],
        MigrationValueSemantics[A]
    ): NeuroVec[A] =
      subVector(ts)

    def gridToIndex(i: Int, j: Int, k: Int, t: Int): Int =
      Indexing.gridToIndex(space.dims.take(4), Vector(i, j, k, t))

    def indexToGrid(idx: Int): Vector[Int] =
      Indexing.indexToGrid(space.dims.take(4), idx)

    private[scalafim] def valueAtCanonicalOrdinal(i: Int): A =
      val index = indexToGrid(i)
      apply(index(0), index(1), index(2), index(3))

    private[scalafim] def valueAtVoxelOrdinal(
        voxelOrdinal: Int,
        time: Int
    ): A =
      val voxel = space.indexToVoxel3D(voxelOrdinal)
      apply(voxel.x, voxel.y, voxel.z, time)

    /** Explicitly copy logical values in canonical last-axis-fastest order. */
    def copyToCanonicalArray(using ClassTag[A]): Array[A] =
      val out = PrimitiveBuffers.ofSize[A](values.size)
      var index = 0
      while index < out.length do
        out(index) = valueAtCanonicalOrdinal(index)
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

    /** Zero-copy time-series view for one canonical voxel ordinal. */
    def series(linearSpatial: Int): Array1[A] =
      val spatialNels = space.spatialDims.product
      require(linearSpatial >= 0 && linearSpatial < spatialNels, "spatial index out of bounds")
      val voxel = space.indexToVoxel3D(linearSpatial)
      values
        .select(0, voxel.x)
        .select(0, voxel.y)
        .select(0, voxel.z)

    def series(i: Int, j: Int, k: Int): Array1[A] =
      val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
      series(lin)

    def series(linearSpatial: Array1[Int]): RavelArray[A, Rank[2]] =
      val spatialNels = space.spatialDims.product
      val tLen = nVolumes
      val nVox = linearSpatial.size
      given DType[A] = values.dtype
      RavelArray.tabulate[A](nVox, tLen) { (position, time) =>
        val lin = linearSpatial(position)
        require(lin >= 0 && lin < spatialNels, "spatial index out of bounds")
        val voxel = space.indexToVoxel3D(lin)
        apply(voxel.x, voxel.y, voxel.z, time)
      }

    def series(linearSpatial: Array[Int]): RavelArray[A, Rank[2]] =
      series(RavelArray.fromSeq(Shape(linearSpatial.length), linearSpatial))

    def series(mask: NeuroVol[Boolean]): RavelArray[A, Rank[2]] =
      series(Mask.indices(mask))

    def subVector(ts: Seq[Int])(using
        ClassTag[A],
        MigrationValueSemantics[A]
    ): NeuroVec[A] =
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
        ClassTag[A],
        MigrationValueSemantics[A]
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
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVec[B] =
      NeuroVec.fromRavel(ravel.map(values)(f), space, label)

    def mapVoxels[B](f: (VoxelCoord, A) => B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
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
    )(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVec[B] =
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
    )(using
        ClassTag[C],
        DType[C],
        MigrationValueSemantics[C]
    ): Either[GridMismatch, NeuroVec[C]] =
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
    )(using
        Applicative[F],
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): F[NeuroVec[B]] =
      Vector
        .tabulate(values.size)(valueAtCanonicalOrdinal)
        .traverse(f)
        .map: traversed =>
          val shape = space.spatialDims
          NeuroVec.fromRavel(
            RavelArray.fromSeq(
              Shape(shape(0), shape(1), shape(2), nVolumes),
              traversed
            ),
            space,
            label
          )

    def map[B](f: A => B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVec[B] =
      mapValues(f)

    def reconstruct(
        values: RavelArray[A, Rank[4]] = vector.values,
        space: NeuroSpace = vector.space,
        label: String = vector.label
    )(using MigrationValueSemantics[A]): Either[NeuroImageError, NeuroVec[A]] =
      NeuroVec.makeRavel(values, space, label)

    private[scalafim] def copy(
        values: RavelArray[A, Rank[4]] = vector.values,
        space: NeuroSpace = vector.space,
        label: String = vector.label
    )(using MigrationValueSemantics[A]): NeuroVec[A] =
      NeuroVec.fromRavel(values, space, label)

  def makeRavel[A](
      values: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String = ""
  )(using MigrationValueSemantics[A]): Either[NeuroImageError, NeuroVec[A]] =
    Image4sInterop
      .seriesFromRavel(values, space, label)
      .map(value => value)

  def fromRavel[A](
      values: RavelArray[A, Rank[4]],
      space: NeuroSpace,
      label: String = ""
  )(using MigrationValueSemantics[A]): NeuroVec[A] =
    makeRavel(values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private[scalafim] def copyFromCanonicalArrayChecked[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using
      DType[A],
      MigrationValueSemantics[A]
  ): Either[NeuroImageError, NeuroVec[A]] =
    val shape = space.dims.take(4)
    val expected = shape.product
    if data.length != expected then
      Left(
        NeuroImageError.LinearSizeMismatch(
          "NeuroSeries canonical array",
          expected,
          data.length
        )
      )
    else
      makeRavel(
        RavelArray.fromSeq(
          Shape(shape(0), shape(1), shape(2), shape(3)),
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
  ): NeuroVec[A] =
    copyFromCanonicalArrayChecked(data, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
