package scalafim.image

import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import scala.reflect.ClassTag
import spire.algebra.Ring

final class SparseNeuroVec[A] private (
    val data: RavelArray[A, Rank[2]],
    val space: NeuroSpace,
    val support: SparseSupport,
    val label: String
):
  require(space.ndim >= 4, "space must be 4D")
  GridCompatibility.requireVolume(
    SeriesSpace.make(space).fold(err => throw new IllegalArgumentException(err.message), _.volumeSpace),
    support.indexSet.space
  )
  require(data.shape == Shape(space.dims(3), support.cardinality), "data shape mismatch")
  val seriesSpace: SeriesSpace =
    SeriesSpace.make(space).fold(err => throw new IllegalArgumentException(err.message), series => series)

  inline def map: IndexLookupVol =
    support.lookup

  lazy val mask: NeuroVol[Boolean] =
    support.toMask(label)

  inline def apply(i: Int, j: Int, k: Int, t: Int)(using Ring[A]): A =
    require(t >= 0 && t < space.dims(3), "t out of bounds")
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    val pos = map.lookup(lin)
    if pos < 0 then summon[Ring[A]].zero else data(t, pos)

  def apply(t: Int)(using ClassTag[A]): SparseNeuroVol[A] =
    volume(t)

  def apply(ts: Seq[Int])(using ClassTag[A]): SparseNeuroVec[A] =
    subVector(ts)

  private[scalafim] def valueAtCanonicalOrdinal(
      fullIndex: Int
  )(using Ring[A]): A =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    require(
      fullIndex >= 0 && fullIndex < spatialNels * tLen,
      "canonical ordinal out of bounds"
    )
    val linSpatial = fullIndex / tLen
    val t = fullIndex % tLen
    val pos = map.lookup(linSpatial)
    if pos < 0 then summon[Ring[A]].zero else data(t, pos)

  def asMatrix(using ClassTag[A], Ring[A]): RavelArray[A, Rank[2]] =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    given DType[A] = data.dtype
    RavelArray.tabulate[A](spatialNels, tLen) { (linearVoxel, time) =>
      val position = support.positionOf(linearVoxel)
      if position < 0 then zero else data(time, position)
    }

  def subArray(
    i: Seq[Int],
    j: Seq[Int],
    k: Seq[Int],
    t: Seq[Int]
  )(using ClassTag[A], Ring[A]): RavelArray[A, Rank[4]] =
    val dims = space.dims.take(4)
    require(i.forall(ii => ii >= 0 && ii < dims(0)), "i index out of bounds")
    require(j.forall(jj => jj >= 0 && jj < dims(1)), "j index out of bounds")
    require(k.forall(kk => kk >= 0 && kk < dims(2)), "k index out of bounds")
    require(t.forall(tt => tt >= 0 && tt < dims(3)), "t index out of bounds")

    given DType[A] = data.dtype
    RavelArray.tabulate[A](i.length, j.length, k.length, t.length) {
      (ii, jj, kk, tt) =>
        apply(i(ii), j(jj), k(kk), t(tt))
    }

  def series(linearSpatial: Int)(using ClassTag[A], spire.algebra.Ring[A]): Array[A] =
    val pos = map.lookup(linearSpatial)
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    val out = PrimitiveBuffers.fillConst[A](tLen, zero)
    if pos >= 0 then
      var t = 0
      while t < tLen do
        out(t) = data(t, pos)
        t += 1
    out

  def series(linearSpatial: Array1[Int])(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    val tLen = space.dims(3)
    val nVox = linearSpatial.size
    val zero = summon[Ring[A]].zero
    given DType[A] = data.dtype
    RavelArray.tabulate[A](tLen, nVox) { (time, position) =>
      val lin = linearSpatial(position)
      val pos = map.lookup(lin)
      if pos >= 0 then data(time, pos) else zero
    }

  def series(linearSpatial: Array[Int])(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    series(RavelArray.fromSeq(Shape(linearSpatial.length), linearSpatial))

  def series(indexSet: VoxelIndexSet)(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    GridCompatibility.requireVolume(seriesSpace.volumeSpace, indexSet.space)
    series(indexSet.unsafeArray)

  def series(roi: ROICoords)(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    series(roi.linearIndices(space.spatialSpace))

  def series(coords: Vector[Vector[Int]])(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    series(ROICoords(coords))

  def series(mask: NeuroVol[Boolean])(using
      ClassTag[A],
      spire.algebra.Ring[A]
  ): RavelArray[A, Rank[2]] =
    series(Mask.indexSet(mask))

  def seriesRoi(roi: ROICoords)(using ClassTag[A], spire.algebra.Ring[A]): ROIVec[A] =
    ROIVec(space, roi, series(roi))

  def select(
      selection: VoxelSelection,
      policy: MissingVoxelPolicy[A]
  )(using ClassTag[A]): Either[SparseSelectionError, RoiSeries[A]] =
    GridCompatibility
      .volume(seriesSpace.volumeSpace, selection.space)
      .left
      .map(SparseSelectionError.Grid.apply)
      .flatMap: _ =>
        policy match
          case MissingVoxelPolicy.RequireCovered =>
            materializeCovered(selection)
          case MissingVoxelPolicy.DropMissing =>
            val requested = selection.indexSet.unsafeArray
            val covered = Array.newBuilder[Int]
            covered.sizeHint(requested.size)
            var i = 0
            while i < requested.size do
              if map.lookup(requested(i)) >= 0 then covered += requested(i)
              i += 1
            val retained = covered.result()
            VoxelSelection
              .make(
                selection.space,
                RavelArray.fromSeq(Shape(retained.length), retained)
              )
              .left
              .map(SparseSelectionError.InvalidSelection.apply)
              .flatMap(materializeCovered)
          case MissingVoxelPolicy.Fill(value) =>
            Right(materializeFilled(selection, value))

  def select(
      region: VoxelRegion,
      policy: MissingVoxelPolicy[A]
  )(using ClassTag[A]): Either[SparseSelectionError, RoiSeries[A]] =
    select(region.toSelection, policy)

  def volume(t: Int)(using ClassTag[A]): SparseNeuroVol[A] =
    require(t >= 0 && t < space.dims(3), "t out of bounds")
    val nVox = map.cardinality
    given DType[A] = data.dtype
    val out =
      RavelArray.tabulate[A](nVox)(p => data(t, p))
    SparseNeuroVol(out, map.indices, space.spatialSpace, label)

  def asDense(using
      ClassTag[A],
      spire.algebra.Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    toDense

  def subVector(ts: Seq[Int])(using ClassTag[A]): SparseNeuroVec[A] =
    val tLen = space.dims(3)
    require(ts.nonEmpty, "ts must be non-empty")
    require(ts.forall(t => t >= 0 && t < tLen), "time index out of bounds")
    val nVox = map.cardinality
    given DType[A] = data.dtype
    val out =
      RavelArray.tabulate[A](ts.length, nVox) { (time, position) =>
        data(ts(time), position)
      }
    val newSpace = space.spatialSpace.addDim(ts.length, Some(Axis.Time))
    SparseNeuroVec(out, newSpace, support, label)

  def concat(that: SparseNeuroVec[A], rest: SparseNeuroVec[A]*)(using ClassTag[A], spire.algebra.Ring[A]): SparseNeuroVec[A] =
    SparseNeuroVec.concatUnion(Vector(this, that) ++ rest.toVector)

  def toDense(using
      ClassTag[A],
      spire.algebra.Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    val nx = space.spatialDims(0)
    val ny = space.spatialDims(1)
    given DType[A] = data.dtype
    val full =
      RavelArray.tabulate[A](nx, ny, space.spatialDims(2), tLen) {
        (i, j, k, time) =>
          val linearVoxel =
            Indexing.gridToIndex3D(space.spatialDims, i, j, k)
          val position = support.positionOf(linearVoxel)
          if position < 0 then zero else data(time, position)
      }
    NeuroVec.fromRavel(full, space, label)

  private def materializeCovered(
      selection: VoxelSelection
  )(using ClassTag[A]): Either[SparseSelectionError, RoiSeries[A]] =
    val requested = selection.indexSet.unsafeArray
    val missing = Array.newBuilder[Int]
    var voxel = 0
    while voxel < requested.size do
      if map.lookup(requested(voxel)) < 0 then missing += requested(voxel)
      voxel += 1

    val missingIndices = missing.result()
    if missingIndices.nonEmpty then
      VoxelRegion
        .make(
          selection.space,
          RavelArray.fromSeq(Shape(missingIndices.length), missingIndices)
        )
        .left
        .map(SparseSelectionError.InvalidSelection.apply)
        .flatMap(region => Left(SparseSelectionError.OutsideSupport(region)))
    else
      given DType[A] = data.dtype
      val out =
        RavelArray.tabulate[A](seriesSpace.nVolumes, requested.size) {
          (time, position) =>
            data(time, map.lookup(requested(position)))
        }
      Right(
        RoiSeries.unsafe(
          seriesSpace,
          selection,
          out,
          label
        )
      )

  private def materializeFilled(
      selection: VoxelSelection,
      fill: A
  )(using ClassTag[A]): RoiSeries[A] =
    val requested = selection.indexSet.unsafeArray
    given DType[A] = data.dtype
    val out =
      RavelArray.tabulate[A](seriesSpace.nVolumes, requested.size) {
        (time, position) =>
          val source = map.lookup(requested(position))
          if source >= 0 then data(time, source) else fill
      }
    RoiSeries.unsafe(
      seriesSpace,
      selection,
      out,
      label
    )

object SparseNeuroVec:
  def apply[A](
      data: RavelArray[A, Rank[2]],
      space: NeuroSpace,
      support: SparseSupport,
      label: String
  ): SparseNeuroVec[A] =
    new SparseNeuroVec(data, space, support, label)

  def apply[A](
      data: RavelArray[A, Rank[2]],
      space: NeuroSpace,
      mask: NeuroVol[Boolean],
      map: IndexLookupVol,
      label: String = ""
  ): SparseNeuroVec[A] =
    val support =
      SparseSupport.checkedCompatibility(space, mask, map)
    new SparseNeuroVec(data, space, support, label)

  def fromDense[A](
    data: Array[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using ClassTag[A], DType[A]): SparseNeuroVec[A] =
    require(space.ndim >= 4, "space must be 4D")
    GridCompatibility.requireSpatial(space, mask.space)
    val indexSet = Mask.indexSet(mask)
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    require(data.length == spatialNels * tLen, "data length mismatch")

    val idx = indexSet.toVector
    val support = SparseSupport.fromIndexSet(indexSet)

    val compact =
      RavelArray.tabulate[A](tLen, idx.length) { (time, position) =>
        data(idx(position) * tLen + time)
      }
    SparseNeuroVec(compact, space, support, label)

  /** Gather a canonical dense series directly into compact rank-2 Ravel
    * storage. No full-volume staging buffer is allocated.
    */
  def fromDense[A](
      source: NeuroVec[A],
      support: SparseSupport,
      label: String
  ): SparseNeuroVec[A] =
    GridCompatibility.requireVolume(
      source.seriesSpace.volumeSpace,
      support.indexSet.space
    )
    val indices = support.indexSet.toVector
    given DType[A] = source.values.dtype
    val compact =
      RavelArray.tabulate[A](source.nVolumes, indices.length) {
        (time, position) =>
          val voxel = source.space.indexToVoxel3D(indices(position))
          source(voxel.x, voxel.y, voxel.z, time)
      }
    SparseNeuroVec(compact, source.space, support, label)

  def fromDense[A](
      source: NeuroVec[A],
      mask: NeuroVol[Boolean],
      label: String
  ): SparseNeuroVec[A] =
    GridCompatibility.requireVolume(
      source.seriesSpace.volumeSpace,
      VolumeSpace.fromSpatialPart(mask.space)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    )
    fromDense(source, SparseSupport.fromMask(mask), label)

  def apply[A](
    data: Array[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean]
  )(using ClassTag[A], DType[A]): SparseNeuroVec[A] =
    fromDense(data, space, mask)

  def apply[A](
    data: Array[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String
  )(using ClassTag[A], DType[A]): SparseNeuroVec[A] =
    fromDense(data, space, mask, label)

  def concatUnion[A](
    vecs: Vector[SparseNeuroVec[A]]
  )(using ClassTag[A], spire.algebra.Ring[A]): SparseNeuroVec[A] =
    require(vecs.nonEmpty, "cannot concat empty vector")
    val base = vecs.head.space.spatialSpace
    vecs.foreach { v =>
      GridCompatibility.requireSpatial(base, v.space)
    }

    val unionIdx =
      vecs.flatMap(_.support.indexSet.toVector).distinct.sorted
    require(unionIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")
    val unionArr =
      RavelArray.fromSeq(Shape(unionIdx.length), unionIdx)
    val unionSupport =
      SparseSupport.fromIndexSet(VoxelIndexSet.unique(base, unionArr))

    val totalT = vecs.map(_.space.dims(3)).sum
    val zero = summon[Ring[A]].zero
    val sourceByTime = Array.ofDim[Int](totalT)
    val localTime = Array.ofDim[Int](totalT)
    var timeOffset = 0
    var source = 0
    while source < vecs.length do
      var time = 0
      while time < vecs(source).space.dims(3) do
        sourceByTime(timeOffset + time) = source
        localTime(timeOffset + time) = time
        time += 1
      timeOffset += vecs(source).space.dims(3)
      source += 1

    val newSpace = base.addDim(totalT, Some(Axis.Time))
    given DType[A] = vecs.head.data.dtype
    val compact =
      RavelArray.tabulate[A](totalT, unionIdx.length) { (time, position) =>
        val vector = vecs(sourceByTime(time))
        val sourcePosition = vector.support.positionOf(unionIdx(position))
        if sourcePosition < 0 then zero
        else vector.data(localTime(time), sourcePosition)
      }
    SparseNeuroVec(compact, newSpace, unionSupport, vecs.head.label)
