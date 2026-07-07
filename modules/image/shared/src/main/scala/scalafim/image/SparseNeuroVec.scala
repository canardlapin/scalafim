package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.Ring

final case class SparseNeuroVec[A](
  data: NDArray[A],
  space: NeuroSpace,
  mask: NeuroVol[Boolean],
  map: IndexLookupVol,
  label: String = ""
):
  require(space.ndim >= 4, "space must be 4D")
  require(mask.space.spatialDims == space.spatialDims, "mask/space mismatch")
  require(map.space.spatialDims == space.spatialDims, "map/space mismatch")
  require(data.shape == Vector(space.dims(3), map.cardinality), "data shape mismatch")
  val seriesSpace: SeriesSpace =
    SeriesSpace.make(space).fold(err => throw new IllegalArgumentException(err.message), series => series)

  inline def apply(i: Int, j: Int, k: Int, t: Int)(using Ring[A]): A =
    require(t >= 0 && t < space.dims(3), "t out of bounds")
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    val pos = map.lookup(lin)
    if pos < 0 then summon[Ring[A]].zero else data(t, pos)

  def apply(t: Int)(using ClassTag[A]): SparseNeuroVol[A] =
    volume(t)

  def apply(ts: Seq[Int])(using ClassTag[A]): SparseNeuroVec[A] =
    subVector(ts)

  def linear(fullIndex: Int)(using Ring[A]): A =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    require(fullIndex >= 0 && fullIndex < spatialNels * tLen, "linear index out of bounds")
    val linSpatial = fullIndex % spatialNels
    val t = fullIndex / spatialNels
    val pos = map.lookup(linSpatial)
    if pos < 0 then summon[Ring[A]].zero else data(t, pos)

  def asMatrix(using ClassTag[A], Ring[A]): NDArray[A] =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    val out = NArrayUtil.fillConst[A](spatialNels * tLen, zero)
    var pos = 0
    while pos < map.cardinality do
      val lin = map.indices(pos)
      var t = 0
      while t < tLen do
        out(lin + t * spatialNels) = data(t, pos)
        t += 1
      pos += 1
    NDArray(out, Vector(spatialNels, tLen))

  def subArray(
    i: Seq[Int],
    j: Seq[Int],
    k: Seq[Int],
    t: Seq[Int]
  )(using ClassTag[A], Ring[A]): NDArray[A] =
    val dims = space.dims.take(4)
    require(i.forall(ii => ii >= 0 && ii < dims(0)), "i index out of bounds")
    require(j.forall(jj => jj >= 0 && jj < dims(1)), "j index out of bounds")
    require(k.forall(kk => kk >= 0 && kk < dims(2)), "k index out of bounds")
    require(t.forall(tt => tt >= 0 && tt < dims(3)), "t index out of bounds")

    val out = NArrayUtil.ofSize[A](i.length * j.length * k.length * t.length)
    var outIdx = 0
    var tt = 0
    while tt < t.length do
      val t0 = t(tt)
      var kk = 0
      while kk < k.length do
        val k0 = k(kk)
        var jj = 0
        while jj < j.length do
          val j0 = j(jj)
          var ii = 0
          while ii < i.length do
            out(outIdx) = apply(i(ii), j0, k0, t0)
            outIdx += 1
            ii += 1
          jj += 1
        kk += 1
      tt += 1
    NDArray(out, Vector(i.length, j.length, k.length, t.length))

  def series(linearSpatial: Int)(using ClassTag[A], spire.algebra.Ring[A]): NArray[A] =
    val pos = map.lookup(linearSpatial)
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    val out = NArrayUtil.fillConst[A](tLen, zero)
    if pos >= 0 then
      var t = 0
      while t < tLen do
        out(t) = data(t, pos)
        t += 1
    out

  def series(linearSpatial: NArray[Int])(using ClassTag[A], spire.algebra.Ring[A]): NDArray[A] =
    val tLen = space.dims(3)
    val nVox = linearSpatial.length
    val zero = summon[Ring[A]].zero
    val out = NArrayUtil.fillConst[A](tLen * nVox, zero)
    var p = 0
    while p < nVox do
      val lin = linearSpatial(p)
      val pos = map.lookup(lin)
      if pos >= 0 then
        var t = 0
        while t < tLen do
          out(t + p * tLen) = data(t, pos)
          t += 1
      p += 1
    NDArray(out, Vector(tLen, nVox))

  def series(indexSet: VoxelIndexSet)(using ClassTag[A], spire.algebra.Ring[A]): NDArray[A] =
    require(indexSet.space == seriesSpace.volumeSpace, "index set/space mismatch")
    series(indexSet.unsafeArray)

  def series(roi: ROICoords)(using ClassTag[A], spire.algebra.Ring[A]): NDArray[A] =
    series(roi.linearIndices(space.spatialSpace))

  def series(coords: Vector[Vector[Int]])(using ClassTag[A], spire.algebra.Ring[A]): NDArray[A] =
    series(ROICoords(coords))

  def series(mask: NeuroVol[Boolean])(using ClassTag[A], spire.algebra.Ring[A]): NDArray[A] =
    series(Mask.indexSet(mask))

  def seriesRoi(roi: ROICoords)(using ClassTag[A], spire.algebra.Ring[A]): ROIVec[A] =
    ROIVec(space, roi, series(roi))

  def volume(t: Int)(using ClassTag[A]): SparseNeuroVol[A] =
    require(t >= 0 && t < space.dims(3), "t out of bounds")
    val nVox = map.cardinality
    val out = NArrayUtil.ofSize[A](nVox)
    var p = 0
    while p < nVox do
      out(p) = data(t, p)
      p += 1
    SparseNeuroVol(out, map.indices, space.spatialSpace, label)

  def asDense(using ClassTag[A], spire.algebra.Ring[A]): NeuroVec[A] =
    toDense

  def subVector(ts: Seq[Int])(using ClassTag[A]): SparseNeuroVec[A] =
    val tLen = space.dims(3)
    require(ts.nonEmpty, "ts must be non-empty")
    require(ts.forall(t => t >= 0 && t < tLen), "time index out of bounds")
    val nVox = map.cardinality
    val out = NArrayUtil.ofSize[A](ts.length * nVox)
    var p = 0
    while p < nVox do
      var tt = 0
      while tt < ts.length do
        out(tt + p * ts.length) = data(ts(tt), p)
        tt += 1
      p += 1
    val newSpace = space.spatialSpace.addDim(ts.length, Some(Axis.Time))
    val newMap = IndexLookupVol(newSpace, map.indices)
    SparseNeuroVec(NDArray(out, Vector(ts.length, nVox)), newSpace, mask, newMap, label)

  def concat(that: SparseNeuroVec[A], rest: SparseNeuroVec[A]*)(using ClassTag[A], spire.algebra.Ring[A]): SparseNeuroVec[A] =
    SparseNeuroVec.concatUnion(Vector(this, that) ++ rest.toVector)

  def toDense(using ClassTag[A], spire.algebra.Ring[A]): NeuroVec[A] =
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    val zero = summon[Ring[A]].zero
    val full = NArrayUtil.fillConst[A](spatialNels * tLen, zero)
    var pos = 0
    while pos < map.cardinality do
      val lin = map.indices(pos)
      var t = 0
      while t < tLen do
        full(lin + t * spatialNels) = data(t, pos)
        t += 1
      pos += 1
    NeuroVec.fromLinear(full, space, label)

object SparseNeuroVec:
  def fromDense[A](
    data: NArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using ClassTag[A]): SparseNeuroVec[A] =
    require(space.ndim >= 4, "space must be 4D")
    require(mask.space.spatialDims == space.spatialDims, "mask/space mismatch")
    val indexSet = Mask.indexSet(mask)
    val spatialNels = space.spatialDims.product
    val tLen = space.dims(3)
    require(data.length == spatialNels * tLen, "data length mismatch")

    val idx = indexSet.toVector
    val lookup = IndexLookupVol(space, indexSet.indices)

    val mat = narr.NArray.ofSize[A](tLen * idx.length)
    var pos = 0
    while pos < idx.length do
      val lin = idx(pos)
      var t = 0
      while t < tLen do
        mat(t + pos * tLen) = data(lin + t * spatialNels)
        t += 1
      pos += 1
    SparseNeuroVec(NDArray(mat, Vector(tLen, idx.length)), space, mask, lookup, label)

  def apply[A](
    data: NArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean]
  )(using ClassTag[A]): SparseNeuroVec[A] =
    fromDense(data, space, mask)

  def apply[A](
    data: NArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String
  )(using ClassTag[A]): SparseNeuroVec[A] =
    fromDense(data, space, mask, label)

  def concatUnion[A](
    vecs: Vector[SparseNeuroVec[A]]
  )(using ClassTag[A], spire.algebra.Ring[A]): SparseNeuroVec[A] =
    require(vecs.nonEmpty, "cannot concat empty vector")
    val base = vecs.head.space.spatialSpace
    vecs.foreach { v =>
      require(v.space.spatialDims == base.spatialDims, "spatial dims mismatch")
      require(v.space.spacing == base.spacing && v.space.origin == base.origin, "space mismatch")
    }

    def toIdx(v: SparseNeuroVec[A]): Vector[Int] =
      Vector.tabulate(v.map.indices.length)(i => v.map.indices(i))

    val unionIdx = vecs.flatMap(toIdx).distinct.sorted
    require(unionIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")
    val unionArr = NArrayUtil.fromArray(unionIdx.toArray)
    val unionMap = IndexLookupVol(base.addDim(1, Some(Axis.Time)), unionArr)
    val unionMask = Mask.fromIndices(base, unionArr)

    val totalT = vecs.map(_.space.dims(3)).sum
    val zero = summon[Ring[A]].zero
    val out = NArrayUtil.fillConst[A](totalT * unionIdx.length, zero)

    var tOffset = 0
    vecs.foreach { v =>
      val tLen = v.space.dims(3)
      var p = 0
      while p < v.map.cardinality do
        val lin = v.map.indices(p)
        val up = unionMap.lookup(lin)
        var t = 0
        val baseIdx = up * totalT + tOffset
        while t < tLen do
          out(baseIdx + t) = v.data(t, p)
          t += 1
        p += 1
      tOffset += tLen
    }

    val newSpace = base.addDim(totalT, Some(Axis.Time))
    val newMap = IndexLookupVol(newSpace, unionArr)
    SparseNeuroVec(NDArray(out, Vector(totalT, unionIdx.length)), newSpace, unionMask, newMap, vecs.head.label)
