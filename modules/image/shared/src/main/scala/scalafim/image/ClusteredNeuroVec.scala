package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.{Field, Ring}
import spire.syntax.field.*
import spire.syntax.ring.*

final case class ClusteredNeuroVec[A](
  cvol: ClusteredNeuroVol,
  ts: NDArray[A],
  clMap: NArray[Int],
  space: NeuroSpace,
  label: String = ""
):
  require(space.ndim == 4, "ClusteredNeuroVec must be 4D")
  GridCompatibility.requireSpatial(space, cvol.space)
  require(ts.ndim == 2, "ts must be 2D (time x clusters)")
  require(ts.shape(0) == space.dims(3), "ts/time mismatch")
  require(ts.shape(1) == cvol.numClusters, "ts/cluster mismatch")
  require(clMap.length == space.spatialDims.product, "clMap length mismatch")
  private val expectedClusterMap = cvol.toDense.values.data
  private val firstClusterMapMismatch =
    var mismatch = Option.empty[Int]
    var i = 0
    while i < clMap.length && mismatch.isEmpty do
      if clMap(i) != expectedClusterMap(i) then mismatch = Some(i)
      i += 1
    mismatch
  require(
    firstClusterMapMismatch.isEmpty,
    firstClusterMapMismatch
      .map(index => s"cluster assignment at spatial index $index disagrees with cluster volume")
      .getOrElse("")
  )

  private val clusterIds: Vector[Int] = cvol.clusterIds
  private val idToCol: Map[Int, Int] = clusterIds.zipWithIndex.toMap

  def nVolumes: Int = ts.shape(0)
  def numClusters: Int = ts.shape(1)

  inline def apply(i: Int, j: Int, k: Int, t: Int)(using Ring[A]): A =
    require(t >= 0 && t < nVolumes, "t out of bounds")
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    val cid = clMap(lin)
    if cid == 0 then summon[Ring[A]].zero
    else
      val col = idToCol(cid)
      ts.data(t + col * nVolumes)

  def apply(t: Int)(using ClassTag[A], Ring[A]): NeuroVol[A] =
    volume(t)

  def apply(tsIdx: Seq[Int])(using ClassTag[A]): ClusteredNeuroVec[A] =
    subVector(tsIdx)

  def series(linearSpatial: Int)(using ClassTag[A], Ring[A]): NArray[A] =
    val spatialNels = space.spatialDims.product
    require(linearSpatial >= 0 && linearSpatial < spatialNels, "spatial index out of bounds")
    val cid = clMap(linearSpatial)
    val out = narr.NArray.ofSize[A](nVolumes)
    val zero = summon[Ring[A]].zero
    if cid == 0 then
      var t = 0
      while t < nVolumes do
        out(t) = zero
        t += 1
    else
      val col = idToCol(cid)
      narr.NArray.copy(ts.data, col * nVolumes, out, 0, nVolumes)
    out

  def series(i: Int, j: Int, k: Int)(using ClassTag[A], Ring[A]): NArray[A] =
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    series(lin)

  def series(linearSpatial: NArray[Int])(using ClassTag[A], Ring[A]): NDArray[A] =
    val spatialNels = space.spatialDims.product
    val tLen = nVolumes
    val nVox = linearSpatial.length
    val out = narr.NArray.ofSize[A](tLen * nVox)
    val zero = summon[Ring[A]].zero
    var p = 0
    while p < nVox do
      val lin = linearSpatial(p)
      require(lin >= 0 && lin < spatialNels, "spatial index out of bounds")
      val cid = clMap(lin)
      if cid == 0 then
        var t = 0
        while t < tLen do
          out(t + p * tLen) = zero
          t += 1
      else
        val col = idToCol(cid)
        narr.NArray.copy(ts.data, col * tLen, out, p * tLen, tLen)
      p += 1
    NDArray(out, Vector(tLen, nVox))

  def series(roi: ROICoords)(using ClassTag[A], Ring[A]): NDArray[A] =
    series(roi.linearIndices(space.spatialSpace))

  def series(coords: Vector[Vector[Int]])(using ClassTag[A], Ring[A]): NDArray[A] =
    series(ROICoords(coords))

  def series(mask: NeuroVol[Boolean])(using ClassTag[A], Ring[A]): NDArray[A] =
    series(Mask.indices(mask))

  def seriesRoi(roi: ROICoords)(using ClassTag[A], Ring[A]): ROIVec[A] =
    ROIVec(space, roi, series(roi))

  def volume(t: Int)(using ClassTag[A], Ring[A]): NeuroVol[A] =
    require(t >= 0 && t < nVolumes, "t out of bounds")
    val spatialNels = space.spatialDims.product
    val out = narr.NArray.ofSize[A](spatialNels)
    val zero = summon[Ring[A]].zero
    var lin = 0
    while lin < spatialNels do
      val cid = clMap(lin)
      if cid == 0 then out(lin) = zero
      else
        val col = idToCol(cid)
        out(lin) = ts.data(t + col * nVolumes)
      lin += 1
    NeuroVol.fromLinear(out, space.spatialSpace, label)

  def toDense(using ClassTag[A], Ring[A]): NeuroVec[A] =
    val spatialNels = space.spatialDims.product
    val tLen = nVolumes
    val out = narr.NArray.ofSize[A](spatialNels * tLen)
    val zero = summon[Ring[A]].zero
    var lin = 0
    while lin < spatialNels do
      val cid = clMap(lin)
      if cid == 0 then
        var t = 0
        while t < tLen do
          out(lin + t * spatialNels) = zero
          t += 1
      else
        val col = idToCol(cid)
        var t = 0
        while t < tLen do
          out(lin + t * spatialNels) = ts.data(t + col * tLen)
          t += 1
      lin += 1
    NeuroVec.fromLinear(out, space, label)

  def asDense(using ClassTag[A], Ring[A]): NeuroVec[A] = toDense

  def toSparse(using ClassTag[A], Ring[A]): SparseNeuroVec[A] =
    val tLen = nVolumes
    val activeIdx = Mask.indices(cvol.mask)
    val out = narr.NArray.ofSize[A](tLen * activeIdx.length)
    var p = 0
    while p < activeIdx.length do
      val cid = cvol.clusters(p)
      val col = idToCol(cid)
      narr.NArray.copy(ts.data, col * tLen, out, p * tLen, tLen)
      p += 1
    val map = IndexLookupVol(space, activeIdx)
    SparseNeuroVec(NDArray(out, Vector(tLen, activeIdx.length)), space, cvol.mask, map, label)

  def subVector(tsIdx: Seq[Int])(using ClassTag[A]): ClusteredNeuroVec[A] =
    val tLen = nVolumes
    require(tsIdx.nonEmpty, "tsIdx must be non-empty")
    require(tsIdx.forall(t => t >= 0 && t < tLen), "time index out of bounds")
    val newT = tsIdx.length
    val kLen = numClusters
    val out = narr.NArray.ofSize[A](newT * kLen)
    var k = 0
    while k < kLen do
      var t = 0
      while t < newT do
        out(t + k * newT) = ts.data(tsIdx(t) + k * tLen)
        t += 1
      k += 1
    val newTs = NDArray(out, Vector(newT, kLen))
    val newSpace = cvol.space.addDim(newT, Some(Axis.Time))
    copy(ts = newTs, space = newSpace)

  def requireCompat(that: ClusteredNeuroVec[?]): Unit =
    GridCompatibility.requireExact(this.space, that.space)
    require(this.numClusters == that.numClusters, "cluster count mismatch")
    require(this.clMap.length == that.clMap.length, "cluster map length mismatch")
    var ok = true
    var i = 0
    while ok && i < clMap.length do
      if this.clMap(i) != that.clMap(i) then ok = false
      i += 1
    require(ok, "cluster assignments mismatch")

  def asMatrix: NDArray[A] = ts

  def centroids(real: Boolean = false): Vector[Vector[Double]] =
    cvol.centroids(real)

  def centroids(centroidType: ClusteredNeuroVol.CentroidType): Vector[Vector[Double]] =
    cvol.centroids(centroidType)

  def centroids(
    centroidType: ClusteredNeuroVol.CentroidType,
    real: Boolean,
    eps: Double,
    maxIter: Int
  ): Vector[Vector[Double]] =
    cvol.centroids(centroidType, real, eps, maxIter)

  def map[B](f: A => B)(using ClassTag[B]): ClusteredNeuroVec[B] =
    ClusteredNeuroVec(cvol, ts.map(f), clMap, space, label)

object ClusteredNeuroVec:

  private def meanReducer[A: Field]: NArray[A] => A =
    (arr: NArray[A]) =>
      val field = summon[Field[A]]
      var acc = field.zero
      var i = 0
      while i < arr.length do
        acc = acc + arr(i)
        i += 1
      acc / field.fromInt(arr.length)

  def fromNeuroVec[A](
    vec: NeuroVec[A],
    cvol: ClusteredNeuroVol,
    reducer: NArray[A] => A,
    label: String = ""
  )(using ClassTag[A], Ring[A]): ClusteredNeuroVec[A] =
    GridCompatibility.requireSpatial(vec.space, cvol.space)
    val lbl = if label.nonEmpty then label else vec.label
    val spatialNels = vec.space.spatialDims.product
    val tLen = vec.nVolumes
    val ids = cvol.clusterIds
    val kLen = ids.length

    val out = narr.NArray.ofSize[A](tLen * kLen)
    val data = vec.values.data

    var k = 0
    while k < kLen do
      val id = ids(k)
      val idxArr = cvol.clusterMap(id)
      if idxArr.length == 1 then
        val lin0 = idxArr(0)
        var t = 0
        while t < tLen do
          out(t + k * tLen) = data(lin0 + t * spatialNels)
          t += 1
      else
        val tmp = narr.NArray.ofSize[A](idxArr.length)
        var t = 0
        while t < tLen do
          var p = 0
          while p < idxArr.length do
            tmp(p) = data(idxArr(p) + t * spatialNels)
            p += 1
          out(t + k * tLen) = reducer(tmp)
          t += 1
      k += 1

    val ts = NDArray(out, Vector(tLen, kLen))

    val clMap = NArrayUtil.fillConst[Int](spatialNels, 0)
    val activeIdx = Mask.indices(cvol.mask)
    var i = 0
    while i < activeIdx.length do
      clMap(activeIdx(i)) = cvol.clusters(i)
      i += 1

    val sp4 = cvol.space.addDim(tLen, Some(Axis.Time))
    ClusteredNeuroVec(cvol, ts, clMap, sp4, lbl)

  def fromNeuroVecMean[A: Field](
    vec: NeuroVec[A],
    cvol: ClusteredNeuroVol,
    label: String = ""
  )(using ClassTag[A]): ClusteredNeuroVec[A] =
    val lbl = if label.nonEmpty then label else vec.label
    fromNeuroVec(vec, cvol, meanReducer[A], lbl)

  def fromMatrix[A](
    ts: NDArray[A],
    cvol: ClusteredNeuroVol,
    label: String = ""
  )(using ClassTag[A], Ring[A]): ClusteredNeuroVec[A] =
    require(ts.ndim == 2, "ts must be 2D (time x clusters)")
    require(ts.shape(1) == cvol.numClusters, "ts/cluster mismatch")
    val tLen = ts.shape(0)
    val spatialNels = cvol.space.spatialDims.product
    val clMap = NArrayUtil.fillConst[Int](spatialNels, 0)
    val activeIdx = Mask.indices(cvol.mask)
    var i = 0
    while i < activeIdx.length do
      clMap(activeIdx(i)) = cvol.clusters(i)
      i += 1
    val sp4 = cvol.space.addDim(tLen, Some(Axis.Time))
    ClusteredNeuroVec(cvol, ts, clMap, sp4, label)
