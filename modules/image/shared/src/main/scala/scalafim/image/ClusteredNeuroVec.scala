package scalafim.image

import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import ravel.map
import scala.reflect.ClassTag
import spire.algebra.{Field, Ring}
import spire.syntax.field.*
import spire.syntax.ring.*

final case class ClusteredNeuroVec[A](
  cvol: ClusteredNeuroVol,
  ts: RavelArray[A, Rank[2]],
  clMap: Array[Int],
  space: NeuroSpace,
  label: String = ""
):
  require(space.ndim == 4, "ClusteredNeuroVec must be 4D")
  GridCompatibility.requireSpatial(space, cvol.space)
  require(ts.shape(0) == space.dims(3), "ts/time mismatch")
  require(ts.shape(1) == cvol.numClusters, "ts/cluster mismatch")
  require(clMap.length == space.spatialDims.product, "clMap length mismatch")
  private val expectedClusterMap = cvol.toDense
  private val firstClusterMapMismatch =
    var mismatch = Option.empty[Int]
    var i = 0
    while i < clMap.length && mismatch.isEmpty do
      if clMap(i) != expectedClusterMap.valueAtCanonicalOrdinal(i) then
        mismatch = Some(i)
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
      ts(t, col)

  def apply(t: Int)(using
      ClassTag[A],
      Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    volume(t)

  def apply(tsIdx: Seq[Int])(using ClassTag[A], DType[A]): ClusteredNeuroVec[A] =
    subVector(tsIdx)

  def series(linearSpatial: Int)(using ClassTag[A], Ring[A]): Array[A] =
    val spatialNels = space.spatialDims.product
    require(linearSpatial >= 0 && linearSpatial < spatialNels, "spatial index out of bounds")
    val cid = clMap(linearSpatial)
    val out = Array.ofDim[A](nVolumes)
    val zero = summon[Ring[A]].zero
    if cid == 0 then
      var t = 0
      while t < nVolumes do
        out(t) = zero
        t += 1
    else
      val col = idToCol(cid)
      var t = 0
      while t < nVolumes do
        out(t) = ts(t, col)
        t += 1
    out

  def series(i: Int, j: Int, k: Int)(using ClassTag[A], Ring[A]): Array[A] =
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    series(lin)

  def series(linearSpatial: Array1[Int])(using
      ClassTag[A],
      Ring[A],
      DType[A]
  ): RavelArray[A, Rank[2]] =
    val spatialNels = space.spatialDims.product
    val tLen = nVolumes
    val nVox = linearSpatial.size
    val zero = summon[Ring[A]].zero
    RavelArray.tabulate[A](tLen, nVox) { (time, position) =>
      val lin = linearSpatial(position)
      require(lin >= 0 && lin < spatialNels, "spatial index out of bounds")
      val cid = clMap(lin)
      if cid == 0 then zero else ts(time, idToCol(cid))
    }

  def series(linearSpatial: Array[Int])(using
      ClassTag[A],
      Ring[A],
      DType[A]
  ): RavelArray[A, Rank[2]] =
    series(RavelArray.fromSeq(Shape(linearSpatial.length), linearSpatial))

  def series(mask: NeuroVol[Boolean])(using
      ClassTag[A],
      Ring[A],
      DType[A]
  ): RavelArray[A, Rank[2]] =
    series(Mask.indices(mask))

  def volume(t: Int)(using
      ClassTag[A],
      Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    require(t >= 0 && t < nVolumes, "t out of bounds")
    val spatialNels = space.spatialDims.product
    val out = Array.ofDim[A](spatialNels)
    val zero = summon[Ring[A]].zero
    var lin = 0
    while lin < spatialNels do
      val cid = clMap(lin)
      if cid == 0 then out(lin) = zero
      else
        val col = idToCol(cid)
        out(lin) = ts(t, col)
      lin += 1
    NeuroVol.copyFromCanonicalArray(out, space.spatialSpace, label)

  def toDense(using
      ClassTag[A],
      Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    val spatialNels = space.spatialDims.product
    val tLen = nVolumes
    val zero = summon[Ring[A]].zero
    val shape = space.spatialDims
    val out =
      RavelArray.tabulate[A](shape(0), shape(1), shape(2), tLen) {
        (i, j, k, time) =>
          val lin = Indexing.gridToIndex3D(shape, i, j, k)
          val cid = clMap(lin)
          if cid == 0 then zero else ts(time, idToCol(cid))
      }
    NeuroVec.fromRavel(out, space, label)

  def asDense(using
      ClassTag[A],
      Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVec[A] =
    toDense

  def subVector(tsIdx: Seq[Int])(using
      ClassTag[A],
      DType[A]
  ): ClusteredNeuroVec[A] =
    val tLen = nVolumes
    require(tsIdx.nonEmpty, "tsIdx must be non-empty")
    require(tsIdx.forall(t => t >= 0 && t < tLen), "time index out of bounds")
    val newT = tsIdx.length
    val kLen = numClusters
    val newTs =
      RavelArray.tabulate[A](newT, kLen) { (time, cluster) =>
        ts(tsIdx(time), cluster)
      }
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

  def asMatrix: RavelArray[A, Rank[2]] = ts

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

  def map[B](f: A => B)(using ClassTag[B], DType[B]): ClusteredNeuroVec[B] =
    ClusteredNeuroVec(cvol, ts.map(f), clMap, space, label)

object ClusteredNeuroVec:

  private def meanReducer[A: Field]: Array[A] => A =
    (arr: Array[A]) =>
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
    reducer: Array[A] => A,
    label: String = ""
  )(using ClassTag[A], Ring[A]): ClusteredNeuroVec[A] =
    GridCompatibility.requireSpatial(vec.space, cvol.space)
    val lbl = if label.nonEmpty then label else vec.label
    val spatialNels = vec.space.spatialDims.product
    val tLen = vec.nVolumes
    val ids = cvol.clusterIds
    val kLen = ids.length

    given DType[A] = vec.values.dtype
    val clusterIndices = ids.map(cvol.clusterMap)
    val scratch = clusterIndices.map(indices => Array.ofDim[A](indices.size))
    val ts =
      RavelArray.tabulate[A](tLen, kLen) { (time, cluster) =>
        val idxArr = clusterIndices(cluster)
        if idxArr.size == 1 then
          vec.valueAtVoxelOrdinal(idxArr(0), time)
        else
          val tmp = scratch(cluster)
          var p = 0
          while p < idxArr.size do
            tmp(p) = vec.valueAtVoxelOrdinal(idxArr(p), time)
            p += 1
          reducer(tmp)
      }

    val clMap = PrimitiveBuffers.fillConst[Int](spatialNels, 0)
    val activeIdx = Mask.indices(cvol.mask)
    var i = 0
    while i < activeIdx.size do
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
    ts: RavelArray[A, Rank[2]],
    cvol: ClusteredNeuroVol,
    label: String = ""
  )(using ClassTag[A], Ring[A]): ClusteredNeuroVec[A] =
    require(ts.shape(1) == cvol.numClusters, "ts/cluster mismatch")
    val tLen = ts.shape(0)
    val spatialNels = cvol.space.spatialDims.product
    val clMap = PrimitiveBuffers.fillConst[Int](spatialNels, 0)
    val activeIdx = Mask.indices(cvol.mask)
    var i = 0
    while i < activeIdx.size do
      clMap(activeIdx(i)) = cvol.clusters(i)
      i += 1
    val sp4 = cvol.space.addDim(tLen, Some(Axis.Time))
    ClusteredNeuroVec(cvol, ts, clMap, sp4, label)
