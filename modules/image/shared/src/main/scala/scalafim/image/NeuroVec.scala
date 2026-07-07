package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

final case class NeuroVec[A](
  values: NDArray[A],
  space: NeuroSpace,
  label: String = ""
):
  require(values.ndim == 4, "NeuroVec must be 4D")
  require(values.shape == space.dims.take(4), "data/space dimension mismatch")
  val seriesSpace: SeriesSpace =
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
    values.data(i)

  def asMatrix: NDArray[A] =
    val spatialNels = space.spatialDims.product
    values.reshape(Vector(spatialNels, nVolumes))

  def subArray(
    i: Seq[Int],
    j: Seq[Int],
    k: Seq[Int],
    t: Seq[Int]
  )(using ClassTag[A]): NDArray[A] =
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

  def volume(t: Int)(using ClassTag[A]): NeuroVol[A] =
    require(t >= 0 && t < nVolumes, "t out of bounds")
    val spatialNels = space.spatialDims.product
    val offset = t * spatialNels
    val out = narr.NArray.ofSize[A](spatialNels)
    narr.NArray.copy(values.data, offset, out, 0, spatialNels)
    NeuroVol.fromLinear(out, space.spatialSpace, label)

  def series(linearSpatial: Int)(using ClassTag[A]): NArray[A] =
    val spatialNels = space.spatialDims.product
    require(linearSpatial >= 0 && linearSpatial < spatialNels, "spatial index out of bounds")
    val tLen = nVolumes
    val out = narr.NArray.ofSize[A](tLen)
    var t = 0
    while t < tLen do
      out(t) = values.data(linearSpatial + t * spatialNels)
      t += 1
    out

  def series(i: Int, j: Int, k: Int)(using ClassTag[A]): NArray[A] =
    val lin = Indexing.gridToIndex3D(space.spatialDims, i, j, k)
    series(lin)

  def series(linearSpatial: NArray[Int])(using ClassTag[A]): NDArray[A] =
    val spatialNels = space.spatialDims.product
    val tLen = nVolumes
    val nVox = linearSpatial.length
    val out = narr.NArray.ofSize[A](tLen * nVox)
    var p = 0
    while p < nVox do
      val lin = linearSpatial(p)
      require(lin >= 0 && lin < spatialNels, "spatial index out of bounds")
      var t = 0
      while t < tLen do
        out(t + p * tLen) = values.data(lin + t * spatialNels)
        t += 1
      p += 1
    NDArray(out, Vector(tLen, nVox))

  def series(indexSet: VoxelIndexSet)(using ClassTag[A]): NDArray[A] =
    require(indexSet.space == seriesSpace.volumeSpace, "index set/space mismatch")
    series(indexSet.unsafeArray)

  def series(roi: ROICoords)(using ClassTag[A]): NDArray[A] =
    series(roi.linearIndices(space.spatialSpace))

  def series(coords: Vector[Vector[Int]])(using ClassTag[A]): NDArray[A] =
    series(ROICoords(coords))

  def series(mask: NeuroVol[Boolean])(using ClassTag[A]): NDArray[A] =
    series(Mask.indexSet(mask))

  def seriesRoi(roi: ROICoords)(using ClassTag[A]): ROIVec[A] =
    val lin = roi.linearIndices(space.spatialSpace)
    ROIVec(space, roi, series(lin))

  def asSparse(mask: NeuroVol[Boolean], label: String = this.label)(using ClassTag[A]): SparseNeuroVec[A] =
    SparseNeuroVec.fromDense(values.data, space, mask, label)

  def asSparse(indices: NArray[Int])(using ClassTag[A]): SparseNeuroVec[A] =
    asSparse(indices, this.label)

  def asSparse(indices: NArray[Int], label: String)(using ClassTag[A]): SparseNeuroVec[A] =
    val indexSet = VoxelIndexSet.unique(seriesSpace.volumeSpace, indices)
    asSparse(indexSet, label)

  def asSparse(indexSet: VoxelIndexSet)(using ClassTag[A]): SparseNeuroVec[A] =
    asSparse(indexSet, this.label)

  def asSparse(indexSet: VoxelIndexSet, label: String)(using ClassTag[A]): SparseNeuroVec[A] =
    require(indexSet.space == seriesSpace.volumeSpace, "index set/space mismatch")
    val m = Mask.fromIndexSet(indexSet)
    asSparse(m, label)

  def asSparse(roi: ROICoords)(using ClassTag[A]): SparseNeuroVec[A] =
    asSparse(roi, this.label)

  def asSparse(roi: ROICoords, label: String)(using ClassTag[A]): SparseNeuroVec[A] =
    asSparse(roi.linearIndices(space.spatialSpace), label)

  def splitClusters(clusters: ClusteredNeuroVol)(using ClassTag[A]): Vector[ROIVec[A]] =
    require(clusters.space.spatialDims == space.spatialDims, "cluster space mismatch")
    clusters.clusterMap.toVector.sortBy(_._1).map { case (_, idx) =>
      val coords = Vector.tabulate(idx.length)(i => Indexing.indexToGrid3D(space.spatialDims, idx(i)))
      ROIVec(space, ROICoords(coords), series(idx))
    }

  def subVector(ts: Seq[Int])(using ClassTag[A]): NeuroVec[A] =
    val tLen = nVolumes
    require(ts.nonEmpty, "ts must be non-empty")
    require(ts.forall(t => t >= 0 && t < tLen), "time index out of bounds")
    val spatialNels = space.spatialDims.product
    val out = narr.NArray.ofSize[A](spatialNels * ts.length)
    var p = 0
    while p < ts.length do
      val t = ts(p)
      narr.NArray.copy(values.data, t * spatialNels, out, p * spatialNels, spatialNels)
      p += 1
    val newSpace = space.spatialSpace.addDim(ts.length, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, label)

  def concat(that: NeuroVec[A], rest: NeuroVec[A]*)(using ClassTag[A]): NeuroVec[A] =
    val all = Vector(this, that) ++ rest.toVector
    all.foreach(v => require(v.space.spatialDims == space.spatialDims, "spatial dims mismatch"))
    all.foreach(v => require(v.space.spacing == space.spacing && v.space.origin == space.origin, "space mismatch"))

    val spatialNels = space.spatialDims.product
    val totalT = all.map(_.nVolumes).sum
    val out = narr.NArray.ofSize[A](spatialNels * totalT)
    var offset = 0
    all.foreach { v =>
      val len = v.nVolumes * spatialNels
      narr.NArray.copy(v.values.data, 0, out, offset, len)
      offset += len
    }
    val newSpace = space.spatialSpace.addDim(totalT, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, label)

  def map[B](f: A => B)(using ClassTag[B]): NeuroVec[B] =
    NeuroVec(values.map(f), space, label)

object NeuroVec:
  def fromLinear[A](data: NArray[A], space: NeuroSpace, label: String = ""): NeuroVec[A] =
    NeuroVec(NDArray(data, space.dims.take(4)), space, label)
