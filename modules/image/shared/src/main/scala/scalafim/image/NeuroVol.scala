package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.{Order, Ring}

final class NeuroVol[A] private[image] (
  protected val image: NeuroImage[A, Volume3D]
) extends NeuroImageView[A, Volume3D]:

  val volumeSpace: VolumeSpace =
    VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)

  inline def apply(i: Int, j: Int, k: Int): A =
    values(i, j, k)

  inline def apply(coord: VoxelCoord): A =
    apply(coord.x, coord.y, coord.z)

  def apply(coords: ROICoords)(using ClassTag[A]): NArray[A] =
    val dims = space.spatialDims
    val out = NArrayUtil.ofSize[A](coords.size)
    var p = 0
    while p < coords.size do
      val c = coords.coords(p)
      require(c(0) >= 0 && c(0) < dims(0), "roi coord out of bounds")
      require(c(1) >= 0 && c(1) < dims(1), "roi coord out of bounds")
      require(c(2) >= 0 && c(2) < dims(2), "roi coord out of bounds")
      out(p) = apply(c(0), c(1), c(2))
      p += 1
    out

  def apply(roi: VoxelRoi)(using ClassTag[A]): NArray[A] =
    require(roi.space == volumeSpace, "ROI/space mismatch")
    val out = NArrayUtil.ofSize[A](roi.size)
    val coords = roi.coords
    var p = 0
    while p < coords.length do
      out(p) = apply(coords(p))
      p += 1
    out

  def apply(roi: ROIVol[?])(using ClassTag[A]): NArray[A] =
    apply(roi.coords)

  def slices(axis: Int = 2)(using ClassTag[A]): Vector[NeuroSlice[A]] =
    slices(SpatialAxis.unsafe(axis))

  def slices(axis: SpatialAxis)(using ClassTag[A]): Vector[NeuroSlice[A]] =
    val dims = space.spatialShape
    Vector.tabulate(dims(axis))(i => slice(axis, i))

  def toVec(using ClassTag[A]): NeuroVec[A] =
    val spatialNels = space.spatialDims.product
    val out = NArrayUtil.ofSize[A](spatialNels)
    NArrayUtil.copyInto(values.data, 0, out, 0, spatialNels)
    val newSpace = space.spatialSpace.addDim(1, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, label)

  def concat(that: NeuroVol[A], rest: NeuroVol[A]*)(using ClassTag[A]): NeuroVec[A] =
    val all = Vector(this, that) ++ rest.toVector
    val base = all.head.space.spatialSpace
    all.foreach(v => require(v.space.spatialDims == base.spatialDims, "spatial dims mismatch"))
    all.foreach(v => require(v.space.spacing == base.spacing && v.space.origin == base.origin, "space mismatch"))

    val spatialNels = base.spatialDims.product
    val totalT = all.length
    val out = NArrayUtil.ofSize[A](spatialNels * totalT)
    var t = 0
    while t < totalT do
      NArrayUtil.copyInto(all(t).values.data, 0, out, t * spatialNels, spatialNels)
      t += 1

    val newSpace = base.addDim(totalT, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, label)

  def asMatrix: NDArray[A] =
    val spatialNels = space.spatialDims.product
    values.reshape(Vector(spatialNels, 1))

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

  def asMask(indices: NArray[Int], label: String): NeuroVol[Boolean] =
    Mask.fromIndexSet(VoxelIndexSet(volumeSpace, indices), label)

  def asMask(indexSet: VoxelIndexSet, label: String): NeuroVol[Boolean] =
    require(indexSet.space == volumeSpace, "index set/space mismatch")
    Mask.fromIndexSet(indexSet, label)

  def asMask(indexSet: VoxelIndexSet): NeuroVol[Boolean] =
    asMask(indexSet, this.label)

  def asMask(indices: NArray[Int]): NeuroVol[Boolean] =
    asMask(indices, this.label)

  def asSparse(mask: NeuroVol[Boolean], label: String = this.label)(using ClassTag[A]): SparseNeuroVol[A] =
    require(mask.space.spatialDims == space.spatialDims, "mask/space mismatch")
    require(mask.space.spacing == space.spacing && mask.space.origin == space.origin, "mask/space mismatch")
    val idx = Mask.indices(mask)
    asSparse(idx, label)

  def asSparse(indices: NArray[Int])(using ClassTag[A]): SparseNeuroVol[A] =
    asSparse(indices, this.label)

  def asSparse(indices: NArray[Int], label: String)(using ClassTag[A]): SparseNeuroVol[A] =
    val indexSet = VoxelIndexSet.unique(volumeSpace, indices)
    asSparse(indexSet, label)

  def asSparse(indexSet: VoxelIndexSet)(using ClassTag[A]): SparseNeuroVol[A] =
    asSparse(indexSet, this.label)

  def asSparse(indexSet: VoxelIndexSet, label: String)(using ClassTag[A]): SparseNeuroVol[A] =
    require(indexSet.space == volumeSpace, "index set/space mismatch")
    val out = NArrayUtil.ofSize[A](indexSet.size)
    var p = 0
    while p < indexSet.size do
      val lin = indexSet(p)
      out(p) = linear(lin)
      p += 1
    SparseNeuroVol.fromIndexSet(out, indexSet, space, label)

  def gridToIndex(i: Int, j: Int, k: Int): Int =
    space.gridToIndex3D(i, j, k)

  def gridToIndex(coord: VoxelCoord): Int =
    space.gridToIndex3D(coord)

  def indexToGrid(idx: Int): Vector[Int] =
    space.indexToGrid3D(idx)

  def indexToVoxel(idx: Int): VoxelCoord =
    space.indexToVoxel3D(idx)

  override def linear(i: Int): A =
    values.data(i)

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

    val out = narr.NArray.ofSize[A](outDims.product)
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
    NeuroSlice.fromLinear(out, sliceSpace, label)

  def map[B](f: A => B)(using ClassTag[B]): NeuroVol[B] =
    NeuroVol(values.map(f), space, label)

  def copy(values: NDArray[A] = this.values, space: NeuroSpace = this.space, label: String = this.label): NeuroVol[A] =
    NeuroVol(values, space, label)

  override def equals(other: Any): Boolean =
    other match
      case that: NeuroVol[?] => sameImage(that)
      case _ => false

  override def hashCode(): Int =
    imageHash

  override def toString: String =
    s"NeuroVol(values=$values, space=$space, label=$label)"

object NeuroVol:
  def apply[A](values: NDArray[A], space: NeuroSpace, label: String = ""): NeuroVol[A] =
    NeuroImage.make[A, Volume3D](values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), image => new NeuroVol(image))

  def fromLinear[A](data: NArray[A], space: NeuroSpace, label: String = ""): NeuroVol[A] =
    new NeuroVol(NeuroImage.fromLinear[A, Volume3D](data, space, label))
