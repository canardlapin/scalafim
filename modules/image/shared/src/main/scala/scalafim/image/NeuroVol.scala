package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.{Order, Ring}

final case class NeuroVol[A](
  values: NDArray[A],
  space: NeuroSpace,
  label: String = ""
):
  require(values.ndim == 3, "NeuroVol must be 3D")
  require(values.shape == space.spatialDims, "data/space dimension mismatch")

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
    Mask.fromIndices(space, indices, label)

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
    val spatialNels = space.spatialDims.product
    val out = NArrayUtil.ofSize[A](indices.length)
    var p = 0
    while p < indices.length do
      val lin = indices(p)
      require(lin >= 0 && lin < spatialNels, "indices out of range")
      out(p) = linear(lin)
      p += 1
    SparseNeuroVol(out, indices, space, label)

  def gridToIndex(i: Int, j: Int, k: Int): Int =
    space.gridToIndex3D(i, j, k)

  def gridToIndex(coord: VoxelCoord): Int =
    space.gridToIndex3D(coord)

  def indexToGrid(idx: Int): Vector[Int] =
    space.indexToGrid3D(idx)

  def indexToVoxel(idx: Int): VoxelCoord =
    space.indexToVoxel3D(idx)

  def linear(i: Int): A =
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

object NeuroVol:
  def fromLinear[A](data: NArray[A], space: NeuroSpace, label: String = ""): NeuroVol[A] =
    NeuroVol(NDArray(data, space.spatialDims), space, label)
