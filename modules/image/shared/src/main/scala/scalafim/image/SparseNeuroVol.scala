package scalafim.image

import ravel.Array1
import ravel.DType
import ravel.NDArray
import spire.algebra.Ring

final case class SparseNeuroVol[A](
  data: Array1[A],
  indices: Array1[Int],
  space: NeuroSpace,
  label: String = ""
):
  require(space.ndim >= 3, "space must be at least 3D")
  private val volumeSpace =
    VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)
  val indexSet: VoxelIndexSet =
    VoxelIndexSet.unique(volumeSpace, indices)
  require(data.size == indexSet.size, "data/indices length mismatch")

  lazy val map: IndexLookupVol = IndexLookupVol(space, indices)

  def toMask: NeuroVol[Boolean] =
    toMask(this.label)

  def toMask(label: String): NeuroVol[Boolean] =
    Mask.fromIndices(space, indexSet.indices, label)

  def toDense(using
      spire.algebra.Ring[A],
      DType[A],
      MigrationValueSemantics[A]
  ): NeuroVol[A] =
    val zero = summon[Ring[A]].zero
    val shape = space.spatialShape
    val filled =
      NDArray.tabulate[A](shape.x, shape.y, shape.z): (x, y, z) =>
        val linear = Indexing.gridToIndex3D(shape, x, y, z)
        val position = map.lookup(linear)
        if position < 0 then zero else data(position)
    NeuroVol.fromRavel(filled, space, label)

object SparseNeuroVol:
  private def ravelValues[A](
      values: Array[A]
  )(using DType[A]): Array1[A] =
    NDArray.fromSeq(ravel.Shape(values.length), values)

  def apply[A](
      data: Array[A],
      indices: Array[Int],
      space: NeuroSpace
  )(using DType[A]): SparseNeuroVol[A] =
    new SparseNeuroVol(
      ravelValues(data),
      NDArray.fromSeq(ravel.Shape(indices.length), indices),
      space
    )

  def apply[A](
      data: Array[A],
      indices: Array[Int],
      space: NeuroSpace,
      label: String
  )(using DType[A]): SparseNeuroVol[A] =
    new SparseNeuroVol(
      ravelValues(data),
      NDArray.fromSeq(ravel.Shape(indices.length), indices),
      space,
      label
    )

  def fromIndexSet[A](
    data: Array1[A],
    indexSet: VoxelIndexSet,
    label: String
  ): SparseNeuroVol[A] =
    SparseNeuroVol(data, indexSet.indices, indexSet.space.toNeuroSpace, label)

  def fromIndexSet[A](
    data: Array1[A],
    indexSet: VoxelIndexSet,
    space: NeuroSpace,
    label: String = ""
  ): SparseNeuroVol[A] =
    val actual = VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)
    GridCompatibility.requireVolume(indexSet.space, actual)
    SparseNeuroVol(data, indexSet.indices, space, label)

  def fromMask[A](
    data: Array1[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using DType[A]): SparseNeuroVol[A] =
    GridCompatibility.requireSpatial(space, mask.space)
    val buf = Array.newBuilder[Int]
    var i = 0
    while i < mask.values.size do
      if mask.valueAtCanonicalOrdinal(i) then buf += i
      i += 1
    val maskIdx = buf.result()
    val vals =
      NDArray.tabulate[A](maskIdx.length)(j => data(maskIdx(j)))
    val indices =
      NDArray.fromSeq(ravel.Shape(maskIdx.length), maskIdx)
    SparseNeuroVol(vals, indices, space, label)
