package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.Ring

final case class SparseNeuroVol[A](
  data: NArray[A],
  indices: NArray[Int],
  space: NeuroSpace,
  label: String = ""
):
  require(space.ndim >= 3, "space must be at least 3D")
  private val volumeSpace =
    VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)
  val indexSet: VoxelIndexSet =
    VoxelIndexSet.unique(volumeSpace, indices)
  require(data.length == indexSet.size, "data/indices length mismatch")

  lazy val map: IndexLookupVol = IndexLookupVol(space, indices)

  def toMask: NeuroVol[Boolean] =
    toMask(this.label)

  def toMask(label: String): NeuroVol[Boolean] =
    Mask.fromIndexSet(indexSet, label)

  def toDense(using ClassTag[A], spire.algebra.Ring[A]): NeuroVol[A] =
    val zero = summon[Ring[A]].zero
    val filled = NArrayUtil.fillConst[A](space.spatialDims.product, zero)
    var i = 0
    while i < indices.length do
      filled(indices(i)) = data(i)
      i += 1
    NeuroVol.fromLinear(filled, space, label)

object SparseNeuroVol:
  def fromIndexSet[A](
    data: NArray[A],
    indexSet: VoxelIndexSet,
    label: String
  ): SparseNeuroVol[A] =
    SparseNeuroVol(data, indexSet.indices, indexSet.space.toNeuroSpace, label)

  def fromIndexSet[A](
    data: NArray[A],
    indexSet: VoxelIndexSet,
    space: NeuroSpace,
    label: String = ""
  ): SparseNeuroVol[A] =
    val actual = VolumeSpace.fromSpatialPart(space).fold(err => throw new IllegalArgumentException(err.message), identity)
    require(actual == indexSet.space, "index set/space mismatch")
    SparseNeuroVol(data, indexSet.indices, space, label)

  def fromMask[A](
    data: NArray[A],
    space: NeuroSpace,
    mask: NeuroVol[Boolean],
    label: String = ""
  )(using ClassTag[A]): SparseNeuroVol[A] =
    require(mask.space.spatialDims == space.spatialDims, "mask/space mismatch")
    val flags = mask.values.data
    val buf = Array.newBuilder[Int]
    var i = 0
    while i < flags.length do
      if flags(i) then buf += i
      i += 1
    val maskIdx = buf.result()
    val vals = narr.NArray.ofSize[A](maskIdx.length)
    var j = 0
    while j < maskIdx.length do
      vals(j) = data(maskIdx(j))
      j += 1
    SparseNeuroVol(vals, NArrayUtil.fromArray(maskIdx), space, label)
