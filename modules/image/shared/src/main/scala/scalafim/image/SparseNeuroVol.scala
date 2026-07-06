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
  require(data.length == indices.length, "data/indices length mismatch")
  private val maxLin = space.spatialDims.product
  private var ok = true
  private var p = 0
  while ok && p < indices.length do
    val v = indices(p)
    if v < 0 || v >= maxLin then ok = false
    p += 1
  require(ok, "indices out of range")

  lazy val map: IndexLookupVol = IndexLookupVol(space, indices)

  def toMask: NeuroVol[Boolean] =
    toMask(this.label)

  def toMask(label: String): NeuroVol[Boolean] =
    Mask.fromIndices(space, indices, label)

  def toDense(using ClassTag[A], spire.algebra.Ring[A]): NeuroVol[A] =
    val zero = summon[Ring[A]].zero
    val filled = NArrayUtil.fillConst[A](space.spatialDims.product, zero)
    var i = 0
    while i < indices.length do
      filled(indices(i)) = data(i)
      i += 1
    NeuroVol.fromLinear(filled, space, label)

object SparseNeuroVol:
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
