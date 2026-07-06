package scalafim.image

import narr.NArray

object Mask:

  type MaskVol = NeuroVol[Boolean]

  def fromIndices(space: NeuroSpace, indices: NArray[Int], label: String = ""): MaskVol =
    val nels = space.spatialDims.product
    val flags = NArrayUtil.fillConst[Boolean](nels, false)
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      require(idx >= 0 && idx < nels, "mask index out of bounds")
      flags(idx) = true
      i += 1
    NeuroVol(NDArray(flags, space.spatialDims), space, label)

  def indices(mask: MaskVol): NArray[Int] =
    val flags = mask.values.data
    val buf = Array.newBuilder[Int]
    var i = 0
    while i < flags.length do
      if flags(i) then buf += i
      i += 1
    NArrayUtil.fromArray(buf.result())

  def all(space: NeuroSpace, label: String = ""): MaskVol =
    NeuroVol.fromLinear[Boolean](
      NArrayUtil.fillConst[Boolean](space.spatialDims.product, true),
      space.spatialSpace,
      label
    )

  def of[A](vol: NeuroVol[A]): MaskVol =
    all(vol.space, vol.label)

  def of[A](vec: NeuroVec[A]): MaskVol =
    all(vec.space.spatialSpace, vec.label)

  def of[A](svol: SparseNeuroVol[A]): MaskVol =
    svol.toMask

  def of[A](svec: SparseNeuroVec[A]): MaskVol =
    svec.mask

  def of(cvol: ClusteredNeuroVol): MaskVol =
    cvol.mask

  def of[A](cvec: ClusteredNeuroVec[A]): MaskVol =
    cvec.cvol.mask

  def of[A](hvec: NeuroHyperVec[A]): MaskVol =
    hvec.mask
