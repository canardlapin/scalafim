package scalafim.image

import narr.NArray

object Mask:

  type MaskVol = NeuroVol[Boolean]

  def fromIndices(space: NeuroSpace, indices: NArray[Int], label: String = ""): MaskVol =
    fromIndexSet(VoxelIndexSet(space, indices), label)

  def fromIndexSet(indexSet: VoxelIndexSet, label: String = ""): MaskVol =
    val volumeSpace = indexSet.space
    val nels = volumeSpace.nVoxels
    val flags = NArrayUtil.fillConst[Boolean](nels, false)
    var i = 0
    while i < indexSet.size do
      val idx = indexSet(i)
      flags(idx) = true
      i += 1
    NeuroVol(NDArray(flags, volumeSpace.dims), volumeSpace.toNeuroSpace, label)

  def indices(mask: MaskVol): NArray[Int] =
    val flags = mask.values.data
    val buf = Array.newBuilder[Int]
    var i = 0
    while i < flags.length do
      if flags(i) then buf += i
      i += 1
    NArrayUtil.fromArray(buf.result())

  def indexSet(mask: MaskVol): VoxelIndexSet =
    VoxelIndexSet(VolumeSpace.unsafe(mask.space.spatialSpace), indices(mask))

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
