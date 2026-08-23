package scalafim.image

import ravel.Array1
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

object Mask:

  type MaskVol = NeuroVol[Boolean]

  def fromIndices(space: NeuroSpace, indices: Array1[Int], label: String = ""): MaskVol =
    fromIndexSet(VoxelIndexSet(space, indices), label)

  def fromIndices(space: NeuroSpace, indices: Array[Int], label: String): MaskVol =
    fromIndexSet(VoxelIndexSet(space, indices), label)

  def fromIndices(space: NeuroSpace, indices: Array[Int]): MaskVol =
    fromIndices(space, indices, "")

  def fromIndexSet(indexSet: VoxelIndexSet, label: String = ""): MaskVol =
    val volumeSpace = indexSet.space
    val shape = volumeSpace.shape
    val flags =
      NDArray.build[Boolean, Rank[3]](
        Shape(shape.x, shape.y, shape.z)
      ): output =>
        var i = 0
        while i < indexSet.size do
          val voxel = Indexing.indexToGrid3D(shape, indexSet(i))
          val canonical =
            (voxel.x * shape.y + voxel.y) * shape.z + voxel.z
          output.writeLinear(canonical, true)
          i += 1
    NeuroVol.fromRavel(flags, volumeSpace.toNeuroSpace, label)

  def indices(mask: MaskVol): Array1[Int] =
    var count = 0
    var i = 0
    while i < mask.values.size do
      if mask.linear(i) then count += 1
      i += 1
    NDArray.build[Int, Rank[1]](Shape(count)): output =>
      var linear = 0
      var position = 0
      while linear < mask.values.size do
        if mask.linear(linear) then
          output.writeLinear(position, linear)
          position += 1
        linear += 1

  def indexSet(mask: MaskVol): VoxelIndexSet =
    VoxelIndexSet(VolumeSpace.unsafe(mask.space.spatialSpace), indices(mask))

  def all(space: NeuroSpace, label: String = ""): MaskVol =
    val shape = space.spatialShape
    NeuroVol.fromRavel[Boolean](
      NDArray.fill(Shape(shape.x, shape.y, shape.z), true),
      space.spatialSpace,
      label
    )

  def of[A](vol: NeuroVol[A]): MaskVol =
    all(vol.space, vol.label)

  @scala.annotation.targetName("ofNeuroVec")
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
