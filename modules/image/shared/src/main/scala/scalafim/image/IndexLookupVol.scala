package scalafim.image

import ravel.Array1
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

final case class IndexLookupVol private (
  space: NeuroSpace,
  indices: Array1[Int],
  private val map: Array1[Int]
):
  val cardinality: Int = indices.size

  def lookup(linear: Int): Int =
    require(linear >= 0 && linear < space.spatialDims.product, "linear index out of range")
    map(linear)

  def coords(linear: Int): Option[Vector[Int]] =
    val pos = lookup(linear)
    if pos < 0 then None else Some(Indexing.indexToGrid3D(space.spatialDims, indices(pos)))

object IndexLookupVol:
  def apply(space: NeuroSpace, rawIndices: Array1[Int]): IndexLookupVol =
    val indexSet = VoxelIndexSet(space, rawIndices)
    fromIndexSet(indexSet)

  def apply(space: NeuroSpace, rawIndices: Array[Int]): IndexLookupVol =
    fromIndexSet(VoxelIndexSet(space, rawIndices))

  /** Preserve the validated order carried by `indexSet`.
    *
    * Compact sparse columns use this order as part of their representation;
    * canonical sorting would silently detach lookup positions from columns.
    */
  private[image] def fromIndexSet(indexSet: VoxelIndexSet): IndexLookupVol =
    val space = indexSet.space.toNeuroSpace
    val nels = indexSet.space.nVoxels
    val idxArr = indexSet.indices
    val mapArr =
      NDArray.build[Int, Rank[1]](Shape(nels)): output =>
        var linear = 0
        while linear < nels do
          output.writeLinear(linear, -1)
          linear += 1
        var position = 0
        while position < idxArr.size do
          output.writeLinear(idxArr(position), position)
          position += 1

    IndexLookupVol(space, idxArr, mapArr)
