package scalafim.image

import narr.NArray

final case class IndexLookupVol private (
  space: NeuroSpace,
  indices: NArray[Int],
  private val map: NArray[Int]
):
  val cardinality: Int = indices.length

  def lookup(linear: Int): Int =
    require(linear >= 0 && linear < space.spatialDims.product, "linear index out of range")
    map(linear)

  def coords(linear: Int): Option[Vector[Int]] =
    val pos = lookup(linear)
    if pos < 0 then None else Some(Indexing.indexToGrid3D(space.spatialDims, indices(pos)))

object IndexLookupVol:
  def apply(space: NeuroSpace, rawIndices: NArray[Int]): IndexLookupVol =
    val indexSet = VoxelIndexSet(space, rawIndices)
    val nels = indexSet.space.nVoxels
    val idxArr = indexSet.unsafeArray
    val mapArr = NArrayUtil.fillConst[Int](nels, -1)
    var i = 0
    while i < idxArr.length do
      mapArr(idxArr(i)) = i
      i += 1

    IndexLookupVol(space, idxArr, mapArr)
