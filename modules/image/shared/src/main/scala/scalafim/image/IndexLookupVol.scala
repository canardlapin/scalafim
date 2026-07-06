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
    val nels = space.spatialDims.product
    val tmp = Array.ofDim[Int](rawIndices.length)
    var t = 0
    while t < rawIndices.length do
      tmp(t) = rawIndices(t)
      t += 1
    val uniq = tmp.distinct.sorted
    require(uniq.forall(i => i >= 0 && i < nels), "indices out of range")

    val idxArr = NArrayUtil.fromArray(uniq)
    val mapArr = NArrayUtil.fillConst[Int](nels, -1)
    var i = 0
    while i < uniq.length do
      mapArr(uniq(i)) = i
      i += 1

    IndexLookupVol(space, idxArr, mapArr)
