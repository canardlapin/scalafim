package scalafim.image

/** Ordered spatial support for compact image data.
  *
  * `indexSet` defines the compact column order and `lookup` maps full-grid
  * linear voxel indices back to those columns. Both are tied to one checked
  * volume geometry. A dense mask is derived only when a compatibility caller
  * asks for one.
  */
final class SparseSupport private (
    val indexSet: VoxelIndexSet,
    val lookup: IndexLookupVol
):
  val cardinality: Int =
    indexSet.size

  def positionOf(linearVoxel: Int): Int =
    lookup.lookup(linearVoxel)

  def toMask(label: String = ""): NeuroVol[Boolean] =
    Mask.fromIndexSet(indexSet, label)

object SparseSupport:
  def fromIndexSet(indexSet: VoxelIndexSet): SparseSupport =
    val lookup = IndexLookupVol.fromIndexSet(indexSet)
    new SparseSupport(indexSet, lookup)

  def fromMask(mask: NeuroVol[Boolean]): SparseSupport =
    fromIndexSet(Mask.indexSet(mask))

  private[image] def checkedCompatibility(
      space: NeuroSpace,
      mask: NeuroVol[Boolean],
      lookup: IndexLookupVol
  ): SparseSupport =
    GridCompatibility.requireSpatial(space, mask.space)
    GridCompatibility.requireSpatial(space, lookup.space)
    val ordered =
      VoxelIndexSet.unique(space, lookup.indices)
    val maskSet = Mask.indexSet(mask)
    require(
      ordered.size == maskSet.size,
      "mask and compact lookup must have the same cardinality"
    )
    var position = 0
    while position < ordered.size do
      require(
        mask.valueAtCanonicalOrdinal(ordered(position)),
        s"compact lookup voxel ${ordered(position)} is outside the mask"
      )
      position += 1
    val canonicalLookup =
      IndexLookupVol.fromIndexSet(ordered)
    new SparseSupport(ordered, canonicalLookup)
