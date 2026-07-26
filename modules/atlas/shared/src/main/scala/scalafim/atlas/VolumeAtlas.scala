package scalafim.atlas

import scalafim.image.*

trait Atlas:
  def ref: AtlasRef
  def provenance: AtlasProvenance
  def regions: RegionIndex
  def quotient: AtlasQuotient

  def family: String = ref.family
  def model: String = ref.model
  def name: String = ref.name
  def representation: AtlasRepresentation = ref.representation

final case class VolumeAtlas(
  ref: AtlasRef,
  regions: RegionIndex,
  volume: ClusteredNeuroVol,
  provenance: AtlasProvenance
) extends Atlas:
  require(ref.representation == AtlasRepresentation.Volume, "VolumeAtlas requires volume representation")

  private val regionIdSet = regions.ids.toSet
  private val payloadIdSet = volume.clusterIds.map(RegionId(_)).toSet
  require(regionIdSet == payloadIdSet, "region ids must match volume cluster ids")

  lazy val quotient: VolumeAtlasQuotient =
    AtlasQuotient.volume(ref.coordSpace.value, ref.name, regions, volume)

  def space: NeuroSpace =
    volume.space

  lazy val labelVolume: NeuroVol[Int] =
    volume.toDense

  def region(id: RegionId): Option[AtlasRegionMetadata] =
    regions.get(id)

  def region(label: String, hemisphere: Option[Hemisphere] = None): Vector[AtlasRegionMetadata] =
    regions.find(label, hemisphere)

  def subset(p: AtlasRegionMetadata => Boolean): VolumeAtlas =
    val kept = regions.regions.filter(p)
    require(kept.nonEmpty, "atlas subset must keep at least one region")
    val keepIds = kept.map(_.id.value).toSet
    val dense = volume.toDense
    val flags = scalafim.image.NArrayUtil.fillConst[Boolean](space.spatialDims.product, false)
    val values = Array.newBuilder[Int]
    var lin = 0
    while lin < flags.length do
      val id = dense.linear(lin)
      if keepIds.contains(id) then
        flags(lin) = true
        values += id
      lin += 1
    val mask = NeuroVol.fromLinear[Boolean](flags, space, volume.label)
    val outRegions = RegionIndex(kept)
    val clusters = scalafim.image.NArrayUtil.fromArray(values.result())
    copy(
      regions = outRegions,
      volume = ClusteredNeuroVol(mask, clusters, outRegions.labelMap, volume.label),
      provenance = provenance.withLabels(LabelSchema.fromRegions(ref, outRegions, provenance.sourceArtifacts))
    )

object VolumeAtlas:
  def apply(ref: AtlasRef, regions: RegionIndex, volume: ClusteredNeuroVol): VolumeAtlas =
    new VolumeAtlas(ref, regions, volume, AtlasProvenance.fromRef(ref, regions))

  def fromLabelVolume(ref: AtlasRef, regions: RegionIndex, labels: NeuroVol[Int], label: String = ""): VolumeAtlas =
    fromLabelVolume(ref, regions, labels, label, AtlasProvenance.fromRef(ref, regions))

  def fromLabelVolume(
    ref: AtlasRef,
    regions: RegionIndex,
    labels: NeuroVol[Int],
    label: String,
    provenance: AtlasProvenance
  ): VolumeAtlas =
    val ids = regions.ids.map(_.value).toSet
    val flags = scalafim.image.NArrayUtil.fillConst[Boolean](labels.space.spatialDims.product, false)
    val values = Array.newBuilder[Int]
    var lin = 0
    while lin < flags.length do
      val id = labels.linear(lin)
      if id != 0 then
        require(ids.contains(id), AtlasError.MissingRegionId(RegionId(id)).message)
        flags(lin) = true
        values += id
      lin += 1

    val clusterValues = values.result()
    val present = clusterValues.toSet
    val missing = ids.diff(present)
    require(missing.isEmpty, s"label volume is missing region ids: ${missing.toVector.sorted.mkString(", ")}")

    val mask = NeuroVol.fromLinear[Boolean](flags, labels.space, label)
    val clusters = scalafim.image.NArrayUtil.fromArray(clusterValues)
    VolumeAtlas(ref, regions, ClusteredNeuroVol(mask, clusters, regions.labelMap, label), provenance)
