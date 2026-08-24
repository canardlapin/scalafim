package scalafim.atlas

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.D3
import locus4s.DomainRegistry
import scalafim.image.*

trait Atlas:
  def ref: AtlasRef
  def provenance: AtlasProvenance
  def regions: RegionIndex
  def realization: AtlasRealization

  def family: String = ref.family
  def model: String = ref.model
  def name: String = ref.name
  def representation: AtlasRepresentation = ref.representation

/** A volumetric atlas whose sole membership owner is its exact realization.
  *
  * Dense categorical labels are explicit derived materializations. The atlas
  * never retains a parallel mask, cluster vector, or legacy dense label image.
  */
final class VolumeAtlas private (
    val realization: VolumeAtlasRealization
) extends Atlas:
  def ref: AtlasRef =
    realization.ref

  def provenance: AtlasProvenance =
    realization.provenance

  lazy val regions: RegionIndex =
    RegionIndex(
      realization.displayOrder.indices
        .map(realization.metadata.apply)
        .toVector
    )

  def space: SampleSpace[realization.F, D3] =
    SampleSpace.create(realization.domain.grid, NonSpatialAxes.empty)

  /** Materialize categorical labels in canonical Ravel order.
    *
    * Each call returns a new dense image; membership remains authoritative in
    * `realization.parcelAssignment`.
    */
  def labelVolume: SomeLabelVolume[Int] =
    realization.parcellation
      .renderCategorical(
        realization.metadata.map(_.id.value),
        background = 0
      )
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated atlas failed to render labels: ${error.message}"
          ),
        identity
      )

  def region(id: RegionId): Option[AtlasRegionMetadata] =
    regions.get(id)

  def region(label: String, hemisphere: Option[Hemisphere] = None): Vector[AtlasRegionMetadata] =
    regions.find(label, hemisphere)

  def subset(p: AtlasRegionMetadata => Boolean): VolumeAtlas =
    val kept = regions.regions.filter(p)
    require(kept.nonEmpty, "atlas subset must keep at least one region")
    val keepIds = kept.iterator.map(_.id).toSet
    val rendered =
      realization.parcellation
        .renderCategorical(
          realization.metadata.map: metadata =>
            if keepIds.contains(metadata.id) then metadata.id.value else 0,
          background = 0
        )
        .fold(
          error =>
            throw new IllegalStateException(
              s"validated atlas subset failed to render labels: ${error.message}"
            ),
          identity
        )
    val outRegions = RegionIndex(kept)
    val outProvenance =
      provenance.withLabels(
        LabelSchema.fromRegions(ref, outRegions, provenance.sourceArtifacts)
      )
    VolumeAtlas.fromLabelVolume(
      ref,
      outRegions,
      rendered,
      outProvenance
    )

object VolumeAtlas:
  def fromRealization(realization: VolumeAtlasRealization): VolumeAtlas =
    new VolumeAtlas(realization)

  def fromLabelVolumeEither(
      ref: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int],
      provenance: AtlasProvenance
  ): Either[AtlasRealizationError, VolumeAtlas] =
    AtlasRealization
      .volumeFromLabelsIn(
        DomainRegistry.empty,
        ref,
        regions,
        labels,
        provenance
      )
      .map(fromRealization)

  def fromLabelVolume(
      ref: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int]
  ): VolumeAtlas =
    fromLabelVolume(
      ref,
      regions,
      labels,
      AtlasProvenance.fromRef(ref, regions)
    )

  def fromLabelVolume(
      ref: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int],
      provenance: AtlasProvenance
  ): VolumeAtlas =
    fromLabelVolumeEither(ref, regions, labels, provenance)
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )
