package scalafim.atlas

import locus4s.{PartialSurjection, Region}
import scalafim.image.{DilationMetric, DilationRadius, DilationTie, VolumeDilation, VolumeDilationError}

enum AtlasDilationError:
  case Kernel(error: VolumeDilationError)
  case Realization(error: AtlasRealizationError)

  def message: String = this match
    case Kernel(error) => error.message
    case Realization(error) => error.message

object AtlasDilation:
  def volume(parent: VolumeAtlasRealization)(
      radius: DilationRadius, metric: DilationMetric, tie: DilationTie,
      mask: Region[parent.X]
  ): Either[AtlasDilationError, VolumeAtlasRealization { type F = parent.F; type X = parent.X }] =
    VolumeDilation(parent.parcellation, radius, metric, tie, mask)
      .left.map(AtlasDilationError.Kernel.apply).flatMap: assignment =>
        val maskBytes = AtlasPublicationBinaryV1.finiteIndexed("scalafim.atlas/dilation-mask", "1",
          mask.ordinalsInDomainOrder.map(_.toString).toVector)
        val step = DerivationStep.DilatedParcels(parent.identity, metric.toString,
          java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(radius.value)), tie.toString,
          Digest.sha256(AtlasPublicationSha256.hex(maskBytes)))
        AtlasRealization.buildVolumeIn[parent.F, parent.X](parent.registry, parent.ref,
          RegionIndex(parent.displayOrder.indices.map(parent.metadata.apply).toVector), parent.domain,
          parent.parcellation.imageMetadata, parent.provenance.withDerivationStep(step),
          [P] => (parcels: locus4s.FiniteDomain[P], ordinals: Map[RegionId, Int]) =>
            PartialSurjection.fromOptionalTargetOrdinals(parent.domain.space, parcels,
              parent.domain.space.indices.map(voxel => assignment(voxel).map(p => ordinals(parent.metadata(p).id))))
              .left.map(AtlasRealization.partialAssignmentError)
        ).left.map(AtlasDilationError.Realization.apply)
