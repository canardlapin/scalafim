package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaRun, SharedBasisArtifact, SharedBasisId, SharedBasisLocator}
import scalafim.image.NeuroSpace
import gale.linalg.DMat

object LatentArchiveCodec:
  def toArchive(
      response: ExplicitLatentResponse,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    ExplicitLatentArchiveCodec.toArchive(response, space, runLabel, creator)

  def toTemporalDctArchive(
      data: DMat,
      space: NeuroSpace,
      components: Int,
      norm: DctNorm = DctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent",
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    ExplicitLatentArchiveCodec.toTemporalDctArchive(
      data = data,
      space = space,
      components = components,
      norm = norm,
      center = center,
      ridge = ridge,
      runLabel = runLabel,
      creator = creator,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def toTemporalDctArchiveSpec(
      data: DMat,
      space: NeuroSpace,
      spec: DctSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent",
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    ExplicitLatentArchiveCodec.toTemporalDctArchiveSpec(
      data = data,
      space = space,
      spec = spec,
      center = center,
      ridge = ridge,
      runLabel = runLabel,
      creator = creator,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def toSharedBasisArchive(
      data: DMat,
      space: NeuroSpace,
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent",
      sourceDomain: DomainId = DomainId.unsafe("shared_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    SharedBasisLatentArchiveCodec.toArchive(
      data = data,
      space = space,
      basis = basis,
      basisId = basisId,
      locator = locator,
      center = center,
      ridge = ridge,
      runLabel = runLabel,
      creator = creator,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def toRadialBasisArchive(
      data: DMat,
      space: NeuroSpace,
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent",
      sourceDomain: DomainId = DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    radialBasis
      .toSharedBasisArtifact(maskDims = maskDims, params = artifactParams)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))
      .flatMap { basis =>
        radialBasis
          .dataInMaskOrder(data)
          .left
          .map(error => ArchiveError.InvalidArchive(error.message))
          .flatMap { canonicalData =>
            toSharedBasisArchive(
              data = canonicalData,
              space = space,
              basis = basis,
              basisId = basisId,
              locator = locator,
              center = center,
              ridge = ridge,
              runLabel = runLabel,
              creator = creator,
              sourceDomain = sourceDomain,
              targetDomain = targetDomain,
              label = label,
              metadata = radialBasis.sharedBasisParams(metadata)
            )
          }
      }

  def toBoldZipArchive(
      response: BoldZipPayload,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    BoldZipLatentArchiveCodec.toArchive(response, space, runLabel, creator)

  def toTransportArchive(
      response: TransportLatentResponse,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    TransportLatentArchiveCodec.toArchive(response, space, runLabel, creator)

  def fromTransportArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, TransportLatentResponse] =
    TransportLatentArchiveCodec.fromArchive(archive, runLabel)

  def fromBoldZipArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, BoldZipPayload] =
    BoldZipLatentArchiveCodec.fromArchive(archive, runLabel)

  def isBoldZipArchive(archive: LnaArchive): Boolean =
    BoldZipLatentArchiveCodec.isArchive(archive)

  def isTransportArchive(archive: LnaArchive): Boolean =
    TransportLatentArchiveCodec.isArchive(archive)

  def openPlan(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchivePlan] =
    maybeOpenPlan(archive, runLabel).flatMap {
      case Some(plan) => Right(plan)
      case None       => Left(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no latent response descriptor"))
    }

  def maybeOpenPlan(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, Option[LatentArchivePlan]] =
    archive.validate.flatMap { valid =>
      valid.run(runLabel) match
        case None =>
          Left(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        case Some(run) =>
          descriptorForRun(valid, runLabel).flatMap {
            case None =>
              Right(None)
            case Some(descriptor) =>
              planForDescriptor(valid, runLabel, run, descriptor).map(Some(_))
          }
    }

  def openDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchiveDescriptor] =
    maybeOpenDescriptor(archive, runLabel).flatMap {
      case Some(descriptor) => Right(descriptor)
      case None             => Left(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no latent response descriptor"))
    }

  def maybeOpenDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
    archive.validate.flatMap { valid =>
      valid.run(runLabel) match
        case None    => Left(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        case Some(_) => descriptorForRun(valid, runLabel)
    }

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchiveResponse] =
    openPlan(archive, runLabel).map(_.archiveResponse)

  def fromExplicitArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, ExplicitLatentResponse] =
    ExplicitLatentArchiveCodec.fromArchive(archive, runLabel)

  def fromSharedBasisArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, SharedBasisLatentArchive] =
    SharedBasisLatentArchiveCodec.fromArchive(archive, runLabel)

  private def descriptorForRun(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
    ArchiveFamily.firstDescriptor(archive, runLabel)

  private def planForDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel,
      run: LnaRun,
      descriptor: LatentArchiveDescriptor
  ): Either[ArchiveError, LatentArchivePlan] =
    ArchiveFamily.forDescriptor(descriptor).openPlan(archive, runLabel, run, descriptor)

  private enum ArchiveFamily:
    case BoldZip, Transport, TemporalDct, SharedBasis, Explicit

    def detects(descriptor: LatentArchiveDescriptor): Boolean =
      (this, descriptor) match
        case (BoldZip, LatentArchiveDescriptor.BoldZip(_)) =>
          true
        case (Transport, LatentArchiveDescriptor.Transport(_)) =>
          true
        case (TemporalDct, LatentArchiveDescriptor.TemporalDct(_, _)) =>
          true
        case (SharedBasis, LatentArchiveDescriptor.SharedBasis(_)) =>
          true
        case (Explicit, LatentArchiveDescriptor.Explicit(_)) =>
          true
        case (Explicit, LatentArchiveDescriptor.TemporalHaar(_, _, _, _)) => true
        case _ =>
          false

    def descriptorOption(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      this match
        case BoldZip =>
          Right(
            BoldZipLatentArchiveCodec
              .descriptorOption(archive, runLabel)
              .map(LatentArchiveDescriptor.BoldZip(_))
          )

        case Transport =>
          Right(
            TransportLatentArchiveCodec
              .descriptorOption(archive, runLabel)
              .map(LatentArchiveDescriptor.Transport(_))
          )

        case TemporalDct =>
          ExplicitLatentArchiveCodec.temporalDctDescriptor(archive, runLabel) match
            case Some(desc) =>
              ExplicitLatentArchiveCodec
                .explicitDescriptor(archive, runLabel)
                .map(embed => LatentArchiveDescriptor.TemporalDct(desc, embed))
                .map(Some(_))
                .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has temporal DCT params but no explicit latent embed descriptor"))
            case None =>
              Right(None)

        case SharedBasis =>
          Right(
            SharedBasisLatentArchiveCodec
              .descriptorOption(archive, runLabel)
              .map(LatentArchiveDescriptor.SharedBasis(_))
          )

        case Explicit =>
          ExplicitLatentArchiveCodec.explicitDescriptor(archive, runLabel) match
            case None       => Right(None)
            case Some(desc) => ExplicitLatentArchiveCodec.explicitDescriptorAlgebra(desc).map(Some(_))

    def openPlan(
        archive: LnaArchive,
        runLabel: RunLabel,
        run: LnaRun,
        descriptor: LatentArchiveDescriptor
    ): Either[ArchiveError, LatentArchivePlan] =
      (this, descriptor) match
        case (BoldZip, descriptor @ LatentArchiveDescriptor.BoldZip(_)) =>
          fromBoldZipArchive(archive, runLabel).map(response => LatentArchivePlan.BoldZip(run, descriptor, response))

        case (Transport, descriptor @ LatentArchiveDescriptor.Transport(_)) =>
          fromTransportArchive(archive, runLabel).map(response => LatentArchivePlan.Transport(run, descriptor, response))

        case (TemporalDct, descriptor @ LatentArchiveDescriptor.TemporalDct(temporal, _)) =>
          fromExplicitArchive(archive, runLabel).flatMap { response =>
            ExplicitLatentArchiveCodec.temporalDctSpec(archive, runLabel, temporal).map { case (spec, center, ridge) =>
              LatentArchivePlan.TemporalDct(run, descriptor, response, spec, center, ridge)
            }
          }

        case (SharedBasis, descriptor @ LatentArchiveDescriptor.SharedBasis(_)) =>
          fromSharedBasisArchive(archive, runLabel).map(response => LatentArchivePlan.SharedBasis(run, descriptor, response))

        case (Explicit, descriptor @ LatentArchiveDescriptor.Explicit(_)) =>
          fromExplicitArchive(archive, runLabel).map(response => LatentArchivePlan.Explicit(run, descriptor, response))

        case (Explicit, descriptor @ LatentArchiveDescriptor.TemporalHaar(_, spec, center, ridge)) =>
          fromExplicitArchive(archive, runLabel).map { response =>
            LatentArchivePlan.TemporalHaar(run, descriptor, response, spec, center, ridge)
          }

        case _ =>
          Left(ArchiveError.InvalidArchive(s"${this.toString} archive family cannot open ${descriptor.kind.toString} descriptor"))

  private object ArchiveFamily:
    val ordered: Vector[ArchiveFamily] =
      Vector(ArchiveFamily.BoldZip, ArchiveFamily.Transport, ArchiveFamily.TemporalDct, ArchiveFamily.SharedBasis, ArchiveFamily.Explicit)

    def firstDescriptor(
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      firstDescriptor(ordered, archive, runLabel)

    def forDescriptor(descriptor: LatentArchiveDescriptor): ArchiveFamily =
      descriptor match
        case LatentArchiveDescriptor.BoldZip(_) =>
          ArchiveFamily.BoldZip
        case LatentArchiveDescriptor.Transport(_) =>
          ArchiveFamily.Transport
        case LatentArchiveDescriptor.TemporalDct(_, _) =>
          ArchiveFamily.TemporalDct
        case LatentArchiveDescriptor.SharedBasis(_) =>
          ArchiveFamily.SharedBasis
        case LatentArchiveDescriptor.Explicit(_) | LatentArchiveDescriptor.TemporalHaar(_, _, _, _) =>
          ArchiveFamily.Explicit

    private def firstDescriptor(
        families: Vector[ArchiveFamily],
        archive: LnaArchive,
        runLabel: RunLabel
    ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
      families.headOption match
        case None =>
          Right(None)
        case Some(family) =>
          family.descriptorOption(archive, runLabel).flatMap {
            case Some(descriptor) => Right(Some(descriptor))
            case None             => firstDescriptor(families.tail, archive, runLabel)
          }
