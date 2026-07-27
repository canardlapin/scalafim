package scalafim.latent

import scalafim.archive.ArchiveError
import scalafim.archive.lna.{LnaRun, TransformDescriptor}

enum LatentArchiveKind:
  case Explicit, TemporalDct, TemporalHaar, SharedBasis, Transport, BoldZip

enum LatentArchiveDescriptor:
  case Explicit(descriptor: TransformDescriptor)
  case TemporalDct(temporalDescriptor: TransformDescriptor, embedDescriptor: TransformDescriptor)
  case TemporalHaar(descriptor: TransformDescriptor, spec: HaarSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(descriptor: TransformDescriptor)
  case Transport(descriptor: TransformDescriptor)
  case BoldZip(descriptor: TransformDescriptor)

  def kind: LatentArchiveKind =
    this match
      case Explicit(_)              => LatentArchiveKind.Explicit
      case TemporalDct(_, _)        => LatentArchiveKind.TemporalDct
      case TemporalHaar(_, _, _, _) => LatentArchiveKind.TemporalHaar
      case SharedBasis(_)           => LatentArchiveKind.SharedBasis
      case Transport(_)             => LatentArchiveKind.Transport
      case BoldZip(_)               => LatentArchiveKind.BoldZip

enum LatentArchiveResponse:
  case Explicit(response: ExplicitLatentResponse)
  case TemporalDct(response: ExplicitLatentResponse, spec: DctSpec, center: Boolean, ridge: RidgePenalty)
  case TemporalHaar(response: ExplicitLatentResponse, spec: HaarSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(response: SharedBasisLatentArchive)
  case Transport(response: TransportLatentResponse)
  case BoldZip(response: BoldZipPayload)

  def kind: LatentArchiveKind =
    this match
      case Explicit(_)              => LatentArchiveKind.Explicit
      case TemporalDct(_, _, _, _)  => LatentArchiveKind.TemporalDct
      case TemporalHaar(_, _, _, _) => LatentArchiveKind.TemporalHaar
      case SharedBasis(_)           => LatentArchiveKind.SharedBasis
      case Transport(_)             => LatentArchiveKind.Transport
      case BoldZip(_)               => LatentArchiveKind.BoldZip

  def latentResponse: Option[LatentResponse] =
    this match
      case Explicit(response)              => Some(response)
      case TemporalDct(response, _, _, _)  => Some(response)
      case TemporalHaar(response, _, _, _) => Some(response)
      case SharedBasis(_)                  => None
      case Transport(response)             => Some(response)
      case BoldZip(response)               => Some(response)

  def capability: LatentResponseCapability =
    this match
      case Explicit(response)              => LatentResponseCapability.Response(response)
      case TemporalDct(response, _, _, _)  => LatentResponseCapability.Response(response)
      case TemporalHaar(response, _, _, _) => LatentResponseCapability.Response(response)
      case SharedBasis(response)           => LatentResponseCapability.DeferredSharedBasis(response)
      case Transport(response)             => LatentResponseCapability.Response(response)
      case BoldZip(response)               => LatentResponseCapability.Response(response)

enum LatentResponseCapability:
  case Response(response: LatentResponse)
  case DeferredSharedBasis(archive: SharedBasisLatentArchive)
  case Unsupported(reason: ArchiveError)

  def latentResponseOption: Option[LatentResponse] =
    this match
      case Response(response) => Some(response)
      case _                  => None

  def selectionResponse: Either[ArchiveError, LatentResponse] =
    this match
      case Response(response) =>
        Right(response)
      case DeferredSharedBasis(_) =>
        Left(ArchiveError.UnsupportedTransform("shared_basis_embed requires an external shared-basis resolver"))
      case Unsupported(reason) =>
        Left(reason)

enum LatentArchivePlan:
  case Explicit(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, response: ExplicitLatentResponse)
  case TemporalDct(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, response: ExplicitLatentResponse, spec: DctSpec, center: Boolean, ridge: RidgePenalty)
  case TemporalHaar(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, response: ExplicitLatentResponse, spec: HaarSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, archive: SharedBasisLatentArchive)
  case Transport(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, response: TransportLatentResponse)
  case BoldZip(run: LnaRun, archiveDescriptor: LatentArchiveDescriptor, response: BoldZipPayload)

  def kind: LatentArchiveKind =
    descriptor.kind

  def runInfo: LnaRun =
    this match
      case Explicit(run, _, _)                      => run
      case TemporalDct(run, _, _, _, _, _)          => run
      case TemporalHaar(run, _, _, _, _, _)         => run
      case SharedBasis(run, _, _)                   => run
      case Transport(run, _, _)                     => run
      case BoldZip(run, _, _)                       => run

  def descriptor: LatentArchiveDescriptor =
    this match
      case Explicit(_, value, _)              => value
      case TemporalDct(_, value, _, _, _, _)  => value
      case TemporalHaar(_, value, _, _, _, _) => value
      case SharedBasis(_, value, _)           => value
      case Transport(_, value, _)             => value
      case BoldZip(_, value, _)               => value

  def archiveResponse: LatentArchiveResponse =
    this match
      case Explicit(_, _, response) =>
        LatentArchiveResponse.Explicit(response)
      case TemporalDct(_, _, response, spec, center, ridge) =>
        LatentArchiveResponse.TemporalDct(response, spec, center, ridge)
      case TemporalHaar(_, _, response, spec, center, ridge) =>
        LatentArchiveResponse.TemporalHaar(response, spec, center, ridge)
      case SharedBasis(_, _, archive) =>
        LatentArchiveResponse.SharedBasis(archive)
      case Transport(_, _, response) =>
        LatentArchiveResponse.Transport(response)
      case BoldZip(_, _, response) =>
        LatentArchiveResponse.BoldZip(response)

  def latentResponse: Option[LatentResponse] =
    capability.latentResponseOption

  def capability: LatentResponseCapability =
    archiveResponse.capability

  def selectionResponse: Either[ArchiveError, LatentResponse] =
    capability.selectionResponse
