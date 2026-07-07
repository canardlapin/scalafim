package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, SharedBasisArtifact, SharedBasisId, SharedBasisLocator}
import scalafim.image.NeuroSpace
import scalafim.linalg.DoubleMatrix

enum LatentEncodingSpec:
  case ProvidedTemporalBasis(
      basis: DoubleMatrix,
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  )
  case TemporalDct(
      spec: DctSpec,
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  )
  case SharedSpatialBasis(
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator],
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  )

object LatentEncodingSpec:
  def providedBasis(
      basis: DoubleMatrix,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    RidgePenalty(ridge).map { penalty =>
      LatentEncodingSpec.ProvidedTemporalBasis(
        basis = basis,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    }

  def dct(
      timepoints: Int,
      components: Int,
      norm: DctNorm = DctNorm.Ortho,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    for
      penalty <- RidgePenalty(ridge)
      spec <- DctSpec(timepoints, components, norm)
    yield dctSpec(
      spec = spec,
      center = center,
      ridge = penalty,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def dctSpec(
      spec: DctSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): LatentEncodingSpec =
    LatentEncodingSpec.TemporalDct(
      spec = spec,
      center = center,
      ridge = ridge,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

  def sharedBasis(
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("shared_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    RidgePenalty(ridge).map { penalty =>
      sharedBasisSpec(
        basis = basis,
        basisId = basisId,
        locator = locator,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    }

  def sharedBasisSpec(
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("shared_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): LatentEncodingSpec =
    LatentEncodingSpec.SharedSpatialBasis(
      basis = basis,
      basisId = basisId,
      locator = locator,
      center = center,
      ridge = ridge,
      sourceDomain = sourceDomain,
      targetDomain = targetDomain,
      label = label,
      metadata = metadata
    )

enum LatentEncodingResult:
  case Explicit(value: ExplicitLatentResponse)
  case TemporalDct(value: ExplicitLatentResponse, spec: DctSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(encoding: SharedBasisEncoding)

  def response: ExplicitLatentResponse =
    this match
      case Explicit(value)              => value
      case TemporalDct(value, _, _, _) => value
      case SharedBasis(value)           => value.response

  def latentResponse: LatentResponse =
    response

object LatentEncoder:
  def encode(
      data: DoubleMatrix,
      spec: LatentEncodingSpec
  ): Either[LatentError, LatentEncodingResult] =
    spec match
      case LatentEncodingSpec.ProvidedTemporalBasis(basis, center, ridge, sourceDomain, targetDomain, label, metadata) =>
        TemporalBasisEncoder
          .encodeProvided(
            data = data,
            basis = basis,
            center = center,
            ridge = ridge.value,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = label,
            metadata = metadata
          )
          .map(LatentEncodingResult.Explicit(_))

      case LatentEncodingSpec.TemporalDct(dctSpec, center, ridge, sourceDomain, targetDomain, label, metadata) =>
        TemporalBasisEncoder
          .encodeDctSpec(
            data = data,
            spec = dctSpec,
            center = center,
            ridge = ridge,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = label,
            metadata = metadata
          )
          .map(response => LatentEncodingResult.TemporalDct(response, dctSpec, center, ridge))

      case LatentEncodingSpec.SharedSpatialBasis(basis, basisId, locator, center, ridge, sourceDomain, targetDomain, label, metadata) =>
        SharedBasisEncoder
          .encode(
            data = data,
            basis = basis,
            basisId = basisId,
            locator = locator,
            center = center,
            ridge = ridge.value,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = label,
            metadata = metadata
          )
          .map(LatentEncodingResult.SharedBasis(_))

  def encodeResponse(
      data: DoubleMatrix,
      spec: LatentEncodingSpec
  ): Either[LatentError, ExplicitLatentResponse] =
    encode(data, spec).map(_.response)

  def toArchive(
      data: DoubleMatrix,
      space: NeuroSpace,
      spec: LatentEncodingSpec,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    spec match
      case LatentEncodingSpec.ProvidedTemporalBasis(_, _, _, _, _, _, _) =>
        encodeResponse(data, spec)
          .left
          .map(archiveError)
          .flatMap { response =>
            LatentArchiveCodec.toArchive(
              response = response,
              space = space,
              runLabel = runLabel,
              creator = creator
            )
          }

      case LatentEncodingSpec.TemporalDct(dctSpec, center, ridge, sourceDomain, targetDomain, label, metadata) =>
        LatentArchiveCodec.toTemporalDctArchiveSpec(
          data = data,
          space = space,
          spec = dctSpec,
          center = center,
          ridge = ridge,
          runLabel = runLabel,
          creator = creator,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = label,
          metadata = metadata
        )

      case LatentEncodingSpec.SharedSpatialBasis(basis, basisId, locator, center, ridge, sourceDomain, targetDomain, label, metadata) =>
        LatentArchiveCodec.toSharedBasisArchive(
          data = data,
          space = space,
          basis = basis,
          basisId = basisId,
          locator = locator,
          center = center,
          ridge = ridge.value,
          runLabel = runLabel,
          creator = creator,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = label,
          metadata = metadata
        )

  private def archiveError(error: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(error.message)
