package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, SharedBasisArtifact, SharedBasisId, SharedBasisLocator}
import scalafim.image.SomeSampleSpace
import gale.linalg.DMat

enum LatentEncodingSpec:
  case ProvidedTemporalBasis(
      basis: DMat,
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      annotations: LatentAnnotation
  )

  case TemporalDct(
      spec: DctSpec,
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      annotations: LatentAnnotation
  )

  case TemporalHaar(
      spec: HaarSpec,
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      annotations: LatentAnnotation
  )
  case SharedSpatialBasis(
      basis: SharedBasisArtifact,
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator],
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      annotations: LatentAnnotation
  )

  case RadialSpatialBasis(
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator],
      center: Boolean,
      ridge: RidgePenalty,
      sourceDomain: DomainId,
      targetDomain: DomainId,
      annotations: LatentAnnotation,
      artifactParams: Map[String, String]
  )

  def latentLabel: LatentLabel =
    annotation.label

  def typedMetadata: LatentMetadata =
    annotation.metadata

  def label: String =
    annotation.labelValue

  def metadata: Map[String, String] =
    annotation.metadataValues

  private def annotation: LatentAnnotation =
    this match
      case ProvidedTemporalBasis(_, _, _, _, _, value)    => value
      case TemporalDct(_, _, _, _, _, value)              => value
      case TemporalHaar(_, _, _, _, _, value)             => value
      case SharedSpatialBasis(_, _, _, _, _, _, _, value) => value
      case RadialSpatialBasis(_, _, _, _, _, _, _, _, value, _) => value

object LatentEncodingSpec:
  def providedBasis(
      basis: DMat,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    for
      penalty <- RidgePenalty(ridge)
      annotation <- LatentAnnotation(label, metadata)
    yield
      LatentEncodingSpec.ProvidedTemporalBasis(
        basis = basis,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotations = annotation
      )

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
      encoding <- dctSpec(
        spec = spec,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    yield encoding

  def dctSpec(
      spec: DctSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    LatentAnnotation(label, metadata).map { annotation =>
      LatentEncodingSpec.TemporalDct(
        spec = spec,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotations = annotation
      )
    }

  def haar(
      timepoints: Int,
      components: Int,
      center: Boolean = false,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    for
      penalty <- RidgePenalty(ridge)
      spec <- HaarSpec(timepoints, components)
      encoding <- haarSpec(
        spec = spec,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    yield encoding

  def haarSpec(
      spec: HaarSpec,
      center: Boolean = false,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("latent.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("latent.samples"),
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    LatentAnnotation(label, metadata).map { annotation =>
      LatentEncodingSpec.TemporalHaar(
        spec = spec,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotations = annotation
      )
    }

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
    for
      penalty <- RidgePenalty(ridge)
      encoding <- sharedBasisSpec(
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
    yield encoding

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
  ): Either[LatentError, LatentEncodingSpec] =
    LatentAnnotation(label, metadata).map { annotation =>
      LatentEncodingSpec.SharedSpatialBasis(
        basis = basis,
        basisId = basisId,
        locator = locator,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotations = annotation
      )
    }

  def radialBasis(
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      sourceDomain: DomainId = DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    for
      penalty <- RidgePenalty(ridge)
      encoding <- radialBasisSpec(
        radialBasis = radialBasis,
        maskDims = maskDims,
        basisId = basisId,
        locator = locator,
        center = center,
        ridge = penalty,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata,
        artifactParams = artifactParams
      )
    yield encoding

  def radialBasisSpec(
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: RidgePenalty = RidgePenalty.Zero,
      sourceDomain: DomainId = DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[LatentError, LatentEncodingSpec] =
    for
      _ <- radialBasis.toSharedBasisArtifact(maskDims, params = artifactParams).left.map(radialError)
      annotation <- LatentAnnotation(label, metadata)
    yield
      LatentEncodingSpec.RadialSpatialBasis(
        radialBasis = radialBasis,
        maskDims = maskDims,
        basisId = basisId,
        locator = locator,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        annotations = annotation,
        artifactParams = artifactParams
      )

  private def radialError(error: RadialBasisError): LatentError =
    LatentError.ProjectionFailed(error.message)

enum LatentEncodingResult:
  case Explicit(value: ExplicitLatentResponse)
  case TemporalDct(value: ExplicitLatentResponse, spec: DctSpec, center: Boolean, ridge: RidgePenalty)
  case TemporalHaar(value: ExplicitLatentResponse, spec: HaarSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(encoding: SharedBasisEncoding)
  case RadialBasis(encoding: RadialBasisEncoding)

  def response: ExplicitLatentResponse =
    this match
      case Explicit(value)              => value
      case TemporalDct(value, _, _, _)  => value
      case TemporalHaar(value, _, _, _) => value
      case SharedBasis(value)           => value.response
      case RadialBasis(value)           => value.response

  def latentResponse: LatentResponse =
    response

object LatentEncoder:
  def encode(
      data: DMat,
      spec: LatentEncodingSpec
  ): Either[LatentError, LatentEncodingResult] =
    spec match
      case LatentEncodingSpec.ProvidedTemporalBasis(basis, center, ridge, sourceDomain, targetDomain, annotations) =>
        TemporalBasisEncoder
          .encodeProvided(
            data = data,
            basis = basis,
            center = center,
            ridge = ridge.value,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = annotations.labelValue,
            metadata = annotations.metadataValues
          )
          .map(LatentEncodingResult.Explicit(_))

      case LatentEncodingSpec.TemporalDct(dctSpec, center, ridge, sourceDomain, targetDomain, annotations) =>
        TemporalBasisEncoder
          .encodeDctSpec(
            data = data,
            spec = dctSpec,
            center = center,
            ridge = ridge,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = annotations.labelValue,
            metadata = annotations.metadataValues
          )
          .map(response => LatentEncodingResult.TemporalDct(response, dctSpec, center, ridge))

      case LatentEncodingSpec.TemporalHaar(haarSpec, center, ridge, sourceDomain, targetDomain, annotations) =>
        TemporalBasisEncoder
          .encodeHaarSpec(
            data = data,
            spec = haarSpec,
            center = center,
            ridge = ridge,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = annotations.labelValue,
            metadata = annotations.metadataValues
          )
          .map(response => LatentEncodingResult.TemporalHaar(response, haarSpec, center, ridge))

      case LatentEncodingSpec.SharedSpatialBasis(basis, basisId, locator, center, ridge, sourceDomain, targetDomain, annotations) =>
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
            label = annotations.labelValue,
            metadata = annotations.metadataValues
          )
          .map(LatentEncodingResult.SharedBasis(_))

      case LatentEncodingSpec.RadialSpatialBasis(radialBasis, maskDims, basisId, locator, center, ridge, sourceDomain, targetDomain, annotations, artifactParams) =>
        RadialBasisEncoder
          .encode(
            data = data,
            radialBasis = radialBasis,
            maskDims = maskDims,
            basisId = basisId,
            locator = locator,
            center = center,
            ridge = ridge.value,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = annotations.labelValue,
            metadata = annotations.metadataValues,
            artifactParams = artifactParams
          )
          .map(LatentEncodingResult.RadialBasis(_))

  def encodeResponse(
      data: DMat,
      spec: LatentEncodingSpec
  ): Either[LatentError, ExplicitLatentResponse] =
    encode(data, spec).map(_.response)

  def toArchive(
      data: DMat,
      space: SomeSampleSpace,
      spec: LatentEncodingSpec,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    spec match
      case LatentEncodingSpec.ProvidedTemporalBasis(_, _, _, _, _, _) =>
        encodeResponse(data, spec)
          .left
          .map(archiveError)
          .flatMap { response =>
            ExplicitLatentArchiveCodec.toArchive(
              response = response,
              space = space,
              runLabel = runLabel,
              creator = creator
            )
          }

      case LatentEncodingSpec.TemporalDct(dctSpec, center, ridge, sourceDomain, targetDomain, annotations) =>
        ExplicitLatentArchiveCodec.toTemporalDctArchiveSpec(
          data = data,
          space = space,
          spec = dctSpec,
          center = center,
          ridge = ridge,
          runLabel = runLabel,
          creator = creator,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = annotations.labelValue,
          metadata = annotations.metadataValues
        )

      case LatentEncodingSpec.TemporalHaar(_, _, _, _, _, _) =>
        encodeResponse(data, spec)
          .left
          .map(archiveError)
          .flatMap { response =>
            ExplicitLatentArchiveCodec.toArchive(
              response = response,
              space = space,
              runLabel = runLabel,
              creator = creator
            )
          }

      case LatentEncodingSpec.SharedSpatialBasis(basis, basisId, locator, center, ridge, sourceDomain, targetDomain, annotations) =>
        SharedBasisLatentArchiveCodec.toArchive(
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
          label = annotations.labelValue,
          metadata = annotations.metadataValues
        )

      case LatentEncodingSpec.RadialSpatialBasis(radialBasis, maskDims, basisId, locator, center, ridge, sourceDomain, targetDomain, annotations, artifactParams) =>
        RadialBasisArchiveCodec.toArchive(
          data = data,
          space = space,
          radialBasis = radialBasis,
          maskDims = maskDims,
          basisId = basisId,
          locator = locator,
          center = center,
          ridge = ridge.value,
          runLabel = runLabel,
          creator = creator,
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = annotations.labelValue,
          metadata = annotations.metadataValues,
          artifactParams = artifactParams
        )

  private def archiveError(error: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(error.message)
