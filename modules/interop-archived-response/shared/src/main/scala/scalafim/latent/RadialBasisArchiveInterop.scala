package scalafim.latent

import gale.linalg.{DMat, DVec}
import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{
  LnaArchive,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisLocator,
  SharedBasisMask
}
import scalafim.image.SomeSampleSpace

extension (radialBasis: RadialBasis)
  def sharedBasisParams(
      extra: Map[String, String] = Map.empty
  ): Map[String, String] =
    extra ++ Map(
      "family" -> "hrbf",
      "radial.kernel" -> radialBasis.kernel.metadataValue,
      "radial.threshold" -> radialBasis.threshold.metadataValue,
      "radial.n_atoms" -> radialBasis.nAtoms.toString,
      "radial.n_voxels" -> radialBasis.nVoxels.toString,
      "radial.atom_levels" -> radialBasis.atomLevels.mkString(",")
    )

  def toSharedBasisArtifact(
      maskDims: Vector[Int],
      kind: String = "hrbf",
      params: Map[String, String] = Map.empty,
      created: Option[String] = None
  ): Either[RadialBasisError, SharedBasisArtifact] =
    for
      order <- radialBasis.activeMaskOrder
      maskSize <- checkedMaskSize(maskDims)
      maskValues <- order.maskValues(maskSize)
      mask <- SharedBasisMask
        .checked(maskDims, maskValues)
        .left
        .map(error => RadialBasisError.InvalidMaskDimensions(error.message))
      loadings = canonicalDMat(radialBasis, order)
      artifact <- SharedBasisArtifact
        .checked(
          loadings = loadings,
          mask = mask,
          kind = kind,
          params = radialBasis.sharedBasisParams(params),
          created = created
        )
        .left
        .map(error =>
          RadialBasisError.InvalidSharedBasisArtifact(error.message)
        )
    yield artifact

private def canonicalDMat(
    radialBasis: RadialBasis,
    order: RadialMaskOrder
): DMat =
  radialBasis.loadings.selectRows(order.maskOrderSelection.ordinals.toVector)

private def checkedMaskSize(
    maskDims: Vector[Int]
): Either[RadialBasisError, Int] =
  if maskDims.isEmpty then
    Left(RadialBasisError.InvalidMaskDimensions(
      "mask dimensions must be non-empty"
    ))
  else if maskDims.exists(_ <= 0) then
    Left(RadialBasisError.InvalidMaskDimensions(
      "mask dimensions must be positive"
    ))
  else Right(maskDims.product)

final case class RadialBasisEncoding(
    encoding: SharedBasisEncoding,
    radialResponse: ExplicitLatentResponse,
    radialBasis: RadialBasis,
    artifact: SharedBasisArtifact,
    basisId: SharedBasisId,
    locator: Option[SharedBasisLocator]
):
  def response: ExplicitLatentResponse =
    radialResponse

  def coefficients: DMat =
    encoding.coefficients

  def offset: Option[DVec] =
    radialResponse.offset

  def decode(
      selection: RadialDecodeSelection = RadialDecodeSelection.All
  ): Either[LatentError, DMat] =
    radialBasis.decode(coefficients, selection, offset)

object RadialBasisEncoder:
  def encode(
      data: DMat,
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      sourceDomain: DomainId =
        DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[LatentError, RadialBasisEncoding] =
    radialBasis
      .toSharedBasisArtifact(
        maskDims = maskDims,
        params = artifactParams
      )
      .left
      .map(radialError)
      .flatMap: artifact =>
        for
          canonicalData <- radialBasis.dataInMaskOrder(data)
          sharedEncoding <-
            SharedBasisEncoder.encode(
              data = canonicalData,
              basis = artifact,
              basisId = basisId,
              locator = locator,
              center = center,
              ridge = ridge,
              sourceDomain = sourceDomain,
              targetDomain = targetDomain,
              label = label,
              metadata = radialBasis.sharedBasisParams(metadata)
            )
          activeOffset <- sharedEncoding.offset match
            case Some(values) =>
              radialBasis
                .vectorInActiveOrderFromMaskOrder(values)
                .map(Some(_))
            case None =>
              Right(None)
          radialResponse <- ExplicitLatentResponse(
            basis = sharedEncoding.coefficients,
            loadings = radialBasis.loadings,
            offset = activeOffset,
            sourceDomain = sourceDomain,
            targetDomain = targetDomain,
            label = sharedEncoding.response.label,
            metadata = sharedEncoding.response.metadata
          )
        yield
          RadialBasisEncoding(
            encoding = sharedEncoding,
            radialResponse = radialResponse,
            radialBasis = radialBasis,
            artifact = artifact,
            basisId = basisId,
            locator = locator
          )

  private def radialError(
      error: RadialBasisError
  ): LatentError =
    LatentError.ProjectionFailed(error.message)

object RadialBasisArchiveCodec:
  def toArchive(
      data: DMat,
      space: SomeSampleSpace,
      radialBasis: RadialBasis,
      maskDims: Vector[Int],
      basisId: SharedBasisId,
      locator: Option[SharedBasisLocator] = None,
      center: Boolean = true,
      ridge: Double = 0.0,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent",
      sourceDomain: DomainId =
        DomainId.unsafe("radial_basis.coefficients"),
      targetDomain: DomainId = DomainId.unsafe("voxels"),
      label: String = "",
      metadata: Map[String, String] = Map.empty,
      artifactParams: Map[String, String] = Map.empty
  ): Either[ArchiveError, LnaArchive] =
    radialBasis
      .toSharedBasisArtifact(
        maskDims = maskDims,
        params = artifactParams
      )
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))
      .flatMap: basis =>
        radialBasis
          .dataInMaskOrder(data)
          .left
          .map(error => ArchiveError.InvalidArchive(error.message))
          .flatMap: canonicalData =>
            SharedBasisLatentArchiveCodec.toArchive(
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
