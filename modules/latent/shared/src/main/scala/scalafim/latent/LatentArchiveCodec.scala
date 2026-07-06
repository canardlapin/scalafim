package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{
  LnaArchive,
  LnaExplicitLatent,
  LnaPipeline,
  LnaTemporalDct,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisLocator,
  TemporalDctNorm,
  TemporalDctParams
}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

object LatentArchiveCodec:
  def toArchive(
      response: ExplicitLatentResponse,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    LnaExplicitLatent.archive(
      response = LnaExplicitLatent.Response(
        basis = toDMat(response.basis),
        loadings = toDMat(response.loadings),
        offset = response.offset.map(_.toVector),
        sourceDomain = response.sourceDomain.value,
        targetDomain = response.targetDomain.value,
        label = response.label,
        metadata = response.metadata
      ),
      space = space,
      runLabel = runLabel,
      creator = creator
    )

  def toTemporalDctArchive(
      data: DoubleMatrix,
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
    TemporalBasisEncoder
      .encodeDct(
        data = data,
        components = components,
        norm = norm,
        center = center,
        ridge = ridge,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
      .left
      .map(error)
      .flatMap { response =>
        LnaTemporalDct.archive(
          response = LnaExplicitLatent.Response(
            basis = toDMat(response.basis),
            loadings = toDMat(response.loadings),
            offset = response.offset.map(_.toVector),
            sourceDomain = response.sourceDomain.value,
            targetDomain = response.targetDomain.value,
            label = response.label,
            metadata = response.metadata
          ),
          params = TemporalDctParams(
            components = components,
            norm = archiveNorm(norm),
            center = center,
            ridge = ridge
          ),
          space = space,
          runLabel = runLabel,
          creator = creator
        )
      }

  def toSharedBasisArchive(
      data: DoubleMatrix,
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
    SharedBasisEncoder
      .encode(
        data = data,
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
      .left
      .map(error)
      .flatMap { encoding =>
        LnaPipeline.sharedBasisEmbedArchiveFromCoefficients(
          coefficients = toDMat(encoding.coefficients),
          space = space,
          basis = encoding.basis,
          basisId = basisId,
          locator = locator,
          offset = encoding.offset.map(_.toVector),
          runLabel = runLabel,
          creator = creator
        )
      }

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, ExplicitLatentResponse] =
    LnaExplicitLatent.read(archive, runLabel).flatMap { response =>
      for
        sourceDomain <- DomainId(response.sourceDomain).left.map(error)
        targetDomain <- DomainId(response.targetDomain).left.map(error)
        latent <- ExplicitLatentResponse(
          basis = toDoubleMatrix(response.basis),
          loadings = toDoubleMatrix(response.loadings),
          offset = response.offset.map(DoubleVector.fromSeq),
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = response.label,
          metadata = response.metadata
        ).left.map(error)
      yield latent
    }

  private def toDMat(matrix: DoubleMatrix): DMat =
    DMat.fromRows(matrix.toRows)

  private def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

  private def archiveNorm(norm: DctNorm): TemporalDctNorm =
    norm match
      case DctNorm.Ortho => TemporalDctNorm.Ortho
      case DctNorm.None  => TemporalDctNorm.None

  private def error(latentError: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(latentError.message)
