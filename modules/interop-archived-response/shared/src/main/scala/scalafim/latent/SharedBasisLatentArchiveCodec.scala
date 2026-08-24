package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{
  DatasetRole,
  LnaArchive,
  LnaPipeline,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisLocator,
  SharedBasisRef,
  TransformDescriptor,
  TransformKind,
  TransformParams
}
import scalafim.image.{DMat as ArchiveDMat, Mask, PrimitiveBuffers, SomeSampleSpace}
import scalafim.image.spatialDims
import gale.linalg.{DMat, DVec}
import scalafim.latent.LatentArchivePayloads.*

final class SharedBasisLatentArchive private (
    val coefficients: DMat,
    val basis: SharedBasisRef,
    val offset: Option[DVec],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val latentLabel: LatentLabel,
    val typedMetadata: LatentMetadata
):
  def label: String =
    latentLabel.value

  def metadata: Map[String, String] =
    typedMetadata.values

  def timepoints: Int =
    coefficients.rows

  def coefficientCount: Int =
    coefficients.cols

  def materialize(
      artifact: SharedBasisArtifact,
      space: Option[SomeSampleSpace] = None
  ): Either[LatentError, ExplicitLatentResponse] =
    SharedBasisLatentArchive.materialize(this, artifact, space)

  def sampleMask(
      space: SomeSampleSpace,
      artifact: SharedBasisArtifact
  ): Either[LatentError, Mask.MaskVol] =
    SharedBasisLatentArchive.sampleMask(space, artifact)

object SharedBasisLatentArchive:
  def apply(
      coefficients: DMat,
      basis: SharedBasisRef,
      offset: Option[DVec],
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  ): Either[LatentError, SharedBasisLatentArchive] =
    if coefficients.rows <= 0 then Left(LatentError.NonPositiveDimension("shared-basis coefficient rows", coefficients.rows))
    else if coefficients.cols <= 0 then Left(LatentError.NonPositiveDimension("shared-basis coefficient columns", coefficients.cols))
    else
      for
        annotation <- LatentAnnotation(label, metadata)
        _ <-
          firstNonFinite("shared-basis coefficients", coefficients)
            .orElse(offset.flatMap(value => firstNonFinite("shared-basis offset", value))) match
            case Some(error) => Left(error)
            case None        => Right(())
      yield new SharedBasisLatentArchive(coefficients, basis, offset, sourceDomain, targetDomain, annotation.label, annotation.metadata)

  def materialize(
      archive: SharedBasisLatentArchive,
      artifact: SharedBasisArtifact,
      space: Option[SomeSampleSpace] = None
  ): Either[LatentError, ExplicitLatentResponse] =
    for
      _ <- validateArtifact(archive, artifact, space)
      response <- ExplicitLatentResponse(
        basis = archive.coefficients,
        loadings = toDoubleMatrix(artifact.loadings),
        offset = archive.offset,
        sourceDomain = archive.sourceDomain,
        targetDomain = archive.targetDomain,
        label = archive.label,
        metadata = materializedMetadata(archive, artifact)
      )
    yield response

  def sampleMask(
      space: SomeSampleSpace,
      artifact: SharedBasisArtifact
  ): Either[LatentError, Mask.MaskVol] =
    if artifact.mask.values.length != space.spatialDims.product then
      Left(LatentError.DimensionMismatch("shared basis mask size", space.spatialDims.product, artifact.mask.values.length))
    else
      val indices = Array.newBuilder[Int]
      var i = 0
      while i < artifact.mask.values.length do
        if artifact.mask.values(i) then indices += i
        i += 1
      Right(Mask.fromIndices(space, PrimitiveBuffers.fromArray(indices.result()), label = s"shared-basis:${artifact.kind}"))

  private def validateArtifact(
      archive: SharedBasisLatentArchive,
      artifact: SharedBasisArtifact,
      space: Option[SomeSampleSpace]
  ): Either[LatentError, Unit] =
    if archive.coefficients.cols != artifact.nAtoms then
      Left(LatentError.DimensionMismatch("shared basis atoms", artifact.nAtoms, archive.coefficients.cols))
    else if artifact.mask.activeCount != artifact.nVoxels then
      Left(LatentError.DimensionMismatch("shared basis active mask count", artifact.nVoxels, artifact.mask.activeCount))
    else
      archive.offset match
        case Some(values) if values.length != artifact.nVoxels =>
          Left(LatentError.DimensionMismatch("shared basis offset length", artifact.nVoxels, values.length))
        case _ =>
          space match
            case Some(value) if artifact.mask.values.length != value.spatialDims.product =>
              Left(LatentError.DimensionMismatch("shared basis mask size", value.spatialDims.product, artifact.mask.values.length))
            case _ =>
              firstNonFinite("shared-basis loadings", toDoubleMatrix(artifact.loadings)) match
                case Some(error) => Left(error)
                case None        => Right(())

  private def materializedMetadata(
      archive: SharedBasisLatentArchive,
      artifact: SharedBasisArtifact
  ): Map[String, String] =
    archive.metadata ++ Map(
      "family" -> "shared_basis",
      "basis.id" -> archive.basis.basisId.value,
      "basis.checksum" -> archive.basis.checksum.value,
      "basis.kind" -> artifact.kind,
      "basis.n_atoms" -> artifact.nAtoms.toString,
      "basis.n_voxels" -> artifact.nVoxels.toString,
      "basis.mask_size" -> artifact.mask.values.length.toString,
      "basis.mask_active" -> artifact.mask.activeCount.toString
    ) ++ archive.basis.locator.map(locator => "basis.locator" -> locator.value).toMap

  private def toDoubleMatrix(matrix: ArchiveDMat): DMat =
    LatentNumerics.matrixFromRows(matrix.toRows)

  private def firstNonFinite(label: String, matrix: DMat): Option[LatentError] =
    val data = matrix.copyData
    var i = 0
    var error = Option.empty[LatentError]
    while i < data.length && error.isEmpty do
      val value = data(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

  private def firstNonFinite(label: String, vector: DVec): Option[LatentError] =
    var i = 0
    var error = Option.empty[LatentError]
    while i < vector.length && error.isEmpty do
      val value = vector(i)
      if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
      i += 1
    error

object SharedBasisLatentArchiveCodec:
  def toArchive(
      data: DMat,
      space: SomeSampleSpace,
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
          creator = creator,
          sourceDomain = Some(targetDomain.value),
          targetDomain = Some(sourceDomain.value),
          label = Option.when(encoding.response.label.nonEmpty)(encoding.response.label),
          metadata = encoding.response.metadata
        )
      }

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, SharedBasisLatentArchive] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- descriptorOption(valid, runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no shared-basis latent descriptor"))
        params <- desc.params match
          case p: TransformParams.SharedBasisEmbed => Right(p)
          case _                                   => Left(ArchiveError.InvalidArchive("shared-basis descriptor missing typed embed params"))
        coefficientsPath <- byRole(desc, DatasetRole.Coefficients)
        coefficients <- doubleMatrixPayload(valid, coefficientsPath, "shared-basis coefficients")
        offset <- optionalOffset(valid, desc, DatasetRole.Offset, "shared-basis offset")
        source <- DomainId(params.targetDomain.getOrElse("shared_basis.coefficients")).left.map(error)
        target <- DomainId(params.sourceDomain.getOrElse("voxels")).left.map(error)
        response <- SharedBasisLatentArchive(
          coefficients = toDoubleMatrix(coefficients),
          basis = params.basis,
          offset = offset.map(DVec.fromSeq),
          sourceDomain = source,
          targetDomain = target,
          label = params.label.getOrElse(""),
          metadata = params.metadata
        ).left.map(error)
        _ <-
          if response.timepoints == run.shape.timepoints then Right(())
          else Left(ArchiveError.ShapeMismatch(s"shared-basis coefficients have ${response.timepoints} rows but run has ${run.shape.timepoints} timepoints"))
      yield response
    }

  def descriptorOption(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms.find { desc =>
      desc.kind == TransformKind.Embed &&
        desc.datasets.exists(_.path.value.startsWith(prefix)) &&
        (desc.params match
          case _: TransformParams.SharedBasisEmbed => true
          case _                                   => false)
    }
