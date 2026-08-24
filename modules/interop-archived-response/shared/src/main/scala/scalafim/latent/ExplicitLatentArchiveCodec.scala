package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel, RunScopedPath}
import scalafim.archive.lna.{
  LnaArchive,
  LnaExplicitLatent,
  LnaTemporalDct,
  TemporalDctParams,
  TransformDescriptor,
  TransformKind,
  TransformParams
}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}
import scalafim.latent.LatentArchivePayloads.*

object ExplicitLatentArchiveCodec:
  def toArchive(
      response: ExplicitLatentResponse,
      space: SomeSampleSpace,
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
      data: DMat,
      space: SomeSampleSpace,
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
    for
      penalty <- RidgePenalty(ridge).left.map(error)
      spec <- DctSpec(data.rows, components, norm).left.map(error)
      archive <- toTemporalDctArchiveSpec(
        data = data,
        space = space,
        spec = spec,
        center = center,
        ridge = penalty,
        runLabel = runLabel,
        creator = creator,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = label,
        metadata = metadata
      )
    yield archive

  def toTemporalDctArchiveSpec(
      data: DMat,
      space: SomeSampleSpace,
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
    TemporalBasisEncoder
      .encodeDctSpec(
        data = data,
        spec = spec,
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
            components = spec.components,
            norm = archiveNorm(spec.norm),
            center = center,
            ridge = ridge.value
          ),
          space = space,
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
          offset = response.offset.map(DVec.fromSeq),
          sourceDomain = sourceDomain,
          targetDomain = targetDomain,
          label = response.label,
          metadata = response.metadata
        ).left.map(error)
      yield latent
    }

  def temporalDctDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms.find { desc =>
      desc.kind == TransformKind.Temporal &&
        desc.datasets.exists(_.path.value.startsWith(prefix)) &&
        (desc.params match
          case TransformParams.TemporalDct(_) => true
          case _                              => false)
    }

  def temporalDctSpec(
      archive: LnaArchive,
      runLabel: RunLabel,
      desc: TransformDescriptor
  ): Either[ArchiveError, (DctSpec, Boolean, RidgePenalty)] =
    for
      run <- archive.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
      params <- desc.params match
        case TransformParams.TemporalDct(value) => Right(value)
        case _                                  => Left(ArchiveError.InvalidArchive("temporal DCT descriptor missing typed params"))
      ridge <- RidgePenalty(params.ridge).left.map(error)
      spec <- DctSpec(run.shape.timepoints, params.components, latentNorm(params.norm)).left.map(error)
    yield (spec, params.center, ridge)

  def explicitDescriptorAlgebra(
      desc: TransformDescriptor
  ): Either[ArchiveError, LatentArchiveDescriptor] =
    desc.params match
      case p: TransformParams.Embed if isHaarExplicitMetadata(p.metadata) =>
        for
          spec <- haarSpecFromMetadata(p.metadata)
          center <- booleanMetadata(p.metadata, "center", default = false)
          ridgeValue <- doubleMetadata(p.metadata, "ridge", default = 0.0)
          ridge <- RidgePenalty(ridgeValue).left.map(error)
        yield LatentArchiveDescriptor.TemporalHaar(desc, spec, center, ridge)
      case _ =>
        Right(LatentArchiveDescriptor.Explicit(desc))

  def explicitDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val runOutput = archive.run(runLabel).map(_.output)
    archive.manifest.transforms.find { desc =>
      LnaExplicitLatent.isExplicitEmbed(desc) &&
        desc.datasets.exists(ref => runOutput.contains(ref.path) || RunScopedPath.from(ref.path, runLabel).isDefined)
    }

  private def isHaarExplicitMetadata(metadata: Map[String, String]): Boolean =
    metadata.get("family").contains("time_haar") || metadata.get("basis").contains("haar")

  private def haarSpecFromMetadata(metadata: Map[String, String]): Either[ArchiveError, HaarSpec] =
    for
      timepoints <- requiredIntMetadata(metadata, "timepoints", "Haar timepoints")
      components <- requiredIntMetadata(metadata, "components", "Haar components")
      spec <- HaarSpec(timepoints, components).left.map(error)
    yield spec

  private def requiredIntMetadata(
      metadata: Map[String, String],
      key: String,
      label: String
  ): Either[ArchiveError, Int] =
    metadata.get(key).flatMap(_.toIntOption) match
      case Some(value) => Right(value)
      case None        => Left(ArchiveError.InvalidArchive(s"$label metadata '$key' is missing or not an integer"))

  private def booleanMetadata(
      metadata: Map[String, String],
      key: String,
      default: Boolean
  ): Either[ArchiveError, Boolean] =
    metadata.get(key) match
      case None          => Right(default)
      case Some("true")  => Right(true)
      case Some("false") => Right(false)
      case Some(value)   => Left(ArchiveError.InvalidArchive(s"metadata '$key' must be true or false, got '$value'"))

  private def doubleMetadata(
      metadata: Map[String, String],
      key: String,
      default: Double
  ): Either[ArchiveError, Double] =
    metadata.get(key).flatMap(_.toDoubleOption) match
      case Some(value) => Right(value)
      case None if metadata.contains(key) =>
        Left(ArchiveError.InvalidArchive(s"metadata '$key' is not a valid double"))
      case None =>
        Right(default)
