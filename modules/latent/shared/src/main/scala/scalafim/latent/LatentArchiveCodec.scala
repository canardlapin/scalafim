package scalafim.latent

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{
  DatasetRef,
  DatasetRole,
  LnaDType,
  LnaArchive,
  LnaManifest,
  LnaExplicitLatent,
  LnaPipeline,
  LnaRun,
  LnaShape,
  LnaTemporalDct,
  Payload,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisLocator,
  SharedBasisRef,
  TransformDescriptor,
  TransformKind,
  TransformParams,
  TemporalDctNorm,
  TemporalDctParams
}
import scalafim.archive.ArchivePath
import scalafim.image.{DMat, Mask, NArrayUtil, NeuroSpace}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, DoubleVector, LinearMap}

enum LatentArchiveResponse:
  case Explicit(response: ExplicitLatentResponse)
  case TemporalDct(response: ExplicitLatentResponse, spec: DctSpec, center: Boolean, ridge: RidgePenalty)
  case SharedBasis(response: SharedBasisLatentArchive)
  case Transport(response: TransportLatentResponse)
  case BoldZip(response: BoldZipPayload)

  def latentResponse: Option[LatentResponse] =
    this match
      case Explicit(response)              => Some(response)
      case TemporalDct(response, _, _, _) => Some(response)
      case SharedBasis(_)                 => None
      case Transport(response)             => Some(response)
      case BoldZip(response)               => Some(response)

final class SharedBasisLatentArchive private (
    val coefficients: DoubleMatrix,
    val basis: SharedBasisRef,
    val offset: Option[DoubleVector],
    val sourceDomain: DomainId,
    val targetDomain: DomainId,
    val label: String,
    val metadata: Map[String, String]
):
  def timepoints: Int =
    coefficients.rows

  def coefficientCount: Int =
    coefficients.cols

  def materialize(
      artifact: SharedBasisArtifact,
      space: Option[NeuroSpace] = None
  ): Either[LatentError, ExplicitLatentResponse] =
    SharedBasisLatentArchive.materialize(this, artifact, space)

  def sampleMask(
      space: NeuroSpace,
      artifact: SharedBasisArtifact
  ): Either[LatentError, Mask.MaskVol] =
    SharedBasisLatentArchive.sampleMask(space, artifact)

object SharedBasisLatentArchive:
  def apply(
      coefficients: DoubleMatrix,
      basis: SharedBasisRef,
      offset: Option[DoubleVector],
      sourceDomain: DomainId,
      targetDomain: DomainId,
      label: String,
      metadata: Map[String, String]
  ): Either[LatentError, SharedBasisLatentArchive] =
    if coefficients.rows <= 0 then Left(LatentError.NonPositiveDimension("shared-basis coefficient rows", coefficients.rows))
    else if coefficients.cols <= 0 then Left(LatentError.NonPositiveDimension("shared-basis coefficient columns", coefficients.cols))
    else
      archiveFirstNonFinite("shared-basis coefficients", coefficients)
        .orElse(offset.flatMap(value => archiveFirstNonFinite("shared-basis offset", value))) match
        case Some(error) =>
          Left(error)
        case None =>
          Right(new SharedBasisLatentArchive(coefficients, basis, offset, sourceDomain, targetDomain, label, metadata))

  def materialize(
      archive: SharedBasisLatentArchive,
      artifact: SharedBasisArtifact,
      space: Option[NeuroSpace] = None
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
      space: NeuroSpace,
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
      Right(Mask.fromIndices(space, NArrayUtil.fromArray(indices.result()), label = s"shared-basis:${artifact.kind}"))

  private def validateArtifact(
      archive: SharedBasisLatentArchive,
      artifact: SharedBasisArtifact,
      space: Option[NeuroSpace]
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
              archiveFirstNonFinite("shared-basis loadings", toDoubleMatrix(artifact.loadings)) match
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

  private def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

object LatentArchiveCodec:
  private val TransportKindKey = "lna.response.kind"
  private val TransportKindValue = "transport_latent"
  private val NativeDecoderRole = DatasetRole.Other("transport_native_decoder_t")
  private val TemplateDecoderRole = DatasetRole.Other("transport_template_decoder_t")
  private val ToAnalysisRole = DatasetRole.Other("transport_to_analysis")
  private val ToRawRole = DatasetRole.Other("transport_to_raw")
  private val BoldZipKindKey = "lna.response.kind"
  private val BoldZipKindValue = "boldzip_sr"
  private val BoldZipKind = TransformKind.Custom(BoldZipKindValue)
  private val BoldZipCarrierThetaRole = DatasetRole.Other("boldzip_carrier_theta")
  private val BoldZipCarrierLoadingsRole = DatasetRole.Other("boldzip_carrier_loadings")
  private val BoldZipCoarseBasisRole = DatasetRole.Other("boldzip_phi_coarse")
  private val BoldZipDetailBasisRole = DatasetRole.Other("boldzip_phi_detail")
  private val BoldZipTextureIndexRole = DatasetRole.Other("boldzip_texture_index")
  private val BoldZipTextureAmplitudeRole = DatasetRole.Other("boldzip_texture_amplitude")
  private val BoldZipEventIndexRole = DatasetRole.Other("boldzip_residual_event_index")
  private val BoldZipEventAmplitudeRole = DatasetRole.Other("boldzip_residual_event_amplitude")
  private val BoldZipSpatialBasisLabelKey = "spatial_basis.label"

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
      data: DoubleMatrix,
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

  def toBoldZipArchive(
      response: BoldZipPayload,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    if response.shape.samples != space.spatialDims.product then
      Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.samples} samples but space has ${space.spatialDims.product} voxels"))
    else
      val base = ArchivePath(s"/scans/${runLabel.value}/step_00_boldzip_sr")
      val temporalPath = base / "temporal_basis"
      val thetaPath = base / "carrier_theta"
      val loadingsPath = base / "carrier_loadings"
      val coarsePath = base / "phi_coarse"
      val detailPath = base / "phi_detail"
      val textureIndexPath = base / "texture_index"
      val textureAmplitudePath = base / "texture_amplitude"
      val eventIndexPath = base / "residual_event_index"
      val eventAmplitudePath = base / "residual_event_amplitude"
      val offsetPath = base / "offset"

      val temporalRef = DatasetRef(
        temporalPath,
        DatasetRole.TemporalBasis,
        Vector(response.temporalBasis.rows, response.temporalBasis.cols),
        Some(LnaDType.Float64)
      )
      val thetaRef = DatasetRef(
        thetaPath,
        BoldZipCarrierThetaRole,
        Vector(response.carrierTheta.rows, response.carrierTheta.cols),
        Some(LnaDType.Float64)
      )
      val loadingsRef =
        Option.when(response.spatialBasis.coarseAtoms > 0)(
          DatasetRef(
            loadingsPath,
            BoldZipCarrierLoadingsRole,
            Vector(response.carrierLoadings.rows, response.carrierLoadings.cols),
            Some(LnaDType.Float64)
          )
        )
      val coarseRef =
        response.spatialBasis.phiCoarse.map { matrix =>
          DatasetRef(coarsePath, BoldZipCoarseBasisRole, Vector(matrix.rows, matrix.cols), Some(LnaDType.Float64))
        }
      val detailRef =
        response.spatialBasis.phiDetail.map { matrix =>
          DatasetRef(detailPath, BoldZipDetailBasisRole, Vector(matrix.rows, matrix.cols), Some(LnaDType.Float64))
        }
      val textureRefs =
        Option.when(response.texture.nonEmpty)(
          Vector(
            DatasetRef(textureIndexPath, BoldZipTextureIndexRole, Vector(response.texture.length, 3), Some(LnaDType.Int32)),
            DatasetRef(textureAmplitudePath, BoldZipTextureAmplitudeRole, Vector(response.texture.length), Some(LnaDType.Float64))
          )
        ).getOrElse(Vector.empty)
      val eventRefs =
        Option.when(response.events.nonEmpty)(
          Vector(
            DatasetRef(eventIndexPath, BoldZipEventIndexRole, Vector(response.events.length, 3), Some(LnaDType.Int32)),
            DatasetRef(eventAmplitudePath, BoldZipEventAmplitudeRole, Vector(response.events.length), Some(LnaDType.Float64))
          )
        ).getOrElse(Vector.empty)
      val offsetRef =
        response.offset.map(values => DatasetRef(offsetPath, DatasetRole.SampleOffset, Vector(values.length), Some(LnaDType.Float64)))

      val refs =
        Vector(temporalRef, thetaRef) ++
          loadingsRef.toVector ++
          coarseRef.toVector ++
          detailRef.toVector ++
          textureRefs ++
          eventRefs ++
          offsetRef.toVector

      val descriptor = TransformDescriptor(
        name = "00_boldzip_sr.json",
        kind = BoldZipKind,
        params = TransformParams.Custom(
          name = BoldZipKindValue,
          sourceDomain = Some(response.sourceDomain.value),
          targetDomain = Some(response.targetDomain.value),
          label = Option.when(response.label.nonEmpty)(response.label),
          metadata = response.metadata ++ Map(
            BoldZipKindKey -> BoldZipKindValue,
            BoldZipSpatialBasisLabelKey -> response.spatialBasis.label
          )
        ),
        inputs = Vector("temporal_basis", "carrier_theta", "spatial_basis"),
        outputs = Vector("latent_response"),
        datasets = refs
      )

      val payloads =
        Map[ArchivePath, Payload](
          temporalPath -> Payload.DoubleMatrix(toDMat(response.temporalBasis)),
          thetaPath -> Payload.DoubleMatrix(toDMat(response.carrierTheta))
        ) ++
          loadingsRef.map(_ => loadingsPath -> Payload.DoubleMatrix(toDMat(response.carrierLoadings))).toMap ++
          response.spatialBasis.phiCoarse.map(matrix => coarsePath -> Payload.DoubleMatrix(toDMat(matrix))).toMap ++
          response.spatialBasis.phiDetail.map(matrix => detailPath -> Payload.DoubleMatrix(toDMat(matrix))).toMap ++
          boldZipTexturePayloads(response.texture, textureIndexPath, textureAmplitudePath) ++
          boldZipEventPayloads(response.events, eventIndexPath, eventAmplitudePath) ++
          response.offset.map(values => offsetPath -> Payload.DoubleVector(values.toVector)).toMap

      Right(
        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(BoldZipKind),
            transforms = Vector(descriptor),
            runs = Vector(LnaRun(runLabel, LnaShape(space, response.shape.timepoints), thetaPath)),
            datasets = refs,
            header = Map(
              "response.kind" -> BoldZipKindValue,
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> response.shape.timepoints.toString,
              "samples" -> response.shape.samples.toString,
              "carriers" -> response.shape.coefficients.toString,
              "temporal.components" -> response.temporalBasis.cols.toString,
              "coarse_basis" -> response.spatialBasis.coarse.metadataValue,
              "detail_basis" -> response.spatialBasis.detail.metadataValue,
              "source_domain" -> response.sourceDomain.value,
              "target_domain" -> response.targetDomain.value
            )
          ),
          payloads = payloads
        )
      )

  def toTransportArchive(
      response: TransportLatentResponse,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-latent"
  ): Either[ArchiveError, LnaArchive] =
    if response.shape.samples != space.spatialDims.product then
      Left(ArchiveError.ShapeMismatch(s"transport response has ${response.shape.samples} samples but space has ${space.spatialDims.product} voxels"))
    else
      for
        nativeDecoder <- linearMapMatrix(response.nativeDecoder)
        templateDecoder <- response.templateDecoder match
          case None => Right(None)
          case Some(decoder) => linearMapMatrix(decoder).map(Some(_))
        toAnalysis <- linearMapMatrix(response.transform.toAnalysis)
        toRaw <- linearMapMatrix(response.transform.toRaw)
      yield
        val base = ArchivePath(s"/scans/${runLabel.value}/step_00_transport_latent")
        val coeffPath = base / "coefficients_analysis"
        val nativeDecoderPath = base / "native_decoder_t"
        val templateDecoderPath = base / "template_decoder_t"
        val toAnalysisPath = base / "to_analysis"
        val toRawPath = base / "to_raw"
        val offsetPath = base / "offset"

        val coefficientRef = DatasetRef(
          coeffPath,
          DatasetRole.Coefficients,
          Vector(response.coefficientsAnalysis.rows, response.coefficientsAnalysis.cols),
          Some(LnaDType.Float64)
        )
        val nativeDecoderRef = DatasetRef(
          nativeDecoderPath,
          NativeDecoderRole,
          Vector(nativeDecoder.cols, nativeDecoder.rows),
          Some(LnaDType.Float64)
        )
        val templateDecoderRef = templateDecoder.map { decoder =>
          DatasetRef(
            templateDecoderPath,
            TemplateDecoderRole,
            Vector(decoder.cols, decoder.rows),
            Some(LnaDType.Float64)
          )
        }
        val toAnalysisRef = DatasetRef(toAnalysisPath, ToAnalysisRole, Vector(toAnalysis.rows, toAnalysis.cols), Some(LnaDType.Float64))
        val toRawRef = DatasetRef(toRawPath, ToRawRole, Vector(toRaw.rows, toRaw.cols), Some(LnaDType.Float64))
        val offsetRef = response.offset.map { values =>
          DatasetRef(offsetPath, DatasetRole.SampleOffset, Vector(values.length), Some(LnaDType.Float64))
        }
        val refs =
          Vector(coefficientRef, nativeDecoderRef, toAnalysisRef, toRawRef) ++
            templateDecoderRef.toVector ++
            offsetRef.toVector

        val descriptor = TransformDescriptor(
          name = "00_transport_latent.json",
          kind = TransformKind.Embed,
          params = TransformParams.Embed(
            basisPath = nativeDecoderPath,
            centerDataWith = offsetRef.map(_.path),
            sourceDomain = Some(response.sourceDomain.value),
            targetDomain = Some(response.targetDomain.value),
            label = Option.when(response.label.nonEmpty)(response.label),
            metadata = response.metadata ++ Map(
              TransportKindKey -> TransportKindValue,
              "family" -> "transport",
              "coordinates" -> "analysis",
              "adjoint_convention" -> transportAdjoint(response.adjointConvention),
              "operator_storage" -> "dense_transpose",
              "has_template_decoder" -> templateDecoder.nonEmpty.toString
            )
          ),
          inputs = Vector("coefficients_analysis", "native_decoder"),
          outputs = Vector("latent_response"),
          datasets = refs
        )

        val payloads =
          Map[ArchivePath, Payload](
            coeffPath -> Payload.DoubleMatrix(toDMat(response.coefficientsAnalysis)),
            nativeDecoderPath -> Payload.DoubleMatrix(toDMat(nativeDecoder.transpose)),
            toAnalysisPath -> Payload.DoubleMatrix(toDMat(toAnalysis)),
            toRawPath -> Payload.DoubleMatrix(toDMat(toRaw))
          ) ++
            templateDecoder.map(decoder => templateDecoderPath -> Payload.DoubleMatrix(toDMat(decoder.transpose))).toMap ++
            response.offset.map(values => offsetPath -> Payload.DoubleVector(values.toVector)).toMap

        LnaArchive(
          manifest = LnaManifest(
            creator = creator,
            requiredTransforms = Vector(TransformKind.Embed),
            transforms = Vector(descriptor),
            runs = Vector(LnaRun(runLabel, LnaShape(space, response.shape.timepoints), coeffPath)),
            datasets = refs,
            header = Map(
              "response.kind" -> TransportKindValue,
              "space.dims" -> space.spatialDims.mkString("x"),
              "timepoints" -> response.shape.timepoints.toString,
              "samples" -> response.shape.samples.toString,
              "coefficients" -> response.shape.coefficients.toString,
              "source_domain" -> response.sourceDomain.value,
              "target_domain" -> response.targetDomain.value
            )
          ),
          payloads = payloads
        )

  def fromTransportArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, TransportLatentResponse] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- transportDescriptor(valid, runLabel)
        params <- desc.params match
          case p: TransformParams.Embed => Right(p)
          case _ => Left(ArchiveError.InvalidArchive("transport latent descriptor missing typed embed params"))
        sourceDomain <- params.sourceDomain.toRight(ArchiveError.InvalidArchive("transport latent descriptor missing source domain"))
        targetDomain <- params.targetDomain.toRight(ArchiveError.InvalidArchive("transport latent descriptor missing target domain"))
        coefficientsPath <- byRole(desc, DatasetRole.Coefficients)
        coefficients <- doubleMatrixPayload(valid, coefficientsPath, "transport coefficients")
        nativeDecoderT <- doubleMatrixPayload(valid, params.basisPath, "transport native decoder")
        templateDecoderT <- optionalDoubleMatrix(valid, desc, TemplateDecoderRole, "transport template decoder")
        toAnalysisPath <- byRole(desc, ToAnalysisRole)
        toAnalysisMatrix <- doubleMatrixPayload(valid, toAnalysisPath, "transport to-analysis")
        toRawPath <- byRole(desc, ToRawRole)
        toRawMatrix <- doubleMatrixPayload(valid, toRawPath, "transport to-raw")
        offset <- optionalOffset(valid, desc)
        _ <-
          if coefficients.rows == run.shape.timepoints then Right(())
          else Left(ArchiveError.ShapeMismatch(s"transport coefficients have ${coefficients.rows} rows but run has ${run.shape.timepoints} timepoints"))
        nativeDecoder <- linearMapFromMatrix(toDoubleMatrix(nativeDecoderT).transpose)
        templateDecoder <- templateDecoderT match
          case None => Right(None)
          case Some(decoderT) => linearMapFromMatrix(toDoubleMatrix(decoderT).transpose).map(Some(_))
        toAnalysis <- linearMapFromMatrix(toAnalysisMatrix)
        toRaw <- linearMapFromMatrix(toRawMatrix)
        transform <- CoefficientTransform(toAnalysis, toRaw).left.map(error)
        source <- DomainId(sourceDomain).left.map(error)
        target <- DomainId(targetDomain).left.map(error)
        adjoint <- transportAdjoint(params.metadata.getOrElse("adjoint_convention", "euclidean_discrete"))
        response <- TransportLatentResponse(
          coefficientsAnalysis = toDoubleMatrix(coefficients),
          nativeDecoder = nativeDecoder,
          transform = transform,
          templateDecoder = templateDecoder,
          offset = offset.map(DoubleVector.fromSeq),
          sourceDomain = source,
          targetDomain = target,
          label = params.label.getOrElse(""),
          metadata = params.metadata - TransportKindKey,
          adjointConvention = adjoint
        ).left.map(error)
        _ <-
          if response.shape.samples == run.shape.spatialSize then Right(())
          else Left(ArchiveError.ShapeMismatch(s"transport response has ${response.shape.samples} samples but run has ${run.shape.spatialSize} voxels"))
      yield response
    }

  def fromBoldZipArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, BoldZipPayload] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- boldZipDescriptor(valid, runLabel)
        params <- desc.params match
          case p: TransformParams.Custom if p.name == BoldZipKindValue => Right(p)
          case _ => Left(ArchiveError.InvalidArchive("BOLDZip descriptor missing typed custom params"))
        sourceDomain <- params.sourceDomain.toRight(ArchiveError.InvalidArchive("BOLDZip descriptor missing source domain"))
        targetDomain <- params.targetDomain.toRight(ArchiveError.InvalidArchive("BOLDZip descriptor missing target domain"))
        temporalPath <- byRole(desc, DatasetRole.TemporalBasis)
        temporalBasis <- doubleMatrixPayload(valid, temporalPath, "BOLDZip temporal basis")
        thetaPath <- byRole(desc, BoldZipCarrierThetaRole)
        carrierTheta <- doubleMatrixPayload(valid, thetaPath, "BOLDZip carrier theta")
        coarseMatrix <- optionalDoubleMatrix(valid, desc, BoldZipCoarseBasisRole, "BOLDZip coarse spatial basis")
        detailMatrix <- optionalDoubleMatrix(valid, desc, BoldZipDetailBasisRole, "BOLDZip detail spatial basis")
        spatialBasis <- BoldZipSpatialBasis(
          sampleCount = run.shape.spatialSize,
          coarse = coarseMatrix.map(matrix => BoldZipCoarseBasis.MatrixBasis(toDoubleMatrix(matrix))).getOrElse(BoldZipCoarseBasis.Absent),
          detail = detailMatrix.map(matrix => BoldZipDetailBasis.MatrixBasis(toDoubleMatrix(matrix))).getOrElse(BoldZipDetailBasis.IdentitySamples),
          label = params.metadata.getOrElse(BoldZipSpatialBasisLabelKey, "")
        ).left.map(error)
        carrierLoadings <- boldZipCarrierLoadings(valid, desc, spatialBasis, carrierTheta.rows)
        texture <- boldZipTextureEntries(valid, desc)
        events <- boldZipEventEntries(valid, desc)
        offset <- optionalOffset(valid, desc, DatasetRole.SampleOffset, "BOLDZip offset")
        source <- DomainId(sourceDomain).left.map(error)
        target <- DomainId(targetDomain).left.map(error)
        response <- BoldZipPayload(
          temporalBasis = toDoubleMatrix(temporalBasis),
          carrierTheta = toDoubleMatrix(carrierTheta),
          carrierLoadings = carrierLoadings,
          spatialBasis = spatialBasis,
          texture = texture,
          events = events,
          offset = offset.map(DoubleVector.fromSeq),
          sourceDomain = source,
          targetDomain = target,
          label = params.label.getOrElse(""),
          metadata = params.metadata -- Set(BoldZipKindKey, BoldZipSpatialBasisLabelKey)
        ).left.map(error)
        _ <-
          if response.shape.timepoints == run.shape.timepoints then Right(())
          else Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.timepoints} timepoints but run has ${run.shape.timepoints} timepoints"))
        _ <-
          if response.shape.samples == run.shape.spatialSize then Right(())
          else Left(ArchiveError.ShapeMismatch(s"BOLDZip response has ${response.shape.samples} samples but run has ${run.shape.spatialSize} voxels"))
      yield response
    }

  def isBoldZipArchive(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(isBoldZipDescriptor)

  def isTransportArchive(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(isTransportDescriptor)

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentArchiveResponse] =
    archive.validate.flatMap { valid =>
      if isBoldZipArchive(valid) then fromBoldZipArchive(valid, runLabel).map(LatentArchiveResponse.BoldZip(_))
      else if isTransportArchive(valid) then fromTransportArchive(valid, runLabel).map(LatentArchiveResponse.Transport(_))
      else
        temporalDctDescriptor(valid, runLabel) match
          case Some(desc) =>
            fromExplicitArchive(valid, runLabel).flatMap { response =>
              temporalDctSpec(valid, runLabel, desc).map { case (spec, center, ridge) =>
                LatentArchiveResponse.TemporalDct(response, spec, center, ridge)
              }
            }
          case None =>
            sharedBasisDescriptor(valid, runLabel) match
              case Some(_) =>
                fromSharedBasisArchive(valid, runLabel).map(LatentArchiveResponse.SharedBasis(_))
              case None =>
                fromExplicitArchive(valid, runLabel).map(LatentArchiveResponse.Explicit(_))
    }

  def fromExplicitArchive(
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

  def fromSharedBasisArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, SharedBasisLatentArchive] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- sharedBasisDescriptor(valid, runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no shared-basis latent descriptor"))
        params <- desc.params match
          case p: TransformParams.SharedBasisEmbed => Right(p)
          case _ => Left(ArchiveError.InvalidArchive("shared-basis descriptor missing typed embed params"))
        coefficientsPath <- byRole(desc, DatasetRole.Coefficients)
        coefficients <- doubleMatrixPayload(valid, coefficientsPath, "shared-basis coefficients")
        offset <- optionalOffset(valid, desc, DatasetRole.Offset, "shared-basis offset")
        source <- DomainId(params.targetDomain.getOrElse("shared_basis.coefficients")).left.map(error)
        target <- DomainId(params.sourceDomain.getOrElse("voxels")).left.map(error)
        response <- SharedBasisLatentArchive(
          coefficients = toDoubleMatrix(coefficients),
          basis = params.basis,
          offset = offset.map(DoubleVector.fromSeq),
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

  private def boldZipDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms
      .find(desc => isBoldZipDescriptor(desc) && desc.datasets.exists(_.path.value.startsWith(prefix)))
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no BOLDZip latent descriptor"))

  private def transportDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms
      .find(desc => isTransportDescriptor(desc) && desc.datasets.exists(_.path.value.startsWith(prefix)))
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no transport latent descriptor"))

  private def temporalDctDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms.find { desc =>
      desc.kind == TransformKind.Temporal &&
        desc.datasets.exists(_.path.value.startsWith(prefix)) &&
        (desc.params match
          case TransformParams.TemporalDct(_) => true
          case _ => false)
    }

  private def temporalDctSpec(
      archive: LnaArchive,
      runLabel: RunLabel,
      desc: TransformDescriptor
  ): Either[ArchiveError, (DctSpec, Boolean, RidgePenalty)] =
    for
      run <- archive.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
      params <- desc.params match
        case TransformParams.TemporalDct(value) => Right(value)
        case _ => Left(ArchiveError.InvalidArchive("temporal DCT descriptor missing typed params"))
      ridge <- RidgePenalty(params.ridge).left.map(error)
      spec <- DctSpec(run.shape.timepoints, params.components, latentNorm(params.norm)).left.map(error)
    yield (spec, params.center, ridge)

  private def sharedBasisDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms.find { desc =>
      desc.kind == TransformKind.Embed &&
        desc.datasets.exists(_.path.value.startsWith(prefix)) &&
        (desc.params match
          case _: TransformParams.SharedBasisEmbed => true
          case _ => false)
      }

  private def isBoldZipDescriptor(desc: TransformDescriptor): Boolean =
    desc.kind == BoldZipKind &&
      (desc.params match
        case p: TransformParams.Custom => p.name == BoldZipKindValue
        case _                         => false)

  private def isTransportDescriptor(desc: TransformDescriptor): Boolean =
    desc.kind == TransformKind.Embed &&
      (desc.params match
        case p: TransformParams.Embed => p.metadata.get(TransportKindKey).contains(TransportKindValue)
        case _ => false)

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"${desc.name} descriptor missing ${role.value} dataset"))

  private def doubleMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, DMat] =
    archive.payload(path) match
      case Some(Payload.DoubleMatrix(data, _)) => Right(data)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double matrix"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def optionalDoubleMatrix(
      archive: LnaArchive,
      desc: TransformDescriptor,
      role: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[DMat]] =
    desc.datasets.find(_.role == role) match
      case None =>
        Right(None)
      case Some(ref) =>
        doubleMatrixPayload(archive, ref.path, label).map(Some(_))

  private def intMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, Payload.IntMatrix] =
    archive.payload(path) match
      case Some(payload: Payload.IntMatrix) => Right(payload)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"$label payload is not an integer matrix"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def doubleVectorPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, Vector[Double]] =
    archive.payload(path) match
      case Some(Payload.DoubleVector(values, _)) => Right(values)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double vector"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def boldZipCarrierLoadings(
      archive: LnaArchive,
      desc: TransformDescriptor,
      spatialBasis: BoldZipSpatialBasis,
      carriers: Int
  ): Either[ArchiveError, DoubleMatrix] =
    optionalDoubleMatrix(archive, desc, BoldZipCarrierLoadingsRole, "BOLDZip carrier loadings").flatMap {
      case Some(matrix) =>
        val values = toDoubleMatrix(matrix)
        if values.rows != spatialBasis.coarseAtoms then
          Left(ArchiveError.ShapeMismatch(s"BOLDZip carrier loadings have ${values.rows} rows but coarse basis has ${spatialBasis.coarseAtoms} atoms"))
        else if values.cols != carriers then
          Left(ArchiveError.ShapeMismatch(s"BOLDZip carrier loadings have ${values.cols} columns but carrier theta has $carriers carriers"))
        else Right(values)
      case None if spatialBasis.coarseAtoms == 0 =>
        Right(DoubleMatrix.zeros(0, carriers))
      case None =>
        Left(ArchiveError.InvalidArchive("BOLDZip archive has a coarse basis but no carrier loadings"))
    }

  private def boldZipTextureEntries(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Vector[BoldZipTextureEntry]] =
    optionalIndexAmplitudeTable(
      archive = archive,
      desc = desc,
      indexRole = BoldZipTextureIndexRole,
      amplitudeRole = BoldZipTextureAmplitudeRole,
      label = "BOLDZip texture"
    ).flatMap {
      case None => Right(Vector.empty)
      case Some((index, amplitudes)) =>
        traverse(Vector.tabulate(index.rows)(identity)) { row =>
          BoldZipTextureEntry
            .checked(
              atom = index(row, 0),
              carrier = index(row, 1),
              amplitude = amplitudes(row),
              lag = index(row, 2)
            )
            .left
            .map(error)
        }
    }

  private def boldZipEventEntries(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Vector[BoldZipResidualEvent]] =
    optionalIndexAmplitudeTable(
      archive = archive,
      desc = desc,
      indexRole = BoldZipEventIndexRole,
      amplitudeRole = BoldZipEventAmplitudeRole,
      label = "BOLDZip residual event"
    ).flatMap {
      case None => Right(Vector.empty)
      case Some((index, amplitudes)) =>
        traverse(Vector.tabulate(index.rows)(identity)) { row =>
          BoldZipResidualEvent
            .checked(
              atom = index(row, 0),
              frame = index(row, 1),
              amplitude = amplitudes(row),
              duration = index(row, 2)
            )
            .left
            .map(error)
        }
    }

  private def optionalIndexAmplitudeTable(
      archive: LnaArchive,
      desc: TransformDescriptor,
      indexRole: DatasetRole,
      amplitudeRole: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[(Payload.IntMatrix, Vector[Double])]] =
    (desc.datasets.find(_.role == indexRole), desc.datasets.find(_.role == amplitudeRole)) match
      case (None, None) =>
        Right(None)
      case (Some(_), None) =>
        Left(ArchiveError.InvalidArchive(s"$label index table is present without amplitudes"))
      case (None, Some(_)) =>
        Left(ArchiveError.InvalidArchive(s"$label amplitudes are present without an index table"))
      case (Some(indexRef), Some(amplitudeRef)) =>
        for
          index <- intMatrixPayload(archive, indexRef.path, s"$label index")
          amplitudes <- doubleVectorPayload(archive, amplitudeRef.path, s"$label amplitude")
          _ <-
            if index.cols == 3 then Right(())
            else Left(ArchiveError.ShapeMismatch(s"$label index table must have 3 columns"))
          _ <-
            if index.rows == amplitudes.length then Right(())
            else Left(ArchiveError.ShapeMismatch(s"$label index rows ${index.rows} do not match amplitude length ${amplitudes.length}"))
        yield Some((index, amplitudes))

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Option[Vector[Double]]] =
    optionalOffset(archive, desc, DatasetRole.SampleOffset, "transport offset")

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor,
      role: DatasetRole,
      label: String
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == role) match
      case None => Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_) => Left(ArchiveError.ShapeMismatch(s"$label payload is not a double vector"))
          case None => Left(ArchiveError.MissingPayload(ref.path))

  private def linearMapMatrix(map: LinearMap): Either[ArchiveError, DoubleMatrix] =
    map
      .forward(DoubleMatrix.eye(map.cols))
      .left
      .map(err => ArchiveError.InvalidArchive(err.message))

  private def linearMapFromMatrix(matrix: DMat): Either[ArchiveError, LinearMap] =
    linearMapFromMatrix(toDoubleMatrix(matrix))

  private def linearMapFromMatrix(matrix: DoubleMatrix): Either[ArchiveError, LinearMap] =
    val rows = scala.collection.mutable.ArrayBuffer.empty[Int]
    val cols = scala.collection.mutable.ArrayBuffer.empty[Int]
    val values = scala.collection.mutable.ArrayBuffer.empty[Double]
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val value = matrix(row, col)
        if !value.isFinite then return Left(ArchiveError.InvalidArchive(s"linear map contains non-finite value at ${row},${col}"))
        if value != 0.0 then
          rows += row
          cols += col
          values += value
        col += 1
      row += 1
    CsrMatrix
      .fromTriplets(matrix.rows, matrix.cols, rows.toArray, cols.toArray, values.toArray)
      .left
      .map(err => ArchiveError.InvalidArchive(err.message))

  private def boldZipTexturePayloads(
      entries: Vector[BoldZipTextureEntry],
      indexPath: ArchivePath,
      amplitudePath: ArchivePath
  ): Map[ArchivePath, Payload] =
    if entries.isEmpty then Map.empty
    else
      Map(
        indexPath -> Payload.IntMatrix(
          rows = entries.length,
          cols = 3,
          values = entries.flatMap(entry => Vector(entry.atom.value, entry.carrier.value, entry.lag.value)),
          dtype = LnaDType.Int32
        ),
        amplitudePath -> Payload.DoubleVector(entries.map(_.amplitude))
      )

  private def boldZipEventPayloads(
      entries: Vector[BoldZipResidualEvent],
      indexPath: ArchivePath,
      amplitudePath: ArchivePath
  ): Map[ArchivePath, Payload] =
    if entries.isEmpty then Map.empty
    else
      Map(
        indexPath -> Payload.IntMatrix(
          rows = entries.length,
          cols = 3,
          values = entries.flatMap(entry => Vector(entry.atom.value, entry.frame.value, entry.duration.value)),
          dtype = LnaDType.Int32
        ),
        amplitudePath -> Payload.DoubleVector(entries.map(_.amplitude))
      )

  private def toDMat(matrix: DoubleMatrix): DMat =
    DMat.fromRows(matrix.toRows)

  private def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

  private def archiveNorm(norm: DctNorm): TemporalDctNorm =
    norm match
      case DctNorm.Ortho => TemporalDctNorm.Ortho
      case DctNorm.None  => TemporalDctNorm.None

  private def latentNorm(norm: TemporalDctNorm): DctNorm =
    norm match
      case TemporalDctNorm.Ortho => DctNorm.Ortho
      case TemporalDctNorm.None  => DctNorm.None

  private def transportAdjoint(value: TransportAdjointConvention): String =
    value match
      case TransportAdjointConvention.EuclideanDiscrete => "euclidean_discrete"

  private def transportAdjoint(value: String): Either[ArchiveError, TransportAdjointConvention] =
    value match
      case "euclidean_discrete" => Right(TransportAdjointConvention.EuclideanDiscrete)
      case other => Left(ArchiveError.InvalidArchive(s"unsupported transport adjoint convention: $other"))

  private def error(latentError: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(latentError.message)

  private def traverse[A, B](values: Iterable[A])(f: A => Either[ArchiveError, B]): Either[ArchiveError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val it = values.iterator
    var failure = Option.empty[ArchiveError]
    while it.hasNext && failure.isEmpty do
      f(it.next()) match
        case Left(error) => failure = Some(error)
        case Right(value) => out += value
    failure.fold(Right(out.result()))(Left(_))

private def archiveFirstNonFinite(label: String, matrix: DoubleMatrix): Option[LatentError] =
  var i = 0
  var error = Option.empty[LatentError]
  while i < matrix.dataArray.length && error.isEmpty do
    val value = matrix.dataArray(i)
    if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
    i += 1
  error

private def archiveFirstNonFinite(label: String, vector: DoubleVector): Option[LatentError] =
  var i = 0
  var error = Option.empty[LatentError]
  while i < vector.length && error.isEmpty do
    val value = vector(i)
    if !value.isFinite then error = Some(LatentError.NonFiniteValue(label, i, value))
    i += 1
  error
