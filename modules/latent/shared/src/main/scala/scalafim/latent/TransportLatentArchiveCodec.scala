package scalafim.latent

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel}
import scalafim.archive.lna.{
  DatasetRef,
  DatasetRole,
  LnaArchive,
  LnaDType,
  LnaManifest,
  LnaRun,
  LnaShape,
  Payload,
  TransformDescriptor,
  TransformKind,
  TransformParams
}
import scalafim.image.NeuroSpace
import scalafim.linalg.DoubleVector
import scalafim.latent.LatentArchivePayloads.*

private[latent] object TransportLatentArchiveCodec:
  private val KindKey = "lna.response.kind"
  private val KindValue = "transport_latent"
  private val NativeDecoderRole = DatasetRole.Other("transport_native_decoder_t")
  private val TemplateDecoderRole = DatasetRole.Other("transport_template_decoder_t")
  private val ToAnalysisRole = DatasetRole.Other("transport_to_analysis")
  private val ToRawRole = DatasetRole.Other("transport_to_raw")

  def toArchive(
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
          case None          => Right(None)
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
              KindKey -> KindValue,
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
              "response.kind" -> KindValue,
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

  def fromArchive(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, TransportLatentResponse] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- descriptor(valid, runLabel)
        params <- desc.params match
          case p: TransformParams.Embed => Right(p)
          case _                        => Left(ArchiveError.InvalidArchive("transport latent descriptor missing typed embed params"))
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
        offset <- optionalOffset(valid, desc, DatasetRole.SampleOffset, "transport offset")
        _ <-
          if coefficients.rows == run.shape.timepoints then Right(())
          else Left(ArchiveError.ShapeMismatch(s"transport coefficients have ${coefficients.rows} rows but run has ${run.shape.timepoints} timepoints"))
        nativeDecoder <- linearMapFromMatrix(toDoubleMatrix(nativeDecoderT).transpose)
        templateDecoder <- templateDecoderT match
          case None           => Right(None)
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
          metadata = params.metadata - KindKey,
          adjointConvention = adjoint
        ).left.map(error)
        _ <-
          if response.shape.samples == run.shape.spatialSize then Right(())
          else Left(ArchiveError.ShapeMismatch(s"transport response has ${response.shape.samples} samples but run has ${run.shape.spatialSize} voxels"))
      yield response
    }

  def descriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    descriptorOption(archive, runLabel)
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no transport latent descriptor"))

  def descriptorOption(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Option[TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms
      .find(desc => isDescriptor(desc) && desc.datasets.exists(_.path.value.startsWith(prefix)))

  def isArchive(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(isDescriptor)

  def isDescriptor(desc: TransformDescriptor): Boolean =
    desc.kind == TransformKind.Embed &&
      (desc.params match
        case p: TransformParams.Embed => p.metadata.get(KindKey).contains(KindValue)
        case _                        => false)

  private def transportAdjoint(value: TransportAdjointConvention): String =
    value match
      case TransportAdjointConvention.EuclideanDiscrete => "euclidean_discrete"

  private def transportAdjoint(value: String): Either[ArchiveError, TransportAdjointConvention] =
    value match
      case "euclidean_discrete" => Right(TransportAdjointConvention.EuclideanDiscrete)
      case other                => Left(ArchiveError.InvalidArchive(s"unsupported transport adjoint convention: $other"))
