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
  TransformDescriptor,
  TransformKind,
  TransformParams,
  TemporalDctNorm,
  TemporalDctParams
}
import scalafim.archive.ArchivePath
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{CsrMatrix, DoubleMatrix, DoubleVector, LinearMap}

object LatentArchiveCodec:
  private val TransportKindKey = "lna.response.kind"
  private val TransportKindValue = "transport_latent"
  private val NativeDecoderRole = DatasetRole.Other("transport_native_decoder_t")
  private val TemplateDecoderRole = DatasetRole.Other("transport_template_decoder_t")
  private val ToAnalysisRole = DatasetRole.Other("transport_to_analysis")
  private val ToRawRole = DatasetRole.Other("transport_to_raw")

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

  def isTransportArchive(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(isTransportDescriptor)

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

  private def transportDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    val prefix = s"/scans/${runLabel.value}/"
    archive.manifest.transforms
      .find(desc => isTransportDescriptor(desc) && desc.datasets.exists(_.path.value.startsWith(prefix)))
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no transport latent descriptor"))

  private def isTransportDescriptor(desc: TransformDescriptor): Boolean =
    desc.kind == TransformKind.Embed &&
      (desc.params match
        case p: TransformParams.Embed => p.metadata.get(TransportKindKey).contains(TransportKindValue)
        case _ => false)

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"transport latent descriptor missing ${role.value} dataset"))

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

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == DatasetRole.SampleOffset) match
      case None => Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_) => Left(ArchiveError.ShapeMismatch("transport offset payload is not a double vector"))
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

  private def toDMat(matrix: DoubleMatrix): DMat =
    DMat.fromRows(matrix.toRows)

  private def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

  private def archiveNorm(norm: DctNorm): TemporalDctNorm =
    norm match
      case DctNorm.Ortho => TemporalDctNorm.Ortho
      case DctNorm.None  => TemporalDctNorm.None

  private def transportAdjoint(value: TransportAdjointConvention): String =
    value match
      case TransportAdjointConvention.EuclideanDiscrete => "euclidean_discrete"

  private def transportAdjoint(value: String): Either[ArchiveError, TransportAdjointConvention] =
    value match
      case "euclidean_discrete" => Right(TransportAdjointConvention.EuclideanDiscrete)
      case other => Left(ArchiveError.InvalidArchive(s"unsupported transport adjoint convention: $other"))

  private def error(latentError: LatentError): ArchiveError =
    ArchiveError.InvalidArchive(latentError.message)
