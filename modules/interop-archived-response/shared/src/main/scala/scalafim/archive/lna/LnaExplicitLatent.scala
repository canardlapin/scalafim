package scalafim.archive.lna

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel, RunScopedPath}
import scalafim.image.{DMat, NeuroSpace}

object LnaExplicitLatent:
  val MetadataKindKey: String =
    ExplicitLatentDescriptor.MetadataKindKey

  val MetadataKindValue: String =
    ExplicitLatentDescriptor.MetadataKindValue

  final case class Response(
      basis: DMat,
      loadings: DMat,
      offset: Option[Vector[Double]] = None,
      sourceDomain: String = "latent.coefficients",
      targetDomain: String = "latent.samples",
      label: String = "",
      metadata: Map[String, String] = Map.empty
  ):
    def timepoints: Int = basis.rows
    def samples: Int = loadings.rows
    def coefficients: Int = basis.cols

  def archive(
      response: Response,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    validateResponse(response, Some(space)).map { valid =>
      val base = ArchivePath(s"/scans/${runLabel.value}/step_00_explicit_latent")
      val basisPath = base / "temporal_basis"
      val loadingsPath = base / "loadings"
      val offsetPath = base / "offset"

      val basisRef = DatasetRef(basisPath, DatasetRole.TemporalBasis, Vector(valid.basis.rows, valid.basis.cols), Some(LnaDType.Float64))
      val loadingsRef = DatasetRef(loadingsPath, DatasetRole.Loadings, Vector(valid.loadings.rows, valid.loadings.cols), Some(LnaDType.Float64))
      val offsetRef = valid.offset.map(values => DatasetRef(offsetPath, DatasetRole.SampleOffset, Vector(values.length), Some(LnaDType.Float64)))

      val basisDescriptor = TransformDescriptor(
        name = "00_temporal_basis.json",
        kind = TransformKind.Basis,
        params = TransformParams.Basis(
          method = valid.metadata.getOrElse("basis", MetadataKindValue),
          k = valid.coefficients,
          center = false,
          scale = false
        ),
        inputs = Vector("time"),
        outputs = Vector("temporal_basis"),
        datasets = Vector(basisRef)
      )

      val embedDescriptor = TransformDescriptor(
        name = "01_explicit_latent.json",
        kind = TransformKind.Embed,
        params = TransformParams.Embed(
          basisPath = basisPath,
          sourceDomain = Some(valid.sourceDomain),
          targetDomain = Some(valid.targetDomain),
          label = Option.when(valid.label.nonEmpty)(valid.label),
          metadata = explicitMetadata(valid.metadata)
        ),
        inputs = Vector("temporal_basis", "loadings"),
        outputs = Vector("latent_response"),
        datasets = Vector(loadingsRef) ++ offsetRef.toVector
      )

      val refs = Vector(basisRef, loadingsRef) ++ offsetRef.toVector
      val payloads =
        Map[ArchivePath, Payload](
          basisPath -> Payload.DoubleMatrix(valid.basis),
          loadingsPath -> Payload.DoubleMatrix(valid.loadings)
        ) ++ valid.offset.map(values => offsetPath -> Payload.DoubleVector(values)).toMap

      LnaArchive(
        manifest = LnaManifest(
          creator = creator,
          requiredTransforms = Vector(TransformKind.Basis, TransformKind.Embed),
          transforms = Vector(basisDescriptor, embedDescriptor),
          runs = Vector(LnaRun(runLabel, LnaShape(space, valid.timepoints), loadingsPath)),
          datasets = refs,
          header = Map(
            "response.kind" -> MetadataKindValue,
            "space.dims" -> space.spatialDims.mkString("x"),
            "timepoints" -> valid.timepoints.toString,
            "samples" -> valid.samples.toString,
            "coefficients" -> valid.coefficients.toString,
            "source_domain" -> valid.sourceDomain,
            "target_domain" -> valid.targetDomain
          )
        ),
        payloads = payloads
      )
    }

  def read(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, Response] =
    archive.validate.flatMap { valid =>
      for
        run <- valid.run(runLabel).toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' not found"))
        desc <- explicitEmbedDescriptor(valid, runLabel)
        response <- descriptorResponse(valid, desc)
        checked <- validateResponse(response, Some(run.shape.space))
        _ <-
          if checked.timepoints == run.shape.timepoints && checked.samples == run.shape.spatialSize then Right(())
          else Left(ArchiveError.ShapeMismatch("explicit latent response shape does not match run shape"))
      yield checked
    }

  def reconstruct(
      archive: LnaArchive,
      runLabel: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, DMat] =
    read(archive, runLabel).map(dense)

  def reconstructEmbed(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, DMat] =
    descriptorResponse(archive, desc).flatMap(response => validateResponse(response, None).map(dense))

  def isExplicitEmbed(desc: TransformDescriptor): Boolean =
    ExplicitLatentDescriptor.matches(desc)

  def dense(response: Response): DMat =
    DMat.fromRows(
      Vector.tabulate(response.timepoints) { time =>
        Vector.tabulate(response.samples) { sample =>
          var sum = response.offset.fold(0.0)(_(sample))
          var component = 0
          while component < response.coefficients do
            sum += response.basis(time, component) * response.loadings(sample, component)
            component += 1
          sum
        }
      }
    )

  private[lna] def descriptorResponse(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Response] =
    if !isExplicitEmbed(desc) then Left(ArchiveError.UnsupportedTransform(desc.kind.value))
    else
      for
        params <- desc.params match
          case p: TransformParams.Embed => Right(p)
          case _ => Left(ArchiveError.InvalidArchive("explicit latent embed descriptor missing typed embed params"))
        sourceDomain <- params.sourceDomain.toRight(ArchiveError.InvalidArchive("explicit latent embed descriptor missing source domain"))
        targetDomain <- params.targetDomain.toRight(ArchiveError.InvalidArchive("explicit latent embed descriptor missing target domain"))
        loadingsPath <- byRole(desc, DatasetRole.Loadings)
        basis <- doubleMatrixPayload(archive, params.basisPath, "temporal basis")
        loadings <- doubleMatrixPayload(archive, loadingsPath, "loadings")
        offset <- optionalOffset(archive, desc)
      yield Response(
        basis = basis,
        loadings = loadings,
        offset = offset,
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = params.label.getOrElse(""),
        metadata = params.metadata - MetadataKindKey
      )

  private def explicitEmbedDescriptor(
      archive: LnaArchive,
      runLabel: RunLabel
  ): Either[ArchiveError, TransformDescriptor] =
    val runOutput = archive.run(runLabel).map(_.output)
    archive.manifest.transforms
      .find { desc =>
        isExplicitEmbed(desc) &&
        desc.datasets.exists(ref => runOutput.contains(ref.path) || RunScopedPath.from(ref.path, runLabel).isDefined)
      }
      .toRight(ArchiveError.InvalidArchive(s"run '${runLabel.value}' has no explicit latent embed descriptor"))

  private def validateResponse(
      response: Response,
      space: Option[NeuroSpace]
  ): Either[ArchiveError, Response] =
    if response.basis.cols != response.loadings.cols then
      Left(ArchiveError.ShapeMismatch(s"temporal basis has ${response.basis.cols} coefficients but loadings have ${response.loadings.cols}"))
    else if response.sourceDomain.trim.isEmpty then
      Left(ArchiveError.InvalidArchive("source domain must be non-empty"))
    else if response.targetDomain.trim.isEmpty then
      Left(ArchiveError.InvalidArchive("target domain must be non-empty"))
    else if response.metadata.keys.exists(_.trim.isEmpty) then
      Left(ArchiveError.InvalidArchive("explicit latent metadata keys must be non-empty"))
    else
      response.offset match
        case Some(values) if values.length != response.samples =>
          Left(ArchiveError.ShapeMismatch(s"offset has ${values.length} samples but loadings have ${response.samples} rows"))
        case Some(values) if values.exists(!_.isFinite) =>
          Left(ArchiveError.InvalidArchive("offset contains non-finite values"))
        case _ =>
          firstNonFinite(response.basis)
            .orElse(firstNonFinite(response.loadings)) match
            case Some(index) =>
              Left(ArchiveError.NonFiniteValue(index))
            case None =>
              space match
                case Some(value) if value.spatialDims.product != response.samples =>
                  Left(ArchiveError.ShapeMismatch(s"space has ${value.spatialDims.product} samples but loadings have ${response.samples} rows"))
                case _ =>
                  Right(response)

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == DatasetRole.SampleOffset) match
      case None => Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_) => Left(ArchiveError.ShapeMismatch("explicit latent offset payload is not a double vector"))
          case None => Left(ArchiveError.MissingPayload(ref.path))

  private def doubleMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, DMat] =
    archive.payload(path) match
      case Some(Payload.DoubleMatrix(data, _)) => Right(data)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"explicit latent $label payload is not a double matrix"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"explicit latent descriptor missing ${role.value} dataset"))

  private def explicitMetadata(metadata: Map[String, String]): Map[String, String] =
    metadata + (MetadataKindKey -> MetadataKindValue)

  private def firstNonFinite(matrix: DMat): Option[Int] =
    var r = 0
    var index = 0
    var bad = Option.empty[Int]
    while r < matrix.rows && bad.isEmpty do
      var c = 0
      while c < matrix.cols && bad.isEmpty do
        if !matrix(r, c).isFinite then bad = Some(index)
        index += 1
        c += 1
      r += 1
    bad
