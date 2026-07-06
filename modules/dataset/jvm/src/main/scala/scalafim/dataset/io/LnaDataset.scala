package scalafim.dataset.io

import scalafim.archive.{ArchiveError, ArchivePath, RunLabel}
import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store, LnaSharedBasisResolver, SharedBasisResolutionContext}
import scalafim.archive.lna.{
  DatasetRole,
  LnaArchive,
  LnaRun,
  Payload,
  SharedBasisArtifact,
  SharedBasisRegistry,
  SharedBasisRegistryCodec,
  TransformDescriptor,
  TransformParams
}
import scalafim.bids.{BidsJson, BidsTable, JsonValue}
import scalafim.dataset.{DatasetId, DatasetMetadata, InMemoryDatasetBackend, LatentArchiveDatasetBackend, LatentResponseDatasetBackend}
import scalafim.image.{DMat, Mask, NArrayUtil, NeuroSpace}
import scalafim.latent.{DomainId, ExplicitLatentResponse, LatentArchiveCodec}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final case class LnaDatasetQuery(
    subject: String,
    session: Option[String] = None,
    task: Option[String] = None,
    space: Option[String] = None
)

final case class LnaDataset private (root: Path):
  def subjects: Either[ArchiveError, Vector[String]] =
    try
      if !Files.isDirectory(root) then Left(ArchiveError.InvalidPath(root.toString, "LNA dataset root is not a directory"))
      else
        val stream = Files.list(root)
        try
          Right(
            stream
              .iterator()
              .asScala
              .filter(Files.isDirectory(_))
              .map(_.getFileName.toString)
              .filter(_.startsWith("sub-"))
              .toVector
              .sorted
          )
        finally stream.close()
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not list LNA dataset subjects at $root: ${e.getMessage}"))

  def datasetDescription: Either[ArchiveError, Option[JsonValue.Obj]] =
    val path = root.resolve("dataset_description.json")
    if !Files.exists(path) then Right(None)
    else
      readString(path).flatMap { text =>
        BidsJson
          .parseObject(text)
          .left
          .map(err => ArchiveError.InvalidArchive(s"dataset_description.json: ${err.message}"))
          .map(Some(_))
      }

  def participants: Either[ArchiveError, Option[BidsTable]] =
    val path = root.resolve("participants.tsv")
    if !Files.exists(path) then Right(None)
    else
      readString(path).flatMap { text =>
        BidsTable
          .parse(text)
          .left
          .map(err => ArchiveError.InvalidArchive(s"participants.tsv: ${err.message}"))
          .map(Some(_))
      }

  def sharedBasisRegistry: Either[ArchiveError, SharedBasisRegistry] =
    val path = root.resolve("bases").resolve("registry.json")
    if !Files.exists(path) then Right(SharedBasisRegistry())
    else readString(path).flatMap(SharedBasisRegistryCodec.parse)

  def findLnaFiles(query: LnaDatasetQuery): Either[ArchiveError, Vector[Path]] =
    for
      subjectLabel <- LnaDataset.subjectLabel(query.subject)
      sessionLabel <- query.session match
        case None => Right(None)
        case Some(value) => LnaDataset.sessionLabel(value).map(Some(_))
      task <- LnaDataset.optionalBareLabel(query.task, "task")
      space <- LnaDataset.optionalBareLabel(query.space, "space")
      files <- findLnaFiles(subjectLabel, sessionLabel, task, space)
    yield files

  def readArchive(path: Path): Either[ArchiveError, LnaArchive] =
    val normalized = resolveInsideRoot(path)
    if !normalized.startsWith(root) then
      Left(ArchiveError.InvalidPath(path.toString, "LNA archive path must stay inside the dataset root"))
    else LnaHdf5Store.default.read(normalized)

  def backendFor(path: Path, id: DatasetId): Either[ArchiveError, LatentArchiveDatasetBackend] =
    val normalized = resolveInsideRoot(path)
    readArchive(path).map { archive =>
      LatentArchiveDatasetBackend(id = id, archive = archive, metadata = metadataFor(normalized))
    }

  def latentBackendFor(
      path: Path,
      id: DatasetId,
      run: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    val normalized = resolveInsideRoot(path)
    readArchive(path).flatMap { archive =>
      for
        runInfo <- archive.run(run).toRight(ArchiveError.InvalidArchive(s"run '${run.value}' not found"))
        backend <-
          if LatentArchiveCodec.isTransportArchive(archive) then
            LatentArchiveCodec
              .fromTransportArchive(archive, run)
              .left
              .map(err => ArchiveError.InvalidArchive(err.message))
              .map { response =>
                LatentResponseDatasetBackend(id, response, runInfo.shape.space, metadataFor(normalized))
              }
          else if hasSharedBasisEmbed(archive) then
            sharedBasisLatentBackend(
              archive = archive,
              archivePath = normalized,
              id = id,
              runInfo = runInfo,
              metadata = metadataFor(normalized)
            )
          else
            LatentArchiveCodec
              .fromArchive(archive, run)
              .left
              .map(err => ArchiveError.InvalidArchive(err.message))
              .map { response =>
                LatentResponseDatasetBackend(id, response, runInfo.shape.space, metadataFor(normalized))
              }
      yield backend
    }

  def materializedBackendFor(
      path: Path,
      id: DatasetId,
      run: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, InMemoryDatasetBackend] =
    val normalized = resolveInsideRoot(path)
    readArchive(path).flatMap { archive =>
      for
        runInfo <- archive.run(run).toRight(ArchiveError.InvalidArchive(s"run '${run.value}' not found"))
        dense <- LnaSharedBasisResolver.reconstruct(
          archive,
          SharedBasisResolutionContext(archivePath = normalized, datasetRoot = Some(root)),
          run
        )
      yield InMemoryDatasetBackend(
        id = id,
        data = dense,
        space = runInfo.shape.space,
        metadata = metadataFor(normalized)
      )
    }

  def readSubject(query: LnaDatasetQuery): Either[ArchiveError, LatentArchiveDatasetBackend] =
    findLnaFiles(query).flatMap {
      case Vector() =>
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject}'"))
      case Vector(path) =>
        backendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)))
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject}': ${many.map(root.relativize).mkString(", ")}"))
    }

  def readSubjectLatent(
      query: LnaDatasetQuery,
      run: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    findLnaFiles(query).flatMap {
      case Vector() =>
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject}'"))
      case Vector(path) =>
        latentBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)), run)
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject}': ${many.map(root.relativize).mkString(", ")}"))
    }

  def readSubjectMaterialized(query: LnaDatasetQuery): Either[ArchiveError, InMemoryDatasetBackend] =
    findLnaFiles(query).flatMap {
      case Vector() =>
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject}'"))
      case Vector(path) =>
        materializedBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)))
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject}': ${many.map(root.relativize).mkString(", ")}"))
    }

  private def findLnaFiles(
      subjectLabel: String,
      sessionLabel: Option[String],
      task: Option[String],
      space: Option[String]
  ): Either[ArchiveError, Vector[Path]] =
    val subjectDir = root.resolve(subjectLabel).normalize()
    if !subjectDir.startsWith(root) then
      Left(ArchiveError.InvalidPath(subjectLabel, "subject path escapes dataset root"))
    else if !Files.isDirectory(subjectDir) then Right(Vector.empty)
    else
      try
        val stream = Files.walk(subjectDir)
        try
          val files =
            stream
              .iterator()
              .asScala
              .filter(Files.isRegularFile(_))
              .filter(path => path.getFileName.toString.endsWith(".lna.h5"))
              .filter(path => sessionLabel.forall(label => path.iterator().asScala.exists(_.toString == label)))
              .filter(path => task.forall(label => path.getFileName.toString.contains(s"task-$label")))
              .filter(path => space.forall(label => path.getFileName.toString.contains(s"space-$label")))
              .map(_.toAbsolutePath.normalize())
              .toVector
              .sortBy(path => root.relativize(path).toString)
          Right(files)
        finally stream.close()
      catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not scan LNA files under $subjectDir: ${e.getMessage}"))

  private def metadataFor(path: Path): DatasetMetadata =
    DatasetMetadata(
      Map(
        "lna.dataset_root" -> root.toString,
        "lna.archive_path" -> path.toString,
        "lna.archive_relative_path" -> root.relativize(path).toString.replace('\\', '/')
      )
    )

  private def sharedBasisLatentBackend(
      archive: LnaArchive,
      archivePath: Path,
      id: DatasetId,
      runInfo: LnaRun,
      metadata: DatasetMetadata
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    archive.validate.flatMap { valid =>
      for
        desc <- sharedBasisDescriptor(valid, runInfo)
        params <- desc.params match
          case p: TransformParams.SharedBasisEmbed => Right(p)
          case _ => Left(ArchiveError.InvalidArchive("shared basis embed descriptor missing typed params"))
        coeffPath <- byRole(desc, DatasetRole.Coefficients)
        coefficients <- doubleMatrixPayload(valid, coeffPath, "coefficients")
        offset <- optionalOffset(valid, desc)
        basisPath <- LnaSharedBasisResolver.resolve(
          params.basis,
          SharedBasisResolutionContext(archivePath = archivePath, datasetRoot = Some(root))
        )
        artifact <- JhdfSharedBasisStore.read(basisPath).flatMap(SharedBasisArtifact.validateFinite)
        _ <- validateSharedBasisShapes(coefficients, offset, artifact, runInfo)
        response <- sharedBasisResponse(coefficients, offset, artifact, params)
        mask <- sharedBasisMask(runInfo.shape.space, artifact)
      yield LatentResponseDatasetBackend(
        id = id,
        response = response,
        space = runInfo.shape.space,
        mask = mask,
        metadata = metadata
      )
    }

  private def validateSharedBasisShapes(
      coefficients: DMat,
      offset: Option[Vector[Double]],
      basis: SharedBasisArtifact,
      runInfo: LnaRun
  ): Either[ArchiveError, Unit] =
    if coefficients.rows != runInfo.shape.timepoints then
      Left(ArchiveError.ShapeMismatch(s"coefficient rows ${coefficients.rows} do not match run timepoints ${runInfo.shape.timepoints}"))
    else if coefficients.cols != basis.nAtoms then
      Left(ArchiveError.ShapeMismatch(s"coefficients have ${coefficients.cols} columns but shared basis has ${basis.nAtoms} atoms"))
    else if basis.mask.values.length != runInfo.shape.spatialSize then
      Left(ArchiveError.ShapeMismatch(s"shared basis mask has ${basis.mask.values.length} entries but run space has ${runInfo.shape.spatialSize} voxels"))
    else
      offset match
        case Some(values) if values.length != basis.nVoxels =>
          Left(ArchiveError.ShapeMismatch(s"offset has ${values.length} values but shared basis has ${basis.nVoxels} active voxels"))
        case Some(values) if values.exists(value => !value.isFinite) =>
          Left(ArchiveError.InvalidArchive("shared basis offset contains non-finite values"))
        case _ =>
          Right(())

  private def sharedBasisResponse(
      coefficients: DMat,
      offset: Option[Vector[Double]],
      basis: SharedBasisArtifact,
      params: TransformParams.SharedBasisEmbed
  ): Either[ArchiveError, ExplicitLatentResponse] =
    for
      sourceDomain <- DomainId(params.targetDomain.getOrElse("shared_basis.coefficients")).left.map(err => ArchiveError.InvalidArchive(err.message))
      targetDomain <- DomainId(params.sourceDomain.getOrElse("voxels")).left.map(err => ArchiveError.InvalidArchive(err.message))
      response <- ExplicitLatentResponse(
        basis = toDoubleMatrix(coefficients),
        loadings = toDoubleMatrix(basis.loadings),
        offset = offset.map(DoubleVector.fromSeq),
        sourceDomain = sourceDomain,
        targetDomain = targetDomain,
        label = params.label.getOrElse(""),
        metadata = params.metadata ++ Map(
          "family" -> "shared_basis",
          "basis.kind" -> basis.kind,
          "basis.n_atoms" -> basis.nAtoms.toString,
          "basis.n_voxels" -> basis.nVoxels.toString,
          "basis.mask_size" -> basis.mask.values.length.toString,
          "basis.mask_active" -> basis.mask.activeCount.toString
        )
      ).left.map(err => ArchiveError.InvalidArchive(err.message))
    yield response

  private def sharedBasisMask(
      space: NeuroSpace,
      basis: SharedBasisArtifact
  ): Either[ArchiveError, Mask.MaskVol] =
    if basis.mask.values.length != space.spatialDims.product then
      Left(ArchiveError.ShapeMismatch(s"shared basis mask has ${basis.mask.values.length} entries but space has ${space.spatialDims.product} voxels"))
    else
      val indices = Array.newBuilder[Int]
      var i = 0
      while i < basis.mask.values.length do
        if basis.mask.values(i) then indices += i
        i += 1
      Right(Mask.fromIndices(space, NArrayUtil.fromArray(indices.result()), label = s"shared-basis:${basis.kind}"))

  private def sharedBasisDescriptor(
      archive: LnaArchive,
      run: LnaRun
  ): Either[ArchiveError, TransformDescriptor] =
    val prefix = s"/scans/${run.label.value}/"
    archive.manifest.transforms.reverseIterator
      .find { desc =>
        desc.params.isInstanceOf[TransformParams.SharedBasisEmbed] &&
        desc.datasets.exists(ref => ref.role == DatasetRole.Coefficients && (ref.path == run.output || ref.path.value.startsWith(prefix)))
      }
      .toRight(ArchiveError.InvalidArchive(s"run '${run.label.value}' has no shared basis embed descriptor"))

  private def optionalOffset(
      archive: LnaArchive,
      desc: TransformDescriptor
  ): Either[ArchiveError, Option[Vector[Double]]] =
    desc.datasets.find(_.role == DatasetRole.Offset) match
      case None =>
        Right(None)
      case Some(ref) =>
        archive.payload(ref.path) match
          case Some(Payload.DoubleVector(values, _)) => Right(Some(values))
          case Some(_) => Left(ArchiveError.ShapeMismatch("shared basis offset payload is not a double vector"))
          case None => Left(ArchiveError.MissingPayload(ref.path))

  private def doubleMatrixPayload(
      archive: LnaArchive,
      path: ArchivePath,
      label: String
  ): Either[ArchiveError, DMat] =
    archive.payload(path) match
      case Some(Payload.DoubleMatrix(data, _)) => Right(data)
      case Some(_) => Left(ArchiveError.ShapeMismatch(s"shared basis $label payload is not a double matrix"))
      case None => Left(ArchiveError.MissingPayload(path))

  private def byRole(desc: TransformDescriptor, role: DatasetRole): Either[ArchiveError, ArchivePath] =
    desc.datasets
      .find(_.role == role)
      .map(_.path)
      .toRight(ArchiveError.InvalidArchive(s"${desc.kind.value} descriptor missing ${role.value} dataset"))

  private def hasSharedBasisEmbed(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(_.params.isInstanceOf[TransformParams.SharedBasisEmbed])

  private def toDoubleMatrix(matrix: DMat): DoubleMatrix =
    DoubleMatrix.fromRows(matrix.toRows)

  private def resolveInsideRoot(path: Path): Path =
    if path.isAbsolute then path.toAbsolutePath.normalize()
    else root.resolve(path).toAbsolutePath.normalize()

  private def readString(path: Path): Either[ArchiveError, String] =
    try Right(Files.readString(path))
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not read ${path.toAbsolutePath}: ${e.getMessage}"))

object LnaDataset:
  private val LabelPattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$".r

  def open(root: Path): Either[ArchiveError, LnaDataset] =
    val normalized = root.toAbsolutePath.normalize()
    if !Files.isDirectory(normalized) then
      Left(ArchiveError.InvalidPath(root.toString, "LNA dataset root does not exist or is not a directory"))
    else Right(LnaDataset(normalized))

  def unsafe(root: Path): LnaDataset =
    open(root).fold(err => throw IllegalArgumentException(err.message), identity)

  private[io] def subjectLabel(value: String): Either[ArchiveError, String] =
    val normalized = if value.startsWith("sub-") then value else s"sub-$value"
    safeLabel(normalized, "subject")

  private[io] def sessionLabel(value: String): Either[ArchiveError, String] =
    val bare = value.stripPrefix("ses-")
    safeLabel(bare, "session").map(valid => s"ses-$valid")

  private[io] def optionalBareLabel(value: Option[String], label: String): Either[ArchiveError, Option[String]] =
    value match
      case None => Right(None)
      case Some(text) => safeLabel(text, label).map(Some(_))

  private[io] def safeLabel(value: String, label: String): Either[ArchiveError, String] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(ArchiveError.InvalidPath(value, s"$label label must be non-empty"))
    else if LabelPattern.matches(trimmed) then Right(trimmed)
    else Left(ArchiveError.InvalidPath(value, s"$label label contains invalid characters"))

  private[io] def datasetIdFromPath(root: Path, path: Path): String =
    root
      .relativize(path.toAbsolutePath.normalize())
      .toString
      .replace('\\', '/')
      .stripSuffix(".lna.h5")
