package scalafim.dataset.io

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store, LnaSharedBasisResolver, SharedBasisResolutionContext}
import scalafim.archive.lna.{
  LnaArchive,
  LnaRun,
  SharedBasisArtifact,
  SharedBasisRegistry,
  SharedBasisRegistryCodec,
  TransformParams
}
import scalafim.bids.{BidsJson, BidsTable, JsonValue}
import scalafim.dataset.{DatasetError, DatasetId, DatasetMetadata, InMemoryDatasetBackend, LatentArchiveDatasetBackend, LatentResponseDatasetBackend}
import scalafim.latent.LatentArchiveCodec

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

private val LnaLabelPattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$".r

private def safeLnaLabel(value: String, label: String): Either[DatasetError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(DatasetError.InvalidLabel(label, value, "must be non-empty"))
  else if LnaLabelPattern.matches(trimmed) then Right(trimmed)
  else Left(DatasetError.InvalidLabel(label, value, "contains invalid characters"))

private def traverseOptional[A](
    value: Option[String],
    make: String => Either[DatasetError, A]
): Either[DatasetError, Option[A]] =
  value match
    case None => Right(None)
    case Some(text) => make(text).map(Some(_))

final case class LnaSubjectLabel private (value: String)

object LnaSubjectLabel:
  def make(value: String): Either[DatasetError, LnaSubjectLabel] =
    val trimmed = value.trim
    val normalized = if trimmed.startsWith("sub-") then trimmed else s"sub-$trimmed"
    safeLnaLabel(normalized, "subject").map(LnaSubjectLabel(_))

  def unsafe(value: String): LnaSubjectLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaSessionLabel private (value: String):
  def bare: String =
    value.stripPrefix("ses-")

object LnaSessionLabel:
  def make(value: String): Either[DatasetError, LnaSessionLabel] =
    val bare = value.trim.stripPrefix("ses-")
    safeLnaLabel(bare, "session").map(valid => LnaSessionLabel(s"ses-$valid"))

  def unsafe(value: String): LnaSessionLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaTaskLabel private (value: String)

object LnaTaskLabel:
  def make(value: String): Either[DatasetError, LnaTaskLabel] =
    safeLnaLabel(value, "task").map(LnaTaskLabel(_))

  def unsafe(value: String): LnaTaskLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaSpaceLabel private (value: String)

object LnaSpaceLabel:
  def make(value: String): Either[DatasetError, LnaSpaceLabel] =
    safeLnaLabel(value, "space").map(LnaSpaceLabel(_))

  def unsafe(value: String): LnaSpaceLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaDatasetQuery(
    subject: LnaSubjectLabel,
    session: Option[LnaSessionLabel],
    task: Option[LnaTaskLabel],
    space: Option[LnaSpaceLabel]
)

object LnaDatasetQuery:
  def apply(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): LnaDatasetQuery =
    unsafe(subject, session, task, space)

  def fromStrings(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): Either[DatasetError, LnaDatasetQuery] =
    for
      subjectLabel <- LnaSubjectLabel.make(subject)
      sessionLabel <- traverseOptional(session, LnaSessionLabel.make)
      taskLabel <- traverseOptional(task, LnaTaskLabel.make)
      spaceLabel <- traverseOptional(space, LnaSpaceLabel.make)
    yield LnaDatasetQuery(subjectLabel, sessionLabel, taskLabel, spaceLabel)

  def unsafe(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): LnaDatasetQuery =
    fromStrings(subject, session, task, space)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromLabels(
      subject: LnaSubjectLabel,
      session: Option[LnaSessionLabel] = None,
      task: Option[LnaTaskLabel] = None,
      space: Option[LnaSpaceLabel] = None
  ): LnaDatasetQuery =
    LnaDatasetQuery(subject, session, task, space)

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
    findLnaFiles(
      subjectLabel = query.subject.value,
      sessionLabel = query.session.map(_.value),
      task = query.task.map(_.value),
      space = query.space.map(_.value)
    )

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
          if LatentArchiveCodec.isBoldZipArchive(archive) then
            LatentArchiveCodec
              .fromBoldZipArchive(archive, run)
              .left
              .map(err => ArchiveError.InvalidArchive(err.message))
              .map { response =>
                LatentResponseDatasetBackend(id, response, runInfo.shape.space, metadataFor(normalized))
              }
          else if LatentArchiveCodec.isTransportArchive(archive) then
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
              .fromExplicitArchive(archive, run)
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
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject.value}'"))
      case Vector(path) =>
        backendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)))
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject.value}': ${many.map(root.relativize).mkString(", ")}"))
    }

  def readSubjectLatent(
      query: LnaDatasetQuery,
      run: RunLabel = RunLabel.indexed(0)
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    findLnaFiles(query).flatMap {
      case Vector() =>
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject.value}'"))
      case Vector(path) =>
        latentBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)), run)
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject.value}': ${many.map(root.relativize).mkString(", ")}"))
    }

  def readSubjectMaterialized(query: LnaDatasetQuery): Either[ArchiveError, InMemoryDatasetBackend] =
    findLnaFiles(query).flatMap {
      case Vector() =>
        Left(ArchiveError.InvalidArchive(s"no LNA files found for subject '${query.subject.value}'"))
      case Vector(path) =>
        materializedBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)))
      case many =>
        Left(ArchiveError.InvalidArchive(s"multiple LNA files match query for subject '${query.subject.value}': ${many.map(root.relativize).mkString(", ")}"))
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
        responseArchive <- LatentArchiveCodec.fromSharedBasisArchive(valid, runInfo.label)
        basisPath <- LnaSharedBasisResolver.resolve(
          responseArchive.basis,
          SharedBasisResolutionContext(archivePath = archivePath, datasetRoot = Some(root))
        )
        artifact <- JhdfSharedBasisStore.read(basisPath).flatMap(SharedBasisArtifact.validateFinite)
        response <- responseArchive
          .materialize(artifact, Some(runInfo.shape.space))
          .left
          .map(err => ArchiveError.InvalidArchive(err.message))
        mask <- responseArchive
          .sampleMask(runInfo.shape.space, artifact)
          .left
          .map(err => ArchiveError.InvalidArchive(err.message))
      yield LatentResponseDatasetBackend(
        id = id,
        response = response,
        space = runInfo.shape.space,
        mask = mask,
        metadata = metadata
      )
    }

  private def hasSharedBasisEmbed(archive: LnaArchive): Boolean =
    archive.manifest.transforms.exists(_.params.isInstanceOf[TransformParams.SharedBasisEmbed])

  private def resolveInsideRoot(path: Path): Path =
    if path.isAbsolute then path.toAbsolutePath.normalize()
    else root.resolve(path).toAbsolutePath.normalize()

  private def readString(path: Path): Either[ArchiveError, String] =
    try Right(Files.readString(path))
    catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not read ${path.toAbsolutePath}: ${e.getMessage}"))

object LnaDataset:
  def open(root: Path): Either[ArchiveError, LnaDataset] =
    val normalized = root.toAbsolutePath.normalize()
    if !Files.isDirectory(normalized) then
      Left(ArchiveError.InvalidPath(root.toString, "LNA dataset root does not exist or is not a directory"))
    else Right(LnaDataset(normalized))

  def unsafe(root: Path): LnaDataset =
    open(root).fold(err => throw IllegalArgumentException(err.message), identity)

  private[io] def datasetIdFromPath(root: Path, path: Path): String =
    root
      .relativize(path.toAbsolutePath.normalize())
      .toString
      .replace('\\', '/')
      .stripSuffix(".lna.h5")
