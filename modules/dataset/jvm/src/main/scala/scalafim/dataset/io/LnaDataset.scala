package scalafim.dataset.io

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.io.{LnaHdf5Store, LnaSharedBasisResolver, SharedBasisResolutionContext}
import scalafim.archive.lna.{LnaArchive, SharedBasisRegistry, SharedBasisRegistryCodec}
import scalafim.bids.{BidsJson, BidsTable, JsonValue}
import scalafim.dataset.{DatasetId, DatasetMetadata, InMemoryDatasetBackend, LatentArchiveDatasetBackend}

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
