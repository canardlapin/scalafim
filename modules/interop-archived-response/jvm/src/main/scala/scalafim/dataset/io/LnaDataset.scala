package scalafim.dataset.io

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.io.{JhdfSharedBasisStore, LnaHdf5Store, LnaSharedBasisResolver, SharedBasisResolutionContext}
import scalafim.archive.lna.{
  LnaArchive,
  LnaRun,
  SharedBasisArtifact,
  SharedBasisRegistry,
  SharedBasisRegistryCodec
}
import scalafim.bids.{BidsJson, BidsTable, JsonValue}
import scalafim.dataset.{
  DatasetError,
  DatasetId,
  DatasetMetadata,
  InMemoryDatasetBackend,
  LatentArchiveDatasetBackend,
  LatentResponseDatasetBackend,
  RunId
}
import scalafim.latent.{LegacyLatentArchiveCodec, LatentArchivePlan, SharedBasisLatentArchive}

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

extension (label: LnaSubjectLabel)
  def bare: String =
    label.value.stripPrefix("sub-")

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

final case class LnaRunEntityLabel private (value: String)

object LnaRunEntityLabel:
  def make(value: String): Either[DatasetError, LnaRunEntityLabel] =
    safeLnaLabel(value.trim.stripPrefix("run-"), "run").map(LnaRunEntityLabel(_))

  def unsafe(value: String): LnaRunEntityLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaAcqLabel private (value: String)

object LnaAcqLabel:
  def make(value: String): Either[DatasetError, LnaAcqLabel] =
    safeLnaLabel(value.trim.stripPrefix("acq-"), "acq").map(LnaAcqLabel(_))

  def unsafe(value: String): LnaAcqLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaDescLabel private (value: String)

object LnaDescLabel:
  def make(value: String): Either[DatasetError, LnaDescLabel] =
    safeLnaLabel(value.trim.stripPrefix("desc-"), "desc").map(LnaDescLabel(_))

  def unsafe(value: String): LnaDescLabel =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class LnaDatasetQuery(
    subject: LnaSubjectLabel,
    session: Option[LnaSessionLabel],
    task: Option[LnaTaskLabel],
    space: Option[LnaSpaceLabel],
    run: Option[LnaRunEntityLabel],
    acq: Option[LnaAcqLabel],
    desc: Option[LnaDescLabel]
):
  def label: String =
    val parts =
      Vector(
        Some(s"subject=${subject.value}"),
        session.map(value => s"session=${value.value}"),
        task.map(value => s"task=${value.value}"),
        space.map(value => s"space=${value.value}"),
        run.map(value => s"run=${value.value}"),
        acq.map(value => s"acq=${value.value}"),
        desc.map(value => s"desc=${value.value}")
      ).flatten
    parts.mkString(", ")

object LnaDatasetQuery:
  def apply(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None,
      run: Option[String] = None,
      acq: Option[String] = None,
      desc: Option[String] = None
  ): LnaDatasetQuery =
    unsafe(subject, session, task, space, run, acq, desc)

  def fromStrings(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None,
      run: Option[String] = None,
      acq: Option[String] = None,
      desc: Option[String] = None
  ): Either[DatasetError, LnaDatasetQuery] =
    for
      subjectLabel <- LnaSubjectLabel.make(subject)
      sessionLabel <- traverseOptional(session, LnaSessionLabel.make)
      taskLabel <- traverseOptional(task, LnaTaskLabel.make)
      spaceLabel <- traverseOptional(space, LnaSpaceLabel.make)
      runLabel <- traverseOptional(run, LnaRunEntityLabel.make)
      acqLabel <- traverseOptional(acq, LnaAcqLabel.make)
      descLabel <- traverseOptional(desc, LnaDescLabel.make)
    yield LnaDatasetQuery(subjectLabel, sessionLabel, taskLabel, spaceLabel, runLabel, acqLabel, descLabel)

  def unsafe(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None,
      run: Option[String] = None,
      acq: Option[String] = None,
      desc: Option[String] = None
  ): LnaDatasetQuery =
    fromStrings(subject, session, task, space, run, acq, desc)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromLabels(
      subject: LnaSubjectLabel,
      session: Option[LnaSessionLabel] = None,
      task: Option[LnaTaskLabel] = None,
      space: Option[LnaSpaceLabel] = None,
      run: Option[LnaRunEntityLabel] = None,
      acq: Option[LnaAcqLabel] = None,
      desc: Option[LnaDescLabel] = None
  ): LnaDatasetQuery =
    LnaDatasetQuery(subject, session, task, space, run, acq, desc)

enum LnaDatasetLookupError:
  case ScanFailed(error: ArchiveError)
  case NoMatches(query: LnaDatasetQuery)
  case Ambiguous(query: LnaDatasetQuery, matches: Vector[String])

  def message: String =
    this match
      case ScanFailed(error) =>
        error.message
      case NoMatches(query) =>
        s"no LNA files found for query ${query.label}"
      case Ambiguous(query, matches) =>
        s"multiple LNA files match query ${query.label}: ${matches.mkString(", ")}"

  def toArchiveError: ArchiveError =
    this match
      case ScanFailed(error) => error
      case other             => ArchiveError.InvalidArchive(other.message)

private final case class LnaFileEntities(
    subject: Option[String],
    session: Option[String],
    task: Option[String],
    space: Option[String],
    run: Option[String],
    acq: Option[String],
    desc: Option[String],
    suffix: Option[String]
):
  def matches(query: LnaDatasetQuery): Boolean =
    subject.contains(query.subject.bare) &&
      query.session.forall(value => session.contains(value.bare)) &&
      query.task.forall(value => task.contains(value.value)) &&
      query.space.forall(value => space.contains(value.value)) &&
      query.run.forall(value => run.contains(value.value)) &&
      query.acq.forall(value => acq.contains(value.value)) &&
      query.desc.forall(value => desc.contains(value.value))

private object LnaFileEntities:
  private val Extension = ".lna.h5"

  def parse(path: Path): Either[DatasetError, LnaFileEntities] =
    val fileName = path.getFileName.toString
    if !fileName.endsWith(Extension) then
      Left(DatasetError.InvalidLabel("LNA filename", fileName, s"must end with $Extension"))
    else
      val stem = fileName.substring(0, fileName.length - Extension.length)
      val parts = stem.split("_").toVector.filter(_.nonEmpty)
      if parts.isEmpty then Left(DatasetError.InvalidLabel("LNA filename", fileName, "must contain BIDS-like entities"))
      else
        val entities = scala.collection.mutable.Map.empty[String, String]
        var suffix = Option.empty[String]
        var index = 0
        var error = Option.empty[DatasetError]
        while index < parts.length && error.isEmpty do
          val part = parts(index)
          val dash = part.indexOf('-')
          if dash > 0 then
            val key = part.substring(0, dash)
            val value = part.substring(dash + 1)
            if value.isEmpty then error = Some(DatasetError.InvalidLabel("LNA filename", fileName, s"empty value for entity '$key'"))
            else if entities.contains(key) then error = Some(DatasetError.InvalidLabel("LNA filename", fileName, s"duplicate entity '$key'"))
            else entities.update(key, value)
          else
            if suffix.isDefined then error = Some(DatasetError.InvalidLabel("LNA filename", fileName, "contains multiple suffix components"))
            else suffix = Some(part)
          index += 1

        error match
          case Some(err) => Left(err)
          case None =>
            Right(
              LnaFileEntities(
                subject = entities.get("sub"),
                session = entities.get("ses"),
                task = entities.get("task"),
                space = entities.get("space"),
                run = entities.get("run"),
                acq = entities.get("acq"),
                desc = entities.get("desc"),
                suffix = suffix
              )
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
    findLnaFilesForQuery(query)

  def resolveLnaFile(query: LnaDatasetQuery): Either[LnaDatasetLookupError, Path] =
    findLnaFiles(query)
      .left
      .map(LnaDatasetLookupError.ScanFailed(_))
      .flatMap {
        case Vector() =>
          Left(LnaDatasetLookupError.NoMatches(query))
        case Vector(path) =>
          Right(path)
        case many =>
          Left(LnaDatasetLookupError.Ambiguous(query, many.map(path => root.relativize(path).toString)))
      }

  def readArchive(path: Path): Either[ArchiveError, LnaArchive] =
    val normalized = resolveInsideRoot(path)
    if !normalized.startsWith(root) then
      Left(ArchiveError.InvalidPath(path.toString, "LNA archive path must stay inside the dataset root"))
    else LnaHdf5Store.default.read(normalized)

  def backendFor(
      path: Path,
      id: DatasetId,
      run: RunLabel
  ): Either[ArchiveError, LatentArchiveDatasetBackend] =
    val normalized = resolveInsideRoot(path)
    readArchive(path).flatMap { archive =>
      LatentArchiveDatasetBackend
        .make(
          id = id,
          archive = archive,
          run = run,
          metadata = metadataFor(normalized)
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
    }

  def backendFor(path: Path, id: DatasetId): Either[ArchiveError, LatentArchiveDatasetBackend] =
    readArchive(path).flatMap: archive =>
      uniqueArchiveRun(archive).flatMap(run => backendFor(path, id, run))

  def latentBackendFor(
      path: Path,
      id: DatasetId,
      run: RunLabel
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    val normalized = resolveInsideRoot(path)
    readArchive(path).flatMap { archive =>
      for
        plan <- LegacyLatentArchiveCodec.openPlan(archive, run)
        backend <- latentBackendFromPlan(plan, normalized, id, metadataFor(normalized))
      yield backend
    }

  def latentBackendFor(
      path: Path,
      id: DatasetId
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    readArchive(path).flatMap: archive =>
      uniqueArchiveRun(archive).flatMap(run => latentBackendFor(path, id, run))

  def materializedBackendFor(
      path: Path,
      id: DatasetId,
      run: RunLabel
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

  def materializedBackendFor(
      path: Path,
      id: DatasetId
  ): Either[ArchiveError, InMemoryDatasetBackend] =
    readArchive(path).flatMap: archive =>
      uniqueArchiveRun(archive).flatMap(run => materializedBackendFor(path, id, run))

  def readSubject(query: LnaDatasetQuery): Either[ArchiveError, LatentArchiveDatasetBackend] =
    resolveLnaFile(query)
      .left
      .map(_.toArchiveError)
      .flatMap(path => backendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path))))

  def readSubjectLatent(
      query: LnaDatasetQuery,
      run: RunLabel
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    resolveLnaFile(query)
      .left
      .map(_.toArchiveError)
      .flatMap(path => latentBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path)), run))

  def readSubjectLatent(
      query: LnaDatasetQuery
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    resolveLnaFile(query)
      .left
      .map(_.toArchiveError)
      .flatMap(path => latentBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path))))

  def readSubjectMaterialized(query: LnaDatasetQuery): Either[ArchiveError, InMemoryDatasetBackend] =
    resolveLnaFile(query)
      .left
      .map(_.toArchiveError)
      .flatMap(path => materializedBackendFor(path, DatasetId(LnaDataset.datasetIdFromPath(root, path))))

  private def findLnaFilesForQuery(query: LnaDatasetQuery): Either[ArchiveError, Vector[Path]] =
    val subjectDir = root.resolve(query.subject.value).normalize()
    if !subjectDir.startsWith(root) then
      Left(ArchiveError.InvalidPath(query.subject.value, "subject path escapes dataset root"))
    else if !Files.isDirectory(subjectDir) then Right(Vector.empty)
    else
      try
        val stream = Files.walk(subjectDir)
        try
          val files = Vector.newBuilder[Path]
          val iterator = stream.iterator().asScala
          var error = Option.empty[ArchiveError]
          while iterator.hasNext && error.isEmpty do
            val path = iterator.next()
            if Files.isRegularFile(path) && path.getFileName.toString.endsWith(".lna.h5") then
              val normalized = path.toAbsolutePath.normalize()
              LnaFileEntities.parse(normalized) match
                case Left(err) =>
                  error = Some(ArchiveError.InvalidArchive(s"${root.relativize(normalized)}: ${err.message}"))
                case Right(entities) =>
                  if entities.matches(query) then files += normalized
          error match
            case Some(err) => Left(err)
            case None =>
              Right(files.result().sortBy(path => root.relativize(path).toString))
        finally stream.close()
      catch case NonFatal(e) => Left(ArchiveError.UnsupportedStorage(s"could not scan LNA files under $subjectDir: ${e.getMessage}"))

  private[io] def metadataFor(path: Path): DatasetMetadata =
    DatasetMetadata(
      Map(
        "lna.dataset_root" -> root.toString,
        "lna.archive_path" -> path.toString,
        "lna.archive_relative_path" -> root.relativize(path).toString.replace('\\', '/')
      )
    )

  private[io] def latentBackendFromPlan(
      plan: LatentArchivePlan,
      archivePath: Path,
      id: DatasetId,
      metadata: DatasetMetadata
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    plan match
      case LatentArchivePlan.SharedBasis(runInfo, _, responseArchive) =>
        sharedBasisLatentBackend(responseArchive, archivePath, id, runInfo, metadata)
      case other =>
        other.selectionResponse.flatMap { response =>
          LatentResponseDatasetBackend
            .make(id, response, other.runInfo.shape.space, metadata)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
        }

  private def sharedBasisLatentBackend(
      responseArchive: SharedBasisLatentArchive,
      archivePath: Path,
      id: DatasetId,
      runInfo: LnaRun,
      metadata: DatasetMetadata
  ): Either[ArchiveError, LatentResponseDatasetBackend] =
    for
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
      backend <- LatentResponseDatasetBackend
        .make(
          id = id,
          response = response,
          space = runInfo.shape.space,
          mask = mask,
          metadata = metadata
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
    yield backend

  private def uniqueArchiveRun(archive: LnaArchive): Either[ArchiveError, RunLabel] =
    archive.manifest.runs.map(_.label) match
      case Vector(run) =>
        Right(run)
      case runs =>
        Left(ArchiveError.InvalidArchive(
          s"archive contains ${runs.length} internal runs; select one explicitly"
        ))

  private[io] def datasetRunId(path: Path): Either[DatasetError, RunId] =
    val normalized = resolveInsideRoot(path)
    LnaFileEntities.parse(normalized).flatMap: entities =>
      entities.run match
        case Some(run) => RunId.make(s"run-$run")
        case None =>
          Left(DatasetError.InvalidLabel(
            "LNA run entity",
            root.relativize(normalized).toString,
            "filename must contain an explicit run-<label> entity"
          ))

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
