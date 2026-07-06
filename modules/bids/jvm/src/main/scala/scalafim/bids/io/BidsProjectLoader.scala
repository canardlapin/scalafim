package scalafim.bids.io

import scalafim.bids.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

enum DerivativeMode:
  case Auto
  case None

final case class BidsLoadConfig(
    strictParticipants: Boolean = true,
    derivatives: DerivativeMode = DerivativeMode.Auto
)

final case class BidsConfoundSelectionFile(context: BidsTableContext, selection: ConfoundSelection):
  def path: BidsPath = context.path
  def subject: Option[String] = context.subject
  def session: Option[String] = context.session
  def task: Option[String] = context.task
  def run: Option[String] = context.run
  def table: BidsTable = selection.table
  def requested: Vector[String] = selection.requested
  def resolved: Vector[String] = selection.resolved
  def diagnostics: Vector[ConfoundDiagnostic] = selection.diagnostics
  def pca: Option[ConfoundPca] = selection.pca

object BidsProjectLoader:
  def load(root: Path, config: BidsLoadConfig = BidsLoadConfig()): Either[BidsError, BidsProject] =
    val rootAbs = root.toAbsolutePath.normalize()
    if !Files.isDirectory(rootAbs) then
      Left(BidsError.Io(rootAbs.toString, "directory does not exist"))
    else
      val derivatives = discoverDerivatives(rootAbs, config.derivatives)
      for
        description <- readDatasetDescription(rootAbs)
        participantsTable <- loadParticipantsTable(rootAbs, config.strictParticipants)
        participants <- readParticipants(rootAbs, participantsTable, derivatives)
        relPaths <- listFiles(rootAbs)
        sidecars <- readSidecars(rootAbs, relPaths)
      yield
        val rootPath = BidsPath(rootAbs.toString.replace('\\', '/'))
        BidsProject(
          root = rootPath,
          description = description.map(_.copy(parentDirectory = Some(rootPath))),
          participants = participants,
          participantsTable = participantsTable,
          derivatives = derivatives,
          manifest = BidsManifest.fromRelativePaths(relPaths, derivatives),
          sidecars = sidecars
        )

  def readTable(project: BidsProject, path: BidsPath): Either[BidsError, BidsTable] =
    resolveProjectPath(project, path)
      .flatMap(readString)
      .flatMap(BidsTable.parse)

  def readParticipantsTable(project: BidsProject): Either[BidsError, Option[BidsTable]] =
    project.participantsTable match
      case some @ Some(_) => Right(some)
      case None =>
        resolveProjectPath(project, BidsPath("participants.tsv")).flatMap { abs =>
          if Files.isRegularFile(abs) then readTable(project, BidsPath("participants.tsv")).map(Some(_))
          else Right(None)
        }

  def readEventTables(project: BidsProject): Either[BidsError, Vector[(BidsPath, BidsTable)]] =
    readEventTableFiles(project).map(_.map(file => file.path -> file.table))

  def readEventTableFiles(project: BidsProject): Either[BidsError, Vector[BidsTableFile]] =
    BidsEither.traverse(project.eventFiles()) { file =>
      resolveProjectPath(project, file.path)
        .flatMap(readString)
        .flatMap(BidsEvents.readTable)
        .map(table => BidsTableFile.from(file, table))
    }

  def readConfoundTables(
      project: BidsProject,
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Vector[(BidsPath, BidsTable)]] =
    readConfoundTableFiles(project, subid = subid, task = task, run = run, session = session, pipeline = pipeline)
      .map(_.map(file => file.path -> file.table))

  def readConfounds(
      project: BidsProject,
      cvars: Either[BidsError, Vector[String]] = Right(ConfoundSets.legacyDefault),
      naAction: NaAction = NaAction.Leave,
      clean: Vector[ConfoundClean] = Vector(ConfoundClean.ZeroVariance),
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Vector[BidsConfoundSelectionFile]] =
    cvars.flatMap { variables =>
      selectConfoundTableFiles(project, subid = subid, task = task, run = run, session = session, pipeline = pipeline) { table =>
        ConfoundSelector.select(
          table,
          ConfoundSelectionConfig(variables = variables, naAction = naAction, clean = clean)
        )
      }
    }

  def readConfoundStrategy(
      project: BidsProject,
      strategy: Either[BidsError, ConfoundStrategy],
      naAction: NaAction = NaAction.Leave,
      clean: Vector[ConfoundClean] = Vector(ConfoundClean.ZeroVariance),
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Vector[BidsConfoundSelectionFile]] =
    strategy.flatMap { resolved =>
      selectConfoundTableFiles(project, subid = subid, task = task, run = run, session = session, pipeline = pipeline) { table =>
        ConfoundSelector.selectStrategy(table, resolved, naAction = naAction, clean = clean)
      }
    }

  def readConfoundTableFiles(
      project: BidsProject,
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Vector[BidsTableFile]] =
    BidsEither.traverse(project.confoundFiles(subid = subid, task = task, run = run, session = session, pipeline = pipeline)) { file =>
      resolveProjectPath(project, file.path)
        .flatMap(readString)
        .flatMap(BidsTable.parse)
        .map(table => BidsTableFile.from(file, table))
    }

  private def selectConfoundTableFiles(
      project: BidsProject,
      subid: String,
      task: String,
      run: String,
      session: String,
      pipeline: Option[PipelineName]
  )(select: BidsTable => Either[BidsError, ConfoundSelection]): Either[BidsError, Vector[BidsConfoundSelectionFile]] =
    readConfoundTableFiles(project, subid = subid, task = task, run = run, session = session, pipeline = pipeline)
      .flatMap { files =>
        BidsEither.traverse(files) { file =>
          select(file.table).map(selection => BidsConfoundSelectionFile(file.context, selection))
        }
      }

  private def readDatasetDescription(root: Path): Either[BidsError, Option[DatasetDescription]] =
    val path = root.resolve("dataset_description.json")
    if !Files.isRegularFile(path) then Right(None)
    else
      readString(path)
        .flatMap(BidsJson.parseObject)
        .map(json => Some(DatasetDescription.fromJson(json, parentDirectory = Some(BidsPath(root.toString.replace('\\', '/'))))))

  private def readParticipants(
      root: Path,
      participantsTable: Option[BidsTable],
      derivatives: Vector[DerivativeRoot]
  ): Either[BidsError, Vector[String]] =
    participantsTable match
      case Some(table) => participantIds(table)
      case None        => Right(inferParticipants(root, derivatives).map(_.stripPrefix("sub-")).distinct.sorted)

  private def loadParticipantsTable(root: Path, strictParticipants: Boolean): Either[BidsError, Option[BidsTable]] =
    val path = root.resolve("participants.tsv")
    if Files.isRegularFile(path) then
      readString(path)
        .flatMap(BidsTable.parse)
        .map(Some(_))
    else if strictParticipants then Left(BidsError.MissingParticipants(path.toString))
    else Right(None)

  private def participantIds(table: BidsTable): Either[BidsError, Vector[String]] =
    table
      .columnNamed("participant_id")
      .map(_.values.flatten.map(_.stripPrefix("sub-")).filter(_.nonEmpty).distinct.sorted)

  private def discoverDerivatives(root: Path, mode: DerivativeMode): Vector[DerivativeRoot] =
    mode match
      case DerivativeMode.None => Vector.empty
      case DerivativeMode.Auto =>
        val derivRoot = root.resolve("derivatives")
        if !Files.isDirectory(derivRoot) then Vector.empty
        else
          val pipelineRoots =
            childDirs(derivRoot)
              .filterNot(path => path.getFileName.toString.startsWith("sub-"))
              .map { path =>
                val name = path.getFileName.toString
                DerivativeRoot(BidsPath(s"derivatives/$name"), PipelineName(name))
              }

          val directDerivative =
            if childDirs(derivRoot).exists(_.getFileName.toString.startsWith("sub-")) &&
              Files.isRegularFile(derivRoot.resolve("dataset_description.json"))
            then Vector(DerivativeRoot(BidsPath("derivatives"), PipelineName("derivatives")))
            else Vector.empty

          (directDerivative ++ pipelineRoots).distinct

  private def inferParticipants(root: Path, derivatives: Vector[DerivativeRoot]): Vector[String] =
    val rawSubjects = childDirs(root).map(_.getFileName.toString).filter(_.startsWith("sub-"))
    val derivativeSubjects =
      derivatives.flatMap { d =>
        val droot = root.resolve(d.root.value)
        childDirs(droot).map(_.getFileName.toString).filter(_.startsWith("sub-"))
      }
    (rawSubjects ++ derivativeSubjects).distinct.sorted

  private def listFiles(root: Path): Either[BidsError, Vector[String]] =
    try
      val stream = Files.walk(root)
      try
        Right(
          stream
            .iterator()
            .asScala
            .filter(Files.isRegularFile(_))
            .map(path => root.relativize(path).toString.replace('\\', '/'))
            .toVector
            .sorted
        )
      finally stream.close()
    catch case e: Exception => Left(BidsError.Io(root.toString, e.getMessage))

  private def readSidecars(root: Path, relPaths: Vector[String]): Either[BidsError, Map[BidsPath, JsonValue.Obj]] =
    val jsonPaths =
      relPaths.filter(path => path.endsWith(".json") && !path.endsWith("dataset_description.json"))
    BidsEither
      .traverse(jsonPaths) { rel =>
        val abs = root.resolve(rel)
        for
          path <- BidsPath.relative(rel)
          json <- readString(abs).flatMap(BidsJson.parseObject)
        yield path -> json
      }
      .map(_.toMap)

  private def readString(path: Path): Either[BidsError, String] =
    try Right(Files.readString(path, StandardCharsets.UTF_8))
    catch case e: Exception => Left(BidsError.Io(path.toString, e.getMessage))

  private def resolveProjectPath(project: BidsProject, path: BidsPath): Either[BidsError, Path] =
    BidsPath.relative(path.value).flatMap { rel =>
      val root = Path.of(project.root.value).toAbsolutePath.normalize()
      val resolved = root.resolve(rel.value).normalize()
      if resolved.startsWith(root) then Right(resolved)
      else Left(BidsError.InvalidPath(path.value, s"path escapes project root '${project.root.value}'"))
    }

  private def childDirs(path: Path): Vector[Path] =
    if !Files.isDirectory(path) then Vector.empty
    else
      val stream = Files.list(path)
      try stream.iterator().asScala.filter(Files.isDirectory(_)).toVector.sortBy(_.getFileName.toString)
      finally stream.close()
