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

enum BidsProjectLoadError:
  case Operation(error: BidsError)
  case Validation(report: BidsIssueReport)

  def message: String =
    this match
      case Operation(error) => error.message
      case Validation(report) =>
        report.issues.map { issue =>
          val location = issue.path.map(path => s" at '${path.value}'").getOrElse("")
          val field = issue.field.map(name => s" field '$name'").getOrElse("")
          s"${issue.severity} ${issue.code}$location$field: ${issue.message}"
        }.mkString("; ")

private[io] final case class BidsCheckedValue[+A](value: A, issues: Vector[BidsIssue])

private[io] final case class BidsCheckedParticipants(
    participants: Vector[String],
    table: Option[BidsTable],
    issues: Vector[BidsIssue]
)

private[io] object BidsCheckedContent:
  private val ParticipantsPath = BidsPath("participants.tsv")

  def datasetDescription(
      path: BidsPath,
      text: String,
      parentDirectory: BidsPath
  ): BidsCheckedValue[Option[DatasetDescription]] =
    BidsJson.parseObject(text) match
      case Left(error) =>
        BidsCheckedValue(
          None,
          Vector(BidsIssue.error(BidsIssueCode.InvalidSidecar, Some(path), None, error.message))
        )
      case Right(json) =>
        BidsCheckedValue(
          Some(DatasetDescription.fromJson(json, parentDirectory = Some(parentDirectory))),
          BidsMetadataValidation.datasetDescription(path, json)
        )

  def participants(
      text: Option[String],
      relativePaths: Vector[String],
      strict: Boolean
  ): BidsCheckedParticipants =
    text match
      case None =>
        val issues =
          if strict then
            Vector(BidsIssue.error(
              BidsIssueCode.MissingRequiredField,
              Some(ParticipantsPath),
              Some("participant_id"),
              "participants.tsv is required by the load configuration"
            ))
          else Vector.empty
        BidsCheckedParticipants(inferParticipants(relativePaths), None, issues)
      case Some(value) =>
        BidsTable.parseChecked(value) match
          case Left(report) =>
            BidsCheckedParticipants(
              inferParticipants(relativePaths),
              None,
              report.issues.map(issueAt(ParticipantsPath, _))
            )
          case Right(table) =>
            val idIndex = table.columns.indexOf("participant_id")
            if idIndex < 0 then
              BidsCheckedParticipants(
                inferParticipants(relativePaths),
                Some(table),
                Vector(BidsIssue.error(
                  BidsIssueCode.MissingRequiredField,
                  Some(ParticipantsPath),
                  Some("participant_id"),
                  "participants table has no participant_id column"
                ))
              )
            else
              val values = table.rows.map(_(idIndex)).zipWithIndex
              val issues = Vector.newBuilder[BidsIssue]
              values.collect { case (None, index) => index }.foreach { index =>
                issues += BidsIssue.error(
                  BidsIssueCode.InvalidTable,
                  Some(ParticipantsPath),
                  Some(s"rows[$index].participant_id"),
                  "participant_id must be non-empty"
                )
              }
              val participants = values.flatMap(_._1).map(_.stripPrefix("sub-")).filter(_.nonEmpty)
              participants.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy(_._1).foreach {
                case (participant, count) if count > 1 =>
                  issues += BidsIssue.error(
                    BidsIssueCode.InconsistentMetadata,
                    Some(ParticipantsPath),
                    Some("participant_id"),
                    s"participant sub-$participant appears $count times"
                  )
                case _ => ()
              }
              BidsCheckedParticipants(participants.distinct.sorted, Some(table), issues.result())

  def sidecar(path: BidsPath, text: String): BidsCheckedValue[Option[(BidsPath, JsonValue.Obj)]] =
    BidsJson.parseObject(text) match
      case Left(error) =>
        BidsCheckedValue(
          None,
          Vector(BidsIssue.error(BidsIssueCode.InvalidSidecar, Some(path), None, error.message))
        )
      case Right(json) => BidsCheckedValue(Some(path -> json), Vector.empty)

  def resolvedMetadata(project: BidsProject): Vector[BidsIssue] =
    project.manifest.files
      .filter(file => file.extension != "json" && file.parsed.exists(_.kind == "bold"))
      .sortBy(_.path.value)
      .flatMap { file =>
        project.metadata(file.path).toOption.toVector.flatMap { metadata =>
          BidsMetadataValidation.resolvedSidecar(file.path, metadata)
        }
      }

  private def issueAt(path: BidsPath, issue: BidsIssue): BidsIssue =
    issue.severity match
      case BidsIssueSeverity.Error =>
        BidsIssue.error(issue.code, Some(path), issue.field, issue.message)
      case BidsIssueSeverity.Warning =>
        BidsIssue.warning(issue.code, Some(path), issue.field, issue.message)

  private def inferParticipants(relativePaths: Vector[String]): Vector[String] =
    relativePaths
      .flatMap { path =>
        path.split('/').toVector.dropRight(1).find(_.startsWith("sub-"))
      }
      .map(_.stripPrefix("sub-"))
      .distinct
      .sorted

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

  def loadStrict(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig()
  ): Either[BidsProjectLoadError, BidsProject] =
    loadChecked(root, config)
      .left
      .map(BidsProjectLoadError.Operation.apply)
      .flatMap(
        _.enforce(BidsValidationPolicy.Strict)
          .left
          .map(BidsProjectLoadError.Validation.apply)
          .map(_.value)
      )

  def loadChecked(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig()
  ): Either[BidsError, BidsValidationReport[BidsProject]] =
    val rootAbs = root.toAbsolutePath.normalize()
    if !Files.isDirectory(rootAbs) then
      Left(BidsError.Io(rootAbs.toString, "directory does not exist"))
    else
      val derivatives = discoverDerivatives(rootAbs, config.derivatives)
      for
        relPaths <- listFiles(rootAbs)
        rootPath = BidsPath(rootAbs.toString.replace('\\', '/'))
        descriptions <- readDatasetDescriptionsChecked(rootAbs, rootPath, relPaths)
        participantsText <- readOptionalString(rootAbs.resolve("participants.tsv"))
        participants = BidsCheckedContent.participants(participantsText, relPaths, config.strictParticipants)
        sidecars <- readSidecarsChecked(rootAbs, relPaths)
      yield
        val manifest = BidsManifest.fromRelativePathsChecked(relPaths, derivatives)
        val project = BidsProject(
          root = rootPath,
          description = descriptions.value,
          participants = participants.participants,
          participantsTable = participants.table,
          derivatives = derivatives,
          manifest = manifest.value,
          sidecars = sidecars.value
        )
        BidsValidationReport.from(
          project,
          manifest.issues ++ descriptions.issues ++ participants.issues ++ sidecars.issues ++
            BidsCheckedContent.resolvedMetadata(project)
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

  def readValidatedEventTables(project: BidsProject): Either[BidsError, Vector[(BidsPath, EventsTable)]] =
    readValidatedEventTableFiles(project).map(_.map(file => file.path -> file.events))

  def readValidatedEventTableFiles(project: BidsProject): Either[BidsError, Vector[BidsEventTableFile]] =
    BidsEither.traverse(project.eventFiles()) { file =>
      resolveProjectPath(project, file.path)
        .flatMap(readString)
        .flatMap { text =>
          BidsEvents
            .readEventsTable(text)
            .left
            .map(error => BidsError.InvalidTable(s"event file '${file.path.value}': ${error.message}"))
        }
        .map(events => BidsEventTableFile.from(file, events))
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

  private def readDatasetDescriptionsChecked(
      root: Path,
      rootPath: BidsPath,
      relPaths: Vector[String]
  ): Either[BidsError, BidsCheckedValue[Option[DatasetDescription]]] =
    val descriptionPaths = relPaths.filter(_.endsWith("dataset_description.json")).sorted
    BidsEither
      .traverse(descriptionPaths) { rel =>
        for
          path <- BidsPath.relative(rel)
          text <- readString(root.resolve(rel))
        yield path -> BidsCheckedContent.datasetDescription(path, text, rootPath)
      }
      .map { descriptions =>
        val rootDescription =
          descriptions.collectFirst {
            case (path, checked) if path.value == "dataset_description.json" => checked.value
          }.flatten
        val missingRoot =
          Option.when(!descriptionPaths.contains("dataset_description.json"))(
            BidsIssue.error(
              BidsIssueCode.MissingRequiredField,
              Some(BidsPath("dataset_description.json")),
              Some("dataset_description.json"),
              "dataset_description.json is missing"
            )
          ).toVector
        BidsCheckedValue(
          rootDescription,
          descriptions.flatMap(_._2.issues) ++ missingRoot
        )
      }

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

  private def readSidecarsChecked(
      root: Path,
      relPaths: Vector[String]
  ): Either[BidsError, BidsCheckedValue[Map[BidsPath, JsonValue.Obj]]] =
    val jsonPaths =
      relPaths.filter(path => path.endsWith(".json") && !path.endsWith("dataset_description.json"))
    BidsEither
      .traverse(jsonPaths) { rel =>
        for
          path <- BidsPath.relative(rel)
          text <- readString(root.resolve(rel))
        yield BidsCheckedContent.sidecar(path, text)
      }
      .map { sidecars =>
        BidsCheckedValue(
          sidecars.flatMap(_.value).toMap,
          sidecars.flatMap(_.issues)
        )
      }

  private def readOptionalString(path: Path): Either[BidsError, Option[String]] =
    if Files.isRegularFile(path) then readString(path).map(Some(_))
    else Right(None)

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
