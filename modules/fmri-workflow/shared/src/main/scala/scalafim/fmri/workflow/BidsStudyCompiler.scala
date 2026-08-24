package scalafim.fmri.workflow

import bids4s.*
import scalafim.dataset.{DatasetShape, RunId, SessionId, SpaceId, SubjectId, TaskId}
import image4s.geometry.GeometryError
import image4s.geometry.Grid

final case class ImageHeaderDescriptor(shape: DatasetShape)

final case class ImageHeaderCatalog(
    headers: Map[BidsPath, ImageHeaderDescriptor],
    failures: Map[BidsPath, String] = Map.empty
):
  require(headers.keySet.intersect(failures.keySet).isEmpty, "header successes and failures must be disjoint")

  def get(path: BidsPath): Option[ImageHeaderDescriptor] =
    headers.get(path)

  def failure(path: BidsPath): Option[String] =
    failures.get(path)

enum CatalogIssueCode:
  case BidsValidation(code: BidsIssueCode)
  case NoBoldFiles
  case MissingEntity
  case InvalidIdentity
  case MissingHeader
  case MissingRepetitionTime
  case MissingEvents
  case MissingConfounds
  case MissingMask
  case AmbiguousCompanion
  case IncompatibleGeometry
  case DuplicateRun
  case InvalidUnit
  case MissingParticipant
  case DuplicateParticipant

enum CatalogIssueCause derives CanEqual:
  case Geometry(error: GeometryError)

final case class CatalogIssue(
    code: CatalogIssueCode,
    path: Option[BidsPath],
    message: String,
    severity: BidsIssueSeverity = BidsIssueSeverity.Error,
    field: Option[String] = None,
    cause: Option[CatalogIssueCause] = None
):
  require(message.trim.nonEmpty, "catalog issue message must be non-empty")

object CatalogIssue:
  def fromBids(issue: BidsIssue): CatalogIssue =
    CatalogIssue(
      code = CatalogIssueCode.BidsValidation(issue.code),
      path = issue.path,
      message = issue.message,
      severity = issue.severity,
      field = issue.field
    )

final case class CatalogCompileReport(
    matchedBoldFiles: Int,
    issues: Vector[CatalogIssue]
):
  require(matchedBoldFiles >= 0, "matched BOLD count must be non-negative")

  def errors: Vector[CatalogIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Error)

  def warnings: Vector[CatalogIssue] =
    issues.filter(_.severity == BidsIssueSeverity.Warning)

  def canCompile: Boolean =
    errors.isEmpty

final case class CatalogCompilation(
    catalog: StudyCatalog,
    issues: Vector[CatalogIssue]
):
  require(
    issues.forall(_.severity == BidsIssueSeverity.Warning),
    "successful catalog compilation may contain warnings but not errors"
  )

  def warnings: Vector[CatalogIssue] = issues

private final case class CandidateRun(
    bold: BidsFile,
    subject: String,
    session: Option[String],
    task: String,
    acquisition: Option[String],
    echo: Option[String],
    space: String,
    resolution: Option[String],
    pipeline: Option[PipelineName],
    runId: RunId,
    header: ImageHeaderDescriptor,
    repetitionTime: RepetitionTime,
    events: BidsFile,
    confounds: Option[BidsFile],
    mask: Option[BidsFile]
)

private final case class UnitKey(
    subject: String,
    session: Option[String],
    task: String,
    acquisition: Option[String],
    echo: Option[String],
    space: String,
    resolution: Option[String],
    pipeline: Option[PipelineName],
    run: Option[String]
):
  def sortKey: String =
    Vector(
      subject,
      session.getOrElse(""),
      task,
      acquisition.getOrElse(""),
      echo.getOrElse(""),
      space,
      resolution.getOrElse(""),
      pipeline.map(_.value).getOrElse(""),
      run.getOrElse("")
    ).mkString("\u0000")

  def id: Either[WorkflowError, FirstLevelUnitId] =
    val segments = Vector(
      Some(s"sub-$subject"),
      session.map(value => s"ses-$value"),
      Some(s"task-$task"),
      acquisition.map(value => s"acq-$value"),
      echo.map(value => s"echo-$value"),
      Some(s"space-$space"),
      resolution.map(value => s"res-$value"),
      pipeline.map(value => s"pipeline-${value.value}"),
      run.map(value => s"run-$value")
    ).flatten
    FirstLevelUnitId(segments.mkString("."))

object BidsStudyCompiler:
  private val EventMatchKeys =
    Vector(EntityKey.Subject, EntityKey.Session, EntityKey.Task, EntityKey.Acquisition, EntityKey.Run, EntityKey.Echo)
  private val ConfoundMatchKeys = EventMatchKeys
  private val MaskMatchKeys =
    Vector(
      EntityKey.Subject,
      EntityKey.Session,
      EntityKey.Task,
      EntityKey.Acquisition,
      EntityKey.Run,
      EntityKey.Echo,
      EntityKey.Space,
      EntityKey.Resolution
    )

  def compile(
      project: BidsProject,
      recipe: DatasetRecipe,
      imageHeaders: ImageHeaderCatalog
  ): Either[CatalogCompileReport, StudyCatalog] =
    val boldFiles = project.query(recipe.boldQuery).filter(isBoldImage)
    val eventFiles = project.manifest.files.filter(isEventFile)
    val confoundFiles = project.manifest.files.filter(isConfoundFile)
    val maskFiles = project.manifest.files.filter(isMaskFile)
    val compiled = boldFiles.map { bold =>
      compileRun(project, recipe, imageHeaders, bold, eventFiles, confoundFiles, maskFiles)
    }
    val issues = Vector.newBuilder[CatalogIssue]

    if boldFiles.isEmpty then
      issues += CatalogIssue(CatalogIssueCode.NoBoldFiles, None, "the dataset recipe matched no BOLD NIfTI files")
    compiled.foreach(result => issues ++= result._1)

    val candidates = compiled.flatMap(_._2)
    val grouped = candidates.groupBy(run => unitKey(run, recipe.runGrouping)).toVector.sortBy(_._1.sortKey)
    val units = Vector.newBuilder[FirstLevelUnit]
    grouped.foreach { case (key, runs) =>
      compileUnit(recipe, imageHeaders, key, runs.sortBy(run => run.runId.value)) match
        case Left(unitIssues) => issues ++= unitIssues
        case Right(unit) => units += unit
    }

    val builtUnits = units.result()
    val participantResult = compileParticipants(project, builtUnits.map(_.subject.value).distinct.sorted)
    issues ++= participantResult._1
    val allIssues = issues.result()
    val report = CatalogCompileReport(boldFiles.length, allIssues)
    if !report.canCompile then Left(report)
    else
      StudyCatalog.make(recipe.datasetId, builtUnits, participantResult._2) match
        case Right(catalog) => Right(catalog)
        case Left(error) =>
          Left(CatalogCompileReport(
            boldFiles.length,
            Vector(CatalogIssue(CatalogIssueCode.InvalidUnit, None, error.message))
          ))

  def compileChecked(
      project: BidsValidationReport[BidsProject],
      recipe: DatasetRecipe,
      imageHeaders: ImageHeaderCatalog
  ): Either[CatalogCompileReport, CatalogCompilation] =
    val bidsIssues = project.issues.map(CatalogIssue.fromBids)
    compile(project.value, recipe, imageHeaders) match
      case Left(report) =>
        Left(report.copy(issues = bidsIssues ++ report.issues))
      case Right(catalog) =>
        val errors = bidsIssues.filter(_.severity == BidsIssueSeverity.Error)
        if errors.nonEmpty then
          val matchedBoldFiles = project.value.query(recipe.boldQuery).count(isBoldImage)
          Left(CatalogCompileReport(matchedBoldFiles, bidsIssues))
        else Right(CatalogCompilation(catalog, bidsIssues))

  private def compileRun(
      project: BidsProject,
      recipe: DatasetRecipe,
      imageHeaders: ImageHeaderCatalog,
      bold: BidsFile,
      eventFiles: Vector[BidsFile],
      confoundFiles: Vector[BidsFile],
      maskFiles: Vector[BidsFile]
  ): (Vector[CatalogIssue], Option[CandidateRun]) =
    val issues = Vector.newBuilder[CatalogIssue]
    val subject = requiredEntity(bold, EntityKey.Subject, issues)
    val task = requiredEntity(bold, EntityKey.Task, issues)
    val space =
      bold.entities.get(EntityKey.Space).orElse {
        if bold.scope == BidsScope.Raw then Some("native") else None
      }
    if space.isEmpty then
      issues += CatalogIssue(CatalogIssueCode.MissingEntity, Some(bold.path), "derivative BOLD file is missing the space entity")

    val runText = bold.entities.get(EntityKey.Run).getOrElse("1")
    val runId = RunId.make(runText) match
      case Left(error) =>
        issues += CatalogIssue(CatalogIssueCode.InvalidIdentity, Some(bold.path), error.message)
        None
      case Right(value) => Some(value)
    val header = imageHeaders.get(bold.path).orElse {
      val detail = imageHeaders.failure(bold.path).map(reason => s": $reason").getOrElse("")
      issues += CatalogIssue(CatalogIssueCode.MissingHeader, Some(bold.path), s"BOLD header is unavailable$detail")
      None
    }
    val repetitionTime = repetitionTimeFor(project, bold).flatMap { value =>
      RepetitionTime(value).toOption
    }.orElse {
      issues += CatalogIssue(CatalogIssueCode.MissingRepetitionTime, Some(bold.path), "BOLD metadata has no positive repetition time")
      None
    }
    val events = selectCompanion("events", bold, eventFiles, EventMatchKeys, requirePipeline = false) match
      case Right(file) => Some(file)
      case Left(issue) =>
        issues += issue
        None
    val confounds =
      recipe.confounds match
        case None => None
        case Some(_) =>
          selectOptionalCompanion("confounds", bold, confoundFiles, ConfoundMatchKeys, requirePipeline = true) match
            case Right(file) =>
              if file.isEmpty then
                issues += CatalogIssue(CatalogIssueCode.MissingConfounds, Some(bold.path), "confound selection was requested but no matching confounds table exists")
              file
            case Left(issue) =>
              issues += issue
              None
    val mask =
      recipe.maskPolicy match
        case MaskPolicy.Explicit(_) => None
        case _ =>
          selectCompanion("mask", bold, maskFiles, MaskMatchKeys, requirePipeline = true) match
            case Right(file) => Some(file)
            case Left(issue) =>
              issues += issue
              None

    val result =
      for
        subjectValue <- subject
        taskValue <- task
        spaceValue <- space
        typedRun <- runId
        headerValue <- header
        tr <- repetitionTime
        eventFile <- events
        _ <-
          if recipe.confounds.isEmpty || confounds.nonEmpty then Some(())
          else None
        _ <-
          recipe.maskPolicy match
            case MaskPolicy.Explicit(_) => Some(())
            case _ => mask.map(_ => ())
      yield CandidateRun(
        bold = bold,
        subject = subjectValue,
        session = bold.entities.get(EntityKey.Session),
        task = taskValue,
        acquisition = bold.entities.get(EntityKey.Acquisition),
        echo = bold.entities.get(EntityKey.Echo),
        space = spaceValue,
        resolution = bold.entities.get(EntityKey.Resolution),
        pipeline = bold.pipeline,
        runId = typedRun,
        header = headerValue,
        repetitionTime = tr,
        events = eventFile,
        confounds = confounds,
        mask = mask
      )
    issues.result() -> result

  private def compileUnit(
      recipe: DatasetRecipe,
      imageHeaders: ImageHeaderCatalog,
      key: UnitKey,
      runs: Vector[CandidateRun]
  ): Either[Vector[CatalogIssue], FirstLevelUnit] =
    val issues = Vector.newBuilder[CatalogIssue]
    val duplicateRuns = runs.map(_.runId.value).groupMapReduce(identity)(_ => 1)(_ + _).collect {
      case (id, count) if count > 1 => id
    }.toVector.sorted
    if duplicateRuns.nonEmpty then
      issues += CatalogIssue(
        CatalogIssueCode.DuplicateRun,
        runs.headOption.map(_.bold.path),
        s"analysis unit has duplicate run ids: ${duplicateRuns.mkString(", ")}"
      )

    val referenceSpace = runs.head.header.shape.space
    runs.foreach { run =>
      Grid.exactCongruence(run.header.shape.grid, referenceSpace.grid) match
        case Left(error) =>
          issues += CatalogIssue(
            CatalogIssueCode.IncompatibleGeometry,
            Some(run.bold.path),
            "BOLD spatial geometry does not match the other runs in its analysis unit",
            cause = Some(CatalogIssueCause.Geometry(error))
          )
        case Right(_) => ()
      run.mask.foreach { mask =>
        imageHeaders.get(mask.path) match
          case None =>
            val detail = imageHeaders.failure(mask.path).map(reason => s": $reason").getOrElse("")
            issues += CatalogIssue(CatalogIssueCode.MissingHeader, Some(mask.path), s"mask header is unavailable$detail")
          case Some(maskHeader) =>
            Grid.exactCongruence(maskHeader.shape.grid, run.header.shape.grid) match
              case Left(error) =>
                issues += CatalogIssue(
                  CatalogIssueCode.IncompatibleGeometry,
                  Some(mask.path),
                  "mask and BOLD spatial geometry differ",
                  cause = Some(CatalogIssueCause.Geometry(error))
                )
              case Right(_) => ()
      }
    }

    val mask = unitMask(recipe, runs) match
      case Left(issue) =>
        issues += issue
        None
      case Right(value) => Some(value)
    val unitId = key.id match
      case Left(error) =>
        issues += CatalogIssue(CatalogIssueCode.InvalidIdentity, runs.headOption.map(_.bold.path), error.message)
        None
      case Right(value) => Some(value)

    val accumulated = issues.result()
    if accumulated.nonEmpty then Left(accumulated)
    else
      (unitId, mask) match
        case (Some(resolvedUnitId), Some(resolvedMask)) =>
          val shape = DatasetShape.unsafe(referenceSpace, runs.map(_.header.shape.timepoints).sum)
          val runInputs = runs.map { run =>
            RunInput.unsafe(
              id = run.runId,
              repetitionTime = run.repetitionTime,
              timepoints = run.header.shape.timepoints,
              bold = artifactRef[BoldImageResource](recipe, run.bold),
              events = artifactRef[EventsTableResource](recipe, run.events),
              confounds = run.confounds.map(artifactRef[ConfoundsTableResource](recipe, _))
            )
          }
          FirstLevelUnit.make(
            id = resolvedUnitId,
            subject = SubjectId(key.subject),
            session = key.session.map(SessionId(_)),
            task = TaskId(key.task),
            space = SpaceId(key.space),
            shape = shape,
            runs = runInputs,
            mask = resolvedMask,
            acquisition = key.acquisition,
            echo = key.echo,
            resolution = key.resolution,
            pipeline = key.pipeline
          ).left.map { error =>
            Vector(CatalogIssue(CatalogIssueCode.InvalidUnit, runs.headOption.map(_.bold.path), error.message))
          }
        case _ =>
          Left(Vector(CatalogIssue(
            CatalogIssueCode.InvalidUnit,
            runs.headOption.map(_.bold.path),
            "compiler invariant failed to resolve unit identity and mask"
          )))

  private def unitMask(
      recipe: DatasetRecipe,
      runs: Vector[CandidateRun]
  ): Either[CatalogIssue, UnitMask] =
    recipe.maskPolicy match
      case MaskPolicy.Explicit(mask) =>
        Right(UnitMask.Single(mask))
      case MaskPolicy.RequireCommonDerivative =>
        val masks = runs.flatMap(_.mask).map(artifactRef[MaskImageResource](recipe, _)).distinct
        masks match
          case Vector(mask) => Right(UnitMask.Single(mask))
          case _ =>
            Left(CatalogIssue(
              CatalogIssueCode.AmbiguousCompanion,
              runs.headOption.map(_.bold.path),
              s"common derivative mask policy resolved ${masks.length} distinct masks"
            ))
      case MaskPolicy.IntersectRunMasks =>
        val masks = runs.flatMap(run => run.mask.map(mask => run.runId -> artifactRef[MaskImageResource](recipe, mask)))
        if masks.length != runs.length then
          Left(CatalogIssue(CatalogIssueCode.MissingMask, runs.headOption.map(_.bold.path), "not every run has a derivative mask"))
        else
          val distinct = masks.map(_._2).distinct
          if distinct.length == 1 then Right(UnitMask.Single(distinct.head))
          else UnitMask.intersection(masks).left.map(error => CatalogIssue(CatalogIssueCode.InvalidUnit, None, error.message))

  private def compileParticipants(
      project: BidsProject,
      subjects: Vector[String]
  ): (Vector[CatalogIssue], Vector[ParticipantRecord]) =
    project.participantsTable match
      case None => Vector.empty -> Vector.empty
      case Some(table) =>
        val issues = Vector.newBuilder[CatalogIssue]
        val records = Vector.newBuilder[ParticipantRecord]
        val idIndex = table.columns.indexOf("participant_id")
        if idIndex < 0 then
          Vector(CatalogIssue(CatalogIssueCode.MissingParticipant, Some(BidsPath("participants.tsv")), "participants table has no participant_id column")) -> Vector.empty
        else
          val rowsBySubject = table.rows.groupBy { row =>
            row(idIndex).map(_.stripPrefix("sub-")).getOrElse("")
          }
          subjects.foreach { subject =>
            rowsBySubject.getOrElse(subject, Vector.empty) match
              case Vector() =>
                issues += CatalogIssue(CatalogIssueCode.MissingParticipant, Some(BidsPath("participants.tsv")), s"no participant row for sub-$subject")
              case Vector(row) =>
                val values = table.columns.zip(row).collect {
                  case (name, value) if name != "participant_id" => name -> value
                }.toMap
                ParticipantRecord.make(SubjectId(subject), values) match
                  case Left(error) =>
                    issues += CatalogIssue(CatalogIssueCode.MissingParticipant, Some(BidsPath("participants.tsv")), error.message)
                  case Right(record) => records += record
              case many =>
                issues += CatalogIssue(
                  CatalogIssueCode.DuplicateParticipant,
                  Some(BidsPath("participants.tsv")),
                  s"participant sub-$subject has ${many.length} rows"
                )
          }
          issues.result() -> records.result().sortBy(_.subject.value)

  private def unitKey(run: CandidateRun, grouping: RunGrouping): UnitKey =
    UnitKey(
      subject = run.subject,
      session = run.session,
      task = run.task,
      acquisition = run.acquisition,
      echo = run.echo,
      space = run.space,
      resolution = run.resolution,
      pipeline = run.pipeline,
      run = if grouping == RunGrouping.OneUnitPerRun then Some(run.runId.value) else None
    )

  private def requiredEntity(
      file: BidsFile,
      key: EntityKey,
      issues: scala.collection.mutable.Builder[CatalogIssue, Vector[CatalogIssue]]
  ): Option[String] =
    file.entities.get(key).orElse {
      issues += CatalogIssue(CatalogIssueCode.MissingEntity, Some(file.path), s"BOLD file is missing required '${key.short}' entity")
      None
    }

  private def repetitionTimeFor(project: BidsProject, bold: BidsFile): Option[Double] =
    val direct = project.metadata(bold.path).toOption.flatMap(metadata => BidsMetadataRecord(bold, metadata).repetitionTime.map(_.value))
    direct.orElse {
      val rawCandidates = project.manifest.files.filter { file =>
        file.scope == BidsScope.Raw && isBoldImage(file) && companionMatches(file, bold, EventMatchKeys, requirePipeline = false)
      }
      rawCandidates
        .flatMap(file => project.metadata(file.path).toOption.flatMap(metadata => BidsMetadataRecord(file, metadata).repetitionTime.map(_.value)))
        .distinct
        .singleOption
    }

  private def selectCompanion(
      label: String,
      target: BidsFile,
      candidates: Vector[BidsFile],
      keys: Vector[EntityKey],
      requirePipeline: Boolean
  ): Either[CatalogIssue, BidsFile] =
    selectOptionalCompanion(label, target, candidates, keys, requirePipeline).flatMap {
      case Some(file) => Right(file)
      case None =>
        val code = label match
          case "events" => CatalogIssueCode.MissingEvents
          case "confounds" => CatalogIssueCode.MissingConfounds
          case _ => CatalogIssueCode.MissingMask
        Left(CatalogIssue(code, Some(target.path), s"no matching $label file"))
    }

  private def selectOptionalCompanion(
      label: String,
      target: BidsFile,
      candidates: Vector[BidsFile],
      keys: Vector[EntityKey],
      requirePipeline: Boolean
  ): Either[CatalogIssue, Option[BidsFile]] =
    val matches = candidates.filter(companionMatches(_, target, keys, requirePipeline))
    if matches.isEmpty then Right(None)
    else
      val scored = matches.map(file => file -> keys.count(file.entities.contains))
      val bestScore = scored.map(_._2).max
      val best = scored.collect { case (file, score) if score == bestScore => file }.sortBy(_.path.value)
      best match
        case Vector(file) => Right(Some(file))
        case many =>
          Left(CatalogIssue(
            CatalogIssueCode.AmbiguousCompanion,
            Some(target.path),
            s"$label lookup matched ${many.length} equally specific files: ${many.map(_.path.value).mkString(", ")}"
          ))

  private def companionMatches(
      candidate: BidsFile,
      target: BidsFile,
      keys: Vector[EntityKey],
      requirePipeline: Boolean
  ): Boolean =
    val entitiesMatch = keys.forall { key =>
      candidate.entities.get(key).forall(value => target.entities.get(key).contains(value))
    }
    val pipelineMatches = !requirePipeline || candidate.pipeline == target.pipeline
    entitiesMatch && pipelineMatches

  private def artifactRef[A](recipe: DatasetRecipe, file: BidsFile): WorkflowArtifactRef[A] =
    new WorkflowArtifactRef[A](recipe.project.location.resolve(file.path.value))

  private def isBoldImage(file: BidsFile): Boolean =
    isNifti(file) && file.parsed.exists(_.kind == "bold")

  private def isEventFile(file: BidsFile): Boolean =
    file.scope == BidsScope.Raw && file.extension == "tsv" && file.parsed.exists(_.kind == "events")

  private def isConfoundFile(file: BidsFile): Boolean =
    file.scope == BidsScope.Derivatives && file.extension == "tsv" && (
      file.parsed.exists(name => name.kind == "confounds" || name.entities.get(EntityKey.Description).contains("confounds")) ||
        file.fileName.contains("confounds")
    )

  private def isMaskFile(file: BidsFile): Boolean =
    file.scope == BidsScope.Derivatives && isNifti(file) && (
      file.parsed.exists(name => name.kind == "mask" || name.entities.get(EntityKey.Description).contains("brain")) ||
        file.fileName.contains("_mask.nii")
    )

  private def isNifti(file: BidsFile): Boolean =
    file.extension == "nii" || file.extension == "nii.gz"

extension [A](values: Vector[A])
  private def singleOption: Option[A] =
    if values.length == 1 then Some(values.head) else None
