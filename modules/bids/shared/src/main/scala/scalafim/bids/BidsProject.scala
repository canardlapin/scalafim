package scalafim.bids

enum RepetitionTimeSource:
  case RepetitionTime
  case VolumeTiming

final case class BidsRepetitionTime(file: BidsFile, value: Double, source: RepetitionTimeSource)

final case class BidsMetadataRecord(file: BidsFile, metadata: JsonValue.Obj):
  def path: BidsPath = file.path
  def subject: Option[String] = file.entities.get(EntityKey.Subject)
  def session: Option[String] = file.entities.get(EntityKey.Session)
  def task: Option[String] = file.entities.get(EntityKey.Task)
  def run: Option[String] = file.entities.get(EntityKey.Run)

  def field(name: String): Option[JsonValue] =
    metadata.fields.get(name)

  def string(name: String): Option[String] =
    field(name).flatMap(_.asString)

  def number(name: String): Option[Double] =
    field(name).flatMap(_.asNumber).filter(value => value.isFinite)

  def repetitionTime: Option[BidsRepetitionTime] =
    number("RepetitionTime")
      .filter(_ > 0.0)
      .map(value => BidsRepetitionTime(file, value, RepetitionTimeSource.RepetitionTime))
      .orElse(volumeTimingRepetitionTime)

  private def volumeTimingRepetitionTime: Option[BidsRepetitionTime] =
    field("VolumeTiming").collect { case JsonValue.Arr(values) =>
      values.flatMap(_.asNumber).filter(_.isFinite)
    }.filter(_.length >= 2).flatMap { timings =>
      val diffs = timings.sliding(2).collect { case Vector(left, right) if right > left => right - left }.toVector
      median(diffs).map(value => BidsRepetitionTime(file, value, RepetitionTimeSource.VolumeTiming))
    }

  private def median(values: Vector[Double]): Option[Double] =
    if values.isEmpty then None
    else
      val sorted = values.sorted
      val mid = sorted.length / 2
      Some(if sorted.length % 2 == 1 then sorted(mid) else (sorted(mid - 1) + sorted(mid)) / 2.0)

final case class BidsTaskKey(subject: String, session: Option[String], task: String)

final case class BidsProject(
    root: BidsPath,
    description: Option[DatasetDescription],
    participants: Vector[String],
    participantsTable: Option[BidsTable] = None,
    derivatives: Vector[DerivativeRoot],
    manifest: BidsManifest,
    sidecars: Map[BidsPath, JsonValue.Obj] = Map.empty
):
  def query(query: BidsQuery = BidsQuery.All): Vector[BidsFile] =
    manifest.query(query)

  def paths(query: BidsQuery = BidsQuery.All): Vector[BidsPath] =
    manifest.paths(query)

  def subjects(scope: BidsScope = BidsScope.All, pipeline: Option[PipelineName] = None): Vector[String] =
    entityValues(EntityKey.Subject, scope, pipeline)

  def sessions(scope: BidsScope = BidsScope.All, pipeline: Option[PipelineName] = None): Vector[String] =
    entityValues(EntityKey.Session, scope, pipeline)

  def tasks(scope: BidsScope = BidsScope.All, pipeline: Option[PipelineName] = None): Vector[String] =
    entityValues(EntityKey.Task, scope, pipeline)

  def runs(scope: BidsScope = BidsScope.All, pipeline: Option[PipelineName] = None): Vector[String] =
    entityValues(EntityKey.Run, scope, pipeline)

  def sessionsBySubject(
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None
  ): Map[String, Vector[String]] =
    query(BidsQuery(scope = scope, pipeline = pipeline))
      .flatMap(file => file.entities.get(EntityKey.Subject).map(sub => sub -> file.entities.get(EntityKey.Session)))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.flatten.distinct.sorted)
      .toMap

  def tasksBySubject(
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None
  ): Map[String, Vector[String]] =
    query(BidsQuery(scope = scope, pipeline = pipeline))
      .flatMap(file => file.entities.get(EntityKey.Subject).map(sub => sub -> file.entities.get(EntityKey.Task)))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.flatten.distinct.sorted)
      .toMap

  def runsByTask(
      scope: BidsScope = BidsScope.All,
      pipeline: Option[PipelineName] = None
  ): Map[BidsTaskKey, Vector[String]] =
    query(BidsQuery(scope = scope, pipeline = pipeline))
      .flatMap { file =>
        for
          sub <- file.entities.get(EntityKey.Subject)
          task <- file.entities.get(EntityKey.Task)
        yield BidsTaskKey(sub, file.entities.get(EntityKey.Session), task) -> file.entities.get(EntityKey.Run)
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.flatten.distinct.sorted)
      .toMap

  def funcScans(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      kind: String = "bold"
  ): Vector[BidsFile] =
    query(
      BidsQuery(
        filename = Vector(s"${kind}\\.nii(\\.gz)?$$"),
        scope = BidsScope.Raw,
        filters = Vector(
          EntityFilter(EntityKey.Subject, subid),
          EntityFilter(EntityKey.Task, task),
          EntityFilter(EntityKey.Run, run),
          EntityFilter(EntityKey.Session, session)
        ),
        matchMode = MatchMode.Regex,
        strict = true
      )
    )

  def preprocScans(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      space: String = ".*",
      desc: String = "preproc",
      kind: String = "bold",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Vector[BidsFile] =
    query(
      BidsQuery(
        filename = Vector("\\.nii(\\.gz)?$"),
        scope = BidsScope.Derivatives,
        pipeline = pipeline,
        filters = Vector(
          EntityFilter(EntityKey.Subject, subid),
          EntityFilter(EntityKey.Task, task),
          EntityFilter(EntityKey.Run, run),
          EntityFilter(EntityKey.Session, session),
          EntityFilter(EntityKey.Space, space)
        ),
        matchMode = MatchMode.Regex,
        strict = true
      )
    ).filter(file => isPreprocessedScan(file, kind = kind, desc = desc))

  def anatScans(
      subid: String = ".*",
      session: String = ".*",
      kind: String = "T1w",
      space: String = ".*",
      desc: String = ".*",
      scope: BidsScope = BidsScope.Raw,
      pipeline: Option[PipelineName] = None
  ): Vector[BidsFile] =
    query(
      BidsQuery(
        filename = Vector(s"${kind}\\.nii(\\.gz)?$$"),
        scope = scope,
        pipeline = pipeline,
        filters = Vector(
          EntityFilter(EntityKey.Subject, subid),
          EntityFilter(EntityKey.Session, session),
          EntityFilter(EntityKey.Space, space),
          EntityFilter(EntityKey.Description, desc)
        ),
        matchMode = MatchMode.Regex,
        strict = true
      )
    )

  def eventFiles(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*"
  ): Vector[BidsFile] =
    query(
      BidsQuery(
        filename = Vector("events\\.tsv$"),
        scope = BidsScope.Raw,
        filters = Vector(
          EntityFilter(EntityKey.Subject, subid),
          EntityFilter(EntityKey.Task, task),
          EntityFilter(EntityKey.Run, run),
          EntityFilter(EntityKey.Session, session)
        ),
        matchMode = MatchMode.Regex,
        strict = true
      )
    )

  def confoundFiles(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Vector[BidsFile] =
    query(
      BidsQuery(
        filename = Vector(".*\\.tsv$"),
        scope = BidsScope.Derivatives,
        pipeline = pipeline,
        filters = Vector(
          EntityFilter(EntityKey.Subject, subid),
          EntityFilter(EntityKey.Task, task),
          EntityFilter(EntityKey.Run, run),
          EntityFilter(EntityKey.Session, session)
        ),
        matchMode = MatchMode.Regex,
        strict = true
      )
    ).filter(isConfoundFile)

  def metadata(path: BidsPath, inherit: Boolean = true): Either[BidsError, JsonValue.Obj] =
    if inherit then inheritedMetadata(path) else directMetadata(path)

  def metadataRecords(files: Vector[BidsFile], inherit: Boolean = true): Either[BidsError, Vector[BidsMetadataRecord]] =
    BidsEither.traverse(files)(file => metadata(file.path, inherit).map(meta => BidsMetadataRecord(file, meta)))

  def scanMetadata(query: BidsQuery, inherit: Boolean = true): Either[BidsError, Vector[BidsMetadataRecord]] =
    metadataRecords(this.query(query), inherit)

  def funcScanMetadata(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      kind: String = "bold",
      inherit: Boolean = true
  ): Either[BidsError, Vector[BidsMetadataRecord]] =
    metadataRecords(funcScans(subid = subid, task = task, run = run, session = session, kind = kind), inherit)

  def preprocScanMetadata(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      space: String = ".*",
      desc: String = "preproc",
      kind: String = "bold",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep")),
      inherit: Boolean = true
  ): Either[BidsError, Vector[BidsMetadataRecord]] =
    metadataRecords(
      preprocScans(
        subid = subid,
        task = task,
        run = run,
        session = session,
        space = space,
        desc = desc,
        kind = kind,
        pipeline = pipeline
      ),
      inherit
    )

  def repetitionTimesFor(files: Vector[BidsFile], inherit: Boolean = true): Either[BidsError, Vector[BidsRepetitionTime]] =
    metadataRecords(files, inherit).map(_.flatMap(_.repetitionTime))

  def repetitionTimes(
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      scope: BidsScope = BidsScope.Raw,
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Vector[BidsRepetitionTime]] =
    val files =
      scope match
        case BidsScope.Raw =>
          funcScans(subid = subid, task = task, run = run, session = session)
        case BidsScope.Derivatives =>
          preprocScans(subid = subid, task = task, run = run, session = session, pipeline = pipeline)
        case BidsScope.All =>
          funcScans(subid = subid, task = task, run = run, session = session) ++
            preprocScans(subid = subid, task = task, run = run, session = session, pipeline = pipeline)
    repetitionTimesFor(files, inherit = true)

  def inferRepetitionTime(
      subid: String,
      task: String,
      run: String = ".*",
      session: String = ".*",
      scope: BidsScope = BidsScope.Raw,
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[BidsError, Option[Double]] =
    repetitionTimes(subid = subid, task = task, run = run, session = session, scope = scope, pipeline = pipeline)
      .map(_.headOption.map(_.value))

  def directMetadata(path: BidsPath): Either[BidsError, JsonValue.Obj] =
    directSidecarPath(path) match
      case None        => Right(JsonValue.EmptyObject)
      case Some(jsonp) => Right(sidecars.getOrElse(jsonp, JsonValue.EmptyObject))

  private def inheritedMetadata(path: BidsPath): Either[BidsError, JsonValue.Obj] =
    BidsName.parse(path.fileName).orElse(BidsName.parseGeneric(path.fileName)).map { targetName =>
      val targetDir = path.parent.map(_.value).getOrElse("")
      val ancestors = ancestorDirectories(targetDir)

      val candidates =
        manifest.files
          .filter(file => file.extension == "json")
          .flatMap { file =>
            val candidateName = BidsName.parseGeneric(file.fileName).toOption
            candidateName
              .filter(candidateApplies(_, targetName))
              .filter(_ => ancestors.contains(file.directory))
              .flatMap(_ => sidecars.get(file.path).map(meta => (file, meta)))
          }
          .sortBy { case (file, _) =>
            val depth = ancestors.indexOf(file.directory)
            val specificity = file.parsed.map(_.entities.keys.length).getOrElse(0)
            (depth, specificity, file.path.value)
          }

      candidates.foldLeft(JsonValue.EmptyObject) { case (acc, (_, meta)) =>
        JsonValue.merge(acc, meta)
      }
    }

  private def directSidecarPath(path: BidsPath): Option[BidsPath] =
    if path.fileName.toLowerCase.endsWith(".json") then Some(path)
    else
      BidsName.KnownExtensions
        .find(ext => path.value.endsWith("." + ext))
        .map(ext => BidsPath(path.value.dropRight(ext.length + 1) + ".json"))

  private def ancestorDirectories(dir: String): Vector[String] =
    if dir.isEmpty then Vector("")
    else
      val parts = dir.split('/').toVector.filter(_.nonEmpty)
      Vector("") ++ parts.indices.map(i => parts.take(i + 1).mkString("/")).toVector

  private def candidateApplies(candidate: BidsName, target: BidsName): Boolean =
    candidate.kind == target.kind &&
      candidate.entities.keys.forall { key =>
        target.entities.get(key).contains(candidate.entities(key))
      }

  private def isConfoundFile(file: BidsFile): Boolean =
    file.parsed.exists { name =>
      name.kind == "confounds" ||
      name.entities.get(EntityKey.Description).contains("confounds")
    } || file.fileName.endsWith("_confounds.tsv")

  private def isPreprocessedScan(file: BidsFile, kind: String, desc: String): Boolean =
    file.parsed.exists { name =>
      val kindMatches = regexFind(name.kind, kind)
      val descMatches = name.entities.get(EntityKey.Description).exists(regexFind(_, desc))
      val preprocKindMatches = regexFind(name.kind, desc)
      (kindMatches && descMatches) || preprocKindMatches
    }

  private def entityValues(key: EntityKey, scope: BidsScope, pipeline: Option[PipelineName]): Vector[String] =
    query(BidsQuery(scope = scope, pipeline = pipeline))
      .flatMap(_.entities.get(key))
      .distinct
      .sorted

  private def regexFind(value: String, pattern: String): Boolean =
    pattern.r.findFirstIn(value).isDefined
