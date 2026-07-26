package scalafim.bids

final case class DerivativeRoot(root: BidsPath, pipeline: PipelineName)

final case class BidsFile(
    path: BidsPath,
    parsed: Option[BidsName],
    scope: BidsScope,
    pipeline: Option[PipelineName] = None,
    size: Option[Long] = None
):
  def fileName: String = path.fileName
  def directory: String = path.parent.map(_.value).getOrElse("")
  def datatype: Option[String] = parsed.flatMap(_.datatype)
  def role: Option[BidsFileRole] =
    parsed.flatMap(BidsRegistry.Builtin.roleFor(_, scope))
  def entities: BidsEntities = parsed.map(_.entities).getOrElse(BidsEntities.Empty)
  def extension: String =
    parsed.map(_.extension).getOrElse {
      BidsName.KnownExtensions.find(e => fileName.endsWith("." + e)).getOrElse("")
    }

  def matches(query: BidsQuery): Boolean =
    val scopeMatches =
      query.scope match
        case BidsScope.Raw => scope == BidsScope.Raw
        case BidsScope.Derivatives => scope == BidsScope.Derivatives
        case BidsScope.All => true

    val pipelineMatches =
      query.pipeline.forall(p => pipeline.exists(_.value == p.value))

    Matching.filenameMatches(fileName, query.filename) &&
      scopeMatches &&
      pipelineMatches &&
      query.filters.forall { filter =>
        entities.get(filter.key) match
          case Some(value) => Matching.entityMatches(value, filter, query.matchMode)
          case None =>
            if query.requireEntity then false
            else if Matching.isWildcard(filter, query.matchMode) then true
            else !query.strict
      }

final case class BidsManifest(files: Vector[BidsFile]):
  def query(query: BidsQuery = BidsQuery.All): Vector[BidsFile] =
    files.filter(_.matches(query)).sortBy(_.path.value)

  def paths(query: BidsQuery = BidsQuery.All): Vector[BidsPath] =
    this.query(query).map(_.path)

object BidsManifest:
  private val AuxiliaryNames: Set[String] =
    Set("dataset_description.json", "participants.tsv", "participants.json", "README", "CHANGES", "LICENSE", ".bidsignore")
  private val AuxiliarySuffixes: Vector[String] =
    Vector("_scans.tsv", "_scans.json", "_sessions.tsv", "_sessions.json", "_channels.tsv", "_electrodes.tsv", "_coordsystem.json")

  def fromRelativePaths(
      paths: IterableOnce[String],
      derivatives: Vector[DerivativeRoot] = Vector.empty
  ): BidsManifest =
    val files =
      paths.iterator.toVector.distinct.map { rawPath =>
        val path = BidsPath(rawPath)
        val derivative = derivatives.find(d => path.startsWithPath(d.root))
        val scope = if derivative.isDefined then BidsScope.Derivatives else BidsScope.Raw
        val parsed = BidsRegistry.Builtin.parsePath(path, scope).toOption
        BidsFile(
          path = path,
          parsed = parsed,
          scope = scope,
          pipeline = derivative.map(_.pipeline)
        )
      }
    BidsManifest(files)

  def fromRelativePathsChecked(
      paths: IterableOnce[String],
      derivatives: Vector[DerivativeRoot] = Vector.empty
  ): BidsValidationReport[BidsManifest] =
    val files = Vector.newBuilder[BidsFile]
    val issues = Vector.newBuilder[BidsIssue]
    val seen = scala.collection.mutable.HashSet.empty[String]

    paths.iterator.foreach { rawPath =>
      BidsPath.relative(rawPath) match
        case Left(error) =>
          issues += BidsIssue.error(
            code = BidsIssueCode.InvalidPath,
            path = BidsPath.from(rawPath).toOption,
            field = Some("path"),
            message = error.message
          )
        case Right(path) if seen.add(path.value) =>
          val derivative = derivatives.find(d => path.startsWithPath(d.root))
          val scope = if derivative.isDefined then BidsScope.Derivatives else BidsScope.Raw
          BidsRegistry.Builtin.parsePath(path, scope) match
            case Right(parsed) =>
              files += BidsFile(
                path = path,
                parsed = Some(parsed),
                scope = scope,
                pipeline = derivative.map(_.pipeline)
              )
            case Left(error) =>
              files += BidsFile(
                path = path,
                parsed = None,
                scope = scope,
                pipeline = derivative.map(_.pipeline)
              )
              issues ++= rejectedPathIssues(path, scope, error)
        case Right(_) => ()
    }

    BidsValidationReport.from(BidsManifest(files.result()), issues.result())

  private def rejectedPathIssues(path: BidsPath, scope: BidsScope, registryError: BidsError): Vector[BidsIssue] =
    if isKnownAuxiliary(path) then Vector.empty
    else BidsName.parseGeneric(path.fileName) match
      case Left(parseError) =>
        if looksLikeBidsCandidate(path) then
          Vector(BidsIssue.error(BidsIssueCode.InvalidName, Some(path), None, parseError.message))
        else
          Vector(BidsIssue.warning(
            BidsIssueCode.UnrecognizedFile,
            Some(path),
            None,
            s"file is not recognized as a registered BIDS file: ${parseError.message}"
          ))
      case Right(generic) =>
        if !looksLikeBidsCandidate(path) then
          Vector(BidsIssue.warning(
            BidsIssueCode.UnrecognizedFile,
            Some(path),
            None,
            s"file role '${generic.kind}.${generic.extension}' is not registered as BIDS data"
          ))
        else if isKnownRole(path, generic, scope) && isInheritedMetadataSidecar(path, generic) then
          Vector.empty
        else if isKnownRole(path, generic, scope) then
          detailedValidationIssues(path, generic, scope).getOrElse(
            Vector(BidsIssue.error(BidsIssueCode.InvalidName, Some(path), None, registryError.message))
          )
        else
          Vector(BidsIssue.warning(
            BidsIssueCode.UnsupportedFileRole,
            Some(path),
            None,
            s"BIDS-like file role '${generic.kind}.${generic.extension}' is not supported by the registered datatype specifications"
          ))

  private def looksLikeBidsCandidate(path: BidsPath): Boolean =
    val name = path.fileName
    !isKnownAuxiliary(path) &&
      (
        BidsRegistry.Builtin.datatypeFolder(path).nonEmpty ||
          name.startsWith("sub-") ||
          name.startsWith("ses-") ||
          (name.contains('_') && BidsName.KnownExtensions.exists(extension => name.endsWith("." + extension)))
      )

  private def isKnownAuxiliary(path: BidsPath): Boolean =
    val name = path.fileName
    AuxiliaryNames(name) || AuxiliarySuffixes.exists(name.endsWith)

  private def isKnownRole(path: BidsPath, name: BidsName, scope: BidsScope): Boolean =
    val observedFolder = BidsRegistry.Builtin.datatypeFolder(path)
    candidateSpecs(observedFolder, scope).exists { spec =>
      spec.kinds.exists(kind => kind.name == name.kind && kind.extensions.contains(name.extension))
    }

  private def isInheritedMetadataSidecar(path: BidsPath, name: BidsName): Boolean =
    name.extension == "json" && BidsRegistry.Builtin.datatypeFolder(path).isEmpty

  private def detailedValidationIssues(
      path: BidsPath,
      name: BidsName,
      scope: BidsScope
  ): Option[Vector[BidsIssue]] =
    val observedFolder = BidsRegistry.Builtin.datatypeFolder(path)
    candidateSpecs(observedFolder, scope)
      .filter(_.kinds.exists(kind => kind.name == name.kind && kind.extensions.contains(name.extension)))
      .flatMap { spec =>
        spec.validateAll(name).left.toOption.map(report => spec.name -> report)
      }
      .sortBy { case (specName, report) => (report.issues.length, specName) }
      .headOption
      .map { case (_, report) => report.issues.map(issueAt(path, _)) }

  private def issueAt(path: BidsPath, issue: BidsIssue): BidsIssue =
    issue.severity match
      case BidsIssueSeverity.Error =>
        BidsIssue.error(issue.code, Some(path), issue.field, issue.message)
      case BidsIssueSeverity.Warning =>
        BidsIssue.warning(issue.code, Some(path), issue.field, issue.message)

  private def candidateSpecs(observedFolder: Option[String], scope: BidsScope): Vector[BidsDatatypeSpec] =
    BidsRegistry.Builtin.specs.filter { spec =>
      val folderMatches = observedFolder.forall(_ == spec.folder)
      val scopeMatches =
        scope match
          case BidsScope.All => true
          case BidsScope.Raw => spec.scope == DatatypeScope.Raw || spec.scope == DatatypeScope.Both
          case BidsScope.Derivatives => spec.scope == DatatypeScope.Derivative || spec.scope == DatatypeScope.Both
      folderMatches && scopeMatches
    }
