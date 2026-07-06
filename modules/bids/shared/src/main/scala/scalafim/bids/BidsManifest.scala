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
