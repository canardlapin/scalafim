package scalafim.bids

final case class BidsRegistry(specs: Vector[BidsDatatypeSpec]):
  private val knownDatatypeFolders: Set[String] =
    specs.map(_.folder).toSet

  def withDatatype(spec: BidsDatatypeSpec): BidsRegistry =
    copy(specs = specs.filterNot(_.name == spec.name) :+ spec)

  def parse(filename: String): Either[BidsError, BidsName] =
    BidsName.parseGeneric(filename).flatMap { name =>
      specs.iterator.map(_.validate(name)).collectFirst { case Right(parsed) => parsed } match
        case Some(parsed) => Right(parsed)
        case None =>
          Left(BidsError.InvalidBidsName(filename, "no registered datatype spec accepted it"))
    }

  def parsePath(path: BidsPath): Either[BidsError, BidsName] =
    parsePath(path, BidsScope.All)

  def parsePath(path: BidsPath, scope: BidsScope): Either[BidsError, BidsName] =
    val observedFolder = datatypeFolder(path)
    BidsName.parseGeneric(path.fileName).flatMap { name =>
      specs.iterator
        .filter(spec => scopeAccepts(spec.scope, scope))
        .map(_.validateInFolder(name, observedFolder))
        .collectFirst { case Right(parsed) => parsed } match
        case Some(parsed) => Right(parsed)
        case None =>
          Left(BidsError.InvalidBidsName(path.value, "no registered datatype spec accepted its suffix, entities, and folder"))
    }

  def datatypeFolder(path: BidsPath): Option[String] =
    path.value
      .split('/')
      .toVector
      .dropRight(1)
      .filter(_.nonEmpty)
      .reverse
      .find(knownDatatypeFolders.contains)

  def roleFor(name: BidsName, scope: BidsScope = BidsScope.All): Option[BidsFileRole] =
    specs.iterator
      .filter(spec => scopeAccepts(spec.scope, scope))
      .map(_.roleFor(name))
      .collectFirst { case Right(role) => role }

  private def scopeAccepts(specScope: DatatypeScope, queryScope: BidsScope): Boolean =
    queryScope match
      case BidsScope.All => true
      case BidsScope.Raw => specScope == DatatypeScope.Raw || specScope == DatatypeScope.Both
      case BidsScope.Derivatives => specScope == DatatypeScope.Derivative || specScope == DatatypeScope.Both

object BidsRegistry:
  val Builtin: BidsRegistry = BidsRegistry(BidsSpecs.Builtins)
