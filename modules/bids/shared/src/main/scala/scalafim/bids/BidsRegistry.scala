package scalafim.bids

final case class BidsRegistry(specs: Vector[BidsDatatypeSpec]):
  def withDatatype(spec: BidsDatatypeSpec): BidsRegistry =
    copy(specs = specs.filterNot(_.name == spec.name) :+ spec)

  def parse(filename: String): Either[BidsError, BidsName] =
    BidsName.parseGeneric(filename).flatMap { name =>
      specs.iterator.map(_.validate(name)).collectFirst { case Right(parsed) => parsed } match
        case Some(parsed) => Right(parsed)
        case None =>
          Left(BidsError.InvalidBidsName(filename, "no registered datatype spec accepted it"))
    }

object BidsRegistry:
  val Builtin: BidsRegistry = BidsRegistry(BidsSpecs.Builtins)
