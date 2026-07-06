package scalafim.bids

final case class BidsName(
    entities: BidsEntities,
    kind: String,
    extension: String,
    datatype: Option[String] = None
):
  def fileName: String =
    val prefix = entities.renderParts
    val stem = (prefix :+ kind).mkString("_")
    s"$stem.$extension"

  def withEntity(key: EntityKey, value: String): Either[BidsError, BidsName] =
    entities.updated(key, value).map(e => copy(entities = e))

object BidsName:
  val KnownExtensions: Vector[String] =
    Vector("nii.gz", "tsv.gz", "lv.h5", "json", "nii", "tsv", "csv", "txt", "h5", "gii", "bval", "bvec")

  def parse(filename: String): Either[BidsError, BidsName] =
    BidsRegistry.Builtin.parse(filename)

  def from(
      entities: BidsEntities,
      kind: String,
      extension: String,
      datatype: Option[String] = None
  ): Either[BidsError, BidsName] =
    val cleanKind = kind.trim
    val cleanExtension = extension.trim.stripPrefix(".")
    if cleanKind.isEmpty then Left(BidsError.InvalidBidsName(kind, "kind must be non-empty"))
    else if cleanExtension.isEmpty then Left(BidsError.InvalidBidsName(kind, "extension must be non-empty"))
    else Right(BidsName(entities, cleanKind, cleanExtension, datatype))

  def parseGeneric(filename: String): Either[BidsError, BidsName] =
    val base = filename.replace('\\', '/').split('/').lastOption.getOrElse(filename).trim
    if base.isEmpty then Left(BidsError.InvalidBidsName(filename, "filename must be non-empty"))
    else
      KnownExtensions.find(e => base.endsWith("." + e)) match
        case None =>
          Left(BidsError.InvalidBidsName(filename, "unknown or missing extension"))
        case Some(extension) =>
          val stem = base.dropRight(extension.length + 1)
          val tokens = stem.split('_').toVector.filter(_.nonEmpty)
          if tokens.isEmpty then Left(BidsError.InvalidBidsName(filename, "missing suffix/kind"))
          else
            val kind = tokens.last
            val entityTokens = tokens.dropRight(1)
            val pairs =
              BidsEither.traverse(entityTokens.zipWithIndex) { case (token, index) =>
                val dash = token.indexOf('-')
                if dash >= 0 then
                  val keyText = token.substring(0, dash)
                  val value = token.substring(dash + 1)
                  if keyText.isEmpty || value.isEmpty then
                    Left(BidsError.InvalidBidsName(filename, s"malformed entity token '$token'"))
                  else EntityKey.fromKeyOrCustom(keyText).map(key => key -> value)
                else if entityTokens.take(index).exists(!_.contains('-')) then
                  Left(BidsError.InvalidBidsName(filename, s"multiple unkeyed modality tokens before '$kind'"))
                else Right(EntityKey.Modality -> token)
              }

            pairs
              .flatMap(BidsEntities.from)
              .flatMap(entities => BidsName.from(entities, kind = kind, extension = extension))
