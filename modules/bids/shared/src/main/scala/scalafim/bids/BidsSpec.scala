package scalafim.bids

enum DatatypeScope:
  case Raw
  case Derivative
  case Both

private def checkedBidsType(value: String, label: String): Either[BidsError, String] =
  val clean = value.trim
  if clean.isEmpty then Left(BidsError.InvalidBidsName(value, s"$label must be non-empty"))
  else Right(clean)

private def unsafeBidsType(value: String, label: String): String =
  val clean = value.trim
  require(clean.nonEmpty, s"$label must be non-empty")
  clean

opaque type BidsDatatype = String

object BidsDatatype:
  def from(value: String): Either[BidsError, BidsDatatype] =
    checkedBidsType(value, "datatype")

  def unsafe(value: String): BidsDatatype =
    unsafeBidsType(value, "datatype")

  extension (datatype: BidsDatatype)
    def value: String = datatype

opaque type BidsDatatypeFolder = String

object BidsDatatypeFolder:
  def from(value: String): Either[BidsError, BidsDatatypeFolder] =
    checkedBidsType(value, "datatype folder")

  def unsafe(value: String): BidsDatatypeFolder =
    unsafeBidsType(value, "datatype folder")

  extension (folder: BidsDatatypeFolder)
    def value: String = folder

opaque type BidsSuffix = String

object BidsSuffix:
  def from(value: String): Either[BidsError, BidsSuffix] =
    checkedBidsType(value, "BIDS suffix")

  def unsafe(value: String): BidsSuffix =
    unsafeBidsType(value, "BIDS suffix")

  extension (suffix: BidsSuffix)
    def value: String = suffix

opaque type BidsFormat = String

object BidsFormat:
  def from(value: String): Either[BidsError, BidsFormat] =
    checkedBidsType(value.stripPrefix("."), "BIDS format")

  def unsafe(value: String): BidsFormat =
    unsafeBidsType(value.stripPrefix("."), "BIDS format")

  extension (format: BidsFormat)
    def value: String = format

final case class BidsFileRole private (
    datatype: BidsDatatype,
    folder: BidsDatatypeFolder,
    suffix: BidsSuffix,
    format: BidsFormat,
    scope: DatatypeScope
):
  def datatypeName: String = datatype.value
  def folderName: String = folder.value
  def suffixName: String = suffix.value
  def formatName: String = format.value

object BidsFileRole:
  def from(
      datatype: String,
      folder: String,
      suffix: String,
      format: String,
      scope: DatatypeScope
  ): Either[BidsError, BidsFileRole] =
    for
      datatype <- BidsDatatype.from(datatype)
      folder <- BidsDatatypeFolder.from(folder)
      suffix <- BidsSuffix.from(suffix)
      format <- BidsFormat.from(format)
    yield BidsFileRole(datatype, folder, suffix, format, scope)

  def unsafe(
      datatype: String,
      folder: String,
      suffix: String,
      format: String,
      scope: DatatypeScope
  ): BidsFileRole =
    BidsFileRole(
      BidsDatatype.unsafe(datatype),
      BidsDatatypeFolder.unsafe(folder),
      BidsSuffix.unsafe(suffix),
      BidsFormat.unsafe(format),
      scope
    )

final case class EntityRule(
    key: EntityKey,
    required: Boolean,
    pattern: Option[String] = Some("[A-Za-z0-9]+")
):
  def validate(value: String): Boolean =
    pattern.forall(rx => value.matches(rx))

final case class BidsKind(name: String, extensions: Vector[String]):
  def nonEmpty: Boolean = name.trim.nonEmpty && extensions.nonEmpty

object BidsKind:
  def from(name: String, extensions: Vector[String]): Either[BidsError, BidsKind] =
    val cleanName = name.trim
    val cleanExtensions = extensions.map(_.trim.stripPrefix(".")).filter(_.nonEmpty).distinct
    if cleanName.isEmpty then Left(BidsError.InvalidBidsName(name, "kind name must be non-empty"))
    else if cleanExtensions.isEmpty then Left(BidsError.InvalidBidsName(name, "BidsKind requires at least one extension"))
    else Right(BidsKind(cleanName, cleanExtensions))

final case class BidsDatatypeSpec(
    name: String,
    folder: String,
    scope: DatatypeScope,
    entities: Vector[EntityRule],
    kinds: Vector[BidsKind],
    derivativeMarkers: Vector[EntityKey] = Vector.empty,
    derivativeKinds: Vector[String] = Vector.empty
):
  private val rulesByKey = entities.map(rule => rule.key -> rule).toMap

  def accepts(bidsName: BidsName): Boolean =
    validate(bidsName).isRight

  def validateInFolder(bidsName: BidsName, observedFolder: Option[String]): Either[BidsError, BidsName] =
    observedFolder match
      case Some(found) if found != folder =>
        Left(
          BidsError.InvalidBidsName(
            bidsName.fileName,
            s"datatype folder '$found' does not match registered folder '$folder' for datatype '$name'"
          )
        )
      case _ => validate(bidsName)

  def roleFor(bidsName: BidsName): Either[BidsError, BidsFileRole] =
    validate(bidsName).flatMap { parsed =>
      BidsFileRole.from(name, folder, parsed.kind, parsed.extension, scope)
    }

  def validate(bidsName: BidsName): Either[BidsError, BidsName] =
    val validKind = kinds.exists(k => k.name == bidsName.kind && k.extensions.contains(bidsName.extension))
    if !validKind then
      Left(BidsError.InvalidBidsName(bidsName.fileName, s"kind '${bidsName.kind}.${bidsName.extension}' is not valid for datatype '$name'"))
    else if scope == DatatypeScope.Derivative && (derivativeMarkers.nonEmpty || derivativeKinds.nonEmpty) then
      val marked = derivativeMarkers.exists(bidsName.entities.contains)
      val derivativeKind = derivativeKinds.contains(bidsName.kind)
      if !marked && !derivativeKind then
        Left(BidsError.InvalidBidsName(bidsName.fileName, s"missing derivative marker for datatype '$name'"))
      else validateEntities(bidsName)
    else validateEntities(bidsName)

  private def validateEntities(bidsName: BidsName): Either[BidsError, BidsName] =
    val unknown = bidsName.entities.keys.filterNot(rulesByKey.contains)
    if unknown.nonEmpty then
      Left(BidsError.InvalidBidsName(bidsName.fileName, s"entities not valid for datatype '$name': ${unknown.map(_.short).mkString(", ")}"))
    else
      entities.find(rule => rule.required && !bidsName.entities.contains(rule.key)) match
        case Some(rule) =>
          Left(BidsError.InvalidBidsName(bidsName.fileName, s"missing required entity '${rule.key.short}' for datatype '$name'"))
        case None =>
          bidsName.entities.keys
            .flatMap { key =>
              val value = bidsName.entities(key)
              rulesByKey.get(key).filterNot(_.validate(value)).map(rule => (key, value, rule))
            }
            .headOption match
            case Some((key, value, rule)) =>
              Left(BidsError.InvalidEntityValue(key.short, value, s"does not match ${rule.pattern.getOrElse("<unrestricted>")}"))
            case None =>
              Right(bidsName.copy(datatype = Some(name)))

object BidsSpecs:
  private def rule(key: EntityKey, required: Boolean, pattern: String = "[A-Za-z0-9]+"): EntityRule =
    EntityRule(key, required, Some(pattern))

  val Func: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "func",
      folder = "func",
      scope = DatatypeScope.Raw,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Task, required = true),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.Contrast, required = false),
        rule(EntityKey.Reconstruction, required = false),
        rule(EntityKey.Run, required = false, "[0-9]+"),
        rule(EntityKey.Echo, required = false, "[0-9]+")
      ),
      kinds = Vector(
        BidsKind("bold", Vector("nii.gz", "nii", "json")),
        BidsKind("events", Vector("tsv")),
        BidsKind("sbref", Vector("nii.gz", "nii", "json")),
        BidsKind("physio", Vector("tsv"))
      )
    )

  val Anat: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "anat",
      folder = "anat",
      scope = DatatypeScope.Raw,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.Contrast, required = false),
        rule(EntityKey.Direction, required = false),
        rule(EntityKey.Reconstruction, required = false),
        rule(EntityKey.Run, required = false, "[0-9]+")
      ),
      kinds = Vector(
        "defacemask",
        "T1w",
        "T2w",
        "T1map",
        "T2map",
        "T2star",
        "FLAIR",
        "FLASH",
        "PDmap",
        "PDT2",
        "inplaneT1",
        "inplaneT2",
        "angio"
      ).map(kind => BidsKind(kind, Vector("nii.gz", "nii", "json")))
    )

  val Dwi: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "dwi",
      folder = "dwi",
      scope = DatatypeScope.Raw,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.Direction, required = false),
        rule(EntityKey.Reconstruction, required = false),
        rule(EntityKey.Run, required = false, "[0-9]+")
      ),
      kinds = Vector(BidsKind("dwi", Vector("nii.gz", "nii", "json", "bval", "bvec")))
    )

  val Fmap: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "fmap",
      folder = "fmap",
      scope = DatatypeScope.Raw,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.Direction, required = false),
        rule(EntityKey.Run, required = false, "[0-9]+")
      ),
      kinds = Vector("magnitude1", "magnitude2", "phasediff", "phase1", "phase2", "fieldmap", "epi")
        .map(kind => BidsKind(kind, Vector("nii.gz", "nii", "json")))
    )

  val FmriprepFunc: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "funcprep",
      folder = "func",
      scope = DatatypeScope.Derivative,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Task, required = true),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.Contrast, required = false),
        rule(EntityKey.Reconstruction, required = false),
        rule(EntityKey.Run, required = false, "[A-Za-z0-9]+"),
        rule(EntityKey.Echo, required = false, "[0-9]+"),
        rule(EntityKey.Space, required = false),
        rule(EntityKey.Resolution, required = false),
        rule(EntityKey.Description, required = false),
        rule(EntityKey.Label, required = false),
        rule(EntityKey.Variant, required = false)
      ),
      kinds = Vector(
        BidsKind("roi", Vector("nii.gz", "nii", "json")),
        BidsKind("regressors", Vector("tsv")),
        BidsKind("latent", Vector("lv.h5")),
        BidsKind("preproc", Vector("nii.gz", "nii", "json")),
        BidsKind("bold", Vector("nii.gz", "nii", "json", "lv.h5")),
        BidsKind("brainmask", Vector("nii.gz", "nii", "json")),
        BidsKind("mask", Vector("nii.gz", "nii", "json")),
        BidsKind("confounds", Vector("tsv")),
        BidsKind("timeseries", Vector("tsv")),
        BidsKind("MELODICmix", Vector("tsv")),
        BidsKind("mixing", Vector("tsv")),
        BidsKind("AROMAnoiseICs", Vector("tsv"))
      ),
      derivativeMarkers = Vector(EntityKey.Space, EntityKey.Resolution, EntityKey.Description, EntityKey.Label, EntityKey.Variant),
      derivativeKinds = Vector("roi", "regressors", "latent", "preproc", "brainmask", "mask", "confounds", "timeseries", "MELODICmix", "mixing", "AROMAnoiseICs")
    )

  val FmriprepAnat: BidsDatatypeSpec =
    BidsDatatypeSpec(
      name = "anatprep",
      folder = "anat",
      scope = DatatypeScope.Derivative,
      entities = Vector(
        rule(EntityKey.Subject, required = true),
        rule(EntityKey.Session, required = false),
        rule(EntityKey.Acquisition, required = false),
        rule(EntityKey.From, required = false),
        rule(EntityKey.To, required = false),
        rule(EntityKey.Contrast, required = false),
        rule(EntityKey.Direction, required = false),
        rule(EntityKey.Reconstruction, required = false),
        rule(EntityKey.Run, required = false, "[0-9]+"),
        rule(EntityKey.Space, required = false),
        rule(EntityKey.Label, required = false),
        rule(EntityKey.Description, required = false),
        rule(EntityKey.Mode, required = false),
        rule(EntityKey.Target, required = false),
        rule(EntityKey.ImageClass, required = false),
        rule(EntityKey.Modality, required = false),
        rule(EntityKey.Hemisphere, required = false, "[LR]")
      ),
      kinds = Vector(
        BidsKind("preproc", Vector("nii.gz", "nii", "json")),
        BidsKind("brainmask", Vector("nii.gz", "nii", "json")),
        BidsKind("probtissue", Vector("nii.gz", "nii", "json")),
        BidsKind("mask", Vector("nii.gz", "nii", "json")),
        BidsKind("T1w", Vector("nii.gz", "nii", "json")),
        BidsKind("probseg", Vector("nii.gz", "nii", "json")),
        BidsKind("dtissue", Vector("nii.gz", "nii", "json")),
        BidsKind("dseg", Vector("nii.gz", "nii", "json")),
        BidsKind("warp", Vector("h5")),
        BidsKind("xfm", Vector("txt", "h5")),
        BidsKind("affine", Vector("txt"))
      ),
      derivativeMarkers = Vector(
        EntityKey.From,
        EntityKey.To,
        EntityKey.Space,
        EntityKey.Label,
        EntityKey.Description,
        EntityKey.Mode,
        EntityKey.Target,
        EntityKey.ImageClass,
        EntityKey.Modality,
        EntityKey.Hemisphere
      ),
      derivativeKinds = Vector("preproc", "brainmask", "probtissue", "mask", "probseg", "dtissue", "dseg", "warp", "xfm", "affine")
    )

  val Builtins: Vector[BidsDatatypeSpec] =
    Vector(Func, Anat, Dwi, Fmap, FmriprepFunc, FmriprepAnat)
