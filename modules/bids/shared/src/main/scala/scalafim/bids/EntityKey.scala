package scalafim.bids

enum EntityKey:
  case Subject
  case Session
  case Task
  case Acquisition
  case Contrast
  case Direction
  case Reconstruction
  case Run
  case Echo
  case Space
  case Resolution
  case Label
  case Description
  case From
  case To
  case Target
  case ImageClass
  case Modality
  case Hemisphere
  case Mode
  case Variant
  case Custom(shortKey: String, name: String)

  def short: String =
    this match
      case Subject        => "sub"
      case Session        => "ses"
      case Task           => "task"
      case Acquisition    => "acq"
      case Contrast       => "ce"
      case Direction      => "dir"
      case Reconstruction => "rec"
      case Run            => "run"
      case Echo           => "echo"
      case Space          => "space"
      case Resolution     => "res"
      case Label          => "label"
      case Description    => "desc"
      case From           => "from"
      case To             => "to"
      case Target         => "target"
      case ImageClass     => "class"
      case Modality       => "mod"
      case Hemisphere     => "hemi"
      case Mode           => "mode"
      case Variant        => "variant"
      case Custom(k, _)   => k

  def canonicalName: String =
    this match
      case Subject        => "subid"
      case Session        => "session"
      case Task           => "task"
      case Acquisition    => "acquisition"
      case Contrast       => "contrast"
      case Direction      => "dir"
      case Reconstruction => "reconstruction"
      case Run            => "run"
      case Echo           => "echo"
      case Space          => "space"
      case Resolution     => "res"
      case Label          => "label"
      case Description    => "desc"
      case From           => "from"
      case To             => "to"
      case Target         => "target"
      case ImageClass     => "class"
      case Modality       => "mod"
      case Hemisphere     => "hemi"
      case Mode           => "mode"
      case Variant        => "variant"
      case Custom(_, nm)  => nm

object EntityKey:
  val StandardOrder: Vector[EntityKey] =
    Vector(
      Subject,
      Session,
      Task,
      Acquisition,
      Contrast,
      Direction,
      Reconstruction,
      Run,
      Echo,
      Space,
      Resolution,
      Label,
      Description,
      From,
      To,
      Target,
      ImageClass,
      Modality,
      Hemisphere,
      Mode,
      Variant
    )

  private val aliases: Map[String, EntityKey] =
    StandardOrder.flatMap(k => Vector(k.short -> k, k.canonicalName -> k)).toMap ++
      Map(
        "sub" -> Subject,
        "ses" -> Session,
        "desc" -> Description,
        "description" -> Description,
        "modality" -> Modality
      )

  def fromKey(key: String): Option[EntityKey] =
    aliases.get(key.trim)

  def fromKeyOrCustom(key: String): Either[BidsError, EntityKey] =
    val clean = key.trim
    if clean.isEmpty then Left(BidsError.UnknownEntity(key))
    else Right(fromKey(clean).getOrElse(EntityKey.Custom(clean, clean)))
