package scalafim.graphics

/** Typed aesthetic environment: the normalized, inspectable form of an `AesSpec`.
  *
  * One entry per bound aesthetic, keyed by the `Aesthetic[A]` enum so lookups
  * return values at the aesthetic's own type. Iteration order follows the
  * `Aesthetic` declaration order, so derived metadata is stable.
  */
final class AesEnv[Row] private (
    private val entries: Map[Aesthetic[?], AesValue[Row, ?]]
):
  def get[A](aesthetic: Aesthetic[A]): Option[AesValue[Row, A]] =
    entries.get(aesthetic).map(_.asInstanceOf[AesValue[Row, A]])

  def isBound(aesthetic: Aesthetic[?]): Boolean =
    entries.contains(aesthetic)

  def bound: Vector[Aesthetic[?]] =
    Aesthetic.values.toVector.filter(entries.contains)

  def updated[A](aesthetic: Aesthetic[A], value: AesValue[Row, A]): AesEnv[Row] =
    new AesEnv(entries.updated(aesthetic, value))

  /** Register a scaled binding; a second scaled binding on the same aesthetic
    * remains a typed error.
    */
  def bind[In, A](binding: ScaleBinding[Row, In, A]): Either[GraphicsError, AesEnv[Row]] =
    get(binding.aesthetic) match
      case Some(value) if value.isScaled =>
        Left(GraphicsError.DuplicateScale(binding.aesthetic.label))
      case _ =>
        Right(updated(binding.aesthetic, binding.toAesValue))

  /** Layer-over-plot inheritance: a scaled local binding wins, then a scaled
    * parent binding, then local, then parent.
    */
  def inherit(parent: AesEnv[Row]): AesEnv[Row] =
    val merged = Map.newBuilder[Aesthetic[?], AesValue[Row, ?]]
    Aesthetic.values.foreach { aesthetic =>
      val local = entries.get(aesthetic)
      val resolved = local match
        case Some(value) if value.isScaled =>
          local
        case _ =>
          parent.entries.get(aesthetic) match
            case Some(value) if value.isScaled => Some(value)
            case parentValue                   => local.orElse(parentValue)
      resolved.foreach(value => merged += aesthetic -> value)
    }
    new AesEnv(merged.result())

  /** Scaled bindings in declaration order, each registered exactly once. */
  def scaledEntries: Vector[RegisteredScale[Row]] =
    Aesthetic.values.toVector.flatMap { aesthetic =>
      entries.get(aesthetic) match
        case Some(scaled: AesValue.Scaled[Row, ?, ?]) =>
          Some(RegisteredScale(aesthetic, scaled))
        case _ =>
          None
    }

object AesEnv:
  def empty[Row]: AesEnv[Row] =
    new AesEnv(Map.empty)

/** A scaled aesthetic binding recorded by the scale registry. */
final case class RegisteredScale[Row](
    aesthetic: Aesthetic[?],
    value: AesValue.Scaled[Row, ?, ?]
):
  def scale: Scale[?, ?] =
    value.scale

  def descriptor: ScaleDescriptor =
    value.scale.descriptor

/** Per-layer registry of scaled bindings, built once from the layer's
  * effective aesthetic environment.
  */
final case class ScaleRegistry[Row] private (entries: Vector[RegisteredScale[Row]]):
  def declarations(layerIndex: Int): Vector[ScaleDeclaration] =
    entries.map { entry =>
      val descriptor = entry.descriptor
      ScaleDeclaration(layerIndex, entry.aesthetic.label, descriptor.name, descriptor.kind)
    }

  def trained(layerIndex: Int): Vector[TrainedScale] =
    entries.map { entry =>
      TrainedScale(layerIndex, entry.aesthetic.label, entry.descriptor, entry.scale)
    }

  def forAesthetic(aesthetic: Aesthetic[?]): Option[RegisteredScale[Row]] =
    entries.find(_.aesthetic == aesthetic)

object ScaleRegistry:
  def fromEnv[Row](env: AesEnv[Row]): ScaleRegistry[Row] =
    ScaleRegistry(env.scaledEntries)
