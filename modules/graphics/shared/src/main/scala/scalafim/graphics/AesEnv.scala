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
    Aesthetic.values.toVector.flatMap(scaledEntry)

  private[graphics] def scaledEntry(aesthetic: Aesthetic[?]): Option[RegisteredScale[Row]] =
    entries.get(aesthetic) match
      case Some(scaled: AesValue.Scaled[Row, ?, ?]) =>
        Some(RegisteredScale.erased(aesthetic, scaled))
      case _ =>
        None

object AesEnv:
  def empty[Row]: AesEnv[Row] =
    new AesEnv(Map.empty)

/** A scaled aesthetic binding with its hidden input/output types kept together.
  * The only erased cast occurs when recovering a binding from `AesEnv`'s
  * heterogeneous map; all observation and retraining operations are typed
  * again inside this value.
  */
sealed trait RegisteredScale[Row]:
  type In
  type Out

  def aesthetic: Aesthetic[Out]
  def value: AesValue.Scaled[Row, In, Out]

  final def scale: Scale[In, Out] =
    value.scale

  final def descriptor: ScaleDescriptor =
    scale.descriptor

  final def sharesDeclaration(that: RegisteredScale[Row]): Boolean =
    scale.asInstanceOf[AnyRef] eq that.scale.asInstanceOf[AnyRef]

  final def observations(rows: Vector[Row]): Vector[ScaleObservation] =
    val out = Vector.newBuilder[ScaleObservation]
    rows.foreach { row =>
      scale.observation(value.value(row)).foreach(out += _)
    }
    out.result()

  final def trainPlotWide(
      observations: Vector[ScaleObservation]
  ): Either[GraphicsError, RegisteredScale[Row]] =
    scale.trainPlotWide(observations).map { trained =>
      RegisteredScale(aesthetic, AesValue.Scaled(value.value, trained))
    }

  final def install(env: AesEnv[Row]): AesEnv[Row] =
    env.updated(aesthetic, value)

  final def declaration(layerIndex: Int): ScaleDeclaration =
    ScaleDeclaration(layerIndex, aesthetic.label, descriptor.name, descriptor.kind)

  final def trained: TrainedScale =
    TrainedScale(aesthetic.label, descriptor, scale)

object RegisteredScale:
  type Aux[Row, In0, Out0] = RegisteredScale[Row] { type In = In0; type Out = Out0 }

  def apply[Row, In0, Out0](
      aesthetic0: Aesthetic[Out0],
      value0: AesValue.Scaled[Row, In0, Out0]
  ): Aux[Row, In0, Out0] =
    new RegisteredScale[Row]:
      type In = In0
      type Out = Out0
      val aesthetic: Aesthetic[Out] = aesthetic0
      val value: AesValue.Scaled[Row, In, Out] = value0

  /** `AesEnv.updated` is the type-safe construction boundary. Map lookup
    * erases that relation, so recover it once here and keep it packaged.
    */
  private[graphics] def erased[Row](
      aesthetic: Aesthetic[?],
      value: AesValue.Scaled[Row, ?, ?]
  ): RegisteredScale[Row] =
    RegisteredScale(
      aesthetic.asInstanceOf[Aesthetic[Any]],
      value.asInstanceOf[AesValue.Scaled[Row, Any, Any]]
    )

/** Per-layer view of the plot-trained bindings in an effective aesthetic
  * environment. Plot-wide uniqueness and training live in
  * `PlotScaleRegistry`; this view preserves layer provenance.
  */
final case class ScaleRegistry[Row] private (entries: Vector[RegisteredScale[Row]]):
  def declarations(layerIndex: Int): Vector[ScaleDeclaration] =
    entries.map(_.declaration(layerIndex))

  def trained: Vector[TrainedScale] =
    entries.map(_.trained)

  def forAesthetic(aesthetic: Aesthetic[?]): Option[RegisteredScale[Row]] =
    entries.find(_.aesthetic == aesthetic)

object ScaleRegistry:
  def fromEnv[Row](env: AesEnv[Row]): ScaleRegistry[Row] =
    ScaleRegistry(env.scaledEntries)

/** The single trained scale table for a plot. Each aesthetic occurs at most
  * once, in `Aesthetic` declaration order.
  */
final case class PlotScaleRegistry private (scales: Vector[TrainedScale]):
  require(scales.map(_.aesthetic).distinct.length == scales.length, "plot scales must be unique by aesthetic")

  def forAesthetic(aesthetic: Aesthetic[?]): Option[TrainedScale] =
    scales.find(_.aesthetic == aesthetic.label)

object PlotScaleRegistry:
  val empty: PlotScaleRegistry =
    PlotScaleRegistry(Vector.empty)

  private[graphics] def from(scales: Vector[TrainedScale]): PlotScaleRegistry =
    PlotScaleRegistry(scales)
