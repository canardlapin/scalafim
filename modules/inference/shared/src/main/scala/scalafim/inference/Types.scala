package scalafim.inference

private object InferenceIdentifier:
  def validate(role: String, value: String): Either[InferenceError, String] =
    if value.nonEmpty && value == value.trim then Right(value)
    else Left(InferenceError.InvalidIdentifier(role, value))

opaque type UnitId = String

object UnitId:
  def apply(value: String): Either[InferenceError, UnitId] =
    InferenceIdentifier.validate("unit id", value)

  private[inference] def unsafe(value: String): UnitId =
    value

  extension (value: UnitId)
    inline def value: String = value

opaque type AssumptionId = String

object AssumptionId:
  def apply(value: String): Either[InferenceError, AssumptionId] =
    InferenceIdentifier.validate("assumption id", value)

  private[inference] def unsafe(value: String): AssumptionId =
    value

  extension (value: AssumptionId)
    inline def value: String = value

opaque type TargetLabel = String

object TargetLabel:
  def apply(value: String): Either[InferenceError, TargetLabel] =
    InferenceIdentifier.validate("target label", value)

  private[inference] def unsafe(value: String): TargetLabel =
    value

  extension (value: TargetLabel)
    inline def value: String = value

opaque type NullLabel = String

object NullLabel:
  def apply(value: String): Either[InferenceError, NullLabel] =
    InferenceIdentifier.validate("null label", value)

  private[inference] def unsafe(value: String): NullLabel =
    value

  extension (value: NullLabel)
    inline def value: String = value

opaque type ConditioningRef = String

object ConditioningRef:
  def apply(value: String): Either[InferenceError, ConditioningRef] =
    InferenceIdentifier.validate("conditioning reference", value)

  extension (value: ConditioningRef)
    inline def value: String = value

opaque type ComponentIx = Int

object ComponentIx:
  def apply(value: Int): Either[InferenceError, ComponentIx] =
    if value >= 0 then Right(value)
    else Left(InferenceError.InvalidComponent(value))

  private[inference] def unsafe(value: Int): ComponentIx =
    value

  extension (value: ComponentIx)
    inline def value: Int = value

opaque type RowIx = Int

object RowIx:
  def apply(value: Int): Either[InferenceError, RowIx] =
    if value >= 0 then Right(value)
    else Left(InferenceError.InvalidPartition(s"row index must be non-negative, got $value"))

  private[inference] def unsafe(value: Int): RowIx =
    value

  extension (value: RowIx)
    inline def value: Int = value

opaque type FeatureIx = Int

object FeatureIx:
  def apply(value: Int): Either[InferenceError, FeatureIx] =
    if value >= 0 then Right(value)
    else Left(InferenceError.InvalidNonNegativeCount("feature index", value))

  private[inference] def unsafe(value: Int): FeatureIx = value

  extension (value: FeatureIx)
    inline def value: Int = value

opaque type RowCount = Int

object RowCount:
  def apply(value: Int): Either[InferenceError, RowCount] =
    positive("row count", value)

  private[inference] def unsafe(value: Int): RowCount =
    value

  extension (value: RowCount)
    inline def value: Int = value

opaque type ReplicateId = Int

object ReplicateId:
  def apply(value: Int): Either[InferenceError, ReplicateId] =
    if value >= 0 then Right(value)
    else Left(InferenceError.InvalidNonNegativeCount("replicate id", value))

  extension (value: ReplicateId)
    inline def value: Int = value

opaque type MonteCarloDraws = Int

object MonteCarloDraws:
  def apply(value: Int): Either[InferenceError, MonteCarloDraws] =
    positive("Monte Carlo draws", value)

  extension (value: MonteCarloDraws)
    inline def value: Int = value

opaque type BatchSize = Int

object BatchSize:
  def apply(value: Int): Either[InferenceError, BatchSize] =
    positive("batch size", value)

  extension (value: BatchSize)
    inline def value: Int = value

opaque type LadderSteps = Int

object LadderSteps:
  def apply(value: Int): Either[InferenceError, LadderSteps] =
    positive("ladder steps", value)

  extension (value: LadderSteps)
    inline def value: Int = value

opaque type ExceedanceBoundary = Int

object ExceedanceBoundary:
  def apply(value: Int): Either[InferenceError, ExceedanceBoundary] =
    positive("exceedance boundary", value)

  extension (value: ExceedanceBoundary)
    inline def value: Int = value

opaque type Alpha = Double

object Alpha:
  def apply(value: Double): Either[InferenceError, Alpha] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(value)
    else Left(InferenceError.InvalidProbability("alpha", value, inclusiveZero = false))

  extension (value: Alpha)
    inline def value: Double = value

opaque type PValue = Double

object PValue:
  def apply(value: Double): Either[InferenceError, PValue] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(InferenceError.InvalidProbability("p-value", value, inclusiveZero = true))

  extension (value: PValue)
    inline def value: Double = value

opaque type RelativeGap = Double

object RelativeGap:
  def apply(value: Double): Either[InferenceError, RelativeGap] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(InferenceError.InvalidProbability("relative gap", value, inclusiveZero = true))

  extension (value: RelativeGap)
    inline def value: Double = value

opaque type RootSeed = Long

object RootSeed:
  def apply(value: Long): RootSeed =
    value

  extension (value: RootSeed)
    inline def value: Long = value

private def positive(role: String, value: Int): Either[InferenceError, Int] =
  if value > 0 then Right(value)
  else Left(InferenceError.InvalidCount(role, value))
