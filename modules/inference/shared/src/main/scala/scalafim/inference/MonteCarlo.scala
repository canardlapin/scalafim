package scalafim.inference

final case class ReplicateStatistic private (
    replicate: ReplicateId,
    value: Double
)

object ReplicateStatistic:
  def from(replicate: ReplicateId, value: Double): Either[InferenceError, ReplicateStatistic] =
    if value.isFinite then Right(ReplicateStatistic(replicate, value))
    else Left(InferenceError.NonFiniteStatistic(s"replicate ${replicate.value}", value))

enum ReplicateProvenance:
  case External
  case Deterministic(seed: RootSeed, algorithm: String)

enum MonteCarloStopReason:
  case Exhausted
  case NonRejectionEarly

final case class MonteCarloReceipt(
    observed: Double,
    alternative: Alternative,
    allocated: MonteCarloDraws,
    consumed: MonteCarloDraws,
    exceedances: Int,
    boundary: Option[ExceedanceBoundary],
    batchSchedule: Vector[Int],
    pValue: PValue,
    standardError: Double,
    stopReason: MonteCarloStopReason,
    replicateIds: Vector[ReplicateId],
    nullValues: Vector[Double],
    provenance: ReplicateProvenance
)

final case class SequentialAccumulator private[inference] (
    observed: Double,
    alternative: Alternative,
    allocated: MonteCarloDraws,
    boundary: ExceedanceBoundary,
    batchSize: BatchSize,
    statistics: Vector[ReplicateStatistic],
    batchSchedule: Vector[Int],
    exceedances: Int,
    provenance: ReplicateProvenance
):
  def consumed: Int = statistics.length
  def stoppedEarly: Boolean = exceedances >= boundary.value
  def complete: Boolean = stoppedEarly || consumed == allocated.value

  def offer(batch: Vector[ReplicateStatistic]): Either[InferenceError, SequentialAccumulator] =
    if complete then Left(InferenceError.InvalidReplicatePlan("cannot add a batch after sequential stopping"))
    else if batch.isEmpty then Left(InferenceError.InvalidCount("sequential batch", 0))
    else if batch.length > batchSize.value || consumed + batch.length > allocated.value then
      Left(InferenceError.InvalidReplicatePlan(
        s"batch of ${batch.length} exceeds batch size ${batchSize.value} or remaining allocation ${allocated.value - consumed}"
      ))
    else
      var i = 0
      var previous = statistics.lastOption.map(_.replicate.value).getOrElse(-1)
      var addedExceedances = 0
      while i < batch.length do
        val current = batch(i)
        if current.replicate.value <= previous then
          return Left(InferenceError.InvalidReplicatePlan(
            s"replicate ids must increase across batches, got ${current.replicate.value} after $previous"
          ))
        if MonteCarlo.isExtreme(observed, alternative, current.value) then addedExceedances += 1
        previous = current.replicate.value
        i += 1
      Right(copy(
        statistics = statistics ++ batch,
        batchSchedule = batchSchedule :+ batch.length,
        exceedances = exceedances + addedExceedances
      ))

  def result: Either[InferenceError, MonteCarloReceipt] =
    if !complete then Left(InferenceError.BudgetExhausted(consumed, allocated.value))
    else
      val denominator = if stoppedEarly then consumed else allocated.value
      MonteCarlo.pValue(exceedances, denominator).map { p =>
        MonteCarlo.receipt(
          observed,
          alternative,
          statistics,
          allocated,
          Some(boundary),
          batchSchedule,
          p,
          if stoppedEarly then MonteCarloStopReason.NonRejectionEarly else MonteCarloStopReason.Exhausted,
          provenance
        )
      }

object MonteCarlo:
  def fixed(
      observed: Double,
      alternative: Alternative,
      statistics: Iterable[ReplicateStatistic],
      provenance: ReplicateProvenance = ReplicateProvenance.External
  ): Either[InferenceError, MonteCarloReceipt] =
    for
      _ <- finiteObserved(observed)
      ordered <- order(statistics)
      allocated <- MonteCarloDraws(ordered.length)
      p <- pValue(countExtreme(observed, alternative, ordered), ordered.length)
    yield receipt(
      observed,
      alternative,
      ordered,
      allocated,
      boundary = None,
      batchSchedule = Vector(ordered.length),
      p,
      MonteCarloStopReason.Exhausted,
      provenance
    )

  def sequential(
      observed: Double,
      alternative: Alternative,
      statistics: Iterable[ReplicateStatistic],
      maxDraws: MonteCarloDraws,
      alpha: Alpha,
      batchSize: BatchSize,
      requestedBoundary: SequentialBoundary = SequentialBoundary.FromAlpha,
      provenance: ReplicateProvenance = ReplicateProvenance.External
  ): Either[InferenceError, MonteCarloReceipt] =
    for
      _ <- finiteObserved(observed)
      ordered <- order(statistics)
      initial <- sequentialAccumulator(
        observed,
        alternative,
        maxDraws,
        alpha,
        batchSize,
        requestedBoundary,
        provenance
      )
      result <- consumeSequential(ordered, initial)
    yield result

  private def consumeSequential(
      offered: Vector[ReplicateStatistic],
      initial: SequentialAccumulator
  ): Either[InferenceError, MonteCarloReceipt] =
    var accumulator = initial
    while !accumulator.complete do
      val thisBatch = Math.min(
        accumulator.batchSize.value,
        accumulator.allocated.value - accumulator.consumed
      )
      if offered.length < accumulator.consumed + thisBatch then
        return Left(InferenceError.BudgetExhausted(offered.length, accumulator.allocated.value))
      accumulator.offer(offered.slice(
        accumulator.consumed,
        accumulator.consumed + thisBatch
      )) match
        case Right(next) => accumulator = next
        case Left(error) => return Left(error)
    accumulator.result

  private[inference] def sequentialAccumulator(
      observed: Double,
      alternative: Alternative,
      maxDraws: MonteCarloDraws,
      alpha: Alpha,
      batchSize: BatchSize,
      requestedBoundary: SequentialBoundary,
      provenance: ReplicateProvenance
  ): Either[InferenceError, SequentialAccumulator] =
    for
      _ <- finiteObserved(observed)
      boundary <- resolveBoundary(requestedBoundary, alpha, maxDraws)
    yield SequentialAccumulator(
      observed,
      alternative,
      maxDraws,
      boundary,
      batchSize,
      Vector.empty,
      Vector.empty,
      exceedances = 0,
      provenance
    )

  private[inference] def receipt(
      observed: Double,
      alternative: Alternative,
      consumed: Vector[ReplicateStatistic],
      allocated: MonteCarloDraws,
      boundary: Option[ExceedanceBoundary],
      batchSchedule: Vector[Int],
      p: PValue,
      reason: MonteCarloStopReason,
      provenance: ReplicateProvenance
  ): MonteCarloReceipt =
    val exceedances = countExtreme(observed, alternative, consumed)
    val standardError = Math.sqrt(p.value * (1.0 - p.value) / consumed.length)
    MonteCarloReceipt(
      observed,
      alternative,
      allocated,
      acceptedDraws(consumed.length),
      exceedances,
      boundary,
      batchSchedule,
      p,
      standardError,
      reason,
      consumed.map(_.replicate),
      consumed.map(_.value),
      provenance
    )

  private def order(values: Iterable[ReplicateStatistic]): Either[InferenceError, Vector[ReplicateStatistic]] =
    val ordered = values.toVector.sortBy(_.replicate.value)
    var i = 1
    while i < ordered.length do
      if ordered(i - 1).replicate == ordered(i).replicate then
        return Left(InferenceError.InvalidReplicatePlan(
          s"duplicate replicate id ${ordered(i).replicate.value}"
        ))
      i += 1
    Right(ordered)

  private[inference] def resolveBoundary(
      requested: SequentialBoundary,
      alpha: Alpha,
      maxDraws: MonteCarloDraws
  ): Either[InferenceError, ExceedanceBoundary] =
    requested match
      case SequentialBoundary.Explicit(value) => Right(value)
      case SequentialBoundary.FromAlpha =>
        ExceedanceBoundary(Math.ceil(alpha.value * (maxDraws.value + 1.0)).toInt)

  private def finiteObserved(value: Double): Either[InferenceError, Unit] =
    if value.isFinite then Right(())
    else Left(InferenceError.NonFiniteStatistic("observed statistic", value))

  private[inference] def pValue(exceedances: Int, draws: Int): Either[InferenceError, PValue] =
    PValue((1.0 + exceedances) / (draws + 1.0))

  private def countExtreme(
      observed: Double,
      alternative: Alternative,
      values: Vector[ReplicateStatistic]
  ): Int =
    var count = 0
    var i = 0
    while i < values.length do
      if isExtreme(observed, alternative, values(i).value) then count += 1
      i += 1
    count

  private[inference] def isExtreme(observed: Double, alternative: Alternative, value: Double): Boolean =
    alternative match
      case Alternative.Greater  => value >= observed
      case Alternative.Less     => value <= observed
      case Alternative.TwoSided => Math.abs(value) >= Math.abs(observed)

  private def acceptedDraws(value: Int): MonteCarloDraws =
    MonteCarloDraws(value) match
      case Right(draws) => draws
      case Left(error)  => throw IllegalStateException(error.message)

final case class BudgetGrant private[inference] (
    ordinal: Int,
    allocated: MonteCarloDraws
)

final case class BudgetState private (
    total: MonteCarloDraws,
    used: Int,
    allocations: Vector[Int],
    schedule: Vector[Int]
):
  def remaining: Int = total.value - used

  def checkout(request: MonteCarloDraws): Either[InferenceError, BudgetGrant] =
    val granted = Math.min(request.value, remaining)
    if granted <= 0 then Left(InferenceError.BudgetExhausted(used, total.value))
    else MonteCarloDraws(granted).map(BudgetGrant(allocations.length, _))

  def record(grant: BudgetGrant, consumed: Int): Either[InferenceError, BudgetState] =
    if grant.ordinal != allocations.length then
      Left(InferenceError.InvalidReplicatePlan(
        s"budget grant ${grant.ordinal} is not the next grant ${allocations.length}"
      ))
    else if consumed < 0 || consumed > grant.allocated.value then
      Left(InferenceError.InvalidReplicatePlan(
        s"consumed $consumed draws from allocation ${grant.allocated.value}"
      ))
    else Right(copy(
      used = used + consumed,
      allocations = allocations :+ grant.allocated.value,
      schedule = schedule :+ consumed
    ))

object BudgetState:
  def initial(total: MonteCarloDraws): BudgetState =
    BudgetState(total, used = 0, allocations = Vector.empty, schedule = Vector.empty)
