package scalafim.inference

final case class LadderRung(
    unit: LatentUnit,
    observed: Double,
    nullStatistics: Vector[ReplicateStatistic]
)

object LadderRung:
  def fromValues(
      step: ComponentIx,
      unit: LatentUnit,
      observed: Double,
      values: Iterable[Double],
      replicateCapacity: MonteCarloDraws
  ): Either[InferenceError, LadderRung] =
    val raw = values.toVector
    for
      draws <- MonteCarloDraws(raw.length)
      assignments <- ReplicatePlan.forStep(step, replicateCapacity, draws)
      statistics <- sequence(assignments.zip(raw).map { case (assignment, value) =>
        ReplicateStatistic.from(assignment.replicate, value)
      })
      _ <-
        if observed.isFinite then Right(())
        else Left(InferenceError.NonFiniteStatistic(s"unit ${unit.id.value}", observed))
    yield LadderRung(unit, observed, statistics)

  private def sequence[A](values: Vector[Either[InferenceError, A]]): Either[InferenceError, Vector[A]] =
    val out = Vector.newBuilder[A]
    var i = 0
    while i < values.length do
      values(i) match
        case Right(value) => out += value
        case Left(error)  => return Left(error)
      i += 1
    Right(out.result())

final case class LadderStepResult(
    unit: LatentUnit,
    observed: Double,
    evidence: Evidence[MonteCarloReceipt],
    selected: Boolean
)

enum LadderTermination:
  case FirstNonSelection(unit: UnitId)
  case Completed
  case BudgetExhausted(unit: UnitId)

final case class LadderResult(
    steps: Vector[LadderStepResult],
    rejectedThrough: Int,
    totalDraws: Int,
    batchSchedule: Vector[Int],
    budget: BudgetState,
    termination: LadderTermination
)

object OrderedLadder:
  def evaluate(
      rungs: Iterable[LadderRung],
      perRung: MonteCarloDraws,
      totalBudget: MonteCarloDraws,
      alpha: Alpha,
      batchSize: BatchSize,
      boundary: SequentialBoundary = SequentialBoundary.FromAlpha,
      provenance: ReplicateProvenance = ReplicateProvenance.External
  ): Either[InferenceError, LadderResult] =
    val ordered = rungs.toVector
    if ordered.isEmpty then Left(InferenceError.InvalidCount("ladder rungs", 0))
    else
      var budget = BudgetState.initial(totalBudget)
      val steps = Vector.newBuilder[LadderStepResult]
      val batches = Vector.newBuilder[Int]
      var rejected = 0
      var index = 0
      var termination = Option.empty[LadderTermination]
      while index < ordered.length && termination.isEmpty do
        val rung = ordered(index)
        budget.checkout(perRung) match
          case Left(_: InferenceError.BudgetExhausted) =>
            steps += LadderStepResult(
              rung.unit,
              rung.observed,
              Evidence.Unavailable(UnavailableReason.Unsupported("global Monte Carlo budget exhausted")),
              selected = false
            )
            termination = Some(LadderTermination.BudgetExhausted(rung.unit.id))
          case Left(error) => return Left(error)
          case Right(grant) =>
            MonteCarlo.sequential(
              rung.observed,
              Alternative.Greater,
              rung.nullStatistics,
              grant.allocated,
              alpha,
              batchSize,
              boundary,
              provenance
            ) match
              case Left(error) => return Left(error)
              case Right(receipt) =>
                budget.record(grant, receipt.consumed.value) match
                  case Left(error) => return Left(error)
                  case Right(nextBudget) => budget = nextBudget
                batches ++= receipt.batchSchedule
                val selected = receipt.pValue.value <= alpha.value
                steps += LadderStepResult(
                  rung.unit,
                  rung.observed,
                  Evidence.Computed(receipt),
                  selected
                )
                if selected then rejected += 1
                else termination = Some(LadderTermination.FirstNonSelection(rung.unit.id))
        index += 1

      Right(LadderResult(
        steps.result(),
        rejected,
        budget.used,
        batches.result(),
        budget,
        termination.getOrElse(LadderTermination.Completed)
      ))
