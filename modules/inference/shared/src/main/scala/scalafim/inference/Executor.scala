package scalafim.inference

trait ExactLadderProtocol[S, K <: TargetKind]:
  def target: TargetSpec[K]
  def roots(initial: S): Either[InferenceError, Vector[Double]]
  def observed(state: S): Either[InferenceError, Double]
  def nullStatistic(
      state: S,
      step: ComponentIx,
      replicate: ReplicateId,
      random: RandomSource
  ): Either[InferenceError, Double]
  def remove(state: S): Either[InferenceError, S]

final case class LadderExecutionConfig(
    perRung: MonteCarloDraws,
    totalBudget: MonteCarloDraws,
    alpha: Alpha,
    batchSize: BatchSize,
    maxSteps: LadderSteps,
    seed: RootSeed,
    unitPolicy: UnitPolicy = UnitPolicy.SingleAxes,
    boundary: SequentialBoundary = SequentialBoundary.FromAlpha
)

final case class ExactLadderRun[S, K <: TargetKind](
    target: TargetSpec[K],
    roots: Vector[Double],
    units: Vector[LatentUnitResult],
    ladder: LadderResult,
    finalState: S
)

object InferenceExecutor:
  def runLadder[S, K <: TargetKind](
      initial: S,
      protocol: ExactLadderProtocol[S, K],
      config: LadderExecutionConfig
  ): Either[InferenceError, ExactLadderRun[S, K]] =
    protocol.roots(initial).flatMap { roots =>
      val limit = Math.min(config.maxSteps.value, roots.length)
      if limit <= 0 then Left(InferenceError.InvalidCount("available ladder roots", limit))
      else execute(initial, protocol, roots, limit, config)
    }

  private def execute[S, K <: TargetKind](
      initial: S,
      protocol: ExactLadderProtocol[S, K],
      roots: Vector[Double],
      limit: Int,
      config: LadderExecutionConfig
  ): Either[InferenceError, ExactLadderRun[S, K]] =
    var state = initial
    var budget = BudgetState.initial(config.totalBudget)
    val steps = Vector.newBuilder[LadderStepResult]
    val batches = Vector.newBuilder[Int]
    var rejected = 0
    var index = 0
    var termination = Option.empty[LadderTermination]

    while index < limit && termination.isEmpty do
      val component = acceptedComponent(index)
      val unit = LatentUnit.Axis(UnitId.unsafe(s"u${index + 1}"), component)
      val observed = protocol.observed(state) match
        case Right(value) => value
        case Left(error)  => return Left(error)

      budget.checkout(config.perRung) match
        case Left(_: InferenceError.BudgetExhausted) =>
          steps += LadderStepResult(
            unit,
            observed,
            Evidence.Unavailable(UnavailableReason.Unsupported("global Monte Carlo budget exhausted")),
            selected = false
          )
          termination = Some(LadderTermination.BudgetExhausted(unit.id))
        case Left(error) => return Left(error)
        case Right(grant) =>
          val assignments = ReplicatePlan.forStep(component, config.perRung, grant.allocated) match
            case Right(value) => value
            case Left(error)  => return Left(error)
          var accumulator = MonteCarlo.sequentialAccumulator(
            observed,
            protocol.target.alternative,
            grant.allocated,
            config.alpha,
            config.batchSize,
            config.boundary,
            ReplicateProvenance.Deterministic(config.seed, RandomSource.Algorithm)
          ) match
            case Right(value) => value
            case Left(error)  => return Left(error)
          while !accumulator.complete do
            val thisBatch = Math.min(
              config.batchSize.value,
              grant.allocated.value - accumulator.consumed
            )
            val statistics = Vector.newBuilder[ReplicateStatistic]
            var draw = accumulator.consumed
            val end = draw + thisBatch
            while draw < end do
              val assignment = assignments(draw)
              val random = RandomSource.forReplicate(config.seed, assignment.replicate)
              protocol.nullStatistic(state, component, assignment.replicate, random) match
                case Left(error) =>
                  return Left(InferenceError.ReplicateFailure(assignment.replicate, error.message))
                case Right(value) =>
                  ReplicateStatistic.from(assignment.replicate, value) match
                    case Right(statistic) => statistics += statistic
                    case Left(error)      => return Left(error)
              draw += 1
            accumulator.offer(statistics.result()) match
              case Right(value) => accumulator = value
              case Left(error)  => return Left(error)
          val receipt = accumulator.result match
            case Right(value) => value
            case Left(error)  => return Left(error)

          budget.record(grant, receipt.consumed.value) match
            case Right(value) => budget = value
            case Left(error)  => return Left(error)
          batches ++= receipt.batchSchedule
          val selected = receipt.pValue.value <= config.alpha.value
          steps += LadderStepResult(unit, observed, Evidence.Computed(receipt), selected)
          if selected then
            rejected += 1
            if index + 1 < limit then
              protocol.remove(state) match
                case Right(value) => state = value
                case Left(error)  => return Left(error)
          else termination = Some(LadderTermination.FirstNonSelection(unit.id))
      index += 1

    val ladder = LadderResult(
      steps.result(),
      rejected,
      budget.used,
      batches.result(),
      budget,
      termination.getOrElse(LadderTermination.Completed)
    )
    val selected = Vector.tabulate(roots.length)(index => index < rejected)
    LatentUnitFormation.form(roots, selected, config.unitPolicy).map { units =>
      ExactLadderRun(protocol.target, roots, units, ladder, state)
    }

  private def acceptedComponent(value: Int): ComponentIx =
    ComponentIx(value) match
      case Right(component) => component
      case Left(error)      => throw IllegalStateException(error.message)
