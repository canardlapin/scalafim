package scalafim.inference

enum RefitPath:
  case FullRefit
  case ExactReduced(algorithm: String)

final case class ExecutionProvenance(
    path: RefitPath,
    rootSeed: RootSeed,
    randomAlgorithm: String
)

final case class ReplicateJob(
    step: ComponentIx,
    withinStep: Int,
    replicate: ReplicateId
)

final case class ReplicateJobPlan(
    problem: ProblemSummary,
    rootSeed: RootSeed,
    randomAlgorithm: String,
    jobs: Vector[ReplicateJob]
)

object ProgramLowering:
  def lower[F, K <: TargetKind, N <: NullKind, D <: DesignKind](
      program: InferenceProgram[F, K, N, D],
      step: ComponentIx,
      capacity: MonteCarloDraws,
      draws: MonteCarloDraws
  ): Either[InferenceError, ReplicateJobPlan] =
    ReplicatePlan.forStep(step, capacity, draws).map { assignments =>
      ReplicateJobPlan(
        program.summary,
        program.spec.seed,
        RandomSource.Algorithm,
        assignments.map(assignment =>
          ReplicateJob(
            assignment.step,
            assignment.withinStep,
            assignment.replicate
          )
        )
      )
    }

trait DataAction[D, A]:
  def apply(data: D, action: A): Either[InferenceError, D]

trait ExactRefitReduction[D, F, A]:
  type Core
  type ReducedFit

  def algorithm: String

  def core(data: D, observed: F): Either[InferenceError, Core]

  def update(core: Core, action: A): Either[InferenceError, ReducedFit]

  def lift(reduced: ReducedFit): Either[InferenceError, F]

final case class RefitOutcome[F](
    fit: F,
    provenance: ExecutionProvenance
)

object RefitExecutor:
  def full[D, F, A](
      data: D,
      action: A,
      seed: RootSeed
  )(using
      transform: DataAction[D, A],
      refit: Refit[D, F]
  ): Either[InferenceError, RefitOutcome[F]] =
    for
      resampled <- transform(data, action)
      fit <- refit.fit(resampled)
    yield RefitOutcome(
      fit,
      ExecutionProvenance(
        RefitPath.FullRefit,
        seed,
        RandomSource.Algorithm
      )
    )

  def exact[D, F, A](
      data: D,
      observed: F,
      action: A,
      seed: RootSeed
  )(using
      reduction: ExactRefitReduction[D, F, A]
  ): Either[InferenceError, RefitOutcome[F]] =
    for
      core <- reduction.core(data, observed)
      reduced <- reduction.update(core, action)
      fit <- reduction.lift(reduced)
    yield RefitOutcome(
      fit,
      ExecutionProvenance(
        RefitPath.ExactReduced(reduction.algorithm),
        seed,
        RandomSource.Algorithm
      )
    )

trait AssociativeReducer[-A, S, +R]:
  def empty: S
  def add(state: S, value: A): S
  def combine(left: S, right: S): S
  def finish(state: S): R

object AssociativeReducer:
  def reduce[A, S, R](
      values: Iterable[A],
      reducer: AssociativeReducer[A, S, R]
  ): R =
    var state = reducer.empty
    val iterator = values.iterator
    while iterator.hasNext do
      state = reducer.add(state, iterator.next())
    reducer.finish(state)

  def reduceChunks[A, S, R](
      chunks: Iterable[Iterable[A]],
      reducer: AssociativeReducer[A, S, R]
  ): R =
    var combined = reducer.empty
    val chunkIterator = chunks.iterator
    while chunkIterator.hasNext do
      var local = reducer.empty
      val values = chunkIterator.next().iterator
      while values.hasNext do
        local = reducer.add(local, values.next())
      combined = reducer.combine(combined, local)
    reducer.finish(combined)

final case class MomentState(
    count: Long,
    mean: Double,
    m2: Double
)

final case class MomentSummary(
    count: Long,
    mean: Double,
    variance: Double
)

object MomentReducer extends AssociativeReducer[Double, MomentState, MomentSummary]:
  override val empty: MomentState = MomentState(0L, 0.0, 0.0)

  override def add(state: MomentState, value: Double): MomentState =
    val nextCount = state.count + 1L
    val delta = value - state.mean
    val nextMean = state.mean + delta / nextCount.toDouble
    val delta2 = value - nextMean
    MomentState(nextCount, nextMean, state.m2 + delta * delta2)

  override def combine(left: MomentState, right: MomentState): MomentState =
    if left.count == 0L then right
    else if right.count == 0L then left
    else
      val count = left.count + right.count
      val delta = right.mean - left.mean
      val leftWeight = left.count.toDouble
      val rightWeight = right.count.toDouble
      MomentState(
        count,
        left.mean + delta * rightWeight / count.toDouble,
        left.m2 + right.m2 + delta * delta * leftWeight * rightWeight / count.toDouble
      )

  override def finish(state: MomentState): MomentSummary =
    MomentSummary(
      state.count,
      state.mean,
      if state.count > 1L then state.m2 / (state.count - 1L).toDouble else 0.0
    )

final case class ExceedanceState(total: Long, exceedances: Long)

final case class ExceedanceReducer(
    observed: Double,
    alternative: Alternative
) extends AssociativeReducer[Double, ExceedanceState, ExceedanceState]:
  override val empty: ExceedanceState = ExceedanceState(0L, 0L)

  override def add(state: ExceedanceState, value: Double): ExceedanceState =
    val exceeds =
      alternative match
        case Alternative.Greater   => value >= observed
        case Alternative.Less      => value <= observed
        case Alternative.TwoSided  => Math.abs(value) >= Math.abs(observed)
    ExceedanceState(
      state.total + 1L,
      state.exceedances + (if exceeds then 1L else 0L)
    )

  override def combine(left: ExceedanceState, right: ExceedanceState): ExceedanceState =
    ExceedanceState(
      left.total + right.total,
      left.exceedances + right.exceedances
    )

  override def finish(state: ExceedanceState): ExceedanceState = state
