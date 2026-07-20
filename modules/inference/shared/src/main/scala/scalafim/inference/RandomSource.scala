package scalafim.inference

/** Immutable SplitMix64 stream. A replicate stream depends only on its root
  * seed and replicate id, never on execution order.
  */
final case class RandomSource private (private val state: Long):
  def nextLong: (Long, RandomSource) =
    val nextState = state + RandomSource.Gamma
    (RandomSource.mix(nextState), RandomSource(nextState))

  def nextDouble: (Double, RandomSource) =
    val (bits, next) = nextLong
    (((bits >>> 11).toDouble * RandomSource.Unit53), next)

  def permutation(size: Int): Either[InferenceError, (Vector[Int], RandomSource)] =
    if size < 0 then Left(InferenceError.InvalidNonNegativeCount("permutation size", size))
    else
      val values = Array.tabulate(size)(identity)
      var cursor = this
      var i = size - 1
      while i > 0 do
        val (uniform, next) = cursor.nextDouble
        val j = Math.min(i, (uniform * (i + 1)).toInt)
        val hold = values(i)
        values(i) = values(j)
        values(j) = hold
        cursor = next
        i -= 1
      Right((values.toVector, cursor))

object RandomSource:
  val Algorithm: String = "splitmix64-v1"

  private val Gamma = 0x9E3779B97F4A7C15L
  private val Unit53 = 1.0 / 9007199254740992.0

  def forReplicate(seed: RootSeed, replicate: ReplicateId): RandomSource =
    RandomSource(mix(seed.value ^ (Gamma * (replicate.value.toLong + 1L))))

  private def mix(input: Long): Long =
    var value = input
    value = (value ^ (value >>> 30)) * 0xBF58476D1CE4E5B9L
    value = (value ^ (value >>> 27)) * 0x94D049BB133111EBL
    value ^ (value >>> 31)

final case class ReplicateAssignment(
    step: ComponentIx,
    withinStep: Int,
    replicate: ReplicateId
)

object ReplicatePlan:
  def forStep(
      step: ComponentIx,
      capacity: MonteCarloDraws,
      draws: MonteCarloDraws
  ): Either[InferenceError, Vector[ReplicateAssignment]] =
    if draws.value > capacity.value then
      Left(InferenceError.InvalidReplicatePlan(
        s"requested ${draws.value} draws from step capacity ${capacity.value}"
      ))
    else
      val start = step.value.toLong * capacity.value.toLong
      val last = start + draws.value.toLong - 1L
      if last > Int.MaxValue then
        Left(InferenceError.InvalidReplicatePlan(
          s"replicate ids overflow Int at step ${step.value} with capacity ${capacity.value}"
        ))
      else
        Right(Vector.tabulate(draws.value) { within =>
          ReplicateAssignment(step, within, acceptedReplicate((start + within).toInt))
        })

  private def acceptedReplicate(value: Int): ReplicateId =
    ReplicateId(value) match
      case Right(replicate) => replicate
      case Left(error)      => throw IllegalStateException(error.message)
