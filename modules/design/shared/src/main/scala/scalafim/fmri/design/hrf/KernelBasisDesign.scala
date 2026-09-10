package scalafim.fmri.design.hrf

import scalafim.fmri.design.event.{ConvolvedTerm, Event, EventTerm}
import scalafim.fmri.hrf.{Seconds, Support}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.ShapePoint
import scalafim.fmri.hrf.linalg.Mat

enum KernelBasisDesignError:
  case PrecisionMismatch(basisStep: Double, precision: Double)
  case NoConditions
  case Membership(detail: String)

  def message: String =
    this match
      case PrecisionMismatch(step, precision) =>
        s"convolution precision $precision must equal the basis fine step $step so the kernel is sampled on its own grid"
      case NoConditions => "the term lowers to no condition columns"
      case Membership(detail) => s"invalid trial membership: $detail"

/** The expanded condition design `A_tilde = [S_1 Phi' ... S_C Phi']` obtained by
  * convolving an event term with a kernel basis, with the column layout needed
  * to contract it against family coefficients:
  * `B(theta) = A_tilde (I_C kron c(theta))`.
  *
  * Columns follow `EventTerm.convolve`: basis-major, `column = basisIx * C + condition`.
  * The amplitude identities are the term's condition tags; the basis columns
  * are compact coordinates, never scientific coefficients.
  */
final class ExpandedConditionDesign private (
    val basis: HrfKernelBasis,
    val term: ConvolvedTerm,
    val conditions: Vector[String]):

  def rows: Int = term.data.rows
  def conditionCount: Int = conditions.length
  def rank: Int = basis.rank
  def columns: Int = conditionCount * rank

  /** Column of basis function `basisIx` for `condition`. */
  def column(condition: Int, basisIx: Int): Int = basisIx * conditionCount + condition

  /** `B(theta)` for coefficients `c` (length `rank`) into a row-major `rows x C` array. */
  def contractInto(coefficients: Array[Double], out: Array[Double]): Unit =
    val t = rows
    val c = conditionCount
    val m = rank
    val data = term.data.data
    val cols = columns
    var r = 0
    while r < t do
      var cond = 0
      while cond < c do
        var acc = 0.0
        var j = 0
        while j < m do
          acc += data(r * cols + j * c + cond) * coefficients(j)
          j += 1
        out(r * c + cond) = acc
        cond += 1
      r += 1

  /** `B(theta)` at a chart point, allocating the coefficient scratch. */
  def designAt(point: ShapePoint): Mat =
    val coefficients = new Array[Double](rank)
    basis.coefficientsInto(point, new Array[Double](basis.fineCount), coefficients)
    val out = new Array[Double](rows * conditionCount)
    contractInto(coefficients, out)
    Mat.unsafe(rows, conditionCount, out)

object ExpandedConditionDesign:

  /** Convolve `term` with the basis kernel on the basis's own fine grid. */
  def lower(
      term: EventTerm,
      samplingFrame: SamplingFrame,
      basis: HrfKernelBasis,
      precision: Seconds,
      dropEmpty: Boolean = true
  ): Either[KernelBasisDesignError, ExpandedConditionDesign] =
    if math.abs(precision.value - basis.spec.fineStep.value) > 1e-12 then
      Left(KernelBasisDesignError.PrecisionMismatch(basis.spec.fineStep.value, precision.value))
    else
      val convolved = term.convolve(basis.kernel, samplingFrame, precision = precision, dropEmpty = dropEmpty)
      val conditions = term.designMatrix(dropEmpty = dropEmpty).conditionTags
      if conditions.isEmpty then Left(KernelBasisDesignError.NoConditions)
      else Right(new ExpandedConditionDesign(basis, convolved, conditions))

/** One-hot trial membership: `condition(trial)` for every trial, with every
  * declared condition owning at least one trial. Stored as indices, never as
  * a dense projector.
  */
final case class TrialMembership private (conditionOfTrial: Vector[Int], conditionCount: Int):
  def trials: Int = conditionOfTrial.length
  def trialsOf(condition: Int): Vector[Int] = conditionOfTrial.indices.filter(conditionOfTrial(_) == condition).toVector

object TrialMembership:
  def make(conditionOfTrial: Vector[Int], conditionCount: Int): Either[KernelBasisDesignError, TrialMembership] =
    if conditionCount < 1 then Left(KernelBasisDesignError.Membership("at least one condition is required"))
    else if conditionOfTrial.isEmpty then Left(KernelBasisDesignError.Membership("at least one trial is required"))
    else if conditionOfTrial.exists(c => c < 0 || c >= conditionCount) then
      Left(KernelBasisDesignError.Membership(s"condition indices must lie in 0 until $conditionCount"))
    else
      val missing = (0 until conditionCount).filterNot(conditionOfTrial.contains)
      if missing.nonEmpty then Left(KernelBasisDesignError.Membership(s"conditions ${missing.mkString(", ")} own no trial"))
      else Right(new TrialMembership(conditionOfTrial, conditionCount))

/** The expanded trial design `X_tilde = [S_1 Phi' ... S_N Phi']`: one column
  * block per trial, obtained by lowering each trial as its own condition. The
  * identity `X_tilde (M kron I_m) = A_tilde` for one-hot membership `M` is the
  * exact `alpha = 0` aggregation law and is tested, not assumed.
  */
final class ExpandedTrialDesign private (
    val basis: HrfKernelBasis,
    val term: ConvolvedTerm,
    val membership: TrialMembership):

  def rows: Int = term.data.rows
  def trials: Int = membership.trials
  def rank: Int = basis.rank
  def columns: Int = trials * rank
  def column(trial: Int, basisIx: Int): Int = basisIx * trials + trial

  /** Sum trial blocks into condition blocks: the condition design implied by membership. */
  def aggregateConditions: Mat =
    val t = rows
    val n = trials
    val c = membership.conditionCount
    val m = rank
    val data = term.data.data
    val cols = columns
    val out = new Array[Double](t * c * m)
    var r = 0
    while r < t do
      var j = 0
      while j < m do
        var trial = 0
        while trial < n do
          val cond = membership.conditionOfTrial(trial)
          out(r * (c * m) + j * c + cond) += data(r * cols + j * n + trial)
          trial += 1
        j += 1
      r += 1
    Mat.unsafe(t, c * m, out)

object ExpandedTrialDesign:

  /** Lower every event of `onsets` as its own trial with the basis kernel. */
  def lower(
      onsets: Vector[Seconds],
      blockIds: Vector[Int],
      durations: Vector[Seconds],
      membership: TrialMembership,
      samplingFrame: SamplingFrame,
      basis: HrfKernelBasis,
      precision: Seconds
  ): Either[KernelBasisDesignError, ExpandedTrialDesign] =
    if math.abs(precision.value - basis.spec.fineStep.value) > 1e-12 then
      Left(KernelBasisDesignError.PrecisionMismatch(basis.spec.fineStep.value, precision.value))
    else if onsets.length != membership.trials then
      Left(KernelBasisDesignError.Membership(s"${onsets.length} onsets for ${membership.trials} trials"))
    else
      val labels = Vector.tabulate(onsets.length)(i => f"trial_${i + 1}%04d")
      val term = EventTerm(
        events = Vector(Event.factor(labels, "trial")),
        onsets = onsets,
        blockIds = blockIds,
        durations = durations,
        termTag = Some("trial")
      )
      val convolved = term.convolve(basis.kernel, samplingFrame, precision = precision, dropEmpty = false)
      // Factor levels are ordered lexically; zero-padded labels keep trial order.
      Right(new ExpandedTrialDesign(basis, convolved, membership))

private[hrf] object KernelBasisDesign:
  /** Support the basis kernel declares, for callers that need the horizon. */
  def support(basis: HrfKernelBasis): Support = basis.kernel.support
