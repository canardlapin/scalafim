package scalafim.fmri.design.hrf

import scalafim.fmri.design.DesignError
import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm, EventTerm}
import scalafim.fmri.hrf.{Seconds, Support}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.ShapePoint
import scalafim.fmri.hrf.linalg.Mat
import scala.util.control.NonFatal

enum KernelBasisDesignError:
  case PrecisionMismatch(basisStep: Double, precision: Double)
  case NoConditions
  case Membership(detail: String)
  case TrialInput(error: DesignError)

  def message: String =
    this match
      case PrecisionMismatch(step, precision) =>
        s"convolution precision $precision must equal the basis fine step $step so the kernel is sampled on its own grid"
      case NoConditions => "the term lowers to no condition columns"
      case Membership(detail) => s"invalid trial membership: $detail"
      case TrialInput(error) => error.message

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

/** Trial lowering is explicit: blocked sources retain schedules, not a T*N*m matrix. */
enum TrialDesignLowering(private val checkedBlockSize: Int):
  case Dense extends TrialDesignLowering(1)
  case Blocked(trialsPerBlock: Int) extends TrialDesignLowering(trialsPerBlock)

  require(checkedBlockSize > 0, "trial block size must be positive")

/** Trial columns always follow caller order; each block is basis-major locally.
  * Materializing a block retains all acquisition rows, so whitening can apply
  * its full recurrence independently to every column, including MA tails.
  */
sealed trait TrialBasisDesign:
  def basis: HrfKernelBasis
  def membership: TrialMembership
  def canonicalToInput: Vector[Int]
  def inputToCanonical: Vector[Int]
  def rows: Int
  final def trials: Int = membership.trials
  final def rank: Int = basis.rank
  final def columns: Int = trials * rank
  final def column(trial: Int, basisIx: Int): Int = basisIx * trials + trial
  def trialsPerBlock: Int
  final def blockCount: Int = 1 + (trials - 1) / trialsPerBlock
  final def trialsInBlock(index: Int): Int =
    require(index >= 0 && index < blockCount, "trial block index out of range")
    math.min(trialsPerBlock, trials - index * trialsPerBlock)
  def block(index: Int): Either[KernelBasisDesignError, Mat]
  /** Retained convolved Double values; excludes basis and schedule metadata. */
  def retainedDesignDataValues: Long

object TrialBasisDesign:
  def lower(
      onsets: Vector[Seconds], blockIds: Vector[Int], durations: Vector[Seconds],
      membership: TrialMembership, samplingFrame: SamplingFrame, basis: HrfKernelBasis,
      precision: Seconds, lowering: TrialDesignLowering
  ): Either[KernelBasisDesignError, TrialBasisDesign] = lowering match
    case TrialDesignLowering.Dense =>
      ExpandedTrialDesign.lower(onsets, blockIds, durations, membership, samplingFrame, basis, precision)
    case TrialDesignLowering.Blocked(size) =>
      ExpandedTrialDesign.records(onsets, blockIds, durations, membership, samplingFrame, basis, precision)
        .map: (term, canonical, inverse) =>
          new BlockedTrialDesign(basis, membership, canonical, inverse, term, samplingFrame, precision,
            math.min(size, membership.trials))

/** Each block uses the same EventTerm convolution as dense lowering. No dense
  * all-trial matrix or all-trial event design matrix is ever materialized.
  */
private final class BlockedTrialDesign(
    val basis: HrfKernelBasis,
    val membership: TrialMembership,
    val canonicalToInput: Vector[Int],
    val inputToCanonical: Vector[Int],
    records: EventTerm,
    frame: SamplingFrame,
    precision: Seconds,
    val trialsPerBlock: Int) extends TrialBasisDesign:
  val rows: Int = frame.blockLens.sum
  val retainedDesignDataValues: Long = 0L
  def block(index: Int): Either[KernelBasisDesignError, Mat] =
    val count = trialsInBlock(index)
    val first = index * trialsPerBlock
    val eventRows = (first until first + count).map(inputToCanonical).toVector
    for
      members <- TrialMembership.make(Vector.fill(count)(0), 1)
      expanded <- ExpandedTrialDesign.lower(eventRows.map(records.onsets), eventRows.map(records.blockIds),
        eventRows.map(records.durations), members, frame, basis, precision)
    yield expanded.term.data

/** The expanded trial design `X_tilde = [S_1 Phi' ... S_N Phi']`: one column
  * block per trial, obtained by lowering each trial as its own condition. The
  * identity `X_tilde (M kron I_m) = A_tilde` for one-hot membership `M` is the
  * exact `alpha = 0` aggregation law and is tested, not assumed.
  */
final class ExpandedTrialDesign private (
    val basis: HrfKernelBasis,
    val term: ConvolvedTerm,
    val membership: TrialMembership,
    /** Canonical event row -> caller's trial column. */
    val canonicalToInput: Vector[Int],
    /** Caller's trial column -> canonical event row. */
    val inputToCanonical: Vector[Int]) extends TrialBasisDesign:

  def rows: Int = term.data.rows
  def trialsPerBlock: Int = trials
  def retainedDesignDataValues: Long = term.data.data.length.toLong
  def block(index: Int): Either[KernelBasisDesignError, Mat] =
    require(index == 0, "dense trial design has one block")
    Right(term.data)

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

  /** Lower complete event records as trial columns in caller input order.
    * Only the event rows are stably grouped by run; membership and matrix
    * columns retain their input identity, including interleaved run records.
    */
  def lower(
      onsets: Vector[Seconds],
      blockIds: Vector[Int],
      durations: Vector[Seconds],
      membership: TrialMembership,
      samplingFrame: SamplingFrame,
      basis: HrfKernelBasis,
      precision: Seconds
  ): Either[KernelBasisDesignError, ExpandedTrialDesign] =
    records(onsets, blockIds, durations, membership, samplingFrame, basis, precision).flatMap:
      (term, canonicalToInput, inverse) =>
        try
          val convolved = term.convolve(basis.kernel, samplingFrame, precision = precision, dropEmpty = false)
          Right(new ExpandedTrialDesign(basis, convolved, membership, canonicalToInput, inverse))
        catch
          case NonFatal(error) => Left(KernelBasisDesignError.TrialInput(DesignError.fromThrowable(error)))

  private[hrf] def records(
      onsets: Vector[Seconds], blockIds: Vector[Int], durations: Vector[Seconds],
      membership: TrialMembership, samplingFrame: SamplingFrame, basis: HrfKernelBasis,
      precision: Seconds
  ): Either[KernelBasisDesignError, (EventTerm, Vector[Int], Vector[Int])] =
    if math.abs(precision.value - basis.spec.fineStep.value) > 1e-12 then
      Left(KernelBasisDesignError.PrecisionMismatch(basis.spec.fineStep.value, precision.value))
    else if onsets.length != membership.trials then
      Left(KernelBasisDesignError.Membership(s"${onsets.length} onsets for ${membership.trials} trials"))
    else if blockIds.nonEmpty && blockIds.length != onsets.length then
      Left(KernelBasisDesignError.TrialInput(DesignError.InvalidSchedule(
        s"blockIds has length ${blockIds.length} but expected ${onsets.length}")))
    else if durations.nonEmpty && durations.length != onsets.length then
      Left(KernelBasisDesignError.TrialInput(DesignError.InvalidSchedule(
        s"durations has length ${durations.length} but expected ${onsets.length}")))
    else if samplingFrame.blockLens.iterator.map(_.toLong).sum > Int.MaxValue.toLong ||
        membership.trials.toLong * basis.rank > Int.MaxValue.toLong then
      Left(KernelBasisDesignError.TrialInput(DesignError.InvalidSchedule(
        "trial design row and column counts must fit array indices")))
    else
      val runs = if blockIds.isEmpty then Vector.fill(onsets.length)(0) else blockIds
      val lengths = if durations.isEmpty then Vector.fill(onsets.length)(Seconds(0.0)) else durations
      var input = 0
      while input < onsets.length do
        if runs(input) < 0 || runs(input) >= samplingFrame.nBlocks then
          return Left(KernelBasisDesignError.TrialInput(DesignError.InvalidSchedule(
            s"trial ${input + 1} has block id ${runs(input)} outside 0 until ${samplingFrame.nBlocks}")))
        if lengths(input).value < 0.0 then
          return Left(KernelBasisDesignError.TrialInput(DesignError.InvalidSchedule(
            s"trial ${input + 1} has negative duration ${lengths(input).value}")))
        input += 1
      val canonicalToInput = onsets.indices.sortBy(i => (runs(i), i)).toVector
      val inverse = new Array[Int](onsets.length)
      canonicalToInput.zipWithIndex.foreach { case (original, canonical) => inverse(original) = canonical }
      EventTerm.validated(
        events = Vector(trialEvent(canonicalToInput)),
        onsets = canonicalToInput.map(onsets),
        blockIds = canonicalToInput.map(runs),
        durations = canonicalToInput.map(lengths),
        termTag = Some("trial")
      ).left.map(KernelBasisDesignError.TrialInput.apply)
        .map(term => (term, canonicalToInput, inverse.toVector))

  /** Internal checked factor construction avoids inferring lexical level order
    * and avoids a linear level lookup for each of the N canonical event rows.
    */
  private[design] def trialEvent(canonicalToInput: Vector[Int]): CategoricalEvent =
    require(canonicalToInput.nonEmpty && canonicalToInput.sorted == canonicalToInput.indices.toVector,
      "canonical trial rows must be a permutation of the input trials")
    val levels = Vector.tabulate(canonicalToInput.length)(i => f"trial_${i + 1}%04d")
    CategoricalEvent("trial", canonicalToInput, levels)

private[hrf] object KernelBasisDesign:
  /** Support the basis kernel declares, for callers that need the horizon. */
  def support(basis: HrfKernelBasis): Support = basis.kernel.support
