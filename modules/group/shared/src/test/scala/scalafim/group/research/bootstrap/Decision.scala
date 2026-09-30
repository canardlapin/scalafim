package scalafim.group.research.bootstrap

/** One-sided Clopper-Pearson bounds with tail delta: exact beta quantiles,
  * U = 1 at k = R and L = 0 at k = 0 (declaration §6).
  */
object ClopperPearson:
  def upper(k: Int, r: Int, delta: Double): Double =
    require(k >= 0 && k <= r && r > 0, s"need 0 <= k <= R, got k=$k R=$r")
    if k == r then 1.0 else Special.betaQuantile(1.0 - delta, k + 1.0, (r - k).toDouble)

  def lower(k: Int, r: Int, delta: Double): Double =
    require(k >= 0 && k <= r && r > 0, s"need 0 <= k <= R, got k=$k R=$r")
    if k == 0 then 0.0 else Special.betaQuantile(delta, k.toDouble, (r - k + 1).toDouble)

enum NullVerdict:
  case Pass, Fail, Unresolved

enum FailureVerdict:
  case Pass, NotPass

/** Paired power verdicts for one power cell (candidate vs native PM/t(n-p)). */
final case class PowerVerdict(gain: Boolean, nonLoss: Boolean, definiteLoss: Boolean)

enum CandidateOutcome:
  case Adopt
  case Bound(subFamily: SubFamily)
  case Decline
  case Unresolved

/** Owner-predeclared sub-families that may Bound (§6 default). */
enum SubFamily(val label: String):
  case LargeN extends SubFamily("n >= 20")
  case LargeNu extends SubFamily("nu >= 40")

  def contains(cell: Cell): Boolean = this match
    case LargeN => cell.n >= 20
    case LargeNu => cell.trueNu match
        case NuLevel.Infinite => true
        case NuLevel.Finite(nu) => nu >= 40

/** Confirmation evidence for one cell and candidate. */
final case class CellEvidence(cell: Cell, studies: Int, nullRejections: Int, studyFailures: Int)

/** Discordance counts on the power stream: n10 = candidate only rejects, n01 = comparator only. */
final case class PowerEvidence(cell: Cell, studies: Int, candidateOnly: Int, comparatorOnly: Int)

/** Frozen decision protocol of §6 (O2 defaults: margin .065, g = 0, non-loss .02). */
object Decision:
  val Studies = 20000
  val Delta = 6.25e-6
  val Margin = 0.065
  val FailureMargin = 0.005
  val GainMargin = 0.0
  val NonLossMargin = 0.02
  /** Frozen integer regions at R = 20000 (re-verified against R qbeta in the reference fixture). */
  val NullPassMax = 1149
  val NullFailMin = 1456
  val FailurePassMax = 59
  /** 52 tails per candidate x 3 candidates. */
  val Tails = 156

  def nullVerdict(k: Int, r: Int): NullVerdict =
    if ClopperPearson.upper(k, r, Delta) <= Margin then NullVerdict.Pass
    else if ClopperPearson.lower(k, r, Delta) > Margin then NullVerdict.Fail
    else NullVerdict.Unresolved

  def failureVerdict(f: Int, r: Int): FailureVerdict =
    if ClopperPearson.upper(f, r, Delta) <= FailureMargin then FailureVerdict.Pass else FailureVerdict.NotPass

  def powerVerdict(e: PowerEvidence): PowerVerdict =
    val l10 = ClopperPearson.lower(e.candidateOnly, e.studies, Delta)
    val u10 = ClopperPearson.upper(e.candidateOnly, e.studies, Delta)
    val l01 = ClopperPearson.lower(e.comparatorOnly, e.studies, Delta)
    val u01 = ClopperPearson.upper(e.comparatorOnly, e.studies, Delta)
    PowerVerdict(
      gain = l10 - u01 > GainMargin,
      nonLoss = l10 - u01 >= -NonLossMargin,
      definiteLoss = u10 - l01 < -NonLossMargin
    )

  /** Level assertions count a study failure as a rejection (§6): pass `nullRejections` already including failures.
    * Precedence: Adopt, then Decline, then Bound, else Unresolved.
    */
  def outcome(cells: Vector[CellEvidence], power: Vector[PowerEvidence]): CandidateOutcome =
    val nulls = cells.map(c => c -> nullVerdict(c.nullRejections, c.studies))
    val failures = cells.map(c => failureVerdict(c.studyFailures, c.studies))
    val powers = power.map(powerVerdict)
    val allPass = nulls.forall(_._2 == NullVerdict.Pass) && failures.forall(_ == FailureVerdict.Pass)
    if allPass && powers.exists(_.gain) && powers.forall(_.nonLoss) then CandidateOutcome.Adopt
    else if nulls.exists(_._2 == NullVerdict.Fail) || powers.exists(_.definiteLoss) then CandidateOutcome.Decline
    else
      val passing = cells.zip(failures).collect {
        case (c, FailureVerdict.Pass) if nullVerdict(c.nullRejections, c.studies) == NullVerdict.Pass => c.cell
      }
      SubFamily.values
        .find(sf => passing.nonEmpty && passing.forall(sf.contains) && nulls.forall((c, v) => !sf.contains(c.cell) || v != NullVerdict.Fail))
        .fold(CandidateOutcome.Unresolved)(CandidateOutcome.Bound(_))
