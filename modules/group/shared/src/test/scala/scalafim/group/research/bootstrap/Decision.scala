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

/** Confirmation evidence for one cell and candidate, built only by `CellEvidence.aggregate`
  * from per-study verdicts, so a raw rejection count cannot be passed in (§6 accounting).
  */
final case class CellEvidence private (cell: Cell, studies: Int, nullRejections: Int, studyFailures: Int)

object CellEvidence:
  /** Study failures are outer PM/solve failures plus Unresolved p bounds; for level each counts as a rejection. */
  def aggregate(cell: Cell, verdicts: Vector[StudyVerdict]): CellEvidence =
    new CellEvidence(cell, verdicts.length, verdicts.count(_.levelRejects), verdicts.count(_.isStudyFailure))

/** Discordance counts on the power stream (n10 = candidate only rejects, n01 = comparator only),
  * built only by `PowerEvidence.aggregate` from paired per-study verdicts.
  */
final case class PowerEvidence private (cell: Cell, studies: Int, candidateOnly: Int, comparatorOnly: Int)

object PowerEvidence:
  /** Candidate: a failure or Unresolved p is a non-rejection. Comparator: a failure is a rejection (§6). */
  def aggregate(cell: Cell, paired: Vector[(StudyVerdict, ComparatorVerdict)]): PowerEvidence =
    val candidate = paired.map(_._1.powerRejects)
    val comparator = paired.map(_._2.rejects)
    val n10 = candidate.zip(comparator).count((a, b) => a && !b)
    val n01 = candidate.zip(comparator).count((a, b) => !a && b)
    new PowerEvidence(cell, paired.length, n10, n01)

/** Why the frozen decision refuses to run. */
enum DecisionRefusal(val message: String):
  case WrongStudyCount(cell: String, studies: Int) extends DecisionRefusal(s"$cell has $studies studies; the protocol fixes R = 20000")
  case WrongConfirmationCells(detail: String) extends DecisionRefusal(s"confirmation cells: $detail")
  case WrongPowerCells(detail: String) extends DecisionRefusal(s"power cells: $detail")

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

  /** The frozen §6 outcome for one candidate. Refuses unless every cell has R = 20000 studies, the cells
    * are exactly 12 distinct core cells including the six fixed ones, and the power cells are exactly the four
    * declared ones. Precedence (pending owner confirmation): Adopt, then Decline, then Bound, else Unresolved.
    */
  def outcome(cells: Vector[CellEvidence], power: Vector[PowerEvidence]): Either[DecisionRefusal, CandidateOutcome] =
    val ids = cells.map(_.cell.id)
    val powerIds = power.map(_.cell.id)
    (cells.map(e => e.cell.id -> e.studies) ++ power.map(e => e.cell.id -> e.studies)).find(_._2 != Studies) match
      case Some((id, r)) => Left(DecisionRefusal.WrongStudyCount(id.value, r))
      case None =>
        if ids.length != 12 || ids.distinct.length != 12 then Left(DecisionRefusal.WrongConfirmationCells(s"need 12 distinct cells, got ${ids.length} (${ids.distinct.length} distinct)"))
        else if !cells.forall(_.cell.family == Family.Core) then Left(DecisionRefusal.WrongConfirmationCells("every confirmation cell is a core Gaussian cell"))
        else if !CellManifest.FixedConfirmation.forall(ids.contains) then Left(DecisionRefusal.WrongConfirmationCells("the six fixed cells must be included"))
        else if powerIds.length != 4 || powerIds.toSet != CellManifest.PowerCells.toSet then
          Left(DecisionRefusal.WrongPowerCells(s"need exactly ${CellManifest.PowerCells.map(_.value).mkString(", ")}"))
        else Right(frozenOutcome(cells, power))

  private def frozenOutcome(cells: Vector[CellEvidence], power: Vector[PowerEvidence]): CandidateOutcome =
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
