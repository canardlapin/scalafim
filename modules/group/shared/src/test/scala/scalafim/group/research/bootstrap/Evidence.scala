package scalafim.group.research.bootstrap

/** One study's verdict for one scheme at level alpha. */
enum StudyVerdict:
  case Reject, Retain, Unresolved
  /** Outer PM / solve failure (observed or restricted fit, or hyperparameters). */
  case Failed

  /** Level accounting (§6): failures and Unresolved p bounds count as rejections. */
  def levelRejects: Boolean = this != Retain

  /** Candidate power accounting (§6): only a resolved rejection counts. */
  def powerRejects: Boolean = this == Reject

  /** §6 study failure: an outer failure or an Unresolved p bound. */
  def isStudyFailure: Boolean = this == Failed || this == Unresolved

object StudyVerdict:
  def of(result: Either[StudyFailure, BootstrapOutcome], alpha: Double): StudyVerdict = result match
    case Left(_) => Failed
    case Right(outcome) =>
      outcome.decision(alpha) match
        case StudyDecision.Reject => Reject
        case StudyDecision.Retain => Retain
        case StudyDecision.Unresolved => Unresolved

  def of(record: StudyRecord, scheme: Scheme, alpha: Double): StudyVerdict =
    record.schemes.find(_._1 == scheme).fold(Failed)(r => of(r._2, alpha))

/** The comparator's (native PM/t(n-p)) verdict on one power study. */
enum ComparatorVerdict:
  case Reject, Retain, Failed

  /** §6: for the comparator a failure counts as a rejection. */
  def rejects: Boolean = this != Retain

object ComparatorVerdict:
  def of(record: StudyRecord, alpha: Double): ComparatorVerdict = record.native match
    case Left(_) => Failed
    case Right(c) => if c.failed then Failed else if c.pValue <= alpha then Reject else Retain

/** How failures and Unresolved bounds enter the pilot k (owner decision 2026-09-30: `CountAsRejection`). */
enum PilotFailureAccounting:
  /** k counts failures and Unresolved bounds as rejections (the §6 level convention). */
  case CountAsRejection
  /** k counts resolved rejections only. */
  case CountAsRetention
  /** Failures and Unresolved bounds leave both k and R. */
  case ExcludeFromDenominator

/** The cells the six selected confirmation cells are drawn from (owner decision 2026-09-30). */
enum SelectionPool:
  /** The 90 core cells minus the six fixed-now cells. */
  case CoreMinusFixed

  def cells: Vector[Cell] = Cell.core.filterNot(c => CellManifest.FixedConfirmation.contains(c.id))

/** Tie order among equal maximum null-excess rates (owner decision 2026-09-30: `LowestIdString`). */
enum TieOrder:
  /** Lexicographically lowest ID string first. */
  case LowestIdString
  /** Manifest (declaration) order first. */
  case ManifestOrder

/** The selection rule of §6 ("the other six are the worst pilot null-excess cells, maximum over candidates
  * of k/R, ties to lowest ID"), parameterised; `SelectionRule.Owner` is the owner-decided rule.
  */
final case class SelectionRule(
    accounting: PilotFailureAccounting,
    pool: SelectionPool,
    candidates: Vector[Scheme],
    ties: TieOrder,
    count: Int
):
  require(count >= 1, "select at least one cell")
  require(candidates.nonEmpty && candidates.forall(_.role == SchemeRole.Candidate), "the max runs over candidates only")

object SelectionRule:
  /** Owner decision 2026-09-30: score = max over B-plug, B-fixV, B-EB of (null rejections + study failures)/R,
    * where null rejections are resolved rejections, i.e. failures and Unresolved bounds count as rejections;
    * pool = 90 core cells minus the six fixed; ties to the lexicographically lowest ID string; six cells.
    */
  val Owner: SelectionRule = SelectionRule(
    accounting = PilotFailureAccounting.CountAsRejection,
    pool = SelectionPool.CoreMinusFixed,
    candidates = Vector(Scheme.Plug, Scheme.FixV, Scheme.EmpiricalBayes),
    ties = TieOrder.LowestIdString,
    count = 6
  )

  /** Version tag recorded in manifest-v2.json. */
  val Version = "selection-rule/v1"

/** Pilot null-stream verdicts of one cell, per scheme (one verdict per study). */
final case class PilotEvidence(cell: Cell, verdicts: Map[Scheme, Vector[StudyVerdict]])

enum SelectionError(val message: String):
  case MissingCell(id: String) extends SelectionError(s"no pilot evidence for pool cell $id")
  case DuplicateCell(id: String) extends SelectionError(s"pilot evidence for $id given twice")
  case MissingCandidate(id: String, scheme: String) extends SelectionError(s"no $scheme verdicts for $id")
  case NoStudies(id: String, scheme: String) extends SelectionError(s"no usable studies for $scheme in $id")
  case TooFewCells(available: Int, wanted: Int) extends SelectionError(s"pool has $available cells, need $wanted")

/** Pure confirmation-cell selection (§6), parameterised by a pending `SelectionRule`. */
object ConfirmationSelection:
  /** The null-excess rate k/R of one scheme's pilot verdicts under an accounting rule. */
  def rate(verdicts: Vector[StudyVerdict], accounting: PilotFailureAccounting): Option[Double] =
    accounting match
      case PilotFailureAccounting.CountAsRejection =>
        Option.when(verdicts.nonEmpty)(verdicts.count(_.levelRejects).toDouble / verdicts.length)
      case PilotFailureAccounting.CountAsRetention =>
        Option.when(verdicts.nonEmpty)(verdicts.count(_ == StudyVerdict.Reject).toDouble / verdicts.length)
      case PilotFailureAccounting.ExcludeFromDenominator =>
        val usable = verdicts.filterNot(_.isStudyFailure)
        Option.when(usable.nonEmpty)(usable.count(_ == StudyVerdict.Reject).toDouble / usable.length)

  /** Per pool cell, the maximum over the rule's candidates of the null-excess rate. */
  def scores(pilot: Vector[PilotEvidence], rule: SelectionRule): Either[SelectionError, Vector[(Cell, Double)]] =
    val byId = pilot.groupBy(_.cell.id)
    byId.find(_._2.length > 1) match
      case Some((id, _)) => Left(SelectionError.DuplicateCell(id.value))
      case None =>
        rule.pool.cells.foldLeft[Either[SelectionError, Vector[(Cell, Double)]]](Right(Vector.empty)) { (acc, cell) =>
          acc.flatMap { done =>
            byId.get(cell.id).map(_.head) match
              case None => Left(SelectionError.MissingCell(cell.id.value))
              case Some(evidence) =>
                rule.candidates
                  .foldLeft[Either[SelectionError, Double]](Right(Double.NegativeInfinity)) { (best, scheme) =>
                    best.flatMap { b =>
                      evidence.verdicts.get(scheme) match
                        case None => Left(SelectionError.MissingCandidate(cell.id.value, scheme.code))
                        case Some(v) =>
                          rate(v, rule.accounting).toRight(SelectionError.NoStudies(cell.id.value, scheme.code)).map(math.max(b, _))
                    }
                  }
                  .map(score => done :+ (cell -> score))
          }
        }

  /** The `rule.count` worst cells, worst first. */
  def select(pilot: Vector[PilotEvidence], rule: SelectionRule = SelectionRule.Owner): Either[SelectionError, Vector[CellId]] =
    scores(pilot, rule).flatMap { scored =>
      if scored.length < rule.count then Left(SelectionError.TooFewCells(scored.length, rule.count))
      else
        val manifestIndex = Cell.all.map(_.id).zipWithIndex.toMap
        val ordered = rule.ties match
          case TieOrder.LowestIdString => scored.sortBy((c, s) => (-s, c.id.value))
          case TieOrder.ManifestOrder => scored.sortBy((c, s) => (-s, manifestIndex(c.id)))
        Right(ordered.take(rule.count).map(_._1.id))
    }
