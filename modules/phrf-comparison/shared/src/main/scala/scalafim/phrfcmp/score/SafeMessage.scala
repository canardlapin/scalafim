package scalafim.phrfcmp.score

/** Redacted message text: a constructor name plus named integer totals, nothing else.
  *
  * Every log line and exception message of the scorer and aggregator is built here. The type has no entry that takes
  * a `Double` or free text, so a score, a mean or a difference cannot be formatted into a message by construction
  * (design section 3.2, redaction).
  */
opaque type SafeMessage = String

object SafeMessage:
  private def isToken(s: String): Boolean =
    s.nonEmpty && s.length <= 64 && s.forall(c => c < 128 && (c.isLetterOrDigit || c == '_' || c == '.' || c == '-'))

  /** `constructor` and every total name must be short tokens; totals are integers. */
  def of(constructor: String, totals: (String, Long)*): SafeMessage =
    require(isToken(constructor), "message constructor must be a short token")
    require(totals.forall((k, _) => isToken(k)), "total names must be short tokens")
    if totals.isEmpty then constructor
    else constructor + "(" + totals.map((k, v) => s"$k=$v").mkString(",") + ")"

  /** Reduce an arbitrary class name to a token (letters, digits, `.`, `_`); never carries a message. */
  def classToken(name: String): String =
    val t = name.map(c => if c < 128 && (c.isLetterOrDigit || c == '.' || c == '_') then c else '_').take(48)
    if t.isEmpty then "Unknown" else t

  extension (m: SafeMessage) def text: String = m

/** Typed reason the scorer or aggregator refused. Fields are integers or closed enums only. */
enum ScoreError:
  case NonFiniteValue(site: ScoreError.Site)
  case NegativeValue(site: ScoreError.Site)
  case TooFewValues(site: ScoreError.Site, n: Int, required: Int)
  case VoxelCountMismatch(site: ScoreError.Site)
  case ArmMissing(cell: PilotCell, method: Method)
  case CellMissing(cell: PilotCell)
  case TooFewDatasets(completed: Int, required: Int)
  case UnequalDatasetCounts(cell: PilotCell, found: Int, expected: Int)
  case DatasetIndexMismatch(cell: PilotCell, position: Int)
  case TruthDegenerate(cell: PilotCell)
  case TrialLayout(site: ScoreError.Site)
  case DfTooSmall(pair: GatingPair, df: Int, required: Int)
  case TimingMissing(quantity: TimingQuantity)
  case Internal(exceptionClass: String)

  /** Messages for the four count-carrying constructors are the constructor token only: dataset counts, df and D
    * are fields, read only by the sealed side. `toString` is the message, never the case-class rendering.
    */
  override def toString: String = message.text

  def message: SafeMessage = this match
    case NonFiniteValue(s)             => SafeMessage.of("NonFiniteValue", "site" -> s.ordinal.toLong)
    case NegativeValue(s)              => SafeMessage.of("NegativeValue", "site" -> s.ordinal.toLong)
    case TooFewValues(_, _, _)         => SafeMessage.of("TooFewValues")
    case VoxelCountMismatch(s)         => SafeMessage.of("VoxelCountMismatch", "site" -> s.ordinal.toLong)
    case ArmMissing(c, m)              => SafeMessage.of("ArmMissing", "cell" -> c.ordinal.toLong, "method" -> m.ordinal.toLong)
    case CellMissing(c)                => SafeMessage.of("CellMissing", "cell" -> c.ordinal.toLong)
    case TooFewDatasets(_, _)          => SafeMessage.of("TooFewDatasets")
    case UnequalDatasetCounts(_, _, _) => SafeMessage.of("UnequalDatasetCounts")
    case DatasetIndexMismatch(c, p)    => SafeMessage.of("DatasetIndexMismatch", "cell" -> c.ordinal.toLong, "position" -> p.toLong)
    case TruthDegenerate(c)            => SafeMessage.of("TruthDegenerate", "cell" -> c.ordinal.toLong)
    case TrialLayout(s)                => SafeMessage.of("TrialLayout", "site" -> s.ordinal.toLong)
    case DfTooSmall(_, _, _)           => SafeMessage.of("DfTooSmall")
    case TimingMissing(q)              => SafeMessage.of("TimingMissing", "quantity" -> q.ordinal.toLong)
    case Internal(c)                   => SafeMessage.of("Internal." + SafeMessage.classToken(c))

object ScoreError:
  /** Where a validation failed, as a closed vocabulary (no free text). */
  enum Site:
    case ConditionError, TrialEstimate, TrialTruth, CoverageObservation, Timing, Centring, IccCluster, Corpus
